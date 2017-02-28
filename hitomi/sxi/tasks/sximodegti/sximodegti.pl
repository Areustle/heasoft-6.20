#!/usr/bin/perl
#
# File name: sximodegti.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/07/28 19:17:42 $
# Version: 0
#
# Create a Good Time Interval (GTI) excluding dead time for each supported SXI
# mode (full window, 1/8 window, full window+burst, 1/8 window+burst,
# 1/8 window+area discrimination).  This is used for exposure map generation.
# The task uses the exposure FFF, and creates GTI(s) for all the modes found.
#
# Tool Dependencies:
# 
# Library Dependencies:
#   heacore/perl/ahlog
#   heacore/perl/ahapp
#   heacore/perl/ahgen
#   gen/lib/perl/ahfilterlib
#   POSIX 
#
# Modification History:
#

# Set up
use strict;
use warnings;

use ahlog ;
use ahapp ;
use ahgen qw (:ALL) ;
use ahfilterlib ;

use POSIX; # floor()

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

# input parameters
my $infile="";        # input sxi exposure fff file
my $outfile="";       # output sxi gti file
my $mergefile="";     # output sxi merged datactlm gti file
my $tstart;           # Start time for pointed observations [sec]
my $tstop;            # Stop time for pointed observations [sec]
my $numdclass=0;      # Output par: number of datactlm's in merged gtifile

# Keyword and miscellaneous variables
my $sorted_infile;    # Temporary file with time-sorted data
my $naxis2;           # Number of rows in input file
my $tlmin_ccd;        # Minimum value for CCD_ID column
my $tlmax_ccd;        # Maximum value for CCD_ID column
my $tnull_ccd;        # Null value for CCD_ID column
my $tlmin_seg;        # Minimum value for SEGMENT column
my $tlmax_seg;        # Maximum value for SEGMENT column
my $tnull_seg;        # Null value for SEGMENT column

my $sximodegtierror = 0;  # sximodegti exit status

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

$sximodegtierror = get_parameters () ;
unless ( $sximodegtierror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;   
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($sximodegtierror);
}

$sximodegtierror = initialize () ;
unless ( $sximodegtierror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;   
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($sximodegtierror);
}

$sximodegtierror = do_work () ;
unless ( $sximodegtierror == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($sximodegtierror);
}

$sximodegtierror = finalize () ;
unless ( $sximodegtierror == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($sximodegtierror);
}

# We're done.
ahapp::end_processing($sximodegtierror);

#########################
# Subroutines
#########################

sub get_parameters {

  $infile     = ahapp::query_parameter("infile");
  $outfile    = ahapp::query_parameter("outfile");
  $mergefile  = ahapp::query_parameter("mergefile");
  $tstart     = ahapp::query_parameter("tstart");
  $tstop      = ahapp::query_parameter("tstop");

  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $tstart_expo;
  my $tstop_expo;
  my $status = 0;

  # Input file checking
  if (isRequiredFileNotFound($infile)) { return 1; }

  # Check if output files exist
  if (removeOutputFileIfClobbering($outfile,$ahapp::clobber)) { return 1; }
  if (removeOutputFileIfClobbering($mergefile,$ahapp::clobber)) { return 1; }

  # clone the input file to the output file
  $sorted_infile = formTemporaryFileName((parse_file_name($infile))[0],"ftsort");
  ahapp::add_temp_file($sorted_infile);
  if (copyFITSFile($infile,$sorted_infile)) { return 1; }

  # Get the INSTRUME keyword from the exposure FFF file
  my $instrume = ahgen::get_keyword($sorted_infile,"EXPOSURE","INSTRUME");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "INSTRUME keyword not defined in $sorted_infile"; return ahgen::get_error_flag;
  }
  if(lc $instrume ne "sxi") {
    ahlog::ah_err "INSTRUME keyword should be SXI, not $instrume"; return 1;
  }

  # a. Open exposure file
  # b. Sort exposure data by TIME.
  $status = ahgen::run_ftool("ftsort",$sorted_infile."[EXPOSURE]",$sorted_infile,"TIME","clobber=yes");
  if ( $status ) { ahlog::ah_err "ftsort failed"; return $status; }

  # Read keywords from input exposure file
  ahlog::ah_info "HIGH", "Reading input exposure file $sorted_infile";
  $naxis2 = ahgen::get_keyword($sorted_infile,"EXPOSURE","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "NAXIS2 keyword not defined in $sorted_infile"; return ahgen::get_error_flag;
  }
  $tstart_expo = ahgen::get_keyword($sorted_infile,"EXPOSURE","TSTART");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "TSTART keyword not defined in $sorted_infile"; return ahgen::get_error_flag;
  }
  $tstop_expo = ahgen::get_keyword($sorted_infile,"EXPOSURE","TSTOP");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "TSTOP keyword not defined in $sorted_infile"; return ahgen::get_error_flag;
  }

  # If the TSTART->TSTOP interval from the exposure file does not fully contain
  # the tstart->tstop interval, # then we cannot make a complete GTI
  # We should use the exposure TSTART and/or TSTOP
  if ( $tstart_expo > $tstart ) {
    ahlog::ah_out " *** Exposure file TSTART must begin before parameter tstart";
    ahlog::ah_out "Using $infile TSTART.";
    $tstart = $tstart_expo;
  } 
  if ( $tstop_expo < $tstop ) {
    ahlog::ah_out " *** Exposure file TSTOP must end after parameter tstop";
    ahlog::ah_out "Using $infile TSTOP.";
    $tstop = $tstop_expo;
  }

  # Read TLMIN,TLMAX,TNULL for CCD_ID,SEGMENT
  my $ccdnum = ahgen::get_column_num($sorted_infile,"EXPOSURE","CCD_ID");
  unless ($ccdnum) { ahlog::ah_err "Could not find column CCD_ID in $infile"; return 1;}
  my $segnum = ahgen::get_column_num($sorted_infile,"EXPOSURE","SEGMENT");
  unless ($segnum) { ahlog::ah_err "Could not find column SEGMENT in $infile"; return 1;}
  $tlmin_ccd = ahgen::get_keyword($sorted_infile,"EXPOSURE","TLMIN$ccdnum");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "TLMIN$ccdnum keyword not defined in $sorted_infile"; return ahgen::get_error_flag;
  }
  $tlmax_ccd = ahgen::get_keyword($sorted_infile,"EXPOSURE","TLMAX$ccdnum");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "TLMAX$ccdnum keyword not defined in $sorted_infile"; return ahgen::get_error_flag;
  }
  $tnull_ccd = ahgen::get_keyword($sorted_infile,"EXPOSURE","TNULL$ccdnum");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "TNULL$ccdnum keyword not defined in $sorted_infile"; return ahgen::get_error_flag;
  }
  $tlmin_seg = ahgen::get_keyword($sorted_infile,"EXPOSURE","TLMIN$segnum");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "TLMIN$segnum keyword not defined in $sorted_infile"; return ahgen::get_error_flag;
  }
  $tlmax_seg = ahgen::get_keyword($sorted_infile,"EXPOSURE","TLMAX$segnum");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "TLMAX$segnum keyword not defined in $sorted_infile"; return ahgen::get_error_flag;
  }
  $tnull_seg = ahgen::get_keyword($sorted_infile,"EXPOSURE","TNULL$segnum");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "TLMIN$segnum keyword not defined in $sorted_infile"; return ahgen::get_error_flag;
  }

  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;                 

  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $time_previous=0;
  my $framenum_previous=0;

  # Frame storage variables
  # Initialize hashes indexed by extension number to hold the CCD_ID,
  # SEGMENT, DATACTLM, etc. to which that extension corresponds.
  # These will be searched for each exposure row
  # to figure out where to put the GTI entry
  my $extnum=0;         # number of extensions
  my %GTI_hdfiles;      # list of header definition files for each GTI extension, for ftcreate
  my %GTI_dtfiles;      # list of data files for each GTI extension, for ftcreate
  my %GTI_datactlms;    # DATACTLM for each GTI extension
  my %GTI_ccdids;       # CCD_ID for each GTI extension
  my %GTI_segments;     # SEGMENT for each GTI extension
  my %GTI_detnams;      # DETNAM for each GTI extension
  my %GTI_datamodes;    # DATAMODE for each GTI extension
  my %GTI_numframes;    # Number of frames for each GTI extension
  my %GTI_tstart;       # TSTART for each GTI extension
  my %GTI_tstop;        # TSTOP for each GTI extension
  
  # The DATACLASS column is being changed to DATACTLM, but this tool
  # still needs to work with old data.  Set up an if-block here to get data
  # from the DATACTLM column if it exists, otherwise get it from the DATACLASS
  # column
  my $datactlm_col="DATACTLM";
  my $colnum = ahgen::get_column_num($sorted_infile,"EXPOSURE","DATACTLM");
  if ($colnum == 0) {
    $datactlm_col="DATACLASS";
    ahlog::ah_debug "DATACTLM column not found; using DATACLASS column";
  }
  
  # Counters
  my $num_rows_previous=0;
  my $num_bad_time=0.;
  my $num_bad_ccdid=0;
  my $num_bad_segment=0;
  my $num_bad_frametype=0;
  my $num_bad_datamode=0;
  my $num_bad_detnam=0;

  my @row_present = ();

  my $status = 0;

  # Initialize the row_present array. This stores the number of rows of each
  # CCD_ID+SEGMENT combination that are present in the current group
  # (should be all ones when we're done reading the group of rows
  push @row_present, 0 foreach (0..7);

  # For each row in TIME-sorted exposure file
  # Read following columns:
  #   TIME
  #   CCD_ID
  #   SEGMENT
  #   DATACTLM or DATACLASS
  #   FRAMENUM (integer counting the frames, unique for each set of 8 segments)
  #   FRAMETYPE (0=event data, 1=rframe data, 2=iframe data, 3=dframe data) (?)
  #   DETNAM (CCD, CCD12, CCD34)
  #   DATAMODE (WINDOW1, WINDOW1BURST, WINDOW2, WINDOW2BURST)
  #   ADOU0ENA (0=off, 1=on but only allowed for CCD_ID=1, SEGMENT=1)
  ahlog::ah_info "LOW", "Reading $naxis2 rows from $infile";
  my @time      = ahgen::read_column($sorted_infile,"EXPOSURE","TIME");
  if($naxis2 != @time      ) { ahlog::ah_err "Error reading TIME column in $sorted_infile"; return 1; }
  my @ccd_id    = ahgen::read_column($sorted_infile,"EXPOSURE","CCD_ID");
  if($naxis2 != @ccd_id    ) { ahlog::ah_err "Error reading CCD_ID column in $sorted_infile"; return 1; }
  my @segment   = ahgen::read_column($sorted_infile,"EXPOSURE","SEGMENT");
  if($naxis2 != @segment   ) { ahlog::ah_err "Error reading SEGMENT column in $sorted_infile"; return 1; }
  my @datactlm = ahgen::read_column($sorted_infile,"EXPOSURE",$datactlm_col);
  if($naxis2 != @datactlm ) { ahlog::ah_err "Error reading DATACTLM column in $sorted_infile"; return 1; }
  my @framenum  = ahgen::read_column($sorted_infile,"EXPOSURE","FRAMENUM");
  if($naxis2 != @framenum  ) { ahlog::ah_err "Error reading FRAMENUM column in $sorted_infile"; return 1; }
  my @frametype = ahgen::read_column($sorted_infile,"EXPOSURE","FRAMETYPE");
  if($naxis2 != @frametype ) { ahlog::ah_err "Error reading FRAMETYPE column in $sorted_infile"; return 1; }
  my @detnam    = ahgen::read_column($sorted_infile,"EXPOSURE","DETNAM");
  if($naxis2 != @detnam    ) { ahlog::ah_err "Error reading DETNAM column in $sorted_infile"; return 1; }
  my @datamode  = ahgen::read_column($sorted_infile,"EXPOSURE","DATAMODE");
  if($naxis2 != @datamode  ) { ahlog::ah_err "Error reading DATAMODE column in $sorted_infile"; return 1; }
  # Check for any errors
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "Error reading columns in $sorted_infile"; 
    return ahgen::get_error_flag;
  }

  # Loop through exposure rows
  foreach my $irow (1..$naxis2) {
    my $time_row      = $time[$irow-1];
    my $ccd_id_row    = $ccd_id[$irow-1];
    my $segment_row   = $segment[$irow-1];
    my $datactlm_row  = sprintf("%lx", $datactlm[$irow-1]); # convert to hex (string) from long int 
    my $framenum_row  = $framenum[$irow-1];
    my $frametype_row = $frametype[$irow-1];
    my $detnam_row    = $detnam[$irow-1];
    my $datamode_row  = $datamode[$irow-1];

    # Timing variables
    my $timedel=0;
    my $timtranb=0;
    my $timtrana=0;
    my $expdeadb=0;
    my $expdeada=0;
    my $flushimb=0;
    my $lastdead=0;
    my $lastdel=0;
    my $nomexpo=0;
    my $timepixr=0;

    my $num_windows=0;
    my $subcycletime=0;
    my $extratime=0;

    my @start_time=();
    my @stop_time=();

    ahlog::ah_debug "Reading row $irow of $naxis2 from $sorted_infile:";
    ahlog::ah_debug "  TIME      : $time_row";
    ahlog::ah_debug "  CCD_ID    : $ccd_id_row";
    ahlog::ah_debug "  SEGMENT   : $segment_row";
    ahlog::ah_debug "  DATACTLM : h$datactlm_row";
    ahlog::ah_debug "  FRAMENUM  : $framenum_row";
    ahlog::ah_debug "  FRAMETYPE : $frametype_row";
    ahlog::ah_debug "  DETNAM    : $detnam_row";
    ahlog::ah_debug "  DATAMODE  : $datamode_row";

    # Skip any row that has a NULL value for time
    if( $time_row eq "NULL" ) { 
      ahlog::ah_info "LOW", "Row $irow: TIME is NULL, skipping."; 
      $num_bad_time++;
      next; 
    }

    # If TIME is before the start of pointed observations, skip this row
    # Assumes TIME is in order (sorted in initialize)
    if($time_row < $tstart) { next; }

    # If TIME is after the end of pointed observations, we're done reading
    # Assumes TIME is in order (sorted in initialize)
    if($time_row > $tstop) { 
      ahlog::ah_info "LOW", "Row $irow: TIME is later than $tstop, finished reading exposure file.";
      last; 
    }

    # Check if frametype is event data (skip rframe, iframe, dframe data).
    if ( $frametype_row != 0 ) {
      ahlog::ah_info "LOW", "Row $irow: FRAMETYPE is not event data, skipping. (FRAMETYPE=$frametype_row)";
      $num_bad_frametype++;
      next;
    }

    # if CCD_ID or SEGMENT are out of range, skip this row
    if( $ccd_id_row < $tlmin_ccd or
        $ccd_id_row > $tlmax_ccd or
        $ccd_id_row == $tnull_ccd ) {
      ahlog::ah_info "LOW", "Row $irow: CCD_ID is out of range, skipping. (CCD_ID=$ccd_id_row)";
      $num_bad_ccdid++;
      next;
    }
    if( $segment_row < $tlmin_seg or
        $segment_row > $tlmax_seg or
        $segment_row == $tnull_seg ) {
      ahlog::ah_info "LOW", "Row $irow: SEGMENT is out of range, skipping.(SEGMENT=$segment_row)";
      $num_bad_segment++;
      next;
    }

    # Check if this row is in a supported mode. If not, go to the next row with
    # a notification.
    # These are the possible combinations:
    # CCD_ID  SEG   DETNAM   DATAMODE
    # 0       0     CCD      WINDOW1
    # 0       1     CCD      WINDOW1
    # 0       0     CCD12    WINDOW2
    # 0       1     CCD12    WINDOW2
    # 0       0     CCD12    WINDOW1BURST
    # 0       1     CCD12    WINDOW1BURST
    # 0       0     CCD12    WINDOW2BURST
    # 0       1     CCD12    WINDOW2BURST
    # 0       0     CCD12    WINDOW1BURST2
    # 0       1     CCD12    WINDOW1BURST2
    # 0       0     CCD12    WINDOW2BURST2
    # 0       1     CCD12    WINDOW2BURST2
    # 1       0     CCD      WINDOW1
    # 1       1     CCD      WINDOW1
    # 1       0     CCD12    WINDOW2
    # 1       1     CCD12    WINDOW2
    # 1       0     CCD12    WINDOW1BURST
    # 1       1     CCD12    WINDOW1BURST
    # 1       0     CCD12    WINDOW2BURST
    # 1       1     CCD12    WINDOW2BURST
    # 1       0     CCD12    WINDOW1BURST2
    # 1       1     CCD12    WINDOW1BURST2
    # 1       0     CCD12    WINDOW2BURST2
    # 1       1     CCD12    WINDOW2BURST2
    # 2       0     CCD      WINDOW1
    # 2       1     CCD      WINDOW1
    # 2       0     CCD34    WINDOW1
    # 2       1     CCD34    WINDOW1
    # 3       0     CCD      WINDOW1
    # 3       1     CCD      WINDOW1
    # 3       0     CCD34    WINDOW1
    # 3       1     CCD34    WINDOW1

    # The following 'if' statments check whether this is an allowed
    # combination according to the table above.

    # For each allowed CCD_ID, check various allowed combinations
    if( $ccd_id_row==0 or $ccd_id_row==1 ) {
      # Check CCD_ID = 0 or CCD_ID = 1
      if( uc $detnam_row eq "CCD" ) {
        # Check DETNAM = 'CCD'
        if( uc $datamode_row ne "WINDOW1" ) {
          # 'WINDOW1' must be used for 'CCD'
          ahlog::ah_info "LOW", "Row $irow: DATAMODE is not supported for GTI, skipping. (DATAMODE=$datamode_row)";
          $num_bad_datamode++;
          next;
        } else { 
          # Row is a supported datactlm
          ahlog::ah_info "LOW", "Row $irow: Found DATACTLM $datactlm_row";
        }
      } elsif ( uc $detnam_row eq "CCD12" ) {
        # Check DETNAM = 'CCD12' for CCD_ID = 0, 1
        if( uc $datamode_row ne "WINDOW2" and 
            uc $datamode_row ne "WINDOW1BURST" and
            uc $datamode_row ne "WINDOW1BURST2" and
            uc $datamode_row ne "WINDOW2BURST" ) {
          # 'CCD12' must have one of these four DATAMODEs
          ahlog::ah_info "LOW", "Row $irow: DATAMODE is not supported for GTI, skipping. (DATAMODE=$datamode_row)";
          $num_bad_datamode++;
          next;
        } else { 
          # Row is a supported datactlm
          ahlog::ah_info "LOW", "Row $irow: Found DATACTLM $datactlm_row";
        }
      } else {
        ahlog::ah_info "LOW", "Row $irow: DETNAM is not supported for GTI, skipping. (DETNAM=$detnam_row)";
        $num_bad_detnam++;
        next;
      }
      # end if CCD_ID = 0,1
    } elsif( $ccd_id_row==2 or $ccd_id_row==3 ) {
      # Check CCD_ID = 2 or CCD_ID = 3
      #if( uc $detnam_row ne "CCD" or 
      if( uc $detnam_row ne "CCD" and
          uc $detnam_row ne "CCD34" ) {
        # CCD_ID 2, 3 must be DETNAM = 'CCD' or 'CCD34'
        ahlog::ah_info "LOW", "Row $irow: DETNAM is not supported for GTI, skipping. (DETNAM=$detnam_row)";
        $num_bad_detnam++;
        next;
      } elsif ( uc $datamode_row ne "WINDOW1" ) {
        # CCD_ID 2, 3 must be DATAMODE = 'WINDOW1'
        ahlog::ah_info "LOW", "Row $irow: DATAMODE is not supported for GTI, skipping. (DATAMODE=$datamode_row)";
        $num_bad_datamode++;
        next;
      } else {
        # Row is a supported DATACTLM
        ahlog::ah_info "LOW", "Row $irow: Found DATACTLM $datactlm_row";
      }
    } else {
      # CCD_ID is wrong
      ahlog::ah_info "LOW", "Row $irow: CCD_ID is not supported for GTI, skipping. (CCD_ID=$ccd_id_row)";
      $num_bad_ccdid++;
      next;
    } # end if CCD_ID

    # Figure out which GTI extension to use for the combination of
    # CCD_ID, SEGMENT, DATACTLM
    my $gtiext = -1; # $gtiext is the extension of the current GTI
    foreach my $gti_ii (1..$extnum) {
      next unless ($ccd_id_row == $GTI_ccdids{$gti_ii});
      next unless ($segment_row == $GTI_segments{$gti_ii});
      next unless ($datactlm_row eq $GTI_datactlms{$gti_ii});
      $gtiext = $gti_ii;
      last;
    }

    # If the GTI extension for this CCD_ID+SEGMENT+DATACTLM doesn't exist yet, create it
    if ( $gtiext == -1 ) {

      # increment number of GTI extensions
      $extnum++;

      # Calculate temporary header and datafile names
      # Remove header and data files if they exist
      my $hdfile="gti_CCD$ccd_id_row\_SEG$segment_row\_DATACTLM$datactlm_row\.hdr";
      my $dtfile="gti_CCD$ccd_id_row\_SEG$segment_row\_DATACTLM$datactlm_row\.dat";
      ahapp::add_temp_file($hdfile); unlink $hdfile;
      ahapp::add_temp_file($dtfile); unlink $dtfile;

      $status = createGTIExtension($ccd_id_row,$segment_row,$datactlm_row,
                                   $detnam_row,$datamode_row,$hdfile);
      if($status) { 
        ahlog::ah_err "Row $irow: Could not create GTI extension for CCD_ID $ccd_id_row, SEGMENT $segment_row, DATACTLM $datactlm_row."; 
        return 1;
      }
      ahlog::ah_info "HIGH", "Created GTI extension for CCD_ID $ccd_id_row, SEGMENT $segment_row, DATACTLM $datactlm_row, ext $extnum.";

      # Populate the hash and arrays with the correct values for this GTI extension
      $gtiext = $extnum;
      $GTI_hdfiles{$gtiext} = $hdfile;
      $GTI_dtfiles{$gtiext} = $dtfile;
      $GTI_ccdids{$gtiext} = $ccd_id_row;
      $GTI_segments{$gtiext} = $segment_row;
      $GTI_datactlms{$gtiext} = $datactlm_row;
      $GTI_detnams{$gtiext} = $detnam_row;
      $GTI_datamodes{$gtiext} = $datamode_row;
      $GTI_numframes{$gtiext} = 0;
      # Set TSTART and TSTOP for this extension; TSTART will not be changed, TSTOP will be 
      # changed as we encounter new exposure rows.
      $GTI_tstart{$gtiext} = $time_row;
      $GTI_tstop{$gtiext} = $time_row;
    }

    # Get the timing  parameters for this DATAMODE
    $status = getTimingParams($datamode_row,
                              \$timedel,\$timtranb,\$timtrana,\$expdeadb,
                              \$expdeada,\$flushimb,\$lastdead,\$lastdel,
                              \$nomexpo,\$timepixr);
    if($status) { 
      ahlog::ah_err "Row $irow: Tried to get timing parameters for unrecognized DATAMODE $datamode_row."; 
      return 1;
    }

    # From ahtime.cxx, here is a description of how to translate frame timing parameters
    # for event data in a given mode.
    #   TIME in exposure file as at the beginning of a frame
    #   TIME in events file in the center of the TIMEDEL window as shown below
    # We need to set START and STOP to beginning and end of TIMEDEL window
    #
    # The SXI instrument operates in fixed-length time cycles given by the
    # keyword, NOMEXPO (i.e. 4s).  The total cycle is further divided into
    # subcycles based on the DATAMODE keyword.  A subcycle is divided in the
    # following way:
    #
    # [---EXPDEADB---|---TIMTRANB----|---FLUSHIMB----|====TIMEDEL====|---TIMTRANA----|---EXPDEADA---]
    #
    # where events are only recorded during the TIMEDEL interval.  While the
    # number of sub-cycles depends on the DATAMODE keyword, it can also be
    # calculated by dividing the total cycle time (NOMEXPO) by the subcycle
    # time illustrated above.  The time assigned to events is within the
    # TIMEDEL interval based on the TIMEPIXR keyword.  For TIMEPIXR=0.5, the
    # time is assigned at the halfway point in the interval as shown below:
    # 
    # [---EXPDEADB---|---TIMTRANB----|---FLUSHIMB----|====TIMEDEL====|---TIMTRANA----|---EXPDEADA---]
    # |----------------SUBCYCLE TIME SHIFT-------------------->
    #
    # The total time shift from the beginning of SEQ_START_TIME is the subcycle
    # time shift shown above plus the total time of all subcycles prior to the
    # active subcycle:
    #
    #  time shift = SUBCYCLE_SHIFT + (icycle * SUBCYCLE_TIME)
    #
    # where SUBCYCLE_SHIFT = EXPDEADB+TIMTRANB+FLUSHIMB+(TIMEPIXR*TIMEDEL), 
    #       SUBCYCLE_TIME = EXPDEADB+TIMTRANB+FLUSHIMB+TIMEDEL+TIMTRANA+EXPDEADA,
    # and icycle ranges from 0 to NEXP-1, where NEXP is the number of
    # nexposures in one complete cycle (e.g. 4s).
    #
    # The SUBCYCLE_SHIFT is constant for all subcycles except the last where
    # TIMEDEL and EXPDEADA can be extended.  For time assignment, the adjusted
    # EXPDEADA can be ignored since it occurs after the exposure window, but
    # an additional shift is needed based on the final TIMEDEL size:
    #
    #  ADJLASTTIME = TIMEPIXR * (LASTDEL - TIMEDEL)
    #
    # In the last subcycle, this value must be added to the time shift.
    #
    # TIMEDEL     / [s] Integration time
    # TIMTRANB    / [s] Transfer time before exposure
    # TIMTRANA    / [s] Transfer time after exposure
    # EXPDEADB    / [s] Deadtime before exposure
    # EXPDEADA    / [s] Deadtime after exposure
    # FLUSHIMB    / [s] Flush out time
    # LASTDEAD    / [s] Last Deadtime after exposure
    # LASTDEL     / [s] Last Integration time in exposure
    # NOMEXPO     / [s] period of seq start time
    # TIMEPIXR    / Bintime start=0 middle=0.5 end=1

    # for time assignment, the above keyword values can be reduced to the following:
    #  - subcycletime: total time of a single subcycle
    #  - num_windows: number of windows (subcycles) in full exposure cycle
    #  - extratime: extra time at the end of the last subcycle
    $subcycletime = $expdeadb+$timtranb+$flushimb+$timedel+$expdeada+$timtrana;
    if ( $lastdel > 0. ) {
      $extratime = ($lastdead-$expdeada)+($lastdel-$timedel);
    } else {
      $extratime = 0.;
    }
    
    # How many GTI windows per exposure frame?
    if( uc $datamode_row eq "WINDOW1" or
        uc $datamode_row eq "WINDOW1BURST" or
        uc $datamode_row eq "WINDOW1BURST2" ) {
        # in full window (with or without burst), there is 1 GTI
        # entry per exposure row
        $num_windows = 1;
    } elsif ( uc $datamode_row eq "WINDOW2" or
              uc $datamode_row eq "WINDOW2BURST" ) {
        # in 1/8 window (with or without burst), there are 8 GTI
        # entries per exposure row
        $num_windows = 8;
    }

    # Set START and STOP times for GTI extension
    for (my $gti_ii=0;$gti_ii<$num_windows;$gti_ii++) {
      # START of GTI entry should be at beginning of TIMEDEL window above
      $start_time[$gti_ii] = $time_row+$subcycletime*$gti_ii+$expdeadb+$timtranb+$flushimb;
      # STOP of GTI entry should be at end of TIMEDEL window above
      $stop_time[$gti_ii] = $start_time[$gti_ii] + $timedel;
    } # end for gti_ii loop 

    # Last window for 1/8 window has a slightly longer exposure time
    # Add it here
    $stop_time[$num_windows-1] += $extratime;

    # Write START STOP FRAMENUM columns to proper GTI file extension
    for (my $gti_ii=0;$gti_ii<$num_windows;$gti_ii++) {
      my $dtfile = $GTI_dtfiles{$gtiext};
      my $start = $start_time[$gti_ii];
      my $stop = $stop_time[$gti_ii];
      $status = writeGTIRow($dtfile,$start,$stop,$framenum_row);
      if($status) { 
        ahlog::ah_err "Row $irow: Could not write to GTI extension: $gtiext."; 
        return 1;
      }
    }

    # Set extension TSTOP to the TIME of this row plus the nominal frame exposure time
    $GTI_tstop{$gtiext} = $time_row + $nomexpo;
    
    # Increment the number of (good) frames hash for this GTI extension
    $GTI_numframes{$gtiext}++;

    # Keep track of how many rows in this TIME group
    if ( $time_row == $time_previous ) {
      # We're still in a new group of 8 segments
      # Increment the array that keeps track of which CCD_ID+SEGMENT
      # combination we have encountered in the group of 8
      $row_present[$ccd_id_row*2+$segment_row]++;
    } else {
      # We're in a new group of 8 segments
      if ( $time_previous == 0 ) {
        # We're at the first row, so move on
        $row_present[$ccd_id_row*2+$segment_row]++;
        $time_previous = $time_row;
        $framenum_previous = $framenum_row;
      } else {
        # We're done with the group, so check it
        # Check for each CCD_ID+SEGMENT combination to see if we have
        # 1 of each of the 8
        for (my $ii=0;$ii<8;$ii++) {
          if( $row_present[$ii] < 1 ) {
            my $ccd_id_frame = floor($ii/2);
            my $segment_frame = $ii%2;
            ahlog::ah_info "HIGH", "Exposure row for frame $framenum_previous, " .
                                   "CCD_ID $ccd_id_frame, SEGMENT $segment_frame " .
                                   "is missing.";
          } elsif ( $row_present[$ii] > 1 ) {
            my $ndups = $row_present[$ii];
            my $ccd_id_frame = floor($ii/2);
            my $segment_frame = $ii%2;
            ahlog::ah_info "HIGH", "Exposure row for frame $framenum_previous, " .
                                   "CCD_ID $ccd_id_frame, SEGMENT $segment_frame " .
                                   "is duplicated $ndups times.";
          }
        } # end CCD_ID+SEGMENT combo
        # Re-initialize row_present array to zeroes, then increment
        # the current CCD_ID+SEGMENT
        for (my $ii=0;$ii<8;$ii++) { $row_present[$ii]=0; }
        $row_present[$ccd_id_row*2+$segment_row]++;
        $time_previous = $time_row;
        $framenum_previous = $framenum_row;
      } # end if time_previous
    } # end if time
  } # end of row loop

  # We're done with the final group, so we have to check it here
  # Check for each CCD_ID+SEGMENT combination to see if we have
  # 1 of each of the 8
  for (my $ii=0;$ii<8;$ii++) {
    if ( $row_present[$ii] < 1 ) {
      my $ccd_id_frame = floor($ii/2);
      my $segment_frame = $ii%2;
      ahlog::ah_info "HIGH", "Exposure row for frame $framenum_previous, " .
                             "CCD_ID $ccd_id_frame, SEGMENT $segment_frame " .
                             "is missing.";
    } elsif ( $row_present[$ii] > 1 ) {
      my $ndups = $row_present[$ii];
      my $ccd_id_frame = floor($ii/2);
      my $segment_frame = $ii%2;
      ahlog::ah_info "HIGH", "Exposure row for frame $framenum_previous, " .
                             "CCD_ID $ccd_id_frame, SEGMENT $segment_frame " .
                             "is duplicated $ndups times.";
    }
  } # End CCD_ID+SEGMENT combo

  # End of loop through exposure

  # Write number of frames into header of each GTI extension and print it
  unless($extnum) { ahlog::ah_err "  No GTI extension found.\n"; return 1; }
  ahlog::ah_info "HIGH", "Found the following numbers of GTI extensions: $extnum";

  # Create the column description file
  my $cdfile="gti.cdf";
  ahapp::add_temp_file($cdfile);
  open CDFILE, ">", $cdfile;
  print CDFILE "START 1D s\n";
  print CDFILE "STOP 1D s\n";
  print CDFILE "FRAMENUM 1J\n";
  close CDFILE;

  my %gti_dclass;
  my %tstart_dclass;
  my %tstop_dclass;

  # Create  GTI file for each frame
  foreach my $gti_ii (1..$extnum) {
    my $gtifile = "gti_$gti_ii\.tmp";
    my $hdfile = $GTI_hdfiles{$gti_ii};
    my $dtfile = $GTI_dtfiles{$gti_ii};
    my $numframes = $GTI_numframes{$gti_ii};
    my $tstart_gti = $GTI_tstart{$gti_ii};
    my $tstop_gti = $GTI_tstop{$gti_ii};
    my $datactlm = $GTI_datactlms{$gti_ii};
    my $ccd_id = $GTI_ccdids{$gti_ii};
    my $segment = $GTI_segments{$gti_ii};
    my $extname = "GTI_SEG" . (2*$ccd_id+$segment) . $datactlm;

    ahapp::add_temp_file($gtifile);

    # Add the NUMFRAMES keyword to the header file
    open HDFILE, ">>", $hdfile;
    print HDFILE "NUMFRAMS = $numframes\n";
    close HDFILE;

    # Run ftcreate and append to the output file
    $status = ahgen::run_ftool("ftcreate","cdfile=$cdfile","datafile=$dtfile",
                               "outfile=$gtifile","headfile=$hdfile","extname=$extname",
                               "clobber=yes");
    if($status) { ahlog::ah_err "Error running ftcreate for GTI $gti_ii"; return $status; }

    # Copy keywords from the primary extension to the output GTI extension
    # This is likely to change. 
    # Need to watch out for overwriting the DETNAM and DATAMODE keywords
    $status = ahfilterlib::copy_keywords((parse_file_name($infile))[0],"0",$gtifile,$extname,"GTI","all");
    if($status) { ahlog::ah_err "Error copying keywords from $infile to $gti_ii"; return $status; }

    # Update the TSTART and TSTOP keyword values
    # Have to do this after copying the keywords from primary header
    $status = ahgen::set_keyword($gtifile,$extname,"TSTART",$tstart_gti);
    if($status) { ahlog::ah_err "Error updating TSTART keyword for GTI $gti_ii"; return $status; }
    $status = ahgen::set_keyword($gtifile,$extname,"TSTOP",$tstop_gti);
    if($status) { ahlog::ah_err "Error updating TSTOP keyword for GTI $gti_ii"; return $status; }

    # Write the sximodegti parameters to the output GTI file
    ahapp::write_parameters ($gtifile,$extname) ;                 

    # Copy the new GTI file to the output file
    $status = ahgen::copy_hdu($gtifile,$extname,$outfile);
    if($status) { ahlog::ah_err "Error copying HDU for GTI $gti_ii to $outfile"; return $status; }

    ahlog::ah_info "HIGH", "  Ext $gti_ii: $numframes frames found.";
    ahlog::ah_info "LOW", "  Ext $gti_ii: TSTART = $tstart_gti.";
    ahlog::ah_info "LOW", "  Ext $gti_ii: TSTOP  = $tstop_gti.";

    # Create an array to push GTI files into for merging
    unless ( exists $gti_dclass{$datactlm} ) { $gti_dclass{$datactlm} = (); }
    unless ( exists $tstart_dclass{$datactlm} ) { $tstart_dclass{$datactlm} = $tstart_gti; }
    unless ( exists $tstop_dclass{$datactlm} ) { $tstop_dclass{$datactlm} = $tstop_gti; }
    push @{$gti_dclass{$datactlm}}, $gtifile;
    if( $tstart_dclass{$datactlm} > $tstart_gti ) { $tstart_dclass{$datactlm} = $tstart_gti; }
    if( $tstop_dclass{$datactlm} < $tstop_gti ) { $tstop_dclass{$datactlm} = $tstop_gti; }

  }
  
  # Create a keyfile to not copy the CCD_ID and SEGMENT keywords
  my $keyfil = "keyfil.txt";
  open KYFILE, ">", $keyfil;
  print KYFILE "!CCD_ID\n";
  print KYFILE "!SEGMENT\n";
  print KYFILE "!EXTNAME\n";
  close KYFILE;
  ahapp::add_temp_file($keyfil);
  
  # Merge the GTI files
  foreach my $datactlm ( sort keys %gti_dclass ) {
    my %mgtime_pars;
    my $gtifile = "merge_DATACTLM$datactlm.gti";
    my $tstart_gti = $tstart_dclass{$datactlm};
    my $tstop_gti = $tstop_dclass{$datactlm};
    my @gtifiles = @{$gti_dclass{$datactlm}};
    my $extname = "GTI_$datactlm";
    ahapp::add_temp_file($gtifile);

    # Increment the output numdclass parameter
    $numdclass++;

    # Merge GTI files with matching DATACLAS
    if( ahfilterlib::merge_gti(\@gtifiles,$gtifile,"AND",$extname,\%mgtime_pars) ) {
      ahlog::ah_err "Error merging GTI files" ;
      return ahgen::get_error_flag;
    } 

    # Copy keywords from the first GTI file in the datactlm.
    # Do not copy the CCD_ID or SEGMENT
    $status = ahgen::run_ftool("cphead",$gtifiles[0]."[1]",$gtifile."[$extname]","keyfil=$keyfil","comment=no","history=no");
    if($status) { ahlog::ah_err "Error copying keywords from $gtifile to $mergefile"; return $status; }

    # Have to do this after copying the keywords from primary header
    $status = ahgen::set_keyword($gtifile,$extname,"TSTART",$tstart_gti);
    if($status) { ahlog::ah_err "Error updating TSTART keyword for GTI $gtifile"; return $status; }
    $status = ahgen::set_keyword($gtifile,$extname,"TSTOP",$tstop_gti);
    if($status) { ahlog::ah_err "Error updating TSTOP keyword for GTI $gtifile"; return $status; }

    # Copy the new GTI file to the output file
    $status = ahgen::copy_hdu($gtifile,$extname,$mergefile);
    if($status) { ahlog::ah_err "Error copying HDU for GTI $gtifile to $mergefile"; return $status; }

  }

  # Print the counters to the log file
  ahlog::ah_info "LOW", "Number of datactlmes            : $numdclass";
  ahlog::ah_info "LOW", "Number of frames excluded:";
  ahlog::ah_info "LOW", "  With TIME = NULL              : $num_bad_time";
  ahlog::ah_info "LOW", "  With CCD_ID out of range      : $num_bad_ccdid";
  ahlog::ah_info "LOW", "  With SEGMENT out of range     : $num_bad_segment";
  ahlog::ah_info "LOW", "  With FRAMETYPE not events     : $num_bad_frametype";
  ahlog::ah_info "LOW", "  With DETNAM not supported     : $num_bad_detnam";
  ahlog::ah_info "LOW", "  With DATAMODE not supported   : $num_bad_datamode";

  return 0;

} 

# ------------------------------------------------------------------------------

sub finalize {

  my $status = 0;

  # Verify output file
  # Files will fail verification due to having all extensions named GTI
  $status = update_checksum_and_verify($outfile);
  unless ($status) { ahlog::ah_err "File $outfile failed ftverify"; return $status; }
  $status = update_checksum_and_verify($mergefile);
  unless ($status) { ahlog::ah_err "File $mergefile failed ftverify"; return $status; }

  # Set the output parameter numdclass
  ahgen::run_ftool("pset","sximodegti","numdclass=$numdclass");

  return 0;
  
}

# ------------------------------------------------------------------------------

# 
# Description: 
#
# Create a GTI Header file in the output GTI file for the combination of:
# CCD_ID, SEGMENT, DETNAM, DATAMODE, ADOU0ENA
#
# Parameters:
# [in] ccd_id      (0,1,2,3)
# [in] segment     (0: AB, 1: CD (Segment ID))
# [in] datactlm    DataClass identifier 
# [in] detnam      Detector subsystem (CCD, CCD12, CCD34)
# [in] datamode    (WINDOW1, WINDOW1BURST, WINDOW2, WINDOW2BURST)
# [in] hdfile      File of header keywords 
# [out] status     Return 0 for successful creation of merged event file
#
sub createGTIExtension ($$$$$$$) {

  my $ccd_id = shift;
  my $segment = shift;
  my $datactlm = shift;
  my $detnam = shift;
  my $datamode = shift;
  my $hdfile = shift;

  my $timedel=0;
  my $timtranb=0;
  my $timtrana=0;
  my $expdeadb=0;
  my $expdeada=0;
  my $flushimb=0;
  my $lastdead=0;
  my $lastdel=0;
  my $nomexpo=0;
  my $timepixr=0;

  my $status = 0;

  # Get the timing parameters so we can write them to keywords
  $status = getTimingParams($datamode,
                            \$timedel,\$timtranb,\$timtrana,\$expdeadb,
                            \$expdeada,\$flushimb,\$lastdead,\$lastdel,
                            \$nomexpo,\$timepixr);
  if($status) { 
    ahlog::ah_err "createGTIExtension: Tried to get timing parameters for unrecognized DATAMODE $datamode."; 
    return 1;
  }

  # Write the keywords to the header description file
  open HDFILE, ">", $hdfile;
  print HDFILE "HDUCLASS = OGIP / format conforms to OGIP/GSFC conventions\n";
  print HDFILE "HDUCLAS1 = GTI / Extension contains Good Time Intervals\n";
  print HDFILE "CCD_ID = $ccd_id / 0: CCD1, 1: CCD2, 2: CCD3, 3: CCD4\n";
  print HDFILE "SEGMENT = $segment / 0: AB, 1: CD (Segment ID)\n";
  print HDFILE "DETNAM = $detnam / CCD:all, CCD12: CCD1+2, CCD34: CCD3+4\n";
  print HDFILE "DATAMODE = $datamode / (WINDOW1, WINDOW1BURST, WINDOW2, WINDOW2BURST)\n";
  print HDFILE "DATACLAS = '$datactlm' / Code to ID all instrument settings\n";
  print HDFILE "TIMEDEL = $timedel / [s] Integration time\n";
  print HDFILE "TIMTRANB = $timtranb / [s] Transfer time before exposure\n";
  print HDFILE "TIMTRANA = $timtrana / [s] Transfer time after exposure\n";
  print HDFILE "EXPDEADB = $expdeadb / [s] Deadtime before exposure\n";
  print HDFILE "EXPDEADA = $expdeada / [s] Deadtime after exposure\n";
  print HDFILE "FLUSHIMB = $flushimb / [s] Flush out time\n";
  print HDFILE "LASTDEAD = $lastdead / [s] Last Deadtime after exposure\n";
  print HDFILE "LASTDEL = $lastdel / [s] Last Integration time in exposure\n";
  print HDFILE "NOMEXPO = $nomexpo / [s] period of seq start time\n";
  print HDFILE "TIMEPIXR = $timepixr / Bintime start=0 middle=0.5 end=1\n";
  close HDFILE;

  return 0;

}

# ------------------------------------------------------------------------------

# 
# Description: 
#
# Return the frame timing parameters for a given DATAMODE
#
# Parameters:
# [in]  datamode    Datamode for specified row
# [out] timedel     Integration time [s]
# [out] timtranb    Transfer time before exposure [s]
# [out] timtrana    Transfer time after exposure [s]
# [out] expdeadb    Deadtime before exposure [s]
# [out] expdeada    Deadtime after exposure [s]
# [out] flushimb    Flush out time [s]
# [out] lastdead    Last deadtime after exposure [s]
# [out] lastdel     Last integration time in exposure [s]
# [out] nomexpo     Period of seq start time [s]
# [out] timpixr     Bin time start=0 middle=0.5 end=1
# [out] status      Return 0 for success
#
sub getTimingParams ($$$$$$$$$$$) {
  
  my $datamode = shift;
  my $timedel  = shift;
  my $timtranb = shift;
  my $timtrana = shift;
  my $expdeadb = shift;
  my $expdeada = shift;
  my $flushimb = shift;
  my $lastdead = shift;
  my $lastdel  = shift;
  my $nomexpo  = shift;
  my $timpixr  = shift;

  my $status = 0;

  # 2015-11-04: Values were taken from Hayashida-san table, given in
  # v0.26 of SXI instrument document.
  # Note that TIMEPIXR should be 0., not 0.5 as initially written to the FFF files
  if( uc $datamode eq "WINDOW1" ) {
    $$timedel  = 3.963136;
    $$timtranb = 0.036864;
    $$timtrana = 0.;
    $$expdeadb = 0.;
    $$expdeada = 0.;
    $$flushimb = 0.;
    $$lastdead = 0.;
    $$lastdel  = 0.;
    $$nomexpo  = 4.;
    $$timpixr  = 0.;
  } elsif( uc $datamode eq "WINDOW1BURST" ) {
    $$timedel  = 1.9395648;
    $$timtranb = 0.;
    $$timtrana = 0.036864;
    $$expdeadb = 1.9865664;
    $$expdeada = 0.0001408;
    $$flushimb = 0.036864;
    $$lastdead = 0.;
    $$lastdel  = 0.;
    $$nomexpo  = 4.;
    $$timpixr  = 0.;
  } elsif( uc $datamode eq "WINDOW2" ) {
    $$timedel  = 0.4631184;
    $$timtranb = 0.036864;
    $$timtrana = 0.;
    $$expdeadb = 0.;
    $$expdeada = 0.;
    $$flushimb = 0.;
    $$lastdead = 0.;
    $$lastdel  = 0.4632592;
    $$nomexpo  = 4.;
    $$timpixr  = 0.;
  } elsif( uc $datamode eq "WINDOW2BURST" ) {
    $$timedel  = 0.0605952;
    $$timtranb = 0.;
    $$timtrana = 0.036864;
    $$expdeadb = 0.3633408;
    $$expdeada = 0.0023184;
    $$flushimb = 0.036864;
    $$lastdead = 0.0024592;
    $$lastdel  = 0.0605952;
    $$nomexpo  = 4.;
    $$timpixr  = 0.;
  } elsif( uc $datamode eq "WINDOW1BURST2" ) {
    $$timedel  = 0.0605952;
    $$timtranb = 0.;
    $$timtrana = 0.036864;
    $$expdeadb = 3.8655360;
    $$expdeada = 0.0001408;
    $$flushimb = 0.036864;
    $$lastdead = 0.;
    $$lastdel  = 0.;
    $$nomexpo  = 4.;
    $$timpixr  = 0.;
  } else {
    # unrecognized DATAMODE
    return 1;
  }

  return 0;

}

# ------------------------------------------------------------------------------

# 
# Description: 
#
# Write to the data description file 
#
# Parameters:
# [in] dtfile      ASCII file to write the row information to
# [in] start       START column value
# [in] stop        STOP column value
# [in] framenum    FRAMENUM column value
# [out] status     Return 0 for successful creation of merged event file
#
sub writeGTIRow ($$$$) {

  my $dtfile = shift;
  my $start = shift;
  my $stop = shift;
  my $framenum = shift;

  # Print to the data file
  open DTFILE, ">>", $dtfile;
  print DTFILE "$start $stop $framenum\n";
  close DTFILE;

  return 0;
}

# ------------------------------------------------------------------------------
#
# $Log: sximodegti.pl,v $
# Revision 1.18  2016/07/28 19:17:42  asargent
# TIMEDEL and EXPDEADB were reversed from what they should have been for Crab. Swapped values for timedel and expdeadb when processing crab data.
#
# Revision 1.17  2016/07/14 15:38:54  asargent
# Updated sximodegti to reflect TRF dated 2016-07-12: added new DATAMODE=WINDOW1BURST2 for the SXI Crab data, which were taken in full window + 0.1 sec burst mode, an unsupported 'diagnostic' mode
#
# Revision 1.16  2016/04/19 16:46:05  klrutkow
# changed DATACLASS to DATACTLM column (but include capability to still work with DATACLASS)
#
# Revision 1.15  2016/03/28 06:54:09  klrutkow
# removed area discrimination dependence
#
# Revision 1.14  2016/03/24 18:03:32  mdutka
# addressing items listed in issue #610
#
# Revision 1.13  2016/01/28 14:58:14  asargent
# Sorted keys alphabetically to maintain order in the output files.
#
# Revision 1.12  2015/12/31 22:28:39  asargent
# Removed re-writing of GTI extension name from sximodegti if there is only one GTI file to be morged (this is now done in the ahfilterlib module)
#
# Revision 1.11  2015/12/09 16:05:10  asargent
# Use exposure file TSTART/TSTOP if parameter tstart/tstop are out of range.
#
# Revision 1.10  2015/12/04 22:14:55  asargent
# Updated output file extension names. Added HDUCLASS and HDUCLAS1 to header keyword list.
#
# Revision 1.9  2015/12/04 20:02:19  asargent
# Remove copying of ADOU0ENA keyword from condensed GTI file.
#
# Revision 1.8  2015/12/04 15:21:11  asargent
# Updated cphead keyfil to not append
#
# Revision 1.7  2015/12/04 03:08:10  asargent
# Added new output parameter counting total number of gti dataclasses. Merged dataclasses into single gti
#
# Revision 1.6  2015/11/24 15:25:56  asargent
# Update TSTART/TSTOP to be the first start/last stop in the GTI extension.
#
#
