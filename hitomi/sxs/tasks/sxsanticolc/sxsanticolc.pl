#!/usr/bin/perl
#
# File name: sxsanticolc.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/10/19 16:53:24 $
# Version: 0
#
# Create a gti file and run the extractor tool to create an SXS
# lightcurve and spectrum file for antico events
#
# Tool Dependencies:
# 
# Library Dependencies:
#   gen/lib/perl/ahlog
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
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

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

my $infile     ="";         # Input event file
my $outroot    ="";         # Root of output file names
my $extract    ="yes";      # Extract spectrum ([yes],no)
my $bintime    = 1;         # Time bin in seconds (d/f= 1 sec)
my $antpsp     ="A";        # Antico PSP to use in light curve (A=PSPA B=PSPB)
my $expr       ="";         # Expression for event columns seslection (or NONE)
my $gtifile    ="";         # Input GTI file
my $numlc      = 1;         # Number of lightcurve to output (d/f=1)
my $picut      ="0-12200";  # PI ranges to create the lightcurve(s) (ex. 0-200,201-1000, etc.)
my $lcstart    =-1;         # Force the lightcurve to start at specific MET time (if -1 use the 1st start GTI)
my $lcstop     =-1;         # Force the lightcurve to stop at specific MET time (if -1 use the last stop GTI)
my $lcthresh   = 1;         # Lightcurve exposure threshold
my $lctzero    ="no";       # Set the TIMEZERO to the first bin of the lightcurve (yes/[no])

my $lcfile     ="";          # output light curve file
my $phafile    ="";          # output PHA file if extract = yes

my $telescop   ="HITOMI";
my $instrume   ="SXS";

my $sxsanticolcerror = 0;  # sxsanticolc exit status

# Miscellaneous parameters
my $filter_expr = "";
my $mergegti = "tmp_sxsanticolc_all.gti";
my $gtilc    = 'tmp_sxsevt.gti';
my $ant_pspa = "(PSP_ID==0||PSP_ID==1)";
my $ant_pspb = "(PSP_ID==2||PSP_ID==3)";
my @pi_expr  = (); # Array to store expressions of PI ranges
my @pi_start = (); # Array to store PI start values
my @pi_stop  = (); # Array to store PI stop values 

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

$sxsanticolcerror = get_parameters () ;
unless ( $sxsanticolcerror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($sxsanticolcerror);
}

$sxsanticolcerror = initialize () ;
unless ( $sxsanticolcerror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($sxsanticolcerror);
}

$sxsanticolcerror = do_work () ;
unless ( $sxsanticolcerror == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($sxsanticolcerror);
}

$sxsanticolcerror = finalize () ;
unless ( $sxsanticolcerror == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($sxsanticolcerror);
}

# We're done.
ahapp::end_processing($sxsanticolcerror);


#########################
# Subroutines
#########################

sub get_parameters {

  $infile     = ahapp::query_parameter("infile");
  $outroot    = ahapp::query_parameter("outroot");
  $extract    = ahapp::query_parameter("extract",1);
  $bintime    = ahapp::query_parameter("bintime");
  $antpsp     = ahapp::query_parameter("antpsp");
  $expr       = ahapp::query_parameter("expr");
  $gtifile    = ahapp::query_parameter("gtifile");
  $numlc      = ahapp::query_parameter("numlc");
  $picut      = ahapp::query_parameter("picut");
  $lcstart    = ahapp::query_parameter("lcstart");
  $lcstop     = ahapp::query_parameter("lcstop");
  $lcthresh   = ahapp::query_parameter("lcthresh");
  $lctzero    = ahapp::query_parameter("lctzero");


  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status = 0;

  my $nrows   = 0;
  my $nrows_a = 0;
  my $nrows_b = 0;
  my @pi_ranges= ();
  
  # Input file checking
  if (isRequiredFileNotFound($infile)) { return 1; }
  if (isOptionalFileNotFound($gtifile)) { return 1; }

  # Set up the expression for filtering
  if ( uc $expr eq "NONE" ) {
    ahlog::ah_out "No event selection specified."; 
    ahlog::ah_out "Using all events to generate the lightcurves";
  } else {
    $filter_expr = $expr;
  }

  if ( uc $antpsp ne "NONE" ) {
    # Check for a valid PSP
    if ( uc $antpsp ne "A" and uc $antpsp ne "B" ) {
      ahlog:ah_err "invalid value for antpsp parameter; should be A or B";
      return 1;
    }

    # Check that we have events in the antico event file
    # and determine whether the given 'antpsp' is valid
    $nrows   = ahgen::get_keyword($infile, "EVENTS","NAXIS2");
    if ( $nrows == 0 ) {
      ahlog::ah_err "No events in file $infile";
      return 1;
    }
    $nrows_a = ahgen::get_keyword($infile, "EVENTS][PSP_ID=0:1","NAXIS2");
    $nrows_b = ahgen::get_keyword($infile, "EVENTS][PSP_ID=2:3","NAXIS2");
    if ( $nrows_a == 0 and $nrows_b == 0 ) {
      ahlog::ah_err "None of the expected values ($ant_pspa or $ant_pspb) are in the PSP_ID column";
      return 1;
    }
    if ( uc $antpsp eq "A" and $nrows_a == 0 ) {
      ahlog::ah_out "Event file does not contain events from PSPA ($ant_pspa)";
      ahlog::ah_out "Using PSPB ($ant_pspb)";
      $antpsp = "B";
    }
    if ( uc $antpsp eq "B" and $nrows_b == 0 ) {
      ahlog::ah_out "Event file does not contain events from PSPB ($ant_pspb)";
      ahlog::ah_out "Using PSPA ($ant_pspa)";
      $antpsp = "A";
    }

    # Add the PSP_ID expression to the expression for filtering
    if ( uc $antpsp eq "A" ) {
      ahlog::ah_out "Using SXS PSP A events ($ant_pspa)";
      if ( $filter_expr ) {
        $filter_expr .= "&&$ant_pspa";
      } else {
        $filter_expr = "$ant_pspa";
      }
    } elsif (uc $antpsp eq "B" ) {
      ahlog::ah_out "Using SXS PSP B events ($ant_pspa)";
      if ( $filter_expr ) {
        $filter_expr .= "&&$ant_pspa";
      } else {
        $filter_expr = "$ant_pspa";
      }
    } else {
      ahlog::ah_out "Using all SXS PSP A and PSP B events";
    }
    ahlog::ah_debug "PSP   : $antpsp";
    ahlog::ah_debug "NROWS : $nrows";
    ahlog::ah_debug "NROWSA: $nrows_a";
    ahlog::ah_debug "NROWSB: $nrows_b";
    ahlog::ah_debug "FILTER: $filter_expr";
  }

  my @gtifiles = ();
  my $evt_start= 0;
  my $evt_stop = 0;
  # Set up GTI expression and set up the time ranges
  if ( $lcstart < 0 ) {
    # read TSTART from event file
    $evt_start = ahgen::get_keyword($infile,"EVENTS","TSTART");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "TSTART keyword not defined in $infile"; return ahgen::get_error_flag;
    }
  } else {
    # Offset the start of GTI a bin include a full bin at lcstart
    $evt_start = $lcstart - $bintime;
  }
  # Update the lcstart to start in the middle of the first bin
  $lcstart = $evt_start + ($bintime/2);

  if ( $lcstop < 0 ) {
    # read TSTOP from event file
    $evt_stop = ahgen::get_keyword($infile,"EVENTS","TSTOP");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "TSTART keyword not defined in $infile"; return ahgen::get_error_flag;
    }
  } else {
    # Offset the stop of GTI a bin include a full bin at lcstop
    $evt_stop = $lcstop + $bintime;
  }
  ahlog::ah_out "Creating lightcurve(s) with bintime=${bintime}s between $evt_start and $evt_stop.";

  # Make GTI file based on lcstart, lcstop
  # Run ftcreate to create GTI based on TSTART/TSTOP
  # Create the header template to a file.
  my($header_template) = 'gti_header_template.tmp';
  my($data_template)   = 'gti_data_template.tmp';
  ahapp::add_temp_file($gtilc);
  ahapp::add_temp_file($header_template);
  ahapp::add_temp_file($data_template);
  open HEADER, ">$header_template";
  print HEADER "START               1D s\n";
  print HEADER "STOP                1D s";
  close HEADER;

  # Print the data template to a file.
  open DATA, ">$data_template";
  print DATA "$evt_start $evt_stop";
  close DATA;

  $status = ahgen::run_ftool('ftcreate',"cdfile=$header_template",
          "datafile=$data_template","outfile=$gtilc",
          "headfile= ","tabtyp=binary",
          "nskip=0","nrows=0","morehdr=0","extname=GTI",
          "anull= ","inull=0","clobber=yes");
  if ($status) { ahlog::ah_err "Could not create GTI."; return $status; }
  push @gtifiles, $gtilc . "[GTI]";

  if ( uc $gtifile ne "NONE" ) {
    push @gtifiles, $gtifile;
  }
  if ( ahgen::check_hdu_exists( $infile, "GTI" ) ) {
    push @gtifiles, $infile . "[GTI]";
  }
  # Merge the input GTI files to create the gti for filtering
  ahapp::add_temp_file($mergegti);
  ahfilterlib::merge_gti(\@gtifiles,$mergegti,"AND","GTI",{});

  # Set up the PI ranges
  @pi_ranges = split ",", $picut;
  if ( @pi_ranges != $numlc or $numlc == 0) {
    ahlog::ah_err "Number of requested lightcurves do not correspond to the number of ranges";
    return 1;
  }
  foreach my $range ( @pi_ranges ) {
    # Search for a dash (-) or colon (:) delimiter for a PI range
    my @range_split  = split /[-:]/, $range;
    if ( @range_split == 2 ) { # require that there only be two values per range
      push @pi_expr , "PI = " . $range_split[0]. ": " . $range_split[1];
      push @pi_start, $range_split[0];
      push @pi_stop , $range_split[1];
    } else {
      ahlog::ah_err " $range not a valid picut range. Use ':' or '-' to signify PI ranges";
      return 1;
    }

  }
  ahlog::ah_out "Found $numlc PI Ranges:";
  ahlog::ah_out "  " . $_ foreach @pi_expr;

  # set up output file names for the lightcurve and the spectrum
  $lcfile = $outroot . ".lc";
  $phafile = $outroot . ".pha";

  # Check for clobber of output files
  if (removeOutputFileIfClobbering($lcfile ,$ahapp::clobber) ) { return 1; }
  if (removeOutputFileIfClobbering($phafile,$ahapp::clobber) && $extract) { return 1; }

  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;                 

  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $status = 0;

  # Cut the event file for GTI using:
  # 1. GTI between lcstart -> lcstop
  # 2. GTI attached to the event file
  # 3. GTI given in the gtifile parameter
  # This GTI cut strictly filters events and is not the GTI used for extractor
  ahlog::ah_out "Filtering events outside of GTI";
  my $evtfile = "tmp_sxsanticolc.evt";
  ahapp::add_temp_file ($evtfile);
  if( removeOutputFileIfClobbering($evtfile ,$ahapp::clobber) ) { return 1; }
  if( ahgen::run_ftool("ftselect",$infile,$evtfile,"gtifilter(\"$mergegti\")","copyall=no") ) { return 1; }

  if ( $filter_expr ) {
    # Clean the event file based the expression parameter
    ahlog::ah_out "Filtering events with expression $filter_expr";
    my $outfile = "tmp_sxsanticolc_expr.evt";
    ahapp::add_temp_file ($outfile);
    if(removeOutputFileIfClobbering($outfile ,$ahapp::clobber) ) { return 1; }
    if(ahgen::run_ftool("ftselect",$evtfile,$outfile,$filter_expr)) { return 1; }
    if(ahgen::copy_fits_file($outfile ."[EVENTS]",$evtfile,"copyall=yes")) { return 1; }
  }

  # Append the GTI for the lightcurve. Append this GTI in order
  # to force the lightcurve to start and end at lcstart and lcstop
  # and include all bins between lcstart and lcstop
  if( ahgen::run_ftool("ftappend",$gtilc."[GTI]",$evtfile) ) { return 1; }

  # Loop over PI ranges
  foreach my $ii  ( 0 .. $numlc - 1 ) {
    my $pi_range = $pi_expr[$ii];
    my $pi_start = $pi_start[$ii];
    my $pi_stop  = $pi_stop[$ii];
    my $rate_out = "RATE"  . $pi_start . "_" . $pi_stop;
    my $error_out= "ERROR" . $pi_start . "_" . $pi_stop;
    my $outlc    = $outroot . "_" . $ii . ".lc";
    ahapp::add_temp_file($outlc);

    ahlog::ah_out "Extracting antico lightcurve with $pi_range";

    # Create the antico PI filtered lightcurve
    $status=ahgen::run_ftool("extractor",
                              "exitnow=no",
                              "filename=$evtfile\[$pi_range]",
                              "eventsout=NONE",
                              "imgfile=NONE",
                              "binf=1",
                              "fullimage=yes",
                              "phafile=NONE",
                              "specbin=1",
                              "wtmapb=no",
                              "wtmapfix=yes",
                              "swmapx=no",
                              "swmapy=no",
                              "binh=1",
                              "wmapver=2",
                              "fitsbinlc=$outlc",
                              "qdpfile=NONE",
                              "binlc=$bintime",
                              "lcthresh=$lcthresh",
                              "lcstart=$lcstart",
                              "lcthwarn=3.0",
                              "lctzero=$lctzero",
                              "unbinlc=NONE",
                              "regionfile=NONE",
                              "timefile=NONE",
                              "adjustgti=no",
                              "gtinam=NONE",
                              "xcolf=NONE",
                              "ycolf=NONE",
                              "zcolf=NONE",
                              "xint=1.0",
                              "yint=1.0",
                              "tcol=TIME",
                              "ecol=PI",
                              "ccol=NONE",
                              "gcol=ITYPE",
                              "gstring=NONE",
                              "xcolh=NONE",
                              "ycolh=NONE",
                              "gtitxt=NONE",
                              "xronwn=NONE",
                              "events=EVENTS",
                              "gti=GTI",
                              "timeorder=no",
                              "timeref=40000.0",
                              "eventkey=NONE",
                              "phamax=TLMAX",
                              "xfkey=TLMAX",
                              "yfkey=TLMAX",
                              "xhkey=TLMAX",
                              "yhkey=TLMAX",
                              "copyall=yes",
                              "clobber=yes",
                            );
      if ( $status ) {
        ahlog::ah_err "extractor failed.";
        return $status;
      }

      # Make the output file
      if ( $ii == 0 ) {
        # Copy the lightcurve TIME, RATE and ERROR columns to the output file
        if(ahgen::copy_fits_file($outlc ."[RATE][col TIME;RATE;ERROR]",$lcfile,"copyall=yes")) { return 1; }
      } else {
        # Paste just the RATE and ERROR columns to the output file
        $status = ahgen::run_ftool("ftpaste",
                                    "infile=$lcfile",
                                    "pastefile=$outlc\[RATE][col RATE; ERROR]",
                                    "outfile=$lcfile",
                                    "copyall=yes",
                                    "clobber=yes");
        if ( $status ) {
          ahlog::ah_err "ftpaste failed.";
          return $status;
        }
      }

      # Rename RATE and ERROR columns.  The columns were originally renamed
      # using extended syntax in the copy_fits_file/ftpaste calls above, but
      # the TUNIT keywords were not being copied.  By renaming the columns
      # afterwards, the TUNIT keywords are retained with the correct values.
      changeColumnName($lcfile,"RATE","RATE",$rate_out);
      changeColumnName($lcfile,"RATE","ERROR",$error_out);

  }

  if ( $extract ) {
    ahlog::ah_out "Extracting antico spectrum with PI=0:12200";
    # Create the grade and region filtered lightcurve and spectra
    $status=ahgen::run_ftool("extractor",
                             "exitnow=no",
                             "filename=$evtfile\[PI=0:12200]",
                             "eventsout=NONE",
                             "imgfile=NONE",
                             "binf=1",
                             "fullimage=yes",
                             "phafile=$phafile",
                             "specbin=1",
                             "wtmapb=no",
                             "wtmapfix=yes",
                             "swmapx=no",
                             "swmapy=no",
                             "binh=1",
                             "wmapver=2",
                             "fitsbinlc=NONE",
                             "qdpfile=NONE",
                             "binlc=$bintime",
                             "lcthresh=$lcthresh",
                             "lcstart=$lcstart",
                             "lcthwarn=3.0",
                             "lctzero=yes",
                             "unbinlc=NONE",
                             "regionfile=NONE",
                             "timefile=NONE",
                             "gtinam=GTI",
                             "xcolf=NONE",
                             "ycolf=NONE",
                             "zcolf=NONE",
                             "xint=1.0",
                             "yint=1.0",
                             "tcol=TIME",
                             "ecol=PI",
                             "ccol=NONE",
                             "gcol=ITYPE",
                             "gstring=NONE",
                             "xcolh=NONE",
                             "ycolh=NONE",
                             "gtitxt=NONE",
                             "xronwn=NONE",
                             "events=EVENTS",
                             "gti=GTI",
                             "timeorder=no",
                             "timeref=40000.0",
                             "eventkey=NONE",
                             "phamax=TLMAX",
                             "xfkey=TLMAX",
                             "yfkey=TLMAX",
                             "xhkey=TLMAX",
                             "yhkey=TLMAX",
                             "copyall=yes",
                             "clobber=yes",
                            );
    if ( $status ) {
      ahlog::ah_err "extractor failed.";
      return $status;
    }
  }

  return 0;

}

# ------------------------------------------------------------------------------

sub finalize {

  my $status = 0;

  ahlog::ah_out "Verifying output files";

  ## Replace the USER keyword to avoid unit test issues...
  $status = ahgen::run_ftool("fthedit",$lcfile . "[PRIMARY]","USER","a","sxsanticolc");
  if($status) { ahlog::ah_err "Could not create lightcurve for file $lcfile"; return $status; }
  $status = ahgen::run_ftool("fthedit",$lcfile . "[RATE]","USER","a","sxsanticolc");
  if($status) { ahlog::ah_err "Could not create lightcurve for file $lcfile"; return $status; }
  $status = ahgen::run_ftool("fthedit",$lcfile . "[GTI]","USER","a","sxsanticolc");
  if($status) { ahlog::ah_err "Could not create lightcurve for file $lcfile"; return $status; }

  # Update checksum/datasum and verify the output files
  $status = update_checksum_and_verify($lcfile);
  unless ( $status ) { ahlog::ah_err "verify failed for $lcfile"; return $status; }
  
  if ( $extract ) {
      # Replace the USER keyword to avoid unit test issues...
  $status = ahgen::run_ftool("fthedit",$phafile . "[PRIMARY]","USER","a","sxsanticolc");
  if($status) { ahlog::ah_err "Could not create spectrum for file $phafile"; return $status; }
  $status = ahgen::run_ftool("fthedit",$phafile . "[SPECTRUM]","USER","a","sxsanticolc");
  if($status) { ahlog::ah_err "Could not create spectrum for file $phafile"; return $status; }
  $status = ahgen::run_ftool("fthedit",$phafile . "[GTI]","USER","a","sxsanticolc");
  if($status) { ahlog::ah_err "Could not create spectrum for file $phafile"; return $status; }
    $status = update_checksum_and_verify($phafile);
    unless ( $status ) { ahlog::ah_err "verify failed for $phafile"; return $status; }
  }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub changeColumnName {

my $infile      = shift ;
my $inext       = shift ;
my $incol       = shift ;
my $outcol      = shift ;

my $incolnum = ahgen::get_column_num($infile,$inext,$incol);
if ($incolnum == 0) { return 1; }     # done if column not found
my $ttype = "TTYPE$incolnum";
ahgen::set_keyword($infile,$inext,$ttype,$outcol,"label for field   $incolnum");

return 0;

}

# ------------------------------------------------------------------------------
