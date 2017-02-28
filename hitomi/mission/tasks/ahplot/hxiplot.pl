#!/usr/bin/perl
#
# File name: ahplot_hxi.pl
# Author: M. S. Dutka NASA GSFC
# $Date: 2016/12/21 18:45:27 $
# Version: 0
#
# Create a region file and run the extractor tool to create an SXS
# image, lightcurve and spectrum file
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
use File::Copy;
use ahplot qw (:ALL) ;

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

my $ehkfile    =""; # ehkfile file
my $infile1    =""; # filtered event file
my $infile2    =""; # unfiltered event file
my $outfile    =""; # Output file
my $gtifile    =""; # GTI time ranges to plot
my $timerange  =""; # Time range to include in the plot
my $binsize    =0;  # Bin size in seconds for LC
my $offset     =""; # Offset X axis to 0?
my $maxpts     =""; # Maximum points per graph
my $qdp        =""; # Stay in qdp
my $clobber    =""; # Overwrite existing files

my $telescop   ="HITOMI"; 
my $instrume;      # INSTRUME keyword
my @saa;           # relavant column data for saa
my $saavalid = 0;  # boolean which determines if the SAA is present 
my $tstart = 0;    # TSTART ehk file
my $tstop = 0;     # TSTOP ehk file
#my $timedel = 0;   # TIMEDEL ehk file
my $nbint = 0;     # Parameter for lcurve determines plotting time range
my $ehkcol = 0;    # Parameter for SAA column in ehk file

my $ahploterror = 0;  # ahplot exit status

my @infiles1 = ();
my @infiles2 = ();

my $tmp_ehkfile="";
my @gtifilelist = ();
my @rate1 = ();
my @rate2 = ();
my @rate3 = ();
my @rate4 = ();
my @gtirownums = ();
my $numgtifiles = 0;
my @row_pointing_start = ();
my @row_pointing_stop = ();
my $device = "";
my $gti_plot_file = "";
my $mjdrefi    =0;               # MJDREFI from ehk file
my $mjdreff    =0;               # MJDREFF from ehk file
#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

$ahploterror = get_parameters () ;
unless ( $ahploterror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($ahploterror);
}

$ahploterror = initialize () ;
unless ( $ahploterror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($ahploterror);
}

$ahploterror = do_work () ;
unless ( $ahploterror == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($ahploterror);
}

$ahploterror = finalize () ;
unless ( $ahploterror == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($ahploterror);
}

# We're done.
ahapp::end_processing($ahploterror);


#########################
# Subroutines
#########################

sub get_parameters {

  $ehkfile    = ahapp::query_parameter("ehkfile");
  $infile1    = ahapp::query_parameter("infile1");
  $infile2    = ahapp::query_parameter("infile2");
  $binsize    = ahapp::query_parameter("binsize");
  $outfile    = ahapp::query_parameter("outfile");
  $gtifile    = ahapp::query_parameter("gtifile");
  $timerange  = ahapp::query_parameter("timerange");
  $offset     = ahapp::query_parameter("offset");
  $maxpts     = ahapp::query_parameter("maxpts");
  $qdp        = ahapp::query_parameter("qdp");
  $clobber    = $ahapp::clobber ? "yes" : "no";

  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status = 0;
  
  # Input file checking
  if ($infile1 =~ /^@/) {
    if(isRequiredFileNotFound(substr($infile1,1))) { return 1; } 
  }
  # Load the input HXI normal event files into an array
  # and make sure each file exists
  @infiles1 = readInputFileList($infile1);
  unless(@infiles1) {
    ahgen::ah_err "No input HXI normal event files.";
    return 1;
  }
  if (isRequiredFileNotFound(@infiles1)) { return 1; }

  if ($infile2 =~ /^@/) {
    if(isRequiredFileNotFound(substr($infile2,1))) { return 1; } 
  }

 

  @infiles2 = readInputFileList($infile2);
  unless(@infiles2) {
    ahgen::ah_err "No input HXI normal event files.";
    return 1;
  }
  if (isRequiredFileNotFound(@infiles2)) { return 1; }

  # Check if ehk file exist
  if (isRequiredFileNotFound($ehkfile)) { return 1; }

  if (removeOutputFileIfClobbering($outfile,$ahapp::clobber) ) { return 1; }

  # Read instume keyword from input event file
  if ($infile1 =~ /^@/) {
    $instrume = ahgen::get_keyword($infiles1[0],"EVENTS","INSTRUME");
  } else {
    $instrume = ahgen::get_keyword($infile1,"EVENTS","INSTRUME");
  }


  # Read TSTART and TSTOP from ehk file then use to 
  # calculate nbint parameter for lcurve
  $tstart = ahgen::get_keyword($ehkfile,"EHK","TSTART");
  $tstop = ahgen::get_keyword($ehkfile,"EHK","TSTOP");
  $mjdrefi = ahgen::get_keyword($ehkfile,"EHK","MJDREFI");
  $mjdreff = ahgen::get_keyword($ehkfile,"EHK","MJDREFF");
 
  #$timedel = ahgen::get_keyword($ehkfile,"EHK","TIMEDEL");
  #if( ahgen::get_error_flag ) { 
  #  ahlog::ah_err "TIMEDEL keyword not defined in $ehkfile"; 
  #  return ahgen::get_error_flag;
  #}
  $nbint = (($tstop - $tstart)/$binsize) + 10;

  # Check the realavant SAA column in the ehk file to see if any rows equal 1 
  # if not exclude it from the file plotting
  if (uc $instrume eq "HXI1") {
    @saa = ahgen::read_column($ehkfile,"EHK","SAA_HXI1");
  } elsif (uc $instrume eq "HXI2") {
    @saa = ahgen::read_column($ehkfile,"EHK","SAA_HXI2");
  } else {
    ahlog::ah_err "Instrume keyword not recogonized (must be HXI1 or HXI2) aborting....";
    ahlog::ah_err ahgen::get_tool_stderr;
    ahapp::end_processing(1);
  }
 
  for (my $ii = 0; $ii < scalar(@saa); ++$ii) {
    if ($saa[$ii] == 1) {   
      $saavalid = 1;
      last;
    }
  }

  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;                 

  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $status = 0;
  
  # Create event file for layer 0 and layer 4 from unfiltered event data
  my $filename_uf_layer0;
  my $filename_uf_layer4;
  my $layer0_list;
  my $layer4_list;

  if ($infile2 =~ /^@/) { 

    # If infile2 is a list select layer 0 events from all file and store these
    # in individual files.  These files are then merged to make the input to 
    # extractor
    for (my $ii=0; $ii<scalar(@infiles2); ++$ii) {
      $filename_uf_layer0 = $infiles2[$ii] . ".layer0";
      if ($clobber) {
        if (-e $filename_uf_layer0) {
          unlink $filename_uf_layer0;
        }
      }
      $status = ahgen::run_ftool("ftselect","infile=$infiles2[$ii]","outfile=$filename_uf_layer0","expression=LAYER==0");
      if ( $status ) {
        ahlog::ah_err "ftselect failed.";
        return $status;
      }
      if ($ii != 0) { 
        $layer0_list = $layer0_list . "\n" . $filename_uf_layer0; 
      } else { 
        $layer0_list = $filename_uf_layer0; 
      }
    }
    print "layer0_list = $layer0_list ";
    $filename_uf_layer0 = "layer0_merged.list";
    if ($clobber) {     
      if (-e $filename_uf_layer0) {
        unlink $filename_uf_layer0;
      }
    }
    open (my $fh1, ">", $filename_uf_layer0);
    print $fh1 $layer0_list;
    close $fh1;
    $filename_uf_layer0 = "@" . $filename_uf_layer0;

    # If infile2 is a list select layer 4 events from all file and store these
    # in individual files.  These files are then merged to make the input to 
    # extractor
    for (my $ii=0; $ii<scalar(@infiles2); ++$ii) {
      $filename_uf_layer4 = $infiles2[$ii] . ".layer4";
      if ($clobber) {
        if (-e $filename_uf_layer4) {
          unlink $filename_uf_layer4;
        }
      }
      $status = ahgen::run_ftool("ftselect","infile=$infiles2[$ii]","outfile=$filename_uf_layer4","expression=LAYER==4");
      if ( $status ) {
        ahlog::ah_err "ftselect failed.";
        return $status;
      }
      if ($ii != 0) { 
        $layer4_list = $layer4_list . "\n" . $filename_uf_layer4; 
      } else { 
        $layer4_list = $filename_uf_layer4; 
      }
    }
    print "layer4_list = $layer4_list \n";
    $filename_uf_layer4 = "layer4_merged.list";
    if ($clobber) {
      if (-e $filename_uf_layer4) {
        unlink $filename_uf_layer4;
      }
    }
    open (my $fh2, ">", $filename_uf_layer4);
    print $fh2 $layer4_list;
    close $fh2;
    $filename_uf_layer4 = "@" . $filename_uf_layer4;

  } else {
    $filename_uf_layer0 = substr($infile2,0,-3) . "_ufa_layer0.evt";
    $filename_uf_layer4 = substr($infile2,0,-3) . "_ufa_layer4.evt"; 

    # Clean up existing files if clobber is set
    if ($clobber) {
      if (-e $filename_uf_layer0) {
        unlink $filename_uf_layer0;
      }
      if (-e $filename_uf_layer4) {
        unlink $filename_uf_layer4;
      }
    }

    $status = ahgen::run_ftool("ftselect","infile=$infile2","outfile=$filename_uf_layer0","expression=LAYER==0");
    if ( $status ) {
      ahlog::ah_err "ftselect failed.";
      return $status;
    }

    $status = ahgen::run_ftool("ftselect","infile=$infile2","outfile=$filename_uf_layer4","expression=LAYER==4");
    if ( $status ) {
      ahlog::ah_err "ftselect failed.";
      return $status;
    }
  }

  # run extractor to create lightcurves for filtered events and unfilters layer 0 and layer 4
  my $hxi_cl_lc = "hxi_cl.lc";
  my $hxi_uf_layer0 = "hxi_uf_layer0";
  my $hxi_uf_layer4 = "hxi_uf_layer4"; 

  ahapp::add_temp_file($hxi_cl_lc);
  ahapp::add_temp_file($hxi_uf_layer0);
  ahapp::add_temp_file($hxi_uf_layer4);

  my $filename = $infile1 . "[PI=     0:  2047]";
  $status = ahgen::run_ftool("extractor","filename=$filename", "eventsout=NONE",
                   "regionfile=NONE", "qdpfile=NONE", "fitsbinlc=$hxi_cl_lc",
                   "lcthresh=0", "lctzero=yes", "xronwn=NONE", "unbinlc=NONE", 
                   "phafile=NONE", "imgfile=NONE", "timefile=NONE", 
                   "adjustgti=no", "gstring=NONE", "timeorder=no", "xcolf=X",
                   "ycolf=Y", "xcolh=NONE", "ycolh=NONE", "xfkey=TLMAX",  
                   "yfkey=TLMAX", "xhkey=TLMAX", "yhkey=TLMAX", "phamax=TLMAX",
                   "specbin=1", "binh=1", "binf=1", "binlc=$binsize", 
                   "tcol=TIME", "ecol=PI", "ccol=NONE", "gcol=NONE", "gti=GTI",
                   "events=EVENTS", "gtitxt=NONE", "timeref=40000.00", 
                   "wtmapb=no", "gtinam=GTI", "exitnow=no");
  if ( $status ) {
    ahlog::ah_err "extractor failed.";
    return $status;
  }

  $filename = $filename_uf_layer0 . "[PI=     0:  2047]";
  $status = ahgen::run_ftool("extractor","filename=$filename", "eventsout=NONE",
                   "regionfile=NONE", "qdpfile=NONE", "fitsbinlc=$hxi_uf_layer0",
                   "lcthresh=0", "lctzero=yes", "xronwn=NONE", "unbinlc=NONE", 
                   "phafile=NONE", "imgfile=NONE", "timefile=NONE", 
                   "adjustgti=no", "gstring=NONE", "timeorder=no", "xcolf=X",
                   "ycolf=Y", "xcolh=NONE", "ycolh=NONE", "xfkey=TLMAX",  
                   "yfkey=TLMAX", "xhkey=TLMAX", "yhkey=TLMAX", "phamax=TLMAX",
                   "specbin=1", "binh=1", "binf=1", "binlc=$binsize", 
                   "tcol=TIME", "ecol=PI", "ccol=NONE", "gcol=NONE", "gti=GTI",
                   "events=EVENTS", "gtitxt=NONE", "timeref=40000.00", 
                   "wtmapb=no", "gtinam=GTI", "exitnow=no");
  if ( $status ) {
    ahlog::ah_err "extractor failed.";
    return $status;
  }


  $filename = $filename_uf_layer4 . "[PI=     0:  2047]";
  $status = ahgen::run_ftool("extractor","filename=$filename", "eventsout=NONE",
                   "regionfile=NONE", "qdpfile=NONE", "fitsbinlc=$hxi_uf_layer4", 
                   "lcthresh=0", "lctzero=yes", "xronwn=NONE", "unbinlc=NONE", 
                   "phafile=NONE", "imgfile=NONE", "timefile=NONE", 
                   "adjustgti=no", "gstring=NONE", "timeorder=no", "xcolf=X",
                   "ycolf=Y", "xcolh=NONE", "ycolh=NONE", "xfkey=TLMAX",  
                   "yfkey=TLMAX", "xhkey=TLMAX", "yhkey=TLMAX", "phamax=TLMAX",
                   "specbin=1", "binh=1", "binf=1", "binlc=$binsize", 
                   "tcol=TIME", "ecol=PI", "ccol=NONE", "gcol=NONE", "gti=GTI",
                   "events=EVENTS", "gtitxt=NONE", "timeref=40000.00", 
                   "wtmapb=no", "gtinam=GTI", "exitnow=no");
  if ( $status ) {
    ahlog::ah_err "extractor failed.";
    return $status;
  }
  
  # Run lcurve to put all the lightcurves (filtered events, layer 0 unfilterd 
  # and layer 4 unfilterd) into one file that is readable by fplot
  # Get the column number for the EHK column SAA_HXI1/2

  if (uc $instrume eq "HXI1") {
    $ehkcol = ahgen::get_column_num($ehkfile,"EHK","SAA_HXI1");
    unless ( $ehkcol ) { 
      ahgen::ah_err "Could not find column SAA_HXI1 in file $ehkfile\[EHK]";
      return 1;
    }
  } elsif (uc $instrume eq "HXI2") {
    $ehkcol = ahgen::get_column_num($ehkfile,"EHK","SAA_HXI2");
    unless ( $ehkcol ) { 
      ahgen::ah_err "Could not find column SAA_HXI2 in file $ehkfile\[EHK]";
      return 1;
    }
  }

  # Declare strings to hold fplot parameter values
  # They will change if SAA information is being
  # or not.
  my $fplot_infile = "";
  my $yparm = "";

  if ($saavalid) {
    $status = ahgen::run_ftool("lcurve","nser=4","cfile4=$ehkfile vy$ehkcol", 
                               "cfile3=$hxi_cl_lc",
                               "cfile2=$hxi_uf_layer0", 
                               "cfile1=$hxi_uf_layer4",
                               "window=-","dtnb=$binsize","nbint=$nbint",
                               "plot=N","plotdev=/XW","plotdnum=4",
                               "outfile=out","outfiletype=2",
                               "clobber=$clobber");
    if ( $status ) {
      ahlog::ah_err "lcurve failed";
    }
  



  } else {  # Observation does not overlap with SAA
    $status = ahgen::run_ftool("lcurve","nser=3", 
                               "cfile3=$hxi_cl_lc",
                               "cfile2=$hxi_uf_layer0", 
                               "cfile1=$hxi_uf_layer4",
                               "window=-","dtnb=$binsize","nbint=$nbint",
                               "plot=N","plotdev=/XW","plotdnum=4",
                               "outfile=out","outfiletype=2",
                               "clobber=$clobber");
    if ( $status ) {
      ahlog::ah_err "lcurve failed";
    }
  

   
  } # endif $saavalid

  ############### Add gti plotting information to the output of lcurve#########

  # Read timerange (if provided) and obtain the row
  # numbers in RA,DEC,ROLL which correspnd to that 
  # time range
  if (uc $timerange ne "NONE") {
    print "Timerange ne NONE \n";
    # Determine the row ranges to read from the input EHK file either
    # from a user specified time range or a  
    my ($start_time,$stop_time) = ahplot::parse_timerange($timerange);
    my ($rpstart,$rpstop) = ahplot::timerange_rownums("out.flc[RATE]",$start_time,$stop_time);
    @row_pointing_start = @$rpstart;
    @row_pointing_stop = @$rpstop; 
    print "row_pointing_start = $row_pointing_start[0] \n";
    print "row_pointing_stop = $row_pointing_stop[0] \n";

    # Read only rows when the telscope is in pointing mode  
    for (my $ii=0; $ii<scalar(@row_pointing_start);++$ii) {
      if ($saavalid) {
          print "reading columns";
        push(@rate1,ahgen::read_column("out.flc","RATE","RATE1",$row_pointing_start[$ii],$row_pointing_stop[$ii]));
        push(@rate2,ahgen::read_column("out.flc","RATE","RATE2",$row_pointing_start[$ii],$row_pointing_stop[$ii]));
        push(@rate3,ahgen::read_column("out.flc","RATE","RATE3",$row_pointing_start[$ii],$row_pointing_stop[$ii]));
        push(@rate4,ahgen::read_column("out.flc","RATE","RATE4",$row_pointing_start[$ii],$row_pointing_stop[$ii]));
      } else {
          print "reading columns \n";

        push(@rate1,ahgen::read_column("out.flc","RATE","RATE1",$row_pointing_start[$ii],$row_pointing_stop[$ii]));
        push(@rate2,ahgen::read_column("out.flc","RATE","RATE2",$row_pointing_start[$ii],$row_pointing_stop[$ii]));
        push(@rate3,ahgen::read_column("out.flc","RATE","RATE3",$row_pointing_start[$ii],$row_pointing_stop[$ii]));  
      }

    }
  } else { # Parameter timerange=NONE
    print "Timerange = NONE \n";
    # Read all rows in EHK file
    if ($saavalid) {
      @rate1 = ahgen::read_column("out.flc","RATE","RATE1");
      @rate2 = ahgen::read_column("out.flc","RATE","RATE2");
      @rate3 = ahgen::read_column("out.flc","RATE","RATE3");
      @rate4 = ahgen::read_column("out.flc","RATE","RATE4");
    } else {
      @rate1 = ahgen::read_column("out.flc","RATE","RATE1");
      @rate2 = ahgen::read_column("out.flc","RATE","RATE2");
      @rate3 = ahgen::read_column("out.flc","RATE","RATE3");  
      @rate4 = (0);
    }
  }

  for (my $ii=0; $ii<10; ++$ii) {
    print "rate1 = $rate1[$ii] \n";
  }

  print "rate1\[1397\] = $rate1[1397] \n";
 
  my @rate1_pars = ahplot::calculate_gti_y_position_parameters(@rate1);
  my @rate2_pars = ahplot::calculate_gti_y_position_parameters(@rate2);
  my @rate3_pars = ahplot::calculate_gti_y_position_parameters(@rate3);
  my @rate4_pars = ahplot::calculate_gti_y_position_parameters(@rate4);

  print "rate1_pars = $rate1_pars[0], $rate1_pars[1], $rate1_pars[2] \n";
  print "rate2_pars = $rate2_pars[0], $rate2_pars[1], $rate2_pars[2] \n";
  print "rate3_pars = $rate3_pars[0], $rate3_pars[1], $rate3_pars[2] \n";
  print "rate4_pars = $rate4_pars[0], $rate4_pars[1], $rate4_pars[2] \n"; 
  #exit 0; 

  # first y postion of GTI intervals, subsequent gti intervals are stacked vertically  
  my @ypos_0 = ();
  $ypos_0[0] = $rate1_pars[1];
  $ypos_0[1] = $rate2_pars[1];
  $ypos_0[2] = $rate3_pars[1];
  $ypos_0[3] = $rate4_pars[1];
 
  # difference in spacing between subsequent gti intervals
  my @deltay = ();
  $deltay[0] = $rate1_pars[2];
  $deltay[1] = $rate2_pars[2];
  $deltay[2] = $rate3_pars[2];
  $deltay[3] = $rate4_pars[2];  

  # Array containing plotting boudariy for each panel
  my @ymin = ();
  my @ymax = ();
 
  # Count the number of gtifiles
  # Parse list of input GTI files
  my @gtifilelist = ();
 
  if (uc $gtifile ne "NONE") {
    @gtifilelist = ahgen::get_file_list($gtifile);
  }  

  $numgtifiles = scalar(@gtifilelist);

  # Plotting boundaries for each panel
  $ymin[0] = $rate1_pars[0];
  $ymin[1] = $rate2_pars[0];
  $ymin[2] = $rate3_pars[0];
  $ymin[3] = $rate4_pars[0];
  $ymax[0] = $ypos_0[0] + (($numgtifiles + 1) * $deltay[0]);
  $ymax[1] = $ypos_0[1] + (($numgtifiles + 1) * $deltay[1]);
  $ymax[2] = $ypos_0[2] + (($numgtifiles + 1) * $deltay[2]);

  print "ypos_0\[3\] = $ypos_0[3] \n";
  print "numgtifiles = $numgtifiles \n";
  print "deltay\[3\] = $deltay[3] \n";

  $ymax[3] = $ypos_0[3] + (($numgtifiles + 1) * $deltay[3]);

  # Getting tstart and tstop from EHK file these are used 
  # when offset mode = yes 
  my $tstart_ehk = ahgen::get_keyword($ehkfile,"EHK","TSTART");
  my $tstop_ehk = ahgen::get_keyword($ehkfile,"EHK","TSTOP");

  ahplot::write_gti_pco($outfile,$qdp,$offset,$tstart_ehk,$tstop_ehk,\@ymin,\@ymax);
 
  # Prepare additional input for make_gti_plot
  my @ycols = ();
  if ($saavalid) {
    @ycols = ("RATE1 1E","RATE2 1E","RATE3 1E","RATE4 1E");
  } else {
    @ycols = ("RATE1 1E","RATE2 1E","RATE3 1E");
  }
  
  $gti_plot_file = "gti_plot.fits";

  # Prepare the data needed for plotting gti
  if ($numgtifiles > 0) {
    ahplot::prepare_gti_plot_data($gtifile,"RATE","TIME","DX",$gti_plot_file,\@ypos_0,\@deltay,\@ycols);
  }

  # lcurve will shift the TIME scale so that the first bin in the output 
  # file has TIME=0.  The amount of shift applied is stored in the TSTARTI
  # and TSTARTF keywords in the TJD format.  Need to convert this number
  # into seconds so that any GTI can be shifted by the same amount.
  my $tstarti=ahgen::get_keyword("out.flc","RATE","TSTARTI");
  my $tstartf=ahgen::get_keyword("out.flc","RATE","TSTARTF");
  $tstarti=$tstarti+40000.;    # convert from TJD to MJD
  my $intime=$tstarti+$tstartf;
  my $mjdref=$mjdrefi+$mjdreff;    # epoch
  $status=ahgen::run_ftool("ahtimeconv",
                           "intime=$intime",
                           "insys=TT",
                           "inform=mjd",
                           "outsys=MET",        # mission elapsed time (time since epoch)
                           "outform=sec",
                           "epochtime=$mjdref",
                           "epochsys=TT",
                           "epochform=mjd",
                          );
  if ( $status ) {
    ahlog::ah_err "ahtimeconv failed; could not compute lcurve offset time.";
    return $status;
  }

  # Get output parameter from ahtimeconv
  ahgen::run_ftool("pget", "ahtimeconv", "outtime");
  my $lcurve_offset = ahgen::get_tool_stdout;


  # shift GTI TIMEs by lcurve offset time
  if ($numgtifiles > 0) {
    $status=ahgen::run_ftool("ftcalc",
                             "infile=$gti_plot_file",
                             "outfile=$gti_plot_file",
                             "column=TIME",
                             "expression=TIME-$lcurve_offset",
                             "clobber=yes",
                            );
    if ($status) {
      ahlog::ah_err "ftcalc failed; could not apply lcurve offset to GTI data.";
      return $status;
    }
  }

  
  # Create temporary file which will be merged with the gti information and
  # then plotted using fplot
  my $tmp_ratefile = "temp_rate.fits";
  ahapp::add_temp_file("temp_rate.fits");
  $status = ahgen::copy_fits_file("out.flc[col TIME]",
                                  $tmp_ratefile,
                                  "copyall=no");

 if ( $status ) {
    ahlog::ah_err "Creating temporary ehk file failed.";
    return $status;
  }

  # Add columns to fits file Neccesary for plotting GTI intervals
  $status = ahgen::add_column($tmp_ratefile,"RATE","DX","0","1D");
  if ( $status ) {
    ahlog::ah_err "adding column failed.";
    return $status;
  }

  if ($saavalid) {
    $status = ahgen::run_ftool("ftpaste",
                               "infile=$tmp_ratefile\[RATE\]",
                               "pastefile=out.flc\[col RATE1;RATE2;RATE3;RATE4\]",
                               "outfile=$tmp_ratefile",
                               "clobber=yes");
    if ( $status ) {
      ahlog::ah_err "adding columns to temp_rate.fits failed.";
      return $status;
    }
  } else {
     $status = ahgen::run_ftool("ftpaste",
                               "infile=$tmp_ratefile\[EHK\]",
                               "pastefile=out.flc\[col RATE1;RATE2;RATE3\]",
                               "outfile=$tmp_ratefile",
                               "clobber=yes");
    if ( $status ) {
      ahlog::ah_err "adding columns to temp_rate.fits failed.";
      return $status;
    }
  }
   
  # Add null row to fits file to separate data and time intervals plots
  ahplot::write_null_row("$tmp_ratefile","TIME");
  
  my $plotfile = "plotfile.tmp";
  ahapp::add_temp_file("plotfile.tmp");
  # If there are no gti files we skip merging gti information to 
  # the plot file 
  if ($numgtifiles > 0) {
    $status = ahgen::run_ftool("ftmerge",
                               "infile=$tmp_ratefile\[RATE\],$gti_plot_file\[RATE\]",
                               "outfile=$plotfile",
                               "clobber=YES");
  } else {
    $plotfile = $tmp_ratefile;
  }
  
  if ($saavalid) {
    $fplot_infile = "$plotfile\[RATE\]\[col TIME;DX;Layer0=RATE1;Layer4=RATE2;Filtered=RATE3;SAA=RATE4;\]";
    $yparm = "Layer0 Layer4 Filtered SAA";   
  } else {
    $fplot_infile = "$plotfile\[RATE\]\[col TIME;Layer0=RATE1;Layer4=RATE2;Filtered=RATE3;\]";
    $yparm = "Layer0 Layer4 Filtered";
  }

  # Run FPLOT to create the postscript file
  $status = ahgen::run_ftool("fplot",
                             "infile=$fplot_infile",
                             "maxpts=$maxpts",
                             "offset=$offset",
                             "xparm=TIME[DX]",
                             "yparm=$yparm",
                             "rows=-",
                             "device=$outfile/cps",
                             "pltcmd=\@ahplot.pco",
                             "binmode=DEFAULT",
                             "sensecase=no"
                            );

  if ( $status ) {
    ahlog::ah_err "fplot failed.";
    return $status;
  }

  return 0;

}

# ------------------------------------------------------------------------------

sub finalize {

  my $status = 0;

  return 0;
  
}

# ------------------------------------------------------------------------------
