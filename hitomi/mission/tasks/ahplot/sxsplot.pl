#!/usr/bin/perl
#
# File name: sxsplot.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/07/25 20:25:39 $
# Version: 0
#
# 1. Plot the housekeeping ADRC data for the SXS
# 2. Plot the lightcurves of the antico, unfiltered & cleaned SXS events
#    and the SAA for the SXI
# 3. Plot the cleaned & unfiltered SXS lightcurves, the SAA for the SXS
#    and the SXS ADRC fluctuation 
#
# Tool Dependencies:
#   extractor
#   lcurve
#   fplot
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
use File::Basename;

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

my $infile1     =""; # Input unfiltered event file
my $infile2     =""; # Input cleaned event file
my $antfile     =""; # Input antico event file
my $mkffile    =""; # Input MKF file
my $ehkfile    =""; # Input EHK file

my $adrcplot   =""; # Output SXS ADRC plot file
my $saaplot    =""; # Output SXS SAA plot file
my $fluctplot  =""; # Output SXS fluctuation plot file

my $lcthresh   =""; # Lightcurve threshold
my $acbinlc    =""; # Bin size for lightcurve
my $binlc      =""; # Bin size for lightcurve

my $offset     =""; # Offset X axis to 0?
my $maxpts     =""; # Maximum points per graph
my $adrcpltcmd =""; # Any legal PLT command
my $saapltcmd  =""; # Any legal PLT command
my $fluctpltcmd=""; # Any legal PLT command
my $timedel    =""; # TIMEDEL value for MKF (DEFAULT=Use MKF keyword)

my $telescop   ="HITOMI";
my $mkfcol     =0;
my $ehkcol     =0;
my @saa;           # relavant column data for saa
my $saavalid = 0;  #boolean which determines if the SAA is present 
my $tstart = 0;    # TSTART ehk file
my $tstop = 0;     # TSTOP ehk file
my $nbint = 0;     # Parameter for lcurve determines plotting time range

my $sxsploterror = 0;  # sxsplot exit status

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

$sxsploterror = get_parameters () ;
unless ( $sxsploterror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($sxsploterror);
}

$sxsploterror = initialize () ;
unless ( $sxsploterror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($sxsploterror);
}

$sxsploterror = do_work () ;
unless ( $sxsploterror == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($sxsploterror);
}

$sxsploterror = finalize () ;
unless ( $sxsploterror == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($sxsploterror);
}

# We're done.
ahapp::end_processing($sxsploterror);


#########################
# Subroutines
#########################

sub get_parameters {

  $infile1     = ahapp::query_parameter("infile1");
  $infile2     = ahapp::query_parameter("infile2");
  $antfile    = ahapp::query_parameter("antfile");
  $mkffile    = ahapp::query_parameter("mkffile");
  $ehkfile    = ahapp::query_parameter("ehkfile");

  $adrcplot   = ahapp::query_parameter("adrcplot");
  $saaplot    = ahapp::query_parameter("saaplot");
  $fluctplot  = ahapp::query_parameter("fluctplot");

  $lcthresh   = ahapp::query_parameter("lcthresh");
  $binlc      = ahapp::query_parameter("binlc");
  $acbinlc    = ahapp::query_parameter("acbinlc");

  $offset     = ahapp::query_parameter("offset");
  $adrcpltcmd = ahapp::query_parameter("adrcpltcmd");
  $saapltcmd  = ahapp::query_parameter("saapltcmd");
  $fluctpltcmd= ahapp::query_parameter("fluctpltcmd");

  $timedel    = ahapp::query_parameter("timedel");

  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status = 0;

  # Input file checking
  # Check the unfiltered sxs event file(s)
  if ($infile1 =~ /^@/) {
    if(isRequiredFileNotFound(substr($infile1,1))) { return 1; } 
  }
  # Load the input SXS unfiltered event files into an array
  # and make sure each file exists
  my @in_sxs_uf_files = readInputFileList($infile1);
  unless(@in_sxs_uf_files) {
    ahgen::ah_err "No input SXS unfiltered event files.";
    return 1;
  }
  if (isRequiredFileNotFound(@in_sxs_uf_files)) { return 1; }

  # Check the cleaned sxs event file(s)
  if ($infile2 =~ /^@/) {
    if(isRequiredFileNotFound(substr($infile2,1))) { return 1; } 
  }
  # Load the input SXS cleaned event files into an array
  # and make sure each file exists
  my @in_sxs_cl_files = readInputFileList($infile2);
  unless(@in_sxs_cl_files) {
    ahgen::ah_err "No input SXS cleaned event files.";
    return 1;
  }
  if (isRequiredFileNotFound(@in_sxs_cl_files)) { return 1; }

  # Check for existing antico, MKF and EHK files
  if (isRequiredFileNotFound($antfile)) { return 1; }
  if (isRequiredFileNotFound($mkffile)) { return 1; }
  if (isRequiredFileNotFound($ehkfile)) { return 1; }
  if (removeOutputFileIfClobbering($adrcplot,$ahapp::clobber) ) { return 1; }
  if (removeOutputFileIfClobbering($saaplot,$ahapp::clobber) ) { return 1; }
  if (removeOutputFileIfClobbering($fluctplot,$ahapp::clobber) ) { return 1; }

  # Read Tstart TSTOP and timedel from ehk file then use to calculate nbint parameter for lcurve
  $tstart = ahgen::get_keyword($ehkfile,"EHK","TSTART");
  $tstop = ahgen::get_keyword($ehkfile,"EHK","TSTOP");
  
  $nbint = int((($tstop - $tstart)/$binlc) + 10);
 

  @saa = ahgen::read_column($ehkfile,"EHK","SAA_SXS");

 
  for (my $ii = 0; $ii < scalar(@saa); ++$ii) {
    if ($saa[$ii] == 1) {   
      $saavalid = 1;
      last;
    }
  }

  # Check if the pltcmd is a text file
  # If so, make a copy of it to a temporary file
  # and add plot and quit commands.
  # 
  # These are failsafes in case the user did not put
  # either plot or quit into their pco file
  if ( $adrcpltcmd =~ /^@/ ) {
    my $pltfile="adrc_sxsplot.pco";
    ahapp::add_temp_file($pltfile);
    if(isRequiredFileNotFound(substr($adrcpltcmd,1))) { return 1; } 
    File::Copy::copy(substr($adrcpltcmd,1),$pltfile);
    open PLT,">>$pltfile";
    print PLT "line off\n";  # Turn off line in between individual data points
    print PLT "mark 2 on 2 3 4 5\n";
    print PLT "cpd $adrcplot/cps \n";
    print PLT "plot\n"; # force pgplot to plot the file
    print PLT "quit\n"; # force pgplot to quit
    close PLT;
    $adrcpltcmd = "\@$pltfile";
  }
  if ( $saapltcmd =~ /^@/ ) {
    my $pltfile ="saa_sxsplot.pco";
    ahapp::add_temp_file($pltfile);
    if(isRequiredFileNotFound(substr($saapltcmd,1))) { return 1; } 
    File::Copy::copy(substr($saapltcmd,1),$pltfile);
    open PLT,">>$pltfile";
    print PLT "line off\n";  # Turn off line in between individual data points
    if ($saavalid) {  # set the point shape for relavant plots (If no SAA do not set those points)
      print PLT "mark 2 on 2 3 4 5\n";
    } else {
      print PLT "mark 2 on 2 3 4\n";
    }
    print PLT "cpd $saaplot/cps \n";
    print PLT "plot\n"; # force pgplot to plot the file
    print PLT "quit\n"; # force pgplot to quit
    close PLT;
    $saapltcmd = "\@$pltfile";
  }
  if ( $fluctpltcmd =~ /^@/ ) {
    my $pltfile="fluct_sxsplot.pco";
    ahapp::add_temp_file($pltfile);
    if(isRequiredFileNotFound(substr($fluctpltcmd,1))) { return 1; } 
    File::Copy::copy(substr($fluctpltcmd,1),$pltfile);
    open PLT,">>$pltfile";
    print PLT "line off\n";  # Turn off line in between individual data points
    if ($saavalid) {  # set the point shape for relavant plots (If no SAA do not set those points)
      print PLT "mark 2 on 2 3 4 5\n";
    } else {
      print PLT "mark 2 on 2 3 4\n";
    }
    print PLT "cpd $fluctplot/cps \n";
    print PLT "plot\n"; # force pgplot to plot the file
    print PLT "quit\n"; # force pgplot to quit
    close PLT;
    $fluctpltcmd = "\@$pltfile";
  }

  # Get the column number for the MKF column HKSXSAC010 (ADRC_CT_CTL_FLUC)
  $mkfcol = ahgen::get_column_num($mkffile,"FILTER","HKSXSAC010");
  unless ( $mkfcol ) {
    ahgen::ah_err "Could not find column HKSXSAC010 in file $mkffile\[FILTER]";
    return 1;
  }

  # Set the maxpts parameter passed to fplot to NAXIS2 in mkffile + 50
  $maxpts = ahgen::get_keyword($mkffile,"FILTER","NAXIS2") + 50;

  # Check the MKF file for the TIMEDEL keyword
  if ( $timedel eq "DEFAULT" ) {
    my $mkf_timedel = ahgen::get_keyword($mkffile,"FILTER","TIMEDEL");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "TIMEDEL keyword not defined in $mkffile"; 
      return ahgen::get_error_flag;
    }
  } else {
    # Make a copy of the MKF file and set the TIMEDEL keyword
    my $tmp_mkffile = "tmp_" . basename($mkffile);
    $tmp_mkffile =~ s/\.gz$//;
    ahapp::add_temp_file($tmp_mkffile);
    $status = ahgen::copy_fits_file($mkffile . "[FILTER][col TIME;ADRC_CAMC_CT0_S0_T=HKSXSAC001;ADRC_CAMC_CT1_S1_T=HKSXSAC004;ADRC_CT_CTL_FLUC=HKSXSAC010;ADRC_CT_MON_FLUC=HKSXSAC011;]",$tmp_mkffile);
    # Overwrite the column number for the MKF column HKSXSAC010 (ADRC_CT_CTL_FLUC)
    # because we are now using the temporary file
    $mkfcol = ahgen::get_column_num($tmp_mkffile,"FILTER","ADRC_CT_CTL_FLUC");
    if ( $status ) {
      ahlog::ah_err "Error copying $mkffile to $tmp_mkffile"; 
      return ahgen::get_error_flag;
    }
    $status = ahgen::set_keyword($tmp_mkffile,"FILTER","TIMEDEL",$timedel);
    if ( $status ) {
      ahlog::ah_err "TIMEDEL keyword not set in $tmp_mkffile"; 
      return ahgen::get_error_flag;
    }
    # Set the temporary MKF file as the MKF file to use in lcurve
    $mkffile=$tmp_mkffile;
  }

  # Get the column number for the EHK column SAA_SXS
  $ehkcol = ahgen::get_column_num($ehkfile,"EHK","SAA_SXS");
  unless ( $ehkcol ) {
    ahgen::ah_err "Could not find column SAA_SXS in file $ehkfile\[EHK]";
    return 1;
  }

  # Check the EHK file for the TIMEDEL keyword
  my $ehk_timedel = ahgen::get_keyword($ehkfile,"EHK","TIMEDEL");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "TIMEDEL keyword not defined in $ehkfile"; 
    return ahgen::get_error_flag;
  }


  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;

  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $status = 0;

  # Set up the temporary file names
  my $sxs_ac_lc = "sxs_ac.lc";
  my $sxs_uf_lc = "sxs_uf.lc";
  my $sxs_cl_lc = "sxs_cl.lc";
  my $sxs_saa_lc = "sxs_saa.lc";
  my $sxs_fl_lc = "sxs_fl.lc";
  my $filename  = "";

  ahapp::add_temp_file($sxs_ac_lc);
  ahapp::add_temp_file($sxs_uf_lc);
  ahapp::add_temp_file($sxs_cl_lc);
  ahapp::add_temp_file($sxs_saa_lc);
  ahapp::add_temp_file($sxs_fl_lc);

  ########################
  # Extract lightcurves
  ########################

  # Extract the antico lightcurve
  $status = ahgen::run_ftool("extractor",
                             "filename=$antfile\[PI= -8192: 12200]",
                             "eventsout=NONE", "regionfile=NONE", "qdpfile=NONE",
                             "fitsbinlc=$sxs_ac_lc",
                             "lcthresh=$lcthresh",
                             "lctzero=yes",
                             "xronwn=NONE", "unbinlc=NONE", "phafile=NONE", "imgfile=NONE", "timefile=NONE",
                             "adjustgti=no", "gstring=NONE", "timeorder=no",
                             "xcolf=NONE",
                             "ycolf=NONE",
                             "xcolh=NONE", "ycolh=NONE",
                             "xfkey=TLMAX", "yfkey=TLMAX", "xhkey=TLMAX", "yhkey=TLMAX", "phamax=TLMAX",
                             "specbin=1", "binh=1", "binf=1",
                             "binlc=$acbinlc",
                             "tcol=TIME",
                             "ecol=PI",
                             "ccol=NONE",
                             "gcol=ITYPE",
                             "gti=GTI", "events=EVENTS",
                             "gtitxt=NONE", "timeref=40000.00",
                             "wtmapb=no",
                             "gtinam=GTI",
                             "exitnow=no");

  if ( $status ) {
    ahlog::ah_err "extractor failed.";
    return $status;
  }

  # Extract the unfiltered SXS events lightcurve
  $status = ahgen::run_ftool("extractor",
                             "filename=$infile1\[PI=-16384: 32768]",
                             "eventsout=NONE", "regionfile=NONE", "qdpfile=NONE",
                             "fitsbinlc=$sxs_uf_lc",
                             "lcthresh=$lcthresh",
                             "lctzero=yes",
                             "xronwn=NONE", "unbinlc=NONE", "phafile=NONE", "imgfile=NONE", "timefile=NONE",
                             "adjustgti=no", "gstring=NONE", "timeorder=no",
                             "xcolf=X",
                             "ycolf=Y",
                             "xcolh=NONE", "ycolh=NONE",
                             "xfkey=TLMAX", "yfkey=TLMAX", "xhkey=TLMAX", "yhkey=TLMAX", "phamax=TLMAX",
                             "specbin=1", "binh=1", "binf=1",
                             "binlc=$binlc",
                             "tcol=TIME",
                             "ecol=PI",
                             "ccol=NONE",
                             "gcol=ITYPE",
                             "gti=GTI", "events=EVENTS",
                             "gtitxt=NONE", "timeref=40000.00",
                             "wtmapb=no",
                             "gtinam=GTI",
                             "exitnow=no");

  if ( $status ) {
    ahlog::ah_err "extractor failed.";
    return $status;
  }

  # Extract the cleaned SXS events lightcurve
  $status = ahgen::run_ftool("extractor",
                             "filename=$infile2\[PI= 0: 32767]"  ,
                             "eventsout=NONE", "regionfile=NONE", "qdpfile=NONE",
                             "fitsbinlc=$sxs_cl_lc",
                             "lcthresh=$lcthresh",
                             "lctzero=yes",
                             "xronwn=NONE", "unbinlc=NONE", "phafile=NONE", "imgfile=NONE", "timefile=NONE",
                             "adjustgti=no", "gstring=NONE", "timeorder=no",
                             "xcolf=X",
                             "ycolf=Y",
                             "xcolh=NONE", "ycolh=NONE",
                             "xfkey=TLMAX", "yfkey=TLMAX", "xhkey=TLMAX", "yhkey=TLMAX", "phamax=TLMAX",
                             "specbin=1", "binh=1", "binf=1",
                             "binlc=$binlc",
                             "tcol=TIME",
                             "ecol=PI",
                             "ccol=NONE",
                             "gcol=ITYPE",
                             "gti=GTI", "events=EVENTS",
                             "gtitxt=NONE", "timeref=40000.00",
                             "wtmapb=no",
                             "gtinam=GTI",
                             "exitnow=no");

  if ( $status ) {
    ahlog::ah_err "extractor failed.";
    return $status;
  }

  ########################
  # Merge SXS lightcurves
  ########################
  if ($saavalid) {
    # Make the merged SXS SAA lightcurve
    $status = ahgen::run_ftool("lcurve",
                               "nser=4",
                               "cfile4=$ehkfile vy$ehkcol",
                               "cfile3=$sxs_ac_lc",
                               "cfile2=$sxs_cl_lc",
                               "cfile1=$sxs_uf_lc",
                               "window=-",
                               "dtnb=$binlc",
                               "nbint=$nbint",
                               "plot=n",
                               "plotdev=/XW",
                               "plotdnum=4",
                               "outfile=$sxs_saa_lc");
    if ( $status ) {
      ahlog::ah_err "lcurve failed.";
      return $status;
    }


    # Make the merged SXS fluctuation lightcurve
    $status = ahgen::run_ftool("lcurve",
                               "nser=4",
                               "cfile4=$ehkfile vy$ehkcol",
                               "cfile3=$mkffile vy$mkfcol",
                               "cfile2=$sxs_uf_lc",
                               "cfile1=$sxs_cl_lc",
                               "window=-",
                               "dtnb=$binlc",
                               "nbint=$nbint",
                               "plot=n",
                               "plotdev=/XW",
                               "plotdnum=4",
                               "outfile=$sxs_fl_lc");
    if ( $status ) {
      ahlog::ah_err "lcurve failed.";
      return $status;
    }
  } else { #no valid SAA time
    # Make the merged SXS SAA lightcurve
    $status = ahgen::run_ftool("lcurve",
                               "nser=3",
                               "cfile3=$sxs_ac_lc",
                               "cfile2=$sxs_cl_lc",
                               "cfile1=$sxs_uf_lc",
                               "window=-",
                               "dtnb=$binlc",
                               "nbint=$nbint",
                               "plot=n",
                               "plotdev=/XW",
                               "plotdnum=4",
                               "outfile=$sxs_saa_lc");
    if ( $status ) {
      ahlog::ah_err "lcurve failed.";
      return $status;
    }

    # Make the merged SXS fluctuation lightcurve
    $status = ahgen::run_ftool("lcurve",
                               "nser=3",
                               "cfile3=$mkffile vy$mkfcol",
                               "cfile2=$sxs_uf_lc",
                               "cfile1=$sxs_cl_lc",
                               "window=-",
                               "dtnb=$binlc",
                               "nbint=$nbint",
                               "plot=n",
                               "plotdev=/XW",
                               "plotdnum=4",
                               "outfile=$sxs_fl_lc");
    if ( $status ) {
      ahlog::ah_err "lcurve failed.";
      return $status;
    }

  }

  ########################
  # Plot the SXS data
  ########################
  # Plot the ADRC DATA
  # ADRC: ah100050020_sxs_adrc.ps [fplot HKSXSAC001,004,010,011 vs TIME]
  # (ADRC_CAMC_CT0_S0_T, ADRC_CAMC_CT1_S1_T, ADRC_CT_CTL_FLUC, ADRC_CT_MON_FLUC)
  $filename = $mkffile . "[FILTER]";
  # Run FPLOT to create the SXS postscript file
  $status = ahgen::run_ftool("fplot",
                             "infile=$filename",
                             "maxpts=$maxpts",
                             "offset=$offset",
                             "xparm=TIME",
                             "yparm=ADRC_CAMC_CT0_S0_T ADRC_CAMC_CT1_S1_T ADRC_CT_CTL_FLUC ADRC_CT_MON_FLUC",
                             "rows=-",
                             "device=$adrcplot/cps",
                             "pltcmd=$adrcpltcmd",
                             "binmode=DEFAULT",
                             "sensecase=no",
                           );
  if ( $status ) {
    ahlog::ah_err "fplot failed.";
    return $status;
  }

  if ($saavalid) {
    # Plot the SXS SAA DATA
    $filename = $sxs_saa_lc . "[RATE][col TIME;RATEUF=RATE1;RATECL=RATE2;RATEAC=RATE3;SXS_SAA=RATE4;]";
    # Run FPLOT to create the SXS postscript file
    $status = ahgen::run_ftool("fplot",
                               "infile=$filename",
                               "maxpts=$maxpts",
                               "offset=$offset",
                               "xparm=TIME",
                               "yparm=RATEAC RATECL RATEUF SXS_SAA",
                               "rows=-",
                               "device=$saaplot/cps",
                               "pltcmd=$saapltcmd",
                               "binmode=DEFAULT",
                               "sensecase=no",
                             );
    if ( $status ) {
      ahlog::ah_err "fplot failed.";
      return $status;
    }

    # Plot the SXS FLUCTUATION DATA
    $filename = $sxs_fl_lc . "[RATE][col TIME;RATECL=RATE1;RATEUF=RATE2;ADRC_CT_CTL_FLUC=RATE3;SXS_SAA=RATE4;]";
    # Run FPLOT to create the SXS postscript file
    $status = ahgen::run_ftool("fplot",
                               "infile=$filename",
                               "maxpts=$maxpts",
                               "offset=$offset",
                               "xparm=TIME",
                               "yparm=RATECL RATEUF ADRC_CT_CTL_FLUC SXS_SAA",
                               "rows=-",
                               "device=$fluctplot/cps",
                               "pltcmd=$fluctpltcmd",
                               "binmode=DEFAULT",
                               "sensecase=no",
                             );
    if ( $status ) {
      ahlog::ah_err "fplot failed.";
      return $status;
    }
  } else {
    # Plot the SXS SAA DATA
    $filename = $sxs_saa_lc . "[RATE][col TIME;RATEUF=RATE1;RATECL=RATE2;RATEAC=RATE3;]";
    # Run FPLOT to create the SXS postscript file
    $status = ahgen::run_ftool("fplot",
                               "infile=$filename",
                               "maxpts=$maxpts",
                               "offset=$offset",
                               "xparm=TIME",
                               "yparm=RATEAC RATECL RATEUF",
                               "rows=-",
                               "device=$saaplot/cps",
                               "pltcmd=$saapltcmd",
                               "binmode=DEFAULT",
                               "sensecase=no",
                             );
    if ( $status ) {
      ahlog::ah_err "fplot failed.";
      return $status;
    }

    # Plot the SXS FLUCTUATION DATA
    $filename = $sxs_fl_lc . "[RATE][col TIME;RATECL=RATE1;RATEUF=RATE2;ADRC_CT_CTL_FLUC=RATE3;]";
    # Run FPLOT to create the SXS postscript file
    $status = ahgen::run_ftool("fplot",
                               "infile=$filename",
                               "maxpts=$maxpts",
                               "offset=$offset",
                               "xparm=TIME",
                               "yparm=RATECL RATEUF ADRC_CT_CTL_FLUC",
                               "rows=-",
                               "device=$fluctplot/cps",
                               "pltcmd=$fluctpltcmd",
                               "binmode=DEFAULT",
                               "sensecase=no",
                             );
    if ( $status ) {
      ahlog::ah_err "fplot failed.";
      return $status;
    }
  }


  return 0;

}

# ------------------------------------------------------------------------------

sub finalize {

  my $status = 0;

  return 0;
  
}

# ------------------------------------------------------------------------------
