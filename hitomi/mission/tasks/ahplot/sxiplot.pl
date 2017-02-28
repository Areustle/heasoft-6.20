#!/usr/bin/perl
#
# File name: sxiplot.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/09/12 20:21:59 $
# Version: 0
#
# Update: 2016/08/16
# Author: J. Eggen NASA GSFC
# Changes: Added behavior to allow creation of lightcurve plots in the 
# event that minus day-earth files are not present.
#
# Plot the lightcurves of the normal SXI events, minus day-earth SXI events
# and the SAA for the SXI
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
use ahplot qw (:ALL) ;
use File::Copy;

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

# parameters
my $infile1    ="";              # Input unfiltered event file
my $infile2    ="";              # Input cleaned event file
my $ehkfile    ="";              # Input EHK file                                 
my $saaplot    ="";              # Output SXI SAA plot file
my $gtifile    ="";              # Input GTI file(s) to overlay on figures (single file or filelist)
my $lcthresh   ="";              # Lightcurve threshold
my $binlc      ="";              # Bin size for lightcurve

my $ehkcol     =0;
my @saa;                         # relevant column data for saa
my $saavalid   =0;               # boolean which determines if the SAA is present 
my $dyevalid   =0;               # boolean which determines if the DYE files are present
my $tstart     =0;               # TSTART ehk file
my $tstop      =0;               # TSTOP ehk file
my $mjdrefi    =0;               # MJDREFI from ehk file
my $mjdreff    =0;               # MJDREFF from ehk file
my $timedel    =0;               # TIMEDEL ehk file
my $nbint      =0;               # Parameter for lcurve determines plotting time range
my $numgtifiles=0;               # number of GTI files given
my $pltcmd     ="quit";          # plot formatting for fplot

my $sxiploterror = 0;            # sxiplot exit status

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

$sxiploterror = get_parameters () ;
unless ( $sxiploterror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($sxiploterror);
}

$sxiploterror = initialize () ;
unless ( $sxiploterror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($sxiploterror);
}

$sxiploterror = do_work () ;
unless ( $sxiploterror == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($sxiploterror);
}

$sxiploterror = finalize () ;
unless ( $sxiploterror == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($sxiploterror);
}

# We're done.
ahapp::end_processing($sxiploterror);


#########################
# Subroutines
#########################

sub get_parameters {

  $infile1    = ahapp::query_parameter("infile1");
  $infile2    = ahapp::query_parameter("infile2");
  $ehkfile    = ahapp::query_parameter("ehkfile");
  $saaplot    = ahapp::query_parameter("saaplot");
  $gtifile    = ahapp::query_parameter("gtifile");
  $lcthresh   = ahapp::query_parameter("lcthresh");
  $binlc      = ahapp::query_parameter("binlc");

  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status = 0;

  # Input file checking

  # Check the normal sxi event file(s)
  if ($infile1 =~ /^@/) {
    if(isRequiredFileNotFound(substr($infile1,1))) { return 1; } 
  }

  # Load the input SXI normal event files into an array
  # and make sure each file exists
  my @in_sxi_files = readInputFileList($infile1);
  unless(@in_sxi_files) {
    ahgen::ah_err "No input SXI normal event files.";
    return 1;
  }
  if (isRequiredFileNotFound(@in_sxi_files)) { return 1; }

  # Check the minus day earth sxi event file(s)
  if ($infile2 =~ /^@/) {
    if(isRequiredFileNotFound(substr($infile2,1))) { return 1; } 
  }

  # Load the input SXI normal event files into an array
  # and make sure each file exists
  my @in_sxi_mdye_files = readInputFileList($infile2);
  unless(@in_sxi_mdye_files) {
     ahlog::ah_info "HIGH", "No input SXI minus day-earth event files.\n";
  }

  # Don't throw a fatal error of no SXI DYE files are found. Instead, set
  # $dyevalid = 0 so we can simply not plot that later.
  my $in_sxi_mdye_files_count;
  $in_sxi_mdye_files_count =  scalar @in_sxi_mdye_files;
  if ($in_sxi_mdye_files_count == 0) {
    $dyevalid = 0; 
  } else {
    $dyevalid = 1;
  }

  # Check if the EHK file exists
  if (isRequiredFileNotFound($ehkfile)) { 
    ahlog::ah_err "Required EHK file not found: $ehkfile\n"; 
    return 1;
  }
  if (removeOutputFileIfClobbering($saaplot,$ahapp::clobber) ) { return 1; }

  # Read timing keywords from ehk file and use them to calculate
  # the nbint parameter for lcurve; MJDREF* keywords used later in doWork
  $tstart = ahgen::get_keyword($ehkfile,"EHK","TSTART");
  $tstop = ahgen::get_keyword($ehkfile,"EHK","TSTOP");
  $mjdrefi = ahgen::get_keyword($ehkfile,"EHK","MJDREFI");
  $mjdreff = ahgen::get_keyword($ehkfile,"EHK","MJDREFF");

  $timedel = ahgen::get_keyword($ehkfile,"EHK","TIMEDEL");
  if( ahgen::get_error_flag ) { 
    ahlog::ah_err "TIMEDEL keyword not defined in $ehkfile"; 
    return ahgen::get_error_flag;
  }
  $nbint = (($tstop - $tstart)/$binlc) + 10;

  # Read SAA column from EHK file; check for any valid SAA
  @saa = ahgen::read_column($ehkfile,"EHK","SAA_SXI");
  for (my $ii = 0; $ii < scalar(@saa); ++$ii) {
    if ($saa[$ii] == 1) {   
      $saavalid = 1;
      last;
    }
  }

  # Get the column number for the EHK column SAA_SXI
  $ehkcol = ahgen::get_column_num($ehkfile,"EHK","SAA_SXI");
  unless ( $ehkcol ) {
    ahgen::ah_err "Could not find column SAA_SXI in file $ehkfile\[EHK]";
    return 1;
  }

  # Check that GTI files exist
  $numgtifiles=0;
  if (uc $gtifile ne "NONE") {
    if ($gtifile =~ /^@/) {
      if(isRequiredFileNotFound(substr($gtifile,1))) { return 1; } 
    }
    my @gtifiles = readInputFileList($gtifile);
    unless(@gtifiles) {
       ahlog::ah_info "HIGH", "No GTI input file(s) found.\n";
       $gtifile="NONE";
    }
    if (uc $gtifile ne "NONE") {
      if (isRequiredFileNotFound(@gtifiles)) { return 1; }
    }
    $numgtifiles=scalar(@gtifiles);
  }

  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;

  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $status = 0;
  my $maxpts = 0;

  # Set up the temporary file names
  my $sxi_norm_lc = "sxi_norm.lc";
  my $sxi_mdye_lc = "sxi_mdye.lc";
  my $sxi_saa_lc = "sxi_saa.lc";
  my $plotfile = "toplot.fits";
  my $filename  = "";

  my @cfiles=();           # files to be plotted
  my $yparm = "";          # list of columns to be plotted
  my $colmap = "";         # expression for renaming columns before plotting

  ahapp::add_temp_file($sxi_norm_lc);
  ahapp::add_temp_file($sxi_mdye_lc);
  ahapp::add_temp_file($sxi_saa_lc);
  ahapp::add_temp_file($plotfile);

  ########################
  # Extract lightcurves
  ########################

  # Extract the general SXI lightcurve
  $status = ahgen::run_ftool("extractor",
                             "filename=$infile1\[PI= 0: 4095]",
                             "eventsout=NONE", "regionfile=NONE", "qdpfile=NONE",
                             "fitsbinlc=$sxi_norm_lc",
                             "lcthresh=$lcthresh",
                             "lctzero=no",
                             "xronwn=NONE", "unbinlc=NONE", "phafile=NONE", "imgfile=NONE", "timefile=NONE",
                             "adjustgti=no", "gstring=NONE", "timeorder=no",
                             "xcolf=X",
                             "ycolf=Y",
                             "xcolh=DETX", 
                             "ycolh=DETY",
                             "xfkey=TLMAX", "yfkey=TLMAX", "xhkey=TLMAX", "yhkey=TLMAX", "phamax=TLMAX",
                             "specbin=1", "binh=1", "binf=1",
                             "binlc=$binlc",
                             "tcol=TIME",
                             "ecol=PI",
                             "ccol=NONE",
                             "gcol=GRADE",
                             "gti=GTI", "events=EVENTS",
                             "gtitxt=NONE", "timeref=40000.00",
                             "wtmapb=no", "wtmapfix=yes", "swmapx=no", "swmapy=no", "wmapver=2",
                             "gtinam=GTI",
                             "exitnow=no");

  if ( $status ) {
    ahlog::ah_err "extractor failed on infile1.";
    return $status;
  }

  # Extract the SXI minus day-earth lightcurve... 
  # UNLESS we don't have any DYE files to work with.
  if ($dyevalid) {
    $status = ahgen::run_ftool("extractor",
                               "filename=$infile2\[PI= 0: 4095]",
                               "eventsout=NONE", "regionfile=NONE", "qdpfile=NONE",
                               "fitsbinlc=$sxi_mdye_lc",
                               "lcthresh=$lcthresh",
                               "lctzero=no",
                               "xronwn=NONE", "unbinlc=NONE", "phafile=NONE", "imgfile=NONE", "timefile=NONE",
                               "adjustgti=no", "gstring=NONE", "timeorder=no",
                               "xcolf=X",
                               "ycolf=Y",
                               "xcolh=DETX", 
                               "ycolh=DETY",
                               "xfkey=TLMAX", "yfkey=TLMAX", "xhkey=TLMAX", "yhkey=TLMAX", "phamax=TLMAX",
                               "specbin=1", "binh=1", "binf=1",
                               "binlc=$binlc",
                               "tcol=TIME",
                               "ecol=PI",
                               "ccol=NONE",
                               "gcol=GRADE",
                               "gti=GTI", "events=EVENTS",
                               "gtitxt=NONE", "timeref=40000.00",
                               "wtmapb=no", "wtmapfix=yes", "swmapx=no", "swmapy=no", "wmapver=2",
                               "gtinam=GTI",
                               "exitnow=no");
  } else {
    ahlog::ah_info "HIGH", "No minus day-earth files... Skipping creation of minus day-earth lightcurve!\n";
  }

  if ( $status ) {
    ahlog::ah_err "extractor failed on infile2.";
    return $status;
  }

  if ($saavalid) {
    if ($dyevalid) {
      $yparm = "RATENORMAL RATEMDYE SXI_SAA";
      $colmap = "col RATENORMAL=RATE1;RATEMDYE=RATE2;SXI_SAA=RATE3;";
      push @cfiles, $sxi_norm_lc;
      push @cfiles, $sxi_mdye_lc;
      push @cfiles, "$ehkfile vy$ehkcol";
    } else {
      $yparm = "RATENORMAL SXI_SAA";
      $colmap = "col RATENORMAL=RATE1;SXI_SAA=RATE2;";
      push @cfiles, $sxi_norm_lc;
      push @cfiles, "$ehkfile vy$ehkcol";
    }
  } else {
    if ($dyevalid) {
      $yparm = "RATENORMAL RATEMDYE";
      $colmap = "col RATENORMAL=RATE1;RATEMDYE=RATE2;";
      push @cfiles, $sxi_norm_lc;
      push @cfiles, $sxi_mdye_lc;
    } else {
      $yparm = "RATENORMAL";
      $colmap = "col RATENORMAL=RATE1;";
      push @cfiles, $sxi_norm_lc;
    }
  }

  my $ncfiles=scalar(@cfiles);
  my $cfile1=$cfiles[0];
  my $cfile2="";
  if ($ncfiles > 1) { $cfile2=$cfiles[1]; }
  my $cfile3="";
  if ($ncfiles > 2) { $cfile3=$cfiles[2]; }
  $status = ahgen::run_ftool("lcurve",
                             "nser=$ncfiles",
                             "cfile3=$cfile3",
                             "cfile2=$cfile2",
                             "cfile1=$cfile1",
                             "window=-",
                             "dtnb=$binlc",
                             "nbint=$nbint",
                             "tunits=2",
                             "plot=n",
                             "plotdev=/XW",
                             "plotdnum=4",
                             "outfile=$sxi_saa_lc");
  if ( $status ) {
    ahlog::ah_err "lcurve failed.";
    return $status;
  }

  # lcurve will shift the TIME scale so that the first bin in the output 
  # file has TIME=0.  The amount of shift applied is stored in the TSTARTI
  # and TSTARTF keywords in the TJD format.  Need to convert this number
  # into seconds so that any GTI can be shifted by the same amount.
  my $tstarti=ahgen::get_keyword($sxi_saa_lc,"RATE","TSTARTI");
  my $tstartf=ahgen::get_keyword($sxi_saa_lc,"RATE","TSTARTF");
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


  # Prepare lcurve output for plotting in 3 steps:
  #  1. copy TIME column only
  #  2. insert DX column (needed if GTI are being plotted)
  #  3. paste remainig columns to be plotted
  $filename=$sxi_saa_lc."[RATE][col TIME]";
  $status=ahgen::run_ftool("ftcopy",
                           "infile=$filename",
                           "outfile=$plotfile",
                           "copyall=no",
                           "clobber=yes",
                          );
  if ( $status ) {
    ahlog::ah_err "ftcopy failed; plot preparation step 1.";
    return $status;
  }

  $status=ahgen::run_ftool("ftcalc",
                           "infile=${plotfile}[RATE]",
                           "outfile=$plotfile",
                           "column=DX",
                           "expression=0.",
                           "tform=D",
                           "clobber=yes",
                          );
  if ( $status ) {
    ahlog::ah_err "ftcalc failed; plot preparation step 2.";
    return $status;
  }

  $filename=$sxi_saa_lc."[RATE][$colmap]";
  $status=ahgen::run_ftool("ftpaste",
                           "infile=$plotfile",
                           "pastefile=$filename",
                           "outfile=$plotfile",
                           "copyall=no",
                           "clobber=yes",
                          );
  if ( $status ) {
    ahlog::ah_err "ftpaste failed; plot preparation step 3.";
    return $status;
  }

  # Append GTI data
  if (uc $gtifile ne "NONE") {

    # Read RATE columns to get span of values for determining vertical
    # placement of GTI intervals; y1pars contains three items: 
    # (min(@y1)-0.1*dy, max(@y1)+0.1*dy, dy) where dy=max(@y1)-min(@y1)
    my @y1=();
    my @y1pars=();
    my @y2=();
    my @y2pars=();
    my @y3=();
    my @y3pars=();

    my @ymin=();
    my @ymax=();
    my @ypos0=();
    my @deltay=();
    my @ycols=();

    @y1=ahgen::read_column($plotfile,"RATE","RATENORMAL");
    @y1pars=ahplot::calculate_gti_y_position_parameters(@y1);
    push @ymin, $y1pars[0];
    push @ymax, $y1pars[1]+(($numgtifiles+1)*$y1pars[2]);
    push @ypos0, $y1pars[1];
    push @deltay, $y1pars[2];
    push @ycols, 'RATENORMAL';

    @y2=();
    @y2pars=();
    if ($dyevalid) {
      @y2=ahgen::read_column($plotfile,"RATE","RATEMDYE");
      @y2pars=ahplot::calculate_gti_y_position_parameters(@y2);
      push @ymin, $y2pars[0];
      push @ymax, $y2pars[1]+(($numgtifiles+1)*$y2pars[2]);
      push @ypos0, $y2pars[1];
      push @deltay, $y2pars[2];
      push @ycols, 'RATEMDYE';
    }

    if ($saavalid) {
      @y3=ahgen::read_column($plotfile,"RATE","SXI_SAA");
      @y3pars=ahplot::calculate_gti_y_position_parameters(@y3);
      push @ymin, $y3pars[0];
      push @ymax, $y3pars[1]+(($numgtifiles+1)*$y3pars[2]);
      push @ypos0, $y3pars[1];
      push @deltay, $y3pars[2];
      push @ycols, 'RATENORMAL';
    }

    # Construct pco file so that fplot displays the GTI properly
    my $pltfile    ="ahplot.pco";
    ahapp::add_temp_file($pltfile);
    ahplot::write_gti_pco($saaplot,"no","no","0","0",\@ymin,\@ymax);
    $pltcmd = "\@$pltfile";

    # prepare FITS file with GTI data formatted for plotting
    my $gti_plot_file="gti_plot.fits";
    ahapp::add_temp_file($gti_plot_file);
    ahplot::prepare_gti_plot_data($gtifile,"RATE","TIME","DX",$gti_plot_file,\@ypos0,\@deltay,\@ycols);

    # shift GTI TIMEs by lcurve offset time
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

    # merge GTI onto data file
    $status=ahgen::run_ftool("ftmerge",
                             "infile=$plotfile,$gti_plot_file",
                             "outfile=$plotfile",
                             "clobber=yes");
    if ($status) {
      ahlog::ah_err "ftmerge failed; could not merge GTI to data file.";
      return $status;
    }
  }

  # Get number of rows in temporary file to pass to fplot
  $maxpts = ahgen::get_keyword($plotfile,"RATE","NAXIS2");

  ########################
  # Plot the SXI data
  ########################

  # Run FPLOT to create the SXI postscript file
  $status = ahgen::run_ftool("fplot",
                             "infile=${plotfile}[RATE]",
                             "maxpts=$maxpts",
                             "xparm=TIME[DX]",
                             "yparm=$yparm",
                             "rows=-",
                             "device=$saaplot/cps",
                             "pltcmd=$pltcmd",
                             "binmode=DEFAULT",
                             "sensecase=no",
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
