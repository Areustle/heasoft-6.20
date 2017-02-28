#!/usr/bin/perl
#
# File name: sxigainfit, hxigainfit and sgdgainfit
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/01/28 15:10:56 $
# Version: 0
#
# ahgengainfit generates a trend file from a list of event files.
#
# Tool Dependencies:
#   ahgtigen
#   ahscreen
#   ahgainfit
# 
# Library Dependencies:
#   gen/lib/perl/ahlog
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
#   gen/lib/perl/ahfilterlib
#
# Modification History:
#

# Set up
use strict;
use warnings;

use ahlog ;
use ahgen qw (:ALL);
use ahapp ;
use File::Basename;

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

our $infile;
our $outevtsuffix;
our $outfile;

# option pars
our $rungtigen;
our $runscreen;

our %ahgtigen_pars;
our %ahscreen_pars;
our %ahgainfit_pars;
our %sxipi_pars;

# ahgtigen/ahscreen pars
our $gtigen_infile;
our $gtifile;
our $selectfile;
our $gtigenlabel;
our $screenlabel;
our $gtiexpr;
our $expr;

# sxi parameters
our $runselcal;
our $runsxipi;
our $hkfile;
our $hkext;
our $hkcolstem;
our $hkvideoid;
our $vtevnoddfile;
our $chtrailfile;
our $ctifile;
our $spthfile;
our $gainfile;
our $patternfile;
our $randomize;
our $seed;

# other shared parameters
our @in_evt_files = ();      # array of input event files
our @evt_files = ();         # array of output event files

our $instrume = "";
our $datamode = "";
our $detnam = "";

our $gainfiterror = 0;

#############
# HXI SGD SXI
#############
my $taskName = basename( $0 );
our $isHXI = $taskName =~ /^hxi/;
our $isSGD = $taskName =~ /^sgd/;
our $isSXI = $taskName =~ /^sxi/;

#########################
#  Main Code Block 
#########################

# Query canonical APE parameters and start logging. 
ahapp::startup () ;

# Pre-Processing
ahapp::begin_processing();

# Get the input parameters
$gainfiterror = GetInputParameters();
unless ( $gainfiterror == 0 ) {
  ahlog::ah_debug "GetInputParameters" ;
  ahapp::end_processing($gainfiterror);
}

# Check the input files for clobber
$gainfiterror = CheckInput();
unless ( $gainfiterror == 0 ) {
  ahlog::ah_debug "CheckInput" ;
  ahapp::end_processing($gainfiterror);
}

# Write all parameters to this script to the log file.
ah_info "HIGH", ahapp::write_parameters () ;                 

# Main Processing 

# Check the input files for clobber
$gainfiterror = GenerateGTI();
unless ( $gainfiterror == 0 ) {
  ahlog::ah_debug "GTIGen" ;
  ahapp::end_processing($gainfiterror);
}

# Check the input files for clobber
$gainfiterror = ScreenEvents();
unless ( $gainfiterror == 0 ) {
  ahlog::ah_debug "ScreenEvents" ;
  ahapp::end_processing($gainfiterror);
}

if($isSXI) {
  # Check the input files for clobber
  $gainfiterror = FilterCalSource();
  unless ( $gainfiterror == 0 ) {
    ahlog::ah_debug "FilterCalSource" ;
    ahapp::end_processing($gainfiterror);
  }
}

if($isSXI) {
  # Check the input files for clobber
  $gainfiterror = CalcPI();
  unless ( $gainfiterror == 0 ) {
    ahlog::ah_debug "CalcPI" ;
    ahapp::end_processing($gainfiterror);
  }
}

# Check the input files for clobber
$gainfiterror = FitGain();
unless ( $gainfiterror == 0 ) {
  ahlog::ah_debug "FitGain" ;
  ahapp::end_processing($gainfiterror);
}

# Verify file is okay
$gainfiterror = Finalize();
unless ( $gainfiterror == 0 ) {
  ahlog::ah_debug "Finalize" ;
  ahapp::end_processing($gainfiterror);
}

ahlog::ah_info "HIGH", "Successfully created FITS file: $outfile" ;    

# We're done.
ahapp::end_processing();

############################
# Pre-Processing Functions 
############################

# 
# Description: 
#
# Get parameter values from the command line and par file
#
sub GetInputParameters {

  $infile           = ahapp::query_parameter("infile");
  $outevtsuffix       = ahapp::query_parameter("outevtsuffix");
  $outfile          = ahapp::query_parameter("outfile");

  # option pars
  $rungtigen        = ahapp::query_parameter("rungtigen",1);
  $runscreen        = ahapp::query_parameter("runscreen",1);

  # ahgtigen/ahscreen pars
  $gtigen_infile    = ahapp::query_parameter("gtigen_infile");
  $gtifile          = ahapp::query_parameter("gtifile");
  $selectfile       = ahapp::query_parameter("selectfile");
  $gtigenlabel      = ahapp::query_parameter("gtigenlabel");
  $screenlabel      = ahapp::query_parameter("screenlabel");
  $gtiexpr          = ahapp::query_parameter("gtiexpr");
  $expr             = ahapp::query_parameter("expr");

  # ahgainfit pars
  $ahgainfit_pars{linefitfile}    = ahapp::query_parameter("linefitfile");
  $ahgainfit_pars{linetocorrect}  = ahapp::query_parameter("linetocorrect");
  $ahgainfit_pars{energycol}      = ahapp::query_parameter("energycol");
  $ahgainfit_pars{splitcol}       = ahapp::query_parameter("splitcol");
  $ahgainfit_pars{numevent}       = ahapp::query_parameter("numevent");
  $ahgainfit_pars{minevent}       = ahapp::query_parameter("minevent");
  $ahgainfit_pars{startenergy}    = ahapp::query_parameter("startenergy");
  $ahgainfit_pars{stopenergy}     = ahapp::query_parameter("stopenergy");
  $ahgainfit_pars{broadening}     = ahapp::query_parameter("broadening");
  $ahgainfit_pars{extraspread}    = ahapp::query_parameter("extraspread");
  $ahgainfit_pars{evchannel}      = ahapp::query_parameter("evchannel");
  $ahgainfit_pars{binwidth}       = ahapp::query_parameter("binwidth");
  $ahgainfit_pars{gridprofile}    = ahapp::query_parameter("gridprofile");
  $ahgainfit_pars{fitwidth}       = ahapp::query_parameter("fitwidth");
  $ahgainfit_pars{background}     = ahapp::query_parameter("background");
  $ahgainfit_pars{spangti}        = ahapp::query_parameter("spangti");
  $ahgainfit_pars{gapdt}          = ahapp::query_parameter("gapdt");
  $ahgainfit_pars{grpoverlap}     = ahapp::query_parameter("grpoverlap");
  $ahgainfit_pars{avgwinrad}      = ahapp::query_parameter("avgwinrad");
  $ahgainfit_pars{calcerr}        = ahapp::query_parameter("calcerr");
  $ahgainfit_pars{writeerrfunc}   = ahapp::query_parameter("writeerrfunc");
  $ahgainfit_pars{minwidth0}      = ahapp::query_parameter("minwidth0");
  $ahgainfit_pars{maxitcycle}     = ahapp::query_parameter("maxitcycle");
  $ahgainfit_pars{r2tol}          = ahapp::query_parameter("r2tol");
  $ahgainfit_pars{searchstepshift}= ahapp::query_parameter("searchstepshift");
  $ahgainfit_pars{maxdshift}      = ahapp::query_parameter("maxdshift");
  $ahgainfit_pars{bisectolshift}  = ahapp::query_parameter("bisectolshift");
  $ahgainfit_pars{searchstepwidth}= ahapp::query_parameter("searchstepwidth");
  $ahgainfit_pars{maxdwidth}      = ahapp::query_parameter("maxdwidth");
  $ahgainfit_pars{bisectolwidth}  = ahapp::query_parameter("bisectolwidth");
  $ahgainfit_pars{minwidth}       = ahapp::query_parameter("minwidth");
  $ahgainfit_pars{nerrshift}      = ahapp::query_parameter("nerrshift");
  $ahgainfit_pars{nerrwidth}      = ahapp::query_parameter("nerrwidth");
  $ahgainfit_pars{shifterrfac}    = ahapp::query_parameter("shifterrfac");
  $ahgainfit_pars{widtherrfac}    = ahapp::query_parameter("widtherrfac");

  # Query for SXI specific parameters
  if($isSXI) {
    # option pars
    $runselcal    = ahapp::query_parameter("runselcal",1);
    $runsxipi     = ahapp::query_parameter("runsxipi");

    # sxipi parameters
    $hkfile       = ahapp::query_parameter("hkfile");
    $hkext        = ahapp::query_parameter("hkext");
    $hkcolstem    = ahapp::query_parameter("hkcolstem");
    $hkvideoid    = ahapp::query_parameter("hkvideoid");
    $vtevnoddfile = ahapp::query_parameter("vtevnoddfile");
    $chtrailfile  = ahapp::query_parameter("chtrailfile");
    $ctifile      = ahapp::query_parameter("ctifile");
    $spthfile     = ahapp::query_parameter("spthfile");
    $gainfile     = ahapp::query_parameter("gainfile");
    $patternfile  = ahapp::query_parameter("patternfile");
    $randomize    = ahapp::query_parameter("randomize");
    $seed         = ahapp::query_parameter("seed");

  }

  return 0;

} # End GetInputParameters

#
# Description:
#
# Check if the output file(s) already exist.  Unless clobber is set, this will
# cause the script to fail. Create output event file(s)
#
# Parameters:
# [in] infile         Input event file
# [in] in_evt_files   Array of input event files
# [in] outfile        Output trend file
# [in] expr           ahscreen: Additional expression to select good event or NONE
# [in] gtifile        Input GTI file
# [in] rungtigen      Option to run ahgtigen (yes/no)
# [in] runscreen      Option to run ahscreen (yes/no)
# [in] instrume       Instrument to run tool with
# [in] datamode       Datamode from event file
# [in] detname        Detname from event file
# [in] evt_files      Array of output event files
# [in] evtfileroot    Output file name (or root)
# [out] status        Output status
#
sub CheckInput {

  my $status = 0;

  ahlog::ah_out "Checking input\n";

  # Set up instrument specific variables
  if($isHXI+$isSGD+$isSXI > 1) {ahlog::ah_err "This shouldn't happen"; return 1;}
  if($isHXI) {
    $instrume = qr/HXI[1-2]?|HX[1-2]/;
  } elsif ($isSGD) {
    $instrume = qr/SGD[1-2]?|SG[1-2]/;
  } elsif ($isSXI) {
    $instrume = qr/SXI/;
  } else {
    ahlog::ah_err "This shouldn't happen"; return 1;
  }

  # get list of files from infile parameter
  if ($infile =~ /^@/) {
    if(isRequiredFileNotFound(substr($infile,1))) { return 1; } 
  }
  @in_evt_files = readInputFileList($infile);
  if(isRequiredFileNotFound(@in_evt_files)) { return 1; }

  # Check if the output file already exists.  Unless clobber is set, this will
  # cause the script to fail.
  if(removeOutputFileIfClobbering($outfile,$ahapp::clobber)) { return 1; }

  # Nothing to do in ahscreen if neither an expression or GTI file
  if($expr =~ /none/i and $gtifile =~ /none/i and !$rungtigen) { $runscreen = 0; }

  # Check valid instrument and set up output event file names if we are cleaning 
  # the event files
  foreach my $evtfile (@in_evt_files) {

    # check instrument is HXI or SGD
    my $file_instrume = ahgen::get_keyword($evtfile,"EVENTS","INSTRUME");
    if(ahgen::get_error_flag) {
      ahlog::ah_err "In file $evtfile, Keyword INSTRUME not found.\n";
      ahlog::ah_debug "ahgen::get_keyword";
      return 1;
    }

    if($file_instrume !~ /$instrume/i) {
      ahlog::ah_err "Invalid instrument ($file_instrume) in file $evtfile";
      ahlog::ah_err "Instrument should be $instrume, " . $instrume . "1 or " . $instrume . "2.\n";
      return 1;
    }

    # check detnam is the same for all files
    my $file_detnam = ahgen::get_keyword($evtfile,"EVENTS","DETNAM");
    if(ahgen::get_error_flag) {
      ahlog::ah_err "In file $evtfile, keyword DETNAM not found.\n";
      return ahgen::get_error_flag;
    }
    if($detnam eq "") { $detnam = $file_detnam; }
    if($detnam ne $file_detnam) {
      ahlog::ah_err "In file $evtfile, keyword DETNAM does not match $detnam";
      return 1;
    }

    # check DATAMODE is the same for all input event files
    my $file_datamode = ahgen::get_keyword($evtfile,"EVENTS","DATAMODE");
    if(ahgen::get_error_flag) {
      ahlog::ah_err "In file $evtfile, keyword DATAMODE not found.\n";
      return ahgen::get_error_flag;
    }
    if($datamode eq "") { $datamode = $file_datamode; }
    if($file_datamode ne $datamode) {
      ahlog::ah_err "Invalid DATAMODE. Datamode should be $datamode\_*, not $file_datamode\n";
      return 1;
    }

    # Event outfile name setup does not matter if we aren't running ahscreen
    # Just use the input files.
    unless($runscreen) { push @evt_files, $evtfile; next; }

    # Search for ff and replace with cl in input event file
    my $TmpOutfile = $evtfile;
    $TmpOutfile =~ s/\.evt.*$/$outevtsuffix/g;

    # Check if the output file already exists.  Unless clobber is set, this will
    # cause the script to fail.
    if(removeOutputFileIfClobbering($TmpOutfile,$ahapp::clobber)) { return 1; }
    if(copyFITSFile($evtfile, $TmpOutfile)) { return 1; }

    # Put the file names in the processing array. ahscreen will create the files.
    push @evt_files, $TmpOutfile;

  }
  ahlog::ah_info "HIGH", "Event files to process: ";
  ahlog::ah_info "HIGH", "  $_" foreach (@evt_files);

  return $status;

} # End CheckInput

############################
#      Main Processing
############################

#
# Description:
#
# Use ahgtigen to generate a GTI file based on CALDB label for gain fitting
#
# Parameters:
# [in] rungtigen      Option to run ahgtigen (yes/no)
# [in] gtigen_infile  Input MKF file
# [in] gtifile        Input GTI file
# [in] instrume       Instrument to run tool with
# [in] selectfile     Input expression file (or NONE, CALDB)
# [in] gtigenlabel    GTI screening expression label in selection file
# [in] gtiexpr        ahgtigen: Expression to create GTI or label for existing expression
# [out] status        Output status
#
sub GenerateGTI {
  
  my $TmpGtiFile = "";
  my $status = 0;

  # Run ahgtigen (optional)
  unless ($rungtigen) { return 0; }
  ahlog::ah_out "Generating GTI";

  $TmpGtiFile = "ahgtigen.tmp";
  ahapp::add_temp_file($TmpGtiFile);

  # Run ahgtigen 
  if(run_ahgtigen($TmpGtiFile)) { return 1; }

  # This is the new GTI file, set as the parameter
  $gtifile = $TmpGtiFile;

  return $status;

}

#
# Description:
#
# Use ahscreen to filter events based on CALDB label for gain fitting
#
# Parameters:
# [in] runscreen      Option to run ahscreen (yes/no)
# [in] in_evt_files   Array of input event files
# [in] evtfileroot    Output file name (or root)
# [in] gtifile        Input GTI file
# [in] selectfile     Input expression file (or NONE, CALDB)
# [in] screenlabel    GTI screening expression label in selection file
# [in] expr           ahscreen: Additional expression to select good event or NONE
# [out] status        Output status
#
sub ScreenEvents {

  my $TmpOutfile = "";
  my $status = 0;

  # Run ahscreen (optional)
  unless ($runscreen) { return $status; }

  ahlog::ah_out "Screening Events";

  # Run ahscreen to clean events on event files
  # Since the hxi/sgd files can be large, we do not want
  # to merge the events. 
  foreach my $evtfile (@evt_files) {
    ahlog::ah_info "HIGH", "  Screening events for file $evtfile";
    if(run_ahscreen($evtfile)) { return 1; }
  }

  # GTI file is no longer needed
  $gtifile = "none";

  return 0;

}

#
# Description:
#
# Use ftselect to filter non-calibration source events
#
# Parameters:
# [in] runselcal      Run calibration filtering (yes, no)
# [in] outevtsuffix     Output event file
# [out] status        Output status
#
sub FilterCalSource {

  my $TmpInfile = "";
  my $TmpOutfile = "";
  my $status = 0;
  my $calsrcexpr = "STATUS[2]==b1";

  unless ($runselcal) { return 0; }
  ahlog::ah_out "Filtering non-calibration source events\n";
  ahlog::ah_out "Using STATUS column flags STATUS[2]";

  # Filter event file on cal source
  foreach my $sxifile (@evt_files) {
    ahlog::ah_info "HIGH", "  Selecting cal source events on file $sxifile";
    $TmpOutfile = $sxifile .".ftselect.tmp";
    ahapp::add_temp_file($TmpOutfile);

    # Use the region filter on the output nxb events
    if(run_ftselect($TmpInfile,$TmpOutfile.".calsrc",$calsrcexpr)) { return 1; };
    if(copyOrMoveFileBasedOnCleanup($TmpOutfile.".calsrc",$TmpOutfile,$ahapp::cleanup)) { return 1; }

  }

  return 0;
}

#
# Description:
#
# Use sxipi in one of two modes: [F]ull or [P]artial. If running in 
# partial mode, sxipi will be run with even/odd and grade corrections
# turned on and charge trail/CTI corrections turned off.
#
# Parameters:
# [in] runsxipi       Run sxipi (PARTIAL, FULL, no)
# [in] hkfile         Output HK file
# [in] hkext          HK extension with video temperatures
# [in] hkcolstem      Column template for video temperatures
# [in] hkvideoid      Video card ID for gain correction of CCD1-4
# [in] vtevnoddfile   Input video evenodd file (or CALDB, NONE)
# [in] chtrailfile    Input charge trail correction file (or CALDB, NONE)
# [in] ctifile        Input CTI correction file (or CALDB, NONE)
# [in] spthfile       Input split threshold file (or CALDB, NONE)
# [in] gainfile       Input gain correction file (or CALDB)
# [in] patternfile    Input grade hit pattern file (or CALDB, NONE)
# [out] status        Output status
#
sub CalcPI {

  my $status = 0;

  if ($runsxipi =~ /^n/i) { return 0; }
  ahlog::ah_out "Calculating PI\n";

  # Running SXIPI
  # if flag is set to PARTIAL:
  # Run sxipi with even/odd and grade corrections
  # Run with charge trail and CTI corrections off
  #
  # If flag is set to full
  # Run sxipi with all corrections turned on

  my $chtrailcor="";
  my $cticor="";
  my $spthiter="";
  my $spthcaldb="";

  if( $runsxipi =~ /^p/i ) {
    $chtrailcor="no";
    $cticor="no";
    $spthiter="no";
    $spthcaldb="no";
  } elsif ( $runsxipi =~ /^f/i ) {
    $chtrailcor="yes";
    $cticor="yes";
    $spthiter="yes";
    $spthcaldb="yes";
  } else {
    ahlog::ah_err "Invalid option for parameters runsxipi. Options are";
    ahlog::ah_err "NONE, PARTIAL, or FULL";
    return 1;
  }

  # Loop through input files, run sxipi
  foreach my $sxifile (@evt_files) {
    ahlog::ah_info "HIGH", "  Running sxipi on file $sxifile";
    if(run_sxipi($sxifile,$chtrailcor,$cticor,$spthiter,$spthcaldb)) { return 1; }
  }

  return 0;

}

#
# Description:
#
# Use ahgainfit create gain trend file
#
# Parameters:
# [in] evt_files       Array of output event files
# [in] outfile         Output trend file
# [in] linefitfile     Input calibration line file (or CALDB)
# [in] linetocorrect   Line to fit (HDU name in linefitfile)
# [in] energycol       Energy column to fit
# [in] splitcol        Column used to separate data
# [in] numevent        Maximum number of events in a single spectrum
# [in] minevent        Minimum number of events in a single spectrum
# [in] gapdt           Time [s] between events to define a gap (or <0)
# [in] grpoverlap      Percentage of overlap between adjacent groups
# [in] startenergy     Start energy [eV] of bin mesh (-1 = automatic)
# [in] stopenergy      Stop energy [eV] of bin mesh (-1 = automatic)
# [in] extraspread     Extend bin mesh energy range [eV]
# [in] evchannel       Conversion factor from channel (energycol) to energy [eV]
# [in] binwidth        Spectrum energy bin width [evchannel]
# [in] broadening      FHWM Gaussian broadening of calibration line profile [eV]
# [in] gridprofile     Calculate only the grid profile (yes/[no])
# [in] fitwidth        Fit spectrum width (yes/no)
# [in] background      Fitted background type (NONE, CONST, SLOPE)
# [in] spangti         Ignore GTI boundaries when binning spectra ([yes]/no)
# [in] avgwinrad       Radius of interval [binwidth] used to update average (-1 for auto)
# [in] minwidth0       Smallest allowed initial value in width fitting [binwidth]
# [in] maxitcycle      Maximum number of fitting iterations
# [in] r2tol           Convergence criterion for R^2
# [in] searchstepshift Step size when fitting shift [binwidth]
# [in] maxdshift       Largest allowed deviation from initial guess of shift [binwidth]
# [in] bisectolshift   Tolerance of shift to stop bisection method [binwidth]
# [in] searchstepwidth Step size when fitting width [binwidth]
# [in] maxdwidth       Largest allowed deviation from initial guess of width [binwidth]
# [in] bisectolwidth   Tolerance of width to stop bisection method [binwidth]
# [in] minwidth        Smallest width to allow in width fitting [binwidth]
# [out] status         Output status
#
sub FitGain {

  my $status = 0;

  ahlog::ah_out "Creating gainfit file\n";

  my $ahgainfit_in = "ahgainfit.in";
  ahapp::add_temp_file( $ahgainfit_in );
  if(@evt_files>1) {
    # Make a list of input files
    open(FH,">",$ahgainfit_in);
    printf FH join("\n",@evt_files);
    close FH;
    $ahgainfit_in = "\@$ahgainfit_in";
  } else {
    # only a single input file
    $ahgainfit_in = $evt_files[0];
  }

  # Fitting events to create gain
  if(run_ahgainfit($ahgainfit_in,$outfile,$gtifile)) { return 1; }

  return 0;

}

#
# Description:
#
# write hxisgdgainfit parameters in output file. Run ftverify on output file.
#
# Parameters:
# [in] outfile        Output trend file
# [out] status        Output status
#
sub Finalize {

  my $status = 0;
  ahlog::ah_out "\nFinalizing...\n";

  # Write ahscreen parameters to output file
  ahapp::write_parameters ($outfile, "Drift_energy") ;

  # Check that the FITS file we just created is valid.
  unless (ahgen::update_checksum_and_verify ($outfile)) {
    ahgen::ah_err "FITS file: $outfile. failed FITS verification test." ;
    ahlog::ah_debug "ahgen::check_fits_file";
    return ahgen::get_error_flag;
  }

  ahlog::ah_info "HIGH", "File $outfile OKAY.\n";

  return $status;

} # Finalize

# ------------------------------------------------------------------------------

sub run_ftselect ($$$) {

  my $toolname="ftselect";
  my $TmpInfile=shift;
  my $TmpOutfile=shift;
  my $expr=shift;

  # ftselect parameters
  my @params = (
    ['infile'       , $TmpInfile],
    ['outfile'      , $TmpOutfile],
    ['expr'         , $expr], 
    ['clobber'      , "yes"]
  );

  if(runTool($toolname,\@params)) { return 1; }

  # Check that not all rows were filtered
  my $naxis2 = ahgen::get_keyword($TmpOutfile,"EVENTS","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $TmpOutfile, NAXIS2 keyword not defined.\n";
    return ahgen::get_error_flag;
  }
  if($naxis2 == 0) {
    ahlog::ah_err "Filtered all rows in event file $TmpInfile";
    return 1;
  }

  return 0;

}

# ------------------------------------------------------------------------------

sub run_sxipi ($$$$$) {

  my $sxifile = shift;
  my $chtrailcor = shift;
  my $cticor = shift;
  my $spthiter = shift;
  my $spthcaldb = shift;

  my $toolname="sxipi";
  my $tmpoutfile=formTemporaryFileName($sxifile,$toolname);
  my $outfile=$sxifile;

  my @params = (
    ['infile'       => $sxifile],
    ['outfile'      => $tmpoutfile],
    ['hkfile'       => $hkfile],
    ['hkext'        => $hkext],
    ['hkcolstem'    => $hkcolstem],
    ['hkvideoid'    => $hkvideoid],
    ['vtevnoddfile' => $vtevnoddfile],
    ['chtrailfile'  => $chtrailfile],
    ['ctifile'      => $ctifile],
    ['spthfile'     => $spthfile],
    ['gainfile'     => $gainfile],
    ['patternfile'  => $patternfile],
    ['startcol'     => "PHAS"],
    ['evnoddcor'    => "yes"],
    ['chtrailcor'   => $chtrailcor],
    ['cticor'       => $cticor],
    ['badpixopt'    => 1],
    ['spthiter'     => $spthiter], 
    ['spthcaldb'    => $spthcaldb],
    ['spthoffset'   => 100],
    ['spthslope'    => 100],
    ['evtthre'      => 100],
    ['negthre'      => -50],
    ['deltatime'    => 8],
    ['debugcol'     => "no"],
    ['randomize'    => $randomize],
    ['seed'         => $seed],
    ['clobber'      => $ahapp::clobber ? "YES" : "NO"]
  );

  if(runTool($toolname,\@params)) { return 1; }
  if(copyOrMoveFileBasedOnCleanup($tmpoutfile,$outfile,$ahapp::cleanup)) { return 1; }

  return 0;

}

# ------------------------------------------------------------------------------

sub run_ahgtigen ($) {

  my $gti_outfile=shift;

  my $toolname="ahgtigen";

  my @params = (
    ['infile'       , $gtigen_infile],
    ['outfile'      , $gti_outfile],
    ['gtifile'      , $gtifile],
    ['expr'         , $expr],
    ['selectfile'   , $selectfile],
    ['label'        , $gtigenlabel],
    ['mergegti'     , "AND"],
    ['cpkeyword'    , "NONE"],
    ['upkeyword'    , "no"],
    ['cleanup'      , "yes"],
    ['clobber'      , "yes"]
  );

  if(runTool($toolname,\@params)) { return 1; }

  return 0;

}

# ------------------------------------------------------------------------------

sub run_ahscreen ($) {

  my $ahscreen_infile=shift;

  my $toolname="ahscreen";
  my $tmpoutfile=formTemporaryFileName($ahscreen_infile,$toolname);
  my $outfile=$ahscreen_infile;

  my @params = (
    ['infile'       , $ahscreen_infile],
    ['outfile'      , $tmpoutfile],
    ['gtifile'      , $gtifile],
    ['expr'         , $expr],
    ['selectfile'   , $selectfile],
    ['label'        , $screenlabel],
    ['mergegti'     , "AND"],
    ['cpkeyword'    , "NONE"],
    ['upkeyword'    , "no"],
    ['cleanup'      , "yes"],
    ['clobber'      , "yes"]
  );

  if(runTool($toolname,\@params)) { return 1; }
  if(copyOrMoveFileBasedOnCleanup($tmpoutfile,$outfile,$ahapp::cleanup)) { return 1; }

  return 0;

}

# ------------------------------------------------------------------------------

sub run_ahgainfit ($$) {

  my $gainfit_infile = shift;
  my $outfile = shift;
  my $gtifile = shift;

  # Set up input files and output files for ahgainfit
  my $toolname="ahgainfit";

  my @params = (
    ['infile'          , $gainfit_infile],
    ['outfile'         , $outfile],
    ['gtifile'         , $gtifile],
    ['clobber'         , "yes"]
  );
  my @ordered_pars = qw( linefitfile linetocorrect energycol splitcol numevent 
                         minevent gtifile gapdt grpoverlap startenergy stopenergy 
                         extraspread evchannel binwidth broadening gridprofile 
                         fitwidth background spangti avgwinrad calcerr 
                         writeerrfunc minwidth0 maxitcycle r2tol searchstepshift
                         maxdshift bisectolshift searchstepwidth maxdwidth 
                         bisectolwidth minwidth nerrshift nerrwidth shifterrfac 
                         widtherrfac ); 
  foreach my $par (@ordered_pars) {
    if($ahgainfit_pars{$par}) { push @params, [$par, $ahgainfit_pars{$par}]; }
  }

  if(runTool($toolname,\@params)) { return 1; }

  return 0;

}

# $Log: ahgengainfit.pl,v $
# Revision 1.7  2016/01/28 15:10:56  asargent
# Removed unnecessary uc conversion in clobber check
#
# Revision 1.6  2016/01/27 16:35:50  asargent
# Updated boolean parameters from uppercase to lowercase
#
# Revision 1.5  2015/11/25 17:53:10  asargent
# Removed centerprof parameter from ahgengainfit run and parfiles.
#
# Revision 1.4  2015/10/27 20:32:41  asargent
# Updated ahgainfit parameters
#
# Revision 1.3  2015/08/14 18:32:14  asargent
# Changed parameter lists from hashes to arrays for runTool
#
# Revision 1.2  2015/08/12 18:30:26  asargent
# Updated par files, removed extraneous logging
#
# Revision 1.1  2015/08/11 21:15:33  asargent
# Moved from gengainfit to ahgengainfit
#
# Revision 1.3  2015/08/06 20:57:25  asargent
# Additional logging and sxi reading of detname/datamode.
#
# Revision 1.2  2015/08/05 23:28:27  asargent
# Removed check for datamode when using sxi
#
# Revision 1.1  2015/08/05 23:15:14  asargent
# Merged sxi into gengainfit
#
