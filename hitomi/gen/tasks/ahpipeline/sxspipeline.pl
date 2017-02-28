#!/usr/bin/perl
#
# File name: sxspipeline.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/11/28 14:55:18 $
# Version: 0
#
# Calibrate sxs data
#
# Tool Dependencies:
# 
# Library Dependencies:
#   gen/lib/perl/ahlog
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
#   gen/lib/perl/ahfilterlib
#   gen/tasks/ahpipeline/ahpllib
#
# Modification History:
#

# Set up
use strict;
use warnings;

use File::Find;
use File::Spec::Functions;
use File::Basename;
use File::Copy;
use File::Path;
use Cwd 'abs_path';
use POSIX qw(strftime);

use ahlog ;
use ahapp ;
use ahgen qw (:ALL) ;
use ahfilterlib ;

use ahpllib qw (:ALL);

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

our %Params = (

  indir                => "", # "Input Directory"
  outdir               => "", # "Output Directory"
  steminputs           => "", # "Stem for input files name"
  stemoutputs          => "", # "Stem for output files name"
  instrument           => "sxs", # "Which instrument"

  verify_input         => 0,  # Verify the input files before processing? 

  attitude             => "",  # "Attitude file"
  housekeeping         => "",  # "Housekeeping file"
  extended_housekeeping=> "",  # "Extended housekeeping file"
  makefilter           => "",  # "Makefilter file, comma seperated list for multiple files"
  orbit                => "",  # "Orbit file, comma seperated list for multiple files"
  timfile              => "",  # "Time file, comma seperated list for multiple files"  
  obsgti              => "",  # "Time file, comma seperated list for multiple files"  
  adrgti              => "",  # "ADR GTI file, used for filtering"

  sxs_start            => 0,   # "SXS CALDB start time"

  ra                   => -999.99999, # RA of nominal pointing [deg]
  dec                  => -999.99999, # Dec of nominal pointing [deg]
  roll                 => 0, # Roll of nominal pointing [deg]

  optdetx              => -999.99999, # SXS optical detx coordinate
  optdety              => -999.99999, # SXS optical dety coordinate
  optfocx              => -999.99999, # SXS optical focx coordinate
  optfocy              => -999.99999, # SXS optical focy coordinate
  optskyx              => -999.99999, # SXS optical skyx coordinate
  optskyy              => -999.99999, # SXS optical skyy coordinate
  ra_pnt               => -999.99999, # RA of sxs pointing [deg]
  dec_pnt              => -999.99999, # Dec of sxs pointing [deg]

  entry_stage          => 1, # 
  exit_stage           => 1, # 

  stage1_switch        => 0, # 
  stage2_switch        => 0, # 
  stage3_switch        => 0, # 
  numerrs              => 0, # 

  calmethod            => "Cal-pix", # Default calmethod is "Cal-pix"

  calc_gtilost         => 0,
  screenlost           => 0,
  calc_mxs             => 1,

);

our %files = (
  attitude              => "",
  housekeeping          => "",
  extended_housekeeping => "",
  makefilter            => "",
  orbit                 => "",
  obsgti               => "",
  regionfile            => "",

  event_uf          => [],
  event_cl          => [],
  antico            => "",
  telgti            => "",
  mxfngti           => "",
  mxcsgti           => "",
  lostgti           => "",
  adrgti            => "",

);

our $zpatt = qr/(\.Z|\.z|\.gzip|\.GZIP|\.gz|\.GZ|\.zip\.ZIP)?/;
our %patterns = (
  mission           => qr/ah/,
  sequence          => qr/[0-9]{1,9}/,
  event_uf          => qr/(sxs_[ps][0-9]px[0-9]{4}_uf\.evt)$zpatt$/,
  event_cl          => qr/(sxs_[ps][0-9]px[0-9]{4}_cl\.evt)$zpatt$/,
  antico            => qr/(sxs_a0ac_uf\.evt)$zpatt$/,
  telgti            => qr/(sxs_tel\.gti)$zpatt$/,
  lostgti           => qr/(sxs_el\.gti)$zpatt$/,
  mxfngti           => qr/(sxs_mxfn\.gti)$zpatt$/,
  mxcsgti           => qr/(sxs_mxcs\.gti)$zpatt$/,
);

my $pipelineerror = 0;  # sxspipeline exit status

our @filelist_output;
our @error_output;
our $gainperseus = 0; # Run gain perseus tool 0 = no 1 = yes
our $hitomi_start = 67164301.0; # 2016-02-17T08:45:01 / Launch date of Hitomi
our $perseus_stop = 68517661.0; # 2016-03-04T00:41:00 / Stop time of perseus observation

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

# Reset the numerrs parameters
$Params{numerrs} = 0;
ahgen::run_ftool("pset","sxspipeline","numerrs=0");

$pipelineerror = get_parameters () ;
unless ( $pipelineerror == 0 ) {
  $Params{numerrs} += 1;
  push @error_output, "get_parameters failed";
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($pipelineerror);
}

$pipelineerror = initialize () ;
unless ( $pipelineerror == 0 ) {
  $Params{numerrs} += 1;
  push @error_output, "initialize failed";
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($pipelineerror);
}

$pipelineerror = do_work () ;
unless ( $pipelineerror == 0 ) {
  $Params{numerrs} += 1;
  push @error_output, "do_work failed";
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($pipelineerror);
}

$pipelineerror = finalize () ;
unless ( $pipelineerror == 0 ) {
  $Params{numerrs} += 1;
  push @error_output, "finalize failed";
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($pipelineerror);
}

# We're done.
ahapp::end_processing($pipelineerror);

#########################
# Subroutines
#########################

# ------------------------------------------------------------------------------

sub get_parameters {

  # Run tool parameters
  # These are hidden, so we can check these first
  # input file pars

  # Required parameters
  $Params{indir}        = ahapp::query_parameter("indir");
  $Params{outdir}       = ahapp::query_parameter("outdir");
  $Params{steminputs}   = ahapp::query_parameter("steminputs");
  $Params{stemoutputs}  = ahapp::query_parameter("stemoutputs");
  $Params{entry_stage}  = ahapp::query_parameter("entry_stage");
  $Params{exit_stage}   = ahapp::query_parameter("exit_stage");

  # Before we get the rest of the parameters, check the entry
  # and exit stages
  if (CheckEntryExitStage($Params{entry_stage},$Params{exit_stage})) { return 1; }

  # Set up the entry and exit stage flags
  $Params{stage1_switch} = 0;
  $Params{stage2_switch} = 0;
  $Params{stage3_switch} = 0;
  if ( $Params{entry_stage} == 1 ) {
    $Params{stage1_switch} = 1;
    $Params{stage2_switch} = 1;
    $Params{stage3_switch} = 1;
  }
  if ( $Params{entry_stage} == 2 ) {
    $Params{stage2_switch} = 1;
    $Params{stage3_switch} = 1;
  }
  if ( $Params{entry_stage} == 3 ) {
    $Params{stage3_switch} = 1;
  }
  if ( $Params{exit_stage} == 1 ) {
    $Params{stage2_switch} = 0;
    $Params{stage3_switch} = 0;
  }
  if ( $Params{exit_stage} == 2 ) {
    $Params{stage3_switch} = 0;
  }

  # Check that we are doing something
  unless ( $Params{stage1_switch} or
           $Params{stage2_switch} or
           $Params{stage3_switch} ) {
           ahlog::ah_err "Nothing to do.";
           return 1;
         }
                                                              
  $Params{attitude}     = ahapp::query_parameter("attitude");
  $Params{makefilter}   = ahapp::query_parameter("makefilter");
  $Params{extended_housekeeping} = ahapp::query_parameter("extended_housekeeping");
  $Params{orbit}        = ahapp::query_parameter("orbit");
  $Params{obsgti}      = ahapp::query_parameter("obsgti");
  $Params{housekeeping} = ahapp::query_parameter("housekeeping");
  $Params{timfile}      = ahapp::query_parameter("timfile");
  $Params{regionfile}   = ahapp::query_parameter("regionfile");
  $Params{adrgti}       = ahapp::query_parameter("adrgti");
                                                              
  $Params{sxs_start}    = ahapp::query_parameter("sxs_start");

  $Params{calc_gtilost}      = ahapp::query_parameter("calc_gtilost",1);
  $Params{screenlost}        = ahapp::query_parameter("screenlost",1);

  $Params{selectfile}        = ahapp::query_parameter("selectfile");
  $Params{leapsecfile}       = ahapp::query_parameter("leapsecfile");
  $Params{tstart}            = ahapp::query_parameter("tstart");
  $Params{tstop}             = ahapp::query_parameter("tstop");

  $Params{sxs_mkflabel}      = ahapp::query_parameter("sxs_mkflabel");
  $Params{sxs_ehklabel}      = ahapp::query_parameter("sxs_ehklabel");
  $Params{sxs_evtlabel}      = ahapp::query_parameter("sxs_evtlabel");

  # Coordinate parameters
  $Params{ra}           = ahapp::query_parameter("ra");
  $Params{dec}          = ahapp::query_parameter("dec");
  $Params{roll}         = ahapp::query_parameter("roll");
  $Params{optdetx}      = ahapp::query_parameter("optdetx");
  $Params{optdety}      = ahapp::query_parameter("optdety");
  $Params{optfocx}      = ahapp::query_parameter("optfocx");
  $Params{optfocy}      = ahapp::query_parameter("optfocy");
  $Params{optskyx}      = ahapp::query_parameter("optskyx");
  $Params{optskyy}      = ahapp::query_parameter("optskyy");
  $Params{ra_pnt}       = ahapp::query_parameter("ra_pnt");
  $Params{dec_pnt}      = ahapp::query_parameter("dec_pnt");

  # ahapp returns cleanup as yes/no
  $Params{cleanup}      = ahapp::query_parameter("cleanup",1);

  if ( $Params{stage1_switch} ) {
    # If we aren't calibrating events, finding tool parameters
    # can significantly slow down the pipeline script
    $Params{teldeffile}        = ahapp::query_parameter("teldeffile");
    $Params{gainantfile}       = ahapp::query_parameter("gainantfile");
    $Params{pixdeffile}        = ahapp::query_parameter("pixdeffile");
    $Params{gainfile}          = ahapp::query_parameter("gainfile");
    $Params{linefitfile}       = ahapp::query_parameter("linefitfile");

    # Miscellaneous/Shared parameter
    $Params{itypecol}            = ahapp::query_parameter("itypecol");
    $Params{ckctrec}             = ahapp::query_parameter("ckctrec");
    $Params{ckctel}              = ahapp::query_parameter("ckctel");
    $Params{ckant}               = ahapp::query_parameter("ckant");
    $Params{ckrisetime}          = ahapp::query_parameter("ckrisetime");
    $Params{tempidx}             = ahapp::query_parameter("tempidx");
    $Params{ntemp}               = ahapp::query_parameter("ntemp");
    $Params{gapdt}               = ahapp::query_parameter("gapdt");
    $Params{extrap}              = ahapp::query_parameter("extrap");
    $Params{randomize}           = ahapp::query_parameter("randomize");
    $Params{seed}                = ahapp::query_parameter("seed");
    $Params{pxphaoffset}         = ahapp::query_parameter("pxphaoffset");
    $Params{acphaoffset}         = ahapp::query_parameter("acphaoffset");

    # CALDB files
    # mxsgti parameters
    $Params{delayfile}         = ahapp::query_parameter("delayfile");
    $Params{stimecol}          = ahapp::query_parameter("stimecol");
    $Params{tioncol}           = ahapp::query_parameter("tioncol");
    $Params{tioffcol}          = ahapp::query_parameter("tioffcol");
    $Params{plslencol}         = ahapp::query_parameter("plslencol");
    $Params{plsspccol}         = ahapp::query_parameter("plsspccol");
    $Params{timeoncol}         = ahapp::query_parameter("timeoncol");
    $Params{timeoffcol}        = ahapp::query_parameter("timeoffcol");
    $Params{calctime}          = ahapp::query_parameter("calctime");
    $Params{calcgti}           = ahapp::query_parameter("calcgti");
    $Params{afterglow}         = ahapp::query_parameter("afterglow");
    $Params{dtdecay}           = ahapp::query_parameter("dtdecay");
    $Params{interp}            = ahapp::query_parameter("interp");
    $Params{margingti}         = ahapp::query_parameter("margingti");
    $Params{dt}                = ahapp::query_parameter("dt");

    # coordevt parameters
    $Params{dattfile}          = ahapp::query_parameter("dattfile");
    $Params{startsys}          = ahapp::query_parameter("coordevt_startsys");
    $Params{stopsys}           = ahapp::query_parameter("stopsys");
    $Params{annaber}           = ahapp::query_parameter("annaber");
    $Params{followsun}         = ahapp::query_parameter("followsun");
    $Params{orbaber}           = ahapp::query_parameter("orbaber");
    $Params{attinterp}         = ahapp::query_parameter("attinterp");
    $Params{dattinterp}        = ahapp::query_parameter("dattinterp");
    $Params{attdt}             = ahapp::query_parameter("attdt");
    $Params{dattdt}            = ahapp::query_parameter("dattdt");
    $Params{chkattgap}         = ahapp::query_parameter("chkattgap");
    $Params{chkdattgap}        = ahapp::query_parameter("chkdattgap");
    $Params{attext}            = ahapp::query_parameter("attext");
    $Params{attcol}            = ahapp::query_parameter("attcol");
    $Params{attform}           = ahapp::query_parameter("attform");
    $Params{orbext}            = ahapp::query_parameter("orbext");
    $Params{orbcol}            = ahapp::query_parameter("orbcol");
    $Params{orbform}           = ahapp::query_parameter("orbform");
    $Params{coordevt_randomize}         = ahapp::query_parameter("coordevt_randomize");
    $Params{randsys}           = ahapp::query_parameter("randsys");
    $Params{randscalesys}      = ahapp::query_parameter("randscalesys");
    $Params{infileext}         = ahapp::query_parameter("infileext");
    $Params{inclfloatcol}      = ahapp::query_parameter("inclfloatcol");
    $Params{inclfloatskycol}   = ahapp::query_parameter("inclfloatskycol");
    $Params{floatcolsuffix}    = ahapp::query_parameter("floatcolsuffix");
    $Params{startwithfloat}    = ahapp::query_parameter("startwithfloat");
    $Params{blankcol}          = ahapp::query_parameter("blankcol");
    $Params{btnull}            = ahapp::query_parameter("btnull");
    $Params{itnull}            = ahapp::query_parameter("itnull");
    $Params{jtnull}            = ahapp::query_parameter("jtnull");
    $Params{ktnull}            = ahapp::query_parameter("ktnull");
    $Params{sbtnull}           = ahapp::query_parameter("sbtnull");
    $Params{uitnull}           = ahapp::query_parameter("uitnull");
    $Params{ujtnull}           = ahapp::query_parameter("ujtnull");

    # sxsanticopi parameters
    # gainantfile, randomize, seed

    # sxsflagpix parameters
    $Params{antpsp}           = ahapp::query_parameter("antpsp");
    $Params{antshift}         = ahapp::query_parameter("antshift");
    $Params{calcant}          = ahapp::query_parameter("calcant");
    $Params{antdtpre}         = ahapp::query_parameter("antdtpre");
    $Params{antdtfol}         = ahapp::query_parameter("antdtfol");
    $Params{antswitch}        = ahapp::query_parameter("antswitch");
    $Params{antphathr}        = ahapp::query_parameter("antphathr");
    $Params{antdurthr}        = ahapp::query_parameter("antdurthr");
    $Params{calcctrec}        = ahapp::query_parameter("calcctrec");
    $Params{ctrecdt}          = ahapp::query_parameter("ctrecdt");
    $Params{calcprox}         = ahapp::query_parameter("calcprox");
    $Params{proxdt}           = ahapp::query_parameter("proxdt");
    $Params{calcctel}         = ahapp::query_parameter("calcctel");
    $Params{cteldt}           = ahapp::query_parameter("cteldt");
    $Params{ctelnear}         = ahapp::query_parameter("ctelnear");
    $Params{calcctel2}        = ahapp::query_parameter("calcctel2");
    $Params{cteldt2}          = ahapp::query_parameter("cteldt2");
    $Params{ctelnear2}        = ahapp::query_parameter("ctelnear2");
    $Params{pxpithr}          = ahapp::query_parameter("pxpithr");
    $Params{usepxpithr}       = ahapp::query_parameter("usepxpithr");
    $Params{calcmxs}          = ahapp::query_parameter("calcmxs");
    $Params{mxsdt}            = ahapp::query_parameter("mxsdt");
    $Params{kalow}            = ahapp::query_parameter("kalow");
    $Params{kahigh}           = ahapp::query_parameter("kahigh");
    $Params{kbeta}            = ahapp::query_parameter("kbeta");
    $Params{dtflag}           = ahapp::query_parameter("dtflag");
    $Params{resetflags}       = ahapp::query_parameter("resetflags");

    # sxssecid parameters
    $Params{dtprimary}           = ahapp::query_parameter("dtprimary");
    $Params{dtlowmid}            = ahapp::query_parameter("dtlowmid");
    $Params{dtmidhigh}           = ahapp::query_parameter("dtmidhigh");
    $Params{tol}                 = ahapp::query_parameter("tol");
    $Params{regrade}             = ahapp::query_parameter("regrade");

    # sxsseccor parameters
    $Params{pulsefile}           = ahapp::query_parameter("pulsefile");
    $Params{phaout}              = ahapp::query_parameter("phaout");

    # sxsgain parameters
    $Params{gaincoeff}           = ahapp::query_parameter("gaincoeff");
    $Params{linetocorrect}       = ahapp::query_parameter("linetocorrect");
    $Params{numevent}            = ahapp::query_parameter("numevent");
    $Params{minevent}            = ahapp::query_parameter("minevent");
    $Params{grpoverlap}          = ahapp::query_parameter("grpoverlap");
    $Params{startenergy}         = ahapp::query_parameter("startenergy");
    $Params{stopenergy}          = ahapp::query_parameter("stopenergy");
    $Params{extraspread}         = ahapp::query_parameter("extraspread");
    $Params{broadening}          = ahapp::query_parameter("broadening");
    $Params{gridprofile}         = ahapp::query_parameter("gridprofile");
    $Params{fitwidth}            = ahapp::query_parameter("fitwidth");
    $Params{background}          = ahapp::query_parameter("background");
    $Params{spangti}             = ahapp::query_parameter("spangti");
    $Params{usemp}               = ahapp::query_parameter("usemp");
    $Params{calcerr}             = ahapp::query_parameter("calcerr");
    $Params{writeerrfunc}        = ahapp::query_parameter("writeerrfunc");
    $Params{avgwinrad}           = ahapp::query_parameter("avgwinrad");
    $Params{minwidth0}           = ahapp::query_parameter("minwidth0");
    $Params{maxitcycle}          = ahapp::query_parameter("maxitcycle");
    $Params{r2tol}               = ahapp::query_parameter("r2tol");
    $Params{searchstepshift}     = ahapp::query_parameter("searchstepshift");
    $Params{maxdshift}           = ahapp::query_parameter("maxdshift");
    $Params{bisectolshift}       = ahapp::query_parameter("bisectolshift");
    $Params{searchstepwidth}     = ahapp::query_parameter("searchstepwidth");
    $Params{maxdwidth}           = ahapp::query_parameter("maxdwidth");
    $Params{bisectolwidth}       = ahapp::query_parameter("bisectolwidth");
    $Params{minwidth}            = ahapp::query_parameter("minwidth");
    $Params{nerrshift}           = ahapp::query_parameter("nerrshift");
    $Params{nerrwidth}           = ahapp::query_parameter("nerrwidth");
    $Params{shifterrfac}         = ahapp::query_parameter("shifterrfac");
    $Params{widtherrfac}         = ahapp::query_parameter("widtherrfac");

    # sxspha2pi parameters
    $Params{calcupi}             = ahapp::query_parameter("calcupi");
    $Params{calcpi}              = ahapp::query_parameter("calcpi");
    $Params{scalefile}           = ahapp::query_parameter("scalefile");
    $Params{secphacol}           = ahapp::query_parameter("secphacol");
    $Params{scaleepi}            = ahapp::query_parameter("scaleepi");
    $Params{scalegrade}          = ahapp::query_parameter("scalegrade");
    $Params{addepicol}           = ahapp::query_parameter("addepicol");
    $Params{method}              = ahapp::query_parameter("method");
    $Params{extended}            = ahapp::query_parameter("extended");
    $Params{binwidth}            = ahapp::query_parameter("binwidth");
    $Params{offset}              = ahapp::query_parameter("offset");
    $Params{tlmax}               = ahapp::query_parameter("tlmax");
    $Params{writetemp}           = ahapp::query_parameter("writetemp");

    # sxsperseus parameters
    $Params{dgfile}              = ahapp::query_parameter("dgfile");
    $Params{offsetfile}          = ahapp::query_parameter("offsetfile");
    $Params{outrange}            = ahapp::query_parameter("outrange");


  }

  if (uc($Params{stemoutputs}) eq "DEFAULT") {
      $Params{stemoutputs} = $Params{steminputs};
  }    

  # Set output report name

  $Params{stemoutputs} =~ s/^\s+//; # Remove leading blanks
  $Params{stemoutputs} =~ s/\s+$//; # Remove trailing blanks

  # Get the stem for log files and temporary files
  $Params{stemreport}    = ahapp::query_parameter("stemreport");

  # If the report stem parameter is empty, create a new one
  if ( $Params{stemreport} ) {
    # Create temporary and log file name stems
    $Params{tempstem} = $Params{stemreport} .  "_tmp_";
    $Params{logstem} = $Params{stemreport} . "_";
  } else {
    # Set the date string in the format YYYYMMDDTHHMMSS
    my $taskstart = strftime "%Y%m%dT%H%M%S", localtime ;
    # Create temporary and log file name stems
    $Params{tempstem} = "sxspipeline_" . $taskstart . "_tmp_";
    $Params{logstem} = "sxspipeline_" . $taskstart . "_";
  }

  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status=0;

  my $indir        = $Params{indir};
  my $outdir       = $Params{outdir};
  my $steminputs   = $Params{steminputs};
  my $stemoutputs  = $Params{stemoutputs};
  my $inst         = $Params{instrument};
  my $attitude     = $Params{attitude};
  my $makefilter   = $Params{makefilter};
  my $extended_housekeeping = $Params{extended_housekeeping};
  my $orbit        = $Params{orbit};
  my $obsgti      = $Params{obsgti};
  my $housekeeping = $Params{housekeeping};
  my $regionfile   = $Params{regionfile};
  my $timfile      = $Params{timfile};
  my $adrgti      = $Params{adrgti};

  # Check the input and output directories
  if (CheckInputDirectory($indir,$outdir)) { return 1; };
  if (CheckOutputDirectory($outdir)) { return 1; };

  # Find and store the list of input files
  @{$files{event_uf}} = FindInputFiles($indir,$steminputs , $patterns{event_uf});
  @{$files{event_cl}} = FindInputFiles($indir,$steminputs , $patterns{event_cl});
  ($files{antico})    = FindInputFiles($indir,$steminputs , $patterns{antico});
  ($files{telgti})    = FindInputFiles($indir,$steminputs , $patterns{telgti});
  ($files{lostgti})   = FindInputFiles($indir,$steminputs , $patterns{lostgti});
  ($files{mxfngti})   = FindInputFiles($indir,$steminputs , $patterns{mxfngti});
  ($files{mxcsgti})   = FindInputFiles($indir,$steminputs , $patterns{mxcsgti});
  
  # Set up TSTART/TSTOP and check that we found a list of unfiltered event files
  if ( $Params{stage1_switch} or $Params{stage2_switch} ) {
    # Get TSTART/TSTOP from the observation GTI
    if ( uc $Params{tstart} eq "DEFAULT" ) {
      my $tstart = ahgen::get_keyword($obsgti,"GTIOBS","TSTART");
      unless ( defined $tstart ) {
        ahlog::ah_out"In file '$obsgti', could not read keyword TSTART";
        return 1;
      }
      $Params{tstart} = $tstart;
    }
    if ( uc $Params{tstop} eq "DEFAULT" ) {
      my $tstop = ahgen::get_keyword($obsgti,"GTIOBS","TSTOP");
      unless ( defined $tstop ) {
        ahlog::ah_err"In file '$obsgti', could not read keyword TSTOP";
        return 1;
      }
      $Params{tstop} = $tstop;
    }
    unless ( @{$files{event_uf}} ) {
        ahlog::ah_err "Cannot find any $inst event files in $indir" ;
        return 1;
    }
  }
  # Check the input CALDB files
  if ( $Params{stage1_switch} ) {
    # Check for file requirements
    if (isOptionalFileNotFound($attitude)) { return 1; }
    if (isOptionalFileNotFound($orbit)) { return 1; }
    if (isOptionalFileNotFound($housekeeping)) { return 1; }
    if (isRequiredFileNotFound($obsgti)) { return 1; }
    if (isOptionalFileNotFound($timfile)) { return 1; }

    # Store the required input files
    $files{attitude}      = $attitude;
    $files{orbit}         = $orbit;
    $files{housekeeping}  = $housekeeping;
    $files{obsgti}        = $obsgti;
    $files{timfile}       = $timfile;

    # Check the CALDB files
    if (isBadCALDBFileParameterValue($Params{teldeffile},"teldeffile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{gainantfile},"gainantfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{pixdeffile},"pixdeffile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{gainfile},"gainfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{linefitfile},"linefitfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{delayfile},"delayfile")) { return 1;}
    unless ( $files{lostgti} ) {
      ahlog::ah_info "HIGH", " *** Cannot find any $inst lost GTI files in $indir" ;
      if ( $Params{calc_gtilost} ) {
        ahlog::ah_info "HIGH", "Creating lost GTI file from scratch" ;
        $files{lostgti} = "NONE";
      } else {
        ahlog::ah_err "Cannot complete SXS calibration without lost GTI file" ;
        ahlog::ah_err "Create lost GTI from scratch using 'calc_gtilost=yes'" ;
        return 1;
      }
    }
    unless ( $files{antico} ) {
      ahlog::ah_err "Cannot find any $inst antico files in $indir" ;
      return 1;
    }
    # Verify that we have a valid line for sxsgain
    if ( lc $Params{calcmxs} eq "no" ) { $Params{calc_mxs} = 0; }
    if ( $Params{calc_mxs} ) {
      if ( uc $housekeeping eq "NONE" ) {
        ahlog::ah_info "HIGH", " *** Cannot find any $inst HK1 files in $indir" ;
        ahlog::ah_info "HIGH", " *** Skipping mxsgti." ;
        $Params{calc_mxs} = 0;
      }
      if ( uc $timfile eq "NONE" ) {
        ahlog::ah_info "HIGH", " *** Cannot find any tim files in $Params{timfile}" ;
        ahlog::ah_info "HIGH", " *** Skipping mxsgti." ;
        $Params{calc_mxs} = 0;
      }
    }

    my $linetocorrect = $Params{linetocorrect};
    if ( lc $linetocorrect eq "mnka" ) {
      $Params{calmethod} = "Cal-pix";
    } elsif ( lc $linetocorrect eq "cuka" or
              lc $linetocorrect eq "cukb" or
              lc $linetocorrect eq "crka" or
              lc $linetocorrect eq "crkb" ) {
      if ( ! $Params{calc_mxs} ) { 
        if ( ! $files{mxfngti} ) {
          ahlog::ah_err "Cannot complete SXS calibration without MXS fine GTI file" ;
          return 1;
        }
      }
      $Params{calmethod} = "MXS";
    } elsif ( lc $linetocorrect eq "alka" or
              lc $linetocorrect eq "alkb" or
              lc $linetocorrect eq "mgka" ) {
      if ( ! $Params{calc_mxs} ) {
        if ( ! $files{mxfngti} ) {
          ahlog::ah_err "Cannot complete SXS calibration without MXS fine GTI file" ;
          return 1;
        }
      }
      $Params{calmethod} = "MXS";
    } else {
      ahlog::ah_err "Not a valid line to correct: $linetocorrect";
      return 1;
    }
    ahlog::ah_info "HIGH","sxsgain calibration method: $Params{calmethod}";
    if ( uc $Params{stopsys} eq "HIGHEST" and uc $files{attitude} eq "NONE" ) {
      ahlog::ah_info "HIGH","No input ATTITUDE file. Setting stopsys as FOC";
      $Params{stopsys} = "FOC";
    }
    if ( uc $files{orbit} eq "NONE" ) {
      ahlog::ah_info "HIGH","No input ORBIT file";
    }
    # Determine if we are correcting perseus files
    if ( $Params{tstart} > $hitomi_start && $Params{tstart} < $perseus_stop ) {
      my $caloffset = "";
      my $gaincoeffs= "";
      $gainperseus = 1;
      if ( uc $Params{dgfile} eq "REFDATA" ) {
        # Find the calibration offset file if it's set to REFDATA
        ahlog::ah_out "Retrieving 'dgfile' from refarea ($ENV{LHEA_DATA})";
        $gaincoeffs= $ENV{LHEA_DATA} . "/ahsxs_dggain.fits";
        $Params{dgfile} = $gaincoeffs;
      }
      if ( uc $Params{offsetfile} eq "REFDATA" ) {
        # Find the calibration offset file if it's set to REFDATA
        ahlog::ah_out "Retrieving 'offsetfile' from refarea ($ENV{LHEA_DATA})";
        $caloffset = $ENV{LHEA_DATA} . "/ahsxs_offsets.fits";
        $Params{offsetfile} = $caloffset;
      }
      if (isRequiredFileNotFound($Params{dgfile})) { return 1; }
      if (isRequiredFileNotFound($Params{offsetfile})) { return 1; }
    }
  }
  if ( $Params{stage2_switch} ) {
    # Check for file requirements
    if (isRequiredFileNotFound($obsgti)) { return 1; }
    if (isRequiredFileNotFound($makefilter)) { return 1; }
    if (isRequiredFileNotFound($extended_housekeeping)) { return 1; }

    $files{obsgti}                = $obsgti;
    $files{makefilter}            = $makefilter;
    $files{extended_housekeeping} = $extended_housekeeping;

    if (isBadCALDBFileParameterValue($Params{selectfile},"selectfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{leapsecfile},"leapsecfile")) { return 1;}
  }
  if ( $Params{stage3_switch} ) {
    $files{regionfile}    = $regionfile;
    if (isOptionalFileNotFound($files{regionfile})) { return 1; }
  }

  # Search for required files for each step

       
  # For cleaning events, Check for required GTI
  if( $Params{stage2_switch} ) {
    # Check if the TSTART of the observation is after February 16, 2016
    # If so, use the GTIADR for screening
    if ( $Params{tstart} > $hitomi_start ) {
      if ( uc $adrgti eq "REFDATA" ) {
        # Find the ADR GTI file if it's set to REFDATA
        ahlog::ah_out "Retrieving ADR GTI file from refarea ($ENV{LHEA_DATA})";
        $adrgti = $ENV{LHEA_DATA} . "/ahsxs_adr.gti";
      }
      if (isRequiredFileNotFound($adrgti)) { return 1; }
      ahlog::ah_out "Found ADR GTI file: $adrgti";
    } else {
      # Thermal vac data being screened (presumably)
      # Do not use GTIADR
      $adrgti = "";
    }
    $files{adrgti}       = $adrgti;

    unless ( $files{telgti} ) {
      ahlog::ah_err "Cannot find any $inst telemetry GTI files in $indir" ;
      return 1;
    }
    unless ( $files{lostgti} ) {
      ahlog::ah_err "Cannot find any $inst lost GTI files in $indir" ;
      return 1;
    }
      # If there is no MXS Fine GTI, do not run any MXS calculations
    unless ( $files{mxfngti} ) { 
      ahlog::ah_out " *** Cannot find any MXS fine GTI file in $indir" ;
      ahlog::ah_out " Not using MXS (fine) off GTI for screening" ;
      $Params{calc_mxs} = 0; 
    }
  }

  # If creating products and not cleaning events, input cleaned
  # event files required
  if( $Params{stage3_switch} and ! $Params{stage2_switch} ) {
    unless ( @{$files{event_cl}} ) {
        ahlog::ah_err "Cannot find any $inst event files in $indir" ;
        return 1;
    }
  }

  # Print a summary of the found files
  if ( ahgen::getdebug ) {
    ahlog::ah_debug "Attitude file              = $files{attitude}\n" if $files{attitude};
    ahlog::ah_debug "Orbit file                 = $files{orbit}\n" if $files{orbit};
    ahlog::ah_debug "Housekeeping file          = $files{housekeeping}\n" if $files{housekeeping};
    ahlog::ah_debug "GTI file                   = $files{obsgti}\n" if $files{obsgti};
    ahlog::ah_debug "Extended housekeeping file = $files{extended_housekeeping}\n" if $files{extended_housekeeping};
    ahlog::ah_debug "Makefilter file            = $files{makefilter}\n" if $files{makefilter};
    ahlog::ah_debug "Region file                = $files{regionfile}\n" if $files{regionfile};
    ahlog::ah_debug "$inst unfiltered file(s) found:" if @{$files{event_uf}};
    foreach my $filename ( @{$files{event_uf}} ) {
      ahlog::ah_debug "  $filename\n";
    }
    ahlog::ah_debug "$inst cleaned file(s) found:" if @{$files{event_cl}};
    foreach my $filename ( @{$files{event_cl}} ) {
      ahlog::ah_debug "  $filename\n";
    }
    ahlog::ah_debug "Antico file                = $files{antico}\n" if $files{antico};
    ahlog::ah_debug "Telemetry Saturation GTI   = $files{telgti}\n" if $files{telgti};
    ahlog::ah_debug "Lost GTI                   = $files{lostgti}\n" if $files{lostgti};
    ahlog::ah_debug "MXS Fine GTI               = $files{mxfngti}\n" if $files{mxfngti};
    ahlog::ah_debug "MXS Coarse GTI             = $files{mxcsgti}\n" if $files{mxcsgti};
  }

  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;                 

  return 0;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $status = 0;

  if ( $Params{stage1_switch} == 1 ) {
    $status = CalibrateEvents();
    unless ( $status == 0 ) {
      ahlog::ah_err "CalibrateEvents" ;
      return $status;
    }
  }

  if ( $Params{stage2_switch} == 1 ) {
    $status = CleanEvents();
    unless ( $status == 0 ) {
      ahlog::ah_err "CleanEvents" ;
      return $status;
    }
  }

  if ( $Params{stage3_switch} == 1 ) {
    $status = CreateProducts();
    unless ( $status == 0 ) {
      ahlog::ah_err "CreateProducts" ;
      return $status;
    }
  }

  return 0;

}

# ------------------------------------------------------------------------------

sub CalibrateEvents {

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nCalibrateEvents\n";
  }

  my @calibrated_sxs_uf_evt_files_prerun = ( );
  my @calibrated_sxs_uf_evt_files = ( );

  my $status = 0;

  #############  setup  parameters for each tool #############

  my $sxs_start    = $Params{sxs_start};

  # Attitude/Orbit files
  my $attitude     = $files{attitude};
  my $orbit        = $files{orbit};
  my $timfile      = $files{timfile};
  my $obsgti       = $files{obsgti};

  # CALDB files
  my $teldeffile   = $Params{teldeffile};
  my $pixdeffile   = $Params{pixdeffile};
  my $delayfile    = $Params{delayfile};
  my $leapsecfile  = $Params{leapsecfile};

  # SXS files
  my $housekeeping  = $files{housekeeping};
  my $lostgti       = $files{lostgti};
  my $antico        = $files{antico};
  my $telgti        = $files{telgti};
  my $lostgti_withext = $lostgti;
  if ( uc $lostgti ne "NONE" ) {
    $lostgti_withext .= "[1]";
  }

  # Calculate output file names
  my $antico_out    = form_outfile_name($antico);
  my $mxfngti       = catfile($Params{outdir},$Params{stemoutputs} . "sxs_mxfn.gti");
  my $mxcsgti       = catfile($Params{outdir},$Params{stemoutputs} . "sxs_mxcs.gti");

  # Initialize sxsgain paramters
  my $sxsgaingti    = $Params{tempstem} . "sxsgain.gti";
  my $pxmerge       = "";
  my $driftfile     = "";

  ahapp::add_temp_file($sxsgaingti);

  # Miscellaneous parameters
  my $tstart        = $Params{tstart};
  my $tstop         = $Params{tstop};
  my $calmethod     = $Params{calmethod};
  my $linetocorrect = $Params{linetocorrect};
  my $cleanup       = $Params{cleanup};

  my @ingtis = ();

  my %gtiinvert_pars = (
    margingti        => $Params{margingti},
    tstart           => $tstart,
    tstop            => $tstop,
    dt               => $Params{dt},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );

  my %mxsgti_pars = (
    timfile          => $timfile,  
    delayfile        => $delayfile,
    stimecol         => $Params{stimecol},
    tioncol          => $Params{tioncol},
    tioffcol         => $Params{tioffcol},
    plslencol        => $Params{plslencol},
    plsspccol        => $Params{plsspccol},
    timeoncol        => $Params{timeoncol},
    timeoffcol       => $Params{timeoffcol},
    calctime         => $Params{calctime},
    calcgti          => $Params{calcgti},
    afterglow        => $Params{afterglow},
    dtdecay          => $Params{dtdecay},
    interp           => $Params{interp},
    margingti        => $Params{margingti},
    tstart           => $Params{tstart},
    tstop            => $Params{tstop},
    dt               => $Params{dt},
    teldeffile       => $teldeffile,
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );

  my %coordevt_pars_pointing = (
      teldeffile        => $teldeffile,
      attfile           => $attitude,
      dattfile          => $Params{dattfile},
      orbfile           => $orbit,
      startsys          => $Params{startsys},
      stopsys           => $Params{stopsys},
      annaber           => $Params{annaber},
      followsun         => $Params{followsun},
      orbaber           => $Params{orbaber},
      attinterp         => $Params{attinterp},
      dattinterp        => $Params{dattinterp},
      attdt             => $Params{attdt},
      dattdt            => $Params{dattdt},
      chkattgap         => $Params{chkattgap},
      chkdattgap        => $Params{chkdattgap},
      attext            => $Params{attext},
      attcol            => $Params{attcol},
      attform           => $Params{attform},
      orbext            => $Params{orbext},
      orbcol            => $Params{orbcol},
      orbform           => $Params{orbform},
      randomize         => $Params{coordevt_randomize},
      seed              => $Params{seed},
      randsys           => $Params{randsys},
      randscalesys      => $Params{randscalesys},
      infileext         => $Params{infileext},
      timecol           => $Params{timecol},
      inclfloatcol      => $Params{inclfloatcol},
      inclfloatskycol   => $Params{inclfloatskycol},
      floatcolsuffix    => $Params{floatcolsuffix},
      startwithfloat    => $Params{startwithfloat},
      blankcol          => $Params{blankcol},
      btnull            => $Params{btnull},
      itnull            => $Params{itnull},
      jtnull            => $Params{jtnull},
      ktnull            => $Params{ktnull},
      sbtnull           => $Params{sbtnull},
      uitnull           => $Params{uitnull},
      ujtnull           => $Params{ujtnull},
      ra                => $Params{ra},
      dec               => $Params{dec},
      roll              => 0,
      clobber => $ahapp::clobber ? "yes" : "no" ,
      chatter => $ahapp::chatter,
  );

  my %coordevt_pars_slew = %coordevt_pars_pointing;
  $coordevt_pars_slew{stopsys} = "FOC";

  my %sxsanticopi_pars = (
    gainantfile =>  $Params{gainantfile},
    acphaoffset =>  $Params{acphaoffset},
    randomize   =>  $Params{randomize},
    seed        =>  $Params{seed},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );

  my %sxsflagpix_pars = (
    antpsp         => $Params{antpsp},
    antshift       => $Params{antshift},
    gtifile        => $lostgti_withext,
    calcant        => $Params{calcant},
    antdtpre       => $Params{antdtpre},
    antdtfol       => $Params{antdtfol},
    antswitch      => $Params{antswitch},
    antphathr      => $Params{antphathr},
    antdurthr      => $Params{antdurthr},
    calcctrec      => "no",
    ctrecdt        => $Params{ctrecdt},
    calcprox       => "no",
    proxdt         => $Params{proxdt},
    calcctel       => $Params{calcctel},
    pixdeffile     => $pixdeffile,
    cteldt         => $Params{cteldt},
    ctelnear       => $Params{ctelnear},
    calcctel2      => $Params{calcctel2},
    cteldt2        => $Params{cteldt2},
    ctelnear2      => $Params{ctelnear2},
    inmxsgti       => "NONE", # This is updated when mxsgti is run
    pxpithr        => $Params{pxpithr},
    usepxpithr     => "NONE",
    calcmxs        => "no",
    mxsdt          => $Params{mxsdt},
    kalow          => $Params{kalow},
    kahigh         => $Params{kahigh},
    kbeta          => $Params{kbeta},
    ckrisetime     => $Params{ckrisetime},
    dtflag         => $Params{dtflag},
    resetflags     => $Params{resetflags},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );
  my %sxsflagpix_pars2 = %sxsflagpix_pars;
  $sxsflagpix_pars2{inantfile} = "NONE";
  $sxsflagpix_pars2{gtifile} = "NONE";
  $sxsflagpix_pars2{calcant} = "no";
  $sxsflagpix_pars2{calcctrec} = $Params{calcctrec};
  $sxsflagpix_pars2{calcprox} = $Params{calcprox};
  $sxsflagpix_pars2{calcctel} = "no";
  $sxsflagpix_pars2{calcctel2} = "no";
  $sxsflagpix_pars2{usepxpithr} = $Params{usepxpithr};
  $sxsflagpix_pars2{resetflags} = "NONE";

  my %sxssecid_pars = (
    itypecol         => $Params{itypecol},
    dtprimary        => $Params{dtprimary},
    dtlowmid         => $Params{dtlowmid},
    dtmidhigh        => $Params{dtmidhigh},
    tol              => $Params{tol},
    pxpithr          => $Params{pxpithr},
    usepxpithr       => "NONE",
    ckctrec          => "no",
    ckctel           => "no",
    ckant            => "no",
    ckrisetime       => $Params{ckrisetime},
    regrade          => $Params{regrade},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );
  my %sxssecid_pars2 = %sxssecid_pars;
  $sxssecid_pars2{usepxpithr} = $Params{usepxpithr};

  my %sxsseccor_pars = (
    pulsefile        => $Params{pulsefile},
    itypecol         => $Params{itypecol},
    phaout           => $Params{phaout},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );

  my %sxsgain_pars = (
    gainfile         => $Params{gainfile},
    tempidx          => $Params{tempidx},
    gaincoeff        => $Params{gaincoeff},
    linefitfile      => $Params{linefitfile},
    linetocorrect    => $linetocorrect,
    calmethod        => $Params{calmethod},
    itypecol         => $Params{itypecol},
    ntemp            => $Params{ntemp},
    numevent         => $Params{numevent},
    minevent         => $Params{minevent},
    gtifile          => $sxsgaingti, # Determined by line
    gapdt            => $Params{gapdt},
    grpoverlap       => $Params{grpoverlap},
    startenergy      => $Params{startenergy},
    stopenergy       => $Params{stopenergy},
    extraspread      => $Params{extraspread},
    pxphaoffset      => $Params{pxphaoffset},
    broadening       => $Params{broadening},
    gridprofile      => $Params{gridprofile},
    fitwidth         => $Params{fitwidth},
    background       => $Params{background},
    spangti          => $Params{spangti},
    usemp            => $Params{usemp},
    ckrisetime       => $Params{ckrisetime},
    calcerr          => $Params{calcerr},
    writeerrfunc     => $Params{writeerrfunc},
    ckant            => $Params{ckant},
    ckctrec          => $Params{ckctrec},
    ckctel           => $Params{ckctel},
    extrap           => $Params{extrap},
    avgwinrad        => $Params{avgwinrad},
    minwidth0        => $Params{minwidth0},
    maxitcycle       => $Params{maxitcycle},
    r2tol            => $Params{r2tol},
    searchstepshift  => $Params{searchstepshift},
    maxdshift        => $Params{maxdshift},
    bisectolshift    => $Params{bisectolshift},
    searchstepwidth  => $Params{searchstepwidth},
    maxdwidth        => $Params{maxdwidth},
    bisectolwidth    => $Params{bisectolwidth},
    minwidth         => $Params{minwidth},
    nerrshift        => $Params{nerrshift},
    nerrwidth        => $Params{nerrwidth},
    shifterrfac      => $Params{shifterrfac},
    widtherrfac      => $Params{widtherrfac},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );

  my %sxspha2pi_pars = (
    calcupi       => $Params{calcupi},
    calcpi        => $Params{calcpi},
    gainfile      => $Params{gainfile},
    scalefile     => $Params{scalefile},
    tempidx       => -1,
    pxphaoffset   => $Params{pxphaoffset},
    secphacol     => $Params{secphacol},
    addepicol     => $Params{addepicol},
    method        => $Params{method},
    scaleepi      => $Params{scaleepi},
    scalegrade    => $Params{scalegrade},
    itypecol      => $Params{itypecol},
    extended      => $Params{extended},
    binwidth      => $Params{binwidth},
    offset        => $Params{offset},
    tlmax         => $Params{tlmax},
    gapdt         => $Params{gapdt},
    ntemp         => $Params{ntemp},
    writetemp     => $Params{writetemp},
    extrap        => $Params{extrap},
    randomize     => $Params{randomize},
    seed          => $Params{seed},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );

  my %sxsperseus_pars = (
    dgfile        => $Params{dgfile},
    offsetfile    => $Params{offsetfile},
    outrange      => $Params{outrange},
    method        => $Params{method},
    extended      => $Params{extended},
    binwidth      => $Params{binwidth},
    offset        => $Params{offset},
    tlmax         => $Params{tlmax},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );

  # Invert the GTILOST for both pixels and antico if
  # flag is true
  if ( $Params{calc_gtilost} ) {

    my $outlostgti = catfile($Params{outdir},$Params{stemoutputs} . "sxs_el.gti");
    my $pxlostoff = $Params{tempstem} . $Params{stemoutputs} . "sxs_el_pxlostoff.gti";
    my $aclostoff = $Params{tempstem} . $Params{stemoutputs} . "sxs_el_aclostoff.gti";

    ahapp::add_temp_file($pxlostoff);
    ahapp::add_temp_file($aclostoff);

    if ( uc $lostgti eq "NONE" ) {
      # Create a GTILOST file with zero rows if there was none given
      if(create_gti_lost($outlostgti,$tstart,$tstop)) { return 1; }
    } else {
      # Copy the input GTI to the output GTI
      $status = copy_hdu($lostgti,"1",$outlostgti);
      if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
      $status = copy_hdu($lostgti,"2",$outlostgti);
      if ($status) { ahlog::ah_err "ftappend failed."; return $status; }
    }

    # Run gtiinvert
    $status = run_gtiinvert ( $outlostgti . "[1]" , $pxlostoff , "GTIFOUNDALL", \%gtiinvert_pars) ;
    if ( $status ) { ahlog::ah_err "gtiinvert failed"; return $status; }
    ahgen::set_keyword($pxlostoff,"GTIFOUNDALL","INSTRUME","SXS");
    ahgen::set_keyword($pxlostoff,"GTIFOUNDALL","DETNAM","PIXEL");
    $status = copy_hdu($pxlostoff,"GTIFOUNDALL",$outlostgti);
    if ($status) {  ahlog::ah_err "ftappend failed."; return $status; }

    # Run gtiinvert
    $status = run_gtiinvert ( $outlostgti . "[2]" , $aclostoff , "GTIFOUNDALL", \%gtiinvert_pars) ;
    if ( $status ) { ahlog::ah_err "gtiinvert failed"; return $status; }
    ahgen::set_keyword($aclostoff,"GTIFOUNDALL","INSTRUME","SXS");
    ahgen::set_keyword($aclostoff,"GTIFOUNDALL","DETNAM","ANTICO");
    $status = copy_hdu($aclostoff,"GTIFOUNDALL",$outlostgti);
    if ($status) { ahlog::ah_err "ftappend failed."; return $status; }
    $files{lostgti} = $outlostgti;
    $lostgti = $files{lostgti};
    push( @filelist_output, $lostgti );

  } # end GTILOST

  if ( $Params{calc_mxs} ) {

    # Determine if the HK file has sufficient data
    my $naxis2 = ahgen::get_keyword($housekeeping,"HK_SXS_FWE","NAXIS2");
    unless ( defined $naxis2 ) {
      ahlog::ah_out"In file '$housekeeping\[HK_SXS_FWE]', could not read keyword NAXIS2";
      ahlog::ah_out"Skipping MXS.";
      $Params{calc_mxs} = 0;
    }
    if ( $naxis2 == 0 ) {
      ahlog::ah_out"In file '$housekeeping\[HK_SXS_FWE]', NAXIS2=0";
      ahlog::ah_out"Skipping MXS.";
      $Params{calc_mxs} = 0;
    }

    # Read the MXSONOFF keyword from each event file
    # If any of the event files have MXSONOFF set to 'OFF'
    # then do not run MXSGTI. If MXSONOFF is set to
    # 'ON' or UNKNOWN' then proceed to mxsgti.
    foreach my $evtfile ( @{$files{event_uf}} ) {
      my $mxsonoff = ahgen::get_keyword($evtfile,"EVENTS","MXSONOFF");
      unless ( defined $mxsonoff ) {
        ahlog::ah_out"In file '$evtfile\[EVENTS]', could not read keyword MXSONOFF";
        ahlog::ah_out"Skipping MXS.";
        $Params{calc_mxs} = 0;
      }
      if ( uc $mxsonoff eq 'OFF' ) {
        ahlog::ah_out"In file '$evtfile\[EVENTS]', MXSONOFF=$mxsonoff";
        ahlog::ah_out"Skipping MXS.";
        $Params{calc_mxs} = 0;
      }
    }

    # Create the fine and coarse MXS GTI
    if ( $Params{calc_mxs} ) {
      my $housekeeping_out = form_outfile_name($housekeeping);
      my $basename_hk = basename($housekeeping_out);
      $basename_hk =~ s/\.hk1.*$/_hk1/;
      my $mxstime_log = $Params{tempstem} . $basename_hk . "_mxstime.log";

      # MXSGTI
      ahlog::ah_out "\nRunning mxsgti on $housekeeping";
      my $tmpfile = $Params{tempstem} . $basename_hk . "_mxsgti.hk1";
      ahapp::add_temp_file($tmpfile);
      $mxsgti_pars{logfile} = $Params{logstem} . $basename_hk . "_mxsgti.log";
      $status = ahpllib::run_mxsgti($housekeeping,$tmpfile,$mxfngti,$mxcsgti,\%mxsgti_pars); 
      File::Copy::move ("mxstime.log", $mxstime_log);
      if ($status) { ahlog::ah_err "run_mxsgti failed"; return $status; }
      if (copyFITSFile($tmpfile,$housekeeping_out)) { return 1; }


      # Update the files hash for the created GTI files
      $files{mxfngti} = $mxfngti;
      $files{mxcsgti} = $mxcsgti;
      $sxsflagpix_pars{inmxsgti} = $mxfngti; 
      $sxsflagpix_pars{calcmxs} = $Params{calcmxs}; 
      # Update the coordinate keywords for the MXS GTI extensions
      push( @filelist_output, $mxfngti );
      push( @filelist_output, $mxcsgti );
    }
  } else {
    # Just copy the input MXS GTI files to the output directory
    # if they exist
    if ( -e $files{mxfngti} ) {
      if (copyFITSFile($files{mxfngti},$mxfngti)) { return 1; }
      $sxsflagpix_pars{inmxsgti} = $mxfngti; 
      $sxsflagpix_pars{calcmxs} = $Params{calcmxs}; 
      push( @filelist_output, $mxfngti );
    }
    if ( -e $files{mxcsgti} ) {
      if (copyFITSFile($files{mxcsgti},$mxcsgti)) { return 1; }
      push( @filelist_output, $mxcsgti );
    }
  }

  # Verify GTI files have
  # Get the general observation GTI
  if ( ! has_good_time( $obsgti, "GTIOBS" ) ) {
    ahlog::ah_out "$obsgti\[GTIOBS] has no GTI.";
    return 1;
  }
  push @ingtis , $obsgti   . "[GTIOBS]";

  if ( ! has_good_time( $telgti, "GTITEL" ) ) {
    ahlog::ah_out "$telgti\[GTITEL] has no GTI.";
    return 1;
  }
  push @ingtis , $telgti   . "[GTITEL]";

  # Get the MXS GTI if we are not using Pixel 12
  if ( lc $linetocorrect eq "cuka" or
       lc $linetocorrect eq "cukb" or
       lc $linetocorrect eq "crka" or
       lc $linetocorrect eq "crkb" ) {
    if ( ! has_good_time( $mxfngti, "GTIMXSFNON13" ) ) {
      ahlog::ah_out "$mxfngti\[GTIMXSFNON13] has no GTI.";
      return 1;
    }
    push @ingtis , $mxfngti   . "[GTIMXSFNON13]"
  } elsif ( lc $linetocorrect eq "alka" or
            lc $linetocorrect eq "alkb" or
            lc $linetocorrect eq "mgka" ) {
    if ( ! has_good_time( $mxfngti, "GTIMXSFNON24" ) ) {
      ahlog::ah_out "$mxfngti\[GTIMXSFNON24] has no GTI.";
      return 1;
    }
    push @ingtis , $mxfngti   . "[GTIMXSFNON24]"
  }

  # Merge GTI files
  $status = ahfilterlib::merge_gti(\@ingtis,$sxsgaingti,"AND","GTI",{});
  if ( $status ) { ahlog::ah_err "merge_gti failed"; return $status; }
  if ( ! has_good_time( $sxsgaingti, "GTI" ) ) {
    ahlog::ah_out "$sxsgaingti\[GTI] has no GTI.";
    return 1;
  }
  ahfilterlib::calc_timing_keys($sxsgaingti,"GTI",$leapsecfile);

  # sxsgain requires the DETNAM to be PIXEL if a single extension
  # GTI file and calmethod=MXS
  ahgen::set_keyword($sxsgaingti,"GTI","DETNAM","PIXEL");

  # Find the first and the last SXS files
  # due to file splitting. We want to attach a GTI extension to the output
  # event file if there is no GTI attached to the event file. 
  # If there is a file split, the first file in time uses the 
  # GTIPOINT TSTART to the event file TSTOP. Each subsequent file split 
  # uses GTIPOINT between that file's TSTART/TSTOP
  # The last file in time uses TSTART from the event file and TSTOP from GTIPOINT
  #
  # Example: An event file is split three times. 
  # GTIPOINT: |----------------------------------------------| 
  # EVENTS  :   |-----------|--------------|--------------|
  # GTI1    : |-------------|
  # GTI2    :               |--------------|
  # GTI3    :                              |-----------------|
  my $first_indx;
  my $last_indx;
  my $tstart_prev=0;
  my $tstop_prev=0;

  # Find the first and last indx for SXS
  foreach my $evtfile ( @{$files{event_uf}} ) {
    my $mode = find_file_mode($evtfile);
    my $indx = find_file_indx($evtfile);
    # Skip any slew file
    if ( lc $mode eq "s" ) { next; }

    if ( $indx == 0 ) {
      # If the indx is 0, then there is no file split
      # We can ignore the first and last indx
      $first_indx=0;
      $last_indx=0;
      next;
    }
    my $tstart = ahgen::get_keyword($evtfile,"EVENTS","TSTART");
    unless ( defined $tstart ) {
      ahlog::ah_out"In file '$evtfile', could not read keyword TSTART";
      ahlog::ah_out"Skipping file.";
      next;
    }
    my $tstop = ahgen::get_keyword($evtfile,"EVENTS","TSTOP");
    unless ( defined $tstop ) {
      ahlog::ah_out"In file '$evtfile', could not read keyword TSTOP";
      ahlog::ah_out"Skipping file.";
      next;
    }
    if ( $tstart_prev == 0 ) { $tstart_prev = $tstart; $first_indx = $indx; }
    if ( $tstop_prev == 0 ) { $tstop_prev = $tstop; $last_indx = $indx; }
    if ( $tstart < $tstart_prev ) { $tstart_prev = $tstart; $first_indx = $indx; }
    if ( $tstop > $tstop_prev ) { $tstop_prev = $tstop; $last_indx = $indx; }
  }

  # SXSANTICOPI
  ahlog::ah_out "\nRunning sxsanticopi on $antico";
  my $basename_ac = basename($antico);
  $basename_ac =~ s/\.evt.*$//;
  my $tmpfile_ac = $Params{tempstem} . $basename_ac . "_sxsanticopi.evt";
  ahapp::add_temp_file($tmpfile_ac);
  $sxsanticopi_pars{logfile} = $Params{logstem} . $basename_ac . "_sxsanticopi.log";
  $status = ahpllib::run_anticopi($antico,$tmpfile_ac,\%sxsanticopi_pars); 
  if ($status) { ahlog::ah_err "run_anticopi failed"; return $status; }
  if (copyFITSFile($tmpfile_ac,$antico_out)) { return 1; }
  push( @filelist_output, $antico_out );

  # SXS PRERUN EVENT CALIBRATION
  ahlog::ah_out "Calibrating SXS events";
  foreach my $sxsfile ( sort @{$files{event_uf}} ) {

    #############  setup input/output files #############

    # Set up input and output files
    my $sxsfile_out   = form_outfile_name($sxsfile);

    # Get basenames (for temporary files) and remove the extension
    my $basename      = basename($sxsfile_out);
    $basename =~ s/\.evt.*$//;

    # Determine the file index for this sxs file
    my $indx = find_file_indx($sxsfile_out);

    # Skip the file if there are no events
    my $naxis2 = ahgen::get_keyword($sxsfile,"EVENTS","NAXIS2");
    unless ( defined $naxis2 ) {
      ahlog::ah_out"In file '$sxsfile', could not read keyword NAXIS2";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxsfile (NAXIS2 not defined)";
      $Params{numerrs} += 1;
      next;
    }
    unless ( $naxis2 ) { 
      ahlog::ah_out"No events found in file '$sxsfile'";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxsfile (NAXIS2=0)";
      $Params{numerrs} += 1;
      next;
    }
    # read keywords TSTART, TSTOP, DATE-OBS, and OBSMODE
    my $tstart = ahgen::get_keyword($sxsfile,"EVENTS","TSTART");
    unless ( defined $tstart ) {
      ahlog::ah_out"In file '$sxsfile', could not read keyword TSTART";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxsfile (TSTART not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $tstop = ahgen::get_keyword($sxsfile,"EVENTS","TSTOP");
    unless ( defined $tstop ) {
      ahlog::ah_out"In file '$sxsfile', could not read keyword TSTOP";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxsfile (TSTOP not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $dateobs = ahgen::get_keyword($sxsfile,"EVENTS","DATE-OBS");
    unless ( defined $dateobs ) {
      ahlog::ah_out"In file '$sxsfile', could not read keyword DATE-OBS";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxsfile (DATE-OBS not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $obsmode = ahgen::get_keyword($sxsfile,"EVENTS","OBS_MODE");
    unless ( defined $obsmode ) {
      ahlog::ah_out"In file '$sxsfile', could not read keyword OBS_MODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxsfile (OBS_MODE not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $detnam = ahgen::get_keyword($sxsfile,"EVENTS","DETNAM");
    if(ahgen::get_error_flag()) {
      ahlog::ah_err "Could not get DETNAM keyword from $sxsfile";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxsfile (DETNAM not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $filter = ahgen::get_keyword($sxsfile,"EVENTS","FILTER");
    if(ahgen::get_error_flag()) {
      ahlog::ah_err "Could not get FILTER keyword from $sxsfile";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxsfile (FILTER not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $datamode = ahgen::get_keyword($sxsfile,"EVENTS","DATAMODE");
    unless ( defined $datamode ) {
      ahlog::ah_out"In file '$sxsfile', could not read keyword DATAMODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxsfile (DATAMODE not defined)";
      $Params{numerrs} += 1;
      next;
    }

    # Print some debug statements
    ahlog::ah_debug "Input File       : $sxsfile";
    ahlog::ah_debug "Output File      : $sxsfile_out";
    ahlog::ah_debug "tstart           : $tstart";
    ahlog::ah_debug "tstop            : $tstop";
    ahlog::ah_debug "dateobs:         : $dateobs";
    ahlog::ah_debug "obsmode:         : $obsmode";

    # can this file ($sxsfile) be processed with the caldb files?
    if ( $tstop < $sxs_start ) {
      ahlog::ah_out"SXS input file $sxsfile occurs before CALDB (sxs_start)";
      ahlog::ah_out"SXS input file $sxsfile will NOT be re-calibrated";
      push @error_output, "Skipped file: $sxsfile (sxs_start occurs before CALDB)";
      $Params{numerrs} += 1;
      next;
    }

    ahlog::ah_out "Calibrating SXS file $sxsfile";

    my $tmpfile = "";
    # Copy the infile(s) to the output file(s)
    if (copyFITSFile($sxsfile,$sxsfile_out)) { return 1; }

    # COORDEVT (events)
    my %coordevt_pars;
    if( uc $obsmode eq "POINTING" ) {
      %coordevt_pars = %coordevt_pars_pointing;
    } else {
      %coordevt_pars = %coordevt_pars_slew;
    }

    # COORDEVT
    ahlog::ah_out "\nRunning coordevt on file $sxsfile_out";
    $tmpfile = $Params{tempstem} . $basename . "_coordevt.evt";
    ahapp::add_temp_file($tmpfile);
    $coordevt_pars{logfile} = $Params{logstem} . $basename . "_coordevt.log";
    $status = ahpllib::run_coordevt($sxsfile_out,$tmpfile,\%coordevt_pars);
    if ( $status ) { ahlog::ah_err "coordevt failed"; return 1; }
    if(copyFITSFile($tmpfile,$sxsfile_out)) { return 1; }

    # SXSFLAGPIX (1)
    ahlog::ah_out "\nRunning sxsflagpix (1) on file $sxsfile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxsflagpix_1.evt";
    ahapp::add_temp_file($tmpfile);
    $sxsflagpix_pars{logfile} = $Params{logstem} . $basename . "_sxsflagpix_1.log";
    $status = ahpllib::run_sxsflagpix($sxsfile_out,$tmpfile,$antico_out,\%sxsflagpix_pars); 
    if ($status) { ahlog::ah_err "run_sxsflagpix failed"; return $status; }
    if(copyFITSFile($tmpfile,$sxsfile_out)) { return 1; }

    # SXSSECID (1)
    ahlog::ah_out "\nRunning sxssecid (1) on file $sxsfile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxssecid_1.evt";
    ahapp::add_temp_file($tmpfile);
    $sxssecid_pars{logfile} = $Params{logstem} . $basename . "_sxssecid_1.log";
    $status = ahpllib::run_sxssecid($sxsfile_out,$tmpfile,\%sxssecid_pars); 
    if ($status) { ahlog::ah_err "run_sxssecid failed"; return $status; }
    if(copyFITSFile($tmpfile,$sxsfile_out)) { return 1; }

    # Attach GTI due to file splitting if there is no GTI already attached. 
    # We want to attach a GTI extension to the output
    # event file if there is no GTI attached to the event file. 
    # If there is a file split, the first file in time uses the 
    # GTIPOINT TSTART to the event file TSTOP. Each subsequent file split 
    # uses GTIPOINT between that file's TSTART/TSTOP
    # The last file in time uses TSTART from the event file and TSTOP from GTIPOINT
    #
    # Example: An event file is split three times. 
    # GTIPOINT: |----------------------------------------------| 
    # EVENTS  :   |-----------|--------------|--------------|
    # GTI Attached:
    # GTI[1]  : |-------------|
    # GTI[2]  :               |--------------|
    # GTI[3]  :                              |-----------------|
    if ( ! ahgen::check_hdu_exists($sxsfile,"GTI") ) {
      ahlog::ah_out "\nAttaching GTI to file $sxsfile_out";
      # Get the GTI for this file split (mode and slew)
      # We want to make sure that the first STOP is greater than TSTART of the events
      # and that the last START is less than TSTOP of the events
      my $cutgti = $Params{tempstem} . $basename . "_split.gti";
      ahapp::add_temp_file($cutgti);
      my $gtiext = "";
      if ( uc $obsmode eq 'SLEW' ) {
        $gtiext = "GTISLEW";
        # Attach updated GTI to SFFa
        # No GTI attached to sff, expanded, recgain
        $status = copy_hdu($obsgti,"GTISLEW][col #EXTNAME=\"GTI\"", $sxsfile_out);
        if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
      } else {

        # Attach the GTI to the reconstructed event file
        if ( $indx == 0 ) {
          # Attach directly the GTIPOINT if there is no file splitting
          # Change the extension name from GTIPOINT to GTI
          $status = copy_hdu($obsgti,"GTIPOINT][col #EXTNAME=\"GTI\"", $sxsfile_out);
          if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
        } else {
          # Get the GTI for this file split
          # We want to make sure that the first STOP is greater than TSTART of the events
          # and that the last START is less than TSTOP of the events
          if ( $indx == $first_indx ) {
            # Bound the GTIPOINT to the tstop of the event file
            $status = copy_hdu($obsgti,"GTIPOINT][START<$tstop][col #EXTNAME=\"GTI\"", $cutgti);
            if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
          } elsif ( $indx == $last_indx ) {
            # Bound the GTIPOINT to the tstart of the event file
            $status = copy_hdu($obsgti,"GTIPOINT][STOP>$tstart][col #EXTNAME=\"GTI\"", $cutgti);
            if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
          } else {
            # Bound the GTIPOINT to the tstart/tstop of the event file
            $status = copy_hdu($obsgti,"GTIPOINT][STOP>$tstart&&START<$tstop][col #EXTNAME=\"GTI\"",$cutgti);
            if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
          }

          if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
          my $lastrow = ahgen::get_keyword($cutgti,"GTI","NAXIS2");
          unless ( defined $lastrow ) {
            ahlog::ah_out"In file '$cutgti\[GTI]', could not read keyword NAXIS2";
            ahlog::ah_out"Skipping file.";
            push @error_output, "Error in file: $sxsfile (Could not append GTI)";
            $Params{numerrs} += 1;
            next;
          }

          # If the indx is not the first, use the TSTART of GTIPOINT
          # Otherwise use the TSTART of the event file
          if ( $indx != $first_indx ) {
            # Make sure that the first START == TSTART
            $status = ahgen::run_ftool('ftedit', "infile=$cutgti\[GTI]","column=START",
                                       "row=1","value=$tstart");
            if ($status) { ahlog::ah_err "ftedit failed."; return $status; }
            # Update TSTART and TSTOP keywords to match GTI file
            ahgen::set_keyword( $cutgti, "GTI", 'TSTART' , $tstart ) ;
          }
          
          # If the indx is not the last, use the TSTOP of GTIPOINT
          if ( $indx != $last_indx ) {
            # Make sure that the last STOP == TSTOP
            $status = ahgen::run_ftool('ftedit',"infile=$cutgti\[GTI]","column=STOP",
                                       "row=$lastrow","value=$tstop");
            if ($status) { ahlog::ah_err "ftedit failed."; return $status; }
            # Update TSTART and TSTOP keywords to match GTI file
            ahgen::set_keyword( $cutgti, "GTI", 'TSTOP' , $tstop ) ;

          }

          # Attach updated GTI to SFFa
          # No GTI attached to sff, expanded, recgain
          $status = copy_hdu($cutgti,"GTI", $sxsfile_out);
          if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
        }
      } # end attach GTI
    } # end if GTI extension exists

    # Update the INSTRUME, DETNAM, DATAMODE keywords in the GTI extension
    ahgen::set_keyword( $sxsfile_out , "GTI", 'INSTRUME' , "SXS" ) ;
    ahgen::set_keyword( $sxsfile_out , "GTI", 'DETNAM' , $detnam ) ;
    ahgen::set_keyword( $sxsfile_out , "GTI", 'FILTER' , $filter ) ;
    ahgen::set_keyword( $sxsfile_out , "GTI", 'DATAMODE' , $datamode ) ;

    # Set the coordinate keywords in the output SXS event file
    push( @filelist_output, $sxsfile_out );
    push @calibrated_sxs_uf_evt_files_prerun, $sxsfile_out;

  } # end loop over each sxs file (prerun)
  
  # Make a gain history file based on input linetocorrect parameter
  # First merge and save event file all of the input event files, ignoring
  # any sxs filters
  #
  # The merged data is filtered by removing any baseline events
  # as well as the observation GTI and lost off GTI. Additionally,
  # pixels are filtered based on the linetocorrect parameter.
  # 
  # If linetocorrect is MnKa, then the data is fit on the calibration
  # pixel, pixel 12
  #
  # Otherwise the data is fit using pixels 0:11 and 13:35 and the MXS GTI
  # in addition to the above filtering.
  {
    my @mergefiles = ();
    my $calname = "";

    # Merge all of the files
    # Do not merge any files with zero rows, these are missing columns
    foreach my $sxsfile ( sort @calibrated_sxs_uf_evt_files_prerun ) {
      my $naxis2 = ahgen::get_keyword($sxsfile,"EVENTS","NAXIS2");
      unless ( defined $naxis2 ) {
        ahlog::ah_out"In file '$sxsfile', could not read keyword NAXIS2";
        ahlog::ah_out"Skipping file.";
        next;
      }
      if ( $naxis2 ) { 
        push @mergefiles , $sxsfile;
      }
    }

    # Check that we have files to merge
    if ( @mergefiles == 0 ) {
      ahlog::ah_err " *** No SXS pointing or slew files found." .
                       ' Cannot create pixel 12 file.' ;
      return 1;
    }

    # Set up the input GTI files and calibration method
    if ( lc $calmethod eq "cal-pix" ) {
      $calname = "pxcal";
    } else {
      $calname = lc $linetocorrect;
    }
    
    # Get the mode from the first SXS event file calibrated
    my $mode = get_sxs_mode( $mergefiles[0] );
    my $calfile         = $Params{tempstem} . "calfile_" . $calname . ".evt";
    my $mergefile       = $Params{tempstem} . "calfile_" . $calname . "_merged.evt";
    my $pxmerge_presort = $Params{tempstem} . "calfile_" . $calname . "_presort.evt";
    my $pxmerge_itype   = $Params{tempstem} . "calfile_" . $calname . "_itype.evt";
    ahapp::add_temp_file($calfile);
    ahapp::add_temp_file($mergefile);
    ahapp::add_temp_file($pxmerge_presort);
    ahapp::add_temp_file($pxmerge_itype);

    $driftfile          = catfile($Params{outdir},$Params{stemoutputs} . "sxs_$mode\_$calname.ghf");

    # If there is only a single event file, then just copy the file
    if ( @mergefiles == 1 ){
      if(ahgen::copy_fits_file($mergefiles[0]."[EVENTS]",$mergefile,"copyall=no")) { return 1; }
    } else {
      # Run ftmerge to merge all of the events
      ahlog::ah_out "\nMerging SXS files:\n  " . join("\n  ",@mergefiles);
      $status = ahgen::run_ftool("ftmerge",join("[EVENTS] ",@mergefiles),$mergefile,"copyall=no");
      if ($status) { ahlog::ah_err "ftmerge failed"; return $status; }
    }
    # Append the sxsgain GTI to the pixel 12 event file
    $status = ahgen::copy_hdu($sxsgaingti,"GTI",$mergefile);
    if ($status) {
      ahlog::ah_err "Could append $sxsgaingti\[GTI] to $mergefile.";
      return $status;
    }

    # Build the filtering expression based on the linetocorrect
    if ( lc $Params{calmethod} eq "cal-pix" ) {
      # Select on PIXEL eq 12 
      ahlog::ah_out "\nSelecting on PIXEL eq 12 from $mergefile";
      $status = run_ftselect($mergefile,"EVENTS",$pxmerge_presort,"PIXEL==12"); 
    } else {
      ahlog::ah_out "\nSelecting on PIXEL ne 12 from $mergefile";
      # Select on PIXEL ne 12
      $status = run_ftselect($mergefile,"EVENTS",$pxmerge_presort,"PIXEL!=12"); 
    }
    if ($status) { ahlog::ah_err "ftselect failed"; return $status; }

    # Sort the merged events by time
    ahlog::ah_out "Sorting $mergefile by time";
    $status = ahgen::run_ftool("ftsort",$pxmerge_presort."[EVENTS]",$calfile,"TIME","clobber=yes"); unlink $pxmerge_presort;
    if($status) { ahlog::ah_err "ftsort failed."; unlink $calfile; return $status; }

    # Select on PIXEL eq 12, ITYPE 0:4 
    ahlog::ah_out "\nSelecting on ITYPE 0:4 from $calfile";
    $status = run_ftselect($calfile,"EVENTS",$pxmerge_itype,"ITYPE<5"); 

    # SXSGAIN
    ahlog::ah_out "\nRunning sxsgain on $calfile";
    $sxsgain_pars{logfile} = $Params{logstem} . $Params{tempstem} . "calfile_" . $calname . "_sxsgain.log";
    $status = ahpllib::run_sxsgain($pxmerge_itype,$driftfile,\%sxsgain_pars);
    if ($status) { ahlog::ah_err "run_sxsgain failed"; return $status; }

    # Check that we have a successful fit
    my $naxis2 = ahgen::get_keyword($driftfile,"Drift_energy","NAXIS2");
    unless ( defined $naxis2 ) {
      ahlog::ah_out"In file '$driftfile\[Drift_energy]', could not read keyword NAXIS2";
      return 1;
    }
    unless ( $naxis2 ) { 
      ahlog::ah_out"No events found in file '$driftfile'";
      return 1;
    }

    push( @filelist_output, $driftfile );

  } # end make gain history file


  # SXS EVENT CALIBRATION
  foreach my $sxsfile_out ( sort @calibrated_sxs_uf_evt_files_prerun ) {

    # Get basenames (for temporary files) and remove the extension
    my $basename      = basename($sxsfile_out);
    $basename =~ s/\.evt.*$//;

    my $tmpfile = "";

    # SXSPHA2PI (1)
    ahlog::ah_out "\nRunning sxspha2pi (1) on $sxsfile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxspha2pi_1.evt";
    ahapp::add_temp_file($tmpfile);
    $sxspha2pi_pars{logfile} = $Params{logstem} . $basename . "_sxspha2pi_1.log";
    $status = ahpllib::run_sxspha2pi($sxsfile_out,$tmpfile,$driftfile,\%sxspha2pi_pars);
    if ($status) { ahlog::ah_err "run_sxspha2pi failed"; return $status; }
    if(copyFITSFile($tmpfile,$sxsfile_out)) { return 1; }

    # SXSFLAGPIX (2)
    ahlog::ah_out "\nRunning sxsflagpix (2) on $sxsfile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxsflagpix_2.evt";
    ahapp::add_temp_file($tmpfile);
    $sxsflagpix_pars2{logfile} = $Params{logstem} . $basename . "_sxsflagpix_2.log";
    $status = ahpllib::run_sxsflagpix($sxsfile_out,$tmpfile,$antico_out,\%sxsflagpix_pars2); 
    if ($status) { ahlog::ah_err "run_sxsflagpix failed"; return $status; }
    if(copyFITSFile($tmpfile,$sxsfile_out)) { return 1; }

    # SXSSECID (2)
    ahlog::ah_out "\nRunning sxssecid (2) on $sxsfile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxssecid_2.evt";
    ahapp::add_temp_file($tmpfile);
    $sxssecid_pars{logfile} = $Params{logstem} . $basename . "_sxssecid_2.log";
    $status = ahpllib::run_sxssecid($sxsfile_out,$tmpfile,\%sxssecid_pars2); 
    if ($status) { ahlog::ah_err "run_sxssecid failed"; return $status; }
    if(copyFITSFile($tmpfile,$sxsfile_out)) { return 1; }

    # SXSSECCOR
    ahlog::ah_out "\nRunning sxsseccor on file $sxsfile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxsseccor.evt";
    ahapp::add_temp_file($tmpfile);
    $sxsseccor_pars{logfile} = $Params{logstem} . $basename . "_sxsseccor.log";
    $status = ahpllib::run_sxsseccor($sxsfile_out,$tmpfile,\%sxsseccor_pars); 
    if ($status) { ahlog::ah_err "run_sxsseccor failed"; return $status; }
    if(copyFITSFile($tmpfile,$sxsfile_out)) { return 1; }

    # SXSPHA2PI (2)
    ahlog::ah_out "\nRunning sxspha2pi (2) on $sxsfile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxspha2pi_2.evt";
    ahapp::add_temp_file($tmpfile);
    $sxspha2pi_pars{logfile} = $Params{logstem} . $basename . "_sxspha2pi_2.log";
    $status = ahpllib::run_sxspha2pi($sxsfile_out,$tmpfile,$driftfile,\%sxspha2pi_pars);
    if ($status) { ahlog::ah_err "run_sxspha2pi failed"; return $status; }
    if(copyFITSFile($tmpfile,$sxsfile_out)) { return 1; }

    # SXSPERSEUS
    if ( $gainperseus ) {
      ahlog::ah_out "\nRunning sxsperseus on $sxsfile_out";
      $tmpfile = $Params{tempstem} . $basename . "_sxsperseus.evt";
      ahapp::add_temp_file($tmpfile);
      $sxsperseus_pars{logfile} = $Params{logstem} . $basename . "_sxsperseus.log";
      $status = ahpllib::run_sxsperseus($sxsfile_out,$tmpfile,$driftfile,\%sxsperseus_pars);
      if ($status) { ahlog::ah_err "run_sxsperseus failed"; return $status; }
      if(copyFITSFile($tmpfile,$sxsfile_out)) { return 1; }
    }

    push @calibrated_sxs_uf_evt_files, $sxsfile_out;

  } # end loop over each sxs file

  # Create an unfiltered calibration event file
  # Create this file after all calibration is completed so PI column is filled
  {
    my @mergefiles = ();
    my $calname = "";

    # Merge all of the files
    # Do not merge any files with zero rows, these are missing columns
    foreach my $sxsfile ( sort @calibrated_sxs_uf_evt_files ) {
      my $naxis2 = ahgen::get_keyword($sxsfile,"EVENTS","NAXIS2");
      unless ( defined $naxis2 ) {
        ahlog::ah_out"In file '$sxsfile', could not read keyword NAXIS2";
        ahlog::ah_out"Skipping file.";
        next;
      }
      if ( $naxis2 ) { 
        push @mergefiles , $sxsfile;
      }
    }

    # Check that we have files to merge
    if ( @mergefiles == 0 ) {
      ahlog::ah_err " *** No SXS pointing or slew files found." .
                       ' Cannot create pixel 12 file.' ;
      return 1;
    }
    # Set up the input GTI files and calibration method
    if ( lc $calmethod eq "cal-pix" ) {
      $calname = "pxcal";
    } else {
      $calname = lc $linetocorrect;
    }

    my $mode = get_sxs_mode( $mergefiles[0] );
    my $pxmerge            = catfile($Params{outdir},$Params{stemoutputs} . "sxs_a0$calname$mode\_uf.evt");
    my $pxmerge_base = basename($pxmerge);
    $pxmerge_base =~ s/\.evt.*$//;
    my $mergefile       = $Params{tempstem} . $pxmerge_base . "_merged.evt";
    my $pxmerge_presort = $Params{tempstem} . $pxmerge_base . "_presort.evt";
    my $pxmerge_itype   = $Params{tempstem} . $pxmerge_base . "_itype.evt";
    ahapp::add_temp_file($mergefile);
    ahapp::add_temp_file($pxmerge_presort);
    ahapp::add_temp_file($pxmerge_itype);

    # If there is only a single event file, then just copy the file
    if ( @mergefiles == 1 ){
      if(ahgen::copy_fits_file($mergefiles[0]."[EVENTS]",$mergefile,"copyall=no")) { return 1; }
    } else {
      # Run ftmerge to merge all of the events
      ahlog::ah_out "\nMerging SXS files:\n  " . join("\n  ",@mergefiles);
      $status = ahgen::run_ftool("ftmerge",join("[EVENTS] ",@mergefiles),$mergefile,"copyall=no");
      if ($status) { ahlog::ah_err "ftmerge failed"; return $status; }
    }
    # Append the sxsgain GTI to the pixel 12 event file
    $status = ahgen::copy_hdu($sxsgaingti,"GTI",$mergefile);
    if ($status) {
      ahlog::ah_err "Could append $sxsgaingti\[GTI] to $mergefile.";
      return $status;
    }

    # Build the filtering expression based on the linetocorrect
    if ( lc $Params{calmethod} eq "cal-pix" ) {
      # Select on PIXEL eq 12 
      ahlog::ah_out "\nSelecting on PIXEL eq 12 from $mergefile";
      $status = run_ftselect($mergefile,"EVENTS",$pxmerge_presort,"PIXEL==12"); 
    } else {
      ahlog::ah_out "\nSelecting on PIXEL ne 12 from $mergefile";
      # Select on PIXEL ne 12
      $status = run_ftselect($mergefile,"EVENTS",$pxmerge_presort,"PIXEL!=12"); 
    }
    if ($status) { ahlog::ah_err "ftselect failed"; return $status; }

    # Sort the merged events by time
    ahlog::ah_out "Sorting $mergefile by time";
    $status = ahgen::run_ftool("ftsort",$pxmerge_presort."[EVENTS]",$pxmerge,"TIME","clobber=yes"); unlink $pxmerge_presort;
    if($status) { ahlog::ah_err "ftsort failed."; unlink $pxmerge; return $status; }

    # Select on PIXEL eq 12, ITYPE 0:4 
    ahlog::ah_out "\nSelecting on ITYPE 0:4 from $pxmerge";
    $status = run_ftselect($pxmerge,"EVENTS",$pxmerge_itype,"ITYPE<5"); 

    push( @filelist_output, $pxmerge );

  }

  # Save lists of newly calibrated '_uf.evt'
  @{$files{event_uf}} = @calibrated_sxs_uf_evt_files;

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nEnd CalibrateEvents\n";
  }

  return 0;
  
}

# ------------------------------------------------------------------------------

sub CleanEvents {

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nCleanEvents\n";
  }

  my $status = 0;

  my $telgti       = $files{telgti};
  my $obsgti       = $files{obsgti};
  my $adrgti       = $files{adrgti};
  my $lostgti      = $files{lostgti};
  my $mxfngti      = $files{mxfngti};
  my $makefilter   = $files{makefilter};
  my $ehk          = $files{extended_housekeeping};
  my $select       = $Params{selectfile};
  my $leapsecfile  = $Params{leapsecfile};
  my $outdir       = $Params{outdir};
  my $stemoutputs  = $Params{stemoutputs};
  my $instrument   = $Params{instrument};
  my $screenlost   = $Params{screenlost};

  my @calibrated_sxs_cl_evt_files = ( );

  # Check that we have good GTI from pointing and attitude GTI
  if ( ! has_good_time( $obsgti, "GTIPOINT" ) ) {
    ahlog::ah_out "$obsgti\[GTIPOINT] has no GTI. Cannot clean SXS Data.";
    return 1;
  }
  if ( ! has_good_time( $obsgti, "GTIATT" ) ) {
    ahlog::ah_out "$obsgti\[GTIATT] has no GTI. Cannot clean SXS Data.";
    return 1;
  }

  # Check that we have good GTI from telemetry saturation GTI
  if ( ! has_good_time( $telgti, "GTITEL" ) ) {
    ahlog::ah_out "$telgti\[GTITEL] has no GTI. Cannot clean SXS Data.";
    return 1;
  }

  # Check that we have good GTI from telemetry saturation GTI
  if ( ! has_good_time( $adrgti, "GTIADROFF" ) ) {
    ahlog::ah_out "$adrgti\[GTIADROFF] has no GTI. Cannot clean SXS Data.";
    return 1;
  }

  # Check that we have good GTI from lost off GTI
  if ( ! has_good_time( $lostgti, "3" ) and $screenlost) {
    ahlog::ah_out "$lostgti\[3] has no GTI. Cannot clean SXS Data.";
    return 1;
  }

  # Check that we have good GTI from MXS off GTI
  if ( $files{mxfngti} ) {
    foreach my $ext ( qw( GTIMXSFNOFF13 GTIMXSFNOFF24 ) ) {
      if ( ! has_good_time( $mxfngti, $ext ) ) {
        ahlog::ah_out "$mxfngti\[$ext] has no GTI. Cannot clean SXS Data.";
        return 1;
      }
    }
  }

  # Set up label and GTI files for EHK/MKF
  my $mkflabel      = $Params{sxs_mkflabel};
  my $ehklabel      = $Params{sxs_ehklabel};
  my $evtlabel      = $Params{sxs_evtlabel};
  my $gtimkf        = $Params{tempstem} . lc $mkflabel . "_mkf.gti";
  my $gtiehk        = $Params{tempstem} . lc $ehklabel . "_ehk.gti";
  my $logfile = "";
  my $cpkeywords = "DATAMODE, DEC_NOM, DETNAM, INSTRUME, MJDREFI, MJDREFF, OBS_ID, PA_NOM, RA_NOM, TELESCOP";

  ahapp::add_temp_file ( $gtimkf );
  ahapp::add_temp_file ( $gtiehk );

  # Create the MKF GTI file
  $logfile = $Params{logstem} . "$mkflabel\_mkf_ahgtigen.log";
  my $gtistatus = create_gti(infile       => $makefilter,
                       outfile      => $gtimkf,
                       selectfile   => $select,
                       instrume     => $instrument,
                       label        => $mkflabel,
                       cpkeyword    => "NONE",
                       upkeyword    => "yes",
                       clobber => $ahapp::clobber ? "yes" : "no" ,
                       chatter => $ahapp::chatter,
                       logfile => $logfile,
                       );
  if ( $status ) { 
    if ( $status == 2 ) {
      ahlog::ah_out "$gtimkf has no GTI. Skipping SXS cleaning.";
    } else {
      ahlog::ah_err "ahgtigen failed. Skipping SXS cleaning.";
    }
    return 1;
  }
  if ( ! has_good_time( $gtimkf, "GTI" ) ) {
    ahlog::ah_out "$gtimkf has no GTI. Skipping SXS cleaning.";
    return 1;
  }

  # Rename MKF GTI extension
  ahgen::set_keyword($gtimkf,"GTI","EXTNAME","GTIMKF");

  # Create the EHK GTI file
  $logfile = $Params{logstem} . "$ehklabel\_ehk_ahgtigen.log";
  $gtistatus = create_gti(infile       => $ehk,
                       outfile      => $gtiehk,
                       selectfile   => $select,
                       instrume     => $instrument,
                       label        => $ehklabel,
                       cpkeyword    => "NONE",
                       upkeyword    => "yes",
                       clobber => $ahapp::clobber ? "yes" : "no" ,
                       chatter => $ahapp::chatter,
                       logfile => $logfile,
                       );
  if ( $status ) { 
    if ( $status == 2 ) {
      ahlog::ah_out "$gtiehk has no GTI. Skipping SXS Cleaning.";
    } else {
      ahlog::ah_err "ahgtigen failed. Skipping SXS cleaning.";
    }
    return 1;
  }
  if ( ! has_good_time( $gtiehk, "GTI" ) ) {
    ahlog::ah_out "$gtiehk has no GTI. Skipping SXS Cleaning.";
    return 1;
  }

  # Rename EHK GTI extension
  ahgen::set_keyword($gtiehk,"GTI","EXTNAME","GTIEHK");

  foreach my $sxsfile ( sort @{$files{event_uf}} ) {

    my $sxsfile_out   = form_cleaned_file_name($sxsfile);
    my @ingtis        = ();

    my $basename      = basename($sxsfile);
    $basename =~ s/\.evt.*$//;

    my $obsmode = ahgen::get_keyword( $sxsfile,"EVENTS","OBS_MODE");
    unless ( defined $obsmode ) {
      ahlog::ah_out"In file '$sxsfile', could not read keyword OBS_MODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxsfile_out (OBS_MODE not defined)";
      $Params{numerrs} += 1;
      next;
    }

    # Skip any slew event files
    if ( uc $obsmode eq "SLEW" ) { next; }

    ahlog::ah_debug "Input File       : $sxsfile";
    ahlog::ah_debug "Output File      : $sxsfile_out";
    ahlog::ah_debug "MKF Label        : $mkflabel";
    ahlog::ah_debug "EHK Label        : $ehklabel";
    ahlog::ah_debug "Event Label      : $evtlabel";

    # Verify that we have good GTI
    if ( ! has_good_time( $sxsfile, "GTI" ) ) {
      ahlog::ah_out "$sxsfile\[GTI] has no GTI. Cannot clean SXS Data.";
      push @error_output, "Skipped file: $sxsfile_out (No GTI)";
      $Params{numerrs} += 1;
      return 1;
    }


    # Append MKF and EHK GTI to uf file
    if (0 != ahgen::check_hdu_exists($sxsfile,"GTIMKF")) { ahgen::delete_hdu($sxsfile,"GTIMKF"); }
    ahgen::copy_hdu($gtimkf,"GTIMKF",$sxsfile);
    ahfilterlib::copy_keywords($sxsfile,"EVENTS",$sxsfile,"GTIMKF","",$cpkeywords);
    if (0 != ahgen::check_hdu_exists($sxsfile,"GTIEHK")) { ahgen::delete_hdu($sxsfile,"GTIEHK"); }
    ahgen::copy_hdu($gtiehk,"GTIEHK",$sxsfile);
    ahfilterlib::copy_keywords($sxsfile,"EVENTS",$sxsfile,"GTIEHK","",$cpkeywords);

    ahlog::ah_out "\nCleaning events in $sxsfile";

    # Load the GTI files into parameter
    push @ingtis, $sxsfile . "[GTI]";
    push @ingtis, $obsgti  . "[GTIPOINT]";
    push @ingtis, $obsgti  . "[GTIATT]";
    push @ingtis, $telgti  . "[GTITEL]";
    push @ingtis, $lostgti . "[3]" if $screenlost;
    push @ingtis, $adrgti  . "[GTIADROFF]" if $files{adrgti};
    push @ingtis, $mxfngti . "[GTIMXSFNOFF13]" if $files{mxfngti};
    push @ingtis, $mxfngti . "[GTIMXSFNOFF24]" if $files{mxfngti};
    push @ingtis, $gtimkf  . "[GTIMKF]";
    push @ingtis, $gtiehk  . "[GTIEHK]";

    # Merge the GTI and screen events
    $logfile = $Params{logstem} . $basename . "_ahscreen.log";
    $status = screen_events(infile      => $sxsfile,
                            outfile     => $sxsfile_out,
                            gtifile     => join(",",@ingtis),
                            upkeyword   => "yes",
                            cpkeyword   => "yes",
                            leapsecfile => $leapsecfile,
                            selectfile  => $select,
                            label       => $evtlabel,
                            clobber     => "yes",
                            chatter     => $ahapp::chatter,
                            logfile => $logfile,
                            );
    if ( $status ) { 
      if ( $status == 2 ) {
        ahlog::ah_info "HIGH", "ahscreen: merged GTI produced no time interval.";
        ahlog::ah_info "HIGH", "Skipping $sxsfile_out.";
        push @error_output, "Skipped file: $sxsfile_out (merged GTI produced no time interval)";
      } elsif ( $status == 3 ) {
        ahlog::ah_info "HIGH", "ahscreen: Filtered all events during event screening.";
        ahlog::ah_info "HIGH", "Skipping $sxsfile_out.";
        push @error_output, "Skipped file: $sxsfile_out (Filtered all events during event screening)";
      } else {
        ahlog::ah_err "ahscreen failed. Skipping $sxsfile_out.";
        push @error_output, "Skipped file: $sxsfile_out (ahscreen failed)";
      }
      unlink $sxsfile_out;
      $Params{numerrs} += 1;
      next;
    }

    # Update the TLMIN/TLMAX keyword since baseline events were removed
    my $picol = ahgen::get_column_num($sxsfile_out,"EVENTS","PI");
    ahgen::set_keyword($sxsfile_out,"EVENTS","TLMIN$picol",0);
    ahgen::set_keyword($sxsfile_out,"EVENTS","TLMAX$picol",32767);
    # Update the coordinate keywords since this is a new GTI
    push( @filelist_output, $sxsfile_out );

    push @calibrated_sxs_cl_evt_files, $sxsfile_out;

  } # end loop over unfiltered files

  # Save lists of newly calibrated '_uf.evt'
  @{$files{event_cl}} = @calibrated_sxs_cl_evt_files;

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nEnd ScreenEvents\n";
  }

  return 0;

}

# ------------------------------------------------------------------------------

sub CreateProducts {

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nCreateProducts\n";
  }

  my $status = 0;
  my $region = $files{regionfile};
  my $instrument = $Params{instrument};

  foreach my $sxsfile ( sort @{$files{event_cl}} ) {

    # Calculate the output file names
    my $pha = $sxsfile;
    $pha =~ s/_cl.evt/_src.pha/;
    my $img = $sxsfile;
    $img =~ s/_cl.evt/_src.img/;
    my $lc = $sxsfile;
    $lc =~ s/_cl.evt/_src.lc/;

    # Run the extractor to create spectra, lightcurves and images
    $status = extract(infile  => $sxsfile,
          phafile => $pha,
          imgfile => $img,
          lcfile  => $lc,
                      region  => $region,
          type    => $instrument,
          );
    if ( $status ) { ahlog::ah_err "extractor failed"; return 1; }

    # +++ Create gif images

  }

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nEnd CreateProducts\n";
  }

  return 0;

}

# ------------------------------------------------------------------------------

sub finalize {

  my $status = 0;
  my $inst = $Params{instrument};

  # Print list of output files calibrated or cleaned, and any products created
  if ( $Params{stage1_switch} ) {
    ahlog::ah_debug "$inst event file(s) calibrated:" if @{$files{event_uf}};
    foreach my $filename ( @{$files{event_uf}} ) {
      ahlog::ah_debug "  $filename\n";
    }
  }
  if ( $Params{stage2_switch} ) {
    ahlog::ah_debug "$inst file(s) screened:" if @{$files{event_cl}};
    foreach my $filename ( @{$files{event_cl}} ) {
      ahlog::ah_debug "  $filename\n";
    }
  }
  if ( $Params{stage3_switch} ) {
  }

  # Check each file
  @filelist_output = sort( @filelist_output );
  my $prev = 'nonesuch';
  my @filelist_output_unique = grep( $_ ne $prev && ( ( $prev ) = $_ ), @filelist_output );

  foreach my $infile ( @filelist_output_unique ) {

    ahlog::ah_debug "\nStamping ahpipeline parameters on file $infile\n" ;

    ## Open file and count number of HDU's (Headers)

    my $hdutotal = ahgen::get_total_hdu($infile);

    # Loop on HDUs and write the coordinate keywords
    for ( my $i1 = 0 ; $i1 < $hdutotal ; $i1++ ) {
      add_coord_keys($infile,$i1);
    }

    ## Update the CHECKSUM and DATASUM keywords
    $status = update_checksum_and_verify($infile);
    unless ( $status ) { ahlog::ah_err "verify failed for $infile"; return $status; }
  }
  ahgen::run_ftool("pset","sxspipeline","numerrs=$Params{numerrs}");

  # Print the errors found from the pipeline
  if ( $Params{numerrs} ) {
    ahlog::ah_out "SXSPIPELINE had the following errors/warnings:";
    foreach my $errlog ( @error_output ) {
      ahlog::ah_out "  $errlog";
    }
  }

  return 0;

}

# ------------------------------------------------------------------------------

sub form_outfile_name {

  my $infile = shift;
  my $outdir = $Params{outdir};
  my $steminputs = $Params{steminputs};
  my $stemoutputs = $Params{stemoutputs};
  my $zpatt = qr/(\.Z|\.z|\.gzip|\.GZIP|\.gz|\.GZ|\.zip\.ZIP)?/;
  my $outfile = "";

  # Get the basename of the input file and calculate the output file name
  # Using the outdir and stemoutputs parameters
  my $basename = basename($infile);
  my ($filebase) = $basename =~ /$steminputs(.*)/;

  # Strip off any 'zip' pattern
  $filebase =~ s/$zpatt$//;

  $outfile = catfile($outdir , $stemoutputs . $filebase);

  return $outfile ;

}

# ------------------------------------------------------------------------------

sub form_cleaned_file_name {

  my $infile = shift;
  my $outdir = $Params{outdir};
  my $steminputs = $Params{steminputs};
  my $stemoutputs = $Params{stemoutputs};
  my $outfile = "";

  # Get the basename of the input file and calculate the output file name
  # Using the outdir and stemoutputs parameters
  my $basename = basename($infile);
  my ($filebase) = $basename =~ /$steminputs(.*)/;
  # Strip off any 'zip' pattern
  $filebase =~ s/$zpatt$//;
  $outfile = catfile($outdir , $stemoutputs . $filebase);
  $outfile =~ s/_uf/_cl/g;

  return $outfile ;

}

# ------------------------------------------------------------------------------

sub find_file_mode {

  my $evtfile = shift;

  my ($mode) = $evtfile =~ /sxs_([ps])[0-9]px[0-9]{4}_uf\.evt$zpatt$/;

  return $mode;

}

# ------------------------------------------------------------------------------

sub find_file_indx {

  my $evtfile = shift;

  my ($indx) = $evtfile =~ /sxs_[ps]([0-9])px[0-9]{4}_uf\.evt$zpatt$/;

  return $indx;

}

# ------------------------------------------------------------------------------

sub get_sxs_mode {

  my $infile = shift;
  my $mode = "";

  ($mode) = $infile =~ qr/sxs_[ps][0-9]px[0-9]([0-9]{3})_[a-z]{2}\.evt$zpatt$/,
  return $mode;

}

# ------------------------------------------------------------------------------

sub create_gti_lost {

  my $filename = shift;
  my $tstart = shift;
  my $tstop  = shift;

  my $status = 0;

  my $filename_base = basename($filename);
  $filename_base =~ s/\.evt.*$//;

  my $filename_ac = $Params{tempstem} . $filename_base . '_gtilost_ac.gti';
  my $filename_base_ac = basename($filename_ac);
  $filename_base_ac =~ s/\.evt.*$//;

  # Create the header template to a file.
  my($header_template) = $Params{tempstem} . $filename_base . '_header_template.tmp';
  open HEADER, ">$header_template";
  print HEADER "START               1D s\n";
  print HEADER "STOP                1D s\n";
  print HEADER "PIXEL               1B\n";
  close HEADER;

  # Create the keyword template to a file.
  my($keyword_template_px) = $Params{tempstem} . $filename_base . '_keyword_template.tmp';
  open KEYWORD, ">$keyword_template_px";
  print KEYWORD "TELESCOP HITOMI / Mission or satellite name\n";
  print KEYWORD "INSTRUME SXS / Instrument name\n";
  print KEYWORD "DETNAM PIXEL / Detector name\n";
  print KEYWORD "TSTART $tstart / Start time\n";
  print KEYWORD "TSTOP $tstop / Stop time\n";
  close KEYWORD;

  # Create the keyword template to a file.
  my($keyword_template_ac) = $Params{tempstem} . $filename_base_ac . '_keyword_template.tmp';
  open KEYWORD, ">$keyword_template_ac";
  print KEYWORD "TELESCOP HITOMI / Mission or satellite name\n";
  print KEYWORD "INSTRUME SXS / Instrument name\n";
  print KEYWORD "DETNAM ANTICO / Detector name\n";
  print KEYWORD "TSTART $tstart / Start time\n";
  print KEYWORD "TSTOP $tstop / Stop time\n";
  close KEYWORD;

  # Print the data template to a file.
  my($data_template) = $Params{tempstem} . $filename_base . '_data_template.tmp';
  open DATA, ">$data_template";
  close DATA;

  # Delete any existing file of the same name.
  unlink $filename;

  # Run ftcreate to create both the GTILOST extensions for PIXEL
  # and ANTICO
  $status = ahgen::run_ftool('ftcreate',"cdfile=$header_template",
          "datafile=$data_template","outfile=$filename",
          "headfile=$keyword_template_px","tabtyp=binary",
          "nskip=0","nrows=0","morehdr=0","extname=GTILOST",
          "anull= ","inull=0","clobber=yes");
        if ($status) {
          ahlog::ah_err "Could not create GTILOST file from scratch.";
          return $status;
        }

  $status = ahgen::run_ftool('ftcreate',"cdfile=$header_template",
          "datafile=$data_template","outfile=$filename_ac",
          "headfile=$keyword_template_ac","tabtyp=binary",
          "nskip=0","nrows=0","morehdr=0","extname=GTILOST",
          "anull= ","inull=0","clobber=yes");
        if ($status) {
          ahlog::ah_err "Could not create GTILOST file from scratch.";
          return $status;
        }

        # Append the antico GTILOST extension to the gtilost file
  $status = ahgen::copy_hdu($filename_ac,"GTILOST",$filename);
        if ($status) {
          ahlog::ah_err "Could not create GTILOST file from scratch.";
          return $status;
        }

  # Clean up the temporary files.
  ahapp::add_temp_file($filename_ac);
  ahapp::add_temp_file($data_template);
  ahapp::add_temp_file($keyword_template_px);
  ahapp::add_temp_file($keyword_template_ac);
  ahapp::add_temp_file($header_template);

  return 0;

}

# ------------------------------------------------------------------------------

sub add_coord_keys {

  my $infile = shift;
  my $ext = shift;
  my $ra = $Params{ra};
  my $dec = $Params{dec};
  my $roll = $Params{roll};
  my $ra_pnt = $Params{ra_pnt};
  my $dec_pnt = $Params{dec_pnt};
  my $optdetx = $Params{optdetx};
  my $optdety = $Params{optdety};
  my $optfocx = $Params{optfocx};
  my $optfocy = $Params{optfocy};
  my $optskyx = $Params{optskyx};
  my $optskyy = $Params{optskyy};

  # Set the nominal pointing keywords
  if ( $ra >= 0. and $ra <= 360. ) {
    ahgen::set_keyword($infile,$ext,"RA_NOM",$ra,'Nominal aspect point R.A.')
  }
  if ( $dec >= -90. and $dec <= 90. ) {
    ahgen::set_keyword($infile,$ext,"DEC_NOM",$dec,'Nominal aspect point Dec')
  }
  if ( $roll != 0 ) {
    ahgen::set_keyword($infile,$ext,"PA_NOM",$roll,'Nominal aspect point Roll' )
  }
  if ( $ra_pnt >= 0. and $ra_pnt <= 360. ) {
    ahgen::set_keyword($infile,$ext,"RA_PNT",$ra_pnt,'Pointing RA')
  }
  if ( $dec_pnt >= -90. and $dec_pnt <= 90. ) {
    ahgen::set_keyword($infile,$ext,"DEC_PNT",$dec_pnt,'Pointing DEC');
  }
  # Set the optical coordinates
  # +++ Not sure of the ranges of these
  if ( $optdetx != -999.99999 ) {
    ahgen::set_keyword($infile,$ext,"OPTDETX",$optdetx,'Optical axis DETX');
  }
  if ( $optdety != -999.99999 ) {
    ahgen::set_keyword($infile,$ext,"OPTDETY",$optdety,'Optical axis DETY')
  }
  if ( $optfocx != -999.99999 ) {
    ahgen::set_keyword($infile,$ext,"OPTFOCX",$optfocx,'Optical axis FOCX' )
  }
  if ( $optfocy != -999.99999 ) {
    ahgen::set_keyword($infile,$ext,"OPTFOCY",$optfocy,'Optical axis FOCY' )
  }
  if ( $optskyx != -999.99999 ) {
    ahgen::set_keyword($infile,$ext,"OPTSKYX",$optskyx,'Optical axis SKYX')
  }
  if ( $optskyy != -999.99999 ) {
    ahgen::set_keyword($infile,$ext,"OPTSKYY",$optskyy,'Optical axis SKYY')
  }

  return 0;

}

# ------------------------------------------------------------------------------
