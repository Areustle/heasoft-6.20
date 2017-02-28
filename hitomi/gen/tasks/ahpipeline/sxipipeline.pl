#!/usr/bin/perl
#
# File name: sxipipeline.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/11/28 14:55:18 $
# Version: 0
#
# Calibrate sxi data
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
  instrument           => "sxi", # "Which instrument"

  verify_input         => 0,  # Verify the input files before processing? 

  attitude             => "",  # "Attitude file"
  housekeeping         => "",  # "Housekeeping file"
  extended_housekeeping=> "",  # "Extended housekeeping file"
  makefilter           => "",  # "Makefilter file, comma seperated list for multiple files"
  orbit                => "",  # "Orbit file, comma seperated list for multiple files"
  timfile              => "",  # "Time file, comma seperated list for multiple files"  
  obsgti              => "",  # "Time file, comma seperated list for multiple files"  

  sxi_start            => 0,   # "SXI CALDB start time"

  ra                   => -999.99999, # RA of nominal pointing [deg]
  dec                  => -999.99999, # Dec of nominal pointing [deg]
  roll                 => 0, # Roll of nominal pointing [deg]

  optdetx              => -999.99999, # SXI optical detx coordinate
  optdety              => -999.99999, # SXI optical dety coordinate
  optfocx              => -999.99999, # SXI optical focx coordinate
  optfocy              => -999.99999, # SXI optical focy coordinate
  optskyx              => -999.99999, # SXI optical skyx coordinate
  optskyy              => -999.99999, # SXI optical skyy coordinate
  ra_pnt               => -999.99999, # RA of sxi pointing [deg]
  dec_pnt              => -999.99999, # Dec of sxi pointing [deg]

  entry_stage          => 1, # 
  exit_stage           => 1, # 

  stage1_switch        => 0, # 
  stage2_switch        => 0, # 
  stage3_switch        => 0, # 

  numerrs              => 0, # 

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
  hpix              => [],
  fpix              => [],
  telgti            => "",
  exposure          => "",
  modegti           => "",
  seggti            => "",

);

our $zpatt = qr/(\.Z|\.z|\.gzip|\.GZIP|\.gz|\.GZ|\.zip\.ZIP)?/;
our %patterns = (
  mission           => qr/ah/,
  sequence          => qr/[0-9]{1,9}/,
  event_uf          => qr/(sxi_[ps][0-9][0-9a-f]{8}_uf\.evt)$zpatt$/,
  event_cl          => qr/(sxi_[ps][0-9][0-9a-f]{8}_cl\.evt)$zpatt$/,
  hpix              => qr/(sxi_a0[0-9a-f]{8}\.hpix)$zpatt$/,
  fpix              => qr/(sxi_[psa][0-9][0-9a-f]{8}\.fpix)$zpatt$/,
  exposure          => qr/(sxi_a0exp\.fits)$zpatt$/,
  modegti           => qr/(sxi_mode\.gti)$zpatt$/,
  telgti            => qr/(sxi_tel\.gti)$zpatt$/,
);

my $pipelineerror = 0;  # sxipipeline exit status

our @filelist_output;
our @error_output;

# Use this comparison function for numeric sorting.
sub numerically { $a <=> $b; }

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

# Reset the numerrs parameters
$Params{numerrs} = 0;
ahgen::run_ftool("pset","sxipipeline","numerrs=0");

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
  $Params{regionfile}   = ahapp::query_parameter("regionfile");
                                                              
  $Params{sxi_start}    = ahapp::query_parameter("sxi_start");

  # CALDB files
  $Params{leapsecfile}       = ahapp::query_parameter("leapsecfile");
  $Params{selectfile}        = ahapp::query_parameter("selectfile");

  $Params{calc_modegti}      = ahapp::query_parameter("calc_modegti",1);

  $Params{sxi_mkflabel}      = ahapp::query_parameter("sxi_mkflabel");
  $Params{sxi_ehklabel}      = ahapp::query_parameter("sxi_ehklabel");
  $Params{sxi_evtlabel}      = ahapp::query_parameter("sxi_evtlabel");
  
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

    # CALDB files
    $Params{teldeffile}        = ahapp::query_parameter("teldeffile");
    $Params{badpixfile}        = ahapp::query_parameter("badpixfile");
    $Params{maskfile}          = ahapp::query_parameter("maskfile");
    $Params{vtevnoddfile}      = ahapp::query_parameter("vtevnoddfile");
    $Params{chtrailfile}       = ahapp::query_parameter("chtrailfile");
    $Params{ctifile}           = ahapp::query_parameter("ctifile");
    $Params{spthfile}          = ahapp::query_parameter("spthfile");
    $Params{gainfile}          = ahapp::query_parameter("gainfile");
    $Params{patternfile}       = ahapp::query_parameter("patternfile");

    $Params{calc_hotpix}       = ahapp::query_parameter("calc_hotpix",1);

    # Shared pars
    $Params{timecol}           = ahapp::query_parameter("timecol");
    $Params{seed}              = ahapp::query_parameter("seed");

    # coordevt pars 
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

    # sximodegti

    # sxiphas
    $Params{colbound}          = ahapp::query_parameter("colbound");

    # searchflickpix
    $Params{chipcol}           = ahapp::query_parameter("chipcol");
    $Params{xcol}              = ahapp::query_parameter("xcol");
    $Params{ycol}              = ahapp::query_parameter("ycol");
    $Params{chancol}           = ahapp::query_parameter("chancol");
    $Params{gradecol}          = ahapp::query_parameter("gradecol");
    $Params{grade}             = ahapp::query_parameter("grade");
    $Params{n_division}        = ahapp::query_parameter("n_division");
    $Params{cleanimg}          = ahapp::query_parameter("cleanimg");
    $Params{cellsize}          = ahapp::query_parameter("cellsize");
    $Params{impfac}            = ahapp::query_parameter("impfac");
    $Params{logprob1}          = ahapp::query_parameter("logprob1");
    $Params{logprob2}          = ahapp::query_parameter("logprob2");
    $Params{iterate}           = ahapp::query_parameter("iterate");
    $Params{flagedge}          = ahapp::query_parameter("flagedge");
    $Params{bthresh}           = ahapp::query_parameter("bthresh");
    $Params{duration}          = ahapp::query_parameter("duration");
    $Params{sigma}             = ahapp::query_parameter("sigma");
    $Params{firstchip}         = ahapp::query_parameter("firstchip");
    $Params{lastchip}          = ahapp::query_parameter("lastchip");
    $Params{xmin}              = ahapp::query_parameter("xmin");
    $Params{xmax}              = ahapp::query_parameter("xmax");
    $Params{ymin}              = ahapp::query_parameter("ymin");
    $Params{ymax}              = ahapp::query_parameter("ymax");
    $Params{chanmin}           = ahapp::query_parameter("chanmin");
    $Params{chanmax}           = ahapp::query_parameter("chanmax");

    # sxiflagpix
    $Params{outbadpix}         = ahapp::query_parameter("outbadpix",1);
    $Params{outbadimg}         = ahapp::query_parameter("outbadimg",1);
    $Params{npixnbr}           = ahapp::query_parameter("npixnbr");
    $Params{nboundnbr}         = ahapp::query_parameter("nboundnbr");
    $Params{citrailnbr}        = ahapp::query_parameter("citrailnbr");
    $Params{ciprenbr}          = ahapp::query_parameter("ciprenbr");
    $Params{echoflag}          = ahapp::query_parameter("echoflag");
    $Params{echomap}           = ahapp::query_parameter("echomap");
    $Params{echonbr}           = ahapp::query_parameter("echonbr");
    $Params{echomin}           = ahapp::query_parameter("echomin");
    $Params{echospth}          = ahapp::query_parameter("echospth");
    $Params{echofrac}          = ahapp::query_parameter("echofrac");
    $Params{bad_status}        = ahapp::query_parameter("bad_status");
    $Params{copyphas}          = ahapp::query_parameter("copyphas");
    $Params{resetflags}        = ahapp::query_parameter("resetflags");

    # sxipi 
    $Params{hkext}             = ahapp::query_parameter("hkext");
    $Params{hkcolstem}         = ahapp::query_parameter("hkcolstem");
    $Params{hkvideoid}         = ahapp::query_parameter("hkvideoid");
    $Params{startcol}          = ahapp::query_parameter("startcol");
    $Params{evnoddcor}         = ahapp::query_parameter("evnoddcor");
    $Params{chtrailcor}        = ahapp::query_parameter("chtrailcor");
    $Params{cticor}            = ahapp::query_parameter("cticor");
    $Params{gaincor}           = ahapp::query_parameter("gaincor");
    $Params{ctigrade}          = ahapp::query_parameter("ctigrade");
    $Params{copygrade}         = ahapp::query_parameter("copygrade");
    $Params{phcut}             = ahapp::query_parameter("phcut");
    $Params{badpixopt}         = ahapp::query_parameter("badpixopt");
    $Params{spthiter}          = ahapp::query_parameter("spthiter");
    $Params{spthcaldb}         = ahapp::query_parameter("spthcaldb");
    $Params{spthoffset}        = ahapp::query_parameter("spthoffset");
    $Params{spthslope}         = ahapp::query_parameter("spthslope");
    $Params{evtthre}           = ahapp::query_parameter("evtthre");
    $Params{negthre}           = ahapp::query_parameter("negthre");
    $Params{deltatime}         = ahapp::query_parameter("deltatime");
    $Params{debugcol}          = ahapp::query_parameter("debugcol");
    $Params{randomize}         = ahapp::query_parameter("randomize");
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
    $Params{tempstem} = "sxipipeline_" . $taskstart . "_tmp_";
    $Params{logstem} = "sxipipeline_" . $taskstart . "_";
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
  my $housekeeping = $Params{housekeeping};
  my $obsgti      = $Params{obsgti};
  my $regionfile   = $Params{regionfile};

  # Check the input and output directories
  if (CheckInputDirectory($indir,$outdir)) { return 1; };
  if (CheckOutputDirectory($outdir)) { return 1; };

  # Find and store the list of input files
  @{$files{event_uf}} = FindInputFiles($indir,$steminputs , $patterns{event_uf});
  @{$files{event_cl}} = FindInputFiles($indir,$steminputs , $patterns{event_cl});
  @{$files{hpix}}     = FindInputFiles($indir,$steminputs , $patterns{hpix});
  ($files{exposure})  = FindInputFiles($indir,$steminputs , $patterns{exposure});
  ($files{modegti})   = FindInputFiles($indir,$steminputs , $patterns{modegti});
  ($files{telgti})    = FindInputFiles($indir,$steminputs , $patterns{telgti});
  
  # Check the input CALDB files
  if ( $Params{stage1_switch} ) {
    # Check for file requirements
    if (isOptionalFileNotFound($attitude)) { return 1; }
    if (isOptionalFileNotFound($orbit)) { return 1; }
    if (isOptionalFileNotFound($housekeeping)) { return 1; }
    if (isRequiredFileNotFound($obsgti)) { return 1; }

    # Store the required input files
    $files{attitude}      = $attitude;
    $files{orbit}         = $orbit;
    $files{obsgti}        = $obsgti;
    $files{housekeeping}  = $housekeeping;

    if (isBadCALDBFileParameterValue($Params{teldeffile},"teldeffile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{badpixfile},"badpixfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{maskfile},"maskfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{vtevnoddfile},"vtevnoddfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{ctifile},"ctifile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{chtrailfile},"chtrailfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{spthfile},"spthfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{gainfile},"gainfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{patternfile},"patternfile")) { return 1;}

    if ( uc $Params{stopsys} eq "HIGHEST" and uc $files{attitude} eq "NONE" ) {
      ahlog::ah_info "HIGH","No input ATTITUDE file. Setting stopsys as FOC";
      $Params{stopsys} = "FOC";
    }
    if ( uc $files{orbit} eq "NONE" ) {
      ahlog::ah_info "HIGH","No input ORBIT file";
    }
    if ( uc $files{housekeeping} eq "NONE" ) {
      ahlog::ah_info "HIGH","No input HOUSEKEEPING file. Setting evnoddcor as no";
      $Params{evnoddcor} = "no";
    }
  }
  if ( $Params{stage2_switch} ) {
    # Check for file requirements
    if (isRequiredFileNotFound($obsgti)) { return 1; }
    if (isRequiredFileNotFound($makefilter)) { return 1; }

    $files{obsgti}       = $obsgti;
    $files{makefilter}    = $makefilter;

    if (isBadCALDBFileParameterValue($Params{leapsecfile},"leapsecfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{selectfile},"selectfile")) { return 1;}
    if (isRequiredFileNotFound($files{makefilter})) { return 1; }
  }
  if ( $Params{stage3_switch} ) {
    $files{regionfile}    = $regionfile;
    if (isOptionalFileNotFound($files{regionfile})) { return 1; }
  }

  # Search for required files for each step

  # Check that we found a list of unfiltered event files
  if ( $Params{stage1_switch} or $Params{stage2_switch} ) {
    if (isRequiredFileNotFound($extended_housekeeping)) { return 1; }
    $files{extended_housekeeping} = $extended_housekeeping;

    unless ( @{$files{event_uf}} ) {
        ahlog::ah_err "Cannot find any $inst event files in $indir" ;
        return 1;
    }
    unless ( $files{telgti} ) {
      ahlog::ah_err "Cannot find any $inst telemetry GTI files in $indir" ;
      return 1;
    }
  }
       
  # For cleaning events, mode GTI is required
  # Check whether we are calculating SXI GTI from the exposure
  # or using an input GTI
  if( $Params{stage2_switch} ) {
    # Check that we have a valid label for SXI
    # If not, throw an error
    if ( $Params{sxi_mkflabel} !~ /#/ or 
         $Params{sxi_ehklabel} !~ /#/ or 
         $Params{sxi_evtlabel} !~ /#/ ) {
      ahlog::ah_err " *** Not a valid label for SXI. Label requires '#'.";
      return 1;
    }

    if($Params{calc_modegti}) {
      unless ( $files{exposure} ) {
        ahlog::ah_err "Cannot find any $inst exposure files in $indir" ;
        return 1;
      }
    } else {
      unless ( $files{modegti} ) {
        ahlog::ah_err "Cannot find any $inst mode GTI files in $indir" ;
        return 1;
      }
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
    foreach my $filename ( sort @{$files{event_uf}} ) {
      ahlog::ah_debug "  $filename\n";
    }
    ahlog::ah_debug "$inst hot pixel  file(s) found:" if @{$files{hpix}};
    foreach my $filename ( sort @{$files{hpix}} ) {
      ahlog::ah_debug "  $filename\n";
    }
    ahlog::ah_debug "$inst cleaned file(s) found:" if @{$files{event_cl}};
    foreach my $filename ( sort @{$files{event_cl}} ) {
      ahlog::ah_debug "  $filename\n";
    }
    if ( defined $files{exposure} ) {
      ahlog::ah_debug "$inst exposure file found:";
      ahlog::ah_debug "  $files{exposure}\n" ;
    }
    if ( defined $files{modegti} ) {
      ahlog::ah_debug "$inst modegti file found:";
      ahlog::ah_debug "  $files{modegti}\n" ;
    }
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

  my @calibrated_sxi_uf_evt_files = ( );
  my @calibrated_sxi_hpix_files = ( );
  my @calibrated_sxi_fpix_files = ( );
  my @calibrated_sxi_fpix_pfiles = ( );
  my @calibrated_sxi_fpix_sfiles = ( );
  my @merged_sxi_fpix_files = ( );

  my $status = 0;

  #############  setup  parameters for each tool #############

  my $calc_hotpix  = $Params{calc_hotpix};
  my $sxi_start    = $Params{sxi_start};
  my $cleanup      = $Params{cleanup};

  # Attitude/Orbit files
  my $attitude      = $files{attitude};
  my $orbit         = $files{orbit};
  my $ehk           = $files{extended_housekeeping};
  my $obsgti        = $files{obsgti};

  # SXI files
  my $hkfile       = $files{housekeeping};

  # MZDYE GTI file
  my $echogti = $Params{tempstem} . "mzdye.gti";

  # TSTART/TSTOP
  my @tstart       = ();
  my @tstop        = ();

  my %coordevt_pars_pointing = (
      teldeffile        => $Params{teldeffile},
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

  my %coordevt_pars_hpix = %coordevt_pars_pointing;
  $coordevt_pars_hpix{stopsys} = "DET";
  $coordevt_pars_hpix{infileext} = "HOTPIX";

  my %coordevt_pars_fpix = %coordevt_pars_pointing;
  $coordevt_pars_fpix{startsys} = "ACT";
  $coordevt_pars_fpix{stopsys} = "DET";
  $coordevt_pars_fpix{infileext} = "PIXELS";

  my %sxiphas_pars = (
    colbound          => $Params{colbound},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );

  my %sxiflagpix_pars = (
    outbadpix       => "NONE",
    outbadimg       => "NONE",
    badpixfile      => $Params{badpixfile},
    maskfile        => $Params{maskfile},
    npixnbr         => $Params{npixnbr},
    nboundnbr       => $Params{nboundnbr},
    citrailnbr      => $Params{citrailnbr},
    ciprenbr        => $Params{ciprenbr},
    echonbr         => $Params{echonbr},
    echomin         => $Params{echomin},
    echospth        => $Params{echospth},
    echofrac        => $Params{echofrac},
    echoflag        => $Params{echoflag},
    gtifile         => $echogti,
    echomap         => $Params{echomap},
    bad_status      => $Params{bad_status},
    copyphas        => $Params{copyphas},
    resetflags      => $Params{resetflags},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );

  my %sxiflagpix_pars_run2 = %sxiflagpix_pars;
  $sxiflagpix_pars_run2{resetflags} = "no";

  my %sxipi_pars = (
    hkext                => $Params{hkext},
    hkcolstem            => $Params{hkcolstem},
    hkvideoid            => $Params{hkvideoid},
    vtevnoddfile         => $Params{vtevnoddfile},
    chtrailfile          => $Params{chtrailfile},
    ctifile              => $Params{ctifile},
    spthfile             => $Params{spthfile},
    gainfile             => $Params{gainfile},
    patternfile          => $Params{patternfile},
    startcol             => $Params{startcol},
    evnoddcor            => $Params{evnoddcor},
    chtrailcor           => $Params{chtrailcor},
    cticor               => $Params{cticor},
    gaincor              => $Params{gaincor},
    ctigrade             => "no",
    copygrade            => $Params{copygrade},
    phcut                => $Params{phcut},
    badpixopt            => $Params{badpixopt},
    spthiter             => $Params{spthiter},
    spthcaldb            => $Params{spthcaldb},
    spthoffset           => $Params{spthoffset},
    spthslope            => $Params{spthslope},
    evtthre              => $Params{evtthre},
    negthre              => $Params{negthre},
    deltatime            => $Params{deltatime},
    debugcol             => $Params{debugcol},
    randomize            => $Params{randomize},
    seed                 => $Params{seed},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );

  my %sxipi_pars_run2 = %sxipi_pars;
  $sxipi_pars_run2{ctigrade} = $Params{ctigrade};

  my %searchflickpix_pars = (
    chipcol     => $Params{chipcol},
    xcol        => $Params{xcol},
    ycol        => $Params{ycol},
    chancol     => $Params{chancol},
    gradecol    => $Params{gradecol},
    grade       => $Params{grade},
    n_division  => $Params{n_division},
    cleanimg    => $Params{cleanimg},
    cellsize    => $Params{cellsize},
    impfac      => $Params{impfac},
    logprob1    => $Params{logprob1},
    logprob2    => $Params{logprob2},
    iterate     => $Params{iterate},
    flagedge    => $Params{flagedge},
    bthresh     => $Params{bthresh},
    duration    => $Params{duration},
    sigma       => $Params{sigma},
    firstchip   => $Params{firstchip},
    lastchip    => $Params{lastchip},
    xmin        => $Params{xmin},
    xmax        => $Params{xmax},
    ymin        => $Params{ymin},
    ymax        => $Params{ymax},
    chanmin     => $Params{chanmin},
    chanmax     => $Params{chanmax},
    clobber => $ahapp::clobber ? "yes" : "no" ,
    chatter => $ahapp::chatter,
  );

  # Create a MZDYE GTI file from the EHK
  # Search for times when (MZDYE_ELV>MZNTE_ELV||MZDYE_ELV>20)
  # +++ What if there is no EHK? Answer: do not apply, not an error
  if ( $ehk ) {
    my $logfile = $Params{logstem} . "echogti_ahgtigen.log";
    ahapp::add_temp_file($echogti);
    $status = create_gti(infile       => $ehk,
                         outfile      => $echogti,
                         gtiexpr      => "(MZDYE_ELV>MZNTE_ELV||MZDYE_ELV>20)",
                         clobber => $ahapp::clobber ? "yes" : "no" ,
                         chatter => $ahapp::chatter,
                         logfile => $logfile,
                         );
    if ( $status ) { 
      if ( $status == 2 ) {
        ahlog::ah_err "$echogti\[GTI] has no GTI from SXI EHK.";
        push @error_output, "No MZDYE GTI found";
      } else {
        ahlog::ah_err "ahgtigen failed for SXI EHK: $echogti";
        push @error_output, "No MZDYE GTI found";
      }
      $Params{numerrs} += 1;
      return 1;
    }
    if ( ! has_good_time( $echogti, "GTI" ) ) {
      ahlog::ah_err "$echogti\[GTI] has no GTI from SXI EHK.";
      push @error_output, "No MZDYE GTI found";
      $Params{numerrs} += 1;
      return 1;
    }
  } else {
    # Do not apply cosmic ray echo GTI
    $sxiflagpix_pars{echoflag} = "no";
    $sxiflagpix_pars_run2{echoflag} = "no";
  }

  # Find the first and the last SXI files
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

  # Find the first and last indx for SXI
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
    if ( $tstart_prev == 0 ) { $tstart_prev = $tstart; $first_indx = $indx;}
    if ( $tstop_prev == 0 ) { $tstop_prev = $tstop; $last_indx = $indx; }
    if ( $tstart < $tstart_prev ) { $tstart_prev = $tstart; $first_indx = $indx; }
    if ( $tstop > $tstop_prev ) { $tstop_prev = $tstop; $last_indx = $indx; }
  }

  ############# Calibrate each sxi file #############
  ahlog::ah_out "Calibrating SXI events";
  foreach my $sxifile ( sort @{$files{event_uf}} ) {

    #############  setup input/output files #############

    # Set up input and output files
    my $hpix          = find_hpix_file($sxifile);
    my $sxifile_out   = form_outfile_name($sxifile);
    my $hpix_out      = uc $hpix eq "NONE" ? $hpix : form_outfile_name($hpix);
    my $fpix          = form_fpix_file($sxifile_out);

    # Set up the output bad pixel file and output bad img file names
    if ( $Params{outbadpix} ) {
      $sxiflagpix_pars_run2{outbadpix} = form_bpix_file($sxifile_out);
    } else {
      $sxiflagpix_pars_run2{outbadpix} = "NONE";
    }
    if ( $Params{outbadimg} ) {
      $sxiflagpix_pars_run2{outbadimg} = form_img_file($sxifile_out);
    } else {
      $sxiflagpix_pars_run2{outbadimg} = "NONE";
    }
    my $bpix_out = $sxiflagpix_pars_run2{outbadpix};
    my $bpix_img = $sxiflagpix_pars_run2{outbadimg};

    # Get basenames (for temporary files) and remove the extension
    my $basename      = basename($sxifile_out);
    $basename =~ s/\.evt.*$//;
    my $basename_hpix = basename($hpix_out);
    $basename_hpix =~ s/\.hpix.*$/_hpix/;
    my $basename_fpix = basename($fpix);
    $basename_fpix =~ s/\.fpix.*$/_fpix/;

    # Determine the file index for this sxi file
    my $indx = find_file_indx($sxifile_out);

    # Skip the file if there are no events
    my $naxis2 = ahgen::get_keyword($sxifile,"EVENTS","NAXIS2");
    unless ( defined $naxis2 ) {
      ahlog::ah_out"In file '$sxifile', could not read keyword NAXIS2";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile (NAXIS2 not defined)";
      $Params{numerrs} += 1;
      next;
    }
    unless ( $naxis2 ) { 
      ahlog::ah_out"No events found in file '$sxifile'";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile (NAXIS2=0)";
      $Params{numerrs} += 1;
      next;
    }
    # read keywords TSTART, TSTOP, DATE-OBS, and OBSMODE
    my $tstart = ahgen::get_keyword($sxifile,"EVENTS","TSTART");
    unless ( defined $tstart ) {
      ahlog::ah_out"In file '$sxifile', could not read keyword TSTART";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile (TSTART not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $tstop = ahgen::get_keyword($sxifile,"EVENTS","TSTOP");
    unless ( defined $tstop ) {
      ahlog::ah_out"In file '$sxifile', could not read keyword TSTOP";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile (TSTOP not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $dateobs = ahgen::get_keyword($sxifile,"EVENTS","DATE-OBS");
    unless ( defined $dateobs ) {
      ahlog::ah_out"In file '$sxifile', could not read keyword DATE-OBS";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile (DATE-OBS not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $obsmode = ahgen::get_keyword($sxifile,"EVENTS","OBS_MODE");
    unless ( defined $obsmode ) {
      ahlog::ah_out"In file '$sxifile', could not read keyword OBS_MODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile (OBS_MODE not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $detnam = ahgen::get_keyword($sxifile,"EVENTS","DETNAM");
    if(ahgen::get_error_flag()) {
      ahlog::ah_err "Could not get DETNAM keyword from $sxifile";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile (DETNAM not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $datamode = ahgen::get_keyword($sxifile,"EVENTS","DATAMODE");
    unless ( defined $datamode ) {
      ahlog::ah_out"In file '$sxifile', could not read keyword DATAMODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile (DATAMODE not defined)";
      $Params{numerrs} += 1;
      next;
    }

    # Save the tstart and tstop regardless of mode
    push @tstart, $tstart;
    push @tstop, $tstop;

    ahlog::ah_debug "Input File       : $sxifile";
    ahlog::ah_debug "Input hpix File  : $hpix";
    ahlog::ah_debug "Output File      : $sxifile_out";
    ahlog::ah_debug "Output hpix File : $hpix_out" unless $calc_hotpix;
    ahlog::ah_debug "Output fpix File : $fpix";
    ahlog::ah_debug "tstart           : $tstart";
    ahlog::ah_debug "tstop            : $tstop";
    ahlog::ah_debug "dateobs:         : $dateobs";
    ahlog::ah_debug "obsmode:         : $obsmode";

    # can this file ($sxifile) be processed with the caldb files?
    if ( $tstop < $sxi_start ) {
      ahlog::ah_out"SXI input file $sxifile occurs before CALDB (sxi_start)";
      ahlog::ah_out"SXI input file $sxifile will NOT be re-calibrated";
      push @error_output, "Skipped file: $sxifile (sxi_start occurs before CALDB)";
      $Params{numerrs} += 1;
      next;
    }

    ahlog::ah_out "Calibrating SXI file $sxifile";

    my $tmpfile = "";
    # Copy the infile(s) to the output file(s)
    if (copyFITSFile($sxifile,$sxifile_out)) { return 1; }
    if ( uc $hpix_out ne "NONE" ) {
      if(copyFITSFile($hpix,$hpix_out)) { return 1; }
    }

    # Perform coordevt calculation on hotpix file
    if( uc $hpix ne "NONE" ) {
      ahlog::ah_out "Running coordevt on file $hpix_out";
      $tmpfile = $Params{tempstem} . $basename_hpix . "_coordevt.hpix";
      ahapp::add_temp_file($tmpfile);
      $coordevt_pars_hpix{logfile} = $Params{logstem} . $basename_hpix  . "_coordevt.log";
      $status = run_coordevt($hpix_out,$tmpfile,\%coordevt_pars_hpix);
      if ( $status ) { ahlog::ah_err "coordevt failed"; return 1; }
      if(copyFITSFile($tmpfile,$hpix_out)) { return 1; }
    } else {
      # Use the input hpix file if not calculating coordinates
      $hpix_out = $hpix;
    }

    # COORDEVT (events)
    ahlog::ah_out "Running coordevt on file $sxifile_out";
    my %coordevt_pars;
    if( uc $obsmode eq "POINTING" ) {
      %coordevt_pars = %coordevt_pars_pointing;
    } else {
      %coordevt_pars = %coordevt_pars_slew;
    }
    $tmpfile = $Params{tempstem} . $basename . "_coordevt.evt";
    ahapp::add_temp_file($tmpfile);
    $coordevt_pars{logfile} = $Params{logstem} . $basename . "_coordevt.log";
    $status = ahpllib::run_coordevt($sxifile_out,$tmpfile,\%coordevt_pars);
    if ( $status ) { ahlog::ah_err "coordevt failed"; return 1; }
    if(copyFITSFile($tmpfile,$sxifile_out)) { return 1; }

    # SXIPHAS
    ahlog::ah_out "Running sxiphas on file $sxifile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxiphas.evt";
    ahapp::add_temp_file($tmpfile);
    $sxiphas_pars{logfile} = $Params{logstem} . $basename . "_sxiphas.log";
    $status = run_sxiphas($sxifile_out,$tmpfile,\%sxiphas_pars);
    if ( $status ) { ahlog::ah_err "sxiphas failed"; return 1; }
    if(copyFITSFile($tmpfile,$sxifile_out)) { return 1; }

    # SXIFLAGPIX (1)
    ahlog::ah_out "Running sxiflagpix (1) on file $sxifile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxiflagpix_1.evt";
    ahapp::add_temp_file($tmpfile);
    $sxiflagpix_pars{logfile} = $Params{logstem} . $basename . "_sxiflagpix_1.log";
    $status = run_sxiflagpix($sxifile_out,$tmpfile,$hpix_out,"NONE",\%sxiflagpix_pars);
    if ( $status ) { ahlog::ah_err "sxiflagpix failed"; return 1; }
    if(copyFITSFile($tmpfile,$sxifile_out)) { return 1; }

    # SXIPI (1)
    ahlog::ah_out "Running sxipi (1) on file $sxifile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxipi_1.evt";
    ahapp::add_temp_file($tmpfile);
    $sxipi_pars{logfile} = $Params{logstem} . $basename . "_sxipi_1.log";
    $status = run_sxipi($sxifile_out,$tmpfile,$hkfile,\%sxipi_pars);
    if ( $status ) { ahlog::ah_err "sxipi failed"; return 1; }
    if(copyFITSFile($tmpfile,$sxifile_out)) { return 1; }

    # SXIPI (2)
    ahlog::ah_out "Running sxipi (2) on file $sxifile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxipi_2.evt";
    ahapp::add_temp_file($tmpfile);
    $sxipi_pars_run2{logfile} = $Params{logstem} . $basename . "_sxipi_2.log";
    $status = run_sxipi($sxifile_out,$tmpfile,$hkfile,\%sxipi_pars_run2);
    if ( $status ) { ahlog::ah_err "sxipi failed"; return 1; }
    if(copyFITSFile($tmpfile,$sxifile_out)) { return 1; }

    # Extract events and find flickering pixels
    ahlog::ah_out "Extracting STATUS[1]==b0 from $sxifile_out";
    my $sxifile_fl = $Params{tempstem} . $basename . "_filtered.evt";
    ahapp::add_temp_file($sxifile_fl);
    $status = run_ftselect($sxifile_out,"EVENTS",$sxifile_fl,"STATUS[1]==b0"); 
    if ( $status ) { ahlog::ah_out "No events to create flickering pixels file."; }

    # SEARCHFLICKPIX
    ahlog::ah_out "Running searchflickpix on file $sxifile_fl";
    $searchflickpix_pars{logfile} = $Params{logstem} . $basename_fpix  . "_searchflickpix.log";
    $status = run_searchflickpix($sxifile_fl,$fpix,\%searchflickpix_pars);
    if ( $status ) { ahlog::ah_err "searchflickpix failed"; return 1; }

    # COORDEVT (flickering pixels)
    ahlog::ah_out "Running coordevt on file $fpix";
    $tmpfile = $Params{tempstem} . $basename_fpix  . "_coordevt.fpix";
    ahapp::add_temp_file($tmpfile);
    $coordevt_pars_fpix{logfile} = $Params{logstem} . $basename_fpix  . "_coordevt.log";
    $status = run_coordevt($fpix,$tmpfile,\%coordevt_pars_fpix);
    if ( $status ) { ahlog::ah_err "coordevt failed"; return 1; }
    if(copyFITSFile($tmpfile,$fpix)) { return 1; }
    
    # SXIFLAGPIX (2)
    ahlog::ah_out "Running sxiflagpix (2) on file $sxifile_out";
    $tmpfile = $Params{tempstem} . $basename . "_sxiflagpix_2.evt";
    ahapp::add_temp_file($tmpfile);
    $sxiflagpix_pars_run2{logfile} = $Params{logstem} . $basename . "_sxiflagpix_2.log";
    $status = run_sxiflagpix($sxifile_out,$tmpfile,$hpix_out,$fpix,\%sxiflagpix_pars_run2);
    if ( $status ) { ahlog::ah_err "sxiflagpix failed"; return 1; }
    if(copyFITSFile($tmpfile,$sxifile_out)) { return 1; }

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
    # GTI1    : |-------------|
    # GTI2    :               |--------------|
    # GTI3    :                              |-----------------|
    if ( ! ahgen::check_hdu_exists($sxifile,"GTI") ) {
      ahlog::ah_out "Attaching GTI to file $sxifile_out";
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
        $status = copy_hdu($obsgti,"GTISLEW][col #EXTNAME=\"GTI\"", $sxifile_out);
        if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
      } else {

        # Attach the GTI to the reconstructed event file
        if ( $indx == 0 ) {
          # Attach directly the GTIPOINT if there is no file splitting
          # Change the extension name from GTIPOINT to GTI
          $status = copy_hdu($obsgti,"GTIPOINT][col #EXTNAME=\"GTI\"", $sxifile_out);
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
            push @error_output, "Error in file: $sxifile (Could not append GTI)";
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
          $status = copy_hdu($cutgti,"GTI", $sxifile_out);
          if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
        }
      } # end attach GTI
    } # end if GTI extension exists

    # Update the INSTRUME, DETNAM, DATAMODE keywords in the GTI extension
    ahgen::set_keyword( $sxifile_out , "GTI", 'INSTRUME' , "SXI" ) ;
    ahgen::set_keyword( $sxifile_out , "GTI", 'DETNAM' , $detnam ) ;
    ahgen::set_keyword( $sxifile_out , "GTI", 'DATAMODE' , $datamode ) ;

    # Set the coordinate keywords in the output SXI event file
    push @calibrated_sxi_uf_evt_files, $sxifile_out;
    push( @filelist_output, $sxifile_out );
    push @calibrated_sxi_fpix_files, $fpix;
    if ( uc $hpix_out ne "NONE" ) {
      push @calibrated_sxi_hpix_files, $hpix_out;
      push( @filelist_output, $hpix_out );
    }
    if ( $Params{outbadimg} ) {
      # Append the Cosmic Ray echo GTI to the bad pixel file
      if ( -e $echogti ) {
        $status = copy_hdu($echogti,"GTI", $bpix_img);
        if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
      }
      push( @filelist_output, $bpix_img );
    }
    if ( $Params{outbadpix} ) {
      push( @filelist_output, $bpix_out );
    }

  } # end loop over each sxi file

  ahlog::ah_debug "End of SXI Event calibration";

  # Save lists of newly calibrated '_uf.evt', '.hpix' and '.fpix' files
  @{$files{event_uf}} = @calibrated_sxi_uf_evt_files;
  @{$files{hpix}} = @calibrated_sxi_hpix_files;
  @{$files{fpix}} = @calibrated_sxi_fpix_files;

  # Empty the fpix file array
  @calibrated_sxi_fpix_files = ();

  my $tstart_obs = ( sort numerically @tstart )[ 0 ];
  my $tstop_obs  = ( sort numerically @tstop )[ -1 ];

  ahlog::ah_out "Merging pointing and slew flickering pixel files";

  # Merge the pointing and slew flickering pixel files 
  foreach my $fpix_p ( @{$files{fpix}} ) {

    # skip any slew files
    if( $fpix_p =~ /sxi_s/ ) { next; }

    # Set up the input/output file names
    my $fpix_s = $fpix_p;
    $fpix_s =~ s/sxi_p/sxi_s/g;
    my $fpix   = $fpix_p;
    $fpix =~ s/sxi_p/sxi_a/g;
    my $pixels = $fpix . ".pixels";

    # Mark files for deletion
    ahapp::add_temp_file($pixels);
    ahapp::add_temp_file($fpix_p);

    # Merge the EVENTS extensions and sort by TIME
    $status = merge_slew_and_pointing($fpix_s,$fpix_p,$fpix,"EVENTS","TIME");
    if ( $status ) { ahlog::ah_err "merge fpix failed"; return 1; }
    # Merge the PIXELS extensions and sort by START then STOP
    $status = merge_slew_and_pointing($fpix_s,$fpix_p,$pixels,"PIXELS","START STOP");
    if ( $status ) { ahlog::ah_err "merge fpix failed"; return 1; }
    # Append the PIXELS extension to the output file
    $status = ahgen::run_ftool("ftappend",$pixels."[PIXELS]",$fpix);
    unlink $pixels; # Just delete the pixels file here
    if ( $status ) { ahlog::ah_err "ftappend failed"; return 1; }

    # Update the TSTART/TSTOP keywords to include slew and pipeline
    ahgen::set_keyword($fpix,"EVENTS","TSTART",$tstart_obs);
    ahgen::set_keyword($fpix,"EVENTS","TSTOP",$tstop_obs);
    ahgen::set_keyword($fpix,"PIXELS","TSTART",$tstart_obs);
    ahgen::set_keyword($fpix,"PIXELS","TSTOP",$tstop_obs);

    push( @filelist_output, $fpix );

    # Save the merged flickering pixel files
    push @calibrated_sxi_fpix_files, $fpix;

  }

  ahlog::ah_debug "End of SXI flickering pixel merge";

  # Save lists of newly merged '.fpix' files
  @{$files{fpix}} = @calibrated_sxi_fpix_files;

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

  my $exposure     = $files{exposure};
  my $telgti       = $files{telgti};
  my $modegti      = $files{modegti};
  my $makefilter   = $files{makefilter};
  my $ehk          = $files{extended_housekeeping};
  my $obsgti       = $files{obsgti};
  my $select       = $Params{selectfile};
  my $leapsecfile  = $Params{leapsecfile};
  my $outdir       = $Params{outdir};
  my $stemoutputs  = $Params{stemoutputs};
  my $instrument   = $Params{instrument};

  my $mkflabel     = $Params{sxi_mkflabel};
  my $ehklabel     = $Params{sxi_ehklabel};
  my $evtlabel     = $Params{sxi_evtlabel};

  my $seggti       = "";

  my @calibrated_sxi_cl_evt_files = ( );

  # Check that we have good GTI from pointing and attitude GTI
  if ( ! has_good_time( $obsgti, "GTIPOINT" ) ) {
    ahlog::ah_out "$obsgti\[GTIPOINT] has no GTI. Cannot clean SXI Data.";
    return 1;
  }
  if ( ! has_good_time( $obsgti, "GTIATT" ) ) {
    ahlog::ah_out "$obsgti\[GTIATT] has no GTI. Cannot clean SXI Data.";
    return 1;
  }

  # Check that we have good GTI from telemetry saturation GTI
  if ( ! has_good_time( $telgti, "GTITEL" ) ) {
    ahlog::ah_out "$telgti\[GTITEL] has no GTI. Cannot clean SXI Data.";
    return 1;
  }

  if($Params{calc_modegti}) {
    $seggti       = catfile($outdir , $stemoutputs . "sxi_seg.gti");
    $modegti      = catfile($outdir , $stemoutputs . "sxi_mode.gti");

    # Read the TSTART, TSTOP keywords from the exposure file
    my $tstart = ahgen::get_keyword($obsgti,"GTIOBS","TSTART");
    unless ( defined $tstart ) {
      ahlog::ah_out"In file '$obsgti', could not read keyword TSTART";
      push @error_output, "In file: $obsgti (TSTART not defined)";
      return 1;
    }
    my $tstop = ahgen::get_keyword($obsgti,"GTIOBS","TSTOP");
    unless ( defined $tstop ) {
      ahlog::ah_out"In file '$obsgti', could not read keyword TSTOP";
      push @error_output, "In file: $obsgti (TSTOP not defined)";
      return 1;
    }
    # Run the sximodegti script
    my $logfile = $Params{logstem} . "sximodegti.log";
    $status = run_sximodegti($exposure,$seggti,$modegti,{ tstart => $tstart, 
                                                          tstop => $tstop, 
                                                          clobber => "yes",
                                                          logfile => $logfile,
                                                        });
    
    if ( $status ) { ahlog::ah_err "sximodegti failed"; return 1; }

    # Write the coordinate keywords to the output GTI files
    push( @filelist_output, $seggti );
    push( @filelist_output, $modegti );

  }

  foreach my $sxifile ( sort @{$files{event_uf}} ) {

    my @ingtis        = ();
    my $sxifile_out   = form_cleaned_file_name($sxifile);
    my $detnam        = get_keyword( $sxifile, "EVENTS", "DETNAM" );
    my $basename      = basename($sxifile);
    $basename =~ s/\.evt.*$//;

    unless ( defined $detnam ) {
      ahlog::ah_out"In file '$sxifile', could not read keyword DETNAM";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile_out (DETNAM not defined)";
      $Params{numerrs} += 1;
      next;
    }
    # If the DATACTLM is set as a hex number rather than a string,
    # the get_keyword routine incorrectly reads the DATACTLM as a
    # real number. In special cases this will incorrectly convert 
    # the value (e.g. 120004e1 is read as 120004)
    #
    # Manually read the keyword from ftkeypar using the 'value'
    # parameter rather than the interpreted type parameter 
    # ('svalue' or 'rvalue')
    ahgen::run_ftool("ftkeypar",$sxifile . "[EVENTS]","DATACTLM");
    ahgen::run_ftool("pget","ftkeypar","value");
    my $dataclass    = lc ahgen::get_tool_stdout();
    $dataclass =~ s/'//g; # if the keyword is a string, remove the single quotes

    unless ( defined $dataclass ) {
      ahlog::ah_out"In file '$sxifile', could not read keyword DATACTLM";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile_out (DATACTLM not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $datamode      = get_keyword( $sxifile, "EVENTS", "DATAMODE" );
    unless ( defined $datamode ) {
      ahlog::ah_out"In file '$sxifile', could not read keyword DATAMODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile_out (DATAMODE not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $obsmode = ahgen::get_keyword( $sxifile,"EVENTS","OBS_MODE");
    unless ( defined $obsmode ) {
      ahlog::ah_out"In file '$sxifile', could not read keyword OBS_MODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sxifile_out (OBS_MODE not defined)";
      $Params{numerrs} += 1;
      next;
    }

    # Skip any slew event files
    if ( uc $obsmode eq "SLEW" ) { next; }

    # Skip 'erasing mode' SXI files
    # These are files with DATACLASS 100000b0 or 100000b1
    if ( lc $dataclass eq '100000b0' or
         lc $dataclass eq '100000b1' ) {
         ahlog::ah_info "HIGH", "Skipping erasing mode SXI event cleaning ($sxifile)";
         push @error_output, "Skipped erasing mode SXI event cleaning ($sxifile)";
         $Params{numerrs} += 1;
         next;
       }

    # Check that we have good GTI from pointing GTI
    unless ( ahgen::check_hdu_exists($modegti, "GTI_$dataclass" ) ) {
      ahlog::ah_out "No GTI_$dataclass extension in $modegti. Cannot clean SXI Data for $sxifile.";
      push @error_output, "Skipped file: $sxifile_out (Extension GTI_$dataclass does not exist in $modegti)";
      $Params{numerrs} += 1;
      next;
    }
    if ( ! has_good_time( $modegti, "GTI_$dataclass" ) ) {
      ahlog::ah_out "$modegti\[GTI_$dataclass] has no GTI. Cannot clean SXI Data for $sxifile.";
      push @error_output, "Skipped file: $sxifile_out (Extension GTI_$dataclass has no GTI)";
      $Params{numerrs} += 1;
      next;
    }

    # Form the label from the DETNAM and DATAMODE 
    my $windowmode    = get_sxi_mode( $detnam, $datamode );
    if ( $windowmode eq "" ) { 
      ahlog::ah_info "HIGH", "Not a valid mode for file $sxifile.";
      ahlog::ah_info "HIGH", "  DETNAM   : $detnam";
      ahlog::ah_info "HIGH", "  DATAMODE : $datamode"; 
      push @error_output, "Skipped file: $sxifile_out (Not a valid mode ($detnam, $datamode))";
      $Params{numerrs} += 1;
      next; 
    }
    my $label_mode = $detnam . $windowmode;
    my $mkflabel_mode = $mkflabel;
    my $ehklabel_mode = $ehklabel;
    my $evtlabel_mode = $evtlabel;
    $mkflabel_mode =~ s/#/$label_mode/;
    $ehklabel_mode =~ s/#/$label_mode/;
    $evtlabel_mode =~ s/#/$label_mode/;
    my $cpkeywords = "DATAMODE, DEC_NOM, DETNAM, INSTRUME, MJDREFI, MJDREFF, OBS_ID, PA_NOM, RA_NOM, TELESCOP";

    ahlog::ah_debug "Input File       : $sxifile";
    ahlog::ah_debug "Output File      : $sxifile_out";
    ahlog::ah_debug "Detnam           : $detnam";
    ahlog::ah_debug "Datamode         : $datamode";
    ahlog::ah_debug "Dataclass        : $dataclass";
    ahlog::ah_debug "MKF Label        : $mkflabel_mode";
    ahlog::ah_debug "EHK Label        : $ehklabel_mode";
    ahlog::ah_debug "Event Label      : $evtlabel_mode";

    # Verify that we have good GTI
    if ( ! has_good_time( $sxifile, "GTI" ) ) {
      ahlog::ah_out "$sxifile\[GTI] has no GTI. Cannot clean SXI Data.";
      push @error_output, "Skipped file: $sxifile_out (No GTI)";
      $Params{numerrs} += 1;
      next;
    }

    # MKF/EHK depend on DETNAM/WINDOW
    my $gtimkf        = $Params{tempstem} . lc $mkflabel_mode . "_mkf.gti";
    my $gtiehk        = $Params{tempstem} . lc $ehklabel_mode . "_ehk.gti";
    my $logfile = "";

    ahapp::add_temp_file ( $gtimkf );
    ahapp::add_temp_file ( $gtiehk );

    if ( ! -e $gtimkf ) { 
      # Create the MKF GTI file
      $logfile = $Params{logstem} . lc $mkflabel_mode . "_mkf_ahgtigen.log";
      $status = create_gti(infile       => $makefilter,
                           outfile      => $gtimkf,
                           selectfile   => $select,
                           instrume     => $instrument,
                           label        => $mkflabel_mode,
                           cpkeyword    => "NONE",
                           upkeyword    => "yes",
                           clobber => $ahapp::clobber ? "yes" : "no" ,
                           chatter => $ahapp::chatter,
                           logfile => $logfile,
                           );
      if ( $status ) { 
        if ( $status == 2 ) {
          ahlog::ah_out "$gtimkf\[GTI] has no GTI from MKF.";
          push @error_output, "Skipped file: $sxifile_out (No GTIMKF found)";
        } else {
          ahlog::ah_err "ahgtigen failed for SXI MKF: $gtimkf";
          push @error_output, "Skipped file: $sxifile_out (ahgtigen failed)";
        }
        $Params{numerrs} += 1;
        next;
      }
      if ( ! has_good_time( $gtimkf, "GTI" ) ) {
        ahlog::ah_out "$gtimkf\[GTI] has no GTI from MKF.";
        push @error_output, "Skipped file: $sxifile_out (No GTIMKF found)";
        $Params{numerrs} += 1;
        next;
      }

      # Rename MKF GTI extension and append to uf file
      ahgen::set_keyword($gtimkf,"GTI","EXTNAME","GTIMKF");
      ahfilterlib::copy_keywords($sxifile,"EVENTS",$gtimkf,"GTIMKF","",$cpkeywords);
    }
    if (0 != ahgen::check_hdu_exists($sxifile,"GTIMKF")) { ahgen::delete_hdu($sxifile,"GTIMKF"); }
    ahgen::copy_hdu($gtimkf,"GTIMKF",$sxifile);

    if ( ! -e $gtiehk ) { 
      # Create the EHK GTI file
      $logfile = $Params{logstem} . lc $ehklabel_mode . "_ehk_ahgtigen.log";
      $status = create_gti(infile       => $ehk,
                           outfile      => $gtiehk,
                           selectfile   => $select,
                           instrume     => $instrument,
                           label        => $ehklabel_mode,
                           cpkeyword    => "NONE",
                           upkeyword    => "yes",
                           clobber => $ahapp::clobber ? "yes" : "no" ,
                           chatter => $ahapp::chatter,
                           logfile => $logfile,
                           );
      if ( $status ) { 
        if ( $status == 2 ) {
          ahlog::ah_err "$gtiehk\[GTI] has no GTI from SXI EHK.";
          push @error_output, "Skipped file: $sxifile_out (No GTIEHK found)";
        } else {
          ahlog::ah_err "ahgtigen failed for SXI EHK: $gtiehk";
          push @error_output, "Skipped file: $sxifile_out (ahgtigen failed)";
        }
        $Params{numerrs} += 1;
        next;
      }
      if ( ! has_good_time( $gtiehk, "GTI" ) ) {
        ahlog::ah_err "$gtiehk\[GTI] has no GTI from SXI EHK.";
        push @error_output, "Skipped file: $sxifile_out (No GTIEHK found)";
        $Params{numerrs} += 1;
        next;
      }

      # Rename EHK GTI extension and append to uf file
      ahgen::set_keyword($gtiehk,"GTI","EXTNAME","GTIEHK");
      ahfilterlib::copy_keywords($sxifile,"EVENTS",$gtiehk,"GTIEHK","",$cpkeywords);
    }
    if (0 != ahgen::check_hdu_exists($sxifile,"GTIEHK")) { ahgen::delete_hdu($sxifile,"GTIEHK"); }
    ahgen::copy_hdu($gtiehk,"GTIEHK",$sxifile);

    ahlog::ah_out "Cleaning events in $sxifile";

    # Load the GTI files into parameter
    push @ingtis, $sxifile . "[GTI]";
    push @ingtis, $obsgti  . "[GTIPOINT]";
    push @ingtis, $obsgti  . "[GTIATT]";
    push @ingtis, $telgti  . "[GTITEL]";
    push @ingtis, $modegti . "[GTI_$dataclass]";
    push @ingtis, $gtimkf  . "[GTIMKF]";
    push @ingtis, $gtiehk  . "[GTIEHK]";

    # Merge the GTI and screen events
    $logfile = $Params{logstem} . $basename . "_ahscreen.log";
    $status = screen_events(infile      => $sxifile,
                            outfile     => $sxifile_out,
                            gtifile     => join(",",@ingtis),
                            upkeyword   => "yes",
                            cpkeyword   => "yes",
                            leapsecfile => $leapsecfile,
                            selectfile  => $select,
                            label       => $evtlabel_mode,
                            clobber     => "yes",
                            chatter     => $ahapp::chatter,
                            logfile => $logfile,
                            );
    if ( $status ) { 
      if ( $status == 2 ) {
        ahlog::ah_info "HIGH", "ahscreen: merged GTI produced no time interval.";
        ahlog::ah_info "HIGH", "Skipping $sxifile_out.";
        push @error_output, "Skipped file: $sxifile_out (merged GTI produced no time interval)";
      } elsif ( $status == 3 ) {
        ahlog::ah_info "HIGH", "ahscreen: Filtered all events during event screening.";
        ahlog::ah_info "HIGH", "Skipping $sxifile_out.";
        push @error_output, "Skipped file: $sxifile_out (Filtered all events during event screening)";
      } else {
        ahlog::ah_err "ahscreen failed. Skipping $sxifile_out.";
        push @error_output, "Skipped file: $sxifile_out (ahscreen failed)";
      }
      unlink $sxifile_out;
      $Params{numerrs} += 1;
      next;
    }

    push( @filelist_output, $sxifile_out );
    
    push @calibrated_sxi_cl_evt_files, $sxifile_out;

  } # end loop over unfiltered files

  # Save lists of newly calibrated '_uf.evt', '.hpix' and '.fpix' files
  @{$files{event_cl}} = @calibrated_sxi_cl_evt_files;

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

  foreach my $sxifile ( sort @{$files{event_cl}} ) {

    # Calculate the output file names
    my $pha = $sxifile;
    $pha =~ s/_cl.evt/_src.pha/;
    my $img = $sxifile;
    $img =~ s/_cl.evt/_src.img/;
    my $lc = $sxifile;
    $lc =~ s/_cl.evt/_src.lc/;

    # Run the extractor to create spectra, lightcurves and images
    $status = extract(infile  => $sxifile,
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
    foreach my $filename ( sort @{$files{event_uf}} ) {
      ahlog::ah_debug "  $filename\n";
    }
    if ( $Params{calc_hotpix} ) {
      ahlog::ah_debug "$inst hot pixel file(s) calibrated:" if @{$files{hpix}};
      foreach my $filename ( sort @{$files{hpix}} ) {
        ahlog::ah_debug "  $filename\n";
      }
    }
    ahlog::ah_debug "$inst flickering pixel file(s) created:" if @{$files{fpix}};
    foreach my $filename ( sort @{$files{fpix}} ) {
      ahlog::ah_debug "  $filename\n";
    }
  }
  if ( $Params{stage2_switch} ) {
    ahlog::ah_debug "$inst file(s) screened:" if @{$files{event_cl}};
    foreach my $filename ( sort @{$files{event_cl}} ) {
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

  ahgen::run_ftool("pset","sxipipeline","numerrs=$Params{numerrs}");

  # Print the errors found from the pipeline
  if ( $Params{numerrs} ) {
    ahlog::ah_out "SXIPIPELINE had the following errors/warnings:";
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

sub get_dataclass_from_filename {

  my $sxifile = shift;
  my $dataclass = "";

  # Parse the dataclass from the input event file name
  ($dataclass) = $sxifile =~ qr/sxi_[ps][0-9]([0-9a-z]{8})_[a-z]{2}\.evt$zpatt$/,
  return $dataclass;

}

# ------------------------------------------------------------------------------

sub find_hpix_file {

  my $sxifile   = shift;
  my $notfound  = "NONE";
  my $dataclass = get_dataclass_from_filename($sxifile);

  # Find the corresponding hot pixel file by searching
  # through the input hot pixel files and finding
  # a corresponding dataclass
  foreach my $hpix ( @{$files{hpix}} ) {
    if ( $hpix =~ qr/(sxi_a0$dataclass\.hpix)$zpatt$/ ) { return $hpix; }
  }

  return $notfound;

}

# ------------------------------------------------------------------------------

sub form_fpix_file {

  my $sxifile = shift;
  my $fpix = $sxifile;

  # Form the fpix file name using the input event file name
  $fpix =~ s/_uf\.evt/\.fpix/;
  $fpix =~ s/$zpatt$//;

  return $fpix;

}

# ------------------------------------------------------------------------------

sub form_bpix_file {

  my $sxifile = shift;
  my $bpix = $sxifile;

  # Form the fpix file name using the input event file name
  $bpix =~ s/_uf\.evt/\.bpix/;
  $bpix =~ s/$zpatt$//;

  return $bpix;

}

# ------------------------------------------------------------------------------

sub form_img_file {

  my $sxifile = shift;
  my $img = $sxifile;

  # Form the fpix file name using the input event file name
  $img =~ s/_uf\.evt/\.bimg/;
  $img =~ s/$zpatt$//;

  return $img;

}

# ------------------------------------------------------------------------------

sub find_file_mode {

  my $evtfile = shift;

  my ($mode) = $evtfile =~ /sxi_([ps])[0-9][0-9a-f]{8}_uf\.evt$zpatt$/;

  return $mode;

}

# ------------------------------------------------------------------------------

sub find_file_indx {

  my $evtfile = shift;

  my ($indx) = $evtfile =~ /sxi_[ps]([0-9])[0-9a-f]{8}_uf\.evt$zpatt$/;

  return $indx;

}

# ------------------------------------------------------------------------------

sub get_sxi_mode {

    my $detnam = shift;
    my $datamode = shift;

    my $mode = "";

    # Calculate the window mode
    if(uc $detnam eq "CCD" and uc $datamode eq "WINDOW1" ) {
      $mode = "W1";
    } elsif(uc $detnam eq "CCD34" and uc $datamode eq "WINDOW1" ) {
      $mode = "WF";
    } elsif(uc $detnam eq "CCD12" and 
      ( uc $datamode eq "WINDOW2" or
        uc $datamode eq "WINDOW1BURST" or
        uc $datamode eq "WINDOW1BURST2" or
        uc $datamode eq "WINDOW2BURST" ) ) {
      $mode = "WA";
    } else {
      ahlog::ah_err "Not a valid WINDOW mode";
    }

    return $mode;

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
