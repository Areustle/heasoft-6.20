#!/usr/bin/perl
#
# File name: hxipipeline.pl
# Author: J. D. Wysk NASA GSFC
# $Date: 2016/11/28 14:55:18 $
# Version: 0
#
# Calibrate hxi data
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
# Known Issues:
# 1) +++ 2016-02-02 JW: GTIEHK consistently produces no GTI events, which causes the file the skip ahscreen. This is likely just screening exclusion issue.
#
# 2) +++ 2016-02-02 JW: cams2att has a issue with name conflicts when, for example, the prefiltfile params are set to the same name as the tempcorfile params.
#
# 3) +++ 2016-02-04 JW: When running stage 3 (with stage 2 off), if the cleaned files are taken from the input directory, the output files are also deposited in the input dir instead of the outdir.
#       The runnning the form_outfile_name subroutine in stage 3 should fix this issue.


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
  instrument           => "", # "Which instrument (HXI1, HXI2, or HXI {Both})"    

  hxi_start            => 0,  # "HXI CALDB start time"
  verify_input         => 0,  # Verify the input files before processing? 

  ra                   => -999.99999, # RA of nominal pointing [deg]
  dec                  => -999.99999, # Dec of nominal pointing [deg]
  roll                 => 0, # Roll of nominal pointing [deg]

  hx1_optdetx              => -999.99999, # HXI1 optical detx coordinate
  hx1_optdety              => -999.99999, # HXI1 optical dety coordinate
  hx1_optfocx              => -999.99999, # HXI1 optical focx coordinate
  hx1_optfocy              => -999.99999, # HXI1 optical focy coordinate
  hx1_optskyx              => -999.99999, # HXI1 optical focx coordinate
  hx1_optskyy              => -999.99999, # HXI1 optical focy coordinate
  hx2_optdetx              => -999.99999, # HXI2 optical detx coordinate
  hx2_optdety              => -999.99999, # HXI2 optical dety coordinate
  hx2_optfocx              => -999.99999, # HXI2 optical focx coordinate
  hx2_optfocy              => -999.99999, # HXI2 optical focy coordinate
  hx2_optskyx              => -999.99999, # HXI2 optical focx coordinate
  hx2_optskyy              => -999.99999, # HXI2 optical focy coordinate
  hx1_ra_pnt               => -999.99999, # HXI1 RA pointing [deg]
  hx1_dec_pnt              => -999.99999, # HXI1 Dec pointing [deg]
  hx2_ra_pnt               => -999.99999, # HXI2 RA pointing [deg]
  hx2_dec_pnt              => -999.99999, # HXI2 Dec pointing [deg]

  attitude             => "",  # "Attitude file"
  extended_housekeeping=> "",  # "Extended housekeeping file"
  makefilter           => "",  # "Makefilter file, comma seperated list for multiple files"
  orbit                => "",  # "Orbit file, comma seperated list for multiple files"
  obsgti              => "",  # "Time file, comma seperated list for multiple files"  

  #############  CALDB Parameters  #############

  hx1teldef          => "CALDB",      # TelDef File for HXI1
  hx2teldef          => "CALDB",      # TelDef File for HXI2
  remapfile          => "CALDB",  # "hxisgdsff/sgdevtid: remapping file"    
  gainfile           => "CALDB",    # "hxisgdpha: PHA calibration functions"
  badpixfile         => "CALDB",    # "hxisgdpha/hxievtid: readout channels "
  fluorefile         => "CALDB",    # "hxievtid: Input fluorescence file "
  enecutfile         => "CALDB",  #
  cm1teldef          => "CALDB",  #
  cm2teldef          => "CALDB",  #
  camstempxy         => "CALDB",  #
  leapsecfile        => "CALDB",  #
  selectfile         => "CALDB",  #

  #############  STAGING Parameters  #############

  entry_stage          => 1, # 
  exit_stage           => 1, # 

  stage1_switch        => 0, # 
  stage2_switch        => 0, # 
  stage3_switch        => 0, # 

  numerrs              => 0, # 

  #############  Parameters for specified tools  #############

  # hxisgdsff pars 
  # NONE

  # hxisgdpha pars 
  outnsubcol         => "no",
  datamode           => "NONE",

  # hxievtid pars 
  occurrenceid  => -1,
  rejectbgo     => "yes",
  outcalfile    => "NONE",
  skipreco      => "no",

  # cams2att pars
  infile1         => "",
  infile2         => "",
  outfile         => "",    
  instrume        => "",
  cams1teldef     => "",    
  cams2teldef     => "",    
  hxiteldef       => "",    
  camstempxy      => "",    
  startstep       => 1,
  stopstep        => 5,
  inext           => "EVENTS",
  outext          => "CAMS_OFFSETS",
  flipsign        => "no",
  tempcorfile1    => "",    
  tempcorfile2    => "",    
  prefiltfile1    => "NONE",
  prefiltfile2    => "NONE",
  offsetfile      => "",    
  filtoffset      => "NONE",
  prefiltexpr     => "DSP_UP==1&&IS_SAMPLING==1",
  filtexpr        => "BAD_UNITS==0",
  gtiexpr0        => "BAD_UNITS==0",
  gtiexpr1        => "BAD_UNITS==2",
  gtiexpr2        => "BAD_UNITS==1",
  startsys        => "RAW",
  deltaxcol       => "DELTARAWX",
  deltaycol       => "DELTARAWY",
  sincol          => "SINANGLE",
  coscol          => "COSANGLE",

  # coordevt pars 
  teldeffile      => "",    
  attfile         => "",    
  dattfile        => "",    
  orbfile         => "",    
  coordevt_startsys        => "LOWEST",
  stopsys         => "HIGHEST",
  annaber         => "NO",
  followsun       => "NO",
  orbaber         => "NO",
  attinterp       => "LINEAR",
  dattinterp      => "LINEAR",
  attdt           => 32., 
  dattdt          => 0.5, 
  chkattgap       => "NO",
  chkdattgap      => "YES",
  attext          => "ATTITUDE",
  attcol          => "QPARAM",
  attform         => "QUAT",
  orbext          => "ORBIT",
  orbcol          => "VELOCITY",
  orbform         => "VECTOR",
  coordevt_randomize       => "TELDEF",
  seed            => "",    
  randsys         => "TELDEF",
  randscalesys    => "TELDEF",
  infileext       => "EVENTS",
  timecol         => "TIME",
  inclfloatcol    => "NO",
  inclfloatskycol => "NO",
  floatcolsuffix  => "_FLOAT",
  startwithfloat  => "NO",
  blankcol        => "YES",
  btnull          => 255 ,
  itnull          => -999,
  jtnull          => -999,
  ktnull          => -999,
  sbtnull         => 255 ,
  uitnull         => -999,
  ujtnull         => -999,
);

our %instrument = (     # "Which instrument (HXI1, HXI2, or Both)"    
  # +++ 2014-12-18 AS: CAMS?
  hx1                   => "",  # HXI HXI1
  hx2                   => "",   # HXI HXI2
);

our %files = (
  infilecm1             => "",  
  infilecm2             => "",  
  attitude              => "",
  orbit                 => "",
  extended_housekeeping => "",
  makefilter            => "",
  obsgti               => "",
  regionfile            => "",
  telgti                => "",

  event_uf          => [],
  event_ufa         => [],
  event_exp         => [],
  event_pse         => [],
  event_cl          => [],
);

our $zpatt = qr/(\.Z|\.z|\.gzip|\.GZIP|\.gz|\.GZ|\.zip\.ZIP)?/;
our %patterns = (
  mission           => qr/ah/,
  sequence          => qr/[0-9]{1,9}/,
  attitude              => qr/(\.att)$zpatt$/,
  orbit                 => qr/(\.orb)$zpatt$/,
  cms_1                 => qr/(cm1_a0_uf\.fits)$zpatt$/,
  cms_2                 => qr/(cm2_a0_uf.fits)$zpatt$/,
  hx1_event_uf          => qr/(hx1_[ps][0-9]cam_uf\.evt)$zpatt$/,
  hx2_event_uf          => qr/(hx2_[ps][0-9]cam_uf\.evt)$zpatt$/,
  hx1_event_ufa         => qr/(hx1_[ps][0-9]camrec_ufa\.evt)$zpatt$/,
  hx2_event_ufa         => qr/(hx2_[ps][0-9]camrec_ufa\.evt)$zpatt$/,
  hx1_event_exp         => qr/(hx1_[ps][0-9]camexp_ufa\.evt)$zpatt$/,
  hx2_event_exp         => qr/(hx2_[ps][0-9]camexp_ufa\.evt)$zpatt$/,
  hx1_event_cl          => qr/(hx1_[ps][0-9]camrec_cl\.evt)$zpatt$/,
  hx2_event_cl          => qr/(hx2_[ps][0-9]camrec_cl\.evt)$zpatt$/,
  hxi_telgti            => qr/(hxi_tel\.gti)$zpatt$/,
);

my $pipelineerror = 0;  # hxipipeline exit status
my $camsflag = 0;       # Flag to skip cams2att if both cams infiles are missing. 

our @filelist_output;
our @error_output;

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

# Reset the numerrs parameters
$Params{numerrs} = 0;
ahgen::run_ftool("pset","hxipipeline","numerrs=0");

$pipelineerror = get_parameters () ;
unless ( $pipelineerror == 0 ) {
  push @error_output, "get_parameters failed";
  $Params{numerrs} += 1;
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

$pipelineerror = CheckInstrument();
unless ( $pipelineerror == 0 ) {
  $Params{numerrs} += 1;
  push @error_output, "CheckInstrument failed";
  ahlog::ah_err "CheckInstrument" ;
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
  $Params{instrument}   = ahapp::query_parameter("instrument");
  $Params{entry_stage}  = ahapp::query_parameter("entry_stage");
  $Params{exit_stage}   = ahapp::query_parameter("exit_stage");

  $Params{hxi_start}    = ahapp::query_parameter("hxi_start");

  $Params{attitude}     = ahapp::query_parameter("attitude");
  $Params{orbit}        = ahapp::query_parameter("orbit");
  $Params{makefilter}   = ahapp::query_parameter("makefilter");
  $Params{extended_housekeeping} = ahapp::query_parameter("extended_housekeeping");
  $Params{obsgti}      = ahapp::query_parameter("obsgti");
  $Params{regionfile}   = ahapp::query_parameter("regionfile");

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

  # CALDB files
  $Params{leapsecfile}       = ahapp::query_parameter("leapsecfile");
  $Params{selectfile}        = ahapp::query_parameter("selectfile");

  $Params{hxi_mkflabel}      = ahapp::query_parameter("hxi_mkflabel");
  $Params{hxi_ehklabel}      = ahapp::query_parameter("hxi_ehklabel");
  $Params{hxi_evtlabel}      = ahapp::query_parameter("hxi_evtlabel");

  if ( $Params{stage1_switch} ) {
    # If we aren't calibrating events, finding tool parameters
    # can significantly slow down the pipeline script

    # CALDB files
    $Params{remapfile}          = ahapp::query_parameter("remapfile");  
    $Params{gainfile}           = ahapp::query_parameter("gainfile");   
    $Params{badpixfile}         = ahapp::query_parameter("badpixfile"); 
    $Params{fluorefile}         = ahapp::query_parameter("fluorefile"); 
    $Params{enecutfile}        = ahapp::query_parameter("enecutfile");
    $Params{hx1teldef}        = ahapp::query_parameter("hx1teldef");
    $Params{hx2teldef}        = ahapp::query_parameter("hx2teldef");
    $Params{cm1teldef}        = ahapp::query_parameter("cm1teldef");
    $Params{cm2teldef}        = ahapp::query_parameter("cm2teldef");
    $Params{camstempxy}        = ahapp::query_parameter("camstempxy");

    # Coordinate parameters
    $Params{ra}           = ahapp::query_parameter("ra");
    $Params{dec}          = ahapp::query_parameter("dec");
    $Params{roll}         = ahapp::query_parameter("roll");
    $Params{hx1_optdetx}      = ahapp::query_parameter("hx1_optdetx");
    $Params{hx1_optdety}      = ahapp::query_parameter("hx1_optdety");
    $Params{hx1_optfocx}      = ahapp::query_parameter("hx1_optfocx");
    $Params{hx1_optfocy}      = ahapp::query_parameter("hx1_optfocy");
    $Params{hx1_optskyx}      = ahapp::query_parameter("hx1_optskyx");
    $Params{hx1_optskyy}      = ahapp::query_parameter("hx1_optskyy");
    $Params{hx2_optdetx}      = ahapp::query_parameter("hx2_optdetx");
    $Params{hx2_optdety}      = ahapp::query_parameter("hx2_optdety");
    $Params{hx2_optfocx}      = ahapp::query_parameter("hx2_optfocx");
    $Params{hx2_optfocy}      = ahapp::query_parameter("hx2_optfocy");
    $Params{hx2_optskyx}      = ahapp::query_parameter("hx2_optskyx");
    $Params{hx2_optskyy}      = ahapp::query_parameter("hx2_optskyy");
    $Params{hx1_ra_pnt}       = ahapp::query_parameter("hx1_ra_pnt");
    $Params{hx1_dec_pnt}      = ahapp::query_parameter("hx1_dec_pnt");
    $Params{hx2_ra_pnt}       = ahapp::query_parameter("hx2_ra_pnt");
    $Params{hx2_dec_pnt}      = ahapp::query_parameter("hx2_dec_pnt");

    # Shared pars
    $Params{occurrenceid}       = ahapp::query_parameter("occurrenceid");
    $Params{seed}               = ahapp::query_parameter("seed");

    #############  Parameters for specified tools  #############

    # hxisgdsff pars 
    # NONE

    # hxisgdpha pars 
    $Params{outnsubcol}      = ahapp::query_parameter("outnsubcol");
    $Params{datamode}        = ahapp::query_parameter("datamode");
    $Params{randomize}          = ahapp::query_parameter("randomize");

    # hxievtid pars 
    $Params{rejectbgo}       = ahapp::query_parameter("rejectbgo");
    $Params{skipreco}        = ahapp::query_parameter("skipreco");
    $Params{outcalfile}    = ahapp::query_parameter("outcalfile");

    # cams2att pars
    $Params{startsys}           = ahapp::query_parameter("startsys");
    $Params{startstep}       = ahapp::query_parameter("startstep");
    $Params{stopstep}        = ahapp::query_parameter("stopstep");
    $Params{inext}           = ahapp::query_parameter("inext");
    $Params{outext}          = ahapp::query_parameter("outext");
    $Params{flipsign}        = ahapp::query_parameter("flipsign");
    $Params{prefiltfile1}    = ahapp::query_parameter("prefiltfile1");
    $Params{prefiltfile2}    = ahapp::query_parameter("prefiltfile2");
    $Params{filtoffset}      = ahapp::query_parameter("filtoffset");
    $Params{prefiltexpr}     = ahapp::query_parameter("prefiltexpr");
    $Params{filtexpr}        = ahapp::query_parameter("filtexpr");
    $Params{gtiexpr0}        = ahapp::query_parameter("gtiexpr0");
    $Params{gtiexpr1}        = ahapp::query_parameter("gtiexpr1");
    $Params{gtiexpr2}        = ahapp::query_parameter("gtiexpr2");
    $Params{deltaxcol}       = ahapp::query_parameter("deltaxcol");
    $Params{deltaycol}       = ahapp::query_parameter("deltaycol");
    $Params{sincol}          = ahapp::query_parameter("sincol");
    $Params{coscol}          = ahapp::query_parameter("coscol");

    # coordevt pars
    $Params{coordevt_startsys}           = ahapp::query_parameter("coordevt_startsys");
    $Params{stopsys}         = ahapp::query_parameter("stopsys");
    $Params{annaber}         = ahapp::query_parameter("annaber");
    $Params{followsun}       = ahapp::query_parameter("followsun");
    $Params{orbaber}         = ahapp::query_parameter("orbaber");
    $Params{attinterp}       = ahapp::query_parameter("attinterp");
    $Params{dattinterp}      = ahapp::query_parameter("dattinterp");
    $Params{attdt}           = ahapp::query_parameter("attdt");
    $Params{dattdt}          = ahapp::query_parameter("dattdt");
    $Params{chkattgap}       = ahapp::query_parameter("chkattgap");
    $Params{chkdattgap}      = ahapp::query_parameter("chkdattgap");
    $Params{attext}          = ahapp::query_parameter("attext");
    $Params{attcol}          = ahapp::query_parameter("attcol");
    $Params{attform}         = ahapp::query_parameter("attform");
    $Params{orbext}          = ahapp::query_parameter("orbext");
    $Params{orbcol}          = ahapp::query_parameter("orbcol");
    $Params{orbform}         = ahapp::query_parameter("orbform");
    $Params{coordevt_randomize}         = ahapp::query_parameter("coordevt_randomize");
    $Params{randsys}         = ahapp::query_parameter("randsys");
    $Params{randscalesys}    = ahapp::query_parameter("randscalesys");
    $Params{infileext}       = ahapp::query_parameter("infileext");
    $Params{timecol}         = ahapp::query_parameter("timecol");
    $Params{inclfloatcol}    = ahapp::query_parameter("inclfloatcol");
    $Params{inclfloatskycol} = ahapp::query_parameter("inclfloatskycol");
    $Params{floatcolsuffix}  = ahapp::query_parameter("floatcolsuffix");
    $Params{startwithfloat}  = ahapp::query_parameter("startwithfloat");
    $Params{blankcol}        = ahapp::query_parameter("blankcol");
    $Params{btnull}          = ahapp::query_parameter("btnull");
    $Params{itnull}          = ahapp::query_parameter("itnull");
    $Params{jtnull}          = ahapp::query_parameter("jtnull");
    $Params{ktnull}          = ahapp::query_parameter("ktnull");
    $Params{sbtnull}         = ahapp::query_parameter("sbtnull");
    $Params{uitnull}         = ahapp::query_parameter("uitnull");
    $Params{ujtnull}         = ahapp::query_parameter("ujtnull");

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
    $Params{tempstem} = "hxipipeline_" . $taskstart . "_tmp_";
    $Params{logstem} = "hxipipeline_" . $taskstart . "_";
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
  my $orbit        = $Params{orbit};
  my $makefilter   = $Params{makefilter};
  my $extended_housekeeping = $Params{extended_housekeeping};
  my $obsgti      = $Params{obsgti};
  my $regionfile   = $Params{regionfile};
  my $remapfile    = $Params{remapfile};
  my $gainfile     = $Params{gainfile};
  my $badpixfile   = $Params{badpixfile};
  my $fluorefile   = $Params{fluorefile};
  my $enecutfile   = $Params{enecutfile};

  # Check the input and output directories
  if (CheckInputDirectory($indir,$outdir)) { return 1; };
  if (CheckOutputDirectory($outdir)) { return 1; };

  # Find and store the list of input files
  if ($inst eq "HXI" || $inst eq "HXI1"){ 
    push( @{$files{event_uf}}, FindInputFiles($indir,$steminputs , $patterns{hx1_event_uf}));
    push( @{$files{event_ufa}}, FindInputFiles($indir,$steminputs , $patterns{hx1_event_ufa}));
    push( @{$files{event_exp}}, FindInputFiles($indir,$steminputs , $patterns{hx1_event_exp}));
    push( @{$files{event_cl}}, FindInputFiles($indir,$steminputs , $patterns{hx1_event_cl}));
  }

  if ($inst eq "HXI" || $inst eq "HXI2"){ 
    push( @{$files{event_uf}}, FindInputFiles($indir,$steminputs , $patterns{hx2_event_uf}));
    push( @{$files{event_ufa}}, FindInputFiles($indir,$steminputs , $patterns{hx2_event_ufa}));
    push( @{$files{event_exp}}, FindInputFiles($indir,$steminputs , $patterns{hx2_event_exp}));
    push( @{$files{event_cl}}, FindInputFiles($indir,$steminputs , $patterns{hx2_event_cl}));
  }

  ($files{telgti})    = FindInputFiles($indir,$steminputs , $patterns{hxi_telgti});
  
  # Check the input CALDB files
  if ( $Params{stage1_switch} ) {
    # Check for file requirements
    if ( ! -e $attitude ) {
      ahlog::ah_out"WARNING: Could not find attitude file: '$attitude'";
    }
    if ( ! -e $orbit ) {
      ahlog::ah_out"WARNING: Could not find orbit file: '$orbit'";
    }
    if (isRequiredFileNotFound($obsgti)) { return 1; }

    # Store the required input files
    ($files{infilecm1})    = FindInputFiles($indir,$steminputs , $patterns{cms_1});
    ($files{infilecm2})    = FindInputFiles($indir,$steminputs , $patterns{cms_2});

    # Check if CAMS files exist.
    # Set to "NONE" if thet don't.
    unless ( (defined $files{infilecm1}) and (-e $files{infilecm1}) ) {
      ahlog::ah_out"WARNING: Coud not find cm1 infile!";
      ahlog::ah_out"Setting infilecm1 parameter to 'NONE'";
      $files{infilecm1} = "NONE"; 
    }
    unless ( (defined $files{infilecm2}) and (-e $files{infilecm2}) ) {
      ahlog::ah_out"WARNING: Coud not find cm2 infile!";
      ahlog::ah_out"Setting infilecm2 parameter to 'NONE'";
      $files{infilecm2} = "NONE"; 
    }
  
    # Skip cam2att if neither exist and create an identity att file.
    if ( $files{infilecm1} eq "NONE" and $files{infilecm2} eq "NONE" ) {
      ahlog::ah_out"WARNING: No input CAMS data!";
      ahlog::ah_out"Skipping cams2att and generating an identity attitude file.";
      $camsflag = 1; 

      my $dattfilehx1     = catfile($outdir , $stemoutputs . "hx1.att");
      my $dattfilehx2     = catfile($outdir , $stemoutputs . "hx2.att");

      my $tstart = ahgen::get_keyword( $obsgti,"GTIOBS","TSTART");
      ahlog::ah_debug "tstart = $tstart}\n";
      unless ( defined $tstart ) {
        ahlog::ah_err"Could not read GTIOBS keyword TSTART";
        return 1;
      }
      my $tstop = ahgen::get_keyword( $obsgti,"GTIOBS","TSTOP");
      ahlog::ah_debug "tstop = $tstop}\n";
      unless ( defined $tstop ) {
        ahlog::ah_err"Could not read GTIOBS keyword TSTOP";
        return 1;
      }

      create_delta_attitude($dattfilehx1,$tstart,$tstop,"HXI1");
      create_delta_attitude($dattfilehx2,$tstart,$tstop,"HXI2");
    }

    $files{attitude}      = $attitude;
    $files{orbit}         = $orbit;
    $files{obsgti}        = $obsgti;

    # Check the CALDB files
    if (isBadCALDBFileParameterValue($Params{remapfile},"remapfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{gainfile},"gainfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{badpixfile},"badpixfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{fluorefile},"fluorefile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{enecutfile},"enecutfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{hx1teldef},"hx1teldef")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{hx2teldef},"hx2teldef")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{cm1teldef},"cm1teldef")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{cm2teldef},"cm2teldef")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{camstempxy},"camstempxy")) { return 1;}
  }

  if ( $Params{stage2_switch} ) {
    # Check for file requirements
    if (isRequiredFileNotFound($obsgti)) { return 1; }
    if (isRequiredFileNotFound($makefilter)) { return 1; }
    if (isRequiredFileNotFound($extended_housekeeping)) { return 1; }

    $files{obsgti}       = $obsgti;
    $files{makefilter}    = $makefilter;
    $files{extended_housekeeping} = $extended_housekeeping;

    if (isBadCALDBFileParameterValue($Params{leapsecfile},"leapsecfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{selectfile},"selectfile")) { return 1;}
  }
  if ( $Params{stage3_switch} ) {
    $files{regionfile}    = $regionfile;
    if (isOptionalFileNotFound($files{regionfile})) { return 1; }
  }

  # Search for required files for each step

  # Check that we found a list of unfiltered event files
  if ( $Params{stage1_switch} or $Params{stage2_switch} ) {
    unless ( @{$files{event_uf}} ) {
      ahlog::ah_err "Cannot find any $inst event files in $indir" ;
      return 1;
    }
  }
       
  # For cleaning events, mode GTI is required
  # Check whether we are calculating HXI GTI from the exposure
  # or using an input GTI
  if( $Params{stage2_switch} ) {
    unless ( $files{telgti} ) {
      ahlog::ah_err "Cannot find any $inst telemetry GTI files in $indir" ;
      return 1;
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
    ahlog::ah_debug "GTI file                   = $files{obsgti}\n" if $files{obsgti};
    ahlog::ah_debug "Extended housekeeping file = $files{extended_housekeeping}\n" if $files{extended_housekeeping};
    ahlog::ah_debug "Makefilter file            = $files{makefilter}\n" if $files{makefilter};
    ahlog::ah_debug "Region file                = $files{regionfile}\n" if $files{regionfile};
    ahlog::ah_debug "Telemetry Saturation GTI   = $files{telgti}\n" if $files{telgti};
    ahlog::ah_debug "$inst unfiltered file(s) found:" if @{$files{event_uf}};
    foreach my $filename ( @{$files{event_uf}} ) {
      ahlog::ah_debug "  $filename\n";
    }
    foreach my $filename ( @{$files{event_ufa}} ) {
      ahlog::ah_debug "  $filename\n";
    }
    foreach my $filename ( @{$files{event_exp}} ) {
      ahlog::ah_debug "  $filename\n";
    }
    ahlog::ah_debug "$inst cleaned file(s) found:" if @{$files{event_cl}};
    foreach my $filename ( @{$files{event_cl}} ) {
      ahlog::ah_debug "  $filename\n";
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

  @{$files{event_ufa}} = ();
  @{$files{event_exp}} = ();
  my @calibrated_hxi_uf_evt_files = ( ); 
  my @calibrated_hxi_ufa_evt_files = ( ); 
  my @calibrated_hxi_exp_evt_files = ( ); 
  my $outdir       = $Params{outdir};
  my $obsgti       = $Params{obsgti};
  my $steminputs   = $Params{steminputs};
  my $stemoutputs  = $Params{stemoutputs};
  my $instrument   = $Params{instrument};
  my $infilecm1    = $files{infilecm1};
  my $infilecm2    = $files{infilecm2};
  my $dattfilehx1  = catfile($outdir , $stemoutputs . "hx1.att");
  my $dattfilehx2  = catfile($outdir , $stemoutputs . "hx2.att");
  my $status = 0;

  #############  setup  parameters for each tool #############

  my %hxisgdsff_pars = (
    remapfile    => $Params{remapfile},
    clobber      => $ahapp::clobber ? "yes" : "no" ,
    chatter      => $ahapp::chatter,
  );

  my %hxisgdpha_pars = (
    gainfile     => $Params{gainfile},  
    badpixfile   => $Params{badpixfile},
    outnsubcol   => $Params{outnsubcol},
    randomize    => $Params{randomize},
    seed         => $Params{seed},     
    datamode     => $Params{datamode},
    clobber      => $ahapp::clobber ? "yes" : "no" ,
    chatter      => $ahapp::chatter,
  );

  my %hxievtid_pars = (
    remapfile    => $Params{remapfile},
    fluorefile   => $Params{fluorefile},
    badpixfile   => $Params{badpixfile},
    enecutfile   => $Params{enecutfile},
    occurrenceid => $Params{occurrenceid},
    rejectbgo    => $Params{rejectbgo},
    outcalfile   => $Params{outtracefile},  # +++ 2015-01-05 AS: Possible ahpipeline parameter, not required
    skipreco     => $Params{skipreco},
    chatter      => $ahapp::chatter,
  );

    ahapp::add_temp_file ( "cams2attgti.tmp" );

  my %cams2att_pars = (
    outfile         => $Params{outfile},        # Archive OBS & Trend: Delta-Attitude
    cams1teldef     => $Params{cm1teldef},    
    cams2teldef     => $Params{cm2teldef},    
    camstempxy      => $Params{camstempxy},     
    startstep       => $Params{startstep},      
    stopstep        => $Params{stopstep},       
    inext           => $Params{inext},          
    outext          => $Params{outext},         
    flipsign        => $Params{flipsign},       
    prefiltfile1    => $Params{prefiltfile1},   
    prefiltfile2    => $Params{prefiltfile2},   
    filtoffset      => $Params{filtoffset},     
    prefiltexpr     => $Params{prefiltexpr},    
    filtexpr        => $Params{filtexpr},       
    gtiexpr0        => $Params{gtiexpr0},       
    gtiexpr1        => $Params{gtiexpr1},       
    gtiexpr2        => $Params{gtiexpr2},       
    gtifile         => "cams2attgti.tmp",       # Pass this a dummy tmp file.
    startsys        => $Params{startsys},       
    deltaxcol       => $Params{deltaxcol},      
    deltaycol       => $Params{deltaycol},      
    sincol          => $Params{sincol},         
    coscol          => $Params{coscol},         
    clobber         => $ahapp::clobber ? "yes" : "no" ,
    chatter         => $ahapp::chatter,
  );

  my %coordevt_pars = (
    attfile         => $Params{attitude},         
    orbfile         => $Params{orbit},          
    startsys        => $Params{coordevt_startsys},             
    stopsys         => $Params{stopsys},          
    annaber         => $Params{annaber},          
    followsun       => $Params{followsun},        
    orbaber         => $Params{orbaber},          
    attinterp       => $Params{attinterp},        
    dattinterp      => $Params{dattinterp},       
    attdt           => $Params{attdt},            
    dattdt          => $Params{dattdt},           
    chkattgap       => $Params{chkattgap},        
    chkdattgap      => $Params{chkdattgap},       
    attext          => $Params{attext},           
    attcol          => $Params{attcol},           
    attform         => $Params{attform},          
    orbext          => $Params{orbext},           
    orbcol          => $Params{orbcol},           
    orbform         => $Params{orbform},          
    randomize       => $Params{coordevt_randomize},
    seed            => $Params{seed},             
    randsys         => $Params{randsys},          
    randscalesys    => $Params{randscalesys},     
    infileext       => $Params{infileext},        
    timecol         => $Params{timecol},          
    inclfloatcol    => $Params{inclfloatcol},     
    inclfloatskycol => $Params{inclfloatskycol},  
    floatcolsuffix  => $Params{floatcolsuffix},   
    startwithfloat  => $Params{startwithfloat},   
    blankcol        => $Params{blankcol},         
    btnull          => $Params{btnull},           
    itnull          => $Params{itnull},           
    jtnull          => $Params{jtnull},           
    ktnull          => $Params{ktnull},           
    sbtnull         => $Params{sbtnull},          
    uitnull         => $Params{uitnull},          
    ujtnull         => $Params{ujtnull},          
    ra              => $Params{ra},               
    dec             => $Params{dec},              
    roll            => 0,
    chatter         => $ahapp::chatter,
  );

  # Sets the stopsys parameter to "FOC" if the att file is missing.
  if ( ! -e $files{attitude} ) {
    ahlog::ah_out"Attitude file '$files{attitude}' is missing!";
    ahlog::ah_out"Setting coordevt pointing par 'stopsys' = 'FOC'";
    $coordevt_pars{stopsys} = "FOC";
  }

  ahlog::ah_debug "camsflag = $camsflag\n";
  unless ( $camsflag != 0 ) {
    my $badunits = "BAD_UNITS==0";

    if ( uc $infilecm1 eq "NONE" ) {
      $badunits = "BAD_UNITS==1";
    }
    if ( uc $infilecm2 eq "NONE" ) {
      $badunits = "BAD_UNITS==2";
    }

    $cams2att_pars{filtexpr} = $badunits;
    $cams2att_pars{gtiexpr0} = $badunits;

    # Set up input and output files for cams2att
    my $infilecm1_out   = "NONE";
    my $infilecm2_out   = "NONE";
  
    if ( -e $infilecm1 ) {
      $infilecm1_out   = form_outfile_name($infilecm1);
    } else {
      ahlog::ah_out"No cm1 infile found.";
      ahlog::ah_out"cm1 outfile set to 'NONE'";
    }
  
    if ( -e $infilecm2 ) {
     $infilecm2_out   = form_outfile_name($infilecm2);
    } else {
      ahlog::ah_out"No cm2 infile found.";
      ahlog::ah_out"cm2 outfile set to 'NONE'";
    }
  
    my $offsethx1       = catfile($outdir , $stemoutputs . "hx1_cms.fits");
    my $offsethx2       = catfile($outdir , $stemoutputs . "hx2_cms.fits");
  
    # Run cams2att for HXI1:
    $cams2att_pars{instrume} = "HXI1";
    $cams2att_pars{hxiteldef} = $Params{hx1teldef};
    $cams2att_pars{tempcorfile1} = $infilecm1_out;
    $cams2att_pars{tempcorfile2} = $infilecm2_out; 
    $cams2att_pars{logfile} = $Params{logstem} . "hx1_cams2att.log";
    $status = ahpllib::run_cams2att($infilecm1,$infilecm2,$dattfilehx1,$offsethx1,\%cams2att_pars);
    if ( $status ) { ahlog::ah_err "cams2att failed"; return $status; }
    push( @filelist_output, $dattfilehx1);
    push( @filelist_output, $offsethx1 );

    # Run cams2att for HXI2:
    $cams2att_pars{instrume} = "HXI2";
    $cams2att_pars{hxiteldef} = $Params{hx2teldef};
    $cams2att_pars{tempcorfile1} = "NONE"; # set this to NONE in the second cams2att run
    $cams2att_pars{tempcorfile2} = "NONE"; 
    $cams2att_pars{logfile} = $Params{logstem} . "hx2_cams2att.log";
    $status = ahpllib::run_cams2att($infilecm1_out,$infilecm2_out,$dattfilehx2,$offsethx2,\%cams2att_pars);
    if ( $status ) { ahlog::ah_err "cams2att failed"; return $status; } 
    push( @filelist_output, $dattfilehx1);
    push( @filelist_output, $offsethx2 );

    # Remove some excess cams2att files
    unlink "coordevt.log";
    unlink "cams2det.log";
    unlink "cams2attgti.tmp";
  }

  # Find the first and the last HXI1/2 files based on TSTART/TSTOP
  # due to file splitting. We want to attach a GTI extension to the output
  # event file. If there is a file split, the first file
  # in time uses the GTIPOINT TSTART to the event file TSTOP
  # Each subsequent file split uses GTIPOINT between that file's TSTART/TSTOP
  # The last file in time uses TSTART from the event file and TSTOP from GTIPOINT
  #
  # Example: An event file is split three times. 
  # GTIPOINT: |----------------------------------------------| 
  # EVENTS  :   |-----------|--------------|--------------|
  # GTI1    : |-------------|
  # GTI2    :               |--------------|
  # GTI3    :                              |-----------------|
  my %first_indx;
  my %last_indx;
  my %tstart_prev;
  my %tstop_prev;

  # Find the first and last indx for HXI
  foreach my $evtfile ( @{$files{event_uf}} ) {
    my $inst = find_file_inst($evtfile);
    my $mode = find_file_mode($evtfile);
    my $indx = find_file_indx($evtfile);
    # Skip any slew file
    if ( lc $mode eq "s" ) { next; }

    if ( $indx == 0 ) {
      # If the indx is 0, then there is no file split
      # We can ignore the first and last indx
      $first_indx{$inst}=0;
      $last_indx{$inst}=0;
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
    unless ( $tstart_prev{$inst} ) { $tstart_prev{$inst} = $tstart; $first_indx{$inst} = $indx; }
    unless ( $tstop_prev{$inst} ) { $tstop_prev{$inst} = $tstop; $last_indx{$inst} = $indx; }
    if ( $tstart < $tstart_prev{$inst} ) { $tstart_prev{$inst} = $tstart; $first_indx{$inst} = $indx; }
    if ( $tstop > $tstop_prev{$inst} ) { $tstop_prev{$inst} = $tstop; $last_indx{$inst} = $indx; }
  }

  ############# Calibrate each hxi file(s) #############
  foreach my $hxifile ( sort @{$files{event_uf}} ) {

    #############  setup input/output files #############

    my $hxi_start    = $Params{hxi_start};    
    # Set up input and output files
    my $hxifile_out   = form_outfile_name($hxifile);
    my $hxifile_out_sffa = $hxifile_out;
    $hxifile_out_sffa =~ s/_uf.evt/rec_ufa.evt/g;

    my $basename      = basename($hxifile_out);
    $basename =~ s/\.evt.*$//;

    # Determine the file index for this hxi file
    my $inst = find_file_inst($hxifile_out);
    my $indx = find_file_indx($hxifile_out);

    # Determine the first and last indexes for this instrument
    my $first_index = $first_indx{$inst};
    my $last_index = $last_indx{$inst};

    # Skip the file if there are no events
    my $naxis2 = ahgen::get_keyword($hxifile,"EVENTS","NAXIS2");
    unless ( defined $naxis2 ) {
      ahlog::ah_out"In file '$hxifile', could not read keyword NAXIS2";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $hxifile (NAXIS2 not defined)";
      $Params{numerrs} += 1;
      next;
    }
    unless ( $naxis2 ) { 
      ahlog::ah_out"No events found in file '$hxifile'";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $hxifile (NAXIS2=0)";
      $Params{numerrs} += 1;
      next;
    }

    # read keywords TSTART, TSTOP, DATE-OBS, and OBSMODE
    my $tstart = ahgen::get_keyword($hxifile,"EVENTS","TSTART");
    unless ( defined $tstart ) {
      ahlog::ah_out"In file '$hxifile', could not read keyword TSTART";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $hxifile (TSTART not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $tstop = ahgen::get_keyword($hxifile,"EVENTS","TSTOP");
    unless ( defined $tstop ) {
      ahlog::ah_out"In file '$hxifile', could not read keyword TSTOP";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $hxifile (TSTOP not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $dateobs = ahgen::get_keyword($hxifile,"EVENTS","DATE-OBS");
    unless ( defined $dateobs ) {
      ahlog::ah_out"In file '$hxifile', could not read keyword DATE-OBS";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $hxifile (DATE-OBS not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $obsmode = ahgen::get_keyword($hxifile,"EVENTS","OBS_MODE");
    unless ( defined $obsmode ) {
      ahlog::ah_out"In file '$hxifile', could not read keyword OBS_MODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $hxifile (OBS_MODE not defined)";
      $Params{numerrs} += 1;
      next;
    }

    # Read the instrument keyword use it to set coordevt pars below 
    my $instrume = ahgen::get_keyword($hxifile,"EVENTS","INSTRUME");
    if(ahgen::get_error_flag()) {
      ahlog::ah_err "Could not get INSTRUME keyword from $hxifile";
      push @error_output, "Skipped file: $hxifile (INSTRUME not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $detnam = ahgen::get_keyword($hxifile,"EVENTS","DETNAM");
    if(ahgen::get_error_flag()) {
      ahlog::ah_err "Could not get DETNAM keyword from $hxifile";
      push @error_output, "Skipped file: $hxifile (DETNAM not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $datamode = ahgen::get_keyword($hxifile,"EVENTS","DATAMODE");
    unless ( defined $datamode ) {
      ahlog::ah_out"In file '$hxifile', could not read keyword DATAMODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $hxifile (DATAMODE not defined)";
      $Params{numerrs} += 1;
      next;
    }

    ahlog::ah_debug "Input File       : $hxifile";
    ahlog::ah_debug "Output File      : $hxifile_out";
    ahlog::ah_debug "tstart           : $tstart";
    ahlog::ah_debug "tstop            : $tstop";
    ahlog::ah_debug "dateobs:         : $dateobs";
    ahlog::ah_debug "obsmode:         : $obsmode";
    ahlog::ah_debug "instrume:        : $instrume";
    ahlog::ah_debug "datamode:        : $datamode";
    ahlog::ah_debug "infilecm1:       : $infilecm1";
    ahlog::ah_debug "infilecm2:       : $infilecm2";

    # can this file ($hxifile) be processed with the caldb files?
    if ( $tstop < $hxi_start ) {
      ahlog::ah_out"HXI input file $hxifile occurs before CALDB (hxi_start)";
      ahlog::ah_out"HXI input file $hxifile will NOT be re-calibrated";
      push @error_output, "Skipped file: $hxifile (hxi_start occurs before CALDB)";
      $Params{numerrs} += 1;
      next;
    }
    if ( $datamode !~ /normal/i ) { 
      ahlog::ah_out " *** HXI input file $hxifile has invalid DATAMODE ($datamode)";
      ahlog::ah_out "HXI input file $hxifile will NOT be re-calibrated";
      push @error_output, "Skipped file: $hxifile (DATAMODE=$datamode)";
      $Params{numerrs} += 1;
      next;
    }

    ahlog::ah_out "Calibrating HXI file $hxifile";

    my $tmpfile = "";
    # Copy the infile(s) to the output file(s)
    if (copyFITSFile($hxifile,$hxifile_out)) { return 1; }

    #############  Run the specified tools  #############

    # HXISGDSFF
    ahlog::ah_out "\nRunning hxisgdsff on file $hxifile_out";
    $tmpfile = $Params{tempstem} . $basename . "_hxisgdsff.evt";
    ahapp::add_temp_file($tmpfile);
    $hxisgdsff_pars{logfile} = $Params{logstem} . $basename . "_hxisgdsff.log";
    $status = ahpllib::run_hxisgdsff($hxifile_out,$tmpfile,\%hxisgdsff_pars);
    if ( $status ) { ahlog::ah_err "hxisgdsff failed"; return $status; }
    if(copyFITSFile($tmpfile,$hxifile_out)) { return 1; }

    # HXISGDPHA
    ahlog::ah_out "\nRunning hxisgdpha on file $hxifile_out";
    $tmpfile = $Params{tempstem} . $basename . "_hxisgdpha.evt";
    ahapp::add_temp_file($tmpfile);
    $hxisgdpha_pars{logfile} = $Params{logstem} . $basename . "_hxisgdpha.log";
    $status = ahpllib::run_hxisgdpha($hxifile_out,$tmpfile,\%hxisgdpha_pars);
    if ( $status ) { ahlog::ah_err "hxisgdpha failed"; return $status; }
    if(copyFITSFile($tmpfile,$hxifile_out)) { return 1; }

    # HXIEVTID
    ahlog::ah_out "\nRunning hxievtid on file $hxifile_out";
    $hxievtid_pars{logfile} = $Params{logstem} . $basename . "_hxievtid.log";
    $status = ahpllib::run_hxievtid($hxifile_out,$hxifile_out_sffa,\%hxievtid_pars);
    if ( $status ) { ahlog::ah_err "hxievtid failed"; return $status; }

    # Set Coordevt params based on obsmode and instrument
    if( uc $obsmode eq "POINTING" ) {

      if (uc $instrume eq "HXI1") {
        $coordevt_pars{dattfile} = $dattfilehx1;
        $coordevt_pars{teldeffile} = $Params{hx1teldef};
      } else {
        $coordevt_pars{dattfile} = $dattfilehx2;
        $coordevt_pars{teldeffile} = $Params{hx2teldef};
      }

      # COORDEVT
      ahlog::ah_out "\nRunning coordevt on file $hxifile_out_sffa";
      $tmpfile = $Params{tempstem} . $basename . "_coordevt.evt";
      ahapp::add_temp_file($tmpfile);
      $coordevt_pars{logfile} = $Params{logstem} . $basename . "_coordevt.log";
      $status = ahpllib::run_coordevt($hxifile_out_sffa,$tmpfile,\%coordevt_pars);
      if ( $status ) { ahlog::ah_err "coordevt failed"; return $status; }
      if(copyFITSFile($tmpfile,$hxifile_out_sffa)) { return 1; }

    }
    ahlog::ah_out "\nAttaching GTI to file $hxifile_out_sffa";
    # Get the GTI for this file split (mode and slew)
    # We want to make sure that the first STOP is greater than TSTART of the events
    # and that the last START is less than TSTOP of the events
    my $cutgti = $Params{tempstem} . $basename . "_gticut.gti";
    ahapp::add_temp_file($cutgti);
    my $gtiext = "";
    if ( uc $obsmode eq 'SLEW' ) {
      $gtiext = "GTISLEW";
      # Attach updated GTI to SFFa
      # No GTI attached to sff, expanded, recgain
      $status = copy_hdu($obsgti,"GTISLEW][col #EXTNAME=\"GTI\"", $hxifile_out_sffa);
      if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
    } else {

      # Attach the GTI to the reconstructed event file
      if ( $indx == 0 ) {
        # Attach directly the GTIPOINT if there is no file splitting
        # Change the extension name from GTIPOINT to GTI
        $status = copy_hdu($obsgti,"GTIPOINT][col #EXTNAME=\"GTI\"", $hxifile_out_sffa);
        if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
      } else {
        # Get the GTI for this file split
        # We want to make sure that the first STOP is greater than TSTART of the events
        # and that the last START is less than TSTOP of the events
        if ( $indx == $first_index ) {
          # Bound the GTIPOINT to the tstop of the event file
          $status = copy_hdu($obsgti,"GTIPOINT][START<$tstop][col #EXTNAME=\"GTI\"", $cutgti);
          if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
        } elsif ( $indx == $last_index ) {
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
          push @error_output, "Error in file: $hxifile (Could not append GTI)";
          $Params{numerrs} += 1;
          next;
        }

        # If the indx is not the first, use the TSTART of GTIPOINT
        # Otherwise use the TSTART of the event file
        if ( $indx != $first_index ) {
          # Make sure that the first START == TSTART
          $status = ahgen::run_ftool('ftedit', "infile=$cutgti\[GTI]","column=START",
                                     "row=1","value=$tstart");
          if ($status) { ahlog::ah_err "ftedit failed."; return $status; }
          # Update TSTART and TSTOP keywords to match GTI file
          ahgen::set_keyword( $cutgti, "GTI", 'TSTART' , $tstart ) ;
        }
        
        # If the indx is not the last, use the TSTOP of GTIPOINT
        if ( $indx != $last_index ) {
          # Make sure that the last STOP == TSTOP
          $status = ahgen::run_ftool('ftedit',"infile=$cutgti\[GTI]","column=STOP",
                                     "row=$lastrow","value=$tstop");
          if ($status) { ahlog::ah_err "ftedit failed."; return $status; }
          # Update TSTART and TSTOP keywords to match GTI file
          ahgen::set_keyword( $cutgti, "GTI", 'TSTOP' , $tstop ) ;

        }

        # Attach updated GTI to SFFa
        # No GTI attached to sff, expanded, recgain
        $status = copy_hdu($cutgti,"GTI", $hxifile_out_sffa);
        if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
      }
    }

    # Update the INSTRUME, DETNAM, DATAMODE keywords in the GTI extension
    ahgen::set_keyword( $hxifile_out_sffa , "GTI", 'INSTRUME' , $instrume ) ;
    ahgen::set_keyword( $hxifile_out_sffa , "GTI", 'DETNAM' , $detnam ) ;
    ahgen::set_keyword( $hxifile_out_sffa , "GTI", 'DATAMODE' , $datamode ) ;
    
    # Set the coordinate keywords in the output HXI event file
    push( @filelist_output, $hxifile_out );
    push( @filelist_output, $hxifile_out_sffa );
    push @calibrated_hxi_uf_evt_files, $hxifile_out;
    push @calibrated_hxi_ufa_evt_files, $hxifile_out_sffa;

  } # end loop over each hxi file

  # Save lists of newly calibrated '_uf.evt'
  @{$files{event_uf}} = @calibrated_hxi_uf_evt_files;
  @{$files{event_ufa}} = @calibrated_hxi_ufa_evt_files;

  foreach my $filename ( @{$files{event_uf}} ) {
    ahlog::ah_debug "uf_file:  $filename\n";
  }

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
  my $makefilter   = $files{makefilter};
  my $ehk          = $files{extended_housekeeping};
  my $obsgti       = $files{obsgti};
  my $select       = $Params{selectfile};
  my $leapsecfile  = $Params{leapsecfile};
  my $outdir       = $Params{outdir};
  my $stemoutputs  = $Params{stemoutputs};
  my $mkflabel     = $Params{hxi_mkflabel};
  my $ehklabel     = $Params{hxi_ehklabel};
  my $evtlabel     = $Params{hxi_evtlabel};
  my @calibrated_hxi_cl_evt_files = ( );
  my @calibrated_hxi_pse_evt_files = ( );

  # Check that we have good GTI from pointing GTI
  if ( ! has_good_time( $obsgti, "GTIPOINT" ) ) {
    ahlog::ah_out "$obsgti\[GTIPOINT] has no GTI. Cannot clean HXI Data.";
    return 1;
  }
  if ( ! has_good_time( $obsgti, "GTIATT" ) ) {
    ahlog::ah_out "$obsgti\[GTIATT] has no GTI. Cannot clean HXI Data.";
    return 1;
  }

  foreach my $hxifile ( sort @{$files{event_ufa}} ) {

    my $basename      = basename($hxifile);
    $basename =~ s/\.evt.*$//;

    my $inst = "";

    my $obsmode = ahgen::get_keyword( $hxifile,"EVENTS","OBS_MODE");
    unless ( defined $obsmode ) {
      ahlog::ah_out"In file '$hxifile', could not read keyword OBS_MODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $hxifile (OBS_MODE not defined)";
      $Params{numerrs} += 1;
      next;
    }

    my $detnam = ahgen::get_keyword( $hxifile,"EVENTS","DETNAM");
    unless ( defined $detnam ) {
      ahlog::ah_out"In file '$hxifile', could not read keyword DETNAM";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $hxifile (DETNAM not defined)";
      $Params{numerrs} += 1;
      next;
    }

    my $instrume = ahgen::get_keyword($hxifile,"EVENTS","INSTRUME");
    if(ahgen::get_error_flag()) {
      ahlog::ah_err "Could not get INSTRUME keyword from $hxifile";
      push @error_output, "Skipped file: $hxifile (INSTRUME not defined)";
      $Params{numerrs} += 1;
      next;
    }

    my $datamode = ahgen::get_keyword($hxifile,"EVENTS","DATAMODE");
    if(ahgen::get_error_flag()) {
      ahlog::ah_err "Could not get DATAMODE keyword from $hxifile";
      push @error_output, "Skipped file: $hxifile (DATAMODE not defined)";
      $Params{numerrs} += 1;
      next;
    }

    # Skip any slew event files
    if ( uc $obsmode eq "SLEW" ) { next; }
    if ( $datamode !~ /normal/i ) { 
      ahlog::ah_out " *** SGD input file $hxifile has invalid DATAMODE ($datamode)";
      ahlog::ah_out "SGD input file $hxifile will NOT be re-calibrated";
      push @error_output, "Skipped file: $hxifile (DATAMODE=$datamode)";
      $Params{numerrs} += 1;
      next;
    }

    my $gtitel_ext = ahgen::get_total_hdu($telgti);
    $gtitel_ext++;
    my $hdu_counter = 1; # Start with HDU2 or Rather the first extension
    my $gti_extension = 0;

    while ($hdu_counter < $gtitel_ext) {

      my $gti_detnam = ahgen::get_keyword( $telgti,"$hdu_counter","DETNAM");
      unless ( defined $gti_detnam ) {
        ahlog::ah_out"In file '$telgti', could not read keyword DETNAM for HDU Exention No. $hdu_counter";
        push @error_output, "Skipped file: $hxifile (DETNAM not defined in $telgti\[$hdu_counter])";
        $Params{numerrs} += 1;
        next;
      }
    
      my $gti_instrume = ahgen::get_keyword($telgti,"$hdu_counter","INSTRUME");
      if(ahgen::get_error_flag()) {
        ahlog::ah_err "Could not get INSTRUME keyword from $telgti for HDU Exention No. $hdu_counter";
        push @error_output, "Skipped file: $hxifile (INSTRUME not defined in $telgti\[$hdu_counter])";
        return 1;
      }

      if ((uc $gti_detnam eq uc $detnam) and (uc $gti_instrume eq uc $instrume)){
        $gti_extension = $hdu_counter;
        $hdu_counter = $gtitel_ext;
      } else {
        $hdu_counter++;
      }

    }

    if ($gti_extension == 0){
      ahlog::ah_err "Could not match INSTRUME and DETNAM keywords for $hxifile and $telgti. Skipping file.";
      push @error_output, "Skipped file: $hxifile (No GTITEL found)";
      $Params{numerrs} += 1;
      next;
    }

    # Check that we have good GTI from telemetry saturation GTI
    if ( ! has_good_time( $telgti, "$gti_extension" ) ) {
      ahlog::ah_out "$telgti\[$gti_extension] has no GTI. Cannot clean HXI Data. Skipping file.";
      push @error_output, "Skipped file: $hxifile (No GTITEL found)";
      $Params{numerrs} += 1;
      next;
    }

    # Verify that we have good GTI
    if ( ! has_good_time( $hxifile, "GTI" ) ) {
      ahlog::ah_out "$hxifile\[GTI] has no GTI. Cannot clean HXI Data.";
      push @error_output, "Skipped file: $hxifile (No GTI found)";
      $Params{numerrs} += 1;
      next;
    }

    if ( uc $instrume eq "HXI1" ) {
      $inst = "hx1";
    } else {
      $inst = "hx2";
    }

    foreach my $type ( qw( clean pseudo ) ) {

      my $hxifile_out   = form_cleaned_file_name($hxifile);
      my $mkflabel_type = $mkflabel;
      my $ehklabel_type = $ehklabel;
      my $evtlabel_type = $evtlabel;
      my $cpkeywords = "DATAMODE, DEC_NOM, DETNAM, INSTRUME, MJDREFI, MJDREFF, OBS_ID, PA_NOM, RA_NOM, TELESCOP";

      if ($type eq "pseudo") {$hxifile_out =~ s/rec_/recpse_/g;}

      my @ingtis        = ();

      # Change the labels if we are cleaning for pseudo events
      if ($type eq "pseudo") { 
        $mkflabel_type = $mkflabel;
        $mkflabel_type =~ s/CAM/PSE/;
        $ehklabel_type = $ehklabel;
        $ehklabel_type =~ s/CAM/PSE/;
        $evtlabel_type = $evtlabel;
        $evtlabel_type =~ s/CAM/PSE/;
      }

      my $gtimkf        = $Params{tempstem} . "$inst\_" . lc $mkflabel_type . "_mkf.gti";
      my $gtiehk        = $Params{tempstem} . "$inst\_" . lc $ehklabel_type . "_ehk.gti";
      my $logfile = "";

      ahapp::add_temp_file ( $gtimkf );
      ahapp::add_temp_file ( $gtiehk );

      ahlog::ah_debug "Input File       : $hxifile";
      ahlog::ah_debug "Output File      : $hxifile_out";
      ahlog::ah_debug "MKF Label        : $mkflabel_type";
      ahlog::ah_debug "EHK Label        : $ehklabel_type";
      ahlog::ah_debug "Event Label      : $evtlabel_type";

      if ( ! -e $gtimkf ) { 
        # Create the MKF GTI file
        $logfile = $Params{logstem} . "$inst\_" . lc $mkflabel_type . "_mkf_ahgtigen.log";
        $status = create_gti(infile       => $makefilter,
                             outfile      => $gtimkf,
                             selectfile   => $select,
                             instrume     => $instrume,
                             label        => $mkflabel_type,
                             cpkeyword    => "NONE",
                             upkeyword    => "yes",
                             clobber      => $ahapp::clobber ? "yes" : "no" ,
                             chatter      => $ahapp::chatter,
                             logfile     => $logfile,
                             );
        if ( $status ) { 
          if ( $status == 2 ) {
            ahlog::ah_out "$gtimkf has no GTI. Skipping $hxifile_out.";
            push @error_output, "Skipped file: $hxifile_out (No GTIMKF found)";
          } else {
            ahlog::ah_err "ahgtigen failed. Skipping $hxifile_out.";
            push @error_output, "Skipped file: $hxifile_out (ahgtigen failed)";
          }
          $Params{numerrs} += 1;
          next;
        }
        if ( ! has_good_time( $gtimkf, "GTI" ) ) {
          ahlog::ah_out "$gtimkf has no GTI. Skipping $hxifile_out.";
          push @error_output, "Skipped file: $hxifile_out (No GTIMKF found)";
          $Params{numerrs} += 1;
          next;
        }

        # Rename MKF GTI extension and append to ufa file
        ahgen::set_keyword($gtimkf,"GTI","EXTNAME","GTIMKF");
        ahfilterlib::copy_keywords($hxifile,"EVENTS",$gtimkf,"GTIMKF","",$cpkeywords);
      }
      if ($type eq "clean")  {
        if (0 != ahgen::check_hdu_exists($hxifile,"GTIMKF")) { ahgen::delete_hdu($hxifile,"GTIMKF"); }
        ahgen::copy_hdu($gtimkf,"GTIMKF",$hxifile);
      }

      if ($type eq "clean")  {
        if ( ! -e $gtiehk ) {
          # Create the EHK GTI file
          $logfile = $Params{logstem} . "$inst\_" . lc $ehklabel_type . "_ehk_ahgtigen.log";
          $status = create_gti(infile       => $ehk,
                               outfile      => $gtiehk,
                               selectfile   => $select,
                               instrume     => $instrume,
                               label        => $ehklabel_type,
                               cpkeyword    => "NONE",
                               upkeyword    => "yes",
                               clobber      => $ahapp::clobber ? "yes" : "no" ,
                               chatter      => $ahapp::chatter,
                               logfile     => $logfile,
                               );
          if ( $status ) { 
            if ( $status == 2 ) {
              ahlog::ah_out "$gtiehk has no GTI. Skipping $hxifile_out.";
              push @error_output, "Skipped file: $hxifile_out (No GTIEHK found)";
            } else {
              ahlog::ah_err "ahgtigen failed. Skipping $hxifile_out.";
              push @error_output, "Skipped file: $hxifile_out (ahgtigen failed)";
            }
            $Params{numerrs} += 1;
            next;
          }
          if ( ! has_good_time( $gtiehk, "GTI" ) ) {
            ahlog::ah_out "$gtiehk has no GTI. Skipping $hxifile_out.";
            push @error_output, "Skipped file: $hxifile_out (No GTIEHK found)";
            $Params{numerrs} += 1;
            next;
          }

          # Rename EHK GTI extension and append to ufa file
          ahgen::set_keyword($gtiehk,"GTI","EXTNAME","GTIEHK");
          ahfilterlib::copy_keywords($hxifile,"EVENTS",$gtiehk,"GTIEHK","",$cpkeywords);
        }
        if (0 != ahgen::check_hdu_exists($hxifile,"GTIEHK")) { ahgen::delete_hdu($hxifile,"GTIEHK"); }
        ahgen::copy_hdu($gtiehk,"GTIEHK",$hxifile);
      }

      if ( $type eq "clean" ) {
        ahlog::ah_out "\nCleaning events in $hxifile";
      } else {
        ahlog::ah_out "\nCleaning pseudo events in $hxifile";
      }

      # Load the GTI files into parameter
      push @ingtis, $hxifile  . "[GTI]";
      push @ingtis, $obsgti   . "[GTIPOINT]";
      push @ingtis, $obsgti   . "[GTIATT]";
      push @ingtis, $telgti   . "[$gti_extension]";
      push @ingtis, $gtimkf   . "[GTIMKF]";
      if ($type eq "clean")  {push @ingtis, $gtiehk   . "[GTIEHK]";}

      # Merge the GTI and screen events
      $logfile = $Params{logstem} . $basename . "_ahscreen.log";
      $status = screen_events(infile      => $hxifile,
                              outfile     => $hxifile_out,
                              gtifile     => join(",",@ingtis),
                              upkeyword   => "yes",
                              cpkeyword   => "yes",
                              leapsecfile => $leapsecfile,
                              selectfile  => $select,
                              label       => $evtlabel_type,
                              clobber     => "yes",
                              chatter     => $ahapp::chatter,
                              logfile     => $logfile,
                              );
      if ( $status ) { 
        if ( $status == 2 ) {
          ahlog::ah_info "HIGH", "ahscreen: merged GTI produced no time interval.";
          ahlog::ah_info "HIGH", "Skipping $hxifile_out.";
          push @error_output, "Skipped file: $hxifile_out (merged GTI produced no time interval)";
        } elsif ( $status == 3 ) {
          ahlog::ah_info "HIGH", "ahscreen: Filtered all events during event screening.";
          ahlog::ah_info "HIGH", "Skipping $hxifile_out.";
          push @error_output, "Skipped file: $hxifile_out (Filtered all events during event screening)";
        } else {
          ahlog::ah_err "ahscreen failed. Skipping $hxifile_out.";
          push @error_output, "Skipped file: $hxifile_out (ahscreen failed)";
        }
        unlink $hxifile_out;
        $Params{numerrs} += 1;
        next;
      }

      # Set the coordinate keywords in the output HXI event file
      push( @filelist_output, $hxifile_out );

      if ($type eq "clean") {
        push @calibrated_hxi_cl_evt_files, $hxifile_out;
      } else {
        push @calibrated_hxi_pse_evt_files, $hxifile_out;
      }

    } # end loop over labels
  } # end loop over unfiltered files

  # Save lists of newly calibrated '_uf.evt'
  @{$files{event_cl}} = @calibrated_hxi_cl_evt_files;
  @{$files{event_pse}} = @calibrated_hxi_pse_evt_files;

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
  my $instrument = "hxi";

  foreach my $hxifile ( sort @{$files{event_cl}} ) {

    # Calculate the output file names
    my $pha = $hxifile;
    $pha =~ s/_cl.evt/_src.pha/;
    my $img = $hxifile;
    $img =~ s/_cl.evt/_src.img/;
    my $lc = $hxifile;
    $lc =~ s/_cl.evt/_src.lc/;

    # Run the extractor to create spectra, lightcurves and images
    $status = extract(infile  => $hxifile,
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
    ahlog::ah_debug "$inst event file(s) calibrated:" if (@{$files{event_uf}} or @{$files{event_ufa}});
    foreach my $filename ( @{$files{event_uf}} ) {
      ahlog::ah_debug "  $filename\n";
    }
    foreach my $filename ( @{$files{event_ufa}} ) {
      ahlog::ah_debug "  $filename\n";
    }
  }
  if ( $Params{stage2_switch} ) {
    ahlog::ah_debug "$inst file(s) screened:" if (@{$files{event_cl}} or @{$files{event_pse}});
    foreach my $filename ( @{$files{event_cl}} ) {
      ahlog::ah_debug "  $filename\n";
    }
    foreach my $filename ( @{$files{event_pse}} ) {
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

    # Loop on HDUs
    if ( $infile =~ /hx1/ ) {
      for ( my $i1 = 0 ; $i1 < $hdutotal ; $i1++ ) {
        add_coord_keys($infile,$i1,"hx1")
      }
    } elsif ( $infile =~ /hx2/ ) {
      for ( my $i1 = 0 ; $i1 < $hdutotal ; $i1++ ) {
        add_coord_keys($infile,$i1,"hx2")
      }
    }

    ## Update the CHECKSUM and DATASUM keywords
    $status = update_checksum_and_verify($infile);
    unless ( $status ) { ahlog::ah_err "verify failed for $infile"; return $status; }
  }


  ahgen::run_ftool("pset","hxipipeline","numerrs=$Params{numerrs}");
  # Print the errors found from the pipeline
  if ( $Params{numerrs} ) {
    ahlog::ah_out "HXIPIPELINE had the following errors/warnings:";
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
  $outfile =~ s/_ufa/_cl/g;

  return $outfile ;

}

# ------------------------------------------------------------------------------

sub find_file_inst {

  my $evtfile = shift;

  my ($inst) = $evtfile =~ /(hx[12])_[ps][0-9]cam_uf\.evt$zpatt$/;

  return $inst;

}

# ------------------------------------------------------------------------------

sub find_file_mode {

  my $evtfile = shift;

  my ($mode) = $evtfile =~ /hx[12]_([ps])[0-9]cam_uf\.evt$zpatt$/;

  return $mode;

}

# ------------------------------------------------------------------------------

sub find_file_indx {

  my $evtfile = shift;

  my ($indx) = $evtfile =~ /hx[12]_[ps]([0-9])cam_uf\.evt$zpatt$/;

  return $indx;

}

# ------------------------------------------------------------------------------

sub add_coord_keys {

  my $infile = shift;
  my $ext = shift;
  my $inst = shift;

  my $ra = $Params{ra};
  my $dec = $Params{dec};
  my $roll = $Params{roll};
  my $ra_pnt  = $Params{$inst . "_ra_pnt"};
  my $dec_pnt = $Params{$inst . "_dec_pnt"};
  my $optdetx = $Params{$inst . "_optdetx"};
  my $optdety = $Params{$inst . "_optdety"};
  my $optfocx = $Params{$inst . "_optfocx"};
  my $optfocy = $Params{$inst . "_optfocy"};
  my $optskyx = $Params{$inst . "_optskyx"};
  my $optskyy = $Params{$inst . "_optskyy"};

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

sub CheckInstrument {

  my $instrument_set = 0;

  for my $instrument_key ( keys %instrument ) {
    $instrument{$instrument_key} = 0;
  }

  # Split the instrument parameter on ,

  $Params{instrument} =~ s/\s+//g;
  foreach my $instrument_value ( split( ',', uc( $Params{instrument} ) ) ) {

    unless ( $instrument_value =~ /^(HXI|HXI1|HXI2)$/i ) {
      ahlog::ah_err "Invalid instrument : $instrument_value" ;
      return 1;
    }

    if ( $instrument_value eq "HXI" ) {
      $instrument{hx1} = 1;
      $instrument{hx2} = 1;
      $instrument_set  = 1;
    } elsif ( $instrument_value eq "HXI1" ) {
      $instrument{hx1} = 1;
      $instrument_set  = 1;
    } elsif ( $instrument_value eq "HXI2" ) {
      $instrument{hx2} = 1;
      $instrument_set  = 1;
    }
  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nParams_instrument = $Params{instrument}\n\n";
    ahlog::ah_debug "instrument_set  = $instrument_set\n\n";
    ahlog::ah_debug "instrument_hxi1 = $instrument{hx1}\n";
    ahlog::ah_debug "instrument_hxi2 = $instrument{hx2}\n";
  }

  if ( $instrument_set == 0 ) {
    ahlog::ah_err "No valid instrument found in instrument parameter: '$Params{instrument}'" ;
    return 1;
  }

  return 0;
}

# ------------------------------------------------------------------------------

sub create_delta_attitude {

  my $filename = shift;
  my $tstart = shift;
  my $tstop  = shift;
  my $inst   = shift;
  my $status = 0;

  # Create the header template to a file.
  my($header_template) = $filename . '_header_template.tmp';
  open HEADER, ">$header_template";
  print HEADER "TIME               1D s\n";
  print HEADER "QPARAM             4D";
  close HEADER;

  # Create the keyword template to a file.
  my($keyword_template) = $filename . '_keyword_template.tmp';
  open KEYWORD, ">$keyword_template";
  print KEYWORD "TELESCOP HITOMI / Mission or satellite name\n";
  print KEYWORD "INSTRUME $inst / Instrument name\n";
  print KEYWORD "ORIGSYS RAW / Originating coordinate system\n";
  print KEYWORD "DESTSYS ACT / Destination coordinate system";
  close KEYWORD;

  # Print the data template to a file.
  my($data_template) = $filename . '_data_template.tmp';
  open DATA, ">$data_template";
  print DATA "$tstart 0 0 0 1\n";
  print DATA "$tstop 0 0 0 1";
  close DATA;

  # Delete any existing file of the same name.
  unlink $filename;

  # Run fcreate to create the file - destroy it explicitly to be
  # sure the parfile is not hanging around later when we look for
  # leftover files.
  $status = ahgen::run_ftool('ftcreate',
                             "cdfile=$header_template",
                             "datafile=$data_template",
                             "outfile=$filename",
                             "headfile=$keyword_template",
                             "tabtyp=binary",
                             "nskip=0",
                             "nrows=0",
                             "morehdr=0",
                             "extname=ATTITUDE",
                             "anull= ",
                             "inull=0",
                             "history=no",
                             "clobber=yes"
                            );
  if ($status) {
    ahlog::ah_err "Could not create ATTITUDE file from scratch.";
    return $status;
  }

  # Clean up the temporary files.
  unlink $data_template;
  unlink $keyword_template;
  unlink $header_template;

  return;

}

# ------------------------------------------------------------------------------
