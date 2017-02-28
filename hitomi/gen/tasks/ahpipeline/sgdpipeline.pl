#!/usr/bin/perl
#
# File name: sgdpipeline.pl
# Author: J. D. Wysk NASA GSFC
# $Date: 2016/11/28 14:55:18 $
# Version: 0
#
# Calibrate sgd data
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
# 1) +++ 2016-02-02 JW: GTIEHK consistently produces no GTI events, which causes the file the skip ahscreen.
#
# 2) +++ 2016-02-04 JW: When running stage 3 (with stage 2 off), if the cleaned files are taken from the input directory, the output files are also deposited in the input dir instead of the outdir.
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
  instrument           => "", # "Which instrument (SGD1, SGD2, or SGD {Both})"    

  sgd_start            => 0,   # "SGD CALDB start time"
  verify_input         => 0,  # Verify the input files before processing? 

  ra                   => -999.99999, # RA of nominal pointing [deg]
  dec                  => -999.99999, # Dec of nominal pointing [deg]
  roll                 => 0, # Roll of nominal pointing [deg]

  sg1_optfocx              => -999.99999, # SGD1 optical focx coordinate
  sg1_optfocy              => -999.99999, # SGD1 optical focy coordinate
  sg1_optskyx              => -999.99999, # SGD1 optical detx coordinate
  sg1_optskyy              => -999.99999, # SGD1 optical dety coordinate
  sg2_optfocx              => -999.99999, # SGD2 optical focx coordinate
  sg2_optfocy              => -999.99999, # SGD2 optical focy coordinate
  sg2_optskyx              => -999.99999, # SGD2 optical detx coordinate
  sg2_optskyy              => -999.99999, # SGD2 optical dety coordinate
  sg1_ra_pnt               => -999.99999, # SGD1 RA pointing [deg]
  sg1_dec_pnt              => -999.99999, # SGD1 Dec pointing [deg]
  sg2_ra_pnt               => -999.99999, # SGD2 RA pointing [deg]
  sg2_dec_pnt              => -999.99999, # SGD2 Dec pointing [deg]
 
  extended_housekeeping=> "",  # "Extended housekeeping file"
  makefilter           => "",  # "Makefilter file, comma seperated list for multiple files"
  obsgti              => "",  # "Time file, comma seperated list for multiple files"  

  #############  CALDB Parameters  #############

  remapfile          => "",   # "hxisgdsff/sgdevtid: remapping file"    
  gainfile           => "CALDB",    # "hxisgdpha: PHA calibration functions"
  badpixfile         => "CALDB",    # "hxisgdpha/sgdevtid: readout channels "
  fluorefile         => "",   # "sgdevtid: Input fluorescence file "
  probseqfile        => "CALDB",  # "sgdevtid: sequence probability file"
  probfovfile        => "CALDB",  # "sgdevtid: FOV probability file"

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

  # sgdevtid pars 
  rejectbgo          => "yes", 
  skipreco           => "no", 
  outtracefile       => "NONE", 
  numsignal          => 48, 
  d10          => 3.2, 
  d1a1a          => 5.0, 
  d1a1b              => 5.0, 
  d1a2           => 14.0, 
  d1a3               => 5.0, 
  a              => 3.0, 
  b              => 3.0, 
  probaccept2        => 0.1, 
  probaccept3        => 0.1, 
  probaccept4        => 0.1, 
  distz              => 1000000.0, 
  paraoffset0        => 1.6, 
  paraoffset1        => 1.0, 
  paraoffset2        => 1.0, 
  weight0            => 1.0, 
  weight1            => 0.0, 
  weight2            => 0.0, 
  weight3            => 0.0, 
  delgmethod         => "ANALYTIC", 
);

our %instrument = (     # "Which instrument (SGD1, SGD2, or Both)"    
  # +++ 2014-12-18 AS: CAMS?
  sg1                   => "",  # SGD SGD1
  sg2                   => "",   # SGD SGD2
);

our %files = (
  extended_housekeeping => "",
  makefilter            => "",
  obsgti               => "",
  selectfile            => "",
  leapsecfile           => "",
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
  sg1_event_uf          => qr/(sg1_[ps][0-9]cc[0-9]_uf\.evt)$zpatt$/,
  sg2_event_uf          => qr/(sg2_[ps][0-9]cc[0-9]_uf\.evt)$zpatt$/,
  sg1_event_ufa         => qr/(sg1_[ps][0-9]cc[0-9]rec_ufa\.evt)$zpatt$/,
  sg2_event_ufa         => qr/(sg2_[ps][0-9]cc[0-9]rec_ufa\.evt)$zpatt$/,
  sg1_event_exp         => qr/(sg1_[ps][0-9]cc[0-9]exp_ufa\.evt)$zpatt$/,
  sg2_event_exp         => qr/(sg2_[ps][0-9]cc[0-9]exp_ufa\.evt)$zpatt$/,
  sg1_event_cl          => qr/(sg1_[ps][0-9]cc[0-9]rec_cl\.evt)$zpatt$/,
  sg2_event_cl          => qr/(sg2_[ps][0-9]cc[0-9]rec_cl\.evt)$zpatt$/,
  sgd_telgti            => qr/(sgd_tel\.gti)$zpatt$/,
);

my $pipelineerror = 0;  # sgdpipeline exit status

our @filelist_output;
our @error_output;

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

# Reset the numerrs parameters
$Params{numerrs} = 0;
ahgen::run_ftool("pset","sgdpipeline","numerrs=0");

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

$pipelineerror = CheckInstrument();
unless ( $pipelineerror == 0 ) {
  $Params{numerrs} += 1;
  ahlog::ah_err "CheckInstrument" ;
  push @error_output, "CheckInstrument failed";
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

# we're done.
ahapp::end_processing($pipelineerror);

#########################
# subroutines
#########################


# ------------------------------------------------------------------------------

sub get_parameters {

  # run tool parameters
  # these are hidden, so we can check these first
  # input file pars

  # required parameters
  $Params{indir}        = ahapp::query_parameter("indir");
  $Params{outdir}       = ahapp::query_parameter("outdir");
  $Params{steminputs}   = ahapp::query_parameter("steminputs");
  $Params{stemoutputs}  = ahapp::query_parameter("stemoutputs");
  $Params{instrument}   = ahapp::query_parameter("instrument");
  $Params{entry_stage}  = ahapp::query_parameter("entry_stage");
  $Params{exit_stage}   = ahapp::query_parameter("exit_stage");

  $Params{sgd_start}    = ahapp::query_parameter("sgd_start");

  $Params{makefilter}   = ahapp::query_parameter("makefilter");
  $Params{extended_housekeeping} = ahapp::query_parameter("extended_housekeeping");
  $Params{obsgti}      = ahapp::query_parameter("obsgti");
  $Params{selectfile}        = ahapp::query_parameter("selectfile");
  $Params{leapsecfile}       = ahapp::query_parameter("leapsecfile");

  # before we get the rest of the parameters, check the entry
  # and exit stages
  if (CheckEntryExitStage($Params{entry_stage},$Params{exit_stage})) { return 1; }

  # set up the entry and exit stage flags
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

  $Params{sgd_mkflabel}      = ahapp::query_parameter("sgd_mkflabel");
  $Params{sgd_ehklabel}      = ahapp::query_parameter("sgd_ehklabel");
  $Params{sgd_evtlabel}      = ahapp::query_parameter("sgd_evtlabel");

  if ( $Params{stage1_switch} ) {
    # If we aren't calibrating events, finding tool parameters
    # can significantly slow down the pipeline script

    # CALDB files
    $Params{remapfile}          = ahapp::query_parameter("remapfile");
    $Params{gainfile}           = ahapp::query_parameter("gainfile");   
    $Params{badpixfile}         = ahapp::query_parameter("badpixfile"); 
    $Params{fluorefile}         = ahapp::query_parameter("fluorefile"); 
    $Params{probseqfile}        = ahapp::query_parameter("probseqfile");
    $Params{probfovfile}        = ahapp::query_parameter("probfovfile");
 
    # Coordinate parameters
    $Params{ra}           = ahapp::query_parameter("ra");
    $Params{dec}          = ahapp::query_parameter("dec");
    $Params{roll}         = ahapp::query_parameter("roll");
    $Params{sg1_optfocx}      = ahapp::query_parameter("sg1_optfocx");
    $Params{sg1_optfocy}      = ahapp::query_parameter("sg1_optfocy");
    $Params{sg1_optskyx}      = ahapp::query_parameter("sg1_optskyx");
    $Params{sg1_optskyy}      = ahapp::query_parameter("sg1_optskyy");
    $Params{sg2_optfocx}      = ahapp::query_parameter("sg2_optfocx");
    $Params{sg2_optfocy}      = ahapp::query_parameter("sg2_optfocy");
    $Params{sg2_optskyx}      = ahapp::query_parameter("sg2_optskyx");
    $Params{sg2_optskyy}      = ahapp::query_parameter("sg2_optskyy");
    $Params{sg1_ra_pnt}       = ahapp::query_parameter("sg1_ra_pnt");
    $Params{sg1_dec_pnt}      = ahapp::query_parameter("sg1_dec_pnt");
    $Params{sg2_ra_pnt}       = ahapp::query_parameter("sg2_ra_pnt");
    $Params{sg2_dec_pnt}      = ahapp::query_parameter("sg2_dec_pnt");

    # Shared pars
    $Params{occurrenceid}       = ahapp::query_parameter("occurrenceid");
    $Params{randomize}          = ahapp::query_parameter("randomize");
    $Params{seed}               = ahapp::query_parameter("seed");

    #############  Parameters for specified tools  #############

    # hxisgdsff pars 
    # NONE

    # hxisgdpha pars 
    $Params{outnsubcol}      = ahapp::query_parameter("outnsubcol");
    $Params{datamode}        = ahapp::query_parameter("datamode");

    # sgdevtid pars 
    $Params{rejectbgo}       = ahapp::query_parameter("rejectbgo");
    $Params{skipreco}        = ahapp::query_parameter("skipreco");
    $Params{outtracefile}    = ahapp::query_parameter("outtracefile");
    $Params{numsignal}       = ahapp::query_parameter("numsignal");
    $Params{d10}             = ahapp::query_parameter("d10");
    $Params{d1a1a}           = ahapp::query_parameter("d1a1a");
    $Params{d1a1b}           = ahapp::query_parameter("d1a1b");
    $Params{d1a2}            = ahapp::query_parameter("d1a2");
    $Params{d1a3}            = ahapp::query_parameter("d1a3");
    $Params{a}               = ahapp::query_parameter("a");
    $Params{b}               = ahapp::query_parameter("b");
    $Params{probaccept2}     = ahapp::query_parameter("probaccept2");
    $Params{probaccept3}     = ahapp::query_parameter("probaccept3");
    $Params{probaccept4}     = ahapp::query_parameter("probaccept4");
    $Params{distz}           = ahapp::query_parameter("distz");
    $Params{paraoffset0}     = ahapp::query_parameter("paraoffset0");
    $Params{paraoffset1}     = ahapp::query_parameter("paraoffset1");
    $Params{paraoffset2}     = ahapp::query_parameter("paraoffset2");
    $Params{weight0}         = ahapp::query_parameter("weight0");
    $Params{weight1}         = ahapp::query_parameter("weight1");
    $Params{weight2}         = ahapp::query_parameter("weight2");
    $Params{weight3}         = ahapp::query_parameter("weight3");
    $Params{delgmethod}      = ahapp::query_parameter("delgmethod");

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
    $Params{tempstem} = "sgdpipeline_" . $taskstart . "_tmp_";
    $Params{logstem} = "sgdpipeline_" . $taskstart . "_";
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
  my $makefilter   = $Params{makefilter};
  my $extended_housekeeping = $Params{extended_housekeeping};
  my $obsgti      = $Params{obsgti};
  my $remapfile    = $Params{remapfile};
  my $gainfile     = $Params{gainfile};
  my $badpixfile   = $Params{badpixfile};
  my $fluorefile   = $Params{fluorefile};
  my $probseqfile  = $Params{probseqfile};
  my $probfovfile  = $Params{probfovfile};

  # Check the input and output directories
  if (CheckInputDirectory($indir,$outdir)) { return 1; };
  if (CheckOutputDirectory($outdir)) { return 1; };

  # Find and store the list of input files
  if ($inst eq "SGD" || $inst eq "SGD1"){ 
    push( @{$files{event_uf}}, FindInputFiles($indir,$steminputs , $patterns{sg1_event_uf}));
    push( @{$files{event_ufa}}, FindInputFiles($indir,$steminputs , $patterns{sg1_event_ufa}));
    push( @{$files{event_exp}}, FindInputFiles($indir,$steminputs , $patterns{sg1_event_exp}));
    push( @{$files{event_cl}}, FindInputFiles($indir,$steminputs , $patterns{sg1_event_cl}));
  }

  if ($inst eq "SGD" || $inst eq "SGD2"){ 
    push( @{$files{event_uf}}, FindInputFiles($indir,$steminputs , $patterns{sg2_event_uf}));
    push( @{$files{event_ufa}}, FindInputFiles($indir,$steminputs , $patterns{sg2_event_ufa}));
    push( @{$files{event_exp}}, FindInputFiles($indir,$steminputs , $patterns{sg2_event_exp}));
    push( @{$files{event_cl}}, FindInputFiles($indir,$steminputs , $patterns{sg2_event_cl}));
  }

  ($files{telgti})    = FindInputFiles($indir,$steminputs , $patterns{sgd_telgti});

  # Check the input CALDB files
  if ( $Params{stage1_switch} ) {
    # Check for file requirements
    if (isRequiredFileNotFound($obsgti)) { return 1; }

    # Store the required input files
    $files{obsgti}        = $obsgti;

    # Check the CALDB files
    if (isBadCALDBFileParameterValue($Params{remapfile},"remapfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{gainfile},"gainfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{badpixfile},"badpixfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{fluorefile},"fluorefile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{probseqfile},"proseqfile")) { return 1;}
    if (isBadCALDBFileParameterValue($Params{probfovfile},"probfovfile")) { return 1;}
  }


  if ( $Params{stage2_switch} ) {
    # Check that we have a valid label for SGD
    # If not, skip SGD cleaning 
    if ( $Params{sgd_mkflabel} !~ /#/ or 
         $Params{sgd_ehklabel} !~ /#/ or 
         $Params{sgd_evtlabel} !~ /#/ ) {
      ahlog::ah_err " *** Not a valid label for SGD. Label requires '#'.";
      return 1;
    }
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
  # Check whether we are calculating SGD GTI from the exposure
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
    ahlog::ah_debug "GTI file                   = $files{obsgti}\n" if $files{obsgti};
    ahlog::ah_debug "Extended housekeeping file = $files{extended_housekeeping}\n" if $files{extended_housekeeping};
    ahlog::ah_debug "Makefilter file            = $files{makefilter}\n" if $files{makefilter};
    ahlog::ah_debug "Telemetry Saturation GTI   = $files{telgti}\n" if $files{telgti};
    ahlog::ah_debug "$inst unfiltered file(s) found:" if (@{$files{event_uf}} or @{$files{event_ufa}} or @{$files{event_exp}});
    foreach my $filename ( @{$files{event_uf}} ) {
      ahlog::ah_debug "event_uf:  $filename\n";
    }
    foreach my $filename ( @{$files{event_ufa}} ) {
      ahlog::ah_debug "event_ufa:  $filename\n";
    }
    foreach my $filename ( @{$files{event_exp}} ) {
      ahlog::ah_debug "event_exp:  $filename\n";
    }
    ahlog::ah_debug "$inst cleaned file(s) found:" if @{$files{event_cl}};
    foreach my $filename ( @{$files{event_cl}} ) {
      ahlog::ah_debug "event_cl  $filename\n";
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
  my @calibrated_sgd_uf_evt_files = ( ); 
  my @calibrated_sgd_ufa_evt_files = ( ); 
  my @calibrated_sgd_exp_evt_files = ( ); 
  my $outdir       = $Params{outdir};
  my $obsgti       = $Params{obsgti};
  my $stemoutputs  = $Params{stemoutputs};
  my $instrument   = $Params{instrument};

  #############  setup  parameters for each tool #############

  my %hxisgdsff_pars = (
    remapfile      => $Params{remapfile},
    clobber        => $ahapp::clobber ? "yes" : "no" ,
    chatter        => $ahapp::chatter,
  );


  my %hxisgdpha_pars = (
    gainfile       => $Params{gainfile},  
    badpixfile     => $Params{badpixfile},
    outnsubcol     => $Params{outnsubcol},
    randomize      => $Params{randomize},
    seed           => $Params{seed},     
    datamode       => $Params{datamode},
    clobber        => $ahapp::clobber ? "yes" : "no" ,
    chatter        => $ahapp::chatter,
  );

  my %sgdevtid_pars = (
    remapfile      => $Params{remapfile},
    fluorefile     => $Params{fluorefile},
    badpixfile     => $Params{badpixfile},
    probseqfile    => $Params{probseqfile},
    probfovfile    => $Params{probfovfile},
    occurrenceid   => $Params{occurrenceid},
    rejectbgo      => $Params{rejectbgo},
    skipreco       => $Params{skipreco},
    outtracefile   => $Params{outtracefile},  # +++ 2015-01-05 AS: Possible ahpipeline parameter, not required
    numsignal      => $Params{numsignal},
    d10            => $Params{d10},
    d1a1a          => $Params{d1a1a},
    d1a1b          => $Params{d1a1b},
    d1a2           => $Params{d1a2},
    d1a3           => $Params{d1a3},
    a              => $Params{a},
    b              => $Params{b},
    probaccept2    => $Params{probaccept2},
    probaccept3    => $Params{probaccept3},
    probaccept4    => $Params{probaccept4},
    distz          => $Params{distz},
    paraoffset0    => $Params{paraoffset0},
    paraoffset1    => $Params{paraoffset1},
    paraoffset2    => $Params{paraoffset2},
    weight0        => $Params{weight0},
    weight1        => $Params{weight1},
    weight2        => $Params{weight2},
    weight3        => $Params{weight3},
    delgmethod     => $Params{delgmethod},
    seed           => $Params{seed},
    clobber        => $ahapp::clobber ? "yes" : "no" ,  
    chatter        => $ahapp::chatter,
  );


  # Find the first and the last SGD1/2 files per compton camera
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

  # Find the first and last indx for SGD1
  foreach my $evtfile ( @{$files{event_uf}} ) {
    my $inst = find_file_inst($evtfile);
    my $mode = find_file_mode($evtfile);
    my $indx = find_file_indx($evtfile);
    my $detnam = find_file_detnam($evtfile);
    # Skip any slew file
    if ( lc $mode eq "s" ) { next; }

    if ( $indx == 0 ) {
      # If the indx is 0, then there is no file split
      # We can ignore the first and last indx
      $first_indx{"$inst\_$detnam"}=0;
      $last_indx{"$inst\_$detnam"}=0;
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
    unless ( $tstart_prev{"$inst\_$detnam"} ) { $tstart_prev{"$inst\_$detnam"} = $tstart; $first_indx{"$inst\_$detnam"} = $indx;}
    unless ( $tstop_prev{"$inst\_$detnam"} ) { $tstop_prev{"$inst\_$detnam"} = $tstop; $last_indx{"$inst\_$detnam"} = $indx; }
    if ( $tstart < $tstart_prev{"$inst\_$detnam"} ) { $tstart_prev{"$inst\_$detnam"} = $tstart; $first_indx{"$inst\_$detnam"} = $indx; }
    if ( $tstop > $tstop_prev{"$inst\_$detnam"} ) { $tstop_prev{"$inst\_$detnam"} = $tstop; $last_indx{"$inst\_$detnam"} = $indx; }
  }

  ############# Calibrate each sgd file #############
  foreach my $sgdfile ( sort @{$files{event_uf}} ) {

    #############  setup input/output files #############

    my $status = 0;
    my $sgd_start    = $Params{sgd_start};    

    # Set up input and output files
    my $sgdfile_out   = form_outfile_name($sgdfile);
    my $sgdfile_out_sffa = $sgdfile_out;
    $sgdfile_out_sffa =~ s/_uf.evt/rec_ufa.evt/g;

    my $basename      = basename($sgdfile);
    $basename =~ s/\.evt.*$//;

    # Determine the file index for this hxi file
    my $inst = find_file_inst($sgdfile_out);
    my $indx = find_file_indx($sgdfile_out);
    my $detnam = find_file_detnam($sgdfile_out);

    # Determine the first and last indexes for this instrument
    my $first_index = $first_indx{"$inst\_$detnam"};
    my $last_index = $last_indx{"$inst\_$detnam"};

    # Skip the file if there are no events
    my $naxis2 = ahgen::get_keyword($sgdfile,"EVENTS","NAXIS2");
    unless ( defined $naxis2 ) {
      ahlog::ah_out"In file '$sgdfile', could not read keyword NAXIS2";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sgdfile (NAXIS2 not defined)";
      $Params{numerrs} += 1;
      next;
    }
    unless ( $naxis2 ) { 
      ahlog::ah_out"No events found in file '$sgdfile'";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sgdfile (NAXIS2=0)";
      $Params{numerrs} += 1;
      next;
    }

    # read keywords TSTART, TSTOP, DATE-OBS, and OBSMODE
    my $tstart = ahgen::get_keyword($sgdfile,"EVENTS","TSTART");
    unless ( defined $tstart ) {
      ahlog::ah_out"In file '$sgdfile', could not read keyword TSTART";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sgdfile (TSTART not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $tstop = ahgen::get_keyword($sgdfile,"EVENTS","TSTOP");
    unless ( defined $tstop ) {
      ahlog::ah_out"In file '$sgdfile', could not read keyword TSTOP";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sgdfile (TSTOP not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $dateobs = ahgen::get_keyword($sgdfile,"EVENTS","DATE-OBS");
    unless ( defined $dateobs ) {
      ahlog::ah_out"In file '$sgdfile', could not read keyword DATE-OBS";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sgdfile (DATE-OBS not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $obsmode = ahgen::get_keyword($sgdfile,"EVENTS","OBS_MODE");
    unless ( defined $obsmode ) {
      ahlog::ah_out"In file '$sgdfile', could not read keyword OBS_MODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sgdfile (OBS_MODE not defined)";
      $Params{numerrs} += 1;
      next;
    }
    # Read the instrument keyword use it to set coordevt pars below 
    my $instrume = ahgen::get_keyword($sgdfile,"EVENTS","INSTRUME");
    if(ahgen::get_error_flag()) {
      ahlog::ah_err "Could not get INSTRUME keyword from $sgdfile";
      push @error_output, "Skipped file: $sgdfile (INSTRUME not defined)";
      $Params{numerrs} += 1;
      next;
    }
    my $datamode = ahgen::get_keyword($sgdfile,"EVENTS","DATAMODE");
    unless ( defined $datamode ) {
      ahlog::ah_out"In file '$sgdfile', could not read keyword DATAMODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sgdfile (DATAMODE not defined)";
      $Params{numerrs} += 1;
      next;
    }

    ahlog::ah_debug "Input File       : $sgdfile";
    ahlog::ah_debug "Output File      : $sgdfile_out";
    ahlog::ah_debug "tstart           : $tstart";
    ahlog::ah_debug "tstop            : $tstop";
    ahlog::ah_debug "dateobs:         : $dateobs";
    ahlog::ah_debug "obsmode:         : $obsmode";
    ahlog::ah_debug "datamode:        : $datamode";

    # can this file ($sgdfile) be processed with the caldb files?
    if ( $tstop < $sgd_start ) {
      ahlog::ah_out"SGD input file $sgdfile occurs before CALDB (sgd_start)";
      ahlog::ah_out"SGD input file $sgdfile will NOT be re-calibrated";
      push @error_output, "Skipped file: $sgdfile (sgd_start occurs before CALDB)";
      $Params{numerrs} += 1;
      next;
    }
    if ( $datamode !~ /normal/i ) { 
      ahlog::ah_out " *** SGD input file $sgdfile has invalid DATAMODE ($datamode)";
      ahlog::ah_out "SGD input file $sgdfile will NOT be re-calibrated";
      push @error_output, "Skipped file: $sgdfile (DATAMODE=$datamode)";
      $Params{numerrs} += 1;
      next;
    }

    ahlog::ah_out "Calibrating SGD file $sgdfile";

    my $tmpfile = "";
    # Copy the infile(s) to the output file(s)
    if (copyFITSFile($sgdfile,$sgdfile_out)) { return 1; }

    #############  Run the specified tools  #############

    # HXISGDSFF
    ahlog::ah_out "\nRunning hxisgdsff on file $sgdfile_out";
    $tmpfile = $Params{tempstem} . $basename . "_hxisgdsff.evt";
    ahapp::add_temp_file($tmpfile);
    $hxisgdsff_pars{logfile} = $Params{logstem} . $basename . "_hxisgdsff.log";
    $status = ahpllib::run_hxisgdsff($sgdfile_out,$tmpfile,\%hxisgdsff_pars);
    if ( $status ) { ahlog::ah_err "hxisgdsff failed"; return $status; }
    if(copyFITSFile($tmpfile,$sgdfile_out)) { return 1; }

    # HXISGDPHA
    ahlog::ah_out "\nRunning hxisgdpha on file $sgdfile_out";
    $tmpfile = $Params{tempstem} . $basename . "_hxisgdpha.evt";
    ahapp::add_temp_file($tmpfile);
    $hxisgdpha_pars{logfile} = $Params{logstem} . $basename . "_hxisgdpha.log";
    $status = ahpllib::run_hxisgdpha($sgdfile_out,$tmpfile,\%hxisgdpha_pars);
    if ( $status ) { ahlog::ah_err "hxisgdpha failed"; return $status; }
    if(copyFITSFile($tmpfile,$sgdfile_out)) { return 1; }

    # SGDEVTID
    ahlog::ah_out "\nRunning sgdevtid on file $sgdfile_out";
    $sgdevtid_pars{logfile} = $Params{logstem} . $basename . "_sgdevtid.log";
    $status = ahpllib::run_sgdevtid($sgdfile_out,$sgdfile_out_sffa,\%sgdevtid_pars);
    if ( $status ) { ahlog::ah_err "sgdevtid failed"; return $status; }

    ahlog::ah_out "\nAttaching GTI to file $sgdfile_out_sffa";
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
      $status = copy_hdu($obsgti,"GTISLEW][col #EXTNAME=\"GTI\"", $sgdfile_out_sffa);
      if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
    } else {

      # Attach the GTI to the reconstructed event file
      if ( $indx == 0 ) {
        # Attach directly the GTIPOINT if there is no file splitting
        # Change the extension name from GTIPOINT to GTI
        $status = copy_hdu($obsgti,"GTIPOINT][col #EXTNAME=\"GTI\"", $sgdfile_out_sffa);
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
          push @error_output, "Error in file: $sgdfile (Could not append GTI)";
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
        $status = copy_hdu($cutgti,"GTI", $sgdfile_out_sffa);
        if ($status) { ahlog::ah_err "ftcopy failed."; return $status; }
      }
    }

    # Update the INSTRUME, DETNAM, DATAMODE keywords in the GTI extension
    ahgen::set_keyword( $sgdfile_out_sffa , "GTI", 'INSTRUME' , $instrume ) ;
    ahgen::set_keyword( $sgdfile_out_sffa , "GTI", 'DETNAM' , $detnam ) ;
    ahgen::set_keyword( $sgdfile_out_sffa , "GTI", 'DATAMODE' , $datamode ) ;
    
    push( @filelist_output, $sgdfile_out );
    push( @filelist_output, $sgdfile_out_sffa );
    push @calibrated_sgd_uf_evt_files, $sgdfile_out;
    push @calibrated_sgd_ufa_evt_files, $sgdfile_out_sffa;

  } # end loop over each sgd file

  # Save lists of newly calibrated '_uf.evt'
  @{$files{event_uf}} = @calibrated_sgd_uf_evt_files;
  @{$files{event_ufa}} = @calibrated_sgd_ufa_evt_files;

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
  my $indir        = $Params{indir};
  my $outdir       = $Params{outdir};
  my $steminputs   = $Params{steminputs};
  my $stemoutputs  = $Params{stemoutputs};

  my $mkflabel     = $Params{sgd_mkflabel};
  my $ehklabel     = $Params{sgd_ehklabel};
  my $evtlabel     = $Params{sgd_evtlabel};

  my @calibrated_sgd_cl_evt_files = ( );
  my @calibrated_sgd_pse_evt_files = ( );

  # Check that we have good GTI from pointing GTI
  if ( ! has_good_time( $obsgti, "GTIPOINT" ) ) {
    ahlog::ah_out "$obsgti\[GTIPOINT] has no GTI. Cannot clean HXI Data.";
    return 1;
  }
  if ( ! has_good_time( $obsgti, "GTIATT" ) ) {
    ahlog::ah_out "$obsgti\[GTIATT] has no GTI. Cannot clean HXI Data.";
    return 1;
  }

  foreach my $sgdfile ( sort @{$files{event_ufa}} ) {

    my $basename      = basename($sgdfile);
    $basename =~ s/\.evt.*$//;

    my $inst = "";

    my $obsmode = ahgen::get_keyword( $sgdfile,"EVENTS","OBS_MODE");
    unless ( defined $obsmode ) {
      ahlog::ah_out"In file '$sgdfile', could not read keyword OBS_MODE";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sgdfile (OBS_MODE not defined)";
      $Params{numerrs} += 1;
      next;
    }

    my $detnam = ahgen::get_keyword( $sgdfile,"EVENTS","DETNAM");
    unless ( defined $detnam ) {
      ahlog::ah_out"In file '$sgdfile', could not read keyword DETNAM";
      ahlog::ah_out"Skipping file.";
      push @error_output, "Skipped file: $sgdfile (DETNAM not defined)";
      $Params{numerrs} += 1;
      next;
    }

    my $instrume = ahgen::get_keyword($sgdfile,"EVENTS","INSTRUME");
    if(ahgen::get_error_flag()) {
      ahlog::ah_err "Could not get INSTRUME keyword from $sgdfile";
      push @error_output, "Skipped file: $sgdfile (INSTRUME not defined)";
      $Params{numerrs} += 1;
      next;
    }

    my $datamode = ahgen::get_keyword($sgdfile,"EVENTS","DATAMODE");
    if(ahgen::get_error_flag()) {
      ahlog::ah_err "Could not get DATAMODE keyword from $sgdfile";
      push @error_output, "Skipped file: $sgdfile (DATAMODE not defined)";
      $Params{numerrs} += 1;
      next;
    }

    # Skip any slew event files
    if ( uc $obsmode eq "SLEW" ) { next; }
    if ( $datamode !~ /normal/i ) { 
      ahlog::ah_out " *** SGD input file $sgdfile has invalid DATAMODE ($datamode)";
      ahlog::ah_out "SGD input file $sgdfile will NOT be re-calibrated";
      push @error_output, "Skipped file: $sgdfile (DATAMODE=$datamode)";
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
        push @error_output, "Skipped file: $sgdfile (DETNAM not defined in $telgti\[$hdu_counter])";
        $Params{numerrs} += 1;
        next;
      }
    
      my $gti_instrume = ahgen::get_keyword($telgti,"$hdu_counter","INSTRUME");
      if(ahgen::get_error_flag()) {
        ahlog::ah_err "Could not get INSTRUME keyword from $telgti for HDU Exention No. $hdu_counter";
        push @error_output, "Skipped file: $sgdfile (INSTRUME not defined in $telgti\[$hdu_counter])";
        $Params{numerrs} += 1;
        next;
      }

      if ((uc $gti_detnam eq uc $detnam) and (uc $gti_instrume eq uc $instrume)){
        $gti_extension = $hdu_counter;
        $hdu_counter = $gtitel_ext;
      } else {
  $hdu_counter++;
      }

    }

    if ($gti_extension == 0){
      ahlog::ah_out "Could not match INSTRUME and DETNAM keywords for $sgdfile and $telgti. Skipping file.";
      push @error_output, "Skipped file: $sgdfile (No GTITEL found)";
      $Params{numerrs} += 1;
      next;
    }

    # Check that we have good GTI from telemetry saturation GTI
    if ( ! has_good_time( $telgti, "$gti_extension" ) ) {
      ahlog::ah_out "$telgti\[$gti_extension] has no GTI. Cannot clean SGD Data.";
      push @error_output, "Skipped file: $sgdfile (No GTITEL found)";
      $Params{numerrs} += 1;
      next;
    }

    # Verify that we have good GTI
    if ( ! has_good_time( $sgdfile, "GTI" ) ) {
      ahlog::ah_out "$sgdfile\[GTI] has no GTI. Cannot clean SGD Data.";
      push @error_output, "Skipped file: $sgdfile (No GTI found)";
      $Params{numerrs} += 1;
      next;
    }
 

    if ( uc $instrume eq "SGD1" ) {
      $inst = "sg1";
    } else {
      $inst = "sg2";
    }

    foreach my $type ( qw( clean pseudo ) ) {

      my $sgdfile_out   = form_cleaned_file_name($sgdfile);
      my $mkflabel_type = $mkflabel;
      my $ehklabel_type = $ehklabel;
      my $evtlabel_type = $evtlabel;
      $mkflabel_type =~ s/#/$detnam/;
      $ehklabel_type =~ s/#/$detnam/;
      $evtlabel_type =~ s/#/$detnam/;
      my $cpkeywords = "DATAMODE, DEC_NOM, DETNAM, INSTRUME, MJDREFI, MJDREFF, OBS_ID, PA_NOM, RA_NOM, TELESCOP";

      if ($type eq "pseudo") {$sgdfile_out =~ s/rec_/recpse_/g;}

      my @ingtis        = ();

      if ($type eq "pseudo") { 
        $mkflabel_type .= "PSE";
        $ehklabel_type .= "PSE";
        $evtlabel_type .= "PSE";
      }

      my $gtimkf        = $Params{tempstem} . "$inst\_" . lc $mkflabel_type . "_mkf.gti";
      my $gtiehk        = $Params{tempstem} . "$inst\_" . lc $ehklabel_type . "_ehk.gti";
      my $logfile = "";
 
      ahapp::add_temp_file ( $gtimkf );
      ahapp::add_temp_file ( $gtiehk );

      ahlog::ah_debug "Input File       : $sgdfile";
      ahlog::ah_debug "Output File      : $sgdfile_out";
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
                             clobber => $ahapp::clobber ? "yes" : "no" ,
                             chatter => $ahapp::chatter,
                             logfile     => $logfile,
                             );
        if ( $status ) { 
          if ( $status == 2 ) {
            ahlog::ah_out "$gtimkf has no GTI. Skipping $sgdfile_out.";
            push @error_output, "Skipped file: $sgdfile_out (No GTIMKF found)";
          } else {
            ahlog::ah_err "ahgtigen failed. Skipping $sgdfile_out.";
            push @error_output, "Skipped file: $sgdfile_out (ahgtigen failed)";
          }
          $Params{numerrs} += 1;
          next;
        }
        if ( ! has_good_time( $gtimkf, "GTI" ) ) {
          ahlog::ah_out "$gtimkf has no GTI. Skipping $sgdfile_out.";
          push @error_output, "Skipped file: $sgdfile_out (No GTIMKF found)";
          $Params{numerrs} += 1;
          next;
        }

        # Rename MKF GTI extension and append to ufa file
        ahgen::set_keyword($gtimkf,"GTI","EXTNAME","GTIMKF");
        ahfilterlib::copy_keywords($sgdfile,"EVENTS",$gtimkf,"GTIMKF","",$cpkeywords);
      }
      if ($type eq "clean")  {
        if (0 != ahgen::check_hdu_exists($sgdfile,"GTIMKF")) { ahgen::delete_hdu($sgdfile,"GTIMKF"); }
        ahgen::copy_hdu($gtimkf,"GTIMKF",$sgdfile);
      }

      if ($type eq "clean") {
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
                               clobber => $ahapp::clobber ? "yes" : "no" ,
                               chatter => $ahapp::chatter,
                               logfile     => $logfile,
                               );
          if ( $status ) { 
            if ( $status == 2 ) {
              ahlog::ah_out "$gtiehk has no GTI. Skipping $sgdfile_out.";
              push @error_output, "Skipped file: $sgdfile_out (No GTIMKF found)";
            } else {
              ahlog::ah_err "ahgtigen failed. Skipping $sgdfile_out.";
              push @error_output, "Skipped file: $sgdfile_out (ahgtigen failed)";
            }
            $Params{numerrs} += 1;
            next;
          }
          if ( ! has_good_time( $gtiehk, "GTI" ) ) {
            ahlog::ah_out "$gtiehk has no GTI. Skipping $sgdfile_out.";
            push @error_output, "Skipped file: $sgdfile_out (No GTIMKF found)";
            $Params{numerrs} += 1;
            next;
          }

          # Rename EHK GTI extension and append to ufa file
          ahgen::set_keyword($gtiehk,"GTI","EXTNAME","GTIEHK");
          ahfilterlib::copy_keywords($sgdfile,"EVENTS",$gtiehk,"GTIEHK","",$cpkeywords);
        }
        if (0 != ahgen::check_hdu_exists($sgdfile,"GTIEHK")) { ahgen::delete_hdu($sgdfile,"GTIEHK"); }
        ahgen::copy_hdu($gtiehk,"GTIEHK",$sgdfile);
      }

      # Load the GTI files into parameter
      push @ingtis, $sgdfile  . "[GTI]";
      push @ingtis, $obsgti   . "[GTIPOINT]";
      push @ingtis, $obsgti   . "[GTIATT]";
      push @ingtis, $telgti   . "[$gti_extension]";
      push @ingtis, $gtimkf   . "[GTIMKF]";
      if ($type eq "clean")  {push @ingtis, $gtiehk   . "[GTIEHK]";}

      if ( $type eq "clean" ) {
        ahlog::ah_out "\nCleaning events in $sgdfile";
      } else {
        ahlog::ah_out "\nCleaning pseudo events in $sgdfile";
      }
      # Merge the GTI and screen events
      $logfile = $Params{logstem} . $basename . "_ahscreen.log";
      $status = screen_events(infile      => $sgdfile,
                              outfile     => $sgdfile_out,
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
          ahlog::ah_info "HIGH", "Skipping $sgdfile_out.";
          push @error_output, "Skipped file: $sgdfile_out (merged GTI produced no time interval)";
        } elsif ( $status == 3 ) {
          ahlog::ah_info "HIGH", "ahscreen: Filtered all events during event screening.";
          ahlog::ah_info "HIGH", "Skipping $sgdfile_out.";
          push @error_output, "Skipped file: $sgdfile_out (Filtered all events during event screening)";
        } else {
          ahlog::ah_err "ahscreen failed. Skipping $sgdfile_out.";
          push @error_output, "Skipped file: $sgdfile_out (ahscreen failed)";
        }
        unlink $sgdfile_out;
        $Params{numerrs} += 1;
        next;
      }

      # Set the coordinate keywords in the output SGD event file
      push( @filelist_output, $sgdfile_out );

      if ($type eq "clean") {
        push @calibrated_sgd_cl_evt_files, $sgdfile_out;
      } else {
        push @calibrated_sgd_pse_evt_files, $sgdfile_out;
      }

    } # end loop over labels
  } # end loop over unfiltered files

  # Save lists of newly calibrated '_uf.evt'
  @{$files{event_cl}} = @calibrated_sgd_cl_evt_files;
  @{$files{event_pse}} = @calibrated_sgd_pse_evt_files;

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
  # +++ 2016-01-26 JW: This is somewhat crude, but should work to ensure the instrument name matches the file being processed.
  my $instrument = "sgd";

  foreach my $sgdfile ( sort @{$files{event_cl}} ) {

    # Calculate the output file names
    my $pha = $sgdfile;
    $pha =~ s/_cl.evt/_src.pha/;
    my $img = $sgdfile;
    $img =~ s/_cl.evt/_src.img/;
    my $lc = $sgdfile;
    $lc =~ s/_cl.evt/_src.lc/;

    # Run the extractor to create spectra, lightcurves and images
    $status = extract(infile  => $sgdfile,
          phafile => $pha,
          lcfile  => $lc,
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
    if ( $infile =~ /sg1/ ) {
      for ( my $i1 = 0 ; $i1 < $hdutotal ; $i1++ ) {
        add_coord_keys($infile,$i1,"sg1")
      }
    } elsif ( $infile =~ /sg2/ ) {
      for ( my $i1 = 0 ; $i1 < $hdutotal ; $i1++ ) {
        add_coord_keys($infile,$i1,"sg2")
      }
    }

    ## Update the CHECKSUM and DATASUM keywords
    $status = update_checksum_and_verify($infile);
    unless ( $status ) { ahlog::ah_err "verify failed for $infile"; return $status; }
  }


  ahgen::run_ftool("pset","sgdpipeline","numerrs=$Params{numerrs}");

  # Print the errors found from the pipeline
  if ( $Params{numerrs} ) {
    ahlog::ah_out "SGDPIPELINE had the following errors/warnings:";
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

  my ($inst) = $evtfile =~ /(sg[12])_[ps][0-9]cc[0-9]_uf\.evt$zpatt$/;

  return $inst;

}

# ------------------------------------------------------------------------------

sub find_file_mode {

  my $evtfile = shift;

  my ($mode) = $evtfile =~ /sg[12]_([ps])[0-9]cc[0-9]_uf\.evt$zpatt$/;

  return $mode;

}

# ------------------------------------------------------------------------------

sub find_file_indx {

  my $evtfile = shift;

  my ($indx) = $evtfile =~ /sg[12]_[ps]([0-9])cc[0-9]_uf\.evt$zpatt$/;

  return $indx;

}

# ------------------------------------------------------------------------------

sub find_file_detnam {

  my $evtfile = shift;

  my ($detnam) = $evtfile =~ /sg[12]_[ps][0-9](cc[0-9])_uf\.evt$zpatt$/;

  return $detnam;

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

    unless ( $instrument_value =~ /^(SGD|SGD1|SGD2)$/i ) {
      ahlog::ah_err "Invalid instrument : $instrument_value" ;
      return 1;
    }

    if ( $instrument_value eq "SGD" ) {
      $instrument{sg1} = 1;
      $instrument{sg2} = 1;
      $instrument_set  = 1;
    } elsif ( $instrument_value eq "SGD1" ) {
      $instrument{sg1} = 1;
      $instrument_set  = 1;
    } elsif ( $instrument_value eq "SGD2" ) {
      $instrument{sg2} = 1;
      $instrument_set  = 1;
    }
  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nParams_instrument = $Params{instrument}\n\n";
    ahlog::ah_debug "instrument_set  = $instrument_set\n\n";
    ahlog::ah_debug "instrument_sgd1 = $instrument{sg1}\n";
    ahlog::ah_debug "instrument_sgd2 = $instrument{sg2}\n";
  }

  if ( $instrument_set == 0 ) {
    ahlog::ah_err "No valid instrument found in instrument parameter: '$Params{instrument}'" ;
    return 1;
  }

  return 0;
}

# ------------------------------------------------------------------------------
