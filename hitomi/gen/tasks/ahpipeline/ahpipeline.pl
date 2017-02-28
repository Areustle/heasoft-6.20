#!/usr/bin/perl
#-------------------------------------------------------------------------------
#
# File name: ahpipeline
#  
# Task name: ahpipeline
#       
# Description:
#
# Script to perform HITOMI Data Reduction.
# Duplicates functionality of HITOMI Pipeline
#
# Author/Date: A. J. Sargent NASA GSFC / 1 July 2014
#
# History:
#
# Single (procedural) program to duplicate the HITOMI "ah1" pipeline processing.
#

#-----------------------------------------------------------

# Set up

use strict;

use ahlog ;
use ahgen qw( copyFITSFile );
use ahapp ;
use ahfilterlib ;
use ahpllib qw( has_good_time form_output_file_name );

use File::Find;
use File::Spec::Functions;
use File::Basename;
use File::Copy;
use File::Path;
use Cwd 'abs_path';
use POSIX qw(strftime floor ceil);

use HEACORE::HEAUTILS;
use HEACORE::HEAINIT;
use Astro::FITS::CFITSIO qw( :longnames :constants );
use HEAGen::HEAGen qw( :all );

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

our $pipelineerror = 0;

my $force_debug ;

# Set the force_debug flag to non-zero (preferably 1) to force ouput of debugging
# messages.
#$force_debug = 1;
$force_debug = '' ;

our %General = (
  stage1_switch         => 0,
  stage2_switch         => 0,
  stage3_switch         => 0,
  reportname            => "ahpipeline_report.txt",
  reportstore           => "",
  reportflag            => 0,
  reportoutput          => 0,
  reportcurdur          => 0,
  reportfilehandle      => undef,
  telescop              => "HITOMI",
  tempstem              => "",
  logstem               => "",
  stemreport            => ""
);

our %Task = (
  datetime              => `date`,
  start                 => "",
  name                  => "ahpipeline",
  version               => "1.0",
  releasedate           => "2014-07-01",
  stem                  => "ahpipeline_1.0",
  emptystem             => "                 ",
);

our %Errors = (

  'hxi' => {
    stage1           => 0, 
    stage2           => 0,
    stage3           => 0,
  },

  'sgd' => {
    stage1           => 0, 
    stage2           => 0,
    stage3           => 0,
  },

  'sxi' => {
    stage1           => 0, 
    stage2           => 0,
    stage3           => 0,
  },

  'sxs' => {
    stage1           => 0, 
    stage2           => 0,
    stage3           => 0,
  },


);
our %Params = (
  indir                => "",  # Input Directory
  outdir               => "",  # Output Directory
  steminputs           => "",  # Stem for input files name
  stemoutputs          => "",  # Stem for output files name
  entry_stage          => 0, # Entry stage of processing (1,2 or 3)
  exit_stage           => 0, # Exit  stage of processing (1,2 or 3)
  instrument           => "",  # Which instrument
  numerrs              => 0, # Total number of errors from all pipelines

  verify_input         => 0, # Verify the input files before processing? 

  attitude             => "",  # Attitude file
  housekeeping         => "",  # Housekeeping file
  extended_housekeeping=> "",  # Extended housekeeping file
  extended_housekeeping2=> "",  # Extended housekeeping2 file
  makefilter           => "",  # Makefilter file, comma seperated list for multiple files
  orbit                => "",  # Orbit file, comma seperated list for multiple files
  timfile              => "",  # Time file, comma seperated list for multiple files  

  # timing parameters
  sxi_start            => 0, # SXI CALDB start time
  hxi_start            => 0, # HXI CALDB start time
  sgd_start            => 0, # SGD CALDB start time
  sxs_start            => 0, # SXS CALDB start time

  # sxs parameters
  linetocorrect        => "",
  calmethod            => "",
  usemp                => "",

  # shared parameters
  randomize            => "",
  seed                 => 0,
  cleanup              => 0,  # Remove temp files?

  coordevt             => {},
  hxi                  => {},
  sgd                  => {},
  sxi                  => {},
  sxs                  => {},

);

our %caldb_files = (

  'gen' => {
    leapsecfile      => "",  #
    selectfile       => "",
    mkfconf          => "",
  },

  'sxi' => {
    teldeffile        => "",  # TelDef File for SXI
    badpixfile        => "",  #
    maskfile          => "",  # 
    vtevnodd          => "",  # 
    ctifile           => "",  # 
    chtrailfile       => "",  # 
    spthfile          => "",  # 
    gainfile          => "",  # 
    patternfile       => "",  # 
  },

  'hxi' => {
    remapfile         => "",  #
    fluorefile        => "",  #
    badpixfile        => "",  #
    enecutfile        => "",  #
    gainfile          => "",  #
  },

  'hx1' => {
    teldeffile        => "",  # TelDef File for HXI1
  },

  'hx2' => {
    teldeffile        => "",  # TelDef File for HXI2
  },

  'cm1' => {
    teldeffile        => "",  # TelDef File for CAMS1
  },

  'cm2' => {
    teldeffile        => "",  # TelDef File for CAMS1
  },

  'cms' => {
    camstempxy        => "",
  },

  'sg1' => {
    teldeffile        => "",  # TelDef File for SGD1
  },

  'sg2' => {
    teldeffile        => "",  # TelDef File for SGD2
  },

  'sgd' => {
    remapfile         => "",  #
    fluorefile        => "",  #
    badpixfile        => "",  #
    gainfile          => "",  #
    probseqfile       => "",  #
    probfovfile       => "",  #
  },

  'sxs' => {
    teldeffile        => "",  # TelDef File for SXS 
    coeftime          => "",  #
    pixdeffile        => "",  #
    gainfile          => "",  #
    linefitfile       => "",  #
    gainantfile       => "",  #
    delayfile        => "",  #
  },

);

our %instrument = (
  sxi                   => "",  # SXI
  sxs                   => "",  # SXS
  hx1                   => "",  # HXI HXI1
  hx2                   => "",  # HXI HXI2
  sg1                   => "",  # SGD SGD1
  sg2                   => ""   # SGD SGD2
);

our %coordinates = (

  'gen' => {
    ra    => -999.99999,
    dec   => -999.99999,
    roll  => 0,
  },

  'hx1' => {
    optdetx   => -999.99999,
    optdety   => -999.99999,
    optfocx   => -999.99999,
    optfocy   => -999.99999,
    optskyx   => -999.99999,
    optskyy   => -999.99999,
    ra_pnt    => -999.99999,
    dec_pnt   => -999.99999,
  },

  'hx2' => {
    optdetx   => -999.99999,
    optdety   => -999.99999,
    optfocx   => -999.99999,
    optfocy   => -999.99999,
    optskyx   => -999.99999,
    optskyy   => -999.99999,
    ra_pnt    => -999.99999,
    dec_pnt   => -999.99999,
  },

  'sg1' => {
    optfocx   => -999.99999,
    optfocy   => -999.99999,
    optskyx   => -999.99999,
    optskyy   => -999.99999,
    ra_pnt    => -999.99999,
    dec_pnt   => -999.99999,
  },

  'sg2' => {
    optfocx   => -999.99999,
    optfocy   => -999.99999,
    optskyx   => -999.99999,
    optskyy   => -999.99999,
    ra_pnt    => -999.99999,
    dec_pnt   => -999.99999,
  },

  'sxi' => {
    optdetx   => -999.99999,
    optdety   => -999.99999,
    optfocx   => -999.99999,
    optfocy   => -999.99999,
    optskyx   => -999.99999,
    optskyy   => -999.99999,
    ra_pnt    => -999.99999,
    dec_pnt   => -999.99999,
  },

  'sxs' => {
    optdetx   => -999.99999,
    optdety   => -999.99999,
    optfocx   => -999.99999,
    optfocy   => -999.99999,
    optskyx   => -999.99999,
    optskyy   => -999.99999,
    ra_pnt    => -999.99999,
    dec_pnt   => -999.99999,
  },

);

our %files = (
  attitude              => "",
  housekeeping          => "",
  extended_housekeeping => "",
  extended_housekeeping2 => "",
  makefilter            => "",
  orbit                 => "",
  timfile               => "",
  regionfile            => "",
  hx1_event_uf          => [],
  hx2_event_uf          => [],
  hx1_event_rec         => [],
  hx2_event_rec         => [],
  hx1_event_exp         => [],
  hx2_event_exp         => [],
  hx1_event_cl          => [],
  hx2_event_cl          => [],
  hx1_event_pse         => [],
  hx2_event_pse         => [],
  hx1_hk                => "",
  hx2_hk                => "",
  hxi_gtitel            => "",
  hx1_lc                => [],
  hx2_lc                => [],
  hx1_pha               => [],
  hx2_pha               => [],
  hx1_img               => [],
  hx2_img               => [],
  sg1_event_uf          => [],
  sg2_event_uf          => [],
  sg1_event_rec         => [],
  sg2_event_rec         => [],
  sg1_event_exp         => [],
  sg2_event_exp         => [],
  sg1_event_cl          => [],
  sg2_event_cl          => [],
  sg1_event_pse         => [],
  sg2_event_pse         => [],
  sg1_hk                => "", 
  sg2_hk                => "", 
  sgd_gtitel            => "",
  sg1_lc                => [],
  sg2_lc                => [],
  sg1_pha               => [],
  sg2_pha               => [],
  sxi_event_uf          => [],
  sxi_event_cl          => [],
  sxi_hk                => "",
  sxi_gtitel            => "",
  sxi_exposure          => "",
  sxi_seggti            => "",
  sxi_modegti           => "",
  sxi_hpix              => [],
  sxi_fpix              => [],
  sxi_lc                => [],
  sxi_pha               => [],
  sxi_img               => [],
  sxs_event_uf          => [],
  sxs_event_cl          => [],
  sxs_hk                => "",
  sxs_gtitel            => "",
  sxs_gtilost           => "",
  sxs_gtimxfn           => "",
  sxs_gtimxcs           => "",
  sxs_antico            => "",
  sxs_lc                => [],
  sxs_pha               => [],
  sxs_img               => [],
);

our $zpatt = qr/(\.Z|\.z|\.gzip|\.GZIP|\.gz|\.GZ|\.zip\.ZIP)?/;
our %patterns = (
  attitude              => qr/(\.att)$zpatt$/,
  housekeeping          => qr/(gen_a0\.hk)$zpatt$/,
  extended_housekeeping => qr/(\.ehk)$zpatt$/,
  extended_housekeeping2=> qr/(\.ehk2)$zpatt$/,
  makefilter            => qr/(\.mkf)$zpatt$/,
  orbit                 => qr/(\.orb)$zpatt$/,
  timfile               => qr/(\.tim)$zpatt$/,
  obsgti                => qr/(_gen\.gti)$zpatt$/,
  hx1_event_uf          => qr/(hx1_[ps][0-9]cam_uf\.evt)$zpatt$/,
  hx2_event_uf          => qr/(hx2_[ps][0-9]cam_uf\.evt)$zpatt$/,
  hx1_event_rec         => qr/(hx1_[ps][0-9]camrec_ufa\.evt)$zpatt$/,
  hx2_event_rec         => qr/(hx2_[ps][0-9]camrec_ufa\.evt)$zpatt$/,
  hx1_event_exp         => qr/(hx1_[psa][0-9]camexp_ufa\.evt)$zpatt$/,
  hx2_event_exp         => qr/(hx2_[psa][0-9]camexp_ufa\.evt)$zpatt$/,
  hx1_event_cl          => qr/(hx1_[ps][0-9]camrec_cl\.evt)$zpatt$/,
  hx2_event_cl          => qr/(hx2_[ps][0-9]camrec_cl\.evt)$zpatt$/,
  hx1_event_pse         => qr/(hx1_[ps][0-9]camrecpse_cl\.evt)$zpatt$/,
  hx2_event_pse         => qr/(hx2_[ps][0-9]camrecpse_cl\.evt)$zpatt$/,
  hx1_lc                => qr/(hx1_[ps][0-9]camrec_src\.lc)$zpatt$/,
  hx2_lc                => qr/(hx2_[ps][0-9]camrec_src\.lc)$zpatt$/,
  hx1_pha               => qr/(hx1_[ps][0-9]camrec_src\.pha)$zpatt$/,
  hx2_pha               => qr/(hx2_[ps][0-9]camrec_src\.pha)$zpatt$/,
  hx1_img               => qr/(hx1_[ps][0-9]camrec_src\.img)$zpatt$/,
  hx2_img               => qr/(hx2_[ps][0-9]camrec_src\.img)$zpatt$/,
  hx1_hk                => qr/(hx1_a0\.hk)$zpatt$/,
  hx2_hk                => qr/(hx2_a0\.hk)$zpatt$/,
  hxi_gtitel            => qr/(hxi_tel\.gti)$zpatt$/,
  sg1_event_uf          => qr/(sg1_[ps][0-9]cc[0-9]_uf\.evt)$zpatt$/,
  sg2_event_uf          => qr/(sg2_[ps][0-9]cc[0-9]_uf\.evt)$zpatt$/,
  sg1_event_rec         => qr/(sg1_[ps][0-9]cc[0-9]rec_ufa\.evt)$zpatt$/,
  sg2_event_rec         => qr/(sg2_[ps][0-9]cc[0-9]rec_ufa\.evt)$zpatt$/,
  sg1_event_exp         => qr/(sg1_[psa][0-9]cc[0-9]exp_ufa\.evt)$zpatt$/,
  sg2_event_exp         => qr/(sg2_[psa][0-9]cc[0-9]exp_ufa\.evt)$zpatt$/,
  sg1_event_cl          => qr/(sg1_[ps][0-9]cc[0-9]rec_cl\.evt)$zpatt$/,
  sg2_event_cl          => qr/(sg2_[ps][0-9]cc[0-9]rec_cl\.evt)$zpatt$/,
  sg1_event_pse         => qr/(sg1_[ps][0-9]cc[0-9]recpse_cl\.evt)$zpatt$/,
  sg2_event_pse         => qr/(sg2_[ps][0-9]cc[0-9]recpse_cl\.evt)$zpatt$/,
  sg1_lc                => qr/(sg1_[ps][0-9]cc[0-9]rec_src\.lc)$zpatt$/,
  sg2_lc                => qr/(sg2_[ps][0-9]cc[0-9]rec_src\.lc)$zpatt$/,
  sg1_pha               => qr/(sg1_[ps][0-9]cc[0-9]rec_src\.pha)$zpatt$/,
  sg2_pha               => qr/(sg2_[ps][0-9]cc[0-9]rec_src\.pha)$zpatt$/,
  sg1_hk                => qr/(sg1_a0\.hk)$zpatt$/,
  sg2_hk                => qr/(sg2_a0\.hk)$zpatt$/,
  sgd_gtitel            => qr/(sgd_tel\.gti)$zpatt$/,
  sxi_event_uf          => qr/(sxi_[ps][0-9][0-9a-f]{8}_uf\.evt)$zpatt$/,
  sxi_event_cl          => qr/(sxi_[ps][0-9][0-9a-f]{8}_cl\.evt)$zpatt$/,
  sxi_lc                => qr/(sxi_[ps][0-9][0-9a-f]{8}_src\.lc)$zpatt$/,
  sxi_pha               => qr/(sxi_[ps][0-9][0-9a-f]{8}_src\.pha)$zpatt$/,
  sxi_img               => qr/(sxi_[ps][0-9][0-9a-f]{8}_src\.img)$zpatt$/,
  sxi_hk                => qr/(sxi_a0\.hk)$zpatt$/,
  sxi_exposure          => qr/(sxi_a0exp\.fits)$zpatt$/,
  sxi_gtitel            => qr/(sxi_tel\.gti)$zpatt$/,
  sxi_modegti           => qr/(sxi_mode\.gti)$zpatt$/,
  sxi_seggti            => qr/(sxi_seg\.gti)$zpatt$/,
  sxi_hpix              => qr/(sxi_a0[0-9a-f]{8}\.hpix)$zpatt$/,
  sxi_fpix              => qr/(sxi_a[0-9][0-9a-f]{8}\.fpix)$zpatt$/,
  sxs_event_uf          => qr/(sxs_[ps][0-9]px[0-9]{4}_uf\.evt)$zpatt$/,
  sxs_event_cl          => qr/(sxs_[ps][0-9]px[0-9]{4}_cl\.evt)$zpatt$/,
  sxs_lc                => qr/(sxs_[ps][0-9]px[0-9]{4}_src\.lc)$zpatt$/,
  sxs_pha               => qr/(sxs_[ps][0-9]px[0-9]{4}_src\.pha)$zpatt$/,
  sxs_img               => qr/(sxs_[ps][0-9]px[0-9]{4}_src\.img)$zpatt$/,
  sxs_hk                => qr/(sxs_a0\.hk1)$zpatt$/,
  sxs_gtitel            => qr/(sxs_tel\.gti)$zpatt$/,
  sxs_gtilost           => qr/(sxs_el\.gti)$zpatt$/,
  sxs_gtimxfn           => qr/(sxs_mxfn\.gti)$zpatt$/,
  sxs_gtimxcs           => qr/(sxs_mxcs\.gti)$zpatt$/,
  sxs_antico            => qr/(sxs_a0ac_uf\.evt)$zpatt$/,
);

our %archive_dirs = (
  attitude              => 'auxil',
  housekeeping          => 'auxil',
  extended_housekeeping => 'auxil',
  extended_housekeeping2=> 'auxil',
  makefilter            => 'auxil',
  orbit                 => 'auxil',
  timfile               => 'auxil',
  obsgti                => 'auxil',
  # Instrument directories
  hx1_event_uf          => 'hxi/event_uf',
  hx2_event_uf          => 'hxi/event_uf',
  hx1_event_rec         => 'hxi/event_uf',
  hx2_event_rec         => 'hxi/event_uf',
  hx1_event_exp         => 'hxi/event_uf',
  hx2_event_exp         => 'hxi/event_uf',
  hx1_event_cl          => 'hxi/event_cl',
  hx2_event_cl          => 'hxi/event_cl',
  hx1_event_pse         => 'hxi/event_cl',
  hx2_event_pse         => 'hxi/event_cl',
  hx1_hk                => 'hxi/hk',
  hx2_hk                => 'hxi/hk',
  hxi_gtitel            => 'hxi/event_uf',
  hx1_lc                => 'hxi/products',
  hx2_lc                => 'hxi/products',
  hx1_pha               => 'hxi/products',
  hx2_pha               => 'hxi/products',
  hx1_img               => 'hxi/products',
  hx2_img               => 'hxi/products',
  sg1_event_uf          => 'sgd/event_uf',
  sg2_event_uf          => 'sgd/event_uf',
  sg1_event_rec         => 'hxi/event_uf',
  sg2_event_rec         => 'hxi/event_uf',
  sg1_event_exp         => 'hxi/event_uf',
  sg2_event_exp         => 'hxi/event_uf',
  sg1_event_cl          => 'sgd/event_cl',
  sg2_event_cl          => 'sgd/event_cl',
  sg1_event_pse         => 'sgd/event_cl',
  sg2_event_pse         => 'sgd/event_cl',
  sg1_hk                => 'sgd/hk',
  sg2_hk                => 'sgd/hk',
  sgd_gtitel            => 'sgd/event_uf',
  sg1_lc                => 'sgd/products',
  sg2_lc                => 'sgd/products',
  sg1_pha               => 'sgd/products',
  sg2_pha               => 'sgd/products',
  sxi_event_uf          => 'sxi/event_uf',
  sxi_event_cl          => 'sxi/event_cl',
  sxi_hk                => 'sxi/hk',
  sxi_gtitel            => 'sxi/event_uf',
  sxi_exposure          => 'sxi/event_uf',
  sxi_seggti            => 'sxi/event_uf',
  sxi_modegti           => 'sxi/event_uf',
  sxi_hpix              => 'sxi/event_uf',
  sxi_fpix              => 'sxi/event_uf',
  sxi_lc                => 'sxi/products',
  sxi_pha               => 'sxi/products',
  sxi_img               => 'sxi/products',
  sxs_event_uf          => 'sxs/event_uf',
  sxs_event_cl          => 'sxs/event_cl',
  sxs_hk                => 'sxs/hk',
  sxs_gtitel            => 'sxs/event_uf',
  sxs_gtilost           => 'sxs/event_uf',
  sxs_gtimxfn           => 'sxs/event_uf',
  sxs_gtimxcs           => 'sxs/event_uf',
  sxs_antico            => 'sxs/event_uf',
  sxs_lc                => 'sxs/products',
  sxs_pha               => 'sxs/products',
  sxs_img               => 'sxs/products',
);

our %hk_times = (
  ehk_start         => -1,
  ehk_stop         => -1,
);
our @filelist_output;
our @skipped_files = ();
our $fptr1   = "";
our $fptr2   = "";
our $command_run = 0;
our @parlist = ();

# Copy function for output intermediate files
our $copyfunc = 0;

#########################
#  Main Code Block 
#########################

# Run Processing Subroutines
ahapp::startup ($force_debug) ;

# Pre-Processing
ahapp::begin_processing();

$pipelineerror = GetRequiredParameters();
unless ( $pipelineerror == 0 ) {
  ahlog::ah_err "GetRequiredParameters" ;
  ahapp::end_processing($pipelineerror);
}

# Define the copy/move function to use based on whether we want to keep temp files
if ( ahapp::getcleanup ) {
  $copyfunc = \&File::Copy::move;
} else {
  $copyfunc = \&File::Copy::copy;
}

#ahapp::print_input_parameters();

$pipelineerror = CheckOverload();
unless ( $pipelineerror == 0 ) {
  ahgen::set_error_flag($pipelineerror);
  ahlog::ah_err "CheckOverload" ;
  ahapp::end_processing($pipelineerror);
}

$pipelineerror = CheckEntryExitStage();
unless ( $pipelineerror == 0 ) {
  ahlog::ah_err "CheckEntryExitStage" ;
  ahapp::end_processing($pipelineerror);
}

$pipelineerror = CheckInputOutputDirectory();
unless ( $pipelineerror == 0 ) {
  ahlog::ah_err "CheckInputOutputDirectory" ;
  ahapp::end_processing($pipelineerror);
}

$pipelineerror = CheckInstrument();
unless ( $pipelineerror == 0 ) {
  ahlog::ah_err "CheckInstrument" ;
  ahapp::end_processing($pipelineerror);
}

$pipelineerror = GetInputParameters();
unless ( $pipelineerror == 0 ) {
  ahlog::ah_err "GetInputParameters" ;
  ahapp::end_processing($pipelineerror);
}

$pipelineerror = CheckCalibrationFiles();
unless ( $pipelineerror == 0 ) {
  ahlog::ah_err "CheckCalibrationFiles" ;
  ahapp::end_processing($pipelineerror);
}

$pipelineerror = FindInputFiles();
unless ( $pipelineerror == 0 ) {
  ahlog::ah_err "FindInputFiles" ;
  ahapp::end_processing($pipelineerror);
}

$pipelineerror = CheckAuxilFiles();
unless ( $pipelineerror == 0 ) {
  ahlog::ah_err "CheckAuxilFiles" ;
  ahapp::end_processing($pipelineerror);
}  

# Main Processing 
$pipelineerror = StageSwitcher();
unless ( $pipelineerror == 0 ) {
  ahlog::ah_err "StageSwitcher" ;
  ahapp::end_processing($pipelineerror);
}

# Post-Processing
$pipelineerror = FileHistory();
unless ( $pipelineerror == 0 ) {
  ahlog::ah_err "FileHistory" ;
  ahapp::end_processing($pipelineerror);
}

$pipelineerror = Report();
unless ( $pipelineerror == 0 ) {
  ahlog::ah_err "Report" ;
  ahapp::end_processing($pipelineerror);
}

ahapp::end_processing;

############################
# Pre-Processing Functions 
############################

sub GetRequiredParameters {

  # Get required parameters
  $Params{indir}                     = ahapp::query_parameter("indir");
  $Params{outdir}                    = ahapp::query_parameter("outdir");
  $Params{steminputs}                = ahapp::query_parameter("steminputs");
  $Params{stemoutputs}               = ahapp::query_parameter("stemoutputs");
  $Params{entry_stage}               = ahapp::query_parameter("entry_stage");
  $Params{exit_stage}                = ahapp::query_parameter("exit_stage");
  $Params{instrument}                = ahapp::query_parameter("instrument");

  # Get boolean parameters
  $Params{verify_input}              = ahapp::query_parameter("verify_input",1);

  if (uc($Params{stemoutputs}) eq "DEFAULT") {
      $Params{stemoutputs} = $Params{steminputs};
  }    

  # Set output report name

  $Params{stemoutputs} =~ s/^\s+//; # Remove leading blanks
  $Params{stemoutputs} =~ s/\s+$//; # Remove trailing blanks

  unless ( $Params{stemoutputs} eq "" ) {
      $General{reportname} = "$Params{stemoutputs}_ahpipeline_report.txt";
  }

  # Set the date string in the format YYYYMMDDTHHMMSS
  $Task{start} = strftime "%Y%m%dT%H%M%S", localtime ;

  # Create temporary and log file name stems
  $General{tempstem}   = "ahpipeline_" . $Task{start} . "_tmp_";
  $General{logstem}    = "ahpipeline_" . $Task{start} . "_";
  $General{stemreport} = "ahpipeline_" . $Task{start};

  return 0;

}

sub CheckOverload {

  if ( ( $ahapp::logfile ne "NONE" ) && ($ahapp::chatter > 2) ) {

    ahlog::ah_out "\n          WARNING          \n\n";
    ahlog::ah_out "Output report requested and chatter level = $ahapp::chatter\n";
    ahlog::ah_out "This can result in a very large output report file\n";
    ahlog::ah_out "Ensure sufficient space available for output report file\n\n";

    for (my $i1 = 5; $i1 > 0; $i1--) {
      ahlog::ah_out"$i1 seconds before continuing\n";
      sleep 1;
    } 
  }

  return 0;
}

sub CheckEntryExitStage {

  my $entry_stage = shift;
  my $exit_stage = shift;

  # Check entry stage is in range 1 - 2

  unless ( $Params{entry_stage} =~ /^(1|2|3)$/ ) {
    ahlog::ah_err "Entry stage must be one of 1,2 or 3" ;
    ahlog::ah_err "Entry stage = $Params{entry_stage}" ;
    return 1;
  }

  # Check exit stage is in range 1 - 2

  unless ( $Params{exit_stage} =~ /^(1|2|3)$/ ) {
    ahlog::ah_err "Exit stage must be one of 1,2 or 3" ;
    ahlog::ah_err "Exit stage = $Params{exit_stage}" ;
    return 1;
  }

  # Check entry stage is before/same as exit stage

  if ( $Params{entry_stage} > $Params{exit_stage} ) {
    ahlog::ah_err "Entry stage after exit stage" ;
    ahlog::ah_err "Entry stage = $Params{entry_stage}" ;
    ahlog::ah_err "Exit stage  = $Params{exit_stage}" ;
    return 1;
  }

  # Decide which stage(s) of processing are to be performed
  # Stage 1 - Calibration
  # Stage 2 - Clean and Filter data (various algorithms)
  # Stage 3 - Products generation

  # Decide which stages (1,2,3) are to be run

  $General{stage1_switch} = 0;
  $General{stage2_switch} = 0;

  if ( $Params{entry_stage} == 1 ) {
    $General{stage1_switch} = 1;
    $General{stage2_switch} = 1;
    $General{stage3_switch} = 1;
  }
  if ( $Params{entry_stage} == 2 ) {
    $General{stage2_switch} = 1;
    $General{stage3_switch} = 1;
  }
  if ( $Params{entry_stage} == 3 ) {
    $General{stage3_switch} = 1;
  }
  if ( $Params{exit_stage} == 1 ) {
    $General{stage2_switch} = 0;
    $General{stage3_switch} = 0;
  }
  if ( $Params{exit_stage} == 2 ) {
    $General{stage3_switch} = 0;
  }

  return 0;

}

sub CheckInputOutputDirectory {


  if ( -e$Params{indir} )
  {
    ahlog::ah_info "LOW", "Input Directory Found : $Params{indir}";
  } else {
    ahlog::ah_err "Input Directory NOT FOUND : $Params{indir}";
    return 1;
  }

  ahlog::ah_out "Input directory  : $Params{indir}";
  ahlog::ah_out "Output directory : $Params{outdir}";

  if ( -e$Params{outdir} ) {

    ahlog::ah_info "LOW", "Output Directory Found : $Params{outdir}" ;

    if ( lc $Params{steminputs} eq lc $Params{stemoutputs} ) {

      if ( lc $Params{outdir} eq lc $Params{indir} ) {

          if ( $General{stage1_switch} ) {
            # Case: indir=outdir, entry_stage=1: error
            # Do not overwrite original files
            ahlog::ah_err "Output directory same as input directory and entry_stage=1";
            return 1;
          } else {
            unless ( $ahapp::clobber ) {
              # Case: indir=outdir, entry_stage=2, clobber=no: error
              ahlog::ah_err "Output directory same as input directory and clobber=no";
              return 1;
            } # End if clobber
          } # End if stage1_switch = 1

      } else {

        # Input directory is different than output directory

        if ( $General{stage1_switch} ) {

          if ($ahapp::clobber) {
            # Case: indir!=outdir, entry_stage=1, clobber=yes: remove contents of outdir
            # We want a clean output directory, remove all contents of output directory
            ahlog::ah_out " *** Removing all contents of output directory $Params{outdir}";
            my $files = catfile ( $Params{outdir} , "*" ); 
            unlink glob $files;
          } else {
            # Case: indir!=outdir, entry_stage=1, clobber=no: error
            ahlog::ah_err "Output directory exists \& clobber=no : $Params{outdir}" ;
            return 1;
          } # end if clobber

        } else {
          unless ( $ahapp::clobber ) {
            # Case: indir!=outdir, entry_stage=2+, clobber=no: error
            ahlog::ah_err "Output directory exists \& clobber=no : $Params{outdir}" ;
            return 1;
          }
        } # End if entry_stage==1

      } # End if outdir!=indir

    } # End of steminputs==stemoutputs

  } else {
    
    # Create the output directory since it does not exist
    # No other checks necessary
    ahlog::ah_info "LOW", "Output Directory Not Found  : $Params{outdir}" ;
    ahlog::ah_info "LOW", "Making New Output Directory : $Params{outdir}" ;
    eval { mkpath( $Params{outdir} ); };
    if ( $@ ) {
      ahlog::ah_err "Cannot make directory : $Params{outdir}" ;
      return 1;
    }

  } # End if outdir exists

  return 0;
}

# Common section for CheckInclusion subroutine

{
  my ( $indir, $outdir );

  # Build a hash of the subdirectories of $outdir

  my %suboutdirs;
  my $found = 0;

  sub CheckInclusion {
    ( $indir, $outdir ) = @_;
    $indir  = abs_path( $indir );
    $outdir = abs_path( $outdir );

    # Traverse the $outdir filesystems, noting the subdirs

    File::Find::find( { wanted => \&buildsub, follow => 1, follow_skip => 2 }, $outdir );

    # Traverse the $indir filesystems

    File::Find::find( { wanted => \&lookout, follow => 1, follow_skip => 2 }, $indir );
    return $found;
  }

  sub buildsub {
    my ( $dev, $ino );
    unless ( defined $File::Find::fullname ) {
      return;
    }
    if ( -d $_ || -d $File::Find::fullname ) {
      ( $dev, $ino ) = stat _;
      $suboutdirs{$File::Find::fullname} = [ $_, $dev, $ino ];
    }
  }

  sub lookout {
    my ( $dev, $ino );
    unless ( defined $File::Find::fullname ) {
      return;
    }
    if ( -d $_ || -d $File::Find::fullname ) {
      if ( $suboutdirs{$File::Find::fullname} ) {
        ( ( $dev, $ino ) = stat _ );
        if ( $dev == $suboutdirs{$File::Find::fullname}->[ 1 ] &&
           $ino == $suboutdirs{$File::Find::fullname}->[ 2 ] ) {
          $File::Find::prune = 1;
          $found             = 1;
        }
      }
    }
  }
}   # CheckInclusion subroutine section

sub CheckInstrument {

  my $instrument_set = 0;

  for my $instrument_key ( keys %instrument ) {
    $instrument{$instrument_key} = 0;
  }

  # Split the instrument parameter on ,

  $Params{instrument} =~ s/\s+//g;
  foreach my $instrument_value ( split( ',', uc( $Params{instrument} ) ) ) {

    unless ( $instrument_value =~ /^(ALL|HXI|HXI1|HXI2|SGD|SGD1|SGD2|SXS|SXI)$/i ) {
      ahlog::ah_err "Invalid instrument : $instrument_value" ;
      return 1;
    }

    if ( $instrument_value eq "ALL" ) {
      for my $instrument_key ( keys %instrument ) {
        $instrument{$instrument_key} = 1;
      }
      $instrument_set = 1;
      last;
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
    } elsif ( $instrument_value eq "SGD" ) {
      $instrument{sg1} = 1;
      $instrument{sg2} = 1;
      $instrument_set  = 1;
    } elsif ( $instrument_value eq "SGD1" ) {
      $instrument{sg1} = 1;
      $instrument_set  = 1;
    } elsif ( $instrument_value eq "SGD2" ) {
      $instrument{sg2} = 1;
      $instrument_set  = 1;
    } elsif ( $instrument_value eq "SXS" ) {
      $instrument{sxs} = 1;
      $instrument_set  = 1;
    } elsif ( $instrument_value eq "SXI" ) {
      $instrument{sxi} = 1;
      $instrument_set  = 1;
    } 
  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nParams_instrument = $Params{instrument}\n\n";
    ahlog::ah_debug "instrument_set  = $instrument_set\n\n";
    ahlog::ah_debug "instrument_hx1  = $instrument{hx1}\n";
    ahlog::ah_debug "instrument_hx2  = $instrument{hx2}\n";
    ahlog::ah_debug "instrument_sgd1 = $instrument{sg1}\n";
    ahlog::ah_debug "instrument_sgd2 = $instrument{sg2}\n";
    ahlog::ah_debug "instrument_sxs  = $instrument{sxs}\n";
    ahlog::ah_debug "instrument_sxi  = $instrument{sxi}\n";
  }

  if ( $instrument_set == 0 ) {
    ahlog::ah_err "No valid instrument found in instrument parameter: '$Params{instrument}'" ;
    return 1;
  }

  return 0;
}

sub GetInputParameters {
  
  #my @reqparm = qw(indir outdir steminputs entry_stage exit_stage instrument);

  # Get auxiliary file parameters
  $Params{attitude}                  = ahapp::query_parameter("attitude");
  $Params{housekeeping}              = ahapp::query_parameter("housekeeping");
  $Params{extended_housekeeping}     = ahapp::query_parameter("extended_housekeeping");
  $Params{makefilter}                = ahapp::query_parameter("makefilter");
  $Params{orbit}                     = ahapp::query_parameter("orbit");
  $Params{timfile}                   = ahapp::query_parameter("timfile");
  $Params{obsgti}                    = ahapp::query_parameter("obsgti");

  $Params{ra}                        = ahapp::query_parameter("ra");
  $Params{dec}                       = ahapp::query_parameter("dec");
  $Params{roll}                      = ahapp::query_parameter("roll");
  $Params{calc_pointing}             = ahapp::query_parameter("calc_pointing",1);
  $Params{calc_optaxis}              = ahapp::query_parameter("calc_optaxis",1);
  $Params{create_ehkmkf}             = ahapp::query_parameter("create_ehkmkf",1);
  $Params{makeregion}                = ahapp::query_parameter("makeregion",1);

  # Get timing parameters
  $Params{sxi_start}                 = ahapp::query_parameter("sxi_start");
  $Params{hxi_start}                 = ahapp::query_parameter("hxi_start");
  $Params{sgd_start}                 = ahapp::query_parameter("sgd_start");
  $Params{sxs_start}                 = ahapp::query_parameter("sxs_start");

  # get randomization
  $Params{randomize}                 = ahapp::query_parameter("randomize");
  $Params{seed}                      = ahapp::query_parameter("seed");
  $Params{screenlost}                = ahapp::query_parameter("screenlost");

  # General CALDB files
  $caldb_files{gen}{leapsecfile}     = ahapp::query_parameter("leapsecfile");
  $caldb_files{gen}{selectfile}      = ahapp::query_parameter("selectfile");
  $caldb_files{gen}{mkfconf}         = ahapp::query_parameter("mkfconf");
  $caldb_files{gen}{cor2file}        = ahapp::query_parameter("cor2file");
  $caldb_files{gen}{cor3file}        = ahapp::query_parameter("cor3file");
  $caldb_files{gen}{saafile}         = ahapp::query_parameter("saafile");

  # HXI CALDB files
  $caldb_files{hx1}{teldeffile}      = ahapp::query_parameter("hx1_teldef");
  $caldb_files{hx2}{teldeffile}      = ahapp::query_parameter("hx2_teldef");
  if ( $instrument{hx1} or $instrument{hx2} ) {
    $caldb_files{hxi}{remapfile}       = ahapp::query_parameter("hxi_remapfile");
    $caldb_files{hxi}{fluorefile}      = ahapp::query_parameter("hxi_fluorefile");
    $caldb_files{hxi}{badpixfile}      = ahapp::query_parameter("hxi_badpixfile");
    $caldb_files{hxi}{enecutfile}      = ahapp::query_parameter("hxi_enecutfile");
    $caldb_files{hxi}{gainfile}        = ahapp::query_parameter("hxi_gainfile");
    # CAMS CALDB files
    $caldb_files{cm1}{teldeffile}      = ahapp::query_parameter("cm1_teldef");
    $caldb_files{cm2}{teldeffile}      = ahapp::query_parameter("cm2_teldef");
    $caldb_files{cms}{camstempxy}      = ahapp::query_parameter("camstempxy");

    $Params{hxi_mkflabel}              = ahapp::query_parameter("hxi_mkflabel");
    $Params{hxi_ehklabel}              = ahapp::query_parameter("hxi_ehklabel");
    $Params{hxi_evtlabel}              = ahapp::query_parameter("hxi_evtlabel");
  }


  # SGD CALDB files
  $caldb_files{sg1}{teldeffile}      = ahapp::query_parameter("sg1_teldef");
  $caldb_files{sg2}{teldeffile}      = ahapp::query_parameter("sg2_teldef");
  if ( $instrument{sg1} or $instrument{sg2} ) {
    $caldb_files{sgd}{remapfile}       = ahapp::query_parameter("sgd_remapfile");
    $caldb_files{sgd}{fluorefile}      = ahapp::query_parameter("sgd_fluorefile");
    $caldb_files{sgd}{badpixfile}      = ahapp::query_parameter("sgd_badpixfile");
    $caldb_files{sgd}{gainfile}        = ahapp::query_parameter("sgd_gainfile");
    $caldb_files{sgd}{probseqfile}     = ahapp::query_parameter("sgd_probseqfile");
    $caldb_files{sgd}{probfovfile}     = ahapp::query_parameter("sgd_probfovfile");

    $Params{sgd_mkflabel}              = ahapp::query_parameter("sgd_mkflabel");
    $Params{sgd_ehklabel}              = ahapp::query_parameter("sgd_ehklabel");
    $Params{sgd_evtlabel}              = ahapp::query_parameter("sgd_evtlabel");

  }

  # SXI CALDB files
  $caldb_files{sxi}{teldeffile}      = ahapp::query_parameter("sxi_teldef");
  if ( $instrument{sxi} ) {
    $caldb_files{sxi}{badpixfile}      = ahapp::query_parameter("sxi_badpixfile");
    $caldb_files{sxi}{maskfile}        = ahapp::query_parameter("sxi_maskfile");
    $caldb_files{sxi}{vtevnodd}        = ahapp::query_parameter("sxi_vtevnodd");
    $caldb_files{sxi}{ctifile}         = ahapp::query_parameter("sxi_ctifile");
    $caldb_files{sxi}{chtrailfile}     = ahapp::query_parameter("sxi_chtrailfile");
    $caldb_files{sxi}{spthfile}        = ahapp::query_parameter("sxi_spthfile");
    $caldb_files{sxi}{gainfile}        = ahapp::query_parameter("sxi_gainfile");
    $caldb_files{sxi}{patternfile}     = ahapp::query_parameter("sxi_patternfile");

    $Params{sxi_mkflabel}              = ahapp::query_parameter("sxi_mkflabel");
    $Params{sxi_ehklabel}              = ahapp::query_parameter("sxi_ehklabel");
    $Params{sxi_evtlabel}              = ahapp::query_parameter("sxi_evtlabel");
  }

  # SXS CALDB files
  $caldb_files{sxs}{teldeffile}      = ahapp::query_parameter("sxs_teldef");
  if ( $instrument{sxs} ) {
    $caldb_files{sxs}{coeftime}        = ahapp::query_parameter("sxs_coeftime");
    $caldb_files{sxs}{pixdeffile}      = ahapp::query_parameter("sxs_pixdeffile");
    $caldb_files{sxs}{scalefile}       = ahapp::query_parameter("sxs_scalefile");
    $caldb_files{sxs}{gainfile}        = ahapp::query_parameter("sxs_gainfile");
    $caldb_files{sxs}{pulsefile}       = ahapp::query_parameter("sxs_pulsefile");
    $caldb_files{sxs}{linefitfile}     = ahapp::query_parameter("sxs_linefitfile");
    $caldb_files{sxs}{gainantfile}     = ahapp::query_parameter("sxs_gainantfile");
    $caldb_files{sxs}{delayfile}       = ahapp::query_parameter("sxs_delayfile");
    $Params{sxs_mkflabel}              = ahapp::query_parameter("sxs_mkflabel");
    $Params{sxs_ehklabel}              = ahapp::query_parameter("sxs_ehklabel");
    $Params{sxs_evtlabel}              = ahapp::query_parameter("sxs_evtlabel");
  }

  # coordevt pars 
  if ( $instrument{hx1} or
       $instrument{hx2} or
       $instrument{sxi} or
       $instrument{sxs} ) {
    $Params{coordevt}{dattfile}          = ahapp::query_parameter("dattfile");
    $Params{coordevt}{coordevt_startsys}          = ahapp::query_parameter("coordevt_startsys");
    $Params{coordevt}{stopsys}           = ahapp::query_parameter("stopsys");
    $Params{coordevt}{annaber}           = ahapp::query_parameter("annaber");
    $Params{coordevt}{followsun}         = ahapp::query_parameter("followsun");
    $Params{coordevt}{orbaber}           = ahapp::query_parameter("orbaber");
    $Params{coordevt}{attinterp}         = ahapp::query_parameter("attinterp");
    $Params{coordevt}{dattinterp}        = ahapp::query_parameter("dattinterp");
    $Params{coordevt}{attdt}             = ahapp::query_parameter("attdt");
    $Params{coordevt}{dattdt}            = ahapp::query_parameter("dattdt");
    $Params{coordevt}{chkattgap}         = ahapp::query_parameter("chkattgap");
    $Params{coordevt}{chkdattgap}        = ahapp::query_parameter("chkdattgap");
    $Params{coordevt}{attext}            = ahapp::query_parameter("attext");
    $Params{coordevt}{attcol}            = ahapp::query_parameter("attcol");
    $Params{coordevt}{attform}           = ahapp::query_parameter("attform");
    $Params{coordevt}{orbext}            = ahapp::query_parameter("orbext");
    $Params{coordevt}{orbcol}            = ahapp::query_parameter("orbcol");
    $Params{coordevt}{orbform}           = ahapp::query_parameter("orbform");
    $Params{coordevt}{coordevt_randomize}         = ahapp::query_parameter("coordevt_randomize");
    $Params{coordevt}{randsys}           = ahapp::query_parameter("randsys");
    $Params{coordevt}{randscalesys}      = ahapp::query_parameter("randscalesys");
    $Params{coordevt}{infileext}         = ahapp::query_parameter("infileext");
    $Params{coordevt}{inclfloatcol}      = ahapp::query_parameter("inclfloatcol");
    $Params{coordevt}{inclfloatskycol}   = ahapp::query_parameter("inclfloatskycol");
    $Params{coordevt}{floatcolsuffix}    = ahapp::query_parameter("floatcolsuffix");
    $Params{coordevt}{startwithfloat}    = ahapp::query_parameter("startwithfloat");
    $Params{coordevt}{blankcol}          = ahapp::query_parameter("blankcol");
    $Params{coordevt}{btnull}            = ahapp::query_parameter("btnull");
    $Params{coordevt}{itnull}            = ahapp::query_parameter("itnull");
    $Params{coordevt}{jtnull}            = ahapp::query_parameter("jtnull");
    $Params{coordevt}{ktnull}            = ahapp::query_parameter("ktnull");
    $Params{coordevt}{sbtnull}           = ahapp::query_parameter("sbtnull");
    $Params{coordevt}{uitnull}           = ahapp::query_parameter("uitnull");
    $Params{coordevt}{ujtnull}           = ahapp::query_parameter("ujtnull");
    $Params{coordevt}{timecol}           = ahapp::query_parameter("timecol");
  }

  # HXI/SGD PARAMETERS
  if ( $instrument{hx1} or 
       $instrument{hx2} or
       $instrument{sg1} or
       $instrument{sg2} ) {
    # hxisgdsff pars 
    # NONE

    # hxisgdpha pars 
    $Params{hxisgd}{outnsubcol}      = ahapp::query_parameter("outnsubcol");
    $Params{hxisgd}{datamode}        = ahapp::query_parameter("datamode");
    $Params{hxisgd}{randomize}          = ahapp::query_parameter("randomize");
  } # end hxi/sgd parameters

  # HXI PARAMETERS
  if ( $instrument{hx1} or $instrument{hx2} ) {
    # cams2att pars
    $Params{hxi}{startsys}           = ahapp::query_parameter("startsys");
    $Params{hxi}{startstep}       = ahapp::query_parameter("startstep");
    $Params{hxi}{stopstep}        = ahapp::query_parameter("stopstep");
    $Params{hxi}{inext}           = ahapp::query_parameter("inext");
    $Params{hxi}{outext}          = ahapp::query_parameter("outext");
    $Params{hxi}{flipsign}        = ahapp::query_parameter("flipsign");
    $Params{hxi}{prefiltfile1}    = ahapp::query_parameter("prefiltfile1");
    $Params{hxi}{prefiltfile2}    = ahapp::query_parameter("prefiltfile2");
    $Params{hxi}{filtoffset}      = ahapp::query_parameter("filtoffset");
    $Params{hxi}{prefiltexpr}     = ahapp::query_parameter("prefiltexpr");
    $Params{hxi}{filtexpr}        = ahapp::query_parameter("filtexpr");
    $Params{hxi}{gtiexpr0}        = ahapp::query_parameter("gtiexpr0");
    $Params{hxi}{gtiexpr1}        = ahapp::query_parameter("gtiexpr1");
    $Params{hxi}{gtiexpr2}        = ahapp::query_parameter("gtiexpr2");
    $Params{hxi}{deltaxcol}       = ahapp::query_parameter("deltaxcol");
    $Params{hxi}{deltaycol}       = ahapp::query_parameter("deltaycol");
    $Params{hxi}{sincol}          = ahapp::query_parameter("sincol");
    $Params{hxi}{coscol}          = ahapp::query_parameter("coscol");

    # hxievtid pars 
    $Params{hxi}{rejectbgo}       = ahapp::query_parameter("rejectbgo");
    $Params{hxi}{skipreco}        = ahapp::query_parameter("skipreco");
    $Params{hxi}{outcalfile}    = ahapp::query_parameter("outcalfile");
    $Params{hxi}{seed}              = $Params{seed};
  } # end hxi parameters

  # HXI PARAMETERS
  if ( $instrument{sg1} or $instrument{sg2} ) {
    # sgdevtid pars
    $Params{sgd}{rejectbgo}       = ahapp::query_parameter("rejectbgo");
    $Params{sgd}{skipreco}        = ahapp::query_parameter("skipreco");
    $Params{sgd}{outtracefile}    = ahapp::query_parameter("outtracefile");
    $Params{sgd}{numsignal}       = ahapp::query_parameter("numsignal");
    $Params{sgd}{d10}             = ahapp::query_parameter("d10");
    $Params{sgd}{d1a1a}           = ahapp::query_parameter("d1a1a");
    $Params{sgd}{d1a1b}           = ahapp::query_parameter("d1a1b");
    $Params{sgd}{d1a2}            = ahapp::query_parameter("d1a2");
    $Params{sgd}{d1a3}            = ahapp::query_parameter("d1a3");
    $Params{sgd}{a}               = ahapp::query_parameter("a");
    $Params{sgd}{b}               = ahapp::query_parameter("b");
    $Params{sgd}{probaccept2}     = ahapp::query_parameter("probaccept2");
    $Params{sgd}{probaccept3}     = ahapp::query_parameter("probaccept3");
    $Params{sgd}{probaccept4}     = ahapp::query_parameter("probaccept4");
    $Params{sgd}{distz}           = ahapp::query_parameter("distz");
    $Params{sgd}{paraoffset0}     = ahapp::query_parameter("paraoffset0");
    $Params{sgd}{paraoffset1}     = ahapp::query_parameter("paraoffset1");
    $Params{sgd}{paraoffset2}     = ahapp::query_parameter("paraoffset2");
    $Params{sgd}{weight0}         = ahapp::query_parameter("weight0");
    $Params{sgd}{weight1}         = ahapp::query_parameter("weight1");
    $Params{sgd}{weight2}         = ahapp::query_parameter("weight2");
    $Params{sgd}{weight3}         = ahapp::query_parameter("weight3");
    $Params{sgd}{delgmethod}      = ahapp::query_parameter("delgmethod");
    $Params{sgd}{seed}              = $Params{seed};
  } # end sgd parameters

  # SXI PARAMETERS
  if ( $instrument{sxi} ) {
    $Params{sxi}{calc_hotpix}       = ahapp::query_parameter("calc_hotpix");
    $Params{sxi}{calc_modegti}      = ahapp::query_parameter("calc_modegti");

    # sxiphas
    $Params{sxi}{colbound}          = ahapp::query_parameter("colbound");

    # searchflickpix
    $Params{sxi}{chipcol}           = ahapp::query_parameter("chipcol");
    $Params{sxi}{xcol}              = ahapp::query_parameter("xcol");
    $Params{sxi}{ycol}              = ahapp::query_parameter("ycol");
    $Params{sxi}{chancol}           = ahapp::query_parameter("chancol");
    $Params{sxi}{gradecol}          = ahapp::query_parameter("gradecol");
    $Params{sxi}{grade}             = ahapp::query_parameter("grade");
    $Params{sxi}{n_division}        = ahapp::query_parameter("n_division");
    $Params{sxi}{cleanimg}          = ahapp::query_parameter("cleanimg");
    $Params{sxi}{cellsize}          = ahapp::query_parameter("cellsize");
    $Params{sxi}{impfac}            = ahapp::query_parameter("impfac");
    $Params{sxi}{logprob1}          = ahapp::query_parameter("logprob1");
    $Params{sxi}{logprob2}          = ahapp::query_parameter("logprob2");
    $Params{sxi}{iterate}           = ahapp::query_parameter("iterate");
    $Params{sxi}{flagedge}          = ahapp::query_parameter("flagedge");
    $Params{sxi}{bthresh}           = ahapp::query_parameter("bthresh");
    $Params{sxi}{duration}          = ahapp::query_parameter("duration");
    $Params{sxi}{sigma}             = ahapp::query_parameter("sigma");
    $Params{sxi}{firstchip}         = ahapp::query_parameter("firstchip");
    $Params{sxi}{lastchip}          = ahapp::query_parameter("lastchip");
    $Params{sxi}{xmin}              = ahapp::query_parameter("xmin");
    $Params{sxi}{xmax}              = ahapp::query_parameter("xmax");
    $Params{sxi}{ymin}              = ahapp::query_parameter("ymin");
    $Params{sxi}{ymax}              = ahapp::query_parameter("ymax");
    $Params{sxi}{chanmin}           = ahapp::query_parameter("chanmin");
    $Params{sxi}{chanmax}           = ahapp::query_parameter("chanmax");

    # sxiflagpix
    $Params{sxi}{outbadpix}         = ahapp::query_parameter("outbadpix");
    $Params{sxi}{outbadimg}         = ahapp::query_parameter("outbadimg");
    $Params{sxi}{npixnbr}           = ahapp::query_parameter("npixnbr");
    $Params{sxi}{nboundnbr}         = ahapp::query_parameter("nboundnbr");
    $Params{sxi}{citrailnbr}        = ahapp::query_parameter("citrailnbr");
    $Params{sxi}{ciprenbr}          = ahapp::query_parameter("ciprenbr");
    $Params{sxi}{echoflag}          = ahapp::query_parameter("echoflag");
    $Params{sxi}{echomap}           = ahapp::query_parameter("echomap");
    $Params{sxi}{echonbr}           = ahapp::query_parameter("echonbr");
    $Params{sxi}{echomin}           = ahapp::query_parameter("echomin");
    $Params{sxi}{echospth}          = ahapp::query_parameter("echospth");
    $Params{sxi}{echofrac}          = ahapp::query_parameter("echofrac");
    $Params{sxi}{bad_status}        = ahapp::query_parameter("bad_status");
    $Params{sxi}{copyphas}          = ahapp::query_parameter("copyphas");
    $Params{sxi}{resetflags}        = ahapp::query_parameter("sxi_resetflags");

    # sxipi 
    $Params{sxi}{hkext}             = ahapp::query_parameter("hkext");
    $Params{sxi}{hkcolstem}         = ahapp::query_parameter("hkcolstem");
    $Params{sxi}{hkvideoid}         = ahapp::query_parameter("hkvideoid");
    $Params{sxi}{startcol}          = ahapp::query_parameter("startcol");
    $Params{sxi}{evnoddcor}         = ahapp::query_parameter("evnoddcor");
    $Params{sxi}{chtrailcor}        = ahapp::query_parameter("chtrailcor");
    $Params{sxi}{cticor}            = ahapp::query_parameter("cticor");
    $Params{sxi}{gaincor}           = ahapp::query_parameter("gaincor");
    $Params{sxi}{ctigrade}          = ahapp::query_parameter("ctigrade");
    $Params{sxi}{copygrade}         = ahapp::query_parameter("copygrade");
    $Params{sxi}{phcut}             = ahapp::query_parameter("phcut");
    $Params{sxi}{badpixopt}         = ahapp::query_parameter("badpixopt");
    $Params{sxi}{spthiter}          = ahapp::query_parameter("spthiter");
    $Params{sxi}{spthcaldb}         = ahapp::query_parameter("spthcaldb");
    $Params{sxi}{spthoffset}        = ahapp::query_parameter("spthoffset");
    $Params{sxi}{spthslope}         = ahapp::query_parameter("spthslope");
    $Params{sxi}{evtthre}           = ahapp::query_parameter("evtthre");
    $Params{sxi}{negthre}           = ahapp::query_parameter("negthre");
    $Params{sxi}{deltatime}         = ahapp::query_parameter("deltatime");
    $Params{sxi}{debugcol}          = ahapp::query_parameter("debugcol");
    $Params{sxi}{randomize}         = $Params{randomize};
    $Params{sxi}{seed}              = $Params{seed};
  } # end sxi parameters

  # SXS Parameters
  if ( $instrument{sxs} ) {

    # Miscellaneous/Shared parameter
    $Params{adrgti}                 = ahapp::query_parameter("adrgti");
    $Params{sxs}{acphaoffset}       = ahapp::query_parameter("acphaoffset");
    $Params{sxs}{pxphaoffset}       = ahapp::query_parameter("pxphaoffset");
    $Params{sxs}{calc_gtilost}      = ahapp::query_parameter("calc_gtilost");
    $Params{sxs}{itypecol}          = ahapp::query_parameter("itypecol");
    $Params{sxs}{ckctrec}           = ahapp::query_parameter("ckctrec");
    $Params{sxs}{ckctel}            = ahapp::query_parameter("ckctel");
    $Params{sxs}{ckant}             = ahapp::query_parameter("ckant");
    $Params{sxs}{ckrisetime}        = ahapp::query_parameter("ckrisetime");
    $Params{sxs}{tempidx}           = ahapp::query_parameter("tempidx");
    $Params{sxs}{ntemp}             = ahapp::query_parameter("ntemp");
    $Params{sxs}{gapdt}             = ahapp::query_parameter("gapdt");
    $Params{sxs}{extrap}            = ahapp::query_parameter("extrap");
    $Params{sxs}{randomize}         = $Params{randomize};
    $Params{sxs}{seed}              = $Params{seed};

    # CALDB files
    # mxsgti parameters
    $Params{sxs}{stimecol}          = ahapp::query_parameter("stimecol");
    $Params{sxs}{tioncol}           = ahapp::query_parameter("tioncol");
    $Params{sxs}{tioffcol}          = ahapp::query_parameter("tioffcol");
    $Params{sxs}{plslencol}         = ahapp::query_parameter("plslencol");
    $Params{sxs}{plsspccol}         = ahapp::query_parameter("plsspccol");
    $Params{sxs}{timeoncol}         = ahapp::query_parameter("timeoncol");
    $Params{sxs}{timeoffcol}        = ahapp::query_parameter("timeoffcol");
    $Params{sxs}{calctime}          = ahapp::query_parameter("calctime");
    $Params{sxs}{calcgti}           = ahapp::query_parameter("calcgti");
    $Params{sxs}{afterglow}         = ahapp::query_parameter("afterglow");
    $Params{sxs}{dtdecay}           = ahapp::query_parameter("dtdecay");
    $Params{sxs}{interp}            = ahapp::query_parameter("interp");
    $Params{sxs}{margingti}         = ahapp::query_parameter("margingti");
    $Params{sxs}{tstart}            = ahapp::query_parameter("tstart");
    $Params{sxs}{tstop}             = ahapp::query_parameter("tstop");
    $Params{sxs}{dt}                = ahapp::query_parameter("dt");

    # sxsanticopi parameters
    # gainantfile, randomize, seed

    # sxsflagpix parameters
    $Params{sxs}{antpsp}           = ahapp::query_parameter("antpsp");
    $Params{sxs}{antshift}         = ahapp::query_parameter("antshift");
    $Params{sxs}{calcant}          = ahapp::query_parameter("calcant");
    $Params{sxs}{antdtpre}         = ahapp::query_parameter("antdtpre");
    $Params{sxs}{antdtfol}         = ahapp::query_parameter("antdtfol");
    $Params{sxs}{antswitch}        = ahapp::query_parameter("antswitch");
    $Params{sxs}{antphathr}        = ahapp::query_parameter("antphathr");
    $Params{sxs}{antdurthr}        = ahapp::query_parameter("antdurthr");
    $Params{sxs}{calcctrec}        = ahapp::query_parameter("calcctrec");
    $Params{sxs}{ctrecdt}          = ahapp::query_parameter("ctrecdt");
    $Params{sxs}{calcprox}         = ahapp::query_parameter("calcprox");
    $Params{sxs}{proxdt}           = ahapp::query_parameter("proxdt");
    $Params{sxs}{calcctel}         = ahapp::query_parameter("calcctel");
    $Params{sxs}{cteldt}           = ahapp::query_parameter("cteldt");
    $Params{sxs}{ctelnear}         = ahapp::query_parameter("ctelnear");
    $Params{sxs}{calcctel2}        = ahapp::query_parameter("calcctel2");
    $Params{sxs}{cteldt2}          = ahapp::query_parameter("cteldt2");
    $Params{sxs}{ctelnear2}        = ahapp::query_parameter("ctelnear2");
    $Params{sxs}{pxpithr}          = ahapp::query_parameter("pxpithr");
    $Params{sxs}{usepxpithr}       = ahapp::query_parameter("usepxpithr");
    $Params{sxs}{calcmxs}          = ahapp::query_parameter("calcmxs");
    $Params{sxs}{mxsdt}            = ahapp::query_parameter("mxsdt");
    $Params{sxs}{kalow}            = ahapp::query_parameter("kalow");
    $Params{sxs}{kahigh}           = ahapp::query_parameter("kahigh");
    $Params{sxs}{kbeta}            = ahapp::query_parameter("kbeta");
    $Params{sxs}{dtflag}           = ahapp::query_parameter("dtflag");
    $Params{sxs}{resetflags}       = ahapp::query_parameter("sxs_resetflags");

    # sxssecid parameters
    $Params{sxs}{dtprimary}           = ahapp::query_parameter("dtprimary");
    $Params{sxs}{dtlowmid}            = ahapp::query_parameter("dtlowmid");
    $Params{sxs}{dtmidhigh}           = ahapp::query_parameter("dtmidhigh");
    $Params{sxs}{tol}                 = ahapp::query_parameter("tol");
    $Params{sxs}{regrade}             = ahapp::query_parameter("regrade");

    # sxsseccor parameters
    $Params{sxs}{phaout}              = ahapp::query_parameter("phaout");

    # sxsgain parameters
    $Params{sxs}{gaincoeff}           = ahapp::query_parameter("gaincoeff");
    $Params{sxs}{linetocorrect}       = ahapp::query_parameter("linetocorrect");
    $Params{sxs}{numevent}            = ahapp::query_parameter("numevent");
    $Params{sxs}{minevent}            = ahapp::query_parameter("minevent");
    $Params{sxs}{grpoverlap}          = ahapp::query_parameter("grpoverlap");
    $Params{sxs}{startenergy}         = ahapp::query_parameter("startenergy");
    $Params{sxs}{stopenergy}          = ahapp::query_parameter("stopenergy");
    $Params{sxs}{extraspread}         = ahapp::query_parameter("extraspread");
    $Params{sxs}{broadening}          = ahapp::query_parameter("broadening");
    $Params{sxs}{gridprofile}         = ahapp::query_parameter("gridprofile");
    $Params{sxs}{fitwidth}            = ahapp::query_parameter("fitwidth");
    $Params{sxs}{background}          = ahapp::query_parameter("background");
    $Params{sxs}{spangti}             = ahapp::query_parameter("spangti");
    $Params{sxs}{usemp}               = ahapp::query_parameter("usemp");
    $Params{sxs}{calcerr}             = ahapp::query_parameter("calcerr");
    $Params{sxs}{writeerrfunc}        = ahapp::query_parameter("writeerrfunc");
    $Params{sxs}{avgwinrad}           = ahapp::query_parameter("avgwinrad");
    $Params{sxs}{minwidth0}           = ahapp::query_parameter("minwidth0");
    $Params{sxs}{maxitcycle}          = ahapp::query_parameter("maxitcycle");
    $Params{sxs}{r2tol}               = ahapp::query_parameter("r2tol");
    $Params{sxs}{searchstepshift}     = ahapp::query_parameter("searchstepshift");
    $Params{sxs}{maxdshift}           = ahapp::query_parameter("maxdshift");
    $Params{sxs}{bisectolshift}       = ahapp::query_parameter("bisectolshift");
    $Params{sxs}{searchstepwidth}     = ahapp::query_parameter("searchstepwidth");
    $Params{sxs}{maxdwidth}           = ahapp::query_parameter("maxdwidth");
    $Params{sxs}{bisectolwidth}       = ahapp::query_parameter("bisectolwidth");
    $Params{sxs}{minwidth}            = ahapp::query_parameter("minwidth");
    $Params{sxs}{nerrshift}           = ahapp::query_parameter("nerrshift");
    $Params{sxs}{nerrwidth}           = ahapp::query_parameter("nerrwidth");
    $Params{sxs}{shifterrfac}         = ahapp::query_parameter("shifterrfac");
    $Params{sxs}{widtherrfac}         = ahapp::query_parameter("widtherrfac");

    # sxspha2pi parameters
    $Params{sxs}{calcupi}             = ahapp::query_parameter("calcupi");
    $Params{sxs}{calcpi}              = ahapp::query_parameter("calcpi");
    $Params{sxs}{secphacol}           = ahapp::query_parameter("secphacol");
    $Params{sxs}{scaleepi}            = ahapp::query_parameter("scaleepi");
    $Params{sxs}{scalegrade}          = ahapp::query_parameter("scalegrade");
    $Params{sxs}{addepicol}           = ahapp::query_parameter("addepicol");
    $Params{sxs}{method}              = ahapp::query_parameter("method");
    $Params{sxs}{extended}            = ahapp::query_parameter("extended");
    $Params{sxs}{binwidth}            = ahapp::query_parameter("binwidth");
    $Params{sxs}{offset}              = ahapp::query_parameter("offset");
    $Params{sxs}{tlmax}               = ahapp::query_parameter("tlmax");
    $Params{sxs}{writetemp}           = ahapp::query_parameter("writetemp");

    # sxsperseus parameters
    $Params{sxs}{dgfile}              = ahapp::query_parameter("dgfile");
    $Params{sxs}{offsetfile}          = ahapp::query_parameter("offsetfile");
    $Params{sxs}{outrange}            = ahapp::query_parameter("outrange");

  } # end sxs parameters

  return 0;

} # GetInputParameters

sub CheckCalibrationFiles {

  # CALDB, if set, provides three environment variables.
  # It is here assumed that if the three environment variables
  # are present and valid, then CALDB is set properly
  
  # The variables are:
  # $CALDB        - Points to top directory of caldb installation
  # $CALDBALIAS   - Single FITS table of instrument name alias.
  # $CALDBCONFIG  - Single ".txt" file, which is configuration file.
  #                 Lists datasets on system by mission/instrument
  #                 and their location.

  if ( exists( $ENV{CALDB} ) ) {
    ah_debug "Environment variable CALDB = $ENV{CALDB}\n";
  } else {
    ah_debug "Environment variable CALDB not set\n";
  }
  if ( exists( $ENV{CALDBALIAS} ) ) {
    ah_debug "Environment variable CALDBALIAS = $ENV{CALDBALIAS}\n";
  } else {
    ah_debug "Environment variable CALDBALIAS not set\n";
  }
  if ( exists( $ENV{CALDBCONFIG} ) ) {
    ah_debug "Environment variable CALDBCONFIG = $ENV{CALDBCONFIG}\n";
  } else {
    ah_debug "Environment variable CALDBCONFIG not set\n";
  }
  
  # +++ Open the caldbconfig ASCII file
  #open ( CDBCNFG, $ENV{CALDBCONFIG}) ;
  #while (my $line = <CDBCNFG>) { print $line; }
  #close ( CDBCNFG );
  # +++ Compare mission (and instrument?) values

  # Check if user-input CALDB files exist and are okay to not
  # waste time in later stages
  my @instruments = [];
  if( $instrument{sxi} == 1 ) { push @instruments, "sxi"; }
  if( $instrument{hx1} == 1 or $instrument{hx2} == 1 ) { push @instruments, "hxi"; }
  if( $instrument{sg1} == 1 or $instrument{sg2} == 1 ) { push @instruments, "sgd"; }
  if( $instrument{sxs} == 1 ) { push @instruments, "sxs"; }

  foreach my $det (@instruments) {
    # iterate through sxi caldb files and verify if user-input
    foreach my $key (keys %{$caldb_files{$det}}) {
      ahlog::ah_debug "Searching for $det CALDB file: $key";
      my $det_calfile = $caldb_files{$det}{$key};
      if(uc($det_calfile) !~ /(CALDB|NONE|REFDATA)/) {
        unless(-e $det_calfile) {
          ahlog::ah_err "User-input CALDB File $det_calfile: " . $det_calfile . " does not exist.";
          ahlog::ah_err "Exiting.\n";
          return 1;
        }
        if($Params{verify_input}) {
          ahgen::check_fits_file($det_calfile);
          if ( ahgen::get_error_flag ) {
            ahlog::ah_err "User-input CALDB File $det_calfile: " . $det_calfile . " failed file verification (ftverify)";
                        return ahgen::get_error_flag;
          }
        }
      }
    }
  }

  return 0;
}

sub FindInputFiles {

  # Find all files in the input directory that match the stem

  ahlog::ah_out "Finding input files with stem $Params{steminputs} in $Params{indir}";

  my @filelist_unparsed = GetFileList( $Params{indir}, $Params{steminputs} );

  if ( $#filelist_unparsed == -1 ) {
    ahlog::ah_err  "No input files found with '$Params{steminputs}' stem" ;
    return 1;
  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "File(s) to check:";
    foreach my $filename ( sort @filelist_unparsed ) {
      ahlog::ah_debug "  $filename\n";
    }
  }

  my @filelist_parsed;

  # Load names of relevant files from input data directory
  foreach my $filename ( sort @filelist_unparsed ) {
    foreach my $filetype ( keys %patterns ) {
      # Skip any of the auxiliary files if we are not searching for them
      if ( $filetype eq 'attitude' or
           $filetype eq 'housekeeping' or
           $filetype eq 'extended_housekeeping' or
           $filetype eq 'makefilter' or
           $filetype eq 'orbit' or
           $filetype eq 'timfile' or
           $filetype eq 'obsgti' ) {
           if ( uc $Params{$filetype} ne 'DEFAULT' ) { next; }
      }
      if ( $filename =~ /$Params{steminputs}$patterns{$filetype}/ ) {

        # Strip off any "zip" extension
        # $2 is the second group in the regex above ($patterns{$filetype})
        if ( $2 ) {
          $filename =~ s/$2$//;
        }

        # If more than one file of a given type are expected
        # add to the list, taking the best one if any duplicates
        if ( ( ref( $files{$filetype} ) ) eq "ARRAY" ) {
          if ( grep { basename($filename) eq basename($_) } @{$files{$filetype}} ) {

            ahlog::ah_out "WARNING: Duplicate files found with name: " . basename($filename) ;

            # we already have a file with this name
            my $bestfile = getBestFileOfType( $filename, $filetype, $files{$filetype} );
            if ( !defined $bestfile ) {
              return 1;
            }
            ahlog::ah_out "WARNING: Using $bestfile" ;
            @{$files{$filetype}} = ( $bestfile, grep {basename($bestfile) ne basename($_)} @{$files{$filetype}} );
            @filelist_parsed = ( $bestfile, grep {basename($bestfile) ne basename($_)} @filelist_parsed );
          } else {
            push @{$files{$filetype}}, $filename;
            push @filelist_parsed, $filename;
          }
        } else {
          if ( $files{$filetype} ) {

            ahlog::ah_out "WARNING: Duplicate files found with name: " . basename($filename) ;

            # we already have a file with this name
            my $bestfile = getBestFileOfType( $filename, $filetype, [ $files{$filetype} ] );
            if ( !defined $bestfile ) {
              return 1;
            }
            ahlog::ah_out "WARNING: Using $bestfile" ;
            $files{$filetype} = $bestfile;
            @filelist_parsed  = ( $bestfile, grep {basename($bestfile) ne basename($_)} @filelist_parsed );
            } else {
              $files{$filetype} = $filename;
              push @filelist_parsed, $filename;
          }
        }
        last;
      }
    }
  }

  # Verify input files were found
  if ( !@filelist_parsed ) {
    ahlog::ah_err "Failed to find any input files matching $Params{steminputs}" ;
    return 1;
  }

  # Verify files as FITS files
  foreach my $filename ( sort @filelist_parsed ) {
    my $VerifyInputFile = VerifyInputFile( $filename );
    unless ( $VerifyInputFile == 0 ) {
      ahlog::ah_err "Error verifying input file $filename in VerifyInputFile" ;
      return $VerifyInputFile;
    }
  }

  return 0;
} # FindInputFiles

sub getBestFileOfType {

  my ( $filename, $filetype, $filelist ) = @_;

  # - If any one of them resides in an "archive-like" directory structure
  #   prefer it to any that do not.
  # - Otherwise take the first found and throw a BIG WARNING
  my $bestfile = $filename;
  my $archdir  = $archive_dirs{$filetype};
  my $newpatt  = catfile( $Params{indir}, $archive_dirs{$filetype},
                          $Params{steminputs} . $patterns{$filetype} );
  $newpatt = qr/$newpatt/;
  my @duplfiles = ( );
  foreach my $listfile ( $filename, grep { basename($filename) eq basename($_) } @$filelist ) {
    if ( $listfile =~ m#$newpatt# ) {
      $bestfile = $listfile;
      last;
    }
    push @duplfiles, $listfile;
  }
  if ( $bestfile eq $filename && $filename !~ m#$newpatt# ) {
    my $basefile = basename( $filename );
    ahlog::ah_err "Duplicate files named $basefile found in input directory:" ;
    map( ahlog::ah_err("    $_") , @duplfiles );
    ahlog::ah_err "Neither conforms to original archive directory structure." ;
    ahlog::ah_err "Cannot determine best file to use." ;
    return undef;
  }
  return $bestfile;
}

sub VerifyInputFile {

  my $filename = shift;

  unless ( $Params{verify_input} ) {
    if ( ahgen::getdebug ) {
      ahlog::ah_debug "VerifyInputFile Omitted for file $filename\n" ;
    }
    return 0;
  }

  # The FITS files (various suffixes) are checked with FTVERIFY

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning VerifyInputFile for input data files\n";
  }

  ahgen::check_fits_file ( $filename );
  if ( ahgen::get_error_flag ) {
    ahlog::ah_err "HIGH", "ftverify failed for file $filename\n" ;
    return ahgen::get_error_flag ;
  } 

  ahlog::ah_debug "ftverify passed for file $filename\n" ;

  return 0;

}    # VerifyInputFile

# Common section for GetFileList Task

{
  my ( @list, $fulldirx, $stem );

  sub GetFileList {
    ( $fulldirx, $stem ) = @_;
    @list  = {};
    $#list = -1;
    &File::Find::find( { wanted => \&Wanted, follow => 1, follow_skip => 2 }, $fulldirx );
    return @list;
  }

  sub Wanted {
    /^.*$stem.*\z/s &&
    ! /^\..*/s &&  # ignore files starting with a dot        
    -f $_ &&
    push @list, $File::Find::name;
  }

}    # GetFileList

sub CheckAuxilFiles {

  my $outdir = $Params{outdir}; 
  my $steminputs = $Params{steminputs}; 
  my $stemoutputs = $Params{stemoutputs};

  ahlog::ah_out "\nFinding Auxiliary Files\n\n";
  if ( ahgen::getdebug ) {
    ahlog::ah_debug "Attitude file              = $Params{attitude}\n";
    ahlog::ah_debug "Housekeeping file          = $Params{housekeeping}\n";
    ahlog::ah_debug "Extended housekeeping file = $Params{extended_housekeeping}\n";
    ahlog::ah_debug "Makefilter file            = $Params{makefilter}\n";
    ahlog::ah_debug "Orbit file                 = $Params{orbit}\n";
    ahlog::ah_debug "Time file                  = $Params{timfile}\n";
    ahlog::ah_debug "Observation GTI file       = $Params{obsgti}\n";
  }

  # Auxiliary File Processing
  my %auxilfile = (
    attitude              => ".att",
    housekeeping          => "gen_a0.hk",
    extended_housekeeping => ".ehk",
    makefilter            => ".mkf",
    orbit                 => ".orb",
    timfile               => ".tim",
    obsgti                => "_gen.gti"
  );

  foreach my $filename_auxil ( keys %auxilfile ) {

    ahlog::ah_out "\nFinding Auxiliary File $filename_auxil" ;

    # If "DEFAULT" is given, use the found file for this auxil type
    if ( uc( $Params{$filename_auxil} ) eq "DEFAULT" ) {
      $Params{$filename_auxil} = $files{$filename_auxil};
    }

    # Check that something was found for this type
    if ( !$Params{$filename_auxil} ) {
      if ( $filename_auxil eq 'attitude' ) {
        # Cannot calculate pointing or EHK without attitude file
        $Params{$filename_auxil} = "NONE";
        $files{$filename_auxil} = "NONE";
        ahlog::ah_out "MISSING : Auxiliary file $filename_auxil." ;
        if ( $Params{create_ehkmkf} ) {
          ahlog::ah_out "Cannot create filter files.";
          $Params{create_ehkmkf} = 0;
        }
        if ( $Params{calc_pointing} ) {
          ahlog::ah_out "Cannot calculate nominal pointing.";
          $Params{calc_pointing} = 0;
        }
        if ( uc $Params{coordevt}{stopsys} eq "SKY" ) {
          ahlog::ah_out "Setting coordevt stopsys to FOC.";
          $Params{coordevt}{stopsys} = "FOC";
        }
        next;
      } elsif ( $filename_auxil eq 'orbit' ) {
        # Cannot calculate EHK without orbit file
        $Params{$filename_auxil} = "NONE";
        $files{$filename_auxil} = "NONE";
        ahlog::ah_out "MISSING : Auxiliary file $filename_auxil." ;
        if ( $Params{create_ehkmkf} ) {
          ahlog::ah_out "Cannot create filter files.";
          $Params{create_ehkmkf} = 0;
        }
        next;
      } elsif ( $filename_auxil eq 'housekeeping' ) {
        # Cannot calculate MKF without HK files
        $Params{$filename_auxil} = "NONE";
        $files{$filename_auxil} = "NONE";
        ahlog::ah_out "MISSING : Auxiliary file $filename_auxil." ;
        if ( $Params{create_ehkmkf} ) {
          ahlog::ah_out "MISSING Auxiliary file $filename_auxil." ;
          ahlog::ah_out "Cannot create filter files.";
          $Params{create_ehkmkf} = 0;
        }
        next;
      } elsif ( $filename_auxil eq 'makefilter' or
                $filename_auxil eq 'extended_housekeeping' ) {
        if ( ! $Params{create_ehkmkf} and $General{stage2_switch} ) {
          ahlog::ah_out "MISSING Auxiliary file $filename_auxil." ;
          ahlog::ah_out "Stage II cannot be completed." ;
          $Params{$filename_auxil} = "NONE";
          $files{$filename_auxil} = "NONE";
         if ( $General{stage1_switch} == 0 ) {
           # Error if we are starting from stage 2
           return 1;
         }
        }
        next;
      } elsif ( $filename_auxil eq 'timfile' ) {
        $Params{$filename_auxil} = "NONE";
        $files{$filename_auxil} = "NONE";
        if ( $instrument{sxs} and
             $Params{sxs}{calcmxs} eq "yes" ) {
          ahlog::ah_out "MISSING Auxiliary file $filename_auxil." ;
          ahlog::ah_out "Cannot run mxsgti.";
          $Params{sxs}{calcmxs} = "no";
        }
        next;
      } # end missing file cases
      ahlog::ah_err "Auxiliary file $filename_auxil not found. Check input directory." ;
      return 1;
    }

    # Check that the file exists and verify it
    my $file_exists = 0;
    my $file_status = 0;
    fits_file_exists($Params{$filename_auxil}, $file_exists, $file_status);
    if ( $file_exists == 0 ) {
      ahlog::ah_err "Auxiliary file $filename_auxil not found: $Params{$filename_auxil}" ;
      return 1;
    }

    # Verify auxiliary files as FITS files
    my $VerifyInputFile = VerifyInputFile( $Params{$filename_auxil} );
    unless ( $VerifyInputFile == 0 ) {
      ahlog::ah_err "Error in VerifyInputFile" ;
      return $VerifyInputFile;
    }

    if ( $Params{create_ehkmkf} and
        (lc $filename_auxil eq "makefilter" or 
         lc $filename_auxil eq "extended_housekeeping" ) ) {
       # Skip EHK and MKF files if we are creating them
       next;
    }

    # Copy auxiliary files to output directory
    my $outname_auxil = form_output_file_name( $Params{$filename_auxil}, $outdir, $steminputs, $stemoutputs );
    ahgen::copy_fits_file( $Params{$filename_auxil}, $outname_auxil );
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "Error copying $Params{$filename_auxil} to $outname_auxil" ;
      return ahgen::get_error_flag ;
    }
    $Params{$filename_auxil} = $outname_auxil;
    $files{$filename_auxil} = $outname_auxil;

    # push the auxil file to the list of files to update keywords
    push( @filelist_output, $outname_auxil );
  }

  # Stage 2 or Stage 3 cannot be run if there is no MKF/EHK file
  if ( $files{makefilter} eq "NONE" or $files{extended_housekeeping} eq "NONE" ) {
     if ( $General{stage3_switch} != 0 ) {
       # Turn off stage 3 if starting from stage 1
       ahlog::ah_out "Stage III cannot be completed." ;
       $General{stage3_switch} = 0;
     }
  }

  ahlog::ah_out "\nUsing auxiliary files";
  ahlog::ah_out "Attitude file              = $files{attitude}\n";
  ahlog::ah_out "Housekeeping file          = $files{housekeeping}\n";
  ahlog::ah_out "Extended housekeeping file = $files{extended_housekeeping}\n" unless $Params{create_ehkmkf};
  ahlog::ah_out "Makefilter file            = $files{makefilter}\n" unless $Params{create_ehkmkf};
  ahlog::ah_out "Orbit file                 = $files{orbit}\n";
  ahlog::ah_out "Time file                  = $files{timfile}\n";
  ahlog::ah_out "Observation GTI file       = $files{obsgti}\n";
  ahlog::ah_out "End check auxiliary files";

  return 0;

}

############################
#      Main Processing 
############################

sub StageSwitcher {

  if ( ahgen::getdebug ) {
      ahlog::ah_debug "\nRunning StageSwitcher\n";
  }

  my $status = 0;
  my $indir = $Params{indir}; 
  my $outdir = $Params{outdir}; 
  my $steminputs = $Params{steminputs}; 
  my $stemoutputs = $Params{stemoutputs};

  # Copy the HK data needed regardless of starting stage
  # Copy the telemetry saturation GTI regardless of starting stage

  ahlog::ah_out "\n\nCopying houskeeping and GTI files from $indir to $outdir\n\n";
  if ( ( $instrument{hx1} == 1 ) || ( $instrument{hx2} == 1 ) ) {
    foreach my $det (qw( hx1 hx2 )) {
      if ( $instrument{$det} == 1 ) {
        if ( length($files{$det."_hk"}) gt 0 ) {
          my $outname_hxi_hk = form_output_file_name( $files{$det."_hk"}, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $files{$det."_hk"}, $outname_hxi_hk);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $outname_hxi_hk" ;
            return ahgen::get_error_flag;
          }
          $files{$det."_hk"} = $outname_hxi_hk;
        } else {
          ahlog::ah_out "\nNo input hk file for $det\n";
          if ( $General{stage2_switch} ) {
            ahlog::ah_out "Screening may not be accurate";
          }
        }
      }
    }

    if ( length($files{hxi_gtitel}) gt 0 ) {
      my $outname_hxi_gtitel = form_output_file_name( $files{hxi_gtitel}, $outdir, $steminputs, $stemoutputs );
      ahgen::copy_fits_file( $files{hxi_gtitel}, $outname_hxi_gtitel);
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "Error copying $outname_hxi_gtitel" ;
        return ahgen::get_error_flag;
      }
      $files{hxi_gtitel} = $outname_hxi_gtitel;
      # push the gti file to the list of files to update keywords
      push( @filelist_output, $outname_hxi_gtitel );
    } else {
      ahlog::ah_out "\nNo input telemetry saturation GTI file for SGD\n";
      if ( $General{stage2_switch} ) {
        ahlog::ah_out "Screening may not be accurate";
      }
    }
  }

  if ( ( $instrument{sg1} == 1 ) || ( $instrument{sg2} == 1 ) ) {
    foreach my $det (qw( sg1 sg2 )) {
      if ( $instrument{$det} == 1 ) {
        if ( length($files{$det."_hk"}) gt 0 ) {
          my $outname_sgd_hk = form_output_file_name( $files{$det."_hk"}, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $files{$det."_hk"}, $outname_sgd_hk);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $outname_sgd_hk" ;
            return ahgen::get_error_flag;
          }
          $files{$det."_hk"} = $outname_sgd_hk;
        } else {
          ahlog::ah_out "\nNo input hk file for $det\n";
          if ( $General{stage2_switch} ) {
            ahlog::ah_out "Screening may not be accurate";
          }
        }
      }
    }

    # Copy the telemetry saturation GTI
    if ( length($files{sgd_gtitel}) gt 0 ) {
      my $sgd_gtitel = form_output_file_name( $files{sgd_gtitel}, $outdir, $steminputs, $stemoutputs );
      ahgen::copy_fits_file( $files{sgd_gtitel}, $sgd_gtitel);
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "Error copying $sgd_gtitel to $Params{outdir}" ;
        return ahgen::get_error_flag;
      }
      # push the gti file to the list of files to update keywords
      push( @filelist_output, $sgd_gtitel );
    } else {
      ahlog::ah_out "\nNo input telemetry saturation GTI file for SGD\n";
      if ( $General{stage2_switch} ) {
        ahlog::ah_out "Screening may not be accurate";
      }
    }
  }

  if ( $instrument{sxi} == 1 ) {
    if ( length($files{sxi_hk}) gt 0 ) {
      my $outname_sxi_hk = form_output_file_name( $files{sxi_hk}, $outdir, $steminputs, $stemoutputs ) ;
      ahgen::copy_fits_file( $files{sxi_hk}, $outname_sxi_hk);
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "Error copying $outname_sxi_hk" ;
        return ahgen::get_error_flag;
      }
      $files{"sxi_hk"} = $outname_sxi_hk;
    } else {
      ahlog::ah_out "\nNo input hk file for sxi\n";
      if ( $General{stage2_switch} ) {
        ahlog::ah_out "Screening may not be accurate";
      }
    }

    # Copy the telemetry saturation GTI
    if ( length($files{sxi_gtitel}) gt 0 ) {
      my $sxi_gtitel = form_output_file_name( $files{sxi_gtitel}, $outdir, $steminputs, $stemoutputs );
      ahgen::copy_fits_file( $files{sxi_gtitel}, $sxi_gtitel);
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "Error copying $sxi_gtitel to $Params{outdir}" ;
        return ahgen::get_error_flag;
      }
      $files{sxi_gtitel} = $sxi_gtitel;
      # push the gti file to the list of files to update keywords
      push( @filelist_output, $sxi_gtitel );
    } else {
      ahlog::ah_out "\nNo input telemetry saturation GTI file for SXI\n";
      if ( $General{stage2_switch} ) {
        ahlog::ah_out "Cannot clean SXS data";
        $Params{numerrs} += 1;
        $instrument{sxi} = 0;
      }
    }

    if ( length($files{sxi_exposure}) gt 0 ) {
      my $sxi_exposure = form_output_file_name( $files{sxi_exposure}, $outdir, $steminputs, $stemoutputs );
      ahgen::copy_fits_file( $files{sxi_exposure}, $sxi_exposure);
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "Error copying $sxi_exposure to $Params{outdir}" ;
        return ahgen::get_error_flag;
      }
      $files{sxi_exposure} = $sxi_exposure;
      # push the exposure file to the list of files to update keywords
      push( @filelist_output, $sxi_exposure );
    } else {
      ahlog::ah_out "\nNo input exposure file for SXI\n";
      if ( $General{stage2_switch} and lc $Params{sxi}{calc_modegti} eq "yes" ) {
        ahlog::ah_out "Cannot clean SXI data";
        $Params{numerrs} += 1;
        $instrument{sxi} = 0;
      }
    }
  }

  if ( $instrument{sxs} == 1 ) {
    if ( length($files{sxs_hk}) gt 0 ) {
      my $outname_sxs_hk = form_output_file_name( $files{sxs_hk}, $outdir, $steminputs, $stemoutputs );
      ahgen::copy_fits_file( $files{sxs_hk}, $outname_sxs_hk);
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "Error copying $outname_sxs_hk" ;
        return ahgen::get_error_flag;
      }
      $files{"sxs_hk"} = $outname_sxs_hk;

    } else {
      ahlog::ah_out "\nNo input hk file for sxs\n";
      if ( $General{stage2_switch} ) {
        ahlog::ah_out "Screening may not be accurate";
      }
    }

    # If we aren't calculating the lost off GTI, copy it over to the output directory 
    if ( lc $Params{sxs}{calc_gtilost} eq "no" ) {
      if ( length($files{sxs_gtilost}) gt 0 ) {
        my $sxs_gtilost = form_output_file_name( $files{sxs_gtilost}, $outdir, $steminputs, $stemoutputs );
        ahgen::copy_fits_file( $files{sxs_gtilost}, $sxs_gtilost);
        if( ahgen::get_error_flag ) {
          ahlog::ah_err "Error copying $sxs_gtilost to $Params{outdir}" ;
          return ahgen::get_error_flag;
        }
        $files{sxs_gtilost} = $sxs_gtilost;
        # push the gtilost file to the list of files to update keywords
        push( @filelist_output, $sxs_gtilost );
      } else {
        ahlog::ah_out "\nNo input GTI lost file for SXS\n";
        if ( $General{stage2_switch} ) {
          ahlog::ah_out "Cannot clean SXS data";
          $Params{numerrs} += 1;
          $instrument{sxs} = 0;
        }
      }
    }

    # Copy the telemetry saturation GTI
    if ( length($files{sxs_gtitel}) gt 0 ) {
      my $sxs_gtitel = form_output_file_name( $files{sxs_gtitel}, $outdir, $steminputs, $stemoutputs );
      ahgen::copy_fits_file( $files{sxs_gtitel}, $sxs_gtitel);
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "Error copying $sxs_gtitel to $Params{outdir}" ;
        return ahgen::get_error_flag;
      }
      $files{sxs_gtitel} = $sxs_gtitel;
      # push the gti file to the list of files to update keywords
      push( @filelist_output, $sxs_gtitel );
    } else {
      ahlog::ah_out "\nNo input telemetry saturation GTI file for SXS\n";
      if ( $General{stage2_switch} ) {
        ahlog::ah_out "Cannot clean SXS data";
        $Params{numerrs} += 1;
        $instrument{sxs} = 0;
      }
    }
  }

  # Set up the nominal pointing parameters
  $coordinates{gen}{ra}   = $Params{ra};
  $coordinates{gen}{dec}  = $Params{dec};
  $coordinates{gen}{roll} = $Params{roll};

  # Recalculate the nominal pointing if specified
  if ( $Params{calc_pointing} ) {
    $status = DeterminePointing();
    unless ( $status == 0 ) {
      ahlog::ah_err "Error calculating Nominal Pointing" ;
      return $status;
    }
  }

  # Verify that ra, dec and roll are within range
  if ( $coordinates{gen}{ra} < 0 or $coordinates{gen}{ra} > 360 ) {
    ahlog::ah_err "Nominal pointing RA must be in range 0 <= RA < 360 (R.A. = $coordinates{gen}{ra})";
    return 1;
  }
  if ( $coordinates{gen}{dec} < -90 or $coordinates{gen}{dec} > 90 ) {
    ahlog::ah_err "Nominal pointing DEC must be in range -90 <= RA <= 90 (Dec. = $coordinates{gen}{dec})";
    return 1;
  }
  
  # Calculate the instrument specific optical axis coordinates
  # Calculate regardless of calc_optaxis parameter if we are creating an EHK file
  if ( $Params{calc_optaxis} or $Params{create_ehkmkf} ) {
    $status = OpticalAxis();
    unless ( $status == 0 ) {
      ahlog::ah_err "Error calculating Optical Axis" ;
      return $status;
    }
  }

  # Run relevant stages
  my $Stage1 = 0;
  my $Stage2 = 0;
  my $Stage3 = 0;

  # Create the EHK and MKF files
  if ( $Params{create_ehkmkf} ) {
    $status = CreateEHKMKF();
    unless ( $status == 0 ) {
      ahlog::ah_err "Error creating EHK and MKF file" ;
      return $status;
    }
  }

  if ( $General{stage1_switch} == 1 ) {
    $Stage1 = Stage1();
    unless ( $Stage1 == 0 ) {
      ahlog::ah_err "Error running Stage1" ;
      return $Stage1;
    }
  }

  if ( $General{stage2_switch} == 1 ) {
    # Check that we have a valid label for SGD
    # If not, skip SGD cleaning 
    if ( $instrument{sg1} == 1 or $instrument{sg2} == 1 ) {
      if ( $Params{sgd_mkflabel} !~ /#/ or 
           $Params{sgd_ehklabel} !~ /#/ or 
           $Params{sgd_evtlabel} !~ /#/ ) {
        ahlog::ah_out " *** Not a valid label for SGD. Label requires '#'.";
        ahlog::ah_out " *** Skipping SGD cleaning";
        $instrument{sg1} = 0;
        $instrument{sg2} = 0;
      }
    }
    # Check that we have a valid label for SXI
    # If not, skip SXI cleaning
    if ( $instrument{sxi} == 1 ) {
      if ( $Params{sxi_mkflabel} !~ /#/ or 
           $Params{sxi_ehklabel} !~ /#/ or 
           $Params{sxi_evtlabel} !~ /#/ ) {
        ahlog::ah_out " *** Not a valid label for SXI. Label requires '#'.";
        ahlog::ah_out " *** Skipping SXI cleaning";
        $instrument{sxi} = 0;
      }
    }
    $Stage2 = Stage2();
    unless ( $Stage2 == 0 ) {
      ahlog::ah_err "Error running Stage2" ;
      return $Stage2;
    }
  }

  # Calculate the region file
  if ( $Params{makeregion} ) {
    $status = MakeRegionFile();
    unless ( $status == 0 ) {
      ahlog::ah_err "Error Making Region File" ;
      return $status;
    }
  } else {
    $files{regionfile} = "NONE";
  }

  if ( $General{stage3_switch} == 1 ) {
    $Stage3 = Stage3();
    unless ( $Stage3 == 0 ) {
      ahlog::ah_err "Error running Stage3" ;
      return $Stage3;
    }
  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of StageSwitcher\n";
  }

  return 0;

}

sub Stage1 {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning Stage1\n";
  }

  ahlog::ah_info "HIGH", "\n===========================================================\n";
  ahlog::ah_info "HIGH", "                   Running Stage I\n";
  ahlog::ah_info "HIGH", "===========================================================\n\n";

  # Process HXI data
  if ( ( $instrument{hx1} == 1 ) ||
       ( $instrument{hx2} == 1 ) ) {

    # S1CalibrateHXI data
    my $S1HxiCalibrate = S1HxiCalibrate();
    unless ( $S1HxiCalibrate == 0) {
        ahlog::ah_err "Error running S1HxiCalibrate";
        return $S1HxiCalibrate;
    }
  }

  # Process SGD data
  if ( ( $instrument{sg1} == 1 ) ||
       ( $instrument{sg2} == 1 ) ) {

    # S1CalibrateSGD data
    my $S1SgdCalibrate = S1SgdCalibrate();
    unless ( $S1SgdCalibrate == 0) {
        ahlog::ah_err "Error running S1SgdCalibrate";
        return $S1SgdCalibrate;
    }
  }

  # Process SXI data 
  if (  $instrument{sxi} == 1 ) {
    # S1CalibrateSXI data
    my $S1SxiCalibrate = S1SxiCalibrate();
    unless ( $S1SxiCalibrate == 0) {
        ahlog::ah_err "Error running S1SxiCalibrate";
        return $S1SxiCalibrate;
    }
  }

  # Process SXS data 
  if (  $instrument{sxs} == 1 ) {
    # S1CalibrateSXS data
    my $S1SxsCalibrate = S1SxsCalibrate();
    unless ( $S1SxsCalibrate == 0) {
        ahlog::ah_err "Error running S1SxsCalibrate";
        return $S1SxsCalibrate;
    }
  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of Stage1\n";
  }

  return 0;
}


sub Stage2 {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning Stage2\n";
  }

  my $Stage2Hxi = 0;
  my $Stage2Sgd = 0;
  my $Stage2Sxs = 0;
  my $Stage2Sxi = 0;

  my @hxifiles = ();
  my @hxifiles_rec = ();
  my @hxifiles_exp = ();
  my @sgdfiles = ();
  my @sgdfiles_rec = ();
  my @sgdfiles_exp = ();
  my @sxsfiles = ();
  my @sxifiles = ();

  my $outdir = $Params{outdir};
  my $steminputs = $Params{steminputs};
  my $stemoutputs = $Params{stemoutputs};

  ahlog::ah_info "HIGH", "\n===========================================================\n";
  ahlog::ah_info "HIGH", "                   Running Stage II\n";
  ahlog::ah_info "HIGH", "===========================================================\n\n";

  # Process HXI data
  if ( ( $instrument{hx1} == 1 ) ||
       ( $instrument{hx2} == 1 ) ) {

    # If Stage 1 has not been run copy unfiltered "_uf" 
    # files from input to output directory
    if ($General{stage1_switch} == 0) {
      foreach my $det ( "hx1", "hx2" ) {
        if ( $instrument{$det} == 1) {
          if ( length(join("", @{$files{$det."_event_uf"}})) gt 0 ) {
            foreach my $hxi_event (@{$files{$det."_event_uf"}}) {
              my $outname_hxi_event = form_output_file_name( $hxi_event, $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $hxi_event, $outname_hxi_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_hxi_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @hxifiles, $outname_hxi_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_hxi_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_uf"}} = [];
            @{$files{$det."_event_uf"}} = @hxifiles;
          } else {
            ahlog::ah_out "\nNo input event file(s) for $det\n";
          }
          if ( length(join("", @{$files{$det."_event_rec"}})) gt 0 ) {
            foreach my $hxi_event (@{$files{$det."_event_rec"}}) {
              my $outname_hxi_event = form_output_file_name( $hxi_event, $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $hxi_event, $outname_hxi_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_hxi_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @hxifiles_rec, $outname_hxi_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_hxi_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_rec"}} = [];
            @{$files{$det."_event_rec"}} = @hxifiles_rec;
          } else {
            ahlog::ah_out "\nNo input reconstructed event file(s) for $det\n";
          }
        }
      }
    }

    # S2FilterHXI data
    my $S2HxiFilter = S2HxiFilter();
    unless ( $S2HxiFilter == 0) {
        ahlog::ah_err "Error running S2HxiFilter";
        return $S2HxiFilter;
    }
  }

  # Process SGD data
  if ( ( $instrument{sg1} == 1 ) ||
       ( $instrument{sg2} == 1 ) ) {

    # If Stage 1 has not been run copy unfiltered "_uf" 
    # files from input to output directory
    if ($General{stage1_switch} == 0) {
      foreach my $det ( "sg1", "sg2" ) {
        if ( $instrument{$det} == 1) {
          if ( length(join("", @{$files{$det."_event_uf"}})) gt 0 ) {
            foreach my $sgd_event (@{$files{$det."_event_uf"}}) {
              my $outname_sgd_event = form_output_file_name( $sgd_event, $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $sgd_event , $outname_sgd_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_sgd_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @sgdfiles, $outname_sgd_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_sgd_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_uf"}} = [];
            @{$files{$det."_event_uf"}} = @sgdfiles;
          } else {
            ahlog::ah_out "\nNo input event file(s) for $det\n";
          }
          if ( length(join("", @{$files{$det."_event_rec"}})) gt 0 ) {
            foreach my $sgd_event (@{$files{$det."_event_rec"}}) {
              my $outname_sgd_event = form_output_file_name( $sgd_event, $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $sgd_event, $outname_sgd_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_sgd_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @sgdfiles_rec, $outname_sgd_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_sgd_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_rec"}} = [];
            @{$files{$det."_event_rec"}} = @sgdfiles_rec;
          } else {
            ahlog::ah_out "\nNo input reconstructed event file(s) for $det\n";
          }
        }
      }
    }

    # S2FilterSGD data
    my $S2SgdFilter = S2SgdFilter();
    unless ( $S2SgdFilter == 0) {
        ahlog::ah_err "Error running S2SgdFilter";
        return $S2SgdFilter;
    }
  }

  # Process SXI data 
  if (  $instrument{sxi} == 1 ) {

    # If Stage 1 has not been run copy unfiltered "_uf" 
    # files from input to output directory
    if ($General{stage1_switch} == 0) {
      if ( length(join("", @{$files{sxi_event_uf}})) gt 0 ) {
        foreach my $sxi_event (@{$files{sxi_event_uf}}) {
          my $outname_sxi_event = form_output_file_name( $sxi_event, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $sxi_event, $outname_sxi_event);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $outname_sxi_event to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          push @sxifiles, $outname_sxi_event;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $outname_sxi_event );
        }
        # Update file array to moved files
        @{$files{sxi_event_uf}} = [];
        @{$files{sxi_event_uf}} = @sxifiles;
      } else {
        ahlog::ah_out "\nNo input event file(s) for sxi\n";
      }

      # If the SXS MXS GTI files are not being created, copy the MXS GTI
      # files over if the exist. If the fine GTI does not exist the screening may
      # not be accurate
      if ( lc $Params{sxi}{calc_modegti} eq "no" ) {
        # Copy the MXS GTI files only if the exist
        if ( $files{sxi_modegti} ) {
          my $sxi_modegti = form_output_file_name( $files{sxi_modegti}, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $files{sxi_modegti}, $sxi_modegti);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $sxi_modegti to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          $files{sxi_modegti} = $sxi_modegti;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $sxi_modegti );
        } else {
          ahlog::ah_out "\nNo input mode GTI file for SXI\n";
          if ( $General{stage2_switch} ) {
            ahlog::ah_out "Screening may not be accurate";
            $Params{numerrs} += 1;
          }
        }
        if ( $files{sxi_seggti} ) {
          my $sxi_seggti = form_output_file_name( $files{sxi_seggti}, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $files{sxi_seggti}, $sxi_seggti);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $sxi_seggti to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          $files{sxi_seggti} = $sxi_seggti;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $sxi_seggti );
        } else {
          ahlog::ah_out "\nNo input segment GTI file for SXI\n";
        }
      }
    }

    # S2FilterSXI data
    my $S2SxiFilter = S2SxiFilter();
    unless ( $S2SxiFilter == 0) {
        ahlog::ah_err "Error running S2SxiFilter";
        return $S2SxiFilter;
    }
  }

  # Process SXS data 
  if (  $instrument{sxs} == 1 ) {
    
       # Copy the unfiltered event files to the output directory if stage 1 was not run
    if ($General{stage1_switch} == 0) {
      if ( length(join("", @{$files{sxs_event_uf}})) gt 0 ) {
        foreach my $sxs_event (@{$files{sxs_event_uf}}) {
          my $outname_sxs_event = form_output_file_name( $sxs_event, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $sxs_event, $outname_sxs_event);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $outname_sxs_event to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          push @sxsfiles, $outname_sxs_event;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $outname_sxs_event );
        }
      } else {
        ahlog::ah_out "\nNo input event file(s) for sxs\n";
      }
      @{$files{sxs_event_uf}} = [];
      @{$files{sxs_event_uf}} = @sxsfiles;
      if ( $files{sxs_antico} ) {
        my $sxs_antico = $files{sxs_antico};
        my $outname_sxs_antico = form_output_file_name( $sxs_antico, $outdir, $steminputs, $stemoutputs );
        ahgen::copy_fits_file( $sxs_antico, $outname_sxs_antico);
        if( ahgen::get_error_flag ) {
          ahlog::ah_err "Error copying $outname_sxs_antico to $Params{outdir}" ;
          return ahgen::get_error_flag;
        }
        # push the antico file to the list of files to update keywords
        $files{sxs_antico} = $outname_sxs_antico;
        push( @filelist_output, $outname_sxs_antico );
      } else {
        ahlog::ah_out "\nNo input event file(s) for sxs\n";
      }
      # Update file array to moved files

      # If the SXS MXS GTI files are not being created, copy the MXS GTI
      # files over if the exist. If the fine GTI does not exist the screening may
      # not be accurate
      if ( lc $Params{sxs}{calcmxs} eq "no" ) {
        # Copy the MXS GTI files only if the exist
        if ( $files{sxs_gtimxfn} ) {
          my $sxs_gtimxfn = form_output_file_name( $files{sxs_gtimxfn}, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $files{sxs_gtimxfn}, $sxs_gtimxfn);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $sxs_gtimxfn to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          $files{sxs_gtimxfn} = $sxs_gtimxfn;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $sxs_gtimxfn );
        } else {
          ahlog::ah_out "\nNo input MXS Fine GTI file for SXS\n";
          if ( $General{stage2_switch} ) {
            ahlog::ah_out "Screening may not be accurate";
            $Params{numerrs} += 1;
          }
        }
        if ( $files{sxs_gtimxcs} ) {
          my $sxs_gtimxcs = form_output_file_name( $files{sxs_gtimxcs}, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $files{sxs_gtimxcs}, $sxs_gtimxcs);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $sxs_gtimxcs to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          $files{sxs_gtimxcs} = $sxs_gtimxcs;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $sxs_gtimxcs );
        }
      }

    }

    if ( $instrument{sxs} == 1 ) {
      # S2FilterSXS data
      my $S2SxsFilter = S2SxsFilter();
      unless ( $S2SxsFilter == 0) {
          ahlog::ah_err "Error running S2SxsFilter";
          return $S2SxsFilter;
      }
    }
  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of Stage2\n";
  }

  return 0;

}

sub Stage3 {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning Stage3\n";
  }

  my $Stage3Hxi = 0;
  my $Stage3Sgd = 0;
  my $Stage3Sxs = 0;
  my $Stage3Sxi = 0;

  my @hxifiles = ();
  my @hxifiles_rec = ();
  my @hxifiles_exp = ();
  my @sgdfiles = ();
  my @sgdfiles_rec = ();
  my @sgdfiles_exp = ();
  my @sxsfiles = ();
  my @sxifiles = ();

  my @hxifiles_cl = ();
  my @hxifiles_pse = ();
  my @sgdfiles_cl = ();
  my @sgdfiles_pse = ();
  my @sxsfiles_cl = ();
  my @sxifiles_cl = ();

  my $outdir = $Params{outdir};
  my $steminputs = $Params{steminputs};
  my $stemoutputs = $Params{stemoutputs};

  ahlog::ah_info "HIGH", "\n===========================================================\n";
  ahlog::ah_info "HIGH", "                   Running Stage III\n";
  ahlog::ah_info "HIGH", "===========================================================\n\n";

  # Process HXI data
  if ( ( $instrument{hx1} == 1 ) ||
       ( $instrument{hx2} == 1 ) ) {

    # If Stage 1 has not been run copy unfiltered "_uf" 
    # files from input to output directory
    if ($General{stage1_switch} == 0) {
      foreach my $det ( "hx1", "hx2" ) {
        if ( $instrument{$det} == 1) {
          if ( length(join("", @{$files{$det."_event_uf"}})) gt 0 ) {
            foreach my $hxi_event (@{$files{$det."_event_uf"}}) {
              my $outname_hxi_event = form_output_file_name( $hxi_event, $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $hxi_event, $outname_hxi_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_hxi_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @hxifiles, $outname_hxi_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_hxi_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_uf"}} = [];
            @{$files{$det."_event_uf"}} = @hxifiles;
          } else {
            ahlog::ah_out "\nNo input event file(s) for $det\n";
          }
          if ( length(join("", @{$files{$det."_event_rec"}})) gt 0 ) {
            foreach my $hxi_event (@{$files{$det."_event_rec"}}) {
              my $outname_hxi_event = form_output_file_name( $hxi_event, $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $hxi_event, $outname_hxi_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_hxi_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @hxifiles_rec, $outname_hxi_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_hxi_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_rec"}} = [];
            @{$files{$det."_event_rec"}} = @hxifiles_rec;
          } else {
            ahlog::ah_out "\nNo input reconstructed event file(s) for $det\n";
          }
        }
      }
    }
    # If Stage 2 has not been run copy cleaned event "_cl" files 
    # from input to output directory

    if ($General{stage2_switch} == 0) {
      foreach my $det ( "hx1", "hx2" ) {
        if ( $instrument{$det} == 1) {
          if ( length(join("", @{$files{$det."_event_cl"}})) gt 0 ) {
            foreach my $hxi_event (@{$files{$det."_event_cl"}}) {
              my $outname_hxi_event  = form_output_file_name( $hxi_event,  $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $hxi_event, $outname_hxi_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_hxi_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @hxifiles_cl, $outname_hxi_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_hxi_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_cl"}} = [];
            @{$files{$det."_event_cl"}} = @hxifiles_cl;
          } else {
            ahlog::ah_out "\nNo input clean event file(s) for $det\n";
          }
          if ( length(join("", @{$files{$det."_event_pse"}})) gt 0 ) {
            foreach my $hxi_event (@{$files{$det."_event_pse"}}) {
              my $outname_hxi_event  = form_output_file_name( $hxi_event,  $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $hxi_event, $outname_hxi_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_hxi_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @hxifiles_pse, $outname_hxi_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_hxi_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_pse"}} = [];
            @{$files{$det."_event_pse"}} = @hxifiles_pse;
          } else {
            ahlog::ah_out "\nNo input clean event file(s) for $det\n";
          }
        }
      }
    }

    # S3HXI data
    my $S3Hxi = S3Hxi();
    unless ( $S3Hxi == 0) {
        ahlog::ah_err "Error running S3Hxi";
        return $S3Hxi;
    }
  }

  # Process SGD data
  if ( ( $instrument{sg1} == 1 ) ||
       ( $instrument{sg2} == 1 ) ) {

    # If Stage 1 has not been run copy unfiltered "_uf" 
    # files from input to output directory
    if ($General{stage1_switch} == 0) {
      foreach my $det ( "sg1", "sg2" ) {
        if ( $instrument{$det} == 1) {
          if ( length(join("", @{$files{$det."_event_uf"}})) gt 0 ) {
            foreach my $sgd_event (@{$files{$det."_event_uf"}}) {
              my $outname_sgd_event = form_output_file_name( $sgd_event, $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $sgd_event, $outname_sgd_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_sgd_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @sgdfiles, $outname_sgd_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_sgd_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_uf"}} = [];
            @{$files{$det."_event_uf"}} = @sgdfiles;
          } else {
            ahlog::ah_out "\nNo input event file(s) for $det\n";
          }
          if ( length(join("", @{$files{$det."_event_rec"}})) gt 0 ) {
            foreach my $sgd_event (@{$files{$det."_event_rec"}}) {
              my $outname_sgd_event = form_output_file_name( $sgd_event, $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $sgd_event, $outname_sgd_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_sgd_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @sgdfiles_rec, $outname_sgd_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_sgd_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_rec"}} = [];
            @{$files{$det."_event_rec"}} = @sgdfiles_rec;
          } else {
            ahlog::ah_out "\nNo input reconstructed event file(s) for $det\n";
          }
        }
      }
    }
    # If Stage 2 has not been run copy cleaned event "_cl" files 
    # from input to output directory

    if ($General{stage2_switch} == 0) {
      foreach my $det ( "sg1", "sg2" ) {
        if ( $instrument{$det} == 1) {
          if ( length(join("", @{$files{$det."_event_cl"}})) gt 0 ) {
            foreach my $sgd_event (@{$files{$det."_event_cl"}}) {
              my $outname_sgd_event  = form_output_file_name( $sgd_event,  $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $sgd_event, $outname_sgd_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_sgd_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @sgdfiles_cl, $outname_sgd_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_sgd_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_cl"}} = [];
            @{$files{$det."_event_cl"}} = @sgdfiles_cl;
          } else {
            ahlog::ah_out "\nNo input clean event file(s) for $det\n";
          }
          if ( length(join("", @{$files{$det."_event_pse"}})) gt 0 ) {
            foreach my $sgd_event (@{$files{$det."_event_pse"}}) {
              my $outname_sgd_event  = form_output_file_name( $sgd_event,  $outdir, $steminputs, $stemoutputs );
              ahgen::copy_fits_file( $sgd_event, $outname_sgd_event);
              if( ahgen::get_error_flag ) {
                ahlog::ah_err "Error copying $outname_sgd_event to $Params{outdir}" ;
                return ahgen::get_error_flag;
              }
              push @sgdfiles_pse, $outname_sgd_event;
              # push the event file to the list of files to update keywords
              push( @filelist_output, $outname_sgd_event );
            }
            # Update file array to moved files
            @{$files{$det."_event_pse"}} = [];
            @{$files{$det."_event_pse"}} = @sgdfiles_pse;
          } else {
            ahlog::ah_out "\nNo input clean event file(s) for $det\n";
          }
        }
      }
    }

    # S3SGD data
    my $S3Sgd = S3Sgd();
    unless ( $S3Sgd == 0) {
        ahlog::ah_err "Error running S3Sgd";
        return $S3Sgd;
    }
  }

  # Process SXI data 
  if (  $instrument{sxi} == 1 ) {
    
    # If Stage 1 has not been run copy unfiltered "_uf" 
    # files from input to output directory

    if ($General{stage1_switch} == 0) {
      if ( length(join("", @{$files{sxi_event_uf}})) gt 0 ) {
        foreach my $sxi_event (@{$files{sxi_event_uf}}) {
          my $outname_sxi_event  = form_output_file_name( $sxi_event, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $sxi_event, $outname_sxi_event);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $outname_sxi_event to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          push @sxifiles, $outname_sxi_event;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $outname_sxi_event );
        }
        # Update file array to moved files
        @{$files{sxi_event_uf}} = [];
        @{$files{sxi_event_uf}} = @sxifiles;
      } else {
        ahlog::ah_out "\nNo input unfiltered event file(s) for sxi\n";
      }
    }

    # If Stage 2 has not been run copy cleaned event "_cl" files 
    # from input to output directory

    if ($General{stage2_switch} == 0) {
      if ( length(join("", @{$files{sxi_event_cl}})) gt 0 ) {
        foreach my $sxi_event (@{$files{sxi_event_cl}}) {
          my $outname_sxi_event  = form_output_file_name( $sxi_event, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $sxi_event, $outname_sxi_event);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $outname_sxi_event to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          push @sxifiles_cl, $outname_sxi_event;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $outname_sxi_event );
        }
        # Update file array to moved files
        @{$files{sxi_event_cl}} = [];
        @{$files{sxi_event_cl}} = @sxifiles_cl;
      } else {
        ahlog::ah_out "\nNo input clean event file(s) for sxi\n";
      }
      # If the SXI mode GTI files are not being created, copy the mode/seg GTI
      # files over if the exist. If the fine GTI does not exist the screening may
      # not be accurate
      if ( lc $Params{sxi}{calc_modegti} eq "no" ) {
        # Copy the MXS GTI files only if the exist
        if ( $files{sxi_modegti} ) {
          my $sxi_modegti = form_output_file_name( $files{sxi_modegti}, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $files{sxi_modegti}, $sxi_modegti);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $sxi_modegti to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          $files{sxi_modegti} = $sxi_modegti;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $sxi_modegti );
        } else {
          ahlog::ah_out "\nNo input mode GTI file for SXI\n";
          if ( $General{stage2_switch} ) {
            ahlog::ah_out "Screening may not be accurate";
            $Params{numerrs} += 1;
          }
        }
        if ( $files{sxi_seggti} ) {
          my $sxi_seggti = form_output_file_name( $files{sxi_seggti}, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $files{sxi_seggti}, $sxi_seggti);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $sxi_seggti to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          $files{sxi_seggti} = $sxi_seggti;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $sxi_seggti );
        } else {
          ahlog::ah_out "\nNo input segment GTI file for SXI\n";
        }
      }
    }

    # S3SXI data
    my $S3Sxi = S3Sxi();
    unless ( $S3Sxi == 0) {
        ahlog::ah_err "Error running S3Sxi";
        return $S3Sxi;
    }
  }

  # Process SXS data 
  if (  $instrument{sxs} == 1 ) {
    
    # If Stage 1 has not been run copy unfiltered "_uf" 
    # files from input to output directory

    if ($General{stage1_switch} == 0) {
      if ( length(join("", @{$files{sxs_event_uf}})) gt 0 ) {
        foreach my $sxs_event (@{$files{sxs_event_uf}}) {
          my $outname_sxs_event  = form_output_file_name( $sxs_event, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $sxs_event, $outname_sxs_event);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $outname_sxs_event to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          push @sxsfiles, $outname_sxs_event;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $outname_sxs_event );
        }
        # Update file array to moved files
        @{$files{sxs_event_uf}} = [];
        @{$files{sxs_event_uf}} = @sxsfiles;
      } else {
        ahlog::ah_out "\nNo input unfiltered event file(s) for sxs\n";
      }
    }

    # If Stage 2 has not been run copy cleaned event "_cl" files 
    # from input to output directory

    if ($General{stage2_switch} == 0) {
      if ( length(join("", @{$files{sxs_event_cl}})) gt 0 ) {
        foreach my $sxs_event (@{$files{sxs_event_cl}}) {
          my $outname_sxs_event  = form_output_file_name( $sxs_event, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $sxs_event, $outname_sxs_event);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $outname_sxs_event to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          push @sxsfiles_cl, $outname_sxs_event;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $outname_sxs_event );
        }
        # Update file array to moved files
        @{$files{sxs_event_cl}} = [];
        @{$files{sxs_event_cl}} = @sxsfiles_cl;
      } else {
        ahlog::ah_out "\nNo input clean event file(s) for sxs\n";
      }
      # If the SXS MXS GTI files are not being created, copy the MXS GTI
      # files over if the exist. If the fine GTI does not exist the screening may
      # not be accurate
      if ( lc $Params{sxs}{calcmxs} eq "no" ) {
        # Copy the MXS GTI files only if the exist
        if ( $files{sxs_gtimxfn} ) {
          my $sxs_gtimxfn = form_output_file_name( $files{sxs_gtimxfn}, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $files{sxs_gtimxfn}, $sxs_gtimxfn);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $sxs_gtimxfn to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          $files{sxs_gtimxfn} = $sxs_gtimxfn;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $sxs_gtimxfn );
        } else {
          ahlog::ah_out "\nNo input MXS Fine GTI file for SXS\n";
          if ( $General{stage2_switch} ) {
            ahlog::ah_out "Screening may not be accurate";
            $Params{numerrs} += 1;
          }
        }
        if ( $files{sxs_gtimxcs} ) {
          my $sxs_gtimxcs = form_output_file_name( $files{sxs_gtimxcs}, $outdir, $steminputs, $stemoutputs );
          ahgen::copy_fits_file( $files{sxs_gtimxcs}, $sxs_gtimxcs);
          if( ahgen::get_error_flag ) {
            ahlog::ah_err "Error copying $sxs_gtimxcs to $Params{outdir}" ;
            return ahgen::get_error_flag;
          }
          $files{sxs_gtimxcs} = $sxs_gtimxcs;
          # push the event file to the list of files to update keywords
          push( @filelist_output, $sxs_gtimxcs );
        }
      }
    }

    # S3SXS data
    my $S3Sxs = S3Sxs();
    unless ( $S3Sxs == 0) {
        ahlog::ah_err "Error running S3Sxs";
        return $S3Sxs;
    }
  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of Stage3\n";
  }

  return 0;

}

############################
#  Stage 1 - Calibration
############################

sub DeterminePointing {

  ahlog::ah_out "\nRunning DeterminePointing\n";

  my $status = 0;
  my $attfile       = $files{attitude};
  my $gtifile       = $files{obsgti};
  my $teldef        = $caldb_files{sxi}{teldeffile};

  my $ra_nom    = -999.99999;
  my $dec_nom   = -999.99999;
  my $pa_nom    = 0.0;

  # Need to query for a TelDef file for aspect
  if ( uc $teldef eq "CALDB" ) {
    my $dateobs;
    $dateobs = ahgen::get_keyword($attfile,"ATTITUDE","DATE-OBS");
    unless ( defined $dateobs ) {
      ahlog::ah_err "In file $attfile, DATE-OBS keyword not defined.\n";
      return 1;
    }
    my $teldeffile = ahfilterlib::call_quzcif($teldef,"SXI","-","TELDEF",$dateobs,"-");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "Error querying CALDB for SXI TelDef";
      return ahgen::get_error_flag;
    }
    $teldef = $teldeffile;
    unless ( defined $teldef ) { ahlog::ah_err "Error querying CALDB for SXI TelDef"; return 1; }
  }

  # Merge the pointing and attitude GTI
  my @ingtis = ( $gtifile . "[GTIPOINT]", $gtifile . "[GTIATT]" );
  ahlog::ah_info "HIGH", "Merging GTIPOINT and GTIATT for aspect calculation";
  my $aspectgti = $General{tempstem} . "aspect.gti";
  ahapp::add_temp_file($aspectgti);
  $status = ahfilterlib::merge_gti(\@ingtis,$aspectgti,"AND","GTI",{});
  if ( $status ) { ahlog::ah_err "merge_gti failed"; return $status; }

  # Run the aspect tool
  $status = ahgen::run_ftool( 'aspect', 
                              "attfile=$attfile",
                              "alignfile=$teldef",#SXITelDef
                              "gtis=$aspectgti",
                              "newattfile=NONE",
                              "nbins=100",
                              "binsize=0.01",
                              "maxrot=0.01",
                              "iterations=100",
                              "timemargin=32",
                              "boundgtis=yes",
                              "chatter=$ahapp::chatter");

  ###################
  # Check for errors
  ###################
  if ( $status ) {
    ahlog::ah_err 'Error in aspect!' ;
    return 1;
  }

  # Get the ra, dec and roll parameters
  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","aspect","ra");
  $ra_nom   = ahgen::get_tool_stdout();
  ahgen::run_ftool("pget","aspect","dec");
  $dec_nom  = ahgen::get_tool_stdout();
  ahgen::run_ftool("pget","aspect","roll");
  $pa_nom   = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  # set the nominal pointing parameters
  $coordinates{gen}{ra} = $ra_nom;
  $coordinates{gen}{dec} = $dec_nom;
  $coordinates{gen}{roll} = $pa_nom;

  ahlog::ah_debug "Nominal telescope pointing: " 
               . "R.A.=$ra_nom Dec=$dec_nom Roll Angle=$pa_nom" ;

  # Write the nominal pointing keywords to the attitude file
  ahgen::set_keyword($attfile,"ATTITUDE","RA_NOM",$ra_nom);
  ahgen::set_keyword($attfile,"ATTITUDE","DEC_NOM",$dec_nom);
  ahgen::set_keyword($attfile,"ATTITUDE","PA_NOM",$pa_nom);

  ahlog::ah_out "\nEnd of DeterminePointing\n";

  return 0;

}

sub OpticalAxis {

  ahlog::ah_out "\nRunning OpticalAxis\n";

  my $status = 0;

  # Read the DATE-OBS keyword from the obsgti file for any CALDB queries
  my $dateobs;
  $dateobs = ahgen::get_keyword($Params{obsgti},"GTIOBS","DATE-OBS");
  unless ( defined $dateobs ) {
    ahlog::ah_err "In file $Params{obsgti}, DATE-OBS keyword not defined.\n";
    return 1;
  }

  foreach my $ins ( qw( hx1 hx2 sg1 sg2 sxi sxs ) ) {

    # Skip the instrument if we are not processing it
    # unless we are calculating an EHK
    if ( ! $Params{create_ehkmkf} ) {
      if ( ! $instrument{$ins} ) { next; }
    }

    my $instrume;
    if ($ins eq 'hx1') {
      $instrume = "HXI1";
    } elsif ($ins eq 'hx2') {
      $instrume = "HXI2";
    } elsif ($ins eq 'sg1') {
      $instrume = "SGD1";
    } elsif ($ins eq 'sg2') {
      $instrume = "SGD2";
    } elsif ($ins eq 'sxi') {
      $instrume = "SXI";
    } elsif ($ins eq 'sxs') {
      $instrume = "SXS";
    }
    
    # Get teldef files

    my $teldef      = $caldb_files{$ins}{teldeffile};
    # Need to query for a TelDef file for aspect
    if ( uc $teldef eq "CALDB" ) {
      my $teldeffile = ahfilterlib::call_quzcif($teldef,$instrume,"-","TELDEF",$dateobs,"-");
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "Error querying CALDB for $instrume TelDef";
        return ahgen::get_error_flag;
      }
      $teldeffile =~ s/\[.*\]//g;
      $teldef = $teldeffile;
      unless ( defined $teldef ) { ahlog::ah_err "Error querying CALDB for $instrume TelDef"; return 1; }
    }

    ###################
    # Read keywords from TelDef file
    ###################
    my $optaxisx = ahgen::get_keyword( $teldef, "PRIMARY", 'OPTAXISX' );
    my $optaxisy = ahgen::get_keyword( $teldef, "PRIMARY", 'OPTAXISY' );
    my $optcoord = ahgen::get_keyword( $teldef, "PRIMARY", 'OPTCOORD' );

    unless ( defined $optaxisx ) { 
      ahlog::ah_err "Keyword OPTAXISX not found in $instrume TelDef file";
      return 1;
    }
    unless ( defined $optaxisy ) { 
      ahlog::ah_err "Keyword OPTAXISY not found in $instrume TelDef file";
      return 1;
    }
    unless ( defined $optcoord ) { 
      ahlog::ah_err "Keyword OPTCOORD not found in $instrume TelDef file";
      return 1;
    }
    
    # Check for valid instrument/optcoord
    # HXI, SXI, SXS start from DEC
    # SGD starts from FOC
    if ( ( $instrume eq "SGD1" or 
           $instrume eq "SGD2" ) and
          $optcoord ne "FOC" ) {
          ahlog::ah_err "Keyword OPTCOORD ($optcoord) invalid for $instrume" ;
          return 1;
       }

    if ( ( $instrume eq "HXI1" or 
           $instrume eq "HXI2" or 
           $instrume eq "SXI" or 
           $instrume eq "SXS" ) and
          $optcoord ne "DET" ) {
          ahlog::ah_err "Keyword OPTCOORD ($optcoord) invalid for $instrume" ;
          return 1;
       }

    my $optfocx = 0.0;
    my $optfocy = 0.0;
    my $optskyx = 0.0;
    my $optskyy = 0.0;
    my $ra_pnt = 0.0;
    my $dec_pnt = 0.0;

    # SGD starts from FOC
    if( $optcoord eq "DET") {

      ###########################################################
      # Compute the OPTFOCX/Y coordinates and write the corresponding
      # calculations to the job.par
      ###########################################################
      $status = ahgen::run_ftool( 'coordpnt',
                                   "input=$optaxisx,$optaxisy",
                                   "outfile=NONE",
                                   "telescop=$General{telescop}",
                                   "instrume=$instrume",
                                   "ra=0.0",
                                   "dec=0.0",
                                   "roll=0.0",
                                   "teldeffile=$teldef",
                                   "startsys=$optcoord",
                                   "stopsys=FOC",
                                   "logfile=NONE",
                                   "chatter=1");

      ###################
      # Check for errors
      ###################
      if ( $status ) {
        ahlog::ah_err 'Error in coordpnt!';
        return $status;
      }

      # Get the calculated coordinates
      ahgen::set_quiet(1);
      ahgen::run_ftool("pget","coordpnt","outx");
      $optfocx = ahgen::get_tool_stdout();
      ahgen::run_ftool("pget","coordpnt","outy");
      $optfocy = ahgen::get_tool_stdout();
      ahgen::set_quiet();

    } else {
      $optfocx = $optaxisx;
      $optfocy = $optaxisy;
    }

    ###########################################################
    # Compute the OPTSKYX/Y coordinates and write the corresponding
    # calculations to the job.par
    ###########################################################

    $status = ahgen::run_ftool( 'coordpnt',
                                "input=$optaxisx,$optaxisy",
                                "outfile=NONE",
                                "telescop=$General{telescop}",
                                "instrume=$instrume",
                                "ra=$coordinates{gen}{ra}",
                                "dec=$coordinates{gen}{dec}",
                                "roll=$coordinates{gen}{roll}",
                                "teldeffile=$teldef",
                                "startsys=$optcoord",
                                "stopsys=SKY",
                                "logfile=NONE",
                                "chatter=1");

    ###################
    # Check for errors
    ###################
    if ( $status ) {
      ahlog::ah_err 'Error in coordpnt!';
      return $status;
    }

    # Get the calculated coordinates
    ahgen::set_quiet(1);
    ahgen::run_ftool("pget","coordpnt","outx");
    $optskyx = ahgen::get_tool_stdout();
    ahgen::run_ftool("pget","coordpnt","outy");
    $optskyy = ahgen::get_tool_stdout();
    ahgen::set_quiet();

    ###########################################################
    # Compute the RA_PNT,DEC_PNT coordinates and write the corresponding
    # calculations to the job.par
    ###########################################################

    $status = ahgen::run_ftool( 'coordpnt',
                                "input=$optaxisx,$optaxisy",
                                "outfile=NONE",
                                "telescop=$General{telescop}",
                                "instrume=$instrume",
                                "ra=$coordinates{gen}{ra}",
                                "dec=$coordinates{gen}{dec}",
                                "roll=$coordinates{gen}{roll}",
                                "teldeffile=$teldef",
                                "startsys=$optcoord",
                                "stopsys=RADEC",
                                "logfile=NONE",
                                "chatter=1");

    ###################
    # Check for errors
    ###################
    if ( $status ) {
      ahlog::ah_err 'Error in coordpnt!';
      return $status;
    }

    # Get the calculated coordinates
    ahgen::set_quiet(1);
    ahgen::run_ftool("pget","coordpnt","outx");
    $ra_pnt = ahgen::get_tool_stdout();
    ahgen::run_ftool("pget","coordpnt","outy");
    $dec_pnt = ahgen::get_tool_stdout();
    ahgen::set_quiet();

    ###################
    # Write optical coordinates to job.par
    ###################
    # Do not write DET coordinate params for SGD
    unless ( $instrume eq "SGD1" or $instrume eq "SGD2" ) {
                $coordinates{$ins}{optdetx} = $optaxisx;
                $coordinates{$ins}{optdety} = $optaxisy;
              }
                $coordinates{$ins}{optfocx} = $optfocx;
                $coordinates{$ins}{optfocy} = $optfocy;
                $coordinates{$ins}{optskyx} = $optskyx;
                $coordinates{$ins}{optskyy} = $optskyy;
                $coordinates{$ins}{ra_pnt}  = $ra_pnt;
                $coordinates{$ins}{dec_pnt} = $dec_pnt;


    ahlog::ah_out "Optical axis angles for $instrume:\n";
    unless ( $instrume eq "SGD1" or $instrume eq "SGD2" ) {
             ahlog::ah_out "OPTDETX=$optaxisx OPTDETY=$optaxisy\n";
            }
            ahlog::ah_out "OPTFOCX=$optfocx  OPTFOCY=$optfocy\n";
            ahlog::ah_out "OPTSKYX=$optskyx  OPTSKYY=$optskyy\n";
            ahlog::ah_out "RA_PNT =$ra_pnt   DEC_PNT=$dec_pnt\n";

  }

  ahlog::ah_out "\nEnd of OpticalAxis\n";

  return 0;

}

sub S1HxiCalibrate {

  # The steps of the HXI calibration function are:
  # For each input file:
  #    a. Set up input/output parameters
  #    b. Read input file keywords
  #    c. Verify input file within CALDB time
  #    d. Run tools cams2att hxisgdsff, hxisgdpha, hxisgdexpand, hxievtid, coordevt

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nRunning S1HxiCalibrate\n";
  }

  my @calibrated_hxi_uf_evt_files = ( );

  my $det = "";

  my $status = 0;

  # Attitude/Orbit files
  my $attfile       = $Params{attitude};
  my $orbfile       = $Params{orbit};
  my $gtifile       = $files{obsgti};

  # HXI CALDB files
  my $hx1teldef    = $caldb_files{hx1}{teldeffile};
  my $hx2teldef    = $caldb_files{hx2}{teldeffile};
  my $remapfile    = $caldb_files{hxi}{remapfile};
  my $gainfile     = $caldb_files{hxi}{gainfile};  
  my $badpixfile   = $caldb_files{hxi}{badpixfile};
  my $fluorefile   = $caldb_files{hxi}{fluorefile};
  my $enecutfile   = $caldb_files{hxi}{enecutfile};
  
  # CAMS CALDB files
  my $cm1teldef    = $caldb_files{cm1}{teldeffile};
  my $cm2teldef    = $caldb_files{cm2}{teldeffile};
  my $camstempxy   = $caldb_files{cms}{camstempxy};

  ############# Populate File List #############
  my $ra = $coordinates{gen}{ra};
  my $dec = $coordinates{gen}{dec};
  my $roll = $coordinates{gen}{roll};
  my $hx1_optdetx = $coordinates{hx1}{optdetx};
  my $hx1_optdety = $coordinates{hx1}{optdety};
  my $hx1_optfocx = $coordinates{hx1}{optfocx};
  my $hx1_optfocy = $coordinates{hx1}{optfocy};
  my $hx1_optskyx = $coordinates{hx1}{optskyx};
  my $hx1_optskyy = $coordinates{hx1}{optskyy};
  my $hx1_ra_pnt  = $coordinates{hx1}{ra_pnt};
  my $hx1_dec_pnt = $coordinates{hx1}{dec_pnt};
  my $hx2_optdetx = $coordinates{hx2}{optdetx};
  my $hx2_optdety = $coordinates{hx2}{optdety};
  my $hx2_optfocx = $coordinates{hx2}{optfocx};
  my $hx2_optfocy = $coordinates{hx2}{optfocy};
  my $hx2_optskyx = $coordinates{hx2}{optskyx};
  my $hx2_optskyy = $coordinates{hx2}{optskyy};
  my $hx2_ra_pnt  = $coordinates{hx2}{ra_pnt};
  my $hx2_dec_pnt = $coordinates{hx2}{dec_pnt};

  my $hxipipeline_log = $General{logstem} . "hxi_stage1.log";

  # Default behaviour expects the ${indir}/sxi/event_uf structure
  # If the directory does not exist, parse the first sxi file name
  # found
  my @hxi_events = (@{$files{"hx1_event_uf"}},@{$files{hx2_event_uf}});
  unless (  @hxi_events ) { 
    # No HXI unfiltered event files
    ahlog::ah_out "No HXI event files found, skipping HXI calibration";
    $instrument{hx1} = 0;
    $instrument{hx2} = 0;
    return 0;
  }
  my $hxifile = $hxi_events[0];
  my $indir = dirname( $hxifile );

  # Grab the input parameters for the HXI calibration
  my @hxi_params = ();
  foreach my $key ( sort keys %{$Params{coordevt}} ) {
    if( $key eq "dattfile" ) { next; }
    push @hxi_params, "$key=$Params{coordevt}{$key}";
  }
  foreach my $key ( sort keys %{$Params{hxisgd}} ) {
    push @hxi_params, "$key=$Params{hxisgd}{$key}";
  }
  foreach my $key ( sort keys %{$Params{hxi}} ) {
    push @hxi_params, "$key=$Params{hxi}{$key}";
  }

  my $instrume;
  if ( $instrument{hx1} and $instrument{hx2} ) {
    $instrume = "HXI";
  } elsif ( $instrument{hx1} and ! $instrument{hx2}) {
    $instrume = "HXI1";
  } elsif ( ! $instrument{hx1} and $instrument{hx2}) {
    $instrume = "HXI2";
  } else {
    ahlog::ah_err "No valid hxi instrument";
    return 1;
  }

  ############# Calibrate each hxi file #############

  $status = ahgen::run_ftool("hxipipeline",
                             "indir=$indir",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{steminputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=1",
                             "exit_stage=1",
                             "instrument=$instrume",
                             "attitude=$attfile",
                             "orbit=$orbfile",
                             "obsgti=$gtifile",
                             "ra=$ra",
                             "dec=$dec",
                             "roll=$roll",
                             "hx1_optdetx=$hx1_optdetx",
                             "hx1_optdety=$hx1_optdety",
                             "hx1_optfocx=$hx1_optfocx",
                             "hx1_optfocy=$hx1_optfocy",
                             "hx1_optskyx=$hx1_optskyx",
                             "hx1_optskyy=$hx1_optskyy",
                             "hx1_ra_pnt=$hx1_ra_pnt",
                             "hx1_dec_pnt=$hx1_dec_pnt",
                             "hx2_optdetx=$hx2_optdetx",
                             "hx2_optdety=$hx2_optdety",
                             "hx2_optfocx=$hx2_optfocx",
                             "hx2_optfocy=$hx2_optfocy",
                             "hx2_optskyx=$hx2_optskyx",
                             "hx2_optskyy=$hx2_optskyy",
                             "hx2_ra_pnt=$hx2_ra_pnt",
                             "hx2_dec_pnt=$hx2_dec_pnt",
                             "hx1teldef=$hx1teldef",
                             "hx2teldef=$hx2teldef",
                             "remapfile=$remapfile",
                             "gainfile=$gainfile",
                             "badpixfile=$badpixfile",
                             "fluorefile=$fluorefile",
                             "enecutfile=$enecutfile",
                             "cm1teldef =$cm1teldef",
                             "cm2teldef =$cm2teldef",
                             "camstempxy=$camstempxy",
                             @hxi_params,
                             "stemreport=$General{stemreport}",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$hxipipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "hxipipeline failed."; return 1; }

  for my $det ("hx1", "hx2") {
    if( $instrument{$det} == 1 ) {
      @calibrated_hxi_uf_evt_files = ();
      $files{$det."_event_uf"} = [];
      # Find the calibrated HXI event files
      # Save lists of newly calibrated '_uf.evt' and '_uf.gti' files
      @calibrated_hxi_uf_evt_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{$det."_event_uf"});
      @{$files{$det."_event_uf"}} = @calibrated_hxi_uf_evt_files;

    }
  }

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","hxipipeline","numerrs");
  $Errors{hxi}{stage1} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  ############# stage 1 hxi end #############

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nEnd S1HxiCalibrate\n";
  }

  return ahgen::get_error_flag;

}

sub S1SgdCalibrate {

  # The steps of the SGD calibration function are:
  # For each input file:
  #   a. Set up input/output parameters
  #   b. Read input file keywords
  #   c. Verify input file within CALDB time
  #   d. Run tools hxisgdsff, hxisgdpha, hxisgdexpand, sgdevtid

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nRunning S1SgdCalibrate\n";
  }

  my @calibrated_sgd_uf_evt_files = ( );

  my $status = 0;

  my $gtifile       = $files{obsgti};

  # SGD CALDB files
  my $remapfile    = $caldb_files{sgd}{remapfile};
  my $gainfile     = $caldb_files{sgd}{gainfile};
  my $badpixfile   = $caldb_files{sgd}{badpixfile};
  my $fluorefile   = $caldb_files{sgd}{fluorefile};
  my $probseqfile  = $caldb_files{sgd}{probseqfile};
  my $probfovfile  = $caldb_files{sgd}{probfovfile};

  my $ra = $coordinates{gen}{ra};
  my $dec = $coordinates{gen}{dec};
  my $roll = $coordinates{gen}{roll};
  my $sg1_optfocx = $coordinates{sg1}{optfocx};
  my $sg1_optfocy = $coordinates{sg1}{optfocy};
  my $sg1_optskyx = $coordinates{sg1}{optskyx};
  my $sg1_optskyy = $coordinates{sg1}{optskyy};
  my $sg1_ra_pnt  = $coordinates{sg1}{ra_pnt};
  my $sg1_dec_pnt = $coordinates{sg1}{dec_pnt};
  my $sg2_optfocx = $coordinates{sg2}{optfocx};
  my $sg2_optfocy = $coordinates{sg2}{optfocy};
  my $sg2_optskyx = $coordinates{sg2}{optskyx};
  my $sg2_optskyy = $coordinates{sg2}{optskyy};
  my $sg2_ra_pnt  = $coordinates{sg2}{ra_pnt};
  my $sg2_dec_pnt = $coordinates{sg2}{dec_pnt};

  my $sgdpipeline_log = $General{logstem} . "sgd_stage1.log";

  # Default behaviour expects the ${indir}/sxi/event_uf structure
  # If the directory does not exist, parse the first sxi file name
  # found
  my @sgd_events = (@{$files{"sg1_event_uf"}},@{$files{sg2_event_uf}});
  unless (  @sgd_events ) { 
    # No SGD unfiltered event files
    ahlog::ah_out "No SGD event files found, skipping SGD calibration";
    $instrument{sg1} = 0;
    $instrument{sg2} = 0;
    return 0;
  }
  my $sgdfile = $sgd_events[0];
  my $indir = dirname( $sgdfile );

  # Grab the input parameters for the SGD calibration
  my @sgd_params = ();
  foreach my $key ( sort keys %{$Params{hxisgd}} ) {
    push @sgd_params, "$key=$Params{hxisgd}{$key}";
  }
  foreach my $key ( sort keys %{$Params{sgd}} ) {
    push @sgd_params, "$key=$Params{sgd}{$key}";
  }

  my $instrume;
  if ( $instrument{sg1} and $instrument{sg2} ) {
    $instrume = "SGD";
  } elsif ( $instrument{sg1} and ! $instrument{sg2}) {
    $instrume = "SGD1";
  } elsif ( ! $instrument{sg1} and $instrument{sg2}) {
    $instrume = "SGD2";
  } else {
    ahlog::ah_err "No valid sgd instrument";
    return 1;
  }

  ############# Calibrate each sgd file #############

  $status = ahgen::run_ftool("sgdpipeline",
                             "indir=$indir",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{steminputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=1",
                             "exit_stage=1",
                             "instrument=$instrume",
                             "obsgti=$gtifile",
                             "ra=$ra",
                             "dec=$dec",
                             "roll=$roll",
                             "sg1_optfocx=$sg1_optfocx",
                             "sg1_optfocy=$sg1_optfocy",
                             "sg1_optskyx=$sg1_optskyx",
                             "sg1_optskyy=$sg1_optskyy",
                             "sg1_ra_pnt=$sg1_ra_pnt",
                             "sg1_dec_pnt=$sg1_dec_pnt",
                             "sg2_optfocx=$sg2_optfocx",
                             "sg2_optfocy=$sg2_optfocy",
                             "sg2_optskyx=$sg2_optskyx",
                             "sg2_optskyy=$sg2_optskyy",
                             "sg2_ra_pnt=$sg2_ra_pnt",
                             "sg2_dec_pnt=$sg2_dec_pnt",
                             "remapfile=$remapfile",
                             "gainfile=$gainfile",
                             "badpixfile=$badpixfile",
                             "fluorefile=$fluorefile",
                             "probseqfile=$probseqfile",
                             "probfovfile=$probfovfile",
                             @sgd_params,
                             "stemreport=$General{stemreport}",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$sgdpipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "sgdpipeline failed."; return 1; }

  for my $det ("sg1", "sg2") {
    if( $instrument{$det} == 1 ) {
      @calibrated_sgd_uf_evt_files = ();
      $files{$det."_event_uf"} = [];
      # Find the calibrated SGD event files
      # Save lists of newly calibrated '_uf.evt' and '_uf.gti' files
      @calibrated_sgd_uf_evt_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{$det."_event_uf"});
      @{$files{$det."_event_uf"}} = @calibrated_sgd_uf_evt_files;

    }
  }

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","sgdpipeline","numerrs");
  $Errors{sgd}{stage1} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  ############# stage 1 sgd end #############

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nEnd S1SgdCalibrate\n";
  }

  return ahgen::get_error_flag;

}

sub S1SxiCalibrate {

  # The steps of the SXI calibration function are:
  # For each input file:
  #   a. Set up input/output parameters
  #   b. Read input file keywords
  #   c. Verify input file within CALDB time
  #   d. Run tools coordevt, sxiphas, sxiflagpix sxipi searchflickpix, sxiflagpix, sxipi

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nRunning S1SxiCalibrate\n";
  }

  my @calibrated_sxi_uf_evt_files = ( );
  my @calibrated_sxi_uf_gti_files = ( );

  my $status = 0;

  my $sxipipeline_log = $General{logstem} . "sxi_stage1.log";

  #############  setup  parameters for each tool #############

  # Attitude/Orbit files
  my $attfile       = $files{attitude};
  my $orbfile       = $files{orbit};
  my $ehkfile       = $files{extended_housekeeping};
  my $gtifile       = $files{obsgti};

  # SXI HK file
  my $hkfile       = $files{sxi_hk};

  # SXI CALDB files
  my $teldeffile    = $caldb_files{sxi}{teldeffile};

  my $maskfile      = $caldb_files{sxi}{maskfile};
  my $badpixfile    = $caldb_files{sxi}{badpixfile};

  my $vtevnoddfile  = $caldb_files{sxi}{vtevnodd};
  my $ctifile       = $caldb_files{sxi}{ctifile};
  my $chtrailfile   = $caldb_files{sxi}{chtrailfile};
  my $spthfile      = $caldb_files{sxi}{spthfile};
  my $gainfile      = $caldb_files{sxi}{gainfile};
  my $patternfile   = $caldb_files{sxi}{patternfile};

  my $ra = $coordinates{gen}{ra};
  my $dec = $coordinates{gen}{dec};
  my $roll = $coordinates{gen}{roll};
  my $optdetx = $coordinates{sxi}{optdetx};
  my $optdety = $coordinates{sxi}{optdety};
  my $optfocx = $coordinates{sxi}{optfocx};
  my $optfocy = $coordinates{sxi}{optfocy};
  my $optskyx = $coordinates{sxi}{optskyx};
  my $optskyy = $coordinates{sxi}{optskyy};
  my $ra_pnt  = $coordinates{sxi}{ra_pnt};
  my $dec_pnt = $coordinates{sxi}{dec_pnt};

  # Default behaviour expects the ${indir}/sxi/event_uf structure
  # If the directory does not exist, parse the first sxi file name
  # found
  unless ( @{$files{sxi_event_uf}} ) { 
    # No SXI unfiltered event files
    ahlog::ah_out "No SXI event files found, skipping SXI calibration";
    $instrument{sxi} = 0;
    return 0;
  }
  my $indir = dirname( ${$files{sxi_event_uf}}[0] );

  # Grab the input parameters for the SXI calibration
  my @sxi_params = ();
  foreach my $key ( sort keys %{$Params{coordevt}} ) {
    push @sxi_params, "$key=$Params{coordevt}{$key}";
  }
  foreach my $key ( sort keys %{$Params{sxi}} ) {
    push @sxi_params, "$key=$Params{sxi}{$key}";
  }

  ############# Calibrate each sxi file #############

  @{$files{sxi_event_uf}} = @calibrated_sxi_uf_evt_files;
  $status = ahgen::run_ftool("sxipipeline",
                             "indir=$indir",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{steminputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=1",
                             "exit_stage=1",
                             "attitude=$attfile",
                             "extended_housekeeping=$ehkfile",
                             "orbit=$orbfile",
                             "obsgti=$gtifile",
                             "housekeeping=$hkfile",
                             "ra=$ra",
                             "dec=$dec",
                             "roll=$roll",
                             "optdetx=$optdetx",
                             "optdety=$optdety",
                             "optfocx=$optfocx",
                             "optfocy=$optfocy",
                             "optskyx=$optskyx",
                             "optskyy=$optskyy",
                             "ra_pnt=$ra_pnt",
                             "dec_pnt=$dec_pnt",
                             "teldeffile=$teldeffile",
                             "badpixfile=$badpixfile",
                             "maskfile=$maskfile",
                             "vtevnoddfile=$vtevnoddfile",
                             "ctifile=$ctifile",
                             "chtrailfile=$chtrailfile",
                             "spthfile=$spthfile",
                             "gainfile=$gainfile",
                             "patternfile=$patternfile",
                             @sxi_params,
                             "stemreport=$General{stemreport}",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$sxipipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "sxipipeline failed."; return 1; }

  # Find the calibrated SXI event files
  @calibrated_sxi_uf_evt_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{sxi_event_uf});

  # Save lists of newly calibrated '_uf.evt' and '_uf.gti' files
  @{$files{sxi_event_uf}} = @calibrated_sxi_uf_evt_files;

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","sxipipeline","numerrs");
  $Errors{sxi}{stage1} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  ############# stage 1 sxi end #############

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nEnd S1SxiCalibrate\n";
  }

  return ahgen::get_error_flag;

}

sub S1SxsCalibrate {

  # The steps of the SXS calibration function are:
  # Run mxsgti
  # Run sxsanticopi
  # For each input file:
  #   a. Set up input/output parameters
  #   b. Read input file keywords
  #   c. Verify input file within CALDB time
  #   d. Run tools coordevt, sxsflagpix, sxssecid, 
  #   e. Extract pixel 12 and run sxsgain
  #   f. run sxspha2pi, sxsflagpix, sxssecid

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nRunning S1SxsCalibrate\n";
  }

  my @calibrated_sxs_uf_evt_files = ( );
  my @calibrated_sxs_uf_gti_files = ( );

  my $status = 0;

  my $sxspipeline_log = $General{logstem} . "sxs_stage1.log";

  #############  setup  parameters #############

  # Attitude/Orbit files
  my $attfile       = $files{attitude};
  my $orbfile       = $files{orbit};
  my $gtifile       = $files{obsgti};
  my $timfile       = $files{timfile};

  my $hkfile        = $files{sxs_hk};

  # SXS CALDB files
  my $teldef        = $caldb_files{sxs}{teldeffile};
  my $coeftimefile  = $caldb_files{sxs}{coeftime};
  my $pixdeffile    = $caldb_files{sxs}{pixdeffile};
  my $gainfile      = $caldb_files{sxs}{gainfile};
  my $scalefile     = $caldb_files{sxs}{scalefile};
  my $pulsefile     = $caldb_files{sxs}{pulsefile};
  my $linefitfile   = $caldb_files{sxs}{linefitfile};
  my $gainantfile   = $caldb_files{sxs}{gainantfile};

  # Coordinate parameters
  my $ra = $coordinates{gen}{ra};
  my $dec = $coordinates{gen}{dec};
  my $roll = $coordinates{gen}{roll};
  my $optdetx = $coordinates{sxs}{optdetx};
  my $optdety = $coordinates{sxs}{optdety};
  my $optfocx = $coordinates{sxs}{optfocx};
  my $optfocy = $coordinates{sxs}{optfocy};
  my $optskyx = $coordinates{sxs}{optskyx};
  my $optskyy = $coordinates{sxs}{optskyy};
  my $ra_pnt  = $coordinates{sxs}{ra_pnt};
  my $dec_pnt = $coordinates{sxs}{dec_pnt};

  # Default behaviour expects the ${indir}/sxs/event_uf structure
  # If the directory does not exist, parse the first sxs file name
  # found
  unless ( @{$files{sxs_event_uf}} ) { 
    # No SXS unfiltered event files
    ahlog::ah_out "No SXS event files found, skipping SXS calibration";
    $instrument{sxs} = 0;
    return 0;
  }
  my $indir = dirname( ${$files{sxs_event_uf}}[0] );

  # Grab the input parameters for the SXI calibration
  my @sxs_params = ();
  foreach my $key ( sort keys %{$Params{coordevt}} ) {
    push @sxs_params, "$key=$Params{coordevt}{$key}";
  }
  foreach my $key ( sort keys %{$Params{sxs}} ) {
    push @sxs_params, "$key=$Params{sxs}{$key}";
  }

  ############# Calibrate each sxs file #############

  @{$files{sxs_event_uf}} = @calibrated_sxs_uf_evt_files;
  $status = ahgen::run_ftool("sxspipeline",
                             "indir=$indir",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{steminputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=1",
                             "exit_stage=1",
                             "attitude=$attfile",
                             "orbit=$orbfile",
                             "timfile=$timfile",
                             "obsgti=$gtifile",
                             "housekeeping=$hkfile",
                             "ra=$ra",
                             "dec=$dec",
                             "roll=$roll",
                             "optdetx=$optdetx",
                             "optdety=$optdety",
                             "optfocx=$optfocx",
                             "optfocy=$optfocy",
                             "optskyx=$optskyx",
                             "optskyy=$optskyy",
                             "ra_pnt=$ra_pnt",
                             "dec_pnt=$dec_pnt",
                             "teldeffile=$teldef",
                             "pixdeffile=$pixdeffile",
                             "gainfile=$gainfile",
                             "scalefile=$scalefile",
                             "pulsefile=$pulsefile",
                             "linefitfile=$linefitfile",
                             "gainantfile=$gainantfile",
                             @sxs_params,
                             "stemreport=$General{stemreport}",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$sxspipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "sxspipeline failed."; return 1; }

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","sxspipeline","numerrs");
  $Errors{sxs}{stage1} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  # Find the calibrated SXI event files

  ############# stage 1 sxs end #############
  @calibrated_sxs_uf_evt_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{sxs_event_uf});
  my ($calibrated_sxs_ac_evt_files) = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{sxs_antico});
  
  # Save lists of newly calibrated '_uf.evt' and '_uf.gti' files
  @{$files{sxs_event_uf}} = @calibrated_sxs_uf_evt_files;
  $files{sxs_antico}      = $calibrated_sxs_ac_evt_files;

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nEnd S1SxsCalibrate\n";
  }

  return ahgen::get_error_flag;

}

############################
#    Stage 2 - Screening
############################

sub CreateEHKMKF {

  ahlog::ah_out "\nRunning CreateEHKMKF\n";

  my $status = 0;

  my $ahfilter_log  = $General{logstem} . "ahfilter.log";
  my $ahmkehk_log   = $General{logstem} . "ahmkehk.log";
  my $mkfconf_ascii = $General{tempstem} . "mkfconf.ascii";
  ahapp::add_temp_file($mkfconf_ascii);

  my $attfile       = $files{attitude};
  my $orbfile       = $files{orbit};
  my $gtifile       = $files{obsgti};
  my $hx1_hk        = $files{hx1_hk};
  my $hx2_hk        = $files{hx2_hk};
  my $teldef        = $caldb_files{sxi}{teldeffile};
  my $mkfconf       = $caldb_files{gen}{mkfconf};
  my $cor2file      = $caldb_files{gen}{cor2file};
  my $cor3file      = $caldb_files{gen}{cor3file};
  my $saafile       = $caldb_files{gen}{saafile};
  my $leapsecfile   = $caldb_files{gen}{leapsecfile};
  my $optaxis       = join(",",
    $coordinates{hx1}{optfocx},
    $coordinates{hx1}{optfocy},
    $coordinates{hx2}{optfocx},
    $coordinates{hx2}{optfocy},
    $coordinates{sxi}{optfocx},
    $coordinates{sxi}{optfocy},
    $coordinates{sxs}{optfocx},
    $coordinates{sxs}{optfocy},
  );

  my $infileroot = catfile($Params{outdir},$Params{stemoutputs});
  my $mkf = catfile($Params{outdir},"$Params{stemoutputs}.mkf");
  my $ehk = catfile($Params{outdir},"$Params{stemoutputs}.ehk");
  my $ehk2= catfile($Params{outdir},"$Params{stemoutputs}.ehk2");

  # Set up a 1 second bin size
  my $binlc = 1.0;

  # Read the tstart and tstop parameters from observation GTI file
  my $ehk_start = ahgen::get_keyword($gtifile,"GTIOBS","TSTART");
  unless ( defined $ehk_start ) {
    ahlog::ah_out"In file '$gtifile', could not read keyword TSTART";
    ahlog::ah_out"Skipping file.";
    return;
  }
  my $ehk_stop = ahgen::get_keyword($gtifile,"GTIOBS","TSTOP");
  unless ( defined $ehk_stop ) {
    ahlog::ah_out"In file '$gtifile', could not read keyword TSTOP";
    ahlog::ah_out"Skipping file.";
    return;
  }
  $hk_times{ehk_start}=$ehk_start-60.0;
  $hk_times{ehk_stop}=$ehk_stop+60.0;

  # Read the tstart and tstop parameters from the attitude file
  my $attstart = ahgen::get_keyword($attfile,"ATTITUDE","TSTART");
  unless ( defined $attstart ) {
    ahlog::ah_out"In file '$gtifile', could not read keyword TSTART";
    ahlog::ah_out"Skipping file.";
    return;
  }
  my $attstop = ahgen::get_keyword($attfile,"ATTITUDE","TSTOP");
  unless ( defined $attstop ) {
    ahlog::ah_out"In file '$attfile', could not read keyword TSTOP";
    ahlog::ah_out"Skipping file.";
    return;
  }
  if ($attstart > $hk_times{ehk_start}) {
      $hk_times{ehk_start}=$attstart;
      ahlog::ah_info "LOW", "Adjusting ehk start to att TSTART $attstart.";
  }
  if ($attstop < $hk_times{ehk_stop}) {
      $hk_times{ehk_stop}= $attstop;
      ahlog::ah_info "LOW", "Adjusting ehk stop to att TSTOP $attstop.";
  }
  if ($hk_times{ehk_stop} < $hk_times{ehk_start}) {
      ahlog::ah_err "ehk stop less than ehk start ($attstop < $attstart).";
      return 1;
  }

  # Calculate the TRIGGER_RATE column for HXI1 and HXI2:
  # 
  # In HXI1 HK files:
  #    extension: HK_HXI1_CAM_SCL
  #    column for trigger: HXI1_CAM_SCL_TRG1
  #    column for livetime: HXI1_CAM_SCL_LIVETIME
  # In HXI2 HK files:
  #    extension: HK_HXI2_CAM_SCL
  #    column for trigger: HXI2_CAM_SCL_TRG1
  #    column for livetime: HXI2_CAM_SCL_LIVETIME
  # 
  # Since they are scaler values, we have to calculate differential value to
  # obtain the trigger rate.
  # 
  # For example, the trigger rate is calculated as follows:
  # 
  #   TriggerRate[i] = ( HXI1_CAM_SCL_TRG1[i] - HXI1_CAM_SCL_TRG1[i-1] ) / ( HXI1_CAM_SCL_LIVETIME[i]*819.2e-6 - HXI1_CAM_SCL_LIVETIME[i-1]*819.2e-6)
  # 
  # Here, 819.2e-6 s corresponds to the LSB (least significant bit) of the 
  # live time scaler.  Also, please note that this trigger rate includes 
  # both signal and background, so it is dominated by the signal when HXI 
  # observes bright sources like Crab.
  if ( $hx1_hk and $instrument{hx1} == 1 ) {

    my $outfile = $General{tempstem} . basename($hx1_hk) . "_ftcopy";
    ahapp::add_temp_file($outfile);
    ahlog::ah_out "Calculating TRIGGER_RATE for HXI1.";
    my $status = ahgen::run_ftool( 'ftcopy',
                                   "infile=$hx1_hk\[HK_HXI1_CAM_SCL][col *;HXI1_TRIGGER_RATE(D)=#NULL]",
                                   "outfile=$outfile",
                                   "copyall=yes",
                                   "clobber=yes");

    # The HK file has already been copied to the output directory
    # Just overwrite the new file.
    ahgen::copy_fits_file( $outfile, $hx1_hk );
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "Error copying $outfile to $hx1_hk" ;
      return ahgen::get_error_flag ;
    }
    my $naxis2 = ahgen::get_keyword($hx1_hk,"HK_HXI1_CAM_SCL","NAXIS2");
    unless ( defined $naxis2 ) {
      ahlog::ah_out"In file '$hx2_hk\[HK_HXI1_CAM_SCL]', could not read keyword NAXIS2";
      return 1;
    }
    if ( $naxis2 ) {
      $outfile = $General{tempstem} . basename($hx1_hk) . "_ftcalc";
      ahapp::add_temp_file($outfile);
      $status = ahgen::run_ftool( 'ftcalc',
                                     "infile=$hx1_hk\[HK_HXI1_CAM_SCL]",
                                     "outfile=$outfile", 
                                     "column=HXI1_TRIGGER_RATE",
                                     "expr=(HXI1_CAM_SCL_TRG1-HXI1_CAM_SCL_TRG1{-1})/((HXI1_CAM_SCL_LIVETIME-HXI1_CAM_SCL_LIVETIME{-1})*819.2e-6)",
                                     "chatter=2" );
      if ($status) {
        ahlog::ah_err "Error calculating TRIGGER_RATE in ftcalc for $hx1_hk";
        return $status;
      }
      # The HK file has already been copied to the output directory
      # Just overwrite the new file.
      ahgen::copy_fits_file( $outfile, $hx1_hk );
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "Error copying $outfile to $hx1_hk" ;
        return ahgen::get_error_flag ;
      }
    }
    push( @filelist_output, $hx1_hk );

  }
  if ( $hx2_hk and $instrument{hx2} == 1 ) {

    my $outfile = $General{tempstem} . basename($hx2_hk) . "_ftcopy";
    ahapp::add_temp_file($outfile);
    ahlog::ah_out "Calculating TRIGGER_RATE for HXI2.";
    my $status = ahgen::run_ftool( 'ftcopy',
                                   "infile=$hx2_hk\[HK_HXI2_CAM_SCL][col *;HXI2_TRIGGER_RATE(D)=#NULL]",
                                   "outfile=$outfile",
                                   "copyall=yes",
                                   "clobber=yes");

    # The HK file has already been copied to the output directory
    # Just overwrite the new file.
    ahgen::copy_fits_file( $outfile, $hx2_hk );
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "Error copying $outfile to $hx2_hk" ;
      return ahgen::get_error_flag ;
    }
    my $naxis2 = ahgen::get_keyword($hx2_hk,"HK_HXI2_CAM_SCL","NAXIS2");
    unless ( defined $naxis2 ) {
      ahlog::ah_out"In file '$hx2_hk\[HK_HXI2_CAM_SCL]', could not read keyword NAXIS2";
      return 1;
    }
    if ( $naxis2 ) {
      $outfile = $General{tempstem} . basename($hx2_hk) . "_ftcalc";
      ahapp::add_temp_file($outfile);
      $status = ahgen::run_ftool( 'ftcalc',
                                     "infile=$hx2_hk\[HK_HXI2_CAM_SCL]",
                                     "outfile=$outfile", 
                                     "column=HXI2_TRIGGER_RATE",
                                     "expr=(HXI2_CAM_SCL_TRG1-HXI2_CAM_SCL_TRG1{-1})/((HXI2_CAM_SCL_LIVETIME-HXI2_CAM_SCL_LIVETIME{-1})*819.2e-6)",
                                     "chatter=2" );
      if ($status) {
        ahlog::ah_err "Error calculating TRIGGER_RATE in ftcalc for $hx2_hk";
        return $status;
      }
      # The HK file has already been copied to the output directory
      # Just overwrite the new file.
      ahgen::copy_fits_file( $outfile, $hx2_hk );
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "Error copying $outfile to $hx2_hk" ;
        return ahgen::get_error_flag ;
      }
    }
    push( @filelist_output, $hx2_hk );

  }

  # ahfilter runs the ahmkehk tool  to create the EHK file
  # and runs makefilter to create to create the MKF file
  ahlog::ah_out "Generating attitude/orbit EHK file $ehk.";
  $status = ahgen::run_ftool('ahfilter',
                             "mkfconf=$mkfconf",
                             "attfile=$attfile",
                             "orbfile=$orbfile",
                             "reference=NONE",
                             "teldeffile=$teldef",
                             "leapsecfile=$leapsecfile",
                             "cor2file=$cor2file",
                             "cor3file=$cor3file",
                             "saafile=$saafile",
                             "outehkfile=$ehk",
                             "outmkffile=$mkf",
                             "attext=ATTITUDE",
                             "attform=EULER",
                             "attcol=EULER",
                             "orbext=ORBIT",
                             "orbcol=A,E,I,AN,AP,MA",
                             "orbform=KEPLERIAN",
                             "timecol=TIME",
                             "optaxis=$optaxis",
                             "tstart=$hk_times{ehk_start}",
                             "tstop=$hk_times{ehk_stop}",
                             "bintime=$binlc",
                             "textend=0.0",
                             "infileroot=$infileroot",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$ahfilter_log",
                             );

  File::Copy::move ("ahmkehk.log", $ahmkehk_log);
  File::Copy::move ("mkfconf.ascii", $mkfconf_ascii);
  # Check for errors.
  if ($status) {
    ahlog::ah_err "Error in ahmkehk for $ehk";
    return $status;
  }

  # Make sure the filter file is time ordered. The ground
  # calibration RPT files have some problems here and this is
  # mostly a check to force processing to work.
  my $mkfsorted = $mkf . ".sorted";
  $status = ahgen::run_ftool('ftsort',$mkf,$mkfsorted,"columns=TIME");
  if ($status) {
    ahlog::ah_err "Error in makefilter for $mkf";
    return $status;
  }
  if (ahgen::copyOrMoveFileBasedOnCleanup($mkfsorted,$mkf,$ahapp::cleanup)) { unlink $mkfsorted;return 1; }
  unlink $mkfsorted;

  # Update the EHK parameter
  $files{extended_housekeeping} = $ehk;
  # Update the MKF parameter
  $files{makefilter} = $mkf;

  # push the ehk/mkf file to the list of files to update keywords
  push( @filelist_output, $ehk );
  push( @filelist_output, $mkf );

  ahlog::ah_out "\nEnd of CreateEHKMKF\n";

  return 0;

}

sub S2HxiFilter {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning S2HxiFilter\n";
  }

  my $makefilter   = $files{makefilter};
  my $ehk          = $files{extended_housekeeping};
  my $gtipoint     = $files{obsgti};
  my $select       = $caldb_files{gen}{selectfile};
  my $leapsecfile  = $caldb_files{gen}{leapsecfile};

  my $status = 0;

  ############# Populate coordinate keywords #############
  my $ra = $coordinates{gen}{ra};
  my $dec = $coordinates{gen}{dec};
  my $roll = $coordinates{gen}{roll};
  my $hx1_optdetx = $coordinates{hx1}{optdetx};
  my $hx1_optdety = $coordinates{hx1}{optdety};
  my $hx1_optfocx = $coordinates{hx1}{optfocx};
  my $hx1_optfocy = $coordinates{hx1}{optfocy};
  my $hx1_optskyx = $coordinates{hx1}{optskyx};
  my $hx1_optskyy = $coordinates{hx1}{optskyy};
  my $hx1_ra_pnt  = $coordinates{hx1}{ra_pnt};
  my $hx1_dec_pnt = $coordinates{hx1}{dec_pnt};
  my $hx2_optdetx = $coordinates{hx2}{optdetx};
  my $hx2_optdety = $coordinates{hx2}{optdety};
  my $hx2_optfocx = $coordinates{hx2}{optfocx};
  my $hx2_optfocy = $coordinates{hx2}{optfocy};
  my $hx2_optskyx = $coordinates{hx2}{optskyx};
  my $hx2_optskyy = $coordinates{hx2}{optskyy};
  my $hx2_ra_pnt  = $coordinates{hx2}{ra_pnt};
  my $hx2_dec_pnt = $coordinates{hx2}{dec_pnt};

  my $hxi_mkflabel= $Params{hxi_mkflabel};
  my $hxi_ehklabel= $Params{hxi_ehklabel};
  my $hxi_evtlabel= $Params{hxi_evtlabel};

  my $hxipipeline_log = $General{logstem} . "hxi_stage2.log";

  my @calibrated_hxi_cl_evt_files = ();

  # Verify that we have HXI calibrated event files
  my @hxi_events = (@{$files{"hx1_event_uf"}},@{$files{hx2_event_uf}});
  unless (  @hxi_events ) { 
    # No HXI unfiltered event files
    ahlog::ah_out "No HXI calibrated event files found, skipping HXI filtering";
    $instrument{hx1} = 0;
    $instrument{hx2} = 0;
    return 0;
  }

  my $instrume;
  if ( $instrument{hx1} and $instrument{hx2} ) {
    $instrume = "HXI";
  } elsif ( $instrument{hx1} and ! $instrument{hx2}) {
    $instrume = "HXI1";
  } elsif ( ! $instrument{hx1} and $instrument{hx2}) {
    $instrume = "HXI2";
  } else {
    ahlog::ah_err "No valid hxi instrument";
    return 1;
  }

  # Run hxipipeline and clean the events
  $status = ahgen::run_ftool("hxipipeline",
                             "indir=$Params{outdir}",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{stemoutputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=2",
                             "exit_stage=2",
                             "instrument=$instrume",
                             "attitude=NONE",
                             "obsgti=$gtipoint",
                             "makefilter=$makefilter",
                             "extended_housekeeping=$ehk",
                             "orbit=NONE",
                             "selectfile=$select",
                             "leapsecfile=$leapsecfile",
                             "hxi_mkflabel=$hxi_mkflabel",
                             "hxi_ehklabel=$hxi_ehklabel",
                             "hxi_evtlabel=$hxi_evtlabel",
                             "ra=$ra",
                             "dec=$dec",
                             "roll=$roll",
                             "hx1_optdetx=$hx1_optdetx",
                             "hx1_optdety=$hx1_optdety",
                             "hx1_optfocx=$hx1_optfocx",
                             "hx1_optfocy=$hx1_optfocy",
                             "hx1_optskyx=$hx1_optskyx",
                             "hx1_optskyy=$hx1_optskyy",
                             "hx1_ra_pnt=$hx1_ra_pnt",
                             "hx1_dec_pnt=$hx1_dec_pnt",
                             "hx2_optdetx=$hx2_optdetx",
                             "hx2_optdety=$hx2_optdety",
                             "hx2_optfocx=$hx2_optfocx",
                             "hx2_optfocy=$hx2_optfocy",
                             "hx2_optskyx=$hx2_optskyx",
                             "hx2_optskyy=$hx2_optskyy",
                             "hx2_ra_pnt=$hx2_ra_pnt",
                             "hx2_dec_pnt=$hx2_dec_pnt",
                             "stemreport=$General{stemreport}",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$hxipipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "hxipipeline failed."; return 1; }

  # Find the calibrated HXI event files

  for my $det ("hx1", "hx2") {
    if( $instrument{$det} == 1 ) {
      @calibrated_hxi_cl_evt_files = ();
      $files{$det."_event_cl"} = [];
      # Find the calibrated HXI event files
      # Save lists of newly calibrated '_cl.evt'
      @calibrated_hxi_cl_evt_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{$det."_event_cl"});
      @{$files{$det."_event_cl"}} = @calibrated_hxi_cl_evt_files;

    }
  }

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","hxipipeline","numerrs");
  $Errors{hxi}{stage2} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of S2HxiFilter\n";
  }

  return 0;
}

sub S2SgdFilter {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning S2SgdFilter\n";
  }

  my $status = 0;

  my $makefilter   = $files{makefilter};
  my $ehk          = $files{extended_housekeeping};
  my $gtipoint     = $files{obsgti};
  my $select       = $caldb_files{gen}{selectfile};
  my $leapsecfile  = $caldb_files{gen}{leapsecfile};

  my @calibrated_sgd_cl_evt_files = ();

  my $ra = $coordinates{gen}{ra};
  my $dec = $coordinates{gen}{dec};
  my $roll = $coordinates{gen}{roll};
  my $sg1_optfocx = $coordinates{sg1}{optfocx};
  my $sg1_optfocy = $coordinates{sg1}{optfocy};
  my $sg1_optskyx = $coordinates{sg1}{optskyx};
  my $sg1_optskyy = $coordinates{sg1}{optskyy};
  my $sg1_ra_pnt  = $coordinates{sg1}{ra_pnt};
  my $sg1_dec_pnt = $coordinates{sg1}{dec_pnt};
  my $sg2_optfocx = $coordinates{sg2}{optfocx};
  my $sg2_optfocy = $coordinates{sg2}{optfocy};
  my $sg2_optskyx = $coordinates{sg2}{optskyx};
  my $sg2_optskyy = $coordinates{sg2}{optskyy};
  my $sg2_ra_pnt  = $coordinates{sg2}{ra_pnt};
  my $sg2_dec_pnt = $coordinates{sg2}{dec_pnt};

  my $sgd_mkflabel= $Params{sgd_mkflabel};
  my $sgd_ehklabel= $Params{sgd_ehklabel};
  my $sgd_evtlabel= $Params{sgd_evtlabel};

  my $sgdpipeline_log = $General{logstem} . "sgd_stage2.log";

  # Verify that we have HXI calibrated event files
  my @sgd_events = (@{$files{"sg1_event_uf"}},@{$files{sg2_event_uf}});
  unless (  @sgd_events ) { 
    # No SGD unfiltered event files
    ahlog::ah_out "No SGD calibrated event files found, skipping SGD filtering";
    $instrument{sg1} = 0;
    $instrument{sg2} = 0;
    return 0;
  }

  my $instrume;
  if ( $instrument{sg1} and $instrument{sg2} ) {
    $instrume = "SGD";
  } elsif ( $instrument{sg1} and ! $instrument{sg2}) {
    $instrume = "SGD1";
  } elsif ( ! $instrument{sg1} and $instrument{sg2}) {
    $instrume = "SGD2";
  } else {
    ahlog::ah_err "No valid sgd instrument";
    return 1;
  }

  # Run sgdpipeline and clean the events
  $status = ahgen::run_ftool("sgdpipeline",
                             "indir=$Params{outdir}",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{stemoutputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=2",
                             "exit_stage=2",
                             "instrument=$instrume",
                             "obsgti=$gtipoint",
                             "makefilter=$makefilter",
                             "extended_housekeeping=$ehk",
                             "selectfile=$select",
                             "leapsecfile=$leapsecfile",
                             "sgd_mkflabel=$sgd_mkflabel",
                             "sgd_ehklabel=$sgd_ehklabel",
                             "sgd_evtlabel=$sgd_evtlabel",
                             "ra=$ra",
                             "dec=$dec",
                             "roll=$roll",
                             "sg1_optfocx=$sg1_optfocx",
                             "sg1_optfocy=$sg1_optfocy",
                             "sg1_optskyx=$sg1_optskyx",
                             "sg1_optskyy=$sg1_optskyy",
                             "sg1_ra_pnt=$sg1_ra_pnt",
                             "sg1_dec_pnt=$sg1_dec_pnt",
                             "sg2_optfocx=$sg2_optfocx",
                             "sg2_optfocy=$sg2_optfocy",
                             "sg2_optskyx=$sg2_optskyx",
                             "sg2_optskyy=$sg2_optskyy",
                             "sg2_ra_pnt=$sg2_ra_pnt",
                             "sg2_dec_pnt=$sg2_dec_pnt",
                             "stemreport=$General{stemreport}",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$sgdpipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "sgdpipeline failed."; return 1; }

  # Find the calibrated SGD event files

  for my $det ("sg1", "sg2") {
    if( $instrument{$det} == 1 ) {
      @calibrated_sgd_cl_evt_files = ();
      $files{$det."_event_cl"} = [];
      # Find the calibrated SGD event files
      # Save lists of newly calibrated '_cl.evt'
      @calibrated_sgd_cl_evt_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{$det."_event_cl"});
      @{$files{$det."_event_cl"}} = @calibrated_sgd_cl_evt_files;

    }
  }

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","sgdpipeline","numerrs");
  $Errors{sgd}{stage2} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of S2SgdFilter\n";
  }

  return 0;

}

sub S2SxiFilter {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning S2SxiFilter\n";
  }

  my $makefilter   = $files{makefilter};
  my $ehk          = $files{extended_housekeeping};
  my $gtipoint     = $files{obsgti};
  my $select       = $caldb_files{gen}{selectfile};
  my $leapsecfile  = $caldb_files{gen}{leapsecfile};

  my $ra = $coordinates{gen}{ra};
  my $dec = $coordinates{gen}{dec};
  my $roll = $coordinates{gen}{roll};
  my $optdetx = $coordinates{sxi}{optdetx};
  my $optdety = $coordinates{sxi}{optdety};
  my $optfocx = $coordinates{sxi}{optfocx};
  my $optfocy = $coordinates{sxi}{optfocy};
  my $optskyx = $coordinates{sxi}{optskyx};
  my $optskyy = $coordinates{sxi}{optskyy};
  my $ra_pnt  = $coordinates{sxi}{ra_pnt};
  my $dec_pnt = $coordinates{sxi}{dec_pnt};

  my $sxi_mkflabel= $Params{sxi_mkflabel};
  my $sxi_ehklabel= $Params{sxi_ehklabel};
  my $sxi_evtlabel= $Params{sxi_evtlabel};

  my @calibrated_sxi_cl_evt_files = ();

  my $sxipipeline_log = $General{logstem} . "sxi_stage2.log";

  # Confirm that we have SXI calibrated event files
  unless ( @{$files{sxi_event_uf}} ) { 
    # No SXI unfiltered event files
    ahlog::ah_out "No SXI calibrated event files found, skipping SXI filtering";
    $instrument{sxi} = 0;
    return 0;
  }

  my $status = 0;

  # Run sxipipeline and clean the events
  $status = ahgen::run_ftool("sxipipeline",
                             "indir=$Params{outdir}",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{stemoutputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=2",
                             "exit_stage=2",
                             "attitude=NONE",
                             "makefilter=$makefilter",
                             "extended_housekeeping=$ehk",
                             "orbit=NONE",
                             "obsgti=$gtipoint",
                             "housekeeping=NONE",
                             "selectfile=$select",
                             "leapsecfile=$leapsecfile",
                             "calc_modegti=$Params{sxi}{calc_modegti}",
                             "sxi_mkflabel=$sxi_mkflabel",
                             "sxi_ehklabel=$sxi_ehklabel",
                             "sxi_evtlabel=$sxi_evtlabel",
                             "ra=$ra",
                             "dec=$dec",
                             "roll=$roll",
                             "optdetx=$optdetx",
                             "optdety=$optdety",
                             "optfocx=$optfocx",
                             "optfocy=$optfocy",
                             "optskyx=$optskyx",
                             "optskyy=$optskyy",
                             "ra_pnt=$ra_pnt",
                             "dec_pnt=$dec_pnt",
                             "clobber=yes",
                             "stemreport=$General{stemreport}",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$sxipipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "sxipipeline failed."; return 1; }

  # Find the calibrated SXI event files
  @calibrated_sxi_cl_evt_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{sxi_event_cl});

  # Save lists of newly calibrated '_cl.evt'
  @{$files{sxi_event_cl}} = @calibrated_sxi_cl_evt_files;

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","sxipipeline","numerrs");
  $Errors{sxi}{stage2} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of S2SxiFilter\n";
  }

  return 0;

}

sub S2SxsFilter {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning S2SxsFilter\n";
  }

  my $makefilter   = $files{makefilter};
  my $ehk          = $files{extended_housekeeping};
  my $ehk2         = catfile($Params{outdir},"$Params{stemoutputs}.ehk2");
  my $gtipoint     = $files{obsgti};
  my $select       = $caldb_files{gen}{selectfile};
  my $leapsecfile  = $caldb_files{gen}{leapsecfile};
  my $screenlost   = $Params{screenlost};
  my $gtiadr       = $Params{adrgti};
  my $antfile      = $files{sxs_antico};

  my @calibrated_sxs_cl_evt_files = ();

  # Coordinate parameters
  my $ra = $coordinates{gen}{ra};
  my $dec = $coordinates{gen}{dec};
  my $roll = $coordinates{gen}{roll};
  my $optdetx = $coordinates{sxs}{optdetx};
  my $optdety = $coordinates{sxs}{optdety};
  my $optfocx = $coordinates{sxs}{optfocx};
  my $optfocy = $coordinates{sxs}{optfocy};
  my $optskyx = $coordinates{sxs}{optskyx};
  my $optskyy = $coordinates{sxs}{optskyy};
  my $ra_pnt  = $coordinates{sxs}{ra_pnt};
  my $dec_pnt = $coordinates{sxs}{dec_pnt};

  my $sxs_mkflabel= $Params{sxs_mkflabel};
  my $sxs_ehklabel= $Params{sxs_ehklabel};
  my $sxs_evtlabel= $Params{sxs_evtlabel};

  my $sxspipeline_log = $General{logstem} . "sxs_stage2.log";
  my $anticolc_log  = $General{logstem} . "sxsanticolc.log";


  # Confirm that we have SXS calibrated event files
  unless ( @{$files{sxs_event_uf}} ) { 
    # No SXS unfiltered event files
    ahlog::ah_out "No SXS calibrated event files found, skipping SXS filtering";
    $instrument{sxs} = 0;
    return 0;
  }

  my $status = 0;

  # Create the EHK 2 file
  if ( -e $antfile and $Params{create_ehkmkf} ) {

      # We want some antico data in the EHK file
      # Setup the Antico EHK data and put it in the EHK file
      my $antlc = $General{tempstem} . "extractor_ant.lc";
      ahapp::add_temp_file($antlc);
      my $ant_start = floor($hk_times{ehk_start});
      my $ant_stop = ceil($hk_times{ehk_stop});
      ahlog::ah_debug "ANTFILE  = $antfile";
      ahlog::ah_debug "ANTSTART = $ant_start";
      ahlog::ah_debug "ANTSTOP  = $ant_stop";
      $status = ahgen::run_ftool("sxsanticolc",
                                 "infile=$antfile",
                                 "outroot=$General{tempstem}extractor_ant",
                                 "extract=no",
                                 "bintime=1.0",
                                 "antpsp=A",
                                 "expr=FLG_BASELINE==b0&&PI>=60&&PI<=12200&&DURATION>2&&DURATION<19",
                                 "gtifile=NONE",
                                 "numlc=1",
                                 "picut=60-12200",
                                 "lcstart=$ant_start",
                                 "lcstop=$ant_stop",
                                 "lcthresh=1",
                                 "lctzero=no", 
                                 "logfile=$anticolc_log",
                               );

      unless ( $status ) {
        # Copy the lightcurve columns into the EHK file
        # Set the column names as:
        #   ANTRATE   = RATE
        #   ANTERROR  = ERROR
        $status = ahgen::run_ftool( 'ftpaste', 
                             "infile=$ehk\[EHK]",
                             "pastefile=$antlc\[RATE][col ANTRATE=RATE60_12200;ANTERROR=ERROR60_12200]",
                             "outfile=$ehk2",
                             "clobber=yes");

        if ( $status ) {
          ahlog::ah_err "Failed to create EHK2 file. Unlinking $ehk2" ;
          unlink $ehk2;
          $ehk2 = "";
        } else {
          ahlog::ah_info "LOW", "Successfuly added ANTRATE and ANTERROR columns to EHK file.";
          rename $ehk2 => $ehk;
        }
      }
  }

  # Run sxspipeline and clean the events
  $status = ahgen::run_ftool("sxspipeline",
                             "indir=$Params{outdir}",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{stemoutputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=2",
                             "exit_stage=2",
                             "attitude=NONE",
                             "makefilter=$makefilter",
                             "extended_housekeeping=$ehk",
                             "orbit=NONE",
                             "obsgti=$gtipoint",
                             "adrgti=$gtiadr",
                             "housekeeping=NONE",
                             "timfile=NONE",
                             "selectfile=$select",
                             "leapsecfile=$leapsecfile",
                             "screenlost=$screenlost",
                             "sxs_mkflabel=$sxs_mkflabel",
                             "sxs_ehklabel=$sxs_ehklabel",
                             "sxs_evtlabel=$sxs_evtlabel",
                             "ra=$ra",
                             "dec=$dec",
                             "roll=$roll",
                             "optdetx=$optdetx",
                             "optdety=$optdety",
                             "optfocx=$optfocx",
                             "optfocy=$optfocy",
                             "optskyx=$optskyx",
                             "optskyy=$optskyy",
                             "ra_pnt=$ra_pnt",
                             "dec_pnt=$dec_pnt",
                             "stemreport=$General{stemreport}",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$sxspipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "sxspipeline failed."; return 1; }

  # Find the calibrated SXS event files
  @calibrated_sxs_cl_evt_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{sxs_event_cl});

  # Save lists of newly calibrated '_cl.evt'
  @{$files{sxs_event_cl}} = @calibrated_sxs_cl_evt_files;

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","sxspipeline","numerrs");
  $Errors{sxs}{stage2} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of S2SxsFilter\n";
  }

  return 0;

}


############################
#     Stage 3 - Products
############################

sub MakeRegionFile {

  ahlog::ah_debug "\nRunning MakeRegionFile\n";

  my %passed = @_;

  my $status = 0;

  # Need instrument type.
  my $obsgti        = $files{obsgti};
  my $regfile       = catfile( $Params{outdir},"$Params{stemoutputs}.reg");
  my $teldef        = $caldb_files{sxs}{teldeffile};

  my $coordpnt_log = $General{logstem} . "coordpnt.log";

  # Set the defaults.
  my %input = (input        => "3.5,3.5",
         outfile      => "NONE",
         telescop     => $General{telescop},
         instrume     => "SXS",
         ra           => 0.0,
         dec          => 0.0,
         roll         => 0.0,
         teldeffile   => $teldef,
         startsys     => "LOWEST",
         stopsys      => "HIGHEST",
         clobber      => "yes",
         regfile      => $regfile,
        );

  # Overwrite the defaults.
  @input{keys %passed} = values %passed;
  
  # Need to query for a TelDef file for aspect
  if ( uc $teldef eq "CALDB" ) {
    my $dateobs;
    $dateobs = ahgen::get_keyword($obsgti,"GTIPOINT","DATE-OBS");
    unless ( defined $dateobs ) {
      ahlog::ah_err "In file $obsgti, DATE-OBS keyword not defined.\n";
      return 1;
    }
    my $teldeffile = ahfilterlib::call_quzcif($teldef,$input{instrume},"-","TELDEF",$dateobs,"-");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "Error querying CALDB for $input{instrume} TelDef";
      return ahgen::get_error_flag;
    }
    $teldeffile =~ s/\[.*\]//g;
    $input{teldeffile} = $teldeffile;
    unless ( defined $input{teldeffile} ) { ahlog::ah_err "Error querying CALDB for $input{instrume} TelDef"; return 1; }
  }

  # Calulcate the coordinates at the center of the SXS
  $status = ahgen::run_ftool( 'coordpnt' ,
                              "input=$input{input}",
                              "outfile=$input{outfile}",
                              "telescop=$input{telescop}",
                              "instrume=$input{instrume}",
                              "ra=$input{ra}",
                              "dec=$input{dec}",
                              "roll=$input{roll}",
                              "teldeffile=$input{teldeffile}",
                              "startsys=$input{startsys}",
                              "stopsys=$input{stopsys}",
                              "chatter=$ahapp::chatter",
                              "logfile=$coordpnt_log",
                        );

  ###################
  # Check for errors
  ###################
  if ( $status ) {
    ahlog::ah_err 'Error in coordpnt!' ;
    return $status;
  }

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","coordpnt","outx");
  my $outx = ahgen::get_tool_stdout();
  ahgen::run_ftool("pget","coordpnt","outy");
  my $outy = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  open(my $REGFILE,'>',$input{regfile});
    print $REGFILE "CIRCLE($outx,$outy,130)";
  close $REGFILE;

  $files{regionfile} = $input{regfile};

  ahlog::ah_out "\nEnd of MakeRegionFile\n";

  return 0;

}

sub S3Hxi {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning S3Hxi\n";
  }

  my $status = 0;

  my $regionfile  = $files{regionfile};

  my @calibrated_hxi_lc_files = ();
  my @calibrated_hxi_pha_files = ();
  my @calibrated_hxi_img_files = ();

  my $hxipipeline_log = $General{logstem} . "hxi_stage3.log";
  
  if ( @{$files{hx1_event_cl}} == 0 and @{$files{hx2_event_cl}} == 0 ) { return 0; }

  my $instrume;
  if ( $instrument{hx1} and $instrument{hx2} ) {
    $instrume = "HXI";
  } elsif ( $instrument{hx1} and ! $instrument{hx2}) {
    $instrume = "HXI1";
  } elsif ( ! $instrument{hx1} and $instrument{hx2}) {
    $instrume = "HXI2";
  } else {
    ahlog::ah_err "No valid hxi instrument";
    return 1;
  }

  $status = ahgen::run_ftool("hxipipeline",
                             "indir=$Params{outdir}",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{stemoutputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=3",
                             "exit_stage=3",
                             "instrument=$instrume",
                             "attitude=NONE",
                             "orbit=NONE",
                             "obsgti=NONE",
                             "housekeeping=NONE",
                             "regionfile=$regionfile",
                             "stemreport=$General{stemreport}",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$hxipipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "hxipipeline failed."; return 1; }

  # Find the calibrated HXI data product files
  for my $det ("hx1", "hx2") {
    if( $instrument{$det} == 1 ) {
      @calibrated_hxi_lc_files = ();
      @calibrated_hxi_pha_files = ();
      @calibrated_hxi_img_files = ();
      $files{$det."_lc"} = [];
      $files{$det."_pha"} = [];
      $files{$det."_img"} = [];
      # Find the calibrated HXI lightcurves, spectra and images
      @calibrated_hxi_lc_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{$det."_lc"});
      @{$files{$det."_lc"}} = @calibrated_hxi_lc_files;
      @calibrated_hxi_pha_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{$det."_pha"});
      @{$files{$det."_pha"}} = @calibrated_hxi_pha_files;
      @calibrated_hxi_img_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{$det."_img"});
      @{$files{$det."_img"}} = @calibrated_hxi_img_files;
    }
  }

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","hxipipeline","numerrs");
  $Errors{hxi}{stage3} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of S3Hxi\n";
  }

  return 0;

}

sub S3Sgd {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning S3Sgd\n";
  }

  my $status = 0;

  my @calibrated_sgd_lc_files = ();
  my @calibrated_sgd_pha_files = ();

  my $sgdpipeline_log = $General{logstem} . "sgd_stage3.log";

  if ( @{$files{sg1_event_cl}} == 0 and  @{$files{sg2_event_cl}} == 0 ) { return 0; }

  my $instrume;
  if ( $instrument{sg1} and $instrument{sg2} ) {
    $instrume = "SGD";
  } elsif ( $instrument{sg1} and ! $instrument{sg2}) {
    $instrume = "SGD1";
  } elsif ( ! $instrument{sg1} and $instrument{sg2}) {
    $instrume = "SGD2";
  } else {
    ahlog::ah_err "No valid sgd instrument";
    return 1;
  }

  $status = ahgen::run_ftool("sgdpipeline",
                             "indir=$Params{outdir}",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{stemoutputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=3",
                             "exit_stage=3",
                             "instrument=$instrume",
                             "obsgti=NONE",
                             "stemreport=$General{stemreport}",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$sgdpipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "sgdpipeline failed."; return 1; }

  # Find the calibrated SGD data product files
  for my $det ("sg1", "sg2") {
    if( $instrument{$det} == 1 ) {
      @calibrated_sgd_lc_files = ();
      @calibrated_sgd_pha_files = ();
      $files{$det."_lc"} = [];
      $files{$det."_pha"} = [];
      # Find the calibrated SGD lightcurves, spectra and images
      @calibrated_sgd_lc_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{$det."_lc"});
      @{$files{$det."_lc"}} = @calibrated_sgd_lc_files;
      @calibrated_sgd_pha_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{$det."_pha"});
      @{$files{$det."_pha"}} = @calibrated_sgd_pha_files;
    }
  }

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","sgdpipeline","numerrs");
  $Errors{sgd}{stage3} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of S3Sgd\n";
  }

  return 0;

}

sub S3Sxi {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning S3Sxi\n";
  }

  my $status = 0;

  my $regionfile  = $files{regionfile};

  my @calibrated_sxi_lc_files = ();
  my @calibrated_sxi_pha_files = ();
  my @calibrated_sxi_img_files = ();

  my $sxipipeline_log = $General{logstem} . "sxi_stage3.log";

  if ( @{$files{sxi_event_cl}} == 0 ) { return 0; }

  $status = ahgen::run_ftool("sxipipeline",
                             "indir=$Params{outdir}",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{stemoutputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=3",
                             "exit_stage=3",
                             "attitude=NONE",
                             "orbit=NONE",
                             "obsgti=NONE",
                             "housekeeping=NONE",
                             "regionfile=$regionfile",
                             "stemreport=$General{stemreport}",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$sxipipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "sxipipeline failed."; return 1; }

  # Find the calibrated SXI event files
  @calibrated_sxi_lc_files = ();
  @calibrated_sxi_pha_files = ();
  @calibrated_sxi_img_files = ();
  $files{sxi_lc} = [];
  $files{sxi_pha} = [];
  $files{sxi_img} = [];
  # Find the calibrated SXI lightcurves, spectra and images
  @calibrated_sxi_lc_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{sxi_lc});
  @{$files{sxi_lc}} = @calibrated_sxi_lc_files;
  @calibrated_sxi_pha_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{sxi_pha});
  @{$files{sxi_pha}} = @calibrated_sxi_pha_files;
  @calibrated_sxi_img_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{sxi_img});
  @{$files{sxi_img}} = @calibrated_sxi_img_files;

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","sxipipeline","numerrs");
  $Errors{sxi}{stage3} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of S3Sxi\n";
  }

  return 0;

}

sub S3Sxs {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning S3Sxs\n";
  }

  my $status = 0;

  my $regionfile  = $files{regionfile};

  my @calibrated_sxs_lc_files = ();
  my @calibrated_sxs_pha_files = ();
  my @calibrated_sxs_img_files = ();

  my $sxspipeline_log = $General{logstem} . "sxs_stage3.log";

  if ( @{$files{sxs_event_cl}} == 0 ) { return 0; }

  $status = ahgen::run_ftool("sxspipeline",
                             "indir=$Params{outdir}",
                             "outdir=$Params{outdir}",
                             "steminputs=$Params{stemoutputs}",
                             "stemoutputs=$Params{stemoutputs}",
                             "entry_stage=3",
                             "exit_stage=3",
                             "attitude=NONE",
                             "orbit=NONE",
                             "obsgti=NONE",
                             "housekeeping=NONE",
                             "timfile=NONE",
                             "regionfile=$regionfile",
                             "stemreport=$General{stemreport}",
                             "clobber=yes",
                             "chatter=$ahapp::chatter",
                             $ahapp::cleanup ? "cleanup=yes" : "cleanup=no",
                             "logfile=$sxspipeline_log",
                            );

  if ( $status ) { ahlog::ah_err "sxspipeline failed."; return 1; }

  # Find the calibrated SXS event files
  @calibrated_sxs_lc_files = ();
  @calibrated_sxs_pha_files = ();
  @calibrated_sxs_img_files = ();
  $files{sxs_lc} = [];
  $files{sxs_pha} = [];
  $files{sxs_img} = [];
  # Find the calibrated SXI lightcurves, spectra and images
  @calibrated_sxs_lc_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{sxs_lc});
  @{$files{sxs_lc}} = @calibrated_sxs_lc_files;
  @calibrated_sxs_pha_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{sxs_pha});
  @{$files{sxs_pha}} = @calibrated_sxs_pha_files;
  @calibrated_sxs_img_files = ahpllib::FindInputFiles($Params{outdir},$Params{stemoutputs},$patterns{sxs_img});
  @{$files{sxs_img}} = @calibrated_sxs_img_files;

  ahgen::set_quiet(1);
  ahgen::run_ftool("pget","sxspipeline","numerrs");
  $Errors{sxs}{stage3} = ahgen::get_tool_stdout();
  ahgen::set_quiet();

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of S3Sxs\n";
  }

  return 0;

}

############################
# Post-Processing Functions 
############################

sub FileHistory {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning FileHistory\n" ;
  }

  # Ensure each file name is processed only once

  @filelist_output = sort( @filelist_output );
  my $prev = 'nonesuch';
  my @filelist_output_unique = grep( $_ ne $prev && ( ( $prev ) = $_ ), @filelist_output );

  my $status = 0;
  my $fptr = "";

  # Check each file

  foreach my $infile ( @filelist_output_unique ) {

    ahlog::ah_debug "\nStamping ahpipeline parameters on file $infile\n" ;

    ## Open file and count number of HDU's (Headers)

    my $hdutotal = ahgen::get_total_hdu($infile);

    # Loop on HDUs

    for ( my $i1 = 0 ; $i1 < $hdutotal ; $i1++ ) {

      add_coord_keys($infile,$i1)

    }

    ## Update the CHECKSUM and DATASUM keywords
    $status = ahgen::update_checksum_and_verify($infile);
    unless ( $status ) { ahlog::ah_err "verify failed for $infile"; return $status; }


  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of FileHistory\n" ;
  }

  return 0;

}

sub Report {

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nRunning Report\n" ;
  }

  my %instrument_report = (
    'hxi' => 0,
    'sgd' => 0,
    'sxi' => 0,
    'sxs' => 0,
  );

  foreach my $inst ( sort keys %instrument ) {
    if ( $instrument{$inst} == 0 ) { next; }
    if ( $inst eq 'hx1' or $inst eq 'hx2' ) {
         $instrument_report{hxi} = 1;
         next;
       }
    if ( $inst eq 'sg1' or $inst eq 'sg2' ) {
         $instrument_report{sgd} = 1;
         next;
       }
         $instrument_report{$inst} = 1;
    }

  # Print a summary of errors and warnings from each pipeline script
  foreach my $inst ( qw( hxi sgd sxi sxs ) ) {
    if ( $instrument_report{$inst} == 0 ) { next; }
    ahlog::ah_out "Total warnings/errors for $inst: ";
    foreach my $stage ( qw ( stage1 stage2 stage3 ) ) {
      if ( $General{$stage."_switch"} == 0 ) { next; }
      ahlog::ah_out "  $stage: $Errors{$inst}{$stage}";
      if ( $Errors{$inst}{$stage} ) {
        ahlog::ah_out "    See logfile " . $General{logstem} . $inst . "_$stage.log for errors/warnings";
      }
    }
  }

  if ( ahgen::getdebug ) {
    ahlog::ah_debug "\nEnd of Report\n" ;
  }

  return 0;
}

# Function to write pointing keywords to FITS files

sub add_coord_keys {

  my $infile = shift;
  my $ext = shift;

  my $ra = $coordinates{gen}{ra};
  my $dec = $coordinates{gen}{dec};
  my $roll = $coordinates{gen}{roll};

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

  return 0;

}
