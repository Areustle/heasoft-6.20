#!/usr/bin/perl
#
# File name: ahnxbgen
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/12/14 15:19:40 $
# Version: 0
#
# Calculates NXB for input event files
#
# Tool Dependencies:
# 
# Library Dependencies:
#   heacore/perl/ahlog
#   heacore/perl/ahapp
#   heacore/perl/ahgen
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
use ahfilterlib ;
use File::Basename;

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

# Parameter variables
our $infile;
our $ehkfile;
our $regfile;
our $innxbfile;
our $innxbehk;
our $inpsefile;
our $outehkfile;
our $outnxbfile;
our $outnxbehk;
our $outpifile;
our $outpsefile;
our $regmode;
our $timefirst;
our $timelast;
our $picol;
our $sortcol;
our @sortbin = ();
our $expr;

# HXI specific parameters
our $tsaacol;
our @tsaabin = ();

# SXI specific parameters
our %sxipi_params;
our $sxipi_run=0;
our $runlcurve;
our $apply_sxipi;

# SXS specific parameters
our $pixels_par;
our @pixels;

# set up temporary file variables
our $telescop="";
our $instrume="";
our $ra_nom;
our $dec_nom;
our $pa_nom;
our $ref_focx;
our $ref_focy;
our $ref_skyx;
our $ref_skyy;
our $sciexpo;
our $t1;
our $t2;
our $totpseevt;

our $nxbgenerror = 0;

#########################
# HXI or SXI or SXS
#########################

# Determine which instrument we're processing
my $taskName = basename( $0 );
our $isHXI = $taskName =~ /^hxi/;
our $isSXI = $taskName =~ /^sxi/;
our $isSXS = $taskName =~ /^sxs/;

#########################
#  Main Code Block 
#########################

# Query canonical APE parameters and start logging. 
ahapp::startup () ;

# Pre-processing
ahapp::begin_processing();

# Get the input parameters
$nxbgenerror = GetInputParameters();
unless ( $nxbgenerror == 0 ) {
  ahlog::ah_debug "GetInputParameters" ;
  ahapp::end_processing($nxbgenerror);
}

# Check the input files for clobber
$nxbgenerror = CheckInput();
unless ( $nxbgenerror == 0 ) {
  ahlog::ah_debug "CheckInput" ;
  ahapp::end_processing($nxbgenerror);
}

# Write all parameters to this script to the log file.
ah_info "HIGH", ahapp::write_parameters () ;                 

# Read keywords from the input event file
$nxbgenerror = GetKeywordsFromInput();
unless ( $nxbgenerror == 0 ) {
  ahlog::ah_debug "GetKeywordsFromInput" ;
  ahapp::end_processing($nxbgenerror);
}

# SXI Only: Run sxipi on event file
if($isSXI) { 
  $nxbgenerror = CalcPI();
  unless ( $nxbgenerror == 0 ) {
    ahlog::ah_debug "CalcPI" ;
    ahapp::end_processing($nxbgenerror);
  }
}

$nxbgenerror = SelectRegion();
unless ( $nxbgenerror == 0 ) {
  ahlog::ah_debug "SelectRegion" ;
  ahapp::end_processing($nxbgenerror);
}

$nxbgenerror = SelectNightData();
unless ( $nxbgenerror == 0 ) {
  ahlog::ah_debug "SelectNightData" ;
  ahapp::end_processing($nxbgenerror);
}

if($isSXI) { 
  $nxbgenerror = RemoveHighBackground();
  unless ( $nxbgenerror == 0 ) {
    ahlog::ah_debug "RemoveHighBackground" ;
    ahapp::end_processing($nxbgenerror);
  }
}

if($isHXI) { $nxbgenerror = ExtractHXISpectra(); }
if($isSXI) { $nxbgenerror = ExtractSXISpectra(); }
if($isSXS) { $nxbgenerror = ExtractSXSSpectra(); }
unless ( $nxbgenerror == 0 ) {
  if($isHXI) { ahlog::ah_debug "ExtractHXISpectra" ; }
  if($isSXI) { ahlog::ah_debug "ExtractSXISpectra" ; }
  if($isSXS) { ahlog::ah_debug "ExtractSXSSpectra" ; }
  ahapp::end_processing($nxbgenerror);
}

$nxbgenerror = Finalize();
unless ( $nxbgenerror == 0 ) {
  ahlog::ah_debug "Finalize" ;
  ahapp::end_processing($nxbgenerror);
}

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

  ahlog::ah_out "Getting Parameters";

  $infile       = ahapp::query_parameter("infile");
  $ehkfile      = ahapp::query_parameter("ehkfile");
  $regfile      = ahapp::query_parameter("regfile");
  $innxbfile    = ahapp::query_parameter("innxbfile");
  $innxbehk     = ahapp::query_parameter("innxbehk");
  $outehkfile   = ahapp::query_parameter("outehkfile");
  $outnxbfile   = ahapp::query_parameter("outnxbfile");
  $outnxbehk    = ahapp::query_parameter("outnxbehk");
  $outpifile    = ahapp::query_parameter("outpifile");
  $regmode      = ahapp::query_parameter("regmode");
  $timefirst    = ahapp::query_parameter("timefirst");
  $timelast     = ahapp::query_parameter("timelast");
  $picol        = "PI";          # this value will be used for all instruments except SXS
  $sortcol      = ahapp::query_parameter("sortcol");
  @sortbin      = split ",", ahapp::query_parameter("sortbin");
  $expr         = ahapp::query_parameter("expr");

  # Get HXI specific parameters
  if($isHXI) {
    $inpsefile    = ahapp::query_parameter("inpsefile");
    $tsaacol      = ahapp::query_parameter("tsaacol");
    @tsaabin      = split ",", ahapp::query_parameter("tsaabin");
  }

  # Get SXI specific parameters
  if($isSXI) {
    $runlcurve    = ahapp::query_parameter("runlcurve",1);
    $apply_sxipi  = ahapp::query_parameter("apply_sxipi",1);
    $sxipi_params{hkfile}       = ahapp::query_parameter("innxbhk");
    $sxipi_params{hkext}         = ahapp::query_parameter("hkext");
    $sxipi_params{hkcolstem}     = ahapp::query_parameter("hkcolstem");
    $sxipi_params{hkvideoid}     = ahapp::query_parameter("hkvideoid");
    $sxipi_params{vtevnoddfile}  = ahapp::query_parameter("vtevnoddfile");
    $sxipi_params{chtrailfile}   = ahapp::query_parameter("chtrailfile");
    $sxipi_params{ctifile}       = ahapp::query_parameter("ctifile");
    $sxipi_params{spthfile}      = ahapp::query_parameter("spthfile");
    $sxipi_params{gainfile}      = ahapp::query_parameter("gainfile");
    $sxipi_params{patternfile}   = ahapp::query_parameter("patternfile");
    $sxipi_params{startcol}      = ahapp::query_parameter("startcol");
    $sxipi_params{evnoddcor}     = ahapp::query_parameter("evnoddcor");
    $sxipi_params{badpixopt}     = ahapp::query_parameter("badpixopt");
    $sxipi_params{evtthre}       = ahapp::query_parameter("evtthre");
    $sxipi_params{deltatime}     = ahapp::query_parameter("deltatime");
    $sxipi_params{randomize}     = ahapp::query_parameter("randomize");
    $sxipi_params{seed}          = ahapp::query_parameter("seed");
  }

  # Get SXS specific parameters
  if($isSXS) {
    $picol = ahapp::query_parameter("picol");

    # the pixel parameter is only read if the regionfile is set to none
    if ( uc $regfile eq "NONE" ) {
      $pixels_par = ahapp::query_parameter("pixels");
    }
  }

  return 0;

} # End GetInputParameters

sub CheckInput {

  ahlog::ah_out "Checking input";

  # Check that input files exist
  if(isRequiredFileNotFound($infile)) { return 1;} 
  if(isRequiredFileNotFound($ehkfile)) { return 1;} 
  if ( $isSXS ) {
    if(isOptionalFileNotFound($regfile)) { return 1;} 
  } else {
    if(isRequiredFileNotFound($regfile)) { return 1;} 
  }
  if(isRequiredFileNotFound($innxbfile)) { return 1;} 
  if(isRequiredFileNotFound($innxbehk)) { return 1;} 
  if($isHXI) { if(isRequiredFileNotFound($inpsefile)) { return 1;} }

  if(lc $outehkfile eq "none") { 
    $outehkfile = "tmpout.ehk";
    ahapp::add_temp_file($outehkfile);
  }
  if(lc $outnxbfile eq "none") { 
    $outnxbfile = "tmpoutnxb.evt";
    ahapp::add_temp_file($outnxbfile);
  }
  if(lc $outnxbehk eq "none") { 
    $outnxbehk = "tmpoutnxb.ehk";
    ahapp::add_temp_file($outnxbehk);
  }

  if($isHXI) {
    $outpsefile = "tmpoutpse.evt";
    ahapp::add_temp_file($outpsefile);
  }

  # Check for output file clobber
  if(removeOutputFileIfClobbering($outehkfile,$ahapp::clobber)) { return 1; }
  if(removeOutputFileIfClobbering($outnxbfile,$ahapp::clobber)) { return 1; }
  if(removeOutputFileIfClobbering($outnxbehk,$ahapp::clobber)) { return 1; }
  if(removeOutputFileIfClobbering($outpifile,$ahapp::clobber)) { return 1; }
  if($isHXI) { if(removeOutputFileIfClobbering($outpsefile,$ahapp::clobber)) { return 1; } }

  # Copy infile to outfile
  if(copyFITSFile($ehkfile,$outehkfile)) { return 1; }
  if(copyFITSFile($innxbfile,$outnxbfile)) { return 1; }
  if(copyFITSFile($innxbehk,$outnxbehk)) { return 1; };
  if($isHXI) { if(copyFITSFile($inpsefile,$outpsefile)) { return 1; } }

  if($isSXS) {
    # Check if PI column is present (via picol parameter)
    if (0 == ahgen::get_column_num($infile,"EVENTS",$picol)) {   # 0 returned if column not found
      ahlog::ah_err "In file $infile, PI column, $picol, not found; check picol parameter.\n";
      return 1;
    }

    # If the region file is set to none parse the pixels parameter
    # delimited either by hyphen or colon
    if ( uc $regfile eq "NONE" ) {
      if($pixels_par eq "-" ) { 
        # Do not fill the pixels array
      } else {
        my @pixpar = split ",", $pixels_par;
        foreach my $split (@pixpar) {
          if ( $split =~ "-" ) {
            my $pixexpr;
            my ($pix1, $pix2) = split "-", $split;
            if ( $pix1 eq "" ) { push @pixels, "PIXEL>=0&&PIXEL<=$pix2"; next;}
            if ( $pix2 eq "" ) { push @pixels, "PIXEL>=$pix1&&PIXEL<=35"; next;}
            if ($pix1 < 0 || $pix1 > 35) { ahlog::ah_err "Pixel parameter out of range"; return 1; }
            if ($pix2 < 0 || $pix2 > 35) { ahlog::ah_err "Pixel parameter out of range"; return 1; }
            if ( $pix1 > $pix2 ) { $pixexpr = "PIXEL>=$pix2&&PIXEL<=$pix1"; }
            else { $pixexpr = "PIXEL>=$pix1&&PIXEL<=$pix2"; }
            push @pixels , $pixexpr;
          } elsif ( $split =~ ":" ) {
            my $pixexpr;
            my ($pix1, $pix2) = split ":", $split;
            if ( $pix1 eq "" ) { push @pixels, "PIXEL>=0&&PIXEL<=$pix2"; next;}
            if ( $pix2 eq "" ) { push @pixels, "PIXEL>=$pix1&&PIXEL<=35"; next;}
            if ($pix1 < 0 || $pix1 > 35) { ahlog::ah_err "Pixel parameter out of range"; return 1; }
            if ($pix2 < 0 || $pix2 > 35) { ahlog::ah_err "Pixel parameter out of range"; return 1; }
            if ( $pix1 > $pix2 ) { $pixexpr = "PIXEL>=$pix2&&PIXEL<=$pix1"; }
            else { $pixexpr = "PIXEL>=$pix1&&PIXEL<=$pix2"; }
            push @pixels , $pixexpr;
          } else {
            if ($split < 0 || $split > 35) { ahlog::ah_err "Pixel parameter out of range"; return 1; }
            my $pixexpr = "PIXEL==$split";
            push @pixels, $pixexpr;
          }
        } # end loop over delimiter
      } # end pixels parameter parse
      ahlog::ah_info "LOW", "No region file, using pixel filters:";
      if ( @pixels ) {
        ahlog::ah_info "LOW", "  $_" foreach @pixels;
      } else {
        ahlog::ah_info "LOW", "  ALL";
      }
    } # end if regfile eq none
  } # end if sxs

  return 0;

} # End CheckInput

sub GetKeywordsFromInput {

  my $status = 0;

  ahlog::ah_out "Getting Keywords";

  # get keywords
  # read instrume, telescop from science event file
  $instrume = ahgen::get_keyword($infile,"EVENTS","INSTRUME");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $infile, INSTRUME keyword not defined.\n";
    return ahgen::get_error_flag;
  }
  $telescop = ahgen::get_keyword($infile,"EVENTS","TELESCOP");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $infile, TELESCOP keyword not defined.\n";
    return ahgen::get_error_flag;
  }

  # read tstart, tstop from science event file
  my $tstart = ahgen::get_keyword($infile,"EVENTS","TSTART");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $infile, TSTART keyword not defined.\n";
    return ahgen::get_error_flag;
  }
  my $tstop = ahgen::get_keyword($infile,"EVENTS","TSTOP");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $infile, TSTOP keyword not defined.\n";
    return ahgen::get_error_flag;
  }

  # set up start/stop background observation times
  $t1 = $tstart - $timefirst*24*60*60;
  $t2 = $tstop + $timefirst*24*60*60;

  if( lc $regmode eq "sky" ) {
    # read reference pixel keywords TCRPX[FOCX,FOCY,X,Y]
    my $focxnum = ahgen::get_column_num($infile,"EVENTS","FOCX") ;
    $ref_focx = ahgen::get_keyword($infile,"EVENTS","TCRPX$focxnum");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "In file $infile, FOCX keyword TCRPX$focxnum not defined.\n";
      return ahgen::get_error_flag;
    }
    my $focynum = ahgen::get_column_num($infile,"EVENTS","FOCY") ;
    $ref_focy = ahgen::get_keyword($infile,"EVENTS","TCRPX$focynum");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "In file $infile, FOCY keyword TCRPX$focynum not defined.\n";
      return ahgen::get_error_flag;
    }
    my $xcolnum = ahgen::get_column_num($infile,"EVENTS","X") ;
    $ref_skyx = ahgen::get_keyword($infile,"EVENTS","TCRPX$xcolnum");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "In file $infile, X keyword TCRPX$xcolnum not defined.\n";
      return ahgen::get_error_flag;
    }
    my $ycolnum = ahgen::get_column_num($infile,"EVENTS","Y") ;
    $ref_skyy = ahgen::get_keyword($infile,"EVENTS","TCRPX$ycolnum");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "In file $infile, Y keyword TCRPX$ycolnum not defined.\n";
      return ahgen::get_error_flag;
    }
    $pa_nom = ahgen::get_keyword($infile,"EVENTS","PA_NOM");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "In file $infile, PA_NOM keyword not defined.\n";
      ahlog::ah_err "Exiting.\n";
      return ahgen::get_error_flag;
    }
  }

  # Read exposure keyword
  $sciexpo = ahgen::get_keyword($infile,"EVENTS","EXPOSURE");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $infile, EXPOSURE keyword not defined.\n";
    return ahgen::get_error_flag;
  }

  return $status;

} # End GetKeywordsFromInput


sub CalcPI {

  my $status = 0;

  unless ( $apply_sxipi ) { return 0; }

  # set the parameters for sxipi run 1
  $sxipi_params{chtrailcor} = "yes";
  $sxipi_params{cticor} = "yes";
  $sxipi_params{gaincor} = "no";
  $sxipi_params{ctigrade} = "no";
  $sxipi_params{copygrade} = "no";
  $sxipi_params{phcut} = "CALDB";
  $sxipi_params{spthiter} = "yes";
  $sxipi_params{spthcaldb} = "yes";
  $sxipi_params{negthre} = -5000;
  $sxipi_params{debugcol} = "no";
  if(run_sxipi($outnxbfile,\%sxipi_params)) { return 1; };

  # set the parameters for sxipi run 2
  $sxipi_params{chtrailcor} = "yes";
  $sxipi_params{cticor} = "yes";
  $sxipi_params{gaincor} = "yes";
  $sxipi_params{ctigrade} = "yes";
  $sxipi_params{copygrade} = "no";
  $sxipi_params{phcut} = "CALDB";
  $sxipi_params{spthiter} = "yes";
  $sxipi_params{spthcaldb} = "yes";
  $sxipi_params{negthre} = -5000;
  $sxipi_params{debugcol} = "no";
  if(run_sxipi($outnxbfile,\%sxipi_params)) { return 1; };

  return $status;

} # End Calc_PI

sub SelectRegion {

  my $TmpOutfile = "";
  my $regexpr= "";

  my $status = 0;

  ahlog::ah_out "Selecting Region";

  if ( $isSXS and uc $regfile eq "NONE" ) { return 0; }

  # Assign X/Y values to NXB data (using same roll angle as in science data)
  # if region mode is DET, this step can be skipped
  if(uc $regmode eq "SKY") {
    my $exprx = "(int)(cos($pa_nom*3.14159/180)*(FOCX-$ref_focx)-sin($pa_nom*3.14159/180)*(FOCY-$ref_focy)+$ref_skyx)";
    my $expry = "(int)(sin($pa_nom*3.14159/180)*(FOCX-$ref_focx)+cos($pa_nom*3.14159/180)*(FOCY-$ref_focy)+$ref_skyy)";

    $TmpOutfile = formTemporaryFileName($outnxbfile,"nxb_evt_skyx.tmp");
    $status = ahgen::run_ftool("ftcalc",$outnxbfile,$TmpOutfile,"X", $exprx,"clobber=yes");
     if( $status ) {
      ahlog::ah_err "Cannot assign X values to NXB data.\n";
      return $status;
    }
    if(copyOrMoveFileBasedOnCleanup($TmpOutfile,$outnxbfile,$ahapp::cleanup)) { return 1; }

    $TmpOutfile = formTemporaryFileName($outnxbfile,"nxb_evt_skyy.tmp");
    $status = ahgen::run_ftool("ftcalc",$outnxbfile,$TmpOutfile,"Y", $expry,"clobber=yes");
     if( $status ) {
      ahlog::ah_err "Cannot assign Y values to NXB data.\n";
      return $status;
    }
    if(copyOrMoveFileBasedOnCleanup($TmpOutfile,$outnxbfile,$ahapp::cleanup)) { return 1; }
  } # end if regmode sky

  # select the NXB using the same region as science data
  # Caveats:
  # 1. Physical for DS9 FK5 stores the region for X and Y
  # 2. FK5 in degrees DS9 stores the region in HMS

  # Set up region filter
  if(uc $regmode eq "SKY") {
    $regexpr = "regfilter(\"$regfile\")";
  } elsif (uc $regmode eq "DET") { # regmode: DET
    $regexpr = "regfilter(\"$regfile\",DETX,DETY)";
  } elsif (uc $regmode eq "FOC") { # regmode: FOC
    $regexpr = "regfilter(\"$regfile\",FOCX,FOCY)";
  } elsif (uc $regmode eq "RAW") { # regmode: RAW
    $regexpr = "regfilter(\"$regfile\",RAWX,RAWY)";
  } else {
    ahlog::ah_err "No valid region given";
    return 1;
  }

  # Use the region filter on the output nxb events
  if(run_ftselect($outnxbfile,"EVENTS",$outnxbfile.".region",$regexpr)) { return 1; };
  if(copyOrMoveFileBasedOnCleanup($outnxbfile.".region",$outnxbfile,$ahapp::cleanup)) { return 1; }

  return 0;

} # End SelectRegion

# Select the night data using the same criteria as the science data
sub SelectNightData {

  my $TmpInfile="";
  my $TmpOutfile="";
  my $StartStopGTI="";
  my $gtiexpr="";

  my $status = 0;

  ahlog::ah_out "Selecting Night Data";

  # ftcreate START/STOP GTI and merge with NXBEVT[GTI]
  $StartStopGTI = "tmp.gti";
  ahapp::add_temp_file($StartStopGTI);
  if(createStartStopGTI($StartStopGTI)) { return 1; }
  $StartStopGTI .= "[STDGTI]";

  # Use ftmerge to copy $nxbgti to $nxbevt[GTI]
  # Set extended syntax to filter all rows from original GTI to be replaced by new GTI
  # This will keep the original header data
  $TmpInfile = $outnxbfile. "[GTI][#row<0]";
  $TmpOutfile = formTemporaryFileName($outnxbfile, "ftmerge.tmp");
  $status = ahgen::run_ftool("ftmerge","infile=$TmpInfile,$StartStopGTI",
                   "outfile=$TmpOutfile","copyall=yes","clobber=yes");
   if( $status ) {
    ahlog::ah_err "Error merging updated NXB GTI";
    return $status;
  }
  if(copyOrMoveFileBasedOnCleanup($TmpOutfile,$outnxbfile,$ahapp::cleanup)) { return 1; }

  # GTI cut on NXBEVT, NXBEHK (around the time of the SCI observation)
  $gtiexpr = "gtifilter(\"$StartStopGTI\")";
  if(run_ftselect($outnxbfile,"EVENTS",$outnxbfile.".gticlean",$gtiexpr)) { return 1; };
  if(run_ftselect($outnxbehk,"EHK",$outnxbehk.".gticlean",$gtiexpr)) { return 1; };
  if ($isHXI) { 
    # GTI cut on pseudo file
    if(run_ftselect($outpsefile,"EVENTS",$outpsefile.".gticlean",$gtiexpr)) { return 1; };
    # Count pseudo events (number of rows)
    $totpseevt = ahgen::get_keyword($outpsefile,"EVENTS","NAXIS2");
    if($status) {
      ahlog::ah_err "Error reading keyword NAXIS2 from $outpsefile.\n";
      return $status;
    }
    if(copyOrMoveFileBasedOnCleanup($outpsefile.".gticlean",$outpsefile,$ahapp::cleanup)) { return 1; }
  }

  # Set the GTI cleaned files as the new output files.
  if(copyOrMoveFileBasedOnCleanup($outnxbfile.".gticlean",$outnxbfile,$ahapp::cleanup)) { return 1; }
  if(copyOrMoveFileBasedOnCleanup($outnxbehk.".gticlean",$outnxbehk,$ahapp::cleanup)) { return 1; }

  # run ftselect on NXBEVT to screen 
  if(lc $expr ne "none") {
    if(run_ftselect($outnxbfile,"EVENTS",$outnxbfile.".expr_clean",$expr)) { return 1; };
    if(copyOrMoveFileBasedOnCleanup($outnxbfile.".expr_clean",$outnxbfile,$ahapp::cleanup)) { return 1; }
  }

  return 0;
} # end SelectNightData

sub RemoveHighBackground {

  my $lcexpr="";
  my $mtexpr="";
  my $gtilc="";

  my $status = 0;

  unless ( $runlcurve ) { return 0; }

  $gtilc = "gtilc.tmp";
  ahapp::add_temp_file($gtilc);

  # 7. Remove time when background rate is high
  # 7a. Extract events in 12-24 keV (fselect PI>=2000&&PI<=4000)
  #$ftselect{outfile} = $nxbevt . ".above12kev.tmp";
  $lcexpr = "PI>=2000&&PI<=4000";

  if(run_ftselect($outnxbfile,"EVENTS",$outnxbfile.".above12kev",$lcexpr)) { return 1; };
  if(copyOrMoveFileBasedOnCleanup($outnxbfile.".above12kev",$outnxbfile,$ahapp::cleanup)) { return 1; }

  # 7b. Make light curve
  my $nxblc = "lc_above12kev.tmp";
  ahapp::add_temp_file($nxblc);
  $status = ahgen::run_ftool("fcurve","infile=$outnxbfile","gtifile=-","outfile=$nxblc",
                   "timecol=TIME","columns=-","binsz=400 ","lowval=INDEF","highval=INDEF",
                   "binmode=Event_rate","extname=LIGHTCURVE","outtimecol=TIME","outcol=RATE",
                   "outerr=NONE");
  if($status) {
    ahlog::ah_err "Cannot run fcurve to create lc_above12kev.tmp.\n";
    return $status;
  }

  # 7c. create gti file from lightcurve
  $mtexpr = "RATE<=1.5";
  if(run_maketime($nxblc,$gtilc,$mtexpr)) { return 1; }

  # 7d. filter nxb events and ehk with gti file
  $lcexpr = "gtifilter(\"$gtilc\")";

  if(run_ftselect($outnxbfile,"EVENTS",$outnxbfile.".gtiabove12kev",$lcexpr)) { return 1; };
  if(run_ftselect($outnxbehk,"EHK",$outnxbehk.".gtiabove12kev",$lcexpr)) { return 1; };
  if(copyOrMoveFileBasedOnCleanup($outnxbfile.".gtiabove12kev",$outnxbfile,$ahapp::cleanup)) { return 1; }
  if(copyOrMoveFileBasedOnCleanup($outnxbehk.".gtiabove12kev",$outnxbehk,$ahapp::cleanup)) { return 1; }

  return $status;
  
} # end runlcurve

sub ExtractHXISpectra {

  my $phasum="";
  my $phaexpr="";
  my $pseudohz=0;
  my $status = 0;

  # Loop on COR:
  # a. maketime on the SCIEHK using cor range
  # b. calculate exposure for the COR(i) GTI files EXPOCOR(i) from SCIEHK
  # c. maketime on the NXBEHK using cor rangehk
  # d. extract spectrum NXBSPEC in NXB using the GTI where COR(i) is valid
  #    using the screened NXBEVT save
  # e. Append weighted spectrum to mathpha expression:
  #    sumpha = sum(NXBSPEC(i)*[EXPOCOR(i)/SCIEXPO])

  $pseudohz = ahgen::get_keyword($outpsefile,"EVENTS","PSEUDOHZ");
  if(ahgen::get_error_flag) {
    ahlog::ah_err "Keyword PSEUDOHZ not found in file $outpsefile\n";
    return ahgen::get_error_flag;
  }

  $phasum = "phasum.tmp";
  ahapp::add_temp_file($phasum);

  ahlog::ah_info "HIGH", "Extracting spectrum"; 
  for(my $ii = 0; $ii <= $#tsaabin; $ii++) {

    my $tsaa_expr = "";

    # Build T_SAA expression
    if($ii == $#tsaabin) {
      $tsaa_expr = "$tsaacol>=" . $tsaabin[-1];
    } else {
      $tsaa_expr = "$tsaacol>=" . $tsaabin[$ii] . "&&$tsaacol<" . $tsaabin[$ii+1];
    }

    ahlog::ah_info "LOW", "  T_SAA: $tsaa_expr";
    
    # Loop over cor bins
    for( my $jj = 0; $jj < $#sortbin; $jj++ ) {

      my $phafile = "";
      my $cor_expr = "";
      my $mtexpr = "";
      my $expocor=0;
      my $expopse=0;
      my $fracexp=0;

      # Set up temporary output file name
      $phafile = "pha_cor_$ii\_$jj.tmp";
      ahapp::add_temp_file($phafile);

      # Build maketime expression and extract expression
      $cor_expr = "$sortcol>=".$sortbin[$jj]."&&$sortcol<".$sortbin[$jj+1];
      $mtexpr = "$cor_expr&&$tsaa_expr";
      ahlog::ah_info "LOW", "    COR  : $cor_expr";
      if(extractNXBSpectrum($phafile,$mtexpr,\$expocor,$picol)) { return 1; }
      # Skip any bins that have no exposure
      if($expocor==0) { next; }

      # Sum all pseudo counts (fracexp=numberOfPseudo/(pseudoHZ*mtexpo))
      $fracexp = $totpseevt/($pseudohz*$expocor);
      # Sum expression for mathpha
      $phaexpr .= "$phafile*($expocor*$fracexp/$sciexpo)+";

    } # end loop on COR
  } # end loop on T_SAA

  # Cleanup expression
  $phaexpr =~ s/^[+]//; # remove any leading plus signs
  $phaexpr =~ s/[+]$//; # remove any trailing plus signs
  $phaexpr =~ s/[+][+]+/[+]/; # remove any plus sign multiples

  # Verify we have an expression
  if(length($phaexpr) == 0) {
    ahlog::ah_err "An invalid expression for mathpha was formed.";
    return 1;
  }

  # Write the expression to a text file
  open PHASUM, ">", $phasum;
  print PHASUM $phaexpr;
  close PHASUM;

  # Create weight-averaged NXB spectrum
  ahlog::ah_info "HIGH", "Creating weight-averaged NXB spectrum";
  $status = ahgen::run_ftool("mathpha","\@$phasum","R",
                             $outpifile,"CALC","%","ncomments=0");
  if( $status ) {
    ahlog::ah_err "Cannot create spectrum from mathpha";
    return $status;
  }

  return 0;

}

sub ExtractSXISpectra {

  my $phasum="";
  my $phaexpr="";

  my $status = 0;

  $phasum = "phasum.tmp";
  ahapp::add_temp_file($phasum);

  ahlog::ah_info "HIGH", "Extracting spectrum"; 
  for( my $jj = 0; $jj < $#sortbin; $jj++ ) {

    my $phafile="";
    my $cor_expr="";
    my $expocor="";

    # Set up temporary output file name
    $phafile = "pha_cor_$jj.tmp";
    ahapp::add_temp_file($phafile);

    # Build maketime expression and extract expression
    $cor_expr = "$sortcol>=".$sortbin[$jj]."&&$sortcol<".$sortbin[$jj+1];
    ahlog::ah_info "LOW", "  COR  : $cor_expr";
    if(extractNXBSpectrum($phafile,$cor_expr,\$expocor,$picol)) { return 1; }
    # Skip any bins that have no exposure
    if($expocor==0) { next; }
    ahlog::ah_info "LOW", "  EXPO : $expocor";

    $phaexpr .= "$phafile*($expocor/$sciexpo)";
    if($jj != $#sortbin - 1) { $phaexpr .= "+\n"; }

  } # end loop on COR

  # Cleanup expression
  $phaexpr =~ s/^[+]//; # remove any leading plus signs
  $phaexpr =~ s/[+]$//; # remove any trailing plus signs
  $phaexpr =~ s/[+][+]+/[+]/; # remove any plus sign multiples

  # Verify we have an expression
  if(length($phaexpr) == 0) {
    ahlog::ah_err "An invalid expression for mathpha was formed.";
    return 1;
  }

  # Write the expression to a text file
  open PHASUM, ">", $phasum;
  print PHASUM $phaexpr;
  close PHASUM;

  # Create weight-averaged NXB spectrum
  ahlog::ah_info "HIGH", "Creating weight-averaged NXB spectrum";
  $status = ahgen::run_ftool("mathpha","\@$phasum","R",
                             $outpifile,"CALC","%","ncomments=0");
   if( $status ) {
    ahlog::ah_err "Cannot create spectrum from mathpha";
    return $status;
  }

  return 0;

}

sub ExtractSXSSpectra {

  my $phasum="";
  my $phaexpr="";

  my $status = 0;

  $phasum = "phasum.tmp";
  ahapp::add_temp_file($phasum);

  ahlog::ah_info "HIGH", "Extracting spectrum"; 

  if ( uc $regfile eq "NONE" and @pixels ) { 
    my $pixexpr = join ( "||", @pixels );
    if(run_ftselect($outnxbfile,"EVENTS",$outnxbfile.".px_clean",$pixexpr)) { return 1; };
    if(copyOrMoveFileBasedOnCleanup($outnxbfile.".px_clean",$outnxbfile,$ahapp::cleanup)) { return 1; }
  }

  for( my $jj = 0; $jj < $#sortbin; $jj++ ) {

    my $phafile="";
    my $cor_expr="";
    my $expocor="";

    # Set up temporary output file name
    $phafile = "pha_cor_$jj.tmp";
    ahapp::add_temp_file($phafile);

    # Build maketime expression and extract expression
    $cor_expr = "$sortcol>=".$sortbin[$jj]."&&$sortcol<".$sortbin[$jj+1];
    ahlog::ah_info "LOW", "  COR  : $cor_expr";
    if(extractNXBSpectrum($phafile,$cor_expr,\$expocor,$picol)) { return 1; }
    # Skip any bins that have no exposure
    if($expocor==0) { next; }

    $phaexpr .= "$phafile*($expocor/$sciexpo)+";

  } # end loop on COR

  # Cleanup expression
  $phaexpr =~ s/^[+]//; # remove any leading plus signs
  $phaexpr =~ s/[+]$//; # remove any trailing plus signs
  $phaexpr =~ s/[+][+]+/[+]/; # remove any plus sign multiples

  # Verify we have an expression
  if(length($phaexpr) == 0) {
    ahlog::ah_err "An invalid expression for mathpha was formed.";
    return 1;
  }

  # Write the expression to a text file
  open PHASUM, ">", $phasum;
  print PHASUM $phaexpr;
  close PHASUM;

  # Create weight-averaged NXB spectrum
  ahlog::ah_info "HIGH", "Creating weight-averaged NXB spectrum";
  $status = ahgen::run_ftool("mathpha","\@$phasum","R",
                             $outpifile,"CALC","%","ncomments=0");
   if( $status ) {
    ahlog::ah_err "Cannot create spectrum from mathpha";
    return $status;
  }

  return 0;
}

sub Finalize {

  ahlog::ah_out "Finalizing";

  # fselect SCIEHK with GTI from SCIEVT to create an observation specific EHK file
  my $gtiexpr = "gtifilter(\"$infile\[GTI]\")";
  if(run_ftselect($outehkfile,"EHK",$outehkfile.".nightgti",$gtiexpr)) { ahapp::add_temp_file($outehkfile.".nightgti"); return 1; }
  if(copyOrMoveFileBasedOnCleanup($outehkfile.".nightgti",$outehkfile,$ahapp::cleanup)) { return 1; }

  # Write parameters to output files
  if(!isFileNone($outehkfile)) { 
    ahapp::write_parameters ($outehkfile); 
    unless (ahgen::update_checksum_and_verify ($outehkfile)) {
      ahgen::ah_err "FITS file: $outehkfile failed FITS verification test." ;
      return ahgen::get_error_flag;
    }
  }
  if(!isFileNone($outnxbfile)) { 
    ahapp::write_parameters ($outnxbfile); 
    unless (ahgen::update_checksum_and_verify ($outnxbfile)) {
      ahgen::ah_err "FITS file: $outnxbfile failed FITS verification test." ;
      return ahgen::get_error_flag;
    }
  }
  if(!isFileNone($outnxbehk) ) { 
    ahapp::write_parameters ($outnxbehk) ; 
    unless (ahgen::update_checksum_and_verify ($outnxbehk)) {
      ahgen::ah_err "FITS file: $outnxbehk failed FITS verification test." ;
      return ahgen::get_error_flag;
    }
  } 

  # Copy keywords to outpifile and update TSTART/TSTOP
  if(ahfilterlib::copy_keywords($infile,"EVENTS",$outpifile,"SPECTRUM","EVENTS","ALL")) { return 1; }
  if(ahgen::set_keyword($outpifile,"SPECTRUM","TSTART",$t1)) { return 1; }
  if(ahgen::set_keyword($outpifile,"SPECTRUM","TSTOP",$t2)) { return 1; }
  ahapp::write_parameters ($outpifile);
  unless (ahgen::update_checksum_and_verify ($outpifile)) {
    ahgen::ah_err "FITS file: $outpifile failed FITS verification test." ;
    return ahgen::get_error_flag;
  }

  return 0;
}

# ------------------------------------------------------------------------------

sub run_ftselect ($$$) {

  my $toolname="ftselect";
  my $infile=shift;
  my $ext=shift;
  my $outfile=shift;
  my $expr=shift;

  # ftselect parameters
  my @params = (
    ['infile'       , $infile."[$ext]"],
    ['outfile'      , $outfile],
    ['expr'         , $expr], 
    ['clobber'      , "yes"]
  );

  if(runTool($toolname,\@params)) { return 1; }

  # Check that not all rows were filtered
  my $naxis2 = ahgen::get_keyword($outfile,$ext,"NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $outfile, NAXIS2 keyword not defined.\n";
    return ahgen::get_error_flag;
  }
  if($naxis2 == 0) {
    ahlog::ah_err "Filtered all rows in event file $infile";
    return 1;
  }

  return 0;

}

# ------------------------------------------------------------------------------

sub run_sxipi {

  my $infile=shift;
  my %sxipi_pars=%{shift()};

  $sxipi_run++;

  my $toolname="sxipi";
  my $tmpoutfile=formTemporaryFileName($infile,$toolname.$sxipi_run);
  my $outfile=$infile;

  my @params = (
    ['infile'       , $infile],
    ['outfile'      , $tmpoutfile], 
    ['clobber'      , $ahapp::clobber ? "YES" : "NO"]
  ); 
  my @ordered_pars = qw( hkfile hkext hkcolstem hkvideoid vtevnoddfile chtrailfile 
                         ctifile spthfile gainfile patternfile startcol evnoddcor 
                         chtrailcor cticor gaincor ctigrade copygrade phcut badpixopt 
                         spthiter spthcaldb spthoffset spthslope evtthre negthre deltatime 
                         debugcol randomize seed );
  foreach my $par (@ordered_pars) {
    if($sxipi_pars{$par}) { push @params, [$par, $sxipi_pars{$par}]; }
  }

  if(runTool($toolname,\@params)) { unlink $tmpoutfile; return 1; }
  if(copyOrMoveFileBasedOnCleanup($tmpoutfile,$outfile,$ahapp::cleanup)) { return 1; }

  return 0;

}

# ------------------------------------------------------------------------------

sub run_maketime ($$$) {

  my $toolname="maketime";
  my $infile=shift;
  my $outfile=shift;
  my $mtexpr=shift;
  my $tmpoutfile=formTemporaryFileName($outfile,$toolname);

  my @params = (
    ['infile'    , $infile],
    ['outfile'   , $outfile],
    ['expr'      , $mtexpr],
    ['name'      , "-"],
    ['value'     , "-"],
    ['time'      , "TIME"],
    ['start'     , "START"],
    ['stop'      , "STOP"],
    ['compact'   , "no"],
    ['prefr'     , 0.0],
    ['postfr'    , 1.0],
    ['clobber'   , "yes"]
  );

  if(runTool($toolname,\@params)) { return 1; }

  return 0;

}

# ------------------------------------------------------------------------------

sub createStartStopGTI ($) {

  my $outfile = shift;

  # setup/create temporary files:
  my $cdfile = "cdfile.tmp";
  my $datafile = "datafile.tmp";

  my $status = 0;

  ahapp::add_temp_file($cdfile);
  ahapp::add_temp_file($datafile);

  open CDFILE, ">", $cdfile;
  print CDFILE "START 1D s\n";
  print CDFILE "STOP 1D s";
  close CDFILE;

  open DATAFILE, ">", $datafile;
  print DATAFILE "$t1 $t2";
  close DATAFILE;

  $status = ahgen::run_ftool("ftcreate","cdfile=$cdfile","datafile=$datafile",
                             "outfile=$outfile","extname=GTI","clobber=yes");
   if( $status ) {
    ahlog::ah_err "Error creating temporary GTI file $outfile";
    return $status;
  }

  # run mgtime to create a cut NXB GTI
  my $TmpOutfile=formTemporaryFileName($outfile,"mgtime.gti");
  my $nxbgti=$outnxbfile."[GTI]";
  my $StartStopGTI=$outfile."[GTI]";
  ahapp::add_temp_file($TmpOutfile);
  $status = ahgen::run_ftool("mgtime","ingtis=$nxbgti,$StartStopGTI", 
                             "outgti=!$TmpOutfile","merge=AND","indates=-");
   if( $status ) {
    ahlog::ah_err "Error creating temporary NXB GTI file $TmpOutfile";
    return $status;
  }
  if(copyOrMoveFileBasedOnCleanup($TmpOutfile,$outfile,$ahapp::cleanup)) { return 1; }

  return $status;

}

sub extractNXBSpectrum ($$$$) {

  my $phafile = shift;
  my $mtexpr = shift;
  my $expocor = shift;
  my $picolname = shift;

  my $gticor = "gti.tmp";
  my $gticor2 = "gti2.tmp";

  my $status = 0;

  ahapp::add_temp_file($gticor);
  ahapp::add_temp_file($gticor2);

  # a. maketime on the SCIEHK using cor range
  if(run_maketime($outehkfile,$gticor,$mtexpr)) { return 1; }

  my $expo_normal = ahfilterlib::get_gti_sum($gticor, "STDGTI");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "Failed calculate exposure of $outehkfile";
    return ahgen::get_error_flag;
  }
  if($expo_normal==0) {
    ahlog::ah_out " *** No exposure in bin $mtexpr";
    $$expocor=0;
    return 0;
  }

  # merge with science GTI (night GTI)
  # This needs to be done after maketime has been run so that there
  # are no gaps in the EHK file which may cause invalid GTIs
  $status = ahgen::run_ftool("mgtime","ingtis=$infile\[GTI],$gticor", 
                             "outgti=!$gticor2","merge=AND","indates=-");
  if( $status ) {
    ahlog::ah_err "Error merging temporary NXB GTI file $gticor2";
    return $status;
  }

  # b. calculate exposure for the COR(i) GTI files EXPOCOR(i) from SCIEHK
  ($$expocor) = ahfilterlib::get_gti_sum($gticor2, "STDGTI");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "Failed calculate exposure of $outehkfile";
    return ahgen::get_error_flag;
  }
  if($$expocor==0) {
    ahlog::ah_out " *** No exposure in bin $mtexpr";
    return 0;
  }

  # c. maketime on the NXBEHK using cor rangehk
  if(run_maketime($outnxbehk,$gticor,$mtexpr)) { return 1; }

  # d. extract spectrum NXBSPEC in NXB using the GTI where COR(i) is valid
  #    using the screened NXBEVT save
  $status = ahgen::run_ftool("extractor", "filename=$outnxbfile", "eventsout=NONE",
                   "imgfile=NONE", "phafile=$phafile", "fitsbinlc=NONE",
                   "regionfile=NONE", "timefile=$gticor",
                   "xcolf=X", "ycolf=Y", "tcol=TIME",
                   "ecol=$picolname", "xcolh=DETX", "ycolh=DETY");
  if( $status ) {
    ahlog::ah_err "Cannot create spectrum $phafile\n";
    return $status;
  }

  return $status;

}

# $Log: ahnxbgen.pl,v $
# Revision 1.24  2016/12/14 15:19:40  rshill
# Make regmode consistently case-insensitive.
#
# Revision 1.23  2016/12/02 17:11:23  mwitthoe
# ahnxbgen: fix typo in function call needed by SXS operation
#
# Revision 1.22  2016/12/02 15:59:53  mwitthoe
# ahnxbgen: for SXS, add parameter picol to specifiy which PI column to use
#
# Revision 1.21  2016/04/06 07:06:10  asargent
# Updated prefr=0 and postfr=1 for maketime runs
#
# Revision 1.20  2016/02/29 16:50:12  asargent
# Added CHECKSUM/DATASUM and ftverify for output files.
#
# Revision 1.19  2016/02/25 19:14:20  asargent
# Fixed bug where science GTI was being prematurely filtering EHK parameters. GTI is now merged for each PHA bin after initial GTI creation. EHK is filtered for science GTI during finalization.
#
# Revision 1.18  2016/01/12 18:05:01  asargent
# Updated pixel parameter parsing function to be more robust, and fixed pixel expression to combine with ORs rather than ANDs.
#
# Revision 1.17  2016/01/11 22:41:19  asargent
# Added sxs pixels filtering if no input region file
#
# Revision 1.16  2016/01/11 14:55:27  asargent
# Various bug fixes and updates to ahnxbgen and par files
#
# Revision 1.15  2015/11/25 17:41:46  asargent
# Ignore case in regmode parameter when selecting region
#
# Revision 1.14  2015/11/25 17:39:57  asargent
# Additional sxipi parameters and second sxipi run.
#
# Revision 1.13  2015/10/29 22:13:59  asargent
# Added pseudo file to hxinxbgen
#
# Revision 1.12  2015/10/20 16:51:36  asargent
# Fixed bug in ahnxbgen when using get_gti_sum
#
# Revision 1.11  2015/08/14 18:24:06  asargent
# Changed runTool params from hash to array
#
# Revision 1.10  2015/08/12 19:13:45  asargent
# Added extension requirement to run_ftselect, added missing PA_NOM keyword retrieval
#
# Revision 1.9  2015/08/11 17:45:17  asargent
# Added copy_keyword function to output spectrum file
#
# Revision 1.8  2015/08/06 21:06:26  asargent
# Removed coordpnt functionality. Have SXS use ExtractSXISpectra()
#
# Revision 1.7  2015/08/06 03:43:51  asargent
# Only read coordpnt parameters if running coordpnt
#
# Revision 1.6  2015/08/06 02:10:39  asargent
# Added tsaacol parameter for hxi
#
# Revision 1.5  2015/08/05 23:17:24  asargent
# Added additional error checking to run_ftselect
#
# Revision 1.4  2015/08/05 21:29:57  asargent
# Updated sxipi parameters, removed selectfile and label parameters.
#
# Revision 1.3  2015/08/05 21:04:16  asargent
# General cleanup of ahnxbgen. Moved to mission to merge HXI, SXI and SXS into single script.
#
# Revision 1.2  2015/08/03 01:45:35  asargent
# Added sxi specific routines, cleaned up tool.
#
# Revision 1.1  2015/07/31 18:35:56  asargent
# Initial version of merged nxbgen
#
