#!/usr/bin/perl
#
# File name: hxisgddtime
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/06/10 20:02:54 $
# Version: 0
#
# hxisgdgainfit generates either a deadtime corrected lightcurve or spectra
#
# Tool Dependencies:
#   ftselect
#   ftcalc
#   ftpaste
#   extractor
#   mathpha
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
use ahfilterlib ;
use File::Basename;

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

our $infile;
our $inlcfile;
our $inspecfile;
our $rspfile;
our $outlcfile;
our $outspecfile;
our $outrmfile;
our $gtifile;
our $phaseinfo;
our $merge;
our $expr;
our $mintimedel;
our $rspweight;

# Temporary files
our @in_evt_files = ();      # array of input event files
our @in_lc_files = ();       # array of input lightcurve files
our @in_spec_files = ();     # array of input spectra files
our @in_gti_files = ();     # array of input gti files
our @in_rsp_files = ();     # array of input response files
our @evt_files = ();      # array of input event files
our @out_lc_files = ();      # array of output event files
our @out_spec_files = ();      # array of output event files
our @gti_ext = ();      # array of output event files
our @spec_ext = ();      # array of output event files
our $num_evt_files = 0;
our $num_lc_files = 0;
our $num_spec_files = 0;
our $num_gti_files = 0;
our $num_rsp_files = 0;
our @rsp_weights = ();     # array of input response files

our $timedel = -1;
our $calclc = 0;
our $calcspec = 0;

our @instrume = ();
our @detnam   = ();
our @insdet   = ();
our %instcheck = (
  hxi     => 0,   # Counter for HXI files
  sgd     => 0,   # Counter for SGD files
  sgd1    => 0,   # Counter for SGD1 files
  sgd2    => 0,   # Counter for SGD2 files

  hxi1    => 0,   # Boolean for HXI1
  hxi2    => 0,   # Boolean for HXI2
  sgd1cc1 => 0,   # Boolean for SGD1 CC1
  sgd1cc2 => 0,   # Boolean for SGD1 CC2
  sgd1cc3 => 0,   # Boolean for SGD1 CC3
  sgd2cc1 => 0,   # Boolean for SGD2 CC1
  sgd2cc2 => 0,   # Boolean for SGD2 CC2
  sgd2cc3 => 0,   # Boolean for SGD2 CC3
);

our $dtimeerror = 0;

#########################
#  Main Code Block 
#########################

# Query canonical APE parameters and start logging. 
ahapp::startup () ;

# Pre-Processing
ahapp::begin_processing();

# Get the input parameters
$dtimeerror = GetInputParameters();
unless ( $dtimeerror == 0 ) {
  ahlog::ah_debug "GetInputParameters" ;
  ahapp::end_processing($dtimeerror);
}

# Check the input files for clobber
$dtimeerror = CheckInput();
unless ( $dtimeerror == 0 ) {
  ahlog::ah_debug "CheckInput" ;
  ahapp::end_processing($dtimeerror);
}

# Write all parameters to this script to the log file.
ah_info "HIGH", ahapp::write_parameters () ;                 

# Select for pseudo events using GTI or expression
$dtimeerror = SelectPseudoEvents();
unless ( $dtimeerror == 0 ) {
  ahlog::ah_debug "SelectPseudoEvents" ;
  ahapp::end_processing($dtimeerror);
}

# Main Processing 

# Check the input files for clobber
$dtimeerror = CalcDeadtime();
unless ( $dtimeerror == 0 ) {
  ahlog::ah_debug "CalcDeadtime" ;
  ahapp::end_processing($dtimeerror);
}

# Check the input files for clobber
$dtimeerror = CalcSpectra();
unless ( $dtimeerror == 0 ) {
  ahlog::ah_debug "CalcSpectra" ;
  ahapp::end_processing($dtimeerror);
}

# Verify file is okay
$dtimeerror = Finalize();
unless ( $dtimeerror == 0 ) {
  ahlog::ah_debug "Finalize" ;
  ahapp::end_processing($dtimeerror);
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

  $infile       = ahapp::query_parameter("infile");
  $inlcfile     = ahapp::query_parameter("inlcfile");
  $inspecfile   = ahapp::query_parameter("inspecfile");
  $rspfile      = ahapp::query_parameter("rspfile");
  $outlcfile    = ahapp::query_parameter("outlcfile");
  $outspecfile  = ahapp::query_parameter("outfile");
  $gtifile      = ahapp::query_parameter("gtifile");
  $phaseinfo    = ahapp::query_parameter("phaseinfo");
  $merge        = ahapp::query_parameter("merge",1);
  $expr         = ahapp::query_parameter("expr");
  $mintimedel   = ahapp::query_parameter("mintimedel");
  $rspweight    = ahapp::query_parameter("rspweight");

  return 0;

}

#
# Description:
#
# Check if the output file(s) already exist.  Unless clobber is set, this will
# cause the script to fail.
#
# Parameters:
# [in] infile           input pseudo event file(s)
# [in] inlcfile         input lightcurve file(s)
# [in] inspecfile       input spectra file(s)
# [in] gtifile          input gti file
# [in] in_evt_files     array of input pseudo event files
# [in] in_lc_files      array of input lightcurves
# [in] in_spec_files    array of input spectra
# [in] outlcfile        output lightcurve file or root
# [in] outspecfile      output spectra file or root
# [in] num_evt_files    total number of input event files
# [in] num_lc_files     total number of input lightcurves
# [in] num_spec_files   total number of input spectra
# [in] calclc           flag to run the lightcurve calculation
# [in] calcspec         flag to run the spectra calculation
# [in] merge            merge output files (yes/no)
# [out] status          Output status
#
sub CheckInput {

  my $status = 0;

  # Check if @ file lists exist
  if ($infile =~ /^@/) {
    if(isRequiredFileNotFound(substr($infile,1))) { return 1;} 
  }
  if ($inlcfile =~ /^@/) {
    if(isRequiredFileNotFound(substr($inlcfile,1))) { return 1; } 
  }
  if ($inspecfile =~ /^@/) {
    if(isRequiredFileNotFound(substr($inspecfile,1))) { return 1; } 
  }
  if ($gtifile =~ /^@/) {
    if(isRequiredFileNotFound(substr($gtifile,1))) { return 1; } 
  }
  if ($rspfile =~ /^@/) {
    if(isRequiredFileNotFound(substr($rspfile,1))) { return 1; } 
  }

  # Read list of input files from each parameter 
  @in_evt_files = readInputFileList($infile);
  @in_lc_files = readInputFileList($inlcfile);
  @in_spec_files = readInputFileList($inspecfile);
  @in_gti_files = readInputFileList($gtifile);

  # Check each input file exists
  if(isRequiredFileNotFound(@in_evt_files)) { return 1;}
  if(isRequiredFileNotFound(@in_lc_files)) { return 1; }
  if(isRequiredFileNotFound(@in_spec_files)) { return 1; }
  if(isRequiredFileNotFound(@in_gti_files)) { return 1; }

  # get number of pseudo event files to process
  $num_evt_files = @in_evt_files;
  $num_lc_files = @in_lc_files;
  $num_spec_files = @in_spec_files;
  $num_gti_files = @in_gti_files;
  $num_rsp_files = @in_rsp_files;

  if(removeOutputFileIfClobbering($outlcfile,$ahapp::clobber)) { return 1; }
  if(removeOutputFileIfClobbering($outspecfile,$ahapp::clobber)) { return 1; }

  # Nothing to merge if only one file
  if($num_evt_files==1) { $merge=0; }

  # Check for rsp files
  if($merge && $num_spec_files>1) { 
    @in_rsp_files = readInputFileList($rspfile); 
    if(isRequiredFileNotFound(@in_rsp_files)) { return 1; }
    $num_rsp_files = @in_rsp_files;
  }

  if($num_lc_files) { 
    if($num_lc_files != $num_evt_files) { 
      ahlog::ah_err "Mismatching number of input lightcurves and event files"; 
      return 1; 
    }
    $calclc=1; 
  } else {
    $outlcfile = "NONE";
  }
  if($num_spec_files) { 
    if($num_spec_files != $num_evt_files) { 
      ahlog::ah_err "Mismatching number of input spectra and event files"; 
      return 1; 
    }
    # Check for correct number of response weights
    if($merge) {
      if($num_rsp_files != $num_spec_files) { 
        ahlog::ah_err "Mismatching number of input response and event files"; 
        return 1; 
      }
    }
    $calcspec=1; 
    $outrmfile = "NONE";
  } else {
    $outspecfile = "NONE";
  }
  if($num_gti_files != $num_evt_files) { 
    ahlog::ah_err "Mismatching number of input GTI and event files"; 
    return 1; 
  }

  # Check that we are doing something
  unless ($calclc or $calcspec) { ahlog::ah_err "Nothing to do."; return 1; }

  ah_out "Processing $num_evt_files pseudo event files: ";
  ah_out "  $_" foreach @in_evt_files;
  ah_out "Processing $num_gti_files GTI files: ";
  ah_out "  $_" foreach @in_gti_files;
  if($num_lc_files) {
    ah_out "Processing $num_lc_files lightcurves: ";
    ah_out "  $_" foreach @in_lc_files;
  }
  if($num_spec_files) {
    ah_out "Processing $num_spec_files spectra: ";
    ah_out "  $_" foreach @in_spec_files;
  }


  # Verify matching input files and save pseudohz keywords
  for (my $ii = 0; $ii < $num_evt_files; $ii++) {

    my $evt_file = $in_evt_files[$ii];
    my $lc_camera = $in_lc_files[$ii];
    my $spec_file = $in_spec_files[$ii];
    my $gti_file = $in_gti_files[$ii];

    # Read the INSTRUME and DETNAM keywords
    my $evt_instrume = ahgen::get_keyword($evt_file,"EVENTS","INSTRUME");
    if(ahgen::get_error_flag) {
      ahlog::ah_err "Keyword INSTRUME not found in file $evt_file\n";
      return ahgen::get_error_flag;
    }
    my $evt_detnam = ahgen::get_keyword($evt_file,"EVENTS","DETNAM");
    if(ahgen::get_error_flag) {
      ahlog::ah_err "Keyword DETNAM not found in file $evt_file\n";
      return ahgen::get_error_flag;
    }

    ahlog::ah_debug "\nFor evt file: $evt_file";
    ahlog::ah_debug "Found INSTRUME and DETNAM:";
    ahlog::ah_debug "  INSTRUME = $evt_instrume";
    ahlog::ah_debug "  DETNAM   = $evt_detnam";
    ahlog::ah_debug "  Checking files:";
    ahlog::ah_debug "    Lightcurve : $lc_camera" if $calclc;
    ahlog::ah_debug "    Spectra    : $spec_file" if $calcspec;
    ahlog::ah_debug "    GTI        : $gti_file";

    # Check for valid instrument
    if($evt_instrume =~ /hx/i) {
      # HXI: Verify that we are not mixing with SGD files
      #      Check INSTRUME for HXI1, HXI2 or HXI
      #      Check for doubles
      if($instcheck{sgd}) { ahlog::ah_err "Cannot mix HXI and SGD files"; return 1; }
      if(uc $evt_instrume eq "HXI1") {
        if($instcheck{hxi1}) { 
          ahlog::ah_err "Instrument $evt_instrume input twice."; 
          ahlog::ah_err "Only one INSTRUME and DETNAM combo allowed."; 
          return 1; 
        }
        $instcheck{hxi1} = 1;
      } elsif (uc $evt_instrume eq "HXI2") {
        if($instcheck{hxi2}) { 
          ahlog::ah_err "Instrument $evt_instrume input twice."; 
          ahlog::ah_err "Only one INSTRUME and DETNAM combo allowed."; 
          return 1; 
        }
        $instcheck{hxi2} = 1;
      } elsif ($instcheck{hxi} > 1) { 
        ahlog::ah_err "Too many input files for instrument $evt_instrume "; 
        return 1;
      } elsif (uc $evt_instrume ne "HXI") {
        ahlog::ah_err "In file $evt_file, INSTRUME $evt_instrume is not a valid instrument";
        return 1;
      }
      $instcheck{hxi} += 1;
      push @insdet, lc $evt_instrume;
    } elsif ($evt_instrume =~ /sg/i) {
      # SGD: Verify that we are not mixing with HXI files
      #      Check INSTRUME for SGD1, SGD2 or SGD 
      #      Check DETNAM for CC1, CC2 or CC3
      #      Check for doubles
      if($instcheck{hxi}) { ahlog::ah_err "Cannot mix HXI and SGD files"; return 1; }
      if(uc $evt_instrume eq "SGD1") {
        if($evt_detnam =~ /cc1/i) {
          if($instcheck{sgd1cc1}) { 
            ahlog::ah_err "Instrument $evt_instrume and DETNAM $evt_detnam input twice."; 
            ahlog::ah_err "Only one INSTRUME and DETNAM combo allowed."; 
            return 1; 
          }
          $instcheck{sgd1cc1} = 1;
        } elsif ($evt_detnam =~ /cc2/i) {
          if($instcheck{sgd1cc2}) { 
            ahlog::ah_err "Instrument $evt_instrume and DETNAM $evt_detnam input twice."; 
            ahlog::ah_err "Only one INSTRUME and DETNAM combo allowed."; 
            return 1; 
          }
          $instcheck{sg1cc2} = 1;
        } elsif ($evt_detnam =~ /cc3/i) {
          if($instcheck{sgd1cc3}) { 
            ahlog::ah_err "Instrument $evt_instrume and DETNAM $evt_detnam input twice."; 
            ahlog::ah_err "Only one INSTRUME and DETNAM combo allowed."; 
            return 1; 
          }
          $instcheck{sg21cc3} = 1;
        } elsif($instcheck{sgd1}>1) { 
          ahlog::ah_err "Too many input files for instrument $evt_instrume "; 
          return 1;
        } else {
          ahlog::ah_err "In file $evt_file, INSTRUME $evt_instrume DETNAM $evt_detnam is not a valid instrument/detnam combo";
          return 1;
        }
        $instcheck{sgd1} += 1;
      } elsif (uc $evt_instrume eq "SGD2") {
        if($evt_detnam =~ /cc1/i) {
          if($instcheck{sgd2cc1}) { 
            ahlog::ah_err "Instrument $evt_instrume and DETNAM $evt_detnam input twice."; 
            ahlog::ah_err "Only one INSTRUME and DETNAM combo allowed."; 
            return 1; 
          }
          $instcheck{sg2cc1} = 1;
        } elsif ($evt_detnam =~ /cc2/i) {
          if($instcheck{sgd2cc2}) { 
            ahlog::ah_err "Instrument $evt_instrume and DETNAM $evt_detnam input twice."; 
            ahlog::ah_err "Only one INSTRUME and DETNAM combo allowed."; 
            return 1; 
          }
          $instcheck{sg2cc2} = 1;
        } elsif ($evt_detnam =~ /cc3/i) {
          if($instcheck{sgd2cc3}) { 
            ahlog::ah_err "Instrument $evt_instrume and DETNAM $evt_detnam input twice."; 
            ahlog::ah_err "Only one INSTRUME and DETNAM combo allowed."; 
            return 1; 
          }
          $instcheck{sg2cc3} = 1;
        } elsif($instcheck{sgd2}>1) { 
          ahlog::ah_err "Too many input files for instrument $evt_instrume "; 
          return 1;
        } else {
          ahlog::ah_err "In file $evt_file, INSTRUME $evt_instrume DETNAM $evt_detnam is not a valid instrument/detnam combo";
          return 1;
        }
        $instcheck{sgd2} += 1;
      } elsif ($instcheck{sgd}>1) {
        ahlog::ah_err "Too many input files for instrument $evt_instrume "; 
        return 1;
      } elsif (uc $evt_instrume ne "SGD") {
        ahlog::ah_err "In file $evt_file, INSTRUME $evt_instrume is not a valid instrument";
        return 1;
      }
      $instcheck{sgd} += 1;
      push @insdet, lc $evt_instrume . "_" . lc $evt_detnam;
    } else {
      ahlog::ah_err "In $evt_file, INSTRUME $evt_instrume not a valid instrument";
      ahlog::ah_err "Instrument should be HXI, HXI1, HXI2 or SGD, SGD1 or SGD2";
      return 1;
    }

    # Verify the input GTI file INSTRUME/DETNAM matches the input pseudo event file
    # Find the GTI extension. Use the given extension or search for HDUCLAS1='GTI     '
    my $hduclas1="";
    my ($gti,$gtiext) = ahgen::parse_file_name($gti_file);
    if(length($gtiext)==0) {
      my $numhdu = ahgen::get_total_hdu($gti);
      foreach my $hdu (1..$numhdu) {
         $hduclas1 = ahgen::get_keyword($gti,$hdu,"HDUCLAS1");
         if(ahgen::get_error_flag) {
           ahlog::ah_err "Keyword HDUCLAS1 not found in file $gti_file\[$hdu]\n";
           return ahgen::get_error_flag;
         }
         if(lc $hduclas1 eq "gti") { $gtiext = $hdu; last; }
      }
    } else {
      $hduclas1 = ahgen::get_keyword($gti,$gtiext,"HDUCLAS1");
      $gti_file = $gti;
    }
    push @gti_ext, $gtiext;
    if(lc $hduclas1 ne "gti") {
      ahlog::ah_err "Could not find GTI extension in file $gti_file";
      return 1;
    }
    my $gti_instrume = ahgen::get_keyword($gti_file,$gtiext,"INSTRUME");
    if(ahgen::get_error_flag) {
      ahlog::ah_err "Keyword INSTRUME not found in file $gti_file\n";
      return ahgen::get_error_flag;
    }
    if($gti_instrume ne $evt_instrume) {
      ahlog::ah_err "INSTRUME keyword does not match in $gti_file\[$gtiext] and $evt_file";
      return 1;
    }
    my $gti_detnam = ahgen::get_keyword($gti_file,$gtiext,"DETNAM");
    if(ahgen::get_error_flag) {
      ahlog::ah_err "Keyword DETNAM not found in file $gti_file\n";
      return ahgen::get_error_flag;
    }
    if($gti_detnam ne $evt_detnam) {
      ahlog::ah_err "DETNAM keyword does not match in $gti_file\[$gtiext] and $evt_file";
      return 1;
    }

    # Verify the input lc file INSTRUME/DETNAM matches the input pseudo event file
    if($calclc) {
      my $lc_instrume = ahgen::get_keyword($lc_camera,"RATE","INSTRUME");
      if(ahgen::get_error_flag) {
        ahlog::ah_err "Keyword INSTRUME not found in file $lc_camera\n";
        return ahgen::get_error_flag;
      }
      if($lc_instrume ne $evt_instrume) {
        ahlog::ah_err "INSTRUME keyword does not match in $lc_camera and $evt_file";
        return 1;
      }
      my $lc_detnam = ahgen::get_keyword($lc_camera,"RATE","DETNAM");
      if(ahgen::get_error_flag) {
        ahlog::ah_err "Keyword DETNAM not found in file $lc_camera\n";
        return ahgen::get_error_flag;
      }
      if($lc_detnam ne $evt_detnam) {
        ahlog::ah_err "DETNAM keyword does not match in $lc_camera and $evt_file";
        return 1;
      }
      my $binsize = ahgen::get_keyword($lc_camera,"RATE","TIMEDEL");
      if(ahgen::get_error_flag) {
        ahlog::ah_err "Keyword TIMEDEL not found in file $lc_camera\n";
         return ahgen::get_error_flag;
      }
      if($binsize < $mintimedel) {
        ahlog::ah_err "TIMEDEL smaller than minimum in file $lc_camera (mintimedel=$mintimedel)";
        return 1;
      }
      # Check for matching TIMEDEL keywords
      if($timedel==-1) { $timedel = $binsize; }
      if($binsize != $timedel) {
        ahlog::ah_err "Non-matching TIMEDEL keywords in input lightcurve files. Cannot sum lightcurves.";
        return 1;
      }
      
      # Set up output file name
      my $outlc="";
      if($num_evt_files==1) {
        $outlc = $outlcfile;
      } elsif(!$merge) { 
        $outlc = $outlcfile . lc $evt_instrume . "_" . lc $evt_detnam . ".lc";
      } else {
        $outlc = "temp_" . lc $evt_instrume . "_" . lc $evt_detnam . ".lc";
        ahapp::add_temp_file($outlc); 
      }
      if(removeOutputFileIfClobbering($outlc,$ahapp::clobber)) { return 1; }
      push @out_lc_files, $outlc;

    }

    # Verify the input spectra file INSTRUME/DETNAM matches the input pseudo event file
    if($calcspec) {
      # Find the GTI extension. Use the given extension or search for HDUCLAS1='GTI     '
      my ($spec,$specext) = ahgen::parse_file_name($spec_file);
      if(length($specext)==0) {
        my $numhdu = ahgen::get_total_hdu($spec);
        foreach my $hdu (0..$numhdu) {
           $hduclas1 = ahgen::get_keyword($spec,$hdu,"HDUCLAS1");
           if(ahgen::get_error_flag) {
             ahlog::ah_err "Keyword HDUCLAS1 not found in file $spec_file\[$hdu]\n";
             return ahgen::get_error_flag;
           }
           if(lc $hduclas1 eq "spectrum") { $specext = $hdu; last; }
        }
      } else {
        $hduclas1 = ahgen::get_keyword($spec,$specext,"HDUCLAS1");
        $spec_file = $spec;
      }
      push @spec_ext, $specext;
      if(lc $hduclas1 ne "spectrum") {
        ahlog::ah_err "Could not find SPECTRUM extension in file $spec_file";
        return 1;
      }
      my $spec_instrume = ahgen::get_keyword($spec_file,$specext,"INSTRUME");
      if(ahgen::get_error_flag) {
        ahlog::ah_err "Keyword INSTRUME not found in file $spec_file\[$specext]\n";
        return ahgen::get_error_flag;
      }
      if($spec_instrume ne $evt_instrume) {
        ahlog::ah_err "INSTRUME keyword does not match in $spec_file\[$specext] and $evt_file";
        return 1;
      }
      my $spec_detnam = ahgen::get_keyword($spec_file,$specext,"DETNAM");
      if(ahgen::get_error_flag) {
        ahlog::ah_err "Keyword DETNAM not found in file $spec_file\[$specext]\n";
        return ahgen::get_error_flag;
      }
      if($spec_detnam ne $evt_detnam) {
        ahlog::ah_err "DETNAM keyword does not match in $spec_file\[$specext] and $evt_file";
        return 1;
      }
      # Check for correct number of response weights
      if( $merge ) {
        @rsp_weights = split ",",$rspweight;
        if( @rsp_weights != $num_rsp_files ) { 
          ahlog::ah_err "Invalid corresponding number of response weights for addrmf ($num_rsp_files required)";
          return 1;
        }
      }

      # Set up output file names
      my $outspec="";
      # Check for output file clobber
      if($num_evt_files==1) {
        $outspec = $outspecfile;
      } elsif(!$merge) { 
        $outspec = $outspecfile . "_" . lc $evt_instrume . "_" . lc $evt_detnam . ".pha";
      } else { 
        $outrmfile = $outspecfile;
        $outrmfile =~ s/.*$//g;
        $outrmfile = $outspecfile . ".rmf";
        $outspec = "temp_" . lc $evt_instrume . "_" . lc $evt_detnam . ".pha";
        ahapp::add_temp_file($outspec); 
      }
      if(removeOutputFileIfClobbering($outrmfile,$ahapp::clobber)) { return 1; }
      if(removeOutputFileIfClobbering($outspecfile . ".pha",$ahapp::clobber)) { return 1; }
      push @out_spec_files, $outspec;
    }

    push @instrume, $evt_instrume;
    push @detnam, $evt_detnam;


  } # end loop over input verifications

  ahlog::ah_debug "Processing: ";
  ahlog::ah_debug "HXI Files   : $instcheck{hxi}";
  ahlog::ah_debug "SGD Files   : $instcheck{sgd}";
  ahlog::ah_debug " $_" foreach @instrume;
  ahlog::ah_debug " $_" foreach @detnam;

  if($merge) {
    if($calcspec) { $outspecfile = $outspecfile . ".pha";}
  }

  # Print the output file information
  if(!$merge && $num_evt_files>1) {
    if($calclc) { 
      ahlog::ah_out "Creating lightcurve(s):";
      ahlog::ah_out "  $_" foreach @out_lc_files; 
    }
    if($calcspec) { 
      ahlog::ah_out "Creating spectra:";
      ahlog::ah_out "  $_" foreach @out_spec_files; 
    }
  } else {
    if($calclc) {
      ahlog::ah_out "Creating lightcurve(s):";
      ahlog::ah_out "  $outlcfile";
    }
    if($calcspec) {
      ahlog::ah_out "Creating spectra:";
      ahlog::ah_out "  $outspecfile";
      ahlog::ah_out "Creating RMF:";
      ahlog::ah_out "  $outrmfile";
    }
  }

  return 0;

}

#
# Description:
#
# filter events from pseudo event file(s)
#
# Parameters:
# [in] gtifile          input gti file
# [in] in_evt_files     array of input pseudo event files
# [in] expr             expression to filter event files
# [in] evt_files        array of working event files
# [out] status          Output status
#
sub SelectPseudoEvents {

  my $status = 0;

  my $TmpInfile = "";
  my $TmpOutfile = "";

  ahlog::ah_out "Cleaning events for file:";


  for (my $ii = 0; $ii < $num_evt_files; $ii++) {
    my $TmpInfile = $in_evt_files[$ii];
    my $gti_file  = $in_gti_files[$ii];
    my $gti_ext  = $gti_ext[$ii];

    ahlog::ah_out " $TmpInfile";

    # select PSEEVT with GTI file
    $TmpOutfile = $in_evt_files[$ii] . ".ftselect_gti.tmp";
    ahapp::add_temp_file($TmpOutfile);
    $status = ahgen::run_ftool("ftselect", $TmpInfile, $TmpOutfile, "gtifilter(\"$gti_file\[$gti_ext\]\")", "copyall=yes");
    if($status) {
      ahlog::ah_err "Error running tool ftslect on file $TmpInfile. Did not create ".$TmpOutfile .".\n";
      return $status;
    }

    # Put original GTI extension into filtered output file
    my $ftdelhduinfile = $TmpOutfile . "[GTI]";
    $status = ahgen::run_ftool("ftdelhdu","infile=$ftdelhduinfile","outfile=none","confirm=YES");
    if($status) {
      ahlog::ah_err "Error running tool ftdelhdu \n";
      return $status;
    }
  
    $status = ahgen::run_ftool("ftappend","infile=$gti_file\[$gti_ext\]","outfile=$TmpOutfile","mode=h");
    if($status) {
      ahlog::ah_err "Error running tool ftappend on file $TmpOutfile. \n";
      return $status;
    } 

    # select PSEEVT to clean additional pseudo events with flags
    if(lc $expr ne "none") {
      $TmpInfile = $TmpOutfile;
      $TmpOutfile = $in_evt_files[$ii] . ".ftselect_cl.tmp";
      ahapp::add_temp_file($TmpOutfile);
      $status = ahgen::run_ftool("ftselect", $TmpInfile, $TmpOutfile, $expr, "copyall=yes");
      if($status) {
        ahlog::ah_err "Error running tool ftselect on file $TmpInfile. Did not create ".$TmpOutfile .".\n";
        return $status;
      }
      
 
    }

    # Add cleaned (or uncleaned) event file to main event file array
    
    $TmpInfile = $TmpOutfile;
    push @evt_files, $TmpInfile;


  }

  return $status;
 
}

#
# Description:
#
# Calculate the deadtime and create lightcurves using the 
# extractor tool
#
# Parameters:
# [in] merge            merge output files (yes/no)
# [in] in_evt_files     array of input pseudo event files
# [in] in_lc_files      array of input lightcurves
# [in] out_lc_files     array of output lightcurves
# [in] num_evt_files    total number of input event files
# [in] num_lc_files     total number of input lightcurves
# [in] calclc           flag to run the lightcurve calculation
# [in] outlcfile        output lightcurve file or root
# [out] status          Output status
#
sub CalcDeadtime {

  my $timezero;

  my $lc_merge = 0;

  my $status = 0;

  unless ($calclc) { return 0; } # end if lightcurve

  if($merge && $num_lc_files>1) { $lc_merge = 1; }

  ahlog::ah_out "Creating Lightcurve for file: ";

  # loop through pseudo files
  for (my $ii = 0; $ii < $num_evt_files; $ii++) {

    my $evt_file = $evt_files[$ii];
    my $lc_camera = $in_lc_files[$ii];
    my $outlc = $out_lc_files[$ii];

    my $evt_instrume = $instrume[$ii];
    my $evt_detnam = $detnam[$ii];
    my $colname = lc $insdet[$ii];

    my $lc_pseudo = "";
    my $lc_dtcorr = "";
    my $fracexp = "";

    ahlog::ah_out "  $evt_file";

    # get keyword PSEUDOHZ pseudo file
    my $pseudohz = ahgen::get_keyword($lc_camera,"RATE","PSEUDOHZ");
    if(ahgen::get_error_flag) {
      ahlog::ah_err "In file " . $lc_camera. ":\n";
      ahlog::ah_err "Keyword PSEUDOHZ not found.\n";
      return ahgen::get_error_flag;
    }
    
    # extract lightcurve of lc_pseudo
    #$lc_pseudo = $evt_file . ".lc.tmp";
    $lc_pseudo = $colname . ".lc.tmp";
    ahapp::add_temp_file($lc_pseudo);

    $status = ahgen::run_ftool("extractor","filename=$evt_file","fitsbinlc=$lc_pseudo",
                               "binlc=$timedel","lcthresh=0.0","lctzero=yes",
                               # the rest of these parameters are unneeded,
                               # but required by extractor:
                               "eventsout=NONE","imgfile=NONE","phafile=NONE",
                               "regionfile=NONE","timefile=NONE","xcolf=NONE", 
                               "ycolf=NONE","tcol=TIME","ecol=NONE");
    if($status) {
      ahlog::ah_err "Could not create lightcurve for file $evt_file";
      return $status;
    }
    # Replace the USER keyword to avoid unit test issues...
    $status = ahgen::run_ftool("fthedit",$lc_pseudo,"USER","a","hxisgddtime");
    if($status) {
      ahlog::ah_err "Could not create lightcurve for file $evt_file";
      return $status;
    }

    # Check that first bin in lc and pseudo files are matching
    my ($evt_bin1) = ahgen::read_column($lc_pseudo,"RATE][col TIME=TIME+#TIMEZERO","TIME",1);
    # Get first time bin from event lightcurve
    my ($lc_bin1) = ahgen::read_column($lc_camera,"RATE][col TIME=TIME+#TIMEZERO","TIME",1);
    # Add zero to convert from string into number
    if($evt_bin1+0 != $lc_bin1+0) {
      ahlog::ah_err "First time bins in $lc_pseudo and $lc_camera do not match";
      return 1;
    }

    # calculate fraction of exposure with equation:
    $fracexp = "(RATE*#TIMEDEL*FRACEXP)/($pseudohz*#TIMEDEL*FRACEXP)";
    $status = ahgen::run_ftool("ftcalc","infile=$lc_pseudo","outfile=$lc_pseudo",
                               "column=LIVETIMEFRAC","expr=$fracexp","clobber=yes");
    if($status) {
      ahlog::ah_err "Could not calculate livetime fraction";
      return $status;
    }

    # ftpaste column LIVETIMEFRAC from LC_PSEUDO to LC_CAMERA (source lightcurve)
    $lc_dtcorr = $outlcfile . "_" . $colname . ".tmp";
    ahapp::add_temp_file($lc_dtcorr);
    $status = ahgen::run_ftool("ftpaste", "infile=$lc_pseudo\[RATE][col LIVETIMEFRAC]", 
                     "pastefile=$lc_camera\[RATE]", "outfile=$lc_dtcorr");
    if($status) {
      ahlog::ah_err "Could not copy LIVETIMEFRAC from $lc_pseudo to $lc_camera.";
      return $status;
    }

    # ftcalc LC_CAMERA columns RATE and ERROR to get LC_DTCORR (source lightcurve)
    if(!$status) {
      $status = ahgen::run_ftool("ftcalc", "infile=$lc_dtcorr\[RATE]", "outfile=$lc_dtcorr",
                     "column=RATE_CORR", "expression=RATE/LIVETIMEFRAC", "clobber=yes");
    }

    if(!$status) {
      $status = ahgen::run_ftool("ftcalc", "infile=$lc_dtcorr\[RATE]", "outfile=$lc_dtcorr", 
                     "column=ERROR_CORR", "expression=ERROR/LIVETIMEFRAC", "clobber=yes");
    }

    if(!$status) {
      $status = ahgen::run_ftool("ftcalc", "infile=$lc_dtcorr\[RATE]", "outfile=$lc_dtcorr", 
                     "column=FRACEXP_CORR", "expression=FRACEXP*LIVETIMEFRAC", "clobber=yes");
    }

    if($status) {
      ahlog::ah_err "Could not calculate deadtime correction columns in file $lc_dtcorr.";
      return $status;
    }

    # Write file to output
    $status = ahgen::copy_hdu($lc_dtcorr, "RATE", $outlc);
    if($status) {
      ahlog::ah_err "Could not create outfile from $outlc.";
      return $status;
    }

  } # end loop through pseudo files

  if($lc_merge) {

    ahlog::ah_out "Merging lightcurves";

    my $rate_expr = "";
    my $error_expr = "";
    my $rate_corr_expr = "";
    my $error_corr_expr = "";

    # Loop through each file and copy columns to output lightcurve file
    # Reverse the direction of the loop to preserve input file ordering
    for (my $ii = $num_evt_files-1; $ii >= 0; $ii--) {
      my $lc_dtcorr = $out_lc_files[$ii];
      my $colname = uc $insdet[$ii];
      $status = ahgen::copy_column($lc_dtcorr,"RATE",$outlcfile,
                                   "RATE_$colname==RATE",
                                   "ERROR_$colname==ERROR",
                                   "LIVETIMEFRAC_$colname==LIVETIMEFRAC",
                                   "FRACEXP_$colname==FRACEXP",
                                   "RATE_CORR_$colname==RATE_CORR",
                                   "ERROR_CORR_$colname==ERROR_CORR",
                                   "FRACEXP_CORR_$colname==FRACEXP_CORR");
      if($status) {
        ahlog::ah_err "Could not create outfile $outlcfile.";
        return $status;
      }
      $rate_expr .= "RATE_$colname+";
      $error_expr .= "(ERROR_$colname^2)+";
      $rate_corr_expr .= "RATE_CORR_$colname+";
      $error_corr_expr .= "(ERROR_CORR_$colname^2)+";
    }
    $status = ahgen::copy_column($out_lc_files[0],"RATE",$outlcfile,"TIME");
    if($status) {
      ahlog::ah_err "Could not create outfile $outlcfile.";
      return $status;
    }

    # Cleanup the expressions
    $rate_expr =~ s/\+$//;
    $error_expr =~ s/\+$//;
    $rate_corr_expr =~ s/\+$//;
    $error_corr_expr =~ s/\+$//;

    # Calculate the error
    $error_expr = "sqrt($error_expr)";
    $error_corr_expr = "sqrt($error_corr_expr)";

    $status = ahgen::run_ftool("ftcalc","infile=$outlcfile\[RATE]","outfile=$outlcfile",
                     "column=RATE","expression=$rate_expr","clobber=yes");
    if($status) {
      ahlog::ah_err "Could not calculate merged RATE in file $outlcfile.";
      return $status;
    }
    $status = ahgen::run_ftool("ftcalc","infile=$outlcfile\[RATE]","outfile=$outlcfile",
                     "column=ERROR","expression=$error_expr","clobber=yes");
    if($status) {
      ahlog::ah_err "Could not calculate merged ERROR in file $outlcfile.";
      return $status;
    }
    $status = ahgen::run_ftool("ftcalc","infile=$outlcfile\[RATE]","outfile=$outlcfile",
                     "column=RATE_CORR","expression=$rate_corr_expr","clobber=yes");
    if($status) {
      ahlog::ah_err "Could not calculate merged RATE_CORR in file $outlcfile.";
      return $status;
    }
    $status = ahgen::run_ftool("ftcalc","infile=$outlcfile\[RATE]","outfile=$outlcfile",
                     "column=ERROR_CORR","expression=$error_corr_expr","clobber=yes");
    if($status) {
      ahlog::ah_err "Could not calculate merged ERROR_CORR in file $outlcfile.";
      return $status;
    }

    @out_lc_files = ();
    push @out_lc_files, $outlcfile;

  }

  return $status;

}

#
# Description:
#
# Calculate the livetimefraction and create a spectra file if merging files
#
# Parameters:
# [in] merge            merge output files (yes/no)
# [in] in_evt_files     array of input pseudo event files
# [in] in_spec_files    array of input spectra
# [in] out_spec_files   array of output spectra
# [in] num_evt_files    total number of input event files
# [in] num_spec_files   total number of input spectra
# [in] calcspec         flag to run the spectra calculation
# [in] outspecfile      output spectra file or root
# [out] status          Output status
#
sub CalcSpectra {


  my $spec_merge = 0;
  my $gtiexpo = 0;
  my $sumexpo = 0;
  my $phasum = "phasum.tmp";
  my $phaexpr = "";

  my $status = 0;

  unless ($calcspec) { return 0; };

  ahlog::ah_out "Calculating Spectra";

  if($merge && $num_spec_files>1) { $spec_merge = 1; }

  # Open the mathpha expression file
  ahapp::add_temp_file($phasum);

  for (my $ii = 0; $ii < $num_evt_files; $ii++) {

    my $evt_file = $evt_files[$ii];
    my $spec_file = $in_spec_files[$ii];
    my $spec_ext = $spec_ext[$ii];
    my $gti_file = $in_gti_files[$ii];
    my $gti_ext  = $gti_ext[$ii];
    my $TmpOutfile = $out_spec_files[$ii];

    my $livetimefraction = 0;

    # File is temporary if we are merging
    ahapp::add_temp_file($TmpOutfile) if $spec_merge;

    # copy file over to output file
    $status = ahgen::run_ftool("ftcopy", $spec_file, $TmpOutfile);
    if($status) {
      ahlog::ah_err "Could not copy file $spec_file to $TmpOutfile.";
      return $status;
    }

    ($gtiexpo) = ahfilterlib::get_gti_sum($gti_file,$gti_ext);
    if(ahgen::get_error_flag) {
      ahlog::ah_err "Error getting GTI exposure in file $gtifile.";
      return ahgen::get_error_flag;
    }

    # get total events in input pseudo file
    my $totevt = ahgen::get_keyword($evt_file,"EVENTS","NAXIS2");
    if(ahgen::get_error_flag) {
      ahlog::ah_err "In file " . $evt_file . ":\n";
      ahlog::ah_err "Keyword NAXIS2 not found.\n";
      return ahgen::get_error_flag;
    }

    # get keyword PSEUDOHZ pseudo file
    my $pseudohz = ahgen::get_keyword($TmpOutfile,$spec_ext,"PSEUDOHZ");
    if(ahgen::get_error_flag) {
      ahlog::ah_err "In file " . $TmpOutfile . ":\n";
      ahlog::ah_err "Keyword PSEUDOHZ not found.\n";
      return ahgen::get_error_flag;
    }

    # Calculate the livetime fraction
    $livetimefraction = $totevt/($pseudohz*$gtiexpo);

    # Calculate the timing keywords
    # EXPOSURE may have additional calculations, but at this time LIVETIME and
    # EXPOSURE are the same
    my $ontime = $gtiexpo;
    my $livetime = $ontime*$livetimefraction;
    my $exposure = $livetime;
    my $deadc = $livetimefraction;
    my $deadapp = "T";

    # Write deadtime corrected ONTIME, LIVETIME, EXPOSURE, DEADC and DEADAPP keywords
    $status = ahgen::set_keyword($TmpOutfile,$spec_ext,"ONTIME",$gtiexpo,"On-source time");
    if($status) {
      ahlog::ah_err "Could not update keyword ONTIME in $TmpOutfile.";
      return $status;
    }
    $status = ahgen::set_keyword($TmpOutfile,$spec_ext,"LIVETIME",$livetime,"On-source time");
    if($status) {
      ahlog::ah_err "Could not update keyword LIVETIME in $TmpOutfile.";
      return $status;
    }
    $status = ahgen::set_keyword($TmpOutfile,$spec_ext,"EXPOSURE",$exposure,"Exposure time");
    if($status) {
      ahlog::ah_err "Could not update keyword EXPOSURE in $TmpOutfile.";
      return $status;
    }
    $status = ahgen::set_keyword($TmpOutfile,$spec_ext,"DEADC",$deadc,"Deadtime correction");
    if($status) {
      ahlog::ah_err "Could not update keyword DEADC in $TmpOutfile.";
      return $status;
    }
    $status = ahgen::set_keyword($TmpOutfile,$spec_ext,"DEADAPP",$deadapp,"Deadtime applied");
    if($status) {
      ahlog::ah_err "Could not update keyword DEADAPP in $TmpOutfile.";
      return $status;
    }
    if ( ahgen::check_hdu_exists($TmpOutfile,"WMAP") ) {
      $status = ahgen::set_keyword($TmpOutfile,"WMAP","ONTIME",$gtiexpo,"On-source time");
      if($status) {
        ahlog::ah_err "Could not update keyword ONTIME in $TmpOutfile\[WMAP].";
        return $status;
      }
      $status = ahgen::set_keyword($TmpOutfile,"WMAP","LIVETIME",$livetime,"On-source time");
      if($status) {
        ahlog::ah_err "Could not update keyword LIVETIME in $TmpOutfile\[WMAP].";
        return $status;
      }
      $status = ahgen::set_keyword($TmpOutfile,"WMAP","EXPOSURE",$exposure,"Exposure time");
      if($status) {
        ahlog::ah_err "Could not update keyword EXPOSURE in $TmpOutfile\[WMAP].";
        return $status;
      }
      $status = ahgen::set_keyword($TmpOutfile,"WMAP","DEADC",$deadc,"Deadtime correction");
      if($status) {
        ahlog::ah_err "Could not update keyword DEADC in $TmpOutfile\[WMAP].";
        return $status;
      }
    }
    if ( ahgen::check_hdu_exists($TmpOutfile,"GTI") ) {
      $status = ahgen::set_keyword($TmpOutfile,"GTI","ONTIME",$gtiexpo,"On-source time");
      if($status) {
        ahlog::ah_err "Could not update keyword ONTIME in $TmpOutfile\[GTI].";
        return $status;
      }
      $status = ahgen::set_keyword($TmpOutfile,"GTI","LIVETIME",$livetime,"On-source time");
      if($status) {
        ahlog::ah_err "Could not update keyword LIVETIME in $TmpOutfile\[GTI].";
        return $status;
      }
      $status = ahgen::set_keyword($TmpOutfile,"GTI","EXPOSURE",$exposure,"Exposure time");
      if($status) {
        ahlog::ah_err "Could not update keyword EXPOSURE in $TmpOutfile\[GTI].";
        return $status;
      }
      $status = ahgen::set_keyword($TmpOutfile,"GTI","DEADC",$deadc,"Deadtime correction");
      if($status) {
        ahlog::ah_err "Could not update keyword DEADC in $TmpOutfile\[GTI].";
        return $status;
      }
    }

    # Calculate the new exposure
    $sumexpo += $exposure;

    # Add the file to the mathpha expression file
    $phaexpr .= "$TmpOutfile+\n";

  } # end loop through spectra files

  # Cleanup expression
  $phaexpr =~ s/^[+]//; # remove any leading plus signs
  $phaexpr =~ s/[+]$//; # remove any trailing plus signs
  $phaexpr =~ s/[+][+]+/[+]/; # remove any plus sign multiples

  # Merge the PHA with mathpha
  if($spec_merge) {

    ahlog::ah_out "Merging the spectra";

    # Write the expression to an ASCII file
    open PHASUM, ">", $phasum;
    print PHASUM $phaexpr;
    close PHASUM;

    # Create weighted rmf
    ahlog::ah_out "Adding rmfs:\n  " . join "\n  ", @in_rsp_files;
    # +++ 2015-10-29 AS: Output merged response file should be saved
    my $rmfexpr= "rmflist.tmp";
    ahapp::add_temp_file($rmfexpr);

    open ADDRMF, ">", $rmfexpr;
    for (my $ii=0; $ii<$num_rsp_files; $ii++ ) {
      print ADDRMF "$in_rsp_files[$ii] $rsp_weights[$ii]\n";
    }
    close ADDRMF;
    $status = ahgen::run_ftool("addrmf","list=\@".$rmfexpr,"rmffile=$outrmfile");
    if($status) {
      ahlog::ah_err "Error running addrmf";
      return $status;
    }

    # Create weight-averaged NXB spectrum
    $status =  ahgen::run_ftool("mathpha","\@$phasum","C",
                               $outspecfile,"CALC","%","ncomments=0",
                               "rmfile=$outrmfile"
                             );
    if($status) {
      ahlog::ah_err "Cannot create merged spectrum with mathpha";
      return $status;
    }

    # Edit RMF file keyword in output file
    $status = ahgen::set_keyword($outspecfile,"SPECTRUM","RMFFILE",$outrmfile);
    if($status) {
      ahlog::ah_err "Could not update RMFFILE keyword in $outspecfile";
      return $status;
    }

    @out_spec_files = ();
    push @out_spec_files, $outspecfile;
  } # end merge spectra

  return 0;

}

#
# Description:
#
# Finalize the output files
#
# Parameters:
# [in] out_lc_files     array of output lightcurves
# [in] out_spec_files   array of output spectra
# [out] status          Output status
#
sub Finalize {

  ahlog::ah_out "Finalizing";

  foreach my $outfile (@out_lc_files, @out_spec_files) {
    ahapp::write_parameters ($outfile , 1) ;
    unless (ahgen::check_fits_file ($outfile)) {
      ahgen::ah_err "FITS file: $outfile. failed FITS verification test." ;
      return ahgen::get_error_flag;
    }
  }
  ahlog::ah_out "Successfully created FITS file:";
  ahlog::ah_out "  $_" foreach @out_lc_files, @out_spec_files;    

  return 0;

}

# $Log: hxisgddtime.pl,v $
# Revision 1.21  2016/06/10 20:02:54  asargent
# Removed duplicate time column in output lightcurve and fixed a lightcurve clobber issue
#
# Revision 1.20  2016/06/10 19:50:55  asargent
# Removed duplicate time column in output lightcurve and fixed a lightcurve clobber issue
#
# Revision 1.19  2016/06/10 19:05:40  asargent
# Fixed bug where hxisgddtime was not checking for duplicate HXI instruments
#
# Revision 1.18  2016/06/02 19:55:00  mdutka
# fixing bug which caused the orignal unmodified infile to be used when generating output
#
# Revision 1.17  2016/03/24 21:33:06  asargent
# Fixed livetime keyword writing, check if WMAP and GTI extensions exist in spectrum file.
#
# Revision 1.16  2016/03/24 21:23:59  asargent
# Updated keyword writing to pha file: write ONTIME, LIVETIME, EXPOSURE, DEADC to all extensions
#
# Revision 1.15  2016/03/18 18:41:50  asargent
# Fixed error message and status setting when reading/writing keywords
#
# Revision 1.14  2015/11/03 17:10:11  asargent
# Added response file parameter and merging when merging spectra. Added search for GTI and SPECTRUM HDUCLAS2 extensions. Updated output file naming.
#
# Revision 1.13  2015/10/28 21:37:17  asargent
# Updated algorithm and bug fixes: added lost TIME column to lightcurve; new parameters for response files and addrmf; create temp files in working directory; Check TIMEDEL keyword
#
# Revision 1.12  2015/08/19 21:44:16  asargent
# Replace the USER keyword from extractor with name of tool to avoid unit test failures
#
# Revision 1.11  2015/08/12 16:46:32  asargent
# Fixed bug with output file names
#
# Revision 1.10  2015/08/11 15:05:05  asargent
# Thorough instrument checking, verify first bin for lightcurve and calculate timing keywords for outspectra columns
#
# Revision 1.9  2015/08/06 04:38:40  asargent
# Removed extraneous comma
#
# Revision 1.8  2015/08/05 23:26:11  asargent
# Removed functions that are now in ahgen
#
# Revision 1.7  2015/07/30 16:55:44  asargent
# General cleanup of hxisgddtime script
#
