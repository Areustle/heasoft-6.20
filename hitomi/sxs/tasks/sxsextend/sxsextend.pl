#!/usr/bin/perl
#
# File name: sxsextend.pl
# Author: M. Witthoeft (ADNET), M. Dutka (KBRWyle)
# $Date: 2016/12/22 14:46:48 $
# Version: 0
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#Tool Dependencies:
#   Library Dependencies:
#   gen/lib/perl/ahlog
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
#
# Modification History:
#

#########################
#  Pragmas
#########################
use strict;
use warnings;

#########################
#  Packages
#########################
use ahgen ;
use ahlog ;
use ahapp ;

use File::Copy;

# turn on AUTOFLUSH
$| = 1;

#########################
#  Input Parameters     #
#########################

# Non-standard APE parameters.
my $inuffile = "";   # Input SXS uf event file
my $outuffile = "";  # Output SXS uf event file 
my $outclfile = "";  # Output SXS cl event file 
my $driftfile = "";  # Input drift correction file
my $gtigenfile = ""; # Input general GTI file (or NONE)
my $gtitelfile = ""; # Input telemetry GTI file (or NONE)
my $gtimxsfile = ""; # Input SXS MXS GTI file (or NONE) 
my $gtiadroff = "";  # Input ADR OFF GTI location  
my $gtimkf = "";     # Input MKF GTI location     
my $gtiehk = "";     # Input EHK GTI location    
my $gtiextra = "";   # Additional GTI used for screening (or NONE)   

# Extended energy parameters
my $eminin = 0.0;    # Minimum input bin energy [eV]   
my $dein = 0.0;      # Input bin size per energy interval [eV]
my $nchanin = 0;     # Number of channels

# CALDB/REFDATA
my $gainfile = "";   # Input gain file (or CALDB)
my $scalefile = "";  # Input EPI scale file for cal-pix (or CALDB) 
my $dgfile = "";     # Input gain coefficients file (if Perseus) 
my $offsetfile = ""; # Input calibration offset file (if Perseus)
my $selectfile = ""; # Input expression file (or NONE, CALDB) 
my $leapsecfile = "";# Input leap second file (or CALDB, REFDATA)

# sxspha2pi parameters
my $secphacol = "";  # Input PHA column to use for secondary correction   
my $method = "";     # Correction method (FIT or AVERAGE) 
my $scaleepi = "";   # Scale EPI values using scalefile (yes/[no])
my $scalegrade = ""; # List of grades to apply scale factors
my $itypecol = "";   # Column containing event grade 
my $gapdt = 0.0;     # Time [s] between events to define a gap (or <0)  
my $ntemp = 0;       # Number of temperatures from gain file to use in interpolation
my $writetemp = "";  # Output temperature used for each event (yes/[no]) 
my $extrap = "";     # Allow extrapolation when determining drift temperature (yes/[no]) 
my $randomize = "";  # Allow randomization in PI to UPI conversion ([yes]/no)
my $seed = 0;        # Random number generator seed (0=use system time)

# sxsperseus parameters
my $outrange = "";   # How events are handled outside time range  

# screening parameters
my $label = "";      # Screening expression label in labelfile
my $expr = "";       # Additional expression for selection (or NONE) 

# standard parameters
my $clobber = "no";
my $debug = "no";
my $history = "no";
my $buffer = 0;

#########################
#  Other Variables 
#########################

my @gtiextra_files = ();
my $tstart_inuffile = 0.0;

# GTI EXTENSION FLAGS
my $gtipoint_flag = "";
my $gtiatt_flag = "";
my $gtitel_flag = "";
my $gtimxsfnoff13_flag = "";
my $gtimxsfnoff24_flag = "";
my $gtimkf_flag = "";
my $gtiehk_flag = "";
my $gtiadroff_flag = "";
my $gti_inuffile_flag = "";

my $binwidth = 0;
my $offset = 0;
my $tlmax = 0;

# run Perseus yes/no
my $runperseus = 0;

#########################
#  Main Code Block 
#########################

ahapp::startup ();

ahapp::begin_processing ();

# Error status
my $error_status = 0;

$error_status = get_parameters () ;
unless ( $error_status == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($error_status);
}

$error_status = initialize () ;
unless ( $error_status == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($error_status);
}

$error_status = do_work () ;
unless ( $error_status == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($error_status);
}

$error_status = finalize () ;
unless ( $error_status == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($error_status);
}

# We're done.
ahapp::end_processing($error_status);

#########################
# Subroutines
#########################

sub get_parameters {
  # Query non-standard APE parameters
  $inuffile = ahapp::query_parameter("inuffile");  
  $outuffile = ahapp::query_parameter("outuffile");  
  $outclfile = ahapp::query_parameter("outclfile");   
  $driftfile = ahapp::query_parameter("driftfile");   
  $gtigenfile = ahapp::query_parameter("gtigenfile"); 
  $gtitelfile = ahapp::query_parameter("gtitelfile"); 
  $gtimxsfile = ahapp::query_parameter("gtimxsfile"); 
  $gtiadroff = ahapp::query_parameter("gtiadroff");  
  $gtimkf = ahapp::query_parameter("gtimkf");     
  $gtiehk = ahapp::query_parameter("gtiehk");     
  $gtiextra = ahapp::query_parameter("gtiextra");   

  # Extended energy parameters
  $eminin = ahapp::query_parameter("eminin");     
  $dein = ahapp::query_parameter("dein");       
  $nchanin = ahapp::query_parameter("nchanin");    

  # CALDB/REFDATA
  $gainfile = ahapp::query_parameter("gainfile");   
  $scalefile = ahapp::query_parameter("scalefile");  
  $dgfile = ahapp::query_parameter("dgfile");     
  $offsetfile = ahapp::query_parameter("offsetfile"); 
  $selectfile = ahapp::query_parameter("selectfile"); 
  $leapsecfile = ahapp::query_parameter("leapsecfile"); 

  # sxspha2pi parameters
  $secphacol = ahapp::query_parameter("secphacol");   
  $method = ahapp::query_parameter("method");      
  $scaleepi = ahapp::query_parameter("scaleepi");    
  $scalegrade = ahapp::query_parameter("scalegrade");  
  $itypecol = ahapp::query_parameter("itypecol");    
  $gapdt = ahapp::query_parameter("gapdt");       
  $ntemp = ahapp::query_parameter("ntemp");       
  $writetemp = ahapp::query_parameter("writetemp");   
  $extrap = ahapp::query_parameter("extrap");     
  $randomize = ahapp::query_parameter("randomize");  
  $seed = ahapp::query_parameter("seed");       

  # sxsperseus parameters
  $outrange = ahapp::query_parameter("outrange");   

  # screening parameters
  $label = ahapp::query_parameter("label");      
  $expr = ahapp::query_parameter("expr");       

  # standard parameters
  $buffer = ahapp::query_parameter("buffer"); 
  if ($ahapp::clobber) { $clobber = "yes"; }
  if ($ahapp::debug) { $debug = "yes"; }
  if ($ahapp::history) { $history = "yes"; } 

  return 0;

}


sub initialize {
  
  my $status = 0;
  my @gtimkf_parsed = ();
  my @gtiehk_parsed = ();
  my @gtiadroff_parsed = (); 

  # Check on required files exit with error if any required file in not found 
  if (ahgen::isRequiredFileNotFound($inuffile)) { return 1; }
  if (ahgen::isRequiredFileNotFound($driftfile)) { return 1; }

  if (ahgen::removeOutputFileIfClobbering($outuffile,$ahapp::clobber)) { return 1; }
  if (ahgen::removeOutputFileIfClobbering($outclfile,$ahapp::clobber)) { return 1; }
 
  if (uc $gtigenfile ne "NONE") {
    if (ahgen::isRequiredFileNotFound($gtigenfile)) { return 1; }
  }

  if (uc $gtitelfile ne "NONE") {
    if (ahgen::isRequiredFileNotFound($gtitelfile)) { return 1; }  
  }

  if (uc $gtimxsfile ne "NONE") {
    if (ahgen::isRequiredFileNotFound($gtimxsfile)) { return 1; }    
  }

  if ((uc $gtiadroff ne "NONE") and (uc $gtiadroff ne "DEFAULT")) {
    if (ahgen::isRequiredFileNotFound($gtiadroff)) { return 1; } 
    @gtiadroff_parsed = ahgen::parse_file_name($gtiadroff);
  }     

  if ((uc $gtimkf ne "NONE") and (uc $gtimkf ne "DEFAULT")) {
    if (ahgen::isRequiredFileNotFound($gtimkf)) { return 1; } 
    @gtimkf_parsed = ahgen::parse_file_name($gtimkf);
  }  

  if ((uc $gtiehk ne "NONE") and (uc $gtiehk ne "DEFAULT")) {
    if (ahgen::isRequiredFileNotFound($gtiehk)) { return 1; } 
    @gtiehk_parsed = ahgen::parse_file_name($gtiehk);
  }      


  # Read gtiextra file list unless it is none.  Make sure each file in the  
  # list exists
  if (uc $gtiextra ne "NONE") { 
    @gtiextra_files = ahgen::get_file_list($gtiextra);
    my @gtiextra_parsed_tmp = ();
    for (my $ii=0; $ii<scalar(@gtiextra_files); ++$ii) {
      @gtiextra_parsed_tmp = ahgen::parse_file_name($gtiextra_files[$ii]);
      if (ahgen::isRequiredFileNotFound($gtiextra_parsed_tmp[0])) {
        return 1;
      } 
    }
  }

  # Read TSTART from inuffile
  $tstart_inuffile = ahgen::get_keyword($inuffile,"EVENTS","TSTART");
  
  # Check for expected GTI extensions.  If any do not exist, write a
  # message to the log file and skip that GTI.  Create set of flags for
  # marking the presence of each GTI.
  if (uc $gtigenfile ne "NONE") {
    if (ahgen::check_hdu_exists($gtigenfile,"GTIPOINT")) { 
      $gtipoint_flag = $gtigenfile ."\[GTIPOINT\]"; 
    } else {
      ahlog::ah_info "HIGH", "File extension $gtigenfile\[GTIPOINT\] is not found; skipping\n";
    } 
    if (ahgen::check_hdu_exists($gtigenfile,"GTIATT")) { 
      $gtiatt_flag = $gtigenfile . "\[GTIATT\]"; 
    } else {
      ahlog::ah_info "HIGH", "File extension $gtigenfile\[GTIATT\] is not found; skipping\n";
    }
  }
  if (uc $gtitelfile ne "NONE") {
    if (ahgen::check_hdu_exists($gtitelfile,"GTITEL")) { 
      $gtitel_flag = $gtitelfile . "\[GTITEL\]"; 
    } else {
      ahlog::ah_info "HIGH", "File extension $gtitelfile\[GTITEL\] is not found; skipping\n";
    }
  }
  if (uc $gtimxsfile ne "NONE") {
    if (ahgen::check_hdu_exists($gtimxsfile,"GTIMXSFNOFF13")) { 
      $gtimxsfnoff13_flag = $gtimxsfile . "\[GTIMXSFNOFF13\]"; 
    } else {
      ahlog::ah_info "HIGH", "File extension $gtimxsfile\[GTIMXSFNOFF13\] is not found; skipping\n";
    }
    if (ahgen::check_hdu_exists($gtimxsfile,"GTIMXSFNOFF24")) { 
     $gtimxsfnoff24_flag = $gtimxsfile . "\[GTIMXSFNOFF24\]"; 
    } else { 
      ahlog::ah_info "HIGH", "File extension $gtimxsfile\[GTIMXSFNOFF24\] is not found; skipping\n";
    }
  } 

  if (uc $gtimkf ne "NONE") {
    my $filename="";
    my $hdu="";
    if (uc $gtimkf eq "DEFAULT") {
       $filename = $inuffile;
       $hdu = "GTIMKF";
    } else {
      if (!$gtimkf_parsed[1]) {
        $filename = $gtimkf;
        $hdu = "GTI";
      } else {
        $filename = $gtimkf_parsed[0];
        $hdu = $gtimkf_parsed[1];
      }    
    }
    if (ahgen::check_hdu_exists($filename,$hdu)) { 
      $gtimkf_flag = $filename . "\[" . $hdu . "\]";
    } else {
      ahlog::ah_info "HIGH", "File extension $filename\[$hdu\] is not found; skipping\n";
    } 
  }

  if (uc $gtiehk ne "NONE") {
    my $filename="";
    my $hdu="";
    if (uc $gtiehk eq "DEFAULT") {
       $filename = $inuffile;
       $hdu = "GTIEHK";
    } else {
      if (!$gtiehk_parsed[1]) {
        $filename = $gtiehk;
        $hdu = "GTI";
      } else {
        $filename = $gtiehk_parsed[0];
        $hdu = $gtiehk_parsed[1];
      }    
    }
    if (ahgen::check_hdu_exists($filename,$hdu)) { 
      $gtiehk_flag = $filename . "\[" . $hdu . "\]";
    } else {
      ahlog::ah_info "HIGH", "File extension $filename\[$hdu\] is not found; skipping\n";
    } 
  }


  if (uc $gtiadroff ne "NONE") {
    my $filename="";
    my $hdu="";
    if (uc $gtiadroff eq "DEFAULT") {
       $filename = $inuffile;
       $hdu = "GTIADROFF";
    } else {
      if (!$gtiadroff_parsed[1]) {
        $filename = $gtiadroff;
        $hdu = "GTI";
      } else {
        $filename = $gtiadroff_parsed[0];
        $hdu = $gtiadroff_parsed[1];
      }    
    }
    if (ahgen::check_hdu_exists($filename,$hdu)) { 
      $gtiadroff_flag = $filename . "\[" . $hdu . "\]";
    } else {
      ahlog::ah_info "HIGH", "File extension $filename\[$hdu\] is not found; skipping\n";
    } 
  }

  if (ahgen::check_hdu_exists($inuffile,"GTI")) { 
    $gti_inuffile_flag = $inuffile . "\[GTI\]";
  } else {
    ahlog::ah_info "HIGH", "File extension $inuffile\[GTI\] is not found; skipping\n";
  }

  # Convert eminin/dein/nchanin parameters to binwidth/offset/tlmax
  $binwidth = $dein;
  $offset = $eminin + $dein;
  $tlmax = $nchanin - 1;
 
  if ($nchanin > 32768) {
    ahlog::ah_info "HIGH", "Warning: nchnanin > 32768 \n";
  }
  
  my $PERSEUS_START = 67164301.0;
  my $PERSEUS_STOP =  68517661.0;


  if (($tstart_inuffile > $PERSEUS_START) && ($tstart_inuffile < $PERSEUS_STOP)) {
    $runperseus = 1;
  }
  
  return 0;
}

sub do_work { 

  my $status=0; 

  # Run sxspha2pi
  $status = ahgen::run_ftool("sxspha2pi",
                             "infile=$inuffile",
                             "outfile=$outuffile",
                             "calcupi=no",
                             "calcpi=yes",
                             "driftfile=$driftfile",
                             "gainfile=$gainfile",
                             "scalefile=$scalefile",
                             "tempidx=-1",
                             "pxphaoffset=0",
                             "secphacol=$secphacol",
                             "addepicol=EPI2",
                             "method=$method",
                             "scaleepi=$scaleepi",
                             "scalegrade=$scalegrade",
                             "itypecol=$itypecol",
                             "extended=yes",
                             "binwidth=$binwidth",
                             "offset=$offset",
                             "tlmax=$tlmax",
                             "gapdt=$gapdt",
                             "ntemp=$ntemp",
                             "writetemp=$writetemp",
                             "extrap=$extrap",
                             "randomize=$randomize",
                             "seed=$seed",
                             "buffer=$buffer",
                             "clobber=$clobber",
                             "chatter=$ahapp::chatter",
                             "logfile=$ahapp::logfile",
                             "debug=$debug",
                             "history=$history",
                             "mode=$ahapp::mode");
  if ($status) { 
    ahlog::ah_err "Error running sxspha2pi.";
    return 1;
  }

  # If running on perseus run sxsperseus
  if ($runperseus) { 
    $status = ahgen::run_ftool("sxsperseus",
                               "infile=$outuffile",
                               "outfile=sxsperseus.out",
                               "driftfile=$driftfile",
                               "dgfile=$dgfile",
                               "offsetfile=$offsetfile",
                               "outrange=$outrange",
                               "method=$method",
                               "extended=yes",
                               "binwidth=$binwidth",  
                               "offset=$offset",    
                               "tlmax=$tlmax",     
                               "buffer=$buffer",
                               "clobber=yes",
                               "chatter=$ahapp::chatter",
                               "logfile=$ahapp::logfile",
                               "debug=$debug",
                               "history=$history",
                               "mode=$ahapp::mode");
    if ($status) {
      ahlog::ah_err "Error running sxsperseus.";
      return 1;
    }
    # Set outfile to the output of sxsperseus
    unlink $outuffile;
    rename("sxsperseus.out",$outuffile);
  }
  
  # Get list of GTI to merge.  Based on GTI flags create
  # a comma-delimited string of filename[extension].
  my $gtilist = "";
  if ($gtipoint_flag) {
    $gtilist = $gtilist . $gtipoint_flag . ",";
  }
  if ($gtiatt_flag) {
    $gtilist = $gtilist . $gtiatt_flag . ",";
  } 
  if ($gtitel_flag) { 
    $gtilist = $gtilist . $gtitel_flag . ",";
  }   
  if ($gtimxsfnoff13_flag) {
    $gtilist = $gtilist . $gtimxsfnoff13_flag . ",";
  }  
  if ($gtimxsfnoff24_flag) {
    $gtilist = $gtilist . $gtimxsfnoff24_flag . ",";
  }
  if ($gtimkf_flag) { 
    $gtilist = $gtilist . $gtimkf_flag . ",";
  }
  if ($gtiehk_flag) {
    $gtilist = $gtilist . $gtiehk_flag . ","; 
  }
  if ($gtiadroff_flag) {
    $gtilist = $gtilist . $gtiadroff_flag . ",";
  }
  if ($gti_inuffile_flag) {
    $gtilist = $gtilist . $gti_inuffile_flag . ",";
  }
  
  # add gti extra to gtilist
  for (my $ii=0; $ii < scalar(@gtiextra_files); ++$ii) {
    $gtilist = $gtilist . $gtiextra_files[$ii] . ",";
  } 

  # remove trailing comma if gtilist is not ""
  if ($gtilist) {
    # If gtilist is not "" last character will always be a comma
    $gtilist = substr($gtilist,0,-1);
  }

  # Run Ahscreen
  ahgen::run_ftool("ahscreen",
                   "infile=$outuffile",
                   "outfile=$outclfile",
                   "gtifile=$gtilist",
                   "expr=$expr",
                   "selectfile=$selectfile",
                   "label=$label",
                   "mergegti=AND",
                   "cpkeyword=yes",
                   "upkeyword=NO",
                   "leapsecfile=$leapsecfile",
                   "outexpr=NONE",
                   "cleanup=yes",
                   "clobber=$clobber",
                   "chatter=$ahapp::chatter",
                   "logfile=$ahapp::logfile",
                   "debug=$debug",
                   "history=$history",
                   "mode=$ahapp::mode");         

  # Set TLMIN for PIE Column to 0
  my $PIE_col_num = ahgen::get_column_num($outclfile,"EVENTS","PIE");
  my $TLMIN_keyname_PIE = "TLMIN" . $PIE_col_num;
  ahgen::run_ftool("fthedit",
                   "infile=$outclfile\[1\]",
                   "keyword=$TLMIN_keyname_PIE",
                   "operation=add",
                   "value=0",
                   "comment=minimum legal value for PIE");

                  
  return 0;
}



  
sub finalize {
  return 0;
}
