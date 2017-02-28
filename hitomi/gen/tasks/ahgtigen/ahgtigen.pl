#!/usr/bin/perl
#
# File name: ahgtigen.pl
# Author: Mike Witthoeft NASA GSFC, A. J. Sargent NASA GSFC
# $Date: 2016/11/17 16:35:00 $
# Version: 0
#
# ahgtigen generates GTIs by using an optional user-input expression combined
# with a CALDB expression using maketime. The GTI are then merged with user-input
# GTI files using mgtime.
#
# Tool Dependencies:
#   maketime
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
use ahapp ;
use ahgen qw (:ALL);
use ahfilterlib ;

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

# Query input and output parameters
our $infile    = "";   # Input file (or file list)
our $gtifile    = "";   # Input file (or file list)
our $outfile    = "";   # Output GTI file
our $gtiexpr    = "";   # Expression to create GTI or label for existing expression
our $mergegti   = "";   # Merge mode OR or AND
our $cpkeyword  = "";   # List of keywords to copy from input files (ALL or NONE accepted)
our $upkeyword  = 1;    # Update header with keywords from input file ([yes]/no) 
our $instrume   = "";   # Instrument to search for CALDB file
our $selectfile = "";   # CALDB or user input HK label file with expresssions (or NONE)
our $label      = "";   # Label to read from CALDB HK label file
our $leapsecfile  = ""; # Leapsecfile for updating timing keywords

our %mgtime_pars;
our %maketime_pars;

# Other variables
our $calcgti=0;        # Perform GTI calculation?

our @in_mkf_files;     # array with input HK files
our $num_mkf_files;    # size of @in_mkf_files
our $fromfile="";

our @in_gti_files;     # array with input GTI files
our @gti_files;        # array with GTI files from maketime
our $num_gti_files;    # size of @gti_files

our $mtexpr = "";

our $ahgtigenerror = 0;

#########################
#  Main Code Block 
#########################

# Query canonical APE parameters and start logging. 
ahapp::startup () ;

# Pre-Processing
ahapp::begin_processing();

# Get the input parameters
$ahgtigenerror = GetInputParameters();
unless ( $ahgtigenerror == 0 ) {
  ahlog::ah_debug "GetInputParameters" ;
  ahapp::end_processing($ahgtigenerror);
}

# Check the input files for clobber
$ahgtigenerror = CheckInput();
unless ( $ahgtigenerror == 0 ) {
  ahlog::ah_debug "CheckInput" ;
  ahapp::end_processing($ahgtigenerror);
}

# Write all parameters to this script to the log file.
ah_info "LOW", ahapp::write_parameters () ;                 

# Main Processing 

# Calculate expression to filter events
$ahgtigenerror = BuildExpression();
unless ( $ahgtigenerror == 0 ) {
  ahlog::ah_debug "BuildExpression" ;
  ahapp::end_processing($ahgtigenerror);
}

# Calculate expression to filter events
$ahgtigenerror = CalcGTI();
unless ( $ahgtigenerror == 0 ) {
  ahlog::ah_debug "CalcGTI" ;
  ahapp::end_processing($ahgtigenerror);
}

$ahgtigenerror = MergeGTI();
unless ( $ahgtigenerror == 0 ) {
  ahlog::ah_debug "MergeGTI" ;
  ahapp::end_processing($ahgtigenerror);
}

# Write expression to file history and par file
$ahgtigenerror = UpdateHistoryKeywords();
unless ( $ahgtigenerror == 0 ) {
  ahlog::ah_debug "UpdateHistoryKeywords" ;
  ahapp::end_processing($ahgtigenerror);
}

# Verify file is okay
$ahgtigenerror = Finalize();
unless ( $ahgtigenerror == 0 ) {
  ahlog::ah_debug "Finalize" ;
  ahapp::end_processing($ahgtigenerror);
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
# [in/out] infile    Input MKF file
# [in/out] gtifile    Input GTI file
# [in/out] outfile    Output GTI file (or NONE)
# [in/out] gtiexpr    GTI expression or label (or NONE)
# [in/out] mergegti   Merge expression mode OR or AND (with label in CALDB)
# [in/out] cpkeyword  Keywords to copy from mkfile (or NONE, ALL)
# [in/out] upkeyword  Update with keywords from mkfile ([yes]/no)
# [in/out] instrume   Instrument name
# [in/out] selectfile Input expression file (or NONE, CALDB)
# [in/out] label      Screening expression label in labelfile
# [out] status        Output status
#
sub GetInputParameters {

  # get parameter values
  $infile    = ahapp::query_parameter("infile");    
  $gtifile    = ahapp::query_parameter("gtifile");    
  $outfile    = ahapp::query_parameter("outfile");    
  $gtiexpr    = ahapp::query_parameter("gtiexpr");    
  $mergegti   = ahapp::query_parameter("mergegti");

  $cpkeyword  = ahapp::query_parameter("cpkeyword");  
  $upkeyword  = ahapp::query_parameter("upkeyword",1);
  $leapsecfile  = ahapp::query_parameter("leapsecfile");
  $instrume   = uc ahapp::query_parameter("instrume");   
  $selectfile = ahapp::query_parameter("selectfile"); 
  $label      = uc ahapp::query_parameter("label");      

  my $outstart= ahapp::query_parameter("outstart");
  my $outstop = ahapp::query_parameter("outstop"); 
  my $time    = ahapp::query_parameter("time");    

  # query mgtime-specific parameters
  $mgtime_pars{time}       = $time;
  $mgtime_pars{instarts}   = ahapp::query_parameter("instarts");
  $mgtime_pars{instops}    = ahapp::query_parameter("instops"); 
  $mgtime_pars{outstart}   = $outstart;
  $mgtime_pars{outstop}    = $outstop;

  # query maketime-specific parameters
  $maketime_pars{time}      = $time;
  $maketime_pars{start}     = $outstart;
  $maketime_pars{stop}      = $outstop;
  $maketime_pars{name}      = "-";
  $maketime_pars{value}     = "-";
  $maketime_pars{prefr}     = ahapp::query_parameter("prefr");   
  $maketime_pars{postfr}    = ahapp::query_parameter("postfr");  
  $maketime_pars{compact}   = "no";

  return 0;

} # GetInputParameters

# 
# Description: 
#
# Check if the output file(s) already exist.  Unless clobber is set, this will
# cause the script to fail. Create output event file(s)
#
# Parameters:
# [in] infile        Input MKF file
# [in] in_mkf_files   Array of input mkf files
# [in] num_mkf_files  Total number of mkf files to be processed
# [in] gtifile        Output GTI file
# [in] in_gti_files   Array of input GTI files
# [in] num_gti_files  Total number of GTI files to be processed
# [in] calcgti        Run maketime (yes/no)
# [in] cpkeyword      Keywords to copy from mkfile (or NONE, ALL)
# [in] upkeyword      Update with keywords from mkfile ([yes]/no)
# [out] status        Output status
#
sub CheckInput { 

  my $status = 0;
  ahlog::ah_out "\nChecking Input\n";

  # Reset the output expression parameter
  ahgen::run_ftool("pset","ahgtigen","outexpr= ");

  # +++ 2015-06-23 AS: Allow comma delimited input file list?
  
  # get list of MKF files from infile parameter
  # Check if @ file lists exist
  if ($infile =~ /^@/) {
    if(isRequiredFileNotFound(substr($infile,1))) { return 1;} 
  }
  if ($gtifile =~ /^@/) {
    if(isRequiredFileNotFound(substr($gtifile,1))) { return 1;} 
  }
  if ($gtiexpr =~ /^@/) {
    if(isRequiredFileNotFound(substr($gtiexpr,1))) { return 1;} 
  }
  
  # Get the leapsec file in the refdata directory if leapsec eq REFDATA
  if ( ahgen::isFileRefData($leapsecfile) ) {
    $leapsecfile = $ENV{LHEA_DATA} . "/leapsec.fits";
  }

  # Read list of input files from each parameter 
  @in_mkf_files = readInputFileList($infile);
  @in_gti_files = readInputFileList($gtifile);

  # Check each input file exists
  if(isRequiredFileNotFound(@in_mkf_files)) { return 1;}
  if(isRequiredFileNotFound(@in_gti_files)) { return 1;}

  # set number of Input MKF/GTI files
  $num_mkf_files=@in_mkf_files;
  $num_gti_files=@in_gti_files;

  # Check for output file clobber
  if(removeOutputFileIfClobbering($outfile,$ahapp::clobber)) { return 1; }

  ahlog::ah_out "Processing $num_mkf_files Input MKF file(s):\n    ".join(",\n    ",@in_mkf_files);
  ahlog::ah_out "Processing $num_gti_files Input GTI file(s):\n    ".join(",\n    ",@in_gti_files);

  # check if there are enough input files for maketime operation
  if ($num_mkf_files) { $calcgti = 1; }

  # Use maketime to copy keywords
  if(lc $cpkeyword eq "all") { $maketime_pars{copykw}="yes"; } else { $maketime_pars{copykw}="no"; }

  # If we are not creating GTI, check if we are merging GTI files
  if ( !$calcgti && $num_gti_files <= 1) {
      ahlog::ah_err "Not enough GTI files for merging";
      return 1;
  }

  # make sure that maketime output file has correct column names
  # depending on whether GTI are merged
  if($calcgti && $num_mkf_files>1) {
    $maketime_pars{start}=$mgtime_pars{instarts};
    $maketime_pars{stop}=$mgtime_pars{instops};
  } else {
    $maketime_pars{start}=$mgtime_pars{outstart};
    $maketime_pars{stop}=$mgtime_pars{outstop};
  }
  
  if($num_mkf_files) {
    # +++ Read a keyword here?
    if($in_mkf_files[0] =~ /\.mkf/) {
      $fromfile = "MKF";
    } elsif ( $in_mkf_files[0] =~ /\.ehk/) {
      $fromfile = "EHK";
    } else {
      $fromfile = "UNKNOWN";
      $label = "none";
    }
    foreach my $file (@in_mkf_files) {
      if ( $fromfile eq "UNKNOWN" ) { last; }
      if($file !~ /\.$fromfile/i) {
        ahlog::ah_err "All input parameter files must be of the same type (.mkf or .ehk)";
        return 1;
      }
    }
  } 

  # calc_timing_keys requires START/STOP as column names
  if(lc $mgtime_pars{instarts} ne "start" or lc $mgtime_pars{instops} ne "stop") { 
    ahlog::ah_info "HIGH", "*** Non-standard start/stop column names";
    ahlog::ah_info "HIGH", "*** Not updating output timing keywords";
    $upkeyword=0; 
  }
  if(lc $mgtime_pars{outstart} ne "start" or lc $mgtime_pars{outstop} ne "stop") { 
    ahlog::ah_info "HIGH", "*** Non-standard start/stop column names";
    ahlog::ah_info "HIGH", "*** Not updating output timing keywords";
    $upkeyword=0; 
  }

  return $status;

} # CheckInput

############################
#      Main Processing 
############################

# 
# Description: 
#
# Build expression based on CALDB settings and user-input expression
#
# Parameters:
# [in] gtiexpr        Additional expression for selection (or NONE)
# [in] instrume       Instrument name
# [in] selectfile     Input expression file (or NONE, CALDB)
# [in] label          Screening expression label in labelfile
# [in] in_mkf_files   Array of event files being processed
# [in] calcgti        Run maketime (yes/no)
# [in/out] mtexpr     Combined expression to be used for screening
# [out] status        Output status
#
sub BuildExpression {
  
  my $calexpr="none";
  my $read_caldb=0;

  my $status = 0;

  unless ($calcgti) { return $status; }

  $read_caldb = determineIfCALDBExpression();

  if(lc $gtiexpr eq "none" and $read_caldb == 0) { $calcgti = 0; return $status; } # nothing to do

  ahlog::ah_out "\nBuilding Expression\n";
  
  # Check if it's possible to check CALDB (or user input selectfile)
  if($read_caldb) {

    my $fromext="";

    if ($fromfile eq "MKF") { 
      $fromext = "FILTER"; 
    } elsif ( $fromfile eq "EHK" ) { 
      $fromext = "EHK"; 
    } else {
      $fromext = "UNKNOWN";
    }

    # Get TSTART and INSTRUME from first event file
    # +++ 2015-06-23 AS: What to do with multiple input files, but different
    # extension names? e.g. FILTER, HK, etc. Just use first extension?
    my $dateobs;
    if(lc $selectfile eq "caldb") {
      $dateobs = ahgen::get_keyword($in_mkf_files[0],$fromext,"DATE-OBS");
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "In file $in_mkf_files[0], DATE-OBS keyword not defined.\n";
        return 1;
      }
    }

    # Determine if reading from CALDB or user-input labelfile
    my $labelfile = ahfilterlib::call_quzcif($selectfile,"GEN","-","SELECT",$dateobs,"-");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "Error querying CALDB for labelfile";
      return 1;
    }

    $calexpr = ahfilterlib::get_caldb_expr($labelfile,$instrume,$fromfile,$label);
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "Error getting expression from $labelfile.";
      return 1;
    }
  }

  # Set up the maketime expression
  if(lc $gtiexpr ne "none") { 
    if( $gtiexpr =~ /^@/ ) {
      open GTIEXPR, "<", substr($gtiexpr,1) ;
      while ( <GTIEXPR> ) { $mtexpr .= $_; chop $mtexpr; }
    } else {
      $mtexpr = $gtiexpr; 
    }
  }
  if(lc $calexpr ne "none") { $mtexpr .= "&&" . $calexpr; }
  #if(lc $calexpr ne "none" and lc $gtiexpr ne "none" ) { $mtexpr = "$gtiexpr&&$calexpr"; }

  # Clean up expressions, remove extraneous &&s
  $mtexpr =~ s/^(\&\&|\|\|)//;
  $mtexpr =~ s/(\&\&|\|\|)$//;

  return $status;

} # BuildExpression

# 
# Description: 
#
# Create GTI using expression from BuildExpression in ftool maketime
#
# Parameters:
# [in] calcgti        Run maketime (yes/no)
# [in] in_mkf_files   Array of input mkf files
# [in] outfile        Output GTI file
# [in] mtexpr         Expression used to calculate GTI
# [in/out] gti_files  Array of GTI files from maketime
# [in/out] num_gti_files Total number of GTI files from maketime
# [out] status        Output status
#
sub CalcGTI {
  
  my $ii = 0;
  my $status = 0;

  unless ($calcgti) { return $status; }
  ahlog::ah_out "\nCalculating GTI\n";

  ahlog::ah_info "LOW", "Using maketime expression: $mtexpr";

  # Maketime has trouble with direct input of complicated
  # expressions.
  # Print the expression to a text file
  my $exprfile = "expr.txt";
  ahapp::add_temp_file($exprfile);
  open EXPR, ">", $exprfile;
  print EXPR $mtexpr . "\n";
  close EXPR;

  # Loop through all input MKF files, calculate GTI
  foreach my $inmkf (@in_mkf_files) {

    my $outgti = $outfile . "_$ii.gti";
    if(run_maketime($inmkf,$outgti,"@" . $exprfile,\%maketime_pars)) { return 1; }

    # Change the name of the header
    ahgen::set_keyword($outgti,"STDGTI","EXTNAME","GTI");

    # Add maketime output to list of GTI files
    push @gti_files, $outgti;
    $ii++;

  } # End loop on MKF files

  $num_gti_files += @gti_files;

  # If we are only creating one GTI file and not merging
  # rename output from maketime to outgti parameter
  if ($num_gti_files==1) { 
    my $gti = shift @gti_files;
    rename $gti => $outfile;
    push @gti_files, $outfile;
    return $status; 
  }

  return $status;

} # CalcGTI

# 
# Description: 
#
# Merge GTI files based on input parameter and output
# from maketime. 
#
# Parameters:
# [in] mergegti       Merge GTI mode ( OR, [AND] )
# [in] calcgti        Run maketime (yes/no)
# [in] num_gti_files  Total number of GTI files to be merged 
# [in] in_gti_files   Array of input GTI files
# [in] outfile        Output GTI file
# [in] gti_files      Array of GTI files from maketime
# [in] upkeyword      Update with keywords from mkfile ([yes],no)
# [out] status        Output status
#
sub MergeGTI {

  my $status = 0;

  if (lc $mergegti eq "none" and $calcgti==0) { 
    ahlog::ah_err "No gti files to create or merge.";
    return 1; 
  }

  if ($num_gti_files<2) { return $status; }
  if (lc $mergegti eq "none") { return $status; }
  ahlog::ah_out "\nMerging $num_gti_files GTI files\n";

  # Add any newly created GTI files from step 1 to list of files to delete
  ahapp::add_temp_file ($_) foreach @gti_files;

  # Merge GTI files. The merge_gti subroutine returns the number of GTI files merged.
  if( ahfilterlib::merge_gti([@in_gti_files,@gti_files], $outfile , $mergegti,"GTI", \%mgtime_pars) ) {
    ahlog::ah_err "Error merging GTI files" ;
    return 1;
  } 
  my $nrows = ahgen::get_keyword($outfile,"GTI","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $outfile, NAXIS2 keyword not defined.\n";
    return 1;
  }
  if ( $nrows == 0 ) {
    ahlog::ah_info "HIGH", "No exposure found after merging GTI.\n";
    return 2;
  }

  # Update GTI output file list
  @gti_files = ();
  push @gti_files, $outfile;
  $num_gti_files = 1;
  
  return $status;

} # MergeGTI

# 
# Description: 
#
# Write the expression used as history keywords in output gti file(s)

# and parameter file
#
# Parameters:
# [in] mtexpr      Combined expression to be used for screening
# [in] gti_files   Array of event files being processed
# [out] status     Output status
#
sub UpdateHistoryKeywords {

  my $status = 0;

  if($mtexpr eq "") { 
    ahgen::run_ftool("pset","ahgtigen","outexpr=NONE");
    return 0; 
  }
  ahlog::ah_out "\nUpdating History\n";

  # Split expression into 72 character elements
  # to write into FITS comments
  my @commentexpr=unpack("(a72)*",$mtexpr);

  # Set final expression used to create GTI to output parameter finalexpr.
  # Need to set it outside of calcgti in case no GTI calculation was done.
  ahgen::run_ftool("pset","ahgtigen","outexpr=$mtexpr");

  # Loop over each output file and each segment of expression
  foreach my $outgti (@gti_files) {
    $status = ahgen::set_keyword($outgti,"GTI","HISTORY","Expression for ahgtigen cleaning:");
    if ($status) {
      ahgen::ah_err "Error setting HISTORY keywords to $outgti.";
      return $status;
    }
    foreach my $comment (@commentexpr) {
      chomp $comment; # remove any newline characters

      # set keyword HISTORY to (part of) expression
      $status = ahgen::set_keyword($outgti,"GTI","HISTORY"," $comment");
      if ($status) {
        ahgen::ah_err "Error setting HISTORY keywords to $outgti.";
        return $status;
      }
    }
  }

  return 0;

} # UpdateKeywords

# 
# Description: 
#
# Copy specified keywords from first input mkf or gti file and write ahgtigen
# parameters in output file. Run ftverify on output file(s)
#
# Parameters:
# [in] gti_files      Array of gti files being processed
# [in] in_mkf_files   Array of input mkf files
# [in] in_gti_files   Array of input gti files
# [in] cpkeyword      Keyword(s) to copy from evt file (or [NONE], ALL)
# [out] status        Output status
#
sub Finalize {

  my $fromext="";
  my $status = 0;
  ahlog::ah_out "\nFinalizing...\n";

  if(lc $fromfile eq "mkf") { $fromext = "FILTER"; } else { $fromext = "EHK"; } 

  foreach my $outgti (@gti_files) {

    # Update the timing keywords in the merged GTI file
    # (e.g. ONTIME, EXPOSURE, TSTART, TSTOP, etc...)
    if($upkeyword) {
      if(ahfilterlib::calc_timing_keys($outgti,"GTI",$leapsecfile)) {
        ahlog::ah_err "Could not update GTI keywords in file $outgti.";
        return 1;
      }
    }

    # Copy keywords from outfile if cpkeys is not NONE
    # If cpkeys is set to ALL, all keywords are copied from input file
    # to output file
    if((lc $cpkeyword ne "none") && (lc $cpkeyword ne "all")) {
      if(@in_mkf_files) {
        # +++ Do we want to hardcode extension names?
        $status = ahfilterlib::copy_keywords($in_mkf_files[0],$fromext,$outgti,"GTI","GTI",$cpkeyword);
      } else {
        $status = ahfilterlib::copy_keywords($in_gti_files[0],"GTI",$outgti,"GTI","GTI",$cpkeyword);
      }
      if($status) { 
        ahlog::ah_err "Could not copy keywords to $outgti";
        return $status; 
      }
    }

    # Write ahgtigen parameters to output file
    ahapp::write_parameters ($outgti, "GTI") ;                 
    
    # Check that the FITS file we just created is valid.
    unless (ahgen::check_fits_file ($outgti)) {
      ahgen::ah_err "FITS file: $outgti. failed FITS verification test." ;
      return 1;
    }

    ahlog::ah_info "HIGH", "File $outgti OKAY.\n";

  }

  return $status;

} # Finalize

sub determineIfCALDBExpression {

  my $read_caldb = 0;

  # Assume not possible initially.
  if($instrume !~ /none/i or $selectfile !~ /none/i or $label !~ /none/i) {
    if($selectfile =~ /none/i)  { $read_caldb=0; ahlog::ah_out "Missing selectfile. Not getting CALDB expression."; }
    elsif($label =~ /none/i)    { $read_caldb=0; ahlog::ah_out "Missing label. Not getting CALDB expression."; }
    elsif($instrume =~ /none/i) { $read_caldb=0; ahlog::ah_out "Missing instrume. Not getting CALDB expression."; }
    else { $read_caldb = 1; }
  }

  return $read_caldb;

}

sub run_maketime {

  my $infile=shift;
  my $outfile=shift;
  my $mtexpr=shift;
  my %maketime_pars=%{shift()};

  my $toolname="maketime";
  my $tmpoutfile=formTemporaryFileName($outfile,$toolname);

  our @params = (
    ['infile'    , $infile],
    ['outfile'   , $tmpoutfile],
    ['expr'      , $mtexpr],
    ['clobber'   , "yes"]
  );

  my @ordered_pars= qw(name value time start stop compact prefr postfr copykw );
  foreach my $par (@ordered_pars) {
    if(defined $maketime_pars{$par}) { push @params, [$par, $maketime_pars{$par}]; }
  }

  if(runTool($toolname,\@params)) { return 1; }
  if(copyOrMoveFileBasedOnCleanup($tmpoutfile,$outfile,$ahapp::cleanup)) { return 1; }

  return 0;

}

#
# $Log: ahgtigen.pl,v $
# Revision 1.38  2016/11/17 16:35:00  mwitthoe
# ahgtigen: fix bugs where the cpkeyword and upkeyword parameters were ignored in the case where no GTI merging is necessary
#
# Revision 1.37  2016/08/26 18:21:03  asargent
# Fixed bug when copying keywords to EHK file.
#
# Revision 1.36  2016/07/08 18:28:32  asargent
# Added error return code 2 when no exposure found after merging GTI
#
# Revision 1.35  2016/04/18 20:50:40  asargent
# Check if maketime parameters are defined before running maketime, rather than check the direct value
#
# Revision 1.34  2016/04/18 19:44:13  asargent
# Added refdata check for leapsec file.
#
# Revision 1.33  2016/04/06 03:14:14  asargent
# Added ability to use any type of file, including HK. Changed default value for postfr parameter to 1
#
# Revision 1.32  2016/01/19 14:19:29  asargent
# Removed instrument specification from caldb query.
#
# Revision 1.31  2016/01/06 02:15:04  asargent
# Fixed bug when writing HISTORY keyword to output file.
#
# Revision 1.30  2016/01/05 23:57:26  asargent
# Adjusted chatter for some logging due to HTML issues when printing log in pipeline
#
# Revision 1.29  2016/01/05 21:46:04  asargent
# Maketime has difficulty with complex expressions, use an at file as the input to maketime.
#
# Revision 1.28  2016/01/02 18:22:32  asargent
# Fixed bug when ahgtigen was trying to incorrectly merge a single GTI file.
#
# Revision 1.27  2015/12/31 16:28:02  asargent
# bug fix: ahscreen was not properly combining user-input and select file expressions if the user-input was set to none
#
# Revision 1.26  2015/10/20 16:23:53  asargent
# Updated merge_gti and calc_timing_keys functions
#
# Revision 1.25  2015/09/25 14:16:29  asargent
# Removed error when gtifile=none and tool was not querying for mergegti parameter.
#
# Revision 1.24  2015/09/22 15:37:38  asargent
# Updated parfile: postfr/prefr are required to be between 0 and 1, fixed query for mergemode parameter when no input gti files.
#
# Revision 1.23  2015/09/22 13:56:14  asargent
# Removed checking of DATE-OBS keyword if not querying CALDB for select file
#
# Revision 1.22  2015/09/16 14:50:57  asargent
# Fixed error description when reading DATE-OBS keyword
#
# Revision 1.21  2015/09/16 14:41:34  asargent
# Fixed bugs in CALDB query.
#
# Revision 1.20  2015/08/14 17:34:02  asargent
# Pass in array rather than hash to runTool
#
# Revision 1.19  2015/08/13 18:47:04  asargent
# Fixed error when writing outexpr parameter to ahscreen instead of ahgtigen
#
# Revision 1.18  2015/08/13 18:28:53  asargent
# Added leapsecfile parameter
#
# Revision 1.17  2015/08/12 20:28:18  asargent
# Uppercase'd fromfile when Building Expression
#
# Revision 1.16  2015/08/12 14:15:42  asargent
# Added missing parameters to merge_gti function
#
# Revision 1.15  2015/08/12 13:54:04  asargent
# Added missing comma during keyword writing
#
# Revision 1.14  2015/08/11 17:46:01  asargent
# Fixed an error in parameter input for copy_keywords function
#
# Revision 1.13  2015/08/11 17:27:13  asargent
# Updated copy_keywords function
#
# Revision 1.12  2015/08/05 17:59:29  asargent
# Removed copyfunc
#
# Revision 1.11  2015/08/05 16:28:47  asargent
# Cleaned up ahgtigen after review with LA.
#
# Revision 1.10  2015/07/08 19:32:53  asargent
# Updated print statements. Removed force_debug
#
# Revision 1.9  2015/06/30 17:36:11  asargent
# Updates to prologue and typo fixes.
#
# Revision 1.8  2015/06/25 19:22:01  asargent
# Updated CALDB expression and GTI file getting section.
#
# Revision 1.7  2015/06/24 17:19:16  asargent
# Bug fixes and function handling changes due to ahfilterlib updates
#
# Revision 1.6  2015/06/23 20:37:10  asargent
# Cleanup of ahgtigen.
#
#
