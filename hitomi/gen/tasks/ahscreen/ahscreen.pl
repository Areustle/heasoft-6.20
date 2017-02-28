#!/usr/bin/perl
#
# File name: ahscreen.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/07/08 16:29:00 $
# Version: 0
#
# Filter ASTRO-H science data using a combination of screening 
# based on time (calculated by ahgtigen) and/or data selection
# based on different data properties (e.g. GRADE for sxi, ITYPE 
# for sxs).
# 
# ahscreen will perform the following steps on an event file based
# on input parameters:
# 1. Merge Events
# 2. Merge GTI
# 3. Build Expression
# 4. Screen Events
# 5. Cut GTI
#
# Tool Dependencies:
#   ftsort
#   ftselect
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

our $infile       = ""; # Input event file
our $outfile      = ""; # Output event file 
our $outgti       = ""; # Output gti file (if mergegti=none )
our $gtifile      = ""; # Input GTI file
our $expr         = ""; # Additional expression for selection (or NONE)
our $selectfile   = ""; # Input expression file (or NONE, CALDB)
our $label        = ""; # Screening expression label in labelfile
our $mergegti     = ""; # Merge mode for GTI (AND, OR, [NONE])
our $cpkeyword    = ""; # Keyword(s) to copy from evt file (or [NONE], ALL)
our $upkeyword    = 1; # Update with keywords from evt file ([yes]/no)
our $leapsecfile  = ""; # Leapsecfile for updating timing keywords

our @in_evt_files = (); # array of input event files
our $eventexpr="NONE";      # Expression used for screening events
our $mergeevts=0;
our $instrume="";
our $exposure=0.;
our $naxis2_orig=0;
our $naxis2_final=0;

our @gti_files = ();    # array of gtifiles 

our $ahscreenerror = 0; # Script status

#########################
#  Main Code Block 
#########################

# Query canonical APE parameters and start logging. 
ahapp::startup () ;

# Pre-Processing
ahapp::begin_processing();

# Get the input parameters
$ahscreenerror = GetInputParameters();
unless ( $ahscreenerror == 0 ) {
  ahlog::ah_debug "GetInputParameters" ;
  ahapp::end_processing($ahscreenerror);
}

# Check the input files for clobber
$ahscreenerror = CheckInput();
unless ( $ahscreenerror == 0 ) {
  ahlog::ah_debug "CheckInput" ;
  ahapp::end_processing($ahscreenerror);
}

# Write all parameters to this script to the log file.
ah_info "LOW", ahapp::write_parameters () ;                 

# Main Processing 

# If list of files were input, merge events if parameter set to yes
$ahscreenerror = MergeEvents();
unless ( $ahscreenerror == 0 ) {
  ahlog::ah_debug "MergeEvents" ;
  ahapp::end_processing($ahscreenerror);
}

# If list of files were input, merge events if parameter set to yes
$ahscreenerror = MergeGTI();
unless ( $ahscreenerror == 0 ) {
  ahlog::ah_debug "MergeGTI" ;
  ahapp::end_processing($ahscreenerror);
}

# GTI cut for each event file
$ahscreenerror = CutGTI();
unless ( $ahscreenerror == 0 ) {
  ahlog::ah_debug "CutGTI" ;
  ahapp::end_processing($ahscreenerror);
}

# Calculate expression to filter events
$ahscreenerror = BuildExpression();
unless ( $ahscreenerror == 0 ) {
  ahlog::ah_debug "BuildExpression" ;
  ahapp::end_processing($ahscreenerror);
}

# If list of GTI files were input, merge GTI if parameter set to yes
$ahscreenerror = ScreenEvents();
unless ( $ahscreenerror == 0 ) {
  ahlog::ah_debug "ScreenEvents" ;
  ahapp::end_processing($ahscreenerror);
}

# Write expression to file history and par file
$ahscreenerror = UpdateHistoryKeywords();
unless ( $ahscreenerror == 0 ) {
  ahlog::ah_debug "UpdateHistoryKeywords" ;
  ahapp::end_processing($ahscreenerror);
}

# Verify file is okay
$ahscreenerror = Finalize();
unless ( $ahscreenerror == 0 ) {
  ahlog::ah_debug "Finalize" ;
  ahapp::end_processing($ahscreenerror);
}

# Print results
ahlog::ah_info "HIGH", "Successfully created FITS file:\n  $outfile" ;    
ahlog::ah_info "HIGH", "Found:\n  $naxis2_final/$naxis2_orig events\n";
ahlog::ah_info "LOW", "Selections applied:\n";
ahlog::ah_info "LOW", "  GTI File:\n    $gtifile\n";
ahlog::ah_info "LOW", "  EVENT:\n    $eventexpr\n";

# We're done.
ahapp::end_processing;

############################
# Pre-Processing Functions 
############################

# 
# Description: 
#
# Get parameter values from the command line and par file
#
# [in/out] infile     Input event file
# [in/out] outfile    Output event file (or root if not merging)
# [in/out] gtifile    Input GTI file
# [in/out] selectfile Input expression file (or NONE, CALDB)
# [in/out] label      Screening expression label in labelfile
# [in/out] expr       Additional expression for selection (or NONE)
# [in/out] mergegti   Merge mode for GTI (AND, OR, [NONE])
# [in/out] mergeexpr  Merge expression with CALDB ([AND], OR)
# [in/out] cpkeyword  Keyword(s) to copy from evt file (or [NONE], ALL)
# [in/out] upkeyword  Update with keywords from evt file ([yes]/no)
# [out] status        Output status
#
sub GetInputParameters {

  # get parameter values
  $infile       = ahapp::query_parameter("infile");
  $outfile      = ahapp::query_parameter("outfile");
  $gtifile      = ahapp::query_parameter("gtifile");
  $expr         = ahapp::query_parameter("expr");
  $selectfile   = ahapp::query_parameter("selectfile");
  $label        = ahapp::query_parameter("label");
  $mergegti     = ahapp::query_parameter("mergegti");
  $cpkeyword    = ahapp::query_parameter("cpkeyword");
  $upkeyword    = ahapp::query_parameter("upkeyword",1);
  $leapsecfile  = ahapp::query_parameter("leapsecfile");

  return 0;

} # GetInputParameters

# 
# Description: 
#
# Check if the output file(s) already exist.  Unless clobber is set, this will
# cause the script to fail. Create output event file(s)
#
# Parameters:
# [in] expr           Additional expression for selection (or NONE)
# [in] selectfile     Input expression file (or NONE, CALDB)
# [in] gtifile        Input GTI file
# [in] infile         Input event file (or @ file list)
# [in] in_evt_files   Array of input event files
# [in] mergeevts      Merge events if file list input ([yes]/no)
# [in] clobber        Overwrite existing output file (yes,[no])
# [out] status        Output status
#
sub CheckInput { 

  my $status = 0;
  my $num_evt_files = 0; # Total number of output event files

  ahlog::ah_out "Checking input";

  # Reset the output expression parameter
  ahgen::run_ftool("pset","ahscreen","outexpr= ");

  # get list of files from infile parameter
  # Check if @ file lists exist
  if ($infile =~ /^@/) {
    if(isRequiredFileNotFound(substr($infile,1))) { return 1; } 
  }
  if ($gtifile =~ /^@/) {
    if(isRequiredFileNotFound(substr($gtifile,1))) { return 1; } 
  }
  @in_evt_files = readInputFileList($infile);
  @gti_files = readInputFileList($gtifile);
  
  if(isRequiredFileNotFound(@in_evt_files)) { return 1; }
  if(isRequiredFileNotFound(@gti_files)) { return 1; }

  # Save number of event files to process
  $num_evt_files=@in_evt_files;

  # Check that we have event files to process
  unless($num_evt_files) {
    ahgen::ah_err "No input event files.";
    return 1;
  }

  if($expr =~ /none/i and 
     $selectfile =~ /none/i and 
     $gtifile =~ /none/i and
     $num_evt_files == 1 ) {
    ahlog::ah_err "No expressions or GTI files. Nothing to do.\n";
    return 1;
  }

  # Get the leapsec file in the refdata directory if leapsec eq REFDATA
  if ( ahgen::isFileRefData($leapsecfile) ) {
    $leapsecfile = $ENV{LHEA_DATA} . "/leapsec.fits";
  }

  # merging needed if more than one event file
  if($num_evt_files>1) { $mergeevts = 1; }

  # Check for output file clobber
  if(removeOutputFileIfClobbering($outfile,$ahapp::clobber)) { return 1; }

  # If we are not merging GTI (i.e. not attaching) then create an output GTI file
  # based on the outfile parameter
  # Searches for the last '.' and removes anything after. Then '.gti' is added to
  # the output GTI file 
  $outgti = $outfile;
  $outgti =~ s/\..+$//;
  $outgti = $outgti . ".gti";
  if ( uc $mergegti ne "NO" ) {
    ahapp::add_temp_file($outgti);
  }
  if ( @gti_files ) { 
    # Check for output file clobber
    if(removeOutputFileIfClobbering($outgti,$ahapp::clobber)) { return 1; }
  }

  if($num_evt_files==1) { 
    if(ahgen::copy_fits_file($in_evt_files[0]."[EVENTS]",$outfile,"copyall=no")) { return 1; }
    # Get the original counts from the merged input file
    $naxis2_orig = ahgen::get_keyword($outfile,"EVENTS","NAXIS2");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "In file $outfile, NAXIS2 keyword not defined.\n";
      return 1;
    }
  }

  if ( uc $mergegti ne "NONE" and
       uc $mergegti ne "AND" and
       uc $mergegti ne "OR" ) {
       ahlog::ah_err "Not a valid mode for mergegti parameter. Must be AND, OR or NONE";
       return 1;
     }

  # Read instrument from first event file.
  $instrume = ahgen::get_keyword($in_evt_files[0],"EVENTS","INSTRUME");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $in_evt_files[0], INSTRUME keyword not defined.\n";
    return 1;
  }


  return $status;

}

############################
#      Main Processing 
############################

# 
# Description: 
#
# Check the list of input files. If more than one file was
# input, merge into one file if mergeevts parameter set
#
# Parameters:
# [in] mergeevts      Merge events if file list input ([yes]/no)
# [in] infile         Input event file (or @ file list)
# [in] outfile        Output event file
# [out] status        Output status
#
sub MergeEvents {

  my $status = 0;
  my $TmpOutfile = "";

  unless ($mergeevts) { return $status; }
  ahlog::ah_out "\nMerging Events\n";
  ahlog::ah_out " $_\n" foreach @in_evt_files;

  # Force merging to use the EVENTS extension
  my @evtfiles = ();
  foreach my $evtfile ( @in_evt_files ) {
    push @evtfiles, $evtfile . "[EVENTS]";
  }
  # Merge the events with copyall=no
  if( ahfilterlib::merge_events(\@evtfiles, $outfile,"no") ) {
    ahlog::ah_err "Unable to merge input files from $infile.\n";
    return $status;
  }

  # order file by time before running GTI filter
  $TmpOutfile = $outfile . ".sorted.tmp";
  ahapp::add_temp_file($TmpOutfile);
  # Sort the events by TIME column
  $status = ahgen::run_ftool("ftsort",
                   "infile=$outfile"."[EVENTS]",
                   "outfile=$TmpOutfile",
                   "columns=TIME");
  if ( $status ) {
    ahlog::ah_err "Error sorting file $outfile by TIME" ;
    return $status;
  }
  if(copyOrMoveFileBasedOnCleanup($TmpOutfile, $outfile, $ahapp::cleanup)) { return 1; }

  # Get the original counts from the merged input file
  $naxis2_orig = ahgen::get_keyword($outfile,"EVENTS","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $outfile, NAXIS2 keyword not defined.\n";
    return 1;
  }

  return $status;
 
} # MergeEvents

# 
# Description: 
#
# Check the list of input files and gtifile parameter. 
# If more than one file was input, merge all GTI into 
# one file if mergegti parameter is set
#
# Parameters:
# [in] mergegti       Merge mode for GTI (AND, OR, [NONE])
# [in] outfile        Output event file
# [in] gti_files      Array of event files being processed
# [in] upkeyword      Update with keywords from evt file ([yes]/no)
# [out] status        Output status
#
sub MergeGTI {
  
  my $status = 0;
  my %mgtime_pars;
  my $mergemode = $mergegti;

  unless (@gti_files) { return $status; }
  if($mergegti =~ /no/i) { $mergemode = "AND"; }
  ahlog::ah_out "\nMerging GTI:\n";
  ahlog::ah_out " $_\n" foreach @gti_files;

  # Merge GTI files. The merge_gti subroutine returns the number of GTI files merged.
  if( ahfilterlib::merge_gti([@gti_files],$outgti,$mergemode,"GTI",\%mgtime_pars) ) {
    ahlog::ah_err "Error merging GTI files" ;
    return 1;
  } 

  my $nrows = ahgen::get_keyword($outgti,"GTI","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $outgti, NAXIS2 keyword not defined.\n";
    return 1;
  }
  if ( $nrows == 0 ) {
    ahlog::ah_info "HIGH", "No exposure found after merging GTI.\n";
    return 2;
  }
  # Update the timing keywords in the merged GTI file
  # (e.g. ONTIME, EXPOSURE, TSTART, TSTOP, etc...)
  if($upkeyword) {
    if(ahfilterlib::calc_timing_keys($outgti,"GTI",$leapsecfile)) {
      ahlog::ah_err "Could not update GTI keywords in file $outgti.";
      return 1;
    }
    $exposure = ahgen::get_keyword($outgti,"GTI","EXPOSURE");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "In file $outgti, EXPOSURE keyword not defined.\n";
      return 1;
    }
  }

  # Set the output GTI as the gtifilter
  $gtifile = $outgti;

  # Delete the GTI extension (if exists) and append the new GTI
  # (There should be no GTI extension since copyall=no)
  if ( ahgen::check_hdu_exists($outfile,"GTI") ) {
    $status = ahgen::run_ftool("ftdelhdu",$outfile."[GTI]","none","confirm=yes");
    if($status) { ahlog::ah_err "Error deleting GTI extension from $outfile"; }
  }

  # If mergegti parameter is set to 'none', then create an output GTI file
  if ( lc $mergegti ne "none" ) {
    if(ahgen::copy_hdu($outgti,"GTI",$outfile)) { ahlog::ah_err "Error appending GTI to $outfile"; return 1; }
    ahapp::add_temp_file($outgti);
  }


  return $status;

} # MergeGTI


# 
# Description: 
#
# Cut event file(s) on GTI. GTI may be from input GTI file,
# merged input GTI with the event file GTI, or all event file
# GTIs merged.
#
# Parameters:
# [in] evt_files      Array of event files being processed
# [in] mergegti       Merge mode for GTI (AND, OR, [NONE])
# [in] gtifile        Input GTI file
# [out] status        Output status
#
sub CutGTI {

  my $status = 0;
  my $TmpInfile = "";
  my $TmpOutfile = "";
  my $gtifilter = "";

  if(lc $gtifile eq "none") { return $status; }
  ahlog::ah_out "\nScreening GTI\n";

  # Filter GTI from event file(s)
  $TmpOutfile = $outfile. ".gticlean.tmp";
  ahapp::add_temp_file($TmpOutfile);
  $gtifilter = "gtifilter(\"$gtifile\")";

  $status = run_ftselect($outfile,$TmpOutfile,$gtifilter);
  if($status) { 
    if ( $status == 3 ) {
      ahlog::ah_info "HIGH", "Filtered all events during GTI screening";
      return 3;
    } else {
      return 1; 
    }
  }
  if(copyOrMoveFileBasedOnCleanup($TmpOutfile, $outfile, $ahapp::cleanup)) { return 1; }

  return $status;

} # CutGTI

# 
# Description: 
#
# Build expression based on CALDB settings and user-input expression
#
# Parameters:
# [in] expr           Additional expression for selection (or NONE)
# [in] selectfile     Input expression file (or NONE, CALDB)
# [in] mergeexpr      Merge expression with CALDB ([AND], OR)
# [in] evt_files      Array of event files being processed
# [in] label          Screening expression label in labelfile
# [in/out] eventexpr  Combined expression to be used for screening
# [out] status        Output status
#
sub BuildExpression {
  
  my $calexpr="";
  my $mgexpr="";
  my $instrument="";

  my $read_caldb=0;

  my $status = 0;

  # Assume not possible initially.
  $read_caldb = determineIfCALDBExpression();

  if(lc $expr eq "none" and $read_caldb == 0) { return $status; } # nothing to do
  ahlog::ah_out "\nBuilding Expression\n";
  
  # Check if it's possible to check CALDB (or user input selectfile)
  if($read_caldb) {

    my $dateobs;
    if(lc $selectfile eq "caldb") {
      # Get TSTART and INSTRUME from first event file
      $dateobs = ahgen::get_keyword($in_evt_files[0],"EVENTS","DATE-OBS");
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "In file $in_evt_files[0], DATE-OBS keyword not defined.\n";
        return 1;
      }
    }

    # Determine if reading from CALDB or user-input labelfile
    my $labelfile = ahfilterlib::call_quzcif($selectfile,"GEN","-","SELECT",$dateobs,"-");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "Error querying CALDB for labelfile";
      return 1;
    }

    $calexpr = ahfilterlib::get_caldb_expr($labelfile,$instrume,"EVENT",$label);
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "Error getting expression from $labelfile.";
      return 1;
    }

  } # end read caldb

  # Build the expression
  if(lc $expr ne "none") { $eventexpr = $expr; }
  if(lc $calexpr ne "none") { $eventexpr = $calexpr; }
  if(lc $calexpr ne "none" and lc $expr ne "none" ) { $eventexpr = "$expr&&$calexpr"; }

  # Clean up expressions, remove extraneous &&s
  $eventexpr =~ s/^(\&\&|\|\|)//;
  $eventexpr =~ s/(\&\&|\|\|)$//;

  return $status;

} # BuildExpression

# 
# Description: 
#
# Filter events based on expression. Expression may be user-input,
# from CALDB file, or a combination of both.
#
# Parameters:
# [in] evt_files      Array of event files being processed
# [in] eventexpr      Combined expression to be used for screening
# [out] status        Output status
#
sub ScreenEvents {

  my $status = 0;
  my $TmpInfile = "";
  my $TmpOutfile = "";

  if($eventexpr eq "NONE") { return $status; }
  ahlog::ah_out "\nScreening Events\n";

  ahlog::ah_info "LOW", "Using ftselect expression: $eventexpr";

  $TmpOutfile = $outfile . ".ftselect.tmp";
  ahapp::add_temp_file($TmpOutfile);

  $status = run_ftselect($outfile,$TmpOutfile,$eventexpr);
  if($status) { 
    if ( $status == 3 ) {
      ahlog::ah_info "HIGH", "Filtered all events during event screening";
      return 3;
    } else {
      return 1; 
    }
  }
  if(copyOrMoveFileBasedOnCleanup($TmpOutfile, $outfile, $ahapp::cleanup)) { return 1; }
  return $status;

} # ScreenEvents

# 
# Description: 
#
# Update the filtering expression used as history keywords 
# in EVENTS extension in output event file(s) and parameter file
#
# Parameters:
# [in] eventexpr      Combined expression to be used for screening
# [in] evt_files      Array of event files being processed
# [out] status        Output status
#
sub UpdateHistoryKeywords {

  if($eventexpr eq "") { 
    ahgen::run_ftool("pset","ahscreen","outexpr=NONE");
    return 0; 
  }
  ahlog::ah_out "\nUpdating History\n";

  # Split expression into 72 character elements
  # to write into FITS comments
  my @commentexpr=unpack("(a72)*",$eventexpr);

  # Set final expression used to create GTI to output parameter finalexpr.
  # Need to set it outside of calcgti in case no GTI calculation was done.
  ahgen::run_ftool("pset","ahscreen","outexpr=$eventexpr");

  # Loop over each output file and each segment of expression
  ahgen::set_keyword($outfile,"EVENTS","HISTORY","Expression for ahscreen cleaning:");
  if (ahgen::get_error_flag) {
    ahgen::ah_err "Error setting HISTORY keywords to $outfile.";
    return 1;
  }
  foreach my $comment (@commentexpr) {
    chomp $comment; # remove any newline characters

    # set keyword HISTORY to (part of) expression
    ahgen::set_keyword($outfile,"EVENTS","HISTORY"," $comment");
    if (ahgen::get_error_flag) {
      ahgen::ah_err "Error setting HISTORY keywords to $outfile.";
      return 1;
    }
  }
  return 0;

} # UpdateKeywords

# 
# Description: 
#
# Copy specified keywords from first input event file and write ahscreen
# parameters in output file. Run ftverify on output file(s)
#
# Parameters:
# [in] evt_files      Array of event files being processed
# [in] in_evt_files   Array of input event files
# [in] cpkeyword      Keyword(s) to copy from evt file (or [NONE], ALL)
# [out] status        Output status
#
sub Finalize {

  my $status = 0;
  ahlog::ah_out "\nFinalizing...\n";

  # Update the final number of counts
  $naxis2_final = ahgen::get_keyword($outfile,"EVENTS","NAXIS2");
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "In file $outfile, NAXIS2 keyword not defined.\n";
    return 1;
  }


  # Copy keywords from outfile if cpkeys is not NONE
  # If cpkeys is set to ALL, all keywords are copied from input file
  # to output file
  if($cpkeyword !~ /none/i) {
    $status = ahfilterlib::copy_keywords($in_evt_files[0],"EVENTS",$outfile,"EVENTS","EVENTS",$cpkeyword);
    if($status) { 
      ahlog::ah_err "Could not copy keywords from $in_evt_files[0] to $outfile.";
      return $status; 
    }
    if ( lc $gtifile ne "none" ) {
      $status = ahfilterlib::copy_keywords($in_evt_files[0],"EVENTS",$outfile,"GTI","GTI","coord");
      $status = ahfilterlib::copy_keywords($in_evt_files[0],"EVENTS",$outfile,"GTI","GTI","obs");
      $status = ahfilterlib::copy_keywords($in_evt_files[0],"EVENTS",$outfile,"GTI","GTI","timing");
      if(ahfilterlib::calc_timing_keys($outfile,"GTI",$leapsecfile)) {
        ahlog::ah_err "Could not update GTI keywords in file $outfile.";
        return 1;
      }
      $status = ahfilterlib::copy_keywords($outfile,"GTI",$outfile,"EVENTS","EVENTS","timing");
      if($status) { 
        ahlog::ah_err "Could not copy keywords from $in_evt_files[0] to $outfile.";
        return $status; 
      }
    }
  }

  # Write ahscreen parameters to output file
  ahapp::write_parameters ($outfile,"EVENTS") ;                 
  
  # Check that the FITS file we just created is valid.
  unless (ahgen::check_fits_file ($outfile)) {
    ahgen::ah_err "FITS file: $outfile. failed FITS verification test." ;
    return 1;
  }

  ahlog::ah_info "HIGH", "File $outfile OKAY.\n";


  return $status;

} # Finalize

sub run_ftselect ($$$) {

  my $toolname="ftselect";
  my $TmpInfile=shift;
  my $TmpOutfile=shift;
  my $expr=shift;
  my $status = 0;

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
    return 1;
  }
  if($naxis2 == 0) {
    ahlog::ah_err "Filtered all rows in event file $TmpInfile";
    return 3;
  }

  return $status;

}

sub determineIfCALDBExpression {

  my $read_caldb = 0;

  # Assume not possible initially.
  if(lc $selectfile ne "none" or lc $label ne "none") {
    if(lc $selectfile eq "none")  { $read_caldb=0; ahlog::ah_out "Missing selectfile. Not getting CALDB expression."; }
    elsif(lc $label eq "none")    { $read_caldb=0; ahlog::ah_out "Missing label. Not getting CALDB expression."; }
    elsif(lc $instrume eq "none") { $read_caldb=0; ahlog::ah_out "Missing instrume. Not getting CALDB expression."; }
    else { $read_caldb = 1; }
  }

  return $read_caldb;

}

#
# $Log: ahscreen.pl,v $
# Revision 1.41  2016/07/08 16:29:00  asargent
# Added new error exit status codes for no exposure created (2) and all events screened (3). All other error status codes are set to 1.
#
# Revision 1.40  2016/04/19 14:15:43  asargent
# Allow for just merging of event files without screening events
#
# Revision 1.39  2016/04/18 19:39:01  asargent
# Retrieve leapsec file from reference area if leapsecfile=REFDATA
#
# Revision 1.38  2016/04/06 07:54:41  asargent
# Force copying/merging to use EVENTS extension from input file
#
# Revision 1.37  2016/04/06 02:50:17  asargent
# Updated GTI merging/attaching method. No additional extensions are copied to the output from the input event file(s). Updated help file.
#
# Revision 1.36  2016/03/24 20:38:09  asargent
# Updated keyword writing to GTI and EVENTS extensions.
#
# Revision 1.35  2016/03/18 18:53:56  asargent
# Removed invalid gtimerge setting if more than one event file.
#
# Revision 1.34  2016/03/12 00:16:15  asargent
# Write keywords to GTI extension if it exists and cpkeywords is set
#
# Revision 1.33  2016/01/19 14:18:56  asargent
# Removed instrument specification from caldb query.
#
# Revision 1.32  2016/01/06 01:13:38  asargent
# Added logging for event files to be merged
#
# Revision 1.31  2016/01/05 23:57:05  asargent
# Adjusted chatter for some logging due to HTML issues when printing log in pipeline
#
# Revision 1.30  2016/01/05 22:55:26  asargent
# Moved counting of cleaned events to finalize routine.
#
# Revision 1.29  2016/01/05 21:42:31  asargent
# Bug fixes and logging updates.
#
# Revision 1.28  2015/12/31 22:20:20  asargent
# Do not delete any GTI extensions, only append. Only merge specified GTI files.
#
# Revision 1.27  2015/12/31 16:27:36  asargent
# bug fix: ahscreen was not properly combining user-input and select file expressions if the user-input was set to none
#
# Revision 1.26  2015/10/20 16:24:14  asargent
# Updated merge_events, merge_gti and calc_timing_keys functions
#
# Revision 1.25  2015/09/22 13:56:38  asargent
# Removed checking of DATE-OBS keyword if not querying CALDB for select file
#
# Revision 1.24  2015/09/16 14:50:31  asargent
# Fixed error description when reading DATE-OBS keyword
#
# Revision 1.23  2015/09/16 14:46:04  asargent
# Fixed bugs during CALDB query.
#
# Revision 1.22  2015/08/14 17:35:25  asargent
# Pass in array rather than hash to runTool
#
# Revision 1.21  2015/08/13 18:36:50  asargent
# Added leapsecfile parameter
#
# Revision 1.20  2015/08/12 20:26:38  asargent
# Removed premature deletion of GTI file
#
# Revision 1.19  2015/08/12 19:48:56  asargent
# Uppercase'd get_caldb_expr fromfile parameter. Fixed bug when comparing instrume string
#
# Revision 1.18  2015/08/11 17:45:49  asargent
# Fixed an error in parameter input for copy_keywords function
#
# Revision 1.17  2015/08/11 17:25:24  asargent
# Updated copy_keywords function
#
# Revision 1.16  2015/08/05 23:23:24  asargent
# Fixed errors in run_ftselect
#
# Revision 1.15  2015/08/05 17:58:14  asargent
# Cleanup of ahscreen after review with LA
#
# Revision 1.13  2015/07/08 19:30:07  asargent
# Updated print statements. Removed force_debug
#
# Revision 1.12  2015/06/30 17:38:21  asargent
# Updates to prologue and typo fixes.
#
# Revision 1.11  2015/06/25 19:22:14  asargent
# Updated CALDB expression and GTI file getting section.
#
# Revision 1.10  2015/06/24 17:18:47  asargent
# Bug fixes and function handling changes due to ahfilterlib updates
#
#
