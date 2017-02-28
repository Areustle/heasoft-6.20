#!/usr/bin/perl
#
# File name: ahfilter.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/12/01 20:50:49 $
# Version: 0
#
# This task calculates a mkf file. This is obtained running ahmkehk and makefilters. 
# Uses the same input file of ahmkehk to run ahmkehk and the configuration file to 
# run makefilter. The configuration file contains several paramaters from the HK 
# files as well as paramaters from ahmkehk. It may be specified as an ascii file 
# or a FITS file. The tool by default looks for the configuration file in CALDB.
# 
# 
#
# Tool Dependencies:
#   ahmkehk
#   makefilter
#   ftlist
#   fthedit
# 
# Library Dependencies:
#   gen/lib/perl/ahlog
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
#
# Modification History:
#

use strict;
use warnings;

use ahlog ;
use ahapp ;
use ahgen qw (:ALL);
use ahfilterlib ;

#########################
# Variable Definitions 
#########################

# Parameter variables
our $mkfconf = "";     # Input makefilter configuration file (CALDB)
our $attfile = "";     # Input attfile file
our $orbfile = "";     # Input orbit file
our $reference = "";   # Input reference file (supplies TIME column)
our $teldeffile = "";  # Input teldef file
our $leapsecfile = ""; # Input leap seconds file
our $cor2file = "";    # Input rigidity file for COR2 column
our $cor3file = "";    # Input rigidity file for COR3 column
our $saafile = "";     # Input rigidity file for SAA columns
our $outehkfile = "";  # Output .ehk file
our $outmkffile = "";  # Output makefilter file
our $attext = "";      # Attitude extension              
our $attform = "";     # Format of input attitude
our $attcol = "";      # Input attitude column
our $orbext = "";      # Orbit extension
our $orbform = "";     # Orbital velocity format (VECTOR, COMPONENTS, KEPLERIAN)
our $orbcol = "";      # Input orbit column
our $timecol = "";     # Time column name
our $optaxis = "";     # Optical axis coordinates in FOC
our $tstart = 0.0;     # Start time
our $tstop = 0.0;      # Stop time
our $bintime = 0.0;    # Step time (s)
our $textend = "";     # Margin time (s)
our $infileroot = "";  # Prefix of the input HK files

our $ahfiltererror = 0;# return status of ftool calls
our $mission="HITOMI"; # mission parameter for makefilter

#########################
#  Main Code Block 
#########################

# Query canonical APE parameters and start logging. 
ahapp::startup () ;

# Pre-Processing
ahapp::begin_processing();

# Get the input parameters
$ahfiltererror = GetInputParameters();
unless ( $ahfiltererror == 0 ) {
  ahlog::ah_err "GetInputParameters" ;
  ahapp::end_processing($ahfiltererror);
}

# Check the input files for clobber
$ahfiltererror = CheckInput();
unless ( $ahfiltererror == 0 ) {
  ahlog::ah_err "CheckInput" ;
  ahapp::end_processing($ahfiltererror);
}

# Write all parameters to this script to the log file.
ah_info "HIGH", ahapp::write_parameters () ;                 

# Main Processing 

# Create the EHK file from attitude and orbit
$ahfiltererror = MakeEHK();
unless ( $ahfiltererror == 0 ) {
  ahlog::ah_err "MakeEHK" ;
  ahapp::end_processing($ahfiltererror);
}

# Create the MKF file from the config file and EHK/HK files
$ahfiltererror = MakeFilter();
unless ( $ahfiltererror == 0 ) {
  ahlog::ah_err "MakeFilter" ;
  ahapp::end_processing($ahfiltererror);
}

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
sub GetInputParameters {

  # get parameter values
  $mkfconf     = ahapp::query_parameter("mkfconf");     # makefilter
  $attfile     = ahapp::query_parameter("attfile");     # ahmkehk
  $orbfile     = ahapp::query_parameter("orbfile");     # ahmkehk
  $reference   = ahapp::query_parameter("reference");   # ahmkehk
  $teldeffile  = ahapp::query_parameter("teldeffile");  # ahmkehk
  $leapsecfile = ahapp::query_parameter("leapsecfile"); # ahmkehk
  $cor2file    = ahapp::query_parameter("cor2file");    # ahmkehk
  $cor3file    = ahapp::query_parameter("cor3file");    # ahmkehk
  $saafile     = ahapp::query_parameter("saafile");     # ahmkehk
  $outehkfile  = ahapp::query_parameter("outehkfile");  # ahmkehk (par: outfile); makefilter (par: infileroot, with modification)
  $attext      = ahapp::query_parameter("attext");      # ahmkehk
  $attform     = ahapp::query_parameter("attform");     # ahmkehk
  $attcol      = ahapp::query_parameter("attcol");      # ahmkehk
  $orbext      = ahapp::query_parameter("orbext");      # ahmkehk
  $orbform     = ahapp::query_parameter("orbform");     # ahmkehk
  $orbcol      = ahapp::query_parameter("orbcol");      # ahmkehk
  $outmkffile  = ahapp::query_parameter("outmkffile");  # makefilter (par: outfile)
  $timecol     = ahapp::query_parameter("timecol");     # ahmkehk
  $optaxis     = ahapp::query_parameter("optaxis");     # ahmkehk
  $tstart      = ahapp::query_parameter("tstart");      # ahmkehk
  $tstop       = ahapp::query_parameter("tstop");       # ahmkehk
  $bintime     = ahapp::query_parameter("bintime");     # ahmkehk
  $textend     = ahapp::query_parameter("textend");     # ahmkehk
  $infileroot  = ahapp::query_parameter("infileroot");  # makefilter

  return 0;

} # GetInputParameters

# 
# Description: 
#
# Check if the output files already exist.  Unless clobber is set, this will
# cause the script to fail.
#
# Parameters:
# [in] clobber    Overwrite existing output file (yes,[no])
# [in] outehkfile Output EHK file from ahmkehk
# [in] outmkffile Output filter file from makefilter
#
sub CheckInput {

  my $status = 0;

  if(isRequiredFileNotFound($attfile)) { return 1;} 
  if(isRequiredFileNotFound($orbfile)) { return 1;} 

  # If an output file already exists, delete it.
  if(removeOutputFileIfClobbering($outehkfile,$ahapp::clobber)) { return 1; }
  if(removeOutputFileIfClobbering($outmkffile,$ahapp::clobber)) { return 1; }

  # Add in CALDB query for configuration file
  if(isFileCALDB($mkfconf)) {
    my $dateobs = ahgen::get_keyword($attfile,"ATTITUDE","DATE-OBS");
    ahlog::ah_info "HIGH", "Querying CALDB for MKF Configuration file.";
    $mkfconf = ahfilterlib::call_quzcif($mkfconf, "GEN", "-", "MKFCONF", $dateobs, "-", $mission);
    if(ahgen::get_error_flag()) { return 1; }
  } else {
    if(isRequiredFileNotFound($mkfconf)) { return 1;} 
  }

  return $status;

}

############################
#      Main Processing 
############################

# 
# Description: 
#
# Generate an EHK file using tool ahmkehk.
#
# Parameters:
# [in] attfile     Input attitude file
# [in] orbfile     Input orbit file
# [in] outehkfile  Output EHK file from ahmkehk
# [in] tstart      Starting time [s] of output file (if 0, use attitude)
# [in] tstop       Ending time [s] of output file (if 0, use attitude)
# [in] textend     Amount to extend tstart/tstop [s]
# [in] reference   Input time reference file (or NONE)
# [in] timecol     Input time column in reference file
# [in] attext      Attitude extension
# [in] attform     Format of input attitude
# [in] attcol      Input attitude column
# [in] teldeffile  Input teldef file (or CALDB)
# [in] corfile     Input cut-off rigidity file (or CALDB)
# [in] leapsecfile Input leap second file (or CALDB)
# [in] orbext      Orbit extension
# [in] orbcol      Orbit columns
# [in] orbform     Orbital velocity format (VECTOR, COMPONENTS, KEPLERIAN)
# [out] status     Return 0 for successful creation of EHK file
#
sub MakeEHK {

  my $status = 0;

  ah_info "HIGH", "Generating an EHK file...";

  # run ahmkehk
  $status=ahgen::run_ftool("ahmkehk",$attfile,$orbfile,$outehkfile,
                    "tstart=$tstart","tstop=$tstop","bintime=$bintime",
                    "textend=$textend","reference=$reference","timecol=$timecol",
                    "attext=$attext","attform=$attform","attcol=$attcol",
                    "teldeffile=$teldeffile","optaxis=$optaxis",
                    "cor2file=$cor2file","cor3file=$cor3file",
                    "saafile=$saafile","leapsecfile=$leapsecfile",
                    "orbext=$orbext","orbcol=$orbcol","orbform=$orbform");
  if ($status) {
    ah_err "Errors detected trying to run FTOOL: ahmkehk on $outehkfile";
    return $status;
  }

  ahapp::write_parameters ($outehkfile,"EHK") ;                 

  # check output file
  $status = ahgen::update_checksum_and_verify($outehkfile);
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "update_checksum_and_verify" ;
    return $status ;
  }

  ahlog::ah_info "HIGH", "Successfully created FITS file $outehkfile\n\n" ;    

  return 0;

}

# 
# Description: 
#
# Generate an EHK file using tool ahmkehk.
#
# Parameters:
# [in] infileroot  Prefix of the input HK files
# [in] outehkfile  Output EHK file from ahmkehk
# [in] outmkffile  Output filter file from makefilter
# [in] mkfconf     Input configuration file from makefilter
# [in] mission     Mission parameter for makefilter
# [out] status     Return 0 for successful creation of EHK file
#
sub MakeFilter {

  my $status = 0;

  # set up parameter -- needs root of filename, obtained from output ehk file
  my @fileparts = split /\./, $outehkfile;
  my $ext = pop(@fileparts);
  if($infileroot eq "" ) {
    # construct infileroot to be the full path of the output EHK file 
    # with the extension removed (the extension is removed by pop() above).
    $infileroot = join ".", @fileparts;
  } else {
    # need to copy ehk file to infileroot.ehk, otherwise makefilter won't work
    # If there are multiple files with different infileroots

    # This assumes there is an "ehk" file in the config file. This seems like a good
    # assumption since the file was just created. If not, the file is ignored and 
    # deleted.
    my $tmpfile=$infileroot . "." . $ext;
    if ($outehkfile ne $tmpfile) {
      ahgen::copy_fits_file( $outehkfile, $tmpfile);
      ahapp::add_temp_file($tmpfile);
    }
  }

  # Makefilter is not compatible with FITS files
  # Check if the input configuration file is an ASCII or FITS file
  # If input config file is a FITS file, convert to ASCII
  ahlog::ah_out "Checking if configuration file $mkfconf is FITS or ASCII";
  if(check_fits_file($mkfconf)) { 
    ahlog::ah_out "$mkfconf is FITS. Converting to ASCII";
    if(convertFITSConfigToASCII()) { return 1; }
  }

  # run makefilter
  ahlog::ah_info "HIGH", "Generating an MKF file...";
  $status=ahgen::run_ftool("makefilter",$mkfconf,$infileroot,$outmkffile,
                    "mission=$mission");
  # makefilter does not return an error code ...
  if ($status) {
    ahlog::ah_err "Errors detected trying to run FTOOL: makefilter on $outehkfile";
    return $status;
  }

  # Fix TNULL values in the mkf file
  $status = fix_nulls($outmkffile,"FILTER");
  if($status) { 
    ahlog::ah_err "fix_nulls" ;
    return $status;
  }

  # Copy the header to the MKF file
  if (ahfilterlib::copy_keywords($attfile,"ATTITUDE",$outmkffile,"FILTER","primary","all")) { return 1; }

  # check output file
  $status = ahgen::update_checksum_and_verify($outmkffile);
  if( ahgen::get_error_flag ) {
    ahlog::ah_err "update_checksum_and_verify" ;
    return $status;
  }

  ahapp::write_parameters ($outmkffile,"FILTER") ;                 

  ahlog::ah_info "HIGH", "Successfully created FITS file $outmkffile" ;    

  return 0;

}

# 
# Description: 
#
# Assign appropriate TNULL values for EHK file from ahmkehk
#
# Parameters:
# [in] outehkfile Output EHK file from ahmkehk
# [out] status     Return 0 for successful creation of EHK file
#
sub fix_nulls {

  # 
  # Makefilter uses an inappropriate default TNULL = -max for columns with
  # unsigned integer types.
  # Set TNULL keywords in the given extension and file to unsigned bytes (B)
  # to 256, short integers (I) to 32768, and long integers (J) to 2147483648
  #
  # Adapted from the fixNulls routine in Swift
  #
  # Note: We only want to do this for the output ehkfile from ahmkehk
  #       and not edit any input files

  my $fixfile = shift;
  my $ext = shift;

  my $status = 0;

  my %max = ('B' => 256, # Unsigned byte,
             'I' => 32768, # Short integer,
             'J' => 2147483648); # Long integer


  ah_info "LOW", "\nChecking file $fixfile\[$ext] to fix nulls...\n";

  # Print out all keywords from EHK file except COMMENT, HISTORY and END, mostly because
  # they don't have equal signs
  ahgen::set_quiet(1); # we don't want any printed output
  $status = run_ftool("ftlist",$fixfile . "[$ext]", "K", "exclude=COMMENT,HISTORY,END");
  if($status) {
    ah_err "Errors detected trying to fix nulls in $fixfile";
    return $status;
  }
  ahgen::set_quiet(); # turn chatter back on

  my %keys;

  foreach ( split "\n", ahgen::get_tool_stdout ) {
    if( /^([\w-]+)\s*=\s*'(.+)'/ ){
      $keys{$1} = $2;
      $keys{$1} =~ s/ +$//;
    } elsif( /^([\w-]+)\s*=\s*([^\s\/]+)/ ){
      $keys{$1} = $2;
    }
  }

  open (my $keyfile , ">", "keys.list");
  ahapp::add_temp_file("keys.list");
  my $n=0;
  foreach my $tform (grep /^TFORM/,  keys %keys) {
    next unless ($keys{$tform} =~ /([IJB])/);

    # get type (e.g., I) and keyword number
    my $type = $1;
    my $num = $tform;
    $num =~ s/TFORM//;

    # Skip unless TZERO key is max
    next unless ($keys{"TZERO$num"} = $max{$type});

    # Add the TNULL keyword and value
    unless( $keys{"TNULL$num"} ){
      ah_info "LOW", "column " . $keys{"TTYPE$num"} . "  has no TNULL keyword. " .
              "Will use " . ($max{$type}-1) . " for TNULL$num.\n";
      print $keyfile "TNULL$num " . ($max{$type}-1) . "\n";
      $n++;
    }
  }
  close $keyfile;

  if($n) {
    $status = run_ftool("fthedit", $fixfile . "[$ext]", "\@keys.list", "a");
    if($status) { 
      ah_err "Errors detected trying to fix nulls in $fixfile";
      return $status;
    }
  }
    
  # Success or nothing to be done 
  return 0;

}

sub convertFITSConfigToASCII {

  my $mkfconf_ascii = "mkfconf.ascii";
  ahapp::add_temp_file($mkfconf_ascii);
  ahlog::ah_out "Configuration file is FITS, converting to ASCII";
  my $mkfconf_fits = $mkfconf;
  # Remove any extension specifications
  $mkfconf_fits =~ s/\[.*\]//g;

  # Read each column needed for the makefilter configuration ascii file
  my @incol      = ahgen::read_column($mkfconf_fits,"MKFCONF","INCOL");
  my @filecol    = ahgen::read_column($mkfconf_fits,"MKFCONF","FILE");
  my @extcol     = ahgen::read_column($mkfconf_fits,"MKFCONF","EXTENSION");
  my @interpcol  = ahgen::read_column($mkfconf_fits,"MKFCONF","INTERP");
  my @outcol     = ahgen::read_column($mkfconf_fits,"MKFCONF","OUTCOL");
  my @commentcol = ahgen::read_column($mkfconf_fits,"MKFCONF","COMMENT");

  # Check for errors
  if(!@incol) { ahlog::ah_err "Error reading INCOL column in $mkfconf"; return 1; }
  if(@incol != @filecol) { ahlog::ah_err "Error reading FILE column in $mkfconf"; return 1; }
  if(@incol != @extcol) { ahlog::ah_err "Error reading EXTENSION column in $mkfconf"; return 1; }
  if(@incol != @interpcol) { ahlog::ah_err "Error reading INTERP column in $mkfconf"; return 1; }
  if(@incol != @outcol) { ahlog::ah_err "Error reading OUTCOL column in $mkfconf"; return 1; }
  if(@incol != @commentcol) { ahlog::ah_err "Error reading COMMENT column in $mkfconf"; return 1; }

  # Write each row to an ascii file
  # Field 1: Column name
  # Field 2: Suffix of file containing the column
  # Field 3: Number or name of extension containing the column
  # Field 4: Interpolation method
  # Field 5: % (mandatory but unused)
  # Field 6: Name of column to fill in output filter file (%=same as input)
  # Anything after / is a comment for column (%=copy from input)
  open MKFCONF, ">", $mkfconf_ascii;
  foreach (my $ii = 0; $ii < @incol; $ii++) {
    #               INCOL       FILE          EXTENSION    INTERP            OUTCOL         COMMENT
    print MKFCONF "$incol[$ii] $filecol[$ii] $extcol[$ii] $interpcol[$ii] % $outcol[$ii] / $commentcol[$ii]\n";
  }
  close MKFCONF;

  # Set the new ascii file as the mkfconf
  $mkfconf = $mkfconf_ascii;

  return 0;

}

#
# Revision Log:
# $Log: ahfilter.pl,v $
# Revision 1.24  2016/12/01 20:50:49  mwitthoe
# ahfilter: fix bug where the output EHK file was being deleted when 1) the output EHK path contained more than one period and 2) the infileroot was specified (i.e. not an empty string)
#
# Revision 1.23  2016/02/22 15:10:46  asargent
# Added optaxis parameter for ahmkehk
#
# Revision 1.22  2016/02/19 00:44:20  klrutkow
# changed telescop to HITOMI
#
# Revision 1.21  2016/01/30 15:03:50  asargent
# Added keyword copying from attitude file to mkf file
#
# Revision 1.20  2016/01/19 14:26:58  asargent
# Fixed bug when reading columns from CALDB queried mkfconf file
#
# Revision 1.19  2016/01/12 16:09:17  asargent
# Fixed bug where ahfilter was deleting the ehk file when the infileroot parameter was set.
#
# Revision 1.18  2015/12/30 15:24:26  asargent
# makefilter does not return an error code...included fits file error checking after makefilter. Changed default orbext parameter
#
# Revision 1.17  2015/08/21 20:14:10  asargent
# Fixed caldb calling bug
#
# Revision 1.16  2015/08/13 14:37:19  asargent
# Fixed bug when getting the filename from call_quzcif
#
# Revision 1.15  2015/08/12 18:49:06  asargent
# Fixed return bug when creating ASCII file
#
# Revision 1.14  2015/08/11 17:55:05  asargent
# Added CALDB query for MKF configuration file
#
# Revision 1.13  2015/08/11 15:37:48  asargent
# Added new function to convert FITS config file to ASCII file for makefilter
#
# Revision 1.12  2015/08/05 18:28:18  asargent
# Minor updates and name changes to script and parameter
#
# Revision 1.11  2015/07/08 21:08:56  asargent
# Fixed typo, removed force_debug
#
# Revision 1.10  2015/06/30 17:40:40  asargent
# Updates to prologue and typo fixes.
#
# Revision 1.9  2015/06/29 15:42:51  asargent
# Minor tweaks and printout updates
#
# Revision 1.8  2015/06/17 14:25:15  asargent
# Added missing logging information
#
# Revision 1.7  2015/06/17 14:20:30  asargent
# Cleanup and added descriptions of tool and subroutines.
#
# revision 1.6
# date: 2015/06/15 20:50:41;  author: asargent;  state: Exp;  lines: +103 -4
# Added subroutine to fix TNULL keywords for certain FITS types.
#
# revision 1.5
# date: 2015/05/06 17:16:01;  author: asargent;  state: Exp;  lines: +13 -5
# New makefilter parameter infileroot added, verify output file from makefilter before any other processing
#
# revision 1.4
# date: 2015/05/05 17:08:43;  author: asargent;  state: Exp;  lines: +52 -62
# Added new parameters to correlate with ahmkehk parameters. Added start and end flags
#
# revision 1.3
# date: 2015/03/19 15:03:02;  author: asargent;  state: Exp;  lines: +12 -3
# Updated ahfilter due to ahmkehk changes
#
# revision 1.2
# date: 2015/03/10 21:18:27;  author: asargent;  state: Exp;  lines: +1 -1
# ahapp library updates
#
# revision 1.1
# date: 2015/03/03 18:17:26;  author: asargent;  state: Exp;
# Changed name from ahfilter to ahfilter.pl
#
