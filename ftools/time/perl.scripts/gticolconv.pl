#!/usr/bin/perl
#
# File name: gticolconv.pl
# Author: A. J. Sargent NASA GSFC
# $Date: 2016/08/19 17:15:08 $
# Version: 0
#
# gticolconv runs in one of two methods:
#   1. Split an input GTI file with START, STOP and a 'split' column
#      and create a GTI file with n-extensions (unique values of 'split' column)
#   2. Merge an input GTI file with n-extensions into a single extension
#      with a START, STOP and 'split' column
#
# Tool Dependencies:
# 
# Library Dependencies:
#   gen/lib/perl/ahlog
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
#
# Modification History:
#

# Set up
use strict;
use warnings;

use ahlog ;
use ahapp ;
use ahgen qw (:ALL) ;

# turn on AUTOFLUSH
$| = 1;

#########################
# Variable Definitions 
#########################

my $infile       ="";
my $outfile      ="";
my $column       ="";
my $direction    ="";
my $keyname      ="";
my $keyform      ="";

my $split        =0;
my $colnum       =0;
my $numhdu       =0;
my $keyroot      ="";
my $numdigits    =2;
my $extname      =0;
my @outext       =();
my %outext;
my %outnum;

my $gticolconverror = 0;  # gticolconv exit status

#########################
#  Main Code Block 
#########################

ahapp::startup () ;

ahapp::begin_processing () ;

$gticolconverror = get_parameters () ;
unless ( $gticolconverror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;  
  ahlog::ah_err "get_parameters" ;
  ahapp::end_processing($gticolconverror);
}

$gticolconverror = initialize () ;
unless ( $gticolconverror == 0 ) {
  ah_info "HIGH", ahapp::write_parameters () ;   
  ahlog::ah_err "initialize" ;
  ahapp::end_processing($gticolconverror);
}

$gticolconverror = do_work () ;
unless ( $gticolconverror == 0 ) {
  ahlog::ah_err "do_work" ;
  ahapp::end_processing($gticolconverror);
}

$gticolconverror = finalize () ;
unless ( $gticolconverror == 0 ) {
  ahlog::ah_err "finalize" ;
  ahapp::end_processing($gticolconverror);
}

# We're done.
ahapp::end_processing($gticolconverror);


#########################
# Subroutines
#########################

sub get_parameters {

  $infile       = ahapp::query_parameter("infile");
  $outfile      = ahapp::query_parameter("outfile");
  $column       = ahapp::query_parameter("column");
  $direction    = ahapp::query_parameter("direction");
  $keyname      = ahapp::query_parameter("keyname");
  $keyform      = ahapp::query_parameter("keyform");

  return 0;

}

# ------------------------------------------------------------------------------

sub initialize {

  my $status = 0;

  # Input file checking
  if (isRequiredFileNotFound($infile)) { return 1; }

  # Check if output files exist
  if (removeOutputFileIfClobbering($outfile,$ahapp::clobber)) { return 1; }

  # Count the number of digits in the keyform paramater (number of #'s)
  $numdigits = () = $keyform =~ /#/g;

  # Check that there are multiple #'s in keyform parameter
  if ( ! $numdigits ) {
    ahlog::ah_err "Missing ## from keyform parameter";
    return 1;
  }

  # Remove the #'s and create a keyword root
  $keyroot = $keyform;
  $keyroot =~ s/#//g;

  ahlog::ah_debug "keyform   = $keyform";
  ahlog::ah_debug "numdigits = $numdigits";
  ahlog::ah_debug "keyroot   = $keyroot";

  # Parse the input file name
  # The extension should only be specified if
  # 'split' mode is being run.
  my ( $filename, $ext ) = ahgen::parse_file_name($infile);
  $extname = $ext;
  $numhdu = get_total_hdu("$infile") - 1;
  ahlog::ah_out "Found $numhdu extensions.";

  # Check requirements for splitting or merging GTI file
  if ( uc $direction eq "SPLIT" ) {
    $split = 1;
    if ( ! $ext ) {
      ahlog::ah_out "No extension name given, searching for GTI extension";
      # Loop through the HDUs to find the first GTI extension
      foreach $ext ( 1 .. $numhdu ) {
        my $hduclas1 = ahgen::get_keyword($infile,$ext,"HDUCLAS1");
        ahlog::ah_debug "For '$infile+$ext', HDUCLAS1=$hduclas1";
        unless ( defined $hduclas1 ) {
          ahlog::ah_out "For '$infile+$ext', could not read keyword HDUCLAS1";
          next;
        }
        if ( uc $hduclas1 ne "GTI" ) {
          ahlog::ah_out "For file $infile HDUCLAS1(=$hduclas1) not GTI in extension $ext. Skipping.";
          next;
        }
        $extname = $ext;
        last;
      }
    }
    ahlog::ah_out "Using $filename\[$extname]";
    
    # Determine the column number for the input 'column' to split
    $colnum = ahgen::get_column_num($filename,$extname,$column);
    if ( $colnum == 0 ) {
      ahlog::ah_err "Could not find column $column in file $infile";
      return 1;
    }

    # Verify that the input column is an integer
    # If the input column is not an integer, quit.
    # We are only checking signed integers J, I or B
    my $tform = ahgen::get_keyword($filename,$extname,"TFORM$colnum");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "TFORM$colnum keyword not defined for column $column in $infile"; 
      return ahgen::get_error_flag;
    }
    ahlog::ah_debug "TFORM$colnum = $tform";
    if ( uc $tform !~ /I|J|B|1I|1J|1B/  ) {
         ahlog::ah_err "Invalid format for column $column. Must be an integer (TFORM$colnum = $tform)";
         return 1;
       }

  } elsif ( uc $direction eq "MERGE" ) {

    # Check the number of HDUs is greater than 2.
    # If user uses an extension specifier ([EXTNAME], [1] or +1)
    # Then function will return 1 HDU and tool will fail
    if ( $numhdu < 2 ) {
      ahlog::ah_err "Not enough HDUs to merge"; 
      return 1;
    }

    # Loop through the HDUs to verify that 'keyname' keyword exists
    my $count=0;
    foreach my $ext ( 1 .. $numhdu ) {
      # Skip any HDUs that are not GTI
      my $hduclas1 = ahgen::get_keyword($infile,$ext,"HDUCLAS1");
      unless ( defined $hduclas1 ) {
        ahlog::ah_out"For '$infile+$ext', could not read keyword HDUCLAS1";
        next;
      }
      if ( uc $hduclas1 ne "GTI" ) {
        ahlog::ah_out"For '$infile+$ext', HDUCLAS1=$hduclas1";
        next;
      }
      # Skip any HDUs that have zero rows
      my $nrows = ahgen::get_keyword($infile,$ext,"NAXIS2");
      unless ( defined $nrows ) {
        ahlog::ah_out"For '$infile+$ext', could not read keyword NAXIS2";
        next;
      }
      unless ( $nrows ) {
        ahlog::ah_out"For '$infile+$ext', NAXIS2=0";
        next;
      }
      my $extname = ahgen::get_keyword($infile,$ext,"EXTNAME");
      unless ( defined $extname ) {
        ahlog::ah_out"For '$infile+$ext', could not read keyword EXTNAME";
        next;
      }
      # Read the user input keyword to parse
      my $keyval = ahgen::get_keyword($infile,$ext,$keyname);
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "$keyname not defined for extension $ext in $infile"; 
        return ahgen::get_error_flag;
      }
      if ( $keyval !~ /$keyroot\d{$numdigits}/ ) {
          ahlog::ah_err "In file $infile+$ext, $keyname(=$keyval) is not a valid format for $keyform";
          next;
      } 
      $count+=1;
      # Parse the keyword and save the integer value and extension name
      $keyval =~ /$keyroot(\d{$numdigits})/;
      my $keynum = int($1);
      my $keyword = "$keyroot$1";
      ahlog::ah_debug "File $infile+$ext: keynum = $keynum, keyval = $keyword, extname = $extname"; 
      $outnum{$ext} = $keynum;
      $outext{$ext} = $extname;
      push @outext, $ext;
    }
    if ( $count == 0 ) {
      ahlog::ah_err "No valid extensions found for merging";
      return 1;
    }
  } else {
    ahlog::ah_err "Invalid direction. Use \"SPLIT\" or \"MERGE\"";
    return 1;
  }

  # Write all parameters to this script to the log file.
  ah_info "HIGH", ahapp::write_parameters () ;                 

  return $status;

}

# ------------------------------------------------------------------------------

sub do_work {

  my $status = 0;

  # Split or merge the GTI file
  if ( $split ) {
    my $numhdu = 0;
    my ( $filename ) = ahgen::parse_file_name($infile);
    my $tmp_infile_sorted = "tmp_infile_sorted.gti";
    ahapp::add_temp_file($tmp_infile_sorted);

    # Sort the input file based on the 'column' parameter and find the
    # unique values to loop over.
    $status = ahgen::run_ftool("ftsort","$filename\[$extname]",$tmp_infile_sorted,$column,"unique=yes","copyall=no");
    my @colval = ahgen::read_column($tmp_infile_sorted, 1, $column );

    # Loop over each 'column' value then extract rows with that value from 
    # original input file. Update several keywords in each extension
    # including the EXTNAME, DETNAM and HDUCLAS keywords
    foreach my $colval ( @colval ) {
      my $colvalue = sprintf("%0".$numdigits."d",$colval);
      my $tmp_outfile = lc "tmp_outfile_$keyroot$colvalue.gti";
      ahapp::add_temp_file($tmp_outfile);
      $numhdu += 1;
      
      # Extract the 'column' value and calculate the TSTART TSTOP keywords
      ahlog::ah_out "Extracting $column = $colval";
      $status = ahgen::copy_hdu($filename,"$extname][$column==$colval",$tmp_outfile,"START","STOP",$column);
      if ( $status ) { ahlog::ah_err "ftcopy failed."; return $status; }

      # Update the TSTART and TSTOP keyword values
      my $outfile_nrows = ahgen::get_keyword($tmp_outfile,1,"NAXIS2");
      if( ahgen::get_error_flag ) {
        ahlog::ah_err "NAXIS2 keyword not defined in $outfile"; return ahgen::get_error_flag;
      }
      my @startcol  = ahgen::read_column($tmp_outfile,1,"START",1);
      my @stopcol   = ahgen::read_column($tmp_outfile,1,"STOP",$outfile_nrows);
      if ( @startcol ) {
        my $tstart  = ( @startcol )[0];
        $status = ahgen::set_keyword($tmp_outfile,1,"TSTART",$tstart);
        if($status) { ahlog::ah_err "Error updating TSTART keyword for $extname"; return $status; }
      }
      if ( @stopcol ) {
        my $tstop   = ( @stopcol )[-1];
        $status = ahgen::set_keyword($tmp_outfile,1,"TSTOP",$tstop);
        if($status) { ahlog::ah_err "Error updating TSTOP keyword for $extname"; return $status; }
      }

      # append the temporary file to the output file
      $status = ahgen::copy_hdu($tmp_outfile,1,$outfile,"START","STOP");
      if ( $status ) { ahlog::ah_err "ftappend failed."; return $status; }

      # Write output keywords
      $status = ahgen::set_keyword($outfile,$numhdu,"EXTNAME","GTI$keyroot$colvalue");
      if ( $status ) { ahlog::ah_err "fthedit failed."; return $status; }
      $status = ahgen::set_keyword($outfile,$numhdu,$keyname,"$keyroot$colvalue");
      if ( $status ) { ahlog::ah_err "fthedit failed."; return $status; }
      $status = ahgen::set_keyword($outfile,$numhdu,"HDUCLASS","OGIP","format conforms to OGIP/GSFC standards");
      if ( $status ) { ahlog::ah_err "fthedit failed."; return $status; }
      $status = ahgen::set_keyword($outfile,$numhdu,"HDUCLAS1","GTI","Extension contains Good Time Intervals");
      if ( $status ) { ahlog::ah_err "fthedit failed."; return $status; }
      $status = ahgen::set_keyword($outfile,$numhdu,"HDUCLAS2","ALL");
      if ( $status ) { ahlog::ah_err "fthedit failed."; return $status; }
    }
  } else {
    # Create a temporary file list for ftmerge
    my $tmp_outfile = lc "tmp_outfile_$column.gti";
    ahapp::add_temp_file($tmp_outfile);
    my $tmp_filelist = "tmp_gtifiles.in";
    ahapp::add_temp_file($tmp_filelist);

    # Create an '@' file list to input into ftmerge
    open FILELIST, ">$tmp_filelist";
    foreach my $ext ( @outext ) {
       my $keynum  = $outnum{$ext};
       my $extname = $outext{$ext};
       ahlog::ah_out "Merging $column = $keynum from extension: $extname";
       print FILELIST "$infile\[$extname][col START;STOP;$column=$keynum]\n";
    }
    close FILELIST;
    $status = ahgen::run_ftool("ftmerge","@".$tmp_filelist,$tmp_outfile,"copyall=no");
    if ( $status ) { ahlog::ah_err "ftmerge failed."; return $status; }

    # calculate the TSTART TSTOP keywords
    my $outfile_nrows = ahgen::get_keyword($tmp_outfile,1,"NAXIS2");
    if( ahgen::get_error_flag ) {
      ahlog::ah_err "NAXIS2 keyword not defined in $outfile"; return ahgen::get_error_flag;
    }
    my @startcol  = ahgen::read_column($tmp_outfile,1,"START",1);
    my @stopcol   = ahgen::read_column($tmp_outfile,1,"STOP",$outfile_nrows);
    if ( @startcol ) {
      my $tstart  = ( @startcol )[0];
      $status = ahgen::set_keyword($tmp_outfile,1,"TSTART",$tstart);
      if($status) { ahlog::ah_err "Error updating TSTART keyword for $extname"; return $status; }
    }
    if ( @stopcol ) {
      my $tstop   = ( @stopcol )[-1];
      $status = ahgen::set_keyword($tmp_outfile,1,"TSTOP",$tstop);
      if($status) { ahlog::ah_err "Error updating TSTOP keyword for $extname"; return $status; }
    }

    # Sort on START, STOP then COLUMN
    $status = ahgen::run_ftool("ftsort","$tmp_outfile\[1]",$outfile,"START,STOP,$column");
    if ( $status ) { ahlog::ah_err "ftsort failed."; return $status; }

    # Update the EXTNAME, KEYNAME and HDUCLASS  keywords
    $status = ahgen::set_keyword($outfile,1,"EXTNAME","GTI$column");
    if ( $status ) { ahlog::ah_err "fthedit failed."; return $status; }
    $status = ahgen::set_keyword($outfile,1,$keyname,"$column");
    if ( $status ) { ahlog::ah_err "fthedit failed."; return $status; }
    $status = ahgen::set_keyword($outfile,1,"HDUCLASS","OGIP","format conforms to OGIP/GSFC standards");
    if ( $status ) { ahlog::ah_err "fthedit failed."; return $status; }
    $status = ahgen::set_keyword($outfile,1,"HDUCLAS1","GTI","Extension contains Good Time Intervals");
    if ( $status ) { ahlog::ah_err "fthedit failed."; return $status; }
    $status = ahgen::set_keyword($outfile,1,"HDUCLAS2","ALL");
    if ( $status ) { ahlog::ah_err "fthedit failed."; return $status; }
  }

  return $status;

}

# ------------------------------------------------------------------------------

sub finalize {

  my $status = 0;

  $status = update_checksum_and_verify($outfile);
  unless ( $status ) { ahlog::ah_err "verify failed for $outfile"; return $status; }

  return 0;

}

# $Log: gticolconv.pl,v $
# Revision 1.3  2016/08/19 17:15:08  asargent
# Removed sxs specific error message and column reading.
#
# Revision 1.2  2016/08/18 13:42:25  asargent
# Updated temporary file name to use 'column' parameter
#
# Revision 1.1  2016/08/17 18:59:22  asargent
# First version of gticolconv
#
