#!/usr/bin/perl
#-------------------------------------------------------------------------------
#
# File name: ahmodhkext.pl
#  
# Task name: ahmodhkext
#       
# Description:
# Merges the extensions of a FITS file together into a single extension
# 
# Author/Date: Kristin Rutkowski NASA GSFC / 20141031
#
# Tool Dependencies:
#   ftsort
#   ftcopy
# 
# Library Dependencies:
#   gen/lib/perl/ahlog
#   gen/lib/perl/ahapp
#   gen/lib/perl/ahgen
#
# Modification History: 
#
#  Ver   Date        Author  Description
#  1.0   2014-09-25   KLR    Initial implementation
#
#-----------------------------------------------------------

use strict;
use warnings;

use File::Copy "cp" ;

use ahlog ;
use ahgen qw (:ALL) ;
use ahapp qw (:ALL) ;

my $force_debug ;

# Set the force_debug flag to non-zero (preferably 1) to force ouput of debugging
# messages.
#$force_debug = 1;
$force_debug = '' ;

#########################
#  Main Code Block 
#########################

my $nargs = scalar @ARGV ;

my $infile  = "" ;
my $outfile = "" ;
my $kw = "" ;
my $colname = "" ;
my $inputext = "" ;
my $outputext = "" ;
my $valuecol = "" ;
my $cleanup = "" ;
my $sortcol = "" ;
my $tform = "" ;
my $deletekey = "" ;

# Query canonical APE parameters and start logging. 
ahapp::startup ($force_debug) ;

# Query non-canonical APE parameters.

$infile     = query_parameter ("infile") ;
$outfile    = query_parameter ("outfile") ;
$kw         = query_parameter ("keyword") ;
$colname    = query_parameter ("colname") ;
$inputext   = query_parameter ("inputext") ;
$outputext  = query_parameter ("outputext") ;
$valuecol   = query_parameter ("valuecol") ;
$tform      = query_parameter ("tform") ;
$sortcol    = query_parameter ("sortcol") ;
$cleanup    = query_parameter ("cleanup", 1) ;
$deletekey  = query_parameter ("deletekey", 1) ;

ah_debug "number of arguments $nargs" ;

my $msg ;
my $cmd ;
my $status ;
my @targetvals ;
my $usevalcol = 0 ;

# Write all parameters to this script to the log file.
ah_info "HIGH", write_parameters () ;

# Check if the parameter colname was passed to us.  If it was
# not, then use the value the parameter 'keyword' for this value.
$colname = $kw unless $colname ;

# Add error checking to make sure all parameters were found...

# Split the $inputext into a list of extensions to process
my @target = split (/,\W*/, $inputext) ;

# Check if a value for for valuecol was set.  If it is, then split it into
# a list of values to populate the column that we are adding to the merged
# extension.  In this case we will not use the value of the keyword.
if  ($valuecol)
    {
    @targetvals = split (/,\W*/, $valuecol) ;

    # Make sure that we have a value in @targetvals for every extension
    # specified in the parameter inputext.  Exit if this check fails.
    if  (scalar @targetvals != scalar @target)
        {

        ah_err "Number of values specified by the parameter valuecol does " . 
               "not match the number of HDUs being processed.  Exiting." ;

        exit (1) ;
        }

    # Check if the parameter 'inputext' was set to the asterisk '*' indicating
    # that the user wanted us to process all expressions.  This is incompatible
    # with using the parameter 'valuecol'
    if  ($target[0] =~ /\*/)
        {
        ah_err   "Values for the parameter 'valuecol' were specified but " . 
                 "the parameter 'inputext' was not set to a list of HDUs " .
                 "to process." ;
        
        ah_err   "The parameter 'inputext' was instead set the asterisk " .
                 "'*' indicating all extensions were to be processed." ;

        ah_err   "These settings are incompatible.  Exiting." ;

        exit (1) ;
        }
               
    $usevalcol = 1 ;
    }

# Get the total number of HDUs in the FITS file.
my $nhdu = get_total_hdu ($infile) ;

ah_info "HIGH", "Number HDUs : $nhdu" ;

my $clone ;
my $merge_self ;

# Check if the parameter 'outfile' was set.  If it was set (and it is not
# set to the same value as the parameter 'infile', then we will copy only 
# the primary HDU and the merged extension to this new file.  Otherwise, we
# will add the merged extension onto the end of the imput file.

# Check if the name of the input file is the same as the outpute file.  If it
# is then we will have to create a new name for the clone.

if  ($infile eq $outfile) 
    {
    # $clone = "tmp.fits" ;
    $merge_self = 1 ;
    }
else
    {
    # $clone = $outfile ;
    $merge_self = 0 ;
    }

$clone = "tmp.fits" ;

# Check if the output file already exists.  Unless clobber is set, this will
# cause the script to fail.
unless ($clobber) 
    {
    if  (-e $outfile) 
        {
        ah_err "Output file already exist but clobber was not set.  Exiting." ;

        exit (1) ;
        }
    }

# Remove a (possibly) pre-existing clone file.
unlink $clone ;

# Clone the input FITS file.
my $err = copy_fits_file ($infile, $clone) ;

my $kval ;
my $extname ;
my $exttype ;
my $target_index ;
my $tfilename ;
my $process_extension ;
my $nhdu_in_temp ;
my @ext ;

# Loop through and process all the HDUs in the FITS file except the first.
for ( my $i = 1 ; $i < $nhdu ; $i++) 
    {
    # Get the name of the extension of the current HDU.
    $extname = get_keyword ($clone, $i, "EXTNAME") ;

    # Check if the extension name matches any on the list of extensions that we
    # we are to process.
    $process_extension = 0 ;

    for ($target_index = 0 ; $target_index < scalar @target ; $target_index++) 
        {
	if  ($target[$target_index] =~ /\*/ || $target[$target_index] =~ /$extname/) 
            {
            $process_extension = 1 ;

	    last ;
	    }
	}

    next unless $process_extension ;

    # Determine if the current HDU is a binary table or an image.
    $exttype = get_keyword ($clone, $i, "XTENSION") ;

    # We can only process binary tables.
    unless ( $exttype =~ /BINTABLE/ )
        {
        ah_err "Tried to process extension $extname, but it is not a binary table."  ;
        ah_err "Exiting." ;

        exit (1) ;
        } 

    # Determine the value that we use to populate the new that we are creating.
    # First check if the flag $usevalcol is set.  If it is, then we will get
    # this value from 
    if  ($usevalcol)
        {
        $kval = $targetvals [$target_index] ;
        }
    else
        {
        # Otherwise, get the value of the keyword from the current HDU.
        $kval = get_keyword ($clone, $i, $kw) ;

        unless (defined $kval)
            {
            ah_warn 'HIGH', "Keyword $kw does not exist in extension $extname" ;

            next ;
            }
        }

    ah_info "HIGH", "Processing extension : $extname" ;
    ah_debug "Keword : $kw Value : $kval" ;

    # Remove the keyword from the extension that we are processing.
    delete_keyword ($clone, $i, $kw) if $deletekey ;

    # Add a column with column name set to the keyword name and its
    # value set to the old keyword value.
    $status = add_column ($clone, $i, $colname, $kval, $tform) ;

    if  ($status)
        {
        ah_err "Could not add column to file $clone." ;

        ah_err get_tool_stderr () ;

        ah_err "Exiting." ;
         
        exit 1 ;
        }
	 
    # Add the extension that we are processing to array of extensions that have
    # been processed.
    push (@ext, $i) ;
    }

# Find that number of extensions that we are going to process/merge.
my $n_ext = scalar @ext ; 

# Check to make their is at least extension that we are to process.
unless ($n_ext > 0)
    {
    ah_info "HIGH", "No extensions were found.  Exiting." ;

    exit (1) ;
    }

# Create a copy of the clone file so thtat we can append a new HDU to it.
# my $tmpfile = "tmp" ;
my $tmpfile = "tmpxx.fits" ;

# my @tnow = localtime () ;

# for (my $i = 0 ; $i < 5 ; $tmpfile = $tmpfile . $tnow[$i++]) {}

ah_debug "Using temporary file: $tmpfile" ;

# Remove a (possibly) pre-existing clone file.
unlink $tmpfile ;

# Copy the FITS file.
if  ($merge_self)
    {
    copy_fits_file ($clone, $tmpfile) ;

    $nhdu_in_temp = $nhdu ;
    }
else
    {
    copy_hdu ($clone, 0, $tmpfile) ;

    $nhdu_in_temp = 1 ;
    }


# Append the first HDU which we processed to the end of the temporary FITS
# file we created.
copy_hdu ($clone, $ext[0], $tmpfile) ;

# Change the name of the HDU we just created.  Note: since we added the new
# extension on the end of the FITS file, its extension number will be same
# as the previous number of extensions.
set_keyword ($tmpfile, $nhdu_in_temp, "EXTNAME", $outputext) ;

# Merge the temporary FITS files into the final output file.

# Only do this if we are processing more then one extension.
if  ($n_ext > 1)
    {
    my @plist = ($tmpfile, $nhdu_in_temp) ;

    for (my $i = 1 ; $i < $n_ext ; $i++) 
        {
        $plist[$i * 2]     =  $clone ;
        $plist[$i * 2 + 1] =  $ext[$i] ;
        }



    $status = merge_fits_file ($tmpfile, $merge_self, $n_ext, @plist) ;

    if  ($status)
        {
        ah_err "Errors detected trying to merge FITS file(s)." ;

        ah_err get_tool_stderr () ;

        ah_err "Exiting." ;
         
        exit 1 ;
        }

    ah_info "HIGH", "Merged FITS extensions " . join (", ", @ext) ; 
    }

# Sort the merged FITS file on the column sortcol.
$status = sort_fits_file ($tmpfile, $nhdu_in_temp, $tmpfile, $sortcol) ;

if  ($status)
    {
    ah_err "Errors detected trying to sort FITS file(s)." ;

    ah_err get_tool_stderr () ;

    ah_err "Exiting." ;
         
    exit 1 ;
    }

ah_info "HIGH", "Sorted extension $nhdu_in_temp on keyword $sortcol" ;

# Add parameters to ahfilecaldb as HISTORY keywords to extension
if  ($history)
    {   
    write_parameters ($tmpfile, $nhdu_in_temp) ;
    }

# Rename the temp file to the clone file, if the remove temporary files option is
# set.
if  ($cleanup)
     { 
     rename $tmpfile, $clone ;

     ah_debug "Removed temporary file: $tmpfile" ;

     rename $clone, $outfile ;

     ah_debug "Removed temporary file: $clone" ;
     }
else
     {
     cp $tmpfile, $outfile ;
     }

# Turn off logging and do any cleanup from the ahapp Perl module.
ahapp::shutdown () ;

# Were done.
exit (0) ;




# $Log: ahmodhkext.pl,v $
# Revision 1.6  2016/03/22 21:56:51  klrutkow
# per issue 610: add prologue to top of file ; added CVS log to bottom of file
#
#





