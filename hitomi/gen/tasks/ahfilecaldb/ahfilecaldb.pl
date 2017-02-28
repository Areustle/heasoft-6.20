#!/usr/bin/perl
use strict;
use warnings;

use File::Temp qw/ :mktemp / ;

use ahlog ;
use ahgen qw (:ALL) ;
use ahapp qw (:ALL) ;

my $force_debug ;

# Set the force_debug flag to non-zero (preferably 1) to force ouput of debugging
# messages.
#$force_debug = 1;
#$force_debug = '' ;
$force_debug = '' ;

#########################
#  Main Code Block 
#########################

# Query canonical APE parameters and start logging. 
ahapp::startup ($force_debug) ;

my $infile = "" ;       # name of input file
my $outfile = "" ;      # name of output file

# Query non-canonical APE parameters.
$infile     = query_parameter ("infile") ;
$outfile    = query_parameter ("outfile") ;
 
my $status ;
my @list ;

# Write all parameters to this script to the log file.
ah_info "HIGH", write_parameters () ;                 

# Find the number of arguements passed to this program
my $nargs = scalar @ARGV ;

# A sample debug message
ah_debug "number of arguements $nargs" ;

# Check if the output file already exists.  Unless clobber is set, this will
# cause the script to fail.
unless ($clobber) 
    {
    if  (-e $outfile) 
        {
        ah_err "Output file already exist but clobber was not set.  Exiting." ;

        exit 1 ;
        }
    }

# If $outfile already exists, delete it.  We have already checked that clobber
# has been set.
unlink $outfile if -e $outfile ;

# Open the input file and all the lines in the log file.
# Make sure that the requested file exists.
unless (-e $infile)
   {
   ah_err "Could not find file $infile. File does not exist." ;
        
   exit 1 ;
   }

# Try to open the file and read its contents.
$status = open (FH, "<", $infile) ;

 unless  ($status) 
    {
    ah_err "Could not open file $infile.  Exiting." ;
    ah_err $! ;

    exit 1 ;
    }

# Process each entry in the file.
while (<FH>)
    {
    chomp ;

    # Check for black lines
    next if /^\s*$/ ;

    # Check for comment lines
    next if /^#/ ;

    @list = (@list, $_) ;
    } 

close FH ;    

my @field ;
my $data ;
my $template ;
my $header ;
my $ext = 0 ;
my $extname ;
my $tmpname ;

# Process each line in the list. 

foreach (@list)  
    {

    # We will need to extract the name of the of the data file, the name of the
    # fits table template, and the name of the file containing the the list of 
    # keywords to add to the extension.
    @field = split ;

    $data     = $field[0] ;
    $template = $field[1] ;
    $header   = $field[2] ;    
    
    ah_info "HIGH", "data :      $data" ;
    ah_info "HIGH", "template :  $template" ;
    ah_info "HIGH", "header :    $header" ;

    # Make sure each file exists
    unless (-e $data)
        { 
        ah_err "Could not locate data file $data. Exiting." ;

        exit 1 ;
        }

    unless (-e $template)
        { 
        ah_err "Could not locate column description file $template. Exiting." ;

        exit 1 ;
        }

    unless (-e $header)
        { 
        ah_err "Could not locate file containing header keywords $header. " .
               "Exiting." ;

        exit 1 ;
        } 

    # Add the new extension to the output file.

    # First determine which extension we are creating.  The first extension
    # can be be done simply.  Subsequent extensions will require more work.
    if  ($ext)
        {
        # Second (or later) extension.

        # First we need to convert the ASCII data table to a FITS file.  Since
        # we need to use the tool FTCREATE to do this, and this tool can not
        # add an extension to an already existing FITS file, we will need to
        # create a temporary file in order to do this.

        # Create the tempory file name.
        # $tmpname = mktemp ("tmpXXXXXXXXXXX") . ".fits" ;
        $tmpname = "tmp.fits" ;

        # Create a new FITS file using the ASCII data table.
        $status = run_ftool ("ftcreate", 
                             $template, 
                             $data, 
                             $tmpname, 
                             "headfile=$header") ;

        # Check for error when running the ftool.  If so, print out some error 
        # messages and quit.
        if  ($status)
            {
            ah_err "Errors detected trying to run FTOOL: ftcreate to create " .
                   "FITS file." ;

            ah_err get_tool_stderr () ;

            ah_err "Exiting." ;
         
            exit 1 ;
            }
        
        # Append the FITS table we just created to the output file.
        $status = run_ftool ("ftappend", $tmpname, $outfile) ;

        # Check for error when running the ftool.  If so, print out some error 
        # messages and quit.
        if  ($status)
            {
            ah_err "Errors detected trying to run FTOOL: ftappend. Could not" .
                   " copy extension to file: $outfile." ;

            ah_err get_tool_stderr () ;

            ah_err "Exiting." ;
         
            exit 1 ;
            }
 
        # Remove the temporary FITS file.
        unlink  $tmpname
        }

    else
        {
        # Initial extension.  

        # We can just create the new FITS file containing this extension.
        $status = run_ftool ("ftcreate", 
                             $template, 
                             $data, 
                             $outfile, 
                             "headfile=$header") ;

        # Check for error when running the ftool.  If so, print out some error 
        # messages and quit.
        if  ($status)
            {
            ah_err "Errors detected trying to run FTOOL: ftcreate to create " .
                   "FITS file." ;

            ah_err get_tool_stderr () ;

            ah_err "Exiting." ;
         
            exit 1 ;
            }
        }

    # Increment the extension number to point to the extension we just added.
    $ext++ ;

    # Remove history keywords.
    $status = delete_history ($outfile, $ext) ;

    # Check for error when running the ftool.  If so, print out some error 
    # messages and quit.
    if  ($status)
        {
        ah_err "Failed to delete HISTORY keywords from extension $ext of " .
                "FITS file $outfile." ;

        ah_err get_tool_stderr () ;

        ah_err "Exiting." ;
         
        exit 1 ;
        }

    # Get the name of the extension if available.
    $extname = get_keyword ($outfile, $ext, "EXTNAME") ;
        
    ah_info "HIGH", "Successfully created extension: $extname" ;    

    # Add parameters to ahfilecaldb as HISTORY keywords to extension
    if  ($history)
        {   
        write_parameters ($outfile, $ext) ;
        }
    # Finished processing extension.  
    }

# Update the checksums in each extension.
$status = run_ftool ("ftchecksum", $outfile, "update=yes") ;

# Check for error when running the ftool.  If so, print out some error 
# messages and quit.
if  ($status)
    {
    ah_err "Could not update checksum in FITS file: $outfile." ;

    ah_err get_tool_stderr () ;

    ah_err "Exiting." ;
         
    exit 1 ;
    }

# Check that the FITS file we just created is valid.
unless (check_fits_file ($outfile))
    {
    ah_err "FITS file: $outfile. failed FITS verification test." ;

    exit 1 ;
    }

ah_info "HIGH", "Successfully created FITS file: $outfile" ;    

# Turn off logging and do any cleanup from the ahapp Perl module.
ahapp::shutdown () ;

# Were done.
exit (0) ;


# === Messages below refer to cvs log of old file: ahfilecaldb (without .pl extension) ===
# 
# revision 1.2
# date: 2013/08/07 20:35:02;  author: ryurow;  state: Exp;  lines: +2 -1
# Update to ahfilecaldb.
# ----------------------------
# revision 1.1
# date: 2013/07/25 22:06:24;  author: ryurow;  state: Exp;
# Added Perl tool ahfilecaldb to repository
