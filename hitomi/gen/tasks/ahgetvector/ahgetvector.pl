#!/usr/bin/perl
#
# File name: ahgetvector.pl
#  
# Task name: ahgetvector.pl
#       
# Description:
# Extract vectors from a FITS file and output in an ASCII file

#-----------------------------------------------------------

use strict;
use warnings;

use Scalar::Util qw (looks_like_number) ;

use ahlog ;
use ahgen qw (:ALL) ;
use ahapp qw (:ALL) ;

# Constants

use constant BUFSIZE => 100 ;

my $force_debug ;

# Set the force_debug flag to non-zero (preferably 1) to force ouput of debugging
# messages.
#$force_debug = 1;
$force_debug = '' ;

#########################
#  Main Code Block 
#########################

my $nargs = scalar @ARGV ;

my $infile    = "" ;
my $outfile   = "" ;
my $selmode   = "" ;
my $xcol      = "" ;
my $ycol      = "" ;
my $row       = "" ;
my $element   = "" ;

# Query canonical APE parameters and start logging. 
ahapp::startup ($force_debug) ;

# Query mandatory non-canonical APE parameters.

$infile        = query_parameter ("infile") ;
$outfile       = query_parameter ("outfile") ;
$selmode       = query_parameter ("selmode") ;
$xcol          = query_parameter ("xcol") ;
$ycol          = query_parameter ("ycol") ;

# Make sure mode is numeric

unless (looks_like_number ($selmode))
    {
    ah_err "Invalid value found for parameter selmode." ;  
    ah_err "Parameter must be an integer." ;

    exit 1 ; 
    }

# Query mode specific non-canonical APE parameters.
# +++ TODO add message to user like "you're about to retrieve the reflectivity 
# for all the energies at the 500th angle."
if  ($selmode == 1)
    {
    $element   = query_parameter ("element") ;
    #ah_info "LOW", "Retrieving every $element element of the $ycol column for every $xcol.";
    }

elsif  ($selmode == 2)
    {
    $row       = query_parameter ("row") ;
    #ah_info "LOW", "Retrieving the $ycol in row $row for every $xcol.";
    }

else
    {
    ah_err "Invalid value found for parameter selmode." ;
    ah_err "Invalid mode." ;

    exit 1 ; 
    }

ah_debug "Number of arguments: $nargs" ;

my $msg ;
my $cmd ;
my $status ;

# Write all parameters to this script to the log file.
ah_info "HIGH", write_parameters () ;

# +++ TODO Add error checking to make sure all parameters were found...

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

# Covert debug flag into a yes/no boolean string required by some FTOOLS.
my $debug_bool_flag = ($debug)? "yes" : "no" ;

# Parse infile to get the input file name and extension ;
my ($input_file_name, $input_file_ext, $trash) = parse_file_name ($infile) ;

# Check to make sure the input files exist
unless (-e $input_file_name)
    { 
    ah_err "Could not locate input file $input_file_name. Exiting." ;

    exit 1 ;
    }

# Check if user requested a specific HDU.  If the user did not request a specific
# HDU then force it to 1.  This might eventually be changed to the last non-gti
# HDU
$input_file_ext = 1 unless $input_file_ext ;

# Get the number HDUs in the input file.
my $nhdu = get_total_hdu ($input_file_name) ;

ah_debug "Number of HDUs: $nhdu" ;

# Check to make sure that the requested HDU actually exists
unless ($input_file_ext < $nhdu)
    { 
    ah_err "Request for HDU: $input_file_ext that does not exist in FITS file. Exiting." ;

    exit 1 ;
    }

# Get a list column names.
my @cols = get_column_names ($input_file_name, $input_file_ext) ;

my $found ;

$found = 0 ;

# Check to make that the xcol actually exist in the HDU
foreach (@cols)
    {
    $found = 1 if uc $_ eq uc $xcol ;    
    }

unless ($found)
    {
    ah_err "No Column named: $xcol in FITS file. Exiting." ;

    exit 1 ;
    }

$found = 0 ;

# Check to make that the ycol actually exist in the HDU
foreach (@cols)
    {
    $found = 1 if uc $_ eq uc $ycol ;    
    }

unless ($found)
    {
    ah_err "No Column named: $ycol in FITS file. Exiting." ;

    exit 1 ;
    }

# Find the number of rows in the HDU that we need to process.  This is only 
# really important for mode 1.
my $nrows = get_keyword ($input_file_name, $input_file_ext, "NAXIS2") ;

# Check to make sure that there is at least one row in the table.
unless ($nrows > 0)
    { 
    ah_err "HDU $input_file_ext does not contain any data. Exiting." ;

    exit 1 ;
    }

ah_debug "Number of rows of data: $nrows" ;

# Check if the user has requested that the output go to STDOUT or a file.
# If the outpout is to go to file, then we will have create the file and
# select it.
unless ($outfile =~ /STDOUT/i)
    {
    if  ( ! open (OUT, '>', $outfile) )
        {
        ah_err "Failed to open file: $outfile", $! ;

        exit 1 ;
        }

    select (OUT) ;
    }

my @xcolbuffer ;
my @ycolbuffer ;
my $count ;
my $row_start ;
my $row_end ;

# Process the FITS file based on mode.
if  ($selmode == 1)
    {
    ah_debug "Selection mode = 1" ;

    # +++ TODO make sure 'element' param doesn't exceed number of elements 
    # (which was found in selmode=2 below - move it up)

    # Write the correct column headers.
    printf "%-11s %-11s\n", $xcol, $ycol ;

    $count = 1 ;

    while ($count < $nrows) 
          {
          $row_start = $count ;
          $row_end = $count + BUFSIZE ;
          
          # Limit $row_end to the last row if $row_end exceeds the table size.
          $row_end = $nrows if  ($row_end > $nrows) ;

          # xcol should not be an array, but there is no way to check 
          # for that at the moment. 
          # +++ TODO this can be checked by inspecting TFORM, as in selmode=2 below
          @xcolbuffer = read_column ($input_file_name, $input_file_ext, $xcol, $row_start, $row_end) ;

          @ycolbuffer = read_column_vector ($input_file_name, $input_file_ext, $ycol, $element, $row_start, $row_end) ;

          for (my $i = 0 ; $i <  scalar @xcolbuffer; $i++)
              {
              printf  "%-11s %-11s\n", $xcolbuffer [$i], $ycolbuffer [$i] ;
              }

          $count = $row_end + 1 ;
          }
    }

else
    {
    ah_debug "Selection mode = 2" ;

    # get the column number of xcol, to form TFORM name
    my $colNum = get_column_num ($input_file_name, $input_file_ext, $xcol) ;
    my $tformName = "TFORM$colNum";

    ah_debug "Column number of $xcol: $colNum";

    # get the vector size from xcol TFORM keyword (ycol should be the same)
    my $numElements = get_keyword ($input_file_name, $input_file_ext, $tformName);
    # remove any characters, keeping just numerical part of TFORM keyword
    $numElements =~ s/[a-zA-Z]//;

    ah_debug "Number of elements in $xcol column: $numElements";

    # +++ TODO make sure 'row' param doesn't exceed numrows

    # Write the correct column headers.
    printf "%-11s %-11s\n", $xcol, $ycol ;

    my $elementStart;
    my $elementEnd;
    my $elementRange;

    $count = 1 ;
    while ($count <= $numElements) 
          {
          $elementStart = $count ;
          $elementEnd = $count + BUFSIZE ;
          
          # Limit $elementEnd to last element if $elementEnd exceeds vector size
          $elementEnd = $numElements if ($elementEnd > $numElements) ;
 
          $elementRange = "$elementStart-$elementEnd";

          @xcolbuffer = read_column_vector ($input_file_name, $input_file_ext, $xcol, $elementRange, $row, $row) ;

          @ycolbuffer = read_column_vector ($input_file_name, $input_file_ext, $ycol, $elementRange, $row, $row) ;

          for (my $i = 0 ; $i <  scalar @xcolbuffer; $i++)
              {
              printf  "%-11s %-11s\n", $xcolbuffer [$i], $ycolbuffer [$i] ;
              }

          $count = $elementEnd + 1 ;
          }
    }

# Turn off logging and do any cleanup from the ahapp Perl module.
ahapp::shutdown () ;

# Were done.
exit (0) ;



# === Messages below refer to old file: ahgetvector (without .pl extension) ===
# 
# revision 1.4
# date: 2015/01/05 18:36:55;  author: mwitthoe;  state: Exp;  lines: +16 -16
# ahgetvector: parameter changes; see issue 472
# ----------------------------
# revision 1.3
# date: 2014/10/31 03:55:57;  author: klrutkow;  state: Exp;  lines: +18 -18
# change parameters/variables 'columnsx' 'columnsy' and 'rows' to 'columnx' and 'columny' and 'row'
# ----------------------------
# revision 1.2
# date: 2014/02/10 17:20:55;  author: ryurow;  state: Exp;  lines: +4 -6
# improved formating of output from ahgetvector
# ----------------------------
# revision 1.1
# date: 2014/01/22 21:21:13;  author: ryurow;  state: Exp;
# Added initial version of ahgetvector to repository
