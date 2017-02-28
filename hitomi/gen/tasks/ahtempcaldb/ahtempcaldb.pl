#!/usr/bin/perl
use strict;
use warnings;

use File::Spec ;

use ahlog ;
use ahgen qw (:ALL) ;
use ahapp qw (:ALL) ;
use ahfilterlib qw (:ALL) ;

my $force_debug ;

# Set the force_debug flag to non-zero (preferably 1) to force ouput of debugging
# messages.
#$force_debug = 1;
$force_debug = '' ;


sub read_directory_files 

{

my $dir = shift ;

# Open the in directory for reading.
my $status = opendir (INDIR, $dir) ;

unless  ($status) 
    {
    ah_err "Could not open directory $dir to read files.  Exiting." ;
    ah_err $! ;

    exit 1 ;
    }

# Read the names of all the files in the directory.
my @contents = readdir (INDIR) ;

# Close the directory
closedir (INDIR) ;

# Sort the contents of the directory.
# my @filelist = sort @contents  # Sort in ascending order ;
my @filelist = sort {$b cmp $a} @contents ;

# Add the directory to the file to make a complete path.
foreach (@filelist)
    {
    $_ = File::Spec->catfile ( $dir, $_ ) ;      
    }

return (@_, @filelist) ;
}


sub merge_input_data

{
my $fn              = shift ;
my $target          = shift ;
my $ext_name        = shift ;
my $nhdu            = shift ;

my $status = 0;

ah_info "LOW", "Using file: $fn as basis for merge file: $target" ;

# copy HDU if first HDU of type, otherwise merge HDUs
if  ($nhdu == 1)
    {
    $status = copy_hdu ($fn, $ext_name, $target) ;
    if  ($status)
        {
        ah_err (get_tool_stderr ()) ;

        return $status ;
        }
    }
else
    {
    $status = merge_fits_file ($target, 
                               1,
                               2,
                               $target,  
                               $ext_name,
                               $fn, 
                               $ext_name) ;
    
    if  ($status)
        {
        ah_error (get_tool_stderr ()) ;
    
        return $status ;
        } 
    }

    return 0;
}

#########################
#  Main Code Block 
#########################


# Query canonical APE parameters and start logging. 
ahapp::startup ($force_debug) ;

# Declare variables to hold non-canonical APE parameters

my $indir ;         # name of input directorie(s)
my $filepattern ;   # name ofinput quartz trend file
my $outmerge ;      # Merge HK file with frequencies and temperatures
my $outfile  ;      # name of output file
my $outtemp ;       # file name to dump input frequencies and temperatures
my $l32ticol ;      # column in quartz ext with L32TI
my $quartzext ;     # name of extension containing quartz clock count
my $tempext ;       # name of extension containing temperature
my $quartzcol ;     # column with quartz clock count
my $u32ticol ;      # column in quartz ext of quartz acquisition time
my $tempcol ;       # column containing quartz temperature in Celcius
my $stimecol ;      # column containing S_TIME
my $leapsecfile ;   # Input leap second file (or CALDB/REFDATA)
my $frqtemfile ;    # Existing freq vs. temp CALDB file
my $tempresol ;     # temperature resolution where measurements are averaged
my $stimemax ;      # maximum value of delta S_TIME
my $averagemode ;   # mode to average frequency: 1) simple average, 2) TBD

# Other variables

my $status = 0 ;
my @list ;
my $count = 0 ;
my $non_fits_count = 0 ;
my $ntemphdu = 0;         # number of temperature extensions used
my $nquartzhdu = 0;       # number of quartz clock extensions used

# Query non-canonical APE parameters.

$indir        = query_parameter ("indir") ;
$filepattern  = query_parameter ("filepattern") ;
$outmerge     = query_parameter ("outmerge") ;
$outfile      = query_parameter ("outfile")  ;
$outtemp      = query_parameter ("outtemp") ;
$l32ticol     = query_parameter ("l32ticol") ;
$quartzext    = query_parameter ("quartzext") ;
$tempext      = query_parameter ("tempext") ;
$quartzcol    = query_parameter ("quartzcol") ;
$u32ticol     = query_parameter ("u32ticol") ;
$tempcol      = query_parameter ("tempcol") ;
$stimecol     = query_parameter ("stimecol") ;
$leapsecfile  = query_parameter ("leapsecfile") ;
$frqtemfile   = query_parameter ("frqtemfile") ;
$tempresol    = query_parameter ("tempresol") ;
$stimemax     = query_parameter ("stimemax") ;
$averagemode  = query_parameter ("averagemode") ;

# Write all parameters to this script to the log file.
ah_info "HIGH", write_parameters () ;

# Add error checking to make sure all parameters were found...

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

# Check if the file specified by outmerge already exists (from a previous run).
# If it does, then delete it.
if  (-e $outmerge) 
    {
    ah_info "HIGH", "Deleting temporary file: $outmerge" ;

    unlink $outmerge ;
    }
     
# Get the directories and search all the files with specific pattern within
# the directories.  The indir parameter can be @filename where filename 
# contains a list of directories

# Check if indir referes file (hopefully containing a list of directories to 
# to read) or if it contains the path of a single directory to scan.  If the 
# indir parameter begins with '@' character then it is file.
if  ($indir =~ /^@(\S*)/)
    {
    # Make sure we got a file name.
    unless ($1)
        {
        ah_err "Could not read file name from parameter indir.  Exiting." ;
        
        exit 1 ;
        }

    my $fn = $1 ;

    # Make sure that the requested file exists.
    unless (-e $fn)
        {
        ah_err "Could not find file $fn. File does not exist." ;
        
        exit 1 ;
        }

    # Try to open the file and read its contents.
    $status = open (FH, "<", $fn) ;

    unless  ($status) 
        {
        ah_err "Could not open file $fn.  Exiting." ;
        ah_err $! ;

        exit 1 ;
        }

    # Process each entry in the file.
    while (<FH>)
        {
        chomp ;

        @list =  read_directory_files ($_, @list) ;
        } 

    close FH ;    
    }

else
    {
    @list =  read_directory_files ($indir) ;
    }
     
# Modify $filepattern so that we don't have to worry about special symbols.
$filepattern = quotemeta ($filepattern) ;
 
# Start the merging.

# Process every file in the list
foreach my $file (@list)
    {
    # Weed out entries equivalent to '.' and '..'
    next if  ($file =~ /\/\.$|^\/\.\.$/) ;

    # Weed out directories
    next if (-d $file) ;

    # Weed out file names that don't contain the pattern $filepattern
    next unless ($file =~ /$filepattern/) ;

    # Weed out non-FITS files.

    # Run the ftverify tool to verify the FITS file.
    unless ( check_fits_file ($file) )
        {
        # Not a FITS file.  Tell the user the bad news.
        ah_info "HIGH", "file: $file is not a FITS file.  Processing aborted." ;

        # Keep count of the number of non-FITS files we find.
        $non_fits_count++ ;

        next ; 
        }            

    # Add/merge quartz clock HDU into output file
    if (check_hdu_exists ($file, $quartzext)) {
      $nquartzhdu++;
      $status = merge_input_data($file, $outmerge, $quartzext, $nquartzhdu);
      if  ($status)
          {
          ah_err ("Error encountered while adding/merging quartz clock HDU.  " .
                  "Exiting." 
                  ) ;

          exit 1 ;
          }
    }

    # Add/merge temperature HDU into output file
    if (check_hdu_exists ($file, $tempext)) {
      $ntemphdu++;
      $status = merge_input_data($file, $outmerge, $tempext, $ntemphdu);
      if  ($status)
          {
          ah_err ("Error encountered while adding/merging temperature HDU.  " .
                  "Exiting." 
                  ) ;

          exit 1 ;
          }
    }

    # Increment count 
    $count++ ;
    }

# Write to the screen the total number of files
ah_info "LOW", "Processed $count files." ;
ah_info "LOW", "Processed $nquartzhdu quartz clock HDUs." ;
ah_info "LOW", "Processed $ntemphdu temperature HDUs." ;

# Sort both the HK_TIMING and HK_HCE extensions.

# Run fsort  infile=outmerge columns=S_TIME method=heap unique=yes 
$status = sort_fits_file ($outmerge, $quartzext, $outmerge, "S_TIME", 0) ;
        
if  ($status)
    {
    ah_err ("Error encountered while running function <sort_fits_file>. " . 
            "Exiting." 
            ) ;

    exit 1 ;
    }

# Run fsort  infile=outmerge columns=S_TIME method=heap unique=yes 
$status = sort_fits_file ($outmerge, $tempext, $outmerge, "S_TIME", 0) ;
        
if  ($status)
    {
    ah_err ("Error encountered while running function <sort_fits_file>. " . 
            "Exiting." 
            ) ;

    exit 1 ;
    }

# Set TSTART and TSTOP (and DATE-END/DATE-END) based on S_TIME
my @tstart ;
my @tstop  ;
my $last_element ;
my $cmd ;
my $utcepoch ;
my $date_obs ;
my $date_end ;


# Process the temperature HDU (HK_TIMING)

# Get TSTART.
@tstart = read_column ($outmerge, $quartzext, "S_TIME", 1) ;

# Get TSTOP.
$last_element = get_keyword ($outmerge, $quartzext, "NAXIS2") ;
@tstop        = read_column ($outmerge, $quartzext, "S_TIME", $last_element) ;

ah_debug "Keyword TSTART set to $tstart[0]" ;
ah_debug "Keyword TSTOP  set to $tstop[0]" ;

# Update TSTART and TSTOP
set_keyword ($outmerge, $quartzext, "TSTART", $tstart[0]) ;
set_keyword ($outmerge, $quartzext, "TSTOP", $tstop[0]) ;

# Read MJDREFI to use as UTC epoch when computing DATE-END/END
$utcepoch=get_keyword($outmerge,$quartzext,"MJDREFI");

# Compute and set DATE-END keyword using ahtimeconv
$date_obs = ahfilterlib::convert_seconds_to_datetime($tstart[0],$utcepoch,"m");
if (!defined $date_obs) {
  ah_warn("Unable to set DATE-END keyword from TSTART=${tstart[0]}");
} else {
  set_keyword($outmerge,$quartzext,"DATE-END",$date_obs);
}

# Compute and set DATE-END keyword using ahtimeconv
$date_end = ahfilterlib::convert_seconds_to_datetime($tstop[0],$utcepoch,"m");
if (!defined $date_end) {
  ah_warn("Unable to set DATE-END keyword from TSTART=${tstop[0]}");
} else {
  set_keyword($outmerge,$quartzext,"DATE-END",$date_end);
}


# Process the temperature HDU (HK_HCE)

# Get TSTART.
@tstart = read_column ($outmerge, $tempext, "S_TIME", 1) ;

# Get TSTOP.
$last_element = get_keyword ($outmerge, $tempext, "NAXIS2") ;
@tstop        = read_column ($outmerge, $tempext, "S_TIME", $last_element) ;

ah_debug "Keyword TSTART set to $tstart[0]" ;
ah_debug "Keyword TSTOP  set to $tstop[0]" ;

# Update TSTART and TSTOP
set_keyword ($outmerge, $tempext, "TSTART", $tstart[0]) ;
set_keyword ($outmerge, $tempext, "TSTOP", $tstop[0]) ;

# Read MJDREFI to use as UTC epoch when computing DATE-END/END
$utcepoch=get_keyword($outmerge,$tempext,"MJDREFI");

# Compute and set DATE-END keyword using ahtimeconv
$date_obs = ahfilterlib::convert_seconds_to_datetime($tstart[0],$utcepoch,"m");
if (!defined $date_obs) {
  ah_warn("Unable to set DATE-END keyword from TSTART=${tstart[0]}");
} else {
  set_keyword($outmerge,$tempext,"DATE-END",$date_obs);
}

# Compute and set DATE-END keyword using ahtimeconv
$date_end = ahfilterlib::convert_seconds_to_datetime($tstop[0],$utcepoch,"m");
if (!defined $date_end) {
  ah_warn("Unable to set DATE-END keyword from TSTOP=${tstop[0]}");
} else {
  set_keyword($outmerge,$tempext,"DATE-END",$date_end);
}

# Run AHTRENDTEMP.

$status = run_ftool ("ahtrendtemp",
                     "$outmerge", 
                     "$frqtemfile", 
                     "$outtemp", 
                     "$outfile", 
                     "starttime=ALL",
                     "stoptime=ALL",
                     "quartzext=$quartzext", 
                     "tempext=$tempext", 
                     "quartzcol=$quartzcol", 
                     "u32ticol=$u32ticol", 
                     "tempcol=$tempcol", 
                     "stimecol=$stimecol",
                     "leapsecfile=$leapsecfile",
                     "tempresol=$tempresol",
                     "stimemax=$stimemax", 
                     "averagemode=$averagemode",
                     "clobber=YES",
                     "chatter=3"
                     ) ;

# Check if ahtrendtemp threw an error.  If it did, print out an appropriate
# error message.  Then quit.
if  ($status)
    {
    ah_err ("Error encountered while running ahtrendtemp.  " . 
            "Returned status: $status ") ; 

    ah_err (get_tool_stderr()) ;

    ah_err ("Exiting.") ;

    exit 1 ;
    }

ah_info "HIGH", "Processing completed successfully." ;

# Turn off logging and do any cleanup from the ahapp Perl module.
ahapp::shutdown () ;

# Were done.
exit (0) ;

