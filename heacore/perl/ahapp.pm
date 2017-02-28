package ahapp ;

use strict ;
use warnings;
use diagnostics;

# use ahlog ;
# use ahgen qw (query_parameter) ;

# Standard Modules
# use HTML::Strip ;
use File::Spec ;
use File::Spec::Functions qw ( rel2abs catfile ) ;
use File::Basename ;
use Cwd ;

BEGIN {
      use Exporter () ;

      our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS) ;

      # set the version for version checking
      # $VERSION = 1.00 ;

      # if using RCS/CVS, this may be preferred
      $VERSION = sprintf "%d.%03d", q$Revision: 1.3 $ =~ /(\d+)/g;

      @ISA = qw(Exporter);
     
      # Functions exported by default.
      @EXPORT = qw();

      #%EXPORT_TAGS = ( ); # eg: TAG => [ qw!name1 name2! ],
      %EXPORT_TAGS = (ALL => \@EXPORT_OK ); # eg: TAG => [ qw!name1 name2! ],

      # your exported package globals go here,
      # as well as any optionally exported functions
      @EXPORT_OK = qw($debug
                      $mode
                      $clobber
                      $chatter
                      $history
                      $cleanup
                      $logfile
                      &startup
                      &shutdown
                      &query_parameter
                      &write_parameters
                      &add_temp_file
                      &delete_temp_files
                      &getcleanup
                      &begin_processing
                      &print_input_parameters
                      &end_processing
                      @parameter_list 
                      );

      # Import the ahlog library so we use those functions here.
      unshift @INC, catfile dirname (rel2abs (__FILE__)) ;

      require ahlog  ;
      ahlog->import ;

      # Set up some defaults.
      }

# Not sure why this needs to be here.
our @EXPORT_OK;

# exported package globals go here
our $mode ;
our $clobber ;
our $debug ;
our $chatter ;
our $logfile ;
our $history ;
our $cleanup ;
our @TMPFILES ;

# exported package globals go here
our %parameter_list ;

# initialize package globals, first exported ones
%parameter_list = () ;

# non-exported package globals go here
my $force_debug ;

# initialize package globals, first exported ones

# Set the force_debug flag to non-zero (preferably 1) to force ouput debugging
# messages.
#$force_debug = 1;
#$force_debug = '' ;
$force_debug = '' ;

# then the others (which are still accessible as $Some::Module::stuff)

# all file-scoped lexicals must be created before
# the functions below that use them.
# file-private lexicals go here

# Initialize constants
$mode = 0;
$clobber = 0;
$debug = 0;
$chatter = 0;
$logfile = "";
$history = 0;
$cleanup = 0;
@TMPFILES = [] ;

#############################################################################

sub startup 

{
my $debug_param = shift ;

# Query default parameters

$mode       = query_parameter ("mode") ;
$clobber    = query_parameter ("clobber") ;
$debug      = query_parameter ("debug") ;
$chatter    = query_parameter ("chatter") ;
$logfile    = query_parameter ("logfile") ;
$history    = query_parameter ("history") ;
$cleanup    = query_parameter ("cleanup") ;
 

# Reformat debug to a flag.
if  ($debug =~ /YES/i) 
    {
    $debug = 1 ;
    }
else
    {
    $debug = 0 ;
    }   

# Reformat clobber to a flag.
if  ($clobber =~ /YES/i) 
    {
    $clobber = 1 ;
    }
else
    {
    $clobber = 0 ;
    }

# Reformat history to a flag.
if  ($history =~ /YES/i) 
    {
    $history = 1 ;
    }
else
    {
    $history = 0 ;
    }
    
# Reformat cleanup to a flag.
if  ($cleanup =~ /YES/i) 
    {
    $cleanup = 1 ;
    }
else
    {
    $cleanup = 0 ;
    }   

# Check if the caller requested debugging information of if the $force_debug
# flag is set.  Either of these conditions will override the APE parameter.
$debug = 1 if ($force_debug || $debug_param) ;

# Set up logging
start_logging ($logfile, 0, $chatter, $debug) ;

ah_debug "Running: ahapp::startup\n" ;

# Alert the user that debugging is forced on.
if ($force_debug || $debug_param) {
  ah_debug "Debug output is forced on, regardless of the debug parameter setting";
}

# Write some information about the script we are running into the log.
my $execname = (File::Spec->splitpath ($0, 0)) [2] ;
my $cmdline = $execname . " " . join (" ", @ARGV) ;

write_log_comment "STARTLOG: " . scalar localtime, 0 ;
write_log_comment "EXECNAME: " . $execname, 0 ;
write_log_comment "RUNPATH:  " . cwd (), 0 ;
write_log_comment "CMDLINE:  " . $cmdline, 0 ;

ah_info "HIGH", "Starting." ;

}

#############################################################################

sub shutdown

{
ah_debug "Running: ahapp::shutdown\n" ;

ah_info "HIGH", "Finished." ;

# Write the time we are shutting down into the log.
write_log_comment "ENDLOG: " . scalar localtime, 0 ;

# Stop the logging process.
stop_logging ;

}

#############################################################################

sub query_parameter

{
my $param   = shift ;
my $logical = shift ;

my $cmd = "" ;
my $val ;

# Get the name of the tool that is being run.
my $tool_name = $0 ;

# Strip out everything except the name of the tool (path and extensions)
$tool_name =~ s/.*\/// ;
$tool_name =~ s/\..*// ;

# +++ 2015-08-13 JP Do not attempt to handle HEADASNOQUERY as a separate
# +++ case. The reason is that pget has no means of applying the command line. 
# +++ This was causing tools to fail when run by a script that sets
# +++ HEADASNOQUERY, because parameters set on the command line were ignored.
# +++ However, this means that HEADASNOQUERY is effectively broken because
# +++ at this time, pquery2 ignores it. If/when pquery2 is modified to
# +++ respect HEADASNOQUERY, this code will work correctly as it currently
# +++ stands. For now, leaving the original code commented out here for
# +++ sake of making clear what is being done and why.
# Decide whether to use the pquery command or the pget command.  We will use
# the pquery command unless the environment variable HEADASNOQUERY is set.
# if  ($ENV{HEADASNOQUERY})
#     {
#     # Create the basic pget command
#     $cmd = "pget $tool_name $param" ;
# 
#     # Use pget to get the value for the requested parameter.
#     $val = qx/$cmd/ ;
#     }
# 
# else
#     {
    # Create the basic pquery command
    $cmd = "pquery2 $tool_name $param" ;

    # Add any arguments passed to program onto the end of the command.  pquery2
    # will be able to read these in case the requested parameter was passed on the
    # command line.
    foreach (@ARGV)
        {
        $cmd = $cmd . " " . "\"$_\"" ;
        }

    # Use pquery2 to get the value for the requested parameter.
    $val = qx/$cmd/ ;
# +++ 2015-08-13 JP This is the closure of the block related to the HEADASNOQUERY
# +++ change explained above.
#    }

# Remove any end of line characters
chomp $val ;

# Add the parameter and its value to the parameter list
$parameter_list{$param} = $val ;

# Convert the parameter to a true / false value for parameters that flagged
# as logical parameters.  (0 = FALSE / 1 = TRUE)
if  ($logical) 
    {
    $val = ($val =~ /yes/i)? 1 : 0 ;
    }

return $val ;
} # end sub query_parameter

#############################################################################

sub write_parameters 

# Create a list of all the parameters set in the script.
# 
# When the function command is preceded by ah_info, the parameters are 
# written to the log file.
#     ah_info "HIGH", ahapp::write_parameters () ;
# 
# Parameters can also be written to a FITS file as a set of HISTORY keywords.
#     if ($history) {   
#       ahapp::write_parameters($outfile, $extname) ;
#     }

{

# Read parameters 
my $fits_file = shift ; 
my $ext       = shift ;

# this will hold the plist command
my $cmd = "" ;

# these will hold the output text that will be written to log or fits file
my @str ;
my @fits_str ;

# the string holding the cut-and-paste friendly format
my $cmd_str ;

# the string result of calling plist
my $plistString;

# splitting the plist results into two elements, separated by the equal sign
my ($currParamName, $theRest);

# the results of query_parameter (pget or pquery) to find value of parameter
my $currParamVal;

# Create a parameter index to keep track of the number of parameters processed.
my $i = 0 ;

# Get the name of the tool without any other path information
my $tool_name = (File::Spec->splitpath ($0, 0)) [2] ;

# Formatted command string starts with tool name
# Prepend space to make cutting/pasting from screen easier
$cmd_str = " " . $tool_name;

# Create the basic plist command
$cmd = "plist $tool_name" ;

# Use pquery2 to get the value for the requested parameter.
$plistString = qx/$cmd/ ;

# Find the maximum length of a parameter name.
my $maxlength = 0 ;

# Compare the length of each parameter name to $maxlength.  If it is longer,
# then update $maxlength with the maximum.
foreach (keys %parameter_list)
    {
    $maxlength = length if (length > $maxlength) ;
    }

# split the plist string into an array, where each line is an element
my @plist = split "\n", $plistString;

# Create a header
push @str, " " ;
push @str, "START PARAMETER list for " . $tool_name ;

if (defined $fits_file && defined $ext)
    {
    push @fits_str, " " ;
    push @fits_str, "START PARAMETER list for " . $tool_name ;
    push @fits_str, " " ;
    }

# Process each of the parameters that have been set for the current tool. Use
# $i to keep track of the number processed.
# analyze each line to find the parameters
foreach my $line (@plist) {

  # make sure this is actually a line with a parameter
  if($line =~ m/=/) {
  
    # split $line into a maximum of two elements separated by any amount of 
    # whitespace, an equals, and any amount of whitespace.
    my ($currParamName, $theRest) = split /\s*=\s*/, $line, 2;
    # remove the left parenthesis from hidden parameters
    $currParamName =~ s/\(//;
    # trim off leading and trailing white space
    $currParamName =~ s/^\s+|\s+$//g;
    
    if(exists $parameter_list{$currParamName}) {
      # We only need to query the parameter if it hasn't been queried 
      # Otherwise the default value for hidden parameters will be printed
      $currParamVal = $parameter_list{$currParamName};
    } else {
      # query the value of the parameter
      # don't use query_parameter because it uses pquery2 by default, which
      # queries the user again
      $cmd = "pget $tool_name $currParamName" ;
      $currParamVal = qx/$cmd/ ;
      # Remove any end of line characters
      chomp $currParamVal ;
    }

    # Concatenate the parameter, surrounded by single quotes, onto the
    # formatted command string
    $cmd_str .= qq{ '};
    $cmd_str .= sprintf ("%s=%s", $currParamName, $currParamVal);
    $cmd_str .= qq{'};

    # Save the info for this parameter in FITS HISTORY format
    if (defined $fits_file && defined $ext)
        {
        push @fits_str, sprintf ("p%-2i %-" . $maxlength . "s = %s", $i, $currParamName, $currParamVal);
        }
    
    $i++;
  } # end-if this line has an = sign

} # end-loop through lines from plist

# Add formatted command to the array to be printed
push @str, $cmd_str;

# Create a footer
push @str, "END PARAMETER list for " . $tool_name ;
push @str, " " ;

# Check if a FITS file name was passed.  If a FITS file name and extension was
# then we will write the parameters as history keywords to the FITS file.
if (defined $fits_file && defined $ext)
    {
    push @fits_str, "END PARAMETER list for " . $tool_name ;
    push @fits_str, " " ;
    foreach (@fits_str)
        {
        my $cmd = "fthedit $fits_file\[$ext\] HISTORY a '" . $_ . "'" ;
        qx/$cmd/ ;
        }
    }

# Return the formatted command containing the list of parameters we generated.
return @str ;

} # end sub write_parameters

#############################################################################

sub getcleanup
{
# Get the current cleanup flag

return $cleanup ;
}

#############################################################################

sub add_temp_file

# Add a new file to the temporary file array

{
  # Read parameters.
  my $infile = shift;

  # Make sure $infile was defined.
  $infile = "" unless defined $infile;

  # Add input file if it exists
  push @TMPFILES, $infile;

  # Were done.
  return 0;
}

#############################################################################

sub delete_temp_files

# Delete all files from the temporary file array and empty array

{
  # Loop through all temporary files and delete existing files
  if (@TMPFILES) {
    foreach my $filename(@TMPFILES) {
      if (-e $filename) { unlink($filename); }
    }
  }

  # Empty temporary file array
  @TMPFILES = ();

  # Were done.
  return 0;
}

#############################################################################

sub begin_processing {

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nRunning begin_processing\n";
  }

  my $toolname = basename($0);

  # Start of processing message

  ahlog::ah_info "HIGH", "\n===========================================================\n";
  ahlog::ah_info "HIGH", "                Running $toolname\n";
  ahlog::ah_info "HIGH", "===========================================================\n\n";

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nEnd of begin_processing\n";
  }

  return 0;

} # begin_processing

sub print_input_parameters {

  ahlog::ah_info "HIGH", write_parameters () ;

  return 0;

} # print_input_parameters

# Subroutine to clean up temporary files, print out
# flag with error condition, and run shutdown
sub end_processing {

  my $status = shift;

  $status = 0 unless defined $status;

  my $toolname = basename($0);

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nRunning end_processing\n";
  }

  if( getcleanup ) {
    # remove temporary files
    delete_temp_files ;
  }

  # End of processing message

  ahlog::ah_info "HIGH", "\n===========================================================\n";
  ahlog::ah_info "HIGH", "                Running $toolname \n";
  ahlog::ah_info "HIGH", " Final Return Code - " . $status;
  if($status) {
    ahlog::ah_info "HIGH", " Exit ERROR CONDITION";
  } else {
    ahlog::ah_info "HIGH", " Exit with no errors";
  }
  ahlog::ah_info "HIGH", "===========================================================\n\n";

  if ( ahlog::getdebug ) {
    ahlog::ah_debug "\nEnd of end_processing\n";
  }

  # Turn off logging and do any cleanup from the ahapp Perl module.
  ahapp::shutdown();

  exit $status;

}

#############################################################################
1;
