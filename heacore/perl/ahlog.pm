package ahlog ;

use strict ;
use warnings;
use diagnostics;

#use lib "/local/data/grok1/ryurow/ah/proc/common" ;
#use lib "/local/data/grok1/ryurow/ah/proc/lib" ;

# Standard Modules
# use HTML::Strip ;
use File::Spec ;
# use Cwd ;

BEGIN {
      use Exporter () ;

      our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS) ;

      # set the version for version checking
      $VERSION = 1.01 ;

      # if using RCS/CVS, this may be preferred
      # $VERSION = sprintf "%d.%03d", q$Revision: 1.1 $ =~ /(\d+)/g;

      @ISA = qw(Exporter);
     
      # Functions exported by default.
      @EXPORT = qw(&get_utc_time 
                   &create_error_msg 
                   &create_log_file_name
                   &open_log_file_read
                   &open_log_file_write
                   &close_log_file 
                   &write_log_line
                   &write_log_comment
                   &read_valid_line
                   &validate_line
                   &read_log_line
                   &find_log_line
                   &parse_line
                   &assign_error_level
                   &search_log
                   &setchatter
                   &getchatter
                   &setdebug
                   &getdebug
                   &ah_out
                   &out
                   &ah_info
                   &ah_warn
                   &warning
                   &ah_err
                   &error
                   &ah_debug
                   &start_logging 
                   &stop_logging
                   );

      #%EXPORT_TAGS = ( ); # eg: TAG => [ qw!name1 name2! ],

      # your exported package globals go here,
      # as well as any optionally exported functions
      # @EXPORT_OK = qw($debug);

      # Set up some defaults.
      }

# Not sure why this needs to be here.
our @EXPORT_OK;

# exported package globals go here

# non-exported package globals go here
our $debug ;
our $LF ;
our $log_file_name ;
our $log_file_comnt_delim ;
our $log_file_field_delim ;
our $log_write_open ;
our $log_read_open ;
our $c_eml_chatter ;
our $c_eml_debug ;
our $c_eml_use_log_file ;
our $c_eml_log_enabled ;

# initialize package globals, first exported ones

# Set the debug flag to non-zero (preferably 1) to ouput debugging messages.
#$debug = 1;
#$debug = '' ;
$debug = '' ;

# then the others (which are still accessible as $Some::Module::stuff)

# all file-scoped lexicals must be created before
# the functions below that use them.
# file-private lexicals go here
$log_write_open = 0 ;
$log_read_open = 0 ;
$log_file_comnt_delim = "" ;
$log_file_field_delim = "" ;

$LF = undef ;

$c_eml_chatter = 1 ;
$c_eml_debug = 0 ;
$c_eml_log_enabled = 0 ;
$c_eml_use_log_file = 1 ;

# Initialize constants

our @FIELDS = qw (error_type error_str) ;  
our $SEPERATOR     = '::' ;
our $COMMENT       = '#' ;
our $STRIPHTMLFLAG = 1 ;
our $DEFAULT_LOG_EXTENSION = "log" ;
our @ERRORLVL = qw(DEBUG INFO WARN ERROR FATAL) ;


#############################################################################

sub assign_error_level

{
# Read parameters.  
# p1 => error level
my $error_level = shift ;

my $nlevel = scalar @ERRORLVL ;

# Make sure that the error level is valid.  
$error_level = 0 if $error_level < 0 ;
$error_level = $nlevel - 1 if $error_level > $nlevel - 1 ;

# Get the error string
my $err = $ERRORLVL [$error_level] ;

# Pad the error string.
$err = sprintf ("%-*s", 7, $err) ; 

return $err ;
}

#############################################################################

sub get_utc_time 

{
my $utc ;
my $sec ;
my $min ;
my $hour;
my $day ;
my $mon ;
my $year ;

# Get the UTC time.
($sec, $min, $hour, $day, $mon, $year) = (gmtime (time)) [0 .. 5] ;

# gmtime gives month as a 0-based number
$mon++;

# Add 1900 to the year.  This converts it to the typical 4 digit year value.
$year += 1900 ;

# Create the UTC time string.
$utc = sprintf ('%4.4d-%2.2d-%2.2dZ%2.2d:%2.2d:%2.2d', $year, $mon, $day, $hour, $min, $sec);

return $utc ;
}

#############################################################################

sub create_error_msg

{
# Read parameters.  
# p1 => error code
# p2 => error level
# p3 => error message
# p4 => true if the UT time should be appended to each log message.
my $ecode = shift ;
my $error = shift ;
my $errmsg = shift ;
my $use_utc_time = shift ;

my @err_msg_arr ;

# Set use_utc_time to its default value, if needed.
$use_utc_time = 1 unless defined $use_utc_time ;

# Detect if a reference was passed as the errmsg.  

# An array error messages may processed at once by passing a reference
# as a parameter to this function. 
if  (ref $errmsg)
    {
    @err_msg_arr = @{$errmsg} ;
    }

# Otherwise, just a single message was passed to us (which may contain 
# multiple lines.
else
    {
    $err_msg_arr [0] = $errmsg ; 
    }

my @logmsg ;
my $count ;
my $msg ;
my $str ;

# Create a prefix string that will form the begining of each log line.  
$str = assign_error_level ($error) . "::" ;

if  ($use_utc_time)
    {
    $str .= get_utc_time () . "::" ;
    }
    
if  ($ecode)
    {
    $str .= $ecode . "::" ;
    }

$count = 0 ;

# Create a log line for each error message that was passed to us.
foreach (@err_msg_arr)
    {
    # Chomp the last new-line character if there is one.
    chomp ($_) ;

    # Append an end of line character to the end of message.
    $_ = $_ . "\n" ;

    # Create the log messages.
  
    # The original error message may contain multiple lines.  If it does then
    # each line will become its own log message.
    foreach $msg (split /^/, $_)
        {
        $logmsg [$count] = $str . $msg ;
 
        $count++
        }
    }

return @logmsg ;
}

#############################################################################

sub create_log_file_name

{
# Read parameters.  
# p2 => extra characters to include log file name
my $base_name   = shift ;
my $include_pid = shift ;
my $extension   = shift ;
my $extra       = shift ;

# Set undefined parameters to their default values:
$include_pid = 0 unless defined $include_pid ;
$extension   = 0 unless defined $extension ;
$extra       = 0 unless defined $extra ;


# Set base name.
unless ($base_name)
    {
#    my $vol ;
#    my $path ;

#    ($vol, $path, $base_name) = File::Spec->splitpath (( caller(0) ) [1]) ;
    $base_name = (File::Spec->splitpath ($0, 0)) [2] ;
    }

$log_file_name = $base_name ;

# regex to remove everything including and after a period:
# \. look for a period (need to escape it with the \)
# .  represents any character after the preceding one (which was a period)
# *  search for that character (any character) 0 or more times
$log_file_name =~ s/\..*//;

# Check if we need to include the PID in the log file name.
if  ($include_pid)
    {
    $log_file_name = $log_file_name . '.' . $$ ;
    }

# Check if we were asked to include any extra characters in the log file name.
if  ($extra)
    {
    $log_file_name = $log_file_name . '.' . $extra ;
    }

# Make sure that an extension for the log file has been defined.  If it has not
# then use the default log file extension.
$extension = 'log' unless ($extension) ;

$log_file_name = $log_file_name . '.' . $extension ;


#if  ($debug)
#    { 
#    print "Log file name: $log_file_name\n" ;
#    }

# print "log file name: $log_file_name\n" ;

return $log_file_name ;

}

#############################################################################

sub open_log_file_read

{
# Read parameters.  
# p2 => logfile (optional).
# p3 => path to log file (optional).
my $logfile = shift ;
my $logfilepath = shift ;

# Check if the user passed a filename to read.  If they did not pass a log file
# name then just return 0 to indicate an error.
if  (! defined $logfile )
    {
    return 0 ;
    }

# Check if $logfilepath is set.  If it is, then we will use it when trying
# to locate the logfile.   Otherwise, we will use the curent working directory.
if  (! $logfilepath) 
    {
    $logfilepath = File::Spec->curdir() ; 
    }

# Determine if $logfile refers to a path or if it is just a file name.  If it
# is just filename then concatenate it with $logfilepath to form a complete 
# path.
if  ($logfile !~ /[\\|\/]/)
    {
    $logfile = File::Spec->catfile ($logfilepath, $logfile) ;
    } 

# Check if a log file is already open.  If it is then close it.
close_log_file () if $log_write_open || $log_read_open ;

# Open the list file.  Exit with we can't open the file.
if  ( ! open ($LF, '<', $logfile) )
    {
    print STDERR "ERROR: Failed to open file: ", $logfile, ": ", $!, "\n" ;
    exit 0 ;
    }

if  ( $debug )
    {
    print "Opened log file: $logfile for reading. \n" ;
    }

# Set the open file flags as appropiate.
$log_write_open = 0 ;
$log_read_open = 1 ;

# Return the file handle.  Were done.
return $logfile ;
}

#############################################################################

sub open_log_file_write

{
# Read parameters.  
# p2 => logfile (optional).
# p3 => path to log file (optional).
my $logfile = shift ;
my $logfilepath = shift ;
my $truncate    = shift ; 

my $modestr ;

# Check if the user passed a filename to read.  If they did not pass a log file
# name then just return 0 to indicate an error.
if  (! defined $logfile )
    {
    return 0 ;
    }

# Check if $logfilepath is set.  If it is, then we will use it when trying
# to locate the logfile.   Otherwise, we will use the curent working directory.
if  (! $logfilepath) 
    {
    $logfilepath = File::Spec->curdir() ; 
    }

# Check if the truncate parameter was passed.  If it wasn't then set it to a
# default value.
$truncate = 0 unless defined $truncate ;

# Determine if $logfile refers to a path or if it is just a file name.  If it
# is just filename then concatenate it with $logfilepath to form a complete 
# path.
if  ($logfile !~ /[\\|\/]/)
    {
    $logfile = File::Spec->catfile ($logfilepath, $logfile) ;
    } 

# Check if a log file is already open.  If it is then close it.
close_log_file () if $log_write_open || $log_read_open ;

# Set the mode to open the log file (append or truncate)
$modestr = ($truncate)? '>' : '>>' ;

# Open the list file.  Exit if we can't open the file.
if  ( ! open ($LF, $modestr, $logfile) )
    {
    print STDERR "ERROR: Failed to open file: ", $logfile, ": ", $!, "\n" ;
    exit 0 ;
    }

if  ( $debug )
    {
    print "Opened log file: $logfile for writing.\n" ;
    }

# Force the file to be unbuffered.
my $def_file_handle = select ($LF) ;
$|++ ;
select ($def_file_handle) ;

# Set the open file flags as appropiate.
$log_write_open = 1 ;
$log_read_open = 0 ;

# Return the file handle.  Were done.
return $logfile ;
}

#############################################################################

sub close_log_file

{
# Read parameters.  
# my $lf   = shift ;

# Close the log file.
close $LF ;

# set the File Handle to undefined.
$LF = undef ;

# Set the open file flags as appropiate.
$log_write_open = 0 ;
$log_read_open = 0 ;

return 0 ;
}

#############################################################################

# Wrapper function to duplicate the behavior of ahlog method error.
# Used to send fatal error messages to log or console.
sub log_error

{
# Implements the AH_ERR macro

# Read parameters.
my (@in) = @_ ;

my @msg ;

my $n_parames = scalar @in ;

# Note: Not subject to chatter (except for chatter level 0) ;

# Return unless logging is enabled.
return unless $c_eml_log_enabled ;

# Write the message to STDOUT unless chatter = 0 
unless ($c_eml_chatter == 0)
    {
    # Create an appropiate error message (or messages)
    @msg = create_error_msg (0, 4, \@in, 0) ;

    } 

# Create an appropiate error message (or messages)
@msg = create_error_msg (0, 4, \@in, 0) ;

# Write the message to the log
write_log_line (\@msg, 0)  ;

return ;
}

#############################################################################

# Wrapper function to duplicate the behavior of ahlog method log.
# Used to send error messages to log or console.
sub logmsg

{
}

#############################################################################

# Wrapper function to duplicate the behavior of ahlog method out.
# Used to send informational messages to log or console.
sub log_out

{
# Implements the AH_OUT macro

# Read parameters.
my (@in) = @_ ;

my @msg ;

my $n_parames = scalar @in ;

# Note: Not subject to chatter (except for chatter level 0) ;

# Return unless logging is enabled.
return unless $c_eml_log_enabled ;

# Write the message to STDOUT unless chatter = 0 
unless ($c_eml_chatter == 0)
    {
    # Create an appropiate error message (or messages)
    @msg = create_error_msg (0, 1, \@in, 0) ;

    } 

# Write the message to the log file if logging is enabled.
if  ($c_eml_use_log_file)
    {
    # Create an appropiate error message (or messages)
    @msg = create_error_msg (0, 1, \@in, 0) ;

    write_log_line (\@msg, 0)  ;
    }

return ;
}

#############################################################################

# Wrapper function to duplicate the behavior of ahlog method warn.
# Used to send warning messages to log or console.
sub log_warning

{
# Implements the AH_WARN macro

# Read parameters.
# p1 => error level
my $lvl = shift ;
my (@in) = @_ ;

my @msg ;

my $n_parames = scalar @in ;

# Note: Not subject to chatter (except for chatter level 0) ;

# Return unless logging is enabled.
return unless $c_eml_log_enabled ;

# Convert the message lvl "HIGH" or "LOW" to numeric value.  This value will
# determine if a warning message or error message is generated.
my $err_lvl = $lvl =~ /HIGH/? 2 : 3 ;

# Write the message to STDOUT unless chatter = 0 
unless ($c_eml_chatter == 0)
    {
    # Create an appropiate error message (or messages)
    @msg = create_error_msg (0, $err_lvl, \@in, 0) ;

    } 

# Create an appropiate error message (or messages)
@msg = create_error_msg (0, $err_lvl, \@in, 0) ;

# Write the message to the log
write_log_line (\@msg, 0)  ;


return ;
}

#############################################################################

sub write_log_msg

# Currrently this subroutine is not being used.  
# Should it be deleted ??

{
my $ecode = shift ;
my $error = shift ;
my $errmsg = shift ;
my $use_utc_time = shift ;
my $close_flag = shift ;
my $logfile = shift ;
my $logfilepath = shift ;

my $log_msg = create_log_file_name ($ecode, $error, $errmsg, $use_utc_time) ;

my $r = write_log_line ($close_flag, $logfile, $logfilepath) ;

return $r ;
}

#############################################################################

sub write_log_line

{
# Read parameters.  
# p1 => log message.
# p2 => close after write (default to TRUE)
# P3 => logfile name (optional)
# p4 => path to log file (optional).
my $line = shift ;
my $close_flag = shift ;
my $logfile = shift ;
my $logfilepath = shift ;

my @log_msg_arr ;

# Detect if a reference was passed as line.  

# multiple lines may be written to the log file at once by passing a reference
# as a parameter to this function. 
if  (ref $line)
    {
    @log_msg_arr = @{$line} ;
    }

# Otherwise, just a single message was passed to us (which may contain 
# multiple lines.
else
    {
    $log_msg_arr [0] = $line ; 
    }

# Set the close_flag to its default value, if needed.
$close_flag = 1 unless defined $close_flag ;

# Check if we need to open a log file.  
# Check if a log file is already open.  If it is then close it.
unless ($log_write_open)
    { 
    # Open the log file.  This will just return the correct file handle if the
    # log file is already open.  Function will return 0 on error
    unless (open_log_file_write ($logfile, $logfilepath))
        {
        # May eventually do something else here.

        return 0 ;
        }
    }

# Write the lines to the log file.
foreach (@log_msg_arr) {print $LF $_}

# Check the close flag, if set then close log file.
close_log_file () if $close_flag ;

# Return the name of the logfile we wrote to.
return 1 ;
}

#############################################################################

sub write_log_comment

{
# Read parameters.  
# p1 => log comment message.
# p2 => close after write (default to TRUE)
# P3 => comment charachter (optional)
# P4 => logfile name (optional)
# p5 => path to log file (optional).
my $line = shift ;
my $close_flag = shift ;
my $cmnt_delimiter = shift ;
my $logfile = shift ;
my $logfilepath = shift ;

# Set the close_flag to its default value, if needed.
$close_flag = 1 unless defined $close_flag ;

# Check if we need to open a log file.  
# Check if a log file is already open.  If it is then close it.
unless ($log_write_open)
    { 
    # Open the log file.  This will just return the correct file handle if the
    # log file is already open.  Function will return 0 on error
    unless (open_log_file_write ($logfile, $logfilepath))
        {
        # May eventually do something else here.

        return 0 ;
        }
    }

# Make sure a comment delimiter was defined.
unless ($cmnt_delimiter)
    {
    # Use the defualt comment delimiter if no comment has alredy been
    # defined for this log file.
    $log_file_comnt_delim = $COMMENT unless $log_file_comnt_delim ;

    # Set the comment delimiter to the one that was previously stored 
    # (or the default one if there is no previously stored delimiter)
    $cmnt_delimiter = $log_file_comnt_delim ;
    }

else
    {
    # Store the new comment delimiter.
    $log_file_comnt_delim = $cmnt_delimiter ;
    }


# Write the line to the log file.
print $LF $cmnt_delimiter . " " . $line . "\n" ;

# Check the close flag, if set then close log file.
close_log_file () if $close_flag ;

# Return the name of the logfile we wrote to.
return 1 ;
}

#############################################################################

sub update_comnt_delim

{
# Read parameters.  
# p1 => comment delimiter
# my $cmnt_delimiter = shift ;

# Make sure a comment delimiter was defined.
unless ($_[0])
    {
    # Use the defualt comment delimiter if no comment has alredy been
    # defined for this log file.
    $log_file_comnt_delim = $COMMENT unless $log_file_comnt_delim ;

    # Set the comment delimiter to the one that was previously stored 
    # (or the default one if there is no previously stored delimiter)
    $_[0] = $log_file_comnt_delim ;
    }

else
    {
    # Store the new comment delimiter.
    $log_file_comnt_delim = $_[0] ;
    }

return ;
}

#############################################################################

sub update_field_delim

{
# Read parameters.  
# p1 => field delimiter
# my $fld_delimiter  = shift ;

# Make sure a field delimiter was defined.
unless ($_[0])
    {
    # Use the defualt field delimiter if no field has alredy been
    # defined for this log file.
    $log_file_field_delim = $SEPERATOR unless $log_file_field_delim ;

    # Set the field delimiter to the one that was previously stored 
    # (or the default one if there is no previously stored delimiter)
    $_[0] = $log_file_field_delim ;
    }

else
    {
    # Store the new field delimiter.
    $log_file_field_delim = $_[0] ;
    }
}

#############################################################################

sub parse_line 

{
# Read parameters.  
# p2 => line.
# p3 => character string used to deliminate log field lines.
my $line           = shift ;
my $fld_delimiter  = shift ;


# Currently this function breaks log line up into fields based on a field
# delimiter.  What we would eventually like to do is take a log line 
# format string parse the log line based on that format string.  It would then
# return a hash with the results of the parse. 

# Make sure a field delimiter was defined.
update_field_delim ($fld_delimiter) ;

# Split the line into fields.
my @fields = split /\Q$fld_delimiter/, $line ;

# Return the array that we found.

# fini.
return @fields ;
}

#############################################################################

sub validate_line

{
# Read parameters.  
# p2 => line.
# p3 => character string used to deliminate log field lines.
# p4 => number of fields that must be present to for line to be valid.
my $line             = shift ;
my $required_fields  = shift ;
my $fld_delimiter    = shift ;

# Make sure that the number of required fields was specified.  If it was not,
# then set this value to 1.
$required_fields = 1 unless $required_fields ;

# Make sure a field delimiter was defined.
update_field_delim ($fld_delimiter) ;

# Currently this function validates a log file line by breaking it up into
# fields and then making sure that there are a least as many fields as are
# required by the parameter $required_fields. What we would eventually like
# to do is take a log line format string and validate the log line against
# that format string. 

# Split the line into fields.
my @tmp = split /\Q$fld_delimiter/, $line ;

my $n_fields = scalar @tmp ;

# Make sure we have enough fields. If not, return 0.
return 0 if $n_fields < $required_fields ;
return 1 ;

}

#############################################################################

sub read_valid_line

{
# Read parameters.  
# p2 => log file handle
# p3 => comment delimiter
my $cmnt_delimiter = shift ;

# Probably should be renamed read non-comment line.

# Make sure a comment delimiter was defined.
update_comnt_delim ($cmnt_delimiter) ;

my $line = '' ;

# Read a valid line from the log file.  Ignore blank lines and lines that
# begin with the comment character.
while  (1) 
    {
    # Check for EOF.
    return '' if eof $LF ;

    # Read line from the log file.
    $_ = <$LF> ;

    # Strip HTML from the line if the STRIP_HTML flag is set.
    s/<(.|\n)*?>//g ;

    # Check for blank line.
    next if /^\s*$/ ;
    
    # Check for comment character.
    next if /^\Q$cmnt_delimiter/ ;

    $line = $_ ;
    last ;
    }      
         
return $line ;
} 

#############################################################################

sub read_log_line

{
# Read parameters.  
# p2 => log file handle
# p3 => comment delimiter
my $index_field    = shift ;
my $fld_delimiter  = shift ;
my $cmnt_delimiter = shift ;

# Make sure a comment delimiter was defined.
update_comnt_delim ($cmnt_delimiter) ;

# Make sure a field delimiter was defined.
update_field_delim ($fld_delimiter) ;

# Make sure the field index was set.  If not, set it to 1.
$index_field = 1 unless $index_field ;

my $line ;

# Read the next valid line from the log file.
while ($line = read_valid_line ())
      {
      if  ($debug)
          {
          print "Read Line: " , $line ;
          }

      # Check if it is valid log line.
      last if  validate_line ($line, $index_field) ;
      }

return $line ;
}

#############################################################################

sub find_log_line

{
# Read parameters.  
# p2 => field number to search on.
# p3 => target value.
# p4 => log file handle
# p5 => comment delimiter
my $index_field      = shift ;
my $target           = shift ;
my $cmnt_delimiter   = shift ;
my $fld_delimiter    = shift ;

my $line ;

# Make sure a comment delimiter was defined.
update_comnt_delim ($cmnt_delimiter) ;

# Make sure a field delimiter was defined.
update_field_delim ($fld_delimiter) ;

# Make sure the field index was set.  If not, set it to 1.
$index_field = 1 unless $index_field ;

# Read lines from the log file until we find a line that matches our 
# search criterion.
while ($line = read_log_line ($index_field))
    {
    # Parse the line.
    my @r = parse_line ( $line ) ;

    # Check if we found a match.  If we did, return the line.
    if  (  $target =~ /$r[$index_field]/ )
        {
        return $line ;
        } 
    }

# Could not read a line that meets our search criterion from the log file. 
return '' ;
}

#############################################################################

sub read_line

{
# Do we still need this???

# Read parameters.

my $line = '' ;

# Check for EOF.
return '' if eof $LF ;

# Read line from the log file.
$line = <$LF> ;

# Strip HTML from the line if the STRIP_HTML flag is set.
#if  ($self->{STRIP_HTML})
#    {
#    $line = $self->{HS}->parse ($line) ;
#    }

return $line ;
}

#############################################################################

sub search_log

{
# Read parameters.
# p2 => field number.
# p3 => target value.
# p4 => logfile (optional).
# p5 => path to log file (optional).
# p6 => comment delimiter (optional)
my $field_index      = shift ;
my $target           = shift ;
my $logfile          = shift ;
my $logfilepath      = shift ;
my $cmnt_delimiter   = shift ;
my $fld_delimiter    = shift ;

# Make sure a comment delimiter was defined.
update_comnt_delim ($cmnt_delimiter) ;

# Make sure a field delimiter was defined.
update_field_delim ($fld_delimiter) ;


# Open the log file.
unless (open_log_file_read ( $logfile, $logfilepath ))
    {
    print "Opened failed\n" ;

    return -1 ;
    }

my @found ;
my $line ;
my $count = 0 ; 

# Read through the log file for matching lines.  Exit when we get to the end of
# of the file.  For each line we find, we will add it to our array of found
# lines.
while (! eof ($LF))
    {
    $line = find_log_line ( $field_index, $target) ;

    $found[$count] = $line if $line ;

    $count++ ;
    }

# Close the log file.
close_log_file () ;

# Return whatever we got.
return @found ;
}

#######################################################
# Functions to provide a similar interface with ahlog.h
#######################################################

sub setchatter

{
# Set the current chatter level

# Read parameters.
# p1 => chatter level
my $chatter = shift ;

# Set chatter level to 1 if no defined value was passed to us.
$chatter = 1 unless defined $chatter ;

# Force the chatter level to be between 0 and 3
$chatter = 0 if $chatter < 0 ;
$chatter = 3 if $chatter > 3 ;

# Set the chatter level
$c_eml_chatter = $chatter ;

# Were done.
return $c_eml_chatter ;
}

#############################################################################

sub getchatter

{
# Get the current chatter level

return $c_eml_chatter ;
}

#############################################################################

sub setdebug

{
# Set the current debug flag

# Read parameters.
# p1 => set/clear the debug flag
my $db_flag = shift ;

# Make sure $db_flag was defined.  If it wasn't, then we will clear the flag.
$db_flag = 0 unless defined $db_flag ;

# Force it 1/0 and save the new debug status
$c_eml_debug = ($db_flag)? 1 : 0 ;

# Were done.
return $c_eml_debug ;

}

#############################################################################

sub getdebug
{
# Get the current debug flag

return $c_eml_debug ;
}

#############################################################################

sub ah_debug

{
# Implements the AH_DEBUG macro

# Read parameters.
my (@in) = @_ ;

my @msg ;

my $n_parames = scalar @in ;

# Note: Not subject to chatter

# Return unless logging is enabled.
return unless $c_eml_log_enabled ;

# Return unless debug mode.
return unless $c_eml_debug ;

# Write the message to STDOUT
# Create an appropiate error message (or messages)
@msg = create_error_msg (0, 0, \@in, 0) ;

foreach (@msg) {print STDOUT $_}

# Write the message to the log file if logging is enabled.
if  ($c_eml_use_log_file)
    {
    # Create an appropiate error message (or messages)
    @msg = create_error_msg (0, 0, \@in, 0) ;

    write_log_line (\@msg, 0)  ;
    }

return ;
}

#############################################################################

sub ah_out

{
# Implements the AH_OUT macro

# Read parameters.
my (@in) = @_ ;

my @msg ;

my $n_parames = scalar @in ;

# Note: Not subject to chatter (except for chatter level 0) ;

# Return unless logging is enabled.
return unless $c_eml_log_enabled ;

# Write the message to STDOUT unless chatter = 0 
unless ($c_eml_chatter == 0)
    {
    # Create an appropiate error message (or messages)
    @msg = create_error_msg (0, 1, \@in, 0) ;

    foreach (@msg) {print STDOUT $_}
    } 

# Write the message to the log file if logging is enabled.
if  ($c_eml_use_log_file)
    {
    # Create an appropiate error message (or messages)
    @msg = create_error_msg (0, 1, \@in, 0) ;

    write_log_line (\@msg, 0)  ;
    }

return ;
}

#############################################################################

sub ah_info

{
# Implements the AH_INFO macro

# Read parameters.
# p1 => error level
my $lvl  = shift ;
my (@in) = @_ ;

my @msg ;

my $n_parames = scalar @in ;

# Return unless logging is enabled.
return unless $c_eml_log_enabled ;

# Convert the message lvl "HIGH" or "LOW" to numeric value to be compared to
# chatter Return unless chatter level is less than or equal to this value.
my $chat_lvl = $lvl =~ /HIGH/? 2 : 3 ;

# Create an appropiate error message (or messages)                                                                                                 
@msg = create_error_msg (0, 1, \@in, 0) ;

# Write the message to the log, regardless of chatter level                                                                                        
write_log_line (\@msg, 0)  ;

# Keep going only if user has set chatter high enough.                                                                                             
return unless $c_eml_chatter >= $chat_lvl ;

# Write the message to STDOUT unless chatter = 0                                                                                                   
unless ($c_eml_chatter == 0) {
    foreach (@msg) {print STDOUT $_}
}

return ;

}

#############################################################################

sub ah_warn

{
# Implements the AH_WARN macro

# Read parameters.
# p1 => error level
my $lvl = shift ;
my (@in) = @_ ;

my @msg ;

my $n_parames = scalar @in ;

# Note: Not subject to chatter (except for chatter level 0) ;

# Return unless logging is enabled.
return unless $c_eml_log_enabled ;

# Convert the message lvl "HIGH" or "LOW" to numeric value.  This value will
# determine if a warning message or error message is generated.
my $err_lvl = $lvl =~ /HIGH/? 2 : 3 ;

# Create an appropiate error message (or messages)                                                                                                 
@msg = create_error_msg (0, $err_lvl, \@in, 0) ;

# Write the message to the log, regardless of chatter level                                                                                        
write_log_line (\@msg, 0)  ;

# Write the message to STDOUT unless chatter = 0                                                                                                   
unless ($c_eml_chatter == 0) {
    foreach (@msg) {print STDERR $_}
}

return ;

}

#############################################################################

sub ah_err

{
# Implements the AH_ERR macro

# Read parameters.
my (@in) = @_ ;

my @msg ;

my $n_parames = scalar @in ;

# Note: Not subject to chatter (except for chatter level 0) ;

# Return unless logging is enabled.
return unless $c_eml_log_enabled ;

# Write the message to STDOUT unless chatter = 0 
unless ($c_eml_chatter == 0)
    {
    # Create an appropiate error message (or messages)
    @msg = create_error_msg (0, 4, \@in, 0) ;

    foreach (@msg) {print STDERR $_}
    } 

# Create an appropiate error message (or messages)
@msg = create_error_msg (0, 4, \@in, 0) ;

# Write the message to the log
write_log_line (\@msg, 0)  ;

return ;
} 

#############################################################################

sub start_logging 

{
# Implements the setup function from ahlog.
# Note that parameter order is different from the ahlog version

# Read parameters.
# p1 => log file name
# p2 => executable name (optional)
# p3 => chatter level   (optional)
# p4 => debug_flag      (optional)
my $log_file_name = shift ;
my $exec_name = shift ;
my $chatter = shift ;
my $debug_flag = shift ;

# Note.  No return value.  This is non-standard from the rest of the package.

# Process the log file name.

# Note.  We can not accept an undefined log file name.  Return if this is the
# case.
return unless defined $log_file_name ;

# Make sure an executable name was passed us.  Otherwise set exec_name to
# the name of the current executable.
$exec_name = (File::Spec->splitpath ($0, 0)) [2] unless $exec_name ;

# If log_file_name is set to NONE, then don't write anything to a log file.
$c_eml_use_log_file = ($log_file_name =~ /NONE/i)? 0: 1 ;

# If we are going to use a log file, then set it up now.
if  ($c_eml_use_log_file)
    {
    # Set the truncate flag.  If the truncate flag is set then instead of appending
    # to the log file, we will replace it.
    my $truncate = 0 ;

    # If log_file_name begins with a '!' then strip this character.  In addition
    # set the truncate flag.
    #if  ($log_file_name =~ /^!(\w*)/)
    if  ($log_file_name =~ /^!(.*)/)
        {
        $log_file_name = $1 ;
        $truncate = 1 ;
        }
        
    # If log_file_name is set to DEFAULT, generate a defualt log file name.
    $log_file_name = create_log_file_name () if ($log_file_name =~ /DEFAULT/i) ;

    # Open the log file.  Just return if this fails
    return unless open_log_file_write ($log_file_name, 0, $truncate) ;
    }

setchatter ($chatter) ;
setdebug ($debug_flag) ;

# Logging can now be enabled.
$c_eml_log_enabled = 1 ;

return
}

#############################################################################

sub stop_logging

{
# Implements the shutdown function from ahlog

# Note.  No return value.  This is non-standard from the rest of the package.

# Close the log file if needed.
close_log_file if $c_eml_use_log_file ;

# Logging can now be disabled.
$c_eml_log_enabled = 0 ;

return ;
}

#############################################################################

1;
