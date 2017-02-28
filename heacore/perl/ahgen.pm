package ahgen ;

use strict ;
use warnings;
use diagnostics;
 
#use ahlog ;

use FindBin ;

# Standard Modules
# use HTML::Strip ;
use File::Spec ;
use File::Spec::Functions qw ( rel2abs catfile ) ;
use File::Basename ;
use IPC::Open3 ;
use Symbol qw( gensym );
use IO::Select ;
use IO::File ;
use File::Copy qw/ move copy /;

BEGIN {
      use Exporter () ;

      our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS) ;

      # set the version for version checking
      #$VERSION = 1.00 ;

      # if using RCS/CVS, this may be preferred
      $VERSION = sprintf "%d.%03d", q$Revision: 1.12 $ =~ /(\d+)/g;

      @ISA = qw(Exporter);
     
      # Functions exported by default.
      @EXPORT = qw();

      %EXPORT_TAGS = (ALL => \@EXPORT_OK ); # eg: TAG => [ qw!name1 name2! ],

      # your exported package globals go here,
      # as well as any optionally exported functions
      @EXPORT_OK = qw(&run_ftool
                      &parse_file_name
                      &get_keyword
                      &set_keyword
                      &delete_keyword
                      &delete_history
                      &get_total_hdu
                      &get_column_names
                      &get_column_num
                      &copy_column
                      &copy_hdu
                      &delete_hdu
                      &create_hdu
                      &get_file_list
                      &check_hdu_exists
                      &add_column
                      &read_column
                      &read_column_vector
                      &read_image
                      &copy_fits_file
                      &merge_fits_file
                      &sort_fits_file
                      &sort_fits_file_all_hdus
                      &compare_fits_files
                      &check_fits_file
                      &get_syspfiles
                      &get_tool_path
                      &get_tool_stdout
                      &get_tool_stderr
                      &get_tool_status
                      &get_error_flag
                      &set_error_flag
                      &set_quiet
                      &update_checksum_and_verify
                      &runTool
                      &formTemporaryFileName
                      &copyFITSFile
                      &copyOrMoveFileBasedOnCleanup
                      &removeOutputFileIfClobbering
                      &readInputFileList
                      &isBadCALDBFileParameterValue
                      &isOptionalFileNotFound
                      &isRequiredFileNotFound
                      &isFileNotFound
                      &isFileCALDB
                      &isFileNone
                      );

      # Import the ahlog library so we use those functions here.

      unshift @INC, catfile dirname (rel2abs (__FILE__)) ;

      require ahlog  ;
      ahlog->import ;

      # Set up some defaults.

      }

# Not sure why this needs to be here.
our @EXPORT_OK;


# non-exported package globals go here
our $debug ;
our $tool_std_output;
our $tool_err_output;

our $PATH ;
our $FTVERSION ;
our $INSTALL_DIR ;
our $COMMAND ;
our $ARGLIST ;
our $NARGS ;
our $SERIOUSNESS ;
our $VERBOSE ;       # should stdout be dumped to the log if no error?
our $LIBS ;
our $BINS ;
our $SYSPFILES ;
our $ENVIRONMENT ;
our $STATUS ;
our $ERRNO ;            
our $ERRMSG ;
our $CORE_DUMPED ;
our $SIGNAL ;
our $HAD_ERROR ;
our $STDOUT_FILE ;
our $CLOBBER_STDOUT_FILE ;
our $IN_BUFFER_SIZE ;
our $OUT_BUFFER_SIZE ;
our $ERR_BUFFER_SIZE ;
our $HEADAS_AVAILABLE ;
our $QUIET ;

our $PGET ;
our $PSET ;
our $PQUERY2 ;
our $PLIST ;



# Set the debug flag to non-zero (preferably 1) to ouput debugging messages.
#$debug = 1;
#$debug = '' ;
$debug = '' ;

$tool_std_output = "" ;
$tool_err_output = "" ;

# then the others (which are still accessible as $Some::Module::stuff)

$PATH = "";
$FTVERSION = 0 ;
$INSTALL_DIR = "" ;
$COMMAND = "" ;
$ARGLIST = {} ;
$NARGS = 0 ;
$SERIOUSNESS = 0 ;         # No idea what this does.
$VERBOSE = 0 ;             # should stdout be dumped to the log if no error?
$LIBS = [] ;               
$BINS = [] ;
$SYSPFILES = [] ;
$ENVIRONMENT = {} ;
$STATUS = 0 ;              # Status after running the last FTOOL (0 = SUCCESS)
$ERRNO = 0 ;               # FTOOL/CFITSIO Error Number
$ERRMSG = 0 ;              # ERROR Message appropiate to the last error recieved.
$CORE_DUMPED = 0 ;    
$SIGNAL = 0 ;              # Not sure why this is here.  FTOOLS should not need to handle signals.
$HAD_ERROR = 0 ;           # Do we really need this.  Errors will be handled by status.
$STDOUT_FILE = "" ;        # Not likely to be used.
$CLOBBER_STDOUT_FILE = 1;  # Not likely to be used.
$IN_BUFFER_SIZE = 8192 ;   # Not sure if needed.
$OUT_BUFFER_SIZE = 4096 ;  # 4096
$ERR_BUFFER_SIZE = 4096 ;  # 4096
$HEADAS_AVAILABLE = 1 ;    # Flag to indicate if we found a HEADAS installation.
$QUIET = 0;


$PGET = "" ;               # Path to pget tool.  Must exist.
$PSET = "" ;               # Path to pset tool.  Must exist.
$PQUERY2 = "" ;            # Path to pquery2 tool.  Must exist.
$PLIST = "" ;              # Path to plist tool.  Must exist.

# all file-scoped lexicals must be created before
# the functions below that use them.
# file-private lexicals go here

# Initialize constants

# Make sure HEADAS is available.

# Get the path to the HEADAS installation.  This should be in the environment
# variable HEADAS.
$INSTALL_DIR = $ENV{HEADAS} ;

# Check to make sure that the HEADAS directory is defined and actually exists.
# if it is not, then we will set the HEADAS_AVAILABLE flag to false and skip
# the rest of the HEADAS_SPECIFIC installation.  
unless ($INSTALL_DIR)
    {
    $ERRMSG = "Environment variable HEADAS must be defined before calling " .
              "this package." ;

    # Set $STATUS to indicate an error.
    $STATUS = -1 ;

    # Set the HEADAS_AVAILABLE flag ;
    $HEADAS_AVAILABLE = 0 ;
    }

unless ($HEADAS_AVAILABLE && -d $INSTALL_DIR)
    {
    $ERRMSG = "Directory specified in environment variable HEADAS " .
              "($INSTALL_DIR) does not exist or is not a directory." ;

    # Set $STATUS to indicate an error and return.
    $STATUS = -1 ;

    # Set the HEADAS_AVAILABLE flag ;
    $HEADAS_AVAILABLE = 0 ;
    }

# Foward declaration p
sub find_ftool ;

if  ($HEADAS_AVAILABLE)
    {

    # Set up the bin path array.
    $BINS->[0] = "$INSTALL_DIR/bin" ;
    $BINS->[1] = "$INSTALL_DIR/scripts" ;

    # Set up the lib path array
    $LIBS->[0] = "$INSTALL_DIR/lib" ;

    # Set up the syspfiles path array
    $SYSPFILES->[0] = "$INSTALL_DIR/syspfiles" ;


    # Set the FTOOLS environment variables
    $ENVIRONMENT->{FTOOLS} = $INSTALL_DIR ;
    $ENVIRONMENT->{LHEASOFT} = $INSTALL_DIR ;
    $ENVIRONMENT->{LHEA_DATA} = "$INSTALL_DIR/../refdata" ;
    $ENVIRONMENT->{LHEA_HELP} = "$INSTALL_DIR/../help" ;
    $ENVIRONMENT->{PGPLOT_FONT} = "$INSTALL_DIR/lib/grfont.dat" ;

    $ENVIRONMENT->{LHEAPERL} = $^X ; # <- perl version running this script

    $ENVIRONMENT->{FTOOLSINPUT } = "stdin" ;
    $ENVIRONMENT->{FTOOLSOUTPUT} = "stdout" ;

    $ENVIRONMENT->{TCLRL_LIBDIR} = "$INSTALL_DIR/lib" ;

    # Check if FVERSION is exists and is excutable.  If it does, then run it 
    # get the current FTOOLS version.
    my $path = find_ftool ("fversion") ;

    if  ($path)
        {
        # Get the version and release date of the FTOOLS installation
        my $verstring = qx/$path/ ;

        # Extract the actual FTOOLS version number from the version string.
        $verstring =~ /V(\d+\.\d+)/ ;

        # Set FTVERSION.  Multiply by 100 for easier comparisons.
        $FTVERSION = $1 * 100 ;
        }
    }

#############################################################################

sub find_ftool

# This routine searches the array of bin directories for the specified FTOOL.
# If it finds the FTOOL, it returns a complete path to it.  Otherwise, it
# returns an empty string.

{
my $ftool = shift ;

my $path ;

# Loop through each of the bin directories looking for the FTOOL.
foreach (@{$BINS})
    {
    # Add the tool name to the end of the directory path.
    $path = File::Spec->catfile ($_, $ftool) ;

    # If the FTOOL exist, then return the complete path to the FTOOL.
    return $path if -e $path && -x $path ;    
    }

# Could not find what we were looking for.  Return an empty string.
set_error_flag(1);
return "" ;

}

#############################################################################

sub find_par_file

# This routine searches the array of bin directories for the specified FTOOL
# parameter file.  If it finds the file, it returns a complete path to it.  
# Otherwise, it returns an empty string.

{
my $ftool = shift ;

my $path ;

# Loop through each of the bin directories looking for the FTOOL.
foreach (@{$SYSPFILES})
    {
    # Add the tool name to the end of the directory path.
    $path = File::Spec->catfile ($_, $ftool) ;

    # If the FTOOL exist, then return the complete path to the FTOOL.
    return $path if -e $path && -x $path ;    
    }

# Could not find what we were looking for.  Return an empty string.
set_error_flag(1);
return "" ;

}


#############################################################################

sub run_ftool

# This subroutine presents a tempory interface to allow the running of FTOOLS
# It is expected that this will be replaced by something more robust.

{
my $ftool = shift ;

# Check for HEADAS.  If its not there, then return the current status.
unless ($HEADAS_AVAILABLE)
    {
    return $STATUS ;
    }

# Clear the output buffers.
$tool_err_output = "" ;
$tool_std_output = "" ;

# Create the command to run the ftool
my $cmd = "$ftool" ;

# Add in any parameters which are needed to run the FTOOL
foreach (@_)
    {
    # Add in the next parameter.
    $cmd = $cmd . " '$_'" if (length($_) > 0);
    }
    
# Make sure the pget tool exists.
unless ($PGET = find_ftool ("pget"))
    {
    $ERRMSG = "Could not locate PGET tool." ;

    $STATUS = -1 ;

    return $STATUS ;
    }

# Make sure the pset tool exists.
unless ($PSET = find_ftool ("pset"))
    {
    $ERRMSG = "Could not locate PSET tool." ;

    $STATUS = -1 ;

    return $STATUS ;
    }

# Make sure the pquery2 tool exists.
unless ($PQUERY2 = find_ftool ("pquery2"))
    {
    $ERRMSG = "Could not locate PQUERY2 tool." ;

    $STATUS = -1 ;

    return $STATUS ;
    }

# Make sure the plist tool exists.
unless ($PLIST = find_ftool ("plist"))
    {
    $ERRMSG = "Could not locate PLIST tool." ;

    $STATUS = -1 ;

    return $STATUS ;
    }

ahlog::ah_out "\nFTOOLS CMD: " . $cmd ;

open COMMAND_FILEHANDLE, "$cmd 2>&1 |";
while (<COMMAND_FILEHANDLE>) {
  $tool_std_output .= $_;
  ahlog::ah_out $_ unless $QUIET;
} 
close COMMAND_FILEHANDLE;


# Get the exit status of the FTOOL we just ran.
if($? == 0) {
  $STATUS = 0 ;
  ahlog::ah_out "" unless $tool_std_output eq "";
} elsif($? == -1) {
  ahlog::ah_err "Failed to execute tool $ftool : \$! = $!";
  $STATUS = $? ;
} elsif ($? & 127) {
  my $coredump = sprintf "%d, %s coredump",($?&127),($?&128)?'with':'without';
  ahlog::ah_err $coredump;
  $STATUS = $?&127;
} else {
  my $other_return = sprintf "%d", $? >> 8;
  ahlog::ah_err "Command failed : returned value $other_return" ;
  $STATUS = $? >> 8;
}

# Remove End-of-Lines from the output (if it is there).
chomp $tool_std_output ;
#chomp $tool_err_output ;

# Were done.  Return tool exit status value (NOT the FTOOL status).
return  $STATUS;
}

#############################################################################


sub parse_file_name

# Decompose a string that possibly contains a file name, HDU extesnsion and 
# leading '!' to indicate that the file should be overwritten.

{
my $str = shift ;



# Create an array to return as the result of the fucntion.
# Array elements will have the following meaning.
# 0 - File Name
# 1 - HDU extension
# 2 - Overwrite Flag
my @out = ("", "", 0) ;

# Parse the string to extract filename, HDU extesnion, and leading '!' 
# character.
 
# Filenames are expected to have following format:
# <!> filename <[ddd]> or <!> filename.fits <+ddd>   
# (elements enclosed in brackes <> are optional)
# (ddd is any string)
if  ($str =~ /^(!?)([\w\-\/+.]+)\+(\d+)/  || $str =~ /^(!?)([\w\-\/+.]+)(\[.*\])?/)
    {
    $out [0] = $2 ;
    # print "filename = ", $2, "\n" ;

    $out [2] = ($1 eq '!')? 1 : 0 ;

    my $ext = $3 ;

    if  (defined $ext)
        {
        $ext =~ s/[\[\]]//g ;
        $out [1]  = $ext ;
        }
    }

return @out ;
}

#############################################################################

sub get_keyword

{
my $file      = shift ;
my $extension = shift ;
my $kword     = shift ;

# Use the ftkeypar tool to get the value of a keyword.

my $val ;

# Should check to make sure we got passed an actual keyword name.

# Create a command to use ftkeypar to get the value of the of the requested
# keyword.
my $cmd = "ftkeypar $file\\[$extension\\] $kword" ;

# Use ftkeypar to get the value for the requested keyword.
qx/$cmd/ ;

# Get the datatype of the parameter that we just queried.
my $datatype = qx/pget ftkeypar datatype/ ;

# chomp it to remove end of lines.
chomp $datatype ;


# Get the parameter value based on the datatype.
if  ($datatype eq 'string') 
    {
    # Use pget to get the keyword value from the par file.
    $val = qx/pget ftkeypar svalue/ ;
    }

elsif ($datatype eq 'integer')
    {
    # Use pget to get the keyword value from the par file.
    $val = qx/pget ftkeypar ivalue/ ;
    }
elsif ($datatype eq 'real')
    {
    # Use pget to get the keyword value from the par file.
    $val = qx/pget ftkeypar rvalue/ ;
    }
# Make sure that we actually got a value.  If we didn't, then return undef.
if(!defined $val) { set_error_flag(1) ; }
return undef unless defined $val ; 

# chomp it to remove end of lines.
chomp $val ;

# print "VAL: $val\n" ;    

return $val ;

}

#############################################################################

sub set_keyword

{
my $file      = shift ;
my $extension = shift ;
my $kword     = shift ;
my $val       = shift ;
my $comment   = shift ;

# Use the fthedit tool to set the value of a keyword.

# Should check to make sure we got passed an actual keyword name and value.

# Create a command to use fthedit to get the value of the of the requested
# keyword.

# Check if ascii keyword file is being used to update keywords, otherwise
# edit keyword directly
if($kword =~ /^@/ ) {
  if( ! -e substr($kword,1)) { 
    set_error_flag(1); 
  } else {
    $STATUS = run_ftool ("fthedit", "$file\[$extension\]", $kword, "a") ;
  }
} else {
  if ( defined $comment ) {
    $STATUS = run_ftool ("fthedit", "$file\[$extension\]", $kword,  "a", $val, "comment=$comment") ;
  } else {
    $STATUS = run_ftool ("fthedit", "$file\[$extension\]", $kword,  "a", $val) ;
  }
}

return $STATUS ;

}

#############################################################################

sub delete_keyword

# Delete the requested keyword from the requested HDU.

{
my $file      = shift ;
my $extension = shift ;
my $kword     = shift ;

my $tmp = $file . "[" . $extension . "]" ;

return run_ftool ("fthedit", $tmp, $kword, "delete") ;
}

#############################################################################

sub delete_history

# Delete all history keywords from the requested HDU.

{
my $file      = shift ;
my $extension = shift ;

my $tmp = $file . "[" . $extension . "]" ;

return run_ftool ("fthedit", $tmp, "HISTORY", "deleteall") ;
}

#############################################################################

sub get_total_hdu

{
my $file = shift ;

# This function may eventually be expanded to get more information about
# the target fits file.

my $val ;

# Use the fstruct ftool to find some basic information about the FITS file.
#qx/fstruct $file/ ; 

# Use ftlist to find the number HDUs.  No clean way to do this.
$val = qx/ftlist $file H | wc -l/ ;

# Use pget to get the number of HDUs from the par file.
# $val = qx/pget fstruct totalhdu/ ;

# chomp it to remove end of lines.
chomp $val ;

# subtract 3 to adjust HDU count so that header lines are not counted
$val -= 3 ;

return $val ;
}

#############################################################################

sub get_column_names

{
my $file      = shift ;
my $extension = shift ;

my @out ; 

# Stupid Perl script trick.  Force the extension to 0 if not given.
$extension = 0 unless $extension ;

# Merge the extension with the name of the FITS file.
my $fn_ext = "$file\[$extension\]" ;

# Run the tool with nothing printed to screen or log file
# This will prevent unnecessary logging of column names
set_quiet(1);

# Run the tool.
$STATUS = run_ftool ("ftlist", $fn_ext, "c", "rownum=No", "colheader=No") ;

# Reset the quiet-ness
set_quiet();

# Check that ftlist did not return an error 
return undef if $STATUS ;

# split the standard output of the tool (the list of values for a particular 
# column) into an array. 
my @buffer = split /^/m, $tool_std_output ;

# remove the top three lines
splice @buffer, 0, 3 ;

# Decompose line to get information the column
foreach (@buffer)
    {
    if  (/\s*\d+\s*(\w+)/)
        {
        push @out, $1 ;
        }
    }

return @out ;
}

#############################################################################

sub get_column_num

{
my $file      = shift ;
my $extension = shift ;
my $colname = shift ;

my @out ; 

my $colnum=0;

# Stupid Perl script trick.  Force the extension to 0 if not given.
$extension = 0 unless $extension ;

# Merge the extension with the name of the FITS file.
my $fn_ext = "$file\[$extension\]" ;

# Run the tool with nothing printed to screen or log file
# This will prevent unnecessary logging of column names
set_quiet(1);

# Run the tool.
$STATUS = run_ftool ("ftlist", $fn_ext, "c", "rownum=No", "colheader=No") ;

# Reset the quiet-ness
set_quiet();

# Check that ftlist did not return an error 
return 0 if $STATUS;

# split the standard output of the tool (the list of values for a particular 
# column) into an array. 
my @colline = split('\n',$tool_std_output);
foreach(@colline){
  if($_ =~ /$colname/i) {
    my @column = split(" ", $_); 
    if(uc $column[1] eq uc $colname) {
      $colnum = $column[0]; 
      last;
    }
  }
}

# If there was no column, set return value to 0
$colnum = 0 unless $colnum ;

return $colnum ;

}

#############################################################################

sub copy_column

# Copy column(s) to specified HDU of a FITS file or to another FITS file.

# If the target FITS file does not exist it will be created.

{
my $source      = shift ;
my $extension   = shift ;
my $target      = shift ;
my @cols        = @_ ;

# The variable $input_spec should give the name of the input file,
# HDU and columns that will be copied/appended.
my $input_spec = "$source\[$extension\]" ;

if  (scalar @cols)
    {
    # Prepend the column specifier.
    $input_spec = $input_spec . "[col " ;

    # Add in all column names
    my $first = 1 ;

    foreach (@cols)
        {
        $input_spec = $input_spec . "; " unless $first ;

        $input_spec = $input_spec . $_ ;

        $first = 0 if $first ;
        } 

    # Close out the list of columns
    $input_spec = $input_spec . "]" ;
    }

# Check if the target exists, if does will use FTPASTE, otherwise we will use
# FTCOPY to create a new file
if  (-e $target)
    {
    $STATUS = run_ftool ("ftpaste", $input_spec , $target, $target, "copyall=no", "clobber=yes" )  ;
    return $STATUS;
    }

else
   {
   $STATUS = run_ftool ("ftcopy", $input_spec, $target, "copyall=no", "clobber=yes" ) ;
   return $STATUS;
   }  

}

#############################################################################

sub copy_hdu

# Copy a HDU to the end of a FITS file or to another FITS file.

# If the target FITS file does not exist it will be created.

{
my $source      = shift ;
my $extension   = shift ;
my $target      = shift ;
my @cols        = @_ ;

# The variable $input_spec should give the name of the input file,
# HDU and columns that will be copied/appended.
my $input_spec = "$source\[$extension\]" ;

if  (scalar @cols)
    {
    # Prepend the column specifier.
    $input_spec = $input_spec . "[col " ;

    # Add in all column names
    my $first = 1 ;

    foreach (@cols)
        {
        $input_spec = $input_spec . "; " unless $first ;

        $input_spec = $input_spec . $_ ;

        $first = 0 if $first ;
        } 

    # Close out the list of columns
    $input_spec = $input_spec . "]" ;
    }

# Check if the target exists, if does will use FTAPPEND, otherwise we will use
# FTCOPY
if  (-e $target)
    {
    $STATUS = run_ftool ("ftappend", $input_spec , $target )  ;
    return $STATUS;
    }

else
   {
   $STATUS = run_ftool ("ftcopy", $input_spec, $target, "copyall=no" ) ;
   return $STATUS;
   }  

}

#############################################################################

sub delete_hdu

# Delete a HDU from a FITS file.

{
my $source      = shift ;
my $extension   = shift ;

set_quiet(1);
# Verify that HDU exists in source file
if( 0 == check_hdu_exists( $source, $extension ) ) 
{
ahlog::ah_info "HIGH", "HDU $extension does not exist, no need to delete.";
set_quiet();
set_error_flag();
return 0;
}
set_quiet();

# The variable $input_spec should give the name of the input file,
# HDU and columns that will be copied/appended.
my $input_spec = "$source\[$extension\]" ;

$STATUS = run_ftool ("ftdelhdu", $input_spec, "none", "confirm=yes" ) ;
return $STATUS;

}

#############################################################################

sub create_hdu

# Create a new HDU at the end of a FITS file.

{
print "This routine is a stub\n" ;

return ;
}

#############################################################################

sub  get_file_list

# return array of filenames
#   if input is an empty string, return an empty list
#   if input is a FITS file, then it is the only element in the output
#   if input is an @filelist, return contents of input (one filename per line)
#   if input is a comma delimited list of files, return an array

{
my $infile = shift ;
  
my $isfilelist = 0 ;
my @out ;

# Check to make sure we got at least one parameter.  If then nothing was passed,
# then assume an empty string.
$infile = "" unless defined $infile ;

# Remove whitespace
$infile =~ s/\s//g ;
  
# Check for an empty string.  Return an empty list in this case.
return @out if $infile eq "" ; 

# Check if the leading character is the  '@' symbol.  This indicates a file list. 
# If infile does begin with the '@' symbol, then remove it.
$isfilelist = ($infile =~ s/^@//) ? 1 : 0 ;

# Not a file list.  We assume this refers to single file.
unless  ($isfilelist) 
    {
    # Split the infile parameter on comma
    @out = split ",",$infile ;

    return @out;
    }

# Its a file list

# Try to open the file
my $ok = open (FH, $infile) ;

# Check for errors
unless ($ok) 
    {
    $tool_err_output = "get_file_list: Errors detected trying to open $infile\n$!" ;

    set_error_flag(1);

    return undef ;
    }

# Read the file contents and add each line the the @out array.
while( <FH> ) 
    {
    chomp;

    push (@out, $_) if $_ ;
    }

close FH;

# Were done.
return @out;
}

#############################################################################

sub check_hdu_exists

# Check that the requested HDU exists in the FITS file.  This returns number
# of the HDU.  0 indicates that the HDU does not exist.

{
my $fn    = shift ;
my $hdu   = shift ;

# Use FTLIST to get a list HDUs in the FITS file
# my $status = run_ftool ("ftlist", $fn_ext, "T", $col, $range, "rownum=\"No\"", "colheader=\"No\"") ;
set_quiet(1);
$STATUS = run_ftool ("ftlist", $fn, "H") ;
set_quiet();

# Check the status.  If the call fails then just return.
return 0 if $STATUS ;

# Split the output from the ftlist tool into an array of lines.
my @out = split '\n', $tool_std_output ;

# Loop through each line in the output looking for a matching HDU
foreach (@out)
    {
    # Extract the HDU number and name.  
    if  (/^HDU\s+(\d+)\s+(\S+)/)
        {
        return $1 if $2 eq $hdu ;        
        }
    } 

# Could not find what we were looking for. Set status and return 0.
set_error_flag(1);

return 0 ;
}

#############################################################################

sub add_column

# Create a copy of a fits file.

{
my $file      = shift ;
my $extension = shift ;
my $column    = shift ;
my $val       = shift ; 
my $tform     = shift ; 

# Force $val to 0 if undefined.
$val = 0 unless defined $val ;

# Force $tform to 'A' if undefined.
$tform = 'A' unless defined $tform && $tform ;

# Merge the extension with the name of the FITS file.
my $tmp1 = $file . "[" . $extension . "]" ;

# Check if the column name is also a keyword.  If it is, then we will temporarily
# Remove it until after we have added the column to the FITS file.  Once the new
# column has been added, we will replace the keyword.

my $column_name_is_kw = 0 ;
my $kw_value = "" ;

# Get the value of any keyword that also has the same as the column we want to add.
# Make sure we can get its value so that we can restore it later.
$kw_value = get_keyword ($file, $extension, $column) ;

$column_name_is_kw = 1 if defined $kw_value ;

# If keyword exists with the same name as the column we are adding, then delete
# that keyword.
if  ($column_name_is_kw)
    {
    ahlog::ah_debug "Column to be added has the same name as already existing keyword." ; 
    ahlog::ah_debug "Keyword $column will be deleted and then restored after the column " .
             "is added." ;

    if  (delete_keyword ($file, $extension, $column))
        {
        $tool_err_output = "Could not delete keyword $column which duplicates " .
                            "name of requested column to add.\n" .
                             $tool_err_output ;

        set_error_flag(1);

        return $STATUS ;
        }
    }

# Add escaped quotes around val so that the FTOOL knows that it is a constant.
# example: '\'Column_Value\''.  But only do this if the column we are creating is
# an alphnumeric string.  
$val = "'\\'" . $val . "\\''" if $tform =~ /^A/i ;

$tform = "tform=" . $tform ;

# Run FTCALC.  Save the status to return as the result of this function.
my $STATUS = run_ftool ("ftcalc", $tmp1, $file, $column, $val, $tform, "clobber=YES") ;

# If we previously removed a keyword, then restore it now.
if  ($column_name_is_kw)
    {
    set_keyword ($file, $extension, $column, $kw_value) ;
    }


return $STATUS ;
}

#############################################################################

sub read_column

# Read values from a column of a FITS table

{
my $file      = shift ;
my $extension = shift ;
my $column    = shift ;
my $rowsmin   = shift ;
my $rowsmax   = shift ;

# Stupid Perl script trick.  Force the extension to 0 if not given.
$extension = 0 unless $extension ;

# Merge the extension with the name of the FITS file.
my $fn_ext = "$file\[$extension\]" ;

# Construct a row range based on whether the user specified a range of rows
# that they wished to be returned or just wanted all of the rows (default).

# Note rows begin with row 1.

my $rowrange = "-" ;

if    ($rowsmax && $rowsmin)
      {
      $rowrange = "$rowsmin-$rowsmax" ;
      }

elsif ($rowsmin)
      {
      $rowrange = "$rowsmin" ;
      }

ahlog::ah_info "LOW", "Row Range is: $rowrange\n" if $rowrange ne "-";
ahlog::ah_info "LOW", "Column being read: $column\n" ;
ahlog::ah_info "LOW", "Filename / Extension $fn_ext \n" ;

my $range = "rows=$rowrange" ;

my $col = "columns=$column" ;
 
# Run the tool.
set_quiet(1);
$STATUS = run_ftool ("ftlist", $fn_ext, "T", $col, $range, "rownum=No", "colheader=No") ;
set_quiet(0);

# Check for an error
return undef if $STATUS ;

# split the standard output of the tool (the list of values for a particular 
# column) into an array. 
my @buffer = split /^/m, $tool_std_output ;


my @out ;

# Remove line-breaks and white space.  Add non-empty lines to the output array
foreach (@buffer)
    {
      chomp $_;
      $_ =~ s/^\s+// ;
      $_ =~ s/\s+$// ;
    push (@out, $_) if length($_)>0 ;   
    }

ahlog::ah_info "LOW", "Read " . scalar @out . " from column $column " ;
# Were done.
return @out ;   
}


#############################################################################

sub read_column_vector

# Read values from a column of a FITS table

{
my $file      = shift ;
my $extension = shift ;
my $column    = shift ;
my $vector    = shift ;
my $rowsmin   = shift ;
my $rowsmax   = shift ;

# Stupid Perl script trick.  Force the extension to 0 if not given.
$extension = 0 unless $extension ;

# vector should be 0 if not specified.
$vector = 0 unless $vector ;

# Merge the extension with the name of the FITS file.
my $fn_ext = "$file\[$extension\]" ;

# Construct a row range based on whether the user specified a range of rows
# that they wished to be returned or just wanted all of the rows (default).

# Note rows begin with row 1.

my $rowrange = "-" ;

if    ($rowsmax && $rowsmin)
      {
      $rowrange = "$rowsmin-$rowsmax" ;
      }

elsif ($rowsmin)
      {
      $rowrange = "$rowsmin" ;
      }

ahlog::ah_info "LOW", "Row Range is: $rowrange\n" ;
ahlog::ah_info "LOW", "Column being read: $column\n" ;
ahlog::ah_info "LOW", "Filename / Extension $fn_ext \n" ;

my $range = "rows=$rowrange" ;

my $col = "columns=$column" ;

my $vec = "vector=$vector" ;
 
# Run the tool.
set_quiet(1);
$STATUS = run_ftool ("ftlist", $fn_ext, "T", $col, $range, $vec, "rownum=No", "colheader=No") ;
set_quiet();

# Check for an error
return undef if $STATUS ;

#$tool_std_output =~ s/\n//mg ;

#for (my $kk = 0 ; $kk < 60 ; $kk++) 
#    {
#    print "$kk\n" ;
#    }

# split the standard output of the tool (the list of values for a particular 
# column) into an array. 
my @buffer = split /^/m, $tool_std_output ;

ahlog::ah_info "LOW", "Read " . scalar @buffer . " from column $column " ;

my @out ;

# Remove line-breaks and white space.  Add non-empty lines to the output array
foreach (@buffer)
    {
    s/\r?\n//g ;
    push (@out, $_) if  $_ ;   
    }

# Were done.
return @out ;   
}

#############################################################################

sub read_image

# Read values from a FITS image

{
my $file      = shift ;
my $extension = shift ;
my $colmin    = shift ;
my $colmax    = shift ;
my $rowmin    = shift ;
my $rowmax    = shift ;

# Force the extension to 0 if not given.
$extension = 0 unless $extension ;

# Merge the extension with the name of the FITS file.
my $fn_ext = "$file\[$extension\]" ;

my $hasrowrange = 0;
my $hascolrange = 0;

# +++ should add error checking this this file really is an image


# Construct a section range based on whether the user specified a range of rows
# and columns that they wished to be returned or just wanted all of the data 
# (default).

# +++ for images, I think we need a full range or no range (but I haven't 
# played with ftlist to verify).  add error checking

# Note rows begin with row 1.
my $rowrange = "-" ;
if ($rowmax && $rowmin) {
  $hasrowrange = 1 ;
  $rowrange = "$rowmin:$rowmax" ;
} else {
  $hasrowrange = 0 ;
}
      
# Note cols begin with row 1.
my $colrange = "-" ;
if ($colmin && $colmax) {
  $hascolrange = 1 ;
  $colrange = "$colmin:$colmax" ;
} else {
  $hascolrange = 0 ;
}

ahlog::ah_info "LOW", "Filename and Extension $fn_ext \n" ;
ahlog::ah_info "LOW", "Row range is: $rowrange \n" if $rowrange ne "-";
ahlog::ah_info "LOW", "Column range is: $colrange \n" ;

my $section = "";
if ($hascolrange) {
  $section = "section=$colrange" ;
}
if ($hascolrange && $hasrowrange) {
  $section .= "," ;
}
if ($hasrowrange) {
  $section .= "$rowrange" ;
} 

# Run the tool.
set_quiet(1);
$STATUS = run_ftool ("ftlist", $fn_ext, "I", $section, "rownum=No", "colheader=No") ;
set_quiet(0);

# Check for an error
return undef if $STATUS ;

# split the standard output of the tool (the list of values for a particular 
# column) into an array. 
my @buffer = split /^/m, $tool_std_output ;

my @out ;

# Remove line-breaks and white space from start and end of lines
# Add non-empty lines to the output array
# note that the rows are reversed.  ftlist prints them in descending order,
# but the output array should have them in ascending order
my $numLines = scalar @buffer;
for (my $iLine = $numLines - 1 ; $iLine >= 0 ; --$iLine) {

  my $arrayPosition = $numLines - $iLine - 1;

  my $currLine = $buffer[$iLine];
  chomp $currLine;
  $currLine =~ s/^\s+// ;
  $currLine =~ s/\s+$// ;
  next if length($currLine)==0 ;  # +++ this may mess up the array numbering
  
  # now split the current row of data, based on spaces
  my @insidebuffer = split /[\s]+/m, $currLine ;
  my $numElements = scalar @insidebuffer;
  for (my $iElement = 0 ; $iElement < $numElements ; ++$iElement) {
    $out[$iElement][$arrayPosition] = $insidebuffer[$iElement];
  }
}

ahlog::ah_info "LOW", "Read " . scalar @out . " from $fn_ext " ;

return @out ;   

}

#############################################################################

sub copy_fits_file

# Create a copy of a fits file.

{
$STATUS = run_ftool ("ftcopy", @_, "clobber=YES") ;
return $STATUS ;
}

#############################################################################

sub merge_fits_file

# Merge one or more HDUs from a FITS file.

{
# Find the number of parameters that was passed to us. 
my $nparam = scalar @_ ;

# Make sure that we got at least three parameters.  
# (target, copyall and nfiles must always be passed)
if  ($nparam < 3)
    { 
    $ERRMSG = "Function merge_fits_file called with too few parameters." ;

    set_error_flag (-1) ;

    return $STATUS;
    }

# Pull off the target, copyall and nfiles parameter.
my $target      = shift ;
my $copyall     = shift ;
my $nfile       = shift ;

$nparam -= 3 ;

# Make sure that we have at least two files to merge.
if  ($nfile < 2)
    { 
    $ERRMSG = "Must have at least two files to merge." ;

    $STATUS = -1;

    return $STATUS;
    }

# Make sure we have enough parameters left to describe all the files
# that will be merged.  Each file requires two parameters, a file name and
# the name of an extension.
if  ( $nparam < $nfile * 2 ) 
    { 
    $STATUS = "Not enough parameters were passed in order for $nfile files " .
              "to be merged." ;

    $STATUS = -1;

    return $STATUS;
    }

ahlog::ah_debug "About to merge: $nfile files" ;

my $lst = "" ;
my $col = "" ;

# Create a string that consists of a comma seperated list of file names and
# extensions to merge.
my $fn ;
my $hdu ;

while  ($nfile)
    {
    $fn  = shift ;
    $hdu = shift ;
    
    $lst = $lst . $fn . "["  . $hdu . "]," ;

    $nfile-- ;
    }

chop $lst ;

ahlog::ah_debug "List of files to merge: $lst" ;

# If any parameters are left, they will be used to specify specific
# columns that will be merged.
if  (@_)
    {
    $col = "columns=" ;

    foreach (@_) {$col = $col . $_ . ","}

    chop $col ;
    }

# Otherwise, merge all columns
else 
    {
    $col = 'columns=*'
    }
         
# Create a string to set the copyall flag correctly
my $cpstr = ($copyall) ? "copyall=YES" : "copyall=NO" ;

# Run the ftmerge FTOOL.
$STATUS = run_ftool ("ftmerge", $lst, $target, $cpstr, "clobber=YES", $col) ;
return $STATUS ;
}

#############################################################################

sub sort_fits_file 

# Sort a FITS file HDU

{
my $source      = shift ;
my $extension   = shift ;
my $target      = shift ;
my $column      = shift ;
my $unique      = shift ;

# Force the unique flag to false if not defined.
$unique = 0 unless defined $unique ;

my $tmp = $source . "[" . $extension . "]" ;

# Create a string to set the unique flag correctly
my $uqstr = ($unique) ? "unique=YES" : "unique=NO" ;

$STATUS = run_ftool ("ftsort", $tmp, $target, $column, $uqstr, "clobber=YES") ;
return $STATUS ;
}

#############################################################################

sub sort_fits_file_all_hdus

# Sort a FITS file.  Sort every HDU which has the desired column and nonzero
# rows

{
my $source      = shift ;
my $outfile     = shift ;
my $column      = shift ;
my $unique      = shift ;

# Force the unique flag to false if not defined.
$unique = 0 unless defined $unique ;

# Create a string to set the unique flag correctly
my $uqstr = ($unique) ? "unique=YES" : "unique=NO" ;

# loop through the extensions
my $numHDU = get_total_hdu($source);
ahlog::ah_debug "File $source has $numHDU HDUs";

# start at 1 (not the primary, but the first extension)
# go up to last extension
for (my $iExt = 1 ; $iExt < $numHDU ; ++$iExt ) {

  # skip if this ext has zero rows
  if (ahgen::get_keyword($source, $iExt, "NAXIS2") == 0) {
    ahlog::ah_debug "HDU $iExt in $source has zero rows; skipping";
    next;
  }
  
  # skip if this column isn't in this extension
  my @columns = get_column_names($source,$iExt);
  my @colmatches = grep { /^$column$/ } @columns;
  unless (@colmatches) {
    ahlog::ah_debug "HDU $iExt in $source does not have column $column; skipping";
    next;
  }

  # sort this extension
  my $fullsource = $source . "[" . $iExt . "]" ;
  $STATUS = run_ftool ("ftsort", $fullsource, $outfile, $column, $uqstr, "clobber=YES") ;
  if ($STATUS) {
    ahlog::ah_err "Could not run ftsort on FITS file: $fullsource.";
    return $STATUS ;
  }

} # end-loop through extensions

return $STATUS ;

}

#############################################################################

sub compare_fits_files

# Used to compare two FITS files to check if they duplicate the same information.
# Returns the number differences between the files if possible.  Otherwise returns
# undef.

{
my $filea      = shift ;
my $fileb      = shift ;

$STATUS = run_ftool ("ftdiff", $filea, $fileb) ;

# Get the last line of standard output.
$tool_std_output =~ /(.*)$/ ;
   
my $ll = $1 ;

# See if we can find the number of differences in this line.
if  ($ll =~ /comparison:\s*(-?\d+)/)
    {
    return $1 ;
    }

# Could not parse the final line.  Return undef.
else 
    {
    set_error_flag(-1);
    return undef ;
    }
}

#############################################################################

sub check_fits_file

# Use ftverify to make sure a file conforms to the FITS standard.

{
my $source      = shift ;

$STATUS = run_ftool ("ftverify", $source, "errreport=e", "prstat=no") ;

# Check if $source is a FITS file by examining the output of 
if  ($tool_std_output =~ /FAILED/)
    {
    set_error_flag(-1);
    return 0 ;
    }

else
    {
    return 1 ;
    }
}

#############################################################################

sub get_syspfiles

# Return the directory containing the "system" parfiles.

{
print "This routine is a stub\n" ;

return ;
}

#############################################################################

sub get_tool_path

# Return the path to the requested FTOOL.  If the tool does not exist then
# an empty string is returned.

{
print "This routine is a stub\n" ;

return ;
}

#############################################################################

sub get_tool_stdout

# Get the standard output text from the last FTOOL that was run.

{
return $tool_std_output;
}

#############################################################################

sub get_tool_stderr

# Get the standard error text from the last FTOOL that was run.

{

return $tool_err_output ;
}

#############################################################################

sub get_tool_status

# Get the status of the last FTOOL that was run.

{
return $ERRNO;
}

#############################################################################

sub get_error_flag

# Get the error status flag.  The error status flag will be set whenever an
# an error is detected running an FTOOL.

{
return $STATUS;
}

#############################################################################

sub set_error_flag

# Set the error status flag.  The error status flag will be set whenever an
# an error is detected running an FTOOL.

# Calling this subroutine with no parameters will clear the ERROR flag.  
# Calling it with any TRUE value will set the error flag.

{
# Read parameters.
my $error = shift;

# Make sure $error was defined.  If it wasn't, then we will clear the flag.
$error = 0 unless defined $error ;

# Force it 1/0 and save the new status
$STATUS = ($error)? $error : 0 ;

# Were done.
return $STATUS;
}

#############################################################################

sub set_quiet

# Don't print anything to stdout, stderr or to the log screen
# Useful if need to run something with a lot of output, but only
# need a subset of information

# Calling this subroutine with no parameters will clear the ERROR flag.  
# Calling it with any TRUE value will set the error flag.

{
# Read parameters.
my $is_quiet = shift;

# Make sure $error was defined.  If it wasn't, then we will clear the flag.
$is_quiet = 0 unless defined $is_quiet ;

# Force it 1/0 and save the new status
$QUIET = ($is_quiet)? 1 : 0 ;

# Were done.
return $QUIET;
}

#############################################################################

sub update_checksum_and_verify

# run ftools, ftchecksum and ftverify (check_fits_file), on file.
# These operations are performed after creating or modifying a FITS file.

{
  my $filename=shift;      # file to operate on

  # Update the checksum
  $STATUS = run_ftool ("ftchecksum", $filename, "update=yes") ;
  if  ($STATUS) {
    ahlog::ah_err "Could not update checksum in FITS file: $filename.";
    ahlog::ah_err get_tool_stderr ();
    return $STATUS;
  }
  
  # Check that the FITS file is valid.
  unless (check_fits_file ($filename)) {
    ahlog::ah_err "FITS file $filename failed FITS verification test." ;
    set_error_flag(1);
    return $STATUS;
  }

}

###################
#  Miscellaneous  #
###################

sub isFileNone {
  my $filename=shift(@_);
  if (lc $filename eq "none") { return 1; }
  return 0;
}

# ------------------------------------------------------------------------------

sub isFileCALDB {
  my $filename=shift(@_);
  if (lc $filename eq "caldb") { return 1; }
  return 0;
}

# ------------------------------------------------------------------------------

sub isFileRefData {
  my $filename=shift(@_);
  if (lc $filename eq "refdata") { return 1; }
  return 0;
}

# ------------------------------------------------------------------------------

sub isFileNotFound {
  my $filename=shift(@_);
  my $required=shift(@_);

  if (!$required && isFileNone($filename)) { return 0; }
  my @basename = parse_file_name($filename);
  if (! -e $basename[0] ) {
    ahlog::ah_err "File $filename does not exist";
    return 1;
  }
  return 0;
}

# ------------------------------------------------------------------------------

sub isRequiredFileNotFound {
  my @filelist=@_;
  my $status = 0;
  foreach my $file (@filelist) { 
    $status |= isFileNotFound($file,1);
  }
  return $status;
}

# ------------------------------------------------------------------------------

sub isOptionalFileNotFound {
  my @filelist=@_;
  my $status = 0;
  foreach my $file (@filelist) { 
    $status |= isFileNotFound($file,0);
  }
  return $status;
}

# ------------------------------------------------------------------------------

sub isBadCALDBFileParameterValue {
  my $filename=shift(@_);
  my $par=shift(@_);

  if(isFileNone($filename)) { 
    ahlog::ah_err "CALDB file $par required";
    return 1; 
  }
  if(!isFileCALDB($filename) && !isFileRefData($filename) && isFileNotFound($filename)) { 
    ahlog::ah_err "CALDB file $par not found";
    return 1;
  }
  return 0;
}

# ------------------------------------------------------------------------------
sub readInputFileList {

  my $filename=shift;

  if (isFileNone($filename)) { $filename=" "; } 
  my @filelist = get_file_list($filename);
  return @filelist;

}

# ------------------------------------------------------------------------------

sub removeOutputFileIfClobbering {
  my $filename=shift;
  my $clobber=shift;
  if (isFileNone($filename)) { return 0; }
  if( -e $filename && !$clobber) {
    ahlog::ah_err "Output file $filename exists, but clobber is not set";
    return 1;
  }
  unlink $filename;
  return 0;
}

# ------------------------------------------------------------------------------

sub copyOrMoveFileBasedOnCleanup {
  my $src=shift(@_);
  my $dest=shift(@_);
  my $cleanup=shift(@_);

  my $copyfunc;
  if($cleanup) {
    $copyfunc = \&File::Copy::move;
  } else {
    $copyfunc = \&File::Copy::copy;
  }

  unless( $copyfunc->( $src, $dest) ) {
    ahlog::ah_err "Failed to move $src to $dest";
    return 1; 
  }
  return 0;
}

# ------------------------------------------------------------------------------

sub copyFITSFile {
  my $src=shift(@_);
  my $dest=shift(@_);
  if (isFileNone($src)) { return 0; }
  my $status = 0;
  $status = copy_fits_file($src, $dest);
  if($status) {
    ahlog::ah_err "failed to copy $src to $dest; status = $status";
    return 1;
  }
  return 0;
}

# ------------------------------------------------------------------------------

sub formTemporaryFileName ($$;$) {
  my $filebase=shift(@_);
  my $suffix=shift(@_);
  my $extra=shift(@_);
  if (! defined $extra) {
    return "$filebase.$suffix";
  } 
  return "$filebase.$suffix.$extra";
}

# ------------------------------------------------------------------------------

sub runTool {
  my $toolname=shift(@_);
  my @params=@{shift @_};
  my @args = ();
  foreach my $ii (0..$#params) {
    push @args, "$params[$ii][0]=$params[$ii][1]" ;
  }
  my $status = run_ftool($toolname, @args);
  if($status) {
    ah_err "Error running tool $toolname; status = $status";
    return 1;
  }
}

1;
