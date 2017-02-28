$utils_version = '.22';
@utils_history = (
"   Version  Date    Author     Reason                                      ",
"----------------------------------------------------------------------     ",
"     .10    6/94   J. Ingham  Original                                     ",
"     .20    8/94   J. Ingham  Fixed getanumber to take negative no's       ",
"                                     Eliminate ld.so errors in runcom      ",
"     .21    9/94   J/ Ingham  Moved the interface routines to interface.pl ",
"     .22    3/95   L. Brown   Added VMS flag setting code                  ",
"     .21    9/94   J. Ingham  Moved the interface routines to interface.pl ",
"     .22    3/95   J. Ingham  Made checkList choose first complete match   ",
"     .22    3/95   L. Brown   Added VMS flag setting code                  ",
"-----------------------------------------------------------------------    ");

push(@version_history,"\n","      -- utils.pl V$utils_version --\n",
     @utils_history);

#========================================
# Here are the subroutines:
#
# checkList($case,$#in_arr+1,@in_arr,@ref_arr)
# split_line($string)
# getarray($prompt)
# getrange($maxrange)
# mean_if_range($defval)
# parse_range(($range,$maxrange)
# yesOrNo($default,@extras)
# postfix_num($root,@intarr)
# atstdir(*subdir)
# ststdir(*locdir)
# twodrow($rowno,$#colnames,@colnames,%array)
# twodcol($colno,$#rownames,@rownames,%array)
# TCLSafe($string)
# runcom($command,$err_routine,$err_string) [last 2 args are optional]
# yakker($command,$verbose,$err_routine,$err_string) [last 2 args are optional]
# print_likely_files($accept_glob,$reject_regexp) [last arg optional]

#
# ------------------------------------------------------------
#


# Are we on VMS?
if ($ENV{'HOME'} =~ /\$.*:\[.*\]|\[.*\]/) {
    $VMS = 1;
#    print "We are on VMS\n";
}
else {
    $VMS = 0;
#    print "We are on UNIX\n";
}



sub checkList {
# This takes @in_arr and @ref_arr, checks that all the elements of
# @in_arr are in @ref_arr.  If elements are found that are not in the 
# @in_arr array, then the user is queried for new entries.  
# 'EXIT' will terminate session and return an undefined value.
#
# Call:
# @out_arr = &checkList($case,$#in_arr+1,@in_arr,@ref_arr);
#
# Arguments:
# obvious...
#
    local($case) = shift;
    local(@in_arr) = splice(@_,0,shift);
    local(@ref_arr) = @_;
    local(@out_arr,$inval,$refval,@found,$gotem,$INFILE,%mark);

#    print "In checkList\n";
#    print "Got CASE: $case\n";
#    print "Got IN ARRAY: (",join(",",@in_arr),")\n";
#    print "Got REF ARRAY: (",join(",",@ref_arr),")\n";
    

    if ($in_arr[0] =~ /all/i ) {    # If the answer is ALL, set to @ref_arr
	@out_arr = grep(!/^ALL$/,@ref_arr);
    }
    elsif ( $in_arr[0] =~ /exit/i ) {
	undef;
    }
    elsif ( $in_arr[0] =~ /^ *@(.*)/ ) {
	$INFILE = $1;
	if(open(INFILE)) {
	    @in_arr = <INFILE>;
	    chop @in_arr;
	    close(INFILE);
	    @out_arr = &checkList($case,$#in_arr+1,@in_arr,@ref_arr);
	}
	else {
	    print STDOUT "Error opening file $INFILE\n";
	    @out_arr = ('ERROR');
	}
    }
    else {
# Change the case, as given by $case variable

	if($case eq 'u') {
	    foreach (@in_arr) { tr/a-z/A-Z/; }
	}
	elsif($case eq 'l') {
	    foreach (@in_arr) { tr/A-Z/a-z/; }
	}

# First strip spaces and remove duplicates from @in_arr:
	%mark = ();
	foreach (@in_arr) { 
	    /^ *([\w.;]+) *$/;
	    $mark{$1}++; 
	}
	@in_arr = keys(%mark);
	%mark = ();
	$gotem = 1;
	foreach $inval (@in_arr) {
	    $#found = -1;
	    foreach $refval (@ref_arr) {
		if ( $refval =~ /^$inval/ ) {
		   $found[++$#found] = $refval;
	       }
	    }
	    if ( $#found  == -1 ){
		print "Unrecognized value entered: $inval\n";
		$gotem = 0;
	    }
	    elsif ( $#found >= 0 ) {
		$out_arr[++$#out_arr] = $found[0];
	    }
	}
	if ( !$gotem ) {
	    @out_arr = 
		&getManyFromList($case,"Try again ( or type EXIT to quit )",
				  $#ref_arr+1,@ref_arr);
	}
	else {
	    return @out_arr; # This returns out_arr
	}
    }				# end else
}

#------------------------------------------------------------

sub getarray {
# This writes the argument as a prompt, and then passes back an array
# from the (space delimited, or comma delimited if any commas are
# found) response string.
#
# Call as:
# @array = &getarray("Prompt string");
#
# Arguments:
# $1 = prompt string
    &split_line(&getScalar($_[0]));
}
#
# ------------------------------------------------------------
#

sub yesOrNo
{
# Query for a yes or no answer, and return 0 for no, 1 for yes.
# If there is a list in extras, then return a place in the extra's list
# + 2.
#
    local($prompt,$default,@extras) = @_;
    local($answer,$i,$upc,$addon,@answer,$notgot);

#
# This upper-cases the default value.  it must be the first letter
# of the option, and must be passed in as lower case.
#
    ($upc = $default) =~ tr/a-z/A-Z/;
    $addon = "(y/n";
    for ( $i = 0; $i <= $#extras; $i++) {
	$addon .= "/".substr($extras[$i],0,1);
    }
    $addon .= "): ";
    $addon =~ s#$default#$upc#;
    print "$prompt $addon";

    $notgot = 1;
    $query = 1;
    if($#extras == -1 ) {
	while ( $notgot ) {
	    $answer = <STDIN> if ( $query );
	    if ( $answer =~ /^ *y/i ) {
		$notgot = 0;
		$answer = 1;
	    }
	    elsif ( $answer =~ /^ *n/i ) {
		$notgot = 0;
		$answer = 0;
	    }
	    elsif ( $answer =~ /^ *$/ ) {
		$answer = $default;
		$query = 0;
	    }
	    else {
		print "Enter \'yes\' or \'no\': ";
	    }
	}
    }
    else { 
	while ( $notgot ) {

	    if ( $query ) {
		$answer = <STDIN> ;
		chop $answer;
	    }

	    if ( $answer =~ /^ *y/i ) {
		$notgot = 0;
		$answer = 1;
	    }
	    elsif ( $answer =~ /^ *n/i ) {
		$notgot = 0;
		$answer = 0;
	    }
	    elsif ( $answer =~ /^ *$/ ) {
		$answer = $default;
		$query = 0;
	    }
	    else  {
		for ( $i = 0;$i <= $#extras; $i++ ) {
		    if ( $extras[$i] =~ /^ *$answer/i ) {
			$notgot = 0;
			last;
		    }
		}
		if ( ! $notgot ) {
		    $answer = $i + 2;
		}		
		else {
		    if ( $#extras == 0 ) {
			print "Enter \'yes\', \'no\' or \'$extras[0]\': ";
		    }
		    else {
			print "Enter \'yes\', \'no\', \'",
			join("\' \'",@extras[0..$#extras-1]),
			"\' or \'$extras[$#extras]\': ";
		    }
		}
	    }
	}
    }
    $answer;
}

#
#------------------------------------------------------------
#
sub split_line {
# This splits the input line, by commas if there are any, otherwise
# by spaces
#
# Call:
# @array = &split_line($string);
#
    local($answer) = @_;
    if(index($answer,",") == -1) {
	split(" ",$answer);
    }
    else {
	split(",",$answer);
    }
}

#------------------------------------------------------------

sub postfix_num {
# This takes ($root,@intarr) and returns an array of strings, with
# the ints of intarr postfixed to $root.
    local($root) = shift;
    local(@post) = @_;
    local($incr);
    for($incr = 0; $incr <= $#post; $incr++) {
	$post[$incr] = $post[$incr]." ";
    }				
    local($tempstr) = join($root,("",@post));
    @post = split(' ',$tempstr);
}
#
# ------------------------------------------------------------
#
sub atstdir {
# Given an array of directories, this returns replaces each with the 
# complete path, or prompts for the correct path if is doesn't exist.
   
    local(*subdir) = @_;
    local($incr,$thisdir);
    for ($incr = 0; $incr <= $#subdir; $incr++) {
	$thisdir = $subdir[$incr];
	&ststdir(*thisdir);
	$subdir[$incr] = $thisdir;
	$subdir[0] = 'EXIT' , last if ( $subdir[$incr] eq 'EXIT' );
    }				# end for
#    print join("\n   ","   ",@subdir);
    @subdir = @subdir;	# returns @datadir
}				# end sub tstdir
#
# ------------------------------------------------------------
#
sub ststdir {
# Given a directory, this replaces the input directory with the full
# path, and queries for a correction if the directory does not exist.    
    local(*locdir) = @_;
    
    while ( !-d $locdir ) {
	print "The directory ",$locdir,
	" does not exist","\n"; # 
	print "Enter the correct name, or type q to quit: ";
	$locdir = <STDIN>; # 
	
	chop $locdir; # 
	if ( $locdir eq "q" ) {
	    $locdir = 'EXIT';
	    return;
	}			# end quit if
    }			# end while
# Now get the full name of the directory:
    $locdir = `cd $locdir;/bin/pwd`;
}


sub twodrow
{
# Extracts a row or parts thereof, from a 2-dim array, %array, 
# made by $array{$row,$col}
#
# CALL: @row = &twodrow($rowno,$#colnames,@colnames,%array)

    local($rowno,$ncols);
    $rowno = shift;
    $ncols = shift;
    local(@colnames) = splice(@_,0,$ncols+1);
    local(%array) = @_;
    local($key,@hslice);
    
    $#hslice = -1;
    foreach $key ( @colnames ) {
	push(@hslice,$array{$key,$rowno});
    }
    @hslice;
}
sub twodcol
{
# Extracts a col from a 2-dim array, %array, made by $array{$row,$col}
#
# CALL: @col = &twodrow($colno,$#rownames,@rownames,%array)

    local($colno,$nrows);
    $colno = shift;
    $nrows = shift;
    local(@rownames) = splice(@_,0,$nrows+1);
    local(%array) = @_;
    local($key,@vslice);
    
    $#vslice = -1;
    foreach $key ( @rownames ) {
	push(@vslice,$array{$colno,$key});
    }
    @vslice;
}

sub getrange 
{
# Queries for, and returns an array of integers giving the chosen elements 
# from an input array of the form: 1-3,6,7-10.

    local($maxrange) = @_;
    print "Enter range [i.e. 1-3,6,7-10] or \'q\' to quit: ";
    $range = <STDIN>;
    chop $range;
    @range = &parse_range($range,$maxrange);
    if( $range[0] == -10 ) {
	@range = &getrange($maxrange);
    }
    else {
	@range;
    }
}

sub parse_range
{
# Parses the range $range, and checks it has no elements > $maxrange.
    local($range,$maxrange) = @_;
    local(@temp,@result,%mark);
    $#result = -1;
    @temp = split(/ *, */,$range);
    foreach $temp (@temp) {
	if ( $temp eq "q" ) {
	    return ('q');
	}
	elsif ( $temp =~ /^ *- *(\d+)/ ) {
	    if ( $1 > $maxrange ) {
		print "Error in range element $temp: out of range. \n";
		return (-10);
	    }
	    else {
		push(@result,(0..$1));
	    }
	}
	elsif ( $temp =~ /^ *(\d+) *- *$/ ) {
	    if ( $1 > $maxrange ) {
		print "Error in range element $temp: out of range. \n";
		return (-10);
	    }
	    else {
		push(@result,($1..$maxrange));
	    }
	}
	elsif ( $temp =~ /^ *(\d+) *- *(\d+) */ ) { 
	    if ( $1 > $maxrange || $2 > $maxrange ) {
		print "Error in range element $temp: out of range. \n";
		return (-10);
	    }
	    else {
		push(@result,($1..$2));
	    }
	}
	elsif ( $temp =~ /^ *(\d+) *$/ ) {
	    if ( $1 > $maxrange ) {
		print "Error in range element $temp: out of range.\n";
		return (-10);
	    }
	    else {
		push(@result,$1);
	    }
	}
	else {
	    print "Error in range element $temp \n";
	    return (-10);
	}
    }
# get the unique elements
    foreach ( @result ) { $mark{$_}++; }
    sort( keys(%mark) );
}


sub runcom
{	
# runcom 1.0 12/94
# This runs the command given as a parameter, traps the output
# returns the output or "ERROR FOR $$" if there is an error.
# The error messages are written to STDERR unless the first parameter
# ($command) begins with a '-' sign.
# The second parameter is an optional error routine to call if 
# an error occurs (e.g. to clean up temporary files and die).
# The third parameter is an optional scalar to pass to the error routine.
    local($command) = $_[0];
    local($err_routine) = $_[1];
    local($err_string) = $_[2];
    local($ERRFIL,@result,$CURHANDLE);
    local($suppress);

if($command =~ s/^-// ) {$suppress = 1;}

if($VMS){
#people who use VMS deserve to wait for all of this to happen
    $COMFIL="com$$.com";
    $ERRFIL="error$$";
    unlink($COMFIL);
    open(COMFIL,">$COMFIL")|| die "couldn't open $COMFIL\n";
    print COMFIL "\$define SYS\$ERROR $ERRFIL\n";
    print COMFIL "\$$command\n";    
    close(COMFIL) || die "couldn't close $COMFIL\n";
    unlink($ERRFIL);
    @result = `\@$COMFIL`;
    unlink($COMFIL);
}
else{
    $ERRFIL = "/tmp/error$$";
    unlink $ERRFIL;
    $command .= " 2> $ERRFIL";
    @result = `$command`;
}

    if ( ! open(ERRFIL) ) {
	unlink $ERRFIL;
	return @result;
    }
    else {
	while ( ($_ = <ERRFIL>) =~ /(ld\.so|^\s*\n$)/ ) { ; } 
	
	if ( ! $_ ) {
	    close ERRFIL;
	    unlink $ERRFIL;
	    return @result;
	}
	else {
	    unless ($suppress) {
# Print the error to STDERR, but preserve the currently selected handle,
# also line buffer the output so that if a command runs away producing junk
# the user can see it quickly.
    	    print "Error running command: \n    $command\n The error was \n\n";
	    $CURHANDLE = select(STDERR);
	    $| = 1;
	    print $_;
	    while ( <ERRFIL>) { print $_; }
	    $| = 0;
	    select($CURHANDLE);
	    unlink $ERRFIL;
            }
            if($err_routine) {&$err_routine($err_string);}
	    "ERROR FOR $$";
	}
    }
}

sub yakker {
#Calls runcom.  Prints output from runcom if $verbose (second argument) 
#is true.
    local(@yak);
    $command = $_[0];
    $verbose = $_[1];
    $err_routine = $_[2];
    $err_string = $_[3];
    @yak = &runcom($command,$err_routine,$err_string);
    if($verbose && @yak) {print @yak;}
}


sub mean_if_range
{
    local($defval) = @_;
    
    if ( $defval =~ /^(\d+\.?\d*|\.\d+)$/ ) {
	$1;
    }
    elsif ($defval =~ /^(\d+\.?\d*|\.\d+) *- *(\d+\.?\d*|\.\d+)$/ ) {
        ( $1 + $2 )/2.0 ;
    }
    else {
        print "More than two elements in range\n";
        exit 1;              
    }
}

sub numerically
{
    $a <=> $b;
}

sub TCLSafe
# This lower cases the first character of a string.
# It translates non-numerics to 'a'
# It converts spaces to _  
{
    local($string) = shift;
    local($char);

    $char = substr($string,0,1);
    $char =~ tr/A-Z/a-z/;
    $char =~ tr/A-Za-z/a/c;
    substr($string,0,1) = $char;
    $string =~ s/ /_/g;
    $string;
}

sub print_likely_files{
#print_likely_files 1.0  1/95
#Prints a list of "likely looking" files with numbers
#The global variable @likely_files will contain the list of files.
#Beware: the likely files array starts at index 0. The display list
#is numbered starting with 1.
#First argument is a globbing specification for likely looking files.
#For example: '*_*src*.fits' will look for RDF _src or _qsrc files.
#Second argument (optional) is a perl regular expression to supress printing
#certain names.  For example '^rh\d' as the second argument will make
#PLF not print files beginning with rh (ROSAT hri files).

local($file,$i,@tmp,@file_st,$accept_glob,$reject_regexp);
$accept_glob = $_[0];
if(@_ > 1) {
    $reject_regexp = $_[1];
}
undef(@likely_files);		# in case this gets called more than once
@tmp = <${accept_glob}>;
if(@tmp) {
    foreach $file (@tmp) {
	next if ($reject_regexp && $file =~ /$reject_regexp/) ;
	push(@likely_files,$file);
    }
    if(@likely_files){
	if (@likely_files>1) {@file_st = ("files","a file");} 
	else {@file_st = ("file","it");}
	print "You have the following likely looking $file_st[0]:\n";
	foreach $file (@likely_files) {
	    $i++;
	    print "$i\t$file\n";
	}
	print "Type corresponding number to select $file_st[1],\n";
	print "or type a specific file or pathname.\n";
    }
}
}

1;

