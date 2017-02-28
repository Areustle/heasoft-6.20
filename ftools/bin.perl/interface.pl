$interface_version = ".10";
@interface_history = (
"   Version  Date    Author     Reason                                      ",
"----------------------------------------------------------------------     ",
"     .10    6/94   J. Ingham  Original                                     ",
"     .11    1/95   J. Ingham  Remove spaces from beginning of input line   ",
"-----------------------------------------------------------------------    ");

push(@version_history,"\n","      -- interface.pl V$interface_version --\n",
     @interface_history);


sub GetNumber
{
    local($prompt,$default,$varname,$append) = @_;

    if ( $append ne "" ) {
	$selResult{$varname} = &getNumber($prompt,$default,$append);
    } else {
	$selResult{$varname} = &getNumber($prompt,$default);
    }
}
sub GetScalar
{
    local($prompt,$default,$varname) = @_;

    $selResult{$varname} = &getScalar($prompt,$default);
}

sub GetOneFromList
{
    local($case,$prompt,$varname) = splice(@_,0,3);
    local(@values) = splice(@_,0,shift);

    $selResult{$varname} = &getOneFromList($case,$prompt,$#values + 1,@values);
}
sub YesOrNo
{
    local($prompt,$varname,$default) = splice(@_,0,3);
    local(@others) = splice(@_,0,shift);

    $selResult{$varname} = &yesOrNo($prompt,$default,@others);
}
#
# ------------------------------------------------------------
#

sub getNumber 
{
    local($prompt,$default,$append) = splice(@_,0,3);
    local($answer,$notgot,$defisnum);
    $append = "recommended" if ( !defined $append ) ;

    if ( $default !~ /none/i ) {
	$prompt .= " [ $default $append ]: ";
    }
    else {
	$prompt .= ": ";
    }
    print $prompt;

    if ( $default =~ /^(-?\d+\.?\d*|-?\.\d+)$/ ) {
	$defisnum = 1;
    }
    else {
	$defisnum = 0;
    }

    $notgot = 1;
    while ( $notgot ) {
	$answer = <STDIN>;
	chop $answer;	
	$answer = &stripLeading($answer);
	if ( $answer =~ /^ *$/ && $defisnum ) {
	    $notgot = 0;
	    $answer = $default;
	}
	elsif ( $answer =~ /^(-?\d+\.?\d*|-?\.\d+)$/ ) {
	    $notgot = 0;
	}
	else {
	    print "Enter a number ( or -999 to quit ) : ";
	}
    }
    $answer;
}


sub getOneFromList {

# This gets one option from a list. 
#
# Call as:
# @out_arr = &getOneFromList($case,$prompt,$#ref_arr+1,@ref_arr)
#
# Arguments:
# $case Uppercase replies, Lowercase replies of case Insensitive
# $prompt the prompt string...
# @ref_arr			

    local($case,$prompt) = splice(@_,0,2);
    local(@ref_arr) = splice(@_,0,shift);
    local($default) = shift;

    local(@outArray);

    @outArray = &getManyFromList($case,$prompt,$#ref_arr+1,@ref_arr);
    while ( $#outArray > 0 ) {
	$prompt = "Please choose only one option: ";
	@outArray = &getManyFromList($case,$prompt,$#ref_arr+1,@ref_arr);
    }
    $outArray[0];
}

#
#------------------------------------------------------------
#
sub getScalar {
    local($prompt,$default) = splice(@_,0,2);
    local($answer);
    if(defined $default) {
	print "$prompt [ $default ]: ";
    }
    else{
	print "$prompt: ";
    }
    $answer = <STDIN>;
    chop($answer);
    $answer = &stripLeading($answer);
    if($answer =~ /^ *$/) {
	$answer = $default;
    }
    $answer;
}

sub print_text
{
    if( defined $textWindow) {
	print WISH @_;
    }
    else {
	$storedMsg .= join("",@_)."\n";
    }

}


sub print_warn
{
    print WISH @_;
}

sub exit_wish
{
    exit;
}

sub MessageYesOrNo
{
    local ($default,@message) = @_;
    &yesOrNo(join("",@message),$default);
}

sub printMinorModes
{
    local($i);
    printf "Index  NEVENTS   ONTIME  $catformat{$chosen_type}",@keylist;

    for ( $i = 0; $i < $nminormodes; $i++ ) {
	printf "%5d  %7d  %7g  $catformat{$chosen_type}",$i,
        &twodrow($i,$#keylist+2,('nevents','ontime',@keylist),%keyarray);
    }			
}

sub getMinorModes
{
    print "\nEnter minor modal configurations to analyse ( by index )\n ";
    @moderange = &getrange($nminormodes - 1);
}

sub GatherResponses
{
    local ($varname);
    foreach $varname (keys %selResult) {
	eval "\$$varname = $selResult{$varname}";
    }
}

sub insertInCanvas
{
    ("","");
}
sub createText
{
    "";
}

sub getManyFromList {

# This gets a list and checks to make sure the elements of
# in_arr belong in ref_arr:
#
# Call as:
# @out_arr = &getManyFromList($case,$prompt,$#ref_arr+1,@ref_arr)
#
# Arguments:
# $case Uppercase replies, Lowercase replies of case Insensitive
# $prompt the prompt string...
# @ref_arr			

    local($case,$prompt) = splice(@_,0,2);
    local(@ref_arr) = splice(@_,0,shift);

    local($gotit,$temp,@in_arr,@out_arr);

#    print "Got CASE: $case\n";
#    print "Got Prompt: $prompt\n";
#    print "Got REF ARRAY: (",join(",",@ref_arr),")\n";

# Make the choice if there is only one:
    if ( $#ref_arr == 0 ) {
	print "Only one value available: $ref_arr[0]\n";
	@out_arr = ($ref_arr[0]);
    }
    else {

# Print out prompt and list of choices:

	print $prompt,"\n The available options are: \n";
	
	if ( @ref_arr <15 ) {
	    print "   * ",join("\n   * ",@ref_arr),"\n";
	}
	else {
	    $niters = @ref_arr/3;
	    for ( $i = 0, $index = 0; $i< $niters; $index += 3,$i++ ) {
		printf "%-20s  %-20s  %-20s\n",@ref_arr[$index..$index+2];
	    }
	    print "$#ref_arr  $niters  $index\n";
	    if ( @ref_arr - $index == 1 ) {
		printf "%-20s \n",$ref_arr[$index+1];
	    }
	    elsif ( @ref_arr - $index == 2 ) {
		printf "%-20s  %-20s\n",@ref_arr[$index+1..$index+2];
	    }
	}
	@in_arr = &getarray("Enter choice");
#    print "getManyFromList got:\n",join("\n",@in_arr),"\n";
	@out_arr = &checkList($case,$#in_arr+1,@in_arr,@ref_arr);
    }
}

sub stripLeading
{
    local($mystring,$stripChars) = splice(@_,0,2);

    if ( defined $stripChars ) {
	$mystring =~ /^[$stripChars]*(.*)$/;
	$1;
    }
    else {
	$mystring =~ /^[ \t]*(.*)$/;
	$1;
    }
}

1;
