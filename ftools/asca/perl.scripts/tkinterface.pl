$tkinterface_version = '.2';
@tkinterface_history = (
"     Version    Date      Author     Reason                                ",
"----------------------------------------------------------                 ",
"       .10    07/02/94  J. Ingham    Original                              ",
"       .20    02/02/95  J. Ingham    Support Tk4.0                         ",
"       .21    01/24/97  L. Brown     Made DirAndMode window larger so ",
"                                     everything shows.",
"----------------------------------------------------------                 ");


push(@version_history,"\n",
     "      -- tkinterface.pl V$tkinterface_version --\n",
     @tkinterface_history);

# This is the amount that we should leave as white space above the inserted
# frames for the canvas elements... For Tk4.0...

$topspace = .004;

#

############################################################
# EXTERNAL VARIABLES: (Not complete...)
# $textWindow - The name of the current text window in canvas .c
# %selFrame   - The return target for entry form replies
# WISH        - The filehandle to an open wish application 
############################################################

############################################################
# Building Forms:
# The cycle for building forms is as follows:  
# 1) Use the GetNumber, GetOneFromList, GetManyFromList, YesOrNo
#    or GetScalar to build up the form in the array @selFrame.
# 2) Use  GatherResponses to put up the form, and return the responses.
#
# In each of the subroutines in 1) you pass a variable name, e.g. 'datamode'
# GatherResponses will put the result in the Perl variable $datamode. 
############################################################

############################################################
# Naming Conventions:
# If there are routines with the same name but differing by case in the
# first letter, e.g. GetManyFromList and getManyFromList, then the UC
# routine will add a line to the form that is being built up, and place
# the result in reported variables.  The LC version will pt up a dialog box
# to query directly for the value, and return it immediately.
############################################################

##################################################
# SUBROUTINE:  GetNumber
# ARGUMENTS:   $prompt  - the prompt string
#              $default - the default value (none for No default)
#              $varname - the variable name (w/o the $)
#              $append  - The string to go in [ $default ]
# DESCRIPTION: Adds an entry to .c.selFrame for entry of a number
############################################################
sub GetNumber
{
    local($prompt,$default,$varname,$append,$base) = (@_);
    
    if ( $base eq "" ) {
	$base = '.c.selFrame';
    }

    if ( $append eq "" ) {
	$append = "recommended";
    }

    if ( $default !~ /none/i ) {
	$prompt .= " [ $default $append ]: ";
    }
    else {
	$prompt .= ": ";
    }
    push(@selFrame,("frame ${base}.${varname}\n",
	 "entry ${base}.${varname}.value -relief sunken  \\\n",
	  "   -textvariable selFrame($varname) -width 10\n",
		" label  ${base}.${varname}.label -text { $prompt }\n",
    "if { \$tk_version < 4.0 } {\n",
    " bind ${base}.${varname}.value <Any-Key> \"[bind Entry <Any-Key>]\\\n",
                "onlyNumbers ${base}.${varname}.value \%K\"\n",
    " } else {\n",
    " bind ${base}.${varname}.value <Any-Key> \"+\\\n",
                "onlyNumbers ${base}.${varname}.value \%K\"\n",
    " } \n",
    " bind ${base}.${varname}.value <Delete> {+\\\n",
                "deleteSelection ${base}.${varname}.value}\n",
    "pack ${base}.${varname}.value -side right -expand true -fill x\n",
    "pack ${base}.${varname}.label -side left\n",
    "pack ${base}.${varname} -side top -fill x -expand true\n"));
    if ( $default =~ /^[-]?\d+\.?\d*$|^[-]?\.\d+$/ ) {
	push(@selFrame,("set selFrame($varname) $default\n"));
    }
    else {				
	push(@selFrame,("set selFrame($varname) \"\"\n"));
    }
}
##################################################
# SUBROUTINE:  GetScalar
# ARGUMENTS:   $prompt  - the prompt string
#              $default - the default value (none for No default)
#              $varname - the variable name (w/o the $)
# DESCRIPTION: Adds an entry to .c.selFrame for entry of a number
############################################################
sub GetScalar
{
    local($prompt,$default,$varname,$base) = (@_);
    
    if ( $base eq "" ) {
	$base = '.c.selFrame';
    }

    if ( $default !~ /none/i ) {
	$prompt .= " [ $default ]: ";
    }
    else {
	$prompt .= ": ";
    }
    push(@selFrame,("frame ${base}.${varname}\n",
	 "entry ${base}.${varname}.value -relief sunken  \\\n",
	  "   -textvariable selFrame($varname) -width 10\n",
		" label  ${base}.${varname}.label -text { $prompt }\n",
    " bind ${base}.${varname}.value <Delete> {+\\\n",
                "deleteSelection ${base}.${varname}.value}\n",
    "pack ${base}.${varname}.value -side right -expand true -fill x\n",
    "pack ${base}.${varname}.label -side left\n",
    "pack ${base}.${varname} -side top -fill x -expand true\n"));
    if ( $default =~ /^[-]?\d+\.?\d*$|^[-]?\.\d+$/ ) {
	push(@selFrame,("set selFrame($varname) $default\n"));
    }
    else {				
	push(@selFrame,("set selFrame($varname) \"\"\n"));
    }
}

############################################################
# SUBROUTINE: GetOneFromList
# ARGUMENTS:
#    $case    - If 'u' uppercase the user's input before match
#               If 'l' lowercase the user's input before match
#               otherwise do case sensitive match
#    $prompt  - the prompt
#    $varname - the name of the parameter (w/o the $) for the 
#               returned value.  In the TCL code, the value
#               will be stored in selFrame($varname)
#    $nvalues - Number of values in @values list ($#values+1)
#    @values  - The list to choose from.
#    $default - Optional: The default value.
#    $base    - Optional: the base frame for these widgets.
# DESCRIPTION: 
#    Adds a line to the entry form $base, with a prompt string and a
#    menu button widget with the list of choices.  The result is recorded
#    in an entry widget.   The value is recorded in the array selFrame. 
############################################################ 
sub GetOneFromList
{
    local($case,$prompt,$varname) = splice(@_,0,3);
    local(@values) = splice(@_,0,shift);
    local($default,$base) = splice(@_,0,2);

#$base is an optional argument...
    if ( !defined $base ) {
	$base = '.c.selFrame';
    }

# Define the widgets:

    push(@selFrame,("frame $base.${varname}\n",
       " label  $base.${varname}.label -text { $prompt }\n",
       " entry $base.${varname}.value -relief sunken \\\n",
       "   -textvariable selFrame($varname) -width 10 -state disabled\n",
       "menubutton $base.${varname}.mb \\\n",
       "-menu $base.${varname}.mb.m -text { Choose One } -relief raised\n",
		    "menu $base.${varname}.mb.m \n"));

# Put the menu values in the menu button:

    foreach $value (@values) {
	push(@selFrame,
	     ("$base.${varname}.mb.m add command  -label $value \\\n",
	     " -command { global selFrame($varname)\n",
             " $base.${varname}.value configure -state normal\n",
	     " set selFrame($varname) $value \n",
             " $base.${varname}.value configure -state disabled} \n"));
    }

# This sets the selection to the default, if there is one.

    if(defined $default && $default ne "") {
	($filled) = grep(/$default/,@values);
	if (defined $filled) {
	    push (@selFrame,"set selFrame($varname) \"$filled\"\n");
	}
    }

# This is the section that packs the entry

    push (@selFrame,
      ("pack $base.${varname}.label -side left\n",
       "pack $base.${varname}.value -side right -expand true -fill x\n",
       "pack $base.${varname}.mb -side right \n",
       "pack $base.${varname} -side top -fill x -expand true\n"));
}

############################################################
# SUBROUTINE: GetManyFromList
# ARGUMENTS:
#    $case    - If 'u' uppercase the user's input before match
#               If 'l' lowercase the user's input before match
#               otherwise do case sensitive match
#    $prompt  - the prompt
#    $label   - Label for the menu button widget
#    $nvalues - Number of values in @values list ($#values+1)
#    @values  - The list to choose from.
#    $ndefaults - Number of defaults in the defaults list
#    @defaults- Optional: The default values.
#    $base    - Optional: the base frame for these widgets.
# DESCRIPTION: 
#    Adds a line to the entry form $base, with a prompt string and a
#    menu button widget with the list of choices.  The result is recorded
#    in an entry widget.   The value is recorded in the array selFrame. 
############################################################ 
sub GetManyFromList
{
    local($case,$prompt,$label) = splice(@_,0,3);
    local(@values) = splice(@_,0,shift);
    local(@defaults) = splice(@_,0,shift);
    local($base) = splice(@_,0,1);
    local($value,$refVal,$default);

#$base is an optional argument...
    if ( !defined $base ) {
	$base = '.c.selFrame';
    }
    $varname = &TCLSafe($label);
# Define the widgets:

    push(@selFrame,("frame $base.${varname}\n",
       " label  $base.${varname}.label -text { $prompt }\n",
       "menubutton $base.${varname}.mb -borderwidth 2 \\\n",
       "-menu $base.${varname}.mb.m -text {$label} -relief raised\n",
		    "menu $base.${varname}.mb.m \n"));

# Put the menu values in the menu button:

    foreach $value (@values) {
	push(@selFrame,
	     ("$base.${varname}.mb.m add checkbutton  -label $value \\\n",
	     " -variable selFrame($value) -onvalue 1 -offvalue 0 \n "));
    }

# This sets the selection to the default, if there is one.

    foreach $refVal (@values) {
	$value = 0;
	foreach $default (@defaults) {
	    $value = 1, last if( $default =~ /$refVal/i);  
	}
	push(@selFrame,"set selFrame($refVal) $value\n");
    }

# This is the section that packs the entry

    push (@selFrame,
      ("pack $base.${varname}.label -side left\n",
       "pack $base.${varname}.mb -side right -ipadx 10\n",
       "pack $base.${varname} -side top -fill x -expand true\n"));
}

############################################################
# SUBROUTINE: YesOrNo
# ARGUMENTS:  
#    $prompt  - the prompt
#    $varname - the name of the parameter (w/o the $) for the 
#               returned value.  In the TCL code, the value
#               will be stored in selFrame($varname)
#    $default - The default value
#    $nothers - Number of values in @others list ($#others+1)
#    @values  - List of additional options (beyond Yes and No).
#    $base    - Optional: the base frame for these widgets.#     
#    $bindVar - Variable to which to bind activation of the widget
#    $bindVal - The value that signifies on for the widget. Has to be
#               a yes or no widget in the same frame.
# DESCRIPTION:
# Adds a line to the form given by $base which contains a prompt string,
# and a set of radio buttons, one for Yes, one for No, and one for each of 
# the elements of the @others array.  
# The value is put in selFrame($varname) in the TCL, and set to 0 for no
# 1 for Yes, and $i+2 for $others($i).
############################################################
sub YesOrNo
{
    local($prompt,$varname,$default) = splice(@_,0,3);
    local(@others) = splice(@_,0,shift);
    local($base,$bindVar,$bindVal) = splice(@_,0,3);

    local($i,$addon,$upc);

    if ( !defined $base ) {
        $base = '.c.selFrame';
    }

    push(@selFrame,("frame $base.${varname}\n",
	" label  $base.${varname}.label -text { $prompt }\n",
	" frame $base.${varname}.b\n",
	"radiobutton $base.${varname}.b.yes -relief raised  \\\n",
	"   -text \"yes\" -variable selFrame($varname) -value 1\n",
	"radiobutton $base.${varname}.b.no -relief raised  \\\n",
	"   -text \"no\" -variable selFrame($varname) -value 0\n",
        "pack $base.${varname}.label -side left\n",
        "pack $base.${varname}.b.yes -side left \n",
        "pack $base.${varname}.b.no -side left \n"));
    for ( $i = 0; $i <= $#others ; $i++) {
	$iplus2 = $i + 2;
	push(@selFrame,   
	     "radiobutton $base.${varname}.b.$others[$i] -relief raised \\\n",
	     "   -text \"$others[$i]\" -variable selFrame($varname) \\\n",
             "-value $iplus2\n",
	     "pack $base.${varname}.b.$others[$i] -side left \n");
	if ( $others[$i] =~ /^$default/ ) {
	    push(@selFrame,"set selFrame($varname) $iplus2\n");
	}
    }
    if (defined $bindVar && defined $bindVal ) {
	if ($bindVal == 1) {
	    push(@selFrame,"$base.${bindVar}.b.yes configure -command {\n",
		 "$base.${varname}.b.yes configure -state normal\n",
                 "$base.${varname}.b.no configure -state normal\n");
	    for ( $i = 0; $i <= $#others ; $i++) {
		push(@selFrame,
                 "$base.${varname}.b.$others[$i] configure -state normal\n");
            }
            push(@selFrame,"}\n");
	    push(@selFrame,"$base.${bindVar}.b.no configure -command {\n",
		 "$base.${varname}.b.yes configure -state disabled\n",
                 "$base.${varname}.b.no configure -state disabled\n");
	    for ( $i = 0; $i <= $#others ; $i++) {
		push(@selFrame,
                 "$base.${varname}.b.$others[$i] configure -state disabled\n");
            }
            push(@selFrame,"}\n");
        }
	elsif ( $bindVal == 0 ) {
	    push(@selFrame,"$base.${bindVar}.b.no configure -command {\n",
		 "$base.${varname}.b.yes configure -state normal\n",
                 "$base.${varname}.b.no configure -state normal\n");
	    for ( $i = 0; $i <= $#others ; $i++) {
		push(@selFrame,
                 "$base.${varname}.b.$others[$i] configure -state normal\n");
            }
            push(@selFrame,"}\n");
	    push(@selFrame,"$base.${bindVar}.b.yes configure -command {\n",
		 "$base.${varname}.b.yes configure -state disabled\n",
                 "$base.${varname}.b.no configure -state disabled\n");
	    for ( $i = 0; $i <= $#others ; $i++) {
		push(@selFrame,
                 "$base.${varname}.b.$others[$i] configure -state disabled\n");
            }
            push(@selFrame,"}\n");
        }
    }
    if ( $default eq 'y' ) {
	push(@selFrame,"set selFrame($varname) 1\n");
    }
    elsif ( $default eq 'n') {
	push(@selFrame,"set selFrame($varname) 0\n");
    }
    push(@selFrame,
         "pack $base.${varname}.b -side right \n",
	 "pack $base.${varname} -side top -fill x -expand true\n");
}

############################################################
# SUBROUTINE: GatherResponses
# ARGUMENTS:
#   $base - The base of the entry form
# DESCRIPTION:
#  This packs the entry form, then reads the variables in the form back into
#  the associative array selFrame.   
#############################################################
sub GatherResponses
{
    local($pause,$base) = splice(@_,0,2);
    if ( !defined $base ) { $base = ".c.selFrame"; }
#
# Set up the Frame for the selections:
#
    if ( defined $ENV{GATHERRDEBUG} ){
	print WISH "exit\n";
	close WISH;
	unlink 'temp.tcl';
	open (WISH,">temp.tcl");
    }
 
    print WISH "frame $base -relief ridge -borderwidth 4\n";

    if ( $pause ) {
    print WISH<<EODELAY;
after 1750 
.c create window 315 \\
   [expr [lindex [.c bbox $textWindow] 3] +2 * \$scrollInc] \\
   -tags selFrame -window $base -anchor n
if { \$tk_version < 4.0 } {
    .c yview [expr int( [lindex [.c bbox selFrame] 1]/\$scrollInc) - 2]
} else {
    set tempvar [expr double([lindex [.c bbox selFrame] 1])/double(\$scrollRegSize)]
    .c yview moveto [expr \$tempvar - $topspace]
}
EODELAY
    }
    else {
    print WISH<<EONODELAY;
.c create window 315 \\
   [expr [lindex [.c bbox $textWindow] 3] +2 * \$scrollInc] \\
   -tags selFrame -window $base -anchor n
if { \$tk_version < 4.0 } {
   .c yview [expr int( [lindex [.c bbox selFrame] 1]/\$scrollInc) - 2]
} else {
    set tempvar [expr double([lindex [.c bbox selFrame] 1])/double(\$scrollRegSize)]
    .c yview moveto [expr \$tempvar - $topspace]
}
EONODELAY
    }
      
    print WISH @selFrame,"\n";
    print WISH<<EOSELWRITE;
frame $base.submit -relief ridge -borderwidth 2
button $base.submit.ok -relief raised -text \"Submit\" -width 20 \\
	-command { global selFrame
                    set filled 1
                    foreach varname [array names selFrame] {		       
			if {\$selFrame(\$varname) == \"\"} {
			    putMessage {Please complete the form before submitting}
                            set filled 0
                            break
			}
		  }
	       if {\$filled == 1} {set Result 1} 
	       }
button $base.submit.cancel -relief raised -text \"Cancel\" -width 20 \\
	-command { global selFrame ; set Result 0 }
pack $base.submit.ok -side left -fill x -padx 20 -pady 2
pack $base.submit.cancel -side right -fill x -padx 20 -pady 2
pack $base.submit -side bottom -fill x -expand true
set Result -1
tkwait variable Result
foreach varname [array names selFrame] {
   puts stdout \"SELFRAME> \$varname \$selFrame(\$varname)\"
}
puts stdout \"SELFRAME> EXIT \$Result\"
flush stdout
EOSELWRITE


$textWindow =~ tr/0123456789/1234567890/;

print WISH<<EOCRETXT1;
.c create text 10 [expr [lindex [.c bbox selFrame] 3] + 2 * \$scrollInc ] \\
        -anchor nw  -tags $textWindow
catch {set font [option get .c textFont Font]}
set error [catch {.c itemconfigure $textWindow -font \$font}] 
EOCRETXT1

#
# This gathers the results:
#
    if ( defined $ENV{GATHERRDEBUG} ){
       exit;
    }
GATHERSEL:
    while (<WISH>) {
	/^SELFRAME> *(\S+) *(\S+)$/;
	eval "\$$1 = $2";
	last GATHERSEL if ( $1 eq "EXIT" );
    }
    if (! $EXIT ) {
	&exit_wish;
    }
}

############################################################
# SUBROUTINE: getScalar
# ARGUMENTS:
#   $prompt  - The prompt
#   $default - The Default
#   $base    - Optional - The base for the dialogue box   
# DESCRIPTION:
#  This puts up a dialog box with prompt, and returns the result
#  Uses the Tcl proc putSimpleDialog from primeWish.
#############################################################
sub getScalar
{
    local($prompt,$default,$base) = splice(@_,0,4);

    if(!defined $default || $default eq 'none' ) {
	$default = "";
    }
    if (!defined $base ) {
	$base = '.dialog';
    }

    print WISH "putSimpleDialog \"$prompt\" \"$default\" \"$base\" \n";

    while ( <WISH> ) {
	$_ =~ /^([A-Z]+)> (.*)$/;
	last if ($1 eq 'MESSAGE');
    }
    $2;
}
############################################################
# SUBROUTINE: getNumber
# ARGUMENTS:
#   $prompt  - The prompt
#   $default - The default
#   $base    - Optional - The base for the dialogue box   
# DESCRIPTION:
#  This puts up a dialog box with prompt, and returns the result
#  Uses the Tcl proc putSimpleDialog from primeWish.
#############################################################
sub getNumber
{
    local($prompt,$default,$base) = splice(@_,0,3);

    if(!defined $default || $default eq 'none' ) {
	$default = "";
    }
    if (!defined $base ) {
	$base = '.dialog';
    }
    print WISH "putNumberDialog \"$prompt\" \"$default\" \"$base\"\n";     
    while ( <WISH> ) {
	$_ =~ /^([A-Z]+)> (.*)$/;
	last if ($1 eq 'MESSAGE');
    }
    $2;
}


############################################################
# SUBROUTINE: getManyFromList
# This gets a list and checks to make sure the elements of
# in_arr belong in ref_arr:
#
# CALL AS:
# @out_arr = &getManyFromList($case,$prompt,$#ref_arr+1,@ref_arr,
#                  $#defaults+1,@defaults,$base)
#
# ARGUMENTS:
# $case Uppercase replies, Lowercase replies of case Insensitive
# $prompt the prompt string...
# @ref_arr    - The list of possible values.
# @defaults   - The list of defaults.
#
# RETURNS: the chosen elements as an array
############################################################

sub getManyFromList {


    local($case,$prompt) = splice(@_,0,2);
    local(@ref_arr) = splice(@_,0,shift);
    local(@defaults) = splice(@_,0,shift);
    local($base) = splice(@_,0,1);

    if(!defined $base) {
	$base = ".dialog";
    }

    print WISH "getManyFromList {$prompt} {",join(" ",@ref_arr),"} {",
    join(" ",@defaults),"} $base \n";

    while ( <WISH> ) {
	if( /^MESSAGE> (\S*) *$/ ) {
	    last if ( $1 eq 'EXIT' );
	    push(@result,$1);
	}
    }
    @result;
}

############################################################
# SUBROUTINE: getOneFromList
# 
# 
#
# CALL AS:
# @out_arr = &getManyFromList($case,$prompt,$#ref_arr+1,@ref_arr,
#                  $#defaults+1,@defaults,$base)
#
# ARGUMENTS:
# $case Uppercase replies, Lowercase replies of case Insensitive
# $prompt the prompt string...
# @ref_arr    - The list of possible values.
# $default   - The list of defaults.
#
# RETURNS: the chosen element
############################################################

sub getOneFromList {


    local($case,$prompt) = splice(@_,0,2);
    local(@ref_arr) = splice(@_,0,shift);
    local($default,$base) = splice(@_,0,2);
    local($base) = splice(@_,0,1);

    if(!defined $base) {
	$base = ".dialog";
    }


    print WISH "getOneFromList {$prompt} {",join(" ",@ref_arr),
    "} {$default} $base \n";

    while ( <WISH> ) {
	if( /^MESSAGE> RESULT (\S*) *$/ ){
	    return $ref_arr[$1];
	}
    }
}

############################################################
# SUBROUTINE: print_warn
# ARGUMENTS:
#   @printline - Lines of text to print
# DESCRIPTION:
# Prints an array of text to a warning dialog box, with an okay  
# button.  Uses the Tcl proc putMessage defined in primeWish.
#############################################################
sub print_warn
{
    local(@printline) = @_;
    print WISH "putMessage \"",@printline,"\"\n";
}


############################################################
# SUBROUTINE: MessageYesOrNo
# ARGUMENTS:
#   $default - The default option
#   @message - Lines of text to print
# DESCRIPTION:
# Prints an array of text to a message dialog box, with Yes and No  
# buttons.  Uses the Tcl proc putMssgYN defined in primeWish.
#############################################################
sub MessageYesOrNo
{
    local ($default,@message) = @_;
    print WISH "putMssgYN \"",@message,"\" \"default\"\n";
    while ( <WISH> ) {
	if ( $_ =~ /MESSAGE> (.*)$/ ) {
	    return $1;
	}
    }
}

############################################################
# SUBROUTINE: printMinorModes
# ARGUMENTS:
#   
# DESCRIPTION:
# Adds the list of Minor Modes to the canvas .c, with a selection button
# for each minor mode, and submit and cancel buttons
# This one uses a single window, rather than a bunch of seperate lines.
# The selection is returned to WISH's STDOUT (i.e. WISH in the parent)
# proceeded with the tag "MODESEL> "
#############################################################
sub printMinorModes
{
    local($base) = shift;
    local($i,$iminus,$templine);

    if (!defined $base ) { $base = ".c.mM"; }
#
#    $TEMPFILE = "temp.tcl";
#    unlink($TEMPFILE);
#    open(TEMPFILE,">$TEMPFILE");
#    


    if ( $nminormodes == 1 ) {
	&print_text("Got the following minor modal configuration:\n");
	$pause = 1;
	print WISH &createTable('nonPropFont');
	$templine = 
	    sprintf("       NEVENTS   ONTIME  $catformat{$chosen_type}\n",
		    @keylist);
	&print_text($templine);
	$templine = sprintf("%5d  %7d  %7g  $catformat{$chosen_type}\n",0,
	      &twodrow(0,$#keylist+2,(nevents,ontime,@keylist),%keyarray));
	&print_text($templine);
	print WISH &createText;
    }
    elsif ( $most_events ) { 
	&print_text("Choosing the maximum from:\n");
	print WISH &createTable('nonPropFont');
	for ( $i = 0; $i < $nminormodes; $i++ ) {
	    $templine = sprintf("%5d  %7d  %7g $catformat{$chosen_type}\n",$i,
                  &twodrow($i,$#keylist+2,(nevents,ontime,@keylist),%keyarray));
	    &print_text($templine);
	}
	print WISH &createText;
    }
    elsif ( $nminormodes > 1 ) {
	&print_text( "Choose one ( or more ) minor modal configuration:\n\n" );
	$templine = sprintf("  NEVENTS   ONTIME  $catformat{$chosen_type}",@keylist);
	chop $templine;

	print WISH<<"EOMODEHEADER";
catch {set font [option get .c nonPropFont Font]}

frame $base -relief ridge -borderwidth 4
.c create window  315 \\
   [expr [lindex [.c bbox $textWindow] 3] + 2 * \$scrollInc] \\
   -tags minorMode -window $base -anchor n
frame $base.header -relief groove 
label $base.header.f -text "    " -width 3
label $base.header.l -relief groove -text {$templine}
catch {$base.header.l configure -font \$font}
pack $base.header -side top -ipady 6 -pady 2 -fill x -expand true
pack $base.header.f -side left -ipadx 1
pack $base.header.l -side left -fill x -expand true
frame $base.body -relief sunken
pack $base.body -side top -ipady 2 -pady 2 -fill x -expand true
EOMODEHEADER

        $templine = sprintf("%7d  %7g $catformat{$chosen_type}",
			&twodrow(0,$#keylist+2,(nevents,ontime,@keylist),%keyarray));
        chop $templine;

        print WISH<<"EOSTARTMODE";
frame $base.body.line0
checkbutton $base.body.line0.b -text \" 0\"  -variable mode(0) -width 3
label $base.body.line0.l -text {$templine} 
catch {$base.body.line0.l configure -font \$font}
pack $base.body.line0 -fill x -expand true -ipadx 6 -side top
pack $base.body.line0.b -side left -ipadx 1
pack $base.body.line0.l -side left -fill x -expand true
EOSTARTMODE

        for ( $i = 1; $i < $nminormodes; $i++ ) {
	    $templine = sprintf("%7d  %7g $catformat{$chosen_type}",
	          &twodrow($i,$#keylist+2,(nevents,ontime,@keylist),%keyarray));
	    chop $templine;
	    $iminus = $i - 1;

	    print WISH<<"EOMODES";
frame $base.body.line$i
checkbutton $base.body.line$i.b -text \" $i\" -variable mode($i) -width 3
label $base.body.line$i.l -text {$templine}
catch {$base.body.line$i.l configure -font \$font}
pack $base.body.line$i -fill x -expand true -side top -ipadx 6
pack $base.body.line$i.b -side left -ipadx 1
pack $base.body.line$i.l -side left -fill x -expand true
EOMODES

        }
        $iminus = $i - 1;

        print WISH<<"EOMODELINE";
set OK 0
frame $base.oK -relief sunken
button $base.oK.b  -text "OK" -relief raised -command {
      global OK; set OK 1}
pack $base.oK -side top -expand true -fill x -pady 2 -ipady 4
pack $base.oK.b -side bottom -fill y -expand true -ipadx 20
if { \$tk_version < 4.0 } {
    .c yview [expr int( [lindex [.c bbox minorMode] 1]/\$scrollInc) - 6] 
} else {
    set tempvar [expr double([lindex [.c bbox minorMode] 1])/double(\$scrollRegSize)]
    .c yview moveto [expr \$tempvar - .013]
}
tkwait variable OK
for {set i 0} {\$i<$nminormodes} {incr i} {
   append modeSelection \$mode(\$i) " " 
}
puts stdout "MODESEL> \$modeSelection"; flush stdout
EOMODELINE
    
        $textWindow =~ tr/0123456789/1234567890/;
print WISH<<EOCRTXT2; 
.c create text 10 [expr [lindex [.c bbox minorMode] 3] + 2 * \$scrollInc ] \\
        -anchor nw  -tags $textWindow
catch {set font [option get .c textFont Font]}
set error [catch {.c itemconfigure $textWindow -font \$font}] 
EOCRTXT2
    }
    else {
        &print_warn("\nNone of the files of type $instru - $datamode pass the",
               " standard selection criteria.\n");
        &print_text("Bye!\n");
        &exit_wish();
    }
#&exit_wish();
}
############################################################
# SUBROUTINE: getMinorModes
# ARGUMENTS:
#   
# DESCRIPTION:
#  This reads back in the minor mode selections. 
#############################################################

sub getMinorModes
{
    local($i);
    while ( <WISH> ) {
	if ( /^MODESEL> +([01][01 ]*)$/ ) {
	    @modesel = split(" ",$1);
	    last;
	}
    }
    $#moderange = -1;
    for ( $i=0; $i <= $#modesel; $i++ ) {
	if ( $modesel[$i] == 1 ) { 
	    push(@moderange,$i);
	}
    }
}

############################################################
# SUBROUTINE: print_text
# ARGUMENTS:
#   @printline - Lines of text to print
# DESCRIPTION:
# Prints an array of text to the current text window (given by the 
# global variable $textWindow) in the canvas .c, and scrolls the canvas
# as needed.  
# If no textWindow has yet been defined, the message is stored for printing 
# when it is defined.
#############################################################
sub print_text
{
    local(@printline) = @_;

    if( defined $textWindow) {
    print WISH ".c insert $textWindow end \"",@printline,"\"\n";

#    print ".c insert $textWindow end \"",@printline,"\"\n";

    print WISH "scrollCanvas $textWindow\n";
    }
    else {
	$storedMsg .= join("",@printline)."\n";
    }
}
############################################################
# SUBROUTINE: create_table
# ARGUMENTS:
#   class name for nonProp font.
# DESCRIPTION:
# Starts a new text window to contain a table.  Uses a non-proportional
# font to store info, passed as the name of the X-default
#############################################################
sub createTable
{
    local ($tmpLine);
   $tmpline = "set begPos [lindex [.c bbox $textWindow] 3]\n".
              "if {\$begPos == \"\"} {set begPos 0}\n"; 
    $textWindow =~ tr/0123456789/1234567890/;
   $tmpline.".c create text 10 \$begPos -anchor nw -tags $textWindow\n".
   "catch {set font [option get .c $_[0] Font]}\n".
   "set error [catch {.c itemconfigure $textWindow -font \$font}]\n". 
   "if {\$error} {\n". 
       "putMessage \"Error reading $_[0] \$font\"\n". 
   "}\n";
   
}

############################################################
# SUBROUTINE: insertInCanvas
# ARGUMENTS:
#   
# DESCRIPTION:
#  This returns a text line to insert text into the  textWindow in 
#  canvas .c.  Used for here documents to WISH   
#############################################################
sub insertInCanvas
{
    (".c insert $textWindow end \"",
     "\"\n scrollCanvas $textWindow");
}
############################################################
# SUBROUTINE: createText
# ARGUMENTS:
#   
# DESCRIPTION:
#  This returns a text line to insert a new textWindow in 
#  canvas .c.  Used for here documents to WISH     
#############################################################

sub createText
{

    local ($tmpLine);
   $tmpline = "set begPos [lindex [.c bbox $textWindow] 3]\n".
              "if {\$begPos == \"\"} {set begPos 0}\n"; 
    $textWindow =~ tr/0123456789/1234567890/;
$tmpline.".c create text 10 \$begPos -anchor nw -tags $textWindow\n".
"catch {set font [option get .c textFont Font]}\n".
"set error [catch {.c itemconfigure $textWindow -font \$font}]\n". 
"if {\$error} {\n". 
    "putMessage \"Error reading textFont \$font\"\n". 
"}\n";
}
############################################################
# SUBROUTINE: clickToDismiss
# ARGUMENTS:
#   
# DESCRIPTION:
#  This halts processing until a click is registered in the canvas
#  .c
#############################################################
sub clickToDismiss
{
    &print_text("      Click anywhere to dismiss window\n");
    print WISH<<'EOCTD';
bind all <1> {+
   global OK 
   set OK [expr $OK + 1]
}
tkwait variable OK
bind all <1> {}
exit
EOCTD
}
############################################################
# SUBROUTINE: exit_wish
# ARGUMENTS:
#   
# DESCRIPTION:
# Exits the perl script, killing the spawned wish as well
#############################################################
sub exit_wish
{
    print "Bye, now!\n";
    print WISH "exit\n";
    exit;
}
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# This is the end of the section of interface routines that must
# appear in both TK and Command line versions.
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

############################################################
# SUBROUTINE: primeWish
# ARGUMENTS:
#   
# DESCRIPTION:
# Sets up necessary globals, and initializes the canvas .c, and
# sends a bunch of necessary Tcl proc's
#############################################################
sub primeWish
{
    local($sysdir) = shift;


	print WISH<<'EODEFINESA';
global mainResults

set mainResults(dirPath) ""
set mainResults(datamode) ""
set mainResults(instru) ""
set mainResults(minorMode) ""
set mainResults(elvMin) ""
set mainResults(corMin) ""
set mainResults(brEarth) ""
set mainResults(sisThresh) ""
set mainResults(fastSel) ""
set mainResults(riseTime) ""

global instInfo

set instInfo(keys) [list {SIS0-FAINT} \
		    {SIS0-BRIGHT} \
		    {SIS0-BRIGHT2} \
		    {SIS0-FAST} \
		    {SIS1-FAINT} \
		    {SIS1-BRIGHT} \
		    {SIS1-BRIGHT2} \
		    {SIS1-FAST} \
		    {GIS2-PH} \
		    {GIS2-MPC} \
		    {GIS3-PH} \
		    {GIS3-MPC}]
EODEFINESA

	print WISH<<"EODEFINESB";
set instInfo(SIS0-FAINT)  {/(ft|ad).+[Ss]0.+01[HhMmLl]\\.}    
set instInfo(SIS0-BRIGHT) {/(ft|ad).+[Ss]0.+02[HhMmLl]\\.}   
set instInfo(SIS0-BRIGHT2) {/(ft|ad).+[Ss]0.+12[HhMmLl]\\.}   
set instInfo(SIS0-FAST)   {/(ft|ad).+[Ss]0.+03[HhMmLl]\\.}   
set instInfo(SIS1-FAINT)  {/(ft|ad).+[Ss]1.+01[HhMmLl]\\.}   
set instInfo(SIS1-BRIGHT) {/(ft|ad).+[Ss]1.+02[HhMmLl]\\.}   
set instInfo(SIS1-BRIGHT2) {/(ft|ad).+[Ss]1.+12[HhMmLl]\\.}   
set instInfo(SIS1-FAST)   {/(ft|ad).+[Ss]1.+03[HhMmLl]\\.}   
set instInfo(GIS2-PH)     {/(ft|ad).+[Gg]2.+70[HhMmLl]\\.}   
set instInfo(GIS2-MPC)    {/(ft|ad).+[Gg]2.+71[HhMmLl]\\.}   
set instInfo(GIS3-PH)     {/(ft|ad).+[Gg]3.+70[HhMmLl]\\.}   
set instInfo(GIS3-MPC)    {/(ft|ad).+[Gg]3.+71[HhMmLl]\\.}

set exten $exten
    
EODEFINESB

	print WISH<<'EODEFINES';
global fsBox

set fsBox(home) [pwd]
set fsBox(path) [pwd]
set fsBox(oldPath) $fsBox(path) 
set fsBox(list) ""

global modeBox

set modeBox(select) ""
set modeBox(session) "xsel"
set modeBox(status) "ok"
set modeBox(high) 1
set modeBox(medium) 1
set modeBox(low) 0

global scrollInc
set scrollInc 10
global tk_version

global ListONumbers
   set ListONumbers [list 1 2 3 4 5 6 7 8 9 0 {period} {minus} {plus}]
   set PrintedNumbers [list 1 2 3 4 5 6 7 8 9 0 . - +]
   set PassMe [list {Left} {Right} {Up} {Down} {Shift_L} {Shift_R} \
                     {Delete} ]
   set PassActions [list \
           {$entryName icursor [expr  [$entryName index insert] - 1]} \
           {$entryName icursor [expr  [$entryName index insert] + 1]} \
           {} {} {} {} {deleteSelection $entryName}]
#End of DEFINES dump           
EODEFINES

	print WISH<<'EOPROCS1';


proc puts_flush {message {stream stdout}} {
    puts $stream $message
    flush $stream
}

proc printListBox {boxName} {
    set numElem [$boxName size]
    for {set i 0} {$i<$numElem} {incr i} {
	puts_flush [$boxName get $i]
    }
}


proc relPositionWindow { window width height relX relY {refWindow .}} { 

    set xcorner [expr [winfo vrootx $refWindow] + \
                           [winfo rootx $refWindow] + $relX]
    set ycorner [expr [winfo vrooty $refWindow] + \
		 [winfo rooty $refWindow] + $relY]

    wm sizefrom $window user
    wm positionfrom $window user
    if { $width == "" } {
       if { $xcorner > 0 } {
         if { $ycorner > 0 } {
             wm geometry $window +$xcorner+$ycorner
         } else {
             wm geometry $window +$xcorner$ycorner
          }
       } else { 
          if { $ycorner > 0 } {
              wm geometry $window $xcorner+$ycorner
          } else {
              wm geometry $window $xcorner$ycorner
          }
       }
    } else {
       if { $xcorner > 0 } {
         if { $ycorner > 0 } {
             wm geometry $window ${width}x${height}+$xcorner+$ycorner
         } else {
             wm geometry $window ${width}x${height}+$xcorner$ycorner
          }
       } else { 
          if { $ycorner > 0 } {
              wm geometry $window ${width}x${height}$xcorner+$ycorner
          } else {
              wm geometry $window ${width}x${height}$xcorner$ycorner
          }
       }

    }

}

proc absPositionWindow { window width height {relX 0} {relY 0}} {
 
# This will place the window in the middle of the screen if
# relX = relY = 0.  Otherwise offset by this much in Pixels.

    wm sizefrom $window user
    wm positionfrom $window user
    if { $width == "" } {
       set xcorner [expr ( [winfo screenwidth $window] ) / 2 \
              + [winfo vrootx $window] + $relX]
       set ycorner [expr ( [winfo screenheight $window] ) / 2 \
              + [winfo vrooty $window] + $relY]
       if { $xcorner > 0 } {
         if { $ycorner > 0 } {
             wm geometry $window +$xcorner+$ycorner
         } else {
             wm geometry $window +$xcorner$ycorner
          }
       } else { 
          if { $ycorner > 0 } {
              wm geometry $window $xcorner+$ycorner
          } else {
              wm geometry $window $xcorner$ycorner
          }
       }
    } else {
       set xcorner [expr ( [winfo screenwidth $window] - $width ) / 2 \
              + [winfo vrootx $window] + $relX]
       set ycorner [expr ( [winfo screenheight $window] - $height ) / 2 \
              + [winfo vrooty $window] + $relY]
       if { $xcorner > 0 } {
         if { $ycorner > 0 } {
             wm geometry $window ${width}x${height}+$xcorner+$ycorner
         } else {
             wm geometry $window ${width}x${height}+$xcorner$ycorner
          }
       } else { 
          if { $ycorner > 0 } {
              wm geometry $window ${width}x${height}$xcorner+$ycorner
          } else {
              wm geometry $window ${width}x${height}$xcorner$ycorner
          }
       }

    }
}
#End of PROCS1 dump
EOPROCS1

	print WISH<<'EOPROCS2';

proc putMessage {message} {
    global tk_version
     catch {destroy .mFmessage}
    if { $tk_version < 4.0 } {
        toplevel .mFmessage -geometry 300x100
     } else {
         toplevel .mFmessage -width 300 -height 100
     }
     absPositionWindow .mFmessage "" "" 0 -300
     frame .mFmessage.outer -relief ridge -borderwidth 2
     message .mFmessage.outer.chooseMode -aspect 300\
	    -relief groove -borderwidth 2\
	    -text $message
#    puts stdout [.mFmessage.outer.chooseMode configure]
     button .mFmessage.outer.okay -text "OK" \
	    -relief raised -padx 30 -pady 3\
	    -command {destroy .mFmessage}
     bind .mFmessage <Return> {destroy .mFmessage}
     wm title .mFmessage "Ascascreen Warning"
     pack .mFmessage.outer -side top -fill both -expand true
     pack .mFmessage.outer.chooseMode -side top  -padx 20 -pady 20 \
            -fill both -expand true
     pack .mFmessage.outer.okay -side bottom -fill x
    focus .mFmessage
    tkwait window .mFmessage
}

proc putMssgYN {message {default ""}} {
    global yesOrNo
    catch {destroy .mFmessage}

    toplevel .mFmessage  
    absPositionWindow .mFmessage "" "" 0 -300
    wm title .mFmessage {Ascascreen Warning}
    if {[string first y $default] != -1 } {
	bind .mFmessage <Return> {global yesOrNo;set yesOrNo 1}
    } elseif {[string first n $default] != -1 } {
        bind .mFmessage <Return> {global yesOrNo;set yesOrNo 0}
    }
    frame .mFmessage.outer -relief ridge -borderwidth 2
    message .mFmessage.outer.message -aspect 300 -relief groove  \
           -borderwidth 2 -text $message
    frame .mFmessage.outer.yesOrNo -relief sunken -borderwidth 2
    set yesOrNo -1
    button .mFmessage.outer.yesOrNo.yes -text "Yes" \
	-relief raised \
	-command {global yesOrNo; set yesOrNo 1}
    button .mFmessage.outer.yesOrNo.no -text "No" \
	-relief raised \
	-command {global yesOrNo; set yesOrNo 0}
    pack .mFmessage.outer -side top -fill both -expand true
    pack .mFmessage.outer.message -side top -fill both -expand true \
	-ipadx 20 -ipady 20 -padx 5 -pady 3
    if {[string first y $default] != -1 } {
	frame .mFmessage.outer.yesOrNo.default -relief sunken -borderwidth 1
	pack .mFmessage.outer.yesOrNo.default -side left \
                -padx 30 -pady 2 -ipadx 4 -ipady 2 -expand true -fill x
	raise .mFmessage.outer.yesOrNo.yes
        pack .mFmessage.outer.yesOrNo.yes -in .mFmessage.outer.yesOrNo.default\
                -ipadx 10 -pady 3 -fill y -expand true 
        pack .mFmessage.outer.yesOrNo.no \
                -side right -padx 30 -pady 3 -ipadx 10 
    } elseif {[string first n $default] != -1 } {
	frame .mFmessage.outer.yesOrNo.default -relief sunken -borderwidth 1
        pack .mFmessage.outer.yesOrNo.yes\
                -side left -padx 30 -pady 3 -ipadx 10
	pack .mFmessage.outer.yesOrNo.default -side right \
                -padx  30 -pady 2 -ipadx 4 -ipady 2 -expand true -fill x
	raise .mFmessage.outer.yesOrNo.no
        pack .mFmessage.outer.yesOrNo.no -in .mFmessage.outer.yesOrNo.default\
                 -ipadx 12 -pady 3 -fill y -expand true  
    } else {
        pack .mFmessage.outer.yesOrNo.yes \
                -side left -padx 30 -pady 3 -ipadx 10
        pack .mFmessage.outer.yesOrNo.no \
                -side right -padx 30 -pady 3 -ipadx 12
    }
    pack .mFmessage.outer.yesOrNo -side bottom -fill x -expand true -padx 5 \
	-pady 5 
    focus .mFmessage
    tkwait variable yesOrNo
    destroy .mFmessage
    puts stdout "MESSAGE> $yesOrNo";flush stdout
}
#End of PROCS2 dump
EOPROCS2

	print WISH<<'EOPROCS3';

proc getOneFromList {prompt refer default {diagName .dialog}} {

    global result
    set reflist [split $refer]

    catch {destroy $diagName}
    toplevel $diagName -relief ridge -borderwidth 2
    absPositionWindow $diagName "" "" -100 -300
    label $diagName.label -text $prompt -relief groove
    pack $diagName.label -side top -ipadx 10 -ipady 5 -fill x -expand true \
	-padx 4 -pady 2
    frame $diagName.check -relief groove -borderwidth 2
    set i 0
    set result [lsearch -exact $reflist $default]
    foreach refelem $reflist {
	frame $diagName.check.check$i
	radiobutton $diagName.check.check$i.b -text " " -relief raised\
	    -variable result -value $i
	label $diagName.check.check$i.l -text $refelem
	pack $diagName.check.check$i.b -padx 2 -pady 2 -expand 0 -side left
	pack $diagName.check.check$i.l -side left -fill none -expand 0
	pack $diagName.check.check$i -side top -fill x -expand true \
	    -padx 5
	set i [incr i]
    }
    pack $diagName.check -padx 4 -pady 2 -ipadx 10 -ipady 5 \
	-expand 1 -fill both -side top

    frame $diagName.ok -relief sunken
    button $diagName.ok.b -relief raised -text OK -command "destroy $diagName"

    pack $diagName.ok -side top -expand 1 -fill x -ipadx 10  -pady 2\
          -padx 8
    pack $diagName.ok.b -side bottom -ipadx 20 -pady 2 -fill none -expand 0
    
    tkwait window $diagName
    puts stdout "MESSAGE> RESULT $result"; flush stdout 

}

proc getManyFromList {prompt refer defaults {diagName .dialog}} {

    global result
    set reflist [split $refer]
    set deflist [split $defaults]

    catch {destroy $diagName}
    toplevel $diagName -relief ridge -borderwidth 2
    absPositionWindow $diagName "" "" -100 -300
    label $diagName.label -text $prompt -relief groove
    pack $diagName.label -side top -ipadx 10 -ipady 5 -fill x -expand true \
	-padx 4 -pady 2
    frame $diagName.check -relief groove -borderwidth 2

    set i 1
    foreach refelem $reflist {
	if { [lsearch -exact $deflist $refelem] == -1 } {
	    set result($refelem) 0
	} else {
	    set result($refelem) 1
	}

	frame $diagName.check.check$i
	checkbutton $diagName.check.check$i.b -text " " -relief raised\
	    -variable result($refelem) -onvalue 1 -offvalue 0
	label $diagName.check.check$i.l -text $refelem
	pack $diagName.check.check$i.b -padx 2 -pady 2 -expand 0 -side left
	pack $diagName.check.check$i.l -side left -fill none -expand 0
	pack $diagName.check.check$i -side top -fill x -expand true \
	    -padx 5
	set i [incr i]
    }

    pack $diagName.check -padx 4 -pady 2 -ipadx 10 -ipady 5 \
	-expand 1 -fill both -side top

    frame $diagName.ok -relief sunken
    button $diagName.ok.b -relief raised -text OK -command "destroy $diagName"

    pack $diagName.ok -side top -expand 1 -fill x -ipadx 10 -ipady 2 \
            -padx 2 -pady 2
    pack $diagName.ok.b -side bottom -ipadx 20 -pady 2 -fill none -expand 0
    
    tkwait window $diagName
    foreach refelem $reflist {
	if {$result($refelem) != 0} {
	    puts stdout "MESSAGE> $refelem"; flush stdout
        } 
    }
    puts stdout "MESSAGE> EXIT";flush stdout
}
#End of PROCS3 dump
EOPROCS3

	print WISH<<'EOPROCS4';


proc putSimpleDialog {message {default ""} {diagName .mFdialog}} {
    global reply
    global tk_version
    catch {destroy $diagName}
    set reply $default
    if { $tk_version < 4.0 } {
       toplevel $diagName -geometry 300x100
    } else {
	toplevel $diagName -width 300 -height 100
    }
    wm title $diagName {Ascascreen Dialogue}
    absPositionWindow $diagName "" "" 0 -200
    frame $diagName.outer -relief ridge -borderwidth 2
    message $diagName.outer.message -aspect 300 -relief groove -borderwidth 2 \
           -text $message
    entry $diagName.outer.reply -relief sunken -borderwidth 2 -textvariable reply
    bind $diagName.outer.reply <Return> "destroy $diagName"
    frame $diagName.outer.ok -relief sunken
    button $diagName.outer.ok.b -relief raised -text submit \
        -command "destroy $diagName"
    pack $diagName.outer -side top -fill both -expand true
    pack $diagName.outer.message -side top -fill both -expand true \
	-ipadx 20 -ipady 20 -padx 5 -pady 3
    pack $diagName.outer.reply -side top -fill x -expand true -padx 5 \
	-pady 5 
    pack $diagName.outer.ok -side top -expand 1 -fill x -ipadx 10 -ipady 2 \
            -padx 2 -pady 2
    pack $diagName.outer.ok.b -side bottom -ipadx 10 -pady 2 -fill none -expand 0
    focus $diagName.outer.reply
    tkwait window $diagName
    puts stdout "MESSAGE> $reply";flush stdout
}

proc putNumberDialog {message {default ""} {diagName .mFdialog}} {
    global reply
    global tk_version
    catch {destroy $diagName}
    set reply $default
    if { $tk_version < 4.0 } {
       toplevel $diagName -geometry 300x100
    } else {
	toplevel $diagName -width 300 -height 100
    }
    absPositionWindow $diagName 300 100 0 -200
    wm title $diagName {Ascascreen Dialogue}
    frame $diagName.outer -relief ridge -borderwidth 2
    message $diagName.outer.message -aspect 300 -relief groove -borderwidth 2 \
           -text $message
    entry $diagName.outer.reply -relief sunken -borderwidth 2 -textvariable reply
    bind $diagName.outer.reply <Return> "destroy $diagName"
    if { $tk_version < 4.0 } {
        bind  $diagName.outer.reply <Any-Key> "[bind Entry <Any-Key>] \
                onlyNumbers $diagName.outer.reply %K"
    } else {
        bind  $diagName.outer.reply <Any-Key> "+ \
                onlyNumbers $diagName.outer.reply %K"
    }
    bind  $diagName.outer.reply <Delete> "+ \
		deleteSelection $diagName.outer.reply"

    pack $diagName.outer -side top -fill both -expand true
    pack $diagName.outer.message -side top -fill both -expand true \
	-ipadx 20 -ipady 20 -padx 5 -pady 3
    pack $diagName.outer.reply -side bottom -fill x -expand true -padx 5 \
	-pady 5 
    focus $diagName.outer.reply
    tkwait window $diagName
    puts stdout "MESSAGE> $reply";flush stdout
}

proc deleteSelection {entryName} {
    set isSel [catch "$entryName delete sel.f sel.l"]
    if {$isSel != 0} {
	set insertPoint [$entryName index insert]
	$entryName delete [expr $insertPoint - 1]
    }
}
#End of PROCS4 dump
EOPROCS4

	print WISH<<'EOPROCS5';


proc onlyNumbers {entryName value} {
   global ListONumbers
   global PrintedNumbers
   global PassMe
   global PassActions

   set position [lsearch -exact $PassMe $value]
   if {$position != -1} {
        eval [lindex $PassActions $position]
        return
   }
   set position [lsearch -exact $ListONumbers $value]
   if { $position == -1 } {
      putMessage {Please enter a number}
      set insertPt [$entryName index insert]
      $entryName delete [expr $insertPt - 1]
      
   } else {
       set insertPt [$entryName index insert ]
       if {$insertPt  != 1 && $position > 10 } {
         if { [regexp -- [string range [$entryName get] 0 \
                          [expr $insertPt - 1]] {^ *$}] == 0} {
             putMessage "A [lindex $PrintedNumbers $position] can only begin a number"
             $entryName delete [expr $insertPt - 1]
             return
         }
       } elseif { $position == 10 } {
             
             if { [string match {*.*.} [$entryName get]] == 1} {
             putMessage "A number can only have one dot"
             $entryName delete [expr $insertPt - 1]
             return
         }
       } 
   }
   
}
 
proc scrollCanvas {textWindow {fast 0} {canvas ".c"}} {
   global scrollInc
   global scrollRegSize
   global tk_version
   set cHeight [winfo height $canvas]
   set bPos [$canvas canvasy $cHeight]
   set bText [lindex [$canvas bbox $textWindow] 3]
 
   if {$bText>$bPos} {
       if { $tk_version < 4.0 } {
	 set target [expr $bText - int($cHeight/2.0)] 
         if {$target<$bPos} {
	     set target [expr int($bText - $cHeight)]
         }
         if {$fast} {
	    $canvas yview [expr int($target/$scrollInc)]
         } else {
           set tPos [expr int($bPos - $cHeight) ]
           set nsteps [expr int(($target - $tPos)/$scrollInc)] 
           set winTop  [expr  int( $tPos / $scrollInc ) ]
           for {set i 0} {$i < $nsteps} {incr i} {
             set winTop [expr $winTop + 1  ]
             $canvas yview $winTop
           }
         }
     } else {
	 set topfrac [expr (double($bText)-double($cHeight)/2.0)/double($scrollRegSize)]
         $canvas yview moveto $topfrac
     }
   }
      
} 
#End of PROCS5 dump       
EOPROCS5
#
# Look for an apps-def file:
# And start up our window:
#

$appsDefault = "Tkascascreen.def";

    print WISH<<"EOAPPSDEF";

proc packMainCanvas {refWindow} {
   global scrollInc
   global tk_version
   global scrollRegSize
   set xsize [expr [lindex [.c bbox \$refWindow] 2] + 10] 
   set ysize [expr [lindex [.c bbox \$refWindow] 2] + 10]
   if {\$xsize < 650} { set xsize 650 }
   if {\$ysize < 450} { set xsize 450 }
   absPositionWindow . \$xsize \$ysize
   .c configure -height \$ysize -width [expr \$xsize - 25] 
   pack .cscroll -side right -fill y -expand true
   pack .c -side left -fill both -expand true
   tkwait visibility .c
   if { [set tk_version] >= 4.0 } {
       set scrollInc 1
   } else {
       set scrollInc [expr double([lindex [.c configure -scrollincrement] 4])]
   }
}


global resFont
catch {destroy .label}
label .label
set resFont [lindex [.label configure -font] 4]


if {[file exists ./$appsDefault]} {
    set error [catch {option readfile ./$appsDefault}]
    if {\$error} { putMessage "Error reading defaults from ./$appsDefault"}
} elseif {[file exists $ENV{HOME}/$appsDefault]} {
    set error [catch {option readfile $ENV{HOME}/$appsDefault}]
    if {\$error} { putMessage "Error reading defaults from $ENV{HOME}/$appsDefault"}
} elseif {[ file exists $ENV{XAPPLRESDIR}/$appsDefault]} {
    set error [catch {option readfile $ENV{XAPPLRESDIR}/$appsDefault}]
    if {\$error} { putMessage "Error reading defaults from $ENV{XAPPLRESDIR}/$appsDefault"}
} elseif { [file exists  {$sysdir/$appsDefault}]} {
    set error [catch {option readfile $sysdir/$appsDefault}]
    if {\$error} { putMessage "Error reading defaults from $sysdir/$appsDefault"}
}

set font [option get .label font Font]
set error [catch {.label configure -font \$font}]
destroy .label


if { \$error } {
   canvas .c
   set textFont [option get .c textFont Font]
   option add Tk*Font \$resFont 
   option add Tk.c.*textFont \$textFont
   destroy .c
}


wm title . "Ascascreen"
wm iconname . "Ascascreen"
wm maxsize . 1000 1000
scrollbar .cscroll -command ".c yview" -relief sunken 
canvas .c -yscrollcommand ".cscroll set" -height 450 -width 625 -scrollregion [list 0 0 600 5000]
set scrollRegSize 5000
. configure -relief ridge -borderwidth 3


EOAPPSDEF

}

############################################################
# SUBROUTINE: putDirAndModeBox
# ARGUMENTS:
#   $datadir   - Optional: starting directory, if undefined, none is used
# DESCRIPTION:
# Sends to WISH the putDirAndMode proc which packs the directory and mode 
# selection box.  Also a few ancillary proc's.   Runs the proc.  Returns
# the variables from Tcl preceded by the string "DirAndMode> "
#############################################################
sub putDirAndModeBox{

    print WISH<<'EODIRMODE';
proc putDirAndMode {} {
    global mainResults
    global fsBox
    global modeBox
    global instInfo
    global exten
    global tk_version

    set fsBox(home) [pwd]
    set fsBox(oldPath) $fsBox(path)
    set fsBox(list) ""

    set frameBorder 4

    toplevel .dirAndMode 

    wm title .dirAndMode {Ascascreen Files}
    wm iconname .dirAndMode {Ascascreen Files}
    wm maxsize .dirAndMode 1000 1000
    wm minsize .dirAndMode 420 600
    relPositionWindow .dirAndMode 420 700 100 -125

# end build of toplevel

    frame .dirAndMode.fsBox -relief ridge -borderwidth $frameBorder

    label .dirAndMode.label -text "Choose a directory:"

# This is the Directory Box part:

    label .dirAndMode.fsBox.mainLabel -text "Subdirectories:"

    listbox .dirAndMode.fsBox.fsBox -relief sunken -borderwidth 2 \
	-yscrollcommand ".dirAndMode.fsBox.fsScrollY set" 
#	-xscrollcommand ".dirAndMode.fsBox.fsScrollX set"

#    scrollbar .dirAndMode.fsBox.fsScrollX -orient horizontal -width 10\
#	-command {.dirAndMode.fsBox.fsBox xview} \
#	-relief raised

    scrollbar .dirAndMode.fsBox.fsScrollY -orient vertical -width 15\
	-command {.dirAndMode.fsBox.fsBox yview} \
	-relief raised

    if { $tk_version < 4.0 } {
	tk_listboxSingleSelect .dirAndMode.fsBox.fsBox
    } else {
	.dirAndMode.fsBox.fsBox configure -selectmode browse
    }

# This is the up directory button:

    button .dirAndMode.fsBox.upDir -text "up" -relief raised \
	-command {
	    global fsBox
	    set fsBox(path) [upDir $fsBox(path)]
	    set fsBox(oldPath) $fsBox(path)
	    fsBoxFillDirlist
	    modeBoxFillSelections $fsBox(path)
	}

# This is the text directory entry box and its label:

    label .dirAndMode.fsBox.dirLabel -text "Directory:"
    entry .dirAndMode.fsBox.dirName -relief sunken -textvariable fsBox(path)
    bind .dirAndMode.fsBox.dirName <Return> {
	global fsBox
	set newPath [.dirAndMode.fsBox.dirName get]
	if {[file isdirectory $newPath]} {
	    set fsBox(path) $newPath
#strip final / from $fsBox(path):
	    set fsBox(path) [string trimright $fsBox(path) { /}]
	    puts stdout "$fsBox(path)"
	    set fsBox(oldPath) $fsBox(path)
	    fsBoxFillDirlist
	    modeBoxFillSelections $fsBox(path)
	} else {
	    set fsBox(path) $fsBox(oldPath)
	    putMessage "Directory $newPath does not exist"
	}
    }

    bind .dirAndMode.fsBox.dirName <Delete> \
	{+deleteSelection .dirAndMode.fsBox.dirName}
# Bind double click in the directory list to CD:

    bind .dirAndMode.fsBox.fsBox <Double-Button-1> {
	global fsBox
	set Nearest [.dirAndMode.fsBox.fsBox nearest %y]
	if { $Nearest >= 0 } {
	    set fsBox(path) $fsBox(path)/[.dirAndMode.fsBox.fsBox get $Nearest]
	    set fsBox(oldPath) $fsBox(path)
	    fsBoxFillDirlist
	    modeBoxFillSelections $fsBox(path)
	}
    }
	

    frame .dirAndMode.modeBox -relief ridge -borderwidth $frameBorder

    frame .dirAndMode.modeBox.exten -relief ridge
    label .dirAndMode.modeBox.exten.label -text "File Extension:"
    radiobutton .dirAndMode.modeBox.exten.fits -variable exten \
	-value fits -text "fits" -command {modeBoxFillSelections $fsBox(path)}
    radiobutton .dirAndMode.modeBox.exten.unf -variable exten \
	-value unf -text "unf" -command {modeBoxFillSelections $fsBox(path)}


    label .dirAndMode.modeBox.label -text "Available Instrument-datamodes"

# This is the available mode list box:

    listbox .dirAndMode.modeBox.modeBox -relief sunken -borderwidth 2 \
	-yscrollcommand ".dirAndMode.modeBox.mbScrollY set" 

    scrollbar .dirAndMode.modeBox.mbScrollY \
	-command {.dirAndMode.modeBox.modeBox yview} \
	-relief raised

    if { $tk_version < 4.0 } {
	tk_listboxSingleSelect .dirAndMode.modeBox.modeBox
    } else {
	.dirAndMode.modeBox.modeBox configure -selectmode browse
    }

# Bind double click in the mode box to select mode & return:
    bind .dirAndMode.modeBox.modeBox <Button-1> "[bind Listbox <Button-1>] ; \
                      doModeBox %y"
    proc doModeBox {clickLoc} {
	global modeBox
	set Nearest [.dirAndMode.modeBox.modeBox nearest $clickLoc]
	if { $Nearest >= 0 } {
	    set modeBox(select) [.dirAndMode.modeBox.modeBox get $Nearest]
	}
    }

    bind .dirAndMode.modeBox.modeBox <Double-Button-1> {
	global modeBox
	set Nearest [.dirAndMode.modeBox.modeBox nearest %y]
	if { $Nearest >= 0 } {
	    set modeBox(select) [.dirAndMode.modeBox.modeBox get $Nearest]
	    destroy .dirAndMode}
    }

    bind .dirAndMode.modeBox.modeBox <Double-Button-2> {
	printListBox .dirAndMode.modeBox.modeBox
    }
    label .dirAndMode.modeBox.chosen -text "Mode-Instrument"
    label .dirAndMode.modeBox.chosenName -relief sunken \
	    -textvariable modeBox(select) 
    menubutton .dirAndMode.modeBox.bitRate -text {Bit Rates} \
        -menu  .dirAndMode.modeBox.bitRate.m -relief raised -borderwidth 2
    menu .dirAndMode.modeBox.bitRate.m
    .dirAndMode.modeBox.bitRate.m add checkbutton -label High \
         -offvalue 0 -onvalue 1 -variable modeBox(high)
    .dirAndMode.modeBox.bitRate.m add checkbutton -label Medium \
         -offvalue 0 -onvalue 1 -variable modeBox(medium)
    .dirAndMode.modeBox.bitRate.m add checkbutton -label Low \
         -offvalue 0 -onvalue 1 -variable modeBox(low)
#    checkbutton .dirAndMode.modeBox.useLow -text "Use Low Bit-rate" \
#	-variable modeBox(useLow)
 
    

# This is the entry for the session name

    frame .dirAndMode.session -relief ridge -borderwidth $frameBorder
    label .dirAndMode.session.label -text "Session Name"
    entry .dirAndMode.session.name -relief sunken \
	    -textvariable modeBox(session) 

    bind .dirAndMode.session.name <Delete> \
	{+deleteSelection .dirAndMode.session.name}
    bind .dirAndMode.session.name <Return> dirAndModeOkay
	

# These are the bottom row buttons:

    frame .dirAndMode.buttons -relief ridge -borderwidth $frameBorder

# This is the Okay button:

    button .dirAndMode.buttons.okay -text okay -relief raised \
	-command dirAndModeOkay	    
    button .dirAndMode.buttons.cancel -text cancel -relief raised -command {
	global fsBox
	global modeBox

	set fsBox(path) ""
	set modeBox(select) ""
	set modeBox(status) "cancel"
	destroy .dirAndMode
    }

# Fill the two list boxes:
    fsBoxFillDirlist
    modeBoxFillSelections $fsBox(path)


     pack .dirAndMode.fsBox -side top -fill x -expand true
     pack .dirAndMode.fsBox.mainLabel -side top -fill x
     pack .dirAndMode.fsBox.upDir  -side top -pady 3 -ipadx .5c -anchor c  
     pack .dirAndMode.fsBox.fsScrollY -side right -fill y  
     pack .dirAndMode.fsBox.fsBox -fill both -expand true 
     pack .dirAndMode.fsBox.dirLabel -side left -fill x 
     pack .dirAndMode.fsBox.dirName -side left -fill x -expand true

     pack .dirAndMode.modeBox -side top -fill x -expand true
     pack .dirAndMode.modeBox.label -side top -fill x 
     pack .dirAndMode.modeBox.exten -side top -fill x -expand true
     pack .dirAndMode.modeBox.exten.label -side left -fill x -padx 30
     pack .dirAndMode.modeBox.exten.fits -side left -fill x -ipadx 10 -padx 10
     pack .dirAndMode.modeBox.exten.unf -side left -fill x -ipadx 10 -padx 10
     pack .dirAndMode.modeBox.mbScrollY -side right -fill y 
     pack .dirAndMode.modeBox.modeBox -fill both -expand true
     pack .dirAndMode.modeBox.chosen -side left -fill x 
     pack .dirAndMode.modeBox.chosenName -side left -fill x -expand true
     pack .dirAndMode.modeBox.bitRate -side left -ipadx 5

     pack .dirAndMode.session -side top -fill x -expand true
     pack .dirAndMode.session.label -side left -fill x 
     pack .dirAndMode.session.name -side left -fill x -expand true

     pack .dirAndMode.buttons -side top -fill x -expand true
     pack .dirAndMode.buttons.okay -side left -fill both -expand true
     pack .dirAndMode.buttons.cancel -side left -fill both -expand true

    update idletask
#    grab .dirAndMode
    tkwait window .dirAndMode
    puts_flush \
       "DirAndMode> HIGH $modeBox(high)"  
    puts_flush \
       "DirAndMode> MEDIUM $modeBox(medium)" 
    puts_flush \
       "DirAndMode> LOW $modeBox(low)"
    puts_flush "DirAndMode> DataDir $fsBox(path)"
    puts_flush "DirAndMode> ModeStr $modeBox(select)"
    puts_flush "DirAndMode> Session $modeBox(session)"
    puts_flush "DirAndMode> Exten $exten"
    puts_flush "DirAndMode> Status $modeBox(status)"
    return [list $fsBox(path) $modeBox(select) $modeBox(session) \
		$modeBox(high)  $modeBox(medium) $modeBox(low) \
                $modeBox(status)]
}
###################################################
# proc upDir
# Argument 
#      path : current path
# returns path to directory one level up from path
# Does not change the current directory
#
##################################################
proc upDir {path} {

    set curdir [pwd]
    cd $path/..
    set uppath [pwd]
    cd $curdir
    return $uppath
}

proc dirAndModeOkay {} {
    global fsBox
    global modeBox
    global instInfo
    if {[lsearch -exact $instInfo(keys) $modeBox(select)] == -1} {
	putMessage "Select a instrument-datamode or click cancel to quit"
    } else {
	set modeBox(status) "ok"
	destroy .dirAndMode
    }
}


##################################################
# proc fsBoxFillDirlist
# Arguments None
# Returns Nothing
# fills the listbox .dirAndMode.fsBox.fsBox with the 
# directories in fsBox(path)
###################################################

proc fsBoxFillDirlist {} {
    global fsBox

    .dirAndMode.fsBox.fsBox delete 0 end
    set dirList [lsort [glob -nocomplain $fsBox(path)/*/]]
    if {[llength $dirList] == 0 } {
	.dirAndMode.fsBox.fsBox insert end "-- No Subdirectories --"
    } else {
	foreach fileName $dirList {
	    regsub "$fsBox(path)/(.*)/" $fileName {\1} tmpName
	    .dirAndMode.fsBox.fsBox insert end $tmpName
	}
    }
}

proc modeBoxFillSelections {dirName} {
    global modeBox
    global instInfo
    global exten

    .dirAndMode.modeBox.modeBox delete 0 end
    set dataList [glob -nocomplain $dirName/*.$exten]
    set numModes [llength $instInfo(keys)]
    for {set i 0} {$i<$numModes} {incr i} {
	set key [lindex $instInfo(keys) $i]
	set pattern $instInfo($key)$exten\$
	set firstMatch [lsearch -regexp $dataList $pattern]
	if { $firstMatch != -1 } {
	    .dirAndMode.modeBox.modeBox insert end $key 
	} 
    }
    if { [.dirAndMode.modeBox.modeBox size] == 0 } {
	.dirAndMode.modeBox.modeBox insert end "-- NONE --"
    }
}

EODIRMODE

    if(defined $datadir) {
	print WISH "set fsBox(path) \"$datadir\"\n";
    }
    print WISH "putDirAndMode\n";

}

# This is necessary for the require to succeed

1;
