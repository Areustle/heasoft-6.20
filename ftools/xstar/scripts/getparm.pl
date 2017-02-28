#!/usr1/local/bin/perl5
#
#  File:   getparm.pl
#  Author: W.T. Bridgman
#  Date:   June 17, 1999
# 
#  Basically a Perl interface for XPI.  Allows you to generate XPI-type
#  user prompts in Perl scripts.  Uses pset to update the appropriate 
#  parameter file.
#
#  Usage:
#   The GetParameter function (described in detail below) is the primary
#   interface for this library.
#
#  Version History:
#    Version 1.0, WTB:  June 17, 1999
#
#=====================================================
#
# GetParameter subroutine
#    Extract a desired parameter value from an IRAF .par file
#
# Calling Method:
#   $x=&GetParameter($ftoolname,$varname,$status);
#
# Input Parameters:
#   $ftoolname = FTOOL name of parameter file for input
#   $varname   = parameter name to select
#
# Returned values:
#   $x = value of parameter (if status code==0)
#   $status = returned condition
#            0  - returned value is parameter value
#            >0 - error.  Returned value is *not* parameter value
#
sub GetParameter{
    local($ftoolname,$varname,$status)=@_;
    $status=0;

    $fpath=&FindParfile($ftoolname.".par"); # find right parameter file

    open(filehandle,"<$fpath")
	||die "GetParameter: Unable to open $fpath\n";
    # read through the parameter file
    $found=0;
    $done=0;
    while(!$done) {
	if(eof(filehandle)){$done=1;} # done reading file
	$parmline=<filehandle>; # read a line from the file
	if(index($parmline,"#")!=0){
	     # if it's not a comment line, then parse it 
	     @settings=split(/,/,$parmline,7); 
	     # extracting values
	     #   settings[0]= variable name
             #   settings[1]= variable type (r=real,i=integer,s=string)
	     #   settings[2]= variable mode (h=hidden,a=auto)
	     #   settings[3]= current value
	     #   settings[4]= minimum value
	     #   settings[5]= maximum value
	     #       (trim extra quotes from strings)
	     if($settings[1] eq "s"){
		 $settings[3]=&DeFrock($settings[3]);
		 $settings[4]=&DeFrock($settings[4]);
		 $settings[5]=&DeFrock($settings[5]);
	     }
	     #   settings[6]= prompt string (trim quotes)
	     $settings[6]=&DeFrock($settings[6]);
	     if($settings[0] eq $varname) {
		 $done=1;
		 $found=1;
		 #printf("GetParameter: @settings\n"); # comment out when done debugging
	     }
	}
    } # end while
    close(filehandle);

    # error checking
    # done but not found
    if(!$found){
	printf("Paramter not found.  Error exit\n");
	$_[2]=1; # set status code
	return(0);
    }

    # parameter found

    # do we prompt for it?
    # if not, just pass it on through...
    if($settings[2] eq 'a') {
	# if yes, then prompt for the value
	$settings[3]=&PromptForValue(@settings);
	# build command string to update parameter file
	$psetcommand="pset ".$ftoolname." ".$varname."=";
	if($settings[1] eq 's') {
	    # make sure quotes enclosing
	    $psetcommand=$psetcommand."'".$settings[3]."'";
	} else {
	    $psetcommand=$psetcommand.$settings[3];
	}
	# print $psetcommand,"\n";
	system($psetcommand); # update the parameter file
    }
    
    $_[2]=$status;
    return($settings[3]);
}
#######################################################
#   $x = &RangeTest($type,$value,$min,$max)
#
#   Input parameters:
#      $type  = value type (r=real,i=integer,s=string)
#      $value = value to test
#      $min   = minimum allowed value
#      $max   = maximum allowed value
#
#   Returns:
#      0  = if value in range
#      >0 = if value out-of-range
#
sub RangeTest {
    local($type,$value,$min,$max)=@_;
    $result=0; # value okay

    if(($min ne "") && ($max ne "")){
	# range specified so check it
	# print("RangeTest: ($min,$max)\n");
	# range checking on strings
	if($type eq "s"){
	    if(($min le $value)&&($max ge $value)){$result=0;}
	    else {$result=1;}
	}
	# range checking on reals
	if($type eq "r"){
	    if(($min<=$value)&&($max>=$value)){$result=0;}
	    else {$result=1;}
	}
	# range checking on integers
	if($type eq "i"){
	    if((int($min)<=int($value))&&(int($max)>=int($value))){$result=0;}
	    else {$result=1;}
	}        
    }
    return($result);
}
#######################################################
#   $x = &PromptForValue(@settings)
#
#   Returns:
#      Value retrieved.
#
sub PromptForValue {
    local($parname,$type,$mode,$curvalue,$min,$max,$promptstr)=@_;
    local($value,$badvalue);

    $value=0;
    $badvalue=1; 

    while($badvalue) {
	if(($min ne "") & ($max ne "")){
	    # range specified so check it
	    print "$promptstr ($min:$max) [$curvalue] ";
	} else {
	    # no range specified
	    print "$promptstr [$curvalue] ";
	}
	$value=<STDIN>;

	# cleanup default vs. entry
	if($value eq "\n") {
	    # if no entry, set to default
	    $value=$curvalue;
	} else {
	    $value=substr($value,0,length($value)-1);
	}

	$badvalue=&RangeTest($type,$value,$min,$max);

    } # end of 'while'


    # force truncation of integer values
    if($type eq "i") {$value=int($value);}

    return($value);
}
#######################################################
#   $string = &DeFrock($string)
#
#   Perl likes to keep the extra quote-marks in the loaded strings.
#   This routine will strip them out.
#
#   Returns:
#      quote-stripped string.
#
sub DeFrock{
    local($string)=@_;
    # strip leading quote if present
    if(substr($string,0,1) eq '"') {
	$string=substr($string,1);
    }
    # strip off trailing quote if present
    if(rindex($string,'"')>0) {
	$string=substr($string,0,rindex($string,'"'));
    }
    return($string);
}
#######################################################
#   $fpath = &FindParfile($filename)
#
#   Search the PFILES paths for the most recently modified 
#   parameter file $filename.
#
#   Input:
#      $filename - parameter file name to find
#
#   Returns:
#      $fpath - full path to most recently modified parameter file
#               This is blank if $filename not found.
#
sub FindParfile{
    local($filename)=@_;

    # extract the pfiles paths - split here into local vs. system
    @pfiles=split(';',$ENV{'PFILES'});

    # search for most recent modification date
    $maxtime=0; # initialize maximum tag
    $fpath=""; # initialize return value
    foreach $path (@pfiles) {
	# split up local and system paths into colon-delimted subpaths
	@sub_pfiles=split(':',$path);
        while ($sub_path = shift @sub_pfiles) {
		last if @statistics=stat($sub_path.'/'.$filename);
	}
	if($statistics[9]>$maxtime) {
	    $fpath=$sub_path.'/'.$filename;
	    $maxtime=$statistics[9];
	}
    }
    return($fpath);
}
1;
