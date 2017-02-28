#!/usr/bin/perl
#
# 
#	libnustarperl.pl
#
#
#	DESCRIPTION:
#               Library functions for NuSTAR perl scripts
#
#	DOCUMENTATION:
#
#
#	CHANGE HISTORY:
#        0.1.0 - NS 12/05/11 - First version
#        0.1.1 - NS 03/10/11 - 'GetPfilesName', 'GetParameterList', 'LoadParameterFromCmdLine', 'LoadParameter',
#                              'GetValPar', 'SetValPar', 'RunningTask', 'Success', 'Error', 'GetFitsPointer'
#                              and 'delFitsCol' routines added
#        0.1.2 - NS 10/10/11 - GetKeyword, GetNumExtName, GetEventStartDate and CallQuzcif routines added
#        0.1.3 - NS 14/10/11 - Handle empty rows in input parameters file
#                            - getHistoryTime, FindExecFile, AddKeyword, LoadBinTable, GetRootStem, IsCompressed,
#                              WriteParameterList, WriteParHistory, CleanTimeNulls and RunFtstat routines added 
#                            - GetFileNameRoot, SetGtiFileName, SetLev2EvtFileName, SetFileNameWithExt, EraseLastExtension, 
#                              CheckRow, Cleanup, CheckXselectLog and TableEmpty routines added 
#        0.1.4 - NS 24/10/11 - Added new values in '%Def' hash table
#        0.1.5 - NS 21/11/11 - GetPntFromExt, IsNumeric, CheckRa, CheckDec, Ra2Deg, Dec2Deg and trimValue routines added
#        0.1.6 - NS 02/12/11 - Modified some values in '%Def' hash table
#        0.1.7 - NS 06/12/11 - Modified some values in '%Def' hash table
#                            - RaDec2XY and GetXspecChatter routines added
#        0.1.8 - NS 22/12/11 - Added 'GetObjFromExt' routine
#        0.1.9 - NS 25/01/12 - Added values in '%Def' hash table
#        0.2.0 - NS 27/02/12 - Added values in '%Def' hash table
#        0.2.1 - NS 08/05/12 - Added values in '%Def' hash table
#        0.2.2 - NS 31/08/12 - 'CreateAbsSymbolicLink' routine added
#        0.2.3 - NS 18/10/12 - Added values in '%Def' hash table
#                            - Added 'GetStartDateFromExt' routine
#        0.2.4 - NS 05/02/13 - Added 'FindCaldbIndxFile' and 'GetCaldbVersion' routines
#        0.2.5 - NS 14/03/13 - Added values in '%Def' hash table
#        0.2.6 - NS 10/06/13 - Added values in '%Def' hash table
#        
#
#	AUTHORS:
#
#       ASDC - ASI Science Data Center
#
#

use strict;
#use vars qw( %Task %Default %Def);
#use base qw(Task::HEAdas);
#use Task qw(:codes);

use Astro::FITS::CFITSIO qw(:longnames :constants);
use File::Copy;
use File::Basename;
#use Math::Trig;
use Cwd;
use Cwd 'abs_path';

use vars qw( %Task  @Par  %Ind  %Def);


%Def = (
	LEV1STEM        => "_uf",
	LEV2STEM        => "_cl",
	GTISTEM         => "_gti",
	MKCONFSTEM      => "_mkf",
	MKFSTEM         => "",
	HKSTEM          => "_fpm",
	BADPIXSTEM      => "_bp",
	HOTPIXSTEM      => "_hp",
	PSDSTEM         => "_psd",
	PSDCORRSTEM     => "_psdcorr",
	MASTSTEM        => "_mast",
	IMGSTEM         => "_sk",
	PHASTEM         => "_sr",
	BKGPHASTEM      => "_bk",
	LCSTEM          => "_sr",
	BKGLCSTEM       => "_bk",
	OPTAXISSTEM     => "_oa",
	DET1REFSTEM     => "_det1",
	EXPOSTEM        => "_ex",
	ARFSTEM         => "_sr",
	RMFSTEM         => "_sr",
	DET1OFFSET      => "_det1offset",
	ASPHIST         => "_asphist",
	APSTOPHISTTEM   => "_apstophisto",
	GRHISTSTEM      => "_grhisto",
	DET1MAP         => "_det1map",
	DET2MAP         => "_det2map",
	SKYMAP          => "_skymap",
	ATTSTEM         => "_att",
	SRCLCCORRSTEM   => "_srcorrfact",
	BKGLCCORRSTEM   => "_bkcorrfact",
	FITSEXT         => ".fits",
	EVTEXT          => ".evt",
	MKCONFEXT       => ".conf",
	MKFEXT          => ".mkf",
	HKEXT           => ".hk",
	IMGEXT          => ".img",
	PHAEXT          => ".pha",
	LCEXT           => ".lc",
	ARFEXT          => ".arf",
	RMFEXT          => ".rmf",
	ATTORBEXT       => ".attorb",
	);


my $PI =  3.14159265358979323846;


sub PrntChty {
    my ( $chatty, $message ) = @_;
    use vars qw ( %Task );
    if ( $chatty <= $Task{chatter} ) {
	print"$message";
    }
    return;
} # PrntChty


sub CompUL {
    my ( $string1, $string2 ) = @_;
    # The subroutine performs a insensitive case
    # compare of two strings. It returns
    # 1 if the strings are equal or 0 if not
    if ( lc($string1) eq lc($string2) ) { return 1; }
    return 0;
} # CompUL


sub GetInputParam {

# This function parses the input parameter specification.
# The $inspec must be of format: <parametername>=<parametervalue>

    my ( $inspec ) = @_;
    use vars qw ( %Task );

    if ( $Task{status} ) { return; }

    my ( $ind ) = index ( $inspec, '=' ); 
    my ( $inpar ) = substr($inspec,0,$ind-1);
    my ( $inval ) = substr($inspec,$ind+1);

    if ( $ind <= 0 || !$inpar || ( !$inval && $inval ne "0" )) {
	print"$Task{'stem'}: Error: Parsing input parameter: $inspec\n";
	print"$Task{'stem'}: Error: Please specify parameters with format: <parametername>=<parametervalue> without blanks\n";
	print"$Task{'stem'}: Error: Type 'fhelp $Task{name}' for more information on parameters\n";
	$Task{status} = 1;
	return;
    }	
    return ($inval);

} #GetInputParam

sub GetPfilesName {
    
    use vars qw ( %Task);

    my ($file1,$file2);
    my $filename = $Task{name} . ".par";

    if ($ENV{PFILES} eq "") {
	    &PrntChty(2,"$Task{stem}: Error: GetPfilesName: PFILES environment variable not found\n");
	    $Task{errmess} ="PFILES environment variable not found";
	    $Task{status} = 1;
	    return;
    }

    my @path = split(/[;:]/,$ENV{PFILES});
    
    $file1 = $path[0] . "/" . $filename;

    if (($#path>0) && ($ENV{PFCLOBBER}==1)) {
	for (my $i = 1; $i <= $#path; $i++) {
	    $file2 = $path[$i] . "/" . $filename;
	    if ( -f $file2 ) {
		if ((stat($file2))[10] > (stat($file1))[10]) {
		    if (!copy ($file2,$file1)) {
			&PrntChty(2,"$Task{stem}: Error: GetPfilesName: Cannot copy parameter file $file2 into $file1\n");
			$Task{errmess} ="Cannot copy parameter file $file2 into $file1";
			$Task{status} = 1;
			return;
		    }
		}
	    }
	}
    }


    if (! -f $file1) {
	&PrntChty(2,"$Task{stem}: Error: GetPfilesName: Parameter file $file1 not found \n");
	$Task{errmess} ="Parameter file '$file1' not found";
	$Task{status} = 1;
	return;
    }
    
    return $file1;

} # GetPfilesName


sub GetParameterList {
    use vars(%Task);

    my $pfile = &GetPfilesName();
    if ($Task{status}) { 
	&PrntChty(2,"$Task{stem}: Error: GetParameterList: Cannot get parameter file name\n");
	return;
    }

    if (!open(FILEPAR,$pfile)) {
	$Task{errmess} = "Cannot open input parameter file ($pfile)";
	$Task{status} = 1;
	return;
    }

    my @arr_par;
    my $i = 0;
    my %keyind;
    while (<FILEPAR>) {
	chomp();

	if (! ($_ =~ /^#/)) {
	
	       my ($name,$type,$mode,$defvalue,$minval,$maxval,$description) = split(",");
	       
	       my %hash;

	       # Skip empty rows
	       if($name eq ""){
		   next;
	       }
	       
	       $defvalue =~ s/^\s+//; # remove leading spaces
	       $defvalue =~ s/\s+$//; # remove trailing spaces	
	       $defvalue =~ s/^"//g;  #Remove apex
               $defvalue =~ s/"$//g;  #Remove apex
	       
	       $hash{name} = $name;
	       $hash{defval} = $defvalue; 
	       $hash{val} = $defvalue;
	       $hash{set} = 0;
	       $hash{type} = $type;  
	       $hash{maxval} = $maxval; 
	       $hash{minval} = $minval; 
	       
	       push (@arr_par,\%hash);
	       
	       $keyind{$name} = $i;
	       
	       $i++;
	   }
	}

	close FILEPAR;
	
    return (\%keyind , @arr_par);

} # GetParameterList

sub LoadParameterFromCmdLine {

    my (@ARGV) = @_;

    use vars qw(@Par %Ind);

    my ( $name, $value, $p);


    if ($#Par < 0) {
	&PrntChty(2,"$Task{stem}: Error: LoadParameterFromCmdLine: List of input parameter is empty \n");
	$Task{errmess} ="LoadParameterFromCmdLine the List of input parameters is empty";
	$Task{status} = 0;
	return 0;

    }

    
    foreach $p (@ARGV) {
	
	($name,$value) = split("=",$p);
	
	if (! exists($Ind{$name})) {
	    &PrntChty(2,"$Task{stem}: Error: parsing input parameter '$name'\n");
	    &PrntChty(2,"$Task{stem}: Error: Parameter name '$name' not found or sintax error\n");
	    &PrntChty(2,"$Task{stem}: Error: Please specify parameters with format: <parametername>=<parametervalue>\n");
	    &PrntChty(2,"$Task{stem}: Error: without blanks\n");
	    &PrntChty(2,"$Task{stem}: Error: Type 'fhelp $Task{name}' for more information on parameters\n");
	    return 0;
	} else {
	    
	    my $v = GetInputParam ($p);
	    
	    if (($Par[$Ind{$name}]->{type} eq "r") || ($Par[$Ind{$name}]->{type} eq "i")) {
		if (($Par[$Ind{$name}]->{minval} ne "") && ($Par[$Ind{$name}]->{maxval} ne "")) {
		    
		    if (($v < $Par[$Ind{$name}]->{minval}) || ($v > $Par[$Ind{$name}]->{maxval})) {
			&PrntChty(2,"$Task{stem}: Error: parsing input parameter '$name'\n");
			&PrntChty(2,"$Task{stem}: Error: Value is out of range minvalue=$Par[$Ind{$name}]->{minval} maxvalue=$Par[$Ind{$name}]->{maxval}\n");
			$Task{errmess} = "LoadParameterFromCmdLine: Parameter $name value is out of range";
			$Task{status} = 0;
			return 0;
		    }
		}
	    }

	    $Par[$Ind{$name}]->{val} = $v;
	    if ($Task{status}) { return 0;}
	    
	    $Par[$Ind{$name}]->{set} = 1;
	}
	
    }

    return 1;

} # LoadParameterFromCmdLine

sub LoadParameter {


    use vars qw(@Par %Ind %Task);
    
    my ($val, $p);

    foreach $p (@Par ) {
	if ( &RequestParameter($p->{name})) {
	    $val = "";
	    if ($p->{set} == 0) {
		chop($val = `pquery2 $Task{name} $p->{name}`);
		$p->{set} = 2;
		#if ( !$val  && $val != 0) {
		if ($val eq "") {
		    &PrntChty(2,"$Task{stem}: Error: LoadParameter: Running 'pquery2 $Task{name} $p->{name}'\n");
		    $Task{errmess} ="Running 'pquery2 $Task{name} $p->{name}'";
		    $Task{status} = 1;
		    return 0;
		} 
		else { $p->{val} = $val;}
	    }
	}
    }
    
    #Reset string of message populate from RequestParameter in case of error
    $Task{errmess} = "";
    return 1;

} # LoadParameter

sub GetValPar {

    my ( $parname , $pos) = @_;

    use vars qw(@Par %Ind %Task);

    my $p;

    if ( !defined($pos) ) { $p = "val";}
    else {$p = $pos;}

    if ( $p eq "val" && !defined($Ind{$parname}) ) {
	$Task{status} = 1;
	$Task{errmess} = "Parameter '$parname' not found in the input parameter file";
	&PrntChty(2,"$Task{stem}: Error: GetValPar: $Task{errmess}\n");
	return;
    }

    return $Par[$Ind{$parname}]->{$p};

} # GetValPar

sub SetValPar {

    my ( $parname , $val,$col) = @_;

    use vars qw(@Par %Ind %Task);

    my $pos;

    if ( !defined($col) ) { $pos = "val";}
    else {$pos = $col;}
    
    my $name = exists($Ind{$parname});

    if ($name) {
    	$Par[$Ind{$parname}]->{$pos} = $val;
    } else {
	$Task{status} = 1;
	$Task{errmess} = "Parameter $parname not found";
    }
} # SetValPar


sub getHistoryTime {

    my @f = localtime();
    my $s = sprintf("%04d-%02d-%02dT%02d:%02d:%02d",$f[5]+1900,$f[4]+1,@f[3,2,1,0]);

    return $s;
    
} # getHistoryTime


sub FindExecFile {
    
    my ( $file ) = @_;
    my ( @searchp )=split(/:/, $ENV{'PATH'});
    my ( $sdir );
    
    foreach $sdir (@searchp){
	if ( -x "$sdir/$file" ) { return 1; }
    }
    
    # Executable Not Found
    return 0;
    
} # FindExecFile


sub CreateDir {

    my ( $directory ) = @_;
    use vars qw (%Task);

    if ( !-d  $directory ) {
	&PrntChty(4,"$Task{stem}: Info: CreateDir: Creating '$directory' directory\n");
	if ( system("mkdir -p $directory")) {
	    $Task{status} = 1;
	    $Task{errmess} = "Creating Directory '$directory: $!'\n";
	    return 1;
	}
    }
    return 0;

} # CreateDir


sub RenameFile {

    # Copy the $file1 into $file2 and delate
    # the $file1. The function returns an error if the
    # $file2 cannot be created but gives only a warning message if 
    # the $file1 cannot be removed


    my ( $file1, $file2 ) = @_;

    if (!copy($file1, $file2)) {
	$Task{status} = 1;
	$Task{errmess} = "Unable to copy file: '$file1' in '$file2'. $!";
	return 1;
    }

    else {
	if ( !unlink($file1) ) {
	    &PrntChty(2,"$Task{stem}: Warning: RenameFile: Unable to remove '$file1' file. $!\n");
	}
    }

    return 0;

} #RenameFile

sub RunningTask {
    use vars qw (%Task);

    &PrntChty(2,"--------------------------------------------------------------\n");
    &PrntChty(2,"             Running ' $Task{name} version $Task{version} '\n");
    &PrntChty(2,"--------------------------------------------------------------\n");

} # RunningTask

sub RunningSub {
    use vars qw (%Task);
    my ($task,$subrout,$message) = @_;
    if ( !defined($message) ) { $message = ""; }
    &PrntChty(3,"$Task{stem}: Info: Running '$subrout' $message\n");
} # RunningSub

sub SuccessSub {
    use vars qw (%Task);
    my ($task,$subrout, $message) = @_;
    if ( !defined($message) ) { $message = ""; }
    &PrntChty(3,"$Task{stem}: Info: '$subrout' exit with success. $message\n");
} # RunningSub

sub Success {
    my ( $message ) = $_;
    use vars qw ( %Task );
    &PrntChty(2,"----------------------------------------------------------\n");
    if ( !defined($message) ) { $message = ""; }
    &PrntChty(2,"$Task{stem}: Exit with success $message\n"); 
    &PrntChty(2,"------------------------------------------------------------\n");
} # $Success

sub Error {
    my ( $message ) = $_;
    use vars qw (%Task);
    &PrntChty(2,"-------------------- $Task{name}  error -----------------------\n");
    if ( defined($message) ) {
	&PrntChty(2,"$Task{'stem'}: $message\n");
    }
    &PrntChty(2,"$Task{'stem'}: $Task{errmess}\n");
    &PrntChty(2,"--------------------------------------------------------------\n");
} # Error

sub RunningComm {
    use vars qw (%Task);
    my($task,$command) = @_;
    &PrntChty(3,"$Task{stem}: Command: $command\n");
} # RunningComm

sub ErrorComm {
    use vars qw (%Task);
    my($task,$subrout,$command) = @_;
    &PrntChty(3,"$Task{stem}: Error: running '$subrout'\n");
    &PrntChty(3,"$Task{stem}: Error: The run was: $command \n");
} # ErrorComm


sub GetFitsPointer {
    
    # This routines returns the pointer to the 
    # fits fileextension or $pointer if
    # defined

    my ($filename,$extname,$pointer,$mode) = @_;

    use vars qw (%Task);

    if (! defined($mode)) { $mode = READONLY;}

    my ($fptr,$status) = (0, 0);
    
    if (defined($pointer)) { $fptr = $pointer;}
    else {
	
	fits_open_file($fptr,$filename,$mode,$status);
	if ($status) {
	    $Task{status} = 1;
	    &PrntChty(2,"$Task{stem}: Error: GetFitsPointer: Cannot open fits file: $filename\n");
	    &PrntChty(2,"$Task{stem}: Error: GetFitsPointer: CFITSIO error: $status\n");
	    $Task{errmess} = "Unable to open fits file: $filename";
	    return;
	}
    }

    if (defined($extname)) {
	fits_movnam_hdu($fptr,ANY_HDU,$extname,0,$status);
	if ($status) {
	    $Task{status} = 1;
	    &PrntChty(2,"$Task{stem}: Error: GetFitsPointer: Cannot move to extension '$extname' of file '$filename'\n");
	    &PrntChty(2,"$Task{stem}: Error: GetFitsPointer: CFITSIO error: $status\n");
	    $Task{errmess} = "Cannot move into extension '$extname' of file '$filename'";
	    return;
	}
    }
    
    return $fptr;
      
}#GetFitsPointer


sub delFitsCol() {
    my ($filename,$extname,@listCol) = @_;
    
    my ($colname,$index);
    my $status = 0;
    
    foreach $colname (@listCol) {
	my $fptr = &GetFitsPointer($filename,$extname,undef,READWRITE);
	if ($Task{status}) { 
	    return 1;
	}
	
        fits_get_colnum($fptr,0, $colname , $index, $status);
	if ($status) {
	    &PrntChty(5,"$Task{stem}: Warning: Column '$colname' not found in $filename file\n");
	} else {
	    &PrntChty(5,"$Task{stem}: Info: delFitsColumn: Delete column : '$colname' in $filename file\n");
	    fits_delete_col($fptr,$index,$status);
	    if ($status) {
		&PrntChty(2,"$Task{stem}: Error: delFitsCol: Unable to delete colname: '$colname'\n");
		$Task{errmess} = "Unable to delete column name $colname from $filename file";
		$Task{status} = 1;
		return 1;
	    }

	    fits_write_chksum($fptr,$status);
	    if ($status) {
		$Task{errmess} = "Error running write_chksum  $filename file , $extname extension";
		&PrntChty(2,"$Task{stem}: Error: delFitsCol - Running write_chksum on file $filename\n");
		$Task{status} = 1;
		return 0;
	    }
	}
	$status =0;
	
	fits_close_file($fptr,$status);
	if ($status) { 
	    $Task{errmess} = "delFitsCol: Unable to close file : $filename";
	    return 1;
	}
    }
    return 0;

} #delFitsCol

sub AddKeyword {

    my ( $fileandext, $keyname, $keyvalue, $comment ) = @_ ;

    use vars qw ( %Task %Default);

    my ( $PARKEY_COMMAND ) = 'fparkey';

    if ( !&FindExecFile($PARKEY_COMMAND) ) {
	&PrntChty(2,"$Task{stem}: Error: AddKeyword: Executable '$PARKEY_COMMAND' not found in User Path\n");
	$Task{errmess} = "Error: Executable '$PARKEY_COMMAND' not found in User Path";
	$Task{status} = 1;
	return 1;
    }

    my $cmd = "$PARKEY_COMMAND $keyvalue $fileandext $keyname add=yes ";

    if (defined($comment)) {
	$cmd .= "comm='$comment'";
    }

    if ( my($FkeyRet) = qx($cmd) ) {
	&PrntChty(2,"$Task{stem}: Error: AddKeyword: Running $cmd \n");
	$Task{errmess} = "Error: Running '$PARKEY_COMMAND $fileandext $keyname'";
	$Task{status} = 1;
	return 1;
    }
    
    return 0;
    
} # AddKeyword


sub GetKeyword {

    # This subroutine returns the value of the "keyname"
    # taken from the heder extension "extname" of the
    # input "filename" or from the extension where the
    # fits file pointer "pointer" is set
    # FT 23/07/2004 - added $checkexist parameter to
    #                 do not print a message if the
    #                 keyword does not exist. If the
    #                 key does not exist the subroutine
    #                 returns the KEY_NO_EXIST code

    my ( $filename, $extname, $pointer, $keyname, $keyvalue, $checkexist ) = @_ ;

    use vars (%Task);
    
    my $fptr = &GetFitsPointer($filename,$extname,$pointer);
    if ($Task{status}) { 
	$Task{errmess} = "Unable to open '$filename' file";
        $Task{status} = 1;
	return 1;
    }
    
    my ($comm,$value,$str);
    my $status = 0;
    fits_read_keyword($fptr,$keyname,$value,$comm,$status);
    if ($status) {
	$Task{status} = $status;
	if ( !($checkexist && $status==KEY_NO_EXIST) ) {
	    if (defined($filename)) { $str = "Error reading keyword $keyname in file $filename";}
	    else {$str= "Error reading keyword $keyname";}
	    &PrntChty(2,"$Task{stem}: Error: GetKeyword: FITSIO error: $status\n");
	    $Task{errmess} = $str;
	    fits_get_errstatus($status,$str);
	    &PrntChty(2,"$Task{stem}: Error: GetKeyword: FITSIO error: $str\n");
	}
	fits_close_file($fptr,$status);
	return 1;
    }

    $value =~ s/^\s+//; # remove leading spaces
    $value =~ s/\s+$//; # remove trailing spaces	
    $value =~ s/^'//g;; #Remove apex
    $value =~ s/'$//g;; #Remove apex
    $value =~ s/\s+$//g;; #Remove right space
    
    $$keyvalue= $value;

    
    if (defined($filename)) {
	fits_close_file($fptr,$status);
	if ($status) {
	    $Task{status} = $status;
	    fits_get_errstatus($status,$str);;
	    $Task{errmess} = $str;
	    return 1;
	}
    }
    
    return 0;

} #GetKeyword


#Return number of extension with extname=parameter
# if not found return -1
sub GetNumExtName {

    my ($filename,$extname) = @_;

    my ($numext, $fptr, $status) = (0, 0, 0);

    fits_open_file($fptr,$filename,READONLY,$status);
    if ($status) {
	$Task{status} = 1;
	&PrntChty(2,"$Task{stem}: Error: GetNumExtName: Fitsio 'fits_open_file' error '$status' on '$filename' file\n");
	$Task{errmess} = "Unable to open fits file : $filename";
	return 5;
    }
    fits_movnam_hdu($fptr,ANY_HDU,$extname,0,$status);
    if ($status == BAD_HDU_NUM ) {
	&PrntChty(4,"$Task{stem}: Warning: GetNumExtName: not found '$extname' extension\n");
	&PrntChty(4,"$Task{stem}: Warning: GetNumExtName: in '$filename' file\n");
	fits_close_file($fptr,$status);
	return -1;
    } elsif ($status) {
	$Task{status} = 1;
	$Task{errmess} = "Unable to move to the extension '$extname' of '$filename'";
	fits_close_file($fptr,$status);
	return;
    }
    
    fits_get_hdu_num($fptr,$numext);
    fits_close_file($fptr,$status);
    
    return $numext-1;
 
} # GetNumExtName


sub LoadBinTable
{
    my ($filefits, $extname,$numext,@columns) = @_;
    use vars qw ( %Task );

    my $status = 0;
    my $fits = Astro::FITS::CFITSIO::open_file($filefits, READONLY, $status);
    if (not $fits) {
	 &PrntChty(2,"$Task{stem}: Error: LoadBinTable: CFITSIO::open_file: error on '$filefits' file\n");
	 $Task{status} = 1;
	 return;
    }
    
    if (defined($extname) && ($extname ne "")) {
	 $fits->movnam_hdu(BINARY_TBL, $extname, 0, $status);
	 if ( $status ) {
	     &PrntChty(2,"$Task{stem}: Error: LoadBinTable: movnam_hdu: error on '$filefits' file\n");
	     &PrntChty(2,"$Task{stem}: Error: LoadBinTable: unable to move to extname: $extname\n");
	     $Task{status} = 1;
	     return;
	 }
    } else {
	 
	 $fits->movabs_hdu($numext+1,undef,$status);
	 if ( $status ) {
	     &PrntChty(2,"$Task{stem}: Error: LoadBinTable: movabs_hdu: error on '$filefits' file\n");
	     &PrntChty(2,"$Task{stem}: Error: LoadBinTable: unable to move to ext num: $numext\n");
	     $Task{status} = 1;
	     return;
	 }
    }

    my $count = -1;
    $fits->get_num_rows($count, $status);
    if ($status) {
	&PrntChty(2,"$Task{stem}: Error: LoadBinTable: get_num_rows: error on '$filefits' file\n");
	&PrntChty(2,"$Task{stem}: Error: LoadBinTable: unable to get the number of rows\n");
	$Task{status} = 1;
	return ;
	 
    }

    foreach my $col (@columns) {

	 $col->{data} = [ ];

	 my $index = -1;
	 my $null  = -1;
	 my $nulls = -1;
	 my ( $type,$repeat,$width, $tzeroval,$comm );

	 if ($fits->get_colnum(0, $col->{name}, $index, $status)) {
	     &PrntChty(2,"$Task{stem}: Error: LoadBinTable: get_colnum: error on '$filefits' file\n");
	     &PrntChty(2,"$Task{stem}: Error: LoadBinTable: unable to get colnum: '$col->{name}'\n");
	     $Task{status} = 1;
	     return ;
	 }

	 if ($fits->get_coltype($index,$type,$repeat,$width,$status)) {
	     &PrntChty(2,"$Task{stem}: Error: LoadBinTable: Unable to get column type ($col) file : $filefits\n");
	     $Task{status} = 1;
	     return ;
	 }
  
	 $fits->read_key(TULONG,"TZERO$index",$tzeroval,$comm,$status);
	 if ($status == KEY_NO_EXIST ) {
	     $tzeroval = -1;
	     $status = 0;
	 }

	 my $tscal = 1;

	 $fits->read_key(TINT,"TSCAL$index",$tscal,$comm,$status);
	 if ($status == KEY_NO_EXIST ) {
	     $tscal = 1;
	     $status = 0;
	 }

	 if ($type == TINT) {
	     if (($tscal == 1) &&($tzeroval == 32768)) {
		 $type = TUINT;
	     }
	 }
	 if ($type == TLONG) {
	     if (($tscal == 1) &&($tzeroval == 2147483648 )) {
		 $type = TULONG;
	     }
	 }

#	 print "Column name: $col->{name} index: $index type: $type (numero righe: $count)\n";
#	 print TDOUBLE . " " . TSHORT . "\n"; 
	 $fits->read_col($type,$index, 1, 1, $count, $null,$col->{data}, $nulls, $status);
	 if ($status) {
	      &PrntChty(2,"Task{stem}: Error: LoadBinTable: unable to read column: '$col->{name}'\n");
	      &PrntChty(2,"Task{stem}: Error: LoadBinTable: from file: $filefits\n");
	      $Task{status} = 1;
	      return ;
	 }

    }

    $fits->close_file($status);

    # convert from parallel arrays to array of keyed structs
    my @table = ();
    for (my $i = 0; $i < $count; ++$i) {
	 my %record;
	 foreach my $col (@columns) {
	     $record{$col->{name}} = $col->{data}[$i];
	 }


	 push(@table, \%record);
    }
    
    return @table;

} # LoadBinTable


sub GetEventStartDate { 

    # This subroutine returns the observation Date and Time
    # with the format needed for CALDB query
    # The Observation Start date is taken from EVENTS extension 
    # 

    my ( $evtfile, $date, $time ) = @_;
    my ( $extension );

    my $numext = &GetNumExtName( $evtfile, "EVENTS" );
    if($numext > -1){
	$extension = "EVENTS";
    }
    else{
	$extension = undef; 
    } 
    
    &GetKeyword($evtfile,$extension,undef,"DATE-OBS",\$$date);
    if ( $Task{status} ) {return 1;}

    my $Tstr = substr($$date,10,1);

    if ( $Tstr eq "T" ) {
	$$time = substr($$date,11);
	$$date = substr($$date,0,10);
    }
    else {
	&GetKeyword($evtfile,$extension,undef,"TIME-OBS",\$$time);
	if ( $Task{status} ) {return 1;}
    }

    &PrntChty(4,"$Task{stem}: Info: GetEventStartDate: Obsevation start date: $$date $$time,\n"); 
    &PrntChty(4,"$Task{stem}: Info: GetEventStartDate: from event file: $evtfile\n"); 

    return 0;

} # GetEventStartDate

sub GetStartDateFromExt { 

    # This subroutine returns the observation Date and Time
    # with the format needed for CALDB query
    # The Observation Start date is taken from <extname> extension 
    # 

    my ( $filename, $extname, $date, $time ) = @_;
    my ( $extension );

    my $numext = &GetNumExtName( $filename, $extname );
    if($numext > -1){
	$extension = $extname;
    }
    else{
	$extension = undef; 
    } 
    
    &GetKeyword($filename,$extension,undef,"DATE-OBS",\$$date);
    if ( $Task{status} ) {return 1;}

    my $Tstr = substr($$date,10,1);

    if ( $Tstr eq "T" ) {
	$$time = substr($$date,11);
	$$date = substr($$date,0,10);
    }
    else {
	&GetKeyword($filename,$extension,undef,"TIME-OBS",\$$time);
	if ( $Task{status} ) {return 1;}
    }

    &PrntChty(4,"$Task{stem}: Info: GetStartDateFromExt: Obsevation start date: $$date $$time,\n"); 
    &PrntChty(4,"$Task{stem}: Info: GetStartDateFromExt: from file: $filename\n"); 

    return 0;

} # GetStartDateFromExt

sub GetPntFromExt {

    my ($filename,$extname,$extnum,$ra_pnt,$dec_pnt) = @_;

    &PrntChty(4,"$Task{stem}: Info: GetPntFromExt: Reading RA_PNT and DEC_PNT Keywords\n"); 
    &PrntChty(4,"$Task{stem}: Info: GetPntFromExt: from '$filename' file\n");

    &GetKeyword($filename,$extname,$extnum,"RA_PNT",$ra_pnt);
    if ( $Task{status} ) { return 1; }

    &GetKeyword($filename,$extname,$extnum,"DEC_PNT",$dec_pnt);
    if ( $Task{status} ) { return 1; }

    return 0;

} # GetPntFromExt

sub GetObjFromExt {

    my ($filename,$extname,$extnum,$ra_obj,$dec_obj) = @_;

    &PrntChty(4,"$Task{stem}: Info: GetObjFromExt: Reading RA_OBJ and DEC_OBJ Keywords\n");
    &PrntChty(4,"$Task{stem}: Info: GetObjFromExt: from '$filename' file\n");

    &GetKeyword($filename,$extname,$extnum,"RA_OBJ",$ra_obj);
    if ( $Task{status} ) { return 1; }

    &GetKeyword($filename,$extname,$extnum,"DEC_OBJ",$dec_obj);
    if ( $Task{status} ) { return 1; }

} # GetObjFromExt


sub CallQuzcif {

    my ( $Dataset, $Date, $Time, $Selection, $expnumfile, $instrume, $detnam ) = @_;  
    my ( $field,@dummy, @outlist , @extlist);
    use vars qw ( %Task );
    
    # The function performs a query to the CALDB database based
    # on $Dataset, $Date, $Time, $Selection parameters (see below
    # for the input parameter description.
    # The function return two array 'outlist' and 'extlist' with the result of the query.
    # The array is filled with filenames and extensions from the quzcif query
    # For example: if the 'quzcif' query gives only one match the array will
    #              content : $outlist[0] = filename
    #                        $extlist[0] = extension
    #              if the 'quzcif' query gives two matches the array will
    #              content : $outlist[0] = filename1
    #                        $extlist[0] = extension1
    #                        $outlist[1] = filename2
    #                        $extlist[1] = extension2
    #              etc ....
    # $Dataset - 'Calibration Dataset Codename'
    # $Date    - 'Requested Date in yyyy-mm-dd format' or "NOW" for the must up to date dataset
    # $Time    - 'Requested Time in hh:mm:ss format' or "-" if $date = "NOW"
    # $Selection - "Boolean selection expression for Boundary params" or "-" if not required
    # $instrume  - Instrument name
    # $detnam    - Detector ID or "-"

    # Check CALDB environment

    if ( !$ENV{CALDB} ) {
	&PrntChty(2,"$Task{stem}: Error: CallQuzcif: CALDB environment not set\n");
	&PrntChty(2,"$Task{stem}: Error: CallQuzcif: Please set the CALDB environment or specify the input parameter for '$Dataset'\n");
	$Task{status} = 1;
	return (\@outlist,\@extlist);
    }
    
    my ( $command ) = "quzcif NuSTAR $instrume $detnam - $Dataset $Date $Time \"$Selection\" retrieve+ clobber=yes"; 
    &PrntChty(2,"$Task{stem}: Info: CallQuzcif: Running $command\n"); 
    my ( @ret ) = qx($command);
    &PrntChty(2,"CallQuzcif: Info: Output 'quzcif' Command: \nCallQuzcif: Info:@ret");
    
    if ( $#ret == -1 ) {
	&PrntChty(2,"$Task{stem}: Error: CallQuzcif: No Calibration File Found for '$Dataset' dataset\n");
	&PrntChty(2,"$Task{stem}: Error: CallQuzcif: 'quzcif' command: \"$command\"\n");
	$Task{status} = 1;
	return (\@outlist,\@extlist);
    }

    if ( $#ret != $expnumfile-1) {
	my $n = $#ret +1;
	&PrntChty(2,"$Task{stem}: Error: CallQuzcif: Found " . $n . " calibration file(s) instead of $expnumfile file(s)\n");
	$Task{status} = 1;
	return (\@outlist,\@extlist);
    }				 
    
    foreach $field ( @ret ) {

	my ( $file, $ext );
	
	if ( $field =~ "ERROR" ) {
	    &PrntChty(2,"$Task{stem}: Error: CallQuzcif: 'quzcif': @ret\n");
	    $Task{status} = 1;
	    return (\@outlist,\@extlist);
	}
	
	($file, $ext ) = split /\s+/,$field;
	push @outlist, $file;
	push @extlist, $ext;
	
    }
    
    return (\@outlist,\@extlist);
    
} # CallQuzcif

sub GetFileNameRoot {
    my ( $infile ) = @_;
    my $infilenopath = basename( $infile );
    if ( &IsCompressed($infilenopath) ) { $infilenopath = &EraseLastExtension($infilenopath); }
    $infilenopath = &EraseLastExtension($infilenopath);
    return $infilenopath;
} # GetFileNameRoot


sub GetRootStem {

# It takes the root of a filename erasing the given 'tailstem'
#   i.e: infile = /home/user/rootfiletest.txt
#        tailstem = test.txt
#        return (rootfile)

    my ( $infile , $tailstem) = @_;
    use vars qw ( %Task );
    
    my ( $infilenopath ) = basename( $infile );
    
    # take the root stem
    
    if ( $infilenopath =~ /$tailstem$/ ){
	$infilenopath =~ s/$tailstem$//g;
	$Task{status} = 0; 
    }
    else{
	$Task{errmess} = "Cannot define standard stem of files";
	$Task{status} = 1;       
	return;
    }
    
    return ($infilenopath);

} # GetRootStem


sub SetGtiFileName {

# The subroutine returns the GTI file name following
# standard naming convention

    my ( $evtfile, $obsmode ) = @_;
    use vars qw ( %Task %Def );

    my ( $filenameroot ) = &GetFileNameRoot( $evtfile );

    $filenameroot =~ s/($Def{LEV1STEM}|$Def{LEV2STEM})$//;
    $filenameroot = $filenameroot.$obsmode.$Def{GTISTEM}.$Def{FITSEXT};

    return $filenameroot;

} # SetGtiFileName


sub SetLev2EvtFileName {

# The subroutine returns the Level2 file name following
# standard naming convention

    my ( $evtfile, $obsmode ) = @_;
    use vars qw ( %Task %Def );

    my ( $filenameroot ) = &GetFileNameRoot( $evtfile );

    $filenameroot =~ s/($Def{LEV1STEM})$//;
    $filenameroot = $filenameroot.$obsmode.$Def{LEV2STEM}.$Def{EVTEXT};

    return $filenameroot;

} # SetLev2EvtFileName


sub SetFileNameWithExt {

    my ( $evtfile, $ext ) = @_;
    use vars qw ( %Task %Def );

    my ( $filenameroot ) = &GetFileNameRoot( $evtfile );

    $filenameroot =~ s/($Def{LEV1STEM}|$Def{LEV2STEM})$//;
    $filenameroot = $filenameroot.$ext;

    return $filenameroot;

} # SetFileNameWithExt


sub IsCompressed {
    my ( $infile ) = @_;
    my ( @ZIPEXTENSIONS ) = (".gz",".Z");
    my $ext;

    foreach $ext ( @ZIPEXTENSIONS ) {
	if ( $infile =~ /$ext$/ ) { return 1; }
    }
    return 0;
} # IsCompressed


sub EraseLastExtension {
    my ( $infile ) = @_;
    my $infile2;
    my $i =  rindex( $infile, '.' );
    if ( $i == -1 ) { $infile2 = $infile; }
    else { $infile2 = substr($infile,0,$i); }
    return $infile2;
} # EraseLastExtension


sub WriteParameterList {
    my ($filename,$hdunum,@list) = @_;

    use vars qw (%Task);
   
    my  ($start);

     if ( &GetValPar("history") =~ /no/i) {
	return 0;
    }

    if (&IsCompressed($filename)) {
	&PrntChty(2,"$Task{stem}: Error: WriteParameterList: Cannot write history into a compressed file (file : $filename)\n");
	$Task{status} = 1;
	$Task{errmess} = "Cannot write history into a compressed file (file : $filename)";
	return 1;
    }
   
    my $status = 0;
    my $fits = Astro::FITS::CFITSIO::open_file($filename, READWRITE, $status);
    if (not $fits) {
	&PrntChty(2,"$Task{stem}: Error: WriteParameterList: Unable to open '$filename' file\n");
	$Task{status} = 1;
	return 1;
    }

    if ($hdunum < 0) {
	$start = 1;
	$fits->get_num_hdus($hdunum,$status);
	if ( $status ) {
	    &PrntChty(2,"$Task{stem}: Error: WriteParameterList: unable to get the number of HDUs\n");
	    &PrntChty(2,"$Task{stem}: Error: WriteParameterList: of '$filename' file\n");
	    $Task{status} = 1;
	    $fits->close_file($status);
	    return;
	}
    } else { 
	$start = $hdunum;
    }   

    for (my $ind = $start; $ind<=$hdunum; $ind++) {
	
	$fits->movabs_hdu($ind,undef,$status);
	if ($status) {  
	    &PrntChty(2, "$Task{stem}: Error: WriteParameterList: Running 'fits->movabs_hdu' (num $ind) on $filename\n");
	    $Task{errmess} = "Running movabs_hdu (num $ind) on $filename";
	    $Task{status} = 1;
	    return 1;
	}

	my $p;

	$fits->write_history(" ",$status); 
	$fits->write_history("START PARAMETER list for $Task{stem} at " . &getHistoryTime(),$status); 
	$fits->write_history(" ",$status); 
    
	my $numpar = 0;
	
	foreach $p (@list) {
	    $numpar++;
	    &WriteParHistory($fits,"P" . $numpar . " ",$p->{name} . " = " . $p->{val},\$status);
	    #$fits->write_history($p->{name} . " = " . $p->{val},$status);
	    if ($status) {  
		&PrntChty(2, "$Task{stem}: Error: Running: 'fits->write_history' on $filename\n");
		$Task{errmess} ="Error running write_history on $filename";
		$Task{status} = 1;
	    return 1;
	    }
	}
	$fits->write_history(" ",$status);     
	$fits->write_history("END PARAMETER list for $Task{stem}",$status);
			
	$fits->write_chksum($status);
	if ($status) {
	    $Task{errmess} = "Running write_chksum (num $ind) on $filename";
	    &PrntChty(2,"$Task{stem}: Error: $Task{errmess}\n");
	    $Task{status} = 1;
	    return 0;
	}

    }
    
    $fits->close_file($status);

    return 0;
} # WriteParameterList

    
sub WriteParHistory {
    my ($filefits,$head,$str,$status) = @_;
    
    my ($len,$part,$beg,$end,@liststr);
    my $const = "HISTORY ";

    $part = 80 - length($const) - length($head);
    $beg = 0;
    $end = $part;

    while ($beg < length($str)) {
	push @liststr, substr($str,$beg,$end);
	$beg +=$part;
    }

    for (my $i = 0; $i <= $#liststr; $i++) {
	$filefits->write_history("$head" . "$liststr[$i]",$$status);
    }
    
} # WriteParHistory 


sub CheckRow {

    use vars qw( %Task );

    my ( $file, $ext, $gtifile, $gtiexpr, $gtiextname ) = @_ ;


    my ( @splitexpr, $ee ) ;
    my ( @columns );

    @splitexpr = split ( /&&/,$gtiexpr );

    my ( @lt_col, @le_col, @e_col, @gt_col, @ge_col ); 
    my ( @lt_val, @le_val, @e_val, @gt_val, @ge_val ); 

    foreach $ee ( @splitexpr ) {
	if ( $ee =~ /(\w+)>=(.*)/ ) { 
	    push @ge_col, $1;
	    push @ge_val, $2;
	} 
	elsif ( $ee =~ /(\w+)>(.*)/ ) {
	    push @gt_col, $1;
	    push @gt_val, $2;
	}
	elsif ( $ee =~ /(\w+)==(.*)/ ) {
	    push @e_col, $1;
	    push @e_val, $2;
	}
	elsif ( $ee =~ /(\w+)<=(.*)/ ) {
	    push @le_col, $1;
	    push @le_val, $2;
	}
	elsif ( $ee =~ /(\w+)<(.*)/ ) {
	    push @lt_col, $1;
	    push @lt_val, $2;
	}
	else {
	    &PrntChty(2,"$Task{stem}: Error: Screening Expression '$ee' not allowed\n");
	    $Task{errmess} = "Cannot Pars screening expression";
	    $Task{status} = 1;
	    return 1;
	}
	push @columns, { name => $1 };
    }

    push @columns, { name => "TIME" };
    push @columns, { name => "ENDTIME" };

    my (@table) = &LoadBinTable($file,"",$ext,@columns);
    if ( $Task{status} ) { return -1;}

    my ( $col, $ind );
    my ( $ok ) = 1;

    $ind = -1;
    foreach $col ( @lt_col ) {
	$ind ++;
	if ( !($table[0]->{$col} <= $lt_val[$ind]) ) { 
	    $ok = 0; 
	    &PrntChty(4,"$Task{stem}: Warning: found '$col' set to '$table[0]->{$col}' that is > '$lt_val[$ind]'\n"); 
	    goto end_check; } 
    }
    $ind = -1;
    foreach $col ( @le_col ) {
	$ind ++;
	if ( !($table[0]->{$col} < $le_val[$ind]) ) { 
	    $ok = 0; 
	    &PrntChty(4,"$Task{stem}: Warning: found '$col' set to '$table[0]->{$col}' that is >= '$le_val[$ind]'\n"); 
	    goto end_check; }; 
    }
    $ind = -1;
    foreach $col ( @e_col ) {
	$ind ++;
	if ( !($table[0]->{$col} == $e_val[$ind]) ) { 
	    $ok = 0; 
	    &PrntChty(4,"$Task{stem}: Warning: found '$col' set to '$table[0]->{$col}' that is != '$e_val[$ind]'\n"); 
	    goto end_check; }; 
    }
    $ind = -1;
    foreach $col ( @gt_col ) {
	$ind ++;
	if ( !($table[0]->{$col} > $gt_val[$ind]) ) { 
	    $ok = 0; 
	    &PrntChty(4,"$Task{stem}: Warning: found '$col' set to '$table[0]->{$col}' that is <= '$gt_val[$ind]'\n"); 
	    goto end_check; }; 
    }
    $ind = -1;
    foreach $col ( @ge_col ) {
	$ind ++;
	if ( !($table[0]->{$col} >= $ge_val[$ind]) ) { 
	    $ok = 0; 
	    &PrntChty(4,"$Task{stem}: Warning: found '$col' set to '$table[0]->{$col}' that is < '$ge_val[$ind]'\n"); 
	    goto end_check; }; 
    }

end_check:

    # Cheate the GTI file, if the row does not match the
    # expression the GTI file will be empty

    my ($fptr,$status) = (0,0);

    &PrntChty(3,"$Task{stem}: Info: Creating '$gtifile' file\n");

    fits_create_file($fptr,$gtifile,$status);
 
    if ($status) {
	$Task{status} = 1;
	$Task{errmess} = "$Task{stem}: Error: cannot create $gtifile";
	return 1;
    }
	    
    fits_write_imghdr($fptr,16,0,0,$status);
    if ($status) {
	$Task{status} = 1;
	$Task{errmess} = "Cannot create $gtifile Primary Header";
	return 1;
    }

    my $tform = [ qw(1D 1D) ];
    my $ttype = [ qw(START STOP) ];
    my $tunit = [ ( 'sec', 'sec' ) ];
    
    my $nrows = 0;
    my $tfields = 2;
    my $pcount = 0;
    my $binname = $gtiextname;
    
    fits_insert_btbl($fptr,$nrows,$tfields,$ttype,$tform,$tunit,$binname,0,$status);
    if ($status) {
	$Task{status} = 1;
	$Task{errmess} = "Cannot create $gtifile '$gtiextname' extension";
	return 1;
    }

    if ( $ok ) {
	&PrntChty(3,"$Task{stem}: Info: Output GTI file with START: $table[0]->{TIME} and STOP: $table[0]->{ENDTIME}\n");
	fits_write_col($fptr,TDOUBLE,1,1,1,1, $table[0]->{TIME},$status);
	if ($status) {
	    $Task{status} = 1;
	    $Task{errmess} = "Cannot write $gtifile 'START' value";
	    return 1;
	}
	fits_write_col($fptr,TDOUBLE,2,1,1,1, $table[0]->{ENDTIME},$status);
	if ($status) {
	    $Task{status} = 1;
	    $Task{errmess} = "Cannot create $gtifile 'STOP' value";
	    return 1;
	}
    }
    else {
	&PrntChty(3,"$Task{stem}: Warning: The '$file' row does not match Timing conditions\n");
	&PrntChty(3,"$Task{stem}: Warning: Created Empty GTI '$gtifile'\n");
    }

    fits_close_file($fptr,$status);
    if ($status) {
	&PrntChty(2,"$Task{stem}: Error: 'fits_close_file' error on '$gtifile'\n");
	$Task{status} = 1;
	$Task{errmess} = "cannot close '$gtifile' file";
	return 1;
    }

    return 0;

} # CheckRow


sub CleanTimeNulls {

    #
    # The subroutine returns: 
    #
    #     - 1 if some NULLs have been found in the TIME column and 
    #         the selection have been applied 
    #              _or_ 
    #         if there is an error
    #
    #     - 0 if no NULLs have been found so no selection needed
    #
    my ( $infile, $outfile, $extension ) = @_;

    my ( $ret, $par, $command, %ftselect );
    my $expr = "\"\!isnull(TIME)&&TIME!=-1\"";
    my $null = 0;
    my $outdir = substr($outfile,0,rindex($outfile,"/"));

    #
    # Check if the input file has NULLS in the TIME column
    #
    $null = &RunFtstat($infile,$extension,"TIME","null",$outdir);
    if ( $Task{status} ) { return 1; }

    #
    # If There are some NULLs in the TIME column runfselect
    #

    if ( $null ) {

	&PrntChty(3,"$Task{stem}: Warning: CleanTimeNulls: Found some records in the\n");
	&PrntChty(3,"$Task{stem}: Warning: CleanTimeNulls: '$infile' file with\n");
	&PrntChty(3,"$Task{stem}: Warning: CleanTimeNulls: TIME value set to NULL\n");
	&RunningSub("CleanTimeNulls","ftselect", " to select from '$infile' rows with expression '$expr'");

	#
	# Build the command line to run 'ftselect'
	#
	
	%ftselect = (
		     infile       => $infile,
		     outfile      => $outfile,
		     expression   => $expr,
		     copyall      => "yes",              
		     clobber      => "no",
		     chatter      => $Task{chatter},
		     history      => $Task{history},
		     );
	
	$command = "ftselect";
	for $par ( keys %ftselect ) { $command .= " $par=$ftselect{$par}"; } 
	
	&RunningComm("CleanTimeNulls",$command);
	
	$ret = 0;
	$ret = system( $command );
	
	if ( $ret != 0 ) {
	    &ErrorComm("CleanTimeNulls","ftselect",$command);
	    $Task{errmess} = "Error: running 'ftselect'";
	    $Task{status} = 1;
	    return 1;
	}
	
	# update checksum
	if ( system("ftchecksum $outfile update=yes chatter=0") ) {
	    &PrntChty(1,"$Task{stem}: Error: Error updating checksum\n"); 
	    $Task{errmess} = "Error: updating checksum";
	    $Task{status} = 1;
	    return 1;
	}
	
	&SuccessSub("CleanTimeNulls","ftselect","'$infile' filtered");
	return 1;
    }
    else {
	&PrntChty(4,"$Task{stem}: Info: CleanTimeNulls: Clean of the '$infile' not needed\n");
	return 0;
    }

} # CleanTimeNulls


sub RunFtstat {

    my ( $file, $ext, $col, $stat1, $outdir ) = @_;

    my ( $ret, $par, $command, %ftstat );

    my ($tmpoutfile) = $outdir.$$."ftstat.out";
    unlink ( $tmpoutfile );

    #
    # Build the command line to run 'ftselect'
    #
	
    %ftstat = (
	       infile       => "\'$file\[$ext\]\[col $col\]'",
	       outfile      => "$tmpoutfile",
	       centroid     => "yes",
	       clip         => "no",              
	       nsigma       => 3,   
	       maxiter      => 20,
	       );
    
    $command = "ftstat";
    
    for $par ( keys %ftstat ) { $command .= " $par=$ftstat{$par}"; } 
    
    &RunningComm("RunFtstat",$command);
    
    $ret = 0;
    $ret = system( $command );
    
    if ( $ret != 0 ) {
	&ErrorComm("RunFtstat","ftstat",$command);
	$Task{errmess} = "Error: running 'ftstat'";
	$Task{status} = 1;
	return 1;
    }
    
    # Get the value of the ftstat parameter '$stat1'
    
    $command = "pget ftstat $stat1";
    
    &RunningComm("RunFtstat",$command);
    
    $ret = 0;
    $ret = `$command`;
    chop $ret;
    
    &PrntChty(4,"$Task{stem}: Info: RunFtstat: Read '$stat1' value = '$ret' for column '$col'\n");
    &PrntChty(4,"$Task{stem}: Info: RunFtstat: in the '$file' file\n");
    
    unlink ( $tmpoutfile );

    return $ret;
    
} # RunFtstat

sub Cleanup {

    # Remove a list of files giving a warning
    # if the file cannot be removed

    my ( @list ) = @_;

    use vars qw ( %Task );
    my $file;

    foreach $file ( @list ) {
	if ( -f $file ) {
	    if ( !unlink ( $file ) ) {
		&PrntChty(2,"$Task{stem}: Warning: Cleanup: Unable to remove '$file' file. $!\n");
	    }
	}
    }

    return 0;

} # Cleanup


sub CheckXselectLog {

    # No error code returned by xselect
    # check errors from 'xselect.log' found in
    # current directory
    my $ret = 0;
    my $XselectLog = "xselect.log";
    if ( !-f $XselectLog ) {
	PrntChty(3,"$Task{stem}: Warning: CheckXselectLog: '$XselectLog' not found\n");
	PrntChty(3,"$Task{stem}: Warning: CheckXselectLog: no checks on 'xselect' run performed\n");
    }
    else {
	if ( ! open ( XSELECTLOG, "<$XselectLog" )) {
	    PrntChty(3,"$Task{stem}: Warning: CheckXselectLog: Cannot read '$XselectLog' file\n");
	    PrntChty(3,"$Task{stem}: Warning: CheckXselectLog: no checks on 'xselect' run performed\n");
	}
	else {
	    while ( <XSELECTLOG> ) { 
		if ( $_ =~ /Error/ ) {
		    chop;
		    PrntChty(2,"$Task{stem}: Error: CheckXselectLog: following error message found for 'xselect' run:\n");
		    PrntChty(2,"$Task{stem}: Error: CheckXselectLog: '$_'\n");
		    $ret = 1;
		}
	    }
	    close XSELECTLOG;
	}
    }

    return $ret;

} # CheckXselectLog


sub TableEmpty {

# Input file name and extension: sring filename+extension
# Return status:
#        0: table not empty
#        1: table empty or error

    my ($filename,$extension) = @_;
    use vars qw (%Task);

    # Get keyword
    my ($keyword);
    &GetKeyword($filename,$extension,undef,"NAXIS2",\$keyword); 
    if ($Task{status}){return 1;}
    $keyword =~ s/ |\'//g;

    if ( $keyword == 0 ) { return 1; } # the table is empty

    return 0; # the table is not empty

} # TableEmpty


sub IsNumeric {
    my $InputString = shift;
    
    if ($InputString !~ /^([-|+])?[0-9]*([.|,]([0-9]*))?((e|E)([-|+])?[0-9]+)?$/) {
	return 0;
    }
    else {
	return 1;	
    }	
} # IsNumeric


sub CheckRa {

    # RA with format 'hh mm ss.s' or 'xx.xx'(deg)

    my ($str) = @_;

    use vars qw(%Task);

    my (@l, $hh, $mm, $ss, $rag) ;

    &PrntChty(4,"CheckRa : Info: CheckRa: Performing check on RA '$str' string\n");

    (@l) = split(" ",$str);
    
    if (&trimValue($str) eq "") {
	return 0;
    }

    for (my $ind =0; $ind <=$#l; $ind++) {
	if (! &IsNumeric($l[$ind])) {
	    return 0;
	}
    }

    if ($#l == 0) { 
	$rag = $l[0];
    }
    else {	
	if ($#l >= 3) {
	    return 0;
	}
	
	$hh = $l[0];
	$mm = $l[1]+0;
	$ss = $l[2]+0;
	
	if (($hh < 0) || ($hh > 23)) {
	    return 0;
	}

	if (($mm < 0) ||  ($mm > 59)) {
	    return 0;
	}

	if (($ss < 0) ||  ($ss > 59.9)) {
	    return 0;
	}
	
	$rag = &Ra2Deg($str);
    }

    if (($rag < 0) || ($rag > 360)) {
	return 0;
    }

    return 1;

} # CheckRa 

sub CheckDec {

    # Dec with format 'dd mm ss.s' or 'xx.xx'(deg)

    my ($str) = @_;

    use vars qw(%Task);

    my ($dd,$mm,$ss,@l,$deg) ;

    &PrntChty(4,"CheckDec: Info: CheckDec: Performing check on DEC '$str' string\n");
    
    if (&trimValue($str) eq "") {
	return 0;
    }

    (@l) = split(" ",$str);

    for (my $ind =0; $ind <=$#l; $ind++) {
	if (! &IsNumeric($l[$ind])) {
	    return 0;
	}
    }

    if ($#l == 0) { 
	$deg = $l[0];
    }
    else {	
	if ($#l >= 3) {
	    return 0;
	}
	
	$dd = $l[0];
	$mm = $l[1]+0;
	$ss = $l[2]+0;

	my $neg = 0;

	if ($str =~ /^\-/) {
	    $neg = 1;
	    $dd = 0 - $dd;
	}

	if (($dd < -90) || ($dd > 90)) {
	    return 0;
	}


	if (($mm < 0) ||  ($mm > 59)) {
	    return 0;
	}

	if (($ss < 0) ||  ($ss > 59.9)) {
	    return 0;
	}
    
	$deg = &Dec2Deg($str);
	if ($neg) { $deg = 0 - $deg;}
    }

    if (($deg < -90) || ($deg > 90)) {
	return 0;
    }

    return 1;

} # CheckDec


sub Ra2Deg {

    my ($str) = @_;

    use vars qw(%Task);

    my ($hh,$mm,$ss,@l,$result) ;

    (@l) = split(" ",$str);
    if ($#l == 0) { 
	$result = $l[0];
    }
    else {	
	
	$hh = $l[0];
	$mm = $l[1]+0;
	$ss = $l[2]+0;

	if ($hh =~ /^-/) { $mm *=-1;$ss*=-1;}

	$result = $hh*15 + ($mm*15/60) + ($ss*15/3600);
    }

    return $result;

} # Ra2Deg


sub Dec2Deg {

    my ($str) = @_;

    use vars qw(%Task);

    my ($dd,$mm,$ss,@l,$result) ;

    ($dd,$mm,$ss) = split(" ",$str);

    (@l) = split(" ",$str);
    if ($#l == 0) { 
	$result = $l[0];
    }
    else {	
	
	$dd = $l[0];
	$mm = $l[1]+0;
	$ss = $l[2]+0;

	if ($dd =~ /^-/) { $mm *=-1;$ss*=-1;}

	$result = $dd + ($mm/60) + ($ss/3600);
    }

    return $result;

} # Dec2Deg


sub RaDec2XY {

    my ($file,$ra,$dec,$x,$y) = @_;

    use vars qw( $PI );

    my ($xsize,$ysize,$radian,$roll2,$gamma1,$ximh,$yimh,$dera,$derar,$decr,$decnomr,$rar,$rarnomr,
	$ralde,$ranom,$decnom,$ranomr,$aixnor,$aiynor,$pixelsize,$aix,$aiy,$numcolx,$numcoly, $roll);



    &PrntChty(3,"$Task{'stem'}: Info: RaDec2XY: Converting ra: $ra deg and dec: $dec deg into x,y pixel\n");


    my ($fptr,$status,$comm) = (0, 0);
    fits_open_file($fptr,$file,READONLY,$status);
    if ($status) {
	$Task{status} = 1;
	$Task{errmess} = "Unable to open fits file : $file";
	return 1;
    }
    
    fits_movnam_hdu($fptr,ANY_HDU,"EVENTS",0,$status);
    if ($status) {
	$Task{status} = 1;
	$Task{errmess} = "Cannot move into EVENTS extension in '$file' file";
	fits_close_file($fptr,$status);
	return 1;
    }

    fits_get_colnum($fptr,0,"X",$numcolx,$status);
    if ($status) { 
	$Task{status} = 1;
	$Task{errmess} = "Column X not found in '$file' file";
	fits_close_file($fptr,$status);
	return 1;
    }
    fits_get_colnum($fptr,0,"Y",$numcoly,$status);
    if ($status) { 
	$Task{status} = 1;
	$Task{errmess} = "Column Y not found in '$file' file";
	fits_close_file($fptr,$status);
	return 1;
    }

    fits_read_keyword($fptr,"TCRVL" . $numcolx,$ranom,$comm,$status);
    if ( $status) { 
	$Task{status} = 1;
	$Task{errmess} = "Keyword TCRVL$numcolx not found in '$file' file";
	fits_close_file($fptr,$status);
	return 1;
    }
    fits_read_keyword($fptr,"TCRVL" . $numcoly,$decnom,$comm,$status);
    if ( $status) { 
	$Task{status} = 1;
	$Task{errmess} = "Keyword TCRVL$numcolx not found in '$file' file";
	fits_close_file($fptr,$status);
	return 1;
    }

    fits_read_keyword($fptr,"TCDLT" . $numcolx,$pixelsize,$comm,$status);
    if ( $status) { 
	$Task{status} = 1;
	$Task{errmess} = "Keyword TCRVL$numcolx not found in '$file' file";
	fits_close_file($fptr,$status);
	return 1;
    }
    fits_close_file($fptr,$status);

    $xsize = 1000;
    $ysize = 1000;

    $radian = 180 / $PI;

    $roll = 270;

    $roll2 = -$roll + 270;
    $gamma1 = -$roll2/$radian;

    $ximh = $xsize/2 + 0.5;
    $yimh = $ysize/2 + 0.5;
    $dera = $ranom - $ra;

    if ($dera < -300) {
	$dera = $ranom + 360 - $ra;
    }
    $derar = $dera/$radian;
    $decr = $dec/$radian;
    $decnomr= $decnom/$radian;
    $rar=$ra/$radian;
    $ranomr=$ranom/$radian;
    
    $ralde=acos((sin($decr)*sin($decnomr))+(cos($decr)*cos($decnomr)*cos($derar)));

    $aixnor=0.0;
    $aiynor=0.0;
    if($ralde !=0) {
	$aixnor = $ralde/sin($ralde)*$radian/-$pixelsize*cos($decr)*sin($derar);
	$aiynor = $ralde/sin($ralde)*$radian/-$pixelsize*(sin($decr)*cos($decnomr)- cos($decr)*sin($decnomr)*cos($derar));
    }

    $aix = $aixnor*cos($gamma1) - $aiynor*sin($gamma1) + $ximh;
    $aiy = $aiynor*cos($gamma1) - $aixnor*sin($gamma1) + $yimh;

    $$x = $aix;
    $$y = $aiy;

    &PrntChty(3,"$Task{'stem'}: Info: RaDec2XY: Calculated x: ${aix}pixel and y: ${aiy}pixel\n");

    if (($aix < 0) || ($aix > 1000)) {
        $Task{errmess} = "Sky X coordinate out of range (0,1000)";
	return 1;
    }

    if (($aiy < 0) || ($aiy > 1000)) {
        $Task{errmess} = "Sky Y coordinate out of range (0,1000)";
	return 1;
    }

    return 0;

} # RaDec2XY


sub trimValue() {
    my $value=shift;
    $value =~ s/^\s+//; # remove leading spaces
    $value =~ s/\s+$//; # remove trailing spaces	
    $value =~ s/^'//g;; #Remove apex
    $value =~ s/'$//g;; #Remove apex
    $value =~ s/\s+$//g;; #Remove right space
    return $value;

} # trimValue


sub GetXspecChatter {

    # xspec chatter ranges from 0 to 25

    my ( $TaskChat ) = @_;

    if ( $TaskChat < 3 ) { return 0; }
    elsif ( $TaskChat <= 4 ) { return 5; } 
    else { return 10; } 

} # GetXspecChatter


sub CreateAbsSymbolicLink{

    my ($filename, $linkname) = @_;
    
    if ( !symlink( abs_path($filename), $linkname ) ){
	$Task{status} = 1;
	$Task{errmess} = "Creating symbolic links '".$linkname."' to '".$filename."' ( $!)";
	return 1;
    }

    return 0;

} # CreateAbsSymbolicLink


sub FindCaldbIndxFile {

# This function find the calibration index file path

    my ( $mission, $instrument, $caldbindxfile ) = @_;

    my $line;

    if (!open(FILECONFIG,"<$ENV{CALDBCONFIG}")) {
	$Task{errmess} = "cannot open CALDB config file $ENV{CALDBCONFIG}.";
	$Task{status} = 1;
	#&PrntChty(2,"$Task{stem}: Error: FindCaldbIndxFile: $Task{errmess}\n");
	return 1;
    }

    while ( $line = <FILECONFIG>) {
	
	chop($line);

	if (!($line=~ /^\#/))
	      {
		  my ($lmission,$linstrument,$ldevice,$ldirectory,$lfileindx,$lp6,$lp7)= split(/\s+/, $line);
		  
		  if(($mission eq $lmission)&&($instrument eq $linstrument))
		  {
		      if ($ldevice eq "CALDB")
		      {
			  $ldevice=$ENV{CALDB};
		      }

		      $$caldbindxfile = $ldevice ."/". $ldirectory ."/". $lfileindx;
		      return 0;
		  }
	      }
    }

    $Task{errmess} = "caldb.indx file not found for mission='$mission' and instrument='$instrument'";
    $Task{status} = 1;
    #&PrntChty(2,"$Task{stem}: Error: FindCaldbIndxFile: $Task{errmess}\n");
    return 1;

} #FindCaldbIndxFile



sub GetCaldbVersion {

    my ( $caldbversion ) = @_;
    my ( $caldbindxfile );
    my ( $fptr, $status, $numext ) = ( 0, 0, 0 );
    my ( $value, $comm, $i );

    use vars qw(%Task);


    # Check the environment variables 'CALDB' e 'CALDBCONFIG'
    
    if ( !$ENV{CALDB} ) {
	$Task{errmess} = "CALDB environment variable not found";
	$Task{status} = 1;
	#&PrntChty(2,"$Task{stem}: Error: GetCaldbVersion: $Task{errmess}\n");
	return 1;
    }
    
    if ( !(-d $ENV{CALDB}) ) {
	$Task{errmess} = "$ENV{CALDB} NOT a valid directory.";
	$Task{status} = 1;
	#&PrntChty(2,"$Task{stem}: Error: GetCaldbVersion: $Task{errmess}\n");
	return 1;
    }
    
    if ( !$ENV{CALDBCONFIG} ) {
	$Task{errmess} = "CALDBCONFIG environment variable not found";
	$Task{status} = 1;
	#&PrntChty(2,"$Task{stem}: Error: GetCaldbVersion: $Task{errmess}\n");
	return 1;
    }
    
    if ( !(-f $ENV{CALDBCONFIG}) ) {
	$Task{errmess} = "$ENV{CALDBCONFIG} NOT a valid file.";
	$Task{status} = 1;
	#&PrntChty(2,"$Task{stem}: Error: GetCaldbVersion: $Task{errmess}\n");
	return 1;
    }


    # Find the calibration index file path

    &FindCaldbIndxFile("NUSTAR","FPM",\$caldbindxfile);
    if ( $Task{status} ) {
	#&PrntChty(2,"$Task{stem}: Error: GetCaldbVersion: unable to find the calibration index file.\n");
	return 1;
    }


    # Read the CALDB version from the calibration index file

    if(!(-f $caldbindxfile)){
	$Task{errmess} = "'$caldbindxfile' calibration index file NOT found.";
	$Task{status} = 1;
	#&PrntChty(2,"$Task{stem}: Error: GetCaldbVersion: $Task{errmess}\n");
	return 1;	
    }
    
    fits_open_file($fptr,$caldbindxfile,READONLY,$status);
    if ($status) {
	$Task{errmess} = "Unable to open fits file '$caldbindxfile'";
	$Task{status} = 1;
	#&PrntChty(2,"$Task{stem}: Error: GetCaldbVersion: $Task{errmess}\n");
	return 1;
    }
    
    fits_get_num_hdus($fptr,$numext,$status);
    if ($status) {
	$Task{errmess} = "Unable to get num hdu in file '$caldbindxfile'";
	$Task{status} = 1;
	#&PrntChty(2,"$Task{stem}: Error: GetCaldbVersion: $Task{errmess}\n");
	return 1;
    }

    for ( $i = 1; $i <= $numext; $i++) {

	$status=0;
	fits_movabs_hdu($fptr,$i,ANY_HDU,$status);
	if ($status) {
	    $Task{errmess} = "Unable to open HDU number $i-1 in file '$caldbindxfile'";
	    $Task{status} = 1;
	    #&PrntChty(2,"$Task{stem}: Error: GetCaldbVersion: $Task{errmess}\n");
	    return 1;
	}

	fits_read_keyword($fptr,"CALDBVER",$value,$comm,$status);
	if (!$status) {
	    fits_close_file($fptr,$status);
	    $$caldbversion = &trimValue($value);
	    return 0;
	}
    }


    fits_close_file($fptr,$status);


    $Task{errmess} = "Unable to find 'CALDBVER' in HDU of file '$caldbindxfile'";
    $Task{status} = 1;
    #&PrntChty(2,"$Task{stem}: Error: GetCaldbVersion: $Task{errmess}\n");
    return 1;

} # GetCaldbVersion



1;

