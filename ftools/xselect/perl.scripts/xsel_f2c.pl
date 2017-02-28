#!/usr/local/bin/perl

$LISTFILE = "xsel.files";
open LISTFILE || die "Can't find the list of files: $LISTFILE \n";
$num_of_files = 0;
$outfile = "/tmp/f2c.report";
system("rm -f $outfile");
while ( <LISTFILE> ){
    chop;
    if(! /[ ]*\#/ && /\.f$/ && ! /vms/ && ! /ulx/ ) {
	($oldfile = $_) =~ s/^[ \t]*//g; # Remove spaces and tabs at BOL
	$oldlist[$num_of_files] = $oldfile;
	$num_of_files += 1;
    }
}
$comlin = join(' ','cat ',@oldlist,'| f2c -C -u -72 -Nx400  -\!c 2>&1',"| tee $outfile " ); 
print $comlin,"\n";
system($comlin);

