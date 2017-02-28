#!/usr1/local/bin/perl5
#
# Displays files in directory which match extension
#

if ( @ARGV != 2 ) {
   printf("  Usage: show_ximfiles.pl [directory] [extension]\n");
   exit;
}

$dir = $ARGV[0];
$ext = $ARGV[1];

$cmd = "ls -1 $dir/*.$ext";

open LISTING, "$cmd |" or die;
@list = <LISTING>;
close LISTING;

foreach $file (@list) {
   chop $file;
   $file =~ /([^\/]*)\.$ext$/;
   printf("   $1\n");
}
