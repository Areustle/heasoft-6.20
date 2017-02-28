#!/usr1/local/bin/perl5

# Before `make install' is performed this script should be runnable withr_stamp
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..1\n"; }
END {print "not ok 1\n" unless $loaded;}
use Astro::FITS::CFITSIO qw( :shortnames :constants );
use HEACORE::HEAUTILS;
use HEACORE::PIL;
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):


my $fptr;
my $argv =['fttest', @ARGV];
my $argc = $#ARGV+2;
$status =0;
$tname = "fttest";
$tvers = "0.1";

$status=HDgtcalf('SWIFT','XRT','NONE','NONE','bias','now','now','now','now','datamode.eq.LOWRATE',100,100,$filenamref,$extnoref,$onlineref,$nret,$nfound,$status);

if ($status) {
   print "Make sure setenv CALDBCONFIG CALDB CALDBALIAS \n";
   $status =0;
}
print "nret = $nret,$status\n";
print "nfound = $nfound,$status\n";

for ( $i=0; $i <$nret; $i++ ) {
     print "filename= $$filenamref[$i] \n";
}
for ( $i=0; $i <$nret; $i++ ) {
     print "online= $$onlineref[$i] \n";
}
for ( $i=0; $i <$nret; $i++ ) {
     print "extno= $$extnoref[$i] \n";
}

print PILInit($argc,$argv) == 0 ? "ok 2 PILInit($argc,$argv) \n" : "not ok 2\n";
print PILGetInt('testp1',$testp1) == 0 ? "ok 3 PILGetInt('testp1',\$testp1), \$testp1=$testp1  \n" : "not ok 3\n";

set_history(1);
set_toolname($tname);
set_toolversion($tvers);

$status=ffopen($fptr,'test.fits',READWRITE,$status);
if ($status) {
   die "Could not open \'test.fits\'. Copy a valid FITS file to \'test.fits\' and retry. Failed";
}
$status = HDpar_stamp($fptr,1, $status);
ffclos($fptr,$status);
 
print "ok 4 $status\n";
