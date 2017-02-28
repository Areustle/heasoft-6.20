#!/usr1/local/bin/perl

use Astro::FITS::CFITSIO qw( :shortnames :constants );
use HEACORE::HEAINIT;
use HEACORE::HEAUTILS;
use HEACORE::PIL;

my $fptr;
my $status =0;

my $argv = ['fttest', @ARGV];
my $argc = $#ARGV+2;


print headas_init($argc,$argv) == 0 ? " ok 2 headas_init \n" : "not ok 2\n";

print PILGetInt('testp1',$testp1) == 0 ? " ok 3 PILGetInt('testp1',\$testp1), \$testp1=$testp1  \n" : "not ok 3\n";

$status=ffopen($fptr,'test.fits',READWRITE,$status);

set_history(1);
$status = HDpar_stamp($fptr,2, $status);

ffclos($fptr,$status);

headas_close($status);
 
print "ok 4 $status\n";
