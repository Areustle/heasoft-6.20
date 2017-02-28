#!/usr/bin/perl -I/lheadev/zpan/headas/heacore/perl_pil/blib/arch -I/lheadev/zpan/headas/heacore/perl_pil/blib/lib 
# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..10\n"; }
END {print "not ok 1\n" unless $loaded;}
use HEACORE::PIL;
$loaded = 1;
print "ok 1\n";


######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

my $argc = 1;
my $argv = ['ftstat'];

print PILInit($argc,$argv) == 0 ? " ok 2 PILInit($argc,$argv) \n" : "not ok 2\n";
print PILGetInt('maxiter',$maxiter) == 0 ? " ok 3 GetInt('maxiter',\$maxiter), \$maxiter=$maxiter  \n" : "not ok 3\n";
print PILPutInt('maxiter',5) == 0 ? " ok 4 PILPutInt('maxiter',5)\n" : "not ok 4\n";
print PILGetReal('mean',$mean) == 0 ? " ok 5 PILGetReal('mean',\$mean), \$mean=$mean\n" : "not ok 5\n";
print PILGetReal4('mean',$mean) == 0 ? " ok 6 PILGetReal4('mean',\$mean), \$mean=$mean\n" : "not ok 6\n";
print PILPutReal('mean',1000.) == 0 ? " ok 7 PILPutReal('mean',1000.)\n" : "not ok 7\n";
print PILGetFname('infile',$infile) == 0 ? " ok 8 PILGetFname('infile',\$infile), \$infile=$infile\n" : "not ok 8\n";
print PILPutFname('infile','input.fits') == 0 ? " ok 9 PILPutFname('infile','input.fits')\n" : "not ok 9\n";
print PILClose($status) == 0 ? " ok 10 PILClose($status)\n" : "not ok 10\n";
my $argv = ['day2time'];
print PILInit($argc,$argv) == 0 ? " ok 11 PILInit($argc,$argv\n" : "not ok 11\n";
print PILGetString('date',$date) == 0 ? " ok 12 PILGetString('date',\$date),\$date=$date\n" : "not ok 12\n";
print PILPutString('leapfile','hello.fits') == 0 ? " ok 13 PILPutString('leapfile','hello.fits') \n" : "not ok 13\n";
print PILClose($status) == 0 ? " ok 14 PILClose($status)\n" : "not ok 14\n";


