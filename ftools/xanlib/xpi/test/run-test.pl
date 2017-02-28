#! /usr/bin/perl
use File::Copy;

# Get name of this script for use in output.
$this_script = $0;
$this_script =~ s:.*/::;
$verbose = 0;

# Set HEADAS before running.
set_up();

# Input for calls to APE & XPI. Initial extraneous "?" is to test
# the "fparhelp" feature of XPI.  APE will return this message:
# "Parameter ba: cannot convert value "?" cleanly to boolean type."
# XPI should pass the "?" to fparhelp which will return this:
# "Cannot find any help for test-xpi"
my $correct_input = "?\nyes\n1.2345678901\nspud.fits\n$ENV{HEADAS}\nspud.fits\n2000000000\nINDEF\n16383\n1.2345\nstring\n11,12-14,15-18\n";
#my $correct_input = "yes\n1.2345678901\nspud.fits\n.\nspud.fits\n2000000000\n2000000000\n2000000000\n1.2345\nstring\n11 12 13 14 15\n";

my $hidden_input = "bh+ dh=1.2345678901 fh=spud.fits frh=Makefile fwh=spud.fits ih=2000000000 sih=16383 rh=1.2345 sh=string";

my $test_binary = "echo '$correct_input$correct_input' | ./test-xpi $hidden_input | grep -v ^\$";
my $cmd = $test_binary;

print "Running command $cmd\n";
my $status = system($cmd) >> 8;
system("diff -w test-ape-get.par test-xpi-get.par");
system("diff -w test-ape-put.par test-xpi-put.par");
exit $status;

sub set_up {

  $ENV{'HEADAS'} =~ /\S/ or die "Set HEADAS before running $this_script\n";

  # Tweak PFILES for this test.
  $ENV{"PFILES"} = ".;".$ENV{"PWD"}."/syspfiles";

  # Remove any previous inputs.
  unlink "./test-ape.par";
  unlink "./test-ape-get.par";
  unlink "./test-ape-put.par";
  unlink "syspfiles/test-ape.par";
  unlink "./test-xpi.par";
  unlink "./test-xpi-get.par";
  unlink "./test-xpi-put.par";
  unlink "syspfiles/test-xpi.par";

  # Copy clean inputs.
  copy("syspfiles/test-xpi-unix.par", "syspfiles/test-ape.par");
  copy("syspfiles/test-xpi-unix.par", "syspfiles/test-xpi.par");

}
