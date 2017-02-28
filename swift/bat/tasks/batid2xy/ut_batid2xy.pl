#!/usr/bin/perl

# This is a PERL script to unit test batid2xy

my $headas = $ENV{HEADAS};
print "my headas is $headas \n";

# Validate environment
foreach my $var (qw(HEADAS)) {
        print "VAR value is $var\n";
        if (not defined ($ENV{$var})) {
            die "Environment variable $var not set \n";
        }
	elsif ($ENV{$var} !~ m/^(http|ftp):/ && not -d $ENV{$var}) {
              print "VAR value is $var\n";
              die " invalid $var directory $ENV{$var} \n";
        }
}

my %options = (
                input   => '/local/data/gcn3a/bat_b9_data',
                output  => '/local/data/gcn3a/bat_b9_output',
);

# Get input and output directories input to this script
# Example ut_batid2xy.pl input=<Directory name> output=<Directory name>

foreach my $arg (@ARGV) {
        if ($arg =~ /^input=(.+)/) {
             $options{input} = $1;
        }
        elsif ($arg =~ /^output=(.+)/) {
             $options{output} = $1;
        }
        else {
            die "Invalid option: $arg \n";
        }
}

my $input = $options{input};
   print " Input directory is $input \n";
my $output = $options{output};
   print " Output directory is $output \n";

my %task = (
             input1        => "$input/batid2xy.bdsd.in",
             input2        => "$input/batid2xy.detid.in",
             input3        => "$input/batid2xy.detxy.in",
             output        => "$output/batid2xy.output",
             template1     => "$input/batid2xy.list.template.1",
             template2     => "$input/batid2xy.list.template.2",
             template3     => "$input/batid2xy.list.template.3",
             template4     => "$input/batid2xy.list.template.4",
);

# Check whether the input file exists
foreach my $key (qw(input1 input2 input3 template1 template2 template3 template4)) {
                 print "Input file name is $task{$key} \n";
                 if(not -f $task{$key}) {
                    die "Invalid $key file: $task{$key} \n";
                 }
}

# Delete the output file if it already exists
foreach my $key (qw(output)) {
                 print "Output file name is $task{$key} \n";
                 if(-f $task{$key}) {
                    system "rm -f $task{$key}";
                 }
}



# Check whether all the required parameters for both tests exist
foreach my $p (qw(input1 input2 input3)) {
        if (not defined ($task{$p})) {
            die "Missing parameter $p \n";
        }
}    

my $code=0;

print "          ";
print "\n-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
print "Test 1 - Read list of BAT IDs in Block/DM/Side/Det (BDSD) form from a FITS table\n";
print "batid2xy $task{input1} > $task{output}\n";
$code=system("batid2xy $task{input1} > $task{output}");
print "Return code is $code \n";
if ($code) {
  print "Test 1 batid2xy failed\n";
}
else {
  print "Test 1 batid2xy passed \n";
  print "Checking output file against template \n";
  system("diff $task{output} $task{template1}");
  system("rm -f $task{output}");
}

my $code=0;

print "          ";
print "\n-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
print "Test 2 - Read list of BAT IDs in Detector ID (DETID) form from a FITS table\n";
print "batid2xy $task{input2} > $task{output}\n";
$code=system("batid2xy $task{input2} > $task{output}");
print "Return code is $code \n";
if ($code) {
  print "Test 2 batid2xy failed\n";
}
else {
  print "Test 2 batid2xy passed \n";
  print "Checking output file against template \n";
  system("diff $task{output} $task{template2}");
  system("rm -f $task{output}");
}

my $code=0;

print "          ";
print "\n-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
print "Test 3 - Read list of BAT IDs in Detector X/Y (DETXY) form from a FITS table\n";
print "batid2xy $task{input3} > $task{output}\n";
$code=system("batid2xy $task{input3} > $task{output}");
print "Return code is $code \n";
if ($code) {
  print "Test 3 batid2xy failed\n";
}
else {
  print "Test 3 batid2xy passed \n";
  print "Checking output file against template \n";
  system("diff $task{output} $task{template3}");
  system("rm -f $task{output}");
}

my $code=0;
print "          ";
print "\n-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
print "Test 4 - Read BAT ID from command line\n";
print "batid2xy NONE 0 4 1 56  > $task{output}\n";
$code=system("batid2xy infile=NONE block=0 dm=4 side=1 det=56  > $task{output}");
print "Return code is $code \n";
if ($code) {
  print "Test 4 batid2xy failed \n";
}
else{
  print "Test 4 batid2xy passed \n";
  print "Checking output file against template \n";
  system("diff $task{output} $task{template4}");
  system("rm -f $task{output}");
}
 
print "\n-------------------------------------------------\n";
print "                END OF TESTS   \n";    
