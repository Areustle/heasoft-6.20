#!/usr/bin/perl
my $headas = $ENV{HEADAS};
print "my headas is $headas \n";
# Validate environment
foreach my $var (qw(HEADAS)) {
        if (not defined ($ENV{$var})) {
            die "Environment variable $var not set \n";
        }
        elsif (not -d $ENV{$var}) {
              die " invalid $var directory $ENV{$var} \n";
        }
}

my %options = (
                input   => '/local/data/gcn3a/bat_b9_data',
                output  => '/local/data/gcn3a/bat_b9_output',
);

# Get input and output directories input to this script
# Example ut_batgse2dpi.pl input=<Directory name> output=<Directory name>

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
	        input     => "$input/batgse2dpi.list.input.1",
                windows   => "$input/batgse2dpi.window.input.2",
                output1   => "$output/batgse2dpi.dpi.output.1",
                output2   => "$output/batgse2dpi.mask.output.2",
                output3   => "$output/batgse2dpi.dpi.output.3",
                output4   => "$output/batgse2dpi.mask.output.4",
                output5   => "$output/batgse2dpi.dpi.output.5",
                histmode1 => 'window',
                histmode2 => 'total',
                clobber   => 'Yes',
                chatter   => 1,
                template1   => "$input/batgse2dpi.dpi.template.1",
                template2   => "$input/batgse2dpi.mask.template.2",
                template3   => "$input/batgse2dpi.dpi.template.3",
                template4   => "$input/batgse2dpi.mask.template.4",
                template5   => "$input/batgse2dpi.dpi.template.5",
);

print "Generating list of input files\n";
#system ("ls -1 $input/batgse2dpi.arr.input.*");
system ("ls -1 $input/batgse2dpi.arr.input.* > $task{input}");
system ("cat $task{input}");   


# Validate the required files
foreach my $key (qw(input windows)) {
            print "File name is $task{$key} \n"; 
            if(not -f $task{$key}) {
               die "Invalid $key file: $task{$key} \n";
            }
}  

# Check if all the required parameters exist for all 4 tests
foreach my $p (qw(input output1 output2 output3 output4 windows)) {
        if (not defined ($task{$p})) {
            die "Missing parameter $p \n";
        }
}

foreach my $p1 (qw(histmode1 histmode2)) {
        if (not defined ($task{$p1}) ) {
            die "Missing parameter $p1 \n";
        }
}

foreach my $p2 (qw(clobber chatter)) {
        if (not defined ($task{$p2}) ) {
            die "Missing parameter $p2 \n";
        }
} 


my $code=0;
print "         ";
print "\n-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
print "Test 1 - Processing with windows cut\n";
print " batgse2dpi $task{input} $task{output1} windows=$task{windows} histmode=$task{histmode1} detmask=\!$task{output2} clobber=$task{clobber} chatter=$task{chatter}\n\n";
$code=system("batgse2dpi $task{input} $task{output1} windows=$task{windows} histmode=$task{histmode1} detmask=\!$task{output2} clobber=$task{clobber} chatter=$task{chatter}");
print "Return code is $code \n";
if ($code){
  print "Test 1 batgse2dpi failed \n";
}
else{
  print "Test 1 batgse2dpi passed \n";
  print "Checking output file against template \n";
  system("ftdiff $task{output1} $task{template1} tolerance=20");
  system("ftdiff $task{output2} $task{template2} tolerance=0.01");
}

my $code=0;
print "         ";
print "\n-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
print "Test 2 - Processing without windows cut\n ";
print " batgse2dpi $task{input} $task{output3} histmode=$task{histmode2} detmask=\!$task{output4} clobber=$task{clobber} chatter=$task{chatter}\n";
$code=system("batgse2dpi $task{input} $task{output3} histmode=$task{histmode2} detmask=\!$task{output4} clobber=$task{clobber} chatter=$task{chatter}");
if ($code){
   print "Test 2 batgse2dpi failed \n";
}
else{
   print "Test 2 batgse2dpi passed \n";
   print "Checking output file against template \n";
   system("ftdiff $task{output3} $task{template3} tolerance=2");
   system("ftdiff $task{output4} $task{template4} tolerance=0.01");
}

my $code=0;
print "         ";
print "\n-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
print "Test 3 - Processing without deadtime correction\n ";
print " batgse2dpi $task{input} $task{output5} histmode=$task{histmode2} detmask=NONE deadapp=NO clobber=$task{clobber} chatter=$task{chatter}\n";
$code=system("batgse2dpi $task{input} $task{output5} histmode=$task{histmode2} detmask=NONE deadapp=NO clobber=$task{clobber} chatter=$task{chatter}");
if ($code){
   print "Test 3 batgse2dpi failed \n";
}
else{
   print "Test 3 batgse2dpi passed \n";
   print "Checking output file against template \n";
   system("ftdiff $task{output5} $task{template5} tolerance=0.01");
}

print "\n-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
print "                END OF TESTS\n ";
