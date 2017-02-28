#!/usr/bin/perl

# This is a PERL script to unit test batid2xy

my $headas = $ENV{HEADAS};
print "my headas is $headas \n";

# Validate environment
foreach my $var (qw(HEADAS FTOOLS)) {
        print "VAR value is $var\n";
        if (not defined ($ENV{$var})) {
            die "Environment variable $var not set \n";
        }
        elsif (not -d $ENV{$var}) {
              print "VAR value is $var\n";
              die " invalid $var directory $ENV{$var} \n";
        }
}

my %options = (
                batidmod   => 'bdsd',
                infile     => 'data.txt',
                outfile    => 'data.fits',
                indir      => '.',
);

foreach my $arg (@ARGV) {
        if ($arg =~ /^batidmod=(.+)/) {
             $options{batidmod} = $1;
        }
        elsif ($arg =~ /^infile=(.+)/) {
             $options{infile} = $1;
        }
        elsif ($arg =~ /^outfile=(.+)/) {
             $options{outfile} = $1;
        }
        elsif ($arg =~ /^indir=(.+)/) {
             $options{indir} = $1;
        }
        else {
            die "Invalid option: $arg \n";
        }
}

my $batidmod = $options{batidmod};
   print " BATIDMOD is $batidmod \n";
my $infile = $options{infile};
   print " Input file is $infile \n";
my $outfile = $options{outfile};
   print " Output file is $outfile \n";
my $indir = $options{indir};
   print " Template directory is $indir \n";

my %task = (
             infile        => "$infile",
             outfile       => "$outfile",
             bdsd_lis      => "$indir/bdsd_mode.lis",
             bdsd_hdr      => "$indir/bdsd_mode.hdr",
             detid_lis     => "$indir/detid_mode.lis",
             detid_hdr     => "$indir/detid_mode.hdr",
             detxy_lis     => "$indir/detxy_mode.lis",
             detxy_hdr     => "$indir/detxy_mode.hdr",
             extname       => "DETID",
             clobber       => "yes",
);

# Check whether the input file exists
foreach my $key (qw(infile bdsd_lis bdsd_hdr detid_lis detid_hdr detxy_lis detxy_hdr)) {
                 if(not -f $task{$key}) {
                    die "Invalid $key file: $task{$key} \n";
                 }
}

# Delete the output file if it already exists
foreach my $key (qw(outfile)) {
                 print "Output file name is $task{$key} \n";
                 if(-f $task{$key}) {
                    system "rm -f $task{$key}";
                 }
}

my $code=0;

if ($batidmod =~ /^bdsd/) {
   print "fcreate $task{bdsd_lis} $task{infile} $task{outfile} extname='$task{extname}' headfile='$task{bdsd_hdr}' clobber='$task{clobber}' \n";
   $code=system("fcreate $task{bdsd_lis} $task{infile} $task{outfile} extname='$task{extname}' headfile='$task{bdsd_hdr}' clobber='$task{clobber}' ");
   print "Return code is $code \n";
   if ($code) {
      print "fcreate for batid2xy failed\n";
   }
   else {
      print "fcreate for batid2xy passed \n";
   }
} elsif ($batidmod =~ /^detid/) {
   print "fcreate $task{detid_lis} $task{infile} $task{outfile} extname='$task{extname}' headfile='$task{detid_hdr}' clobber='$task{clobber}' \n";
   $code=system("fcreate $task{detid_lis} $task{infile} $task{outfile} extname='$task{extname}' headfile='$task{detid_hdr}' clobber='$task{clobber}' ");
   print "Return code is $code \n";
   if ($code) {
      print "fcreate for batid2xy failed\n";
   }
   else {
      print "fcreate for batid2xy passed \n";
   }
} elsif ($batidmod =~ /^detxy/) {
   print "fcreate $task{detxy_lis} $task{infile} $task{outfile} extname='$task{extname}' headfile='$task{detxy_hdr}' clobber='$task{clobber}' \n";
   $code=system("fcreate $task{detxy_lis} $task{infile} $task{outfile} extname='$task{extname}' headfile='$task{detxy_hdr}' clobber='$task{clobber}' ");
   print "Return code is $code \n";
   if ($code) {
      print "fcreate for batid2xy failed\n";
   }
   else {
      print "fcreate for batid2xy passed \n";
   }
}


print "\n-------------------------------------------------\n";
print "                END OF CONVERSION   \n";    



