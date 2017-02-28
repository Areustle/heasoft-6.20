#!/usr/bin/perl

# This is a PERL script to unit test the BAT tools
# This script will call all of the selected BAT unit tests
#
# Usage:
#  ut_batALLTOOLS.pl input=inputdir output=outputdir extractor=fextract-events \
#    tasks=tasks
#
#  inputdir - name of input directory where test data and templates are stored;
#  outputdir - name of output scratch directory; allowed to be empty;
#  extractorname - name of extractor program, either "extractor" or
#    "fextract-events"
#  tasks - comma-separated list of tests
#    Allowed names are:  (order is not important)
#      * any BAT task name 
#      * "all" means every supported task (not obsolete)
#      * "bat" means only supported BAT tasks
#      * "nonbat" means only supported non-BAT tasks such as attjumpcorr
#      * a minus-sign in front of any task name disables that
#        task.  Example: all,-batclean tests everything except batclean
#      * "obsolete" means every obsolete task
#      * DEFAULT: "all"
#  

print " ==========================================================\n";
print "            BAT UNIT TEST SUITE \n";
print " ==========================================================\n";

# Validate environment
print "ENVIRONMENT VARIABLES:\n";
foreach my $var (qw(HEADAS CALDB)) {
        if (not defined ($ENV{$var})) {
            die "ERROR: Environment variable $var not set \n";
        }
        print "    $var = $ENV{$var}\n";
        if ($ENV{$var} !~ m/^(http|ftp):/ && not -d $ENV{$var}) {
              print "VAR value is $var\n";
              die " invalid $var directory $ENV{$var} \n";
        }
}

my %options = (
                input   => 'bat_test_data',
                output  => 'bat_test_output',
	        tasks   => 'all',
);

if ($ENV{CALDB} !~ m/^(http|ftp):/ && 
    ! -f "$ENV{CALDB}/data/swift/bat/caldb.indx" ) {
    die "ERROR: the CALDB variable does not point to a valid BAT CALDB database";
}

# Get input and output directories input to this script
# Example ut_batfftimage.pl input=<Directory name> output=<Directory name>

foreach my $arg (@ARGV) {
        if ($arg =~ /^input=(.+)/) {
             $options{input} = $1;
        }
        elsif ($arg =~ /^output=(.+)/) {
             $options{output} = $1;
        }
	elsif ($arg =~ /^extractor=(.+)/) {
	     $options{extractor} = $1;
	}
	elsif ($arg =~ /^tasks=(.+)/) {
	     $options{tasks} = $1;
	}
        else {
            die "Invalid option: $arg \n";
        }
}

my $input = $options{input};
   print " Input directory is $input \n";
my $output = $options{output};
   print " Output directory is $output \n";
$options{extractor} = "fextract-events" if (! defined($options{extractor}) );

my %task = (
             input         => "$input",
             output        => "$output",
);

# Check whether all the required parameters for both tests exist
foreach my $p (qw(input output)) {
        if (not defined ($task{$p})) {
            die "Missing parameter $p \n";
        }
}    

# Determine all tasks to test
@tasks = split(/,/,$options{tasks});
foreach $t (@tasks) {
  if ($t =~ / *([-+])?([a-zA-Z][-a-zA-Z]*) */) {
    if ($1 eq "-") {
      # "-taskname" means disabled
      $enabled{$2} = 0;
    } else {
      # "taskname" means enabled
      $foundpos = 1;
      $enabled{$2} = 1;
    }
  }
}
# Default if only "-taskname"'s were specified, then assume
# the person meant "all,-taskname"
if (! defined($foundpos) ) {
  $enabled{all} = 1;
}

sub doing {
  my ($t,@fallbacks) = (@_);
  if ($#fallbacks == -1) { @fallbacks = ("all"); }
  my $e = 0;
#  print "Task:$t=$enabled{$t} ";
  foreach $fallback (@fallbacks) {
    $e = $e || $enabled{$fallback};
#    print "$fallback=$enabled{$fallback} ";
  }
  my $result = $enabled{$t} || (!defined($enabled{$t}) && $e);
#  print "\n   --> $result\n";
  return $result;
}
  
if (doing("batbinevt","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batbinevt-- \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batbinevt");
$code{"batbinevt/test1"} = system("ut_batbinevt_lc input=$task{input} output=$task{output}");
system("punlearn batbinevt");
$code{"batbinevt/test2"} = system("ut_batbinevt_pha input=$task{input} output=$task{output}");
system("punlearn batbinevt");
$code{"batbinevt/test3"} = system("ut_batbinevt_dpi input=$task{input} output=$task{output}");
system("punlearn batbinevt");
$code{"batbinevt/test4"} = system("ut_batbinevt_dph input=$task{input} output=$task{output}");
system("punlearn batbinevt");
$code{"batbinevt/test5"} = system("ut_batbinevt_dph2dpi input=$task{input} output=$task{output}");
system("punlearn batbinevt");
$code{"batbinevt/test6"} = system("ut_batbinevt_pha1 input=$task{input} output=$task{output}");
system("punlearn batbinevt");
$code{"batbinevt/test7"} = system("ut_batbinevt_dph2pha input=$task{input} output=$task{output}");
}

if (doing("batcelldetect","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batcelldetect-- \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batcelldetect");
$code{"batcelldetect/nodistort"} = system("ut_batcelldetect input=$task{input} output=$task{output}");
system("punlearn batcelldetect");
$code{"batcelldetect/distort"} = system("ut_batcelldetect input=$task{input} output=$task{output} distfile=INDEF");
}

if (doing("batclean","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batclean-- \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batclean");
$code{"batclean/test1"} = system("ut_batclean_1 input=$task{input} output=$task{output}");
system("punlearn batclean");
$code{"batclean/test2"} = system("ut_batclean_2 input=$task{input} output=$task{output}");
system("punlearn batclean");
$code{"batclean/test3"} = system("ut_batclean_3 input=$task{input} output=$task{output}");
system("punlearn batclean");
$code{"batclean/test4"} = system("ut_batclean_4 input=$task{input} output=$task{output}");
}

# OBSOLETE
if (doing("batdph2dpi","obsolete")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batdph2dpi--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batdph2dpi");
$code{"batdph2dpi"} = system("ut_batdph2dpi input=$task{input} output=$task{output}");
}

# OBSOLETE
if (doing("batdph2pha","obsolete")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batdph2pha--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batdph2pha");
$code{"batdph2pha"} = system("ut_batdph2pha input=$task{input} output=$task{output}");
}

if (doing("batdrmgen","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batdrmgen--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batdrmgen");
$code{"batdrmgen"} = system("ut_batdrmgen input=$task{input} output=$task{output}");
}

if (doing("batdrmgen-multi","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batdrmgen-multi--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batdrmgen-multi");
$code{"batdrmgen-multi"} = system("ut_batdrmgen-multi input=$task{input} output=$task{output}");
}

if (doing("bateconvert","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --bateconvert--   \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn bateconvert");
$code{"bateconvert/test1"} = system("ut_bateconvert_1 input=$task{input} output=$task{output}");
system("punlearn bateconvert");
$code{"bateconvert/test2"} = system("ut_bateconvert_2 input=$task{input} output=$task{output}");
system("punlearn bateconvert");
$code{"bateconvert/test3"} = system("ut_bateconvert_3 input=$task{input} output=$task{output}");
system("punlearn bateconvert");
$code{"bateconvert/test4"} = system("ut_bateconvert_4 input=$task{input} output=$task{output}");
}

if (doing("baterebin","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --baterebin--   \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn baterebin");
$code{"baterebin"} = system("ut_baterebin input=$task{input} output=$task{output}");
}

if (doing("batevt2dpi","bat","all")) {
# print "          \n";
# print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
# print " UNIT TEST FOR --batevt2dpi--  \n";
# print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
# $code{"batevt2dpi"} = system("ut_batevt2dpi input=$task{input} output=$task{output}");
}

if (doing("batfftimage","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batfftimage--   \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batfftimage");
$code{"batfftimage/test1"} = system("ut_batfftimage input=$task{input} output=$task{output}");
system("punlearn batfftimage");
$code{"batfftimage/test2"} = system("ut_batfftimage_pcodemap input=$task{input} output=$task{output}");
}

if (doing("batgse2dpi","bat","all")) {
# print "          \n";
# print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
# print " UNIT TEST FOR --batgse2dpi--    \n";
# print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
# $code{"batgse2dpi"} = system("ut_batgse2dpi.pl input=$task{input} output=$task{output}");
}

if (doing("bathotpix","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --bathotpix--   \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn bathotpix");
$code{"bathotpix"} = system("ut_bathotpix input=$task{input} output=$task{output}");
}

if (doing("batid2xy","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batid2xy--   \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batid2xy");
$code{"batid2xy"} = system("ut_batid2xy.pl input=$task{input} output=$task{output}");
}

if (doing("batmasktaglc","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batmasktaglc--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batmasktaglc");
$code{"batmasktaglc/test1"} = system("ut_batmasktaglc input=$task{input} output=$task{output}");
system("punlearn batmasktaglc");
$code{"batmasktaglc/test2"} = system("ut_batmasktaglc input=$task{input} output=$task{output} suffix=00312242001 ebounds=CALDB");
}

if (doing("batmasktagpha","bat","all")) {
# print "          \n";
# print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
# print " UNIT TEST FOR --batmasktagpha--  \n";
# print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
# $code{"batmasktagpha"} = system("ut_batmasktagpha.pl input=$task{input} output=$task{output}");
}

if (doing("batmaskwtevt","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batmaskwtevt--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batmaskwtevt");
$code{"batmaskwtevt/nodist"} = system("ut_batmaskwtevt input=$task{input} output=$task{output}");
system("punlearn batmaskwtevt");
$code{"batmaskwtevt/dist"} = system("ut_batmaskwtevt input=$task{input} output=$task{output} distfile=INDEF");
}

if (doing("batmaskwtimg","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batmaskwtimg--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batmaskwtimg");
$code{"batmaskwtimg/nodist"} = system("ut_batmaskwtimg input=$task{input} output=$task{output}");
system("punlearn batmaskwtimg");
$code{"batmaskwtimg/dist"} = system("ut_batmaskwtimg input=$task{input} output=$task{output} distfile=INDEF");
}

# OBSOLETE
if (doing("batsumdph","obsolete")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batsumdph--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batsumdph");
$code{"batsumdph"} = system("ut_batsumdph input=$task{input} output=$task{output}");
}

if (doing("battsplit","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --battsplit--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn battsplit");
$code{"battsplit"} = system("ut_battsplit input=$task{input} output=$task{output}");
}

if (doing("battblocks","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --battblocks--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn battblocks");
$code{"battblocks/test1"} = system("ut_battblocks_evt input=$task{input} output=$task{output}");
system("punlearn battblocks");
$code{"battblocks/test2"} = system("ut_battblocks_poi input=$task{input} output=$task{output}");
system("punlearn battblocks");
$code{"battblocks/test3"} = system("ut_battblocks_gau input=$task{input} output=$task{output}");
}


if (doing("batupdatephakw","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batupdatephakw--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batupdatephakw");
$code{"batupdatephakw/test1"} = system("ut_batupdatephakw input=$task{input} output=$task{output}");
system("punlearn batupdatephakw");
$code{"batupdatephakw/test2"} = system("ut_batupdatephakw1 input=$task{input} output=$task{output}");
}

if (doing("batphasyserr","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batphasyserr--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batphasyserr");
$code{"batphasyserr/test1"} = system("ut_batphasyserr input=$task{input} output=$task{output} phatype=1");
system("punlearn batphasyserr");
$code{"batphasyserr/test2"} = system("ut_batphasyserr input=$task{input} output=$task{output} phatype=2");
}

if (doing("batphasimerr","bat","all")) {
print "          \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batphasimerr--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batphasimerr");
$code{"batphasimerr/manual"} = system("ut_batphasimerr input=$task{input} output=$task{output} bkgfile=INDEF");
system("punlearn batphasimerr");
$code{"batphasimerr/caldb"} = system("ut_batphasimerr input=$task{input} output=$task{output} bkgfile=CALDB");
}

if (doing("batoccultmap","bat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batoccultmap--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batoccultmap");
$code{"batoccultmap/test1"} = system("ut_batoccultmap input=$task{input} output=$task{output}");
system("punlearn batoccultmap");
$code{"batoccultmap/test2"} = system("ut_batoccultmap_contour input=$task{input} output=$task{output}");
}

if (doing("batwarpimg","bat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batwarpimg--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batwarpimg");
$code{"batwarpimg"} = system("ut_batwarpimg input=$task{input} output=$task{output}");
}

if (doing("batoccultgti","bat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batoccultgti--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batoccultgti");
$code{"batoccultgti"} = system("ut_batoccultgti input=$task{input} output=$task{output}");
}

if (doing("batglobalgti","bat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batglobalgti--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batglobalgti");
$code{"batglobalgti"} = system("ut_batglobalgti input=$task{input} output=$task{output}");
}

if (doing("batdetmask","bat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batdetmask--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batdetmask");
$code{"batdetmask"} = system("ut_batdetmask input=$task{input} output=$task{output}");
}

if (doing("batimgstatpos","bat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batimgstatpos--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batimgstatpos");
$code{"batimgstatpos/nodistort"} = system("ut_batimgstatpos input=$task{input} output=$task{output} distfile=NONE");
system("punlearn batimgstatpos");
$code{"batimgstatpos/distort"} = system("ut_batimgstatpos input=$task{input} output=$task{output} distfile=INDEF");
}


if (doing("batgrbproduct","bat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batgrbproduct--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batgrbproduct");
$code{"batgrbproduct"} = system("ut_batgrbproduct input=$task{input} output=$task{output} obsid=00232683000 extractor=$options{extractor}");
}

if (doing("batsurvey","bat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batsurvey--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batsurvey");
$code{"batsurvey"} = system("ut_batsurvey input=$task{input} output=$task{output} obsid=batsurvey-00035025015");
}

if (doing("batsurvey-catmux","bat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --batsurvey-catmux--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn batsurvey-catmux");
$code{"batsurvey-catmux"} = system("ut_batsurvey-catmux input=$task{input} output=$task{output}");
}

if (doing("rebingausslc","nonbat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --rebingausslc-- \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn rebingausslc");
$code{"rebingausslc"} = system("ut_rebingausslc input=$task{input} output=$task{output}");
}


if (doing("attjumpcorr","nonbat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --attjumpcorr--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn attjumpcorr");
$code{"attjumpcorr/test1"} = system("ut_attjumpcorr input=$task{input} output=$task{output} testcase=1");
system("punlearn attjumpcorr");
$code{"attjumpcorr/test2"} = system("ut_attjumpcorr input=$task{input} output=$task{output} testcase=2");
}

if (doing("ftimgcalc","nonbat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --ftimgcalc--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn ftimgcalc");
$code{"ftimgcalc"} = system("ut_ftimgcalc input=$task{input} output=$task{output}");
}

if (doing("ftcoco","nonbat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --ftcoco--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn ftcoco");
$code{"ftcoco/gal"} = system("ut_ftcoco input=$task{input} output=$task{output} incoord=R");
system("punlearn ftcoco");
$code{"ftcoco/equ"} = system("ut_ftcoco input=$task{input} output=$task{output} incoord=G");
}

if (doing("ftjoin","nonbat","all")) {
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n";
print " UNIT TEST FOR --ftjoin--  \n";
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
system("punlearn ftjoin");
$code{"ftjoin/inner"} = system("ut_ftjoin input=$task{input} output=$task{output} jointype=INNER");
system("punlearn ftjoin");
$code{"ftjoin/left"} = system("ut_ftjoin input=$task{input} output=$task{output} jointype=LEFTOUTER");
system("punlearn ftjoin");
$code{"ftjoin/right"} = system("ut_ftjoin input=$task{input} output=$task{output} jointype=RIGHTOUTER");
system("punlearn ftjoin");
$code{"ftjoin/full"} = system("ut_ftjoin input=$task{input} output=$task{output} jointype=FULLOUTER");
}

print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";
print "                END OF ALL BAT TESTS   \n";    
print "-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*\n\n";

print "================================================= \n";
print "         BAT TEST SUMMARY \n";
print "================================================= \n";

@tests = sort keys(%code);
$passed = 0; $failed = 0;
foreach $test (@tests) {
    if ($code{$test} == 0) {
	$result = "pass";
	$passed ++;
    } else {
	$result = "fail";
	$failed ++;
    }
    printf "%30s: %s\n", $test, $result;
}

print "BAT GRAND SUMMARY:  $passed passed,   $failed failed\n";

exit 0;
