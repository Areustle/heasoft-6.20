
package BAT::tdrss_lc;

use Astro::FITS::CFITSIO qw(:longnames :constants);
use SimpleFITS;
use BAT::log;

# Copy a TDRSS light curve and split out the RAW_COUNTS vector column
# into individual components

sub split {
    my ($self,$log,$infile,$outfile) = @_;

    $colexpr = 
	"TIME=TIME + (0.5-#TIMEPIXR)*TIMEDEL;" .
	"COUNTS1=RAW_COUNTS[1];" .
	"COUNTS2=RAW_COUNTS[2];" . 
	"COUNTS3=RAW_COUNTS[3];" . 
	"COUNTS4=RAW_COUNTS[4];" .
	"TIMEDEL;" .
	"#HDUCLAS3=\"COUNT\"";

    my $fname = "$infile"."[col $colexpr]";
    $cmd = "ftcopy infile='$fname' outfile=$outfile clobber=yes";
    
    $retval = BAT::log->call($log,$cmd);
    return $retval;
}

# Estimate Bayesian blocks

sub bb {
    my ($self,$log,$infile,$bbgti,$durgti,$column) = @_;

    $column = "RATE" unless ($column);
    $cmd = "battblocks infile='$infile' outfile='$bbgti' durfile='$durgti' bkgsub=yes " .
	"timecol=TIME countscol='$column' hduclas3=RATE chatter=1 " .
	"clobber=yes";

    $retval = BAT::log->call($log,$cmd);
    return $retval;
}

sub stats {
    my ($self,$log,$file,$column,$stat) = @_;

    system("pset fstatistic sum=0");
    $cmd = "fstatistic infile='$file' colname='$column' rows=- outfile=STDOUT " .
	"maxval=INDEF minval=INDEF";
    $? = BAT::log->call($log,"$cmd > /dev/null");

    $value = `pquery2 fstatistic $stat`;
    chomp($value);

    return $value;
}

1;
