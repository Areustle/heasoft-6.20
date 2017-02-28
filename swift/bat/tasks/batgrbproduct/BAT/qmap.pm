
package BAT::qmap;
use Astro::FITS::CFITSIO qw(:longnames :constants);
use BAT::log;

# Construct a quality map from event data
#
# Step 40
#
# This module derives a detector quality map based on the input data
# provided.  First, it searches for a detector enable/disable map.  If
# one is found, then it is used, otherwise none is used.
#
# Next, the data are accumulated into a detector plane image, and
# searched for hot pixels.  This pixels are diabled in the final
# quality map.
#


# 
# enablemap - Find enable/disable maps which immediately precedes the time $t
#
# $self - class name (ignored)
# $t - time to search for
# $globspec - wildcard spec matching enable/disable maps
#
# RETURNS: file name (and CFITSIO syntax) specifying enable map
#
# EXCEPTIONS: * no matching disable maps found
#

sub enablemap {
    my ($self, $t, $globspec) = @_;
    my ($file, $nrows, $tena, $status);
    my ($ipre, $imin);
    my (@enablemaps, @times, @files, @indices);

    @enablemaps = glob($globspec);
    die "ERROR: no enable/disable maps found" if ($#enablemaps == -1);

    foreach $file (@enablemaps) {
#	print "enable_file=$file\n";
	$status = SimpleFITS->open("<$file")
	    ->move("Det_Enable_Map")
	    ->readkey("NAXIS2",$nrows)
	    ->readcol("TIME",{type=>TDOUBLE},$tena)
	    ->close()->status();
	next if ($status);
	next if ($nrows <= 0);
#	print "  nrows=$nrows\n";

	push @times, @$tena;           # Times        
	push @files, ($file) x $nrows; # File names
	push @indices, (1 .. $nrows);  # Row numbers
    }

    die "ERROR: no valid enable map files found" if ($#times == -1);

    # Find the largest time which does not exceed $t, the time of
    # the burst.
    foreach $i (0 .. $#times) {
#	print "   t[$i] = $times[$i]    $files[$i] sub $indices[$i]\n";
	if ($times[$i] > $times[$ipre] && $times[$i] < $t) { $ipre = $i; }
	if ($times[$i] < $times[$imin])                    { $imin = $i; }
    }
    if (!defined($ipre)) {
	# Could not find a preceding enable map, so revert to the 
	# Earliest map
	$ipre = $imin;
	warn "  NOTE: Could not find enable map preceding MET $t\n" .
	     "         (using $files[$ipre] row $indices[$ipre])\n";
    }

    # Create the file name for the detector enable map we found
    return "$files[$ipre]"."[Det_Enable_Map; FLAG($indices[$ipre])]";
}

# 
# enablemap - Find enable/disable maps which immediately precedes the time $t
#
# $self - class name (ignored)
# $infiles - wildcard spec of files to accumulate (or @batch file)
# $outfile - temporary detector plane image file name
# $detmask - enable/disable map
#
# RETURNS: nothing
#
# EXCEPTIONS: * no matching input files found
#             * batbinevt failed
#             * bathotpix failed
#
sub hotpix {
    my ($self, $log, $infiles, $outfile, $detmask) = @_;
    my (@globlist, @errlog);
    my ($inlist, $dpifile, $cmd);

    if ("$infiles" =~ m/^@/) { 
	@globlist = ("$infiles");   # Batch file
    } else {
	@globlist = glob("$infiles");
    }
    die "ERROR: no input files" if ($#globlist == -1);
    $inlist = join(",",@globlist);

    $dpifile = "$outfile" . ".dpi.tmp";

    # Make a DPI based on the input events/DPH
    $cmd = "batbinevt infile='$inlist' outfile=$dpifile outtype=DPI timedel=0 timebinalg=u energybins=- " .
	"weighted=NO outunits=COUNTS clobber=yes";
    $? = BAT::log->callnote($log, $cmd, 
			    "Creating trial DPI to search for hot pixels");
    if ($? || ! -f $dpifile) {
	die "ERROR: batbinevt failed while making $dpifile";
    }

    # Default detmask of "NONE" indicates all detectors
    # enabled.
    $detmask = "NONE" unless($detmask);

    # Search for a global mask from CALDB
    $globalmask = "$outfile".".global";
    $cmd = "batdetmask date='$dpifile' outfile='$globalmask' detmask='$detmask' ".
      "keyword='TSTART' clobber=YES";
    unlink("$globalmask");
    $? = BAT::log->callnote($log, $cmd,
			"Calling batdetmask to retrieve global bad detectors");
    if ($? || ! -f $globalmask) {
      warn "WARNING: batdetmask failed while making $globalmask.  Falling ".
	"back to detmask=$detmask.";
      $globalmask = "$detmask";
    } 

    # Make quality map based on the input DPI, plus any disabled
    # detectors.  
    $cmd = "bathotpix infile='$dpifile' outfile='$outfile' detmask='$globalmask' clobber=yes";
    $? = BAT::log->callnote($log, $cmd,
			"Locating noisy detectors");
    if ($? || ! -f $dpifile) {
	die "ERROR: bathotpix failed while making $outfile";
    }

    return;
}

# 
# qmap - Compute quality map
#
# $self - class name (ignored)
# $info - generic information hash reference
#         OUTPUTS: $info->{enamap} - name of found enable/disable map
#                  $info->{qmap} - name of quality map
# $t - time of burst - to search for enable/disable map
# $enaglob - enable/disable wildcard spec
# $evglob - event wildcard spec (or @batch file)
# $qmap - name of output quality map
#
# RETURNS: revised $info
#
# EXCEPTIONS: 
#
sub qmap {
    my ($self, $info, $log, $t, $enaglob, $evglob, $qmap) = @_;


    eval {
	$enamap = BAT::qmap->enablemap($t, "$enaglob");
    };
    if ($?) { 
	warn $?;
	$enamap = "NONE";
    }

    $info->{enable_map} = "$enamap";

    eval {
	BAT::qmap->hotpix($log, "$evglob", "$qmap", "$enamap");
      };
    if ($?) { 
	warn $?;
	$qmap = "NONE";
    }
    $info->{qmap} = "$qmap";

    return $info;
}


1;
