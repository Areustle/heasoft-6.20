
package BAT::gti;
use Astro::FITS::CFITSIO qw(:longnames :constants);
use SimpleFITS;

#
# Utility routines for GTI processing
#

# This module provides some routines for GTI processing.  The first
# routine reads a good time interval file, while the second one writes
# a new GTI file.

# read - read GTI into memory
#
# If the read fails, then $start and $stop will be undef.
#
# $self - class name (ignored)
# $filename - GTI file to read
# $ext - extension name to open
# $start - upon output, array reference to START data
# $stop - upon output, array reference to STOP data
# $header - upon output, hash reference to header keywords
#
# RETURNS: nothing
#
# EXCEPTIONS: none
#
sub read {
    my ($self,$filename,$ext,$start,$stop,$header) = @_;
    my ($fits);

    undef($_[3]);    undef($_[4]);    undef($_[5]);

    $fits = SimpleFITS->open("<$filename")->move("$ext");
    $status = $fits->status();

    return if ($status);

    $status = $fits
	->readcol("START",{type=>TDOUBLE},$start)
	->readcol("STOP", {type=>TDOUBLE},$stop)
	->readheader($header)
	->status();
    $fits->setstatus(0)->close();
    return if ($status);

    $_[3] = $start;
    $_[4] = $stop;
    $_[5] = $header;

}

# write - write GTI to disk
#
# NOTE: if the file exists, then data are appended.
#
# $self - class name (ignored)
# $filename - GTI file to write
# $extname - extension name to create
# $start - array reference to START data
# $stop - array reference to STOP data
# $header - hash reference to header keywords
#           keywords written are: HDUCLAS*, MJDREF*, TIMEZERO
#
# RETURNS: nothing
#
# EXCEPTIONS: none
#
sub write {
    my ($self,$filename,$extname,$start,$stop,$header) = @_;
    my ($fits,$status);
    my (@keynames);

    if ( -f "$filename" ) {
      # File exists already, open for append
      $fits = SimpleFITS->open("+<$filename");
    } else {
      # File does not exist yet, create it
      $fits = SimpleFITS->open(">$filename");
    }
    $status = $fits->status();
    return ($status) if ($status);

    $status = $fits
	->createtab("$extname")
	->writekey("NAXIS2",1)
	->insertcol({TTYPE => ["START", "GTI Start Time"], 
		     TFORM => "1D", TUNIT => "s"})
	->insertcol({TTYPE => ["STOP",  "GTI Stop Time"],
		     TFORM => "1D", TUNIT => "s"})
	->writecol("START",{type=>TDOUBLE},$start)
	->writecol("STOP", {type=>TDOUBLE},$stop)
	->status();

    if (! $status ) {
	$fits->writekey("HDUCLASS","OGIP",
			"Conforms to OGIP/GSFC standards");
	$fits->writekey("HDUCLAS1","GTI",
			"Contains good time intervals");
	$fits->writekey("HDUCLAS2","STANDARD",
			"Contains standard good time intervals");
	$fits->writekey("HDUVERS","1.0.0",
			"Version of GTI header");

	if ($header->{"MJDREF"}) {
	    $fits->writekey("MJDREF",$header->{"MJDREF"},
			    "MJD Epoch of TIME = 0");
	}
	if ($header->{"MJDREFI"}) {
	    $fits->writekey("MJDREFI",$header->{"MJDREFI"},
			    "MJD Epoch of TIME = 0 (Integer)");
	}
	if ($header->{"MJDREFF"}) {
	    $fits->writekey("MJDREFF",$header->{"MJDREFF"},
			    "MJD Epoch of TIME = 0 (Fraction)");
	}
	if ($header->{"TIMEZERO"}) {
	    $fits->writekey("TIMEZERO",$header->{"TIMEZERO"},
			    "Offset of TIME value");
	}
    }
    
    $status = $fits->status();
    $fits->setstatus(0)->close();

    return $status;
}
    


1;
