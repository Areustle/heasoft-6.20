
package BAT::tdrss;
use Astro::FITS::CFITSIO qw(:longnames :constants);
use SimpleFITS;

# 
# Retrieve TDRSS GRB information
#
# centroid - retrieve centroid (position) information
# $self - class name (ignored)
# $info - generic information hash reference
#    OUTPUTS: $info->{bat_pos} - BAT GRB position, [$ra, $dec]
#             $info->{xrt_pos} - XRT GRB position, [$ra, $dec]
#             $info->{trigtime/stop} - BAT GRB trigger start/stop time
#             $info->{datetrig} - BAT GRB trigger date
#             $info->{backstrt/stop} - Background start/stop times
#             $info->{mjdrefi/f} - MJDREF integer/fractional values
#             $info->{utcfinit} - UTCFINIT value
#             $info->{trigsatf} - trigger number satisfied
#             $info->{ratesnr} - rate trigger signal to noise
#             $info->{imgsnr} - imaging signal to noise
#             $info->{imagetrg} - was this an image trigger?
#             $info->{catsrc} - is this a known catalog source?
#             $info->{grbdetec} - is this a GRB detection?
#             $info->{pointsrc} - is this a point source?
# 
# $path - wildcard spec for TDRSS files (BAT & XRT)
#
# RETURNS: revised $info
#
# EXCEPTIONS: none
#
# MODIFICATIONS:
#   28 Sep 2004 CM
#     Read the LEAP2001 keyword, in case it ever gets written
#   13 Aug 2007 CM
#     Check the FOREEXPO keyword, to determine if this is a short GRB
#   14 Aug 2007 CM
#     Add the sm_time() function to query the scaled map for trigger times
#
sub centroid {
    my ($self,$info,$path) = @_;
    my ($fits,$filename,$status,$bat_ra,$bat_dec,$xrt_ra,$xrt_dec);
    my ($best_pos,$bat_pos,$xrt_pos);
    my ($trigtime,$trigstop,$backstrt,$backstop);

    while ($filename = glob($path)) {
	$status = 0;
	next unless ($fits = SimpleFITS->open("<$filename"));


	# 
	# ===================================== BAT keywords
	#    CE = present in centroid message
	#    AL = present in alert message
	#                                                # Required for...
	$status = $fits
	    ->readkey("BRA_OBJ",$ra)                     # Analysis CE
	    ->readkey("BDEC_OBJ",$dec)                   # Analysis CE
	    ->status();
	$fits->setstatus(0);
	if ($status == 0) {
	    $info->{bat_pos} = [$ra, $dec];

	    # Read start/stop time keywords
	    $fits->readkey("TRIGTIME",$info->{trigtime}) # Analysis CE AL
		->readkey("TRIGSTOP",$info->{trigstop})  # Analysis CE
		->readkey("BACKSTRT",$info->{backstrt})  # Analysis CE
		->readkey("BACKSTOP",$info->{backstop})  # Analysis CE
		->readkey("FOREEXPO",$info->{foreexpo})  # Analysis CE
		->readkey("MJDREFI",$info->{mjdrefi})    # Analysis CE AL
		->readkey("MJDREFF",$info->{mjdreff})    # Analysis CE AL
		->readkey("UTCFINIT",$info->{utcfinit})  # Analysis CE AL
		->readkey("DATETRIG",$info->{datetrig})  #          CE AL
		->readkey("TRIGSATF",$info->{trigsatf})  # Analysis CE AL
		->readkey("RATESNR",$info->{ratesnr})    # Summary  CE AL
		->readkey("IMGSNR",$info->{imgsnr})      # Summary  CE
		->readkey("IMAGETRG",$info->{imagetrg})  # Refinement CE
		->readkey("CATSRC",$info->{catsrc})      # Summary  CE
		->readkey("GRBDETEC",$info->{grbdetec})  # Summary  CE
		->readkey("POINTSRC",$info->{pointsrc})  # Refinement CE
		->readkey("LEAP2001",$info->{leap2001}); #         -none-

	} else {

	    # ===================================== XRT keywords
	    $status = $fits
		->readkey("XRA_OBJ",$ra)
		->readkey("XDEC_OBJ",$dec)
		->status();
	    if ($status == 0) {
		$info->{xrt_pos} = [$ra, $dec];
	    }
	}

	$fits->setstatus(0)->close();
    }

    return $info;
}


# 
# Retrieve TDRSS GRB information
#
# sm_times - retrieve time intervals from scaled map file
# $self - class name (ignored)
# $info - generic information hash reference
#    OUTPUTS: $info->{trigtime/stop} - BAT GRB trigger start/stop time
#             $info->{backstrt/stop} - Background start/stop times
sub sm_time {
    my ($self,$info,$path) = @_;
    my ($fits,$filename);
    my ($trigtime,$trigstop,$backstrt,$backstop);
    my ($mStartForeSec, $mStartForeSubsec, $mEndForeSec, $mEndForeSubsec);
    my ($mStartBackSec, $mStartBackSubsec, $mEndBackSec, $mEndBackSubsec);

    while ($filename = glob($path)) {
      $status = 0;
      next unless ($fits = SimpleFITS->open("<$filename", ext => 2));
      
      
      # ===================================== BAT scaled map time interval
      $status = $fits
	  ->readcol("mStartForeSec",   {rows=>1, type=>TDOUBLE}, $mStartForeSec)
	  ->readcol("mStartForeSubsec",{rows=>1, type=>TDOUBLE}, $mStartForeSubsec)
	  ->readcol("mEndForeSec",     {rows=>1, type=>TDOUBLE}, $mEndForeSec)
	  ->readcol("mEndForeSubsec",  {rows=>1, type=>TDOUBLE}, $mEndForeSubsec)
	  ->readcol("mStartBackSec",   {rows=>1, type=>TDOUBLE}, $mStartBackSec)
	  ->readcol("mStartBackSubsec",{rows=>1, type=>TDOUBLE}, $mStartBackSubsec)
	  ->readcol("mEndBackSec",     {rows=>1, type=>TDOUBLE}, $mEndBackSec)
	  ->readcol("mEndBackSubsec",  {rows=>1, type=>TDOUBLE}, $mEndBackSubsec)
	  ->status();

      $fits->setstatus(0)->close();
      
      if ($status) {
	# Astro::FITS::CFITSIO::fits_report_error(STDERR, $status);
	die "WARNING: could not read scaled map time interval from $filename (status $status)";
      } else {
	# Perform the data scaling for subseconds (1 subsec = 20 usec)
	$info->{trigtime} = $mStartForeSec->[0] + $mStartForeSubsec->[0]*2.0e-5;
	$info->{trigstop} = $mEndForeSec->[0]   + $mEndForeSubsec->[0]  *2.0e-5;
	$info->{backstrt} = $mStartBackSec->[0] + $mStartBackSubsec->[0]*2.0e-5;
	$info->{backstop} = $mEndBackSec->[0]   + $mEndBackSubsec->[0]  *2.0e-5;

	# We found what we were looking for.  Quit the loop.
	return $info;
      }
      
    }
    
    die "ERROR: could not find a valid scaled map file";
}


# Columns in scaled map file
#    35 mStartForeSec      1J [s]               label for field  35
#    36 mStartForeSubsec   1I [2*10**(-5) s]    label for field  36
#    37 mEndForeSec        1J [s]               label for field  37
#    38 mEndForeSubsec     1I [2*10**(-5) s]    label for field  38

#    42 mStartBackSec      1J [s]               label for field  42
#    43 mStartBackSubsec   1I [2*10**(-5) s]    label for field  43
#    44 mEndBackSec        1J [s]               label for field  44
#    45 mEndBackSubsec     1I [2*10**(-5) s]    label for field  45


# From: David Palmer <palmer@...>
# Subject: Re: ver1 of the 2nd-trigger circular (145729)
# Date: Wed, 13 Jul 2005 11:06:27 -0600
# ...
# More than you need to know:
# The trigger time is the start of a 320 ms interval that contains the 
# end of the time bin.  Thus a 32 ms trigger can start anywhere from 32 
# ms before to 288 ms after the nominal trigger time.  (I believe those 
# are the limits.  I might cast the net a bit wider.)  Also although 
# the initial search is done on strides of half the trigger duration 
# (except for 4 ms triggers, since I don't store 2 ms rate histories), 
# when we trigger, I go back through the 320 ms spacing and search on 4 
# ms steps to get the best interval (with the trigger duration) for 
# imaging.  Therefore, a 64 ms trigger's actual image time will not 
# necessarily be  on a 32ms boundary.




1;
