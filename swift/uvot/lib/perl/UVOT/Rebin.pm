# $Source: /headas/headas/swift/uvot/lib/perl/UVOT/Rebin.pm,v $
# $Revision: 1.3 $
# $Date: 2008/12/17 22:01:28 $
#
#	Frank Marshall's rebin function
# /Home/heasfs/marshall/perl/lib/README_rebin.txt
# 
# This file documents the Perl module rebin.pm.
# It was written by F. Marshall beginning in January, 2007.
# The module is in the file /Home/heasfs/marshall/perl/lib/rebin.pm.
# 
# The purpose of the module is to take a time-ordered sequence 
# of rates (a light curve) and combine neighboring data points
# to produce a light curve that includes all the data, but
# with fewer points that are upper limits. Another goal is not
# to hide significant variability in the light curve.
# The module is intended for use with UVOT GRB afterglows,
# and it is expected that the final light curve will be plotted
# as a function of the log of the time. It is expected that all 
# the times are > 0 (e.g., time from the trigger).
# 
# The value returned by the module is either an integer giving
# the number of data points in the rebinned light curve
# or (for failure) a string containing a brief error message
# 
# Input parameters are either pointers to arrays or values
# used to adjust the algorithm for rebinning the light curve.
# There are 5 arrays for the original data (rate, undertainty,
# time, time half-width, and exposure), and another 5 arrays
# for the rebinned light curve.
# 
# There are 5 parameters used for determining how the data are
# rebinned. There are no default values for these parameters. 
# The parameters are:
# 
# * minimum sigma ($min_sig). Neighboring points are combined until
# a signal-to-noise ratio (SNR) exceeding $min_sig is obtained 
# (subject to limits on the range of times and consistency of values 
# -- see below). Larger values of $min_sig produce fewer data points.
# The nominal value is 2.0.
# 
# * maximum ratio ($max_time). The next data point will not be combined
# with the earlier data if the ratio of the (new) end time to the start time 
# exceeds $max_time. In this case the earlier data will become a value
# in the rebinned light curve even though its SNR is low. Larger values 
# of $max_time produce fewer data points.
# The nominal value is 2.0.
# 
# * minimum ratio ($min_time). Neighboring data points will be combined
# until the ratio of the end time to start time exceeds $min_time
# even if the SNR of the combined data exceeds $min_sig. This allows 
# significant detections to be combined if they are close together in time.
# Larger values of $min_time produce fewer data points.
# Setting $min_tim to a value < 1 turns this off.
# The nominal value is 1.2.
# 
# * minimum SNR ($min_snr). A new data point will not be combined
# with earlier data if its value is significantly different (i.e.,
# the differnce is signficant at the $min_snr sigma level). 
# Larger values of $min_snr produce fewer data points.
# The nominal value is 2.0.
# 
# * minimum SNR ($min_snr_orph). The initial rebinned light curve
# is processed in a second pass in an attempt to eliminate "orphans". 
# An orphan is a data point with very low SNR following a point with a  
# high SNR. They typically occur immediately before a gap in the light curve.
# The orphan is combined with the previous point if the resulting
# time span satisfies $max_time and the difference in the two values
# is less than $min_snr_orph sigma. Larger values of $min_snr_orph
# produce fewer points. The nominal value is 2.0.
# 
# Experience with the UVOT GRB data shows that this scheme with its nominal
# parameter values usually produces "attractive" rebinned light curves.
# Of course, "attractive" is a subjective evaluation.
#
# $Log: Rebin.pm,v $
# Revision 1.3  2008/12/17 22:01:28  rwiegand
# Corrected key used to control combining orphans.
#
# Revision 1.2  2008/08/18 15:40:41  rwiegand
# Reimplemented using structured data.
#
# Revision 1.1  2007/10/17 21:18:50  rwiegand
# Module containing Frank Marshall's rebin function.
#

use strict;

package UVOT::Rebin;

use constant DEBUG => 0;

my %DEFAULTS = (
	MIN_SIGMA => 2,
		# start new point if SNR > MIN_SIGMA

	MAX_TIME_RATIO => 2,
		# start new point if tmax/tmin > MAX_TIME_RATIO

	MIN_TIME_RATIO => 1.2,
		# do not start new point if tmax/tmin < MIN_TIME_RATIO

	SNR_DIFF => 2,
		# parameter for not starting new point 

	SNR_DIFF_ORPHAN => 2,
		# parameter for orphan
);

# This routine takes a light curve and rebins data
# according to specified parameters.

# mod 2007-01-10 to add sub orphan
# mod 2007-01-14 to convert rate and sigma to total and net counts
#		     we assume initial time bin gives exposure
# mod 2007-01-16 to track exposure times
# mod 2007-02-02 to return error messages for problems



# ================ rebin ==============================

sub rebin
{
	my ($in, $out, %args) = @_;

	foreach my $key (keys(%DEFAULTS)) {
		if (not defined($args{$key})) {
			$args{$key} = $DEFAULTS{$key};
		}
	}

	my $min_sig = $args{MIN_SIGMA};
	my $max_time = $args{MAX_TIME_RATIO};
	my $min_time = $args{MIN_TIME_RATIO};
	my $min_snr = $args{SNR_DIFF};
	my $min_snr_orph = $args{SNR_DIFF_ORPHAN};


# min sig (larger values produce fewer points)
# maximum ratio of end time to start time (larger values produce fewer points)
#  min. ratio of end time to start time. This allows significant
#     values to be combined if they are close together
#     (larger values produce fewer points)
#     a value < 1 turns this off
#  min. snr to consider 2 points to be consistent
#     (larger values produce fewer points)
#  min. snr to consider 2 points to be consistent for checking for orphans
#     (larger values produce fewer points)

# outputs are:
#	number of new values (0 for failure) or error message

# there are 2 passes through the data (unless min_snr_orph < .001)
# first pass combines data based primarily on SNR
# second pass combines low SNR points (orphans) with previous points if certain
#   criteria are met

	@$out = ();

	if ($min_sig < 0.001 || $max_time < 0.001){
	  return "invalid MIN_SIGMA or MAX_TIME_RATIO";
	}

	my $n = @$in;
	if ($n == 0) {
		return 'empty array';
	}

	my $first = $in->[0];

# start with first point
	my $val = $first->{RATE};
	my $err = $first->{RATE_ERR};
	if ($err <= 0. || $first->{TIME_ERR} < 0.){
		return "Invalid value for uncertainty for point 0";
	}

	my $dt = $first->{EXPOSURE}; # has exposure time
	my $sumcnts = $err * $err * $dt * $dt;
	my $sumnet = $dt * $val;
	my $sumdt = $dt;
	my $wt = $err * $err;
	my $sumr = $val / $wt;
	my $sumsig = 1. / $wt;
	my $xmin = $first->{TIME} - $first->{TIME_ERR};
	my $xmax = $first->{TIME} + $first->{TIME_ERR};
	my @group;

	my ($snr, $xmax1, $trange, $trange1, $diff);

	my $sumval = 0;
	my @firstPass;

	for (my $i = 0; $i < $n; ++$i) {

		$sumval = $sumnet / $sumdt;

		my $i1 = $i + 1;
		my $ith = $in->[$i];
		my $i1th = $i1 < $n ? $in->[$i1] : undef;

		push(@group, $ith);

		if ($i1th) {
			$val = $i1th->{RATE};
			$err = $i1th->{RATE_ERR};

# check for invalid data
			if ($err <= 0. || $i1th->{TIME_ERR} < 0.) {
				return "Invalid value for uncertainty for point $i+1";
			}

			if ($i1th->{TIME} < $ith->{TIME}) {
				return "Time out of order at points $i $i1";
			}

# compute values needed for tests
			$xmax1 = $i1th->{TIME} + $i1th->{TIME_ERR};
			$snr = $sumval / (sqrt($sumcnts) / $sumdt);
			if ($snr < 0.) { $snr = -$snr; }
			$trange = 0;
			$trange1 = 0;
			if ($xmin != 0.){
				$trange = $xmax1 / $xmin;
				$trange1 = $xmax / $xmin;
			}
			$diff = 0.;
			if ($sumsig > 0.) {
				$diff = $val - ($sumnet / $sumdt);
				if ($diff < 0.) {$diff = -$diff; }
# change to signifigance
				$diff = $diff / sqrt(($sumcnts / $sumdt) + $err * $err);
			}	      
		} # close if ($i1th)

# check for invalid data	    
# close out the old value if any of the following are true
#   have no more new data
#   time range with new point straddles 0
#   trange with new point exceeds max value
#   snr exceeds min value and trange without new point exceeds min value
#   new point is significantly different than old
if (DEBUG) {
	printf "%d tmin %6d tmax %6d tmax1 %6d trange: %4.2f trange1: %4.2f "
			. "sumval: %6.4f snr: %4.2f diff: %4.2f\n",
			$i1-1, $xmin, $xmax, $xmax1, $trange, $trange1,
			$sumval, $snr, $diff;
}
if (DEBUG) {
	printf "sumcnts: %6.1f sumnet: %6.1f sumdt: %6.0f\n",
			$sumcnts,$sumnet,$sumdt;
}

		if (($i1 == $n) ||
				($xmin <= 0. && $xmax1 >= 0.) ||
				($trange > $max_time) ||
				($snr > $min_sig && $trange > $min_time) ||
				($diff > $min_snr)) {
# close the old value
			my $tav = ($xmin + $xmax) / 2.0;
			my %rebinned = (
				TIME => $tav,
				TIME_ERR => $tav - $xmin,
				RATE => $sumval,
				RATE_ERR => sqrt($sumcnts) / $sumdt,
				EXPOSURE => $sumdt,
				GROUP => [ @group ],
			);
			push(@firstPass, \%rebinned);

if (DEBUG) {
	printf "* write new point %d tav: %6.0f val: %6.4f sig: %6.4f\n",
			$tav, $sumval, $rebinned{RATE_ERR};
}

# start the new sums
			$sumr = 0.;
			$sumsig = 0.;
			$xmin = $i1th ? ($i1th->{TIME} - $i1th->{TIME_ERR}) : 0;
			$sumdt = 0.;
			$sumnet = 0.;
			$sumcnts = 0.;
			@group = ();
		} # close if ($i1 == $n || ...

# add in new data point
	    $xmax = $xmax1;
	    $wt = $err * $err;
	    $sumr += $val / $wt;
	    $sumsig += 1. / $wt;
	    $dt = $i1th ? $i1th->{EXPOSURE} : 0;
	    $sumcnts += ($err * $err * $dt * $dt);
	    $sumnet += ($dt * $val);
	    $sumdt += $dt;
# print "add point dt: $dt err: $err val: $val\n";

	} # close for (my $i; ...

# ------- check for orphans ------------

# check for orphans if $min_snr_orph > 0
	if ($min_snr_orph > 0.001) {
		my @secondPass;
		checkForOrphans(\@firstPass, \@secondPass, %args);
		@$out = @secondPass;
	}
	else {
		@$out = @firstPass;
	}

	return undef;
}



sub checkForOrphans
{
	my ($in, $out, %args) = @_;

# process is to check whether the next data point should be combined 
# with the previous.
# The criteria are:
#   new data point has low SNR
#   combination satisfies time span criterion
#   data points are consistent

	my $min_sig = $args{MIN_SIGMA};
	my $max_time = $args{MAX_TIME_RATIO};
	my $min_snr_orph = $args{SNR_DIFF_ORPHAN};

	my $count = @$in;

	for (my $i = 0; $i < $count; ++$i) {

		my ($snr, $val1, $err1, $snr1, $t1max, $tmin, $val, $err, $diff);
		my ($t0, $t1, $val2, $sig2, $dt);

		my $ith = $in->[$i];
		my $i1th = $i+1 < $count ? $in->[$i+1] : undef;

		if ($i1th) {
			$val1 = $i1th->{RATE};
			$err1 = $i1th->{RATE_ERR};
			$snr1 = $val1 / $err1;
			if ($snr1 < 0) { $snr1 = -$snr1; }
			if ($snr1 < $min_sig) {
				$t1max = $i1th->{TIME} + $i1th->{TIME_ERR};
				$tmin = $ith->{TIME} - $ith->{TIME_ERR};
				if (($t1max / $tmin) < $max_time) {
					$val = $ith->{RATE};
					$err = $ith->{RATE_ERR};
					$diff = $val1 - $val;
					if ($diff < 0) { $diff = -$diff; }
					$snr = $diff / sqrt($err1 * $err1 + $err * $err);
				}
			} # close if ($snr1 < $min_sig)
		} # close if ($i1th)

		if (defined($snr) and $snr < $min_snr_orph) {
			$t0 = $ith->{EXPOSURE};
			$t1 = $i1th->{EXPOSURE};
			$dt = $t0 + $t1;
			$val2 = ($val * $t0 + $val1 * $t1) / $dt;
			$sig2 = sqrt($err*$err*$t0*$t0 + $err1*$err1*$t1*$t1)/$dt;
			my %merged = (
				RATE => $val2,
				RATE_ERR => $sig2,
				TIME => ($t1max + $tmin) / 2,
				TIME_ERR => ($t1max - $tmin) / 2,
				EXPOSURE => $dt,
				GROUP => [ @{ $ith->{GROUP} }, @{ $i1th->{GROUP} } ],
			);
			push(@$out, \%merged);

			# note the extra increment on $i if the orphan was combined
			++$i;

		} # close if (defined($snr) and ...
		else {
			push(@$out, $ith);
		}
	}
}


1;

