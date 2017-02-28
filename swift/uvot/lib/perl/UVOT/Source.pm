# $Source: /headas/headas/swift/uvot/lib/perl/UVOT/Source.pm,v $
# $Revision: 1.40 $
# $Date: 2015/09/22 14:14:21 $
#
# $Log: Source.pm,v $
# Revision 1.40  2015/09/22 14:14:21  rwiegand
# Avoid calculations triggering divide-by-zero exceptions when source and/or background regions do not overlap image.
#
# Revision 1.39  2013/07/03 16:26:50  rwiegand
# Need to divide by LSS_FACTOR when determining CORR_RATE_LIMIT.
#
# Revision 1.38  2013/07/01 18:39:11  rwiegand
# Updated the derivation of the rate limit to correctly address uncertainty
# in the background.
#
# Revision 1.37  2013/06/24 17:58:44  rwiegand
# Include the aperture, large scale sensitivity, and detector sensitivity
# corrections in the upper limit calculations (previously, only coincidence
# loss was included).  Added the CORR_RATE_LIMIT columns and derive the MAG_LIM,
# MAG_HZ_LIM, MAG_AA_LIM, FLUX_AA_LIM, FLUX_HZ_LIM columns from CORR_RATE_LIMIT
# instead of COI_RATE_LIMIT.
#
# Revision 1.36  2012/07/27 16:47:20  rwiegand
# Include SATURATED column in output table.
#
# Revision 1.35  2011/11/07 22:55:38  rwiegand
# Perform sanity checks on LSS factor.
#
# Revision 1.34  2011/02/28 17:10:08  rwiegand
# Moved flux density calculations to library.  Identify UVOT magnitudes
# as Vega system.
#
# Revision 1.33  2011/02/09 21:07:12  rwiegand
# Extensions to support multiple magnitude systems.
#
# Revision 1.32  2010/12/08 14:41:10  rwiegand
# Motivated by requirement for uvotdetect to support LSS correction, modified
# photometry module so that each correction uses the generic names for the
# input (UNCORR_RATE) and output (CORR_RATE).
#
# Revision 1.31  2010/06/23 19:16:51  rwiegand
# Revised loading / application of detector sensitivity to support processing
# multiple filters.
#
# Revision 1.30  2010/06/03 18:54:19  rwiegand
# Updated the fully corrected rate (CORR_RATE) to include the detector
# sensitivity loss correction.
#
# Revision 1.29  2010/05/28 18:01:00  rwiegand
# Changed name of detector sensitivity loss time column from TZERO to TIME.
#
# Revision 1.28  2010/04/30 21:08:03  rwiegand
# Implemented detector sensitivity correction.  Applied a patch from Scott
# Koch to avoid a divide by zero exception.
#
# Revision 1.27  2010/03/03 21:57:06  rwiegand
# Support taking LSS corrections from a sky LSS image.
#
# Revision 1.26  2009/07/22 00:43:03  rwiegand
# Oops, needed a weaker OR.
#
# Revision 1.25  2009/07/21 23:23:46  rwiegand
# Provide routines for loading the aperture correction calibration and
# applying the corrections.
#
# Revision 1.24  2009/07/20 19:52:48  rwiegand
# Keep separate magnitude and flux statistical and systematic errors.
#
# Revision 1.23  2009/04/29 21:12:18  rwiegand
# Prefer swifttime to met2utc for time conversions.
#
# Revision 1.22  2009/03/13 18:29:13  rwiegand
# Determine coincidence loss error factor for background similar to source
# and apply to background error rate.
#
# Revision 1.21  2009/03/03 19:58:40  rwiegand
# Patch to coincidence loss error calculations caught by Frank, fixed by Wayne.
#
# Revision 1.20  2008/10/31 17:05:02  rwiegand
# Accept negative NaN in applyLargeScaleSensitivity.
#
# Revision 1.19  2008/10/16 18:14:23  rwiegand
# By default, use uvotinteg instead of ximage to determine counts in regions.
# Use dummy CoI correction factor for negative rates.  Prevent warnings when
# trying to load WCS information.
#
# Revision 1.18  2008/08/21 21:16:44  rwiegand
# Being able to override SRC_AREA was a bad idea.
#
# Revision 1.17  2008/08/18 15:36:16  rwiegand
# Split off code which calls ximage from photometry calculations.
#
# Revision 1.16  2008/07/02 15:41:21  rwiegand
# Store the source significance in the NSIGMA key.  Store the most corrected
# rate and error in CORR_RATE(_ERR).  Throw an exception if an attempt is made
# to coincidence loss correct a negative rate.
#
# Revision 1.15  2008/05/12 17:49:26  rwiegand
# Apply minimum count error of 1 [see 2008 May 12 note from Wayne].
#
# Revision 1.14  2008/04/22 19:55:11  rwiegand
# Allow controlling fwhmsig parameter of uvotapercorr.
#
# Revision 1.13  2008/03/28 17:58:09  rwiegand
# Throw an exception if frame time is not positive.
#
# Revision 1.12  2007/11/05 22:55:11  rwiegand
# Use NULL (undef/NaN) for unknown detector positions instead of -1.  Give
# warning when raw positions are truncated.
#
# Revision 1.11  2007/11/01 16:06:50  rwiegand
# Only load the LSS coefficient at the RAW position instead of the whole
# image.
#
# Revision 1.10  2007/10/31 15:19:01  rwiegand
# Divide by LSS_FACTOR instead of multiplying.
#
# Revision 1.9  2007/10/30 21:25:37  rwiegand
# Wayne pointed out that updateDetectorPosition was calculating DET world
# coordinates instead of pixel as needed.
#
# Revision 1.8  2007/10/25 13:36:52  rwiegand
# Implemented support for large scale sensitivity (LSS).
#
# Revision 1.7  2007/07/26 18:49:38  rwiegand
# Paul Kuin's update of the coi-correction to the background error.  Wayne's
# fix to the coi-correction of the background error for aperture sizes
# smaller than the standard aperture.
#
# Revision 1.6  2007/07/17 14:41:09  rwiegand
# Added parameter for PSF CALDB file.
#
# Revision 1.5  2007/07/12 18:45:35  rwiegand
# Corrected calculation of circle radius from area (duh).
#
# Revision 1.4  2007/05/25 15:00:07  rwiegand
# Store frametime and other coincidence loss parameters with calibration
# hash.  Updated calculation of number of frames.
#
# Revision 1.3  2007/05/21 18:56:49  rwiegand
# Added source calibration library routines.
#
# Revision 1.2  2007/04/19 19:43:57  rwiegand
# Added routine for calibrating source magnitude/flux.
#
# Revision 1.1  2006/12/18 19:20:55  rwiegand
# Helper functions for source calibration.
#

use strict;

package UVOT::Source;

use Task qw(:codes);

use Math;
use UVOT::Calibration;

use constant FRAME_COUNT_LIMIT => 0.98;
use constant VERTICAL_TRANSFER_TIME_s => 6e-7;
use constant VERTICAL_PIXELS_PER_IMAGE => 290;

use constant DURATION_DAY_s => 24 * 60 * 60;
use constant DURATION_YEAR_s => 365.25 * DURATION_DAY_s;

use constant c_m_per_s => 2.99792458e8;

use constant k_A_per_m => 1e10;


my %DEFAULT_LAMBDA_A = (
	V => 5401.8,
	B => 4328.5,
	U => 3501.3,
	UVW1 => 2634.1,
	UVM2 => 2230.6,
	UVW2 => 2030.2,
	WHITE => 3470.7,
);



sub runXimage
{
	my ($task, $hdu) = @_;

	my $command = 'ximage'
		. " 'set exit_on_startfail 1;"
		. qq( read/szx=$hdu->{NAXIS1}/szy=$hdu->{NAXIS2} "$hdu->{FITSEXT}";)
		. qq( counts/regionfile="$hdu->{SRC_PATH}";)
		. qq( counts/regionfile="$hdu->{BKG_PATH}";)
		. qq( counts/regionfile="$hdu->{STD_PATH}";)
		. " exit;' 2>&1";

	my $result = $task->shell($command);

	if (not $result->{error}) {
		my $xim = 0;
		foreach my $line (@{ $result->{lines} }) {
			if ($line =~ /^#\s+(\S+)\s+(\S+)\s+(\S+)/) {
				++$xim;
				if ($xim == 1) {
					$hdu->{RAW_TOT_CNTS} = $1;
					$hdu->{SRC_AREA} = $2 * $hdu->{AS2PP};
				}
				elsif ($xim == 2) {
					$hdu->{RAW_BKG_CNTS} = $1;
					$hdu->{BKG_AREA} = $2 * $hdu->{AS2PP};
				}
				elsif ($xim == 3) {
					$hdu->{RAW_STD_CNTS} = $1;
					$hdu->{STD_AREA} = $2 * $hdu->{AS2PP};
				}
			}
		}
		if ($xim != 3) {
			$task->error(BAD_EXECUTE,
				"unexpected XIMAGE output\n---$result->{text}---");
		}
	}

}


sub runUvotinteg
{
	my ($task, $hdu) = @_;

	my $subpixels = $hdu->{SUBPIXELS} || $ENV{UVOT_SOURCE_SUBPIXELS} || 8;

	my $command = $task->buildCommand('uvotinteg',
			infile => $hdu->{FITSEXT},
			regfile => "$hdu->{SRC_PATH},$hdu->{BKG_PATH},$hdu->{STD_PATH}",
			operation => 'AREA,SUM',
			subpixels => $subpixels,
			);

	my $result = $task->shell($command);

	if (not $result->{error}) {
		my $index = 0;
		my %LOOKUP = (
			'area:1' => 'SRC_AREA',
			'sum:1' => 'RAW_TOT_CNTS',

			'area:2' => 'BKG_AREA',
			'sum:2' => 'RAW_BKG_CNTS',

			'area:3' => 'STD_AREA',
			'sum:3' => 'RAW_STD_CNTS',
		);
		foreach my $line (@{ $result->{lines} }) {
			if ($line =~ /^monitor: (area|sum):\s+(\d+\.\d+)/) {
				my $tag = $1;
				if ($tag eq 'area') {
					++$index;
				}
				my $value = $2;
				my $lookup = "$tag:$index";
				if (exists($LOOKUP{$lookup})) {
					my $key = $LOOKUP{$lookup};
					if ($tag eq 'area') {
						$hdu->{$key} = $value * $hdu->{AS2PP};
						if ($value <= 0) {
							$task->warning("$key is $value");
						}
					}
					else {
						$hdu->{$key} = $value;
					}
				}
			}
		}
		if ($index != 3) {
			$task->error(BAD_EXECUTE,
				"unexpected uvotinteg output\n---$result->{text}---");
		}
	}

}


sub updateFrametimeParameters
{
	my ($cal, $o) = @_;

	if (not $cal->{FRAMETIME_s}) {
		$cal->{FRAMETIME_s} = $o->{FRAMTIME}
				|| UVOT::Calibration::DEFAULT_FRAMETIME_s;
	}

	if ($cal->{FRAMETIME_s} <= 0) {
		die "invalid frame time $cal->{FRAMETIME_s}\n";
	}

	if (not exists($cal->{DEADTIME_CORRECTED})) {
		$cal->{DEADTIME_CORRECTED} = 1;
	}

	$cal->{DEADTIME_f} = VERTICAL_TRANSFER_TIME_s * VERTICAL_PIXELS_PER_IMAGE
			/ $cal->{FRAMETIME_s};
	my $kDead = $cal->{DEADTIME_CORRECTED} ? (1 - $cal->{DEADTIME_f}) : 1;
	$o->{NFRAMES} = $o->{EXPOSURE} / $cal->{FRAMETIME_s} / $kDead;
}


sub deriveRatesAux
{
	my ($cnts, $nframes) = @_;
	my $cntserr;
	if ($cnts < $nframes) {
		$cntserr = sqrt($cnts * ($nframes - $cnts) / $nframes);
	}
	else {
		$cntserr = sqrt($cnts);
	}
	if ($cntserr < 1) {
		$cntserr = 1;
	}
	return $cntserr;
}


sub deriveBkgErr
{
	my ($cnts, $nframes, $bkg_area) = @_;
	my $cntserr;
# Assume a coincidence region of 80 sq arc sec
	my $radius = 5;
	my $area_ratio = $bkg_area / 80.;
	if ($area_ratio < 1) { $area_ratio = 1; }
	my $nframes_eff = $nframes * $area_ratio;
	if ($cnts < $nframes_eff) {
		$cntserr = sqrt($cnts * ($nframes_eff - $cnts) / $nframes_eff);
	}
	else {
		$cntserr = sqrt($cnts);
	}
	if ($cntserr < 1) {
		$cntserr = 1;
	}
	return $cntserr;
}


sub deriveRates
{
	my ($o) = @_;

# EXPOSURE in seconds
# *_AREA in arcsec^2
	foreach my $key (qw(RAW_TOT_CNTS RAW_BKG_CNTS RAW_STD_CNTS
			SRC_AREA BKG_AREA STD_AREA EXPOSURE)) {
		die "UVOT::Source: missing $key" if not exists($o->{$key});
	}

	$o->{RAW_TOT_CNTS_ERR} = deriveRatesAux($o->{RAW_TOT_CNTS}, $o->{NFRAMES});
	$o->{RAW_BKG_CNTS_ERR} = deriveBkgErr($o->{RAW_BKG_CNTS}, $o->{NFRAMES},
			$o->{BKG_AREA});
	$o->{RAW_STD_CNTS_ERR} = deriveRatesAux($o->{RAW_STD_CNTS}, $o->{NFRAMES});

	$o->{RAW_TOT_RATE} = $o->{RAW_TOT_CNTS} / $o->{EXPOSURE};
	$o->{RAW_TOT_RATE_ERR} = $o->{RAW_TOT_CNTS_ERR} / $o->{EXPOSURE};

	if ($o->{BKG_AREA} > 0) {
		$o->{RAW_BKG_RATE} = $o->{RAW_BKG_CNTS} / $o->{EXPOSURE} / $o->{BKG_AREA};
		$o->{RAW_BKG_RATE_ERR} = $o->{RAW_BKG_CNTS_ERR} / $o->{EXPOSURE} / $o->{BKG_AREA};
	}

	$o->{RAW_STD_RATE} = $o->{RAW_STD_CNTS} / $o->{EXPOSURE};
	$o->{RAW_STD_RATE_ERR} = $o->{RAW_STD_CNTS_ERR} / $o->{EXPOSURE};

# needs update
	# $o->{NET1_CNTS} = $o->{REG1_CNTS} - $o->{BKG_CNTS} * ($o->{REG1_AREA} * $o->{AS2PP});
	# $o->{NET1_CNTS_ERR} = sqrt($o->{REG1_CNTS_ERR}**2
	# 		+ ($o->{BKG_CNTS_ERR} * $o->{REG1_AREA} * $o->{AS2PP})**2);

	if ($o->{BKG_AREA} > 0) {
		$o->{NET1_CNTS} = $o->{RAW_TOT_CNTS}
				- $o->{RAW_BKG_CNTS} * ($o->{SRC_AREA} / $o->{BKG_AREA});
		$o->{NET1_CNTS_ERR} = sqrt($o->{RAW_TOT_CNTS_ERR}**2
				+ ($o->{RAW_BKG_CNTS_ERR} * $o->{SRC_AREA} / $o->{BKG_AREA}) ** 2);
	}

	if (defined($o->{NET1_CNTS})) {
		$o->{NET1_RATE} = $o->{NET1_CNTS} / $o->{EXPOSURE};
		$o->{NET1_RATE_ERR} = $o->{NET1_CNTS_ERR} / $o->{EXPOSURE};
	}

# The NET1_ values should be used ONLY to determine the
# significance of a detection which I think is the SIGDET parameter in
# the uvotsource code. They should never be passed to the
# uvotcoincidence or uvotmag functions.

	$o->{NSIGMA} = 0;
	if (defined($o->{NET1_CNTS_ERR}) and $o->{NET1_CNTS_ERR} > 0) {
		$o->{NSIGMA} = $o->{NET1_CNTS} / $o->{NET1_CNTS_ERR};
		if ($o->{NSIGMA} < 0) {
			$o->{NSIGMA} = 0;
		}
	}
}



sub processImage
{
	my ($task, $hdu, %args) = @_;

	if ($ENV{UVOT_SOURCE_XIMAGE}) {
		runXimage($task, $hdu);
	}
	else {
		runUvotinteg($task, $hdu);
	}

	performPhotometry($task, $hdu, %args);
}


sub performPhotometry
{
	my ($task, $hdu, %args) = @_;

	updateFrametimeParameters($args{COINCIDENCE}, $hdu);

	deriveRates($hdu);

	updateStandardCoincidenceLossFactors($args{COINCIDENCE}, $hdu);

	$hdu->{UNCORR_RATE} = $hdu->{RAW_STD_RATE};
	$hdu->{UNCORR_RATE_ERR} = $hdu->{RAW_STD_RATE_ERR};
	applyCoincidenceLossCorrection($hdu);

	$hdu->{UNCORR_RATE} = $hdu->{CORR_RATE};
	$hdu->{UNCORR_RATE_ERR} = $hdu->{CORR_RATE_ERR};
	if (uc($args{APERCORR}) eq 'CURVEOFGROWTH') {
		methodCurveOfGrowth($task, $args{CAL_PSF}, $hdu);
	}
	else {
		methodNoApertureCorrection($task, $hdu);
	}

	determineRateLimits($hdu, $args{NSIGMA});

	updateDetectorPosition($task, $hdu);

	$hdu->{UNCORR_RATE} = $hdu->{CORR_RATE};
	$hdu->{UNCORR_RATE_ERR} = $hdu->{CORR_RATE_ERR};
	applyLargeScaleSensitivity($task, $args{CAL_LSS}, $hdu);

	$hdu->{UNCORR_RATE} = $hdu->{CORR_RATE};
	$hdu->{UNCORR_RATE_ERR} = $hdu->{CORR_RATE_ERR};
	applyDetectorSensitivityCorrection($task, $args{CAL_SENSCORR}, $hdu);

	# divide by LSS_FACTOR, but multiply by each other factor
	$hdu->{CORR_RATE_LIMIT} = $hdu->{RAW_RATE_LIMIT} * $hdu->{COI_STD_FACTOR}
			* $hdu->{AP_FACTOR} / $hdu->{LSS_FACTOR} * $hdu->{SENSCORR_FACTOR};

	applyColorTable($task, $args{COLORTABLE}, $hdu);
}


sub methodNoApertureCorrection
{
	my ($task, $hdu) = @_;

	$hdu->{AP_FACTOR} = 1;
	$hdu->{AP_FACTOR_ERR} = 1;

	$hdu->{AP_COI_SRC_RATE} = $hdu->{UNCORR_RATE};
	$hdu->{AP_COI_SRC_RATE_ERR} = $hdu->{UNCORR_RATE_ERR};

	$hdu->{CORR_RATE} = $hdu->{AP_COI_SRC_RATE};
	$hdu->{CORR_RATE_ERR} = $hdu->{AP_COI_SRC_RATE_ERR};
}


sub methodCurveOfGrowth
{
	my ($task, $cal, $hdu) = @_;

	my $fwhmunc = $hdu->{FWHM_UNCERTAINTY_percent};
	if (not defined($fwhmunc) or $fwhmunc < 0) {
		$fwhmunc = UVOT::Calibration::DEFAULT_FWHM_UNCERTAINTY_percent;
	}

	UVOT::Calibration::primeEncircledEnergy($task, $cal, $hdu->{FILTER});
	if ($cal->{ERROR}) {
		$task->error(BAD_INPUT, $cal->{ERROR});
		return;
	}

	my $radius = sqrt($hdu->{SRC_AREA} / Math::PI);

	my %tmp = (
		RATE => $hdu->{UNCORR_RATE},
		RATE_ERR => $hdu->{UNCORR_RATE_ERR},
		APERTURE_RADIUS_arcsec => $radius,
		FWHM_UNCERTAINTY_percent => $fwhmunc,
	);

	applyEncircledEnergy($task, $cal, \%tmp);

	$hdu->{CORR_RATE} = $tmp{CORR_RATE};
	$hdu->{CORR_RATE_ERR} = $tmp{CORR_RATE_ERR};
	$hdu->{AP_COI_SRC_RATE} = $tmp{CORR_RATE};
	$hdu->{AP_COI_SRC_RATE_ERR} = $tmp{CORR_RATE_ERR};

	if (($hdu->{COI_SRC_RATE} == 0.0) || ($hdu->{COI_SRC_RATE_ERR} == 0.0)) {
		$hdu->{AP_FACTOR} = 1.0;
		$hdu->{AP_FACTOR_ERR} = 0.0;
	}
	else {
		$hdu->{AP_FACTOR} = $hdu->{AP_COI_SRC_RATE} / $hdu->{COI_SRC_RATE};
		$hdu->{AP_FACTOR_ERR} = $hdu->{AP_COI_SRC_RATE_ERR} / $hdu->{COI_SRC_RATE_ERR};
	}

}


sub applyEncircledEnergy
{
	my ($task, $fullcal, $source) = @_;

	my $radius = $source->{APERTURE_RADIUS_arcsec};
	my $cal = $fullcal->{PRIMED};

	my $last = $cal->{TABLE}[-1];

	delete($source->{FWHM_ERR_SYS});

	if ($radius > $last->{RADIUS}) {
		addWarning($source,
				sprintf('aperture radius %.2f > CALDB PSF limit %.2f',
						$radius, $last->{RADIUS}));
		$source->{ENCIRCLED} = Math::max(1, $last->{REEF});
	}
	elsif ($radius <= 0) {
		$source->{ENCIRCLED} = 1;
		$source->{FWHM_ERR_SYS} = 1.0;
		addWarning($source, 'unable to determine systematic count rate uncertainty');
	}
	else {
		my $p = $cal->{TABLE}[0];
		my $q = undef;
		foreach my $e (@{ $cal->{TABLE} }) {
			if ($e == $p) {
				# do not allow the first entry to be the 
			}
			elsif ($e->{RADIUS} >= $radius) {
				$q = $e;
				last;
			}
			$p = $e;
		}

		my $fraction = ($radius - $p->{RADIUS}) / ($q->{RADIUS} - $p->{RADIUS});

		$source->{ENCIRCLED} = $p->{REEF} + $fraction * ($q->{REEF} - $p->{REEF});

		addNote($source,
				sprintf('The two nearest PSF data points are at %.3f and %.3f arcsec',
						$p->{RADIUS}, $q->{RADIUS}),
				sprintf('with fractional encircling energy of %.1f%% and %.1f%%',
						100 * $p->{REEF}, 100 * $q->{REEF}),
				sprintf('Encircling entergy in %s at %.2f arcsec = %.1f%%',
						$source->{FILTER}, $radius, 100 * $source->{ENCIRCLED}),
				);

	}

	if (not defined($source->{FWHM_ERR_SYS})) {
		my $fwhmsig = $source->{FWHM_UNCERTAINTY_percent} / 100;

		my ($e1, $e2, $e3, $e4);
		my $p = undef;
		foreach my $e (@{ $cal->{TABLE} }) {
			$e->{RADIUS_LO} = $e->{RADIUS} / (1 + $fwhmsig);
			$e->{RADIUS_HI} = $e->{RADIUS} / (1 - $fwhmsig);

			if ($p) {

				if ($p->{RADIUS_LO} < $radius and $e->{RADIUS_LO} >= $radius) {
					$e1 = $p;
					$e2 = $e;
				}

				if ($p->{RADIUS_HI} < $radius and $e->{RADIUS_HI} >= $radius) {
					$e3 = $p;
					$e4 = $e;
				}
			}

			$p = $e;
		}

		my $encirclo = undef;
		my $encirchi = undef;

		if ($e1 and $e2) {
			my $dradlo = ($radius - $e1->{RADIUS_LO})
					/ ($e2->{RADIUS_LO} - $e1->{RADIUS_LO});
			$encirclo = $e1->{REEF} + $dradlo * ($e2->{REEF} - $e1->{REEF});
		}

		if ($e3 and $e4) {
			my $dradhi = ($radius - $e3->{RADIUS_HI})
					/ ($e4->{RADIUS_HI} - $e3->{RADIUS_HI});
			$encirchi = $e3->{REEF} + $dradhi * ($e4->{REEF} - $e3->{REEF});
		}

		if (defined($encirclo) and defined($encirchi)) {
			$source->{FWHM_ERR_SYS} = ($encirclo - $encirchi) / 2;
		}
		elsif (defined($encirclo)) {
			$source->{FWHM_ERR_SYS} = $encirclo - $source->{ENCIRCLED};
		}
		elsif (defined($encirchi)) {
			$source->{FWHM_ERR_SYS} = $source->{ENCIRCLED} - $encirchi;
		}
		else {
			$source->{FWHM_ERR_SYS} = 1.0;
			addWarning($source, 'unable to determine systematic count rate uncertainty');
		}

		if (defined($source->{FWHM_ERR_SYS})) {
			addNote($source,
					sprintf('Systematic count rate uncertainty due to time-dependent FWHM = %.1f%%',
					100 * $source->{FWHM_ERR_SYS}));
		}
	}

	{
		$source->{CORR_RATE} = $source->{RATE} / $source->{ENCIRCLED};
		$source->{CORR_RATE_ERR} = sqrt(
				($source->{RATE_ERR} / $source->{ENCIRCLED}) ** 2
				+ ($source->{FWHM_ERR_SYS} * $source->{CORR_RATE}) ** 2);
	}

}


sub addWarning
{
	my ($source, @text) = @_;
	push(@{ $source->{WARNINGS} }, @text);
}


sub addNote
{
	my ($source, @text) = @_;
	push(@{ $source->{NOTES} }, @text);
}


sub determineRateLimits
{
	my ($hdu, $nsigma) = @_;

	$hdu->{MAG_LIM_SIG} = $nsigma;

=pod
From: Frank Marshall [mailto:francis.e.marshall@nasa.gov] 
Sent: Wednesday, May 29, 2013 5:42 PM
Subject: Re: [Swift #2765] uvotsource upper limits

assume the relative uncertainty in the background is much less than the uncertainty in the source, and that the uncertainty in the counts is the square root of the counts.

Let:
c_s = counts in source region
c_b = counts in background region
r   = pixels in source region / pixels in background region
S   = SNR for a detection
w   = net counts = c_s - c_b * r

$1 is pixels in the ximage output
$2 is counts/pixel in ximage output

approximate the uncertainty in c_s as c_s**0.5 and the SNR of a detection as  w/c_s**0.5 (not including any uncertainty in the background).

So,
S = w/c_s**0.5 -> S**2 * c_s = w**2

or 

w**2 - w*S**2 - c_b*r*S**2 = 0

The formula for the S-sigma limit for the net counts is just the positive root of this quadratic equation, and the upper limit count rate is just w divided by the exposure.

---
Date: Mon, 1 Jul 2013 08:37:49 -0500
From: Frank Marshall <francis.e.marshall@nasa.gov>
To: Paul Kuin <npkuin@gmail.com>
CC: "uvot_cal@swift.psu.edu" <uvot_cal@swift.psu.edu>
Subject: Re: [Uvot_cal] uvotsource upper limits

Assuming that the uncertainty in the background is the square root of 
the number of counts and I have done the algebra correctly, we should 
replace r with (r + r*r) in the quadratic equation.
If the areas are equal (r=1), then this term doubles, but in most 
circumstances the change is small.

The new quatratic is:

	w**2 - w*S**2 - c_b*(r+r*r)*S**2 = 0
=cut

	if (not defined($hdu->{BKG_AREA}) or $hdu->{BKG_AREA} == 0) {
		return;
	}

	my $a = 1;
	my $sigma2 = $nsigma * $nsigma;
	my $b = -$sigma2;
	my $r = $hdu->{SRC_AREA} / $hdu->{BKG_AREA};
	my $c = -$hdu->{RAW_BKG_CNTS} * ($r + $r * $r) * $sigma2;
	my $posRoot = (-$b + sqrt($b*$b - 4 * $a * $c)) / (2 * $a);

	$hdu->{RAW_RATE_LIMIT} = $posRoot / $hdu->{EXPOSURE};
	$hdu->{COI_RATE_LIMIT} = $hdu->{RAW_RATE_LIMIT} * $hdu->{COI_STD_FACTOR};
}


sub log10
{
	my ($x) = @_;
	my $y = log($x) / log(10);
	return $y;
}


sub findMagFlux
{
	my ($task, $cal, $source) = @_;

	my $problem = undef;

	if (defined($source->{RATE}) and $source->{RATE} > 0) {
		$source->{MAG} = $cal->{ZPT} - 2.5 * log10($source->{RATE});
		my $statErr = 2.5 / log(10) * $source->{RATE_ERR} / $source->{RATE};
		$source->{MAG_ERR_STAT} = $statErr;

		my $sysErr = $cal->{ZPE};
		$source->{MAG_ERR_SYS} = $sysErr;

		$source->{MAG_ERR_TOTAL} = sqrt($sysErr **2 + $statErr **2);

		if ($cal->{SYS_ERR}) {
			$source->{MAG_ERR} = $source->{MAG_ERR_TOTAL};
		}
		else {
			$source->{MAG_ERR} = $source->{MAG_ERR_STAT};
		}
	}
	else {
		$source->{MAG} = $cal->{NULL};
		$source->{MAG_ERR_STAT} = $cal->{NULL};
		$source->{MAG_ERR_SYS} = $cal->{NULL};
		$source->{MAG_ERR_TOTAL} = $cal->{NULL};
		$source->{MAG_ERR} = $cal->{NULL};
		$problem = sprintf('bad rate %.1f', $source->{RATE});
	}

	{
		$source->{FLUX} = $source->{RATE} * $cal->{FCF};

		my $statErr = $source->{RATE_ERR} * $cal->{FCF};
		$source->{FLUX_ERR_STAT} = $statErr;

		my $sysErr = abs($source->{RATE}) * $cal->{FCE};
		$source->{FLUX_ERR_SYS} = $sysErr;

		$source->{FLUX_ERR_TOTAL} = sqrt($sysErr **2 + $statErr ** 2);

		if ($cal->{SYS_ERR}) {
			$source->{FLUX_ERR} = $source->{FLUX_ERR_TOTAL};
		}
		else {
			$source->{FLUX_ERR} = $source->{FLUX_ERR_STAT};
		}
	}

	return $problem;
}


sub applyColorTable
{
	my ($task, $cal, $hdu) = @_;

	if ($cal->{SYSTEMS}) {
		foreach my $system (split(',', $cal->{SYSTEMS})) {
			applyColorTableAux($task, $cal, $hdu, $system);
		}
	}
	else {
		$task->warning("magnitude systems not installed");
		applyColorTableAux($task, $cal, $hdu, 'Vega');
	}
}


sub applyColorTableAux
{
	my ($task, $cal, $hdu, $system) = @_;

	my $filter = $hdu->{FILTER};
	if (not $filter) {
		$task->warning("applyColorTable: FILTER not available");
		return;
	}

	if (not defined($system)) {
		$system = '';
	}

	UVOT::Calibration::primeColorTable($task, $cal, $filter, $system);

	my %tmp;
	$tmp{RATE} = $hdu->{CORR_RATE};
	$tmp{RATE_ERR} = $hdu->{CORR_RATE_ERR};
	findMagFlux($task, $cal, \%tmp);
	$hdu->{MAG} = $tmp{MAG};
	$hdu->{FLUX_AA} = $tmp{FLUX};
	foreach my $err (qw(ERR ERR_STAT ERR_SYS)) {
		$hdu->{qq(MAG_$err)} = $tmp{qq(MAG_$err)};
		$hdu->{qq(FLUX_AA_$err)} = $tmp{qq(FLUX_$err)};
	}

	$tmp{RATE} = $hdu->{CORR_RATE_LIMIT};
	$tmp{RATE_ERR} = 0;
	findMagFlux($task, $cal, \%tmp);
	$hdu->{MAG_LIM} = $tmp{MAG};
	$hdu->{FLUX_AA_LIM} = $tmp{FLUX};

	$tmp{RATE} = $hdu->{COI_BKG_RATE};
	$tmp{RATE_ERR} = $hdu->{COI_BKG_RATE_ERR};
	findMagFlux($task, $cal, \%tmp);
	$hdu->{MAG_BKG} = $tmp{MAG};
	$hdu->{FLUX_AA_BKG} = $tmp{FLUX};
	foreach my $err (qw(ERR ERR_STAT ERR_SYS)) {
		$hdu->{qq(MAG_BKG_$err)} = $tmp{qq(MAG_$err)};
		$hdu->{qq(FLUX_AA_BKG_$err)} = $tmp{qq(FLUX_$err)};
	}

	$tmp{RATE} = $hdu->{COI_FRAME_LIMIT};
	$tmp{RATE_ERR} = 0;
	findMagFlux($task, $cal, \%tmp);
	$hdu->{MAG_COI_LIM} = $tmp{MAG};
	$hdu->{FLUX_AA_COI_LIM} = $tmp{FLUX};


	# convert the flux densities to milliJanskys
	my $lambdaA = $cal->{WAV};
	if (not $lambdaA) {
		if ($DEFAULT_LAMBDA_A{$filter}) {
			$lambdaA = $DEFAULT_LAMBDA_A{$filter};
			$task->warning("missing wavelength for $filter filter");
		}
		else {
			$task->report("missing wavelength for $filter filter");
		}
	}

	my $freqHz = 0;
	$lambdaA |= 0;

	$hdu->{FLUX_LAMBDA_A} = $lambdaA;

	if ($lambdaA) {
		$freqHz = c_m_per_s * k_A_per_m / $lambdaA;
	}

	$hdu->{FLUX_FREQ_Hz} = $freqHz;

	# conversion factor from erg/s/cm^2/A to mJy => 1e-26 erg/s/cm^2/Hz
	# dimensional conversion: multiply by A/Hz =~ lambda/f
	# since f = c/lambda, multiply by lambda^2/c
	my $flux2mJy = 1e26 * $lambdaA**2 / (c_m_per_s * k_A_per_m);

	foreach my $id ('', qw(ERR ERR_STAT ERR_SYS BKG
			BKG_ERR_STAT BKG_ERR_SYS BKG_ERR LIM COI_LIM)) {
		my $tag = $id ? "_$id" : $id;
		$hdu->{'FLUX_HZ' . $tag} = $hdu->{'FLUX_AA' . $tag} * $flux2mJy;
	}

	if ($cal->{SYSTEM}) {

		my $prefix = $cal->{SYSTEM} . '_';

		# MAG_LIM_SIG is duplicated even though it is not system dependent
		my @keys = qw(
			MAG
			MAG_BKG
			MAG_LIM
			MAG_LIM_SIG
			MAG_COI_LIM
			FLUX_AA
			FLUX_AA_BKG
			FLUX_AA_LIM
			FLUX_AA_COI_LIM
			FLUX_HZ
			FLUX_HZ_BKG
			FLUX_HZ_LIM
			FLUX_HZ_COI_LIM
		);

		foreach my $err (qw(ERR ERR_STAT ERR_SYS)) {
			push(@keys,
					qq(MAG_$err),
					qq(MAG_BKG_$err),
					qq(FLUX_AA_$err),
					qq(FLUX_AA_BKG_$err),
					qq(FLUX_HZ_$err),
					qq(FLUX_HZ_BKG_$err),
					);
		}

		foreach my $key (@keys) {
			$hdu->{$prefix . $key} = $hdu->{$key};
		}
	}
}



sub findCoincidenceLossFactors
{
	my ($cal, $hdu, $o) = @_;

	# $cal has POLY, MULTFUNC, PLINFUNC, DEADTIME_CORRECTED
	# $o has RATE, RATE_ERR on input
	# 	COI_f, COI_ERR_f, RATE_COR, RATE_ERR_COR on output

	my $kDead = $cal->{DEADTIME_CORRECTED} ? (1 - $cal->{DEADTIME_f}) : 1;
	my $frameCounts = $o->{RATE} * $cal->{FRAMETIME_s} * $kDead;
	my $frameCountsErr = $o->{RATE_ERR} * $cal->{FRAMETIME_s} * $kDead;

	$o->{SATURATED} = 0;
	if ($frameCounts > FRAME_COUNT_LIMIT) {
		$frameCounts = FRAME_COUNT_LIMIT;
		$o->{SATURATED} = 1;
	}

	my $polyValue = $cal->{POLY}->value($frameCounts);
	if ($cal->{PLINFUNC}) {
		$polyValue = 1 / $polyValue;
	}

	my $newRate = -log(1 - $frameCounts) * $polyValue
			/ $cal->{FRAMETIME_s} / (1 - $cal->{DEADTIME_f});
	my $newRateErr = $frameCountsErr / (1 - $frameCounts) * $polyValue
			/ $cal->{FRAMETIME_s} / (1 - $cal->{DEADTIME_f});

	if ($o->{SATURATED}) {
		$newRateErr = $newRate;
	}

	if ($o->{RATE} > 0) {
		$o->{COI_f} = $newRate / $o->{RATE};
		$o->{COI_ERR_f} = $newRateErr / $o->{RATE};
	}
	elsif ($o->{RATE} < 0) {
		$o->{COI_f} = 1;
		$o->{COI_ERR_f} = 0;
		print "warning: non-physical input supplied, rates must be non-negative\n";
	}
	else {
		$o->{COI_f} = 1;
		$o->{COI_ERR_f} = 0;
	}

	if ($o->{RATE_ERR} > 0) {
		$o->{COI_ERR_ERR_f} = $newRateErr / $o->{RATE_ERR};
	}
	else {
		$o->{COI_ERR_ERR_f} = 1;
	}
}


sub updateStandardCoincidenceLossFactors
{
	my ($cal, $o) = @_;
	my %tmp;

	$tmp{RATE} = $o->{RAW_STD_RATE};
	$tmp{RATE_ERR} = $o->{RAW_STD_RATE_ERR};
	findCoincidenceLossFactors($cal, $o, \%tmp);
	$o->{COI_STD_FACTOR} = $tmp{COI_f};
	$o->{COI_STD_FACTOR_ERR} = $tmp{COI_ERR_f};
	$o->{COI_STD_ERR_FACTOR} = $tmp{COI_ERR_ERR_f};
	$o->{SATURATED} = $tmp{SATURATED};

	$tmp{RATE} = $o->{RAW_BKG_RATE} * $o->{STD_AREA};
	$tmp{RATE_ERR} = $o->{RAW_BKG_RATE_ERR} * $o->{STD_AREA};
	findCoincidenceLossFactors($cal, $o, \%tmp);
	$o->{COI_BKG_FACTOR} = $tmp{COI_f};
	$o->{COI_BKG_FACTOR_ERR} = $tmp{COI_ERR_f};
	$o->{COI_BKG_ERR_FACTOR} = $tmp{COI_ERR_ERR_f};

	# frame rate limited count rate
	my $frameLimit = 1 / $cal->{FRAMETIME_s};
	$tmp{RATE} = $frameLimit;
	$tmp{RATE_ERR} = 0;
	findCoincidenceLossFactors($cal, $o, \%tmp);
	$o->{COI_STD_LIM_FACTOR} = $tmp{COI_f};
	$o->{COI_STD_LIM_FACTOR_ERR} = $tmp{COI_ERR_f};
	$o->{RAW_FRAME_LIMIT} = $frameLimit;
	$o->{COI_FRAME_LIMIT} = $frameLimit * $o->{COI_STD_LIM_FACTOR};

}


sub applyCoincidenceLossCorrection
{
	my ($o) = @_;

	$o->{COI_TOT_RATE} = $o->{COI_STD_FACTOR} * $o->{RAW_TOT_RATE};
	$o->{COI_TOT_RATE_ERR} = $o->{COI_STD_ERR_FACTOR} * $o->{RAW_TOT_RATE_ERR};

	$o->{COI_BKG_RATE} = $o->{COI_BKG_FACTOR} * $o->{RAW_BKG_RATE};
	$o->{COI_BKG_RATE_ERR} = $o->{COI_BKG_ERR_FACTOR} * $o->{RAW_BKG_RATE_ERR};

	$o->{COI_SRC_RATE} = $o->{COI_TOT_RATE}
			- $o->{COI_BKG_RATE} * $o->{SRC_AREA};
	$o->{COI_SRC_RATE_ERR} = sqrt($o->{COI_TOT_RATE_ERR} ** 2
			+ ($o->{COI_BKG_RATE_ERR} * $o->{SRC_AREA}) ** 2);

	$o->{CORR_RATE} = $o->{COI_SRC_RATE};
	$o->{CORR_RATE_ERR} = $o->{COI_SRC_RATE_ERR};
}


sub getWCSAux
{
	my ($task, $header, $suffix) = @_;

	my $wcs = undef;

	my $key = "_cache:WCS$suffix";
	if ($header->{$key}) {
		$wcs = $header->{$key};
	}
	else {
		my @suffix = $suffix ? (suffix => $suffix) : ();
		$wcs = $task->getWCS($header, @suffix, quiet => 1);
		if ($wcs->{TYPE}) {
			$header->{$key} = $wcs;
		}
	}

	return $wcs;
}


sub updateDetectorPosition
{
	my ($task, $hdu, $href) = @_;

	my $type = $hdu->{XY_TYPE};
	my $detx = undef;
	my $dety = undef;

	my ($mm, $mx, $my);

	if ($type eq 'NONE') {
	}
	elsif ($type eq 'DET') {
		# nothing to do
		$detx = $hdu->{X};
		$dety = $hdu->{Y};
	}
	elsif ($type eq 'MM') {
		$mx = $hdu->{X};
		$my = $hdu->{Y};
		$mm = 1;
	}
	elsif ($type eq 'IMAGE') {
		my $detwcs = getWCSAux($task, $hdu->{HEADER}, 'D');
		if ($detwcs->{TYPE}) {
			($mx, $my) = $task->pixToWorld($detwcs, $hdu->{X}, $hdu->{Y});
			$mm = 1;
		}
		else {
			$task->warning("updateDetectorPosition: unable to get WCS D from header");
		}
	}
	elsif ($type eq 'RADEC') {
		# convert RA,DEC to FITS then FITS to DET
		my $skywcs = getWCSAux($task, $hdu->{HEADER});
		my $detwcs = getWCSAux($task, $hdu->{HEADER}, 'D');
		if ($skywcs->{TYPE} and $detwcs->{TYPE}) {
			my ($px, $py) = $task->worldToPix($skywcs, $hdu->{X}, $hdu->{Y});
			($mx, $my) = $task->pixToWorld($detwcs, $px, $py);
			$mm = 1;
		}
	}
	else {
		$task->warning("updateDetectorPosition: unknown XY_TYPE '$type'");
	}

	if ($mm) {
		$detx = $mx / 0.009075 + 1100.5;
		$dety = $my / 0.009075 + 1100.5;
	}

	$hdu->{DETX} = $detx;
	$hdu->{DETY} = $dety;
}


sub applyLargeScaleSensitivity
{
	my ($task, $cal, $hdu) = @_;

	# initialize for no correction
	$hdu->{LSS_FACTOR} = 1.0;

	if (not $cal) {
		$task->warning('applyLargeScaleSensitivity: no calibration loaded');
	}
	elsif ($cal->{NONE}) {
		# skip LSS correction
	}
	elsif (not $cal->{FITS}) {
		$task->warning('applyLargeScaleSensitivity: no calibration data');
	}
	elsif ($cal->{SKYLSS}) {
		$task->verbose('applyLargeScaleSensitivity: taking LSS from sky image');
		if (not defined($hdu->{RA}) or not defined($hdu->{DEC})) {
			$task->warning('applyLargeScaleSensitivity: unknown RA/DEC');
		}
		else {
			# look up the image value of the SKYLSS pixel corresponding to RA/DEC
			my $fits = $cal->{FITS};
			my $status;

			my $header = $cal->{FITS_HEADER};
			if (not $header) {
				$status = $fits->readheader($header)->status;
				$cal->{FITS_HEADER} = $header;
			}

			my $skywcs = getWCSAux($task, $cal->{FITS_HEADER});
			my $naxis = $header->{NAXIS} || 0;
			my $naxis1 = $header->{NAXIS1} || 0;
			my $naxis2 = $header->{NAXIS2} || 0;
			if ($naxis == 2 and $naxis1 > 0 and $skywcs->{TYPE}) {
				my ($px, $py) = $task->worldToPix($skywcs, $hdu->{RA}, $hdu->{DEC});
				$task->verbose(sprintf('applyLargeScaleSensitivity: RA=%.6f, DEC=%.6f => X=%.1f, Y=%.1f',
						$hdu->{RA}, $hdu->{DEC}, $px, $py));
				my $prefix = 'setting LSS factor 1';
				if ($px < 0 or $px > $naxis1+1 or $py < 0 or $py > $naxis2) {
					$task->warning("$prefix- outside of LSS map");
				}
				else {
					my $fptr = $fits->handle;
					my $anynull = 0;
					my $npixels = 1;
					my $data = [ 0 ];
					my $offset = int($px+0.5) + int($py+0.5) * $naxis1;
					$fptr->read_img_flt(1, $offset, $npixels, -1, $data, $anynull, $status);
					my $value = $data->[0];

					if ($status or $anynull) {
						$task->warning("$prefix- unable to load SKYLSS at offset $offset [$status]");
					}
					elsif ($value > 0) {
						$hdu->{LSS_FACTOR} = $value;
					}
					else {
						$task->warning("$prefix- read non-positive value $value");
					}
				}
			}
			else {
				$task->warning('applyLargeScaleSensitivity: unable to determine sky WCS');
			}
		}
	}
	elsif (not defined($hdu->{DETX}) or $hdu->{DETX} =~ /^-?NaN$/i) {
		$task->warning('applyLargeScaleSensitivity: unknown detector position');
	}
	else {
		my ($rawx, $rawy) = UVOT::Calibration::estimateRAWfromDET(
				$hdu->{DETX}, $hdu->{DETY});
		my $rawx0 = Math::max(0, Math::min(2047, $rawx));
		my $rawy0 = Math::max(0, Math::min(2047, $rawy));
		if ($rawx0 != $rawx or $rawy0 != $rawy) {
			$task->warning(sprintf('truncated RAW position %.2f,%.2f => %.2f,%.2f',
					$rawx, $rawy, $rawx0, $rawy0));
		}
		$rawx = $rawx0;
		$rawy = $rawy0;
		if ($task->chatter(4)) {
			$task->verbose(sprintf('DET %.2f,%.2f => RAW %.2f,%.2f',
					$hdu->{DETX}, $hdu->{DETY}, $rawx, $rawy));
		}

		my $fits = $cal->{FITS};
		my $extname = "LSSENS$hdu->{FILTER}";
		my $status = $fits->move($extname)->status;
		delete($cal->{FITS_HEADER});
		if ($status) {
			$task->warning("applyLargeScaleSensitivity: unable to move to $extname [$status]");
		}
		else {
			my $fptr = $fits->handle;
			my $anynull = 0;
			my $npixels = 1;
			my $data = [ 0 ];
			my $offset = int($rawx+0.5) + int($rawy+0.5) * 2048;
			$fptr->read_img_flt(1, $offset, $npixels, -1, $data, $anynull, $status);
			if ($status or $anynull) {
				$task->warning("unable to load LSS at offset $offset [$status]");
			}
			else {
				$hdu->{LSS_FACTOR} = $data->[0];
			}
		}
	}

	$hdu->{LSS_RATE} = $hdu->{UNCORR_RATE} / $hdu->{LSS_FACTOR};
	$hdu->{LSS_RATE_ERR} = $hdu->{UNCORR_RATE_ERR} / $hdu->{LSS_FACTOR};

	$hdu->{CORR_RATE} = $hdu->{LSS_RATE};
	$hdu->{CORR_RATE_ERR} = $hdu->{LSS_RATE_ERR};
}


sub applyDetectorSensitivityCorrection
{
	my ($task, $cal, $hdu) = @_;

	# initialize for no correction
	$hdu->{SENSCORR_FACTOR} = 1.0;

	if (not $cal) {
		$task->warning('applyDetectorSensitivity: no calibration defined');
	}
	elsif ($cal->{NONE}) {
		# skip detector sensitivity correction
	}
	else {

		UVOT::Calibration::primeDetectorSensitivity($task, $cal, $hdu->{FILTER});
		if ($cal->{ERROR}) {
			$task->error(BAD_INPUT, $cal->{ERROR});
			return;
		}

		my $primed = $cal->{PRIMED};

		# see notes from Wayne Landsman 2010 Jan 4, and SENSCORR calibration
		# file COMMENT keywords.

		my $tMiddle = ($hdu->{TSTART} + $hdu->{TSTOP}) / 2;

		my $select = undef;

		# find the latest record with TIME <= $tMiddle
		foreach my $e (@{ $primed->{TABLE} }) {
			if ($e->{TIME} <= $tMiddle) {
				if (not defined($select)) {
					$select = $e;
				}
				elsif ($e->{TIME} > $select->{TIME}) {
					$select = $e;
				}
			}
		}

		if (not defined($select)) {
			$task->warning("applyDetectorSensitivity: TMIDDLE=$tMiddle precedes earliest calibration");
		}
		else {
			my $exponent = ($tMiddle - $select->{TIME}) / DURATION_YEAR_s;
			$hdu->{SENSCORR_FACTOR} = (1 + $select->{OFFSET})
					* ((1 + $select->{SLOPE}) ** $exponent);
		}
	}

	$hdu->{SENSCORR_RATE} = $hdu->{UNCORR_RATE} * $hdu->{SENSCORR_FACTOR};
	$hdu->{SENSCORR_RATE_ERR} = $hdu->{UNCORR_RATE_ERR} * $hdu->{SENSCORR_FACTOR};

	$hdu->{CORR_RATE} = $hdu->{SENSCORR_RATE};
	$hdu->{CORR_RATE_ERR} = $hdu->{SENSCORR_RATE_ERR};
}


1;

