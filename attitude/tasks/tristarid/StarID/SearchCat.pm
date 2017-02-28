# $Source: /headas/headas/attitude/tasks/tristarid/StarID/SearchCat.pm,v $
# $Revision: 1.9 $
# $Date: 2008/09/12 20:16:22 $
#
#	Use the WCS tool scat (search catalog) program to load sources.
#
#	Currently set up for USNO B1 catalog, but could support others.
#
#
# $Log: SearchCat.pm,v $
# Revision 1.9  2008/09/12 20:16:22  rwiegand
# Implemented data type scat for better support of multiple magnitudes.
#
# Revision 1.8  2006/03/06 16:13:54  rwiegand
# Allow control of scat sort.
#
# Revision 1.7  2006/02/08 17:42:57  rwiegand
# Populate UNIT field of source objects.
#
# Revision 1.6  2006/01/25 15:23:46  rwiegand
# Extended SearchCat module to support many scat catalag interfaces.  Allow
# user to pass filter and epoch into catalog loading.  Use Wayne's magnitude
# estimation for UVOT filters when loading USNO B1.
#
# Revision 1.5  2005/10/31 13:56:10  rwiegand
# Renamed the catalog partition paramater to catspec.  This removes the
# need for a dummy directory when the actual data is not even local.
#
# Revision 1.4  2005/10/12 13:35:51  rwiegand
# Ignore the N (infra-red) magnitude.
#
# Revision 1.3  2005/10/10 12:09:26  rwiegand
# Updated magnitude estimation.
#
# Revision 1.2  2005/10/09 22:39:59  rwiegand
# Sort scat results by magnitude.  Select the first valid magnitude.
#
# Revision 1.1  2005/08/29 11:56:23  rwiegand
# Support using WCS Tools scat to access catalogs.
#

use strict;

package StarID::SearchCat;
use base qw(StarID::Partition);
use Task qw(:codes);

use FileHandle;

use Math;
use Math::Polynomial;


my %GSC_CLASS_STRING = (
	0 => 'star',
	1 => 'galaxy',
	2 => 'blend',
	3 => 'non-star',
	4 => 'unclassified',
	5 => 'artifact',
);


# see
#	Subject: Re: Recipe for predicting mags from USNO catalog
#	From: Wayne Landsman <landsman@sampa.gsfc.nasa.gov>
#	Date: Tue, 13 Dec 2005 09:24:31 -0500
# estimate V = 0.444*b1 + 0.556*r1
#			B-V = 0.556*(b1 - r1) 
# let bvhat = -0.33, if (B-V) < -0.33
#              1.23, if (B-V) > 1.23
#              B-V, otherwise
# estimate filter mag = V + poly(bvhat)
my %USNOB1_MAG_POLY = (
  U    => Math::Polynomial->new(0.268329, 2.80589, -5.15155, 6.16753, -1.88971),
  UVM2 => Math::Polynomial->new(0.401748, 6.48230, -4.52295, 10.4968, -5.46192),
  UVW1 => Math::Polynomial->new(0.419600, 5.41212, -6.18773, 6.66283, -2.16846),
  UVW2 => Math::Polynomial->new(0.402177, 7.56133, -4.89295, 4.60327, -2.15464),
);


sub platePath
{
	my ($self, $plate) = @_;
	return $plate->{path};
}


sub getPlate
{
	my ($self, $spec) = @_;

	my $task = $self->{task};
	$self->{bounds} = $spec;

	my $raDeg = Math::toDegrees($spec->ra);
	my $decDeg = Math::toDegrees($spec->dec);
	my $radiusAS = 3600 * Math::toDegrees($spec->{radius});
	$radiusAS = sprintf('%.1f', $radiusAS + 0.05);

	if (not $self->{years2000}) {
		if ($spec->{epoch}) {
			$self->{years2000} = $spec->{epoch};
		}
		else {
			my @t = gmtime;
			$self->{years2000} = (1900 + $t[5]) + $t[7] / 365;
		}
	}

	my $scout = $task->temporary('scout');

	my $subtype = $self->{subtype} || 'ub1';
	my $sort = $self->{sort} || 'm';

	$ENV{$self->{envvar}} = $self->{location};
	my $command = 'scat'
			. " -c $subtype"	# which catalog to search
			. " -s $sort"	# sort control
			. ' -d'     # report RA,Dec in degrees
			. ' -j'     # always output J2000
			. " -n $self->{limit}"  # limit to brightest <limit> objects
			. " -r $radiusAS"  # set radius of search
						# set epoch of returned positions
			. sprintf(' -y %.3f', $self->{years2000}) 
			. sprintf(' %.4f', $raDeg)
			. sprintf(' %.4f', $decDeg)
			. ' J2000'
			;
	$task->shell("$command > $scout 2>&1");

	my $plate = StarID::Partition::Plate->new(
				id => 'dynamic',
				spec => $spec,
				scout => $scout,
				);

	if (my $subtype = $self->{subtype}) {
		return $self->getPlateSubtype($plate, $subtype);
	}
	else {
		return $self->getPlateOld($plate);
	}
}



sub getPlateOld
{
	my ($self, $plate) = @_;

	my $task = $self->{task};
	my $in = FileHandle->new($plate->{scout});
	if (not $in) {
		$task->error(BAD_INPUT, "unable to open $plate->{scout} [$!]");
	}

	my $catalog = $task->temporary('catalog');
	my $out = FileHandle->new($catalog, 'w');
	if (not $in) {
		$task->error(BAD_OUTPUT, "unable to create $catalog [$!]");
	}

	if ($task->isValid) {

		while (<$in>) {
			if (/^\d+/) {
				# looks like a record
				my ($id, $raDeg, $decDeg,
						$magB1, $magR1, $magB2, $magR2, $magN,
						$pm, $ni, $sg, $dist) = split;

				my $ra = Math::toRadians($raDeg);
				my $dec = Math::toRadians($decDeg);

				my $magR = usnob1AverageMag($magR1, $magR2);
				my $magB = usnob1AverageMag($magB1, $magB2);
				my $mag = 'NULL';
				if ($magR < 90 and $magB < 90) {
# http://www.aerith.net/astro/color_conversion/JG/USNO-B1.0.html.
#   V = 0.444B1 + 0.556R1 +/- 0.5 = R1 + 0.444(B1-R1)) 
					$mag = 0.444 * $magB + 0.556 * $magR;
				}
				elsif ($magR < 90) {
					$mag = $magR;
				}
				else {
					foreach my $tmp ($magR1, $magR2, $magB1, $magB2) {
						if ($tmp < 90) {
							$mag = $tmp;
							last;
						}
					}
				}
				$out->print("ID=$id, RA=$ra, DEC=$dec, MAG=$mag, TYPE=NULL\n");
			}
#			elsif (/\S/) {
#				chomp;
#				$task->report($_);
#			}
		}
		undef($in);
		$out->close;

		# reformat UB1 to Plate format

		$plate->{path} = $catalog;
	}
	else {
		$plate = undef;
	}

	return $plate;
}



sub getPlateSubtype
{
	my ($self, $plate, $subtype) = @_;

	my $task = $self->{task};

	my $in = FileHandle->new($plate->{scout});
	if (not $in) {
		$task->error(BAD_INPUT, "unable to open $plate->{scout} [$!]");
	}

	my @label = split(',', $self->{fields});
	my $nfields = @label;

	my @contents;

	my $resolve = "resolve$self->{data}";
	if (not $self->can($resolve)) {
		$task->warning("unable to $resolve");
		$resolve = 'resolveSimple';
	}

	if ($task->isValid) {

		while (<$in>) {

			my @field = split(' ');

			if (@field == $nfields) {

				my %data = map { $label[$_] => $field[$_] } (0 .. $nfields - 1);

				if (my $object = $self->$resolve(\%data)) {
					push(@contents, $object);
				}
			}
		}

		undef($in);

		$plate->{path} = $plate->{scout};
	}

	$plate->{contents} = \@contents;

	return $plate;
}



sub resolveSimple
{
	my ($raw) = @_;
	bless($raw, 'StarID::Source');
}


sub resolveDEFAULT
{
	my ($self, $href) = @_;
	$href->{TYPE} = 'UNKNOWN';
	my $object = bless($href, 'StarID::Source');
}


sub resolveGSC2
{
	my ($self, $href) = @_;
	$href->{RA} = Math::toRadians($href->{RA});
	$href->{DEC} = Math::toRadians($href->{DEC});
	$href->{UNIT} = Math::rd2unit($href->{RA}, $href->{DEC});
	$href->{MAG_KEYS} = 'MAGF,MAGJ,MAGV,MAGN';
	$href->{MAG} = $href->{MAGJ};
	$href->{TYPE} = $GSC_CLASS_STRING{$href->{CLASS}} || 'UNKNOWN';
	my $object = bless($href, 'StarID::Source');
}


sub resolveSCAT
{
	my ($self, $href) = @_;
	$href->{RA} = Math::toRadians($href->{RA});
	$href->{DEC} = Math::toRadians($href->{DEC});
	$href->{UNIT} = Math::rd2unit($href->{RA}, $href->{DEC});
	my $magKey = $self->{mag} || 'MAG';
	$href->{MAG} = $href->{$magKey};
	if (not $href->{TYPE}) {
		$href->{TYPE} = 'UNKNOWN';
	}
	my $object = bless($href, 'StarID::Source');
}


sub usnob1AverageMag
{
	my ($mag1, $mag2) = @_;
	if ($mag1 < 90 and $mag2 < 90) {
		return ($mag1 + $mag2) / 2;
	}
	elsif ($mag1 < 90) {
		return $mag1;
	}
	else {
		return $mag2;
	}
}


sub resolveUSNOB1
{
	my ($self, $href) = @_;

	my $bounds = $self->{bounds} || { };

	$href->{RA} = Math::toRadians($href->{RA});
	$href->{DEC} = Math::toRadians($href->{DEC});

	$href->{UNIT} = Math::rd2unit($href->{RA}, $href->{DEC});

	$href->{MAG_KEYS} = 'MAGR1,MAGR2,MAGB1,MAGB2,MAGN';

	my $avgR = usnob1AverageMag($href->{MAGR1}, $href->{MAGR2});
	my $avgB = usnob1AverageMag($href->{MAGB1}, $href->{MAGB2});

	my ($estMag, $estV, $estB);

	# prefer plate 1 mags
	if ($href->{MAGR1} < 90 and $href->{MAGB1} < 90) {
		$estV = 0.444 * $href->{MAGB1} + 0.556 * $href->{MAGR1};
		$estB = $href->{MAGB1};
	}
	elsif ($avgR < 90 and $avgB < 90) {
		$estV = 0.444 * $avgB + 0.556 * $avgR;
		$estB = $avgB;
	}

	if (my $filter = $bounds->{filter} || $bounds->{FILTER}) {
		if (my $poly = $USNOB1_MAG_POLY{$filter}) {
			if ($estV and $estB) {
				my $estB_V = $estB - $estV;
				if ($estB_V < -0.33) {
					$estB_V = -0.33;
				}
				elsif ($estB_V > 1.23) {
					$estB_V = 1.23;
				}
				$estMag = $estV + $poly->value($estB_V);
			}
		}
		elsif ($filter eq 'B') {
			$estMag = $estB;
		}
		elsif ($filter eq 'R') {
			$estMag = $avgR;
		}
	}

	if (not defined($estMag)) {
		$estMag = 99;
		if ($estV) {
			# use the V estimate if nothing better is available
			$estMag = $estV;
		}
		else {
			foreach my $key (qw(MAGR1 MAGR2 MAGB1 MAGB2)) {
				if (my $mag = $href->{$key}) {
					if ($mag < 90) {
						$estMag = $mag;
						last;
					}
				}
			}
		}
	}

	$href->{MAG} = $estMag;

	$href->{TYPE} = 'NULL';

	my $object = bless($href, 'StarID::Source');
}



1;

