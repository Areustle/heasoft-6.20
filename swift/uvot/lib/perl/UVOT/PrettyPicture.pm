# $Source: /headas/headas/swift/uvot/lib/perl/UVOT/PrettyPicture.pm,v $
# $Revision: 1.4 $
# $Date: 2004/06/17 21:18:26 $
#
# $Log: PrettyPicture.pm,v $
# Revision 1.4  2004/06/17 21:18:26  rwiegand
# No longer using UVOT::Convert for converting between pixels/degrees.
#
# Revision 1.3  2004/03/04 16:03:41  rwiegand
# Updated to use ds9 v3 instead of perl Imager module.
#
# Revision 1.2  2003/12/15 22:04:17  rwiegand
# Load Imager modules at run time.  Added descriptive text to image.
#
# Revision 1.1  2003/11/26 20:00:19  rwiegand
# Modules for sharing among UVOT tasks.
#

use strict;

package UVOT::PrettyPicture;
use base qw(Task::FITS);
use Task qw(:codes);

use Math;
use Astro::FITS::CFITSIO qw(:constants :longnames);


# most of these actually have defaults, but require them for now
my @REQUIRED_KEYWORDS = qw(NAXIS1 NAXIS2 CTYPE1 CTYPE2
	CRPIX1 CRPIX2 CRVAL1 CRVAL2 CDELT1 CDELT2);
my %REQUIRED_KEYWORDS = map { $_ => 1 } @REQUIRED_KEYWORDS;


sub execute
{
	my ($self) = @_;

	$self->initialize;

	$self->grabKeywords
		if $self->isValid;

	$self->createTestSources
		if $self->isValid and $self->{testSources};

	$self->createDetailedImage
		if $self->isValid;

	$self->finalize;
}


sub initialize
{
	my ($self) = @_;

	# make sure we have all the fields

	foreach my $key (qw(infile outfile RA DEC)) {
		if (not $self->{$key}) {
			$self->error(BAD_TASK,
				"missing required field $key");
		}
	}

	$self->{xrtError} = 5 / 3600;   # 5 arcsec
	$self->{batError} = 2 / 60;     # 1-4 arcmin

	foreach my $key (qw(RA DEC)) {
		$self->{$key . '_d'} = Math::radiansToDegrees($self->{$key});
	}

	$self->{cleanup} ||= 'no';
}


sub grabKeywords
{
	my ($self) = @_;

	my $path = $self->{infile};
	my $status = 0;
	my $fits = Astro::FITS::CFITSIO::open_file($path, READONLY, $status);
	if (not $fits) {
		$self->error(BAD_INPUT,
			"unable to open $path [$status]");
		return;
	}

	my $header = $fits->read_header;

	foreach my $key (@REQUIRED_KEYWORDS, qw(CUNIT1 CUNIT2 CROTA2)) {
		if (exists($header->{$key})) {
			my $value = $header->{$key};
			if ($value =~ /^'(.+?)'$/) {
				$value = $1;
			}
			$value =~ s/^\s+//;
			$value =~ s/\s+$//;
			$self->{$key} = $value;
		}
		elsif ($REQUIRED_KEYWORDS{$key}) {
			$self->error(BAD_INPUT,
				"$path missing keyword $key");
		}
	}

	$self->{CUNIT1} ||= 'deg';
	$self->{CUNIT2} ||= 'deg';

	$self->{CROTA2} ||= 0;

	if ($self->{CTYPE1} ne 'RA---TAN' or $self->{CTYPE2} ne 'DEC--TAN') {
		$self->error(BAD_INPUT,
			"bad image CTYPEs [$self->{CTYPE1}/$self->{CTYPE2}]");
	}
	else {
		$self->{TYPE} = '-TAN';
	}

	if ($self->{CUNIT1} ne 'deg' or $self->{CUNIT2} ne 'deg') {
		$self->error(BAD_INPUT,
			"bad image CUNITs [$self->{CUNIT1}/$self->{CUNIT2}]");
	}

	$fits->close_file($status);
}


sub createDetailedImage
{
    my ($self) = @_;

    my $regions = $self->temporary('pretty', ext => '.reg');

    my $fh = FileHandle->new($regions, 'w');
    if (not $fh) {
        $self->error(BAD_OUTPUT,
            "unable to create regions file $regions: $!");
        return;
    }

    $fh->print(q(# Region file format: DS9 version 3.0
global select=1 edit=1 move=1 delete=1 include=1 fixed=0 source
));

	my ($x, $y) = $self->rd2xy($self->{RA}, $self->{DEC});
	my $r1 = $self->convertDegreesToPixels($self->{xrtError});

	my $r2 = $self->convertDegreesToPixels($self->{batError});

    # error circles
    $fh->print("global color=green\n");
    $fh->print("image;circle($x,$y,$r1)\n");
    $fh->print("image;circle($x,$y,$r2)\n");

    # blue crosses for matches
    $fh->print("global color=blue\n");

	foreach my $s (@{ $self->{sources} }) {

		next if not $s->{MATCHED};

		my ($x, $y) = $self->rd2xy($s->{RA}, $s->{DEC});

		my $n = 3;

		# + shaped cross
        my $y1 = $y - $n;
        my $y2 = $y + $n - 1;
		$fh->print("image;line($x,$y1,$x,$y2) # line=0 0\n");

        my $x1 = $x - $n;
        my $x2 = $x + $n - 1;
		$fh->print("image;line($x1,$y,$x2,$y) # line=0 0\n");
	}

	# put a cross in the lower left hand corner
	# $fh->print("image;line(3,1,3,5) # line=0 0\n");
	# $fh->print("image;line(1,3,5,3) # line=0 0\n");
	# $fh->print("image;line(3.5,1.5,3.5,5.5)\n");
	# $fh->print("image;line(1.5,3.5,5.5,3.5)\n");

    # label directions
    $fh->print("global color=black\n");
	$fh->print(qq(global font="helvetica 16 bold"\n));
	$fh->print("image;text(-30,150) # text={E}\n");
	$fh->print("image;text(150,330) # text={N}\n");

    # information
	$fh->print(qq(global font="helvetica 12 normal"\n));
    my $xbase = 350;
    my $xindent = 400;
    $y = 300;
    my $dy = -20;

    # scale
	$fh->print("image;text($xbase,$y) # text={scale}\n");
    my $x1 = $xbase + 50;
    my $x2 = $x1 + $self->convertDegreesToPixels(1 / 60);
    my $y1 = $y;
    my $y2 = $y + 10;
    my $ym = ($y1 + $y2) / 2;
	$fh->print("image;line($x1,$y1,$x1,$y2) # line=0 0\n");
	$fh->print("image;line($x2,$y1,$x2,$y2) # line=0 0\n");
	$fh->print("image;line($x1,$ym,$x2,$ym) # line=0 0\n");
    $y += $dy;
	$fh->print("image;text($xindent,$y) # text={1 arcmin}\n");
    $y += 2 * $dy;

    # center
	$fh->print("image;text($xbase,$y) # text={center}\n");
    $y += $dy;
    my $center = sprintf('%.3f %+.3f', $self->{RA_d}, $self->{DEC_d});
	$fh->print("image;text($xindent,$y) # text={$center}\n");
    $y += 2 * $dy;

    # circles
	$fh->print("image;text($xbase,$y) # text={circles}\n");
    $y += $dy;
	$fh->print("image;text($xindent,$y) # text={XRT 5 arcsec}\n");
    $y += $dy;
	$fh->print("image;text($xindent,$y) # text={BAT 2 arcmin}\n");
    $y += 2 * $dy;

    $fh->close;

    my $command = "ds9 -file $self->{infile} -invert "
            . "-region $regions -saveas png $self->{outfile} -exit";

    my $result = $self->runCommand($command);
}


sub wcsRand
{
	my ($self, $n) = @_;

	my $naxis = $self->{'NAXIS' . $n};
	my $crpix = $self->{'CRPIX' . $n};
	my $crval = $self->{'CRVAL' . $n};
	my $cdelt = $self->{'CDELT' . $n};

	my $degrees = $crval + (rand() * $naxis - $crpix) * $cdelt;
	my $radians = Math::degreesToRadians($degrees);

	return $radians;
}


sub createTestSources
{
	my ($self) = @_;

	my @sources;
	for (my $i = 0; $i < $self->{testSources}; ++$i) {
		push(@sources, {
				RA => $self->wcsRand(1),
				DEC => $self->wcsRand(2),
			});
	}
	$self->{sources} = \@sources;
}


sub convertDegreesToPixels
{
	my ($self, $deg) = @_;

	my $pixels = abs($deg / $self->{CDELT1});
	# was $deg * UVOT::Convert::PLATE_SCALE_d / $self->{binning};

	return $pixels;
}


sub rd2xy
{
	my ($self, $ra, $dec) = @_;

	my $rad = Math::radiansToDegrees($ra);
	my $decd = Math::radiansToDegrees($dec);

	my ($xpix, $ypix);

	my $status = 0;
	Astro::FITS::CFITSIO::fits_world_to_pix(
		$rad, $decd,
		$self->{CRVAL1}, $self->{CRVAL2},
		$self->{CRPIX1}, $self->{CRPIX2},
		$self->{CDELT1}, $self->{CDELT2},
		$self->{CROTA2}, $self->{TYPE},
		$xpix, $ypix,
		$status,
		);

	# offset for ds9 drawing
	$xpix += 0.5;
	$ypix += 0.5;

	$self->report(sprintf('ra %.2f dec %.2f => %.2f %.2f',
		$rad, $decd, $xpix, $ypix));

	return ($xpix, $ypix);
}


sub finalize
{
	my ($self) = @_;

	return if $self->{cleanup} ne 'yes';

}


1;

