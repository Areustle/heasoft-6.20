
# $Source: /headas/headas/swift/gen/lib/perl/Astro/Julian.pm,v $
# $Revision: 1.1 $
# $Date: 2002/09/30 18:35:53 $
#
#	Conversion between Gregorian and Julian dates.
#
# $Log: Julian.pm,v $
# Revision 1.1  2002/09/30 18:35:53  wiegand
# Initial revision
#

use strict;

package Astro::Julian;

use base qw(Exporter);

@Astro::Julian::EXPORT_OK = qw(
	ymdToJulian ydToJulian
	julianToYMD
);

%Astro::Julian::EXPORT_TAGS = (

	functions => [ qw(ymdToJulian ydToJulian
		julianToYMD) ],
);




sub ymdToJulian
{
	my ($year, $month, $day, $fraction) = @_;

use integer;
	$fraction ||= 0;

	my $a = (14 - $month) / 12;
	my $y = $year + 4800 - $a;
	my $m = $month + 12 * $a - 3;

	my $jdn = $day + (153 * $m + 2) / 5
		+ $y * 365 + $y / 4 - $y / 100 + $y / 400 - 32045;

no integer;
	my $julian = $jdn + $fraction - 0.5;

	return $julian;
}



sub julianToYMD
{
	my ($jd) = @_;

	my $jdp = $jd + 0.5;

use integer;
	my ($L, $n, $i, $j);

	$L = $jdp + 68569;
	$n = 4 * $L / 146097;
	$L = $L - (146097 * $n + 3) / 4;
	$i = (4000 * ( $L + 1) ) / 1461001;
	$L = $L - (1461 * $i) / 4 + 31;
	$j = (80 * $L) / 2447;

	my $day = $L - (2447 * $j) / 80;
	$L = $j / 11;
	my $month = $j + 2 - 12 * $L;
	my $year = 100 * ($n - 49) + $i + 1;

no integer;
	my $fraction = $jdp - int($jdp);

	return ($year, $month, $day, $fraction);
}


sub ydToJulian
{
	my ($year, $doy, $fraction) = @_;

	my $jd0 = ymdToJulian($year, 1, 1, $fraction);

	my $julian = $jd0 + $doy - 1;

	return $julian;
}


1;

