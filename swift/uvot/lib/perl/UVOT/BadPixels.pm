# $Revision: 1.3 $
# $Source: /headas/headas/swift/uvot/lib/perl/UVOT/BadPixels.pm,v $
# $Date: 2004/07/09 15:56:31 $
#
# $Log: BadPixels.pm,v $
# Revision 1.3  2004/07/09 15:56:31  rwiegand
# Renamed column in bad pixel list table.
#
# Revision 1.2  2004/04/30 14:57:36  rwiegand
# Updated bad pixel table extension name.
#
# Revision 1.1  2004/04/28 21:10:42  rwiegand
# Loads UVOT bad pixel calibration file and provides look up services.
#

use strict;

package UVOT::BadPixels;

use base qw(Task::FITS);
use Task qw(:codes);

use Astro::FITS::CFITSIO qw(:constants :longnames);



sub loadPath
{
	my ($self, $path, %args) = @_;

	my $status = 0;
	my $fits = Astro::FITS::CFITSIO::open_file($path, READONLY, $status);
	if ($status) {
		$self->error(BAD_INPUT, "unable to open $path [$status]");
		return;
	}

	my $extname = 'BADPIX';
	if ($fits->movnam_hdu(BINARY_TBL, $extname, 0, $status)) {
		$self->error(BAD_INPUT,
				"unable to move to $extname extension");
	}

	$self->loadFITS($fits, %args)
		if $self->isValid;

	if ($fits) {
		my $tmp = 0;
		$fits->close_file($tmp);
	}

}


sub positionKey ($$)
{
	my ($x, $y) = @_;
	return "bple$x,$y";
}


sub loadFITS
{
	my ($self, $fits, %args) = @_;

	my @columns = (
		{ name => 'RAWX' },
		{ name => 'RAWY' },
		{ name => 'YLENGTH' },
		{ name => 'QUALITY' },
		{ name => 'TIME', typ => 'dbl' },
	);

	my %columns = map { $_->{name} => $_ } @columns;

	my $count;
	my $status = 0;
	if ($fits->get_num_rows($count, $status)) {
		$self->error(BAD_INPUT,
			"unable to get number of bad pixel list entries [$status]");
	}
	else {
		$self->report("loading $count bad pixel list entries");
	}

	foreach my $c (@columns) {
		my ($colnum, $anynul);
		my @data;
		my $method = $c->{typ} ? "read_col_$c->{typ}" : 'read_col_int';

		if ($fits->get_colnum(CASEINSEN, $c->{name}, $colnum, $status)) {
			$self->error(BAD_INPUT,
				"unable to get bad pixel list $c->{name} column index");
		}
		elsif ($fits->$method($colnum, 1, 1, $count, $c->{null},
					\@data, $anynul, $status)) {
			$self->error(BAD_INPUT,
				"unable to read source list $c->{name} column data");
		}
		else {
			$c->{data} = \@data;
		}
	}

	my @entries;

	if ($self->isValid) {

		for (my $i = 0; $i < $count; ++$i) {
			my %entry = map { $_ => $columns{$_}{data}[$i] } keys(%columns);

			if (not exists($args{time}) or $args{time} >= $entry{TIME}) {
				push(@entries, \%entry);
			}
		}
	}

	foreach my $e (@entries) {

		for (my $i = 0; $i < $e->{YLENGTH}; ++$i) {
			my $x = $e->{RAWX};
			my $y = $e->{RAWY} - $i;

			if ($x < 0 or $x > 2047 or $y < 0 or $y > 2047) {
				$self->warning("invalid bad pixel list entry [x=$x,y=$y]");
			}

			my $key = positionKey($x, $y);
			$self->{$key} = $e;
		}
	}
}



# %args including time/at?
sub isBad
{
	my ($self, $x, $y) = @_;
	my $key = positionKey($x, $y);
	return $self->{$key};
}



1;

