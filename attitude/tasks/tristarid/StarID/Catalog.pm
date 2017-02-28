# $Source: /headas/headas/attitude/tasks/tristarid/StarID/Catalog.pm,v $
# $Revision: 1.5 $
# $Date: 2006/02/22 15:43:46 $
#
# $Log: Catalog.pm,v $
# Revision 1.5  2006/02/22 15:43:46  rwiegand
# Bound searches on catalogs.
#
# Revision 1.4  2006/01/25 21:27:11  rwiegand
# Implemented radius filtering within Catalog::contents.
#
# Revision 1.3  2006/01/25 15:23:46  rwiegand
# Extended SearchCat module to support many scat catalag interfaces.  Allow
# user to pass filter and epoch into catalog loading.  Use Wayne's magnitude
# estimation for UVOT filters when loading USNO B1.
#
# Revision 1.2  2005/10/10 12:09:56  rwiegand
# StarID::Object no longer exists.
#
# Revision 1.1  2005/10/07 20:42:32  rwiegand
# Source catalog modules.
#

use strict;

use Math;



####################################################################

package StarID::Catalog;


sub new
{
	my ($class, %args) = @_;

	my %data = (
		contents => $args{contents} || [ ],
	);

	my $object = bless(\%data, $class);

	return $object;
}


sub contents
{
	my ($self, $position, $radius) = @_;
	my $contents = $self->{contents};
	if ($position and $radius) {
		$contents = $self->filter($contents, $position, $radius);
	}
	return $contents;
}


sub filter
{
	my ($self, $list, $position, $radius) = @_;

	my $mincos = cos($radius);
	my @filtered;
	$self->apply(sub {
				my ($o) = @_;
				if ($o->cosangle($position) >= $mincos) {
					push(@filtered, $o);
				}
			});
	$list = \@filtered;

	return $list;
}


sub size
{
	my $array = shift->contents;
	return scalar(@$array);
}


sub apply
{
	my ($self, $method) = @_;

	foreach my $o (@{ $self->contents }) {
		$method->($o);
	}
}


sub query
{
	my ($self, $query) = @_;

	my @result;

	foreach my $o (@{ $self->contents }) {
		if ($query->($o)) {
			push(@result, $o);
		}
	}

	my $subcatalog = Catalog->new(contents => \@result);

	return $subcatalog;
}


sub add
{
	my ($self, @objects) = @_;

	foreach my $o (@objects) {
		if (not UNIVERSAL::isa($o, 'StarID::Source')) {
			die(__PACKAGE__ . ": [add] invalid object $o");
		}
	}

	push(@{ $self->{contents} }, @objects);
}


####################################################################

package StarID::Catalog::Linear;
use base qw(StarID::Catalog);


####################################################################

package StarID::Catalog::Indexed;
use base qw(StarID::Catalog);

use Math;



sub new
{
	my ($first, %args) = @_;

	my $object = $first->SUPER::new(%args);

	my $p = $args{center};

	my $r = $p->{radius} || Math::degreesToRadians(1);
	my $n = $args{n} || 2;

	my $unit = Math::rd2unit($p->ra, $p->dec);

	my ($u1, $u2) = Math::createSystem($unit);

	$object->{unit} = $unit;
	$object->{nx} = $u1;
	$object->{ny} = $u2;

	my $rPrime = sin($r) / cos($r);
	$object->{k} = $rPrime / $n;
	$object->{q} = $n / $rPrime;
	$object->{scale} = $n / $r;

	$object->{children} = { };
	$object->{minx} = $object->{maxx} = 0;
	$object->{miny} = $object->{maxy} = 0;

	$object->apply(sub {
			my ($o) = @_;
			place($object, $o);
		});

	return $object;
}


sub contents
{
	my ($self, $position, $radius) = @_;

	if (not $position or not $radius) {
		return $self->{contents};
	}

	# this works in two phases:
	#	first, we collect all of the objects from the gridded catalog which
	#		are in grid squares that have some/any point within radius
	#	second, filter the individual objects for being within radius
	my $unit = $position->unit;
	my $ox = Math::v3dot($self->{nx}, $unit);
	my $oy = Math::v3dot($self->{ny}, $unit);

	my $cx = $self->{q} * $ox;
	my $cy = $self->{q} * $oy;

	my $sx = POSIX::floor($cx);
	my $sy = POSIX::floor($cy);

	my $delta = $radius * $self->{scale};

	my $i0 = POSIX::floor($cx - $delta);
	my $i1 = POSIX::ceil($cx + $delta);
	my $j0 = POSIX::floor($cy - $delta);
	my $j1 = POSIX::ceil($cy + $delta);

	my $rangeCount = 0;
	if ($i0 < $self->{minx}) {
		$i0 = $self->{minx};
		++$rangeCount;
	}
	if ($i1 > $self->{maxx}) {
		$i1 = $self->{maxx} + 1;
		++$rangeCount;
	}
	if ($j0 < $self->{miny}) {
		$j0 = $self->{miny};
		++$rangeCount;
	}
	if ($j1 > $self->{maxy}) {
		$j1 = $self->{maxy} + 1;
		++$rangeCount;
	}

	if ($rangeCount == 4) {
		my $filtered = $self->filter($self->{contents}, $position, $radius);
		return $filtered;
	}

	my @objects;

	my $mincos = cos($radius);

	my $uprime = [ $ox, $oy, 1 ];
	Math::v3normalize($uprime);
	my $check = sub {
		my ($x, $y) = @_;
		my $checkpoint = [ $x, $y, $self->{q} ];
		Math::v3normalize($checkpoint);
		my $cosangle = Math::u3cosangle($uprime, $checkpoint);
		return $cosangle >= $mincos;
	};

	for (my $i = $i0; $i < $i1; ++$i) {

		for (my $j = $j0; $j < $j1; ++$j) {

			my $include = 0;

			my $testX = ($i < $sx) ? $i + 1 : $i;
			my $testY = ($j < $sy) ? $j + 1 : $j;

			if ($i == $sx and $j == $sy) {
				$include = 1;
			}
			elsif ($i == $sx) {
				$include = $check->($cx, $testY);
			}
			elsif ($j == $sy) {
				$include = $check->($testX, $cy);
			}
			else {
				# these unit vectors could be pre-calculated...
				$include = $check->($testX, $testY);
			}

			if ($include) {
				$self->addContents(\@objects, $i, $j);
			}
		}
	}

	my $filtered = $self->filter(\@objects, $position, $radius);

	return $filtered;
}


sub addContents
{
	my ($self, $aref, $i, $j) = @_;

	my $cat = $self->fetch($i, $j);

	if ($cat) {
		push(@$aref, @{ $cat->contents });
	}
}


sub fetch
{
	my ($self, $sx, $sy, $create) = @_;

	my $hash = $self->{children};
	my $key = qq($sx,$sy);

	my $out = undef;

	if (exists($hash->{$key})) {
		$out = $hash->{$key};
	}
	elsif ($create) {
		$out = $hash->{$key} = StarID::Catalog::Linear->new;
		if ($sx < $self->{minx}) {
			$self->{minx} = $sx;
		}
		elsif ($sx > $self->{maxx}) {
			$self->{maxx} = $sx;
		}
		if ($sy < $self->{miny}) {
			$self->{miny} = $sy;
		}
		elsif ($sy > $self->{maxy}) {
			$self->{maxy} = $sy;
		}
	}

	return $out;
}


sub place
{
	my ($self, $object) = @_;

	my $u = $object->unit;

	my $ox = Math::v3dot($self->{nx}, $u);
	my $oy = Math::v3dot($self->{ny}, $u);

	my $sx = POSIX::floor($self->{q} * $ox);
	my $sy = POSIX::floor($self->{q} * $oy);

	my $sub = $self->fetch($sx, $sy, 1);

	$sub->add($object);
}



1;

