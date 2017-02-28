# $Source: /headas/headas/attitude/tasks/tristarid/StarID/CatalogLoader.pm,v $
# $Revision: 1.8 $
# $Date: 2010/07/22 21:13:18 $
#
# $Log: CatalogLoader.pm,v $
# Revision 1.8  2010/07/22 21:13:18  rwiegand
# Optionally direct match reference objects against detections.  Provide
# alternate match filtering algorithm (filter.mode=BASE_COUNT_RANGE).
# Make Group goodness depend on number, intensity of matches, and distance.
#
# Revision 1.7  2008/05/08 21:40:33  rwiegand
# Calculate unit vector to center of catalog.
#
# Revision 1.6  2006/03/08 15:00:30  rwiegand
# Find bounding circle by collecting extremes of unit vector components
# instead of working in RA/DEC.
#
# Revision 1.5  2006/01/25 15:23:46  rwiegand
# Extended SearchCat module to support many scat catalag interfaces.  Allow
# user to pass filter and epoch into catalog loading.  Use Wayne's magnitude
# estimation for UVOT filters when loading USNO B1.
#
# Revision 1.4  2005/11/02 21:00:49  rwiegand
# Set default data directory to catspec directory.
#
# Revision 1.3  2005/10/31 13:56:10  rwiegand
# Renamed the catalog partition paramater to catspec.  This removes the
# need for a dummy directory when the actual data is not even local.
#
# Revision 1.2  2005/10/10 12:09:56  rwiegand
# StarID::Object no longer exists.
#
# Revision 1.1  2005/10/07 20:42:32  rwiegand
# Source catalog modules.
#
# Revision 1.2  2005/03/04 19:40:20  rwiegand
# Calculate a tighter bound on the sub-catalog to load.  Make use of the
# partition description when creating the catalog.
#
# Revision 1.1  2004/06/17 21:23:28  rwiegand
# The code for loading a partitioned star catalog was modularized from the
# uvotstarid since several tasks need it.  The Partition and SquareRADec
# modules were relocated here with some refactoring and a new Partition
# subclass was added for dealing with the hierarchical triangular mesh
# partitioning used by the Guide Star Catalog folks.

use strict;

package StarID::CatalogLoader;
use base qw(Task::Subtask);

use Task qw(:codes);

use Math;
use StarID::Source;
use StarID::Partition;
use StarID::Catalog;
use Astro::Julian qw(ymdToJulian);

use constant MAG_NULL => 99;

# supported Partitions
#	StarID::SearchCat
#	StarID::SquareRADec
#	StarID::HierTriMesh



sub execute
{
	my ($self) = @_;

	$self->initialize
		if $self->isValid;

	$self->loadPartition
		if $self->isValid;

	$self->loadPlate
		if $self->isValid;
}


sub findBoundingCircle
{
	my ($task, $sources) = @_;

	my @umin = (2, 2, 2);
	my @umax = (-2, -2, -2);

	foreach my $s (@$sources) {
		my $unit = $s->unit;
		foreach my $i (0, 1, 2) {
			if ($unit->[$i] < $umin[$i]) {
				$umin[$i] = $unit->[$i];
			}
			if ($unit->[$i] > $umax[$i]) {
				$umax[$i] = $unit->[$i];
			}
		}
	}

	my $unit = [ 0, 0, 0 ];
	foreach my $i (0, 1, 2) {
		$unit->[$i] = ($umin[$i] + $umax[$i]) / 2;
	}
	Math::v3normalize($unit);

	my @radec = Math::v3rdl($unit);
	my $bounds = StarID::Source->new(
			ID => 'bounds',
			RA => $radec[0],
			DEC => $radec[1],
			);

	my $mincos = 1;

	foreach my $s (@$sources) {
		my $cos = Math::u3cosangle($s->unit, $unit);
		if ($cos < $mincos) {
			$mincos = $cos;
		}
	}

	$bounds->{radius} = 1.1 * POSIX::acos($mincos) + Math::ARC_MIN_r;

	return $bounds;
}


sub initialize
{
	my ($self) = @_;

	$self->{directory} ||= '.';
	if (my $spec = $self->{catspec}) {
		my $slash = rindex($spec, '/');
		if ($slash >= 0) {
			$self->{directory} = substr($spec, 0, $slash);
		}
	}
	else {
		$self->{catspec} = "$self->{directory}/PARTITION";
	}

	if ($self->{sources}) {
		$self->{bounds} = $self->findBoundingCircle($self->{sources});
	}
	elsif (my $bounds = $self->{bounds}) {
		if ($bounds->{degrees}) {
			# convert to radians
			my %new = %$bounds;
			foreach my $key (qw(RA DEC radius)) {
				if (exists($bounds->{$key})) {
					$new{$key} = Math::degreesToRadians($bounds->{$key});
				}
			}
			$self->{bounds} = StarID::Source->new(%new);
		}
	}
	else {
		$self->error(BAD_INPUT, "missing bounds/sources");
	}

	if (my $bounds = $self->{bounds}) {

		$bounds->{UNIT} = Math::rd2unit($bounds->{RA}, $bounds->{DEC});

		if (my $filter = $bounds->{filter}) {
			$bounds->{filter} =~ s/'//g;
		}

		if (my $dateobs = $bounds->{dateobs}) {
			$dateobs =~ s/'//g;
			my ($year, $month, $day, $hour, $min, $sec)
						= split(/[-'T:]/, $dateobs);
			my $fraction = ($hour * 3600 + $min * 60 + $sec) / 86400;
			my $jdobs = ymdToJulian($year, $month, $day, $fraction);
			my $jdy0 = ymdToJulian($year, 1, 1);
			my $jdy1 = ymdToJulian($year+1, 1, 1);
			$bounds->{epoch} = $year + ($jdobs - $jdy0) / ($jdy1 - $jdy0);
		}
	}

	$self->{bounds}{radius} ||= 5 * Math::ARC_MIN_r;
}


sub loadPartition
{
	my ($self) = @_;

	my %info;

	my $path = $self->{catspec};
	$self->report("loading $path");

	# assign default directory
	$info{directory} = $self->{directory};

	my $fh = FileHandle->new($path);
	if (not $fh) {
		$self->error(BAD_INPUT, "unable to open $path [$!]");
	}
	else {
		while (<$fh>) {
			chomp;
			if (/^\s*$/ or /^\s*#/) {
				# comment
			}
			elsif (/^\s*(\S+)\s+=>\s+(\S+)\s*$/) {
				my $value = $2;
				my ($key, $subkey) = split('/', $1, 2);
				if (defined($subkey)) {
					$info{$key}{$subkey} = $value;
				}
				else {
					$info{$key} = $value;
				}
			}
			else {
				$self->getTask->warning("ignoring $_");
			}
		}
		undef($fh);
	}

	if (not defined($info{type})) {
		$self->error(BAD_INPUT, "missing type keyword");
	}
	elsif (not defined($info{data})) {
		$self->error(BAD_INPUT, "missing data keyword");
	}
	else {
		eval("require $info{type}");
		$self->{partition} = $info{type}->new(%info,
				task => $self->getTask,
				catspec => $self->{catspec},
				);

		my $method = 'resolve' . uc($info{data});
		$self->{resolver} = sub {
			my ($raw) = @_;
			$self->$method($raw);
		};
	}
}


sub loadPlate
{
	my ($self) = @_;

	my $partition = $self->{partition};
	my $plate = $partition->getPlate($self->{bounds});

	if (not $plate) {
		$self->error(BAD_INPUT, "unable to get plate");
		return;
	}

	# if loading the plate already determined the contents, just return
	if ($plate->{contents}) {
		$self->{catalog} = $self->createCatalog(
				contents => $plate->{contents},
				center => $self->{bounds},
				);
		return;
	}

	my $path = $partition->platePath($plate);

	my $fh = FileHandle->new($path);
	if (not $fh) {
		$self->error(BAD_INPUT, "unable to open $path [$!]");
    }
    else {
        $self->report("loading catalog from $path")
            if $self->chatter(3);

        my $method = $partition->{packed}
                ? 'loadPackedPlate'
                : 'loadFormedPlate';

        $self->{catalog} = $self->$method(
            handle => $fh,
            format => $partition->{format},
            fields => $partition->{fields},
            );

        undef($fh);
    }
}


sub createCatalog
{
	my ($self, %args) = @_;
	my $catinfo = $self->{partition}{catalog};
	my $tag = $catinfo->{type} || 'Linear';
	my $class = "StarID::Catalog::$tag";
	my $catalog = $class->new(%args, %$catinfo);
	return $catalog;
}


sub loadPackedPlate
{
	my ($self, %args) = @_;

	my $ENDIANNESS = (pack('N', 1) eq pack('L', 1)) ? 'big' : 'little';

	# this is not pure perl, but pretty reliable
	# abs($PACK_SWAP{$type}) = number of bytes
	#	if ($PACK_SWAP{$type} < 0) swapping necessary
	my %PACK_SWAP = (
		N => -4,
		n => -2,
		s => 2,
		S => 2,
		i => 4,
		I => 4,
		l => 4,
		L => 4,
		d => 8,
		f => 4,
	);

	my $fh = $args{handle};
	my $format = $args{format};
	my @fields = split(',', $args{fields});

	# determine the record size
	my $packed = pack($format, (0) x @fields);
	my $bytes = length($packed);
	$self->report("loadPackedPlate: records are $bytes bytes")
		if $self->chatter(5);

	my $endianness = $self->{partition}{endianness};
	my @swaps;
	if (defined($endianness) and ($endianness ne $ENDIANNESS)) {

		$self->report('endianness correction necessary')
			if $self->chatter(4);

		my $offset = 0;

		# doesn't handle all pack specifiers with multipliers...
		foreach my $x (split(//, $format)) {
			my $info = $PACK_SWAP{$x};
			if (not $info) {
				die("unknown pack format '$x'");
			}
			else {
				if ($info > 0) {
					push(@swaps, [ $offset, $info ]);
				}
				$offset += abs($info);
			}
		}

		if ($offset != $bytes) {
			die("swap setup mismatch $offset != $bytes");
		}
	}

	my @objects;

	binmode($fh);

	my $record;
	my $read;
	while ($read = $fh->read($record, $bytes) == $bytes) {

		if (@swaps) {
			foreach my $s (@swaps) {
				my $was = substr($record, $s->[0], $s->[1]);
				substr($record, $s->[0], $s->[1]) = reverse($was);
			}
		}

		my @values = unpack($format, $record);

		my %info = map { $fields[$_] => $values[$_] } (0 .. @fields - 1);

		if (my $object = $self->resolveObject(\%info)) {
			push(@objects, $object);
		}
	}

	if (not defined($read)) {
		$self->error(BAD_INPUT, "read error [$!]");
	}
	elsif ($read == 0) {
		# eof
	}
	elsif ($read != $bytes) {
		$self->error(BAD_INPUT, "read $read != $bytes bytes");
	}
	else {
		# huh?!
		$self->error(BAD_INPUT, "read => $read ?!");
	}

	my $catalog = $self->createCatalog(
			contents => \@objects,
			center => $self->{bounds},
			);

	return $catalog;
}


sub loadFormedPlate
{
	my ($self, %args) = @_;

	my $fh = $args{handle};
	my %fields = map { $_ => 1 } split(',', $args{fields});
	my $count = keys(%fields);

	my @objects;

	while (<$fh>) {

		my $line = $_;

		my @f = split(/, /, $line);
		if (@f != $count) {
			# there is an null field at the end of each line
			die("wrong number of fields " . scalar(@f) . " in '$line'");
		}

		my %info;
		foreach my $f (@f) {
			if ($f =~ /^(\w+)=(.+)/) {
				if (not $fields{$1}) {
					die("unexpected field identifier '$1'");
				}
				$info{$1} = $2;
			}
			elsif ($f =~ /^\s*$/) {
				# ignore
			}
			else {
				die("invalid field '$f'");
			}
		}

		if (my $object = $self->resolveObject(\%info)) {
			push(@objects, $object);
		}
	}

	my $catalog = $self->createCatalog(
			contents => \@objects,
			center => $self->{bounds},
			);

	return $catalog;
}


sub resolveObject
{
	my ($self, $raw) = @_;
	return $self->{resolver}->($raw);
}


sub getCatalog
{
	return shift->{catalog};
}


sub resolveDefault
{
	my ($self, $raw) = @_;

	# ra, dec given in radians
	my $ra = $raw->{RA};
	my $dec = $raw->{DEC};

	# normalizes ra, dec
	my $unit = Math::rd2unit($ra, $dec);
	($ra, $dec) = Math::v3rdl($unit);

	my $object = StarID::Source->new(
			ID   => $raw->{ID},
			UNIT => $unit,
			MAG  => $raw->{MAG} || MAG_NULL,
			RA   => $ra,
			DEC  => $dec,
			TYPE => $raw->{TYPE},
			);

	return $object;
}


sub resolveGSC
{
	my ($self, $raw) = @_;
	my $object = $self->resolveDefault($raw);
	return $object;
}


sub resolveDEFAULT
{
	my ($self, $raw) = @_;
	my $object = $self->resolveDefault($raw);
	return $object;
}


sub normalizeRA ($)
{
	my ($alpha) = @_;
	my $ra = Math::remainder($alpha, Math::PIx2);
	return $ra;
}


sub normalizeDEC ($)
{
	my ($delta) = @_;
	# first get -pi < $delta < $pi
	my $dec0 = POSIX::fmod($delta, Math::PI);
	my $dec;
	if (abs($dec0) > Math::PIo2) {
		my $signum = ($dec0 < 0) ? -1 : 1;
		$dec = $signum * Math::PI - $dec0;
	}
	else {
		$dec = $dec0;
	}
	return $dec;
}


sub applyUSNOProperMotion
{
	my %args = @_;

	my $pmScale = $args{pmScale} || 1;
	my $epsilon = $args{epsilon} || 1e-2;

	my ($ra0, $dec0) = Math::v3rdl($args{unit});

	my $decMotion = $args{pmRate} * cos($args{pmAngle});
	my $raMotion = 0;

	my $cosdec = cos($dec0);
	if (abs($cosdec) > $epsilon) {
		$raMotion = -$args{pmRate} * sin($args{pmAngle}) / $cosdec;
	}

	my $trueRA = normalizeRA($ra0 + $raMotion * $pmScale);
	my $trueDEC = normalizeDEC($dec0 + $decMotion * $pmScale);

	my $trueUnit = Math::rd2unit($trueRA, $trueDEC);

	my $outUnit = $args{outUnit} || $args{unit};
	@{ $outUnit } = @{ $trueUnit };

	return $outUnit;
}


sub resolveUSNO
{
	my ($self, $raw) = @_;

	my $rawUnit = [ $raw->{x}, $raw->{y}, $raw->{z} ];

	my $properUnit;

	if (defined($raw->{pmRate})) {
		$properUnit = [ 0, 0, 0 ];
		applyUSNOProperMotion(
				unit => $rawUnit,
				pmRate => $raw->{pmRate},
				pmAngle => $raw->{pmAngle},
				outUnit => $properUnit,
				);
	}
	else {
		$properUnit = $rawUnit;
	}

	# normalizes ra, dec
	my ($ra, $dec) = Math::v3rdl($properUnit);

	my $object = StarID::Source->new(
			ID   => "$raw->{id1}:$raw->{id2}",
			UNIT => $properUnit,
			MAG  => $raw->{mag} || MAG_NULL,
			RA   => $ra,
			DEC  => $dec,
			);

	return $object;
}


1;

