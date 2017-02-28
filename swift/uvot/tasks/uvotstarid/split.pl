
# split a SKYMAP star catalog into the tfc.pl format
#	id
#		\w+
#	ra
#		\d\d \d\d \d\d(.\d+)?
#		HH MM SS.sss
#	dec
#		[+-]\d\d \d\d \d\d(.\d+)?
#		DD MM SS.sss
#	mag
#		[+-]\d+.\d+
#		useful magnitude measurement
#	pmRate
#		\d+.\d+
#		proper motion rate
#	pmAngle
#		\d+.\d+
#		proper motion direction, 0 - 2 pi, 0 corresponds to +pole
#	type

use strict;

package Splitter;

use base qw(Task);
use Task qw(:codes);

use FileHandle;

use Math;
use POSIX;
use Partition;
use StarID;


use constant UNKNOWN_MAGNITUDE => 100;


my @histogram = ();


sub new
{
	my ($class, %args) = @_;

	if (not -f $args{input} and (ref($args{input}) ne 'ARRAY')) {
		die("invalid input catalog '$args{input}'\n");
	}

	if (-d $args{output}) {
		if (not $args{appendOutput}) {
			die("invalid output '$args{output}' exists\n");
		}
	}
	elsif (not mkdir($args{output})) {
		die("unable to create directory $args{output}: $!\n");
	}

	if (int($args{buffer}) < 1) {
		$args{buffer} = 1;
	}

	if (not $args{mode}) {
		$args{mode} = 'text';
	}

	my $object = bless(\%args, $class);

	return $object;
}


sub initialize
{
	my ($self) = @_;

	my $format = '';
	my @fields = ();

	my $isKey = 1;
	my $key = undef;

	my $info = $self->{pack} ? $self->{packing} : $self->{formatting};

	foreach my $x (@{ $info }) {
		if ($isKey) {
			push(@fields, $x);
			$key = $x;
		}
		else {
			if ($self->{pack}) {
				$format .= $x;
			}
			else {
				$format .= "$key=$x, ";
			}
		}
		$isKey = not $isKey;
	}

	$self->{format} = $format;
	$self->{fields} = \@fields;
}


my $DUMPER;
sub stringify
{
	my ($x) = @_;
	if (not $DUMPER) {
require Data::Dumper;
		$DUMPER = Data::Dumper->new([ ]);
		$DUMPER->Indent(2);
		$DUMPER->Quotekeys(0);
#		$DUMPER->Sortkeys(1);
		$DUMPER->Terse(1);
	}

	$DUMPER->Values([ $x ]);
	my $s = $DUMPER->Dump;
	$DUMPER->Reset;

	return $s;
}


sub ensureDirectory
{
	my ($final) = @_;

	my $p = rindex($final, '/');
	if ($p > 0) {
		my $parent = substr($final, 0, $p);
		ensureDirectory($parent);
	}

	if (not -d $final) {
		if (not mkdir($final)) {
			die("unable to mkdir $final: $!\n");
		}
	}
}


sub putQueue
{
	my ($self, $q, $name) = @_;

	my $path = "$self->{output}/$name";
	my $dir = substr($path, 0, rindex($path, '/'));
	if (not -d $dir) {
		ensureDirectory($dir);
	}
	my $fh = FileHandle->new($path, 'a');
	if (not $fh) {
		die("unable to open $path: $!\n");
	}

	foreach my $o (@$q) {
		$fh->print($o);
	}

	undef($fh);
	@$q = ();
}


sub put
{
	my ($self, @objects) = @_;

	my $partition = $self->{partition};

	my $fields = $self->{fields};

	foreach my $o (@objects) {

		my @values = @$o{@$fields};

		my $s = undef;
		if ($self->{pack}) {
			$s = pack($self->{format}, @values);
		}
		else {
			$s = sprintf($self->{format}, @values) . "\n";
		}

		my @plateRefs = $partition->findPossibleOverlaps($o);
		++$histogram[@plateRefs];

		foreach my $p (@plateRefs) {
			my $name = $partition->key($p);
			my $q = $self->{q}{$name};
			if (not $q) {
				$q = $self->{q}{$name} = [ ];
			}
			push(@$q, $s);
			if (@$q > $self->{buffer}) {
				$self->putQueue($q, $name);
			}
		}
	}
}


sub flushOutput
{
	my ($self) = @_;

	my $queued = $self->{q};

	foreach my $key (keys(%$queued)) {
		$self->putQueue($queued->{$key}, $key);
	}
}


sub writeSummary
{
	my ($self) = @_;

	my $path = "$self->{output}/PARTITION";

	my $fh = FileHandle->new($path, 'w');
	if ($fh) {
		my $p = $self->{partition};

		$p->{pack} = $self->{pack};
		$p->{fields} = join(',', @{ $self->{fields} });
		$p->{format} = $self->{format};

		$p->writeSummary($fh);

		undef($fh);
	}
	else {
		die("unable to create summary $path: $!\n");
	}
}


sub execute
{
	my ($self) = @_;

	$self->initialize;

	my @input = (ref($self->{input}) eq 'ARRAY')
			? @{ $self->{input} }
			: ($self->{input});

	foreach my $path (@input) {

		my $fh = FileHandle->new($path);
		if (not $fh) {
			die("unable to open $path: $!\n");
		}

		my $method = "process$self->{mode}Catalog";
		$self->$method($fh, path => $path);

		undef($fh);
	}

	$self->flushOutput;
	$self->writeSummary;
}



# need a similar routine for each catalog format to be processed

sub processSkymapCatalog
{
	my ($self, $fh) = @_;

	my $count = 0;
	my $rsd = qr([ 0-9]);

	while (<$fh>) {

		my $record = $_;

		# last record is garbage
		next if length($record) < 500;

		++$count;
		if (not $count % 1000) {
			print "record $count\n";
		}

		my ($id)      = unpack('x27A8', $record);

		my ($mag)     = unpack('x232A6', $record);

		my ($rawRA)   = unpack('x118A11', $record);
		my ($rawDEC)  = unpack('x129A11', $record);

		my ($rawProperRA)    = unpack('x149A8', $record);
		my ($rawProperDEC)   = unpack('x157A8', $record);

		my ($x, $y, $z) = unpack('x193A9A9A9', $record);

		$id =~ s/\s//g;

		if ($mag !~ /^[-+ ]?\d+\.\d+$/) {
			$mag = UNKNOWN_MAGNITUDE;
		}

		if ($rawRA !~ /^($rsd\d)($rsd\d)($rsd\d.\d+)$/) {
			die("invalid RA '$rawRA' for $id\n");
		}

		my $ra = Math::degreesToRadians((3600 * $1 + 60 * $2 + $3) / 240);

		if ($rawDEC !~ /^[+-]($rsd\d)($rsd\d)($rsd\d.\d+)$/) {
			die("invalid DEC '$rawDEC' for $id\n");
		}

		my $dec = Math::degreesToRadians($1 + $2 / 60 + $3 / 3600);
		if (substr($rawDEC, 0, 1) eq '-') {
			$dec = -$dec;
		}

		my $pmRA      = Math::degreesToRadians(15 * $rawProperRA / 3600);
		my $q = (substr($rawProperDEC, 0, 1) eq '-') ? -1 : 1;
		my $pmDECRest = substr($rawProperDEC, 1);
		my $pmDEC     = $q * Math::degreesToRadians($pmDECRest / 3600);

		my $pmRate = sqrt(($pmRA * cos($dec)) ** 2 + $pmDEC ** 2);
		my $pmAngle = $pmRate ? POSIX::acos($pmDEC / $pmRate) : 0;
		if ($pmAngle < 0) {
			$pmAngle += Math::PIx2;
		}

		my %info = (
			id => $id,
			ra => $ra,
			dec => $dec,
			x => $x,
			y => $y,
			z => $z,
			mag => $mag,
			pmRate => $pmRate,
			pmAngle => $pmAngle,
		);

		my $object = StarID::Object->new(%info);

		$self->put($object);
	}
}


sub processUSNOA2Catalog
{
	my ($self, $fh, %args) = @_;

use constant USNO_A2_RECORD_LENGTH => 6;

	binmode($fh);
	{
		my $t = scalar(gmtime());
		$self->report("starting $args{path} at $t");
	}

	my $group = 0;
	if ($args{path} =~ /zone(\d+).cat$/) {
		$group = $1;
	}
	else {
		$self->warning("unrecognized name: $args{path}");
	}

	my $count = 0;
	my $record = undef;

	my $length = 3 * 4; # 4 bytes each for ra, dec, mag
	while (my $read = $fh->read($record, $length)) {

		if ($read < 0) {
			$self->fatal(BAD_INPUT,
				"[processUSANOA2Catalog] read error $!");
		}
		elsif ($read != $length) {
			# garbage record
			$self->warning("[processUSANOA2Catalog] partial record $read");
			next;
		}

		++$count;

		my $id = "$group:$count";
		if (not $count % 1000) {
			print "record $id\n";
		}

		my ($rawRA, $rawDEC, $rawMAG) = unpack('N3', $record);

		if (($rawRA < 0) || ($rawRA > 24 * 15 * 3600 * 100)) {
			$self->fatal(BAD_INPUT,
				"record $id: raw RA out of range [$rawRA]");
			next;
		}

		if (($rawDEC < 0) || ($rawDEC > 180 * 3600 * 100)) {
			$self->fatal(BAD_INPUT,
				"record $id: raw DEC out of range [$rawDEC]");
			next;
		}

		my $magString = sprintf('%+011d', $rawMAG);
		my $magS = substr($magString, 0, 1);
		my $magQ = substr($magString, 1, 1);
		my $magFFF = substr($magString, 2, 3);
		my $magBBB = substr($magString, 5, 3);
		my $magRRR = substr($magString, 8, 3);

		my $ra = Math::degreesToRadians($rawRA / 100 / 3600);
		my $dec = Math::degreesToRadians($rawDEC / 100 / 3600 - 90);

		my $unit = Math::rd2unit($ra, $dec);
		my $x = $unit->[0];
		my $y = $unit->[1];
		my $z = $unit->[2];

		my $mag = UNKNOWN_MAGNITUDE;
		if ($magBBB < 250) {
			if ($magRRR < 250) {
				$mag = ($magBBB + $magRRR) / 2 / 10;
			}
			else {
				$mag = $magBBB / 10;
			}
		}
		elsif ($magRRR < 250) {
			$mag = $magRRR / 10;
		}

		my %info = (
			id1 => $group,
			id2 => $count,
			ra => $ra,
			dec => $dec,
			x => $x,
			y => $y,
			z => $z,
			mag => $mag,
		);

		my $object = StarID::Object->new(%info);

		$self->put($object);
	}
}


sub processHubbleCatalog
{
	my ($self, $fh, %args) = @_;

	{
		my $t = scalar(gmtime());
		$self->report("starting $args{path} at $t");
	}

	my $count = 0;
	my $ln = 0;
	my $record = undef;

	while (defined($record = <$fh>)) {

		++$ln;
		$record =~ s/\s+$//;
		if ($ln < 3) {
			print "ignoring line $ln [$record]\n"
				if $self->chatter(3);
			next;
		}
		elsif (length($record) != 59) {
			print "bad record [$record][line $ln]\n";
			next;
		}
		my $id  = substr($record, 0, 10);
		my $rawRA  = substr($record, 11, 9);
		my $rawDEC = substr($record, 21, 9);
		my $mag = substr($record, 37, 5);

		++$count;

		if (not $count % 1000) {
			print "record $id\n";
		}

		if ($rawRA < 0 || $rawRA > 360) {
			$self->fatal(BAD_INPUT,
				"record $id: bad RA [$rawRA]");
			next;
		}

		if ($rawDEC < -90 or $rawDEC > 90) {
			$self->fatal(BAD_INPUT,
				"record $id: bad DEC [$rawDEC]");
			next;
		}

		my $ra = Math::degreesToRadians($rawRA);
		my $dec = Math::degreesToRadians($rawDEC);

		my $unit = Math::rd2unit($ra, $dec);
		my $x = $unit->[0];
		my $y = $unit->[1];
		my $z = $unit->[2];

		my %info = (
			id => $id,
			ra => $ra,
			dec => $dec,
			x => $x,
			y => $y,
			z => $z,
			mag => $mag,
		);

		my $object = StarID::Object->new(%info);

		$self->put($object);
	}
}



# main
{
	print "started at ", scalar(gmtime), "\n";

	my $splitter = Splitter->new(

		input => 'input/SKYMAP.raw',
		input => [ # SA2
			'input/zone0000.cat',
			'input/zone0075.cat',
			'input/zone0150.cat',
			'input/zone0225.cat',
			'input/zone0300.cat',
			'input/zone0375.cat',
			'input/zone0450.cat',
			'input/zone0525.cat',
			'input/zone0600.cat',
			'input/zone0675.cat',
			'input/zone0750.cat',
			'input/zone0825.cat',
 			'input/zone0900.cat',
			'input/zone0975.cat',
			'input/zone1050.cat',
			'input/zone1125.cat',
			'input/zone1200.cat',
			'input/zone1275.cat',
			'input/zone1350.cat',
			'input/zone1425.cat',
			'input/zone1500.cat',
			'input/zone1575.cat',
			'input/zone1650.cat',
			'input/zone1725.cat',
		],
		input => [
			map { "./decode.exe $_ |" }
				map { glob("$_/*.GSC") } glob('[NS]*')
		],
		output => '/local/data/woof4/wiegand/partitionGSC',
		output => './out',

		mode => 'Skymap',
		mode => 'USNOA2',
		mode => 'Hubble',
		appendOutput => 0,

		buffer => 30,
		partition => Partition::SquareRADec->new(
			directory => '/tmp',
			order => 180,
			overlapAngle => 5 * Math::ARC_MIN_r,
			),

		pack => 0,
		pack => 1,

		# SKYMAP packing/formatting
		packing => [
			id       => 'A24',
			x        => 'd',
			y        => 'd',
			z        => 'd',
			mag      => 'f',
			pmAngle  => 'f',
			pmRate   => 'f',
		],

		formatting => [ # pretty
			id => '%20s',
			x => '%+010.7f',
			y => '%+010.7f',
			z => '%+010.7f',
			mag => '%+07.3f',
			ra => '%07.5f',
			dec => '%+08.5f',
			pmAngle => '%+06.3f',
			pmRate => '%.3e',
		],

		formatting => [
			id => '%s',
			x => '%f',
			y => '%f',
			z => '%f',
			ra => '%f',
			dec => '%f',
			mag => '%.3f',
			pmAngle => '%f',
			pmRate => '%e',
		],

		formatting => [ # GSC pretty
			id => '%20s',
			ra => '%07.5f',
			dec => '%+08.5f',
			mag => '%+07.3f',
		],

		# USNO A2 packing/formatting
		# store ID as two numbers, pitch the pmRate, pmAngle [0]
		packing => [
			id1      => 'N',
			id2      => 'N',
			x        => 'd',
			y        => 'd',
			z        => 'd',
			mag      => 'f',
		],

		packing => [
			id       => 'N',
			ra       => 'd',
			dec      => 'd',
			mag      => 'f',
		],

		);

	$splitter->execute;

	for (my $i = 0; $i < @histogram; ++$i) {
		my $count = $histogram[$i] || 0;
		print "findPossiblePlates: $i => $count\n";
	}

	print "finished at ", scalar(gmtime), "\n";
}

