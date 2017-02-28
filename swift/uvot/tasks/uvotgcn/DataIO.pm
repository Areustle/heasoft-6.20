# $Source: /headas/headas/swift/uvot/tasks/uvotgcn/DataIO.pm,v $
# $Revision: 1.2 $
# $Date: 2005/11/17 19:56:01 $
#
#	Read/write perl data structures
#
#	woof =
#	{ one => 1, two => 2 } = {
#		one => 1,
#		two => 2,
#	}
#		
# $Log: DataIO.pm,v $
# Revision 1.2  2005/11/17 19:56:01  rwiegand
# Support constraint for type of data being loaded.
#
# Revision 1.1  2005/10/31 14:04:30  rwiegand
# Module for reading/writing arbitrary perl data structures.
#
# Revision 1.1  2005/10/24 11:44:10  wiegand
# Initial revision
#
# Revision 1.2  2005/10/23 11:22:26  Bob
# Implemented input.
#
# Revision 1.1  2005/10/23 09:35:57  Bob
# Initial revision
#

use strict;

package DataIO;

use FileHandle;




sub new
{
	my %args = (
		indent => '    ',
		depth => 0,
	);

	my $object = bless(\%args);

	return $object;
}



sub putPath
{
	my ($self, $path, @data) = @_;

	my $fh = FileHandle->new($path, 'w')
		or die("unable to create $path [$!]");


	$self->{handle} = $fh;
	$self->{_ref} = $self->{ref} || { };

	foreach my $datum (@data) {
		$self->putHandle($datum);
	}

	$fh->close
		or die("unable to close $path [$!]");

	return 1;
}



sub getPath
{
	my ($self, $path, %args) = @_;

	my $fh = FileHandle->new($path)
		or die("unable to open $path [$!]");

	$self->{handle} = $fh;
	$self->{_ref} = $self->{ref} || { };
	$self->{done} = 0;

	my @data = $self->getArray;
	$fh->close
		or die("unable to close $path [$!]");

	if ($args{single}) {
		if (@data != 1) {
			my $count = @data;
			die("getPath: expecting $args{single} but got $count items");
		}
		elsif (not UNIVERSAL::isa($data[0], $args{single})) {
			my $type = ref($data[0]);
			die("getPath: expecting $args{single} but got $type");
		}
		return wantarray ? @data : $data[0];
	}

	return wantarray ? @data : \@data;
}



sub putHandle
{
	my ($self, $data) = @_;

	if (not defined($data)) {
		$self->putUndef($data);
	}
	elsif (not ref($data)) {
		$self->putScalar($data);
	}
	else {
		$self->putReference($data);
	}
}


sub putReference
{
	my ($self, $data) = @_;

	my $ref = $self->{_ref};

	if ($ref->{$data}) {
		$self->putIndented("REF $data");
	}
	else {
		$ref->{$data} = 1;

		if (UNIVERSAL::isa($data, 'HASH')) {
			$self->putHASH($data);
		}
		elsif (UNIVERSAL::isa($data, 'ARRAY')) {
			$self->putARRAY($data);
		}
		elsif (UNIVERSAL::isa($data, 'SCALAR')) {
			$self->putSCALAR($data);
		}
		else {
			die("putReference: $data not allowed");
			# $self->putCODE($data);
			# $self->putGLOB($data);
		}
	}
}



sub putIndented
{
	my ($self, $data) = @_;
	my $indent;
	if ($self->{indented}) {
		$indent = '';
		$self->{indented} = 0;
	}
	else {
		$indent = $self->{indent} x $self->{depth};
	}
	my $term = "\n";
	if ($self->{prefix}) {
		$term = '';
		$self->{prefix} = 0;
	}
	$self->{handle}->print("$indent$data$term");
}


sub putUndef
{
	my ($self, $scalar) = @_;
	$self->putIndented('undef');
}


sub isNumber
{
	my ($scalar) = @_;
	my $reExp = qr(e[-+]?\d+)i;
	if ($scalar =~ /^-?\d+$reExp?$/) {
		return 1;
	}
	elsif ($scalar =~ /^-?\d+\.\d*$reExp?$/) {
		return 1;
	}
	elsif ($scalar =~ /^-?\d*\.\d+$reExp?$/) {
		return 1;
	}
	return 0;
}


sub putScalar
{
	my ($self, $scalar) = @_;
	if (isNumber($scalar)) {
		$self->putIndented($scalar);
	}
	elsif ($scalar =~ /'/) {
		# need to check for unmatched )s
		$self->putIndented(qq(q($scalar)));
	}
	else {
		$self->putIndented(qq('$scalar'));
	}
}


sub putSCALAR
{
	my ($self, $scalar) = @_;
	$self->putIndented("\\ ${ $scalar } # $scalar");
}


sub putHASH
{
	my ($self, $href) = @_;
	if (not keys(%$href)) {
		$self->putIndented("{ } # $href");
	}
	else {
		$self->putIndented("{ # $href");
		++$self->{depth};
		foreach my $key (sort(keys(%$href))) {
			$self->{prefix} = 1;
			if ($key =~ /^\w+$/) {
				$self->putIndented("$key => ");
			}
			else {
				$self->putIndented("'$key' => ");
			}
			$self->{indented} = 1;
			$self->putHandle($href->{$key});
		}
		--$self->{depth};
		$self->putIndented('}');
	}
}


sub putARRAY
{
	my ($self, $aref) = @_;
	if (not @$aref) {
		$self->putIndented("[ ] # $aref");
	}
	else {
		$self->putIndented("[ # $aref");
		++$self->{depth};
		foreach my $e (@$aref) {
			$self->putHandle($e);
		}
		--$self->{depth};
		$self->putIndented(']');
	}
}


sub getLine
{
	my ($self) = @_;

	return if $self->{done};

	my $line;

	do {
		$line = $self->{handle}->getline;
	} while (defined($line) and ($line =~ /^\s*$/ or $line =~ /^\s*#/));

	if (not defined($line)) {
		$self->{done} = 1;
	}
	else {
		$line =~ s/^\s+//;
		$line =~ s/\s+$//;
	}

	return $line;
}


sub getElement
{
	my ($self, $line) = @_;

	if (not defined($line)) {
		$line = $self->getLine;
		return if $self->{done};
	}

	my $element;
	my $first = substr($line, 0, 1);

	if ($first eq "'" or $first eq 'q') {
		$element = $self->getScalar($line);
	}
	elsif ($line =~ /^-?\.?\d/) {
		$element = $self->getScalar($line);
	}
	elsif ($first eq 'u') {
		$element = undef;
	}
	elsif ($first eq '{') { # match }
		$element = $self->getHASH($line);
	}
	elsif ($first eq '[') { # match ]
		$element = $self->getARRAY($line);
	}
	elsif ($first eq '\\') {
		$element = $self->getSCALAR($line);
	}
	elsif ($line =~ /^REF/) {
		$element = $self->getREF($line);
	}
	else {
		die("getElement: unexpected $line");
	}

	return $element;
}


sub getArray
{
	my ($self) = @_;

	my @data;

	while (1) {
		my $element = $self->getElement;
		last if $self->{done};
		push(@data, $element);
	}

	return @data;
}


sub getScalar
{
	my ($self, $string) = @_;
	my $value;
	if ($string =~ /^'(.*)'$/) {
		$value = $1;
	}
	elsif ($string =~ /^q\(.*\)$/) {
		$value = $1;
	}
	else {
		# verify is a number
		$value = $string;
	}
	return $value;
}


sub getREF
{
	my ($self, $line) = @_;

	if ($line =~ /^REF ([A-Za-z:]+\(\w+\))/) {
		my $id = $1;
		if (my $ref = $self->{_ref}{$id}) {
			return $ref;
		}
		else {
			die("unknown reference $id");
		}
	}
	else {
		die("invalid reference $line");
	}
}


sub initReference
{
	my ($self, $ref, $tag) = @_;
	if ($tag =~ /\s*# ([A-Za-z:]+\(\w+\))/) {
		my $id = $1;
		$self->{_ref}{$id} = $ref;
		# print "got reference $id\n";
	}
	elsif ($tag) {
		print "ignoring reference $tag\n";
	}
}


sub getHASH
{
	my ($self, $line) = @_;

	my $href;

	if ($line =~ /^{ }(.*)/) {
		$href = { };
		$self->initReference($href, $1);
	}
	elsif ($line =~ /^{(.*)/) {
		$href = { };
		$self->initReference($href, $1);
		while (1) {
			my $line = $self->getLine;
			my $key;
			my $rest;
			# match {
			if ($line =~ /^}/) {
				last;
			}
			elsif ($line =~ /^(\w+) => (.+)/) {
				$key = $1;
				$rest = $2;
			}
			elsif ($line =~ /^'(.+?)' => (.+)/) {
				$key = $1;
				$rest = $2;
			}
			else {
				die("bad HASH entry: $line");
			}
			my $value = $self->getElement($rest);
			$href->{$key} = $value;
		}
	}
	else {
		die("getHASH: $line");
	}

	return $href;
}


sub getARRAY
{
	my ($self, $line) = @_;

	my $aref = [ ];

	if ($line =~ /^\[\s*\](.*)/) {
		$self->initReference($aref, $1);
	}
	elsif ($line =~ /^\[(.*)/) {
		$self->initReference($aref, $1);
		while (1) {
			my $line = $self->getLine;
			# match [
			if ($line =~ /^]/) {
				last;
			}
			my $value = $self->getElement($line);
			push(@$aref, $value);
		}
	}
	else {
		die("getARRAY: $line");
	}

	return $aref;
}



1;

