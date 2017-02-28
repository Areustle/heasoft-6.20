
use Getopt::Long;
use FileHandle;


my %options = (
	lineLength => 72,
);


use constant OPTIONS => qw(
	verbose!
	lineLength=i
	exclude=s@
	extension=s
);


if (not Getopt::Long::GetOptions(\%options, OPTIONS)) {
	die("$0: bad options\n");
}

my $verbose = $options{verbose};
my $lineLength = $options{lineLength};
my $extension = defined($options{extension})
	? $options{extension} : 'local';


foreach my $f (@ARGV) {
	my $fi = FileHandle->new($f);
	if (not $fi) {
		die("unable to open '$f': $!\n");
	}

	my $base = ($f =~ /(.+)\./) ? $1 : $f;
	my $outfile = "$base.$extension";
	my $fo = FileHandle->new(">$outfile");
	if (not $fo) {
		die("unable to open '$outfile': $!\n");
	}

	binmode($fo);

	my %dependencies = ();

	while (defined(my $line = <$fi>)) {

		if ($line !~ /^\w+\.o:/) {
			next;
		}

		my ($target, $dependencies) = split(/:/, $line);

print "target $target\n" if $verbose;
		my $v = $dependencies{$target} || [ ];
		$dependencies{$target} = $v;

		my @dependencies = split(' ', $dependencies);
		foreach my $d (@dependencies) {
			my $exclude = undef;
			foreach my $x (@{ $options{exclude} }) {
				if ($d =~ $x) {
print "excluding $d\n" if $verbose;
					$exclude = 1;
					last;
				}
			}
			if (not $exclude) {
print "dependency $d\n" if $verbose;
				push(@{ $v }, $d);
			}
		}
	}

	foreach my $t (sort(keys(%dependencies))) {

		my $pending = "$t:";

		# put at least one dependency on each line but otherwise
		# do not exceed line length
		foreach my $d (@{ $dependencies{$t} }) {
			if (length($pending) + length($d) > $lineLength) {
				$fo->print("$pending\n");
				$pending = "$t:";
			}
			else {
				$pending .= " $d";
			}
		}

		if (length($pending) > length($t) + 1) {
			$fo->print("$pending\n");
		}
	}

	$fi->close;
	$fo->close;
}

