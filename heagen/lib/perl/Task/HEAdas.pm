# $Source: /headas/headas/heagen/lib/perl/Task/HEAdas.pm,v $
# $Revision: 1.1 $
# $Date: 2013/08/27 15:13:44 $
#
#
# $Log: HEAdas.pm,v $
# Revision 1.1  2013/08/27 15:13:44  irby
# Task.pm is needed by barycorr (heagen), so relocate these items to
# heagen to avoid heagen (mission-independent) having a dependency on
# Swift (mission-specific):
#
#    swift/gen/lib/perl/Task.pm
#    swift/gen/lib/perl/Task/Makefile
#    swift/gen/lib/perl/Task/FITS.pm
#    swift/gen/lib/perl/Task/Getopt.pm
#    swift/gen/lib/perl/Task/HEAdas.pm
#    swift/gen/lib/perl/Task/Subtask.pm
#
# Visit their old location (swift/gen/lib/perl) to view their CVS history.
#
# Revision 1.14  2011/08/29 17:43:18  rwiegand
# Modified putParameterHistory to return CFITSIO status.
#
# Revision 1.13  2005/05/20 20:58:28  rwiegand
# Use HDpar_note interface.
#
# Revision 1.12  2005/03/10 19:20:45  rwiegand
# Made doCommand quieter by default.
#
# Revision 1.11  2005/03/04 19:16:19  rwiegand
# Allow user to specify parameters not to fetch since that is usually
# a smaller set.
#
# Revision 1.10  2005/01/24 16:28:28  rwiegand
# Make use of HEADAS temporary directory environment variable [HEADAS_TMPDIR].
#
# Revision 1.9  2004/11/04 21:26:54  rwiegand
# Allow exceptions to be thrown for non-error termination.
#
# Revision 1.8  2004/10/12 22:45:09  rwiegand
# Added a unitTest method.
#
# Revision 1.7  2004/08/05 19:46:43  rwiegand
# Optionally set temporary directory to match outfile parameter.
#
# Revision 1.6  2004/07/09 15:55:06  rwiegand
# Allow user to specify additional required environment variables.
#
# Revision 1.5  2004/06/17 21:12:37  rwiegand
# Use HEACORE perl support for PIL, program driver.
#
# Revision 1.4  2003/10/16 20:54:27  rwiegand
# Set HEADASNOQUERY environment variable after parseOptions.
#
# Revision 1.3  2003/09/30 14:11:07  rwiegand
# Made subclass of Task::FITS.  Set HEADASNOQUERY environment variable to
# prevent hanging when invalid parameter lists are passed to sub-ftools.
#
# Revision 1.2  2002/12/12 21:01:52  rwiegand
# Made HEAdas tasks cache PIL parameters.  Overrode parseOptions to fetch
# values from parameter files.
#
# Revision 1.1  2002/06/27 20:19:15  wiegand
# Initial revision

package Task::HEAdas;

use strict;

use base qw(Task::FITS);

use Task qw(:codes);

use Astro::FITS::CFITSIO qw(:constants :longnames);

use HEACORE::HEAINIT;
use HEACORE::PIL;
use HEACORE::HEAUTILS;


sub run
{
	my ($self) = @_;

	$self->{code} = 0;

	my $outer = headas_main(sub {

			eval {

				set_toolname($self->{tool});
				set_toolversion($self->{version});

				$self->execute;
			};

			if ($@) {
				if ($@ =~ /^ok:/) {
					$self->warning(substr($@, 4));
				}
				else {
					$self->error(BAD_EXECUTE, "exception: $@");
				}
			}

			my $args = $self->args;
			$self->removeTemporaries if $args->{cleanupFlag};

			$self->timestamp('complete');

			return $self->{code};
		});

	return $outer;
}


sub unitTest
{
	my ($self) = @_;

	$self->run;

	my $word = $self->{code} ? 'failed' : 'passed';
	$self->report("test $word");
}


sub finalize
{
	# do nothing
}


sub parseOptions
{
	my ($self, %args) = @_;

	if (UNIVERSAL::isa($args{argv}, 'ARRAY')) {
		$self->SUPER::parseOptions(@{ $args{argv} });
	}

	if (UNIVERSAL::isa($args{options}, 'ARRAY')) {

		foreach my $name (@{ $args{options} }) {
			$self->queryParameter($name);
		}
	}

	# set this HEADASNOQUERY so ftools spawned by this task do not
	# get hung waiting for input
	$ENV{HEADASNOQUERY} = 1;
}


sub pilOptions
{
	my ($self, %args) = @_;

	my %pil;
	my @par;
	foreach my $opt (@{ $args{options} }) {
		my $name = '';
		my $type;
		if ($opt =~ /^(\w+)=(\w+)$/) {
			$name = $1;
			$type = $2;
		}
		elsif ($opt =~ /^(\w+)$/) {
			$name = $1;
			$type = 'string';
		}
		else {
			$self->warning("ignoring invalid PIL option '$opt'");
		}

		if ($name) {
			$pil{$name} = $type;
			push(@par, $name);
		}
	}

	$self->{pil} = \%pil;

	if (UNIVERSAL::isa($args{get}, 'ARRAY')) {
		foreach my $par (@{ $args{get} }) {
			$self->queryPIL($par);
		}
	}
	elsif (UNIVERSAL::isa($args{noget}, 'ARRAY')) {
		my %noget = map { $_ => 1 } @{ $args{noget} };
		foreach my $par (@par) {
			$self->queryPIL($par)
				if not $noget{$par};
		}
	}
	elsif ($args{get}) {
		foreach my $par (@par) {
			$self->queryPIL($par);
		}
	}

	# set this HEADASNOQUERY so ftools spawned by this task do not
	# get hung waiting for input
	$ENV{HEADASNOQUERY} = 1;

	if ($ENV{TMP_FROM_OUTFILE} and $pil{outfile}) {
		# set temporary directory based on outfile parameter
		if (my $tmp = $self->getParameter('outfile')) {
			$self->setTemporaryDirectory($tmp);
		}
	}
	elsif ($ENV{HEADAS_TMPDIR}) {
		$self->setTemporaryDirectory($ENV{HEADAS_TMPDIR});
	}
}


sub validateEnvironment
{
	my ($self, @vars) = @_;

	# validate environment
	foreach my $var (qw(HEADAS PFILES), @vars) {
		if (not defined($ENV{$var})) {
			$self->fatal(BAD_USAGE,
					"environment variable $var not set");
		}
		elsif ($var eq 'PFILES') {
			my $x = 0;
			map { -d and $x = 1 } split(/[:;]/, $ENV{$var});
			if (not $x) {
				$self->fatal(BAD_USAGE,
						"invalid directory $ENV{$var}");
			}
		}
		elsif (not -d $ENV{$var}) {
			$self->fatal(BAD_USAGE,
					"invalid $var directory $ENV{$var}");
		}
	}
}


# get parameter from .par file
sub queryParameter
{
	my ($self, $name) = @_;

	my $value = undef;

	my $command = qq(pquery2 $self->{tool} $name $self->{qqArgs});
	my $result = $self->doCommand($command);
	if ($result->{error}) {
		$self->error(BAD_INPUT, "[readParameter] $name");
	}
	else {
		$value = $result->{lines}[0];
		$self->args->{$name} = $value;
	}

	return $value;
}


sub getParameter
{
	my ($self, $name) = @_;

	# return cached value if already fetched
	my $value = $self->args->{$name};

	if (not defined($value)) {
		if ($self->{pil}) {
			$value = $self->queryPIL($name);
		}
		else {
			$value = $self->queryParameter($name);
		}
	}

	return $value;
}


sub queryPIL
{
	my ($self, $name) = @_;

	my $type = $self->{pil}{$name} || 'string';;

	my $value = '';
	my $status = 0;

	if ($type eq 'file') {
		$status = PILGetFname($name, $value);
	}
	elsif ($type eq 'string' or $type eq 'str') {
		$status = PILGetString($name, $value);
	}
	elsif ($type eq 'integer' or $type eq 'int') {
		$value = 0;
		$status = PILGetInt($name, $value);
	}
	elsif ($type eq 'boolean' or $type eq 'bool') {
		my $flag = 0;
		$status = PILGetBool($name, $flag);
		$self->args->{$name . 'Flag'} = $flag;
		$value = $flag ? 'yes' : 'no';
	}
	elsif ($type eq 'real' or $type eq 'float') {
		$value = 0;
		$status = PILGetReal($name, $value);
	}
	else {
		# treat as string;
		$status = PILGetString($name, $value);
	}

	if ($status) {
		$self->error(BAD_INPUT,
				"unable to get $name parameter [$status]");
	}
	else {
		$self->args->{$name} = $value;
	}

	return $value;
}


sub putParameterHistory
{
	my ($self, $file) = @_;

	my $status = 0;

	if (ref($file)) {
		# assume is fptr
		HDpar_stamp($file, 1, $status);
	}
	else {
		my $fits = Astro::FITS::CFITSIO::open_file($file, READWRITE, $status);
		if ($status) {
			$self->warning("unable to open $file [$status]");
		}
		else {
			HDpar_stamp($fits, 1, $status);
			$fits->close_file($status);
		}
	}

	if ($status) {
		$self->warning('error writing parameter history');
	}

	return $status;
}


sub parameterNote
{
	my ($self, @args) = @_;
	HDpar_note(@args);
}



1;

