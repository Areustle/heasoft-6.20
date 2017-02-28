# $Source: /headas/headas/heagen/lib/perl/Task.pm,v $
# $Revision: 1.1 $
# $Date: 2013/08/27 15:13:44 $
#
#	Task module
#
#
# $Log: Task.pm,v $
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
# Revision 1.27  2009/05/04 18:28:43  rwiegand
# Added note report type.
#
# Revision 1.26  2008/09/04 21:28:14  rwiegand
# Added moveFile method.
#
# Revision 1.25  2005/11/21 20:21:31  rwiegand
# Removed an extra space from warning and verbose messages.  Corrected
# masking of system exit status.
#
# Revision 1.24  2005/10/31 16:32:53  rwiegand
# Support for capturing message to a file.
#
# Revision 1.23  2005/08/29 18:24:02  rwiegand
# Tweaked reporting methods.
#
# Revision 1.22  2005/08/29 11:42:04  rwiegand
# Report message tweak.
#
# Revision 1.21  2005/05/18 21:12:43  rwiegand
# Added generic FITS i/o error code.
#
# Revision 1.20  2005/03/10 19:20:45  rwiegand
# Made doCommand quieter by default.
#
# Revision 1.19  2005/02/09 18:51:00  rwiegand
# Default reporting of sub-process output based on chatter.
#
# Revision 1.18  2005/02/04 18:55:26  rwiegand
# Trim leading and trailing whitespace from sub-process output.
# Renamed doCommand to shell.
#
# Revision 1.17  2005/01/24 16:28:27  rwiegand
# Make use of HEADAS temporary directory environment variable [HEADAS_TMPDIR].
#
# Revision 1.16  2004/10/12 22:43:39  rwiegand
# Fixed duplicate 'error:' prefix in messages.
#
# Revision 1.15  2004/08/05 19:45:58  rwiegand
# Report errors on stderr.  Support for specifying temporary directory.
#
# Revision 1.14  2004/06/17 21:09:11  rwiegand
# Set tool name to program/script name and version if not already set.  Tag
# error messages accordingly.
#
# Revision 1.13  2004/01/13 16:13:06  rwiegand
# Support for cleaning up and external ownership of temporary files.
#
# Revision 1.12  2003/10/24 15:42:42  rwiegand
# Provide finer control of reporting of subprocess output.
#
# Revision 1.11  2003/09/30 14:08:59  rwiegand
# Prefixed sub initialize with underscore.
#
# Revision 1.10  2003/07/25 20:22:20  rwiegand
# Added buildCommand and runCommand for invoking external tools.
#
# Revision 1.9  2003/05/14 13:42:46  rwiegand
# Support for temporary files
#
# Revision 1.8  2003/02/10 18:11:06  rwiegand
# Import FileHandle module.
#
# Revision 1.7  2003/01/22 16:10:23  rwiegand
# Made fatal an alias for error.  Store string being executed in doCommand
# result hash.
#
# Revision 1.6  2002/11/21 16:38:29  wiegand
# Added export tags.  Renamed fatal => error.  Added call to initialize
# in new.
#
# Revision 1.5  2002/09/30 18:25:54  wiegand
# Added stringify
#
# Revision 1.4  2002/08/12 18:31:32  wiegand
# Added BAD_EXECUTE code and warning method
#
# Revision 1.3  2002/07/25 13:22:58  wiegand
# Fixed code constants.  Moved arguments to sub-HASH
#
# Revision 1.2  2002/07/19 13:34:02  wiegand
# Revamped parameter support
#
# Revision 1.1  2002/06/27 20:22:18  wiegand
# Initial revision
#
#

package Task;

use strict;

use base qw(Exporter);

@Task::EXPORT_OK = qw(BAD_USAGE BAD_TASK BAD_INPUT BAD_OUTPUT BAD_EXECUTE
				BAD_FITS);
%Task::EXPORT_TAGS = (
	codes => [ qw(BAD_USAGE BAD_TASK BAD_INPUT BAD_OUTPUT BAD_EXECUTE
				BAD_FITS) ],
);

use FileHandle;
use File::Copy qw();

use constant BAD_USAGE   =>  1;
use constant BAD_TASK    =>  2;
use constant BAD_INPUT   =>  3;
use constant BAD_OUTPUT  =>  4;
use constant BAD_EXECUTE =>  5;

use constant BAD_FITS    =>  11;



sub report
{
	my $self = shift;
	my $tool = $self->{tool} || 'task';
	my $text = "$tool: @_\n";
	print $text;

	if (my $fh = $self->{_loghandle}) {
		$fh->print($text);
	}
}


sub error
{
	my ($self, $code, @rest) = @_;
	$self->{code} = $code;
	my $str = "error: @rest";
	print STDERR "$self->{tool}: $str\n"
		if $ENV{TASK_STDERR};
	$self->report($str);
}


sub fatal
{
	&error;
}


sub warning
{
	my ($self, @rest) = @_;
	$self->report('warning:', @rest);
}


sub note
{
	my ($self, @rest) = @_;
	$self->report('note:', @rest);
}


sub verbose 
{
	my ($self, @rest) = @_;
	$self->report('verbose:', @rest);
}


sub debug 
{
	my ($self, @rest) = @_;
	$self->report('debug: ', @rest);
}


sub isValid
{
	my ($self) = @_;
	return not $self->{code};
}


sub new
{
	my ($class, %args) = @_;

	my $object = bless({
			code => 0,
			%args,
		}, $class);

	$object->_initialize;

	return $object;
}


sub _initialize
{
	my ($self) = @_;
	$self->{tool} ||= $0;
	$self->{toolfull} ||= $0;
	my $slash = rindex($self->{tool}, '/');
	if ($slash >= 0) {
		$self->{tool} = substr($0, $slash + 1);
	}

	$self->{version} ||= "0.1";
	$self->timestamp('initialized');
}


sub chatter
{
	my ($self, $level) = @_;
	my $chatter = $self->args->{chatter} || 0;
	if (not defined($level)) {
		return $chatter;
	}
	return $chatter >= $level;
}


sub doCommand
{
	my ($self, $command, %args) = @_;
	if (not exists($args{report})) {
		$args{report} = 6;
	}
	return shell($self, $command, \%args);
}


sub shell
{
	my ($self, $command, $args) = @_;

	my ($first) = split(/\s+/, $command);

	if ($self->chatter(3)) {
		$self->report("shell $command");
	}
	elsif ($self->chatter) {
		$self->report("running $first");
	}

	my $result = {
		command => $command,
		command1 => $first,
	};

	if ($args->{pipeFrom}) {
		$result->{handle} = FileHandle->new("$command|");
	}
	else {
		my $output = qx($command);
		if (defined($output)) {
			$output =~ s/^\s+//;
			$output =~ s/\s+$//;
			$result->{output} = $output;
			$result->{lines} = [ split(/\n/, $output) ];
		}
	}

	$result->{status} = $?;
	my $code = $result->{code} = $result->{status} >> 8;
	my $os = $result->{os} = $result->{status} & 0xff;

	if ($result->{status}) {
		my $s = ($self->chatter(3) || $args->{output}) ? $first : $command;
		$self->error(BAD_OUTPUT, "error running $s [code=$code, os=$os]");
	}

	$self->_reportResult($result, $args);

	return $result;
}


sub _reportResult
{
	my ($self, $result, $args) = @_;

	my $report = exists($args->{report}) ? $args->{report} : 3;

	my $method = undef;
	if ($result->{code}) {
		if ($report or exists($args->{error})) {
			$method = 'warning';
		}
	}
	elsif ($report and $self->chatter($report)) {
		$method = 'report';
	}

	if (not $method) {
	}
	elsif ($args->{pipeFrom}) {
		$self->$method("--- piping from $result->{command1}\n");
	}
	elsif ($args->{pipeTo}) {
		$self->$method("--- piping to $result->{command1}\n");
	}
	else {
		my $prefix = "\n--- begin $result->{command1}\n";
		my $suffix = "\n--- end $result->{command1}";

		my $text = $result->{output};
		if ($args->{lines}) {
			if (scalar(@{ $result->{lines} }) > $args->{lines}) {
				# grab first and last half
				my $half = int($args->{lines} / 2);
				my @pre = @{ $result->{lines} }[0 .. $half - 1];
				my @post = @{ $result->{lines} }[-$half .. -1];
				$text = join("\n", @pre, "[... $result->{command1}]", @post);
			}
		}

		if ($text =~ /^\s*$/) {
			$self->$method("--- begin/end $result->{command1}");
		}
		else {
			$self->$method("$prefix$text$suffix");
		}
	}
}


sub buildCommand
{
	my ($self, $exec, @args) = @_;

	my $command = $exec;
	while (@args) {
		my $parameter = shift(@args);
		my $value = shift(@args);

		if (defined($value)) {
			$command .= qq( "$parameter=$value");
		}
		else {
			$self->error(BAD_TASK,
				"$exec missing $parameter value");
		}
	}

	return $command;
}


sub runCommand
{
	my ($self, $command, %args) = @_;

	my $result = $self->shell($command, \%args);

	if ($result->{code}) {
		if ($args{failure}) {
			$self->report($args{failure});
		}
		$self->error(BAD_OUTPUT,
			"$command failed [$result->{code}]");
	}
	elsif ($args{success}) {
		$self->report($args{success});
	}

	return $result;
}



sub args
{
	my ($self) = @_;
	my $args = $self->{args};
	if (not $args) {
		$self->{args} = $args = { };
	}
	return $args;
}


sub get
{
	my ($self, $name) = @_;
	my $args = $self->args;
	my $value = exists($args->{$name}) ? $args->{$name} : undef;
	return $value;
}


sub set
{
	my ($self, $name, $value) = @_;
	my $args = $self->args;
	$args->{$name} = $value;
}


sub parseOptions
{
	my ($self, @args) = @_;

	$self->{rawArgs} = \@args;

	# quote arguments to pass on command line
	my $qqs = '';

	foreach my $arg (@args) {
		if ($arg =~ /^(\w+)=(.+)/) {
			$self->args->{$1} = $2;
		}
		$qqs .= qq( "$arg");
	}

	$self->{qqArgs} = $qqs;
}


sub execute
{
	my ($self) = @_;
}


my $DUMPER = undef;
sub stringify
{
	my $self = shift;

	if (not $DUMPER) {
use Data::Dumper;
		$DUMPER = Data::Dumper->new([ ]);
		$DUMPER->Indent(2);
		$DUMPER->Terse(1);
		$DUMPER->Quotekeys(0);
		$DUMPER->Sortkeys(1) if $DUMPER->can('Sortkeys');
	}

	$DUMPER->Values([ @_ ]);
	my $s = $DUMPER->Dump;
	$s =~ s/\s+$//;
	$DUMPER->Reset;

	return $s;
}


sub temporary
{
	my ($self, $basename, %args) = @_;

	$self->{_tmpdir} = '' if not defined($self->{_tmpdir});
	$self->{_tmpi} ||= 0;
	++$self->{_tmpi};

	my $dir = $args{dir} || $self->{_tmpdir};
	my $ext = $args{ext} || '';
	my $pid = $$;

	my $temporary;
	while (-e ($temporary = "$dir$basename.$pid.$self->{_tmpi}$ext")) {
		++$self->{_tmpi};
	}

	if (not $args{release}) {
		$self->addTemporary($temporary);
	}

	return $temporary;
}


sub addTemporary
{
	my ($self, $path) = @_;
	push(@{ $self->{_tmp} }, $path);
}


sub temporaries
{
	my ($self) = @_;
	my @tmp;
	if ($self->{_tmp}) {
		@tmp = @{ $self->{_tmp} };
	}
	return @tmp;
}


sub removeTemporaries
{
	my ($self) = @_;

	foreach my $path ($self->temporaries) {
		if (-e $path) {
			unlink($path)
				or $self->warning("unable to unlink $path [$!]");
		}
	}

	$self->{_tmp} = [ ];
}


sub setTemporaryDirectory
{
	my ($self, $hint) = @_;

	my $tmp = '';

	my $lastSlash = rindex($hint, '/');

	if (-d $hint) {
		$tmp = $hint;
	}
	elsif ($lastSlash >= 0) {
		$tmp = substr($hint, 0, $lastSlash + 1);
	}

	if ($tmp and substr($tmp, -1) ne '/') {
		$tmp .= '/';
	}

	$self->{_tmpdir} = $tmp;
}


sub timestamp
{
	my ($self, $message) = @_;

	my $time = time();

	if (not $self->{startTimestamp}) {
		$self->{startTimestamp} = $time;
	}

	my $delta = $self->{lastTimestamp}
			? $time - $self->{lastTimestamp}
			: 'null';

	$self->{lastTimestamp} = $time;

	my $elapsed = $time - $self->{startTimestamp};

	$self->report("$message [time=$time, delta=$delta, elapsed=$elapsed]")
		if $self->chatter(4);
}



sub setLogFile
{
	my ($self, $path) = @_;

	my $fh;

	if (-e $path) {
		$self->warning("$path already exists, will try to append");
		$fh = FileHandle->new($path, 'a');
	}
	else {
		$fh = FileHandle->new($path, 'w');
	}

	if ($fh) {
		$self->{_loghandle} = $fh;
		$self->timestamp("log file started");
	}
	else {
		$self->warning("unable to open $path [$!]");
	}
}


sub closeLogFile
{
	my ($self) = @_;

	if (my $fh = $self->{_loghandle}) {
		$self->timestamp("closing log file");
		$fh->close;
	}
}


sub moveFile
{
	my ($task, $from, $to) = @_;

	if (not File::Copy::move($from, $to)) {
		$task->error(BAD_OUTPUT, "unable to move $from => $to [$!]");
	}
}


1;

