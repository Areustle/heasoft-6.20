# $Source: /headas/headas/swift/uvot/lib/perl/UVOT/Time.pm,v $
# $Revision: 1.6 $
# $Date: 2016/02/23 21:34:58 $
#
# $Log: Time.pm,v $
# Revision 1.6  2016/02/23 21:34:58  rwiegand
# Modified uvotproduct to use swifttime when converting between MET and UTC and updated labels to reflect that.
#
# Revision 1.5  2013/03/12 19:49:27  rwiegand
# Updated UVOT::Time::utc2met to use swifttime.  Note that this is inconsistent
# with met2utc.
#
# Revision 1.4  2012/07/13 15:24:27  rwiegand
# Use sec2time instead of swifttime to convert from MET to UTC for consistency
# with other software at SDC.
#
# Revision 1.3  2010/02/24 21:02:35  rwiegand
# Use pget to retrieve swifttime outtime instead of trying to parse stdout and
# discarding stderr.
#
# Revision 1.2  2009/04/29 21:12:18  rwiegand
# Prefer swifttime to met2utc for time conversions.
#
# Revision 1.1  2008/03/26 14:28:14  rwiegand
# Added module for converting between MET and ISO/UTC times.
#

package UVOT::Time;
use Task qw(:codes);

use strict;


my @MONTH_NAME = qw(Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec);
my $ix;
my %MONTH = map { (uc($_) => ++$ix) } @MONTH_NAME;


sub met2utc
{
	my ($task, $met, %args) = @_;

	my $datetime = undef;
	my $result;

	# 2012Jul11: no longer want to use swifttime since it would be
	# inconsistent with other Swift software.
	if ($args{SWIFTTIME}) {

		my $command = $task->buildCommand('swifttime',
				intime => $met,
				insystem => 'MET',
				informat => 's',
				outsystem => 'UTC',
				outformat => 'c',
				);

		my $tmpchatter = $task->args->{chatter};
		if ($tmpchatter < 3) {
			$tmpchatter = 0;
		}
		elsif ($tmpchatter < 5) {
			$tmpchatter -= 2;
		}
		local($task->{args}{chatter}) = $tmpchatter;

		$task->shell($command . ' 2>&1');

		# if there was a problem, use last to escape the if () BLOCK
		last if not $task->isValid;

		my $result = $task->shell('pget swifttime outtime');
		# Converted time: 2009Apr01 at 08:35:24.658UTC
		if ($result->{output} =~ /^\s*(\d+)\s*(\w+?)\s*(\d+)\s+at\s+([0-9:]+)(\.\d*)?/) {
			my $year = $1;
			my $mword = $2;
			my $month = $MONTH{uc($mword)};
			my $dom = $3;
			my $tod = $4;
			my $frac = $5;
			$datetime = sprintf('%d-%02d-%02dT%s', $year, $month, $dom, $tod);
			if ($args{subs}) {
				$datetime .= substr(sprintf("%.$args{subs}f", $frac), 1);
			}
		}
		else {
			$task->warning("unable to recognize swifttime outtime value '$result->{output}'");
		}
	}

	if (not $datetime) {
		# try using sec2time
		my $command = $task->buildCommand('sec2time',
				offset => $met,
				leapfile => "$ENV{HEADAS}/refdata/leapsec.fits",
				datezero => '2001-01-01',
				);

		local($task->{args}{chatter}) = 0;
		my $result = $task->shell($command);
		foreach my $line (@{ $result->{lines} }) {
			if ($line =~ /^\s*is date and time.+?= (\S+)\s+([0-9:]{8})(\.\d+)?/) {
				my $frac = $3 || 0;
				$datetime = $1 . 'T' . $2;
				if ($args{subs}) {
					$datetime .= substr(sprintf("%.$args{subs}f", $frac), 1);
				}
			}
		}
	}

	if (not defined($datetime)) {
		$datetime = 'YYYY-DD-MMThh:mm:ss';
		$task->error(BAD_INPUT,
				"unable to convert $met to $datetime\n$result->{lines}");
	}

	return $datetime;
}


sub utc2met
{
	my ($task, $utc) = @_;

	my $date;
	my $time;
	my ($year, $mon, $day);
	if ($utc =~ /^((\d{4})-(\d{2})-(\d{2}))T(\d{2}:\d{2}:\d{2}(\.\d+)?)$/) {
		$date = $1;
		($year, $mon, $day) = ($2, $3, $4);
		$time = $5;
	}
	else {
		$task->error(BAD_INPUT, "utc2met: invalid UTC $utc");
		return;
	}

	my $met = undef;
	my $result = undef;

	# As of 2013/03/12 try to use swifttime for conversions from UTC to MET
	# even though this is inconsistent with met2utc.
	if (1) {
		my $formUTC = sprintf('%d%s%02d at %s', $year, $MONTH_NAME[$mon-1], $day, $time);
		my $command = $task->buildCommand('swifttime',
				intime => $formUTC,
				insystem => 'UTC',
				informat => 'c',
				outsystem => 'MET',
				outformat => 's',
				);

		my $tmpchatter = $task->args->{chatter};
		if ($tmpchatter < 3) {
			$tmpchatter = 0;
		}
		elsif ($tmpchatter < 5) {
			$tmpchatter -= 2;
		}
		local($task->{args}{chatter}) = $tmpchatter;

		$task->shell($command . ' 2>&1');

		# if there was a problem, use last to escape the if () BLOCK
		last if not $task->isValid;

		my $result = $task->shell('pget swifttime outtime');
		if ($result->{output} =~ /^\s*(\d+\.\d+)/) {
			$met = $1;
		}
	}

	if (not $met) {
		my $command = $task->buildCommand('time2sec',
			date => $date,
			time => $time,
			leapfile => "$ENV{HEADAS}/refdata/leapsec.fits",
			datezero => '2001-01-01',
			);

		local($task->{args}{chatter}) = 0;
		$result = $task->shell($command);
		if ($task->isValid) {
			$result = $task->shell('pget time2sec offset');
			if ($result->{lines}[0] =~ /\d/) {
				$met = $result->{lines}[0];
				chomp($met);
			}
		}
	}

	if (not defined($met)) {
		$result ||= { };
		$task->error(BAD_INPUT,
				"unable to convert $utc to MET\n--- $result->{command}\n$result->{output}\n---");
	}

	return $met;
}


1;

