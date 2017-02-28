# $Source: /headas/headas/swift/uvot/tasks/uvotgcn/UVOT/GCN.pm,v $
# $Revision: 1.3 $
# $Date: 2006/01/17 21:27:35 $
#
# $Log: GCN.pm,v $
# Revision 1.3  2006/01/17 21:27:35  rwiegand
# Take EXPID into account when determining uniquer.
#
# Revision 1.2  2005/12/16 16:35:54  rwiegand
# Made library of Frank Marshall's afterglow candidate codes.
#
# Revision 1.1  2005/11/21 18:50:37  rwiegand
# Module containing routines for UVOT GCN processing.
#

use strict;

package UVOT::GCN;

use FileHandle;
use Fcntl qw(:flock);
use File::Basename;
use File::Copy;

use Task qw(:codes);
use DataIO;


use constant PACKET_SRCLIST => 'SRCLIST';
use constant PACKET_IMAGE => 'IMAGE';


# default output extensions
my %DEFAULT_EXT = (
	job => 'info',
	log => 'log',
	find => 'log',
	packet => 'fits',
	sky => 'fits.gz',
	sources => 'fits.gz',
	catalog => 'fits.gz',
	field => 'ps.gz',
	signal => 'txt',
);



sub acquireLock
{       
	my ($task, $name) = @_;
	my $fh = FileHandle->new($name, 'w')
		or die("unable to create $name [$!]");

	flock($fh, LOCK_EX); 
	$task->{locks}{$name} = $fh;
	$task->report("acquireLock: locked $name");
}
        
        
sub releaseLock
{
	my ($task, $name) = @_;
	my $locks = $task->{locks};
	if (my $fh = $locks->{$name}) {
		flock($fh, LOCK_UN);
		delete($locks->{$name});
		$task->report("releaseLock: unlocked $name");
	}
	else {
		$task->error(BAD_TASK, "releaseLock: do not have $name lock");
	}
}


sub locateTriggerInfo
{
	my ($task, %args) = @_;

	my $problem;

	foreach my $key (qw(ARCHIVE TARG_ID)) {
		if (not $args{$key}) {
			$problem = "locateTriggerInfo: missing $key";
		}
	}
	return $problem if $problem;

	my $trigdir = "$args{ARCHIVE}/$args{TARG_ID}";
	if (not -d $trigdir) {
		mkdir($trigdir)
			or $task->warning("unable to create directory $trigdir [$!]");
	}

	if (not -d $trigdir) {
		$problem = "missing trigger directory $trigdir";
	}
	else {
		$task->{triggerDir} = $trigdir;
		$task->{triggerFile} = "$trigdir/$args{TARG_ID}.info";
		$task->{triggerLock} = "$trigdir/$args{TARG_ID}.lock";
	}

	return $problem;
}



sub loadJobInfo
{
	my ($task, $path) = @_;

	my $job;

	eval {
		my $io = DataIO->new;
		$job = $io->getPath($path, single => 'HASH');
	};

	if ($@) {
		$task->error(BAD_INPUT, "unable to load $path: $@");
	}

	return $job;
}


sub loadTriggerInfo
{
	my ($task, %args) = @_;

	if ($args{lock}) {
		acquireLock($task, $task->{triggerLock});
	}

	my @info;
	my $path = $task->{triggerFile};

	eval {
		my $io = DataIO->new;
		@info = $io->getPath($path);
	};

	if ($@) {
		$task->error(BAD_INPUT, "unable to load $path: $@");
	}

	if ($args{lock}) {
		releaseLock($task, $task->{triggerLock});
	}

	return @info;
}


sub saveInfo
{
	my ($task, $path, @info) = @_;

	eval {
		my $io = DataIO->new;
		$io->putPath($path, @info);
	};

	if ($@) {
		$task->error(BAD_INPUT, "unable to save $path: $@");
	}
}


sub nameOutputFile
{
	my ($task, $which, %args) = @_;

	foreach my $key (qw(TARG_ID TYPE UNIQUER)) {
		if (not $args{$key}) {
			$task->warning("nameOutputFile: missing $key");
			return;
		}
	}

	my $filext = "\L$args{TYPE}.$DEFAULT_EXT{$which}";

	my $arcname = join('_', $args{TARG_ID}, $args{UNIQUER},
				'uvot', $which, $filext);

	return $arcname;
}


sub uniquer
{
    my ($task, $jobInfo, $triggerInfo) = @_;

	my $details = $jobInfo->{PACKET};

	my $count = 0;
	foreach my $info (@{ $triggerInfo }) {
			if ($info->{TARG_ID} == $jobInfo->{TARG_ID}
				and $info->{PACKET}{TYPE} eq $details->{TYPE}
				and $info->{PACKET}{EXPID} eq $details->{EXPID}) {
			++$count;
		}
	}

	my $uniquer = $details->{BAT_TRIG_TIME}
			? sprintf('o%d', $details->{EXPID} - $details->{BAT_TRIG_TIME})
			: "e$details->{EXPID}";
	if ($count) {
		$uniquer .= "_v$count";
	}

	return $uniquer;
}


sub archiveResults
{
	my ($task, @results) = @_;

	foreach my $result (@results) {
		my $outfile = $result->{outfile};
		my $arcfile = $result->{arcfile};
		if (-e $arcfile) {
			my $fixext = '';
			# if an output is named x.fits.gz
			#	rename to x.fits.n.gz instead of x.fits.gz.n
			if (substr($arcfile, -3) eq '.gz') {
				substr($arcfile, -3) = '';
				$fixext = '.gz';
			}
			for (my $postfix = 1; 1; ++$postfix) {
				my $probe = "$arcfile.$postfix$fixext";
				if (not -e $probe) {
					$arcfile = $result->{arcfile} = $probe;
					last;
				}
			}
		}
		if (not File::Copy::copy($outfile, $arcfile)) {
			$task->warning("unable to archive $outfile to $arcfile [$!]");
		}
		else {
			my ($name, $dir) = File::Basename::fileparse($arcfile);
			$result->{arcname} = $name;
		}
	}
}



# attempts to set $jobInfo->{PACKET}{BAT_TRIG_TIME}
sub determineTriggerTime
{
	my ($task, $jobInfo) = @_;

	foreach my $tag (qw(XRT BAT BLC)) {

		my $key = $tag . '_NOTICE';

		if (my $notice = $jobInfo->{$key}) {

			if ($notice->{BAT_TRIG_TIME}) {
				my $details = $jobInfo->{PACKET};
				if (not $details->{BAT_TRIG_TIME}) {
					$details->{BAT_TRIG_TIME} = $notice->{BAT_TRIG_TIME};
					$task->report("stored trigger time $details->{BAT_TRIG_TIME}");
				}
				elsif (abs($details->{BAT_TRIG_TIME} - $notice->{BAT_TRIG_TIME}) > 1) {
					$task->warning("$key has different trigger time [$notice->{BAT_TRIG_TIME}");
				}
			}
		}
		else {
			$task->warning("determineTriggerTime: missing $key");
		}
	}
}


1;

