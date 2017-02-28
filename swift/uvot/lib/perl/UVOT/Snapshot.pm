# $Source: /headas/headas/swift/uvot/lib/perl/UVOT/Snapshot.pm,v $
# $Revision: 1.4 $
# $Date: 2006/07/27 14:52:07 $
#
# $Log: Snapshot.pm,v $
# Revision 1.4  2006/07/27 14:52:07  rwiegand
# Simplified snapshot determination by just checking time separation between
# attitude records.
#
# Revision 1.3  2005/12/07 15:27:20  rwiegand
# Detect large time gaps in attitude file and assume snapshot boundary.
# Added duration field.
#
# Revision 1.2  2005/12/02 23:00:58  rwiegand
# Reimplemented detection of snapshots in analyzeAttitude.  The previous
# approach did not catch snapshots that had began or ended with the
# attitude file.
#
# Revision 1.1  2005/05/19 13:03:20  rwiegand
# Determine snapshots from attitude file.
#

use strict;

package UVOT::Snapshot;

use base qw(Task::Subtask);
use Task qw(:codes);

use Astro::FITS::CFITSIO qw(:constants);

# sw*sat.fits[ACS_DATA]
#	TTYPE3 = FLAGS / 1=10arcmin, 2=settled, 3=SAA, 4=SafeHold
use constant TEN_ARCMIN_BIT => (1 << 7);
use constant SETTLED_BIT    => (1 << 6);
# use constant IN_SAA_BIT     => (1 << 5);
# use constant SAFE_HOLD_BIT  => (1 << 4);



package UVOT::Snapshot::Entry;

sub new
{
	my ($class, %args) = @_;
	return bless(\%args);
}


sub start
{
	my ($self) = @_;
	return $self->{START};
}

sub stop
{
	my ($self) = @_;
	return $self->{STOP};
}


package UVOT::Snapshot;


sub analyzeAttitude
{
	my ($self, $path) = @_;

	my $status = 0;
	my $fptr = Astro::FITS::CFITSIO::open_file($path, READONLY, $status);
	if ($status) {
		$self->error(BAD_INPUT, "unable to open $path [$!]");
		return;
	}

	my $count = 0;
	my $colnum = 0;
	my $anynul = 0;
	my $null = -1;
	my @time;


	if ($fptr->movnam_hdu(BINARY_TBL, 'ATTITUDE', 0, $status)) {
		$self->error(BAD_INPUT,
				"unable to move to ATTITUDE table [$!]");
	}
	elsif ($fptr->get_num_rows($count, $status)) {
		$self->error(BAD_INPUT,
				"unable to get number of rows in ATTITUDE table [$!]");
	}
	elsif ($fptr->get_colnum(CASEINSEN, 'TIME', $colnum, $status)) {
		$self->error(BAD_INPUT,
				"unable to locate TIME column in ATTITUDE table [$!]");
	}
	elsif ($fptr->read_col_dbl($colnum, 1, 1, $count, $null,
			\@time, $anynul, $status)) {
		$self->error(BAD_INPUT,
				"unable to read TIME data in ATTITUDE table [$!]");
	}
	else {
		my @snapshots;
		my $start = 0;
		my $stop = 0;
		my $tLast = 0;

		my $storeSnapshot = sub {
			my %data = (
				START => $start,
				STOP => $stop,
				DURATION => $stop - $start,
			);
			push(@snapshots, UVOT::Snapshot::Entry->new(%data));
			$start = $stop = 0;
		};

		for (my $i = 0; $i < $count; ++$i) {

			my $tCurrent = $time[$i];

			if (not $start) {
				$start = $tCurrent;
			}
			elsif ($tCurrent - $tLast > 1000) {
				$stop = $tLast;
				$storeSnapshot->();
			}

			$tLast = $tCurrent;
		}

		if ($start) {
			$stop = $time[-1];
			$storeSnapshot->();
		}

		$self->{snapshots} = \@snapshots;
	}

	$status = 0;
	$fptr->close_file($status);
}


1;

