# $Source: /headas/headas/swift/uvot/lib/perl/UVOT/AspcorrTable.pm,v $
# $Revision: 1.1 $
# $Date: 2009/07/15 18:22:00 $
#
#	Create a UVOT aspect corrections table.
#
#
# $Log: AspcorrTable.pm,v $
# Revision 1.1  2009/07/15 18:22:00  rwiegand
# Extracted code for writing aspect corrections table from uvotskycorr to
# library for use by uvotskycorr and uvotgraspcorr.
#

use strict;

package UVOT::AspcorrTable;

use SimpleFITS;
use Task qw(:codes);
use Astro::FITS::CFITSIO qw(:constants);


my %DEFAULT = (
	OBS_ID => '',
	RA_PNT => -999,
	DEC_PNT => -999,
	PA_PNT => -999,
);


my @COLUMNS = (
	{ name => 'OBS_ID', form => '12A',
			comment => 'Observation ID' },
	{ name => 'EXTNAME', form => '16A',
			comment => 'Exposure extension' },
	{ name => 'ASPCORR', form => 'I',
			comment => 'Aspect correction status' },
	{ name => 'FILTER', form => '8A',
			comment => 'Filter' },

	{ name => 'TSTART', form => 'D', units => 's',
			comment => 'Exposure start time' },
	{ name => 'TSTOP', form => 'D', units => 's',
			comment => 'Exposure stop time' },
	{ name => 'EXPOSURE', form => 'E', units => 's',
			comment => 'Exposure duration' },

	{ name => 'RA_PNT', form => 'E', units => 'deg',
			comment => 'Observation R.A.' },
	{ name => 'DEC_PNT', form => 'E', units => 'deg',
			comment => 'Observation Declination' },
	{ name => 'PA_PNT', form => 'E', units => 'deg',
			comment => 'Observation roll' },

	{ name => 'MATCHES', form => 'I', key => 'ASPCOUNT',
			comment => 'Aspect correction match count' },
	{ name => 'DET_OBJ', form => 'I', key => 'ASPNDET',
			comment => 'Number of detected objects' },
	{ name => 'REF_OBJ', form => 'I', key => 'ASPNREF',
			comment => 'Number of reference objects' },
	{ name => 'RESID_MEAN', form => 'E', key => 'ASPMEAN', units => 'arcsec',
			comment => 'Aspect correction mean residual' },
	{ name => 'RESID_RMS', form => 'E', key => 'ASPRMS', units => 'arcsec',
			comment => 'Aspect correction RMS residual' },
	{ name => 'RESID_SIGMA', form => 'E', key => 'ASPSIGMA', units => 'arcsec',
			comment => 'Aspect correction residual sigma' },
	# { name => 'RESID_MAX', form => 'E', key => 'ASPWORST', units => 'arcsec' },
	{ name => 'CORR_POS', form => 'E', key => 'ASPDELTA', units => 'arcsec',
			comment => 'Aspect correction position change' },
	{ name => 'CORR_ROLL', form => 'E', key => 'ASPROLL', units => 'arcmin',
			comment => 'Aspect correction roll' },
	{ name => 'QDELTA', form => '4D', repeat => 4,
			comment => 'Aspect correction quaternion' },
	{ name => 'QROTATION', form => '4D', repeat => 4,
			comment => 'Aspect correction rotation' },
	{ name => 'WHEELPOS', form => 'I',
			comment => 'Filter wheel position' },
);



# saveCorrections($task, $path, $corrections, %args)
#	$task : Task::FITS object
#	$path : path to write output to
#	$corrections : array ref of corrections [1]
#	%args : specify CARDS, NULL, OBS_ID, {RA,DEC,PA}_PNT
#
#	[1] each correction is a hash reference with keys
#		EXTNAME, FILTER, ASPCORR, TSTART, TSTOP, EXPOSURE,
#		MATCHES, DET_OBJ, REF_OBJ, RESID_MEAN, RESID_RMS, RESID_SIGMA,
#		RESID_MAX, CORR_POS, CORR_ROLL, WHEELPOS, QDELTA, QROTATION
#	with types as given in @COLUMNS.

sub saveCorrections
{
	my ($task, $path, $corrections, %args) = @_;

	my $null = $args{NULL} || -999;

	foreach my $key (qw(OBS_ID RA_PNT DEC_PNT PA_PNT)) {
		if (not defined($args{$key})) {
			$args{$key} = $DEFAULT{$key};
		}
	}

	my $aref = $corrections;
	if (not $aref or not @$aref) {
		$task->report('no corrections to save');
		return;
	}

	my $fits = SimpleFITS->create($path);
	my $status = $fits->status;
	if ($status) {
		$task->warning("unable to create $path [$status]");
		return;
	}

	my $rows = @$aref;
	$fits->handle->create_tbl(BINARY_TBL, $rows, 0, 0, 0, 0, 'ASPCORR', $status);

	foreach my $col (@COLUMNS) {
		$col->{null} = $null;
		my @data;
		foreach my $record (@$aref) {

			foreach my $key (qw(OBS_ID RA_PNT DEC_PNT PA_PNT)) {
				$record->{$key} = $args{$key};
			}

			my $key = $col->{key} || $col->{name};
			my $datum;
			if (defined($record->{$key})) {
				$datum = $record->{$key};
			}
			else {
				if (my $repeat = $col->{repeat}) {
					$datum = [ ($col->{null}) x $repeat ];
				}
				else {
					$datum = $col->{null};
				}
			}

			push(@data, (ref($datum) eq 'ARRAY') ? @$datum : $datum);
		}

		$task->writeColumn($fits->handle, $col, \@data);
	}

	if ($args{CARDS}) {
		foreach my $hdu (1, 2) {
			my $status = 0;
			$fits->move($hdu);
			foreach my $card (@{ $args{CARDS} }) {
				$fits->handle->write_record($card, $status);
			}
			$fits->handle->update_key_str('CREATOR',
					"$task->{tool} $task->{version}",
					'File creation software', $status);
			$fits->handle->write_date($status);
			$task->putParameterHistory($fits->handle);
			$task->updateChecksums($fits->handle);
		}
	}

	$status = $fits->close
			->status;
	if ($status) {
		$task->warning("unable to close file [$status]");
		return;
	}

	return 1;
}


1;

