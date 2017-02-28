# $Revision: 1.13 $
# $Source: /headas/headas/attitude/tasks/tristarid/StarID/SourceTable.pm,v $
# $Date: 2012/08/16 15:59:03 $
#
# $Log: SourceTable.pm,v $
# Revision 1.13  2012/08/16 15:59:03  rwiegand
# Allow non-positive matchtol to skip catalog cross-referencing.
#
# Revision 1.12  2011/03/02 15:55:01  rwiegand
# Fixed moving to a user-specified table extension.  Tweaked logging.
#
# Revision 1.11  2011/03/02 15:10:00  rwiegand
# Transferring columns was broken because rows (for nearby catalog sources)
# could be inserted during the source table post-processing.  The fix is
# to rewrite all column data.
#
# Revision 1.10  2009/03/31 14:24:54  rwiegand
# Corrected TUNIT values to follow conventions.
#
# Revision 1.9  2008/10/17 14:35:36  rwiegand
# Updated TDISPn for RA and DEC columns.
#
# Revision 1.8  2008/03/28 15:24:35  rwiegand
# Removed stray newline.
#
# Revision 1.7  2007/10/05 19:14:57  rwiegand
# Added a column for use in sorting.
#
# Revision 1.6  2006/03/27 18:15:09  rwiegand
# When an observation has been matched to a catalog object, populate the
# XXX_DELTA columns based on distance of detection from XXX position.
#
# Revision 1.5  2006/02/22 15:43:46  rwiegand
# Bound searches on catalogs.
#
# Revision 1.4  2006/01/25 21:27:11  rwiegand
# Implemented radius filtering within Catalog::contents.
#
# Revision 1.3  2006/01/25 15:23:46  rwiegand
# Extended SearchCat module to support many scat catalag interfaces.  Allow
# user to pass filter and epoch into catalog loading.  Use Wayne's magnitude
# estimation for UVOT filters when loading USNO B1.
#
# Revision 1.2  2005/12/01 19:43:59  rwiegand
# Report the distance to the nearest available catalog entry in the
# CAT_DELTA column.
#
# Revision 1.1  2005/10/07 20:42:32  rwiegand
# Source catalog modules.
#
# Revision 1.18  2005/04/04 18:12:00  rwiegand
# Added specs for coincidence loss corrected source rate and error.
#
# Revision 1.17  2005/03/21 14:29:40  rwiegand
# Display two more digits of RA and DEC.
#
# Revision 1.16  2005/03/11 22:08:31  rwiegand
# Write NULL MAG_DELTA if catalog object magnitude is not known.
#
# Revision 1.15  2005/03/04 19:40:20  rwiegand
# Calculate a tighter bound on the sub-catalog to load.  Make use of the
# partition description when creating the catalog.
#
# Revision 1.14  2005/02/08 21:48:43  rwiegand
# Added primary sort of observed version catalog sources followed by
# sub-sort by distance from reference position.
#
# Revision 1.13  2005/02/08 21:01:46  rwiegand
# Was failing to pass NULL value when reading column data.
#
# Revision 1.12  2004/12/09 21:42:52  rwiegand
# Switched errors in RA and Dec from degrees to arcsec.
#
# Revision 1.12  2004/12/09 21:21:28  rwiegand
# Switched errors in RA and Dec from variance in degrees to linear arcsec.
#
# Revision 1.11  2004/12/07 22:27:33  rwiegand
# Changed area of background rate to arcsec^2 instead of pixel.
#
# Revision 1.10  2004/12/07 21:18:53  rwiegand
# Renamed background column to indicate rate and updated rate column units.
#
# Revision 1.9  2004/11/03 20:34:21  rwiegand
# Store all source list column specifications locally.  Support for
# propagating columns.
#
# Revision 1.8  2004/06/17 21:17:35  rwiegand
# Use REFID which is harder for user to screw with than row number.
#
# Revision 1.7  2004/05/10 14:10:12  rwiegand
# User keywords are handled externally.  Support passing the FITS object back
# to the caller.
#
# Revision 1.6  2004/05/03 21:54:13  rwiegand
# Allow caller to specify units [radians or degrees].
#
# Revision 1.5  2004/04/30 14:59:39  rwiegand
# Added REFID and STARID columns.
#
# Revision 1.4  2004/04/22 20:55:59  rwiegand
# Initialize to an empty catalog if one is not provided.
#
# Revision 1.3  2004/02/13 22:37:55  rwiegand
# Updated columns in output source list.  Allow caller to specify a list of
# reference positions for which the separation of each source will be reported.
#
# Revision 1.2  2004/01/05 16:44:56  rwiegand
# Express RA and DEC in degrees instead of radians.
#
# Revision 1.1  2003/11/26 20:00:19  rwiegand
# Modules for sharing among tasks.
#

use strict;

package StarID::SourceTable;

use base qw(Task::FITS);
use Task qw(:codes);

use Astro::FITS::CFITSIO qw(:constants);
use SimpleFITS;

use StarID::Source;
use StarID::Catalog;

use constant EXTNAME => 'SOURCES';


# we want to sort observed objects before catalog objects
use constant CLASS_OBSERVED   => 1;
use constant CLASS_REFERENCE  => 2;

use constant REF_NULL => -1;
use constant POS_NULL => -1;
use constant SKY_NULL => -999;
use constant MAG_NULL => 99;
use constant TYPE_NULL => '';
use constant ID_NULL => '';


my @PRE_COLUMNS = (
	{ name => 'RA',
		form => 'D',
		comment => 'Right Ascension',
		units => 'deg',
		display => 'F11.6',
	},
	{ name => 'DEC',
		form => 'D',
		comment => 'Declination',
		units => 'deg',
		display => 'F11.6',
	},
	{ name => 'RA_ERR',
		form => 'D',
		comment => 'Error in RA',
		units => 'arcsec',
		display => 'F8.4',
		# display => 'E13.6',
		null => SKY_NULL,
	},
	{ name => 'DEC_ERR',
		form => 'D',
		comment => 'Error in DEC',
		units => 'arcsec',
		display => 'F8.4',
		# display => 'E13.6',
		null => SKY_NULL,
	},
	{ name => 'REFID',
		form => 'I',
		comment => 'Source number',
		null => REF_NULL,
	},
	{ name => 'STARID',
		form => '16A',
		comment => 'Catalog identifier',
		null => ID_NULL,
	},
	{ name => 'MAG',
		form => 'E',
		units => 'mag',
		comment => 'Magnitude',
		null => MAG_NULL,
		display => 'F6.3',
	},
	{ name => 'MAG_ERR',
		form => 'E',
		units => 'mag',
		comment => 'Error in MAG',
		null => MAG_NULL,
		display => 'F6.3',
		# display => 'E11.4',
	},
);

my @POST_COLUMNS = (
	{ name => 'CAT_DELTA',
		form => 'E',
		units => 'arcsec',
		null => POS_NULL,
		comment => 'Distance from catalog object',
		display => 'F6.2',
	},
	{ name => 'MAG_DELTA',
		form => 'E',
		comment => 'Magnitude difference from catalog object',
		null => MAG_NULL,
		display => 'F6.2',
	},
	{ name => 'TYPE',
		form => '12A',
		# catalog extended, binary, ...
		null => TYPE_NULL,
		comment => 'Object type: point, extended',
	},
	{ name => 'PACKET',
		form => '12A',
		comment => 'Source packet indicator',
		null => TYPE_NULL,
	},
	{ name => 'RATE',
		form => 'E',
		comment => 'Source count rate',
		units => 'count/s',
		null => MAG_NULL,
		display => 'F8.4',
	},
	{ name => 'RATE_ERR',
		form => 'E',
		comment => 'Error in RATE',
		units => 'count/s',
		null => MAG_NULL,
		display => 'F6.4',
	},
	{ name => 'RATE_BKG',
		form => 'E',
		comment => 'Background count rate',
		units => 'count/s/arcsec**2',
		null => MAG_NULL,
		display => 'F6.4',
	},
	{ name => 'SATURATED',
		form => 'L',
		comment => 'Saturated source',
	},
	{ name => 'FLUX',
		form => 'E',
		comment => 'Flux',
		units => 'erg/s/cm**2/angstrom',
		null => MAG_NULL,
		display => 'E13.6',
	},
	{ name => 'FLUX_ERR',
		form => 'E',
		comment => 'Error in FLUX',
		units => 'erg/s/cm**2/angstrom',
		null => MAG_NULL,
		display => 'E13.6',
	},
	{ name => 'X_IMAGE',
		form => 'E',
		comment => 'Object position along x',
		units => 'pixel',
		null => POS_NULL,
		display => 'F8.2',
	},
	{ name => 'X_ERR',
		form => 'E',
		comment => 'Error in X_IMAGE',
		units => 'pixel',
		null => POS_NULL,
		display => 'F8.4',
	},
	{ name => 'Y_IMAGE',
		form => 'E',
		comment => 'Object position along y',
		units => 'pixel',
		null => POS_NULL,
		display => 'F8.2',
	},
	{ name => 'Y_ERR',
		form => 'E',
		comment => 'Error in Y_IMAGE',
		units => 'pixel',
		null => POS_NULL,
		display => 'F8.4',
	},
	{ name => 'PROF_MAJOR',
		form => 'E',
		comment => 'Profile RMS along major axis',
		units => 'arcsec',
		null => SKY_NULL,
		display => 'F6.2',
	},
	{ name => 'PROF_MINOR',
		form => 'E',
		comment => 'Profile RMS along minor axis',
		units => 'arcsec',
		null => SKY_NULL,
		display => 'F6.2',
	},
	{ name => 'PROF_THETA',
		form => 'E',
		comment => 'Position angle (CCW/world-x)',
		units => 'deg',
		null => SKY_NULL,
		display => 'F8.2',
	},
	{ name => 'THRESHOLD',
		form => 'E',
		comment => 'Detection threshold above background',
		units => 'sigma',
		display => 'F8.4',
		null => POS_NULL,
	},
	{ name => 'FLAGS',
		form => 'J',
		comment => 'Extraction flags',
		null => POS_NULL,
	},
	{ name => 'ORDER',
		form => 'J',
		comment => 'Sort order',
		null => POS_NULL,
	},
);


my @COLUMNS = (@PRE_COLUMNS, @POST_COLUMNS);
my %COLUMNS = map { $_->{name} => $_ } @COLUMNS;



sub getSpec
{
	my ($task, $colname) = @_;

	my $spec = undef;

	if (exists($COLUMNS{$colname})) {
		my %spec = %{ $COLUMNS{$colname} };
		$spec = \%spec;
	}
	elsif ($colname =~ /^(\w+)_DELTA$/) {
		my $tag = $1;
		$spec = {
			name => $tag . '_DELTA',
			form => 'E',
			units => 'arcsec',
			null => POS_NULL,
			comment => "Distance from $tag position",
			display => 'F6.2',
		};
	}
	else {
		$task->warning("StarID::SourceTable: invalid column '$colname'");
	}

	return $spec;
}


sub execute
{
	my ($self, %args) = @_;

	foreach my $key (qw(leaveOpen)) {
		if ($args{$key}) {
			$self->warning("SourceTable.execute: $key is obsolete");
		}
	}

	$self->validate;

	$self->writeSourceList(%args)
		if $self->isValid;
}


sub validate
{
	my ($self) = @_;

	# make sure all the options are set
	$self->{unobserved} ||= 0;

	$self->{reference} ||= [ ];
	$self->{catalog} ||= StarID::Catalog->new;

	my $count = @{ $self->{sources} };

	if (@{ $self->{reference} } > 0) {
		$self->{target} = $self->{reference}[0];
	}
	elsif ($count > 0) {
		# this can fail if RA crosses 0/360
		my $raSum = 0;
		my $decSum = 0;
		$self->applyToSources(sub {
				my ($s) = @_;
				$raSum += $s->ra;
				$decSum += $s->dec;
			});

		$self->{target} = StarID::Source->new(
				RA => $raSum / $count,
				DEC => $decSum / $count,
				);
	}

	if (my $target = $self->{target}) {
		$target->{UNIT} = Math::rd2unit($target->ra, $target->dec);
	}
	else {
		$target->{UNIT} = [ 1, 0, 0 ];
	}
}


sub determinePacketIndicator ($)
{
	my ($s) = @_;

	my $type = TYPE_NULL;

	if ($s->{stamp}) {
		$type = $s->{stamp};
	}
	elsif ($s->{binning}) {
		$type = $s->{binning};
	}

	return $type;
}


sub applyToSources
{
	my ($self, $method) = @_;

	foreach my $s (@{ $self->{sources} }) {
		if (ref($method) eq 'CODE') {
			$method->($s);
		}
		else {
			$self->$method($s);
		}
	}
}


sub isNumeric
{
	my ($s) = @_;
	return 0 if not defined($s);
	if ($s =~ /^\d+(\.\d*)?(e[-+]?\d+)?$/) {
		return 1;
	}
	if ($s =~ /^\d*(\.\d+)?([eE][-+]?\d+)?$/) {
		return 1;
	}
	return 0;
}


sub printEntry
{
	my ($label, $entry) = @_;

	print "$label:",
		"\n\tSTARID=$entry->{STARID}",
		"\n\tREFID=$entry->{REFID}",
		"\n\tRAW_TOT_RATE=$entry->{RAW_TOT_RATE}",
		"\n";
}


sub putSourceList
{
	my ($self, $fits, %args) = @_;

	my $status = 0;
	my @entries;
	my %matched;

	# add entries for observed sources
	my $matchrad = Math::degreesToRadians($self->{matchtol} / 3600);

	my @refColumns;
	foreach my $p (@{ $self->{reference} }) {

		my $name = uc($p->id);
		$p->{colname} = $name . '_DELTA';

		push(@refColumns, {
				name => $p->{colname},
				computed => 1,
				form => 'E',
				units => 'arcsec',
				null => POS_NULL,
				comment => "Distance from $name position",
				display => 'F6.2',
			},
			);
	}

	my @allColumns = (@PRE_COLUMNS, @refColumns, @POST_COLUMNS);
	my %columns = map { ($_->{name} => $_) } @allColumns;

	if ($args{columns}) {
		foreach my $col (@{ $args{columns} }) {
			my $colname = $col->{name};
			if (not $columns{$colname}) {
				push(@allColumns, $col);
			}
			else {
			}
			$columns{$colname} = $col;
		}
	}

	my $catSize = $self->{catalog}->size;

	$self->applyToSources(sub {
		my ($s) = @_;

		my @delta;
		foreach my $p (@{ $self->{reference} }) {
			my $delta = $s->angle($p);
			push(@delta, $p->{colname} => $delta);
		}

		my $packet = $s->{PACKET} || TYPE_NULL;

		if ($matchrad <= 0 or $ENV{STARID_SKIP_CATALOG}) {
			my %entry = (%$s,
				PACKET => $packet,
				@delta,
			);
			push(@entries, \%entry);
			return;
		}

		my $matches = 0;
		# find which objects in the catalog it is within sepangle of

		foreach my $o (@{ $self->{catalog}->contents($s, $matchrad) }) {
			++$matches;
			$matched{$o} = 1;
			$s->{MATCHED} = 1;
			my $delta = isNumeric($o->mag) ? $s->mag - $o->mag : MAG_NULL;
			my %entry = (%$s,
				STARID => $o->id,
				PACKET => $packet,
				TYPE => $o->type,
				CAT_DELTA => $s->angle($o),
				MAG_DELTA => $delta,
				@delta,
			);
			push(@entries, \%entry);
		}

		if (not $matches) {
			foreach my $p (@{ $self->{reference} }) {
				my $delta = $s->angle($p);
				push(@delta, $p->{colname} => $delta);
			}

			# find nearest catalog object
			my $maxcos = -2;
			my $scale = 1;
			for (my $i = 0; $maxcos == -2 and $i < 4; ++$i) {
				$scale *= 3;
				my $list = $self->{catalog}->contents($s, $scale * $matchrad);
				foreach my $o (@$list) {
					my $cosangle = $s->cosangle($o);
					if ($cosangle > $maxcos) {
						$maxcos = $cosangle;
					}
				}
				if (@$list == $catSize) {
					# explored all objects in catalog
					last;
				}
			}

			my $catDelta = ($maxcos == -2) ? POS_NULL : POSIX::acos($maxcos);

			my %entry = (%$s,
				STARID => ID_NULL,
				PACKET => $packet,
				TYPE => TYPE_NULL,
				CAT_DELTA => $catDelta,
				MAG_DELTA => MAG_NULL,
				@delta,
			);
			push(@entries, \%entry);
		}
	});

	# report unobserved sources
	my $args = $self->args;

	my @unmatched;
	if ($matchrad > 0 and $self->{unobserved} > 0 and $self->{target}) {

		my $target = $self->{target};

		my $radians = Math::degreesToRadians($self->{unobserved} / 3600);
		my $uncos = cos($radians);

		foreach my $o (@{ $self->{catalog}->contents }) {

			next if $matched{$o};

			next if $target->cosangle($o) < $uncos;

			my @delta;

			foreach my $p (@{ $self->{reference} }) {
				my $delta = $p->angle($o);
				push(@delta, $p->{colname} => $delta);
			}

			push(@unmatched, {
					REFID => REF_NULL,
					STARID => $o->id,
					PACKET => TYPE_NULL,
					RA => $o->ra,
					DEC => $o->dec,
					MAG => isNumeric($o->mag) ? $o->mag : MAG_NULL,
					TYPE => $o->type,
					CAT_DELTA => POS_NULL,
					MAG_DELTA => MAG_NULL,
					@delta,
				});
		}
	}

	# only keep brightest unmatched entries
	if ($self->{uncount} > 0) {
		my @sorted = sort { $a->{MAG} <=> $b->{MAG} } @unmatched;

		my $limit = $self->{uncount};
		foreach my $o (@sorted) {
			if ($o->{MAG} < $self->{unmag}) {
				push(@entries, $o);
				last if --$limit == 0;
			}
			last if $o->{MAG} > $self->{unmag};
		}
	}

	my $targunit = $self->{target}{UNIT};

	foreach my $e (@entries) {

		my $unit = Math::rd2unit($e->{RA}, $e->{DEC});

		$e->{RA} = Math::radiansToDegrees($e->{RA});
		$e->{DEC} = Math::radiansToDegrees($e->{DEC});

		$e->{distance} = Math::u3angle($unit, $targunit);

		# convert separations to arcsecs
		$e->{CAT_DELTA} = 3600 * Math::radiansToDegrees($e->{CAT_DELTA})
			if $e->{CAT_DELTA} != POS_NULL;

		foreach my $r (@{ $self->{reference} }) {
			my $key = $r->{colname};
			$e->{$key} = 3600 * Math::radiansToDegrees($e->{$key})
				if $e->{$key} != POS_NULL;
		}
	}

	my %sources = map { $_->{REFID} => $_ } @entries;
	foreach my $e (@entries) {
		# make other fields available
		if ($e->{REFID} == REF_NULL) {
			$e->{class} = CLASS_REFERENCE;
		}
		else {
			$e->{class} = CLASS_OBSERVED;
		}
		my $s = $sources{$e->{REFID}};
		foreach my $c (@PRE_COLUMNS, @POST_COLUMNS) {
			my $tag = $c->{name};
			if (not defined($e->{$tag})) {
				if (defined($s) and defined($s->{$tag})) {
					$e->{$tag} = $s->{$tag};
				}
				else {
					$e->{$tag} = $c->{null};
				}
			}
		}
	}

	# sort entries by class and then distance from target
	my @sorted = sort {
			$a->{class} <=> $b->{class}
			or $a->{distance} <=> $b->{distance}
			or $a->{STARID} <=> $b->{STARID}
			or $a->{REFID} <=> $b->{REFID}
		} @entries;
	{
		my $i = 0;
		foreach my $s (@sorted) {
			$s->{ORDER} = ++$i;
		}
	}

	foreach my $col (@allColumns) {
		my $colname = $col->{name};
		my @data = map { $_->{$colname} } @entries;
		$self->writeColumn($fits, $col, \@data);
	}
}


sub writeSourceList
{
	my ($self, %args) = @_;

	my $path = $self->{outfile};
	my $extname = $args{extname} || $self->{extname} || EXTNAME;

	my $status = 0;

	my $simple = $self->{simple};
	if (not $simple) {
		$simple = SimpleFITS->create($path);
		$status = $simple->status;
		if ($status) {
			$self->error(BAD_OUTPUT,
				"unable to create $path [$status]");
			return;
		}
	}

	my $fits = $simple->handle;

	if ($fits->create_tbl(BINARY_TBL, 0, 0, 0, 0, 0, $extname, $status)) {
		$self->error(BAD_OUTPUT,
			"unable to create binary table");
	}

	if ($args{scale} eq 'degrees') {
		my $degreesToRadians = Math::degreesToRadians(1);
		$self->applyToSources(sub {
				my ($s) = @_;
				$s->{RA} *= $degreesToRadians;
				$s->{DEC} *= $degreesToRadians;
			});
	}

	$self->putSourceList($fits, %args)
		if $self->isValid;

	if ($args{scale} eq 'degrees') {
		my $radiansToDegrees = Math::radiansToDegrees(1);
		$self->applyToSources(sub {
				my ($s) = @_;
				$s->{RA} *= $radiansToDegrees;
				$s->{DEC} *= $radiansToDegrees;
			});
	}

	if (not $self->{simple}) {
		if ($fits->close_file($status)) {
			$self->error(BAD_OUTPUT,
					"unable to close source list [$status]");
		}
	}

	$self->report("wrote source list to $path")
		if $self->isValid;

}


sub loadFile
{
	my ($self, $path, %args) = @_;

	if ($args{leaveOpen}) {
		$self->warning("SourceTable.loadFile: leaveOpen obsolete");
	}

	my $simple = $args{simple};
	if (not $simple) {
		$simple = SimpleFITS->readonly($path);
	}

	my $status = $simple->status;
	if ($status) {
		$self->error(BAD_INPUT, "unable to open $path [$status]");
		return;
	}

	if ($args{extname}) {
		$status = $simple->move($args{extname})->status;
		if ($status) {
			$self->error(BAD_INPUT,
					"unable to move to extension '$args{extname}' [$status]");
		}
	}

	$self->loadFITS($simple, %args)
		if $self->isValid;

	if (not $args{simple}) {
		$status = $simple->close->status;
	}

	return $self->{sources};
}


sub loadFITS
{
	my ($self, $simple, %args) = @_;

	my @required;
	if ($args{columns}) {
		@required = @{ $args{columns} };
	}
	else {
		foreach my $col (@COLUMNS) {
			if ($col->{required}) {
				push(@required, $col->{name});
			}
		}
	}

	my @sources;
	$simple->loadtable(\@sources);

	my $status = $simple->status;
	if ($status) {
		$self->error(BAD_INPUT,
			"unable to load source list [$status]");
		return;
	}

	my $count = scalar(@sources);
	$self->report("loaded $count sources");

	my $row = 0;
	foreach my $s (@sources) {
		$s->{ROW} = ++$row;
		bless($s, 'StarID::Source');
	}

	$self->{sources} = \@sources;

	if ($args{scale} eq 'radians') {
		my $degreesToRadians = Math::degreesToRadians(1);
		$self->applyToSources(sub {
				my ($s) = @_;
				$s->{RA} *= $degreesToRadians;
				$s->{DEC} *= $degreesToRadians;
			});
	}

	my $ncol = $simple->ncols;
	$self->report("source list has $ncol columns")
		if $self->chatter(4);

	my $header;
	$status = $simple->readheader($header, clean => 1)->status;

	my @columns;
	my %MY_KEY = (
		TTYPE => 'name',
		TFORM => 'form',
		TUNIT => 'units',
		TDISP => 'display',
	);

	for (my $i = 1; $i <= $ncol; ++$i) {
		my %spec;
		my ($ttype, $tform, $tunit, $tnull);
		my $comment = $header->{COMMENTS}{'TTYPE' . $i};
		foreach my $key (qw(TTYPE TFORM TUNIT TNULL TDISP)) {
			my $keyn = $key . $i;
			if (defined($header->{$keyn})) {
				my $myKey = $MY_KEY{$key};
				$spec{$myKey} = $header->{$keyn};
			}
		}
		if ($spec{name} and $spec{form}) {
			$spec{comment} = $comment;
			push(@columns, \%spec);
		}
	}

	$self->{columns} = \@columns;

	my @colnames = map { $_->{name} } @columns;

	$self->{loaded} = \@colnames;
	$self->verbose("source list columns: ", join(',', @colnames))
		if $self->chatter(4);

	my %loaded = map { ($_ => 1) } @colnames;

	foreach my $required (@required) {
		if (not $loaded{$required}) {
			$self->error(BAD_INPUT,
				"source list missing required column $required");
		}
	}

	return $self->{sources};
}


1;

