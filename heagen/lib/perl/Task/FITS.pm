# $Source: /headas/headas/heagen/lib/perl/Task/FITS.pm,v $
# $Revision: 1.1 $
# $Date: 2013/08/27 15:13:44 $
#
#
# $Log: FITS.pm,v $
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
# Revision 1.42  2008/10/16 18:11:11  rwiegand
# Allow avoiding warning if unable to determine WCS information.
#
# Revision 1.41  2008/04/04 21:44:57  rwiegand
# Allow 4-3-3 CTYPEia.
#
# Revision 1.40  2008/03/28 15:25:35  rwiegand
# Was losing CROTA2 in getWCS.
#
# Revision 1.39  2007/11/05 22:58:02  rwiegand
# Do not assume linear pixel/world transformation if CTYPEn are missing.
#
# Revision 1.38  2007/10/31 15:17:28  rwiegand
# When CTYPE1 and CTYPE2 differ only in the last character (e.g., DETX and
# DETY), set TYPE to LINEAR.
#
# Revision 1.37  2006/10/23 15:47:06  rwiegand
# Store file name and directory when parsing FITS URL.
#
# Revision 1.36  2006/03/13 23:20:53  rwiegand
# Fixed typo in LINEAR WCS projection.
#
# Revision 1.35  2006/01/25 16:46:24  rwiegand
# Support logical columns.
#
# Revision 1.34  2005/11/02 15:44:59  rwiegand
# Execute commands using Task::shell.
#
# Revision 1.33  2005/09/14 17:11:35  rwiegand
# When preparing CALDB queries, allow DATE as a fallback for DATE-OBS.
#
# Revision 1.32  2005/09/13 18:22:33  rwiegand
# Added reporting of CALDB query input and output.
#
# Revision 1.31  2005/08/29 11:42:07  rwiegand
# Report message tweak.
#
# Revision 1.30  2005/06/20 12:11:11  rwiegand
# Fix key class returned by fits_get_keyclass for some unrecognized WCS keys.
#
# Revision 1.29  2005/05/18 21:12:18  rwiegand
# Implemented HDgtcalf based queryCALDB.
#
# Revision 1.28  2005/04/04 15:57:01  rwiegand
# CFITSIO does not recognize PCi_ja [WCS] keys
#
# Revision 1.27  2005/03/04 19:17:03  rwiegand
# Support for converting linearly between FITS and world coordinates.
#
# Revision 1.26  2005/01/21 22:04:30  rwiegand
# Addressed a problem with long keycards resulting in bogus keycards.
#
# Revision 1.25  2005/01/21 19:35:36  rwiegand
# Use ftemplate to create empty primary HDU.
#
# Revision 1.24  2004/12/29 21:18:54  rwiegand
# Added routine to remove quotes from string keyword values.  Extended
# createEmptyFITS to optionally insert header keywords.
#
# Revision 1.23  2004/11/03 21:00:55  rwiegand
# Better PIL support.
#
# Revision 1.22  2004/10/17 11:37:08  rwiegand
# Ensure variable is defined before pattern match.
#
# Revision 1.21  2004/10/12 22:44:48  rwiegand
# Assume CTYPE[12] = ...[XY] give Cartesian coordinates.
#
# Revision 1.20  2004/09/17 18:54:30  rwiegand
# Added getWCS that can handle both primary and alternate WCS keywords.
#
# Revision 1.19  2004/09/02 19:37:49  rwiegand
# Unable to deal with parameters without default values.
#
# Revision 1.18  2004/09/01 13:39:18  rwiegand
# Removed modification of PFILES environment variable since it was causing
# so much trouble.
#
# Revision 1.17  2004/08/09 22:12:19  rwiegand
# Grabbed the PFILES pre- instead of post- semicolon.
#
# Revision 1.16  2004/08/05 21:19:34  rwiegand
# Implemented workaround to perl bug modifying %ENV.
#
# Revision 1.15  2004/08/05 19:50:17  rwiegand
# Disabled the code for temporarily modifying PFILES environment variable
# since it caused perl to SEGV.  Added interface to fits_pix_to_world and
# fits_world_to_pix.
#
# Revision 1.14  2004/07/09 15:55:34  rwiegand
# Added CALDB interface routine.
#
# Revision 1.13  2004/06/30 18:20:28  rwiegand
# Overrode buildCommand to set all tool parameters.
#
# Revision 1.12  2004/06/17 21:11:03  rwiegand
# Added support for short (int) column data.  Call fits_set_btblnull for
# integer column data with NULLs.
#
# Revision 1.11  2004/05/10 14:08:47  rwiegand
# Added support for collecting key cards by type and updating checksums.
#
# Revision 1.10  2004/04/08 17:34:26  rwiegand
# Added routines to grab FITS header cards and parse a FITS file name.
#
# Revision 1.9  2004/03/04 16:28:26  rwiegand
# Missed a line manually merging versions for the last change.
#
# Revision 1.8  2004/03/04 15:57:42  rwiegand
# Reworked support for NULLs in tables.  Added function to retrieve/store
# selected keywords.
#
# Revision 1.7  2004/01/13 16:15:01  rwiegand
# Support for negative values.
#
# Revision 1.6  2004/01/05 16:46:30  rwiegand
# Added support for writing null values into tables.
#
# Revision 1.5  2003/11/26 19:50:42  rwiegand
# Added support for string columns.
#
# Revision 1.4  2003/09/30 18:48:43  rwiegand
# Use fits_update_key instead of fits_write_key to avoid duplicate keywords.
# Modified putKeywords to support keywords with double values.
#
# Revision 1.4  2003/09/30 18:45:21  rwiegand
# Added support for keywords with double values.
#
# Revision 1.3  2003/09/30 17:25:30  rwiegand
# Added support for writing blocks of keywords and (table) column data.
#
# Revision 1.2  2002/07/17 20:03:29  wiegand
# Directly inherit from Task
#
# Revision 1.1  2002/06/27 20:19:15  wiegand
# Initial revision
#
#

package Task::FITS;

use strict;

use base qw(Task);

use FileHandle;

use Task qw(:codes);
use Math;
use Astro::FITS::CFITSIO qw(:longnames :constants);
use HEACORE::HEAUTILS;

use constant LINEAR => 'linear';


my %INTEGER_TYPE = (
	byt => 1,
	sht => 1,
	int => 1,
	lng => 1,
);

my %REAL_TYPE = (
	flt => 1,
	dbl => 1,
);


my %CALDB_QUERY = (
	mission => { key => 'TELESCOP' },
	instrument => { key => 'INSTRUME' },
	detector => { key => 'DETNAM', default => '-' },
	filter => { key => 'FILTER', default => '-' },
	date => { default => 'now' },
	time => { default => 'now' },
	startdate => { key => 'DATE-OBS', default => 'now', timesplit => 0,
			key2 => 'DATE' },
	starttime => { key => 'DATE-OBS', default => 'now', timesplit => 1,
			key2 => 'DATE' },
	stopdate => { key => 'DATE-OBS', default => 'now', timesplit => 0,
			key2 => 'DATE' },
	stoptime => { key => 'DATE-OBS', default => 'now', timesplit => 1,
			key2 => 'DATE' },
	expression => { default => '-', par => 'expr' },
);

my @WCS_CONV = qw(
	CRVAL1 CRVAL2 CRPIX1 CRPIX2 CDELT1 CDELT2
	CUNIT1 CUNIT2 CTYPE1 CTYPE2 CROTA2
);

my @WCS_RM = qw(
	PC1_1 PC1_2 PC2_1 PC2_2
	CD1_1 CD1_2 CD2_1 CD2_2
);

my @WCS_KEY = (@WCS_CONV, @WCS_RM);

my %WCS_DEFAULT = (
	CRVAL1 => 0,
	CRVAL2 => 0,
	CRPIX1 => 0,
	CRPIX2 => 0,
	CDELT1 => 1,
	CDELT2 => 1,
	CTYPE1 => '',
	CTYPE2 => '',
	CUNIT1 => '',
	CUNIT2 => '',
	CROTA2 => 0,

	PC1_1 => 1,
	PC1_2 => 0,
	PC2_1 => 0,
	PC2_2 => 1,

	CD1_1 => 0,
	CD1_2 => 0,
	CD2_1 => 0,
	CD2_2 => 0,
);


my @UNRECOGNIZED_WCS_KEYS = qw(
	WCSAXES
	P[CSV]\d+_\d+
	WCSNAME
	C(RDER|SYER)\d+

	WCAX\d+
	\d+C(TYP|UNI|RVL|DLT|RPX)\d+
	\d+C(TY|UN|RV|DE|RP)\d+[A-Z]
	\d\dPC\d+
	\d+[VS]\d+_\d+
	\d+V\d+_X
	WCS[NTX]\d+
	\d+C(RD|SY)\d+

	TC(TY|UN|RV|DE|RP)\d+[A-Z]
	T[CPVS]\d+_\d+
	T(WCS|CRD|CSY)\d+
);


sub report
{
	my ($self, @args) = @_;
	$self->SUPER::report(@args);
	my $s = '';
	while (my $code = Astro::FITS::CFITSIO::fits_read_errmsg($s)) {
		$self->SUPER::report("[FITS/$code] $s");
	}
}


sub isReal
{
	my ($x) = @_;
	my $real = undef;
	my $reExp = qr([eE][-+]?\d+);
	if (($x =~ /^-?\d+\.\d*$/)
			or ($x =~ /^-?\d*\.\d+$/)
			or ($x =~ /^-?\d+($reExp)$/)
			or ($x =~ /^-?\d+\.\d*($reExp)$/)
			or ($x =~ /^-?\d*\.\d+($reExp)$/)
			) {
		$real = 1;
	}
	return $real;
}


sub putKeywords
{
	my ($self, $fits, @list) = @_;

	while (@list) {
		my $key = shift(@list);
		my $info = shift(@list);

		my ($val, $comment, $args) = @$info;
		my @val = ($val);
		$comment ||= '';

		my $real = $args->{type} && $REAL_TYPE{$args->{type}};
		$real ||= isReal($val);

		my $status = 0;
		my $type = $args->{type};
		if (not $type) {
			if ($val =~ /^-?\d+$/) {
				$type = 'lng';
			}
			elsif (isReal($val)) {
				$type = 'dbl';
			}
			else {
				$type = 'str';
			}
		}
		elsif ($type eq 'int') {
			$type = 'lng';
		}

		if ($real) {
			if ($args->{decimals}) {
				push(@val, $args->{decimals});
			}
			else {
				push(@val, length(sprintf('%f', $val)) + 2);
			}
		}

		my $method = "update_key_$type";

		$fits->$method($key, @val, $comment, $status);

		if ($status) {
			$self->error(BAD_OUTPUT,
					"unable to write keyword $key => $val");
		}
	}
}


sub writeColumn
{
	my ($self, $fits, $col, $data) = @_;

	my $colnum = $col->{colnum};
	my $exists = 0;
	my $status = 0;

	if (not $colnum) {
		# see if it already exists
		my $tmp = 0;
		if ($fits->get_colnum(CASEINSEN, $col->{name}, $colnum, $tmp)) {
			# clear error messages
			my $buffer = '';
			while ($tmp = Astro::FITS::CFITSIO::fits_read_errmsg($buffer)) {
				# $self->report($buffer);
			}
		}
		$exists = $colnum;
	}

	if (not $colnum) {
		# place it after existing columns
		my $existing;
		if ($fits->get_num_cols($existing, $status)) {
			$self->error(BAD_EXECUTE,
					"unable to determine number of columns");
		}
		else {
			$colnum = $existing + 1;
		}
	}

	if ($self->isValid and not $exists) {
		$fits->insert_col($colnum, $col->{name}, $col->{form}, $status);
		if ($status) {
			$self->error(BAD_OUTPUT,
					"writeColumn: unable to insert column '$col->{name}'");
		}
	}

	if ($self->isValid and $col->{comment}) {
		my $ttype = "TTYPE$colnum";
		$fits->modify_comment($ttype, $col->{comment}, $status);
	}

	if ($self->isValid and $col->{units}) {
		my $tunit = "TUNIT$colnum";
		$fits->update_key_str($tunit, $col->{units}, undef, $status);
	}

	my $type = $col->{typ} || '';

	if ($type) {
		# user specified
	}
	elsif ($col->{form} =~ /K/) {
		$type = 'int';
	}
	elsif ($col->{form} =~ /J/) {
		$type = 'lng';
	}
	elsif ($col->{form} =~ /I/) {
		$type = 'sht';
	}
	elsif ($col->{form} =~ /E/) {
		$type = 'flt';
	}
	elsif ($col->{form} =~ /D/) {
		$type = 'dbl';
	}
	elsif ($col->{form} =~ /A/) {
		$type = 'str';
	}
	elsif ($col->{form} =~ /L/) {
		$type = 'log';
	}
	else {
		$self->error(BAD_OUTPUT,
				"writeColumn: invalid form '$col->{form}' for $col->{name} column");
	}

	my $method = 'write_col_' . $type;
	my @null;

	if ($self->isValid and defined($col->{null})) {

		$method = 'write_colnull_' . $type;
		@null = ($col->{null});

		if ($INTEGER_TYPE{$type}) {
			$fits->set_btblnull($colnum, $col->{null}, $status);
			my $tnull = "TNULL$colnum";
			$self->putKeywords($fits,
					$tnull => [ $col->{null}, 'null value', { type => 'lng' } ],
					);
		}

		foreach my $x (@$data) {
			if (not defined($x)) {
				$x = $col->{null};
			}
		}
	}

	my $first = 1;
	my $felem = 1;
	my $nrows = @$data;
	$fits->$method($colnum, $first, $felem, $nrows, $data, @null, $status)
		if $self->isValid;

	if ($self->isValid and $col->{display}) {
		my $tdisp = "TDISP$colnum";
		$fits->update_key_str($tdisp, $col->{display}, undef, $status);
	}

	if ($status) {
		$self->error(BAD_FITS, "unable to write column [$status]");
	}
}



sub storeKeywords
{
	my ($self, $thing, %args) = @_;

	my $header = undef;

	if (ref($thing) eq 'HASH') {
		# ready
	}
	elsif (ref($thing)) {
		$header = $thing->read_header;
	}
	else {
		my $mode = $args{leaveOpen} ? READWRITE : READONLY;
		my $status = 0;
		my $fits = Astro::FITS::CFITSIO::open_file($thing, $mode, $status);
		if ($status) {
			$self->error(BAD_INPUT, "unable to open $thing [$status]");
		}
		else {
			$header = $fits->read_header;
			if ($args{leaveOpen}) {
				$args{hash}{fits} = $fits;
			}
			else {
				$fits->close_file($status);
			}
		}
	}

	$self->storeHeaderKeywords($header, %args);

	return $header;
}


sub storeHeaderKeywords
{
	my ($self, $header, %args) = @_;

	my $hash = $args{hash} || $self;
	my $pre = ($args{path} . ' ') || '';

	foreach my $key (@{ $args{keys} }) {
		if (not exists($header->{$key})) {
			if (not $args{optional}) {
				$self->error(BAD_INPUT,
					"${pre}missing required keyword $key");
			}
		}
		else {
			my $value = $header->{$key};
			if ($value =~ /^'(.+?)'$/) {
				$value = $1;
			}
			$value =~ s/\s+$//;
			$hash->{$key} = $value;

			$self->report("keyword $key |$header->{$key}| => |$value|")
				if $self->chatter(5);
		}
	}

}


sub storeHeaderCards
{
	my ($self, $fits, %args) = @_;

	my $array = $args{array};

	if ($args{keys}) {
		foreach my $key (@{ $args{keys} }) {

			my $card = '';

			my $status = 0;
			if ($fits->read_card($key, $card, $status)) {
				$self->warning("unable to read card for $key")
					if not $args{optional};
			}
			else {
				push(@$array, $card);
			}
		}
	}

	if ($args{types}) {
		my %types = map { $_ => 1 } @{ $args{types} };
		my %exclude;
		%exclude = map { $_ => 1 } @{ $args{exclude} }
			if $args{exclude};

		my $status = 0;
		my $ncards;
		my $more;
		$fits->get_hdrspace($ncards, $more, $status);

		for (my $i = 1; $i <= $ncards; ++$i) {

			my $card;
			my $key;
			my $length;
			$fits->read_record($i, $card, $status);

			my $type = Astro::FITS::CFITSIO::fits_get_keyclass($card);
			Astro::FITS::CFITSIO::fits_get_keyname($card, $key, $length, $status);
			# 2005/04/04 CFITSIO bug, doesn't recognize several WCS keys
			if ($type == TYP_USER_KEY) {
				foreach my $re (@UNRECOGNIZED_WCS_KEYS) {
					if ($card =~ /^$re/) {
						$type = TYP_WCS_KEY;
						last;
					}
				}
			}
			push(@$array, $card)
				if $types{$type} and not $exclude{$key};
		}
	}
}


sub parseInputURL
{
	my ($self, $path) = @_;

	my $spec = undef;

	my $status = 0;
	my ($filetype, $filebase, $filetmp,
			$extspec, $filtspec, $binspec, $colspec) = ();

	if (Astro::FITS::CFITSIO::fits_parse_input_url($path,
			$filetype, $filebase, $filetmp,
			$extspec, $filtspec, $binspec, $colspec, $status)) {
		$self->warning("unable to parse $path [$status]");
	}
	else {

require File::Basename;
		my ($name, $dir) = File::Basename::fileparse($filebase);

		$spec = {
			path => $path,
			name => $name,
			dir => $dir,
			filetype => $filetype,
			filebase => $filebase,
			filetmp => $filetmp,
			extspec => $extspec,
			filtspec => $filtspec,
			binspec => $binspec,
			colspec => $colspec,
		};
	}

	return $spec;
}


sub updateChecksums
{
	my ($self, $fits, @extensions) = @_;

	if (ref($fits)) {
		my $status = 0;

		if (@extensions) {
			foreach my $extension (@extensions) {
				$fits->movabs_hdu($extension, ANY_HDU, $status);
				$fits->write_chksum($status);
			}
		}
		else {
			$fits->write_chksum($status);
		}

		if ($status) {
			$self->error(BAD_OUTPUT, "error writing checksums [$status]");
		}
	}
	else {
		my $command = $self->buildCommand('ftchecksum',
				infile => $fits,
				update => 'yes',
				);
		$self->shell($command);
	}
}


sub getParameterList
{
	my ($self, $tool) = @_;

	my @lines;
	my $semi = index($ENV{PFILES}, ';');
	if ($semi >= 0) {
		my @dirs = split(/:/, substr($ENV{PFILES}, $semi+1));
		my $parfile = undef;
		foreach my $d (@dirs) {
			my $path = "$d/$tool.par";
			if (-f $path) {
				$parfile = $path;
				last;
			}
		}
		if ($parfile) {
			my $fh = FileHandle->new($parfile);
			@lines = <$fh>;
			undef($fh);
		}
	}

	my @param;
	foreach my $line (@lines) {

		next if $line =~ /^\s*#/;

		# ($param, $type, $mode, $default, $min, $max, $comment)
		my @field = split(/,/, $line);

		if (@field == 7) {
			foreach my $f (@field) {
				$f =~ s/^\s+//;
				$f =~ s/\s+$//;
			}

			my $value = $field[3];
			if ($value =~ /^"(.*?)"$/) {
				$value = $1;
			}
			my %info = (
				name => $field[0],
				value => $value,
				comment => $field[-1],
			);
			push(@param, \%info);
		}
	}

	return @param;
}



sub buildCommand
{
	my ($self, $exec, @args) = @_;

	my $command = $self->SUPER::buildCommand($exec, @args);

	my @params = $self->getParameterList($exec);
	my %defaults = map { ($_->{name} => $_->{value}) } @params;

	my %args = @args;
	foreach my $p (keys(%defaults)) {
		if (not exists($args{$p})) {
			my $default = $defaults{$p};
			$command .= qq( "$p=$default")
				if ($p ne 'mode' and $default !~ /^\s*$/);
		}
	}

	return $command;
}



sub queryCALDBquzcif
{
	my ($self, $codename, %args) = @_;

	my $header = $args{header} || { };

	my @quzcif = (codename => $codename);

	foreach my $key (qw(mission instrument detector filter
			date time expression)) {

		my $spec = $CALDB_QUERY{$key};
		my $value = $args{$key} || $args{uc($key)};

		if (not defined($value) and exists($spec->{key})) {
			if ($key eq 'date' or $key eq 'time') {
				# yyyy-mm-ddThh:mm:ss
				my ($date, $time) = split('T', $header->{'DATE-OBS'});
				$value = ($key eq 'date') ? $date : $time;
			}
			else {
				$value = $header->{$spec->{key}};
				if (defined($value) and $value =~ /^'(\S+)\s*'$/) {
					$value = $1;
				}
			}
		}

		if (not defined($value)) {
			$value = $spec->{default};
		}

		if (not defined($value)) {
			my $optional = $spec->{key} ? "/$spec->{key}" : '';
			$self->error(BAD_INPUT,
					"queryCALDB($codename) missing $key$optional");
		}
		else {
			my $par = $spec->{par} || $key;
			push(@quzcif, $par => $value);
		}
	}

	my @caldb;

	my $command = $self->buildCommand('quzcif', @quzcif);
	my $result = $self->shell($command);

	if ($result->{error}) {
		$self->warning("quzcif failed [$result->{error}]");
	}
	else {
		foreach my $line (@{ $result->{lines} }) {
			if ($line =~ /^(\S+)\s+(\d+)\s*$/) {
				push(@caldb, { path => $1, ext => $2 });
			}
			else {
				$self->report("queryCALDB ignoring $line")
					if $self->chatter(5);
			}
		}
	}

	if (not @caldb) {
		$self->report("queryCALDB($codename) no items found")
			if $self->chatter;
	}
	elsif ($args{asString}) {
		my $h = $caldb[0];
		my $s = $h->{path};
		if ($args{withExt}) {
			$s .= "[$h->{ext}]";
		}
		$self->report("selected $codename => $s")
			if $self->chatter(3);
		return $s;
	}

	if (@caldb and $self->chatter(3)) {
		my @desc = map { $self->stringify } @caldb;
		my $count = @caldb;
		$self->report(join("\n\t", "found $count $codename", @desc));
	}

	return @caldb;
}


sub queryCALDB
{
	my ($self, $codename, %args) = @_;

	if ($args{quzcif}) {
		return $self->queryCALDBquzcif($codename, %args);
	}

	my $header = $args{header} || { };
	my $text = "CODENAME => \U$codename";

	my %qualifier;
	if (my $arg = $args{qualifiers}) {
		my $qualifiers = '';
		if ($arg =~ /^CALDB$/i) {
		}
		elsif ($arg =~ /^CALDB:(.+)/i) {
			$qualifiers = $1;
		}
		elsif ($arg =~ /^CALDB/i) {
			$self->warning("ignoring CALDB query qualifiers $arg");
		}
		else {
			$qualifiers = $arg;
		}

		foreach my $qualifier (split(',', $qualifiers)) {
			my ($key, $value) = split('=', $qualifier);
			$qualifier{lc($key)} = $value;
		}
	}


	my @args;

	foreach my $key (qw(mission instrument detector filter codename
			startdate starttime stopdate stoptime expression)) {

		if ($key eq 'codename') {
			push(@args, $codename);
			next;
		}

		my $spec = $CALDB_QUERY{$key};
		my $value = $qualifier{$key} || $args{$key} || $args{uc($key)};

		if (not defined($value) and exists($spec->{key})) {
			my $key2 = $spec->{key2} || '';
			$value = $header->{$spec->{key}} || $header->{$key2};
			if (defined($value) and $value =~ /^'(\S+)\s*'$/) {
				$value = $1;
			}
			if (defined($value) and exists($spec->{timesplit})) {
					# yyyy-mm-ddThh:mm:ss
					my @datetime = split('T', $value);
					$value = $datetime[$spec->{timesplit}];
			}
		}

		if (not defined($value)) {
			$value = $spec->{default};
		}

		if (not defined($value)) {
			my $optional = $spec->{key} ? "/$spec->{key}" : '';
			$self->error(BAD_INPUT,
					"queryCALDB($codename) missing $key$optional");
		}
		else {
			push(@args, $value);
			$text .= "\n\t\U$key => $value";
		}
	}

	$self->report("querying CALDB with\n\t$text")
		if $self->chatter(3);

	my @caldb;

	my $maxret = $args{maxret} || 10;
	my $fnamesize = $args{fnamesize} || 1024;
	my ($files, $extnos, $onlines, $nret, $nfound) = (0) x 5;
	my $status = 0;

	HDgtcalf(@args, $maxret, $fnamesize,
			$files, $extnos, $onlines, $nret, $nfound, $status);

	if ($status) {
		$self->warning("HDgtcalf failed [$status]");
	}
	else {
		for (my $i = 0; $i < $nret; ++$i) {
			push(@caldb, {
					path => $files->[$i],
					ext => $extnos->[$i],
					online => $onlines->[$i],
				});
		}
	}

	if (not @caldb) {
		$self->report("queryCALDB($codename) no items found")
			if $self->chatter;
	}
	elsif ($args{asString}) {
		my $h = $caldb[0];
		my $s = $h->{path};
		if ($args{withExt}) {
			$s .= "[$h->{ext}]";
		}
		$self->report("selected $codename $s")
			if $self->chatter(2);
		return $s;
	}

	if (@caldb and $self->chatter(3)) {
		my @desc = map { $self->stringify($_) } @caldb;
		$self->report(join("\n\t", "found $codename", @desc));
	}

	return @caldb;
}


sub writeCardsToHandle
{
	my ($self, $cards, $fh) = @_;

	foreach my $card (@$cards) {
		if (length($card) > 75) {
			next if $card =~ /^COMMENT\s+FITS \(Flexible/;
			next if $card =~ /^COMMENT\s+and Astrophysics/;
			$card =~ s/^(\w+)\s+=\s+/$1=/;
		}
		$fh->print("$card\n");
	}
}



sub createEmptyFITS
{
	my ($self, $path, %args) = @_;

	my @par;

	my $headfile = $self->temporary('keys');
	my $fh = FileHandle->new($headfile, 'w');
	if (not $fh) {
		$self->error(BAD_OUTPUT, "unable to create $headfile [$!]");
		return;
	}

	$fh->print("SIMPLE = T\nBITPIX = 8\n");

	if ($args{infile}) {
		my $status = 0;
		my $fits = Astro::FITS::CFITSIO::open_file(
				$args{infile}, READONLY, $status);
		if ($status) {
			$self->error(BAD_INPUT, "unable to open $args{infile} [$status]");
			return;
		}

		my @cards;
		$self->storeHeaderCards($fits,
				types => [ TYP_REFSYS_KEY, TYP_USER_KEY,
							TYP_COMM_KEY, TYP_CONT_KEY ],
				array => \@cards,
				);

		$fits->close_file($status);

		$self->writeCardsToHandle(\@cards, $fh);
	}

	$fh->close;

	{
		my $command = $self->buildCommand('ftemplate',
				template => $headfile,
				outfile  => $path,
				);
		$self->shell($command);
	}
}


sub getWCS
{
	my ($self, $header, %args) = @_;

	my $suffix = $args{suffix} || '';
	my @suffixKey = map { $_ . $suffix } @WCS_KEY;

	my $toler = $args{toler} || 0.0002;

	my %tmp;
	$self->storeHeaderKeywords($header,
			keys => \@suffixKey,
			optional => 1,
			hash => \%tmp,
			);

	my %wcs;
	my $cdExists = undef;
	my $pcExists = undef;
	foreach my $key (@WCS_KEY) {
		my $keyp = $key . $suffix;
		if (exists($tmp{$keyp})) {
			$wcs{$key} = $tmp{$keyp};
			$cdExists = 1 if $key =~ /^CD/;
			$pcExists = 1 if $key =~ /^PC/;
		}
	}

	# this logic is translated from cfitsio/wcsutil.c
	if (not exists($wcs{CDELT1})) {

		if ($cdExists) {

			my $cd11 = $wcs{CD1_1} || 0;
			my $cd12 = $wcs{CD1_2} || 0;
			my $cd21 = $wcs{CD2_1} || 0;
			my $cd22 = $wcs{CD2_2} || 0;

			my $phia = atan2( $cd21, $cd11);
			my $phib = atan2(-$cd12, $cd22);

			if ($phib < $phia) {
				my $tmp = $phia;
				$phia = $phib;
				$phib = $tmp;
			}

			if ($phib - $phia > Math::PIo2) {
				$phia += Math::PI;
			}

			if (abs($phia - $phib) > $toler) {
				$wcs{APPROXIMATE} = 1;
			}

			my $phi = ($phia + $phib) / 2;
			$wcs{CDELT1} = $cd11 / cos($phi);
			$wcs{CDELT2} = $cd22 / cos($phi);
			$wcs{CROTA2} = Math::radiansToDegrees($phi);

			if ($wcs{CDELT2} < 0) {
				$wcs{CDELT1} *= -1;
				$wcs{CDELT2} *= -1;
				$wcs{CROTA2} -= 180;
			}
		}
		else {
			$wcs{CDELT1} = 1;
			if (not exists($wcs{CDELT2})) {
				$wcs{CDELT2} = 1;
			}
			if (not exists($wcs{CROTA2})) {
				$wcs{CROTA2} = 0;
			}
		}
	}
	else {
		if (not exists($wcs{CDELT2})) {
			$wcs{CDELT2} = 1;
		}

		if (not exists($wcs{CROTA2}) and $pcExists) {

			my $pc11 = $wcs{PC1_1} || 1;
			my $pc12 = $wcs{PC1_2} || 0;
			my $pc21 = $wcs{PC2_1} || 0;
			my $pc22 = $wcs{PC2_2} || 1;

			my $phia = atan2( $pc21, $pc11);
			my $phib = atan2(-$pc12, $pc22);

			if ($phib < $phia) {
				my $tmp = $phia;
				$phia = $phib;
				$phib = $tmp;
			}

			if ($phib - $phia > Math::PIo2) {
				$phia += Math::PI;
			}

			if (abs($phia - $phib) > $toler) {
				$wcs{APPROXIMATE} = 1;
			}

			my $phi = ($phia + $phib) / 2;
			$wcs{CROTA2} = Math::radiansToDegrees($phi);
		}
		else {
			$wcs{CROTA2} = $wcs{CROTA2} || 0;
		}
	}

	foreach my $key (@WCS_CONV) {
		if (not exists($wcs{$key})) {
			$wcs{$key} = $WCS_DEFAULT{$key};
		}
	}

	my $ctype1 = $wcs{CTYPE1};
	my $ctype2 = $wcs{CTYPE2};
	my $reported = undef;
	if (not $ctype1 and not $ctype2) {
		$self->warning('missing CTYPEn')
			if not $args{quiet};
		$reported = 1;
	}
	elsif (length($ctype1) == 8 and length($ctype2) == 8) {
		my $type1 = substr($ctype1, -4);
		if ($type1 ne substr($ctype2, -4)) {
			$self->error(BAD_INPUT, "CTYPEn mismatch $ctype1/$ctype2");
			return undef;
		}
		else {
			$wcs{TYPE} = $type1;
		}
	}
	elsif (length($ctype1) == 12 and length($ctype2) == 12) {
		my $type1 = substr($ctype1, 4, 4);
		my $type2 = substr($ctype1, -4);
		if ($type1 ne substr($ctype2, 4, 4)) {
			$self->error(BAD_INPUT, "CTYPEn mismatch $ctype1/$ctype2");
			return undef;
		}
		elsif ($type2 ne substr($ctype2, -4)) {
			$self->error(BAD_INPUT, "CTYPEn mismatch $ctype1/$ctype2");
			return undef;
		}
		else {
			$wcs{TYPE} = $type1;
		}
	}
	elsif (length($ctype1) == length($ctype2)) {
		my $lessOne = length($ctype1) - 1;
		my $tag1 = substr($ctype1, 0, $lessOne);
		my $tag2 = substr($ctype2, 0, $lessOne);
		if ($tag1 eq $tag2) {
			$wcs{TYPE} = LINEAR;
		}
	}

	if (not $reported and not $wcs{TYPE}) {
		$self->warning("unrecognized CTYPEn $ctype1/$ctype2");
	}

	return \%wcs;
}


sub worldToPix
{
	my ($self, $wcs, $xworld, $yworld) = @_;

	my ($xpix, $ypix);

	my $status = 0;

	if ($wcs->{TYPE} eq LINEAR) {
		my $radians = Math::degreesToRadians($wcs->{CROTA2});
		my $cos = cos($radians);
		my $sin = sin($radians);
		my $dx0 = $xworld - $wcs->{CRVAL1};
		my $dy0 = $yworld - $wcs->{CRVAL2};
		my $dx = $dx0 * $cos + $dy0 * $sin;
		my $dy = $dy0 * $cos - $dx0 * $sin;
		$xpix = $dx / $wcs->{CDELT1} + $wcs->{CRPIX1};
		$ypix = $dy / $wcs->{CDELT2} + $wcs->{CRPIX2};
	}
	else {
		Astro::FITS::CFITSIO::fits_world_to_pix(
				$xworld, $yworld,
				$wcs->{CRVAL1}, $wcs->{CRVAL2},
				$wcs->{CRPIX1}, $wcs->{CRPIX2},
				$wcs->{CDELT1}, $wcs->{CDELT2},
				$wcs->{CROTA2}, $wcs->{TYPE},
				$xpix, $ypix,
				$status,
				);
	}

	return ($xpix, $ypix);
}


sub pixToWorld
{
	my ($self, $wcs, $xpix, $ypix) = @_;

	my ($xworld, $yworld);

	my $status = 0;

	if ($wcs->{TYPE} eq LINEAR) {
		my $radians = Math::degreesToRadians($wcs->{CROTA2});
		my $cos = cos($radians);
		my $sin = sin($radians);
		my $dx0 = $xpix - $wcs->{CRPIX1};
		my $dy0 = $ypix - $wcs->{CRPIX2};
		my $dx = $dx0 * $cos - $dy0 * $sin;
		my $dy = $dx0 * $sin + $dy0 * $cos;
		$xworld = $wcs->{CRVAL1} + $dx * $wcs->{CDELT1};
		$yworld = $wcs->{CRVAL2} + $dy * $wcs->{CDELT2};  
	}
	else {
		Astro::FITS::CFITSIO::fits_pix_to_world(
				$xpix, $ypix,
				$wcs->{CRVAL1}, $wcs->{CRVAL2},
				$wcs->{CRPIX1}, $wcs->{CRPIX2},
				$wcs->{CDELT1}, $wcs->{CDELT2},
				$wcs->{CROTA2}, $wcs->{TYPE},
				$xworld, $yworld,
				$status,
				);
	}

	return ($xworld, $yworld);
}



sub cleanHeaderStrings
{
	my ($header) = @_;
	while (my ($key, $value) = each(%$header)) {
		if ($value =~ /^'/) {
			$value =~ s/^'//;
			$value =~ s/\s*'$//;
			$header->{$key} = $value;
		}
	}
}



1;

