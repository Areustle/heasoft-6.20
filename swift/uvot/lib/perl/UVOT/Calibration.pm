
use strict;

package UVOT::Calibration;

use Task qw(:codes);

use SimpleFITS;
use Math::Polynomial;

use constant DEFAULT_FRAMETIME_s => 0.0110322;

use constant DEFAULT_FWHM_UNCERTAINTY_percent => 15;


my @FILTER_NAMES = qw(
	B
	U
	V
	WHITE
	UVW1
	UVW2
	UVM2
	VGRISM
	UGRISM
);

my %COLORTABLE_FILTER = (
	UGRIS => 'UGRISM',
	VGRIS => 'VGRISM',
);

my %DEFAULT_COLORTABLE = (
	ZPT => 99,
	ZPE => 99,
	FCF => 0,
	FCE => 0,
	WAV => 1,
);



sub loadCoincidenceLoss
{
	my ($task, $cal, %args) = @_;
	my $header = $args{HEADER};
	my $path;
	if ($args{PAR} =~ /^CALDB/i) {
		$path = $task->queryCALDB('COINCIDENCE',
				header => $header,
				qualifiers => $args{PAR},
				asString => 1, withExt => 1,
				);
		if (my $parname = $args{PAR_NOTE}) {
			$task->parameterNote($parname => $path)
				if $path;
		}
	}
	else {
		$path = $args{PAR} . '[COINCIDENCE]';
	}

	if (not $path) {
		$task->error(BAD_INPUT,
				"unable to determine coincidence loss calibration");
		return;
	}

	my $fits = SimpleFITS->readonly($path);
	my $status = $fits->status;
	if ($status) {
		$task->error(BAD_INPUT,
				"unable to open $path [$status]");
		undef($fits);
		return;
	}

	my @table;
	$fits->loadtable(\@table)
			->close
			->status;
	if ($status) {
		$task->error(BAD_INPUT,
				"unable to load coincidence loss table [$status]");
		undef($fits);
		return;
	}

	my $best;

	foreach my $o (@table) {
		if (not $best or
				($o->{TIME} > $best->{TIME} and $o->{TIME} < $header->{TSTOP})) {
			$best = $o;
		}
	}

	if ($best) {
		my @coeff;
		my $tag;
		if ($best->{MULTFUNC}) {
			$cal->{MULTFUNC} = 1;
			@coeff = @{ $best->{MULTFUNC} };
			$tag = 'MULTFUNC';
		}
		else {
			$cal->{PLINFUNC} = 1;
			@coeff = @{ $best->{PLINFUNC} };
			$tag = 'PLINFUNC';
		}
		my $poly = Math::Polynomial->new(@coeff);
		$cal->{POLY} = $poly;
		$cal->{COIAPT} = $best->{COIAPT} || 6;
				# Stephen is updating CALDB to include COIAPT

		if (exists($args{FRAMETIME_s})
				and uc($args{FRAMETIME_s}) ne 'DEFAULT') {
			$cal->{FRAMETIME_s} = $args{FRAMETIME_s};
		}

		if (exists($args{DEADTIME_CORRECTED})) {
			$cal->{DEADTIME_CORRECTED} = $args{DEADTIME_CORRECTED};
		}
		else {
			$cal->{DEADTIME_CORRECTED} = 1;
		}
	}
	else {
		$task->error(BAD_INPUT,
				"no coincidence loss calibration for TIME $header->{TSTOP}");
	}
}


# this loads the ZPTx keywords from the *COLORMAG extension(s) as described
# in the UVOT Calibration Datahandbook
sub loadColorTable
{
	my ($task, $cal, %args) = @_;
	my $path;
	if ($args{PAR} =~ /^CALDB/i) {
		$path = $task->queryCALDB('COLORTABLE',
				header => $args{HEADER},
				qualifiers => $args{PAR},
				asString => 1, withExt => 1,
				);
		if (my $parname = $args{PAR_NOTE}) {
			$task->parameterNote($parname => $path)
				if $path;
		}
	}
	else {
		$path = $args{PAR} . '[COLORMAG]';
	}

	if (not $path) {
		$task->error(BAD_INPUT,
				"unable to determine zero points calibration");
		return;
	}

	my $fits = SimpleFITS->readonly($path);
	my $status = $fits->status;
	if ($status) {
		$task->error(BAD_INPUT,
				"unable to open $path [$status]");
		undef($fits);
		return;
	}

	loadColorTableAux($task, $cal, $fits, %args);

	$status = $fits->close
			->status;
	if ($status) {
		$task->error(BAD_INPUT,
				"unable to close $path [$status]");
	}

	if (my $header = $args{HEADER}) {
		if (my $filter = $header->{FILTER}) {
			primeColorTable($task, $cal, $filter, 'Vega');
		}
		else {
			$task->status("loadColorTable: filter not specified");
		}
	}
}


sub loadColorTableAux
{
	my ($task, $cal, $fits, %args) = @_;

	my $status = storeColorTable($task, $cal, $fits, %args);

	my $nhdu = $fits->nhdu;

	for (my $i = 1; $i < $nhdu; ++$i) {
		$status = $fits->move($i+1)->status;
		if ($status) {
			$task->warning("unable to move to HDU $i+1");
		}

		$status = storeColorTable($task, $cal, $fits, %args);
		if ($status) {
			$task->warning("unable to load color table $i+1: $status");
		}
	}

	# Require the Vega system to be last since the last system processed
	# by applyColorTable will set the unqualified MAG, MAG_ERR, FLUX, ...
	delete($cal->{SYSTEM_HASH}{Vega});
	my @systems = (sort(keys(%{ $cal->{SYSTEM_HASH} })), 'Vega');
	$cal->{SYSTEMS} = join(',', @systems);
	delete($cal->{SYSTEM_HASH});

	if (not $cal->{REPORTED} and $task->chatter(4)) {
		foreach my $system (@systems) {
			$task->report("$system magnitude system");
			foreach my $filter (@FILTER_NAMES) {
				my $s = sprintf('%-8s:', $filter);
				foreach my $key (qw(ZPT ZPE FCF FCE WAV)) {
					$s .= " $key=$cal->{qq($system:$key:$filter)}";
				}
				$task->report("\t$s");
			}
		}
		$cal->{REPORTED} = 1;
	}
}



sub storeColorTable
{
	my ($task, $cal, $fits, %args) = @_;

	my $header;
	my $status = $fits->readheader($header, clean => 1)->status;
	if ($status) {
		return "unable to read color table header [$status]";
	}

	my $system = '';
	my $extname = $header->{EXTNAME} || 'NONE';
	if ($extname =~ /^COLORMAG$/) {
		$system = 'Vega';
	}
	elsif ($extname =~ /^(\w+?)COLORMAG$/) {
		$system = $1;
	}
	else {
		return "storeColorTable: unexpected EXTNAME='$extname'";
	}

	if ($cal->{SYSTEM_HASH}{$system}) {
		# $task->verbose("storeColorTable: already have $system system");
		return;
	}

	$cal->{SYSTEM_HASH}{$system} = 1;

	foreach my $key (keys(%$header)) {
		# note that the grism filters are actually truncated
		if ($key =~ /^(ZPT|ZPE|FCF|FCE|WAV)(\S+)/) {
			my ($type, $truncfilt) = ($1, $2);
			my $filter = $COLORTABLE_FILTER{$truncfilt} || $truncfilt;
			my $calkey = "$system:$type:$filter";
			$cal->{$calkey} = $header->{$key};
		}
	}

}


sub primeColorTable
{
	my ($task, $cal, $filter, $system) = @_;

	if (not defined($system)) {
		$system = 'Vega';
	}

	if (not defined($cal->{SYSTEM}) or not defined($cal->{FILTER})) {
		# $task->verbose("no system/filter cached");
	}
	elsif ($cal->{SYSTEM} eq $system and $cal->{FILTER} eq $filter) {
		# $task->verbose("already set up for $system / $filter");
		return;
	}

	foreach my $key (qw(ZPT ZPE FCF FCE WAV)) {
		# note that the grism filters are actually truncated
		my $value = $cal->{qq($system:$key:$filter)};
		if (defined($value)) {
			$cal->{$key} = $value;
		}
		else {
			$task->warning("no $system $filter $key available");
			$cal->{$key} = $DEFAULT_COLORTABLE{$key};
		}
	}

	$cal->{SYSTEM} = $system;
	$cal->{FILTER} = $filter;
}


sub loadLargeScaleSensitivity
{
	my ($task, $cal, %args) = @_;
	my $path;
	my %open;
	my $par = $args{PAR};
	if ($par =~ /^CALDB/i) {
		$path = $task->queryCALDB('SKYFLAT',
				header => $args{HEADER},
				qualifiers => $par,
				asString => 1, withExt => 1,
				);
		if (my $parname = $args{PAR_NOTE}) {
			$task->parameterNote($parname => $path)
				if $path;
		}
	}
	elsif ($par =~ /^SKY:(.+)/i) {
		$path = $1;
		$cal->{SKYLSS} = 1;
		$open{type} = 'image';
	}
	elsif (uc($par) eq 'NONE') {
		$cal->{NONE} = 1;
		return;
	}
	else {
		$path = $args{PAR};
	}

	if (not $path) {
		$task->error(BAD_INPUT,
				"unable to determine large scale sensitivity calibration");
		return;
	}

	my $fits = SimpleFITS->open($path, %open);
	my $status = $fits->status;
	if ($status) {
		$task->error(BAD_INPUT,
				"unable to open $path [$status]");
		return;
	}

	if ($cal->{SKYLSS}) {
		my $nhdus = $fits->nhdu;
		my $fptr = $fits->handle;
		my $curhdu;
		$fptr->get_hdu_num($curhdu);
		while (1) {
			my $bitpix = -1;
			my $naxis = -1;
			my @axes = (0) x 4;
			$fptr->get_img_parm($bitpix, $naxis, \@axes, $status);
			if ($naxis == 2 and $axes[0] > 0) {
				$task->verbose("HDU $curhdu bitpix=$bitpix, NAXIS=$naxis, NAXIS1=$axes[0]");
				last;
			}
			elsif ($curhdu < $nhdus) {
				# move to the next HDU (probably should not do this if an
				# an extension was specified)
				$task->verbose("HDU $curhdu does not look appropriate");
				++$curhdu;
				$fits->move($curhdu);
			}
			else {
				$task->error(BAD_INPUT, "no appropriate LSS image found");
				last;
			}
		}
	}

	$cal->{FITS} = $fits;
}



=pod
pro det2raw,xd,yd,xr,yr
;Convert from detector pixel coordinates to raw pixel coordinates:
;
;INPUTS:
;   XD, YD - Pixel positions in detector coordinates
; OUTPUTS:
;   XR YR - Pixel positions in raw coordinates.
;
if N_params() LT 2 then begin 
 print,'Syntax - det2raw,xd,yd,xr,yr'
 return
 endif
 
 Coefficients of 4th order polynomail to convert from XD, YD, to XR, YR
 
kx = $
[[-78.649210d, 0.015565185 , -1.7515713e-07, 4.3219390e-10, -8.1065362e-13],$
[1.0244568 , -6.4775283e-06,   2.5891338e-09, -3.8141409e-12, 9.0024773e-15],$
[2.5823715e-08, -1.7746245e-09, -6.1715464e-12, 7.1074961e-17, 2.1666564e-18],$
[9.0604263e-09,-2.9960087e-12, 1.7198618e-14, 4.0598908e-18, -7.6395990e-21], $
[-1.8713561e-12, -8.5996806e-16, 7.0224033e-19, -1.4290023e-21, -1.2595451e-25]]

ky = $
[[ -75.469185d, 0.98578166, -5.4731736e-06, 1.7460179e-08, -1.7065813e-12],$
[0.012609845, -4.0656442e-06, 9.0395420e-10, -3.6852265e-12, -6.8798446e-16],$
[4.1295693e-06, 2.8856846e-09, -5.1419799e-12, 1.8097329e-14, -1.5504342e-18],$
[-1.2398514e-09, -3.6408521e-12, -2.5093620e-15, 2.7977826e-18, 1.5104439e-21],$
[-2.8081675e-12, 1.1067681e-14, 7.2664194e-19, -5.8928808e-21, 1.6373034e-24]]

np = 4
;Some rescaling for numerical precision
x2 = xd - 1023.5 + 104
y2 = yd - 1023.5 + 78
xr = 0.
yr = 0. 
for i=0,np do begin
 for j=0,np do begin
     xr = xr + kx[i,j]*x2^j*y2^i    
     yr = yr + ky[i,j]*x2^j*y2^i
endfor
endfor
xr = xr + 1023.5 
yr = yr + 1023.5 
return
end
=cut

sub estimateRAWfromDET
{
	my ($xd, $yd) = @_;

	my $kx = [
[-78.649210, 0.015565185 , -1.7515713e-07, 4.3219390e-10, -8.1065362e-13],
[1.0244568 , -6.4775283e-06, 2.5891338e-09, -3.8141409e-12, 9.0024773e-15],
[2.5823715e-08, -1.7746245e-09, -6.1715464e-12, 7.1074961e-17, 2.1666564e-18],
[9.0604263e-09,-2.9960087e-12, 1.7198618e-14, 4.0598908e-18, -7.6395990e-21],
[-1.8713561e-12, -8.5996806e-16, 7.0224033e-19, -1.4290023e-21, -1.2595451e-25]
];

	my $ky = [
[ -75.469185, 0.98578166, -5.4731736e-06, 1.7460179e-08, -1.7065813e-12],
[0.012609845, -4.0656442e-06, 9.0395420e-10, -3.6852265e-12, -6.8798446e-16],
[4.1295693e-06, 2.8856846e-09, -5.1419799e-12, 1.8097329e-14, -1.5504342e-18],
[-1.2398514e-09, -3.6408521e-12, -2.5093620e-15, 2.7977826e-18, 1.5104439e-21],
[-2.8081675e-12, 1.1067681e-14, 7.2664194e-19, -5.8928808e-21, 1.6373034e-24]
];

	my $np = 4;
# ;Some rescaling for numerical precision
	my $x2 = $xd - 1023.5;
	my $y2 = $yd - 1023.5;
	my $xr = 0.;
	my $yr = 0.;
	for (my $i=0; $i <= $np; ++$i) {
		for (my $j=0; $j <= $np; ++$j) {
			$xr = $xr + $kx->[$j][$i] * ($x2**$j) * ($y2**$i);
			$yr = $yr + $ky->[$j][$i] * ($x2**$j) * ($y2**$i);
		}
	}

	$xr = $xr + 1023.5;
	$yr = $yr + 1023.5;

	return ($xr, $yr);
}


# this loads REEF<filter> extension(s) from swureefyyyymmddvnnn.fits files

sub loadEncircledEnergy
{
	my ($task, $cal, %args) = @_;
	my $path;
	# load encircled energy extension(s)
	if ($args{PAR} =~ /^CALDB/i) {
		$path = $task->queryCALDB('REEF',
				header => $args{HEADER},
				filter => '-',		# just need the file, not the filter
				qualifiers => $args{PAR},
				asString => 1,
				);
		if (my $parname = $args{PAR_NOTE}) {
			$task->parameterNote($parname => $path)
				if $path;
		}
	}
	else {
		$path = $args{PAR};
	}

	if (not $path) {
		$task->error(BAD_INPUT,
				"unable to determine PSF calibration");
		return;
	}

	my $fits = SimpleFITS->readonly($path);
	my $status = $fits->status;
	if ($status) {
		$task->error(BAD_INPUT,
				"unable to open $path [$status]");
		return;
	}

	my $nhdu = $fits->nhdu;

	for (my $i = 2; $i <= $nhdu; ++$i) {

		my $filter;
		my $pixsize;
		my @table;

		$status = $fits->move($i)
				->readkey(FILTER => $filter)
				->readkey(PIXSIZE => $pixsize)
				->loadtable(\@table)
				->status;

		if ($status) {
			$task->error(BAD_INPUT,
					"unable to load REEF HDU $i [$status]");
		}
		else {
			my %info = (
				PIXSIZE_deg => $pixsize,
				TABLE => \@table,
				ROWS => scalar(@table),
				FILTER => $filter,
			);
			$cal->{qq(REEF:$filter)} = \%info;
		}
	}

	$fits->close->status;
}



sub primeEncircledEnergy
{
	my ($task, $cal, $filter) = @_;

	my $key = qq(REEF:$filter);
	my $altkey = qq(REEF:B);

	if (ref($cal->{$key})) {
		$cal->{PRIMED} = $cal->{$key};
	}
	elsif ($filter eq 'WHITE' and ref($cal->{$altkey})) {
		$cal->{PRIMED} = $cal->{$altkey};
		$cal->{WARNING} = "primeEncircledEnergy: using B calibration for WHITE";
	}
	else {
		$task->error(BAD_INPUT,
				"primeEncircledEnergy: no calibration for filter '$filter'");
	}

	if (not $cal->{ERROR}) {
		my $count = scalar(@{ $cal->{PRIMED}{TABLE} });
		if (not $count) {
			$task->error(BAD_INPUT,
					"primeEncircledEnergy: no rows for filter '$filter'");
		}
	}
}


sub loadDetectorSensitivity
{
	my ($task, $cal, %args) = @_;

	my $header = $args{HEADER} || { };
	my $filter = $header->{FILTER};

	my $path0 = $args{PAR};
	my $path = undef;
	my @ext;

	if ($path0 =~ /^CALDB/i) {
		$path = $task->queryCALDB('SENSCORR',
				header => $args{HEADER},
				filter => '-',		# just need the file, not the filter
				qualifiers => $path0,
				asString => 1, withExt => 1,
				);
		if (my $parname = $args{PAR_NOTE}) {
			$task->parameterNote($parname => $path)
				if $path;
		}
	}
	elsif (uc($path0) eq 'NONE') {
		$cal->{NONE} = 1;
		return;
	}
	else {
		my $spec = $task->parseInputURL($path0);
		if (not defined($spec)) {
			$task->warning("loadDetectorSensitivity: invalid input $path0");
		}
		elsif (defined($spec->{extspec}) and length($spec->{extspec})) {
			$path = $path0;
			@ext = ($spec->{extspec});
			$cal->{USER} = 1;
		}
		else {
#			if (not defined($filter)) {
#				$filter = 'V';
#				$task->warning('loadDetectorSensitivity: neither FILTER nor HDU'
#						. ' specified- trying V');
#			}
			$path = $path0;
		}
	}

	if (not $path) {
		$task->error(BAD_INPUT,
				"unable to determine detector sensitivity calibration");
		return;
	}

	my $fits = SimpleFITS->readonly($path);
	my $status = $fits->status;
	if ($status) {
		$task->error(BAD_INPUT,
				"unable to open $path [$status]");
		undef($fits);
		return;
	}

# if the extension was specified as part of the file name, always use it,
# otherwise, load all extensions

	if (not @ext) {
		my $nhdu = $fits->nhdu;
		@ext = (2 .. $nhdu);
	}

	foreach my $e (@ext) {

		my $extFilter;
		my @table;

		$status = $fits->move($e)
				->readkey(FILTER => $extFilter)
				->loadtable(\@table)
				->status;

		if ($status) {
			$task->error(BAD_INPUT,
					"unable to load SENSCORR HDU $e [$status]");
		}
		else {
			my %info = (
				TABLE => \@table,
				ROWS => scalar(@table),
				FILTER => $extFilter,
			);
			$cal->{qq(SENSCORR:$extFilter)} = \%info;
			if ($cal->{USER}) {
				$cal->{'SENSCORR:DEFAULT'} = \%info;
			}
			$task->debug("loaded $info{ROWS} SENSCORR $info{FILTER} rows")
				if $task->chatter(6);
		}
	}

	# if the user did not specify the extension, make V the default
	if (not $cal->{USER}) {
		$cal->{'SENSCORR:DEFAULT'} = $cal->{'SENSCORR:V'};
	}

	$fits->close->status;
}



sub primeDetectorSensitivity
{
	my ($task, $cal, $filter) = @_;

	my $key = qq(SENSCORR:$filter);
	my $defaultKey = 'SENSCORR:DEFAULT';

	if (ref($cal->{$key})) {
		$cal->{PRIMED} = $cal->{$key};
	}
	elsif (ref($cal->{$defaultKey})) {
		$cal->{PRIMED} = $cal->{$defaultKey};
		if (not $cal->{USER}) {
			$task->warning("primeDetectorSensitivity: no $filter calibration using default $cal->{PRIMED}{FILTER}");
		}
	}
	else {
		$task->warning("primeDetectorSensitivity: no calibration for filter '$filter'");
	}

}



1;

