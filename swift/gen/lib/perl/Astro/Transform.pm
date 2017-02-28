#! /usr/bin/perl -w
# $Source: /headas/headas/swift/gen/lib/perl/Astro/Transform.pm,v $
# $Revision: 1.6 $
# $Date: 2005/11/02 15:44:58 $
#
#
# $Log: Transform.pm,v $
# Revision 1.6  2005/11/02 15:44:58  rwiegand
# Execute commands using Task::shell.
#
# Revision 1.5  2005/05/06 21:49:47  rwiegand
# The buildTransform method was looking in the wrong hash for a key that
# should not have been checked anyway.  Allow the caller to pass in the
# time at which to sample the attitude.
#
# Revision 1.4  2005/03/21 14:33:43  rwiegand
# Fixed typo in MJDREFI.
#
# Revision 1.3  2005/03/04 19:18:33  rwiegand
# Added finer control of history parameter for sub-processes.
#
# Revision 1.2  2005/01/31 21:20:04  rwiegand
# Redirected getWCS call through parent.
#
# Revision 1.1  2005/01/19 19:31:44  rwiegand
# Extracted module for building image transforms using the headas/attitude
# tools.
#

package Astro::Transform;

use strict;

use base qw(Task::Subtask);
use Task qw(:codes);

use FileHandle;
use POSIX;

use Astro::FITS::CFITSIO qw(:constants);



sub loadHeader
{
	my ($self, $path) = @_;

	my $status = 0;
	my $fits = Astro::FITS::CFITSIO::open_file($path, READONLY, $status);
	if ($status) {
		$self->error(BAD_INPUT, "unable to open $path [$status]");
		return;
	}

	my $header = undef;
	($header, $status) = $fits->read_header;
	if ($status) {
		$self->error(BAD_INPUT, "unable to read $path header [$status]");
	}

	{
		my $tmp = 0;
		$fits->close_file($tmp);
	}

	return $header;
}


sub buildTransform
{
	my ($self, %args) = @_;

	# check for required arguments
	foreach my $key (qw(inspec outspec from to teldef teldefpath)) {
		if (not exists($args{$key})) {
			$self->error(BAD_INPUT, "buildTransform requires $key");
		}
	}
	return if not $self->isValid;

	my $header = $args{header};

	my $from = $args{from};
	my $to   = $args{to};

	my %info = (
		inspec  => Task::FITS::parseInputURL($self, $args{inspec}),
		outspec => Task::FITS::parseInputURL($self, $args{outspec}),

		from    => $from,
		to      => $to,
		teldef  => $args{teldef},
		teldefpath => $args{teldefpath},
		method  => $args{method} || 'AREA',

		ra      => $args{ra},
		dec     => $args{dec},

		mjdref  => 0,
		time    => $args{time} || 0,
		segment => $args{segment} || 0,
		attfile => $args{attfile} || 'none',
		bitpix  => $args{bitpix} || 0,

		zeronulls  => $args{zeronulls} || 'no',
		aberration => $args{aberration} || 'no',
		history    => $args{history} || 'yes',
		fullHistory=> ($args{fullHistory} ? 'yes' : 'no'),
		seed       => $args{seed} || 0,
	);

	$self->{history} = $info{fullHistory};

	if (not $info{inspec} or not length($info{inspec}{extspec})) {
		$self->error(BAD_INPUT, "inspec does not include extension");
	}
	else {
		$info{inpathext} = $args{inspec};
	}

	if (not $info{outspec} or not length($info{outspec}{extspec})) {
		$self->error(BAD_INPUT, "outspec does not include extension");
	}
	else {
		$info{outpath} = $info{outspec}{filebase};
		$info{outpathext} = $args{outspec};
	}


	###########################################
	# see if we actually need these parameters
	###########################################
	if ($from eq 'SKY' or $to eq 'SKY'
			or ($to eq 'DET' and $info{attfile} !~ /^NONE$/i)) {

		if (exists($header->{MJDREFI}) and exists($header->{MJDREFF})) {
			$info{mjdref} = $header->{MJDREFI} + $header->{MJDREFF};
		}
		elsif (exists($header->{MJDREF})) {
			$info{mjdref} = $header->{MJDREF};
		}
		else {
			$info{mjdref} = 51910;
			$self->warning("missing MJD reference keyword(s) - defaults to $info{mjdref}");
		}

		if ($info{time}) {
			# time set by caller
		}
		elsif (exists($header->{TSTART}) and exists($header->{TSTART})) {
			$info{time} = ($header->{TSTART} + $header->{TSTOP}) / 2;
		}
		else {
			$self->error(BAD_INPUT, "missing TSTART/TSTOP keyword(s)");
		}


		if (not exists($args{ra}) or not exists($args{dec})) {
			$self->error(BAD_INPUT, "missing ra/dec for SKY transform");
		}


		if (not -f $info{attfile}) {
			$self->error(BAD_INPUT, "missing attfile for SKY transform");
		}

	}
	return if not $self->isValid;


	$info{tel2tel} = $self->temporary('tel2tel', ext => '.xform');
	{
		########################################################
		# extract the transform
		# NOTE: we might not have to do this for every image
		#########################################################

		my $getxform = $self->buildCommand('getxform',
				teldef     => $info{teldefpath},
				from       => $info{from},
				to         => $info{to},
				image      => 'yes',
				segment    => $info{segment},
				ra         => $info{ra},
				dec        => $info{dec},
				attfile    => $info{attfile},
				time       => $info{time},
				mjdref     => $info{mjdref},
				aberration => $info{aberration},
				outfile    => $info{tel2tel},
				history    => $info{fullHistory},
				);

		$self->shell($getxform);
		return if not $self->isValid;
	}


	if ($args{bbox}) {

		$self->getBoundingTransform(\%info,
				header => $header,
				teldef => $info{teldef},
				);
	}

	else {
		$info{transform} = $info{tel2tel};
		$info{to_dimenx} = $info{teldef}{$to . '_XSIZ'};
		$info{to_dimeny} = $info{teldef}{$to . '_YSIZ'};
	}

	return \%info;
}



sub performTransform
{
	my ($self, $info) = @_;

	#######################################
	# apply the transform
	#######################################
	my $imagetrans = $self->buildCommand('imagetrans',
			infile    => $info->{inpathext},
			outfile   => $info->{outpath},
			transform => $info->{transform},
			inverse   => 'none',
			method    => $info->{method},
			dimenx    => $info->{to_dimenx},
			dimeny    => $info->{to_dimeny},
			seed      => $info->{seed},
			bitpix    => $info->{bitpix},
			zeronulls => $info->{zeronulls},
			history   => $info->{history},
			);

	$self->shell($imagetrans);
	return if not $self->isValid;


	#########################################
	# write the WCS keywords to the image
	#########################################
	my $getwcs = $self->buildCommand('getwcs',
			teldef   => $info->{teldefpath},
			coord    => $info->{to},
			segment  => $info->{segment},
			ra       => $info->{ra},
			dec      => $info->{dec},
			outfile  => $info->{outpathext},
			history  => $info->{fullHistory},
			);
	$self->shell($getwcs);


	####################################################
	# adjust the WCS keywords in the output image for
	# windowing and binning
	###################################################
	if ($info->{postxform}) {

		$self->report('transforming WCS');

		my $transform_wcs = $self->buildCommand('transform_wcs',
				infile     => $info->{outpathext},
				transform  => $info->{postxform},
				outfile    => $info->{outpathext},
				history    => $info->{fullHistory},
				);

		$self->shell($transform_wcs);
	}

}



sub combineTransform
{
	my ($self, $target, $command) = @_;

	my $tmp = $self->temporary('combxform');
	if (-e $tmp) {
		unlink($tmp)
			or $self->warning("unable to remove $tmp [$!]");
	}

	my $combinexform = $self->buildCommand('combinexform',
			outfile  => $tmp,
			command  => $command,
			history  => $self->{history},
			);

	my $result = $self->shell($combinexform);
	if ($result->{error}) {
		die($result->{error});
	}

	if (-e $target) {
		unlink($target)
			or $self->warning("unable to remove $target [$!]");
	}

	rename($tmp, $target)
		or $self->error(BAD_OUTPUT,
				"unable to rename $tmp to $target [$!]");
}



sub getRawWindow
{
	my ($self, $header) = @_;

	my $window = { };
	my $tryWCS = 0;
	foreach my $key (qw(WINDOWX0 WINDOWY0 WINDOWDX WINDOWDY BINX BINY)) {
		if (not exists($header->{$key})) {
			$tryWCS = 1;
			$self->warning("input header missing $key, will try WCS");
			last;
		}
	}

	if ($tryWCS) {
		my $wcs = $self->{task}->getWCS($header);
		if (not $wcs) {
			return;
		}

		my $x0 = (0.5 - $wcs->{CRPIX1}) * $wcs->{CDELT1}
							+ $wcs->{CRVAL1} + 0.5;
		my $y0 = (0.5 - $wcs->{CRPIX2}) * $wcs->{CDELT2}
							+ $wcs->{CRVAL2} + 0.5;

		my $ctype1 = $header->{CTYPE1};
		if ($ctype1 ne 'RAWX') {
			$self->error(BAD_INPUT,
					"unable to derive UVOT RAW window from non-RAW WCS");
		}
		elsif (fmod($wcs->{CDELT1}, 1) != 0 or fmod($wcs->{CDELT2}, 1) != 0) {
			$self->error(BAD_INPUT,
					"CDELTn not integral [$wcs->{CDELT1}/$wcs->{CDELT2}]");
		}
		elsif (abs(fmod($x0, 1)) != 0 or abs(fmod($y0, 1)) != 0) {
			$self->error(BAD_INPUT,
					"X0/Y0 not integral [$x0/$y0]");
		}
		else {
			$window->{x0} = $x0;
			$window->{y0} = $y0;
			$window->{dx} = $header->{NAXIS1} * $wcs->{CDELT1};
			$window->{dy} = $header->{NAXIS2} * $wcs->{CDELT2};
			$window->{binx} = $wcs->{CDELT1};
			$window->{biny} = $wcs->{CDELT2};
		}
	}
	else {
		$window->{x0} = $header->{WINDOWX0};
		$window->{y0} = $header->{WINDOWY0};
		$window->{dx} = $header->{WINDOWDX};
		$window->{dy} = $header->{WINDOWDY};
		$window->{binx} = $header->{BINX};
		$window->{biny} = $header->{BINY};
	}

	return $window;
}


sub getBoundingTransform
{
	my ($self, $info, %args) = @_;

	my $header = $args{header};
	my $teldef = $args{teldef};

	my $window = $self->getRawWindow($header);
	return if not $window;

	my $binx = $window->{binx};
	my $biny = $window->{biny};

	############################################
	# but we get the offset in different ways 
	# depending on the coordinate type
	############################################
	my @window = (undef, undef);

	if ($info->{from} eq 'RAW') {
		###########################################################
		# for raw coordinates we can just use the WINDOW keywords
		# These give the position of the center of the first unbinned
		# pixel in teldef RAW coordinates.
		# we subtract a half pixel to give us the position of
		# the pixel corner rather than the center. This is to be
		# consistent with what we get when we use the WCS keywords
		# to determine windowing.
		###########################################################
		$window[0] = $window->{x0} - 0.5;
		$window[1] = $window->{y0} - 0.5;

	} else {

		################################################
		# get the WCS keywords which would be used for 
		# an unwindowed and unbinned image
		################################################
		my $telwcs = $self->temporary('telwcs');

		my $getwcs = $self->buildCommand('getwcs',
					teldef    => $info->{teldefpath},
					coord     => $info->{from},
					segment   => $info->{segment},
					ra        => $info->{ra},
					dec       => $info->{dec},
					outfile   => $telwcs,
					history   => $info->{fullHistory},
					);

		$self->shell($getwcs);
		return if not $self->isValid;

		my $teldef_wcs = $self->loadHeader($telwcs);
		return if not $self->isValid;

		####################################################
		# compare the two sets of WCS keywords to get
		# the coordinate of the center of the first pixel
		###################################################
		foreach my $axis (1, 2) {

			###############################################
			# first get the world coordinate of the corner
			# of the first pixel in the input image
			###############################################
			my $crpix = $header->{"CRPIX$axis"};
			my $crval = $header->{"CRVAL$axis"};
			my $cdelt = $header->{"CDELT$axis"};

# TODO: not necessarily linear
			my $world = (0.5 - $crpix) * $cdelt + $crval;

			#################################################
			# then convert the world coordinate to a pixel
			# coordinate in the official teldef coordinate
			# system
			##################################################

			$crpix = $teldef_wcs->{"CRPIX$axis"};
			$crval = $teldef_wcs->{"CRVAL$axis"};
			$cdelt = $teldef_wcs->{"CDELT$axis"};

# TODO: not necessarily linear
			$window[$axis-1] = ($world - $crval) / $cdelt + $crpix;

		} # end of loop over axes

	} # end if these are not RAW


	##############################################################
	# now that we have the coordinate of the corner of the first
	# pixel in the windowed image, find the offset between that
	# value and the pixel number of the corner of the first
	# pixel as specified in the teldef file
	#############################################################
	my $first_x = $teldef->{$info->{from} . 'XPIX1'};
	my $first_y = $teldef->{$info->{from} . 'YPIX1'};

	$self->report("window[0]=$window[0] first_x=$first_x")
			if $self->chatter(5);
	$self->report("window[1]=$window[1] first_y=$first_y")
			if $self->chatter(5);

	my $off_x = $window[0] - ($first_x - 0.5);
	my $off_y = $window[1] - ($first_y - 0.5);

	$self->report("off_x=$off_x off_y=$off_y")
		if $self->chatter(5);


	########################################################
	# now construct a transform which goes from the
	# windowed and binned image to the official teldef 
	# pixel numbering, and then does the transform to the
	# unwindowed and unbinned coordinates.
	########################################################
	my $in2tel = $self->temporary('in2tel', ext => '.xform');
	{
		my $adjust = "trans(-0.5, -0.5)"
				. " scale($binx, $biny)"
				. " trans(+0.5, +0.5)"
				. " trans($off_x, $off_y)"
				. " file($info->{tel2tel})";

		$self->combineTransform($in2tel, $adjust);
	}


	############################################################
	# now we handle windowing and binning in the output image
	# this is derived from the wndowing and binning of the
	# input image. The binning should be the same in both images, 
	# and we set the windowing to just include the bounding box
	# of the input image.
	############################################################
	my $to_offx = 0.0;
	my $to_offy = 0.0;

	############################################
	# get the dimensions of the input image
	############################################
	my $dimenx = $header->{NAXIS1};
	my $dimeny = $header->{NAXIS2};

	################################################
	# make a list of coordinates which "walk" the
	# boundary of the input image. We have to do
	# this since the UVOT RAW -> DET transform 
	# has non-linear distortion corrections.
	# So we can't just transform the corners
	################################################
	my $in_coords = $self->temporary('incoord');
	{
		my $fh = FileHandle->new($in_coords, 'w');
		if (not $fh) {
			$self->error(BAD_OUTPUT, "unable to create $in_coords [$!]");
			return;
		}

		for (my $i = 1; $i <= $dimenx + 1; $i++) {
			my $x = $i - 0.5;
			my $y = $dimeny + 0.5;
			$fh->print("$x 0.5\n");
			$fh->print("$x $y\n");
		}

		for (my $i = 2; $i<= $dimeny; $i++) {
			my $x = $dimenx + 0.5;
			my $y = $i - 0.5;

			$fh->print("0.5 $y\n");
			$fh->print("$x $y\n");
		}
		$fh->close;
	}

	################################################
	# transform the coordinates to the unbinned and
	# unwindowed output image coordinates.
	################################################
	my $out_coords = $self->temporary('outcoord');
	my $applyxform = $self->buildCommand('applyxform',
					infile     => $in_coords,
					transform  => $in2tel,
					outfile    => $out_coords,
					history    => $info->{fullHistory},
					);

	$self->shell($applyxform);
	return if not $self->isValid;


	################################################
	# now read the transformed coordinates 
	# and get the bounding box
	################################################
	my $minx = undef;
	my $miny = undef;
	my $maxx = undef;
	my $maxy = undef;
	{
		my $fh = FileHandle->new($out_coords);
		if (not $fh) {
			$self->error(BAD_INPUT,
					"unable to open applyxform output $out_coords [$!]");
			return;
		}
		my $first = $fh->getline;
		my (undef, undef, $x0, $y0)=  split(/\s+/, $first);
		$minx = $maxx = $x0;
		$miny = $maxy = $y0;

		while (<$fh>) {

			my ($dumx, $dumy, $x, $y) = split;

			if ($x < $minx) { $minx = $x; }
			if ($x > $maxx) { $maxx = $x; }
			if ($y < $miny) { $miny = $y; }
			if ($y > $maxy) { $maxy = $y; }
		}
		$fh->close;

		$self->report("minx=$minx, miny=$miny maxx=$maxx, maxy=$maxy")
			if $self->chatter(4);
	}


	#################################################################
	# now calculate the offset between the lower left corner of
	# the bounding box and the lower left corner of the teldef
	# coordinates.
	#################################################################
	my $to_first_x = $teldef->{$info->{to} . 'XPIX1'};
	my $to_first_y = $teldef->{$info->{to} . 'YPIX1'};

	$to_offx = $minx - ($to_first_x - 0.5);
	$to_offy = $miny - ($to_first_y - 0.5);


	###################################################
	# now expand the bounding box to the pixel grid
	###################################################
	$to_offx = floor($to_offx);
	$to_offy = floor($to_offy);

	my $to_dimenx = ceil($maxx - $to_first_x - $to_offx + 0.5);
	my $to_dimeny = ceil($maxy - $to_first_y - $to_offy + 0.5);


	$self->report("to_offx=$to_offx to_offy=$to_offy "
				  . "to_dimenx=$to_dimenx to_dimeny=$to_dimeny")
			if $self->chatter(4);


	########################################
	# we're doing the inverse transform, so
	# invert some things
	########################################
	$to_offx = - $to_offx;
	$to_offy = - $to_offy;

	my $to_binx = 1. / $binx;
	my $to_biny = 1. / $biny;


	###############################################################
	# now actually apply the windowing and binning to the image
	###############################################################

	$info->{postxform} = $self->temporary('post', ext => '.xform');

	{
		my $adjust = "trans($to_offx, $to_offy)"
					. " trans(-0.5, -0.5)"
					. " scale($to_binx, $to_biny)"
					. " trans(+0.5, +0.5)";
		$self->combineTransform($info->{postxform}, $adjust);
	}


	{
		$info->{transform} = $self->temporary('in2out', ext => '.xform');
		my $command = "file($in2tel)"
					. " file($info->{postxform})";

		$self->combineTransform($info->{transform}, $command);
	}

	############################################
	# adjust the output image size for binning
	############################################
	$info->{to_dimenx} = int(($to_dimenx + $binx-1) / $binx);
	$info->{to_dimeny} = int(($to_dimeny + $biny-1) / $biny);
}


1;

