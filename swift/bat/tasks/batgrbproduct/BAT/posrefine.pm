# Perform position refinement of a BAT flight position
#
# This module uses batcelldetect to compute a refined position for a source
# based on an initial flight position.
#
# refine - position refinement
#
# $self - class name (ignored)
# $info - generic information hash reference
#         INPUTS: $info->{$possrc} - initial [RA,Dec] of source in degrees
#                 $info->{err_rad} - estimated error radius in degrees
#         OUTPUTS:$info->{batcelldetect_poserr} - 1 if position error is bad
#                 $info->{refined_pos} - revised [RA,Dec] of source in deg
#                 $info->{refined_err} - revised error radius in deg
#                 $info->{refined_chi2} - chi-square value of fit
#                 $info->{refined_snr} - signal to noise ratio
# $log - name of running BAT command long
# $incat - name of input catalog (to be created by this task)
# $outcat - name of output catalog with refined position
# $img - name of image file to use
# $pcodefile - name of partial coding file to use
# $possrc - origin of source position, to be used to index $info->{$possrc}
# $trig_id - name of trigger to be used in catalog file
# $region - name of output region file 
#
# RETURNS: revised $info
#
# EXCEPTIONS: 
#     * failed to create input catalog
#     * failed to write input catalog data
#     * batcelldetect failed
#     * failed to open output catalog for reading
#     * failed to read output catalog data
#

package BAT::posrefine;
use Astro::FITS::CFITSIO qw(:longnames :constants);
use BAT::log;

sub refine {
    my ($self,$info,$log,$incat,$outcat,$img,$pcodefile,$possrc,$trig_id,$region) = @_;
    my ($pcodethresh);
    my ($fits,$status,$cmd);
    my ($ra,$dec,$err_rad);
    my ($ra_new,$dec_new,$err_rad_new,$radec_chi2,$snr,$line);

    # Extract position from info structure
    $ra  = $info->{$possrc}->[0];
    $dec = $info->{$possrc}->[1];
    $err_rad = $info->{err_rad};
    $region = "NONE" unless $region;
    $pcodefile = "NONE" unless $pcodefile;
    $pcodethresh = $info->{pcodethresh};

    # Remove input/output catalogs
    unlink("$incat");
    unlink("$outcat");

    # Make input catalog
    $fits = SimpleFITS->open(">$incat");
    $status = $fits->status();
    die "ERROR: could not open $incat for writing" if $status;

    # Write catalog columns
    $status = $fits
	->createtab("BAT_CATALOG")
	->writekey("NAXIS2",1)
	->insertcol({TTYPE => ["NAME", "Source Name"],
		     TFORM => "20A"})
	->insertcol({TTYPE => ["RA_OBJ",  "Source Right Ascension"],
		     TFORM => "1D", TUNIT => "deg"})
	->insertcol({TTYPE => ["DEC_OBJ",  "Source Declination"],
		     TFORM => "1D", TUNIT => "deg"})
	->insertcol({TTYPE => ["ERR_RAD",  "Source Error Radius"],
		     TFORM => "1D", TUNIT => "deg"})
	->writecol("NAME",   {},"TRIG_$trig_id")
	->writecol("RA_OBJ", {},$ra)
	->writecol("DEC_OBJ",{},$dec)
	->writecol("ERR_RAD",{},$err_rad)
	->status();
    
    die "ERROR: could not create table in $incat" if $status;
    $fits->close();

    unlink ($region) if ($region ne "NONE" && -f $region);
    # Run source detection for this source
    $cmd = "batcelldetect infile='$img' outfile=$outcat snrthresh=6.0 incatalog='$incat' ".
	"srcdetect=NO posfit=YES niter=1 regionfile=$region pcodethresh='$pcodethresh' ".
	"pcodefile='$pcodefile' clobber=yes";
    $? = BAT::log->callnote($log,$cmd,
			    "Calculating refined BAT position from $img");
    
    if ($? || ! -f $outcat ) {
	die "ERROR: batcelldetect failed in position refinement";
    }
    
    # Check for bad version of batcelldetect
    foreach $line (@$log) {
	if ($line =~ m/batcelldetect v1\.[0-9]\b/) {
	    $info->{batcelldetect_poserr} = 1;
	}
    }

    $fits = SimpleFITS->open("<$outcat")->move("BAT_CATALOG");
    $status = $fits->status();
    die "ERROR: could not open $outcat" if ($status);

    $status = $fits
	->readcol("RA_OBJ",{type=>TDOUBLE},$ra_new)
	->readcol("DEC_OBJ",{type=>TDOUBLE},$dec_new)
	->readcol("ERR_RAD",{type=>TDOUBLE},$err_rad_new)
	->readcol("CHI2_NU",{type=>TDOUBLE},$radec_chi2)
	->readcol("SNR",{type=>TDOUBLE},$snr)
	->status();
    $fits->setstatus(0)->close();
    return if ($status);
    
    die "ERROR: could not read $outcat" if ($status);
    
    print "     done\n";

    $info->{refined_pos} = [$ra_new->[0], $dec_new->[0]];
    $info->{refined_err} = $err_rad_new->[0];
    $info->{refined_chi2} = $radec_chi2->[0];
    $info->{refined_snr} = $snr->[0];

    return $info;
}

1;
