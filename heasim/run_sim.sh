#! bin/sh

# This script is configured to execute a point source power law simulation, as observed
# with HITOMI instrument SXI.  Edit parameters to change.

# if there's no output directory, then make it.
if [ ! -d output ]; then
    mkdir output
fi

mission="hitomi"                                           # mission
instrume="sxi"                                            # instrume
cal_dir="sxi"                                               # master calibration data directory
resp_dir=$HEASIM_SUPPORT/$mission/$cal_dir/response    # response directory
psf_dir=$HEASIM_SUPPORT/$mission/$cal_dir/psf          # psf directory
vig_dir=$HEASIM_SUPPORT/$mission/$cal_dir/vignette     # vignette directory
back_dir=$HEASIM_SUPPORT/$mission/$cal_dir/background  # background directory
source_dir=$HEASIM_SUPPORT/testdata/source_data                          # directory containing source files

arf=$resp_dir/sxt-i_140505_ts02um_int01.8r.arf
rmf=$resp_dir/ah_sxi_20120702.rmf
outfile=output/hitomi_sxi_plaw.fits                            


############# ASIGNMENT BLOCK ##############

mission=$mission
instrume=$instrume
filter=none
instmode=none
rapoint=150.00
decpoint=50.00
roll=0.00
exposure=10000.
flagsubex=no
#subexposure=1000.
resample=yes
insrcdeffile=$source_dir/plaw_test.txt
outfile=$outfile
psffile=$psf_dir/sxt-i_EEF_4p5keV_140617_type1.fits
vigfile=$vig_dir/SXT_VIG_140618_type1.fits
rmffile=$rmf
arffile=$arf
arfrmftol=1.0e0
intbackfile=$back_dir/ah_sxi_pch_nxb_full_20110530.pi
psbackfile=none
difbackfile=none
pszbackfile=none
dtpileup=0.0
getinfile=no
debug=no
clobber=yes
mode=ql
mdbfile=$LHEA_DATA/heasim.mdb
seed=1234567890    # if == 0, ignore and seed using system time   


######### EXECUTION BLOCK ###################
opt=""
#opt="gdb --args"
#opt="valgrind -v --leak-check=full --show-reachable=yes"

punlearn heasim
$opt heasim \
    mission=$mission \
    instrume=$instrume \
    filter=$filter \
    instmode=$instmode \
    rapoint=$rapoint \
    decpoint=$decpoint \
    roll=$roll \
    exposure=$exposure \
    flagsubex=$flagsubex \
    resample=$resample \
    insrcdeffile=$insrcdeffile \
    outfile=$outfile \
    psffile=$psffile \
    vigfile=$vigfile \
    rmffile=$rmffile \
    arffile=$arffile \
    arfrmftol=$arfrmftol \
    intbackfile=$intbackfile \
    psbackfile=$psbackfile \
    difbackfile=$difbackfile \
    pszbackfile=$pszbackfile \
    dtpileup=$dtpileup \
    getinfile=$getinfile \
    debug=$debug \
    clobber=$clobber \
    mode=$mode \
    mdbfile=$mdbfile \
    seed=$seed






