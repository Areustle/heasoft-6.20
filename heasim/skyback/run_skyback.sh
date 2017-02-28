#! /bin/sh

# THIS SCRIPT RUNS THE SKY BACKGROUND TOOL.

############# ASSIGNMENT BLOCK ##############
# Create a definition block here, so we can preserve the useful comments for each parameter

outfileroot="heasim" # Output file name base
exposure=10000       # exposure time in sec
ra=150.00    # central RA in decimal degrees
dec=50.00    # central Dec in decimal degrees
radius=30.0      # radius of FOV in arcmin
emin=0.1         # energy grid minimum in keV
emax=10.0        # energy grid maximum in keV
de=0.025         # energy grid spacing in keV
flaglogns=yes    # include emission from logN-logS ?
flaggal=yes      # include soft Xray Galactic and LHB emission ?
flagswcx=no        # include SWCX emission ?
flagdgrb=no        # include diffuse gamma ray emission ?
flaggrxe=no        # include galactic ridge emission ?
slopebright1=1.7   # logN-logS slope 1, bright end
slopefaint1=0.9    # logN-logS slope 1, faint end
fluxbreak=2.5e-14  # logN-logS flux at power-law break
norm1=8.0e3         # logN-logS normalization in sources/sq.degree for slope 1
slope2=1.1          # logN-logS slope 2
norm2=0.0e0         # logN-logS normalization in sources/sq.degree for slope 2
fluxsens=1.0e-14    # logN-logS flux sensitivity limit
sigtonoise=5.0                # logN-logS signal to noise corresponding to flux limit
fluxmin=1.0e-16               # lower flux limit of logN
fluxmax=5.0e-13               # upper flux limit of logNS in erg/cm^2/sec, both components
ctstoflux=1.0e-11   # count rate to flux conversion in erg/cm^2/sec/ct
bandpasslo=0.5      # lower limit in keV for bandpass over which logN-logS is defined
bandpasshi=2.0      # upper limit in keV for bandpass over which logN-logS is defined
spectype1=0         # spectral type for slope-1 source: 0=single_spectrum, 1=multi, 2=torus
specmod1=pow      # slope-1 source spectral model if spectype1=0 (single spectrum)
specpar1=1.9      # slope-1 source spectral parameter if spectype1=0 (single spectrum)
nhmod1=0.0        # slope-1 source spectral model NH if spectype1=0 (single spectrum)
fabs0=0.24    # fraction of slope-1 sources, NH < 1e21 cm^-2
fabs1=0.24    # fraction of slope-1 sources, NH=1e21 - 1e22 cm^-2
fabs2=0.24    # fraction of slope-1 sources, NH=1e22 - 1e23 cm^-2
fabs3=0.24    # fraction of slope-1 sources, NH=1e23 - 1e24 cm^-2
fabs4=0.03    # fraction of slope-1 sources, NH=1e24 - 1e25 cm^-2
fabs5=0.01    # fraction of slope-1 sources, NH > 1e25 cm^-2
fpar0=0.2    # fraction index=1.5-1.7 if spectype1=1, opening angle < 30 if spectype1=2
fpar1=0.2    # fraction index=1.7-1.9 if spectype1=1, opening angle 30-45 if spectype1=2
fpar2=0.2    # fraction index=1.9-2.1 if spectype1=1, opening angle 45-60 if spectype1=2
fpar3=0.2    # fraction index=2.1-2.3 if spectype1=1, opening angle 60-75 if spectype1=2
fpar4=0.2    # fraction index=2.3-2.5 if spectype1=1, opening angle 75-90 if spectype1=2
samespec=yes   # do slope-1 and slope-2 sources have the same spectra?
specmod2=pow   # heasim-supported spectral model, ignored if samespec == yes
specpar2=1.9   # heasim-supported spectral model parameter, ignored if samespec == yes
nhmod2=0.0     # absorption, ignored if samespec == yes
debug=no                 # print additional diagnostic output to terminal screen
clobber=yes              # overwrite existing output files
mode=ql                  # query mode for automatic parameters
seed=1234567890    # seed for random number generator.  If < 0, ignore and seed from system time
swcxOVII=0.0       # Solar Wind Continuum Exchange OVII line flux in LU; 1 LU = 1 photon/s/cm^2/str
swcxcont=3.17e-16  # Solar Wind Continuum Exchange continnum flux




######### EXECUTION BLOCK ###################
# This is the actual execution block, since there can be no comments in the function call.

punlearn skyback
skyback \
    outfileroot=$outfileroot \
    exposure=$exposure \
    ra=$ra \
    dec=$dec \
    radius=$radius \
    emin=$emin \
    emax=$emax \
    de=$de \
    flaglogns=$flaglogns \
    flaggal=$flaggal \
    flagswcx=$flagswcx \
    flagdgrb=$flagdgrb \
    flaggrxe=$flaggrxe \
    slopebright1=$slopebright1 \
    slopefaint1=$slopefaint1 \
    fluxbreak=$fluxbreak \
    norm1=$norm1 \
    slope2=$slope2 \
    norm2=$norm2 \
    fluxsens=$fluxsens \
    sigtonoise=$sigtonoise \
    fluxmin=$fluxmin \
    fluxmax=$fluxmax \
    ctstoflux=$ctstoflux \
    bandpasslo=$bandpasslo \
    bandpasshi=$bandpasshi \
    spectype1=$spectype1 \
    specmod1=$specmod1 \
    specpar1=$specpar1 \
    nhmod1=$nhmod1 \
    fabs0=$fabs0 \
    fabs1=$fabs1 \
    fabs2=$fabs2 \
    fabs3=$fabs3 \
    fabs4=$fabs4 \
    fabs5=$fabs5 \
    fpar0=$fpar0 \
    fpar1=$fpar1 \
    fpar2=$fpar2 \
    fpar3=$fpar3 \
    fpar4=$fpar4 \
    samespec=$samespec \
    specmod2=$specmod2 \
    specpar2=$specpar2 \
    nhmod2=$nhmod2 \
    debug=$debug \
    clobber=$clobber \
    mode=$mode \
    seed=$seed \
    swcxOVII=$swcxOVII \
    swcxcont=$swcxcont
    
