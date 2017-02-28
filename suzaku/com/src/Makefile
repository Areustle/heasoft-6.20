HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT         = aste_com

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_CFLAGS		= \
-I../../aste_anl/include/ \
-I../../com_cli/include/ \
-I../../xrrt/src/ \
-I../../xrs/include/ \
-I../../xis/include/ \
-Imodule/AEcomHKfitsWrite/ \
-Imodule/SimASTE_Root/ \
-Imodule/aebarycen/ \
-Ifunction/aeFitsHeaderUtil/ \
-Ifunction/aetimeUtil/ \
-Ifunction/aste_aspect/ \
-Ifunction/aste_gethk/ \
-Ifunction/aste_gti/ \
-Ifunction/aste_ti2time/ \
-Ifunction/aste_orbit/ \
-Ifunction/aste_caldb/ \
-Ifunction/axBary/ \
-Ifunction/xis_contami/ \
-Ifunction/xis_effarea/ \
-Ifunction/xis_psf/ \
-I../../xis/src/function/xisSciUtil/ \
-I../../xis/src/function/xisPixelQuality/ \
${HD_STD_CFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	=

HD_LIBRARY_SRC_c	= \
module/aeattcor/aeattcor_body.c \
module/AEaspectATTFILE/AEaspectATTFILE.c \
module/AEcomHKfitsWrite/AEcomHKfitsWrite.c \
module/AEcomHKfitsWrite/comFitsHeaderUtil.c \
module/AEmkEHKfitsWrite/AEmkEHKfitsWrite.c \
module/AEmkEHKtimeGen/AEmkEHKtimeGen.c \
module/SimASTE_ECStoXRTIN/SimASTE_ECStoXRTIN.c \
module/SimASTE_PhotonFitsWrite/SimASTE_PhotonFitsWrite.c \
module/SimASTE_PhotonGen/SimASTE_PhotonGen.c \
module/SimASTE_PhotonRead/SimASTE_PhotonRead.c \
module/SimASTE_Root/SimASTE_Root.c \
module/SimASTE_Root/SimASTE_Util.c \
module/SimASTE_XISRMFsim/SimASTE_XISRMFsim.c \
module/SimASTE_XISarfGen/SimASTE_XISarfGen.c \
module/SimASTE_XISevtFitsWrite/SimASTE_XISevtFitsWrite.c \
module/SimASTE_XRSRMFsim/SimASTE_XRSRMFsim.c \
module/SimASTE_XRSevtFitsWrite/SimASTE_XRSevtFitsWrite.c \
module/SimASTE_XRTOUTtoDET/SimASTE_XRTOUTtoDET.c \
module/SimASTE_XRTsim/SimASTE_XRTsim.c \
module/SimASTE_XRTsim/c_fcerr.c \
module/SimASTE_XRTsim/xrrtrandom.c \
module/aeaspect/aeaspect_body.c \
module/aemkpinudhk/aemkpinudhk_body.c \
module/aetimecalc/aetimecalc_body.c \
module/aecoordcalc/aecoordcalc_body.c \
module/headas_body/headas_body.c \
module/aebarycen/aebarycen_body.c \
module/xiscontamicalc_body/xiscontamicalc_body.c \
module/xisarfgen/xisarfgen_body.c \
function/aeFitsHeaderUtil/aeFitsHeaderUtil.c \
function/aetimeUtil/aetimeUtil.c \
function/aste_aspect/aste_aspect.c \
function/aste_gethk/aste_gethk.c \
function/aste_gti/aste_gti.c \
function/aste_ti2time/aste_ti2time.c \
function/aste_orbit/aste_orbit.c \
function/aste_caldb/aste_caldb.c \
function/axBary/axBary.c \
function/axBary/bary.c \
function/axBary/clock.c \
function/axBary/ctatv.c \
function/axBary/dpleph.c \
function/axBary/haxBary.c \
function/axBary/phaseHist.c \
function/axBary/scorbit.c \
function/axBary/xCC.c \
function/xis_contami/xis_contami.c \
function/xis_effarea/xis_effarea.c \
function/xis_psf/xis_psf.c

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o}

include ${HD_STD_MAKEFILE}
