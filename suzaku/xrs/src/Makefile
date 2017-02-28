HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT         = aste_xrs

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_CFLAGS		= \
-I../../aste_anl/include/ \
-I../../com_cli/include/ \
-I../../xrs/include/ \
-Ifunction/xrsFitsHeaderUtil/ \
-Ifunction/xrs_gain/ \
-I../../com/src/function/aeFitsHeaderUtil/ \
-I../../com/src/function/aste_ti2time/ \
${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_LIBRARY_SRC_c	= \
function/xrsFitsHeaderUtil/xrsFitsHeaderUtil.c \
function/xrs_gain/xrs_gain.c \
module/XRSpixelCoincidence/XRSpixelCoincidence.c \
module/XRSeventFitsRDWT/XRSeventFitsRDWT.c \
module/XRSdriftMake/XRSdriftMake.c \
module/XRSphaToPI/XRSphaToPI.c \
module/XRSpixelToXY/XRSpixelToXY.c \
module/XRStimeSet/XRStimeSet.c \
module/XRStimeSet/xrs_tick2ti.c \
module/xrsimage/ephemeris.c \
module/xrsimage/image.c \
module/xrsimage/info.c \
module/xrsimage/param.c \
module/xrsimage/polygon.c \
module/xrsimage/xrs_cornercoord.c \
module/xrsimage/xrs_det_image.c \
module/xrsimage/xrs_sky_image.c \
module/xrsimage/xrsimage.c

HD_LIBRARY_SRC_f	= \
module/xrsqlp/xrsqlp.f

HD_SHLIB_LIBS		= \
${HD_LFLAGS} -lastetool -laste_com -lBNK -lEVS \
-lCLI -lCOM -l${ATFUNCTIONS} -l${CFITSIO} -l${PIL}

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o} ${HD_LIBRARY_SRC_f:.f=.o}

include ${HD_STD_MAKEFILE}
