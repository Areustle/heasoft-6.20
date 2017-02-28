HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT         = aste_xis

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_CFLAGS		= \
-I../../aste_anl/include/ \
-I../../com_cli/include/ \
-I../../xis/include/ \
-I../../com/src/function/aetimeUtil/ \
-I../../com/src/function/aeFitsHeaderUtil/ \
-I../../com/src/function/aste_caldb/ \
-I../../com/src/function/aste_gethk/ \
-I../../com/src/function/aste_ti2time/ \
-I../../com/src/function/aste_gti/ \
-I../../com/src/function/aste_orbit/ \
-Ifunction/xisEditEventFitsUtil/ \
-Ifunction/xisEventFitsUtil/ \
-Ifunction/xisFitsHeaderUtil/ \
-Ifunction/xisGrade/ \
-Ifunction/xisNamingFunc/ \
-Ifunction/xisPixelQuality/ \
-Ifunction/xisRespUtil/ \
-Ifunction/xisSciUtil/ \
-Ifunction/xisUtility/ \
-Imodule/XISeditEventFits/ \
-Imodule/XISputPixelQuality/ \
-D__XISREAD__ \
${HD_STD_CFLAGS}
# -D__XISREAD__ is a kludge for xisEventFitsUtil.c and xisFitsHeaderUtil.c

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_LIBRARY_SRC_c	= \
function/xisEditEventFitsUtil/xisEditEventFitsUtil.c \
function/xisEventFitsUtil/xisEventFitsUtil.c \
function/xisFitsHeaderUtil/xisFitsHeaderUtil.c \
function/xisGrade/xisGradeUtil.c \
function/xisNamingFunc/xisNamingFunc.c \
function/xisPixelQuality/xisPixelQuality.c \
function/xisRespUtil/xisRespUtil.c \
function/xisSciUtil/xisSciUtil.c \
function/xisUtility/xisutility.c \
module/xis5x5to3x3/xis5x5to3x3_body.c \
module/xisnxbgen/xisnxbgen_body.c \
module/xisucode/xisucode_body.c \
module/XIScoord/XIScoord.c \
module/XISctiCorrection/XISctiCorrection.c \
module/XISeditEventFits/XISeditEventFits.c \
module/XISgrade/XISgrade.c \
module/XISpha2pi/XISpha2pi.c \
module/XISpreparePHASCORR/XISpreparePHASCORR.c \
module/XISputPixelQuality/XISputPixelQuality.c \
module/XISreadEvent/XISreadEvent.c \
module/XISreadExp/XISreadExp.c \
module/XISreadFrame/XISreadFrame.c \
module/XISreadHotpixels/XISreadHotpixels.c \
module/XIStime/XIStime.c \
module/XIStrailCorrection/XIStrailCorrection.c \
module/XISrmfgen/XISrmfgen_body.c \
module/XIScheckEventNo/XIScheckEventNo.c \
module/XIScheckLLE/XIScheckLLE.c \
module/XISexpMapGen/XISexpMapGen.c

HD_SHLIB_LIBS		= \
${HD_LFLAGS} -lastetool -laste_com -lBNK -lEVS \
-lCLI -lCOM -l${ATFUNCTIONS} -l${CFITSIO} -l${PIL}

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o}

include ${HD_STD_MAKEFILE}
