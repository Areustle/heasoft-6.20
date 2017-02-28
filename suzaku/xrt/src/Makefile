HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	=

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_LIBRARY_ROOT         = aste_xrt

HD_CFLAGS		= \
-I../../aste_anl/include/ \
-I../../com_cli/include/ \
-I../../com/src/function/aste_caldb/ \
-Imodule/XRSarfRoot/ \
-Imodule/XISarfRoot/ \
${HD_STD_CFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_LIBRARY_SRC_c	= \
module/XISarfBuild/XISarfBuild.c \
module/XISarfRoot/XISarfRoot.c \
module/XIScalcExposure/XIScalcExposure.c \
module/XRSarfBuild/XRSarfBuild.c \
module/XRSarfRoot/XRSarfRoot.c \
module/XRScalcExposure/XRScalcExposure.c \
module/XRTeffectiveArea/XRTeffectiveArea.c \
module/XRTthermalShield/XRTthermalShield.c

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o}

include ${HD_STD_MAKEFILE}
