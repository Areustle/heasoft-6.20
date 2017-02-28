HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= axBary

HD_CTASK_SRC_c		= haxBary.c axBary.c bary.c clock.c ctatv.c dpleph.c \
			  phaseHist.c scorbit.c xCC.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -l${CFITSIO} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

include ${HD_STD_MAKEFILE}
