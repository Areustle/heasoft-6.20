HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= decodeevt

HD_CTASK_SRC_c		= hdecodeevt.c decodeevt.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -l${HEAUTILS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= decodeevt.par

HD_INSTALL_HELP		= decodeevt.hlp decodeevt.txt

include ${HD_STD_MAKEFILE}
