HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fvelmap

HD_CTASK_SRC_c		= hfvelmap.c

HD_CTASK_SRC_f		= binmaps.f firstevent.f fvelmap.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lvela${FT_VERS} \
			  -l${CFTOOLSLIB} -l${FTOOLSLIB} -l${CFITSIO} \
			  -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= fvelmap.par

HD_INSTALL_HELP		= fvelmap.hlp fvelmap.txt

include ${HD_STD_MAKEFILE}
