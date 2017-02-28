HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= xstar

HD_CTASK_SRC_c		= hxstar.c

HD_CTASK_SRC_f		= xstar.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lxstar${FT_VERS} -l${CFTOOLSLIB} -l${FTOOLSLIB} \
			  -l${CFITSIO} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= xstar.par

HD_INSTALL_HELP		= xstar.hlp xstar.txt

include ${HD_STD_MAKEFILE}
