HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= ascalin

HD_CTASK_SRC_c		= hascalin.c

HD_CTASK_SRC_f		= ascalin.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_SHELL_SCRIPTS = ascalin.e gis3bitfix.e

HD_INSTALL_PFILES	= ascalin.par

HD_INSTALL_HELP		= ascalin.txt

include ${HD_STD_MAKEFILE}
