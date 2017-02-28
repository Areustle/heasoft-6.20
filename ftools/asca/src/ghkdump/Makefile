HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= ghkdump

HD_CTASK_SRC_c		= hghkdump.c

HD_CTASK_SRC_f		= ghkdump.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ghkdump.par

HD_INSTALL_HELP		= ghkdump.hlp ghkdump.txt

include ${HD_STD_MAKEFILE}
