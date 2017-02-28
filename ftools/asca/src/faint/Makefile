HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= faint

HD_CTASK_SRC_c		= hfaint.c faintv31.c faintv40.c

HD_CTASK_SRC_f		= faint.f faintv50.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= faint.par

HD_INSTALL_HELP		= faint.hlp faint.txt

include ${HD_STD_MAKEFILE}
