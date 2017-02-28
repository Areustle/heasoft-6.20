HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= correctrdd

HD_CTASK_SRC_c		= hcorrectrdd.c correctrdd.c

HD_CTASK_SRC_f		= icorrectrdd.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= correctrdd.par

HD_INSTALL_HELP		= correctrdd.hlp correctrdd.txt

include ${HD_STD_MAKEFILE}
