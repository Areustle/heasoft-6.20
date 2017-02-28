HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= efsearch

HD_CTASK_SRC_c		= hefsearch.c

HD_CTASK_SRC_f		= efsearch.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lxron${FT_VERS} -l${CFITSIO} -lplt \
			  -l${XANLIB} -l${PIL} -l${READLINE} ${LINK_PGPLOT} \
			  ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= efsearch.par

HD_INSTALL_HELP		= efsearch.hlp efsearch.txt

include ${HD_STD_MAKEFILE}
