HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= autocor

HD_CTASK_SRC_c		= hautocor.c

HD_CTASK_SRC_f		= autocor.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lxron${FT_VERS} -l${CFITSIO} -lplt \
			  -l${XANLIB} -l${PIL} -l${READLINE} ${LINK_PGPLOT} \
			  ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= autocor.par

HD_INSTALL_HELP		= autocor.hlp autocor.txt

include ${HD_STD_MAKEFILE}
