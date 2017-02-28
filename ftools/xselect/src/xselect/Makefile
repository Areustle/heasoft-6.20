HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= xselect

HD_CTASK_SRC_c		= xsel_cxpi.c

HD_CTASK_SRC_f		= xsel.f xsel_xpi.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS} -I../../include

HD_CLIBS		= ${HD_LFLAGS} \
			  -lxself${FT_VERS} -l${CFTOOLSLIB} -l${FTOOLSLIB} \
			  -lplt -l${XANLIB} -l${PIL} -l${READLINE} \
			  -l${CFITSIO} ${LINK_PGPLOT} \
			  ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

include ${HD_STD_MAKEFILE}
