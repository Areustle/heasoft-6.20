HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= sisclean

HD_CTASK_SRC_c		= hsisclean.c

HD_CTASK_SRC_f		= sisclean.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lplt \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} ${LINK_PGPLOT} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= sisclean.par

HD_INSTALL_HELP		= sisclean.hlp sisclean.txt

include ${HD_STD_MAKEFILE}
