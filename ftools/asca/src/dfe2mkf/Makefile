HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= dfe2mkf

HD_CTASK_SRC_c		= hdfe2mkf.c

HD_CTASK_SRC_f		= dfe2mkf.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= dfe2mkf.par

HD_INSTALL_HELP		= dfe2mkf.hlp dfe2mkf.txt

include ${HD_STD_MAKEFILE}
