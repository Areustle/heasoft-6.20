HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= timeconv

HD_CTASK_SRC_c		= htimeconv.c asca_geocen.c

HD_CTASK_SRC_f		= asca_barycen.f timeconv.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= timeconv.parin

HD_INSTALL_HELP		= timeconv.hlp timeconv.txt

include ${HD_STD_MAKEFILE}
