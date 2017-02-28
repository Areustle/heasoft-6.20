HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= ascatape

HD_CTASK_SRC_c		= hascatape.c tapeio.c

HD_CTASK_SRC_f		= ascatape.f subdir_sort.f utility_unix.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ascatape.par

HD_INSTALL_HEADERS	= tapeio.h

HD_INSTALL_HELP		= ascatape.hlp ascatape.txt

include ${HD_STD_MAKEFILE}
