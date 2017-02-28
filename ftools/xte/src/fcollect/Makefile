HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fcollect

HD_CTASK_SRC_c		= hfcollect.c fcollect.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} -l${HEAUTILS} -l${PIL} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= fcollect.par

HD_INSTALL_HELP		= fcollect.hlp fcollect.txt

include ${HD_STD_MAKEFILE}
