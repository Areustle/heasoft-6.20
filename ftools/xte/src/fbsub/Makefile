HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fbsub

HD_CTASK_SRC_c		= hfbsub.c fbsub.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= fbsub.par

HD_INSTALL_HELP		= fbsub.hlp fbsub.txt

include ${HD_STD_MAKEFILE}
