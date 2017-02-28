HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= recofmi

HD_CTASK_SRC_c		= hrecofmi.c closetable.c recofmi.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= recofmi.par

HD_INSTALL_HELP		= recofmi.hlp recofmi.txt

include ${HD_STD_MAKEFILE}
