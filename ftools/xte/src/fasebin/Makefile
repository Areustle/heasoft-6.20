HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fasebin

HD_CTASK_SRC_c		= hfasebin.c bary.c clock.c ctatv.c dpleph.c fasebin.c \
			  phasehist.c scorbit.c xcc.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= fasebin.par

HD_INSTALL_HELP		= fasebin.hlp fasebin.txt

include ${HD_STD_MAKEFILE}
