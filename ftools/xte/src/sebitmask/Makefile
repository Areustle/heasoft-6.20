HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= sebitmask

HD_CTASK_SRC_c		= hsebitmask.c boolpars.c sebithelp.c

HD_CTASK_SRC_f		= sebitmask.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= sebitmask.par

HD_INSTALL_HELP		= sebitmask.hlp sebitmask.txt

include ${HD_STD_MAKEFILE}
