HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= sbmerge

HD_CTASK_SRC_c		= hsbmerge.c sbmerge.c

# Turn off optimization for this task (${COPT} not included):
HD_CFLAGS		= ${CFLAGS} ${HD_IFLAGS} ${DEFS}

HD_CLIBS		= ${HD_LFLAGS} -lxte${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= sbmerge.par

HD_INSTALL_HELP		= sbmerge.hlp sbmerge.txt

include ${HD_STD_MAKEFILE}
