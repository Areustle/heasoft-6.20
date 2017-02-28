HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= castpart

HD_CTASK_SRC_c		= hcastpart.c

HD_CTASK_SRC_f		= castpart.f putfit.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lros${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= castpart.par

HD_INSTALL_HELP		= castpart.hlp castpart.txt

include ${HD_STD_MAKEFILE}
