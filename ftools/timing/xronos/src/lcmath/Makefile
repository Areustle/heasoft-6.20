HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= lcmath

HD_CTASK_SRC_c		= hlcmath.c

HD_CTASK_SRC_f		= lcmathcopy.f lcmath.f lcmathinit.f lcmathmake.f \
			  lcmathopen.f lcmathrdbkg.f lcmathread.f \
			  lcmathwrite.f lcmathgabvd.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lxron${FT_VERS} -l${CFITSIO} -lplt \
			  -l${XANLIB} -l${PIL} -l${READLINE} ${LINK_PGPLOT} \
			  ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= lcmath.par

HD_INSTALL_HELP		= lcmath.hlp lcmath.txt

include ${HD_STD_MAKEFILE}
