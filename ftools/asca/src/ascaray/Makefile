HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= ascaray

HD_CTASK_SRC_c		= hascaray.c

HD_CTASK_SRC_f		= ascaray.f ray_anl.f ray_owens.f

# Turn off optimization for this task (${COPT} not included):
HD_CFLAGS               = ${CFLAGS} ${HD_IFLAGS} ${DEFS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_HELP		= ascaray.hlp ascaray.txt

HD_INSTALL_PFILES	= ascaray.parin

HD_INSTALL_REFDATA	= ascaxrt1_0.fits

include ${HD_STD_MAKEFILE}
