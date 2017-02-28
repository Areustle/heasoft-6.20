HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= exoleuv

HD_CTASK_SRC_c		= hexoleuv.c

HD_CTASK_SRC_f		= exoleuv.f exosat_uv.f leuv_make.f leuv_par.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lexo${FT_VERS} \
			  -l${CFITSIO} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= exoleuv.par

HD_INSTALL_HELP		= exoleuv.hlp exoleuv.txt

include ${HD_STD_MAKEFILE}
