
HD_COMPONENT_NAME	= hitomi

HD_COMPONENT_VERS	= 

HD_CTASK		= sxsperseus

HD_CTASK_SRC_c		= sxsperseus.c

HD_CFLAGS		= ${HD_STD_CXXFLAGS}

HD_CLIBS		= ${HD_LFLAGS}  -lahlog_1.0 \
			  -lparam_wrappers -l${HEAAPP} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= sxsperseus.par

HD_INSTALL_HEADERS	=

HD_INSTALL_REFDATA      = ahsxs_offsets.fits ahsxs_dggain.fits

HD_INSTALL_HELP		= ${HD_CTASK}.html 

include ${HD_STD_MAKEFILE}
