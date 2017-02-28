HD_COMPONENT_NAME       = hitomi

HD_COMPONENT_VERS       =

HD_CXXTASK              = ahmkehk

HD_CXXTASK_SRC_cxx      = ${HD_CXXTASK}.cxx

HD_CXXFLAGS             = ${HD_STD_CXXFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CXXLIBS              = ${HD_LFLAGS}  -lcoordfits -lcoord -l${ATFUNCTIONS} ${HD_STD_CLIBS} \
			  -l${AHMATH} -l${AHTIME} -l${AHMISSION} ${AH_STD_LIBS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C} -l${HEAAPP} -lgeomag

HD_INSTALL_TASKS        = ${HD_CXXTASK}

HD_INSTALL_PFILES       = ${HD_CXXTASK}.par

HD_INSTALL_HELP         = ${HD_CXXTASK}.html

HD_INSTALL_REFDATA	= rigidity_20060421.fits rigidity_20160101.fits

include ${HD_STD_MAKEFILE}
