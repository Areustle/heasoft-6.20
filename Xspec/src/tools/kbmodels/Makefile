HD_COMPONENT_NAME       = Xspec

HD_COMPONENT_VERS       = 

HD_CXXTASK              = kbmodels

HD_CXXTASK_SRC_cxx      = kbmodels.cxx

HD_CXXFLAGS             = ${HD_STD_CXXFLAGS}

HD_CXXLIBS              = ${HD_LFLAGS} -lXSFunctions -lXSModel -lXSUtil -lXS \
			  -l${HEASP} -l${CCFITS} -l${HEAINIT} -l${HEAUTILS} \
		          -l${PIL} -l${HEAIO} -l${WCSLIB} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS        = ${HD_CXXTASK}

HD_INSTALL_PFILES       = kbmodels.par

HD_INSTALL_HELP		= kbmodels.html

include ${HD_STD_MAKEFILE}

