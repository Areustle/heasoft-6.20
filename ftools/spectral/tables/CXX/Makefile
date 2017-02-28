HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_CXXTASK		= testtable

HD_CXXTASK_SRC_cxx	= testtable.cxx TableModel.cxx

HD_CXXFLAGS		= ${HD_STD_CFLAGS}

HD_CXXLIBS		= ${HD_LFLAGS} -l${CCFITS} -l${CFITSIO} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CXXTASK}

HD_CLEAN		= *.o testtable

include ${HD_STD_MAKEFILE}
