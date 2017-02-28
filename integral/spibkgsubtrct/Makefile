HD_COMPONENT_NAME	= integral

HD_COMPONENT_VERS	= 

HD_CXXTASK		= spibkgsubtrct

HD_CXXTASK_SRC_c	= ${HD_CXXTASK}.c

HD_CXXTASK_SRC_cxx	= spibkg_common.cxx spibkgHi.cxx spibkgLo.cxx \
			  spibkgMed.cxx

HD_CFLAGS		= ${HD_STD_CFLAGS}

# until Xspec header files are installed & referred to in a consistent way:
XSPEC_SRC		= ${HD_SRC}/../Xspec

HD_CXXFLAGS		= -I${XSPEC_SRC}/src \
			  -I${XSPEC_SRC}/src/main \
			  -I${XSPEC_SRC}/src/include \
			  -I${XSPEC_SRC}/src/XSFit \
			  -I${XSPEC_SRC}/src/XSFit/FitMethod \
			  -I${XSPEC_SRC}/src/XSFunctions \
			  -I${XSPEC_SRC}/src/xslib \
			  -I${XSPEC_SRC}/src/XSModel \
			  -I${XSPEC_SRC}/src/XSModel/Model \
			  -I${XSPEC_SRC}/src/XSPlot \
			  -I${XSPEC_SRC}/src/XSUser \
			  -I${XSPEC_SRC}/src/XSUtil \
			  -I${XSPEC_SRC}/src/XSUtil/Utils \
			  -I${XSPEC_SRC}/src/XSUtil/Error \
			  ${HD_STD_CXXFLAGS}

HD_CXXLIBS		= ${HD_LFLAGS} ${HD_STD_LIBS} -lXSUtil ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CXXTASK}

HD_INSTALL_HELP		= ${HD_CXXTASK}.txt

HD_INSTALL_PFILES	= ${HD_CXXTASK}.par

HD_INSTALL_HEADERS	= spibkg_common.h

include ${HD_STD_MAKEFILE}
