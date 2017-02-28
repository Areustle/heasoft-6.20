HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_CXXTASK		= initpackage

HD_CXXTASK_SRC_cxx	= initpackage.cxx ModelMap.cxx

HD_CXXFLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit \
			  -I${HD_SRC}/src/XSFit/FitMethod \
			  -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSModel/Model \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil ${HD_STD_CXXFLAGS} \
			  -DINXSPEC

HD_CXXLIBS		= ${HD_STD_CXXLIBS}

HD_INSTALL_HEADERS	= ModelMap.h xspec.h xsudmget.cxx xsudmget64.cxx

HD_INSTALL_TASKS	= ${HD_CXXTASK}

HD_INSTALL_SHELL_SCRIPTS	= xspackage

HD_INSTALL_EXTRA	= template

include ${HD_STD_MAKEFILE}

template: 
	@${HD_INSTALL} xspackage.tmpl ${HD_LIB}/xspackage.tmpl ${HD_CP_P}
