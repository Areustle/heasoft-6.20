HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= XSMinuit

HD_LIBRARY_SRC_cxx	= MinuitCalcStat.cxx Minuit.cxx

HD_CXXFLAGS		= -Iminuit2/inc \
                          -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit \
			  -I${HD_SRC}/src/XSFit/FitMethod \
			  -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSModel/Model \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil ${HD_STD_CXXFLAGS} \
			  -DINXSPEC

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HELP		=

HD_INSTALL_HEADERS	= MinuitCalcStat.h Minuit.h

HD_CLEAN                = ${HD_LIBRARY_SRC_cxx:.cxx=.o}

HD_SHLIB_LIBS		= ${HD_LFLAGS} \
			  -lMinuit2 -lXSFit -lXSModel -lXSUtil -lXS \
			  ${GUI_LIBS} -l${TCLREADLINE} -l${TCL} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

include ${HD_STD_MAKEFILE}
