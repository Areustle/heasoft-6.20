HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= Integral

HD_LIBRARY_SRC_cxx	= Integral.cxx SPI_Data.cxx SPI_Response.cxx

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


HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= SPI_Data.h SPI_Response.h

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${CFITSIO} -l${CCFITS} \
			  -lXSModel -lXSUtil -l${TCL} ${SYSLIBS}

include ${HD_STD_MAKEFILE}
