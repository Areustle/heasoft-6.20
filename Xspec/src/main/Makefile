HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_CXXTASK		= xspec

HD_CXXTASK_SRC_cxx	= xspec.cxx

HD_CXXFLAGS		= -I${HD_SRC}/src -I${HD_SRC}/src/main \
			  -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit \
			  -I${HD_SRC}/src/XSFit/FitMethod \
			  -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSModel/Model \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil ${HD_STD_CXXFLAGS} \
			  ${XINCLUDES} -DINXSPEC

HD_CXXLIBS		= ${HD_LFLAGS} ${XS_CYGWIN_LIBS} -lXSUser \
			  -lXSPlot -lXSMinuit -lMinuit2 -lxsmix -lXSFunctions -lXSFit -lXSModel -lXSUtil \
			  -lplt ${LINK_PGPLOT_TCL} -lXS \
			  ${GUI_LIBS} -l${TCLREADLINE} -l${TCL} -l${WCSLIB} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CXXTASK}

include ${HD_STD_MAKEFILE}

purify:
	$(PURIFY_PATH)/purify -best-effort -check-debug-timestamps=no -cache-dir=./pure-cache ${CXX} -o xspec_cp xspec.o ${HD_CXXLIBS}
