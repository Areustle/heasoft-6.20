HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= xsmix

HD_LIBRARY_SRC_cxx	= Cluster.cxx \
			  ClusterRegion.cxx \
			  Projection.cxx \
			  PsfImage.cxx \
			  WmapRegion.cxx \
			  PsfTraits.cxx \
			  XRTResponse.cxx \
			  ascaCluster.cxx \
			  gisres.cxx \
			  gisresf.cxx \
			  project.cxx \
			  recorn.cxx \
			  suzaku.cxx \
			  xmmc.cxx \
			  xsmixFunctionMap.cxx

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

XS_SOURCE_FLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main \
			  -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit \
			  -I${HD_SRC}/src/XSFit/FitMethod \
			  -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib\
			  -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSModel/Model \
			  -I${HD_SRC}/src/XSPlot \
			  -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil

HD_CXXFLAGS		= ${HD_STD_CXXFLAGS} ${XS_SOURCE_FLAGS} -DINITPACKAGE

HD_CFLAGS		= ${HD_STD_CFLAGS} ${XS_SOURCE_FLAGS} -DINITPACKAGE

HD_FFLAGS		= ${HD_STD_FFLAGS} ${XS_SOURCE_FLAGS} -DINITPACKAGE

HD_SHLIB_LIBS		= ${HD_LFLAGS}  \
			  -lXSFunctions -lXSModel -lXSUtil -lXS \
			  ${GUI_LIBS} -l${TCLREADLINE} -l${TCL} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

include ${HD_STD_MAKEFILE}
