HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= XSPlot

HD_LIBRARY_SRC_cxx	= Commands/PlotBackground.cxx \
			  Commands/PlotChiSq.cxx \
			  Commands/PlotData.cxx \
			  Commands/PlotDelchi.cxx \
			  Commands/PlotDem.cxx \
			  Commands/PlotEfficiency.cxx \
			  Commands/PlotEqw.cxx \
			  Commands/PlotFoldmodel.cxx \
			  Commands/PlotGoodness.cxx \
			  Commands/PlotIcounts.cxx \
			  Commands/PlotInsensitivity.cxx \
			  Commands/PlotMcmc.cxx \
			  Commands/PlotModel.cxx \
			  Commands/PlotRatio.cxx \
			  Commands/PlotResiduals.cxx \
			  Commands/PlotSensitivity.cxx \
			  Commands/PlotSum.cxx \
			  Commands/PlotUnfolded.cxx \
			  Plot/CreateBinnedPlotGroups.cxx \
			  Plot/CreateContourPlotGroups.cxx \
			  Plot/PlotCommandCreator.cxx \
			  Plot/PlotCommand.cxx \
			  Plot/PlotDirector.cxx \
			  Plot/PlotGroupCreatorClasses.cxx \
			  Plot/PlotGroupCreator.cxx \
			  Plot/PlotGroup.cxx \
			  Plot/PlotPane.cxx \
			  Plot/PlotPkgCreator.cxx \
			  Plot/PlotPkg.cxx \
			  Plot/PlotSettings.cxx \
			  Plot/PlotVector.cxx \
			  Plt/PltPkg.cxx

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

HD_INSTALL_HEADERS	= Commands/ContourPlot.h \
			  Commands/PlotBackground.h \
			  Commands/PlotChiSq.h \
			  Commands/PlotData.h \
			  Commands/PlotDelchi.h \
			  Commands/PlotDem.h \
			  Commands/PlotEfficiency.h \
			  Commands/PlotEqw.h \
			  Commands/PlotFoldmodel.h \
			  Commands/PlotGoodness.h \
			  Commands/PlotIcounts.h \
			  Commands/PlotInsensitivity.h \
			  Commands/PlotMcmc.h \
			  Commands/PlotModel.h \
			  Commands/PlotRatio.h \
			  Commands/PlotResiduals.h \
			  Commands/PlotSensitivity.h \
			  Commands/PlotSum.h \
			  Commands/PlotUnfolded.h \
			  Plot/CreateBinnedPlotGroups.h \
			  Plot/CreateContourPlotGroups.h \
			  Plot/PlotCommandCreator.h \
			  Plot/PlotCommand.h \
			  Plot/PlotDirector.h \
			  Plot/PlotGroupCreatorClasses.h \
			  Plot/PlotGroupCreator.h \
			  Plot/PlotGroup.h \
			  Plot/PlotPane.h \
			  Plot/PlotPkgCreator.h \
			  Plot/PlotPkg.h \
			  Plot/PlotSettings.h \
			  Plot/PlotStyle.h \
			  Plot/PlotTypes.h \
			  Plot/PlotVector.h \
			  Plt/PltCtoF.h \
			  Plt/PltPkg.h

HD_CLEAN		= ${HD_LIBRARY_SRC_cxx:.cxx=.o} \
			  Commands/SunWS_cache \
			  Plot/SunWS_cache \
			  Plt/SunWS_cache

HD_SHLIB_LIBS		= ${HD_LFLAGS} -lplt ${LINK_PGPLOT} \
			  -lXSFit -lXSModel -lXSUtil -lXS \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} \
			  ${F77LIBS4C}

include ${HD_STD_MAKEFILE}
