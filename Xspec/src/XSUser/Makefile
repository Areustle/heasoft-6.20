HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= XSUser

HD_LIBRARY_SRC_cxx	= Global/Global.cxx \
			  Global/XSGlobal.cxx \
			  Handler/XSinterface.cxx \
			  Handler/xsAbund.cxx \
			  Handler/xsAddcomp.cxx \
			  Handler/xsArf.cxx \
			  Handler/xsAutosave.cxx \
			  Handler/xsBackgrnd.cxx \
			  Handler/xsBayes.cxx \
                          Handler/xsChain.cxx \
			  Handler/xsChatter.cxx \
			  Handler/xsComsum.cxx \
			  Handler/xsCorfile.cxx \
			  Handler/xsCornorm.cxx \
			  Handler/xsCosmo.cxx \
			  Handler/xsCpd.cxx \
			  Handler/xsData.cxx \
			  Handler/xsDelcomp.cxx \
			  Handler/xsDiagrsp.cxx \
			  Handler/xsDummyrsp.cxx \
			  Handler/xsEditmod.cxx \
			  Handler/xsEnergies.cxx \
			  Handler/xsEqwidth.cxx \
			  Handler/xsError.cxx \
			  Handler/xsExit.cxx \
			  Handler/xsFakeit.cxx \
			  Handler/xsFit.cxx \
			  Handler/xsFlux.cxx \
			  Handler/xsFreeze.cxx \
			  Handler/xsFtest.cxx \
                          Handler/xsGain.cxx \
			  Handler/xsGoodness.cxx \
			  Handler/xsHelp.cxx \
			  Handler/xsIdentify.cxx \
			  Handler/xsIgnore.cxx \
			  Handler/xsImprove.cxx \
			  Handler/xsInitpackage.cxx \
			  Handler/xsIplot.cxx \
			  Handler/xsLmod.cxx \
			  Handler/xsLog.cxx \
			  Handler/xsMargin.cxx \
			  Handler/xsMdefine.cxx \
			  Handler/xsMethod.cxx \
			  Handler/xsModel.cxx \
			  Handler/xsNewpar.cxx \
			  Handler/xsNot.cxx \
			  Handler/xsNotice.cxx \
			  Handler/xsParallel.cxx \
			  Handler/xsPlot.cxx \
			  Handler/xsQuery.cxx \
			  Handler/xsRenorm.cxx \
			  Handler/xsResponse.cxx \
			  Handler/xsRmodel.cxx \
			  Handler/xsSave.cxx \
			  Handler/xsScript.cxx \
			  Handler/xsSetplot.cxx \
			  Handler/xsShow.cxx \
			  Handler/xsStatistic.cxx \
			  Handler/xsSteppar.cxx \
			  Handler/xsSystematic.cxx \
			  Handler/xsTclout.cxx \
			  Handler/xsThaw.cxx \
			  Handler/xsTime.cxx \
			  Handler/xsUndo.cxx \
			  Handler/xsUntie.cxx \
			  Handler/xsVersion.cxx \
			  Handler/xsWeight.cxx \
			  Handler/xsXsect.cxx \
			  Handler/xsXset.cxx \
                          Handler/HandlerUtils.cxx \
			  Help/Help.cxx \
			  Help/HelpComposite.cxx \
                          UserInterface/TclRedAlert.cxx \
			  UserInterface/TclSigInt.cxx \
			  UserInterface/TclStream.cxx \
			  UserInterface/TkGui.cxx \
			  UserInterface/echcom.cxx \
			  UserInterface/execscript.cxx \
			  UserInterface/unknow.cxx \
			  UserInterface/TclRegEx.cxx

HD_CXXFLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit \
			  -I${HD_SRC}/src/XSFit/FitMethod \
			  -I${HD_SRC}/src/XSFit/FitMethod/Minuit/minuit2/inc \
			  -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib \
			  -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSModel/Model \
			  -I${HD_SRC}/src/XSPlot \
			  -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUser/Global \
			  -I${HD_SRC}/src/XSUser/Handler \
			  -I${HD_SRC}/src/XSUser/Help \
			  -I${HD_SRC}/src/XSUser/UserInterface \
			  -I${HD_SRC}/src/XSUtil \
			  ${HD_STD_CXXFLAGS} -DINXSPEC

HD_FFLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil ${HD_STD_FFLAGS} \
			  -IGlobal -IHelp -IUserInterface

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= Global/Global.h \
			  Global/XSGlobal.h \
                          Handler/HandlerUtils.h \
			  Handler/XSinterface.h \
			  Help/Help.h \
			  Help/HelpComposite.h \
                          UserInterface/TclRedAlert.h \
			  UserInterface/TclSigInt.h \
			  UserInterface/TclStream.h \
			  UserInterface/TkGui.h \
			  UserInterface/xstcl.h \
			  UserInterface/TclRegEx.h

HD_CLEAN		= ${HD_LIBRARY_SRC_cxx:.cxx=.o} \
			  ${HD_LIBRARY_SRC_f:.f=.o} \
			  Global/SunWS_cache \
			  Handler/SunWS_cache \
			  Help/SunWS_cache \
			  UserInterface/SunWS_cache

HD_SHLIB_LIBS		= ${HD_LFLAGS} -lXSPlot -lplt -l${PGPLOT} \
			  -lXSMinuit -lXSFunctions -lXSFit -lXSModel -lXSUtil -lXS \
			  ${GUI_LIBS} -l${TCLREADLINE} -l${TCL} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

include ${HD_STD_MAKEFILE}
