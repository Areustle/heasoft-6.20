HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= XSFit

HD_LIBRARY_SRC_cxx	= Fit/Bayes.cxx \
			  Fit/Fit.cxx \
			  Fit/FitErrorCalc.cxx \
                          Fit/FitErrorOutput.cxx \
			  Fit/FitMethod.cxx \
			  Fit/FitStrategy.cxx \
			  Fit/Grid.cxx \
                          Fit/ParallelStats.cxx \
			  Fit/StatManager.cxx \
			  Fit/StatMethod.cxx \
			  Fit/Step.cxx \
			  FitMethod/LevMarq/InvertSVD.cxx \
			  FitMethod/LevMarq/LevMarq.cxx \
			  FitMethod/LevMarq/svdcmp.cxx \
			  MCMC/AsciiChain.cxx \
			  MCMC/Chain.cxx \
			  MCMC/ChainIO.cxx \
			  MCMC/ChainManager.cxx \
			  MCMC/IntegProbGrid.cxx \
			  MCMC/FITSChain.cxx \
			  MCMC/MarginGrid.cxx \
			  Randomizer/RandomizerBase.cxx \
			  Randomizer/RandomizerPolicies.cxx \
			  StatMethod/ChiSquare/ChiSquare.cxx \
			  StatMethod/ChiSquare/PearsonChiSquare.cxx \
			  StatMethod/EDF/EDFVariants.cxx \
			  StatMethod/Cstat/CstatVariants.cxx \
			  StatMethod/Runs/Runs.cxx


#			  FitMethod/Unimplemented/Anneal/Anneal.cxx \
#			  FitMethod/Unimplemented/Anneal/SACalcStat.cxx \
#			  FitMethod/Unimplemented/Genetic/GNCalcStat.cxx \
#			  FitMethod/Unimplemented/Genetic/Genetic.cxx \
#			  StatMethod/Unimplemented/Lstat/Lstat.cxx


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

HD_FFLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil ${HD_STD_FFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= Fit/Bayes.h \
			  Fit/Fit.h \
			  Fit/FitErrorCalc.h \
                          Fit/FitErrorOutput.h \
			  Fit/FitMethod.h \
			  Fit/FitStrategy.h \
			  Fit/Grid.h \
                          Fit/ParallelStats.h \
			  Fit/StatManager.h \
			  Fit/StatMethod.h \
			  Fit/Step.h \
			  FitMethod/LevMarq/LevMarq.h \
			  MCMC/AsciiChain.h \
			  MCMC/Chain.h \
			  MCMC/ChainIO.h \
			  MCMC/ChainManager.h \
			  MCMC/IntegProbGrid.h \
			  MCMC/FITSChain.h \
			  MCMC/MarginGrid.h \
			  Randomizer/Randomizer.h \
			  Randomizer/RandomizerBase.h \
			  Randomizer/RandomizerPolicies.h \
			  StatMethod/ChiSquare/ChiSquare.h \
			  StatMethod/ChiSquare/PearsonChiSquare.h \
			  StatMethod/Cstat/Cstat.h \
			  StatMethod/Cstat/CstatVariants.h \
			  StatMethod/Cstat/LorStatT.h \
			  StatMethod/EDF/EDF.h \
			  StatMethod/EDF/EDFVariants.h \
			  StatMethod/Runs/Runs.h

#			  FitMethod/Unimplemented/Anneal/Anneal.h \
#			  FitMethod/Unimplemented/Genetic/Genetic.h

HD_CLEAN		= ${HD_LIBRARY_SRC_cxx:.cxx=.o} \
			  ${HD_LIBRARY_SRC_f:.f=.o} \
			  Fit/SunWS_cache \
			  FitMethod/LevMarq/SunWS_cache \
			  MCMC/SunWS_cache \
			  Randomizer/SunWS_cache \
			  StatMethod/ChiSquare/SunWS_cache \
			  StatMethod/Cstat/SunWS_cache \
			  StatMethod/EDF/SunWS_cache

HD_SHLIB_LIBS		= ${HD_LFLAGS} -lXSModel -lXSUtil -lXS \
			  -l${CCFITS} -l${CFITSIO} ${SYSLIBS} ${F77LIBS4C}

include ${HD_STD_MAKEFILE}


