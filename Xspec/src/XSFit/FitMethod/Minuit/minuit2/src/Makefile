HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= Minuit2

HD_LIBRARY_SRC_cxx	= \
                          AnalyticalGradientCalculator.cxx \
                          BasicMinimumError.cxx \
                          CombinedMinimumBuilder.cxx \
                          DavidonErrorUpdator.cxx \
                          FumiliBuilder.cxx \
                          FumiliErrorUpdator.cxx \
                          FumiliGradientCalculator.cxx \
                          FumiliMinimizer.cxx \
                          FumiliStandardChi2FCN.cxx \
                          FumiliStandardMaximumLikelihoodFCN.cxx \
                          GenAlgoOptions.cxx \
                          HessianGradientCalculator.cxx \
                          InitialGradientCalculator.cxx \
                          LaEigenValues.cxx \
                          LaInnerProduct.cxx \
                          LaInverse.cxx \
                          LaOuterProduct.cxx \
                          LaSumOfElements.cxx \
                          LaVtMVSimilarity.cxx \
                          MPIProcess.cxx \
                          MinimizerOptions.cxx \
                          Minuit2Minimizer.cxx \
                          MnApplication.cxx \
                          MnContours.cxx \
                          MnCovarianceSqueeze.cxx \
                          MnEigen.cxx \
                          MnFcn.cxx \
                          MnFumiliMinimize.cxx \
                          MnFunctionCross.cxx \
                          MnGlobalCorrelationCoeff.cxx \
                          MnHesse.cxx \
                          MnLineSearch.cxx \
                          MnMachinePrecision.cxx \
                          MnMinos.cxx \
                          MnParabolaFactory.cxx \
                          MnParameterScan.cxx \
                          MnPlot.cxx \
                          MnPosDef.cxx \
                          MnPrint.cxx \
                          MnScan.cxx \
                          MnSeedGenerator.cxx \
                          MnStrategy.cxx \
                          MnTiny.cxx \
                          MnUserFcn.cxx \
                          MnUserParameterState.cxx \
                          MnUserParameters.cxx \
                          MnUserTransformation.cxx \
                          ModularFunctionMinimizer.cxx \
                          NegativeG2LineSearch.cxx \
                          Numerical2PGradientCalculator.cxx \
                          ParametricFunction.cxx \
                          ScanBuilder.cxx \
                          SimplexBuilder.cxx \
                          SimplexParameters.cxx \
                          SimplexSeedGenerator.cxx \
                          SinParameterTransformation.cxx \
                          SqrtLowParameterTransformation.cxx \
                          SqrtUpParameterTransformation.cxx \
                          VariableMetricBuilder.cxx \
                          VariableMetricEDMEstimator.cxx \
                          mnbins.cxx \
                          mndasum.cxx \
                          mndaxpy.cxx \
                          mnddot.cxx \
                          mndscal.cxx \
                          mndspmv.cxx \
                          mndspr.cxx \
                          mnlsame.cxx \
                          mnteigen.cxx \
                          mntplot.cxx \
                          mnvert.cxx \
                          mnxerbla.cxx

HD_CXXFLAGS		= -I../inc/ -DMATH_NO_PLUGIN_MANAGER=1 ${HD_STD_CXXFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HELP		= 

HD_INSTALL_HEADERS	=

HD_CLEAN                = ${HD_LIBRARY_SRC_cxx:.cxx=.o} \
			  ${HD_LIBRARY_SRC_f:.f=.o}

HD_SHLIB_LIBS		= ${HD_LFLAGS} ${HD_STD_LIBS} ${SYSLIBS}

include ${HD_STD_MAKEFILE}
