HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= XSUtil

HD_LIBRARY_SRC_cxx	= Error/Error.cxx \
			  FunctionUtils/ComponentInfo.cxx \
			  FunctionUtils/FunctionUtility.cxx \
			  FunctionUtils/XSCCall.cxx \
			  FunctionUtils/XSF77.cxx \
			  FunctionUtils/XSModelFunction.cxx \
			  FunctionUtils/xsFortran.cxx \
			  Numerics/AstroFunctions.cxx \
			  Numerics/Beta.cxx \
			  Numerics/BinarySearch.cxx \
			  Numerics/CauchyRand.cxx \
			  Numerics/CosmologyFunction.cxx \
			  Numerics/ExpInt.cxx \
			  Numerics/Gamma.cxx \
			  Numerics/Gauss.cxx \
			  Numerics/IncGamma.cxx \
			  Numerics/Integrate.cxx \
			  Numerics/LinearInterp.cxx \
			  Numerics/LnBang.cxx \
			  Numerics/MathOperator.cxx \
			  Numerics/ModularCounter.cxx \
			  Numerics/Poisson.cxx \
                          Numerics/RandomLux.cxx \
			  Numerics/RandomLuxAdapter.cxx \
			  Numerics/UniformRand.cxx \
			  Parse/AbstractExpression.cxx \
			  Parse/LinkExpression.cxx \
			  Parse/MathExpression.cxx \
			  Parse/ModelExprContexts.cxx \
			  Parse/ModelExpression.cxx \
			  Parse/XSparse.cxx \
			  Parse/XSRegEx.cxx \
			  Signals/EventHandlers.cxx \
			  Signals/SignalHandler.cxx \
                          Utils/IosHolder.cxx \
			  Utils/Observer.cxx \
			  Utils/ProcessManager.cxx \
			  Utils/Reference.cxx \
			  Utils/VerbosityManager.cxx \
			  Utils/XSstream.cxx \
			  Utils/XSutility.cxx

HD_LIBRARY_SRC_f	= 

HD_CXXFLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit \
			  -I${HD_SRC}/src/XSFit/FitMethod \
			  -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSModel/Model \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil \
			  -I${HD_SRC}/src/XSUtil/Numerics \
			  ${HD_STD_CXXFLAGS} -DINXSPEC

HD_FFLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil ${HD_STD_FFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= Error/Error.h \
			  FunctionUtils/ComponentInfo.h \
			  FunctionUtils/FunctionUtility.h \
			  FunctionUtils/funcType.h \
			  FunctionUtils/XSModelFunction.h \
			  FunctionUtils/xsFortran.h \
			  Numerics/AdaptiveIntegrate.h \
			  Numerics/AstroFunctions.h \
			  Numerics/Beta.h \
			  Numerics/BinarySearch.h \
			  Numerics/CosmologyFunction.h \
			  Numerics/ExpInt.h \
			  Numerics/Gamma.h \
			  Numerics/Histogram.h \
			  Numerics/IncGamma.h \
			  Numerics/Integrate.h \
			  Numerics/LinearInterp.h \
			  Numerics/LnBang.h \
			  Numerics/MathOperator.h \
			  Numerics/ModularCounter.h \
			  Numerics/Numerics.h \
                          Numerics/RandomGenerator.h \
                          Numerics/RandomLux.h \
			  Numerics/RandomLuxAdapter.h \
			  Parse/AbstractExpression.h \
			  Parse/LinkExpression.h \
			  Parse/MathExpression.h \
			  Parse/ModelExprContexts.h \
			  Parse/ModelExpression.h \
			  Parse/XSModExpTree.h \
			  Parse/XSparse.h \
			  Parse/XSRegEx.h \
			  Signals/EventHandlers.h \
			  Signals/SignalHandler.h \
                          Utils/IosHolder.h \
			  Utils/Observer.h \
			  Utils/ProcessManager.h \
			  Utils/Reference.h \
			  Utils/VerbosityManager.h \
			  Utils/XSstream.h \
			  Utils/XSutility.h

HD_CLEAN		= ${HD_LIBRARY_SRC_cxx:.cxx=.o} \
			  ${HD_LIBRARY_SRC_f:.f=.o} \
			  FunctionUtils/SunWS_cache \
			  Numerics/SunWS_cache \
			  Parse/SunWS_cache \
			  Signals/SunWS_cache \
			  Utils/SunWS_cache

HD_SHLIB_LIBS		= ${HD_LFLAGS} -lXS ${SYSLIBS} ${F77LIBS4C}

include ${HD_STD_MAKEFILE}
