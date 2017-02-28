HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	=

HD_CXXTASK		= makefilter

HD_TASK_ALIAS		= newmakefilter

HD_CXXTASK_SRC_cc	= makefilter.cc config.cc hkcolumn.cc \
			  hkfile.cc \
                          ASCIIConfig.cc FITSConfig.cc FITSFileBroker.cc \
                          Value.cc Data.cc StringData.cc BitData.cc \
                          BooleanData.cc \
                          Exception.cc FITSException.cc PILException.cc \
                          BadOpException.cc HDException.cc \
                          Point.cc Interpolation.cc TwoPointInterp.cc \
                          StepInterpolation.cc LinearInterp.cc \
                          FilterFile.cc Param.cc

HD_CXXFLAGS		= ${HD_STD_CXXFLAGS} 

HD_CXXLIBS		= ${HD_STD_CXXLIBS}

HD_INSTALL_TASKS	= ${HD_CXXTASK}

HD_INSTALL_PFILES	= ${HD_CXXTASK}.par ${HD_TASK_ALIAS}.par

HD_INSTALL_HELP		= ${HD_CXXTASK}.html ${HD_TASK_ALIAS}.html

HD_INSTALL_EXTRA	= install-alias

include ${HD_STD_MAKEFILE}

install-alias:
	@${HD_INSTALL} ${HD_BIN}/${HD_CXXTASK}${BSUF} \
		${HD_BIN}/${HD_TASK_ALIAS}${BSUF} ${HD_LN_S}
