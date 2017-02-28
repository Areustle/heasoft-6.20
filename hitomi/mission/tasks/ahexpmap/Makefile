HD_COMPONENT_NAME       = hitomi

HD_COMPONENT_VERS       =

HD_CXXTASK              = ahexpmap

HD_CXXTASK_SRC_cxx      = ${HD_CXXTASK}.cxx

HD_CXXFLAGS             = ${HD_STD_CXXFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CXXLIBS              = ${HD_LFLAGS} -larfgenlib -limagetranslib -lcoordfits -lcoord -lahmission -lparam_wrappers -lrandom ${HD_STD_CLIBS} ${AH_STD_LIBS}

HD_INSTALL_TASKS        = ${HD_CXXTASK}

HD_INSTALL_PFILES       = ${HD_CXXTASK}.par

HD_INSTALL_HELP         = ${HD_CXXTASK}.html

include ${HD_STD_MAKEFILE}
