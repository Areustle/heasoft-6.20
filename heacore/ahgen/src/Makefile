HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= ${AHGEN}

HD_LIBRARY_SRC_cxx	= ahgen.cxx ahfile.cxx ahrandom.cxx ahtest.cxx BitBuf.cxx

HD_CXXFLAGS		= ${HD_STD_CXXFLAGS}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${AHLOG} -l${HEAUTILS} -l${PIL} \
			  -l${CFITSIO} -l${READLINE} -l${HEAIO}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

include ${HD_STD_MAKEFILE}
