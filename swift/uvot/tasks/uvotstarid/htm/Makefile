HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	= 

CXX=g++
HD_CXXTASK		= htmintersect

HD_CXXTASK_SRC_cpp	= intersect.cpp cc_aux.cpp \
			  HtmRange.cpp HtmRangeIterator.cpp Htmio.cpp \
			  RangeConvex.cpp SkipList.cpp SkipListElement.cpp \
			  SpatialConstraint.cpp SpatialDomain.cpp \
			  SpatialEdge.cpp SpatialException.cpp \
			  SpatialIndex.cpp SpatialInterface.cpp \
			  SpatialVector.cpp VarStr.cpp

HD_CXXFLAGS		= ${HD_STD_CFLAGS} -Wno-deprecated

HD_CXXLIBS		= ${HD_STD_CLIBS}

HD_INSTALL_TASKS	= ${HD_CXXTASK}

include ${HD_STD_MAKEFILE}
