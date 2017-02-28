HD_COMPONENT_NAME	= hitomi

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= ahmission

HD_LIBRARY_SRC_cxx	= ahmission.cxx caldb.cxx camstoffset.cxx delay.cxx hxiteldef.cxx keyword.cxx camsteldef.cxx camsio.cxx timfile.cxx spline.cxx

HD_CXXFLAGS		= ${HD_STD_CXXFLAGS}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${AHFITS} -l${AHGEN} -l${AHLOG} \
			  -l${HEAUTILS} -l${PIL} -l${CFITSIO} -l${READLINE} -l${HEAIO}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

include ${HD_STD_MAKEFILE}
