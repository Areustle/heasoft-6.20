HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= ${AHFITS}

HD_LIBRARY_SRC_cxx	= ahfits_base.cxx ahfits_header.cxx ahfits_buffer.cxx ahfits_connect.cxx ahfits_router.cxx \
			  ahfits_colinfo.cxx ahfits_file.cxx ahfits_file_list.cxx ahfits_image.cxx ahfits_row.cxx

HD_CXXFLAGS		= ${HD_STD_CXXFLAGS}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${AHGEN} -l${AHLOG} -l${HEAUTILS} \
			  -l${PIL} -l${CFITSIO} -l${READLINE} -l${HEAIO}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

include ${HD_STD_MAKEFILE}
