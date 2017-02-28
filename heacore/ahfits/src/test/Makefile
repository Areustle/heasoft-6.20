HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	= 

HD_TEST_CXXTASK		= testahfits

HD_TEST_CXXTASK_SRC_cxx	= ${HD_TEST_CXXTASK}.cxx ut_ahfits_file.cxx ut_ahfits_file_list.cxx \
			  ut_ahfits_colinfo.cxx ut_ahfits_header.cxx ut_ahfits_row.cxx \
			  ut_ahfits_row_nulls.cxx ut_ahfits_image.cxx ut_ahfits_buffer.cxx

HD_CXXFLAGS		= ${HD_STD_CXXFLAGS}

HD_CXXLIBS		= ${HD_LFLAGS} -l${AHFITS} -l${AHGEN} -l${AHLOG} ${HD_STD_LIBS} ${SYSLIBS}

HD_TEST_TASKS		= ${HD_TEST_CXXTASK}

HD_INSTALL_HELP		= ${HD_TEST_CXXTASK}.html

HD_CLEAN		= ${HD_TEST_CXXTASK}.log \
			  output/*.fits output/*.hk output/*.out

include ${HD_STD_MAKEFILE}

run-test:
	./${HD_TEST_CXXTASK}
