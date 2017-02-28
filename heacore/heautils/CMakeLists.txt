PROJECT(heautils)
include_directories(${ape_SOURCE_DIR}/include)
SET(LIBNAME hdutils)
SET(SOURCES headas_error.c headas_error_init.c headas_error_manager.c headas_error_map.c headas_history.c headas_toolname.c headas_utils.c headas_parstamp.c headas_gti.c HDgtcalf.c HDgtcalf_internal.c HDgtcalf_f77_wrap.c headas_file_check.c)
SET(HEADER_FILES headas_error.h headas_utils.h headas_gti.h hdcal.h)

INCLUDE (${CMAKE_SOURCE_DIR}/BUILD_DIR/BuildLib.cmake)

TARGET_LINK_LIBRARIES(hdutils cfitsio ape)
