PROJECT(heaio)
SET(LIBNAME hdio)
SET(SOURCES headas_stdio.c)
SET(HEADER_FILES headas_stdio.h)

INCLUDE (${CMAKE_SOURCE_DIR}/BUILD_DIR/BuildLib.cmake)

TARGET_LINK_LIBRARIES(hdio cfitsio)
