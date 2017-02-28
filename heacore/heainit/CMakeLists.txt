PROJECT(heainit)
SET(LIBNAME hdinit)
SET(SOURCES headas_init.c)
SET(HEADER_FILES headas.h headas_main.c)

INCLUDE (${CMAKE_SOURCE_DIR}/BUILD_DIR/BuildLib.cmake)

TARGET_LINK_LIBRARIES(hdinit hdutils hdio cfitsio ape)

INSTALL_TARGETS(/lib RUNTIME_DIRECTORY /bin ${LIBNAME})
