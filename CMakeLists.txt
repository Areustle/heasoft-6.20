PROJECT(headas)
SUBDIRS(heacore heatools demo)
INCLUDE_DIRECTORIES(${cfitsio_SOURCE_DIR} ${ape_SOURCE_DIR} ${heaio_SOURCE_DIR} ${heautils_SOURCE_DIR} ${heainit_SOURCE_DIR})


