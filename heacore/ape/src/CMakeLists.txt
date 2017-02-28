PROJECT(ape)
SET(LIBNAME ape)
SET(SOURCES ape_binary.c ape_error.c ape_io.c ape_list.c ape_msg.c ape_par.c ape_par_group.c ape_test.c ape_trad.c ape_util.c pil.c pil_error.c)
SET(HEADER_FILES ape/ape_binary.h ape/ape_error.h ape/ape_io.h ape/ape_list.h ape/ape_msg.h ape/ape_par.h ape/ape_par_group.h ape/ape_test.h ape/ape_trad.h ape/ape_util.h cfortran.h pil.h pil_error.h)

INCLUDE (${CMAKE_SOURCE_DIR}/BUILD_DIR/BuildLib.cmake)

