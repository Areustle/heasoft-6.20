PROJECT(hoops)
SET(LIBNAME hoops)
SET(SOURCES hoops_exception.cxx hoops_group.cxx hoops_limits.cxx hoops_par.cxx hoops_pil.cxx hoops_prim.cxx hoops_prompt_group.cxx)
SET(HEADER_SUB /hoops)
SET(HEADER_FILES  hoops/hoops.h hoops/hoops_exception.h hoops/hoops_group.h hoops/hoops_itor.h hoops/hoops_limits.h hoops/hoops_numeric_limits.h hoops/hoops_par.h hoops/hoops_pil.h hoops/hoops_pil_factory.h hoops/hoops_prim.h hoops/hoops_prompt_group.h)

INCLUDE (${CMAKE_SOURCE_DIR}/BUILD_DIR/BuildLib.cmake)

TARGET_LINK_LIBRARIES(hoops ape)
