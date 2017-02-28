PROJECT(pil)
SET(LIBNAME pil)
SET(SOURCES pil.c pil_sys.c pil_error.c)
SET(HEADER_FILES pil.h pil_error.h)

INCLUDE (${CMAKE_SOURCE_DIR}/BUILD_DIR/BuildLib.cmake)

ADD_EXECUTABLE(pget pget.c)
TARGET_LINK_LIBRARIES(pget pil)
ADD_EXECUTABLE(plist plist.c)
TARGET_LINK_LIBRARIES(plist pil)
ADD_EXECUTABLE(pquery2 pquery2.c)
TARGET_LINK_LIBRARIES(pquery2 pil)
ADD_EXECUTABLE(pset pset.c)
TARGET_LINK_LIBRARIES(pset pil)
ADD_EXECUTABLE(punlearn punlearn.c)
TARGET_LINK_LIBRARIES(punlearn pil)

INSTALL_TARGETS(/bin pget plist pquery2 pset punlearn)

