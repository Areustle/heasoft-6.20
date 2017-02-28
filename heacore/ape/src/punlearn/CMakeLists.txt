PROJECT(punlearn)

ADD_EXECUTABLE(punlearn punlearn.c)
TARGET_LINK_LIBRARIES(punlearn ape)

INSTALL_TARGETS(/bin punlearn)
