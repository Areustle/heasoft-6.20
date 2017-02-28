PROJECT(pquery2)

ADD_EXECUTABLE(pquery2 pquery2.c)
TARGET_LINK_LIBRARIES(pquery2 ape)

INSTALL_TARGETS(/bin pquery2)
