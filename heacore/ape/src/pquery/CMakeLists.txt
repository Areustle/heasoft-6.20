PROJECT(pquery)

ADD_EXECUTABLE(pquery pquery.c)
TARGET_LINK_LIBRARIES(pquery ape)

INSTALL_TARGETS(/bin pquery)
