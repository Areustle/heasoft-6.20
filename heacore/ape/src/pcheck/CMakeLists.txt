PROJECT(pcheck)

ADD_EXECUTABLE(pcheck pcheck.c)
TARGET_LINK_LIBRARIES(pcheck ape)

INSTALL_TARGETS(/bin pcheck)
