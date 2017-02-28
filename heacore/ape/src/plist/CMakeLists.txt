PROJECT(plist)

ADD_EXECUTABLE(plist plist.c)
TARGET_LINK_LIBRARIES(plist ape)

INSTALL_TARGETS(/bin plist)
