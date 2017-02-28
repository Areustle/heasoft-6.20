PROJECT(ftcreate)
ADD_EXECUTABLE(ftcreate ftcreate.c gtoken.c )
INSTALL_FILES(/syspfiles FILES ftcreate.par)
INSTALL_TARGETS(/bin ftcreate)