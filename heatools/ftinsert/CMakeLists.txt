PROJECT(ftinsert)
ADD_EXECUTABLE(ftinsert ftinsert.c )
INSTALL_FILES(/syspfiles FILES ftinsert.par)
INSTALL_TARGETS(/bin ftinsert)