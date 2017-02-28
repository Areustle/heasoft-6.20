PROJECT(ftchecksum)
ADD_EXECUTABLE(ftchecksum ftchecksum.c )
INSTALL_FILES(/syspfiles FILES ftchecksum.par)
INSTALL_TARGETS(/bin ftchecksum)