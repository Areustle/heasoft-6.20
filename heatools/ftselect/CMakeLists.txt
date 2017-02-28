PROJECT(ftselect)
ADD_EXECUTABLE(ftselect ftselect.c )
INSTALL_FILES(/syspfiles FILES ftselect.par)
INSTALL_TARGETS(/bin ftselect)