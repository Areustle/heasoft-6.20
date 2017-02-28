PROJECT(ftcopy)
ADD_EXECUTABLE(ftcopy ftcopy.c )
INSTALL_FILES(/syspfiles FILES ftcopy.par)
INSTALL_TARGETS(/bin ftcopy)