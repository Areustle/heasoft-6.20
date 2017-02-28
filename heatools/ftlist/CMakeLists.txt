PROJECT(ftlist)
ADD_EXECUTABLE(ftlist ftlist.c )
INSTALL_FILES(/syspfiles FILES ftlist.par)
INSTALL_TARGETS(/bin ftlist)