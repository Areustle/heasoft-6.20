PROJECT(ftmerge)
ADD_EXECUTABLE(ftmerge ftmerge.c )
INSTALL_FILES(/syspfiles FILES ftmerge.par)
INSTALL_TARGETS(/bin ftmerge)