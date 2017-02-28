PROJECT(ftdelrow)
ADD_EXECUTABLE(ftdelrow ftdelrow.c )
INSTALL_FILES(/syspfiles FILES ftdelrow.par)
INSTALL_TARGETS(/bin ftdelrow)