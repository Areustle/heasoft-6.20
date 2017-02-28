PROJECT(ftappend)
ADD_EXECUTABLE(ftappend ftappend.c )
INSTALL_FILES(/syspfiles FILES ftappend.par)
INSTALL_TARGETS(/bin ftappend)