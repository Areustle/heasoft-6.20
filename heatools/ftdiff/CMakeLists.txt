PROJECT(ftdiff)
ADD_EXECUTABLE(ftdiff ftdiff.c )
INSTALL_FILES(/syspfiles FILES ftdiff.par)
INSTALL_TARGETS(/bin ftdiff)