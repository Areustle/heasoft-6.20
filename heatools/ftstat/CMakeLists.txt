PROJECT(ftstat)
ADD_EXECUTABLE(ftstat ftstat.c )
INSTALL_FILES(/syspfiles FILES ftstat.par)
INSTALL_TARGETS(/bin ftstat)