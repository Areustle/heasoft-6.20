PROJECT(ftpaste)
ADD_EXECUTABLE(ftpaste ftpaste.c )
INSTALL_FILES(/syspfiles FILES ftpaste.par)
INSTALL_TARGETS(/bin ftpaste)