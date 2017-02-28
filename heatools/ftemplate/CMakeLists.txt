PROJECT(ftemplate)
ADD_EXECUTABLE(ftemplate ftemplate.c )
INSTALL_FILES(/syspfiles FILES ftemplate.par)
INSTALL_TARGETS(/bin ftemplate)