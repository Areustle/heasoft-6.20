PROJECT(ftedit)
ADD_EXECUTABLE(ftedit ftedit.c )
INSTALL_FILES(/syspfiles FILES ftedit.par)
INSTALL_TARGETS(/bin ftedit)