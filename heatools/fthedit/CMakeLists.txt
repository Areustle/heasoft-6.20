PROJECT(fthedit)
ADD_EXECUTABLE(fthedit fthedit.c )
INSTALL_FILES(/syspfiles FILES fthedit.par)
INSTALL_TARGETS(/bin fthedit)