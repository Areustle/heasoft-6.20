PROJECT(ftkeypar)
ADD_EXECUTABLE(ftkeypar ftkeypar.c )
INSTALL_FILES(/syspfiles FILES ftkeypar.par)
INSTALL_TARGETS(/bin ftkeypar)