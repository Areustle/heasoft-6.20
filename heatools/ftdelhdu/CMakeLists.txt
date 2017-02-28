PROJECT(ftdelhdu)
ADD_EXECUTABLE(ftdelhdu ftdelhdu.c )
INSTALL_FILES(/syspfiles FILES ftdelhdu.par)
INSTALL_TARGETS(/bin ftdelhdu)