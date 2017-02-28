PROJECT(ftcalc)
ADD_EXECUTABLE(ftcalc ftcalc.c )
INSTALL_FILES(/syspfiles FILES ftcalc.par)
INSTALL_TARGETS(/bin ftcalc)