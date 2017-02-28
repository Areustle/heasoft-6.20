PROJECT(ftimgcreate)
ADD_EXECUTABLE(ftimgcreate ftimgcreate.c parseDouble.c parseLong.c )
INSTALL_FILES(/syspfiles FILES ftimgcreate.par)
INSTALL_TARGETS(/bin ftimgcreate)