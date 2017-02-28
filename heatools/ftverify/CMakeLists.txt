PROJECT(ftverify)
ADD_EXECUTABLE(ftverify ftverify.c fvrf_data.c fvrf_file.c fvrf_head.c fvrf_key.c fvrf_misc.c )
INSTALL_FILES(/syspfiles FILES ftverify.par)
INSTALL_TARGETS(/bin ftverify)
