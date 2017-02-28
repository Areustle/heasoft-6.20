HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	=

HD_CTASK		= hxdbstjudge

HD_CTASK_SRC_c		= ${HD_CTASK}.c

HD_CFLAGS		= ${HD_STD_CFLAGS} -DTOOLSUB=${HD_CTASK}

HD_CLIBS		= ${HD_LFLAGS} \
-laste_hxd -laste_com -laste_xis -lxrrt-dummy -lastetool -lANL \
-lBNK -lEVS -lCOM -lCLI -lcernlib-dummy -l${ATFUNCTIONS} -lmpfit \
${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ${HD_CTASK}.par

HD_INSTALL_HELP		= ${HD_CTASK}.html

HD_CLEAN		= ${HD_CTASK}.c

include ${HD_STD_MAKEFILE}

.SUFFIXES: .def

.def.c:
	@PATH="${HD_BLD_BIN}:${HD_TOP_BIN}:${HD_BLD_LIB}:${HD_TOP_LIB}:${HC_BLD_EXEC_PFX}/lib:${HC_EXEC_PFX}/lib:${PATH}"; \
	LD_LIBRARY_PATH="${HC_BLD_EXEC_PFX}/lib:${HD_BLD_LIB}:${HC_EXEC_PFX}/lib:${HD_LIB}:${LD_LIBRARY_PATH}"; \
	DYLD_LIBRARY_PATH="${HC_BLD_EXEC_PFX}/lib:${HD_BLD_LIB}:${HC_EXEC_PFX}/lib:${HD_LIB}:${DYLD_LIBRARY_PATH}"; \
	export PATH; \
	export LD_LIBRARY_PATH; \
	export DYLD_LIBRARY_PATH; \
	(cat $<; echo ""; echo $@; echo yes) | mkanlinit${BSUF}
