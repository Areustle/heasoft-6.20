HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	=

HD_CXXTASK		= xissimarfgen

HD_CXXTASK_SRC_c	= ${HD_CXXTASK}.c

HD_CFLAGS		= ${HD_STD_CFLAGS} -DTOOLSUB=${HD_CXXTASK}

HD_CXXFLAGS		= ${HD_STD_CXXFLAGS} -DTOOLSUB=${HD_CXXTASK}

HD_CXXLIBS		= ${HD_LFLAGS} \
-laste_com -laste_xis -lxrrt -laste_com -lastetool -lANL -lBNK \
-lEVS -lCOM -lCLI -lcernlib-dummy -l${ATFUNCTIONS} \
${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CXXTASK}

HD_INSTALL_PFILES	= ${HD_CXXTASK}.par

HD_INSTALL_HELP		= ${HD_CXXTASK}.txt

HD_CLEAN		= ${HD_CXXTASK}.c

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
