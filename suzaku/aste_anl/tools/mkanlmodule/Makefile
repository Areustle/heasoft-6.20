HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	=

HD_CTASK		= mkanlmodule

HD_CTASK_SRC_c		= mkanlmodule.c write_anlmodule.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
-lBNK -lEVS -lANL -lCOM -lCLI -lcernlib-dummy \
${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

include ${HD_STD_MAKEFILE}
