HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	=

HD_CTASK		= xisucode

HD_CTASK_SRC_c		= xisucode.c

HD_CFLAGS		= ${HD_STD_CFLAGS} -DTOOLSUB=${HD_CTASK}

HD_CLIBS		= ${HD_LFLAGS} \
-laste_xis -laste_com -lxrrt-dummy -lastetool -lANL -lBNK \
-lEVS -lCOM -lCLI -lcernlib-dummy -l${ATFUNCTIONS} \
${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

#HD_INSTALL_PFILES	= xisucode.par

HD_INSTALL_HELP		= xisucode.txt

include ${HD_STD_MAKEFILE}
