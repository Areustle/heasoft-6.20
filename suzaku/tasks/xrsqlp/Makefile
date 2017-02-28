HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	= 

HD_CTASK		= xrsqlp

HD_CTASK_SRC_c		= xrsqlp_main.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
-laste_xrs -laste_com -laste_xis -lxrrt-dummy -lastetool -lANL -lBNK \
-lEVS -lCOM -lCLI -lcernlib-dummy -l${ATFUNCTIONS} \
${LINK_PGPLOT} ${XLIBS} -lparam_wrappers \
${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= xrsqlp.par

HD_INSTALL_HELP		= xrsqlp.txt

include ${HD_STD_MAKEFILE}
