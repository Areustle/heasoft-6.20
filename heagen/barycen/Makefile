HD_COMPONENT_NAME       = heagen

HD_COMPONENT_VERS       =

HD_CTASK              = barycen

HD_CTASK_SRC_c        = barycen.c from_barycorr.c

#HD_CFLAGS             = ${HD_STD_CFLAGS} -g -std=c99 -D_BSD_SOURCE -Wall -Wextra -I${HEADAS}/include
HD_CFLAGS             = ${HD_STD_CFLAGS} -g -std=c99 -D_BSD_SOURCE -Wall -Wextra

#HD_CLIBS              = ${HD_LFLAGS} -L${HEADAS}/lib -lcoordfits -lcoord ${HD_STD_CLIBS}
HD_CLIBS              = ${HD_LFLAGS} -lcoordfits -lcoord ${HD_STD_CLIBS}

HD_INSTALL_TASKS        = ${HD_CTASK}

HD_INSTALL_PFILES	= ${HD_CTASK}.par

HD_INSTALL_HEADERS      = 

HD_INSTALL_HELP         = barycen.html

include ${HD_STD_MAKEFILE}
