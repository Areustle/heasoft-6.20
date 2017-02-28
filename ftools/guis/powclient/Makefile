HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= powclient

HD_LIBRARY_SRC_c	= PowClient.c PowCreateData.c PowDataCommands.c \
			  PowDataUtils.c

HD_CFLAGS		= ${HD_STD_CFLAGS} ${XINCLUDES}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${TCL}

HD_INSTALL_HEADERS	= powdata.h

HD_INSTALL_EXTRA	= install-pow-extras

include ${HD_STD_MAKEFILE}

POWCLIENT_EXTRAS	= POWplotClient.tcl

install-pow-extras:
	@for extra in ${POWCLIENT_EXTRAS}; do \
		${HD_INSTALL} $$extra ${HD_LIB}/pow/$$extra ${HD_CP_P}; \
	done
