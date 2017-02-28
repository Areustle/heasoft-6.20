HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	=

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_LIBRARY_ROOT         = COM

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	=

HD_LIBRARY_SRC_c	= com_cwrapper.c

HD_LIBRARY_SRC_f	= chval.f comhelp.f comutl.f crange.f inquire.f \
			  modbits.f modval.f modval_sub.f screen.f showit.f \
			  switch.f telbit.f tellit.f

HD_SHLIB_LIBS		= ${HD_LFLAGS} -lCLI ${F77LIBS4C}

include ${HD_STD_MAKEFILE}
