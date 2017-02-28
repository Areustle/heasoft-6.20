HD_COMPONENT_NAME	= heasim

HD_COMPONENT_VERS	=

HD_CTASK		= psf_ascii2fits

HD_CTASK_SRC_c         = psftool.c

HD_CTASK_SRC_f		=

HD_CFLAGS               = ${HD_STD_CFLAGS} -g -Wno-unused-function -Wno-unknown-pragmas
# -g:                    Enable gdb debugging
# -Wno-unused-function:  Hide compiler warnings from unused functions
# -Wno-unknown-pragmas:  Hide compiler warnings from pragma marks

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -l$(CCFITS) ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= 

include ${HD_STD_MAKEFILE}



