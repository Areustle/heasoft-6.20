HD_COMPONENT_NAME	= heasim

HD_COMPONENT_VERS	=

HD_CTASK		= heasim

HD_CTASK_SRC_c          = heasim.c initialize.c doWork.c finalize.c utils.c legacy_heasp.c

HD_CTASK_SRC_f		=

HD_CFLAGS               = ${HD_STD_CFLAGS} -g -Wno-unused-function -Wno-unknown-pragmas -std=c99 -D_BSD_SOURCE
# -g:                    Enable gdb debugging
# -Wno-unused-function:  Hide compiler warnings from unused functions
# -Wno-unknown-pragmas:  Hide compiler warnings from pragma marks
# -D_BSD_SOURCE:         Contains standard definition of realpath() function

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS}\
			  -lXSFunctions -lXSModel -lXSUtil -lXS -l${WCSLIB} -l$(CCFITS) -l${HEASP} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C} -lhdutils_2.7

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ${HD_CTASK}.par

HD_INSTALL_REFDATA      = heasim.mdb heasim_source_sample.txt

HD_INSTALL_HELP         = heasim.html

include ${HD_STD_MAKEFILE}


