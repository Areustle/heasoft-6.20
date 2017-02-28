HD_COMPONENT_NAME	= heasim

HD_COMPONENT_VERS	=

HD_CTASK		= skyback

HD_CTASK_SRC_c         = skyback.c initialize.c finalize.c utils.c doWork.c

HD_CTASK_SRC_f		=

HD_CFLAGS               = ${HD_STD_CFLAGS} -g -Wno-unused-function -Wno-unknown-pragmas -std=c99 -D_BSD_SOURCE
# -g:                    Enable gdb debugging
# -Wno-unused-function:  Hide compiler warnings from unused functions
# -Wno-unknown-pragmas:  Hide compiler warnings from pragma marks
# -D_BSD_SOURCE:         Contains standard definition of realpath() function

HD_FFLAGS		= ${HD_STD_FFLAGS}


HD_CLIBS                = ${HD_LFLAGS}\
                          -lXSFunctions -lXSModel -lXSUtil -lXS -l${WCSLIB} -l$(CCFITS) ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ${HD_CTASK}.par

HD_INSTALL_REFDATA      = rac.fits  raij.fits  ram.fits  ranh.fits  torus1006.fits

HD_INSTALL_HELP		= skyback.html

include ${HD_STD_MAKEFILE}



