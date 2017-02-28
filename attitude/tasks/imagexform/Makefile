HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	=

HD_CTASK		= imagexform

HD_CTASK_SRC_c		= imagexform.c param.c info.c wcs.c image.c \
                          events_transform.c intensity_transform.c \
                          bbox_transform.c center_transform.c \
                          area_transform.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

LOC_LIBS		= -lcoordfits -lcoord -lparam_wrappers -lephemeris \
                          -lrandom -lrew

HD_CLIBS		= ${HD_LFLAGS} ${LOC_LIBS} ${HD_STD_LIBS} ${SYSLIBS}

HD_TEST_PERL_SCRIPTS	= ut-imagexform

HD_TEST_PFILES          = ut-imagexform.par

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= imagexform.par

HD_INSTALL_HELP		= imagexform.html

include ${HD_STD_MAKEFILE}
