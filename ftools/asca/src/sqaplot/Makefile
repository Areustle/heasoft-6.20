HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= sqaplot

HD_CTASK_SRC_c		= hsqaplot.c extras.c grading.c picture.c science.c \
			  secular2.c sqaplot.c

HD_CTASK_SRC_f		= gradev50.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} ${LINK_PGPLOT} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= sqaplot.parin

HD_INSTALL_HELP		= sqaplot.hlp sqaplot.txt

include ${HD_STD_MAKEFILE}
