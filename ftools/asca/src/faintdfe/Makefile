HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= faintdfe

HD_CTASK_SRC_c		= hfaintdfe.c faintdfe.c function.c grade.c info.c \
			  param.c specdump.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxpi_wrappers${FT_VERS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= faintdfe.parin

HD_INSTALL_HELP		= faintdfe.hlp faintdfe.txt

include ${HD_STD_MAKEFILE}
