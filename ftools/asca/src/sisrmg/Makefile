HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= sisrmg

HD_CTASK_SRC_c		= hsisrmg.c caltls.c points.c rmebxt.c rmloop.c \
			  secular.c sisrmg.c sp_0001.c sp_0031.c sp_0041.c
# le_0002.c included by points.c
# rddsub.c included by secular.c

HD_CTASK_SRC_f		= getpha.f

# Turn off optimization for this task on Linux (using COPT_SYS instead of COPT):
HD_CFLAGS		= ${COPT_SYS} ${CFLAGS} ${HD_IFLAGS} ${DEFS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PERL_SCRIPTS	= sisgbr

HD_INSTALL_PFILES	= sisrmg.parin

HD_INSTALL_HELP		= sisrmg.hlp sisgbr.txt sisrmg.txt

include ${HD_STD_MAKEFILE}
