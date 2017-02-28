HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= st2rpsf

HD_CTASK_SRC_c		= hst2rpsf.c

HD_CTASK_SRC_f		= st2rpsf.f

# Do not use -fast on Solaris (using COPT_SYS instead of COPT):
HD_CFLAGS		= ${COPT_SYS} ${CFLAGS} ${HD_IFLAGS} ${DEFS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_STD_CLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= st2rpsf.par

HD_INSTALL_HELP		= st2rpsf.hlp st2rpsf.txt

include ${HD_STD_MAKEFILE}
