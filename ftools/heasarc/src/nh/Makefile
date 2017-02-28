HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= nh

HD_CTASK_SRC_c		= hnh.c

HD_CTASK_SRC_f		= nh.f nhinit.f nhmake.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

# Use -O0 instead of -O on Linux (using FOPT_SYS instead of FOPT):
HD_FFLAGS		= ${FOPT_SYS} ${FFLAGS} ${HD_IFLAGS} ${DEFS}

HD_CLIBS		= ${HD_STD_CLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= nh.parin

HD_INSTALL_HELP		= nh.hlp nh.txt

HD_INSTALL_REFDATA	= h1_nh_DL.fits h1_nh_LAB.fits

include ${HD_STD_MAKEFILE}
