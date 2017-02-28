HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= genrsp

HD_CTASK_SRC_c		= hgenrsp.c

HD_CTASK_SRC_f		= agauss.f clcrsp.f foldin.f genrsp.f getsig.f \
			  grspvl.f gtdtsz.f gtrssz.f inprmf.f rdinpd.f \
			  rsppar.f stbins.f wrtrsp.f xfndxt.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_STD_CLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= genrsp.par

HD_INSTALL_HELP		= genrsp.hlp genrsp.txt

include ${HD_STD_MAKEFILE}
