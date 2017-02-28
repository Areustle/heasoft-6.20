HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= mkcolor

HD_CTASK_SRC_c		= hmkcolor.c

HD_CTASK_SRC_f		= location.f mkcalloc.f mkccalc.f mkcinit.f \
			  mkclvl.f mkcolor.f mkcralloc.f mkcrdsz.f \
			  mkcread.f mkcwrite.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -l${XANLIB} -l${PIL} -l${READLINE} -l${CFITSIO} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= mkcolor.par

HD_INSTALL_HELP		= mkcolor.hlp mkcolor.txt

include ${HD_STD_MAKEFILE}
