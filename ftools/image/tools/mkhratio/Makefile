HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= mkhratio

HD_CTASK_SRC_c		= hmkhratio.c

HD_CTASK_SRC_f		= location.f mkhalloc.f mkhbin.f mkhcalc.f \
			  mkhinit.f mkhralloc.f mkhratio.f mkhrdsz.f \
			  mkhread.f mkhwrite.f null.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -l${XANLIB} -l${PIL} -l${READLINE} -l${CFITSIO} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= mkhratio.par

HD_INSTALL_HELP		= mkhratio.hlp mkhratio.txt

include ${HD_STD_MAKEFILE}
