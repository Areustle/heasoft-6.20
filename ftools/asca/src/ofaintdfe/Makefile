HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= ofaintdfe

HD_CTASK_SRC_c		= hofaintdfe.c ocfitsis.c oclassify3.c oerror.c \
			  ofaintdfe.c omain.c
# included by ofaintdfe.c:
#  oerfcc.c ogetTemplate.c onew_zero.c opolint.c
#  oqromb.c oreadTempPara.c otrapzd.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ofaintdfe.parin

include ${HD_STD_MAKEFILE}
