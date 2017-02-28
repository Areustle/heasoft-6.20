HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= mkfilter2

HD_CTASK_SRC_c		= hmkfilter2.c atQuatToEuler.c dp10DyeElv2.c duvmif2.c \
			  faio.c frftime.c mkf2CalInf.c mkf2GetHK.c \
			  mkf2InitGISRecd.c mkf2InitRecd.c mkf2InitSISRecd.c \
			  mkf2Put2Fits.c mkf2RdctHK.c mkf2ReadGTI.c \
			  mkf2ReadTime.c mkfilter2.c mkfNSAS2Value.c

HD_CTASK_SRC_f		= mkf2Ftphbn.f mkf2ReadInfFil.f numcal2.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= mkfilter2.parin

HD_INSTALL_HELP		= mkfilter2.hlp mkfilter2.txt

include ${HD_STD_MAKEFILE}
