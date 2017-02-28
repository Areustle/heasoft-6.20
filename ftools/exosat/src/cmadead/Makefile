HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= cmadead

HD_CTASK_SRC_c		= hcmadead.c

HD_CTASK_SRC_f		= applycorre.f chkfiles.f cmadead.f cmadeadinit.f \
			  cmadeadmake.f deadcor.f excopyimg.f exisitlc.f \
			  exreadlc.f extable.f imagedead.f mall1.f mall2.f \
			  update.f writearray.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lexo${FT_VERS} \
			  -l${CFITSIO} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= cmadead.par

HD_INSTALL_HELP		= cmadead.hlp cmadead.txt

include ${HD_STD_MAKEFILE}
