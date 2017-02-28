HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= specmat

HD_CTASK_SRC_c		= hspecmat.c readinput.c

HD_CTASK_SRC_f		= altmode.f behead.f calfilename.f celgald.f \
			  csint.f deteff.f deteff_interp.f detres_interp.f \
			  earthp.f exhist.f fclassb.f fknif.f likefile.f \
			  linterp.f locate8.f locate.f mconv.f obsdat.f \
			  orncomb.f respfun.f scalef.f shadow.f sigma.f \
			  specmat.f speconvpha.f speconvrmf.f tconv.f \
			  timeln.f writemat.f wtdfac.f zenithcut.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= specmat.par

HD_INSTALL_HELP		= specmat.hlp specmat.txt

include ${HD_STD_MAKEFILE}
