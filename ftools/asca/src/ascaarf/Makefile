HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= ascaarf

HD_CTASK_SRC_c		= hascaarf.c arffil.c gisnort.c

HD_CTASK_SRC_f		= ascaarf.f clcang.f clcerf.f clcpps.f clcprf.f \
			  clcrrf.f corarf.f gpasca.f gtbmap.f gtenrg.f \
			  gtimap.f gtsize.f gtwmap.f rayres.f sisfdg.f \
			  wrtarf.f xrtefa.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ascaarf.par

HD_INSTALL_HELP		= ascaarf.hlp ascaarf.txt

include ${HD_STD_MAKEFILE}
