HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= ascaeffmap

HD_CTASK_SRC_c		= hascaeffmap.c jcoord.c jcoordf.c

HD_CTASK_SRC_f		= ascaeffmap.f clcang1.f clceff.f fndext.f \
			  gpaeffm.f grpdat.f gtenrg1.f gtsize1.f gtsmap.f \
			  gtwmap1.f rbenrg.f rddtog.f rdgrpc.f wrteff.f \
			  xrt_eff_area.f

HD_CFLAGS		= ${HD_STD_CFLAGS} -DNOATFUNC

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ascaeffmap.par

HD_INSTALL_HELP		= ascaeffmap.hlp ascaeffmap.txt

include ${HD_STD_MAKEFILE}
