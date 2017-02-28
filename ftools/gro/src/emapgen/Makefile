HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= emapgen

HD_CTASK_SRC_c		= hemapgen.c

HD_CTASK_SRC_f		= addbin_m.f array2fcube.f binevt.f bit5pull.f \
			  brectn.f celgal_m.f convrt.f getext.f grdtjd.f \
			  initlz.f mapgen.f read_qvpfits.f rltime.f \
			  tjdgrd.f wrt_mapfts.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= emapgen.parin

HD_INSTALL_HELP		= emapgen.hlp emapgen.txt

include ${HD_STD_MAKEFILE}
