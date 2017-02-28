HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= a2lcurve

HD_CTASK_SRC_c		= ha2lcurve.c

HD_CTASK_SRC_f		= a2lcurve.f coords_work.f co_rawfile.f cross.f \
			  cvxyz.f dirck.f dot.f dsc5.f effic.f getpar.f \
			  lcclose_and_lcinfo.f lccls.f lcinit.f lcwrt.f \
			  marshall.f open_1st_rawfile.f process_read_info.f \
			  qualchecks_and_ratecalcs.f rdraw.f rotat.f \
			  rotatx.f scanan.f scnchk.f set_fovflags.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -l${CFITSIO} -l${XANLIB} -l${PIL} -l${READLINE} \
			  ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= a2lcurve.par

HD_INSTALL_HELP		= a2lcurve.hlp a2lcurve.txt

include ${HD_STD_MAKEFILE}
