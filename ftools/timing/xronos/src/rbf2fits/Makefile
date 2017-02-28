HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= rbf2fits

HD_CTASK_SRC_c		= hrbf2fits.c

HD_CTASK_SRC_f		= rbf2fits.f rbfinit.f shftim.f tim_point.f \
			  write_comment.f write_deftype0.f write_deftype1.f \
			  write_deftype2.f write_descriptive.f \
			  write_detnam.f write_gskey.f write_hdu.f \
			  write_lekey.f write_mekey.f write_timing.f \
			  write_type0_data.f write_type1_data.f \
			  write_type2_data.f write_write.f xrrsl3.f xrshfcon.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lxron${FT_VERS} -l${CFITSIO} -lplt \
			  -l${XANLIB} -l${PIL} -l${READLINE} ${LINK_PGPLOT} \
			  ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= rbf2fits.par

HD_INSTALL_HELP		= rbf2fits.hlp rbf2fits.txt

HD_DISTCLEAN		= xrrsl3.f

include ${HD_STD_MAKEFILE}

xrrsl3.f: xrrsl3.sun xrrsl3.vms
	@rm -f xrrsl3.f; \
	if [ "x${EXT}" = xosf ]; then \
		${HD_LN_S} xrrsl3.vms $@; \
	else \
		${HD_LN_S} xrrsl3.sun $@; \
	fi
