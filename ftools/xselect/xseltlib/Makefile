HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= xselt${FT_VERS}

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_LIBRARY_SRC_c	= xsel_cxpi.c xsel_tcltk.c xsel_tcmd.c xsel_tutils.c

HD_LIBRARY_SRC_f	= xsel_cmd.f xsel_init.f xsel_xpi.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_EXTRA	= install-xselt-scripts

include ${HD_STD_MAKEFILE}

XSELT_SCRIPTS		= pkgIndex.tcl ranlib.tcl xsel_help.tcl xsel_miss.tcl \
			  xsel_ctrl.tk xsel_file.tk xsel_help.tk xsel_learn.tk \
			  xsel_menu.tk xsel_plot.tk xsel_fv.fv

install-xselt-scripts:
	@for file in ${XSELT_SCRIPTS}; do \
		${HD_INSTALL} $$file ${HD_LIB}/txselect/$$file ${HD_CP_P}; \
	done
