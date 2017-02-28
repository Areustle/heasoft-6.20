HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= xseltcl

HD_CTASK_SRC_c		= xsel_tcl.c xsel_tclmain.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lxselt${FT_VERS} -lxself${FT_VERS} \
			  -l${CFTOOLSLIB} -l${FTOOLSLIB} -lplt \
			  -l${XANLIB} -l${PIL} -l${READLINE} -l${CFITSIO} \
			  ${LINK_PGPLOT_TCL} -l${TCL} \
			  ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_EXTRA	= install-xselt-scripts

include ${HD_STD_MAKEFILE}

XSELT_SCRIPTS		= xsel_end.tcl xsel_init.tcl

install-xselt-scripts:
	@for file in ${XSELT_SCRIPTS}; do \
		${HD_INSTALL} $$file ${HD_LIB}/txselect/$$file ${HD_CP_P}; \
	done
