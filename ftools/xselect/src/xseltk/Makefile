HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= xseltk

HD_CTASK_SRC_c		= xsel_tk.c xsel_tkmain.c

HD_CFLAGS		= ${HD_STD_CFLAGS} ${XINCLUDES}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lxselt${FT_VERS} -lxself${FT_VERS} \
			  -l${CFTOOLSLIB} -l${FTOOLSLIB} -lplt \
			  -l${XANLIB} -l${PIL} -l${READLINE} -l${CFITSIO} \
			  ${LINK_PGPLOT_TCL} -l${TK} -l${TCL} \
			  ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_EXTRA	= install-xselt-scripts install-xseltk-extras

include ${HD_STD_MAKEFILE}

XSELT_SCRIPTS		= xsel.tk xsel_end.tk xsel_init.tk

XSELTK_EXTRAS		= .xselect xselect.tkstp ixselect

install-xselt-scripts:
	@for file in ${XSELT_SCRIPTS}; do \
		${HD_INSTALL} $$file ${HD_LIB}/txselect/$$file ${HD_CP_P}; \
	done

install-xseltk-extras:
	@for file in ${XSELTK_EXTRAS}; do \
		${HD_INSTALL} $$file ${HD_BIN}/$$file ${HD_CP_P}; \
	done
