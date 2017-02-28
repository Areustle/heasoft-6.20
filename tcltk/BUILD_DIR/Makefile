HD_COMPONENT_NAME	= tcltk

HD_COMPONENT_VERS	=

HD_SUBDIRS		= @h_components@

HD_DISTCLEAN		= config.cache config.log config.status \
			  configure.lineno cxxtest.out fortest.out \
			  hd_install headas-setup hmakerc

HD_CLEAN		= ${HD_BLD_EXEC_PFX} ${HD_BLD_PFX} \
			  ${TCLREADLINE_DIR}/lib${TCLREADLINE}${SHLIB_SUFFIX}

HD_CTASK		= hd_install

HD_CTASK_SRC_c		= hd_install.c

HD_CFLAGS               = ${CFLAGS}

HD_INSTALL_TASKS	= hd_install

HD_INSTALL_SHELL_SCRIPTS	= hd_scanenv hmake lhea-fixperl

HD_INSTALL_EXTRA	= install-BUILD_DIR install-symlinks

include ${HD_STD_MAKEFILE}

distclean: hd-std-distclean
	cp -pf Makefile.in Makefile

# Install BUILD_DIR; tap dance needed because the built tools live in BUILD_DIR
#-------------------------------------------------------------------------------
install-BUILD_DIR:
	@${HD_INSTALL} ${HD_BUILD_DIR} ${HD_EXEC_PFX}/BUILD_DIR ${HD_CP_P}
#-------------------------------------------------------------------------------

# Specializations for pow
#-------------------------------------------------------------------------------
subdir-pow:
	@${HD_MAKE} hd-std-subdir HD_SUBDIR=pow
	@${HD_MAKE} hd-std-subdir HD_SUBDIR=pow HD_TARGET=wcslib-obj-remove
#-------------------------------------------------------------------------------

# Specializations for tcl
#-------------------------------------------------------------------------------
subdir-tcl:
	cd ${TCL_DIR}; ${MAKE} ${HD_TARGET}

all-tcl default-tcl: build-tcl publish-tcl

build-tcl:
	${MAKE} subdir-tcl
	@if [ "x${SYMBOLS}" = xyes ]; then \
		${HD_INSTALL} ${TCL_DIR}/lib${TCL}g${SHLIB_SUFFIX} \
			${TCL_DIR}/lib${TCL}${SHLIB_SUFFIX} ${HD_LN_S}; \
	fi

publish-tcl:
	${MAKE} install-tcl HD_TOP_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_CP_P="${HD_LN_S}"

#	${MAKE} install-tcl HD_TOP_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_TOP_LIB=${HD_BLD_LIB} HD_LIB=${HD_BLD_LIB} HD_TOP_INC=${HD_BLD_INC} HD_INC=${HD_BLD_INC} HD_CP_P="${HD_LN_S}"

install-tcl:
	${MAKE} subdir-tcl HD_TARGET="install" \
		prefix=${HD_EXEC_PFX} exec_prefix=${HD_EXEC_PFX} \
		libdir=${HD_LIB}
	@${HD_INSTALL} ${TCL_DIR}/../generic/tclInt.h ${HD_INC} ${HD_CP}
	@${HD_INSTALL} ${TCL_DIR}/../generic/tclIntDecls.h ${HD_INC} ${HD_CP}
	@${HD_INSTALL} ${TCL_DIR}/tclsh${BSUF} ${HD_BIN}/tclsh${BSUF} ${HD_CP}
	@if [ "x${SHLIB_SUFFIX}" = x.dll ]; then \
		${HD_INSTALL} ${HD_BIN}/lib${TCL}${SHLIB_SUFFIX} \
			${HD_LIB}/lib${TCL}${SHLIB_SUFFIX} ${HD_LN_S}; \
	fi
	@if [ "x${SYMBOLS}" = xyes ]; then \
		${HD_INSTALL} ${HD_LIB}/lib${TCL}g${SHLIB_SUFFIX} \
			${HD_LIB}/lib${TCL}${SHLIB_SUFFIX} ${HD_LN_S}; \
	fi

distclean-tcl:
	@${MAKE} subdir-tcl HD_TARGET=distclean
	@rm -rf ${TCL_DIR}/so_locations ${TCL_DIR}/a.out.dSYM
#-------------------------------------------------------------------------------

# Specializations for tk
#-------------------------------------------------------------------------------
subdir-tk:
	cd ${TK_DIR}; ${MAKE} ${HD_TARGET}

all-tk default-tk: build-tk publish-tk

build-tk:
	${MAKE} subdir-tk

publish-tk:
	${MAKE} install-tk HD_TOP_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_CP_P="${HD_LN_S}"

install-tk:
	${MAKE} subdir-tk HD_TARGET="install" \
		prefix=${HD_EXEC_PFX} exec_prefix=${HD_EXEC_PFX} \
		libdir=${HD_LIB}
	@${HD_INSTALL} ${TK_DIR}/../generic/tkCanvas.h ${HD_INC} ${HD_CP}
	@${HD_INSTALL} ${TK_DIR}/../generic/tkInt.h ${HD_INC} ${HD_CP}
	@${HD_INSTALL} ${TK_DIR}/../generic/tkIntDecls.h ${HD_INC} ${HD_CP}
	@${HD_INSTALL} ${TK_DIR}/../generic/tkPort.h ${HD_INC} ${HD_CP}
	@${HD_INSTALL} ${TK_DIR}/tkUnixPort.h ${HD_INC} ${HD_CP}
	@${HD_INSTALL} ${TK_DIR}/wish${BSUF} ${HD_BIN}/wish${BSUF} ${HD_CP}
	@if [ "x${SHLIB_SUFFIX}" = x.dll ]; then \
		${HD_INSTALL} ${HD_BIN}/lib${TK}${SHLIB_SUFFIX} \
			${HD_LIB}/lib${TK}${SHLIB_SUFFIX} ${HD_LN_S}; \
	fi
	@if [ "x${SYMBOLS}" = xyes ]; then \
		${HD_INSTALL} ${HD_LIB}/lib${TK}g${SHLIB_SUFFIX} \
			${HD_LIB}/lib${TK}${SHLIB_SUFFIX} ${HD_LN_S}; \
	fi

distclean-tk:
	@${MAKE} subdir-tk HD_TARGET=distclean
	@rm -rf ${TK_DIR}/so_locations ${TK_DIR}/a.out.dSYM
#-------------------------------------------------------------------------------

# Specializations for itcl
#-------------------------------------------------------------------------------
subdir-itcl:
	cd ${ITCL_DIR}; ${MAKE} ${HD_TARGET}

all-itcl default-itcl: build-itcl publish-itcl

build-itcl:
	${MAKE} subdir-itcl

publish-itcl:
	${MAKE} install-itcl HD_TOP_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_CP_P="${HD_LN_S}"

install-itcl:
	${MAKE} subdir-itcl HD_TARGET="install" \
		prefix=${HD_EXEC_PFX} exec_prefix=${HD_EXEC_PFX} \
		libdir=${HD_LIB}
	@if [ "x${SYMBOLS}" = xyes ]; then \
		${HD_INSTALL} ${HD_LIB}/${ITCL}/lib${ITCL}g${SHLIB_SUFFIX} \
		       ${HD_LIB}/${ITCL}/lib${ITCL}${SHLIB_SUFFIX} ${HD_LN_S}; \
	fi

distclean-itcl:
	@${MAKE} subdir-itcl HD_TARGET=distclean
	@rm -rf ${ITCL_DIR}/a.out.dSYM ${ITCL_DIR}/configure.lineno \
		${ITCL_DIR}/so_locations
#-------------------------------------------------------------------------------

# Specializations for itk
#-------------------------------------------------------------------------------
subdir-itk:
	cd ${ITK_DIR}; ${MAKE} ${HD_TARGET}

all-itk default-itk: build-itk publish-itk

build-itk:
	${MAKE} subdir-itk

publish-itk:
	${MAKE} install-itk HD_TOP_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_CP_P="${HD_LN_S}"

install-itk:
	${MAKE} subdir-itk HD_TARGET="install" \
		prefix=${HD_EXEC_PFX} exec_prefix=${HD_EXEC_PFX} \
		libdir=${HD_LIB}
	@if [ "x${SYMBOLS}" = xyes ]; then \
		${HD_INSTALL} ${HD_LIB}/${ITK}/lib${ITK}g${SHLIB_SUFFIX} \
			${HD_LIB}/${ITK}/lib${ITK}${SHLIB_SUFFIX} ${HD_LN_S}; \
	fi

distclean-itk:
	@${MAKE} subdir-itk HD_TARGET=distclean
	@rm -rf ${ITK_DIR}/a.out.dSYM ${ITK_DIR}/configure.lineno \
		${ITK_DIR}/so_locations
#-------------------------------------------------------------------------------

# Specializations for iwidgets
#-------------------------------------------------------------------------------
subdir-iwidgets:
	cd ${IWIDGETS_DIR}; ${MAKE} ${HD_TARGET}

all-iwidgets default-iwidgets: build-iwidgets publish-iwidgets

build-iwidgets:
	${MAKE} subdir-iwidgets

publish-iwidgets:
	${MAKE} install-iwidgets HD_TOP_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_CP_P="${HD_LN_S}"

install-iwidgets:
	${MAKE} subdir-iwidgets HD_TARGET="install" \
		prefix=${HD_EXEC_PFX} exec_prefix=${HD_EXEC_PFX} \
		LD_LIBRARY_PATH="${TCL_DIR}:${LD_LIBRARY_PATH}" \
		DYLD_LIBRARY_PATH="${TCL_DIR}:${DYLD_LIBRARY_PATH}"
	@cd ${HD_TOP_LIB}; rm -f iwidgets; ln -sf ${IWIDGETS} iwidgets

distclean-iwidgets:
	@${MAKE} subdir-iwidgets HD_TARGET=distclean
	@rm -f ${IWIDGETS_DIR}/iwidgets.tcl ${IWIDGETS_DIR}/pkgIndex.tcl
#-------------------------------------------------------------------------------

# Specializations for iwidgets2.2.0
#-------------------------------------------------------------------------------
subdir-iwidgets220:
	cd ${IWIDGETS220_DIR}; ${MAKE} ${HD_TARGET}

all-iwidgets220 default-iwidgets220: build-iwidgets220 publish-iwidgets220

build-iwidgets220:
	${MAKE} subdir-iwidgets220

publish-iwidgets220:
	${MAKE} install-iwidgets220 HD_TOP_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_CP_P="${HD_LN_S}"

install-iwidgets220:
	${MAKE} subdir-iwidgets220 HD_TARGET="install" \
		prefix=${HD_EXEC_PFX} exec_prefix=${HD_EXEC_PFX} \
		LD_LIBRARY_PATH="${TCL_DIR}:${LD_LIBRARY_PATH}"
#-------------------------------------------------------------------------------

# Specializations for tix
#-------------------------------------------------------------------------------
subdir-tix:
	cd ${TIX_DIR}; ${MAKE} ${HD_TARGET}

all-tix default-tix: build-tix publish-tix

build-tix:
	${MAKE} subdir-tix

publish-tix:
	${MAKE} install-tix HD_TOP_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_CP_P="${HD_LN_S}"

install-tix:
	${MAKE} subdir-tix HD_TARGET="install" \
		prefix=${HD_EXEC_PFX} exec_prefix=${HD_EXEC_PFX} \
		libdir=${HD_LIB}

distclean-tix:
	@${MAKE} subdir-tix HD_TARGET=distclean
	@rm -rf ${TIX_DIR}/a.out.dSYM ${TIX_DIR}/configure.lineno \
		${TIX_DIR}/so_locations
#-------------------------------------------------------------------------------

# Specializations for tclreadline
#-------------------------------------------------------------------------------
subdir-tclreadline:
	cd ${TCLREADLINE_DIR}; ${MAKE} ${HD_TARGET}

all-tclreadline default-tclreadline: build-tclreadline publish-tclreadline

build-tclreadline:
	${MAKE} subdir-tclreadline
	@if [ "x${EXT}" = xwin ]; then \
		rm -rf ./$$.tmp; mkdir ./$$.tmp; cd ./$$.tmp; \
		echo "${AR} x ${TCLREADLINE_DIR}/.libs/libtclreadline.a"; \
		${AR} x ${TCLREADLINE_DIR}/.libs/libtclreadline.a; \
		echo "${SHLIB_LD} ${LDFLAGS_C} -o ${TCLREADLINE_DIR}/lib${TCLREADLINE}${SHLIB_SUFFIX} * -L${HD_LFLAGS} -L${TCL_DIR} -l${TCL} -l${READLINE} ${SYSLIBS}"; \
		${SHLIB_LD} ${LDFLAGS_C} -o ${TCLREADLINE_DIR}/lib${TCLREADLINE}${SHLIB_SUFFIX} * -L${HD_LFLAGS} -L${TCL_DIR} -l${TCL} -l${READLINE} ${SYSLIBS}; \
		cd ../; rm -rf ./$$.tmp; \
	fi

publish-tclreadline:
	${MAKE} install-tclreadline HD_TOP_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_EXEC_PFX=${HD_BLD_EXEC_PFX}

install-tclreadline:
	${MAKE} subdir-tclreadline HD_TARGET="install" \
		prefix=${HD_EXEC_PFX} exec_prefix=${HD_EXEC_PFX} libdir=${HD_LIB} TCLRL_DIR=${HD_LIB}/tclreadline${TCLREADLINE_VERS} TCLRL_LIBDIR=${HD_LIB} tclrldir=${HD_LIB}/tclreadline${TCLREADLINE_VERS}
	@cd ${HD_TOP_LIB}; rm -f tclreadline; ln -sf tclreadline${TCLREADLINE_VERS} tclreadline
	@if [ "x${HD_LIB_STYLE}" = xstatic ]; then \
		${HD_INSTALL} ${HD_LIB}/libtclreadline.a ${HD_LIB}/lib${TCLREADLINE}.a ${HD_LN_S}; \
	fi
	@if [ "x${EXT}" = xwin ]; then \
		${HD_INSTALL} ${TCLREADLINE_DIR}/lib${TCLREADLINE}${SHLIB_SUFFIX} ${HD_LIB} ${HD_CP_P}; \
	fi
#-------------------------------------------------------------------------------

# Specializations for xpa
#-------------------------------------------------------------------------------
subdir-xpa:
	cd ${XPA_DIR}; ${MAKE} ${HD_TARGET}

all-xpa default-xpa: build-xpa publish-xpa

build-xpa:
	@if [ "x${FV_BUILD}" = xyes ]; then \
		${MAKE} subdir-xpa HD_TARGET="${HD_TARGET}" \
			TCL_CFLAGS="-I${HD_BLD_INC}" LDFLAGS="${FV_CFLAGS}"; \
	else \
		${MAKE} subdir-xpa HD_TARGET="${HD_TARGET}" \
			TCL_CFLAGS="-I${HD_BLD_INC}"; \
	fi; \

publish-xpa:
	${MAKE} subdir-xpa HD_TARGET="tclxpa install" \
		prefix=${HD_BLD_EXEC_PFX} exec_prefix=${HD_BLD_EXEC_PFX} \
		TCL_LIBS="${HD_BLD_LIB}/lib${TCL}${SHLIB_SUFFIX}"

install-xpa:
	@rm -f ${XPA_DIR}/libtclxpa*
	${MAKE} subdir-xpa HD_TARGET="tclxpa install" \
		prefix=${HD_EXEC_PFX} exec_prefix=${HD_EXEC_PFX} \
		TCL_LIBS="${HD_LIB}/lib${TCL}${SHLIB_SUFFIX}"

distclean-xpa:
	@${MAKE} subdir-xpa HD_TARGET=distclean
	@rm -f ${XPA_DIR}/configure.lineno ${XPA_DIR}/so_locations
#-------------------------------------------------------------------------------

# Specializations for pgplotperl/extf77perl
#-------------------------------------------------------------------------------
# Remove Perl module Makefiles after building or installing so that if the
# Perl installation changes before a subsequent make we won't have lingering
# dependencies (on e.g. Perl's "config.h") that will cause make to fail.

subdir-pgplotperl:
	${HD_MAKE} makemake-extf77perl
	cd ${EXTF77PERL_DIR}; ${MAKE} ${HD_TARGET}
	${HD_MAKE} makemake-pgplotperl
	cd ${PGPLOTPERL_DIR}; ${MAKE} ${HD_TARGET} OTHERLDFLAGS="${PERL_OTHERLDFLAGS}"
	@rm -f ${EXTF77PERL_DIR}/Makefile
	@rm -f ${PGPLOTPERL_DIR}/Makefile

makemake-extf77perl: ${EXTF77PERL_DIR}/Makefile

makemake-pgplotperl: ${PGPLOTPERL_DIR}/Makefile

${EXTF77PERL_DIR}/Makefile: ${EXTF77PERL_DIR}/Makefile.PL
	cd ${EXTF77PERL_DIR}; ${PERL} Makefile.PL

${PGPLOTPERL_DIR}/Makefile: ${PGPLOTPERL_DIR}/Makefile.PL
	cd ${PGPLOTPERL_DIR}; PGPLOT_DIR=${HD_SRC}/pgplot; F77LIBS="${F77LIBS4C}"; export PGPLOT_DIR F77LIBS; ${PERL} Makefile.PL

all-pgplotperl default-pgplotperl:
	${HD_MAKE} build-pgplotperl

build-pgplotperl:
	@if [ ! -d "${HD_SRC}/pgplot/pgtmp" ]; then \
		echo "Could not find ${HD_SRC}/pgplot/pgtmp/."; \
		echo "Please build PGPLOT in ${HD_SRC}/pgplot first."; \
		exit 1; \
	fi
	${MAKE} subdir-pgplotperl HD_TARGET=

install-pgplotperl: makemake-pgplotperl
	@${HD_INSTALL} ${PGPLOTPERL_DIR}/blib/arch/auto/PGPLOT/PGPLOT${DYNPERL_SUFFIX} ${HD_PERL_LIB}/auto/PGPLOT/PGPLOT${DYNPERL_SUFFIX} ${HD_CP_P}
	@${HD_INSTALL} ${PGPLOTPERL_DIR}/blib/lib/PGPLOT.pm  ${HD_PERL_LIB}/PGPLOT.pm ${HD_CP_P}
	@rm -f ${PGPLOTPERL_DIR}/Makefile

# Clean in reverse order since pgplotperl's Makefile needs a built extf77perl:
clean-pgplotperl:
	${HD_MAKE} makemake-pgplotperl
	cd ${PGPLOTPERL_DIR}; ${MAKE} clean
	${HD_MAKE} makemake-extf77perl
	cd ${EXTF77PERL_DIR}; ${MAKE} clean

# Must rebuild extf77perl here in order to make the pgplotperl Makefile:
distclean-pgplotperl:
	${HD_MAKE} makemake-extf77perl
	cd ${EXTF77PERL_DIR}; ${MAKE}
	${HD_MAKE} makemake-pgplotperl
	cd ${PGPLOTPERL_DIR}; ${MAKE} distclean
	cd ${EXTF77PERL_DIR}; ${MAKE} distclean
#-------------------------------------------------------------------------------
