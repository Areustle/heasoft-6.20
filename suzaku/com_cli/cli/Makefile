HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	=

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_LIBRARY_ROOT         = CLI

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	=

HD_LIBRARY_SRC_c	= \
clhelp.c cli_cwrapper.c clrdln.c clsignal.c unixc.c
HD_LIBRARY_SRC_f	= \
chard.f clalir.f clargs.f clatof.f clecho.f cleval.f clflag.f \
clgetl.f clgetlun.f clhcmd.f clicmd.f clifelse.f cliset.f cllab.f cllog.f \
clmacr.f clopen.f clpath.f clproc.f clsleep.f clstatus.f clsubr.f clword.f \
intrdl.f iopen.f keyrd.f lenrd.f logrd.f miscunix.f \
numrd.f opnrd.f printf.f qopen.f sysunix.f txtrd.f version.f \
${F77_F90_F95_ONLY_SRC_f} ${G77_ONLY_SRC_f} ${G95_ONLY_SRC_f} \
${GFORTRAN_ONLY_SRC_f} ${IFORT_ONLY_SRC_f}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${READLINE} ${F77LIBS4C}

HD_ADD_SHLIB_LIBS	= yes

HD_DISTCLEAN		= clhelp.c

default: build-libCLI

all: default publish

include ${HD_STD_MAKEFILE}

FC_BASENAME_F77		= ${shell echo ${FC_BASENAME} | grep -c f77}
FC_BASENAME_F90		= ${shell echo ${FC_BASENAME} | grep -c f90}
FC_BASENAME_F95		= ${shell echo ${FC_BASENAME} | grep -c f95}
FC_BASENAME_G77		= ${shell echo ${FC_BASENAME} | grep -c g77}
FC_BASENAME_G95		= ${shell echo ${FC_BASENAME} | grep -c g95}
FC_BASENAME_GFORTRAN	= ${shell echo ${FC_BASENAME} | grep -c gfortran}
FC_BASENAME_IFORT	= ${shell echo ${FC_BASENAME} | grep -c ifort}

# If using g77, compile g77_fileio.f.
# If using g95, compile g95_fileio.f.
# If using gfortran (or GNU f95 [=gfortran]), compile gfortran_fileio.f.
# If using ifort, compile ifort_fileio.f.
# If using f77, f90, or f95, compile f90_fileio.f.
build-libCLI:
	@if [ ${FC_BASENAME_G77} != 0  ]; then \
		${HD_MAKE} ${HD_LIBRARY_ROOT} \
			G77_ONLY_SRC_f="g77_fileio.f"; \
	elif [ ${FC_BASENAME_F77} != 0 -a "x${G77}" = xyes ]; then \
		${HD_MAKE} ${HD_LIBRARY_ROOT} \
			G77_ONLY_SRC_f="g77_fileio.f"; \
	elif [ ${FC_BASENAME_G95} != 0 ]; then \
		${HD_MAKE} ${HD_LIBRARY_ROOT} \
			G95_ONLY_SRC_f="g95_fileio.f"; \
	elif [ ${FC_BASENAME_GFORTRAN} != 0 ]; then \
		${HD_MAKE} ${HD_LIBRARY_ROOT} \
			GFORTRAN_ONLY_SRC_f="gfortran_fileio.f"; \
	elif [ ${FC_BASENAME_F95} != 0 -a "x${G77}" = xyes ]; then \
		${HD_MAKE} ${HD_LIBRARY_ROOT} \
			GFORTRAN_ONLY_SRC_f="gfortran_fileio.f"; \
	elif [ ${FC_BASENAME_IFORT} != 0 ]; then \
		${HD_MAKE} ${HD_LIBRARY_ROOT} \
			IFORT_ONLY_SRC_f="ifort_fileio.f"; \
	elif [ ${FC_BASENAME_F77} != 0 -a "x${G77}" = xno ]; then \
		${HD_MAKE} ${HD_LIBRARY_ROOT} \
			F77_F90_F95_ONLY_SRC_f="f90_fileio.f"; \
	elif [ ${FC_BASENAME_F90} != 0 -a "x${G77}" = xno ]; then \
		${HD_MAKE} ${HD_LIBRARY_ROOT} \
			F77_F90_F95_ONLY_SRC_f="f90_fileio.f"; \
	elif [ ${FC_BASENAME_F95} != 0 -a "x${G77}" = xno ]; then \
		${HD_MAKE} ${HD_LIBRARY_ROOT} \
			F77_F90_F95_ONLY_SRC_f="f90_fileio.f"; \
	else \
		echo "Cannot determine which Fortran code to compile!"; \
		exit 1; \
	fi

clhelp.c: ../doc/cli-userguide.doc
	echo "void clhelp_() { int i; static char *m[] = {" >$@
	sed -e 's/^/"/' -e 's/$$/",/' ../doc/cli-userguide.doc | \
	sed 's/\\/\\\\/' >>$@
	echo "0}; for (i=0; m[i]; i++) {clecho_(m[i],strlen(m[i]));} }" >>$@

clhelp.o: clhelp.c
