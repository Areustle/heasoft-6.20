HD_COMPONENT_NAME	= tcltk

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= ${PGPLOT}

HD_CLEAN		= pgtmp lib*pgplot* ${HD_BLD_LIB}/lib*pgplot* \
			  ${HD_BLD_LIB}/rgb.txt ${HD_BLD_LIB}/grfont.dat \
			  ${HD_BLD_BIN}/pgdisp${BSUF} \
			  ${HD_BLD_BIN}/pgxwin_server${BSUF} \
			  grfont.dat pgpack${BSUF} fonts/pgpack.o cpgplot.h

HD_SHLIB_LIBS		= ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

default: shared-pgplot

all: default publish-pgplot

publish: publish-pgplot

install: install-pgplot

include ${HD_STD_MAKEFILE}

makemake-pgplot: pgtmp/makefile

# Do the standard pgplot setup & makefile generation:
pgtmp/makefile:
	@if [ ! -d pgtmp ]; then \
	  mkdir pgtmp; \
	fi; \
	cd pgtmp; \
	if [ ! -f drivers.list ]; then \
	  ../makemake .. ${PGPLOT_SYS} ${PGPLOT_CONFIG}; \
	fi; \
	if [ "x${PGPLOT_BLD_PNG}" = "xno" ]; then \
		sed "/PNDRIV/d" drivers.list > drivers.list-png; \
		mv drivers.list-png drivers.list; \
	fi; \
	../makemake .. ${PGPLOT_SYS} ${PGPLOT_CONFIG}


# Make the standard pgplot targets (include powpgplot if building for Hera):
build-pgplot: makemake-pgplot
	@if [ "x${HERA_BUILD}" = xyes ]; then \
	  lib_targets="libpgplot.a libcpgplot.a ${TKPGPLOT} libpowpgplot.a"; \
	else \
	  lib_targets="libpgplot.a libcpgplot.a ${TKPGPLOT}"; \
	fi; \
	cd pgtmp; \
	${MAKE} $$lib_targets pgdisp pgxwin_server \
		CCOMPL="${CC} -I${HD_INC} -I${HD_BLD_INC} ${CFLAGS} ${PGPLOT_PNG_INC} ${XLIBS}" \
		FCOMPL="${FC} ${FFLAGS}" \
		TK_INCL="-I${TCL_DIR}/../generic -I${TK_DIR}/../generic" \
		XINCL="${XINCLUDES}"
	@${HD_MAKE} pgpack HD_CTASK=pgpack HD_CTASK_SRC_f=fonts/pgpack.f \
		HD_CFLAGS="${HD_STD_CFLAGS}" HD_FFLAGS="${HD_STD_FFLAGS}" \
		HD_CLIBS="${F77LIBS4C}"
	rm -f grfont.dat
	./pgpack <fonts/grfont.txt

# Now that the static libs are built, unpack them and make shared libraries:
#
# pgplot:
# - Linux: Build a version-numbered shared library and create a symbolic
#   "libpgplot4perl" link to it for the PGPLOT perl module to use.
# - Cygwin & Darwin: Build version-numbered static library for tools to
#   link against.  No version-numbered shared library, only a shared
#   "libpgplot4perl" for PGPLOT perl module.
#
# cpgplot:
# - Linux: Build a version-numbered shared library and create a symbolic
#   "libpgplot4perl" link to it for the PGPLOT perl module to use.
# - Cygwin & Darwin: Build version-numbered static library for tools to
#   link against.  No version-numbered shared library, only a shared
#   "libcpgplot4perl" for PGPLOT perl module.
#
# tkpgplot:
# - Linux: Build a version-numbered shared library.
# - Cygwin & Darwin: Do not build any shared library since we want ximage
#   (currently the only tool which links with tkpgplot) to use the static
#   (version-numbered) library instead.
#
# powpgplot:
# - Hera only: Build a version-numbered shared library.
#

shared-pgplot: build-pgplot
	@if [ -f pgtmp/libpgplot.a ]; then \
	   rm -rf hd_pgtmp; mkdir hd_pgtmp; cd hd_pgtmp; \
	   echo "${AR} x ../pgtmp/libpgplot.a; rm -f _*"; \
	   ${AR} x ../pgtmp/libpgplot.a; rm -f _*; \
	   cd ../; \
	   if [ "x${PGPLOT_SYS}" = xcygwin -o "x${PGPLOT_SYS}" = xdarwin ]; then \
	      ${HD_MAKE} pgplot4perl HD_LIBRARY_ROOT="pgplot4perl" HD_LIBRARY_OBJ="hd_pgtmp/*.o"; \
	   else \
	      ${HD_MAKE} ${PGPLOT} HD_LIBRARY_ROOT="${PGPLOT}" HD_LIBRARY_OBJ="hd_pgtmp/*.o"; \
	   fi; \
	   rm -rf hd_pgtmp; \
	else \
	   echo "libpgplot.a not found - exiting"; exit 1; \
	fi; \
	if [ -f pgtmp/libcpgplot.a ]; then \
	   rm -rf hd_pgtmp; mkdir hd_pgtmp; cd hd_pgtmp; \
	   echo "${AR} x ../pgtmp/libcpgplot.a; rm -f _*"; \
	   ${AR} x ../pgtmp/libcpgplot.a; rm -f _*; \
	   cd ../; \
	   if [ "x${PGPLOT_SYS}" = xcygwin -o "x${PGPLOT_SYS}" = xdarwin ]; then \
	      ${HD_MAKE} cpgplot4perl HD_LIBRARY_ROOT="cpgplot4perl" HD_LIBRARY_OBJ="hd_pgtmp/cpg*.o" HD_SHLIB_LIBS="${HD_SHLIB_LIBS} -L./ -lpgplot4perl"; \
	   else \
	      ${HD_MAKE} c${PGPLOT} HD_LIBRARY_ROOT="c${PGPLOT}" HD_LIBRARY_OBJ="hd_pgtmp/cpg*.o"; \
	   fi; \
	   rm -rf hd_pgtmp; \
	else \
	   echo "libcpgplot.a not found - exiting"; exit 1; \
	fi; \
	if [ "x${PGPLOT_SYS}" != xcygwin -a "x${PGPLOT_SYS}" != xdarwin ]; then \
	  if [ -f pgtmp/libtkpgplot.a ]; then \
	   rm -rf hd_pgtmp; mkdir hd_pgtmp; cd hd_pgtmp; \
	   echo "${AR} x ../pgtmp/libtkpgplot.a; rm -f _*"; \
	   ${AR} x ../pgtmp/libtkpgplot.a; rm -f _*; \
	   cd ../; \
	   ${HD_MAKE} tk${PGPLOT} HD_LIBRARY_ROOT="tk${PGPLOT}" HD_LIBRARY_OBJ="hd_pgtmp/tkpgplot.o"; \
	   rm -rf hd_pgtmp; \
	  else \
	   echo "libtkpgplot.a not found; not making libtk${PGPLOT}${SHLIB_SUFFIX}"; \
	  fi; \
	fi; \
	if [ -f pgtmp/libpowpgplot.a ]; then \
	   rm -rf hd_pgtmp; mkdir hd_pgtmp; cd hd_pgtmp; \
	   echo "${AR} x ../pgtmp/libpowpgplot.a; rm -f _*"; \
	   ${AR} x ../pgtmp/libpowpgplot.a; rm -f _*; \
	   cd ../; \
	   ${HD_MAKE} pow${PGPLOT} HD_LIBRARY_ROOT="pow${PGPLOT}" HD_LIBRARY_OBJ="hd_pgtmp/*.o"; \
	   rm -rf hd_pgtmp; \
	else \
	   echo "libpowpgplot.a not found; not making libpow${PGPLOT}${SHLIB_SUFFIX}"; \
	fi

# Handle version numbers.  As the PGPLOT perl module needs to preferentially
# pick up the shared, unnumbered versions of pgplot and cpgplot (instead of
# their static counterparts), all the unnumbered static libs are renamed with
# version numbers.  This is particularly important on Cygwin, where otherwise
# the unnumbered static library would be picked up by the PGPLOT perl module
# instead.  Where applicable (see comments above), unnumbered symbolic links
# are made to the numbered/shared pgplot and cpgplot.
pgplot-version-numbers:
	@${HD_INSTALL} pgtmp/libpgplot.a lib${PGPLOT}.a ${HD_MV_F}
	@${HD_INSTALL} pgtmp/libcpgplot.a libc${PGPLOT}.a ${HD_MV_F}
	@${HD_INSTALL} pgtmp/libtkpgplot.a libtk${PGPLOT}.a ${HD_MV_F}
	@${HD_INSTALL} pgtmp/libpowpgplot.a libpow${PGPLOT}.a ${HD_MV_F}
	@${HD_INSTALL} lib${PGPLOT}${SHLIB_SUFFIX} libpgplot4perl${SHLIB_SUFFIX} ${HD_LN_S}
	@${HD_INSTALL} libc${PGPLOT}${SHLIB_SUFFIX} libcpgplot4perl${SHLIB_SUFFIX} ${HD_LN_S}

publish-pgplot: pgplot-version-numbers
	@${MAKE} install-pgplot HD_TOP_EXEC_PFX=${HD_BLD_EXEC_PFX} \
		HD_EXEC_PFX=${HD_BLD_EXEC_PFX} HD_CP_P="${HD_LN_S}"
	@${HD_CP_P} pgtmp/cpgplot.h cpgplot.h

install-pgplot: pgplot-version-numbers
	@${MAKE} install-libraries \
		HD_INSTALL_LIBRARIES="pgtmp/pgplot pgtmp/cpgplot pgtmp/powpgplot pgtmp/tkpgplot ${PGPLOT} c${PGPLOT} pow${PGPLOT} tk${PGPLOT} pgplot4perl cpgplot4perl"
	@${MAKE} install-headers \
		HD_INSTALL_HEADERS="pgtmp/cpgplot.h pgtmp/tkpgplot.h"
	@${MAKE} install-tasks \
		HD_INSTALL_TASKS="pgtmp/pgxwin_server pgtmp/pgdisp"
	${HD_INSTALL} grfont.dat ${HD_LIB}/grfont.dat ${HD_CP_P}
	${HD_INSTALL} pgtmp/rgb.txt ${HD_LIB}/rgb.txt ${HD_CP_P}
