##########################################################################
# package:   makefiles                                                   #
# version:   2.4.7, 21-Jun-2002                                          #
# file:      Makefile(.in)                                               #
# author:    Jerzy.Borkowski@obs.unige.ch                                #
# copyright: ISDC, chemin d'Ecogia 16, 1290 Versoix, Switzerland         #
##########################################################################
#
# DO __NOT__ EDIT THIS FILE. Edit makeisdc1.in file instead. Refer to
# makefiles.txt file for more information.

# ======================= ISDC SOFTWARE REPOSITORY =========================
#
# Top of ISDC software repository. By default set to /isdc/dev. Change to
# something else as needed. People outside ISDC may want to set ISDC_DEV to
# something completely different. To change it, set ISDC_ENV environment
# variable before running configure script.

ISDC_ENV		= /isdc/dev

# List of ISDC_ENV subdirectories.

ISDC_BIN_DIR		= ${ISDC_ENV}/bin
ISDC_F90MOD_DIR		= ${ISDC_ENV}/f90mod
ISDC_HELP_DIR		= ${ISDC_ENV}/help
ISDC_INC_DIR		= ${ISDC_ENV}/include
ISDC_LIB_DIR		= ${ISDC_ENV}/lib

# ======================= EXTERNAL SOFTWARE LINKAGE ========================
#
# Define where 3rd party software not written by, but used at ISDC resides.
# If project links external libraries or includes *.h files define them here.
# For each external library define XXX_INC_DIR and XXX_LIB_DIR and add them
# together to  EXTERNAL_INC_DIR and EXTERNAL_LIB_DIR respectively. If there
# are also libraries with F90 bindings and F90 modules then include them in
# EXTERNAL_F90MOD_LIB as well. Defaults are valid for ISDC only. To change
# them set appropriate environment variable before running configure script.

EXTERN_1_INC_DIR	= 
EXTERN_1_LIB_DIR	= 
EXTERN_1_F90MOD_DIR	= 

EXTERN_2_INC_DIR	= 
EXTERN_2_LIB_DIR	= 
EXTERN_2_F90MOD_DIR	= 

# now everything is put together into 1 line

EXTERNAL_INC_DIR	= ${EXTERN_1_INC_DIR} ${EXTERN_2_INC_DIR}
EXTERNAL_LIB_DIR	= ${EXTERN_1_LIB_DIR} ${EXTERN_2_LIB_DIR}
EXTERNAL_F90MOD_DIR	= ${EXTERN_1_F90MOD_DIR} ${EXTERN_2_F90MOD_DIR}

# ISDC's ROOT compilation flag

ISDCROOTDIR_TO_SKIP	= 
ISDCROOT_INC_DIR	= -I/isdc/dev/root/include/root
ISDCROOT_LIB_DIR	= -L/isdc/dev/root/lib

# ===================== STUFF AUTOCONFIGURED BY AUTOCONF ===================
#
# This section is autoconfigured and should not be edited by hand. If you do
# edit it, you should know autoconf's details.

# VPATH _MAY_ be useful in the future ...

srcdir			= .


# system and host information ...

MY_HOST			= i686-pc-linux-gnu
MY_ARCH			= _LINUX

# Various install stuff. Please note, that we ignore INSTALL variable and
# use our own install program. This setup assumes tar and gzip programs are
# in PATH.

RANLIB			= ranlib
INSTALL			= /isdc/dev/support-sw/makefiles/2.4.7/ac_stuff/isdc_install
CP			= /bin/cp
MKDIR			= /bin/mkdir
MV			= /bin/mv
RM			= /bin/rm
RMDIR			= /bin/rmdir
TEE			= /usr/bin/tee
TAR_EXE			= tar
GZIP_EXE		= gzip

# Yet more install stuff. The following is a trick to allow trouble-free (?)
# installation without F90 compiler. This is really a hack, but what can
# I do ?

_LOCAL_F90_INSTALL	= $(MAKE) f90_install
LOCAL_F90_INSTALL	= ${_LOCAL_F90_INSTALL}

_GLOBAL_F90_INSTALL	= $(MAKE) global_f90_install
GLOBAL_F90_INSTALL	= ${_GLOBAL_F90_INSTALL}

# =========================== TARGET 'all' =================================
#
# Top level target. Executed when make is run without arguments. This has to
# the first target defined in the Makefile, therefore it is placed at the
# beginning of makeisdc1(.in). Real work however is submitted to
# all_late_bind target which is defined after all user defined targets are
# defined.
#
#      -  <<-- ignore error return code from command

all:		all_late_bind

# ======================== TARGET 'testcommands' ============================
#
# This target is invoked by 'make test'. It does nothing, but since this is
# DOUBLE COLON target, real testcommands:: target can be specified in
# makeisdc1.in file.

testcommands::

testf90commands::

# ===================== STUFF COMMON TO ALL LANGUAGES ======================
#
# Define directories to look at. It's good to include current directory as 
# the 1st item. LAST_LIBS contains the list of system libraries which have to 
# be linked for typical ISDC executable (DAL/CFITSIO used). Those libraries 
# should be given at the end of the command line.

TOTAL_INC_DIR		= -I. -I.. -I../include -I${LOCAL_INSTALL_INC_DIR} ${EXTERNAL_INC_DIR} -I${ISDC_INC_DIR} ${ISDCROOT_INC_DIR} -I${ISDC_INC_DIR}/root
TOTAL_LIB_DIR		= -L. -L.. -L../lib -L${LOCAL_INSTALL_LIB_DIR} ${EXTERNAL_LIB_DIR} -L${ISDC_LIB_DIR} ${ISDCROOT_LIB_DIR}
TOTAL_F90MOD_DIR	= -${F90MOD_PREFIX}. -${F90MOD_PREFIX}.. -${F90MOD_PREFIX}${LOCAL_INSTALL_F90MOD_DIR} ${EXTERNAL_F90MOD_DIR} -${F90MOD_PREFIX}${ISDC_F90MOD_DIR}

DEFS			= -DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" -DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DSTDC_HEADERS=1 -DHAVE_SYS_TYPES_H=1 -DHAVE_SYS_STAT_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRING_H=1 -DHAVE_MEMORY_H=1 -DHAVE_STRINGS_H=1 -DHAVE_INTTYPES_H=1 -DHAVE_STDINT_H=1 -DHAVE_UNISTD_H=1 -DSIZEOF_SHORT=2 -DSIZEOF_INT=4 -DSIZEOF_LONG=4 -DSIZEOF_LONG_LONG=8 -DSIZEOF_VOID_P=4 -DSTDC_HEADERS=1 -DHAVE_MALLOC_H=1 -DHAVE_PTHREAD_H=1 -DHAVE_STDLIB_H=1 -DHAVE_STRCHR=1 -DHAVE_MEMCPY=1 -DHAVE_GMTIME_R=1 -DHAVE_STRTOLL=1 -DHAVE_LLABS=1 -DHAVE_READLINE_LIBS=1 
LIBOBJS			= 
X_INCLUDES		= 
X_LIBRARIES		= -L/usr/X11/lib
LAST_LIBS		= -lreadline -ltermcap -lm  
POSIX4_LIB		= 

ifndef	NAGLIB
NAGLIB			= nag
endif

ISDC_COMMON_LIBS	= -lcommon -ldal -lpil -lril -lcfitsio ${LAST_LIBS}

# ========================= C SPECIFIC STUFF ===============================
#
# note : we are redefining .c.o rule

CC			= gcc -Df2cFortran
CFLAGS			= -g -O2
ALL_C_CFLAGS		= ${CFLAGS} ${TOTAL_INC_DIR} ${X_INCLUDES} -D${MY_ARCH} ${DEFS}
ALL_C_LDFLAGS		= ${X_LIBRARIES} ${TOTAL_LIB_DIR}

.c.o:
	${CC} -c ${ALL_C_CFLAGS} $< -o $(<D)/$(@F)

# ========================= C++ SPECIFIC STUFF =============================
#
# note : we are redefining .cxx.o, .cpp.o, .cc.o and .C.o rules

CXX			= g++
CXXFLAGS		= -g
ALL_CXX_CFLAGS		= ${CXXFLAGS} ${TOTAL_INC_DIR} ${X_INCLUDES} -D${MY_ARCH} ${DEFS}
ALL_CXX_LDFLAGS		= ${X_LIBRARIES} ${TOTAL_LIB_DIR}

.C.o:
	${CXX} -c ${ALL_CXX_CFLAGS} $< -o $(<D)/$(@F)

.cc.o:
	${CXX} -c ${ALL_CXX_CFLAGS} $< -o $(<D)/$(@F)

.cpp.o:
	${CXX} -c ${ALL_CXX_CFLAGS} $< -o $(<D)/$(@F)

.cxx.o:
	${CXX} -c ${ALL_CXX_CFLAGS} $< -o $(<D)/$(@F)

# ========================= F90 SPECIFIC STUFF =============================
#
# We need to update .SUFFIXES since make on IRIX does not understand .f90 by
# default. If you have access to F90 compiler which does not accept neither
# -I nor -M in command line as a include directory switch, you have to update
# ./configure.in. The same for filename extension of F90 modules (kmo, M and
# mod are currently supported). F90_EARLY_* is to allow unconditional
# USE UNIX_F90. Some compilers by default know about IARGC, GETARG, etc...,
# some other do not and require USE UNIX_F90. F90_INT8_* contains list of
# files which require INTEGER*8 support (I know that this is unportable,
# maybe later I will find better solution). TO_INSTALL_F90MOD - accomodates
# for SGI F90 compiler which produces module files always in uppercase
# (filename not contents). Who has invented this ? Having this try to build
# automatically dependency tree ...
# In general - if possible DO NOT USE F90 AT ALL, pleeeease !!!!!!!!!!!!!!!

F90 			= f90 -static-flib -Am
F90FLAGS		= -g
LIB_F90_EARLY_SOURCES	= ${_LIB_F90_EARLY_SOURCES}
LIB_F90_PTR64_SOURCES	= 
LIB_F90_PTR32_SOURCES	= ${_LIB_F90_PTR32_SOURCES}
LIB_F90_INT8_SOURCES	= ${_LIB_F90_INT8_SOURCES}
LIB_F90_SOURCES		= ${_LIB_F90_SOURCES}
LIB_F90_EARLY_OBJECTS	= ${_LIB_F90_EARLY_OBJECTS}
LIB_F90_PTR64_OBJECTS	= 
LIB_F90_PTR32_OBJECTS	= ${_LIB_F90_PTR32_OBJECTS}
LIB_F90_INT8_OBJECTS	= ${_LIB_F90_INT8_OBJECTS}
LIB_F90_OBJECTS		= ${_LIB_F90_OBJECTS}
F90_TESTCOMMANDS	= testf90commands
F90MOD_PREFIX		= I
F90MOD_EXT		= mod
TO_INSTALL_F90MOD	= ${TO_INSTALL_F90MOD_NORMAL}
ALL_F90_CFLAGS		= ${F90FLAGS} ${TOTAL_F90MOD_DIR}
ALL_F90_LDFLAGS		= ${X_LIBRARIES} ${TOTAL_LIB_DIR}
ALL_F90MOD_EXT		= *.kmo *.M *.mod *.m *.vo *.inc

.SUFFIXES:
.SUFFIXES:	.f90 .cxx .cpp .cc $(SUFFIXES)

.f90.o:
	$(F90) -c $(ALL_F90_CFLAGS) $< -o $(<D)/$(@F)

# =========== INCLUDE makeisdc1 FOR ACTUAL TARGET DEFINITIONS  =============

include makeisdc1

# ======================= TARGET 'all_late_bind' ===========================
#
# this is ghost of 'all' target. It has to be after user defined targets.
# However default target (all) has to be before any other target, thus 
# all merely references this target.

all_late_bind_int:		check_gnu_make ${ALL_TARGETS} ${ALL_F90_TARGETS}

all_late_bind:	check_gnu_make
		@if test -f make_failed.log; then ${RM} -f make_failed.log; fi;
		@if test -f make_ok.log; then ${RM} -f make_ok.log; fi;
		( $(MAKE) all_late_bind_int && touch make_ok.log ) 2>&1 | ${TEE} make_failed.log ;
		@if test -f make_ok.log; then \
		  ${RM} -f make_ok.log; \
		  ${MV} make_failed.log make_ok.log; \
		else \
		  exit 1; \
		fi;
		@set -e; \
		for i in $(SUBDIRS) .end_of_list; do \
		 ( k=no; \
		   for j in $(EXTERNAL_PACKAGES) .end_of_list; do if test $$j = $$i ; then k=yes; fi; done; \
		   if test "x$$i" != "x${ISDCROOTDIR_TO_SKIP}" ; then \
		     if test "$$k" = "yes" ; then \
		       if test -d $$i; then cd $$i; $(MAKE) ; fi ; \
		     else \
		       if test -f $$i/Makefile; then \
		         cd $$i; $(MAKE) $@; \
		       else \
		         if test "$$i" != ".end_of_list"; then \
			   echo ""; echo "Cannot find $$i/Makefile. Rerun ac_stuff/configure to rebuild Makefiles structure !!!"; \
			   echo "This message appears if you have previously run make targz or make tarclean"; echo "" ; \
			   exit 1; \
		         fi; \
		       fi; \
		     fi; \
		   fi; \
		 ) || exit 1; done ;

# =============== DEFINITIONS FOR make install/install_global ==============
#
# Add any additional C and/or F90 executables to install to TO_INSTALL_BIN
# and TO_INSTALL_F90, parameter files for those executables (if any) in
# TO_INSTALL_PAR and TO_INSTALL_F90_PAR respectively, C/C++ header files and
# libraries in TO_INSTALL_INC and TO_INSTALL_LIB respectively. Last but not
# least, put in TO_INSTALL_F90MOD_NORMAL names of F90 module files to be
# installed in _lowercase_ and in TO_INSTALL_F90MOD_UPPER put those names
# in _UPPERCASE_. Please use ${F90MOD_EXT} as an extension of those files.
# All of this is due to some peculiarities of F90 compilers.

TO_INSTALL_F90MOD_INT8_NORMAL += ${_TO_INSTALL_F90MOD_INT8_NORMAL}
TO_INSTALL_F90MOD_INT8_NORMAL += 
TO_INSTALL_F90MOD_INT8_UPPER += ${_TO_INSTALL_F90MOD_INT8_UPPER}
TO_INSTALL_F90MOD_NORMAL += ${TO_INSTALL_F90MOD_INT8_NORMAL}
TO_INSTALL_F90MOD_UPPER += ${TO_INSTALL_F90MOD_INT8_UPPER}
TO_INSTALL_F90MOD_NORMAL += ${_TO_INSTALL_F90MOD_EARLY_NORMAL}
TO_INSTALL_F90MOD_NORMAL += 
TO_INSTALL_F90MOD_UPPER += ${_TO_INSTALL_F90MOD_EARLY_UPPER} 

# Where to install files locally. Some people may want to set
# LOCAL_INSTALL_ANCHOR to '..' to get semi-global install (a sort of).
# Default value (./installed_locally) is set by configure, but can be
# overwritten by settting LOCAL_INSTALL_ANCHOR env. variable before running
# configure.

LOCAL_INSTALL_ANCHOR	= ./installed_locally

LOCAL_INSTALL_BIN_DIR	= ${LOCAL_INSTALL_ANCHOR}/bin
LOCAL_INSTALL_F90MOD_DIR = ${LOCAL_INSTALL_ANCHOR}/f90mod
LOCAL_INSTALL_INC_DIR	= ${LOCAL_INSTALL_ANCHOR}/include
LOCAL_INSTALL_LIB_DIR	= ${LOCAL_INSTALL_ANCHOR}/lib
LOCAL_INSTALL_HELP_DIR	= ${LOCAL_INSTALL_ANCHOR}/help
LOCAL_INSTALL_PFILES_DIR = ${LOCAL_INSTALL_ANCHOR}/pfiles
LOCAL_INSTALL_SCRIPTS_DIR = ${LOCAL_INSTALL_ANCHOR}/share/default_scripts
LOCAL_INSTALL_SHARE_DIR	= ${LOCAL_INSTALL_ANCHOR}/share
LOCAL_INSTALL_TEMPLATES_DIR = ${LOCAL_INSTALL_ANCHOR}/templates

# This is only for global installation. Make sure that you have
# permission to write to those directories !

GLOBAL_INSTALL_BIN_DIR	= ${ISDC_ENV}/bin
GLOBAL_INSTALL_F90MOD_DIR = ${ISDC_ENV}/f90mod
GLOBAL_INSTALL_INC_DIR	= ${ISDC_ENV}/include
GLOBAL_INSTALL_LIB_DIR	= ${ISDC_ENV}/lib
GLOBAL_INSTALL_HELP_DIR	= ${ISDC_ENV}/help
GLOBAL_INSTALL_PFILES_DIR = ${ISDC_ENV}/pfiles
GLOBAL_INSTALL_SCRIPTS_DIR = ${ISDC_ENV}/share/default_scripts
GLOBAL_INSTALL_SHARE_DIR = ${ISDC_ENV}/share
GLOBAL_INSTALL_TEMPLATES_DIR = ${ISDC_ENV}/templates

# ============== STANDARD TARGETS FOR INSTALLATION/CLEANUP =================
#
# Please, do not change _ANYTHING_ in this section. Targets in this section
# are common to all Makefile(.in)s and contain only definitions of clean
# and install specific targets. Complication due to requirement to support
# systems without F90 compiler.
#
#     -  <<-- means ignore error return code from command

.PHONY:	all all_late_bind check_gnu_make check_isdc_dev_allow clean distclean global_install install subdirs_warning tarclean_quiet tarclean targz targz_internal testcommands testf90commands test world

check_gnu_make:
		@if [ "x${MAKELEVEL}" = "x" ]; then \
		  echo "" ; \
		  echo "                     -------- ERROR --------" ; \
		  echo "" ; \
		  echo "  You are not using GNU make. This Makefile requires GNU make program." ; \
		  echo "  If on SUN/Solaris, then you are most likely using /usr/ccs/bin/make." ; \
		  echo "  Please, modify your path to include directory with GNU make before" ; \
		  echo "  /usr/ccs/bin directory and/or ask local computer guru to install GNU make." ; \
		  echo "  Aborting ..." ; \
		  echo "" ; \
		  echo "" ; \
		  exit 1; \
		fi;

check_isdc_dev_allow:
		@if test "x${ISDC_ENV}" = "x/isdc/dev" -a "x${ISDC_DEV_ALLOW_GLOBAL_INSTALL}" != "xyes" ; then \
		  echo "" ; \
		  echo "                     -------- ERROR --------" ; \
		  echo "" ; \
		  echo "  You are trying to global_install software in /isdc/dev directory. Previous" ; \
		  echo "  version of makefiles used to unconditionally install software in that" ; \
		  echo "  directory, which many people found too dangerous (too easy to overwrite" ; \
		  echo "  files). So, now in order to be able to global_install in /isdc/dev it is" ; \
		  echo "  necessary to set environment variable :" ; \
		  echo "" ; \
		  echo "      setenv ISDC_DEV_ALLOW_GLOBAL_INSTALL yes" ; \
		  echo "" ; \
		  echo "  before running make global_install. You do not have to rerun configure, just" ; \
		  echo "  set this environment variable, then rerun make global_install." ; \
		  echo "  Aborting ..." ; \
		  echo "" ; \
		  echo "" ; \
		  exit 1; \
		fi;

clean:
	-	${RM} -f core ${CLEAN_TARGETS} ${CLEAN_F90_TARGETS} *.o *~ ${LIB_FULL_NAME} *.out work.pc work.pcl ${ALL_F90MOD_EXT} ${INSTALL}
	-	${RM} -rf SunWS_cache ac_tmp_dir unit*test/out/* ${RECURSIVE_CLEAN_TARGETS}
		@set -e; \
		for i in $(SUBDIRS) .end_of_list; do \
		 ( k=no; \
		   for j in $(EXTERNAL_PACKAGES) .end_of_list; do if test $$j = $$i ; then k=yes; fi; done; \
		   if test "x$$i" != "x${ISDCROOTDIR_TO_SKIP}" ; then \
		     if test "$$k" = "yes" ; then \
		       if test -d $$i; then cd $$i; $(MAKE) $@; fi ; \
		     else \
		       if test -f $$i/Makefile; then \
		         cd $$i; $(MAKE) $@; \
		       else \
		         if test "$$i" != ".end_of_list"; then \
			   echo ""; echo "Cannot find $$i/Makefile. Rerun ac_stuff/configure to rebuild Makefiles structure !!!"; \
			   echo "This message appears if you have previously run make targz or make tarclean"; echo "" ; \
			   exit 1; \
		         fi; \
		       fi; \
		     fi; \
		   fi; \
		 ) || exit 1; done ;

distclean:	clean
	-	${RM} -f make_ok.log make_failed.log confdefs.h config.cache config.status *.log *LOG* ${TAR_FILE_NAME}.tar ${TAR_FILE_NAME}.tar.gz
	-	${RM} -rf ${LOCAL_INSTALL_BIN_DIR} ${LOCAL_INSTALL_LIB_DIR} ${LOCAL_INSTALL_INC_DIR} ${LOCAL_INSTALL_HELP_DIR} ${LOCAL_INSTALL_F90MOD_DIR} ${LOCAL_INSTALL_TEMPLATES_DIR} ${LOCAL_INSTALL_SHARE_DIR} ${LOCAL_INSTALL_PFILES_DIR}
	-	if test -d "${LOCAL_INSTALL_ANCHOR}"; then ${RMDIR} ${LOCAL_INSTALL_ANCHOR} ; fi ;
		@set -e; \
		for i in $(SUBDIRS) .end_of_list; do \
		 ( k=no; \
		   for j in $(EXTERNAL_PACKAGES) .end_of_list; do if test $$j = $$i ; then k=yes; fi; done; \
		   if test "x$$i" != "x${ISDCROOTDIR_TO_SKIP}" ; then \
		     if test "$$k" = "yes" ; then \
		       if test -d $$i; then cd $$i; $(MAKE) $@; fi ; \
		     else \
		       if test -f $$i/Makefile; then \
		         cd $$i; $(MAKE) $@; \
		       else \
		         if test "$$i" != ".end_of_list"; then \
			   echo ""; echo "Cannot find $$i/Makefile. Rerun ac_stuff/configure to rebuild Makefiles structure !!!"; \
			   echo "This message appears if you have previously run make targz or make tarclean"; echo "" ; \
			   exit 1; \
		         fi; \
		       fi; \
		     fi; \
		   fi; \
		 ) || exit 1; done ;

unit_test_clean:
	-	${RM} -rf unit*test
		@set -e; \
		for i in $(SUBDIRS) .end_of_list; do \
		 ( k=no; \
		   for j in $(EXTERNAL_PACKAGES) .end_of_list; do if test $$j = $$i ; then k=yes; fi; done; \
		   if test "x$$i" != "x${ISDCROOTDIR_TO_SKIP}" ; then \
		     if test "$$k" = "yes" ; then \
		       cd . ; \
		     else \
		       if test -f $$i/Makefile; then \
		         cd $$i; $(MAKE) $@; \
		       else \
		         if test "$$i" != ".end_of_list"; then \
			   echo ""; echo "Cannot find $$i/Makefile. Rerun ac_stuff/configure to rebuild Makefiles structure !!!"; \
			   echo "This message appears if you have previously run make targz or make tarclean"; echo "" ; \
			   exit 1; \
		         fi; \
		       fi; \
		     fi; \
		   fi; \
		 ) || exit 1; done ;


f90_install:	${TO_INSTALL_F90MOD} ${TO_INSTALL_F90_LIB} ${TO_INSTALL_F90_BIN} ${ALL_F90_TARGETS}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_F90MOD_DIR} -c ${TO_INSTALL_F90MOD}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_LIB_DIR} -c ${TO_INSTALL_F90_LIB}
		for i in ${TO_INSTALL_F90_LIB} .end_of_list; do ( if test -r ${LOCAL_INSTALL_LIB_DIR}/$$i ; then ${RANLIB} ${LOCAL_INSTALL_LIB_DIR}/$$i ; fi ) || exit 1; done
		${INSTALL} -q -m 775 -d ${LOCAL_INSTALL_BIN_DIR} -c ${TO_INSTALL_F90_BIN}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_SHARE_DIR} -c ${TO_INSTALL_F90_SHARE}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_PFILES_DIR} -c ${TO_INSTALL_F90_PAR}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_HELP_DIR} -c ${TO_INSTALL_F90_HELP}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_SCRIPTS_DIR} -c ${TO_INSTALL_F90_SCRIPTS}
		for i in ${TO_INSTALL_F90_BIN} .end_of_list; do ( if test -r $$i.txt ; then ${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_HELP_DIR} -c $$i.txt ; fi ) || exit 1; done

global_f90_install:	${TO_INSTALL_F90MOD} ${TO_INSTALL_F90_LIB} ${TO_INSTALL_F90_BIN} ${ALL_F90_TARGETS}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_F90MOD_DIR} -c ${TO_INSTALL_F90MOD}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_LIB_DIR} -c ${TO_INSTALL_F90_LIB}
		for i in ${TO_INSTALL_F90_LIB} .end_of_list; do ( if test -r ${GLOBAL_INSTALL_LIB_DIR}/$$i ; then ${RANLIB} ${GLOBAL_INSTALL_LIB_DIR}/$$i ; fi ) || exit 1; done
		${INSTALL} -q -m 775 -d ${GLOBAL_INSTALL_BIN_DIR} -c ${TO_INSTALL_F90_BIN}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_SHARE_DIR} -c ${TO_INSTALL_F90_SHARE}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_PFILES_DIR} -c ${TO_INSTALL_F90_PAR}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_HELP_DIR} -c ${TO_INSTALL_F90_HELP}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_SCRIPTS_DIR} -c ${TO_INSTALL_F90_SCRIPTS}
		for i in ${TO_INSTALL_F90_BIN} .end_of_list; do ( if test -r $$i.txt ; then ${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_HELP_DIR} -c $$i.txt ; fi ) || exit 1; done

${INSTALL}:	${INSTALL}.c
		${CC} ${ALL_C_CFLAGS} -o ${INSTALL} ${INSTALL}.c ${ALL_C_LDFLAGS}

install_int:	check_gnu_make ${INSTALL} ${TO_INSTALL_INC} ${TO_INSTALL_LIB} ${TO_INSTALL_BIN} ${ALL_TARGETS}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_INC_DIR} -c ${TO_INSTALL_INC}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_LIB_DIR} -c ${TO_INSTALL_LIB}
		for i in ${TO_INSTALL_LIB} .end_of_list; do ( if test -r ${LOCAL_INSTALL_LIB_DIR}/$$i ; then ${RANLIB} ${LOCAL_INSTALL_LIB_DIR}/$$i ; fi ) || exit 1; done
		${INSTALL} -q -m 775 -d ${LOCAL_INSTALL_BIN_DIR} -c ${TO_INSTALL_BIN}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_SHARE_DIR} -c ${TO_INSTALL_SHARE}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_PFILES_DIR} -c ${TO_INSTALL_PAR}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_HELP_DIR} -c ${TO_INSTALL_HELP}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_TEMPLATES_DIR} -c ${TO_INSTALL_TEMPLATES}
		${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_SCRIPTS_DIR} -c ${TO_INSTALL_SCRIPTS}
		for i in ${TO_INSTALL_BIN} .end_of_list; do ( if test -r $$i.txt ; then ${INSTALL} -q -m 664 -d ${LOCAL_INSTALL_HELP_DIR} -c $$i.txt ; fi ) || exit 1; done
		${INSTALL} -q ${TO_INSTALL_EXTRA_LOCAL}
		${LOCAL_F90_INSTALL}

install:	check_gnu_make
		@if test -f make_failed.log; then ${RM} -f make_failed.log; fi;
		@if test -f make_ok.log; then ${RM} -f make_ok.log; fi;
		( $(MAKE) install_int && touch make_ok.log ) 2>&1 | ${TEE} make_failed.log ;
		@if test -f make_ok.log; then \
		  ${RM} -f make_ok.log; \
		  ${MV} make_failed.log make_ok.log; \
		else \
		  exit 1; \
		fi;
		@set -e; \
		for i in $(SUBDIRS) .end_of_list; do \
		 ( k=no; \
		   for j in $(EXTERNAL_PACKAGES) .end_of_list; do if test $$j = $$i ; then k=yes; fi; done; \
		   if test "x$$i" != "x${ISDCROOTDIR_TO_SKIP}" ; then \
		     if test "$$k" = "yes" ; then \
		       if test -d $$i; then cd $$i; $(MAKE) $@; fi ; \
		     else \
		       if test -f $$i/Makefile; then \
		         cd $$i; $(MAKE) $@; \
		       else \
		         if test "$$i" != ".end_of_list"; then \
			   echo ""; echo "Cannot find $$i/Makefile. Rerun ac_stuff/configure to rebuild Makefiles structure !!!"; \
			   echo "This message appears if you have previously run make targz or make tarclean"; echo "" ; \
			   exit 1; \
		         fi; \
		       fi; \
		     fi; \
		   fi; \
		 ) || exit 1; done ;

global_install_int:	check_gnu_make check_isdc_dev_allow ${INSTALL} ${TO_INSTALL_INC} ${TO_INSTALL_LIB} ${TO_INSTALL_BIN} ${ALL_TARGETS}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_INC_DIR} -c ${TO_INSTALL_INC}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_LIB_DIR} -c ${TO_INSTALL_LIB}
		for i in ${TO_INSTALL_LIB} .end_of_list; do ( if test -r ${GLOBAL_INSTALL_LIB_DIR}/$$i ; then ${RANLIB} ${GLOBAL_INSTALL_LIB_DIR}/$$i ; fi ) || exit 1; done
		${INSTALL} -q -m 775 -d ${GLOBAL_INSTALL_BIN_DIR} -c ${TO_INSTALL_BIN}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_SHARE_DIR} -c ${TO_INSTALL_SHARE}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_PFILES_DIR} -c ${TO_INSTALL_PAR}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_HELP_DIR} -c ${TO_INSTALL_HELP}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_TEMPLATES_DIR} -c ${TO_INSTALL_TEMPLATES}
		${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_SCRIPTS_DIR} -c ${TO_INSTALL_SCRIPTS}
		for i in ${TO_INSTALL_BIN} .end_of_list; do ( if test -r $$i.txt ; then ${INSTALL} -q -m 664 -d ${GLOBAL_INSTALL_HELP_DIR} -c $$i.txt ; fi ) || exit 1; done
		${INSTALL} -q ${TO_INSTALL_EXTRA_GLOBAL}
		${GLOBAL_F90_INSTALL}

global_install:		check_gnu_make
		@if test -f make_failed.log; then ${RM} -f make_failed.log; fi;
		@if test -f make_ok.log; then ${RM} -f make_ok.log; fi;
		( $(MAKE) global_install_int && touch make_ok.log ) 2>&1 | ${TEE} make_failed.log ;
		@if test -f make_ok.log; then \
		  ${RM} -f make_ok.log; \
		  ${MV} make_failed.log make_ok.log; \
		else \
		  exit 1; \
		fi;
		@set -e; \
		for i in $(SUBDIRS) .end_of_list; do \
		 ( k=no; \
		   for j in $(EXTERNAL_PACKAGES) .end_of_list; do if test $$j = $$i ; then k=yes; fi; done; \
		   if test "x$$i" != "x${ISDCROOTDIR_TO_SKIP}" ; then \
		     if test "$$k" = "yes" ; then \
		       if test -d $$i; then cd $$i; $(MAKE) $@; fi ; \
		     else \
		       if test -f $$i/Makefile; then \
		         cd $$i; $(MAKE) $@; \
		       else \
		         if test "$$i" != ".end_of_list"; then \
			   echo ""; echo "Cannot find $$i/Makefile. Rerun ac_stuff/configure to rebuild Makefiles structure !!!"; \
			   echo "This message appears if you have previously run make targz or make tarclean"; echo "" ; \
			   exit 1; \
		         fi; \
		       fi; \
		     fi; \
		   fi; \
		 ) || exit 1; done ;
		@if test "x${MAKELEVEL}" = "x0" ; then \
		  echo "" ; \
		  echo "" ; \
		  echo "  ************ everything compiled and installed successfully ***********" ; \
		  echo "" ; \
		  echo "  executables are installed in                     : ${ISDC_ENV}/bin" ; \
		  echo "  parameter files are installed in                 : ${ISDC_ENV}/pfiles" ; \
		  echo "  F90 modules are installed in                     : ${ISDC_ENV}/f90mod" ; \
		  echo "  documentation, help, .txt files are installed in : ${ISDC_ENV}/help" ; \
		  echo "  C/C++ include .h files are installed in          : ${ISDC_ENV}/include" ; \
		  echo "  .a, .so library files are installed in           : ${ISDC_ENV}/lib" ; \
		  echo "  script files are installed in                    : ${ISDC_ENV}/share/default_scripts" ; \
		  echo "  other shared files are installed in              : ${ISDC_ENV}/share" ; \
		  echo "  FITS file template files are installed in        : ${ISDC_ENV}/templates" ; \
		  echo "" ; \
		  echo "" ; \
		fi;

global_install_ignore_err:	check_gnu_make
		@if test -f make_failed.log; then ${RM} -f make_failed.log; fi;
		@if test -f make_ok.log; then ${RM} -f make_ok.log; fi;
	-	( $(MAKE) global_install_int && touch make_ok.log ) 2>&1 | ${TEE} make_failed.log ;
		@if test -f make_ok.log; then \
		  ${RM} -f make_ok.log; \
		  ${MV} make_failed.log make_ok.log; \
		fi;
		@set -e; \
		for i in $(SUBDIRS) .end_of_list; do \
		 ( k=no; \
		   for j in $(EXTERNAL_PACKAGES) .end_of_list; do if test $$j = $$i ; then k=yes; fi; done; \
		   if test "x$$i" != "x${ISDCROOTDIR_TO_SKIP}" ; then \
		     if test "$$k" = "yes" ; then \
		       if test -d $$i; then cd $$i; $(MAKE) global_install || echo "dummy_to_force_exit_code_0" >/dev/null; fi ; \
		     else \
		       if test -f $$i/Makefile; then \
		         cd $$i; $(MAKE) $@ || echo "dummy_to_force_exit_code_0" >/dev/null; \
		       else \
		         if test "$$i" != ".end_of_list"; then \
			   echo ""; echo "Cannot find $$i/Makefile. Rerun ac_stuff/configure to rebuild Makefiles structure !!!"; \
			   echo "This message appears if you have previously run make targz or make tarclean"; echo "" ; \
			   exit 1; \
		         fi; \
		       fi; \
		     fi; \
		   fi; \
		 ) || exit 1; done ;
		@if test "x${MAKELEVEL}" = "x0" ; then \
		  echo "" ; \
		  echo "" ; \
		  echo "  ************ make global_install_ignore_err done ***********" ; \
		  echo "" ; \
		  echo "  executables are installed in                     : ${ISDC_ENV}/bin" ; \
		  echo "  parameter files are installed in                 : ${ISDC_ENV}/pfiles" ; \
		  echo "  F90 modules are installed in                     : ${ISDC_ENV}/f90mod" ; \
		  echo "  documentation, help, .txt files are installed in : ${ISDC_ENV}/help" ; \
		  echo "  C/C++ include .h files are installed in          : ${ISDC_ENV}/include" ; \
		  echo "  .a, .so library files are installed in           : ${ISDC_ENV}/lib" ; \
		  echo "  script files are installed in                    : ${ISDC_ENV}/share/default_scripts" ; \
		  echo "  other shared files are installed in              : ${ISDC_ENV}/share" ; \
		  echo "  FITS file template files are installed in        : ${ISDC_ENV}/templates" ; \
		  echo "" ; \
		  echo "" ; \
		fi;

subdirs_warning:
		@if test "x${MAKELEVEL}" = "x0" -a "x${SUBDIRS}" != "x" ; then \
		  echo "" ; \
		  echo "" ; \
		  echo "  Any subdirectories specified in SUBDIRS had their Makefile(.in) files removed." ; \
		  echo "  Please rerun ac_stuff/configure script in order to rebuild Makefile(.in) files" ; \
		  echo "" ; \
		  echo "" ; \
		fi;

tarclean_quiet:	distclean
		@set -e; \
		for i in $(SUBDIRS) .end_of_list; do \
		 ( k=no; \
		   for j in $(EXTERNAL_PACKAGES) .end_of_list; do if test $$j = $$i ; then k=yes; fi; done; \
		   if test "x$$i" != "x${ISDCROOTDIR_TO_SKIP}" ; then \
		     if test "$$k" != "yes" ; then \
		       if test -f $$i/Makefile; then \
		         cd $$i; $(MAKE) $@; \
			 for j in README.make* config.* configure* isdc_install* cfortran* Makefile* makeisdc1 ; do if test -f $$j; then echo "removing `pwd`/$$j"; ${RM} -f $$j; fi; done; \
			 if test ! -f ac_stuff_keep_flag; then if test -d ac_stuff; then ${RM} -rf ac_stuff; fi; fi; \
			 for j in autoconf installed_locally; do if test -d $$j; then ${RM} -rf $$j; fi; done; \
		       fi; \
		     fi; \
		   fi; \
		 ) || exit 1; done ;

tarclean:	tarclean_quiet subdirs_warning

targz_internal:
		${TAR_EXE} cvf ${TAR_FILE_NAME}.tar ` for i in * ; do if test $$i != Makefile.in -a $$i != Makefile -a $$i != makeisdc1 -a \( $$i != ac_stuff -o -r ac_stuff_keep_flag \) -a \( $$i != CVS -o -r CVS_keep_flag \) ; then echo $$i ; fi ; done `
		${GZIP_EXE} ${TAR_FILE_NAME}.tar

targz:		tarclean_quiet targz_internal subdirs_warning

dist_targz:	unit_test_clean targz

test:		testcommands ${F90_TESTCOMMANDS}
		@set -e; \
		for i in $(SUBDIRS) .end_of_list; do \
		 ( k=no; \
		   for j in $(EXTERNAL_PACKAGES) .end_of_list; do if test $$j = $$i ; then k=yes; fi; done; \
		   if test "x$$i" != "x${ISDCROOTDIR_TO_SKIP}" ; then \
		     if test "$$k" != "yes" ; then \
		       if test -f $$i/Makefile; then \
		         cd $$i; $(MAKE) $@; \
		       else \
		         if test "$$i" != ".end_of_list"; then \
			   echo ""; echo "Cannot find $$i/Makefile. Rerun ac_stuff/configure to rebuild Makefiles structure !!!"; \
			   echo "This message appears if you have previously run make targz or make tarclean"; echo "" ; \
			   exit 1; \
		         fi; \
		       fi; \
		     fi; \
		   fi; \
		 ) || exit 1; done ;

world:
		echo "" ;
		echo "                     -------- ERROR --------" ;
		echo "" ;
		@echo "make world is not supported in this version of makefiles. "
		@echo "To build (or install) ISDC software package(s) type :"
		@echo " "
		@echo "    setenv ISDC_ENV /put/path/here"
		@echo "    \$\{ISDC_ENV\}/bin/ac_stuff/configure (or ac_stuff/configure if exists)"
		@echo "    make  (or make global_install)"
		@echo " "

# ======================== end of Makefile(.in) ============================
