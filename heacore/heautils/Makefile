HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT		= ${HEAUTILS}

HD_LIBRARY_SRC_c	= headas_copykeys.c headas_error.c headas_error_init.c \
			  headas_error_manager.c headas_error_map.c \
			  headas_history.c headas_toolname.c headas_utils.c \
			  headas_parstamp.c headas_gti.c \
			  HDgtcalf.c HDgtcalf_internal.c HDgtcalf_f77_wrap.c \
			  headas_file_check.c headas_rand.c mt.c headas_lock.c \
			  headas_polyfit.c headas_smooth.c headas_sort.c \
			  headas_svdfit.c

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${PIL} -l${CFITSIO} -l${READLINE} \
			  -l${HEAIO} ${SYSLIBS}

HD_ADD_SHLIB_LIBS	= yes

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_INSTALL_HEADERS	= headas_copykeys.h headas_error.h headas_utils.h \
			  headas_gti.h hdcal.h headas_rand.h headas_lock.h \
			  hd_math_compat.h headas_polyfit.h

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HELP		= HDgtcalf.html headas_gti.html

HD_CLIBS		= ${HD_LFLAGS} -l${HEAUTILS} -l${PIL} -l${CFITSIO} \
			  -l${READLINE} ${SYSLIBS}

HD_TEST_SUBDIRS		= ut

HD_TEST_CTASK		= HDgtcalf_test

HD_TEST_CTASK_SRC_c	= HDgtcalf_test.c

include ${HD_STD_MAKEFILE}
