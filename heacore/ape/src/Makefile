HD_COMPONENT_NAME	= heacore

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= ${PIL}

HD_LIBRARY_SRC_c	= ape_binary.c ape_error.c ape_io.c ape_list.c \
			  ape_msg.c ape_par.c ape_par_group.c ape_session.c ape_test.c \
			  ape_trad.c ape_util.c pil.c pil_error.c

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${READLINE}

HD_ADD_SHLIB_LIBS	= yes

HD_CFLAGS		= -I. -I../include ${HD_STD_CFLAGS} -DUSE_READLINE

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

include ${HD_STD_MAKEFILE}

run-test:
	${HD_MAKE} subdir HD_SUBDIR=test HD_TARGET=$@

