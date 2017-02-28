HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= coordfits

HD_LIBRARY_SRC_c	= attconvert.c attfile.c att_iterator.c attout.c \
			  genattfile.c genorbfile.c \
			  teldef.c teldef_coord.c teldef_xform.c \
			  teldef_nonlinear.c align.c pointing.c comboxform.c \
			  coordwcs.c \
			  teldef2.c tr_basic.c tr_skyatt.c tr_multiseg.c \
			  tr_rawtodet.c tr_telescope.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${HEAUTILS} -l${PIL} -lcoord \
			  -l${CFITSIO} -l${READLINE} -l${HEAIO}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= attconvert.h attfile.h att_iterator.h attout.h \
			  genattfile.h genorbfile.h \
			  coordfits.h teldef.h align.h pointing.h comboxform.h \
			  coordwcs.h \
			  teldef2.h tr_basic.h tr_rawtodet.h tr_skyatt.h \
			  tr_multiseg.h tr_telescope.h coordfits2.h

include ${HD_STD_MAKEFILE}
