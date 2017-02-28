HD_COMPONENT_NAME	= suzaku

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT         = astetool

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_LIBRARY_SRC_c	= \
aste_att.c aste_coord_ch2mm.c aste_coord_core.c \
aste_coord_hxd.c aste_coord_teldef.c \
aste_coord_xis.c aste_coord_xrs.c aste_coord_xrt.c \
aste_rand.c aste_time.c aste_time_tt.c attfile.c hrndm.c hrndm_dbl.c hrndmb.c \
hrndmrmf.c aste_hdouble.c aste_rpt.c aste_telescop.c aste_instrume.c

HD_SHLIB_LIBS		= ${HD_LFLAGS} \
-lcernlib-dummy -l${ATFUNCTIONS} -l${CFITSIO}

include ${HD_STD_MAKEFILE}
