HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= xself${FT_VERS}

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_LIBRARY_SRC_c	= xsel_ape_init.c xsel_unix_c.c

HD_LIBRARY_SRC_f	= xsel_bin.f xsel_clear.f xsel_common_init.f \
			  xsel_extract.f xsel_filter.f \
			  xsel_fits.f xsel_ftools.f xsel_mdb.f \
			  xsel_mission.f xsel_proc_command.f xsel_read.f \
			  xsel_save.f \
			  xsel_select.f xsel_unix.f xsel_unx.f xsel_utils.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS} -I../include

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

include ${HD_STD_MAKEFILE}
