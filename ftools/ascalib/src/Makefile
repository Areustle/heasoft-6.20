HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT		= asca${FT_VERS}

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_LIBRARY_SRC_c	= general/ascatime.c general/fftcnv.c general/giscnv.c \
			  general/gisres.c general/gisresf.c general/nrutil.c \
			  rddlib/functions.c rddlib/main.c rddlib/numerical.c \
			  rddlib/rdd.c rddlib/rddparam.c

HD_LIBRARY_SRC_f	= ascalin/aberration.f ascalin/adjust_sis_gain.f \
			  ascalin/ascatout.f ascalin/asp_record.f \
			  ascalin/catnum.f ascalin/cosine2euler.f \
			  ascalin/dir_cos.f ascalin/dir_cos_inv.f \
			  ascalin/dot_prod.f ascalin/euler2xrt.f \
			  ascalin/four_chip_image.f ascalin/get_gain_norm.f \
			  ascalin/get_gis_gain.f ascalin/get_sis_gain.f \
			  ascalin/gis_ele2det.f ascalin/gis_pi.f \
			  ascalin/gis_record.f ascalin/int2str.f \
			  ascalin/make_att_name.f ascalin/matrix_mult3.f \
			  ascalin/matrix_proj3.f ascalin/modify_f_kw.f \
			  ascalin/modify_j_kw.f ascalin/modifyn_f_kw.f \
			  ascalin/modifyn_j_kw.f ascalin/nominal_aspect.f \
			  ascalin/open_asca_data.f ascalin/open_asp_file.f \
			  ascalin/parse_giscal_header.f \
			  ascalin/parse_sis_cal_hdr.f ascalin/put_gis_event.f \
			  ascalin/put_sis_event.f ascalin/random.f \
			  ascalin/rd2sat.f ascalin/read_asca_data.f \
			  ascalin/read_binext_image.f ascalin/read_gain_hist.f \
			  ascalin/read_gis_cal.f ascalin/read_sis_cal.f \
			  ascalin/read_sis_gain_file.f ascalin/roll_angle.f \
			  ascalin/rt2rti.f ascalin/sat2rd.f \
			  ascalin/sis_ele2det.f ascalin/sis_pi.f \
			  ascalin/sis_record.f ascalin/sun_sat_ang.f \
			  ascalin/sun_scr_ang.f ascalin/update_coordpro.f \
			  ascalin/update_gis_axis_key.f \
			  ascalin/update_pi_axis_key.f \
			  ascalin/update_sis_axis_key.f \
			  ascalin/update_sky_keywords.f \
			  general/gtgmap.f general/timesubs.f general/trns.f \
			  xrt/eef93a.f xrt/xrtea95b.f xrt/xrtpsf95b.f

# unused: xrt/trns.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS} -I./ascalin -I./xrt

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o} ${HD_LIBRARY_SRC_f:.f=.o}

include ${HD_STD_MAKEFILE}
