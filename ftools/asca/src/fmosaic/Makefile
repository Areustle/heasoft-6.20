HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= fmosaic

HD_CTASK_SRC_c		= hfmosaic.c

HD_CTASK_SRC_f		= def_det_off.f def_image_wcs.f def_nevents.f \
			  fill_en.f fmosaic.f gchan_eb.f get_bcg_fn.f \
			  get_rig.f image2fits.f imgwcs2fits.f ini_events.f \
			  make_bkg.f make_image.f mosaic_read.f read_back.f \
			  read_emap.f read_events.f valid_det_pix.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} \
			  -lasca${FT_VERS} -l${ATFUNCTIONS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PERL_SCRIPTS	= fmosaic2img

HD_INSTALL_PFILES	= fmosaic.par

HD_INSTALL_HELP		= fmosaic2img.txt fmosaic.txt

include ${HD_STD_MAKEFILE}
