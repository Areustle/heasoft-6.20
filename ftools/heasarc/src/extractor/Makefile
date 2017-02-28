HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= extractor

HD_CTASK_SRC_c		= hextractor.c clieee.c xclock1.c fregions.c

HD_CTASK_SRC_f		= bboxsz.f cnvpos.f cnvsiz.f evprse.f exdeltmp.f \
			  exnxev.f exnxtm.f extevh.f extgetev.f extractor.f \
			  extsev.f extwcs.f fixwmp.f gexpar.f grade.f \
			  rmvdky.f sort_time.f subkeys.f times.f \
			  tmftyp.f w_dskeys.f w_events.f w_fblc.f w_gti.f \
			  w_image.f w_pha.f w_qdp.f w_refkeys.f w_regions.f wstdky.f \
			  xclsfl.f xronout.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lxron${FT_VERS} -lplt ${LINK_PGPLOT} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= extractor.par

HD_INSTALL_HELP		= extractor.hlp extractor.txt

include ${HD_STD_MAKEFILE}
