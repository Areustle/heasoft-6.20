HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_CTASK		= ximage

HD_LIBRARY_ROOT		= ximage${FT_VERS}

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_LIBRARY_SRC_c	= cmd/cmddef.c cmd/journal.c cmd/procfcmd.c \
			  cmd/tclcom.c cmd/tcllog.c cmd/tclparm.c cmd/tclvar.c \
			  cmd/ximunk.c cmd/xparse_tcl.c cmd/xsource.c \
			  cmd/ximdefault.c \
			  hdrmap/chheader.c hdrmap/mopproc.c \
			  hdrmap/remapcore.c hdrmap/remapprep.c \
			  hdrmap/wcscmd.c hdrmap/wcsmanager.c \
			  implot/curvecmd.c implot/drawcmd.c implot/gridcmd.c \
			  implot/selectcmd.c implot/tclimplot.c \
			  lib/xcommon.c readwr/filefromlist.c readwr/astfits.c \
			  readwr/wr_fithdr.c region/region.c region/ximreg.c \
			  stat/suzaku_psf.c \
			  stat/swift_psf.c stat/swift_vign.c \
			  stat/baylike.c stat/baypri.c \
			  stat/qsimp.c stat/rtbis.c stat/rtsafe.c stat/zbrac.c

HD_LIBRARY_SRC_f	= cmd/chatter.f cmd/gpars.f cmd/ximcmd.f cmd/ximinit.f \
			  extract/exask.f \
			  extract/exdeltmp.f \
			  extract/exmjd.f \
			  extract/exmovleft.f \
			  extract/exrdwin.f \
			  extract/exremcom.f \
			  extract/extract.f \
			  extract/getevents.f \
			  extract/getgti.f \
			  extract/select_inten.f \
			  extract/select_phase.f \
			  extract/select_time.f \
			  extract/sort_time.f \
			  extract/wrcolor.f \
			  extract/write_qdp.f \
			  grid/abs_to_rel.f \
			  grid/arccos.f \
			  grid/arcsin.f \
			  grid/cubspl4.f \
			  grid/curfun.f \
			  grid/curvil.f \
			  grid/difsys4.f \
			  grid/draw.f \
			  grid/equ_gal.f \
			  grid/galac.f \
			  grid/gal_equ.f \
			  grid/gdraw.f \
			  grid/grid.f \
			  grid/gridlab.f \
			  grid/gridmp.f \
			  grid/gridrnd.f \
			  grid/gridx.f \
			  grid/lever_x.f \
			  grid/meridien.f \
			  grid/parallele.f \
			  grid/plcurv.f \
			  grid/rad2det.f \
			  grid/relocate.f \
			  grid/rel_to_abs.f \
			  grid/setpro.f \
			  grid/setrem.f \
			  hdrinfo/calccor.f \
			  hdrinfo/ccamoeba.f \
			  hdrinfo/ccminfun.f \
			  hdrinfo/finding_chart.f \
			  hdrinfo/ghdwcs.f \
			  hdrinfo/header.f \
			  hdrinfo/iminfo.f \
			  hdrinfo/offset.f \
			  hdrinfo/pix2ra.f \
			  hdrinfo/show.f \
			  hdrinfo/skyview.f \
			  hdrmap/coord.f \
			  hdrmap/counts.f \
			  hdrmap/cphead.f \
			  hdrmap/cprrmap.f \
			  hdrmap/crop.f \
			  hdrmap/dalloc.f \
			  hdrmap/expiremap.f \
			  hdrmap/flip90work.f \
			  hdrmap/flip.f \
			  hdrmap/flipwork.f \
			  hdrmap/getexmap.f \
			  hdrmap/gettmpmap.f \
			  hdrmap/freetmpmap.f \
			  hdrmap/gheadd.f \
			  hdrmap/gheadi.f \
			  hdrmap/gheads.f \
			  hdrmap/gheadtype.f \
			  hdrmap/i2alloc.f \
			  hdrmap/inithdrs.f \
			  hdrmap/initmaps.f \
			  hdrmap/iscpmapid.f \
			  hdrmap/iseqmapid.f \
			  hdrmap/isloaded.f \
			  hdrmap/lsmapid.f \
			  hdrmap/mapalloc.f \
			  hdrmap/maparith.f \
			  hdrmap/mapcopy.f \
			  hdrmap/mapexpr.f \
			  hdrmap/map.f \
			  hdrmap/mapfree.f \
			  hdrmap/mapidx.f \
			  hdrmap/mapint.f \
			  hdrmap/mapname.f \
			  hdrmap/maprcmd.f \
			  hdrmap/maptrfm.f \
			  hdrmap/mapvalget.f \
			  hdrmap/mapvalset.f \
			  hdrmap/marith.f \
			  hdrmap/marithwork.f \
			  hdrmap/matchhdr.f \
			  hdrmap/moper.f \
			  hdrmap/mopwork.f \
			  hdrmap/pmapfree.f \
			  hdrmap/polybox.f \
			  hdrmap/polygon.f \
			  hdrmap/polylbl.f \
			  hdrmap/prbox.f \
			  hdrmap/prhead.f \
			  hdrmap/ralloc.f \
			  hdrmap/rdtxtcol.f \
			  hdrmap/rebin.f \
			  hdrmap/rebinwork.f \
			  hdrmap/remap.f \
			  hdrmap/rescale.f \
			  hdrmap/rescalework.f \
			  hdrmap/resize.f \
			  hdrmap/resizework.f \
			  hdrmap/rotate.f \
			  hdrmap/rotatework.f \
			  hdrmap/setcurmap.f \
			  hdrmap/setdismap.f \
			  hdrmap/slice.f \
			  hdrmap/slicework.f \
			  hdrmap/workalloc.f \
			  implot/calchist.f \
			  implot/calclevs.f \
			  implot/cct.f \
			  implot/closepg.f \
			  implot/colors.f \
			  implot/conxbeg.f \
			  implot/conxbuf.f \
			  implot/conxend.f \
			  implot/copy_coltab.f \
			  implot/cpd.f \
			  implot/crosssect.f \
			  implot/info_coltab.f \
			  implot/istk.f \
			  implot/label.f \
			  implot/lblpt.f \
			  implot/levels.f \
			  implot/list_col_tab.f \
			  implot/mod_coltab.f \
			  implot/pcontour.f \
			  implot/pg_start.f \
			  implot/pimage.f \
			  implot/plot_frame.f \
			  implot/plot_start.f \
			  implot/read_coltab.f \
			  implot/read_levels.f \
			  implot/refresh_coltab.f \
			  implot/scale.f \
			  implot/set_defcols.f \
			  implot/set_vpcbnds.f \
			  implot/set_vpvals.f \
			  implot/smc.f \
			  implot/spectro_frame.f \
			  implot/surface.f \
			  implot/timestamp.f \
			  implot/title.f \
			  implot/viewport.f \
			  implot/vplabel.f \
			  implot/wrcoltab.f \
			  lib/calimgpix.f \
			  lib/cnv_radec.f \
			  lib/dec_to_exp.f \
			  lib/detidx.f \
			  lib/disro.f \
			  lib/drwcir.f \
			  lib/drwelp.f \
			  lib/edetidx.f \
			  lib/findchr.f \
			  lib/get_color.f \
			  lib/get_itel.f \
			  lib/get_optax.f \
			  lib/get_refram.f \
			  lib/getseed.f \
			  lib/get_skyhea.f \
			  lib/get_telstr.f \
			  lib/help.f \
			  lib/imgpix.f \
			  lib/inxypix.f \
			  lib/line_pgstate.f \
			  lib/lkupcal.f \
			  lib/lkupfile.f \
			  lib/matchfunc.f \
			  lib/matchkey.f \
			  lib/matchopts.f \
			  lib/null.f \
			  lib/parseilst.f \
			  lib/prstart.f \
			  lib/qustrip.f \
			  lib/rdtoxy.f \
			  lib/stdqsort.f \
			  lib/streval.f \
			  lib/strjust.f \
			  lib/sym_pgstate.f \
			  lib/text_pgstate.f \
			  lib/write_radec.f \
			  lib/xdstr.f \
			  lib/xidate.f \
			  lib/ximprec.f \
			  lib/xim_startup.f \
			  lib/xistr.f \
			  lib/xytrd.f \
			  mdb/addmdb.f \
			  mdb/chmdb.f \
			  mdb/cpmdb.f \
			  mdb/gmdbd.f \
			  mdb/gmdbi.f \
			  mdb/gmdbs.f \
			  mdb/gmdbtype.f \
			  mdb/initmdb.f \
			  mdb/matchmdb.f \
			  mdb/rdmdb.f \
			  mdb/setmdb.f \
			  mdb/wrmdb.f \
			  readwr/evaltmpl.f \
			  readwr/exoifil.f \
			  readwr/fcidxd.f \
			  readwr/getcol.f \
			  readwr/getexohdr.f \
			  readwr/go_evttbl.f \
			  readwr/go_imgext.f \
			  readwr/gt_fitstype.f \
			  readwr/gt_gtis.f \
			  readwr/gtialloc.f \
			  readwr/gtilib.f \
			  readwr/gt_imgsz.f \
			  readwr/gt_missinfo.f \
			  readwr/hdrtomem.f \
			  readwr/matchext.f \
			  readwr/rd_ckeys.f \
			  readwr/rd_detcoor.f \
			  readwr/rd_detkeys.f \
			  readwr/rd_elimits.f \
			  readwr/rd_eqxkey.f \
			  readwr/rd_evt_xim.f \
			  readwr/rd_exohdr.f \
			  readwr/rd_exoimg.f \
			  readwr/rd_exosz.f \
			  readwr/rd_expkeys.f \
			  readwr/rd_gti.f \
			  readwr/rd_imgsz.f \
			  readwr/rd_img_xim.f \
			  readwr/rd_objkey.f \
			  readwr/rd_obskeys.f \
			  readwr/read_image.f \
			  readwr/sethdr.f \
			  readwr/wr_asc.f \
			  readwr/wr_aschdr.f \
			  readwr/wr_exo.f \
			  readwr/wr_fits.f \
			  readwr/write_image.f \
			  readwr/xrstrext.f \
			  region/bgcntreg.f \
			  region/boxreg.f \
			  region/circlereg.f \
			  region/cntreg.f \
			  region/ezregplot.f \
			  region/getregions.f \
			  stat/asca_eef.f \
			  stat/asca_vign.f \
			  stat/astroe_psf.f \
			  stat/astroe_vign.f \
			  stat/backgd_in.f \
			  stat/backgd_out.f \
			  stat/background.f \
			  stat/backvals.f \
			  stat/barycenter.f \
			  stat/bglevels.f \
			  stat/binchan.f \
			  stat/bxhtest.f \
			  stat/bxhwork.f \
			  stat/centroid.f \
			  stat/chandra_psf.f \
			  stat/chandra_vign.f \
			  stat/cnvl.f \
			  stat/contig_srch.f \
			  stat/detect_bgr.f \
			  stat/detect_out.f \
			  stat/detect_search.f \
			  stat/do_back.f \
			  stat/dps_cntrd.f \
			  stat/dynm_thresh.f \
			  stat/einstein_vign.f \
			  stat/error_radius.f \
			  stat/excess.f \
			  stat/excess_in.f \
			  stat/excess_out.f \
			  stat/excess_search.f \
			  stat/fermi.f \
			  stat/final_checks.f \
			  stat/flatback.f \
			  stat/flim.f \
			  stat/funct.f \
			  stat/getbgmap.f \
			  stat/get_exposure.f \
			  stat/get_expowgt.f \
			  stat/get_vign.f \
			  stat/histomax.f \
			  stat/hripsf.f \
			  stat/initstat.f \
			  stat/intensity.f \
			  stat/iposs.f \
			  stat/jetx_psf.f \
			  stat/jetx_vign.f \
			  stat/local_bg.f \
			  stat/loc_excess.f \
			  stat/movxy.f \
			  stat/nobox.f \
			  stat/npsf.f \
			  stat/offbary.f \
			  stat/par.f \
			  stat/parin.f \
			  stat/pepsi.f \
			  stat/poiss_uplim.f \
			  stat/poiss_uplim2.f \
			  stat/prback.f \
			  stat/psf.f \
			  stat/psfoff.f \
			  stat/qsort_detect.f \
			  stat/raw_counts.f \
			  stat/rd_fitsmap.f \
			  stat/rdpsf.f \
			  stat/rebinmax.f \
			  stat/remove_sources.f \
			  stat/resolution.f \
			  stat/rosat_vign.f \
			  stat/sax_psf.f \
			  stat/sax_vign.f \
			  stat/search.f \
			  stat/search_in.f \
			  stat/search_out.f \
			  stat/set_threshold.f \
			  stat/smooth.f \
			  stat/smoothwork.f \
			  stat/smth.f \
			  stat/sosta_back.f \
			  stat/sosta.f \
			  stat/sosta_misc.f \
			  stat/sosta_out.f \
			  stat/sosta_source.f \
			  stat/trybary.f \
			  stat/ullim.f \
			  stat/uplimit.f \
			  stat/vdot.f \
			  stat/vign_awaki.f \
			  stat/vignetting.f \
			  stat/wavelet.f \
			  stat/wrback.f \
			  stat/wrdetect.f \
			  stat/xmm_psf.f \
			  stat/xmm_vign.f \
			  stat/xpolog.f \
			  stat/zfun.f \
			  txtio/txinfo.f \
			  txtio/txinitc.f \
			  txtio/txinit.f \
			  txtio/txrdcol.f \
			  txtio/txrdfile.f \
			  txtio/txrdicol.f \
			  txtio/txrdikey.f \
			  txtio/txrdkey.f \
			  txtio/txrdskey.f \
			  txtio/txrdval.f \
			  txtio/txwrcol.f \
			  txtio/txwrcom.f \
			  txtio/txwrfile.f \
			  txtio/txwrhdr.f \
			  txtio/txwricol.f \
			  txtio/txwrikey.f \
			  txtio/txwrkey.f \
			  txtio/txwrskey.f

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_CFLAGS		= ${HD_STD_CFLAGS} ${XINCLUDES} ${TCL_DEFS}

HD_FFLAGS		= ${HD_STD_FFLAGS} -I./include \
			  -I./extract -I./grid -I./hdrmap -I./implot \
			  -I./lib -I./mdb -I./stat -I./txtio

HD_CTASK_SRC_c		= ${HD_CTASK}.c

HD_CLIBS		= ${HD_LFLAGS} -lximage${FT_VERS} -l${PIL} -l${TCL} \
			  ${AST_LIBS} -lastadd${FT_VERS} -lpgadd${FT_VERS} \
			  -lplt -l${XANLIB} -l${PIL} -l${TCLREADLINE} \
			  -l${READLINE} -l${TK} -l${CFITSIO} -l${HEAUTILS} \
			  -lc${PGPLOT} -ltk${PGPLOT} ${LINK_PGPLOT_TCL} \
                          ${XLIBS} ${SYSLIBS} ${F77LIBS4C} 

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o} ${HD_LIBRARY_SRC_f:.f=.o}

include ${HD_STD_MAKEFILE}
