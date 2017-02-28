HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= like

HD_CTASK_SRC_c		= hlike.c jputenv_.c jsetenv_.c readinput.c

HD_CTASK_SRC_f		= allfit.f amoebaj.f auto_out.f basis_fu.f \
			  biastest.f catproca.f catproc.f cbgaus.f \
			  celgald.f celgal.f chood.f cnfcal.f cntsbias.f \
			  cnttest.f command.f cospsf.f ctlred.f datared.f \
			  dbrent.f derivati.f docadd.f egrpsf.f errmap.f \
			  error.f exp_anal.f fiterr.f fitred.f fitrit.f \
			  fknif.f func_lim.f funkj.f gasbad.f \
			  gasbias.f gascnts.f gastest.f get_flux.f \
			  getsrcnm.f golden.f gtcirc.f hdrred.f hood.f \
			  iauname.f info.f input_id.f intent.f invert.f \
			  jfind_il.f jfnderr1.f jfnderr2.f jfndposa.f \
			  jfndpos.f jgetsyst.f jlistpro.f jloc_pos.f \
			  jmapfine.f jopt_pos.f jpsfinfo.f jreport.f \
			  jsort.f jwrmap.f l_check.f lhood.f like.f \
			  liktot.f limit.f listproc.f lpi_zt.f lubksb.f \
			  ludcmp.f mapadd.f mapcnv.f mapcor.f mapcpy.f \
			  mapcpyro.f mapdiv.f mapdivro.f maperr.f mapfine.f \
			  maphigh.f mapinc.f mapixl.f mapmax.f mapmlt.f \
			  mapmult.f mapred.f maprit.f mapritro.f maprst.f \
			  mapsrc.f mapsrcj.f mapsub.f mapsum.f mapval.f \
			  multproc.f net_info.f next_src.f omap.f \
			  optimize.f optimum.f pixel_se.f profile.f \
			  psfadd.f psfadj.f psfbld.f psfremov.f psf_rep.f \
			  psfrepla.f psf_rosat.f psf_scal.f psfval.f \
			  psmcor.f psmget.f psmmat.f readcat.f realcomp.f \
			  removenull.f residue.f residuej.f roiadj.f \
			  roiset.f rosatpsf.f saspsf.f set_amat.f \
			  set_debu.f setup.f srcounts.f srctest.f svbksb.f \
			  svdcmp.f svdfit.f swapchar.f swapreal.f totmap.f \
			  to_upper.f valmap.f
# unused: fitserro.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} ${LINK_PGPLOT} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= like.par

HD_INSTALL_HELP		= like.hlp like.txt

include ${HD_STD_MAKEFILE}
