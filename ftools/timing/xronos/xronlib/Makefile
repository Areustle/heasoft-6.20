HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= xron${FT_VERS}

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_LIBRARY_SRC_c	= math/xrparseday.c

HD_LIBRARY_SRC_f	= fits/isitfits.f fits/xrapplopt.f fits/xrapptun.f \
			  fits/xrchkgti.f fits/xrcolopt.f fits/xrdectun.f \
			  fits/xrfagree.f fits/xrfrdhe.f fits/xrfrdpon.f \
			  fits/xrfrdyco.f fits/xrftgbdv.f fits/xrftgcol.f \
			  fits/xrftgdes.f fits/xrftgdky.f fits/xrftgext.f \
			  fits/xrftgtky.f fits/xrftgycf.f fits/xrftinit.f \
			  fits/xrftlgti.f fits/xrftpdky.f fits/xrftptky.f \
			  fits/xrftwcol.f fits/xrftwext.f fits/xrfwrlhe.f \
			  fits/xrfwrshe.f fits/xropnfil.f fits/xrphaopt.f \
			  interval/xrapplewi.f interval/xrapplfwi.f \
			  interval/xrapplpwi.f interval/xrapplspwi.f \
			  interval/xrappltwi.f interval/xrappwins.f \
			  interval/xrexpprof.f interval/xrfindstart.f \
			  interval/xrfold.f interval/xrforcesim.f \
			  interval/xrgetexp.f interval/xrgetexpf.f \
			  interval/xrgetintv.f interval/xrintstat.f \
			  interval/xrpper.f interval/xrprepint.f \
			  interval/xrreaddata.f interval/xrrnmean.f \
			  interval/xrtyint.f \
			  math/dnear.f math/drecipes.f math/four1.f \
			  math/realft.f math/timofday.f math/twofft.f \
			  math/xracffast.f math/xracfslow.f math/xrccffast.f \
			  math/xrccfslow.f math/xrchithr.f math/xrdhms.f \
			  math/xrfft8.f math/xrmminv.f math/xrporegr.f \
			  math/xrpowtwo.f math/xrregr.f math/xrrunmean.f \
			  math/xrsignd.f math/xrslwft.f math/xrstdalloc.f \
			  math/xrtmpalloc.f math/xrtrere.f math/xrtsfslow.f \
			  math/xryrdiff.f \
			  output/xrftwrhk.f output/xrgetfm.f output/xrplt.f \
			  output/xrpltcmd.f output/xrpltvar.f \
			  output/xrwroutf.f output/xrwroutplot.f \
			  output/xrwrsta.f output/xrxaxis.f output/xrxrebin.f \
			  output/xryaxis.f output/xryrebin.f \
			  par/xrcheckfiles.f par/xrdecopco.f par/xrdecopt.f \
			  par/xrfilext.f par/xrgetfiles.f par/xrgetfold.f \
			  par/xrgetfout.f par/xrgetlc.f par/xrgetname.f \
			  par/xrgetnbin.f par/xrgetparams.f par/xrgetplto.f \
			  par/xrgetres.f par/xrgettime.f par/xrgettrend.f \
			  par/xrgetuser.f par/xrgetwin.f par/xrparseopt.f \
			  par/xrrdpf.f par/xrrdwin.f par/xrslash.f \
			  par/xrstrext.f par/xrtyana.f par/xrtywin.f \
			  par/xrversion.f \
			  programs/xracsf.f programs/xrccfs.f programs/xref.f \
			  programs/xres.f programs/xrlc.f programs/xrpsdf.f \
			  programs/xrsta.f programs/xrtss.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS} -I../include -I./interval

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

#?HD_DISTCLEAN		= ../include/dmcommon.inc ../include/io.inc

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o} ${HD_LIBRARY_SRC_f:.f=.o}

include ${HD_STD_MAKEFILE}
