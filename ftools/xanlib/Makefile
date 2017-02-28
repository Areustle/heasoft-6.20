HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT		= ${XANLIB}

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

# -DREADLINE_LIBRARY is used by xpi/ihf codes which include readline.h
HD_CFLAGS		= ${HD_STD_CFLAGS} -DREADLINE_LIBRARY -I./xpi

HD_FFLAGS		= ${HD_STD_FFLAGS} -I./inc \
			  -I./estec -I./termio -I./xparse -I./xpi

HD_SHLIB_LIBS		= ${F77LIBS4C}

HD_ADD_SHLIB_LIBS	= yes

HD_INSTALL_HEADERS	= xpi/xpi.h xpi/lheasoft-version-info.h

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_TEST_SUBDIRS		= xpi/test

# unused: xparse/uertst.f
# cod inc xcoms

HD_LIBRARY_SRC_c	= memory/udmget.c sysdep/getdat.c sysdep/ranlux.c \
			  sysdep/setenv.c sysdep/sysc.c \
			  termio/cio.c xparse/cstderrwrt.c xparse/xclock.c \
			  xpi/cldefpf.c xpi/freadline.c xpi/hea_status.c \
			  xpi/lheasoft-version-info.c xpi/opdefpf.c \
			  xpi/tbfdpr.c xpi/tbfdpriraf.c xpi/xanadul.c \
			  xpi/xanaduy.c xpi/xpigetenv.c xpi/xpisetenv.c \
			  xpi/yyparsef.c xpi/ihf/ihf.c \
			  xpi/ihf/ihf_be_helpful.c xpi/ihf/ihf_rd_help_file.c \
			  xpi/apeparsepar.c xpi/xpigettext.c

HD_LIBRARY_SRC_f	= coords/aitoff.f coords/angle.f \
			  coords/chdec.f coords/chra.f coords/coco.f \
			  coords/ctg.f coords/degrad.f coords/dir_cos.f \
			  coords/getdaynum.f coords/getdec.f coords/getra.f \
			  coords/gtc.f coords/iscoord.f coords/isctime.f \
			  coords/isitdg.f coords/parsedec.f coords/parsera.f \
			  coords/raddeg.f coords/schdec.f coords/schra.f \
			  coords/shf_ymd.f coords/shfdyr.f \
			  coords/slprec.f coords/timak.f coords/timka.f \
			  coords/timsa.f coords/timsk.f coords/timxa.f \
			  coords/timxk.f coords/tranc.f coords/xalpha.f \
			  coords/xangle.f coords/xdec.f coords/xdelta.f \
			  coords/xequinox.f coords/xkdate.f coords/xkddat.f \
			  coords/xkjd.f coords/xkmjd.f coords/xkyear.f \
			  coords/xmdyd.f coords/xmonth.f coords/xprec.f \
			  coords/xra.f coords/xrdang.f coords/xydmd.f \
			  estec/aliado.f estec/alias.f estec/aliaui.f \
			  estec/chqual.f estec/cmpstr.f estec/crtfmt.f \
			  estec/decomp.f estec/dfault.f estec/doqual.f \
			  estec/get00.f estec/get0c.f estec/get0d.f \
			  estec/get0i.f estec/get0j.f estec/get0l.f \
			  estec/get0r.f estec/get1d.f estec/get1i.f \
			  estec/get1j.f estec/get1r.f estec/getcmd.f \
			  estec/getudr.f estec/gmatch.f estec/gtcom.f \
			  estec/gtqual.f estec/isalnum.f estec/isalpha.f \
			  estec/isdigit.f estec/isdouble.f estec/isfloat.f \
			  estec/isint.f estec/isliteral.f estec/islower.f \
			  estec/isprint.f estec/istime.f estec/isupper.f \
			  estec/ldcmds.f estec/lftjst.f estec/loc0c.f \
			  estec/loc0d.f estec/loc0i.f estec/loc0j.f \
			  estec/loc0r.f estec/lstblk.f estec/lstcmd.f \
			  estec/lstkey.f estec/lstkw.f estec/mchlst.f \
			  estec/nostar.f estec/opndef.f estec/pagbrk.f \
			  estec/par0c.f estec/par0d.f estec/par0i.f \
			  estec/par0j.f estec/par0l.f estec/par0r.f \
			  estec/post0c.f estec/post0d.f estec/post0i.f \
			  estec/post0j.f estec/post0r.f estec/post1d.f \
			  estec/prmpt.f estec/qlf0c.f estec/qlf0d.f \
			  estec/qlf0i.f estec/qlf0j.f estec/qlf0l.f \
			  estec/qlf0r.f estec/qlmtch.f estec/qsorsm.f \
			  estec/qsortr.f estec/recall.f estec/reclui.f \
			  estec/sortql.f estec/strnum.f estec/tbldcm.f \
			  estec/tbldkw.f estec/udc.f estec/xct0c.f \
			  estec/xct0d.f estec/xct0i.f estec/xct0j.f \
			  estec/xct0k.f estec/xct0r.f estec/xexact.f \
			  estec/xi4.f estec/xindex.f estec/xr4.f \
			  estec/xr8.f estec/ystclq.f estec/ystcls.f \
			  press/gammln.f press/misc.f press/poidev.f \
			  press/ran1.f \
			  sf/bftoch.f sf/cppksf.f sf/cpsbsf.f sf/nxpksf.f \
			  sf/opnrsf.f sf/opnwsf.f sf/ptxtsf.f sf/rst2sf.f \
			  sf/rstrsf.f sf/rsubsf.f sf/skpksf.f sf/sksbsf.f \
			  sf/termsf.f sf/wpkhsf.f sf/wstrsf.f sf/wsubsf.f \
			  sla/addet.f sla/afin.f sla/airmas.f sla/altaz.f \
			  sla/amp.f sla/ampqk.f sla/aop.f sla/aoppa.f \
			  sla/aoppat.f sla/aopqk.f sla/atmdsp.f sla/atms.f \
			  sla/atmt.f sla/av2m.f sla/bear.f sla/caf2r.f \
			  sla/caldj.f sla/calyd.f sla/cc2s.f sla/cc62s.f \
			  sla/cd2tf.f sla/cldj.f sla/clyd.f sla/combn.f \
			  sla/cr2af.f sla/cr2tf.f sla/cs2c.f sla/cs2c6.f \
			  sla/ctf2d.f sla/ctf2r.f sla/daf2r.f sla/dafin.f \
			  sla/dat.f sla/dav2m.f sla/dbear.f sla/dbjin.f \
			  sla/dc62s.f sla/dcc2s.f sla/dcmpf.f sla/dcs2c.f \
			  sla/dd2tf.f sla/de2h.f sla/deuler.f sla/dfltin.f \
			  sla/dh2e.f sla/dimxv.f sla/djcal.f sla/djcl.f \
			  sla/dm2av.f sla/dmat.f sla/dmoon.f sla/dmxm.f \
			  sla/dmxv.f sla/dpav.f sla/dr2af.f sla/dr2tf.f \
			  sla/drange.f sla/dranrm.f sla/ds2c6.f sla/ds2tp.f \
			  sla/dsep.f sla/dsepv.f sla/dt.f sla/dtf2d.f \
			  sla/dtf2r.f sla/dtp2s.f sla/dtp2v.f sla/dtps2c.f \
			  sla/dtpv2c.f sla/dtt.f sla/dv2tp.f sla/dvdv.f \
			  sla/dvn.f sla/dvxv.f sla/e2h.f sla/earth.f \
			  sla/ecleq.f sla/ecmat.f sla/ecor.f sla/eg50.f \
			  sla/el2ue.f sla/epb.f sla/epb2d.f sla/epco.f \
			  sla/epj.f sla/epj2d.f sla/eqecl.f sla/eqeqx.f \
			  sla/eqgal.f sla/etrms.f sla/euler.f sla/evp.f \
			  sla/fitxy.f sla/fk425.f sla/fk45z.f sla/fk524.f \
			  sla/fk52h.f sla/fk54z.f sla/fk5hz.f sla/flotin.f \
			  sla/galeq.f sla/galsup.f sla/ge50.f sla/geoc.f \
			  sla/gmst.f sla/gmsta.f sla/h2e.f sla/h2fk5.f \
			  sla/hfk5z.f sla/idchf.f sla/idchi.f sla/imxv.f \
			  sla/intin.f sla/invf.f sla/kbj.f sla/m2av.f \
			  sla/map.f sla/mappa.f sla/mapqk.f sla/mapqkz.f \
			  sla/moon.f sla/mxm.f sla/mxv.f sla/nut.f sla/nutc.f \
			  sla/nutc80.f sla/oap.f sla/oapqk.f sla/obs.f \
			  sla/pa.f sla/pav.f sla/pcd.f sla/pda2h.f sla/pdq2h.f \
			  sla/permut.f sla/pertel.f sla/pertue.f sla/planel.f \
			  sla/planet.f sla/plante.f sla/plantu.f sla/pm.f \
			  sla/polmo.f sla/prebn.f sla/prec.f sla/preces.f \
			  sla/precl.f sla/prenut.f sla/pv2el.f sla/pv2ue.f \
			  sla/pvobs.f sla/pxy.f sla/range.f sla/ranorm.f \
			  sla/rcc.f sla/rdplan.f sla/refco.f sla/refcoq.f \
			  sla/refro.f sla/refv.f sla/refz.f sla/rverot.f \
			  sla/rvgalc.f sla/rvlg.f sla/rvlsrd.f sla/rvlsrk.f \
			  sla/s2tp.f sla/sep.f sla/sepv.f sla/smat.f \
			  sla/subet.f sla/supgal.f sla/svd.f sla/svdcov.f \
			  sla/svdsol.f sla/tp2s.f sla/tp2v.f sla/tps2c.f \
			  sla/tpv2c.f sla/ue2el.f sla/ue2pv.f sla/unpcd.f \
			  sla/v2tp.f sla/vdv.f sla/vn.f sla/vxv.f sla/xy2xy.f \
			  sla/zd.f \
			  stats/betacf.f stats/betai.f stats/crank.f \
			  stats/cumga.f stats/dell.f stats/dfacl.f \
			  stats/erfcc.f stats/ergs.f stats/exosat_uv.f \
			  stats/fac.f stats/facl.f stats/funzione.f \
			  stats/gamlin.f stats/gamma.f stats/gauss_test.f \
			  stats/indexx.f stats/int_browse.f stats/intbv.f \
			  stats/ipos1.f stats/kcorr.f stats/kendl1.f \
			  stats/kolm1.f stats/kolmo.f stats/lum.f \
			  stats/momnts.f stats/p3coef.f stats/p3int.f \
			  stats/pcorre.f stats/pearsn.f stats/poiss.f \
			  stats/polog.f stats/probks.f stats/qran.f \
			  stats/rank.f stats/seaton.f stats/sfit.f \
			  stats/simp1.f stats/sort2.f stats/spear.f \
			  sysdep/bltoby.f sysdep/delfil.f sysdep/filbyt.f \
			  sysdep/get_pgdev.f sysdep/getrem.f sysdep/gtinit.f \
			  sysdep/gtpoir.f sysdep/gtstat.f sysdep/inqrwr.f \
			  sysdep/lchop.f sysdep/lunlst.f sysdep/openda.f \
			  sysdep/rmvblk.f sysdep/rmvchr.f \
			  sysdep/rmvlbk.f sysdep/rmvnul.f sysdep/rmvxbk.f \
			  sysdep/setcht.f sysdep/seterr.f sysdep/setlog.f \
			  sysdep/setrca.f sysdep/setscr.f sysdep/stlog.f \
			  sysdep/sys.f sysdep/trfnam.f sysdep/trlnm.f \
			  sysdep/wrstat.f sysdep/xfill.f sysdep/xrenam.f \
			  sysdep/xsquez.f sysdep/xwait.f \
			  termio/alf.f termio/cget.f termio/edicom.f \
			  termio/errlog.f termio/lenact.f termio/logger.f \
			  termio/qline.f termio/script.f termio/sysio.f \
			  termio/ttwrt.f termio/xtend.f termio/xwrcmd.f \
			  ufny/ufnydemo.f \
			  xhelp/chtobl.f xhelp/fndtop.f xhelp/gtxhlp.f \
			  xhelp/ichbeg.f xhelp/opnche.f xhelp/rdche.f \
			  xhelp/rdpag.f xhelp/shfdoc.f xhelp/shxhlp.f \
			  xhelp/smatch.f xhelp/tyxhlp.f xhelp/wrtpag.f \
			  xhelp/wrttop.f xhelp/xhelpsub.f \
			  xparse/fxwrite.f xparse/prsint.f xparse/qmatch.f \
			  xparse/retnum.f xparse/retstr.f xparse/squeez.f \
			  xparse/xaerror.f xparse/xasthn.f \
			  xparse/xchatr.f xparse/xchaty.f xparse/xcheck.f \
			  xparse/xchkbl.f xparse/xchkcl.f xparse/xchkdl.f \
			  xparse/xchkop.f xparse/xchose.f xparse/xclser.f \
			  xparse/xclslg.f xparse/xclssc.f xparse/xcmget.f \
			  xparse/xcmsav.f xparse/xcomnd.f xparse/xcread.f \
			  xparse/xcreadh.f xparse/xdebug.f xparse/xerror.f \
			  xparse/xflush.f xparse/xfrlun.f xparse/xgenrd.f \
			  xparse/xgrnge.f xparse/xgtarg.f xparse/xgtcht.f \
			  xparse/xgtint.f xparse/xgtlin.f xparse/xgtlr4.f \
			  xparse/xgtlun.f xparse/xgtmch.f xparse/xgtnum.f \
			  xparse/xgtonf.f xparse/xgtrl4.f xparse/xgtrng.f \
			  xparse/xgtrrg.f xparse/xgtstr.f xparse/xinfix.f \
			  xparse/xinird.f xparse/xisint.f xparse/xisnum.f \
			  xparse/xlgnme.f xparse/xlist.f xparse/xlogit.f \
			  xparse/xmatch.f xparse/xmove.f xparse/xnxarg.f \
			  xparse/xnxtrd.f xparse/xopner.f xparse/xopnlg.f \
			  xparse/xopnsc.f xparse/xopsys.f xparse/xpari4.f \
			  xparse/xparr4.f xparse/xparse.f xparse/xpflsh.f \
			  xparse/xpmsg1.f xparse/xpmsg2.f xparse/xpmsg3.f \
			  xparse/xprint.f xparse/xprnge.f xparse/xprrl4.f \
			  xparse/xprrng.f xparse/xprrrg.f xparse/xpstlg.f \
			  xparse/xptarg.f xparse/xpxgtr.f xparse/xpxgts.f \
			  xparse/xqmtch.f xparse/xquest.f xparse/xqwrte.f \
			  xparse/xqxmch.f xparse/xseti4.f xparse/xsetr4.f \
			  xparse/xstatn.f xparse/xtsti4.f xparse/xtstr4.f \
			  xparse/xunids.f xparse/xwarn.f xparse/xwrite.f \
			  xpi/aliado1.f xpi/alias1.f xpi/dumpcmd.f \
			  xpi/dumpkey.f xpi/dumpkw.f xpi/dumppar.f \
			  xpi/gtbuf.f xpi/gtbufc.f xpi/gtcom2.f xpi/gtdellog.f \
			  xpi/ldcmds1.f xpi/lstblk1.f xpi/lstcmd1.f \
			  xpi/lstkw1.f xpi/pclgsb.f xpi/pclgsd.f xpi/pclgsg.f \
			  xpi/pclgsi.f xpi/pclgsl.f xpi/pclgsr.f xpi/pclgss.f \
			  xpi/pclgst.f xpi/pclgsx.f xpi/pclpsb.f xpi/pclpsd.f \
			  xpi/pclpsi.f xpi/pclpsl.f xpi/pclpsr.f xpi/pclpss.f \
			  xpi/pclpst.f xpi/pclpsx.f xpi/pgetiraf.f \
			  xpi/promptc.f xpi/recall1.f xpi/reclui1.f \
			  xpi/setscr1.f xpi/tbgtcm.f xpi/tbgtkw.f xpi/tbgtky.f \
			  xpi/tbgtpr.f xpi/tbldal.f xpi/tbldkw1.f xpi/tbldky.f \
			  xpi/tbldpr.f xpi/tbldstand.f xpi/tbldstiraf.f \
			  xpi/tblfpr.f xpi/tbsvpr.f xpi/uclgno.f xpi/uclgot.f \
			  xpi/uclgsb.f xpi/uclgsd.f xpi/uclgsg.f xpi/uclgsi.f \
			  xpi/uclgsl.f xpi/uclgsr.f xpi/uclgss.f xpi/uclgst.f \
			  xpi/uclgsx.f xpi/uclpsb.f xpi/uclpsd.f xpi/uclpsi.f \
			  xpi/uclpsl.f xpi/uclpsr.f xpi/uclpss.f xpi/uclpst.f \
			  xpi/uclpsx.f xpi/udc1.f xpi/uerget.f xpi/umsput.f \
			  xpi/xgtarg1.f xpi/xinird1.f xpi/xnxtrd1.f \
			  xpi/xpiopenlog.f xpi/xpiparmode.f xpi/xpisavepar.f \
			  xpi/xpitbldcm.f xpi/xpivers.f xpi/ycapcmd.f \
			  xpi/ycapdelim.f xpi/ycapline.f xpi/ycappar.f \
			  xpi/ycapval.f xpi/ycmprg.f xpi/ydebug.f \
			  xpi/ygetcmd.f xpi/ygetline.f xpi/yinit.f \
			  xpi/ynextpar.f xpi/yprint.f xpi/ystclq1.f \
			  xpi/ystcls1.f xpi/ystcmd.f xpi/ystmat.f \
			  ${F77_G77_ONLY_SRC_f}

HD_DISTCLEAN		= sysdep/sysc.c sysdep/sys.f termio/cio.c termio/sysio.f
# dpi.inc status.codes sysdep/writebuf.inc

default: build-libxanlib

all: default publish

run-test:
	cd xpi/test; ${HD_MAKE} run-test

include ${HD_STD_MAKEFILE}

FC_BASENAME_F77		= ${shell echo ${FC_BASENAME} | grep -c f77}
FC_BASENAME_F95		= ${shell echo ${FC_BASENAME} | grep -c f95}
FC_BASENAME_G95		= ${shell echo ${FC_BASENAME} | grep -c g95}
FC_BASENAME_GFORTRAN	= ${shell echo ${FC_BASENAME} | grep -c gfortran}
FC_BASENAME_IFORT	= ${shell echo ${FC_BASENAME} | grep -c ifort}

build-libxanlib:
	@if [ ${FC_BASENAME_F77} = 1 -o \( ${G77} = yes -a ${FC_BASENAME_F95} = 0 -a ${FC_BASENAME_G95} = 0 -a ${FC_BASENAME_GFORTRAN} = 0 \) ]; then \
		${HD_MAKE} ${XANLIB} HD_LIBRARY_ROOT=${XANLIB} \
			F77_G77_ONLY_SRC_f="estec/xloc.f coords/c11_shf.f coords/shf_c11.f coords/xshf.f"; \
	else \
		${HD_MAKE} ${XANLIB} HD_LIBRARY_ROOT=${XANLIB} \
			F77_G77_ONLY_SRC_f=""; \
	fi

distclean: clean hd-std-distclean

clean: hd-std-clean
	rm -f ${HD_LIBRARY_SRC_c:.c=.o}
	rm -f ${HD_LIBRARY_SRC_f:.f=.o}
	@if [ -f estec/xloc.o ]; then \
		echo "rm -f estec/xloc.o"; \
		rm -f estec/xloc.o; \
	fi
	@if [ -f coords/c11_shf.o ]; then \
		echo "rm -f coords/c11_shf.o"; \
		rm -f coords/c11_shf.o; \
	fi
	@if [ -f coords/shf_c11.o ]; then \
		echo "rm -f coords/shf_c11.o"; \
		rm -f coords/shf_c11.o; \
	fi
	@if [ -f coords/xshf.o ]; then \
		echo "rm -f coords/xshf.o"; \
		rm -f coords/xshf.o; \
	fi

# Use sys.lnx for all GNU or Intel Fortran (G77 may be 'no' for ifort),
# except on Cygwin:
sysdep/sys.f:
	@-rm -f $@; \
	if [ "x${EXT}" = xwin ]; then \
		${HD_LN_S} sysdep/sys.${EXT} $@; \
	elif [ ${G77} = yes -o ${FC_BASENAME_IFORT} != 0 ]; then \
		${HD_LN_S} sys.lnx $@; \
	elif [ -f sysdep/sys.${EXT} ]; then \
		${HD_LN_S} sys.${EXT} $@; \
	fi

# Use sysclnx.c whenever GCC='yes', except in Cygwin:
sysdep/sysc.c:
	@-rm -f $@; \
	if [ "x${EXT}" = xwin ]; then \
		${HD_LN_S} sysdep/sysc${EXT}.c $@; \
	elif [ ${GCC} = yes ]; then \
		${HD_LN_S} sysclnx.c $@; \
	elif [ -f sysdep/sysc${EXT}.c ]; then \
		${HD_LN_S} sysc${EXT}.c $@; \
	else \
		${HD_LN_S} syscspud.c $@; \
	fi

termio/cio.c:
	@-rm -f $@; \
	if [ -f termio/cio${EXT}.c ]; then \
		if [ "x${EXT}" = xwin ]; then \
			${HD_LN_S} termio/cio${EXT}.c $@; \
		else \
			${HD_LN_S} cio${EXT}.c $@; \
		fi; \
	fi

termio/sysio.f:
	@-rm -f $@; \
	if [ -f termio/sysio.${EXT} ]; then \
		if [ "x${EXT}" = xwin ]; then \
			${HD_LN_S} termio/sysio.${EXT} $@; \
		else \
			${HD_LN_S} sysio.${EXT} $@; \
		fi; \
	fi
