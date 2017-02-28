HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= xstar${FT_VERS}

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_LIBRARY_SRC_c	= xstartablelib.c xstarutillib.c

HD_LIBRARY_SRC_f	=  amcol.f    amcrs.f    anl1.f    binemis.f    bkhsgo.f    bremem.f \
calc_kato.f    calc_maxwell_rates.f    calc_sampson_h.f    calc_sampson_p.f    calc_sampson_s.f    calc_spline.f \
calt57.f    calt60_62.f    calt66.f    calt67.f    calt68.f    calt69.f \
calt70.f    calt71.f    calt72.f    calt73.f    calt74.f    calt77.f \
chisq.f    cmpfnc.f    comp2.f    comp3.f    comp.f    deleafnd.f \
deletefile.f    dfact.f    dprint.f    dprinto2.f    dprinto.f    dprints2.f \
dprints.f    drd.f    dread.f    dsec.f    ee1exp.f    ee1expo.f \
eint.f    ener.f    enxt.f    erc.f    errmess.f    exint1.f \
exint_n.f    exp10.f    expint.f    expo.f    fact.f    fbg.f \
ff2.f    fheader.f    find53.f    fitsclose.f    flinabs.f    fnappend.f \
fparmlist.f    freef.f    fstepr2.f    fstepr3.f    fstepr4.f    fstepr.f \
func1.f    func2.f    func2i.f    func2l.f    func3.f    func3p.f \
func.f    funcsyn.f    fwrtascii.f    gull1.f    heatf.f    heatt.f \
hgf.f    hphotx.f    hunt3.f    huntf.f    impact.f    impactn.f \
impcfn.f    init.f    interpol_huntd.f    intin.f    ioneqm.f    irc.f \
ispcg2.f    ispec4.f    ispec.f    ispecg.f    ispecgg.f    istruc.f \
leqt2f.f    levwk.f    linopac.f    lubksb.f    ludcmp.f    milne.f \
mprove.f    msolvelucy.f    nbinc.f    pescl.f    pescv.f    pexs.f \
phextrap.f    phint53.f    phint53hunt.f    phint53old.f    phint53pl.f    phintfo.f \
pow.f    pprint.f    prep_spline.f    printerror.f    rdflo.f    rdint.f \
readtbl.f    remtms.f    rread1.f    rstepr2.f    rstepr3.f    rstepr4.f \
rstepr.f    savd.f    setptrs.f    splinem.f    starf.f    step.f \
stpcut.f    szcoll.f    szirc.f    szirco.f    trnfrc.f    trnfrn.f \
ucalc.f    uclgsr8.f    unsavd.f    upsil.f    upsiln.f    velimp.f    voigtedoppler.f \
voigte.f    voigtelorentzian.f    writespectra2.f    writespectra3.f    writespectra4.f    writespectra.f \
xstarcalc.f    xstarsetup.f getlunx.f frelunx.f lunlstx.f



HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= xstartablelib.h xstarutillib.h

include ${HD_STD_MAKEFILE}

xstartablelib.o: ${CFITSIO_DIR}/fitsio.h ${CFITSIO_DIR}/fitsio2.h \
		xstartablelib.h xstarutillib.h

xstarutillib.o: ${CFITSIO_DIR}/fitsio.h ${CFITSIO_DIR}/fitsio2.h \
		xstarutillib.h
