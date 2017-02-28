      subroutine pprint(jj,jkstep,
     $ tp,xlum,lwri,lpri,r,t,xpx,p,lcdd,numrec,npass,
     $ nnmax,nlimd,rmax,xpxcol,xi,zeta,lfix,zremsz,epi,ncn2,
     $ abel,cfrac,emult,taumax,xeemin,spectype,specfile,specunit,
     $ kmodelname,nloopctl,nparms,parname,partype,parms,parcomm,
     $ atcredate,lun11,tinf,xcol,vturbi,critf,radexp,
     $ delr,rdel,enlum,xee,ababs,
     $ bremsa,bremsint,tau0,dpthc,tauc,
     $ idat1,rdat1,kdat1,nptrs,np2,
     $ npar,npnxt,npfi,npfirst,
     $ nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $ npconi2,ncsvn,
     $ ntotit,lnerrd,
     $ xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $ xilev,bilev,rniss,
     $ rcem,oplin,rccemis,brcems,opakc,opakscatt,cemab,opakab,
     $ cabab,elumab,elum,zrems)
c
c     this routine prints
c     author:  T. Kallman
c
c     variable categories:
c     step indeces:
c      jj,jkstep,
c     input parameters:
c      tp,xlum,lwri,lpri,xpx,p,lcdd,numrec,npass,
c      nnmax,nlimd,rmax,xpxcol,xi,zeta,lfix,
c      abel,cfrac,emult,taumax,xeemin,
c      tinf,xcol,vturbi,critf,ababs,
c     other input
c      spectype,specfile,specunit,kmodelname,nloopctl,
c      nparms,parname,partype,parms,parcomm,lun11,
c     input spectrum
c      zremsz,epi,ncn2,bremsa,
c     database quantities
c      idat1,rdat1,kdat1,nptrs,np2,
c      npar,npnxt,npfi,npfirst,nplin,nplini,
c      npcon,npconi,npilev,npilevi,npconi2,
c      nlsvn,ncsvn,
c     step diagnostics
c      ntotit,lnerrd,
c     state variables
c      r,t,xee,xii,xilev,bilev,rniss,
c     derived state variables
c      delr,rdel,enlum,
c     rates
c      rrrt,pirt,htt,cll,httot,cltot,hmctot,
c      cllines,clcont,htcomp,clcomp,clbrems,
c     emissivities and opacities
c      rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
c     optical depths and luminosities
c      tau0,dpthc,tauc,elumab,elum,zrems
c
c
C     A plethora of printing options...
C
C        jj
C         1 - 500 strongest emissions lines, sorted by strength
C         19 - RRC luminosities
C         23 - 500 strongest absorption lines, sorted by strength
C         24 - absorption edges
C         2 - print input parameter listC
c         4 - continuum opacity and emissivity
c         5 - energy sums
c         6 - continuum luminosities and depths
c         8 - line list
c         10 - ion abundances and thermal rates (erg/sec)
C        11 - Write FITS file with summary of ion abundances
C        12 - append abundance values to the data array for xout_abund1.fits
C             Doesn't actually write the file, just accumulates values.
c        13 - blank space
c        14 - line opacities and emissivities
c        15 - line luminosities
C        18 - line wavelengths and levels
c        20 - line finding list
c        21 - level opacities and emissivities
c         7 - level populations
C        17 - print column headings for this pass
C         9 - print short summary line of the radial zone
c        16 - times
c        22 - ionization parameter etc.
c        25 - outputting to common block
c        26 - ferland print
C
C     Modifications:
C        1998/12/17, WTB: Fix FITS keyword format problem for
C                       writeascii routine.  Removed dependence on
C                       writeimage routine.
C        1999/01/04, WTB: Added model name to writeascii parameter list
C        1999/01/05, WTB: Removed log(Xi)& log(U1) columns from calls #11
C                       & #12
C                       Inserted '_' in spaces for ion names in call #11
C
      implicit none
      include './PARAM'
c
      integer nptmpdim
      parameter (nptmpdim=400000)
c
c     master data
      integer idat1(nidat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      real*8 rdat1(nrdat1)
      character(1) kdat1(nkdat1)
c     pointers to master data
      integer npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      integer npfi(ntyp,nni)
c     pointers to line data
      integer nplin(nnnl),nplini(ndat2)
c     pointers to line data
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)
c     line luminosities
      real*8 elum(3,nnnl)
c     line emissivities
      real*8 rcem(2,nnnl)
c     line opacities
      real*8 oplin(nnnl)
c     line optical depths
      real*8 tau0(2,nnnl)
c     energy bins
      real*8 epi(ncn)
c     continuum lum
      real*8 zrems(4,ncn),
     $          zremsz(ncn)
c     continuum optical depths
      real*8 dpthc(2,ncn)
c     continuum flux
      real*8 bremsa(ncn)
      real*8 bremsint(ncn)
c     continuum emissivities
      real*8 rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      real*8 opakc(ncn),opakscatt(ncn)
c     level populations
      real*8 xilev(nnml),bilev(nnml),rniss(nnml)
      real*8 cemab(2,nnml),cabab(nnml),opakab(nnml)
      real*8 elumab(2,nnml)
      real*8 tauc(2,nnml)
c     ion abundances
      real*8 xii(nni)
c     heating/cooling
      real*8 htt(nni),cll(nni)
c     the atomic data creation date
      character(63) atcredate
      real*8 rrrt(nni),pirt(nni)
      real*8 abel(nl),ababs(nl)
      real*8 xcoltmp(nni)
      integer kltmp(5000)
      real*8 zrtmp(999,3999),zrtmpcol(999,3999),
     $   zrtmpc(999,3999),zrtmph(999,3999)
c      real*8 epi2(ncn),zrems2(3,ncn)
c
c      common /ewout/newout,lnewo(nnnl),kdewo(8,nnnl),
c     $  kdewol(20,nnnl),kdewou(20,nnnl),aijewo(nnnl),flinewo(nnnl),
c     $  ggloewo(nnnl),ggupewo(nnnl),
c     $  elewo(nnnl),tau0ewo(nnnl),elout(2,nnnl),zrtmp,epi2,zrems2
c     A feature added November 2007 is output of the strongest lines,
c     sorted by element and ion into a common block called 'ewout'
c     The contents of the common block are:
c       newout:  number of lines in the list.
c       lnewo:   array conatining line indexes.
c       kdewo:   character array containing the name of the ion
c       kdewol:  character array containing the name of the lower level
c       kdewou:  character array containing the name of the upper level
c       aijewo:  array containing A values for the lines
c       flinewo: array containing f values for the lines
c       ggloewo: array containing statistical weights for the lower levels
c       ggupewo: array containing statistical weights for the upper levels
c       elewo:   array containing the line wavelengths
c       tau0ewo: array containing the line center depths
c       elout:   array containing line luminosities in xstar units (erg/s/10^38)
c       zrtmp: a 2d array 999x3999, containing a zone-by-zone summary of the
c                state of the gas.  The second index is the zone number
c                the first index is as follows:
c                1: radius (cm)
c                2: log(xi)
c                3: electron fraction (relative to nuclei)
c                4: nucleus number density
c                5: pressure (dynes/cm^2)
c                6: tempeatre/10^4 K
c                7:  heating-cooling/(heating+cooling)
c                8-..: ion fractions for all the ions in the model
c                    a model with non-zero abundance for all elements
c                    will have 168 ions (excluding bare nuclei)
c                    numbered such that 1=H0, 2=He0, 3=He+, 4=C0,
c                    ... 168=Ni27+.  In this case the upper limit
c                    for these columns will be 168+8=176
c       epi: energy grid in eV, length=99999
c       zrems: spectrum in erg/s/erg/10**38.  This is a 2d array 3x99999,
c           where column 1=transmitted outward flux, including diffuse
c           emission, column 2=diffuse inward emission (no direct flux)
c           column 3=diffuse outward emission (no direct flux)
c
c
c       character(1) kdewo,kdewol,kdewou
c       character(1) klevl(20),klevu(20)
c       real*8  flinewo, aijewo,
c      real*8 ggloewo, ggupewo, elewo, tau0ewo
c      real*8 elout
c      integer nilino, jkktmp, lup, lnewo, lupfnd, llofnd
c      integer newout
c
      character(20) parname(55)
      character(10) partype(55)
      real*8 parms(55)
      character(30) parcomm(55),kmodelname
      character(8) spectype, specfile
      character(1) kdtmp(100),kblnk,klablo(20),klabup(20)
      integer nparms, specunit, nloopctl
      character(8) kabstring(30)
      character(8) ktmp8
      character(20) ktmp20,klevu,klevl,kblnk20
      character(9) kinam1
      character(133) tmpst
      character(16) knam,klabs(3999),kunits(3999),kform(3999),ktmp,
     $              kblnk16
      character(1) klev(100,nd),kdat(nptmpdim)
      integer klen, unit, status
      real*8  rlev(10,nd)
      integer ilv(10,nd),nlpt(nd),iltp(nd)
      real*8  elsv(nnnl)
      integer jpnt(nnnl)
c jg
      real*8 xnx, xpx, xee, eliml, elimh
      real*8 elmmtpp, elcomp, xeltp, cabcompare
      real*8 abund1, cfrac, t, p, tp, xlum
      real*8 xpxcol, zeta, taumax, xeemin, critf, radexp
      real*8 vturbi, opsum, tstar, fstr, rsum1
      real*8 rsum2, sgtmp, tmp, crayj, fstro
      real*8 emult, ekkr, delte, rssmn, elsum, ergsev
      real*8 sumtmp1, sumtmp2, r19, tmp1, tmp2, tmp1o
      real*8 tmp2o, err, sum1, sum2, sum3, sum4
      real*8 r, ener, etst, aij, gglo, ggup, flin
      real*8 httot, cltot, htcomp, clcont, cllines
      real*8 clcomp, clbrems, uu1, enlum, alguu1
      real*8 skse, ecc, ekt, sksec, zetac, enn0
      real*8 egam, rdel, hmctot, vvthermsc
      real*8 elmtp, elmtpb, ethi, ethc, terr
      real*8 ett, optpp, optppo, tmp2c, xcol,fpr2
      real*8 tmp3, tmp4, tmp5, tmp6, tmp7, tmp8
      real*8 ttot, enlumx
      real*8 uux, alguux, eth, abundel
      real*8  xi, delr, elin, flux1, flux2
      real*8 rmax, tinf, rdum, dep, rss, bbe, rocc

      integer jj, lun11, lpril, kltmpo, nlplmx, lm
      integer nlpl, lnn, nlsvn, ln, ml, ltyp, lrtyp
      integer lcon, nrdt, nidt, nkdt, nilin
      integer lmm, kl2, k
      integer kltmpn, mm, kk, j, ipmat, klel, mlel
      integer jkk, nnz, klion,mlion, jk, mt2, mllel
      integer kl, nkdti, mlleltp, nlevmx, mltype
      integer mlpar, lk, nilin2, mllz, nlev, kkkl, idest1
      integer llo, np2, nlevp, idest2, jkk3, ndtmp
      integer mllz2, iltmp, ltyp2, lrtyp2, lcon2, nrdt2
      integer nidt2, nkdt2, lcdd, numrec, nlimd, lwri, lpri
      integer lfix, npass, numcon, ncn2, i, jlk, lun11sv
      integer ktt, lfnd, nell, lkk, nelin, jkl, mmlv, nidti
      integer nlyc, nry, jkstep, ilevup, ilevlo, jkko
      integer niter, jjj, jp1, npi, npc, mll, ltypc
      integer ilevt, npio, mllev, mlcu, mm2, mmtmp, ll
      integer ntotit, nb1, nb10
      integer lnerrd, nbinc
      integer nnmax, ncsvn,mlm
      integer np1i,np1r,np1k,np1i2,np1r2,np1k2,np1ki
c
      logical done
c
c     Not used
      integer javi
      real*8 javir
c      character(80) javik
c
      save zrtmpc,zrtmph,zrtmp
c
      data kblnk20/'                    '/
      data kblnk/' '/,kblnk16/'                '/
      data kabstring/'H abund=','Heabund=','Liabund=',
     $               'Beabund=','B abund=','C abund=','N abund=',
     $               'O abund=','F abund=','Neabund=','Naabund=',
     $               'Mgabund=','Alabund=','Siabund=','P abund=',
     $               'S abund=','Clabund=','Arabund=','K abund=',
     $               'Caabund=','Scabund=','Tiabund=','V abund=',
     $               'Crabund=','Mnabund=','Feabund=','Coabund=',
     $               'Niabund=','Cuabund=','Znabund='/
c
c
      javi=nnmax
      javir=rmax
      javi=nparms
c      javik=parname(1)
c      javik=partype(1)
      javir=tinf
      javir=bremsa(1)
      bremsa(1)=javir
      javi=nplini(1)
      javi=npilevi(1)
      javi=npconi(1)
      javi=ncsvn
      javir=bilev(1)
      javir=xi
      javi=lnerrd
c      lnerrd=javi
c

      xnx=xpx*xee
c
      if ((jj.ne.9).and.(jj.ne.12))
     $ write (lun11,9211)jj
 9211 format (1x, 'print option:',i2)
      if ((jj.le.0).or.(jj.gt.27)) return
c
      goto (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
     $23,24,25,26,27),
     $  jj
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
1     continue
c
      lpril=0
c     500 strongest emission lines, sorted by strength
      write (lun11,*)'emission line luminosities (erg/sec/10**38))'
      kltmpo=0
      nlplmx=500
      eliml=0.1
      elimh=1.0e10
c     find the strongest lines.
      do  lm=1,nlplmx
        kltmp(lm)=0
        enddo
c
c     step through lines
      nlpl=1
      do lnn=1,nlsvn
c
c       get line data
        ln=lnn
        ml=nplin(ln)
        mlm=ml-1
        call drd(ltyp,lrtyp,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $    nptrs,0,lun11)
        elin=abs(rdat1(np1r))
c
c       exclude rate type 14
        if ((lrtyp.ne.14).and.(lrtyp.ne.9)) then
c
c         get ion data
          nilin=npar(ml)
          mlm=nilin-1
          call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
          nilin2=idat1(np1i+nidt-1)
c
c         get lum and test for strength, wavelength
          elmmtpp=(elum(2,ln)+elum(1,ln))/2.
          if (lpril.ne.0)
     $       write (lun11,*)lnn,elin,nilin,elmmtpp,ln,ml
          if ((ln.gt.0).and.(ln.lt.nnnl)
     $       .and.(elin.ge.eliml).and.(elin.le.elimh)
     $       .and.(elin.le.8.9e+6)
     $       .and.(elmmtpp.gt.1.e-36)
     $       .and.(nilin2.gt.0).and.(nilin2.le.nni))
     $        then
c
c           insertion sort
            lmm=0
            elcomp=1.e+10
            do while ((lmm.lt.nlpl).and.(elmmtpp.lt.elcomp))
              lmm=lmm+1
              kl2=kltmp(lmm)
              elcomp=0.
              if (kl2.gt.0)
     $          elcomp=(elum(2,kl2)+elum(1,kl2))/2.
              enddo
            if (lpril.ne.0)
     $       write (lun11,8516)ln,elin,elmmtpp,lmm,nlpl,kl2,elcomp
 8516       format (1h ,i4,2e12.4,3i4,e12.4)
            kltmpo=ln
            do  k=lmm,min(nlplmx,nlpl)
              if ((lpril.ne.0).and.(kltmp(k).ne.0))
     $          write (lun11,*)'in 557 loop',k,kltmp(k),kltmpo
              kltmpn=kltmp(k)
              kltmp(k)=kltmpo
              kltmpo=kltmpn
              enddo
           nlpl=min(nlplmx,nlpl+1)
           if (lpril.ne.0)
     $       write (lun11,*)'done with 557 loop',lm
            endif
c           end of insertion
c
          endif
c
        enddo
c
      if (nlpl.gt.0) kltmp(nlpl)=kltmpo
c
c     printing loop
      write (lun11,959)
c
c     step through lines
      do  kk=1,nlpl
        if (lpril.ne.0)
     $    write (lun11,*)'kk=',kk
        ln=kltmp(kk)
        if (ln.ne.0) then
c
c         get line data
          ml=nplin(ln)
          if (ml.ne.0) then
            if (lpril.ne.0)
     $      write (lun11,*)'   ',ln,ml
            mlm=ml-1
            call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $        nptrs,0,lun11)
            elin=abs(rdat1(np1r))
c
c           get ion data
            nilin=npar(ml)
            mlm=nilin-1
            call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $        nptrs,0,lun11)
            do mm=1,nkdt
              kdtmp(mm)=kdat1(np1k-1+mm)
              enddo
            do mm=nkdt+1,9
              kdtmp(mm)=kblnk
              enddo
c            nilin=idat1(np1i+2)
            if (lpril.ne.0)
     $      write (lun11,*)ml,nilin,npar(ml)
c
c           print
            write (lun11,9955)kk,ln,(kdtmp(mm),mm=1,9),elin,
     $      elum(1,ln),elum(2,ln)
 9955       format (1x,2i8,1x,9a1,3(1pe13.5))
c
            endif
c
          endif
c
        enddo
c
      write (lun11,993)
c
      return
c
c
 19   continue
c
c     print 500 strongest recombination continua
      lpril=0
      write (lun11,*)'recombination continuum luminosities',
     $  '(erg/sec/10**38))'
      write (lun11,*)'index, ion, level, energy (eV), RRC luminosity '
c
C     lpril is flag for printing debug information
      if (lpril.ne.0) then
        write (lun11,*)'raw data'
        do j=1,nnml
          if (xilev(j).gt.1.e-37)
     $     write (lun11,*)j,xilev(j),elumab(1,j)
          enddo
        endif
c
C     First look for element data (jk is element index)        
      klel=11
      mlel=npfirst(klel)
      jk=0
      kk=0
      jkk=0
c
c     step through elements
      do while (mlel.ne.0)
c
c       get element data
        jk=jk+1
        mt2=mlel-1
        call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mt2,
     $        nptrs,0,lun11)
        mllel=idat1(np1i+nidt-1)
        xeltp=rdat1(np1r)
        xeltp=abel(mllel)
        nnz=idat1(np1i)
        if (lpril.ge.1)
     $        write (lun11,*)'element:',jk,mlel,mllel,nnz,
     $                  (kdat1(np1k-1+mm),mm=1,nkdt)
c
C       ignore if the abundance is small
        if (xeltp.lt.1.e-10) then
            jkk=jkk+nnz
          else
c
c           now step thru ions (jkk is ion index)
            klion=12
            mlion=npfirst(klion)
            jkk=0
            kl=0
            do while ((mlion.ne.0).and.(kl.lt.nnz))
              jkk=jkk+1
c
C             retrieve ion name from kdati
              mlm=mlion-1
              call drd(ltyp,lrtyp,lcon,
     $            nrdt,np1r,nidti,np1i,nkdti,np1ki,mlm,
     $            nptrs,0,lun11)
c
C             if not accessing the same element, skip to the next element
              mlleltp=idat1(np1i+nidti-2)
              if (mlleltp.eq.mllel) then
c
                kl=kl+1
                if (lpril.ge.1)
     $            write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $                        (kdat1(np1ki+mm-1),mm=1,nkdti)
c
c               now find level data
                call func2l(jkk,lpril,lun11,t,xee,xpx,
     $              idat1,rdat1,kdat1,nptrs,
     $              npar,npnxt,npfi,
     $              rniss,rlev,ilv,
     $              nlpt,iltp,nlev,klev)
c
c               now step through rate type 7 data
                mltype=7
                ml=npfi(mltype,jkk)
                mllz=0
                if (ml.ne.0) mllz=npar(ml)
                mlpar=0
                if (ml.ne.0) mlpar=npar(ml)
                do while ((ml.ne.0).and.(mlpar.eq.mllz))
c
c                 get rrc data
                  kkkl=npconi2(ml)
                  if (lpril.ne.0) write (lun11,*)kkkl,ml,idest1,
     $                    elumab(1,kkkl),elumab(2,kkkl)
c
c                 test for non-zero rrc data
                  if ((kkkl.gt.0).and.(kkkl.le.ndat2)
     $                .and.((elumab(1,kkkl).gt.1.e-36)
     $                .or.(elumab(2,kkkl).gt.1.e-36))) then
c
c                   get rrc  data
                    mlm=ml-1
                    call drd(ltyp,lrtyp,lcon,
     $                nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $                nptrs,0,lun11)
                    idest1=idat1(np1i+nidt-2)
                    nlevp=nlev
                    idest2=nlevp+idat1(np1i-1+nidt-3)-1
c
c                   label for lower level
                    do lk=1,20
                      write (ktmp20(lk:lk),'(a1)')klev(lk,idest1)
                      enddo
                    klevl=ktmp20
c
c                   label for upper level
                    write (ktmp20(1:20),'(a20)')'continuum           '
                    klevu=ktmp20
c
c                   ion label
                    do lk=1,nkdti
                      write (ktmp8(lk:lk),'(a1)')kdat1(np1ki+lk-1)
                      enddo
                    do lk=nkdti+1,8
                      write (ktmp8(lk:lk),'(a1)')kblnk
                      enddo
c
                    eth=rlev(4,idest1)-rlev(1,idest1)
                    ett=eth
c
c                   get upper level data
                    if (idest2.gt.nlevp) then
                      jkk3=jkk+1
                      if (lpril.gt.1)
     $                  write (lun11,*)jkk3,ndtmp,nlevp,idest2
                      ndtmp=npfi(13,jkk3)
                      if (lpril.gt.1)
     $                  write (lun11,*)jkk3,ndtmp,nlevp,idest2
                      if (ndtmp.le.0) stop 'ndtmp error'
                      mllz=npar(ndtmp)
                      iltmp=0
                      do while ((ndtmp.ne.0).and.
     $                    (iltmp.ne.(idest2-nlevp+1)).and.
     $                    (npar(ndtmp).eq.mllz)) 
                        mlm=ndtmp-1
                        call drd(ltyp2,lrtyp2,lcon2,
     $                    nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $                    nptrs,0,lun11)
                        iltmp=idat1(np1i2+nidt2-2)
                        if (lpril.gt.1) write (lun11,*)nidt2,iltmp,ndtmp
                        ndtmp=npnxt(ndtmp)     
                        if (ndtmp.le.0) stop 'ndtmp error'           
                        enddo
c                     NB fix to excited level PI and rec
                      ett=ett+rdat1(np1r2)
                      eth=ett
                      if (lpril.gt.1)
     $                  write (lun11,*) ndtmp,iltmp,idest2,ett
c                     label for lower level
                      ktmp20=kblnk20
                      do lk=1,nkdt2
                        write (ktmp20(lk:lk),'(a1)')kdat1(np1k2+lk-1)
                        enddo
                      klevu=ktmp20
                      endif
c
c                   other data
                    mmlv=npilev(idest1,jkk)
                    write (lun11,9293)kkkl,mmlv,ktmp8,idest1,idest2,
     $                  klevl,klevu,eth,elumab(1,kkkl),elumab(2,kkkl)
c
c                   done with this rrc
                    endif
c
c                 end of loop over rrcs
                  ml=npnxt(ml)
                  if (ml.ne.0) mlpar=npar(ml)
                  enddo
c
c               end of test for element
                endif
c
C             Go to next ion
              mlion=npnxt(mlion)
              enddo
c
c         end of test for non-zero element abund
          endif
c
        mlel=npnxt(mlel)
C       Go to next element
        enddo
c
      write (lun11,993)
c
      return
c
c
 23   continue
c
      lpril=0
c     print 500 strongest absoprtion lines
      write (lun11,*)'line depths'
      kltmpo=0
      nlplmx=500
      eliml=0.1
      elimh=1.0e10
c     find the strongest lines.
      do  lm=1,nlplmx
        kltmp(lm)=0
        enddo
c
c     step through lines
      nlpl=1
      do lnn=1,nlsvn
c
c       get line data
        ln=lnn
        ml=nplin(ln)
        mlm=ml-1
        call drd(ltyp,lrtyp,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $    nptrs,0,lun11)
        elin=abs(rdat1(np1r))
c
c       exclude rate type 14
        if ((lrtyp.ne.14).and.(lrtyp.ne.9)) then
c
c         get ion data
          nilin=npar(ml)
          mlm=nilin-1
          call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
          nilin2=idat1(np1i+nidt-1)
c
c         get lum and test for strength, wavelength
          elmmtpp=tau0(1,ln)
          if (lpril.ne.0)
     $       write (lun11,*)lnn,elin,nilin,elmmtpp,ln,ml
          if ((ln.gt.0).and.(ln.lt.nnnl)
     $       .and.(elin.ge.eliml).and.(elin.le.elimh)
     $       .and.(elin.le.8.9e+6)
     $       .and.(elmmtpp.gt.1.e-36)
     $       .and.(nilin2.gt.0).and.(nilin2.le.nni))
     $        then
c
c           insertion sort
            lmm=0
            elcomp=1.e+10
            do while ((lmm.lt.nlpl).and.(elmmtpp.lt.elcomp))
              lmm=lmm+1
              kl2=kltmp(lmm)
              elcomp=0.
              if (kl2.gt.0)
     $          elcomp=tau0(1,kl2)
              enddo
            if (lpril.ne.0)
     $       write (lun11,8516)ln,elin,elmmtpp,lmm,nlpl,kl2,elcomp
            kltmpo=ln
            do  k=lmm,min(nlplmx,nlpl)
              if ((lpril.ne.0).and.(kltmp(k).ne.0))
     $          write (lun11,*)'in 557 loop',k,kltmp(k),kltmpo
              kltmpn=kltmp(k)
              kltmp(k)=kltmpo
              kltmpo=kltmpn
              enddo
           nlpl=min(nlplmx,nlpl+1)
           if (lpril.ne.0)
     $       write (lun11,*)'done with 557 loop',lm
            endif
c           end of insertion
c
          endif
c
        enddo
c
      if (nlpl.gt.0) kltmp(nlpl)=kltmpo
c
c     printing loop
      write (lun11,959)
 959  format (1x,'index, ion, wavelength, reflected, transmitted')
c
c     step through lines
      do  kk=1,nlpl
        if (lpril.ne.0)
     $    write (lun11,*)'kk=',kk
        ln=kltmp(kk)
        if (ln.ne.0) then
c
c         get line data
          ml=nplin(ln)
          if (ml.ne.0) then
            if (lpril.ne.0)
     $      write (lun11,*)'   ',ln,ml
            mlm=ml-1
            call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $        nptrs,0,lun11)
            elin=abs(rdat1(np1r))
c
c           get ion data
            nilin=npar(ml)
            mlm=nilin-1
            call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $        nptrs,0,lun11)
            do mm=1,nkdt
              kdtmp(mm)=kdat1(np1k-1+mm)
              enddo
            do mm=nkdt+1,9
              kdtmp(mm)=kblnk
              enddo
c            nilin=idat1(np1i+2)
            if (lpril.ne.0)
     $      write (lun11,*)ml,nilin,npar(ml)
c
c           print
            write (lun11,9955)kk,ln,(kdtmp(mm),mm=1,9),elin,
     $      tau0(1,ln),tau0(2,ln)
c
            endif
c
          endif
c
        enddo
c
      write (lun11,993)
c
      return
c
c
 24   continue
c
      lpril=0
c     print 500 strongest absorption edges
      write (lun11,*)'absorption edge depths'
      write (lun11,*)'index, ion, level, energy (eV), depth '
c
C     lpril is flag for printing debug information
      if (lpril.ne.0) then
        write (lun11,*)'raw data'
        do j=1,nnml
          if (xilev(j).gt.1.e-37)
     $     write (lun11,*)j,xilev(j),tauc(1,j)
          enddo
        endif
c
C     First look for element data (jk is element index)        
      klel=11
      mlel=npfirst(klel)
      jk=0
      kk=0
      jkk=0
c
c     step through elements
      do while (mlel.ne.0)
c
c       get element data
        jk=jk+1
        mt2=mlel-1
        call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mt2,
     $        nptrs,0,lun11)
        mllel=idat1(np1i+nidt-1)
        xeltp=rdat1(np1r)
        xeltp=abel(mllel)
        nnz=idat1(np1i)
        if (lpril.ge.1)
     $        write (lun11,*)'element:',jk,mlel,mllel,nnz,
     $                  (kdat1(np1k-1+mm),mm=1,nkdt)
c
C       ignore if the abundance is small
        if (xeltp.lt.1.e-10) then
            jkk=jkk+nnz
          else
c
c           now step thru ions (jkk is ion index)
            klion=12
            mlion=npfirst(klion)
            jkk=0
            kl=0
            do while ((mlion.ne.0).and.(kl.lt.nnz))
              jkk=jkk+1
c
C             retrieve ion name from kdati
              mlm=mlion-1
              call drd(ltyp,lrtyp,lcon,
     $            nrdt,np1r,nidti,np1i,nkdti,np1ki,mlm,
     $            nptrs,0,lun11)
c
C             if not accessing the same element, skip to the next element
              mlleltp=idat1(np1i+nidti-2)
              if (mlleltp.eq.mllel) then
c
                kl=kl+1
                if (lpril.ge.1)
     $            write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $                        (kdat1(np1ki+mm-1),mm=1,nkdti)
c
c               now find level data
                call func2l(jkk,lpril,lun11,t,xee,xpx,
     $              idat1,rdat1,kdat1,nptrs,
     $              npar,npnxt,npfi,
     $              rniss,rlev,ilv,
     $              nlpt,iltp,nlev,klev)
c
c               now step through rate type 7 data
                mltype=7
                ml=npfi(mltype,jkk)
                mllz=0
                if (ml.ne.0) mllz=npar(ml)
                mlpar=0
                if (ml.ne.0) mlpar=npar(ml)
                do while ((ml.ne.0).and.(mlpar.eq.mllz))
c
c                 get rrc data
                  kkkl=npconi2(ml)
                  if (lpril.ne.0) write (lun11,*)kkkl,ml,idest1,
     $                    elumab(1,kkkl),elumab(2,kkkl)
c
c                 test for non-zero rrc data
                  if ((kkkl.gt.0).and.(kkkl.le.ndat2)
     $                .and.((elumab(1,kkkl).gt.1.e-36)
     $                .or.(elumab(2,kkkl).gt.1.e-36))) then
c
c                   get rrc  data
                    mlm=ml-1
                    call drd(ltyp,lrtyp,lcon,
     $                nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $                nptrs,0,lun11)
                    idest1=idat1(np1i+nidt-2)
                    nlevp=nlev
                    idest2=nlevp+idat1(np1i-1+nidt-3)-1
c
c                   label for lower level
                    do lk=1,20
                      write (ktmp20(lk:lk),'(a1)')klev(lk,idest1)
                      enddo
                    klevl=ktmp20
c
c                   label for upper level
                    write (ktmp20(1:20),'(a20)')'continuum           '
                    klevu=ktmp20
c
c                   ion label
                    do lk=1,nkdti
                      write (ktmp8(lk:lk),'(a1)')kdat1(np1ki+lk-1)
                      enddo
                    do lk=nkdti+1,8
                      write (ktmp8(lk:lk),'(a1)')kblnk
                      enddo
c
                    eth=rlev(4,idest1)-rlev(1,idest1)
                    ett=eth
c
c                   get upper level data
                    if (idest2.gt.nlevp) then
                      jkk3=jkk+1
                      if (lpril.gt.1)
     $                  write (lun11,*)jkk3,ndtmp,nlevp,idest2
                      ndtmp=npfi(13,jkk3)
                      if (lpril.gt.1)
     $                  write (lun11,*)jkk3,ndtmp,nlevp,idest2
                      if (ndtmp.le.0) stop 'ndtmp error'
                      mllz=npar(ndtmp)
                      iltmp=0
                      do while ((ndtmp.ne.0).and.
     $                    (iltmp.ne.(idest2-nlevp+1)).and.
     $                    (npar(ndtmp).eq.mllz)) 
                        mlm=ndtmp-1
                        call drd(ltyp2,lrtyp2,lcon2,
     $                    nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $                    nptrs,0,lun11)
                        iltmp=idat1(np1i2+nidt2-2)
                        if (lpril.gt.1) write (lun11,*)nidt2,iltmp,ndtmp
                        ndtmp=npnxt(ndtmp)     
                        if (ndtmp.le.0) stop 'ndtmp error'           
                        enddo
c                     NB fix to excited level PI and rec
                      ett=ett+rdat1(np1r2)
                      eth=ett
                      if (lpril.gt.1)
     $                  write (lun11,*) ndtmp,iltmp,idest2,ett
c                     label for lower level
                      ktmp20=kblnk20
                      do lk=1,nkdt2
                        write (ktmp20(lk:lk),'(a1)')kdat1(np1k2+lk-1)
                        enddo
                      klevu=ktmp20
                      endif
c
c                   other data
                    mmlv=npilev(idest1,jkk)
                    write (lun11,9293)kkkl,mmlv,ktmp8,idest1,idest2,
     $                  klevl,klevu,eth,tauc(1,kkkl),tauc(2,kkkl)
 9293               format(1x,2i6,1x,a8,2i6,1x,2(a20,1x),3(1pe11.3))
c
c                   done with this rrc
                    endif
c
c                 end of loop over rrcs
                  ml=npnxt(ml)
                  if (ml.ne.0) mlpar=npar(ml)
                  enddo
c
c               end of test for element
                endif
c
C             Go to next ion
              mlion=npnxt(mlion)
              enddo
c
c         end of test for non-zero element abund
          endif
c
        mlel=npnxt(mlel)
C       Go to next element
        enddo
c
      return
c
c
 2    continue
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C     Print list of input parameters
c
c
c
      write (lun11,'(1x)')
      write (lun11,*)'input parameters:'
      write (lun11,*)'covering fraction=',cfrac
      write (lun11,*)'temperature (/10**4K)=',t
      write (lun11,*)'constant pressure switch (1=yes, 0=no)=',1-lcdd
      write (lun11,*)'pressure (dyne/cm**2)=',p
      write (lun11,*)'density (cm**-3)=',xpx
      write (lun11,*)'spectrum type=',spectype
      write (lun11,*)'spectrum file=',specfile
      write (lun11,*)'spectrum units? (0=energy, 1=photons)',specunit
      write (lun11,*)'radiation temperature or alpha=',tp
      write (lun11,*)'luminosity (/10**38 erg/s)=',xlum
      write (lun11,*)'column density (cm**-2)=',xpxcol
      write (lun11,*)'log(ionization parameter)=',zeta
      do j=1,nl
         write (lun11,*)kabstring(j),abel(j)
         enddo
      write (lun11,*)'model name=',kmodelname
      write (lun11,*)'number of steps=',numrec
      write (lun11,*)'number of iterations=',nlimd
      write (lun11,*)'write switch (1=yes, 0=no)=',lwri
      write (lun11,*)'print switch (1=yes, 0=no)=',lpri
      write (lun11,*)'step size choice switch=',lfix
      write (lun11,*)'loop control (0=standalone)=',nloopctl
      write (lun11,*)'number of passes=',npass
      write (lun11,*)'emult=',emult
      write (lun11,*)'taumax=',taumax
      write (lun11,*)'xeemin=',xeemin
      write (lun11,*)'critf=',critf
      write (lun11,*)'vturbi=',vturbi
      write (lun11,*)'ncn2=',ncn2
      write (lun11,*)'radexp=',radexp
      write (lun11,'(1x)')
      write (lun11,993)
      return

c      r19=r*(1.e-19)
c      uu1=enlum/(12.56*xpx*r19*r19)/3.e+10
c      alguu1=log10(max(1.e-24,uu1))
c      skse=xlum/(xpx*r19*r19)
c      zeta=log10(max(1.e-24,skse))
c      ecc=2.998e+10
c      ekt=t*(0.861707)*ergsev
c      sksec=skse/12.56/((1.+xee)*ekt*ecc)
c      zetac=log10(max(1.e-24,sksec))
c      enn0=xpx
c      nlyc=nbinc(13.7,epi,ncn2)
c      nry=nlyc+1
c      egam=zremsz(nry)/(2.*12.56*enn0*ecc*r19*r19+1.e-24)
c      nry=nbinc(13.6,epi,ncn2)+1
c      write (lun11,993)
c      write (lun11,*)'input parameters:'
c      write (lun11,*)'continuum luminosity=',xlum
c      write (lun11,*)'pressure or density=',xpx
c      write (lun11,*)'radius=',r
c      write (lun11,*)'ionization parameter=',skse
c      write (lun11,993)
c      write (tmpst,993)
c      call xwrite(tmpst,10)
c      write (tmpst,*)'input parameters:'
c      call xwrite(tmpst,10)
c      write (tmpst,*)'continuum luminosity=',xlum
c      call xwrite(tmpst,10)
c      write (tmpst,*)'pressure or density=',xpx
c      call xwrite(tmpst,10)
c      write (tmpst,*)'radius=',r
c      call xwrite(tmpst,10)
c      write (tmpst,*)'ionization parameter=',skse
c      call xwrite(tmpst,10)
c      write (tmpst,993)
c      call xwrite(tmpst,10)


3     continue
c
C     Write the parameter list to the FITS file
C     When changing this list, make sure nparms, parname, partype,
C     and parcomm are also properly updated.  Watch out for the
C     model name which is currently parcomm(37)
      parms(1)=cfrac
      parms(2)=t
      parms(3)=1-lcdd
      parms(4)=p
      parms(5)=xpx
      parms(6)=0.0
      parcomm(6)=spectype
      parms(7)=0.0
      parcomm(7)=specfile
      parms(8)=specunit
      parms(9)=tp
      parms(10)=xlum
      parms(11)=xpxcol
      parms(12)=zeta
      parms(13)=numrec
      parms(14)=nlimd
      parms(15)=lwri
      parms(16)=lpri
      parms(17)=lfix
      do j=1,nl
         parms(17+j)=abel(j)
         enddo
      parms(17+nl+1)=emult
      parms(17+nl+2)=taumax
      parms(17+nl+3)=xeemin
      parms(17+nl+4)=critf
      parms(17+nl+5)=vturbi
      parms(17+nl+6)=npass
      parms(17+nl+7)=0.0
      parcomm(17+nl+7)=kmodelname
      parms(17+nl+8)=nloopctl
c
      return
c
c
4     continue
c
c     print continuum opacity and emissivity
      write (lun11,*)
     $ 'continuum opacity and emissivities (/cm**3/sec/10**38)'
      write (lun11,*)'channel, energy,      opacity,    sigma*e**3,'
     $, 'scattered,  rec. in,   rec. out,  brem. em., source, bbe,',
     $'photon occ'
      opsum=0.
      tstar=t
      ekkr=xnx*(6.65e-25)
      ekkr=max(1.e-20,ekkr)
      optpp=max(opakc(1),ekkr)
      fstr=0.
      rsum1=0.
      rsum2=0.
      numcon=ncn2
c    
c     step thru continuum bins
      do 135 kl=2,numcon
c
c        sigma*e**3
         sgtmp=(opakc(kl)*(epi(kl)/1000.)**3)/max(1.e-24,xpx)
c
c        calculate sum and rosseland mean
         if ((kl.gt.1).and.(epi(kl).gt.100.))
     $    opsum=opsum+(opakc(kl)+opakc(kl-1))*(epi(kl)-epi(kl-1))/2.
         i=kl
         tmp = epi(i)*1.16/tstar
         crayj = 1./tmp
         fstro = fstr
         if ( tmp.le.50. ) then
            if ( tmp.gt.1.e-4 ) crayj = 1./(exp(tmp)-1.)
            crayj=crayj*crayj
c            fstr= cconst*tmp*crayj*epi(i)**3/tstar
            fstr= tmp*crayj*epi(i)**3/tstar
         endif
         optppo=optpp
         optpp=max(opakc(kl),ekkr)
         delte=epi(kl)-epi(kl-1)
         rsum1=min(1.e+20,rsum1+(fstr/optpp+fstro/optppo)*delte/2.)
         rsum2=min(1.e+20,rsum2+(fstr+fstro)*delte/2.)
c
c        source function
         rss=(rccemis(1,kl)+rccemis(2,kl)+brcems(kl)/12.56)/
     $           (1.e-34+opakc(kl))
c         rss=(rccemis(1,kl)+rccemis(2,kl))/(1.e-34+opakc(kl))
c
c        planck function
         bbe=2.*epi(kl)**3*(1.5642e+22)
     $       /(exp(epi(kl)/(0.861707*t))-1.+1.e-34)
c
c        photon occupation number
         rocc=rss/(bbe+1.e-34)
c
c        print
         write (lun11,967)kl,epi(kl),opakc(kl),sgtmp,opakscatt(kl),
     $            rccemis(1,kl),rccemis(2,kl), brcems(kl),rss,bbe,rocc
967      format (1h ,i6,10(1pe13.5))
c
135      continue
c
c     print summed opacities
      write (lun11,*)'opsum cont=',opsum
      rssmn=rsum2/rsum1
c      ens1=rssmn/(t*1.e+4)**(-3.5)/xpx/xpx/1.66e-24
      write (lun11,*)'rosseland mean opacity=',t,rssmn
      write (lun11,993)
c
      return
C
5     continue
c
c     print energy sums
      elsum=0.
      ergsev=1.602197e-12
      do jlk=1,nlsvn
         ln=jlk
         ml=nplin(ln)
         mlm=ml-1
         call drd(ltyp,lrtyp,lcon,
     $     nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $     nptrs,0,lun11)
         elin=abs(rdat1(np1r))
         if ((elin.lt.1.e+8).and.(elin.gt.1.)) then
           elsum=elsum+elum(1,ln)+elum(2,ln)
           endif
         enddo
      sumtmp1=0.
      sumtmp2=0.
      ergsev=1.602197e-12
      r19=r*1.e-19
      tmp1=zremsz(1)*(1.-exp(-dpthc(1,1)))
      tmp2=zrems(3,1)+zrems(2,1)
      do jk=2,ncn2
         tmp1o=tmp1
         tmp1=zremsz(jk)*(1.-exp(-dpthc(1,jk)))
         sumtmp1=sumtmp1+(tmp1+tmp1o)*(epi(jk)-epi(jk-1))*ergsev/2.
         tmp2o=tmp2
         tmp2=zrems(2,jk)+zrems(3,jk)
         sumtmp2=sumtmp2+(tmp2+tmp2o)*(epi(jk)-epi(jk-1))*ergsev/2.
         enddo
      err=(sumtmp1-sumtmp2-elsum)/(sumtmp1+1.e-24)
      write (lun11,9981)sumtmp1,sumtmp2,elsum,err
 9981 format (1x,'energy sums: abs, cont, line, err:',4(1pe13.5))
      write (lun11,993)
c      write (lun11,*),httot,cltot,fpr2dr,httot/(sumtmp1+1.e-24),
c     $  cltot/(elsum+sumtmp2+1.e-24)
c
      return
c
c
6     continue
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c     print continuum luminosities and depths
      write (lun11,*)'continuum luminosities (/sec/10**38) and depths'
      write (lun11,*)'channel,energy,inc.,trn. lum.,ref. lum.,'
     $,'scattered,backward depth,forward depth'
c
      numcon=ncn2
      sum1=0.
      sum2=0.
      sum3=0.
      sum4=0.
      ergsev=1.602197e-12
      r19=r*1.e-19
      fpr2=12.56*r19*r19
c
c     step thru continuum bins
      do kl=1,numcon
c
c       planck function
        bbe=2.*epi(kl)**3*(1.5642e+22)
     $       /(exp(epi(kl)/(0.861707*t))-1.+1.e-34)
c
c       photon occupation number
        rocc=zrems(1,kl)/(bbe+1.e-34)/fpr2/12.56
c
        write (lun11,968)kl,epi(kl),zremsz(kl),
     $    zrems(1,kl),zrems(2,kl),zrems(3,kl),zrems(4,kl),
     $    dpthc(1,kl),dpthc(2,kl),bbe,rocc
c
c       sums
        if (kl.gt.1) then
           sum1=sum1+(zremsz(kl)+zremsz(kl-1))
     $         *(epi(kl)-epi(kl-1))*ergsev/2.
           sum2=sum2+(zrems(1,kl)+zrems(1,kl-1))
     $         *(epi(kl)-epi(kl-1))*ergsev/2.
           sum3=sum3+(zrems(2,kl)+zrems(2,kl-1))
     $         *(epi(kl)-epi(kl-1))*ergsev/2.
           sum4=sum4+(zrems(3,kl)+zrems(3,kl-1))
     $         *(epi(kl)-epi(kl-1))*ergsev/2.
           endif
968      format (1h ,i6,10(1pe13.5))
c
         enddo
c
      write (lun11,*)'norms:'
      write (lun11,9698)sum1,sum2,sum3,sum4
 9698 format (20x,4(1pe13.5))
      write (lun11,993)
c
      return
c
c
8     continue
c
      
      lpril=0
      write (lun11,*)'line list'
      write (lun11,9943)
9943  format (1x,'     wave          element  ion  glo          gup   '
     $ ,'      fij')
c
c     step through lines
      nlpl=1
      do lnn=1,nlsvn
c
c       get line data
        ln=lnn
        ml=nplin(ln)
        mlm=ml-1
        call drd(ltyp,lrtyp,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $    nptrs,0,lun11)
        elin=abs(rdat1(np1r))
c
c
c       exclude rate type 14
        if ((lrtyp.ne.14).and.(abs(elin).gt.0.1).and.(lrtyp.ne.9)
     $       .and.(abs(elin).lt.9.e+9)) then
c
          ergsev=1.602197e-12
          ener=ergsev*(12398.41)/max(elin,1.e-24)
          etst=ener/ergsev
          idest1=idat1(np1i)
          idest2=idat1(np1i+1)
          aij=rdat1(np1r+2)
          if (ltyp.eq.82) aij=rdat1(np1r+3)
c
c         get ion data
          nilin=npar(ml)
          mlm=nilin-1
          call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
          do ktt=1,min(8,nkdt)
            write (kinam1(ktt:ktt),'(a1)')kdat1(np1k-1+ktt)
            enddo
          do ktt=nkdt+1,9
            write (kinam1(ktt:ktt),'(a1)')kblnk
            enddo
c
c          if (lpri.ge.1)
c     $      write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
c     $          (kdat1(np1ki+mm-1),mm=1,nkdti)
c
c         now find level data
          jkk=idat1(np1i+nidt-1)
          call func2l(jkk,lpril,lun11,t,xee,xpx,
     $              idat1,rdat1,kdat1,nptrs,
     $              npar,npnxt,npfi,
     $              rniss,rlev,ilv,
     $              nlpt,iltp,nlev,klev)

          ggup=rlev(2,idest1)
          gglo=rlev(2,idest2)
          do lk=1,20
            klablo(lk)=klev(lk,idest1)
            klabup(lk)=klev(lk,idest2)
            enddo
          flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
          write (lun11,9944)lnn,elin,kinam1,aij,flin,gglo,ggup,
     $             klablo,klabup
9944      format (1h ,i9,e12.4,1x,a9,4(1pe12.4),1x,20a1,1x,20a1)
c
          endif
        enddo
      write (lun11,993)
c
      return
c
c
10    continue
c
      write (lun11,*)'ion abundances and  rates (/sec)'
      write (lun11,947)
947   format (1x,'index, ion, abundance, recombination, ionization,')
c
c     step thru ions
      klion=12
      mlion=npfirst(klion)
      lk=0
      do while (mlion.ne.0) 
c
c        get ion data
         lk=lk+1
         ltyp=klion
         mlm=mlion-1
         call drd(ltyp,lrtyp,lcon,
     $     nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $     nptrs,0,lun11)
         do mm=1,nkdt
           kdtmp(mm)=kdat1(np1k-1+mm)
           enddo
         do mm=nkdt+1,9
           kdtmp(mm)=kblnk
           enddo
c
c        get element data
         nell=npar(mlion)
         mlm=nell-1
         call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
c         write (lun11,*)mlion,lk,np1i,nidt,np1i+nidt-1,
c     $      idat1(np1i+nidt-1),ababs(idat1(np1i+nidt-1)),mlm
         if ((idat1(np1i+nidt-1).gt.0)
     $     .and.(idat1(np1i+nidt-1).le.nl)) then
           abundel=ababs(idat1(np1i+nidt-1))
c
c          print out
           if (abundel.gt.1.e-15)
     $      write (lun11,9046)lk,(kdtmp(mm),mm=1,9),
     $      xii(lk),rrrt(lk),pirt(lk)
9046       format (1x,i4,1x,9a1,5(1pe16.8))
c
           endif
c
         mlion=npnxt(mlion)
         enddo
c
      write (lun11,*)'heating and cooling rates (erg/sec)'
      write (lun11,9947)
9947  format (1x,'index, element, heating, cooling: ')
c
c     step thru elements
      klel=11
      mlel=npfirst(klel)
      lk=0
      do while (mlel.ne.0) 
c
c        get element data
         lk=lk+1
         ltyp=klel
         mlm=mlel-1
         call drd(ltyp,lrtyp,lcon,
     $     nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $     nptrs,0,lun11)
         do mm=1,nkdt
           kdtmp(mm)=kdat1(np1k-1+mm)
           enddo
         do mm=nkdt+1,9
           kdtmp(mm)=kblnk
           enddo
c
         if ((idat1(np1i+nidt-1).gt.0)
     $     .and.(idat1(np1i+nidt-1).le.nl)) then
c
           abundel=ababs(idat1(np1i+nidt-1))
           if (abundel.gt.1.e-15)
     $      write (lun11,9046)lk,(kdtmp(mm),mm=1,9),htt(lk),cll(lk)
c
           endif
c
         mlel=npnxt(mlel)
         enddo
c
      write (lun11,*)'total heating, cooling:',
     $            httot,cltot
      write (lun11,*)'partial heating rates: photo,compton',
     $            httot-htcomp,htcomp
      write (lun11,*)'partial cooling rates: rec,lines,brems,compton',
     $            clcont,cllines,clcomp,clbrems
      write (lun11,993)
c
c
      return
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Write FITS file with summary of ion abundances
C     by radial zone
C
11    continue
c
      knam='xout_abund1.fits'
      call fheader(unit,knam,atcredate,kmodelname,status)

c      write (lun11,*)'in pprint, 11: numrec=',numrec
c      call writeimage(knam)
      do mm=1,999
        kunits(mm)=kblnk16
        klabs(mm)=kblnk16
        kform(mm)=kblnk16
        enddo
      klabs(1)='radius          '
      kform(1)='E11.3'
      kunits(1)='cm'
      klabs(2)='delta_r         '
      kform(2)='E11.3'
      kunits(2)='cm'
      klabs(3)='ion_parameter   '
      kform(3)='E11.3'
      kunits(3)='erg*cm/s'
      klabs(4)='x_e             '
      kform(4)='E11.3'
      kunits(4)=' '
      klabs(5)='n_p             '
      kform(5)='E11.3'
      kunits(6)='cm**(-3)'
      klabs(6)='pressure        '
      kform(6)='E11.3'
      kunits(6)='dynes/cm**2'
      klabs(7)='temperature     '
      kform(7)='E11.3'
      kunits(7)='10**4 K'
      klabs(8)='frac_heat_error'
      kform(8)='E11.3'
      kunits(8)=' '
C     Search for the ion names in the database
      klion=12
      mlion=npfirst(klion)
      do lkk=1,nni
        xcoltmp(lkk)=0.
        enddo
      lk=0
      do while (mlion.ne.0) 
           lk=lk+1
           ltyp=klion
           mlm=mlion-1
           call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
c
c          get element abundance
           nelin=npar(mlion)
           ml=nelin
           mlm=ml-1
           call drd(ltyp,lrtyp,lcon,
     $       nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $       nptrs,0,lun11)
           mllel=idat1(np1i+nidt-1)
           xeltp=ababs(mllel)
c
c          go back to ion data
           mlm=mlion-1
           call drd(ltyp,lrtyp,lcon,
     $       nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $       nptrs,0,lun11)

C          Compute string length from character array by searching backwards
C          for first non-blank character
           klen=0
           do mm=1,nkdt
             kdat(mm)=kdat1(np1k-1+mm)
             enddo
           do mm=nkdt+1,9
             kdat(mm)=kblnk
             enddo
           do mm=1,9
             if(kdat(10-mm).ne.' '.and.klen.eq.0) then
                klen=10-mm
             endif
             enddo
c           write (lun11,*)'kdat:',(kdat(mm),mm=1,9)
c           write (lun11,*)'klen:',klen
C          Replace ' ' in ion names to '_' to match FITS standard
           do mm=1,9
             if(kdat(mm).eq.' '.and.mm.lt.klen) then
                write (ktmp(mm:mm),'(a1)')'_'
             else
                write (ktmp(mm:mm),'(a1)')kdat(mm)
             endif
             enddo
           do mm=10,16
             write (ktmp(mm:mm),'(a1)')' '
             enddo
           do jkl=2,numrec
c             write (lun11,*)jkl,lk,zrtmp(2,jkl),zrtmp(8+lk,jkl),
c     $                             zrtmp(5,jkl),xeltp,xcoltmp(lk)
             xcoltmp(lk)=xcoltmp(lk)
     $         +(zrtmp(8+lk,jkl)*zrtmp(5,jkl)
     $             +zrtmp(8+lk,jkl-1)*zrtmp(5,jkl-1))
     $         *(zrtmp(2,jkl)-zrtmp(2,jkl-1))*xeltp/2.
             enddo
           klabs(8+lk)=ktmp
           kform(8+lk)='E11.3'
           kunits(8+lk)=' '
           mlion=npnxt(mlion)
           enddo
c
      call fwrtascii(unit,'ABUNDANCES',zrtmp,8+lk,
     $                  numrec,klabs,kform,kunits,lun11)
c
c     calculate columns
      numrec=1
      do lkk=1,lk
        zrtmpcol(8+lkk,numrec)=xcoltmp(lkk)
        enddo
      do lkk=1,8
        zrtmpcol(lkk,numrec)=0.
        enddo
c
      call fwrtascii(unit,'COLUMNS   ',zrtmpcol,8+lk,
     $                  numrec,klabs,kform,kunits,lun11)
c
      klel=11
      mlel=npfirst(klel)
      lk=0
      do while (mlel.ne.0) 
           lk=lk+1
           ltyp=klel
           mlm=mlel-1
           call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
c
C          Compute string length from character array by searching backwards
C          for first non-blank character
           klen=0
           do mm=1,nkdt
             kdat(mm)=kdat1(np1k-1+mm)
             enddo
           do mm=nkdt+1,9
             kdat(mm)=kblnk
             enddo
           do mm=1,9
             if(kdat(10-mm).ne.' '.and.klen.eq.0) then
                klen=10-mm
             endif
             enddo
           do mm=1,9
             if(kdat(mm).eq.' '.and.mm.lt.klen) then
                write (ktmp(mm:mm),'(a1)')'_'
             else
                write (ktmp(mm:mm),'(a1)')kdat(mm)
             endif
             enddo
           do mm=10,16
             write (ktmp(mm:mm),'(a1)')' '
             enddo
           klabs(8+lk)=ktmp
           kform(8+lk)='E11.3'
           kunits(8+lk)=' '
           mlel=npnxt(mlel)
           enddo
      klabs(8+lk+1)='compton'
      kform(8+lk+1)='E11.3'
      kunits(8+lk+1)=' '
      klabs(8+lk+2)='total'
      kform(8+lk+2)='E11.3'
      kunits(8+lk+2)=' '
      call fwrtascii(unit,'HEATING                                   ',
     $ zrtmph,8+lk+2,numrec,klabs,kform,kunits,lun11)
c
      klabs(8+lk+1)='compton'
      kform(8+lk+1)='E11.3'
      kunits(8+lk+1)=' '
      klabs(8+lk+2)='brems'
      kform(8+lk+2)='E11.3'
      kunits(8+lk+2)=' '
      klabs(8+lk+3)='total'
      kform(8+lk+3)='E11.3'
      kunits(8+lk+3)=' '
      call fwrtascii(unit,'COOLING                                    ',
     $ zrtmpc,8+lk+3,numrec,klabs,kform,kunits,lun11)
c
      call fitsclose(lun11,unit,status)
c
      return
c
c
12    continue
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Add ionic abundances info in this radial zone to array for
C     eventual inclusion in xout_abund1.fits
C     Modifies zrtmp
C
      ergsev=1.602197e-12
      r19=r*(1.e-19)
c      write (lun11,*)enlum,xpx,r,xlum,jkstep
      uu1=enlum/(12.56*xpx*r19*r19)/3.e+10
c      write (lun11,*)uu1
      alguu1=log10(max(1.e-24,uu1))
      skse=xlum/(xpx*r19*r19)
      zeta=log10(max(1.e-24,skse))
      ecc=2.998e+10
      ekt=t*(0.861707)*ergsev
c      sksec=skse/(12.56*((1.+xee)*ekt+pradl/(1.e-24+xpx))*ecc)
      sksec=skse/12.56/((1.+xee)*ekt*ecc)
      zetac=log10(max(1.e-24,sksec))
      enn0=xpx
      nlyc=nbinc(13.7d0,epi,ncn2)
      nry=nlyc+1
      egam=zremsz(nry)/(2.*12.56*enn0*ecc*r19*r19+1.e-24)
      nry=nbinc(13.6d0,epi,ncn2)+1
C     Copy the values for radial zone jkstep
      if (jkstep.gt.3999) return
      zrtmp(1,jkstep)=r
      zrtmp(2,jkstep)=rdel
      zrtmp(3,jkstep)=zeta
      zrtmp(4,jkstep)=xee
      zrtmp(5,jkstep)=xpx
      zrtmp(6,jkstep)=p
      zrtmp(7,jkstep)=t
      zrtmp(8,jkstep)=hmctot
      do lk=1,8
        zrtmpc(lk,jkstep)=zrtmp(lk,jkstep)
        zrtmph(lk,jkstep)=zrtmp(lk,jkstep)
        enddo
      klion=12
      mlion=npfirst(klion)
      lk=0
      do while (mlion.ne.0)
        lk=lk+1
        zrtmp(8+lk,jkstep)=xii(lk)
        mlion=npnxt(mlion)
        enddo
      klel=11
      mlel=npfirst(klel)
      lk=0
      do while (mlel.ne.0)
        lk=lk+1
        zrtmpc(8+lk,jkstep)=htt(lk)
        zrtmph(8+lk,jkstep)=cll(lk)
        mlel=npnxt(mlel)
        enddo
      zrtmph(8+lk+1,jkstep)=htcomp
      zrtmph(8+lk+2,jkstep)=httot
      zrtmpc(8+lk+1,jkstep)=clcomp
      zrtmpc(8+lk+2,jkstep)=clbrems
      zrtmpc(8+lk+3,jkstep)=cltot
c
      return
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
c
13    continue
c
993   format (1h )
c
      return
c
c
14    continue
c
      write (lun11,900)
900   format ('line opacities and emissivities',
     $ ' (erg/cm**3/sec/10**38)')
      write (lun11,915)
915   format (1x,'index,wavelength,energy,ion,opacity,rec. em.,',
     $'coll. em.,fl. em.,di. em.,cx. em.')
c
c     step through lines
      nlpl=1
      write (lun11,*)nlsvn
      do lnn=1,nlsvn
c
c       get line data
        ln=lnn
        ml=nplin(ln)
        mlm=ml-1
        call drd(ltyp,lrtyp,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $    nptrs,0,lun11)
        elin=abs(rdat1(np1r))
        if ((lrtyp.ne.14).and.(lrtyp.ne.9).and.(abs(elin).gt.0.1)
     $       .and.(abs(elin).lt.9.e+9)) then

          ergsev=1.602197e-12
          ener=ergsev*(12398.41)/max(elin,1.e-24)
          etst=ener/ergsev
          idest1=idat1(np1i)
          idest2=idat1(np1i+1)
          aij=rdat1(np1r+2)
c
c         get ion data
          nilin=npar(ml)
          mlm=nilin-1
          call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
           do ktt=1,min(8,nkdt)
            write (kinam1(ktt:ktt),'(a1)')kdat1(np1k-1+ktt)
            enddo
          do ktt=nkdt+1,9
            write (kinam1(ktt:ktt),'(a1)')kblnk
            enddo
c
          j=ln
          write (lun11,904)j,elin,etst,kinam1,oplin(j),rcem(1,j),
     $                      rcem(2,j)
904       format (1h ,i9,2(1pe13.5),1x,a9,6(1pe13.5))
c
          endif
        enddo
      write (lun11,993)
c
      return
c
c
 15   continue
c
      write (lun11,*)'line luminosities (erg/sec/10**38) and depths'
      write (lun11,9923)
9923  format (1x,' line, wavelength, ion, ref. lum.,trn. lum.,',
     $'backward depth, forward depth')
c     step through lines
      nlpl=1
      do lnn=1,nlsvn
c
c       get line data
        ln=lnn
        ml=nplin(ln)
        mlm=ml-1
        call drd(ltyp,lrtyp,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $    nptrs,0,lun11)
        elin=abs(rdat1(np1r))
        if ((lrtyp.ne.14).and.(lrtyp.ne.9).and.(abs(elin).gt.0.1)
     $       .and.(abs(elin).lt.9.e+9)) then

          elin=abs(rdat1(np1r))
          ergsev=1.602197e-12
          ener=ergsev*(12398.41)/max(elin,1.e-24)
          etst=ener/ergsev
          idest1=idat1(np1i)
          idest2=idat1(np1i+1)
          aij=rdat1(np1r+2)
c
c         get ion data
          nilin=npar(ml)
          mlm=nilin-1
          call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
          do ktt=1,min(8,nkdt)
            write (kinam1(ktt:ktt),'(a1)')kdat1(np1k-1+ktt)
            enddo
          do ktt=nkdt+1,9
            write (kinam1(ktt:ktt),'(a1)')kblnk
            enddo
c
          j=ln
          elmtp=elum(1,j)
          elmtpb=elum(2,j)
          write (lun11,9924)j,elin,kinam1,
     $     elmtp,elmtpb,tau0(1,j), tau0(2,j)
9924      format (1h ,i9,1pe13.5,1x,a9,1x,4(1pe13.5))
c
          endif
        enddo
      write (lun11,993)
c
      return
c
c
 18   continue
c
      lpril=0
      write (lun11,*)'line wavelengths and levels'
c     step through lines
      nlpl=1
      do lnn=1,nlsvn
c
c       get line data
        ln=lnn
        ml=nplin(ln)
        mlm=ml-1
        call drd(ltyp,lrtyp,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $    nptrs,0,lun11)
        elin=abs(rdat1(np1r))
c
c       exclude rate type 14
        if ((lrtyp.ne.14).and.(lrtyp.ne.9).and.(abs(elin).gt.0.1)
     $       .and.(abs(elin).lt.9.e+9)) then
c
          ergsev=1.602197e-12
          ener=ergsev*(12398.41)/max(elin,1.e-24)
          etst=ener/ergsev
          idest1=idat1(np1i)
          idest2=idat1(np1i+1)
          aij=rdat1(np1r+2)
c
c         get ion data
          nilin=npar(ml)
          mlm=nilin-1
          call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
          do ktt=1,min(8,nkdt)
            write (kinam1(ktt:ktt),'(a1)')kdat1(np1k-1+ktt)
            enddo
          do ktt=nkdt+1,9
            write (kinam1(ktt:ktt),'(a1)')kblnk
            enddo
c
          if (lpril.ge.1)
     $      write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $          (kdat1(np1ki+mm-1),mm=1,nkdti)
c
c         now find level data
          jkk=idat1(np1i+nidt-1)
          call func2l(jkk,lpril,lun11,t,xee,xpx,
     $              idat1,rdat1,kdat1,nptrs,
     $              npar,npnxt,npfi,
     $              rniss,rlev,ilv,
     $              nlpt,iltp,nlev,klev)

          ggup=rlev(2,idest1)
          gglo=rlev(2,idest2)
          do lk=1,20
            klablo(lk)=klev(lk,idest1)
            klabup(lk)=klev(lk,idest2)
            enddo
          flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
          ilevlo=idest1
          ilevup=idest2
c
          j=ln
          write (lun11,9929)j,elin,kinam1,
     $      (klev(mm,ilevlo),mm=1,20),(klev(mm,ilevup),mm=1,20),
     $      rlev(1,ilevlo),rlev(1,ilevup),rlev(2,ilevlo),rlev(2,ilevup),
     $      rlev(3,ilevlo),rlev(3,ilevup),
     $      ilv(1,ilevlo),ilv(1,ilevup),ilv(2,ilevlo),ilv(2,ilevup),
     $      ilv(3,ilevlo),ilv(3,ilevup)
 9929     format (1h ,i9,1pe13.5,1x,a9,1x,2(20a1,1x),6(1pe13.5),
     $          6i6)
c
          endif
        enddo
      write (lun11,993)
c
      return
c
c
 20   continue
c
      lpril=0
      write (lun11,*)'line finding list'
      do jlk=1,nlsvn
         j=jlk
         ml=nplin(j)
         mlm=ml-1
         call drd(ltyp,lrtyp,lcon,
     $     nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $     nptrs,0,lun11)
         elin=abs(rdat1(np1r))
         jpnt(j)=j
         elsv(j)=abs(elin)
         enddo
c     sort
      done=.false.
      niter=0
      do while (.not.done)
        niter=niter+1
        done=.true.
c        do jjj=1,100
        do jjj=1,nlsvn-1
          j=jpnt(jjj)
          jp1=jpnt(jjj+1)
c          write (lun11,*)jjj,j,jp1,
c     $           elsv(jp1),elsv(j)
          if (elsv(jp1).lt.elsv(j)) then
            jpnt(jjj)=jp1
            jpnt(jjj+1)=j
            done=.false.
            endif
          enddo
        enddo
c
c     print out sorted list
      do jlk=1,nlsvn
        j=jpnt(jlk)
        lnn=j
c
c       get line data
        ln=lnn
        ml=nplin(ln)
        mlm=ml-1
        call drd(ltyp,lrtyp,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $    nptrs,0,lun11)
c
c       exclude rate type 14
        if ((lrtyp.ne.14).and.(lrtyp.ne.9).and.(abs(elin).gt.0.1)
     $       .and.(abs(elin).lt.9.e+9)) then
c
          elin=abs(rdat1(np1r))
          ergsev=1.602197e-12
          ener=ergsev*(12398.41)/max(elin,1.e-24)
          etst=ener/ergsev
          idest1=idat1(np1i)
          idest2=idat1(np1i+1)
          aij=rdat1(np1r+2)
c
c         get ion data
          nilin=npar(ml)
          mlm=nilin-1
          call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
          do ktt=1,min(8,nkdt)
            write (kinam1(ktt:ktt),'(a1)')kdat1(np1k-1+ktt)
            enddo
          do ktt=nkdt+1,9
            write (kinam1(ktt:ktt),'(a1)')kblnk
            enddo
c
          if (lpril.ge.1)
     $      write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $          (kdat1(np1ki+mm-1),mm=1,nkdti)
c
c         now find level data
          jkk=idat1(np1i+nidt-1)
          call func2l(jkk,lpril,lun11,t,xee,xpx,
     $              idat1,rdat1,kdat1,nptrs,
     $              npar,npnxt,npfi,
     $              rniss,rlev,ilv,
     $              nlpt,iltp,nlev,klev)

          ggup=rlev(2,idest1)
          gglo=rlev(2,idest2)
          do lk=1,20
            klablo(lk)=klev(lk,idest1)
            klabup(lk)=klev(lk,idest2)
            enddo
          flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
          ilevlo=idest1
          ilevup=idest2
c
          write (lun11,9929)j,elin,kinam1,
     $      (klev(mm,ilevlo),mm=1,20),(klev(mm,ilevup),mm=1,20),
     $      rlev(1,ilevlo),rlev(1,ilevup),rlev(2,ilevlo),rlev(2,ilevup),
     $      rlev(3,ilevlo),rlev(3,ilevup),
     $      ilv(1,ilevlo),ilv(1,ilevup),ilv(2,ilevlo),ilv(2,ilevup),
     $      ilv(3,ilevlo),ilv(3,ilevup)
           endif
         enddo
      write (lun11,993)
c
      return
c
 21   continue
c
      lpril=0
      write (lun11,*)' level opacities and emissivities'
      write (lun11,*)'index,energy,ion,level,index,emiss in,emiss out,th
     $reshold opacity,absorbed energy,depth in, depth out'
c
C     First look for element data (jk is element index)        
      klel=11
      mlel=npfirst(klel)
      jk=0
      kk=0
      jkk=0
c
c     step through elements
      do while (mlel.ne.0)
c
c       get element data
        jk=jk+1
        mt2=mlel-1
        call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mt2,
     $        nptrs,0,lun11)
        mllel=idat1(np1i+nidt-1)
        xeltp=rdat1(np1r)
        xeltp=abel(mllel)
        nnz=idat1(np1i)
        if (lpril.ge.1)
     $        write (lun11,*)'element:',jk,mlel,mllel,nnz,
     $                  (kdat1(np1k-1+mm),mm=1,nkdt)
c
C       ignore if the abundance is small
        if (xeltp.lt.1.e-10) then
            jkk=jkk+nnz
          else
c
c           now step thru ions (jkk is ion index)
            klion=12
            mlion=npfirst(klion)
            jkk=0
            kl=0
            do while ((mlion.ne.0).and.(kl.lt.nnz))
              jkk=jkk+1
c
C             retrieve ion name from kdati
              mlm=mlion-1
              call drd(ltyp,lrtyp,lcon,
     $            nrdt,np1r,nidti,np1i,nkdti,np1ki,mlm,
     $            nptrs,0,lun11)
              ethi=rdat1(np1r)
c
C             if not accessing the same element, skip to the next element
              mlleltp=idat1(np1i+nidti-2)
              if (mlleltp.eq.mllel) then
c
                kl=kl+1
                if (lpril.ge.1)
     $            write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $                        (kdat1(np1ki+mm-1),mm=1,nkdti)
c
c               now find level data
                call func2l(jkk,lpril,lun11,t,xee,xpx,
     $              idat1,rdat1,kdat1,nptrs,
     $              npar,npnxt,npfi,
     $              rniss,rlev,ilv,
     $              nlpt,iltp,nlev,klev)
c
c               now step through rate type 7 data
                mltype=7
                ml=npfi(mltype,jkk)
                mllz=0
                if (ml.ne.0) mllz=npar(ml)
                mlpar=0
                if (ml.ne.0) mlpar=npar(ml)
                do while ((ml.ne.0).and.(mlpar.eq.mllz))
c
c                 get rrc data
                  kkkl=npconi2(ml)
                  if (lpril.ne.0) write (lun11,*)kkkl,ml,idest1,
     $                    elumab(1,kkkl),elumab(2,kkkl)
c
c                 test for non-zero rrc data
                  if ((kkkl.gt.0).and.(kkkl.le.ndat2)
     $                .and.((elumab(1,kkkl).gt.1.e-36)
     $                .or.(elumab(2,kkkl).gt.1.e-36))) then
c
c                   get rrc  data
                    mlm=ml-1
                    call drd(ltyp,lrtyp,lcon,
     $                nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $                nptrs,0,lun11)
                    idest1=idat1(np1i+nidt-2)
                    nlevp=nlev
                    idest2=nlevp+idat1(np1i-1+nidt-3)-1
c
c                   label for lower level
                    do lk=1,20
                      write (ktmp20(lk:lk),'(a1)')klev(lk,idest1)
                      enddo
                    klevl=ktmp20
c
c                   label for upper level
                    write (ktmp20(1:20),'(a20)')'continuum           '
                    klevu=ktmp20
c
c                   ion label
                    do lk=1,nkdti
                      write (ktmp8(lk:lk),'(a1)')kdat1(np1ki+lk-1)
                      enddo
                    do lk=nkdti+1,8
                      write (ktmp8(lk:lk),'(a1)')kblnk
                      enddo
c
                    eth=rlev(4,idest1)-rlev(1,idest1)
                    ett=eth
c
c                   get upper level data
                    if (idest2.gt.nlevp) then
                      jkk3=jkk+1
                      if (lpril.gt.1)
     $                  write (lun11,*)jkk3,ndtmp,nlevp,idest2
                      ndtmp=npfi(13,jkk3)
                      if (lpril.gt.1)
     $                  write (lun11,*)jkk3,ndtmp,nlevp,idest2
                      if (ndtmp.le.0) stop 'ndtmp error'
                      mllz=npar(ndtmp)
                      iltmp=0
                      do while ((ndtmp.ne.0).and.
     $                    (iltmp.ne.(idest2-nlevp+1)).and.
     $                    (npar(ndtmp).eq.mllz)) 
                        mlm=ndtmp-1
                        call drd(ltyp2,lrtyp2,lcon2,
     $                    nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $                    nptrs,0,lun11)
                        iltmp=idat1(np1i2+nidt2-2)
                        if (lpril.gt.1) write (lun11,*)nidt2,iltmp,ndtmp
                        ndtmp=npnxt(ndtmp)     
                        if (ndtmp.le.0) stop 'ndtmp error'           
                        enddo
c                     NB fix to excited level PI and rec
                      ett=ett+rdat1(np1r2)
                      eth=ett
                      if (lpril.gt.1)
     $                  write (lun11,*) ndtmp,iltmp,idest2,ett
c                     label for lower level
                      ktmp20=kblnk20
                      do lk=1,nkdt2
                        write (ktmp20(lk:lk),'(a1)')kdat1(np1k2+lk-1)
                        enddo
                      klevu=ktmp20
                      endif
c
c                   other data
                    mmlv=npilev(idest1,jkk)
                    cabcompare=bremsint(nbinc(ethc,epi,ncn2))
                    mlcu=kkkl
                    write (lun11,969)kkkl,mmlv,ktmp8,idest1,idest2,
     $                  klevl,klevu,eth,
     $                  cemab(1,mlcu),cemab(2,mlcu),opakab(mlcu),
     $                  cabab(mlcu),tauc(1,mlcu),tauc(2,mlcu)
 969                format (1x,2i6,1x,a8,1x,2i6,1x,2(a20,1x),
     $                  8(1pe13.5),2i6)
c
c                   done with this rrc
                    endif
c
c                 end of loop over rrcs
                  ml=npnxt(ml)
                  if (ml.ne.0) mlpar=npar(ml)
                  enddo
c
c               end of test for element
                endif
c
C             Go to next ion
              mlion=npnxt(mlion)
              enddo
c
c         end of test for non-zero element abund
          endif
c
        mlel=npnxt(mlel)
C       Go to next element
        enddo
c
      write (lun11,993)
c
      return
C
c
7     continue
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Write level populations
c
      write (lun11,9985)
9985  format (1x,' level populations ')
      write (lun11,9986)
 9986 format (1x,' ion                      level              '
     $,' e_exc population')

C     lpril is flag for printing debug information
      lpril=0
      if (lpril.ne.0) then
        write (lun11,*)'raw data'
        do j=1,nnml
          if (xilev(j).gt.1.e-37)
     $     write (lun11,*)j,xilev(j),elumab(1,j)
          enddo
        endif
c
C     First look for element data (jk is element index)
      klel=11
      mlel=npfirst(klel)
      jk=0
c
c     step through elements
      do while (mlel.ne.0)
c
c       get element data
        jk=jk+1
        mt2=mlel-1
        call drd(ltyp,lrtyp,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,mt2,
     $    nptrs,0,lun11)
        mllel=idat1(np1i+nidt-1)
        nnz=idat1(np1i)
        xeltp=rdat1(np1r)
        xeltp=abel(mllel)
        if (lpril.ne.0)
     $        write (lun11,*)'element:',jk,mlel,mllel,nnz,
     $                    (kdat1(np1k-1+mm),mm=1,nkdt),xeltp
c
C       ignore if the abundance is small
        if (xeltp.lt.1.e-10) then
            jkk=jkk+nnz
          else
c
c           now step thru ions (jkk is ion index)
            klion=12
            mlion=npfirst(klion)
            jkk=0
            kl=0
            do while ((mlion.ne.0).and.(kl.lt.nnz))
c
              jkk=jkk+1
C             retrieve ion name from kdati
              mlm=mlion-1
              call drd(ltyp,lrtyp,lcon,
     $            nrdt,np1r,nidt,np1i,nkdti,np1ki,mlm,
     $            nptrs,0,lun11)
c
C             if not accessing the same element, skip to the next element
              mlleltp=idat1(np1i+nidt-2)
              if (mlleltp.eq.mllel) then
c
                kl=kl+1
                if (lpril.ne.0)
     $            write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $                        (kdat1(np1ki+mm-1),mm=1,nkdti)
                do ktt=1,min(8,nkdti)
                  write (kinam1(ktt:ktt),'(a1)')kdat1(np1ki-1+ktt)
                  enddo
                do ktt=nkdti+1,9
                  write (kinam1(ktt:ktt),'(a1)')kblnk
                  enddo
c
c               get level data
                call func2l(jkk,lpril,lun11,t,xee,xpx,
     $              idat1,rdat1,kdat1,nptrs,
     $              npar,npnxt,npfi,
     $              rniss,rlev,ilv,
     $              nlpt,iltp,nlev,klev)
c
c               step thru levels
                do mm2=1,nlev
c
c                 get level pointer
                  mmtmp=npilev(mm2,jkk)
                  if (mmtmp.ne.0) then
                    kkkl=mmtmp
                    mmlv=mmtmp
c
c                   test for level pop
                    if (xilev(kkkl).gt.1.d-64) then
c
c                     get data
                      eth=rlev(1,mm2)
                      dep=xilev(kkkl)/(rniss(kkkl)+1.e-34)
                      write (lun11,9296)kkkl,kinam1,
     $                   (klev(lk,mm2),lk=1,20),eth,xilev(kkkl),
     $                   rniss(kkkl),dep
 9296                 format (1x,i6,1x,a8,1x,(20a1),7(1pe13.5))
c

c                     end of test for level pop
                      endif

c                   end of test for level pointer
                    endif
c
c                 end of step thru levels
                  enddo
c
c               end of test for element
                endif
c
C             Go to next ion
              mlion=npnxt(mlion)
              enddo
c
C           end of test for abundance 
            endif
c
C       Go to next element
        if (mlel.ne.0) mlel=npnxt(mlel)
        enddo
c
      write (lun11,*)'done with 7'
      write (lun11,993)
c
c
      return
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Print a short summary line of the radial calculation
C
9     continue
c
      elsum=0.
      ergsev=1.602197e-12
      do jlk=1,nlsvn
         ln=jlk
         ml=nplin(ln)
         mlm=ml-1
         call drd(ltyp,lrtyp,lcon,
     $     nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $     nptrs,0,lun11)
         elin=abs(rdat1(np1r))
         if ((elin.lt.1.e+8).and.(elin.gt.1.)) then
           elsum=elsum+elum(1,ln)+elum(2,ln)
           endif
         enddo
      sumtmp1=0.
      sumtmp2=0.
      ergsev=1.602197e-12
      r19=r*1.e-19
      tmp1=zremsz(1)
      tmp2=zrems(1,1)
      do jk=2,ncn2
         tmp1o=tmp1
         tmp1=zremsz(jk)
         sumtmp1=sumtmp1+(tmp1+tmp1o)*(epi(jk)-epi(jk-1))*ergsev/2.
         tmp2o=tmp2
         tmp2=zrems(1,jk)
         sumtmp2=sumtmp2+(tmp2+tmp2o)*(epi(jk)-epi(jk-1))*ergsev/2.
         enddo
c      terr=(sumtmp1-sumtmp2-elsum)/(sumtmp1+1.e-24)
      terr=(sumtmp1-sumtmp2)/(sumtmp1+1.e-24)
      uu1=enlum/(12.56*xpx*r19*r19)/3.e+10
      alguu1=log10(max(1.e-24,uu1))
      skse=xlum/(xpx*r19*r19)
      zeta=log10(max(1.e-24,skse))
      ecc=2.998e+10
      ekt=t*(0.861707)*ergsev
      sksec=skse/12.56/((1.+xee)*ekt*ecc)
      zetac=log10(max(1.e-24,sksec))
      enn0=xpx
      nlyc=nbinc(13.7d0,epi,ncn2)
      nry=nlyc+1
      egam=zremsz(nry)/(2.*12.56*enn0*ecc*r19*r19+1.e-24)
      nry=nbinc(13.6d0,epi,ncn2)+1
c      write (lun11,9969)r,rdel,zeta,xee,xpx,t,hmctot,
c     $ dpthc(1,nry),dpthc(2,nry),ntotit,lnerrd
c9969  format (1x,9(1pe10.3),2i3)
      tmp1=log10(r)
      tmp2=log10(max(1.e-36,min(99.,rdel/r)))
      tmp2c=log10(max(xcol,1.e-10))
      tmp3=log10(xpx)
      tmp4=log10(t)+4.
      tmp5=log10(max(dpthc(1,nry),1.e-10))
      tmp6=log10(max(dpthc(2,nry),1.e-10))
      tmp7=min(99.99,max(-99.99,hmctot*100.))
      tmp8=min(99.99,max(-99.99,terr*100.))
      write (tmpst,9889)tmp1,tmp2,tmp2c,zeta,xee,tmp3,tmp4,tmp7,
     $ tmp8,tmp5,tmp6,ntotit
      write (lun11,9889)tmp1,tmp2,tmp2c,zeta,xee,tmp3,tmp4,tmp7,
     $ tmp8,tmp5,tmp6,ntotit
 9889  format (1x,11(1x,f6.2),2i3)
      call xwrite(tmpst,10)
c
      return
c
c
 16   continue
c
c     times
!      write (lun11,*)'times:',tread,tloop,tfunc,trates1,thcor,trates2,    !jg
!     $          theat
      ttot=0.
      do ll=1,ntyp
!        ttmpi=tucalc(ll)/max(1,ncall(ll))   !jg
!        ttot=ttot+tucalc(ll)   !jg
!        write (lun11,9892)ll,ncall(ll),tucalc(ll),ttmpi   !jg
! 9892   format (1x,2i8,2(1pe11.3))
        enddo
      write (lun11,*)'total ucalc=',ttot
      write (lun11,993)
c
      return
c
c
 17   continue
c
c     column headings 
      klabs(1)='log(r)'
      klabs(2)='delr/r'
      klabs(3)='log(N)'
      klabs(4)='log(xi)'
      klabs(5)=' x_e  '
      klabs(6)='log(n)'
      klabs(7)='log(t)'
      klabs(8)='h-c(%)'
      klabs(9)='h-c(%)'
      klabs(10)='log(tau)'
c      klabs(10)='ntotit'
      write (lun11,9979)(klabs(mm),mm=1,10)
      write (tmpst,9979)(klabs(mm),mm=1,10)
9979  format (2x,3(1x,a6),1x,a7,a6,4(1x,a6),(1x,a9))
      call xwrite(tmpst,10)
      klabs(1)='      '
      klabs(2)='      '
      klabs(3)='      '
      klabs(4)='      '
      klabs(5)='      '
      klabs(6)='      '
      klabs(7)='      '
      klabs(8)='      '
      klabs(9)='      '
      klabs(10)='fwd   '
      klabs(11)='rev   '
      write (lun11,9989)(klabs(mm),mm=1,11)
      write (tmpst,9989)(klabs(mm),mm=1,11)
      call xwrite(tmpst,10)
9989  format (3x,11a7)
c
      return
c
c
 22   continue
c
c     ionization parameter etc.
      rdum=delr
      delr=rdum
      ergsev=1.602197e-12
      r19=r*(1.e-19)
c      write (lun11,*)enlum,xpx,r,xlum
      uu1=enlum/(12.56*xpx*r19*r19)/3.e+10
c      write (lun11,*)uu1
      enlumx=0.
      nb1=nbinc(100d0,epi,ncn2)
      nb10=nbinc(10000.d0,epi,ncn2)
c      write (lun11,*)'nb1=',nb1,nb10
      do kl=nb1,nb10
c        write (lun11,*)kl,epi(kl),zremsz(kl),enlumx
        enlumx=enlumx+(zremsz(kl)/epi(kl)+zremsz(kl-1)/epi(kl-1))
     $                *(epi(kl)-epi(kl-1))/2.
        enddo
      uux=enlumx/(12.56*xpx*r19*r19)/3.e+10
      alguux=log10(max(1.e-24,uux))
      alguu1=log10(max(1.e-24,uu1))
      skse=xlum/(xpx*r19*r19)
      zeta=log10(max(1.e-24,skse))
      ecc=2.998e+10
      ekt=t*(0.861707)*ergsev
c      sksec=skse/(12.56*((1.+xee)*ekt+pradl/(1.e-24+xpx))*ecc)
      sksec=skse/12.56/((1.+xee)*ekt*ecc)
      zetac=log10(max(1.e-24,sksec))
      enn0=xpx
      nlyc=nbinc(13.7d0,epi,ncn2)
      nry=nlyc+1
      egam=zremsz(nry)/(2.*12.56*enn0*ecc*r19*r19+1.e-24)
      nry=nbinc(13.6d0,epi,ncn2)+1
9968  format (1x,' log(Xi)=',1pe11.3, ' log(u1)=',1pe11.3,
     $ ' log(ux)=',1pe11.3,' gamma=',1pe11.3, ' rdel=',1pe11.3)
 9965 format (1x,' r=',1pe11.3,' t=',1pe11.3,' log(xi)=',1pe11.3,
     $ ' n_e=',1pe11.3,' n_p=',1pe11.3)
9966  format (1x,'httot=',1pe11.3,' cltot=',1pe11.3,
     $      'taulc=',1pe11.3,'taulcb=',1pe11.3)
      write(lun11,9965)r,t,zeta,xnx,xpx
      write(lun11,9966)httot,cltot,dpthc(1,nry),dpthc(2,nry)
      write(lun11,9968)zetac,alguu1,alguux,egam,rdel
      write (lun11,993)
c
      return
c
 25   continue
c
c      write (lun11,*)'outputting to the common block',nlsvn
c      do mm=1,ncn2
c        epi2(mm)=epi(mm)
c        do ll=1,3
c          zrems2(ll,mm)=zrems(ll,mm)
c          enddo
c        enddo
c      lpril=1
c      nilino=0
c      jkktmp=0
c      do j=1,nlsvn
c          kk=j
c          ln=nplin(j)
c          ml=ln
c          if (ml.ne.0) then
cc            write (lun11,*)'   ',j,ml
c            mlm=ml-1
c            call drd(ltyp,lrtyp,lcon,
c     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
c     $        idat1,rdat1,kdat1,nptrs,0,lun11)
c            elin=rdat1(np1r)
c            llo=idat1(np1i)
c            lup=idat1(np1i+1)
c            elin=rdat1(np1r)
c            aij=rdat1(np1r+2)
c            nilin=npar(ml)
c            if ((nilin.gt.0).and.(nilin.lt.ndat2)) then
c                if (nilin.ne.nilino) jkktmp=jkktmp+1
c                nilino=nilin
c                mlm=nilin-1
c                call drd(ltyp,lrtyp,lcon,
c     $            nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
c     $            idat1,rdat1,kdat1,nptrs,0,lun11)
c                do mm=1,nkdt
c                  kdtmp(mm)=kdat1(np1k-1+mm)
c                  enddo
c                do mm=nkdt+1,9
c                  kdtmp(mm)=kblnk
c                  enddo
c                nilin=idat1(np1i+2)
cc                write (lun11,*)ml,nilin,npar(ml)
c                newout=newout+1
c                newout=min(newout,nnnl)
c                lnewo(newout)=j
c                ml=npfi(13,jkktmp)
c                mllz=npar(ml)
c                lupfnd=0
c                llofnd=0
c                mlpar=npar(ml)
c                do while ((ml.ne.0).and.(mlpar.eq.mllz)
c     $            .and.((llofnd.ne.1).or.(lupfnd.ne.1)))
cc                    write (lun11,*)ml,nptrs(2,ml),mltype,jkk
c                  mlm=ml-1
c                  call drd(ltyp,lrtyp,lcon,
c     $              nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
c     $              idat1,rdat1,kdat1,nptrs,0,lun11)
c                  nlevmx=nlevmx+1
c                  nlev=idat1(np1i+nidt-2)
cc                  write (lun11,*)ml,nlev,llo,lup,(kdat1(np1k-1+mm),mm=1,nkdt)
c                  if (nlev.eq.llo) then
c                    do mm=1,20
c                      if (mm.le.nkdt) then
c                          klevl(mm)=kdat1(np1k-1+mm)
c                        else
c                          klevl(mm)=kblnk
c                        endif
c                      enddo
cc                   write (lun11,*)kk,ktmp2
c                    llofnd=1
c                    gglo=rdat1(np1r+1)
c                    endif
c                  if (nlev.eq.lup) then
c                    do mm=1,20
c                      if (mm.le.nkdt) then
c                          klevu(mm)=kdat1(np1k-1+mm)
c                        else
c                          klevu(mm)=kblnk
c                        endif
c                      enddo
c                    lupfnd=1
c                    ggup=rdat1(np1r+1)
c                    endif
c                  ml=npnxt(ml)
c                  if (ml.ne.0) mlpar=npar(ml)
c                  enddo
c                if ((llofnd.eq.1).and.(lupfnd.eq.1)) then
c                  flinewo(newout)=(1.e-16)*aij*ggup*elin*elin
c     $                             /((0.667274)*gglo)
c                  aijewo(newout)=aij
c                  ggloewo(newout)=gglo
c                  ggupewo(newout)=ggup
c                  do mm=1,8
c                    kdewo(mm,newout)=kdtmp(mm)
c                    enddo
c                  do mm=1,20
c                    kdewol(mm,newout)=klevl(mm)
c                    enddo
c                  do mm=1,20
c                    kdewou(mm,newout)=klevu(mm)
c                    enddo
c                  elewo(newout)=elin
c                  tau0ewo(newout)=tau0(1,j)
c                  elout(1,newout)=elum(1,j)
c                  elout(2,newout)=elum(2,j)
cc                  write (lun11,*)kk,ln,j,(kdtmp(mm),mm=1,8),elin,
cc     $             tau0(1,j),elum(1,j),elum(2,j),newout
cc9955             format (1x,2i8,1x,8a1,3(1pe11.3))
c                  endif
c              endif
c            endif
c          enddo
c      call commonprint(lun11)
c
      return
c
 26   continue
c
      return
c
c     ferland print
      lpril=0
c     print 500 strongest emission lines
      write (lun11,*)'log(emission line fluxes (erg/sec/cm^2))'
      kltmpo=0
      nlplmx=500
      eliml=0.1
      elimh=1.0e10
c     find the strongest lines.
      do  lm=1,nlplmx
        kltmp(lm)=0
        enddo
      nlpl=1
      do lnn=1,nlsvn
        ln=lnn
        ml=nplin(ln)
        mlm=ml-1
        call drd(ltyp,lrtyp,lcon,
     $    nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $    nptrs,0,lun11)
        elin=abs(rdat1(np1r))
        if ((lrtyp.ne.14).and.(lrtyp.ne.9)) then
          nilin=npar(ml)
          mlm=nilin-1
          call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
          nilin2=idat1(np1i+nidt-1)
          elmmtpp=(elum(2,ln)+elum(1,ln))/2.
          if (lpril.ne.0)
     $       write (lun11,*)lnn,elin,nilin,elmmtpp,ln,ml
          if ((ln.gt.0).and.(ln.lt.nnnl)
     $       .and.(elin.ge.eliml).and.(elin.le.elimh)
     $       .and.(elin.le.8.9e+6)
     $       .and.(elmmtpp.gt.1.e-36)
     $       .and.(nilin2.gt.0).and.(nilin2.le.nni))
     $        then
            lmm=0
            elcomp=1.e+10
            do while ((lmm.lt.nlpl).and.(elmmtpp.lt.elcomp))
              lmm=lmm+1
              kl2=kltmp(lmm)
              elcomp=0.
              if (kl2.gt.0)
     $          elcomp=(elum(2,kl2)+elum(1,kl2))/2.
              enddo
            if (lpril.ne.0)
     $       write (lun11,8516)ln,elin,elmmtpp,lmm,nlpl,kl2,elcomp
c 8516       format (1h ,i4,2e12.4,3i4,e12.4)
            kltmpo=ln
            do  k=lmm,min(nlplmx,nlpl)
              if ((lpril.ne.0).and.(kltmp(k).ne.0))
     $          write (lun11,*)'in 557 loop',k,kltmp(k),kltmpo
              kltmpn=kltmp(k)
              kltmp(k)=kltmpo
              kltmpo=kltmpn
              enddo
           nlpl=min(nlplmx,nlpl+1)
           if (lpril.ne.0)
     $       write (lun11,*)'done with 557 loop',lm
            endif
          endif
        enddo
       if (nlpl.gt.0) kltmp(nlpl)=kltmpo
c      nlpl=nlpl-1
      write (lun11,9599)
 9599 format (1x,'index, ion, wavelength, reflected, transmitted,total')
      r19=r*1.e-19
      do  kk=1,nlpl
        if (lpril.ne.0)
     $    write (lun11,*)'kk=',kk
        ln=kltmp(kk)
        if (ln.ne.0) then
          ml=nplin(ln)
          if (ml.ne.0) then
            if (lpril.ne.0)
     $      write (lun11,*)'   ',ln,ml
            mlm=ml-1
            call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $        nptrs,0,lun11)
            elin=abs(rdat1(np1r))
            nilin=npar(ml)
            mlm=nilin-1
            call drd(ltyp,lrtyp,lcon,
     $        nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $        nptrs,0,lun11)
            do mm=1,nkdt
              kdtmp(mm)=kdat1(np1k-1+mm)
              enddo
            do mm=nkdt+1,9
              kdtmp(mm)=kblnk
              enddo
             flux1=elum(1,ln)/12.56/r19/r19
             flux2=elum(2,ln)/12.56/r19/r19
c            nilin=idat1(np1i+2)
            if (lpril.ne.0)
     $      write (lun11,*)ml,nilin,npar(ml)
            write (lun11,9956)kk,ln,(kdtmp(mm),mm=1,9),elin,
     $      log10(flux1),log10(flux2),log10(flux1+flux2)
 9956       format (1x,2i8,1x,9a1,4(1pe13.5))
            endif
          endif
        enddo
      write (lun11,993)
      return
c

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Write  ion column densities
c     requires that zrtmp be filled by calling pprint(12)
C
 27   continue
c
      write (lun11,*)'ion column densities'
      write (lun11,9447)
9447   format (1x,'index, ion, column density')
c
      do lk=1,nni
        xcoltmp(lk)=0.
        enddo
c
c     step thru ions
      klion=12
      mlion=npfirst(klion)
      lk=0
      do while (mlion.ne.0) 
c
c        get ion data
         lk=lk+1
         ltyp=klion
         mlm=mlion-1
         call drd(ltyp,lrtyp,lcon,
     $     nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $     nptrs,0,lun11)
         do mm=1,nkdt
           kdtmp(mm)=kdat1(np1k-1+mm)
           enddo
         do mm=nkdt+1,9
           kdtmp(mm)=kblnk
           enddo
c
c        get element data
         nell=npar(mlion)
         mlm=nell-1
         call drd(ltyp,lrtyp,lcon,
     $      nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $      nptrs,0,lun11)
c         write (lun11,*)mlion,lk,np1i,nidt,np1i+nidt-1,
c     $      idat1(np1i+nidt-1),ababs(idat1(np1i+nidt-1)),mlm
         if ((idat1(np1i+nidt-1).gt.0)
     $     .and.(idat1(np1i+nidt-1).le.nl)) then
           abundel=ababs(idat1(np1i+nidt-1))
           xeltp=abundel
c
           do jkl=2,numrec
c             write (lun11,*)jkl,lk,zrtmp(2,jkl),zrtmp(8+lk,jkl),
c     $                             zrtmp(5,jkl),xeltp,xcoltmp(lk)
             xcoltmp(lk)=xcoltmp(lk)
     $         +(zrtmp(8+lk,jkl)*zrtmp(5,jkl)
     $             +zrtmp(8+lk,jkl-1)*zrtmp(5,jkl-1))
     $         *(zrtmp(2,jkl)-zrtmp(2,jkl-1))*xeltp/2.
             enddo
c
c          print out
           if (xcoltmp(lk).gt.1.e-15)
     $      write (lun11,9446)lk,(kdtmp(mm),mm=1,9),
     $      xcoltmp(lk)
9446       format (1x,i4,1x,9a1,1pe16.8)
c
           endif
c
         mlion=npnxt(mlion)
         enddo
c
c
      return
      end
