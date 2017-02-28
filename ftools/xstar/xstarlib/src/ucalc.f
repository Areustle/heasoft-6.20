      subroutine ucalc(ndesc,nrdesc,ml,lcon,jkion,vturbi,
     $   nrdt,np1r,nidt,np1i,nkdt,np1k,ans1,ans2,
     $   ans3,ans4,idest1,idest2,idest3,idest4,
     $   abund1,abund2,ptmp1,ptmp2,xpx,opakab,
     $   opakc,opakscatt,rccemis,fline,lpriu,kdesc2,
     $   rr,delr,t,trad,tsq,xee,xh1,xh0,
     $   epi,ncn2,bremsa,bremsint,
     $   rniss,rlev,ilev,nlpt,iltp,nlev,klev,lfast,lun11,
     $          idat1,rdat1,kdat1,nptrs,np2,
     $   npar,npnxt,npfi,npfirst,
     $   nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $   npconi2,ncsvn,rates,vsav,idrates)
c
c     this routine calculates rates for all atomic processes
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
c
      integer nptmpdim
      parameter (nptmpdim=ncn)
c
      character(1) klev(100,nd)
      character(49) kdesc(ntyp),kdesc2
      character(29) krdesc(ntyp)
c     master data 
      integer idat1(nidat1),nptrs(nptt,ndat2)
      integer np2
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
c     the saved rates
      real*8 rates(4,ndat2),vsav(4,ndat2)
      integer idrates(2,ndat2)
      real*8 epi(ncn)
      real*8 rniss(nd)
      real*8 bremsa(ncn),bremsint(ncn)
      real*8 rccemis(2,ncn),opakc(ncn),opakscatt(ncn)
      real*8 fline(2,nnnl)
      integer ilev(10,nd),nlpt(nd),iltp(nd)
      real*8 rlev(10,nd)
      real*8 aa(11),aaa(11,10),bbb(10),sg(ncn)
      real*8 rstorey(5),dcfe(8),defe(8),alhe(2),alh(2)
      real*8 etmpp(nptmpdim),stmpp(nptmpdim),ttmp(400),xsec(100)
      real*8 stmpe(nptmpdim)
      real*8  zc,eion,far,gam,scal,etmp8(ncn),stmp8(ncn)
      real*8 scal2
      real*8 a, aa1,aarec, aax, abund1, abund2, adi, aij, 
     &     airt, al, algt, alm, alp, alph
      real*8 alpha, alppp, ans1, ans1o, ans2, ans2d, ans3, 
     &     ans3d, ans4, ans4s, ansar2, ansar2o, ap, arad, atan, atmp, b,
     &     bb
      real*8 bbb2, bbrec, bbx, bdi, beta, bethe, c, beth,
     &     cai, ccrec, ccx, ch, ch2, ch3, chi, chir, chitmp, 
     &     cii
      real*8 cij, cijpp, cion, citmp1, citmp2, cji, clu, cn, cno, crate, 
     &     crec, crit53, csum, csum2, cul, d, ddd, ddx, delea
      real*8 del1, del2, dele, delev, delt, den, dirt, dirtemp, 
     &     e, e0, e1, eai, ecm, ediff, ee1exp, ee1expo, eelo, eeup
      real*8 eex, eexc, efnd, eij, eijry, ekt, elammu, elin, elo, em1, 
     &     em2ph, emax, enelec, ener, enn, ep, epii, erel, ergsev,eijkev
      real*8 eta, eth, etkh, etmp, ett, ett2, ettry, eup, exp10, 
     &     expo, exptmp, f1, f2, fchi, ff, ff2, fh2lke, fi, flin
      real*8 float, fudge, gamma, gflin, ggl, gglo, ggu, ggup, hecxrt, 
     &     hij, opakab, texp, flinabs, opakb1,
     $     p1, p2, p3, p4, p5, phi, phi1, phi2
      real*8 pi, pp, ppp, psi, ptmp1, ptmp2, q2, qq, r19, rate, 
     &     rcemsum, rctmp1, rctmp2, rec, 
     &     rinf,rcem1,rcem2,rnist
      real*8 rm, rr, rrrt, rs, s0, scale, sd, se, sg0, delr,
     &     sgth, sigma, sigvtherm, sqrt, sscal, sth, sum
      real*8 swrat, t, t0, t1, t3s2, t6, tbig, temp, tfnd, 
     &     time1, time2, tk, tm, tmr, tq, trad
      real*8 tsq, tst, ttz, tz, 
     &     upsil, upsiln, vth, vtherm, vturb, vturbi, wav, xee, xh0, 
     $     xh1, xhe1
      real*8 xkt, xkto, xnx, xpx, xx, y, ya, ypow, yw, ywsq, yy, z1, 
     &     zap, zeff, zz, zzz, y0,y1,yyqq
      real*8 dc,dt4,t2,term1,term2,term3,optst,opcrit,hcxrt
      real*8 er,ee1,ee2,ee3,er1,co,z2s,qij,sig,cr,crp,cr1,
     $     tmin,tmax,alphamilne,amilnerr
      real*8 tstr(100),cstr(100),rdattmp(100)
      real*8 tt,e3,e2,rho,ee,term4
c      real*8 min       !jg
      integer nspline
      integer i57, ic, idest1, idest2, idest3, idest4, 
     &     ierr, ik, il, iltmp, int, iq, ist, 
     &     itmp, iz
      integer jj, jkion, jkk, jkk2, jkk3, jkkl, jlo, kdim, kl, l2, lcon,
     &     lcon2, lf, lfast, lfastl, lfasto, lff,  lforce, li, li1,lii
      integer lk, ll, lm, lorb, lpri, lprib, lpric, lpril, lprim, 
     &     lprisv, lprit, lpriu, lrcalc, lrtyp, lrtyp2, lskp, ltyp, 
     &     ltyp2, lun11, luse8, nkdti
      integer lz, m, ml, ml2, ml3, mlion, mllz, mlp, 
     &     mm, mm5, mml, n, na, nb1, nbinc, nbmx,mlm
      integer ncn2, ncsvn, ndesc, ndtmp, nelin, nf, ni, 
     &     nidt, nidt2, nidti, nilin, nind, nistage, njj, nptmp
      integer nkdt, nkdt2, nlev, nlevp, nll, nlsvn, nmin, nmx, 
     &     nn,  nnz, nphint, npr, nprn, ndtmpo
      integer nq, nrdt, nrdti, nrdesc, nrdt2, nsh, nskp, ntcs, 
     &     ntmp, ntmp2, nu, numcon2, nzel, nterm
      integer lunsv,lfnd
      integer np1r,np1i,np1k,np1r2,np1i2,np1k2
      integer lctype,ncase,npts
      integer ntem
c
c     Not used
      real*8 javir
      integer javi
c      character(80) javik
c
      save aa,bb,ddd,ett,ggup,gglo,hij,opcrit,
     $         swrat,elin,pi,c,ergsev,etmp8,luse8
c
c      data opcrit/1.e-39/
      data opcrit/1.e-26/
      data ergsev/1.602197e-12/
      data pi/3.1415927/,c/2.997925e10/,luse8/0/
      data krdesc(1)/'ground state ionization      '/
      data krdesc(2)/'level ionization/recombinatio'/
      data krdesc(3)/'bound-bound collision        '/
      data krdesc(4)/'bound-bound radiative        '/
      data krdesc(5)/'bound-free collision (level) '/
      data krdesc(6)/'total recombination          '/
      data krdesc(8)/'total recombination          '/
      data krdesc(7)/'bound-free radiative (level) '/
      data krdesc(9)/'2 photon decay               '/
      data krdesc(11)/'element data                 '/
      data krdesc(12)/'ion data                     '/
      data krdesc(13)/'level data                   '/
      data krdesc(23)/'collisional superlevel->spect'/
      data krdesc(14)/'radiative superlevel->spect  '/
      data krdesc(15)/'CI total rate                '/
      data krdesc(40)/'CI from superlevels          '/
      data krdesc(41)/'non-radiative auger transtion'/
      data krdesc(42)/'Inner shell photoabsorption  '/
      data kdesc(1)/'radiative recombination:  aldrovandi and pequign '/
      data kdesc(2)/'charge exch. h0: Kingdon and Ferland             '/
      data kdesc(3)/'autoionization: hamilton, sarazin chevalier      '/
      data kdesc(4)/'line data radiative: mendosa; raymond and smith  '/
      data kdesc(5)/'2 photon transition collisional                  '/
      data kdesc(6)/'level data                                       '/
      data kdesc(7)/'dielectronic recombination: aldrovandi and pequi '/
      data kdesc(8)/'dielectronic recombination: arnaud and raymond   '/
      data kdesc(9)/'charge exch. H0 Kingdon and Ferland              '/
      data kdesc(10)/'charge exchange H+ Kingdon and Ferland          '/
      data kdesc(11)/'2 photon radiative                              '/
      data kdesc(12)/'photoionization, excited levels: hydrogenic     '/
      data kdesc(13)/'element data:                                   '/
      data kdesc(14)/'ion data:                                       '/
      data kdesc(15)/'photoionization: barfield koontz and huebner    '/
      data kdesc(16)/'arnaud and raymond ci                           '/
      data kdesc(17)/'collisional excitation hydrogenic: cota         '/
      data kdesc(18)/'radiative recombination hydrogenic: cota        '/
      data kdesc(19)/'photoionization: hullac                         '/
      data kdesc(20)/'charge exchange H+ Kingdon and Ferland          '/
      data kdesc(21)/'pixc bkh continued 3                            '/
      data kdesc(22)/'dielectronic recombination: storey              '/
      data kdesc(23)/'photoionization, excited levels: clark          '/
      data kdesc(24)/'pi xc clark continued                           '/
      data kdesc(25)/'collisional ionization: raymond and smith       '/
      data kdesc(26)/'collisional ionization hydrogenic: cota         '/
      data kdesc(27)/'photoionization: hydrogenic                     '/
      data kdesc(28)/'line data collisional: mendosa; raymond and smi '/
      data kdesc(29)/'collisional ionization data: scaled hydrogenic  '/
      data kdesc(30)/'radiative recombination hydrogenic: gould and t '/
      data kdesc(31)/'line data no levels                             '/
      data kdesc(32)/'collisional ionization: cota                    '/
      data kdesc(33)/'line data collisional: hullac                   '/
      data kdesc(34)/'line data radiative: mendosa; raymond and smitha'/
      data kdesc(35)/'photoionization: table (from bkh)               '/
      data kdesc(36)/'photoionization, excited levels:hydrogenic(no l)'/
      data kdesc(37)/'iron 3pq dr data from badnell                   '/
      data kdesc(38)/'total rr  from badnell amdpp.phys.strath.ac.uk  '/
      data kdesc(39)/'total dr  from badnell amdpp.phys.strath.ac.uk  '/
      data kdesc(40)/'                                                '/
      data kdesc(41)/'                                                '/
      data kdesc(42)/'                                                '/
      data kdesc(43)/'total photoionization cross sections tabulated  '/
      data kdesc(44)/'                                                '/
      data kdesc(45)/'                                                '/
      data kdesc(46)/'                                                '/
      data kdesc(47)/'                                                '/
      data kdesc(48)/'                                                '/
      data kdesc(49)/'op pi xsections for inner shells                '/
      data kdesc(50)/'op line rad. rates                              '/
      data kdesc(51)/'op and chianti line coll rates                  '/
      data kdesc(52)/'same as 59 but rate type 7                      '/
      data kdesc(53)/'op pi xsections                                 '/
      data kdesc(54)/'h-like cij, bautista (hlike ion)                '/
      data kdesc(55)/'hydrogenic pi xsections, bautista format        '/
      data kdesc(56)/'tabulated collision strength, bautista          '/
      data kdesc(57)/'effective charge to be used in coll. ion.       '/
      data kdesc(58)/'hlike rec rates, bautista                       '/
      data kdesc(59)/'verner pi xc                                    '/
      data kdesc(60)/'calloway h-like coll. strength                  '/
      data kdesc(62)/'calloway h-like coll. strength                  '/
      data kdesc(61)/'h-like cij, bautista (non-hlike ion)            '/
      data kdesc(63)/'h-like cij, bautista (hlike ion)                '/
      data kdesc(64)/'hydrogenic pi xsections, bautista format        '/
      data kdesc(65)/'effective charge to be used in coll. ion.       '/
      data kdesc(66)/'Like type 69 but, data in fine structure.       '/
      data kdesc(67)/'Effective collision strengths from Keenan et al.'/
      data kdesc(68)/'coll. strength He-like ions by Zhang & Sampason '/
      data kdesc(69)/'Kato & Nakazaki (1996) fit to Helike coll. strgt'/
      data kdesc(70)/'Coefficients for phot x-section of suplevels    '/
      data kdesc(71)/'Transition rates from superlevel to spect. lvls '/
      data kdesc(72)/'Autoinization rates (in s^-1) for satellite lvls'/
      data kdesc(73)/'Fit to coll. strengths satellite lvls Helike ion'/
      data kdesc(74)/'Delta functions to add to phot. x-sections  DR  '/
      data kdesc(75)/'autoionization data for Fe XXiV satellites      '/
      data kdesc(76)/'2 photon decay                                  '/
      data kdesc(77)/'coll rates from 71                              '/
      data kdesc(78)/'Auger level data                                '/
      data kdesc(79)/'fluorescence line data                          '/
      data kdesc(80)/' Collisional ionization rates gnd of Fe and Ni  '/
      data kdesc(81)/' Bhatia Fe XIX collision strengths              '/
      data kdesc(82)/' Fe UTA rad rates                               '/
      data kdesc(83)/' Fe UTA level data                              '/
      data kdesc(84)/' Iron K Pi xsections, spectator Auger binned    '/
      data kdesc(85)/' Iron K Pi xsections, spectator Auger summed    '/
      data kdesc(86)/' Iron K Auger data from Patrick                 '/
      data kdesc(88)/' Iron inner shell resonance excitation (Patrick)'/
      data kdesc(91)/' aped line wavelengths same as 50               '/
      data kdesc(92)/' aped collision strengths                       '/
      data kdesc(93)/' OP PI xsections?                               '/
      data kdesc(94)/' OP PI xsections?                               '/
      data kdesc(95)/' Bryans CI rates                                '/
      data kdesc(97)/' CI rates from inner shells from palmeri 2016   '/
      data kdesc(98)/' chianti2016 collisional rates                  '/

      javir=trad
c      trad=javir
      javi=nlpt(1)
      javi=iltp(1)
c      javik=klev(1,1)
      javi=nlsvn
c      nlsvn=javi
      javi=npcon(1)
      javi=npconi(1)
      javi=npilev(1,1)
      javi=npilevi(1)
      javi=npconi2(1)
      javi=ncsvn
      javi=idrates(1,1)
c      javik=krdesc(1)
c
      call remtms(time1)
c
      xnx=xpx*xee
c
      lpri=lpriu
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc:',ndesc,lcon,nrdt,nidt,nkdt,
     $  ml,(rdat1(np1r+mm-1),mm=1,nrdt),(idat1(np1i+mm-1),mm=1,nidt),
     $  (kdat1(np1k+mm-1),mm=1,nkdt)
       if (lpri.gt.1) write (lun11,*)'in ucalc, inputs:',
     $   t,xee,xpx,xnx
c
      vturb=vturbi
c
      kdesc2=kdesc(ndesc)
c
      if (luse8.eq.0) then
        luse8=1
        do mm=1,ncn2
          etmp8(mm)=dble(epi(mm)/13.605692)
          enddo
        endif
      nlevp=nlev
      ans1=0.
      ans2=0.
      ans3=0.
      ans4=0.
      idest1=0
      idest2=0
      idest3=idat1(np1i+nidt-1)
      idest4=idat1(np1i+nidt-1)+1
      opakab=0.
      lforce=1
c
      go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
     $  17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,
     $  36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,
     $  56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,
     $  76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,
     $  96,97,98,99), 
     $  ndesc
c
c
c     rr, a&p formula
1     continue
c      write (lun11,*)'in ucalc, ndesc=1'
      arad=rdat1(np1r)
      eta=rdat1(np1r+1)
      rrrt=arad/t**eta
      ans1=rrrt*xnx
      idest1=1
      idest2=0
c      write (lun11,*)'in ucalc, ndesc=1'
      go to 9000
c
c     h charge exchange recombination
 2    continue
      if (t.gt.5.) go to 9000
      aax=rdat1(np1r)
      bbx=rdat1(np1r+1)
      ccx=rdat1(np1r+2)
      ddx=rdat1(np1r+3)      
      rate=aax*expo(log(t)*bbx)*max(0.,(1.+ccx*expo(ddx*t)))*(1.e-9)
      ans1=rate*xh0
c      if (lpri.ge.1) write (lun11,*)'note turning off charge exchange'
c      ans1=0.
      ans2=0.
      if (nrdesc.eq.5) then
        ans2=rate*xh0
        ans1=0.
        endif
      idest1=1
      idest2=nlevp
      if (lpri.gt.1) write (lun11,*)'type 2 data',aax,bbx,ccx,ddx,rate,
     $                               xh0,ans1,idest2
      go to 9000
c      beth=rdat1(np1r)
c      alh(1)=rdat1(np1r+1)
c      alh(2)=rdat1(np1r+2)
c      ntcs=2
c      if (t.lt.1.) ntcs=1
c      hcxrt = beth*t**alh(ntcs)
c      xh1 = xiin(1)*xpx
c      xh1=0.
c      xh2 =max(0.,(1.-xiin(1)))*xpx
c      ans1=hcxrt*xh1
c      idest1=1
c      idest2=0
c      go to 9000
c
 3    continue
c     autoionization rates
      ekt = t*(0.861707)
      cai=rdat1(np1r)
      eai=rdat1(np1r+1)
      airt = cai*expo(-eai/ekt)/tsq
      ans1=airt*xnx
      idest1=1
      idest2=1
      go to 9000
c
 4    continue
c     line rates, coll and rad
c           write (lun11,*)'level data'
c           do 1906 ll=1,nlev
c             write (lun11,*)ll,(rlev(mm,ll),mm=1,3),
c     $          (ilev(mm,ll),mm=1,3),(klev(mm,ll),mm=1,3)
c 1906        continue
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      elin=abs(rdat1(np1r))
      flin=rdat1(np1r+1)
c      if (flin.le.1.e-10) flin=1.
      eeup=rlev(1,idest1)
      eelo=rlev(1,idest2)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         endif
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
      a=rdat1(np1r+4)
      hij=elin*1.e-8
      elammu=elin*1.e-4     
      aij=(6.67e+7)*gglo*flin/ggup/elammu/elammu
c     this is a fudge to avoid badnumerics from fine structure.
      if (flin.le.1.01e-12) aij=1.e+5
      if (elin.ge.1.e+9) aij=1.e+5
      ans1=aij*(ptmp1+ptmp2)
      ans4=aij*(ptmp1+ptmp2)*ergsev*12398.41/abs(elin)
      vtherm=((vturb*1.e+5)**2+(1.29e+6/sqrt(a/t))**2)**(0.5)
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
      sigvtherm=sigma
      ener=12398.41/abs(elin)
      nb1=nbinc(ener,epi,ncn2)
      ans2=0.
c      ans2=sigvtherm*bremsa(nb1)*vtherm/3.e+10
      if (elin.gt.0.99e+9) then 
         ans2=0.
         sigvtherm=0.
         endif
      ans1=ans1+ans2*ggup/(1.e-36+gglo)
      opakab=sigvtherm
      ans3=ans2*ener*ergsev
      ans4=ans1*ener*ergsev
c      write (lun11,*)'ltyp=4',idest1,idest2,elin,flin,ggup,gglo
      go to 9000
c
 5    continue
c     2photon rates, col
      idest1=idat1(np1i+1)
      idest2=idat1(np1i)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      ans1=0.
      ans2=0.
      ggup=rlev(2,idat1(np1i+1))
      gglo=rlev(2,idat1(np1i))
      hij=elin*1.e-8
      ekt=t*(0.861707)
      eex=abs(rlev(1,idat1(np1i))-rlev(1,idat1(np1i+1)))
      ans2=(8.629e-8)*rdat1(np1r+4)*t**rdat1(np1r+5)/ggup
      exptmp=expo(-eex/ekt)
      ans1=(8.629e-8)*rdat1(np1r+4)*t**rdat1(np1r+5)*exptmp/gglo
c      write (lun11,*)'ltyp=5',idest1,idest2,elin,flin,ggup,gglo,
c     $       ans2,eex,ekt,exptmp,ans1
      go to 9000
c      write (lun11,*)'in ucalc, ltyp=17',idat1(np1i),idat1(np1i+1),ggup,
c     $          gglo,eex,ans2,ans1
c
c     level quantities, partition function
 6    continue      
      go to 9000
c
c     dr, a&p formula
 7    continue
      adi=rdat1(np1r)
      bdi=rdat1(np1r+1)
      t0=rdat1(np1r+2)
      t1=rdat1(np1r+3)
      ap=1.
      dirt=adi*ap*(1.e-06)*expo(-t0/t)
     $  *(1.+bdi*expo(-t1/t))/(t*sqrt(t))
      ans1=dirt*xnx
c      if (lpri.ne.0) write (lun11,*)'type 7 data:',
c     $  adi,bdi,t0,t1,dirt,xnx,ans1
      idest1=1
      idest2=0
      go to 9000
c
c     dr, arnaud and raymond
 8    continue
      dirt=0.
      ekt=0.861707*t
      t3s2=t**(-1.5)
      tmr = 1.e-6*t3s2
      do 820 n = 1,4
        dcfe(n)=rdat1(np1r+n-1)
        defe(n)=rdat1(np1r-1+n+4)
        dirt = dirt + dcfe(n)*expo(-defe(n)/ekt)
 820     continue
      dirt = dirt*tmr
      ans1=dirt*xnx
      idest1=1
      idest2=0
      go to 9000
c
c     he charge exchange
 9    continue
      aax=rdat1(np1r)
      bbx=rdat1(np1r+1)
      ccx=rdat1(np1r+2)
      ddx=rdat1(np1r-1+4)
      texp=min(t,1000.)**bbx
      rate=aax*texp*(1.+ccx*expo(ddx*t))*(1.e-9)
      ans2=rate*xh0
      ans1=0.
      idest1=1
      idest2=nlevp
      if (nidt.gt.1) then
        idest1=idat1(np1i)
        idest2=nlevp+idat1(np1i+1)-1
        ans2=ans2/6.
        endif
c      if (jkion.eq.18) idest1=3
      go to 9000
      bethe=rdat1(np1r)
      alhe(1)=rdat1(np1r+1)
      alhe(2)=rdat1(np1r+2)
      ntcs=2
      if (t.lt.1.) ntcs=1
      hecxrt = bethe*t**alhe(ntcs)
c      xhe1 = xiin(2)*xpx
      xhe1=0.
      ans1=hecxrt*xhe1
      idest1=1
      idest2=0
      go to 9000
c
c
 10   continue
c     charge transfer ionzation as used in func2, for level rates    
      aax=rdat1(np1r)
      bbx=rdat1(np1r+1)
      ccx=rdat1(np1r+2)
      ddx=rdat1(np1r-1+4)
      eex=rdat1(np1r-1+7)
      rate=aax*t**bbx*(1.+ccx*expo(ddx*t))
     $             *expo(-eex/t)*(1.e-9)
      ans1=rate*xh1
      ans2=0.
c     this is tricky: func1 only uses the rate type 15 rate if idest1=1
c     for O I we have idest1=1,2,3, so it's OK
      idest1=idat1(np1i)
      idest2=nlevp
      go to 9000
c
 11   continue
      idest2=idat1(np1i)
      idest1=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      dele=abs(rlev(1,idest2)-rlev(1,idest1))
      ggl=rdat1(np1r+2)
      ggu=rdat1(np1r-1+4)
      ans1=(6.669e+15)*rdat1(np1r+1)*ggl/(ggu*rdat1(np1r)*rdat1(np1r))
      ans4=ans1*dele*ergsev
      go to 9000
c
 12   continue
      go to 36
c
 13   continue
      go to 9000
c
 14   continue
      go to 9000
c
 15   continue
      lprisv=lpri
      if (lpri.gt.1) write (lun11,*)'ltyp=15',ml,npar(ml)
c      if (lpri.gt.1) write (lun11,*)(rdat1(np1r-1+jj),jj=1,nrdt)
c      if (lpri.ne.0) write (lun11,*)(idat1(np1i-1+jj),jj=1,nidt)
c      if (lpri.ne.0) write (lun11,*)(kdat1(np1k-1+jj),jj=1,nkdt)
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      ntmp=nrdt/2
      do ml2=1,ntmp+1
        etmpp(ml2)=rdat1(np1r-1+2*ml2-1)
        stmpp(ml2)=rdat1(np1r-1+2*ml2)*1.e-18
        enddo
      mlm=nilin-1
      call drd(ltyp2,lrtyp2,lcon2,
     $  nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $  nptrs,0,lun11)
      idest1=idat1(np1i+nidt-2)
      idest2=idat1(np1i+nidt-3)-idat1(np1i+nidt-1)
      if (lpri.gt.1)
     $ write (lun11,*)ml,nilin,rdat1(np1r),idest1
      ett=rdat1(np1r2)
      if (lpri.gt.1)
     $ write (lun11,*)'ett=',ett
      nb1=nbinc(ett,epi,ncn2)
      gglo=rlev(2,1)
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      d=rdat1(np1r+1)
      do mm=1,11
        aa(mm)=rdat1(np1r-1+3+mm)
        enddo
c      aa(1)=min(max(aa(1),-6.),6.)
      ekt=t*(0.861707)
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=15:',lcon,
     $               nrdt,nidt,nkdt
      if (lpri.gt.1)
     $ write (lun11,891)(rdat1(np1r-1+mm),mm=1,nrdt)
 891  format (1x,10(1pe10.3))
      if (lpri.gt.1)
     $ write (lun11,892)(idat1(np1i-1+mm),mm=1,nidt)
 892  format (1x,10(i6))
      if (lpri.gt.1)
     $ write (lun11,893)(kdat1(np1k-1+mm),mm=1,nkdt)
 893  format (1x,100a1)
      na=idat1(np1i-1+nidt-5)
      nsh=idat1(np1i-1+nidt-4)
      do lk=1,na
        ll=idat1(np1i-1+9*lk-5)
        if (lpri.gt.1)
     $   write (lun11,*)'ll=',ll,lk
        lz=15*(lk-1)
        ett=rdat1(np1r-1+1+lz)
        ddd=rdat1(np1r-1+2+lz)
        bb=rdat1(np1r-1+3+lz)
        aa(1)=rdat1(np1r-1+4+lz)
        aa(2)=rdat1(np1r-1+5+lz)
        if (lpri.gt.1)
     $   write (lun11,*)'ltest=2',ett,ddd,bb,aa(1)
        do 1011 mml=1,5
          aa(2+mml)=rdat1(np1r-1+mml+5+lz)
 1011     continue
        do 1012 mml=1,4
          aa(7+mml)=rdat1(np1r-1+mml+10+lz)
 1012     continue
        bbb(lk)=bb
        do 1013 mml=1,11
          aaa(mml,lk)=aa(mml)
          if (lpri.gt.1)
     $     write (lun11,*)mml,aa(mml)
 1013     continue
        if (lpri.gt.1)
     $   write (lun11,*)'ltest=0',ll,na,nsh,aa(7)
        enddo
      lprib=0
      if (lpri.gt.1) lprib=lpri
      if (lpri.gt.1)
     $ write (lun11,*)'calling bkhsgo:',ett,t,
     $ ddd,(bbb(mm),mm=1,3),na,
     $ aaa(1,1),aaa(7,1)
      lfastl=1
      call bkhsgo(sg,ett,ddd,bbb,na,
     $         aaa,epi,ncn2,t,lprib,lfastl,lun11)
      lprib=0
c      if (lpri.gt.1) lprib=lpri      
      gglo=rlev(2,1)
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,xpx,opakab,
     $ opakc,lprib,epi,ncn2,bremsa,t,swrat,xnx,lfastl,lun11)
      if (lpri.ge.1) then
        npr=nbinc(ett,epi,ncn2)
        write (lun11,*)'bkh threshold xsection:',
     $         npr,ett,sg(npr)
        endif
      lpri=lprisv
      go to 9000
c
 16   continue
      ekt=t*(0.861707)
      njj=int(nrdt/5)
      lpriu=0
      if (lpriu.ne.0) 
     $ write (lun11,*)'ltyp=16:',idat1(np1i+nidt-1)
      csum=0.
      csum2=0.
      do mm=1,njj
        mm5=5*(mm-1)
        eth=rdat1(np1r-1+mm5+1)
        a=rdat1(np1r-1+mm5+2)
        b=rdat1(np1r-1+mm5+3)
        c=rdat1(np1r-1+mm5+4)
        d=rdat1(np1r-1+mm5+5)
        xx=eth/ekt
        if (lpriu.ne.0) 
     $   write (lun11,*)'xx=',xx,eth,ekt,a,b,c,d,np1r,mm5
        em1=ee1expo(xx)
        f1=em1/xx
        if (lpriu.ne.0) 
     $   write (lun11,*)'before ff2:',f1,em1,xx
        f2=ff2(xx,lpriu,lun11)
        if (lpriu.ne.0) 
     $   write (lun11,*)xx,a,b,c,d,em1,f1,f2
        term1=a*(1.-xx*f1)
        term2=b*(1.+xx-xx*(2.+xx)*f1)
        term3=c*f1
        term4=d*xx*f2
        fi=term1+term2+term3+term4
        fi=max(fi,0.)
        csum=csum+fi*expo(-xx)/xx
        csum2=csum2+fi/xx
        if (lpriu.ne.0) 
     $   write (lun11,*)term1,term2,term3,term4
        if (lpriu.ne.0) 
     $   write (lun11,*)mm,mm5,a,b,c,d,xx,f1,fi,csum
        enddo
      citmp1=csum*(6.69e-7)/ekt**(1.5)
      ans1=citmp1*xnx
      citmp2=csum2*(6.69e-7)/ekt**(1.5)
      idest1=1
      idest2=1
      ggup=rlev(2,nlevp)
      gglo=rlev(2,1) 
c     note that rinf has exponential removed
      rinf=(2.08e-22)*gglo/ggup/t/tsq
      ans2=citmp2*xnx
      ans2=ans2*rinf*xnx
      if (nrdesc.eq.5) then
         idest2=nlevp
        else
          idest2=1
        endif  
      if (lpriu.ne.0) 
     $   write (lun11,*)csum,citmp1,citmp2,ans1,
     $     ggup,gglo,rinf,ans2,idest1,idest2
      go to 9000
c
 17   continue
c     line rates, col
      ans1=0.
      ans2=0.
      hij=elin*1.e-8
c      write (lun11,*)'ltyp=4',idest1,idest2,elin,flin,ggup,gglo
      ekt=t*(0.861707)
      idest1=idat1(np1i+1)
      idest2=idat1(np1i)
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      if (eelo.gt.eeup) then
        idest1=idat1(np1i)
        idest2=idat1(np1i+1)
        eeup=rlev(1,idest2)
        eelo=rlev(1,idest1)
        endif
      eex=eeup-eelo
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      ans2=(8.629e-8)*rdat1(np1r)*t**rdat1(np1r+1)/ggup
      ans1=0.
      exptmp=expo(-eex/ekt)
      exptmp=1.
      if (ekt.gt.eex/20.) 
     $ ans1=ans2*ggup*exptmp/gglo
      if (lpri.gt.1)
     $ write (lun11,*)'in ucalc, ltyp=17',idat1(np1i),
     $   idat1(np1i+1),ggup,
     $   gglo,eex,ans2,ans1
      go to 9000
c
 18   continue
      aarec=rdat1(np1r)
      bbrec=rdat1(np1r+1)
      ccrec=rdat1(np1r+2)
      ttz=rdat1(np1r-1+4)
      algt=log10(t/(1.e-32+ttz))+4.
      algt=max(algt,3.5)
      algt=min(algt,7.5)
      idest1=idat1(np1i)
      ans1=exp10(aarec+bbrec*(algt-ccrec)**2)/t/1.e+4
      ans1=ans1*xnx
c      ans1=0.
      ans2=0.
      idest2=0
      go to 9000
c
 19   continue
      etkh=rdat1(np1r-1+5)
      enelec=1.
      eth=etkh
      nb1=nbinc(eth,epi,ncn2)
      idest1=idat1(np1i)
      idest2=nlevp
      ggup=rlev(2,nlevp)
      gglo=rlev(2,idest1)
      swrat=gglo/ggup
      ekt=t*(0.861707)
      lm=nb1
      do while (lm.le.nphint) 
         bbb2=epi(lm)/max(etkh,1.e-30)
         etmp=log(bbb2)
         alppp=rdat1(np1r)+etmp*(rdat1(np1r+1)+etmp*
     $         (rdat1(np1r+2)+etmp*rdat1(np1r-1+4)))
         ppp=expo(alppp)
         sg(lm)=(1.e-18)*enelec*ppp*(13.606)/etkh
         call enxt(eth,nb1,lpri,epi,ncn2,t,lfastl,lun11,
     $                  lm,nskp,nphint,lrcalc)
        lm=lm+nskp
        enddo
      call phintfo(sg,eth,ans1,ans2,ans3,ans4,
     $ abund1,abund2,xpx,opakab,
     $ opakc,lprib,epi,ncn2,bremsa,t,swrat,xnx,lfastl,lun11)
      go to 9000
c
 20   continue
c     charge transfer ionzation as used in func1, for total rate    
      aax=rdat1(np1r)
      bbx=rdat1(np1r+1)
      ccx=rdat1(np1r+2)
      ddx=rdat1(np1r-1+4)
      eex=rdat1(np1r-1+5)
      rate=aax*t**bbx*(1.+ccx*expo(ddx*t))
     $             *expo(-eex/t)*(1.e-9)
      ans1=rate*xh1
      ans2=0.
      idest1=1
      idest2=nlevp
      idest3=idat1(np1i+nidt-1)
      idest4=idest3+1
      go to 9000
c
 21   continue
c     h charge exchange dalgarno and butler
      beth=rdat1(np1r)
      alh(1)=rdat1(np1r+1)
      alh(2)=rdat1(np1r+2)
      ntcs=2
      if (t.lt.1.) ntcs=1
      hcxrt = beth*t**alh(ntcs)
      ans1=hcxrt*xh1
      idest1=1
      idest2=0
      go to 9000
c
c     dr storey
 22   continue
      ans1=0.
      ans2=0.
      idest1=1
      idest2=0
      if (t.gt.6.) go to 9000      
      do 221 kl=1,5
        rstorey(kl)=rdat1(np1r-1+kl)
 221    continue
c      if (rstorey(5).lt.0.) go to 9000
      t3s2=t**(-1.5)
      dirtemp=
     $   (1.e-12)*(rstorey(1)/t+rstorey(2)
     $   +t*(rstorey(3)+t*rstorey(4)))*t3s2
     $   *expo(-rstorey(5)/t)
      dirtemp=max(dirtemp,0.)
      if (lpri.gt.1) write (lun11,*)'in ucalc, ltyp=22:',
     $   ndesc,lcon,nrdt,nidt,nkdt,
     $  ml,(rdat1(np1r-1+mm),mm=1,nrdt),(idat1(np1i-1+mm),mm=1,nidt),
     $  (kdat1(np1k-1+mm),mm=1,nkdt),dirtemp,xnx
      ans1=dirtemp*xnx
      idest1=1
      idest2=0
      go to 9000
c
 23   continue
      lfastl=1
      lprisv=lpri
c      lpri=2
      if (lpri.gt.1)
     $ write (lun11,*)'in ucalc, 23:',rdat1(np1r),rdat1(np1r+1),
     $  rdat1(np1r+2),rdat1(np1r-1+4),
     $  rdat1(np1r-1+5),swrat,idat1(np1i),idat1(np1i+1),idat1(np1i+2),
     $  idat1(np1i-1+4),idat1(np1i-1+5)
      ett=rlev(1,idat1(np1i))
      eth=ett
      nb1=nbinc(eth,epi,ncn2)
      gglo=rlev(2,idat1(np1i))
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      ekt=t*(0.861707)
      jkk2=idat1(np1i+nidt-1)
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      jkk3=0
      jkk=0
      ndtmp=npfirst(12)
      do while ((jkk.ne.jkk2).and.(ndtmp.ne.0)) 
        jkk3=jkk3+1
        mlm=ndtmp-1
        call drd(ltyp2,lrtyp2,lcon2,
     $    nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $    nptrs,0,lun11)
        jkk=idat1(np1i2+nidt2-1)
        ndtmp=npnxt(ndtmp)
        enddo
      if (ndtmp.le.0) go to 9000
      zzz=float(idat1(np1i2))
      ndtmp=npfi(13,jkk3)
      mllz=npar(ndtmp)
      if (lpri.gt.1) write (lun11,*)jkk,jkk2,jkk3,zzz,ndtmp
      iltmp=1
      mlm=ndtmp-1
      call drd(ltyp2,lrtyp2,lcon2,
     $    nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $    nptrs,0,lun11)
      ndtmpo=ndtmp
      ndtmp=npnxt(ndtmp)
      do while ((ndtmp.ne.0).and.(iltmp.ne.idat1(np1i))
     $      .and.(npar(ndtmp).eq.mllz)) 
        mlm=ndtmp-1
        call drd(ltyp2,lrtyp2,lcon2,
     $    nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $    nptrs,0,lun11)
        iltmp=idat1(np1i2+nidt2-2)
        ndtmpo=ndtmp
        ndtmp=npnxt(ndtmp)
        enddo
      ndtmp=ndtmpo
      nprn=idat1(np1i)
      enn=float(nprn)
      if ((enn.le.1.e-24).or.(zzz.le.1.e-24).or.(ett.le.1.e-6))
     $  go to 9000
      sg0=6.3e-18*enn/zzz/zzz
      if (lpri.gt.1) 
     $ write (lun11,*)'ind=23:',ml,nilin,zzz,jkk,nprn,sg0,ett
      ll=nb1     
      do while (ll.le.nphint) 
        epii=epi(ll)
        sg(ll)=sg0*(epii/ett)**(-3)
        call enxt(ett,nb1,lpri,epi,ncn2,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        enddo
      lprib=0
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,xpx,opakab,
     $ opakc,lprib,epi,ncn2,bremsa,t,swrat,xnx,lfastl,lun11)
      lpri=lprisv
c      ans2=ans2*xnx
      idest1=idat1(np1i+nidt-2)
      idest2=nlevp
      go to 9000
c
 24   continue
      go to 9000
c
 25   continue
      idest4=idat1(np1i+nidt-1)+1
      idest3=idat1(np1i+nidt-1)      
      if (nrdesc.eq.5) then
         idest1=idat1(np1i+nidt-2)
         idest2=nlevp
        else
         idest2=1
         idest1=1
        endif
      e=rdat1(np1r)
      a=rdat1(np1r+1)
      b=rdat1(np1r+2)
      c=rdat1(np1r-1+4)
      d=rdat1(np1r-1+5)
      cion = 0.
      chir = (t*1.e+4)/(11590.*e)
      citmp1=cion
      ans1=citmp1*xnx
      ans2=0.
c      idest2=1
      if ( chir.le..0115 ) go to 9000
      chi = max(chir,0.1)
      ch2 = chi*chi
      ch3 = ch2*chi
      alpha = (.001193+.9764*chi+.6604*ch2+.02590*ch3)
     &        /(1.0+1.488*chi+.2972*ch2+.004925*ch3)
      beta = (-.0005725+.01345*chi+.8691*ch2+.03404*ch3)
     &       /(1.0+2.197*chi+.2457*ch2+.002503*ch3)
      ch = 1./chi
      fchi = 0.3*ch*(a+b*(1.+ch)+(c-(a+b*(2.+ch))*ch)*alpha+d*beta*ch)
      chitmp=expo(-1./chir)
      cion = 2.2e-6*sqrt(chir)*fchi/(e*sqrt(e))
      citmp1=cion
      ans1=citmp1*xnx
      ggup=rlev(2,nlevp)
      gglo=rlev(2,nidt-1) 
c     note that rinf has exponential removed
      rinf=(2.08e-22)*gglo/ggup/t/tsq
      ans2=ans1*rinf*xnx
      ans1=ans1*chitmp
      go to 9000
c
 26   continue
      go to 9000
c      ekt=t*(0.861707)
c      idest1=idat1(np1i)
c      gglo=rlev(2,idest1)
c      edelt=abs(rlev(1,idest1)-rlev(1,nlev))
c      exptmp=expo(-edelt/ekt)
c      ans1=(4.1416e-9)*rdat1(np1r)*t**rdat1(np1r+1)/gglo
c      ggup=rlev(2,nlev)
c      rinf=(2.08e-22)*gglo/ggup/t/tsq
c      ans2=ans1*rinf
c      ans1=ans1*exptmp
c      write (lun11,*)'ltyp=26',idest1,gglo,ggup,
c     $   edelt,rdat1(np1r),rdat1(np1r+1),ans1
c      idest2=nlev
c      go to 9000
c
 27   continue
      lprisv=lpri
c      ett=rdat1(np1r+1)
      lfastl=1
      idest1=1
      if (nrdesc.eq.1) then
          idest2=0
        else
          idest2=nlevp
        endif
      ett=abs(rlev(1,nlev)-rlev(1,1))
c      if (lpri.gt.1)
c      write (lun11,*)'in ucalc, ind=27:',rlev(1,nlev),
c     $     rlev(1,1),nlev,ett
      if (ett.le.1.e-5) go to 9000
      eth=ett
      nb1=nbinc(eth,epi,ncn2)
      gglo=rlev(2,idat1(np1i))
      swrat=gglo
      ekt=t*(0.861707)
      ll=nb1
      do while (ll.le.nphint) 
        epii=epi(ll)
        e=epii
        eth=ett
        zap = e/eth - 1.
        y = e/eth
        yy=sqrt(zap)
        yy=max(yy,1.e-04)
        fh2lke=((6.3e-18)/rdat1(np1r)/rdat1(np1r))
     $   *y**(-4)*expo(4.-4.*atan(yy)/yy)
     $   /(1.-expo(-6.2832/yy))
c        fh2lke=((6.3e-18)/rdat1(np1r)/rdat1(np1r))*y**(-3)
        sg(ll)=fh2lke
        if (lpri.ge.2) write (lun11,*)ll,epii,zap,y,yy,fh2lke
        call enxt(ett,nb1,lpri,epi,ncn2,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        enddo
      lprib=0
      if (lpri.gt.1) lprib=1
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,xpx,opakab,
     $ opakc,lprib,epi,ncn2,bremsa,t,swrat,xnx,lfastl,lun11)
      lpri=lprisv
      go to 9000
c
 28   continue
c     line rates, col
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if (rlev(1,idat1(np1i+1)).lt.rlev(1,idat1(np1i))) then
        idest2=idat1(np1i)
        idest1=idat1(np1i+1)
        endif
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      nind=5
      if (nrdt.ge.12) then
        do ll=1,4
          ttmp(ll)=rdat1(np1r-1+nrdt-4+ll)
          enddo
        jlo=0
        call hunt3(ttmp,4,t,jlo,0,lun11)     
        nind=nrdt-8+jlo      
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      elin=abs(rdat1(np1r))
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
c      nind=nrdt-2
      cijpp=rdat1(np1r-1+nind)
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      cji=(8.626e-8)*cijpp/tsq/ggup
      cij=0.
      exptmp=expo(-delt)
      cij=cji*ggup*exptmp/tsq/gglo
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=28',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdt,(rdat1(np1r-1+mm),mm=1,8),nind,jlo
        write (lun11,*)'       ',cij,cji,xnx,cijpp,exptmp
        endif
      elin=0.
      go  to 9000
c
 29   continue
      go to 9000
c      anstmp=rdat1(np1r+1)*(8.626e-8)/tsq
c      ans2=anstmp*(2.08e-22)*(rdat1(np1r+2)/rdat1(np1r-1+4))/t/tsq
c      ans1=0.
c      delt=rdat1(np1r)/t
c      if (delt.lt.50.) then
c         exptmp=1.
c         exptmp=expo(-delt)
c         ans1=anstmp*exptmp
c         endif
c      idest1=idat1(np1i+1)
c      idest2=nlev
c      write (lun11,*)'ltyp=29',ans1,ans2,(rdat1(np1r-1+ii),ii=1,4),anstmp,xnx
c      go to 9000
c
 30   continue
c      write (lun11,*)'ltyp=30',idat1(np1i)
        nmx=idat1(np1i)
        t6=t/100.
        zeff=float(nmx)
        beta=zeff*zeff/(6.34*t6)
        yy=beta
        vth=(3.10782e+7)*sqrt(t)
c       fudge factor makes the 2 expressions join smoothly
        ypow=min(1.,(0.06376)/yy/yy)
        fudge=0.9*(1.-ypow)+(1./1.5)*ypow
        phi1=(1.735+log(yy)+1./6./yy)*fudge/2.
        phi2=yy*(-1.202*log(yy)-0.298)
        phi=phi1
        if (yy.lt.0.2525) phi=phi2
        rrrt=2.*(2.105e-22)*vth*yy*phi
        ans1=rrrt*xnx
        ans2=0.
        idest1=1
        idest2=0
      go to 9000
c
 31   continue
c     line rates, coll and rad
c           write (lun11,*)'level data'
c           do 1906 ll=1,nlev
c             write (lun11,*)ll,(rlev(mm,ll),mm=1,3),
c     $          (ilev(mm,ll),mm=1,3),(klev(mm,ll),mm=1,3)
c 1906        continue
      idest1=idat1(np1i+1)
      idest2=idat1(np1i)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      elin=abs(rdat1(np1r))
      flin=rdat1(np1r+1)
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
c      a=rdat1(np1r-1+5)
      ans1=0.
      ans2=0.
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      nelin=npar(nilin)
      if ((nilin.le.0).or.(nelin.le.0)) go to 9000
      mlm=nelin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      a=rdat1(np1r2+1)
      hij=elin*1.e-8
      aij=(0.02655)*flin*8.*pi/hij/hij*gglo/(1.e-24+ggup)
      ans1=aij*(ptmp1+ptmp2)
      vtherm=((vturb*1.e+5)**2+(1.29e+6/sqrt(a/t))**2)**(0.5)
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
      sigvtherm=sigma
      ener=12398.41/abs(elin)
      nb1=nbinc(ener,epi,ncn2)
      ans2=sigvtherm*bremsa(nb1)*vtherm/3.e+10
      if (elin.gt.0.99e+9) then 
         ans2=0.
         sigvtherm=0.
         endif
      ans1=ans1+ans2*ggup/(1.e-36+gglo)
c     notice that opakab does not have abundance in
      opakab=sigvtherm
      ans3=ans2*ener*ergsev
      ans4=ans1*ener*ergsev
      delea=0.
      lfasto=4
      if (lfasto.ge.4) ans2=0.
c      if (opakab.gt.1.e-34) 
c     $  call linopac(lpri,lun11,opakab,rcem1,rcem2,elin,vturb,t,a,
c     $               delea,epi,ncn2,opakc,opakscatt,rccemis,fline,
c     $               lfasto)
      ans4=ans1*ener*ergsev
c      write (lun11,*)'ltyp=31',idest1,idest2,elin,flin,ggup,gglo
      go to 9000
c
 32   continue  
      idest1=idat1(np1i)
      gglo=rdat1(np1r-1+4)
      ans1=0.
      ans2=0.
      go to 9000
c      if (gglo.lt.1.e-24) go to 9000
c      ekt=t*(0.861707)
c      edelt=rdat1(np1r+2)
c      ans1=(4.1416e-9)*rdat1(np1r)*t**rdat1(np1r+1)*expo(-edelt/ekt)
c     $        /gglo
c      write (lun11,*)'ltyp=26',idest1,gglo,edelt,rdat1(np1r),rdat1(np1r+1),ans1
c      idest2=nlev
c      go to 9000
c
 33   continue
c     line rates, col
      idest1=idat1(np1i+1)
      idest2=idat1(np1i)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      ggup=rlev(2,idat1(np1i+1))
      gglo=rlev(2,idat1(np1i))
      elin=abs(rdat1(np1r))
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
      nind=4
      cijpp=rdat1(np1r-1+nind)
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      exptmp=expo(-delt)
      cij=(8.626e-8)*cijpp*exptmp/tsq/gglo
      cji=(8.626e-8)*cijpp/tsq/ggup
      ans1=cij*xnx
      ans2=cji*xnx
      go to 9000
c
 34   continue
c     line rates, coll and rad
c           write (lun11,*)'level data'
c           do 1906 ll=1,nlev
c             write (lun11,*)ll,(rlev(mm,ll),mm=1,3),
c     $          (ilev(mm,ll),mm=1,3),(klev(mm,ll),mm=1,3)
c 1906        continue
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      elin=abs(rdat1(np1r))
      aij=rdat1(np1r+1)
      eeup=rlev(1,idest1)
      eelo=rlev(1,idest2)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         endif
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
c      ggup=rdat1(np1r-1+4)
c      gglo=rdat1(np1r+2)
      a=rdat1(np1r-1+5)
      hij=elin*1.e-8
      elammu=elin*1.e-4     
c      flin=aij*hij*hij*ggup/((0.02655)*8.*pi*gglo)
      flin=aij*hij*hij*ggup/((0.667274)*gglo)
      ans1=aij*(ptmp1+ptmp2)
      vtherm=((vturb*1.e+5)**2+(1.29e+6/sqrt(a/t))**2)**(0.5)
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
      sigvtherm=sigma
      ener=12398.41/abs(elin)
      nb1=nbinc(ener,epi,ncn2)
      ans2=sigvtherm*bremsa(nb1)*vtherm/3.e+10
      if (elin.gt.0.99e+9) then 
         ans2=0.
         sigvtherm=0.
         endif
      ans1=ans1+ans2*ggup/(1.e-36+gglo)
c     notice that opakab does not have abundance in
      opakab=sigvtherm
      delea=0.
      lfasto=4
      if (lfasto.ge.4) ans2=0.
      ans3=ans2*ener*ergsev
      ans4=ans1*ener*ergsev
c      if (opakab.gt.1.e-34) 
c     $  call linopac(lpri,lun11,opakab,rcem1,rcem2,elin,vturb,t,a,
c     $               delea,epi,ncn2,opakc,opakscatt,rccemis,fline,
c     $               lfasto)
c      if (lpri.ne.0)
c     $ write (lun11,*)'ltyp=34',idest1,idest2,elin,flin,ggup,gglo,
c     $                         a,aij,hij,pi
      go to 9000
c
 35   continue
      lprisv=lpri
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=15:',lcon
      ett=rdat1(np1r)
      if (ett.le.(1.e-24)) go to 9000
      ntmp=(nrdt-1)/2
      do ml2=1,ntmp
        etmpp(ml2)=rdat1(np1r-1+1+2*ml2)
        stmpp(ml2)=rdat1(np1r-1+2*ml2)
        enddo
      nb1=nbinc(ett,epi,ncn2)
      gglo=rlev(2,1)
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) then
         write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      numcon2=max(2,ncn2/50)
      nphint=ncn2-numcon2
      idest1=idat1(np1i-1+6)
      idest2=idat1(np1i-1+5)-idat1(np1i-1+7)
      ekt=t*(0.861707)
      jlo=0
      ll=nb1
      lfastl=1
      do while (ll.le.nphint)
          epii=epi(ll)
          efnd=(epii-ett)/13.605692
          call hunt3(etmpp,ntmp,efnd,jlo,0,lun11)
          ml2=jlo
          mlp=ml2+1
          del1=(efnd-etmpp(ml2))/(etmpp(mlp)-etmpp(ml2))
          del2=(efnd-etmpp(mlp))/(etmpp(mlp)-etmpp(ml2))
          sg(ll)=-stmpp(ml2)*del2+stmpp(mlp)*del1
c          if (lpri.gt.1)
c     $    write (lun11,*)ll,epii,sg(ll),ml2,stmpp(ml2),stmpp(mlp),
c     $              del1,del2
          call enxt(ett,nb1,lpri,epi,ncn2,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
          ll=ll+nskp
          enddo
      lprib=0
      if (lpri.gt.1) lprib=lpri           
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,xpx,opakab,
     $ opakc,lprib,epi,ncn2,bremsa,t,swrat,xnx,lfastl,lun11)
      if (lpri.gt.1) then
        npr=nbinc(ett,epi,ncn2)+2
        write (lun11,*)'bkh threshold xsection:',
     $         npr,ett,sg(npr)
        endif
      lpri=lprisv
      go to 9000

c
 36   continue
c      photoionization, excited levels:hydrogenic(no l)
      lprisv=lpri
c      lpri=2
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=36:',(idat1(np1i-1+mm),mm=1,5)
      idest1=idat1(np1i+nidt-2)
      ett=rlev(1,nlevp)-rlev(1,idest1)
      idest2=nlevp
      if (ett.le.1.e-5) go to 9000
      eth=ett
      nb1=nbinc(eth,epi,ncn2)
      gglo=rlev(2,idest1)
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      ekt=t*(0.861707)
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      nelin=npar(nilin)
      if (lpri.gt.1)
     $ write (lun11,*)'in ucalc, ind=36:',
     $   ml,nilin,nelin
      if ((nilin.le.0).or.(nelin.le.0)) go to 9000
      mlm=nilin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      nistage=idat1(np1i2)
      mlm=nelin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      nzel=idat1(np1i2)
      nq=idat1(np1i)
      nq=min(10,nq)
      zz=float(nzel-nistage+1)
      sgth=(6.3e-18)*nq*nq/zz/zz
      if (lpri.gt.1) write (lun11,*)nb1,nq,nzel,nistage,zz,
     $                              ett,sgth,idest1,gglo,ggup
      ll=nb1
      lfastl=1
      do while (ll.le.nphint)
        epii=epi(ll)
        sg(ll)=sgth*(epii/ett)**(-3)
        call enxt(ett,nb1,lpri,epi,ncn2,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        enddo
      lprib=0
      if (lpri.gt.1) lprib=lpri
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,xpx,opakab,
     $ opakc,lprib,epi,ncn2,bremsa,t,swrat,xnx,lfastl,lun11)
      lpri=lprisv
      go to 9000
c
 37   continue
c     total dr for fe 3pq ions from badnell 2006 Ap. J. Lett 651 L73
      dirt=0.
      ekt=0.861707*t
      t3s2=t**(-1.5)
      tmr = 1.e-6*t3s2
      nterm=idat1(np1i)
      do  n = 1,nterm
        dcfe(n)=rdat1(np1r-1+n)
        defe(n)=rdat1(np1r-1+n+4)
        dirt = dirt + dcfe(n)*expo(-defe(n)/ekt)
        enddo
      dirt = dirt*tmr
      ans1=dirt*xnx
      idest1=1
      idest2=0
      go to 9000
c
 38   continue
c     total rr  from badnell http://amdpp.phys.strath.ac.uk/tamoc/DATA/DR/
      a=rdat1(np1r)
      b=rdat1(np1r+1)
      t0=rdat1(np1r+2)/1.e+4
      t1=rdat1(np1r-1+4)/1.e+4
      if (nrdt.gt.4) then
        c=rdat1(np1r-1+5)
        t2=rdat1(np1r-1+6)/1.e+4
        b=b+c*exp(-t2/t)
        endif
      term1=(T/T0)**(0.5)
      term2=(1.+(T/T0)**(0.5))**(1.-b)
      term3=(1.+(T/T1)**(0.5))**(1.+b)
      rrrt=a/(1.e-34+term1*term2*term3)
      ans1=rrrt*xnx
      if (lpri.gt.1) write (lun11,*)a,b,c,t0,t1,t2,
     $         term1,term2,term3,rrrt,ans1
      idest1=1
      idest2=0
      go to 9000
c
 39   continue
c     total dr  from badnell http://amdpp.phys.strath.ac.uk/tamoc/DATA/DR/
      dirt=0.
      ekt=0.861707*t
      t3s2=t**(-1.5)
      tmr = 1.e-6*t3s2
      nterm=nrdt/2
      do  n = 1,nterm
        dc=rdat1(np1r-1+n)
        dt4=rdat1(np1r-1+n+nterm)/1.e+4
        dirt = dirt + dc*exp(-dt4/t)
        if (lpri.gt.1) write (lun11,*)n,dc,dirt
        enddo
      dirt = dirt*tmr
      ans1=dirt*xnx
      if (lpri.gt.1) write (lun11,*)nterm,dirt,ans1
      idest1=1
      idest2=0
      go to 9000
c
 40   continue
      go to 9000
c
 41   continue
      go to 9000
c
 42   continue
      go to 9000
c
 43   continue
c     total photoionization cross sections tabulated in 
c     format like 53 (not used)
      go to 9000
c
 44   continue
      go to 9000
c
 45   continue
      go to 9000
c
 46   continue
      go to 9000
c
 47   continue
      go to 9000
c
 48   continue
      go to 9000
c
 49   continue
 499  continue
c     op pi xsections
c     old version
      lprisv=lpri
c      if (lpri.ge.1) lpri=2
c     these are the initial and final levels and indeces
c     notice that these are relative to the current ion
c     (not relative to the element as a whole)
      idest1=idat1(np1i+nidt-2)
      idest4=idat1(np1i+nidt-3)
      idest2=nlevp+max(0,idat1(np1i-1+nidt-3))-1
      if (lpri.gt.1) write (lun11,*)'idest1=',idest1,idest2
      if ((idest1.ge.nlevp).or.(idest1.le.0)) go to 9000
      if (ml.le.0) go to 9000
      eth=rlev(4,idest1)-rlev(1,idest1)
      ett=eth
      nilin=npar(ml)
      if (lpri.gt.1) write (lun11,*)'nilin=',nilin,ml
      if (nilin.le.0) go to 9000
      ntmp=nrdt/2
      do ml2=1,ntmp+1
        etmpp(ml2)=rdat1(np1r-1+2*ml2-1)
        stmpp(ml2)=rdat1(np1r-1+2*ml2)*1.e-18
        stmpp(ml2)=max(stmpp(ml2),0.)
        enddo
c      ett=ett+max(0.,13.605692*etmpp(1))
      optst=abund1*stmpp(1)
c      if ((optst.lt.opcrit).and.(lfast.eq.2)) go to 9000
      if (lpri.gt.1) write (lun11,*)'ett=',ett,etmpp(1)
      if (ett.le.0.) go to 9000
      ntmp2=nptmpdim
      call phextrap(etmpp,stmpp,ntmp,ntmp2,ett,ncn2,lpri,lun11)
      nb1=nbinc(ett,epi,ncn2)
      tst=abs(bremsint(nb1)/max(1.e-24,vsav(1,ml))-1.)
      xkt=ett/(0.861707*t)
      r19=rr/1.e+19
c      if ((tst.le.0.01).and.(lforce.ne.1)) then
      if (lforce.ne.1) then
        xkto=vsav(4,ml)
        tq=ee1exp(xkt)/max(1.e-24,ee1exp(xkto))
        ans1=rates(1,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans2=rates(2,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        ans3=rates(3,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans4=rates(4,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        if (lpri.gt.1) write (lun11,*)'type 49 scaling:',
     $    ml,vsav(2,ml),r19,tq,xnx,vsav(3,ml),xkt,xkto,tst,
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml),ett,t
        go to 9000
        endif
      vsav(2,ml)=r19
      vsav(1,ml)=bremsint(nb1)
      vsav(3,ml)=xnx
      vsav(4,ml)=xkt
      mlm=nilin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdti,np1r2,nidti,np1i2,nkdti,np1k2,mlm,
     $  nptrs,0,lun11)
      emax=etmpp(ntmp)*13.6+eth
      gglo=rlev(2,idest1)
      ggup=rlev(2,nlevp)
      idest3=idat1(np1i-1+nidti)
      idest4=idest3+1
      if (idest2.gt.nlevp) then
        jkk3=jkion+1
        if (lpri.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        ndtmp=npfi(13,jkk3)
        if (lpri.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        mllz=npar(ndtmp)
        iltmp=0
        do while ((ndtmp.ne.0).and.(iltmp.ne.(idest2-nlevp+1))
     $      .and.(npar(ndtmp).eq.mllz)) 
           mlm=ndtmp-1
           call drd(ltyp2,lrtyp2,lcon2,
     $       nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $       nptrs,0,lun11)
           iltmp=idat1(np1i2+nidt2-2)
           if (lpri.gt.1) then 
             write (lun11,*)nidt2,iltmp,ndtmp
             write (lun11,*)np1r2,np1i2,np1k2,mlm
             call dprinto(ltyp2,lrtyp2,lcon2,
     $          nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,
     $          rdat1,idat1,kdat1,lun11) 
             endif
           ndtmp=npnxt(ndtmp)           
           enddo
         ggup=rdat1(np1r2+1)
         if (lpri.gt.1)
     $    write (lun11,*) ndtmp,iltmp,idest2,ggup
         endif
      if (lpri.gt.1) write (lun11,*)'before phint53'
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      if (lpri.gt.1) then
        write (lun11,*)'type 49 data:',idat1(np1i),
     $    idat1(np1i+nidt-1),t,xnx,
     $    eth,gglo,ggup,swrat
        call dprinto(ndesc,nrdesc,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,rdat1,idat1,kdat1,lun11) 
        endif
      lprib=0
      if (lpri.gt.1) lprib=lpri
      rnist=rniss(idest1)
     $  *exp(-(ett+max(0.,13.605692*etmpp(1)))/(0.861707)/t)
     $  /rniss(nlevp)
      if (lpri.gt.1) 
     $  write (lun11,*)'ett=',ett,etmpp(1),
     $  ett+max(0.,13.605692*etmpp(1)),
     $  rniss(idest1)/rniss(nlevp),rnist
      call phint53(stmpp,etmpp,ntmp,ett,ans1,ans2,ans3,ans4,
     $  abund1,abund2,ptmp1,ptmp2,xpx,opakab,rnist,
     $  opakc,rccemis,lprib,epi,ncn2,bremsa,t,swrat,xnx,
     $  lfast,lun11)
      if (lpri.gt.1) then
        npr=nb1
        write (lun11,*)'bautista threshold xsection:',
     $         npr,ett,eth,rdat1(np1r),sg(npr),ans2,swrat
        endif
      rates(1,ml)=ans1
      rates(2,ml)=ans2
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      if (lpri.gt.1) write (lun11,*)'rates:',
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml)
      lpri=lprisv
      go to 9000
c
 50   continue
c     op line rad. rates 
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
c     nb check this out:  no bound-bound decays from continuum
      if ((idest1.le.0).or.(idest1.ge.nlev)
     $  .or.(idest2.le.0).or.(idest2.ge.nlev))
     $      go to 9000
      aij=rdat1(np1r+2)
      ans1=aij*(ptmp1+ptmp2)
c      aij=min(aij,1.e+10)
      eeup=rlev(1,idest1)
      eelo=rlev(1,idest2)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         endif
      elin=abs(rdat1(np1r))
      if (elin.le.1.e-34) go to 9000
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      nelin=npar(nilin)
      if ((nilin.le.0).or.(nelin.le.0)) go to 9000
      flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
c
      mlm=nelin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      a=rdat1(np1r2+1)
      vtherm=((vturb*1.e+5)**2+(1.29e+6/sqrt(a/t))**2)**(0.5)
      ener=12398.41/elin
      dele=ener*vtherm/3.e+10
      elammu=elin*1.e-4     
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
      sigvtherm=sigma
      jkkl=nplini(ml)                  
      if (jkkl.le.0) go to 9000
      ml3=nplin(jkkl)
      if (ml3.le.0) go to 9000
      mlm=ml3-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $  nptrs,0,lun11)
      elin=abs(rdat1(np1r))
      ener=12398.41/abs(elin)
      nb1=nbinc(ener,epi,ncn2)
      ans2=sigvtherm*bremsa(nb1)*vtherm/3.e+10
     $      *flinabs(ptmp1)
c
c     turning off rex
      ans2=0.
c
      if (elin.gt.0.99e+9) then 
         ans2=0.
         sigvtherm=0.
         endif
c      ans1=ans1+ans2*ggup/(1.e-36+gglo)
c     note that now opakab does not have abundance in
      opakab=sigvtherm
      lfasto=2
c      lfasto=4
      delea=0.
      lfnd=0
      lpriu=0
c      if (lpri.ge.1) lpriu=3
      call deleafnd(jkion,idest1,ml,
     $   nrdt,np1r,nidt,np1i,nkdt,np1k,
     $   idat1,rdat1,kdat1,nptrs,np2,
     $   npar,npnxt,npfi,npfirst,
     $   nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $   npconi2,ncsvn,delea,lfnd,lpriu,lun11)
c
      if (lfnd.eq.0) delea=rdat1(np1r+2)*(4.136e-15)
      ans4=ans1*ener*ergsev
      ans3=ans2*ener*ergsev
      rcem1=abund2*ans4*ptmp1/(1.e-34+ptmp1+ptmp2)
      rcem2=abund2*ans4*ptmp2/(1.e-34+ptmp1+ptmp2)
      opakb1=sigvtherm*abund1
c     this test should prevent calculation when called from func2
c     since abund1 will be zero
c      lpriu=lpri
      lpriu=0
      if ((nrdesc.ne.9).and.(lfasto.le.4).and.(opakb1*delr.gt.1.e-8))
     $ call linopac(lpriu,lun11,opakb1,ans2,sigvtherm,vtherm,bremsa,
     $               rcem1,rcem2,elin,vturb,t,a,delea,epi,ncn2,
     $               opakc,opakscatt,rccemis,fline,lfasto)
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=50:',
     $  ml,nilin,nelin,elin,flin,rdat1(np1r+2),gglo,ggup,a,vtherm,vturb,
     $  ans1,ans2,idest1,idest2,idest3,idest4,nlev,sigvtherm,
     $  bremsa(nb1),nb1,abund1,abund2,delea,lfnd
      if (nrdesc.ne.9) go to 9000
c
c       special for 2 photon
        ansar2=0.
        em2ph=aij
        lskp=1
        emax=ener
        nbmx=nbinc(emax,epi,ncn2)
        if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=50:',
     $  ml,nilin,nelin,elin,flin,rdat1(np1r),gglo,ggup,a,vtherm,ans2
     $   ,nbmx
        rcemsum=0.       
        lfastl=0        
        ll=2
        do while (ll.le.nbmx) 
          ansar2o=ansar2
          ansar2=epi(ll)*epi(ll)*max(0.,(epi(nbmx)-epi(ll)))
          rcemsum=rcemsum+(ansar2+ansar2o)
     $                   *(epi(ll)-epi(ll-lskp))/2.
          call enxt(epi(1),nb1,0,epi,ncn2,t,lfastl,lun11,
     $                  ll,lskp,nphint,lrcalc)
          ll=ll+lskp
          enddo
        rctmp1=0.
        rctmp2=0.
        ll=2
        do while (ll.le.nbmx) 
          ansar2=epi(ll)*epi(ll)*max(0.,(epi(nbmx)-epi(ll)))
          ansar2=ansar2*em2ph*emax/(1.e-24+rcemsum)
          rctmp1=abund2*ansar2*ptmp1/12.56
          rctmp2=abund2*ansar2*ptmp2/12.56
          rccemis(1,ll)=rccemis(1,ll)+rctmp1
          rccemis(2,ll)=rccemis(2,ll)+rctmp2
          call enxt(epi(1),nb1,0,epi,ncn2,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
          ll=ll+nskp
          enddo
      go to 9000
c
 51   continue
c     line rates, col, burgess and tully from manuel
      idest1=idat1(np1i+2)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      eeup=rlev(1,idest1)
      eelo=rlev(1,idest2)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         eeup=rlev(1,idest1)
         eelo=rlev(1,idest2)
         endif
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
      eijry=rdat1(np1r)
      eij=eijry*13.605692
      elin=12398.41/eij
      hij=elin*1.e-8
c      if (lpri.ne.0)
c     $ write (lun11,*)'type 51 data:',elin
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      if (lpri.gt.1)
     $ write (lun11,*)elin,ekt,delt
c      if (delt.gt.50.) go to 9000            
      c=rdat1(np1r+1)
      p1=rdat1(np1r+2)
      p2=rdat1(np1r-1+4)
      p3=rdat1(np1r-1+5)
      p4=rdat1(np1r-1+6)
      p5=rdat1(np1r-1+7)
      tk=t*1.e+4
c      tk=max(tk,(1.e+4)*12398.54/elin/(0.861707)/50.)
      tk=max(tk,2.8777e+6/elin)
      ik=idat1(np1i)
      cijpp=upsil(ik,eijry,c,p1,p2,p3,p4,p5,tk)
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      cji=(8.626e-8)*cijpp/tsq
     $      /ggup
      exptmp=expo(-delt)
      cij=cji*ggup*exptmp/gglo
      if (lpri.gt.1)
     $ write (lun11,*)'ltyp=51',c,p1,p2,p3,p4,p5,ik,
     $      eij,idest1,idest2,cij,cji,xnx,cijpp
      ans1=cij*xnx
      ans2=cji*xnx
      elin=0.
      go to 9000
c
 52   continue
c     same as 59 but rate type 7
      go to 59
c
 53   continue
 533   continue
c     op pi xsections
      lprisv=lpri
c      if (lpri.ge.1) lpri=2
c     these are the initial and final levels and indeces
c     notice that these are relative to the current ion
c     (not relative to the element as a whole)
      idest1=idat1(np1i+nidt-2)
      idest2=nlevp+idat1(np1i-1+nidt-3)-1
      if (lpri.gt.1) write (lun11,*)'idest1=',idest1,idest2,nlevp,ml
      if ((idest1.ge.nlevp).or.(idest1.le.0)) go to 9000
      if (ml.le.0) go to 9000
      eth=rlev(4,idest1)-rlev(1,idest1)
      eexc=rlev(1,idest1)
      ett=eth
      nilin=npar(ml)
      if (lpri.gt.1) write (lun11,*)'nilin=',nilin,ml
      if (nilin.le.0) go to 9000
      ntmp=nrdt/2
c      ett=ett+max(0.,13.605692*etmpp(1))
      if (lpri.gt.1) write (lun11,*)'ett=',ett,etmpp(1)
      if (ett.le.0.) go to 9000
      nb1=nbinc(ett,epi,ncn2)
      tst=abs(bremsint(nb1)/max(1.e-24,vsav(1,ml))-1.)
      xkt=ett/(0.861707*t)
      r19=rr/1.e+19
c      if ((tst.le.0.01).and.(lforce.ne.1)) then
      if (lforce.ne.1) then
        xkto=vsav(4,ml)
        tq=ee1exp(xkt)/max(1.e-24,ee1exp(xkto))
        ans1=rates(1,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans2=rates(2,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        ans3=rates(3,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans4=rates(4,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        if (lpri.gt.1) write (lun11,*)'type 53 scaling:',
     $    ml,vsav(2,ml),r19,tq,xnx,vsav(3,ml),xkt,xkto,tst,
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml),ett,t
        go to 9000
        endif
      vsav(2,ml)=r19
      vsav(1,ml)=bremsint(nb1)
      vsav(3,ml)=xnx
      vsav(4,ml)=xkt
      mlm=nilin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      emax=etmpp(ntmp)*13.6+eth
      gglo=rlev(2,idest1)
      ggup=rlev(2,nlevp)
      idest3=idat1(np1i+nidt-1)
      idest4=idest3+1
      if (idest2.gt.nlevp) then
        jkk3=jkion+1
        if (lpri.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        ndtmp=npfi(13,jkk3)
        if (lpri.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        if (ndtmp.le.0) go to 9000
        mllz=npar(ndtmp)
        iltmp=0
        do while ((ndtmp.ne.0).and.(iltmp.ne.(idest2-nlevp+1))
     $      .and.(npar(ndtmp).eq.mllz)) 
           mlm=ndtmp-1
           call drd(ltyp2,lrtyp2,lcon2,
     $       nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $       nptrs,0,lun11)
           iltmp=idat1(np1i2+nidt2-2)
           if (lpri.gt.1) write (lun11,*)nidt2,iltmp,ndtmp
           ndtmp=npnxt(ndtmp)     
           if (ndtmp.le.0) go to 9000                 
           enddo
c        NB fix to excited level PI and rec
         ett=ett+rdat1(np1r2)
         eth=ett
         ggup=rdat1(np1r2+1)
         if (lpri.gt.1)
     $    write (lun11,*) ndtmp,iltmp,idest2,ggup,ett
         endif
      sscal=1.
      do ml2=1,ntmp
        etmpp(ml2)=rdat1(np1r-1+2*ml2-1)
        stmpp(ml2)=rdat1(np1r-1+2*ml2)*1.e-18*sscal
        stmpp(ml2)=max(stmpp(ml2),0.)
        stmpe(ml2)=stmpp(ml2)*(etmpp(ml2)*13.605692+ett)
        if (lpri.gt.1) write (lun11,9819)ml2,etmpp(ml2),stmpp(ml2)
 9819   format (1x,i6,2(1pe11.3))
        enddo
      optst=abund1*stmpp(1)
c      if ((optst.lt.opcrit).and.(lfast.eq.2)) go to 9000
      ntmp2=nptmpdim
c     nb includes extrapolation
c     this is dangerous.  It does the right thing for ground-ground, 
c       but some cross sections should not be extrapolated.
c      call phextrap(etmpp,stmpp,ntmp,ntmp2,ett,ncn2,lpri,lun11)
      if (lpri.gt.1) write (lun11,*)'before phint53',eexc,eth,lfast
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      if (lpri.gt.1) then 
        write (lun11,*)'type 53 data:',idat1(np1i),
     $    idat1(np1i+nidt-1),t,xnx,
     $    eth,gglo,ggup,swrat
        call dprinto(ndesc,nrdesc,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,rdat1,idat1,kdat1,lun11) 
        endif
      lprib=0
      if (lpri.gt.1) lprib=lpri
      rnist=rniss(idest1)
     $  *exp(-(ett+max(0.,13.605692*etmpp(1)))/(0.861707)/t)
     $  /rniss(nlevp)
      if (lpri.gt.1) 
     $  write (lun11,*)'ett=',ett,etmpp(1),
     $  ett+max(0.,13.605692*etmpp(1)),
     $  rniss(idest1)/rniss(nlevp),rnist
      call phint53(stmpp,etmpp,ntmp,ett,ans1,ans2,ans3,ans4,
     $  abund1,abund2,ptmp1,ptmp2,xpx,opakab,rnist,
     $  opakc,rccemis,lprib,epi,ncn2,bremsa,t,swrat,xnx,
     $  lfast,lun11)
      if (lpri.gt.1) then
        npr=nb1
        write (lun11,*)'bautista threshold xsection:',
     $         npr,ett,eth,rdat1(np1r),sg(npr),ans2,swrat
        endif
      if (lpri.gt.1) then
        temp=t*1.e+4
        do ml2=1,ntmp
          etmpp(ml2)=rdat1(np1r-1+2*ml2-1)
          stmpp(ml2)=rdat1(np1r-1+2*ml2)
          enddo
        lprim=0
        call milne(temp,ntmp,etmpp,stmpp,ett/13.6,alphamilne,
     $     lun11,lprim)
        alphamilne=alphamilne*xnx
        amilnerr=(log10(alphamilne/max(1.e-34,ans2)))
        if ((abs(amilnerr).gt.0.05)
     $    .and.((alphamilne.gt.1.e-28).or.(ans2.gt.1.e-28))
     $    .and.(lfast.gt.1))
     $     write (lun11,*)'milne error',alphamilne,ans2,amilnerr
        endif
      rates(1,ml)=ans1
      rates(2,ml)=ans2
c      rates(2,ml)=alphamilne
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      if (lpri.gt.1) write (lun11,*)'rates:',
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml)
      lpri=lprisv
      go to 9000
c
 54   continue
c     h-like cij, bautista (hlike ion)
      idest1=idat1(np1i-1+nidt-3)
      idest2=idat1(np1i+nidt-3)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      ans3=0.
      ans4=0.
      lprisv=lpri
c      if (lpri.ge.1) lpri=2
      if (lpri.gt.1) write (lun11,*)'type 54 data:',
     $  idat1(np1i-1+nidt-3),idat1(np1i+nidt-3)
      if (rlev(1,idest2).lt.rlev(1,idest1)) then
        itmp=idest2
        idest2=idest1
        idest1=itmp
        endif
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      elin=12398.41/abs(eeup-eelo+1.e-24)
      hij=elin*1.e-8
      ekt=0.861707*t
      delt=12398.41/elin/ekt
c      if (delt.gt.50.) go to 9000
      ni=ilev(1,idest2)   
      li=ilev(3,idest2)
      nf=ilev(1,idest1)
      lf=ilev(3,idest1)
      if (lpri.gt.1) write (lun11,*)
     $  eeup,eelo,elin,ni,li,nf,lf
      if (ni.eq.nf) go to 9000
      if (ni.lt.nf) then
        ntmp=ni
        ni=nf
        nf=ntmp
        endif
      iq=idat1(np1i+nidt-2)
      if (lpri.gt.1)
     $ write (lun11,*)'before anl1:',ni,nf,li,lf,iq,idest1,idest2,
     $  eelo,eeup,idat1(np1i-1+nidt-3),idat1(np1i+nidt-3)
      call anl1(ni,nf,lf,iq,alm,alp,lpri,lun11)
      ans1=alp
      if (li.lt.lf) ans1=alm
      lpri=lprisv
      go to 9000
c
 55   continue
c      hydrogenic pi xsections, bautista format
      lprisv=lpri
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=55:',(idat1(np1i-1+mm),mm=1,5)
      idest1=idat1(np1i+nidt-2)
      ett=rlev(1,nlevp)-rlev(1,idest1)
      idest2=nlevp
      if (ett.le.1.e-5) go to 9000
      eth=ett
      nb1=nbinc(eth,epi,ncn2)
      gglo=rlev(2,idest1)
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      ekt=t*(0.861707)
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      nelin=npar(nilin)
      if (lpri.gt.1)
     $ write (lun11,*)'in ucalc, ind=55:',
     $   ml,nilin,nelin
      if ((nilin.le.0).or.(nelin.le.0)) go to 9000
      mlm=nilin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      nistage=idat1(np1i2)
      mlm=nelin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      nzel=idat1(np1i2)
      zz=float(nzel-nistage)
      sgth=(6.3e-18)/zz/zz
      ll=nb1
      lfastl=1
      do while (ll.le.nphint) 
        epii=epi(ll)
        sg(ll)=sgth*(epii/ett)**(-3)
        call enxt(ett,nb1,lpri,epi,ncn2,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        enddo
      lprib=0
      if (lpri.gt.1) lprib=lpri
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,xpx,opakab,
     $ opakc,lprib,epi,ncn2,bremsa,t,swrat,xnx,lfastl,lun11)
      lpri=lprisv
      go to 9000
c
 56   continue
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)  
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat1(np1i+1)).lt.rlev(1,idat1(np1i))) then
        idest2=idat1(np1i)
        idest1=idat1(np1i+1)
        endif
      lprisv=lpri
c      if (lpri.ge.1) lpri=2
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      dele=abs(eeup-eelo)
      if (dele.le.1.e-16) go to 9000
      ntmp=nrdt/2
      do kl=1,ntmp
        ttmp(kl)=rdat1(np1r-1+kl)
        enddo
      tfnd=log10(t*1.e+4)
      jlo=0
      call hunt3(ttmp,ntmp,tfnd,jlo,0,lun11)
      jlo=min(jlo,ntmp-1)
      nind=ntmp+jlo
      if (lpri.gt.1) write (lun11,*)'type 56:',
     $  idest1,idest2,ggup,gglo,dele,jlo,nind,
     $  rdat1(np1r-1+nind),rdat1(np1r-1+nind+1),
     $  tfnd,ttmp(jlo+1),ttmp(jlo)
      cijpp=(rdat1(np1r-1+nind+1)-max(1.e-36,rdat1(np1r-1+nind)))
     $   *(tfnd-ttmp(jlo))/(ttmp(jlo+1)-ttmp(jlo)+1.e-24)
     $     +max(1.e-36,rdat1(np1r-1+nind))
c
c     NB a fudge for Fe XXIV q line
c      if ((jkion.eq.349).and.(idest1.eq.1).and.(idest2.ge.26)) then
c         cijpp=cijpp*166.
c         endif
c      
      cijpp=max(0.,cijpp)
      ekt=0.861707*t
      delt=dele/ekt
      cij=0.
      exptmp=expo(-delt)      
      if (lpri.gt.1) write (lun11,*)'type 56:',
     $  idest1,idest2,ggup,gglo,dele
      cij=(8.626e-8)*cijpp*exptmp/tsq/gglo
      cji=(8.626e-8)*cijpp/tsq/ggup
      ans1=cij*xnx
      ans2=cji*xnx      
      if (lpri.gt.1) write (lun11,*)'type 56 data:',
     $  idest1,idest2,dele,cijpp,delt,exptmp,cij,cji,
     $  nind,gglo,ggup,tfnd,ntmp,jlo,ntmp,ttmp(jlo)
      lpri=lprisv
      go to 9000
c
 57   continue
c     same as  65 (?)
c     effective charge to be used in coll. ion. 
      lprisv=lpri
      lpri=0      
c      if (lprisv.ge.1) lpri=2
      tz=t*1.e+4
      idest1=idat1(np1i+nidt-2)
      idest2=nlevp
      if (lpri.gt.1)
     $ write (lun11,*)'in ucalc at 57:',idest1,idat1(np1i),rdat1(np1r)
      if ((idat1(np1i).le.0).or.(idest1.le.1).or.(idest1.gt.nlevp)) 
     $        go to 9000
      i57=idat1(np1i)
      eth=max(0.,rlev(1,nlevp)-rlev(1,idest1))
      ekt=0.861707*t
c      tz=max(tz,(1.e+4)*eth/(0.861707)/50.)
c      tz=max(tz,2.320975e+02*eth)
      e1=rlev(1,idest1)
      ep=rlev(4,idest1)
      if (ep.le.0.) go to 9000
      call calt57(tz,xnx,e1,ep,i57,cion,crec,lun11,lpri)  
      if (lpri.gt.1)
     $ write (lun11,*)'ltype=57:',cion,crec,gglo,ggup,nlevp,idest1,rinf,
     $  eth,ekt,ans1,ans2
c
c     trying a fudge to test cloudy's ci
c      if (lprisv.ge.1) write (lun11,*)'fudging ci for test'
c      if (i57.eq.2) cion=cion*2.
c      if (i57.eq.2) crec=crec*2.
c      if (i57.eq.3) cion=cion*5.
c      if (i57.eq.3) crec=crec*5.
c      if (i57.eq.4) cion=cion*15.
c      if (i57.eq.4) crec=crec*15.
c
      ans1=cion*xnx
      ggup=rlev(2,nlevp)
      gglo=rlev(2,idest1) 
c     note that rinf has exponential removed
      rinf=gglo/(1.e-36+ggup)
      ans2=crec*rinf*xnx*xnx
c     set to zero for ground state because we have more accurate rates 
c     for these levels: types 95 or 25
      if (idest1.eq.1) then
        ans1=0.
        ans2=0.
        endif
      go to 9000
c
 58   continue
c      bautista cascade rates. defunct.
      go to 9000
c
 59   continue
      lprisv=lpri
      lpril=lpri
c      if (lpri.ge.1) lpril=2
      if (lpril.gt.1) write (lun11,*)'ltyp=59',ml,npar(ml)
      if (lpril.gt.1) write (lun11,*)(rdat1(np1r-1+jj),jj=1,nrdt)
      if (lpril.gt.1) write (lun11,*)(idat1(np1i-1+jj),jj=1,nidt),nidt
      if (lpril.gt.1) write (lun11,*)(kdat1(np1k-1+jj),jj=1,nkdt)
      if (ml.le.0) go to 9000
c
c
c     experiment with only vfky
c      if (nrdt.le.6) go to 9000
c
      lfastl=1
      nilin=npar(ml)
      idest3=idat1(np1i+nidt-1)
      idest4=idat1(np1i+nidt-3)
c     why was this statement here?
      if (idest4.gt.idest3+1) go to 9000 
      idest1=idat1(np1i+nidt-2)
      idest2=nlevp+idat1(np1i-1+nidt-3)-1
      idest2=max(idest2,1)
c      nb must uncomment these if func2a is called
c      if (nrdesc.eq.7) then
c        idest2=nlevp
c        endif
      if ((nilin.le.0).or.(nilin.gt.np2)) go to 9000
      mlm=nilin-1
      call drd(ltyp2,lrtyp2,lcon2,
     $  nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $  nptrs,0,lun11)
      if (lpril.gt.1)
     $ write (lun11,*)ml,nilin,rdat1(np1r),idest1,rdat1(np1r2),nlevp
      ett=rdat1(np1r2)
      if ((idest1.gt.nlevp).or.(idest1.le.0)) go to 9000
      if (ml.le.0) go to 9000
      if (ett.le.0.) go to 9000
      nb1=nbinc(ett,epi,ncn2)
      numcon2=max(2,ncn2/50)
      nphint=ncn2-numcon2
      nphint=max(nphint,nb1+1)
      if (nb1.ge.nphint-1) go to 9000
      ett=rdat1(np1r)
      nb1=nbinc(ett,epi,ncn2)
      if (nb1.ge.(ncn2-1)) go to 9000
      tst=abs(bremsint(nb1)/max(1.e-24,vsav(1,ml))-1.)
      if (lpril.gt.1)
     $ write (lun11,*)ett,nb1,bremsint(nb1),ml,vsav(1,ml),tst,
     $  lforce
      xkt=ett/(0.861707*t)
      r19=rr/1.e+19
      if (lforce.ne.1) then
        xkto=vsav(4,ml)
        tq=ee1exp(xkt)/max(1.e-24,ee1exp(xkto))
        ans1=rates(1,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans2=rates(2,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        ans3=rates(3,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans4=rates(4,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
c        write (lun11,*)'in ucalc2, scaling used'
        go to 9000
        endif
      vsav(2,ml)=r19
      vsav(1,ml)=bremsint(nb1)
      vsav(3,ml)=xnx
      vsav(4,ml)=xkt
      gglo=rlev(2,1)
      ggup=rlev(2,nlevp)
      if (idest2.gt.nlevp) then
        jkk3=jkion+1
        if (lpril.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        ndtmp=npfi(13,jkk3)
        if (lpril.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        if (ndtmp.le.0) go to 9000
        mllz=npar(ndtmp)
        iltmp=0
        do while ((ndtmp.ne.0).and.(iltmp.ne.(idest2-nlevp+1))
     $      .and.(npar(ndtmp).eq.mllz)) 
           mlm=ndtmp-1
           call drd(ltyp2,lrtyp2,lcon2,
     $       nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $       nptrs,0,lun11)
           iltmp=idat1(np1i2+nidt2-2)
           if (lpril.gt.1) write (lun11,*)nidt2,iltmp,ndtmp
           ndtmp=npnxt(ndtmp)     
           if (ndtmp.le.0) go to 9000                 
           enddo
c        NB fix to excited level PI and rec
         ett=ett+rdat1(np1r2)
         eth=ett
         ggup=rdat1(np1r2+1)
         if (lpril.gt.1)
     $    write (lun11,*) ndtmp,iltmp,idest2,ggup,ett
         endif
      if (lpril.gt.1) write (lun11,*)nlevp,ggup
      if (ggup.le.1.e-24) then
        if (lpril.gt.1) write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      ett=rdat1(np1r)
      nb1=nbinc(ett,epi,ncn2)       
      if (lpril.gt.1)
     $ write (lun11,*)'ett=',ett,nb1,nphint,swrat,gglo,ggup
      if (nb1.ge.(nphint-1)) go to 9000
      if ((bremsint(nb1).lt.1.e-20).and.(lpril.gt.1)) 
     $    write (lun11,*)'skipping 59',
     $         nb1,bremsint(nb1)
      if (bremsint(nb1).lt.1.e-20) go to 9000
      if (nrdt.eq.9) then
          ett=rdat1(np1r)
          emax=rdat1(np1r+1)
          e0=rdat1(np1r+2)
          s0=rdat1(np1r-1+4)
          ya=rdat1(np1r-1+5)
          pp=rdat1(np1r-1+6)
          yw=rdat1(np1r-1+7)
          y0=rdat1(np1r-1+8)
          y1=rdat1(np1r-1+9)
          l2=0
        else
          e0=rdat1(np1r+1)
          s0=rdat1(np1r+2)
          ya=rdat1(np1r-1+4)
          pp=rdat1(np1r-1+5)
          yw=rdat1(np1r-1+6)
          y0=0.
          y1=0.
          l2=idat1(np1i+2)
        endif
      ywsq=yw*yw
      qq=5.5+l2-pp/2.
      if (lpril.gt.1) write (lun11,*)'qq=',
     $   l2,qq,ya,ywsq,pp,yw,s0
      ll=nb1
      do while (ll.le.nphint) 
        epii=epi(ll)
        xx=epii/e0-y0
        if (nrdt.eq.9) then
            yy=sqrt(xx*xx+y1*y1)
          else
            yy=xx
          endif
        yyqq=qq*log(max(1.e-34,yy))
        yyqq=exp(-min(60.,max(-60.,yyqq)))
        term1=((xx-1.)*(xx-1.)+ywsq)
        term2=yyqq
        term3=(1.+sqrt(yy/ya))**(-pp)
        ff=term1*term2*term3
        sg(ll)=s0*ff*(1.e-18)
        if (lpril.gt.1) write (lun11,*)ll,epii,sg(ll),
     $    yy,yyqq,xx,term1,term2,term3,qq,ff
        call enxt(ett,nb1,0,epi,ncn2,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        enddo
      ekt=t*(0.861707)
      lprib=0
      if (lpril.gt.1) lprib=lpril
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,xpx,opakab,
     $ opakc,lprib,epi,ncn2,bremsa,t,swrat,xnx,lfastl,lun11)
      if (lpril.gt.1) then
        npr=nb1
        write (lun11,*)'verner threshold xsection:',
     $         npr,ett,sg(npr),opakab
        endif
c     nb this turns off all recombination into excited levels 
c     for type 59...
c      if ((nrdesc.eq.1).or.(idest1.gt.1)) then
c        ans4=0.
c        ans2=0.
c        endif
      rates(1,ml)=ans1
      rates(2,ml)=ans2
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      lpri=lprisv
      go to 9000
c
 60   continue
c      go to 9000
c     calloway h-like coll. strength    
      lpril=0
c      if (lpri.ge.1) lpril=2
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat1(np1i+1)).lt.rlev(1,idat1(np1i))) then
        idest2=idat1(np1i)
        idest1=idat1(np1i+1)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      dele=abs(rlev(1,idest2)-rlev(1,idest1))
      if (dele.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=dele/ekt
      temp=t*1.e+4
      temp=max(temp,0.02*dele*1.e+4/(0.861707))
      call calt60_62(temp,nrdt,ndesc,np1r,np1i,rdat1,idat1,cijpp)
c      cijpp=cijpp/2./2.
      cji=(8.626e-8)*cijpp/tsq/(1.e-16+ggup)
      exptmp=expo(-delt)
      cij=cji*ggup*exptmp/(1.e-16+gglo)
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpril.gt.1) then
        write (lun11,*)'ltyp=60',idest1,idest2,temp,flin,ggup,gglo
        write (lun11,*)'       ',nrdt,(rdat1(np1r-1+mm),mm=1,8),jlo
        write (lun11,*)'       ',cij,cji,xnx,cijpp,exptmp,dele,delt
        endif
      go to 9000
c
 61   continue
      go to 9000
c
 62   continue
      go to 60
c
 63   continue
c      if (lpri.ne.0) write (lun11,*) 'type 63 data not implemented'
c      go to 9000
      lpril=0
c      if (lpri.ge.1) lpril=2
      idest1=idat1(np1i-1+nidt-3)
      idest2=idat1(np1i+nidt-3)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      elin=12398.41/abs(eeup-eelo+1.e-24)
      hij=elin*1.e-8
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      if (lpril.ne.0) write (lun11,*)'delt=',delt
      if (delt.gt.50.) go to 9000
      ni=ilev(1,idest1)
      li=ilev(3,idest1)
      nf=ilev(1,idest2)
      lf=ilev(3,idest2)
      sum=0.
      iq=idat1(np1i+nidt-2)
      if (lpril.ne.0)
     $ write (lun11,*)'ltyp=63',idest1,idest2,ni,li,nf,lf
      if (nf.eq.ni) then
        if (lpril.ne.0)
     $     write (lun11,*)'nf=ni',lf,li
        if (abs(lf-li).eq.1) then
          lff=min(lf,li)
          lii=max(lf,li)
          li1=max(1,lff)     ! mab
          do  nn=li1,ni-1
            if (lpril.ne.0)
     $        write (lun11,*)'before anl1'
           if (lii.ge.1) then
            if (lpril.ne.0)
     $        write (lun11,*)'li=1',ni,nn,lii-1,iq
            call anl1(ni,nn,lii-1,iq,alm,alp,lpril,lun11)
c              write (lun11,*)'li=1',ni,nn,lii-1,iq,alp
            sum=sum+alp
           endif
           if (nn.gt.lii+1) then
            if (lpril.ne.0)
     $        write (lun11,*)'nn=li+1',ni,nn,lii+1,iq
            call anl1(ni,nn,lii+1,iq,alm,alp,lpril,lun11)
            sum=sum+alm
           endif
          enddo
          if (lpril.ne.0)
     $     write (lun11,*)'after anl1',sum
          ecm=abs(rlev(1,idest1)-rlev(1,idest2))*8059.9
          ecm=0.
          nnz=idat1(np1i-1+4)
          tbig=t*1.e+4
          z1=1.
          rm=1800.
          il=0
          psi=0.75/nnz/nnz*lii/(2*lii+1)*ni*ni*(ni*ni-lii*lii)
          if (lpril.ne.0)
     $     write (lun11,*)'before amcrs',ecm,ni,lii,sum
          call amcrs(ni,lii,tbig,nnz,z1,rm,xnx,sum,ecm,psi,il,cn,
     $        lpril,lun11)
          cno=cn
          iz=idat1(np1i-1+4)
          if (lf.lt.li) then
            ans1=cn
            ans2=cn*rlev(2,idest1)/rlev(2,idest2)
          else
            ans2=cn
            ans1=cn*rlev(2,idest2)/rlev(2,idest1)
          endif
          if (lpril.ne.0)
     $     write (lun11,*)'after amcrs',cn,iz,cno,ans1,ans2
        endif
      else
        if (lpril.ne.0)
     $     write (lun11,*)'nf.ne.ni'
        aa1=0.
        if (abs(lf-li).eq.1) then
          sum=0.
          nu=max(ni,nf)
          nll=min(ni,nf)
          do lff=0,nll-1
            call anl1(nu,nll,lff,iq,alm,alp,lpri,lun11)
             sum=sum+alp*(2*lff+3)
             if (lff.gt.0)  then
              sum=sum+alm*(2*lff-1)
             endif
             if (lff.eq.lf .and. li.gt.lf) aa1=alp
             if (lff.eq.lf .and. li.lt.lf) aa1=alm
             if (lpril.ne.0) write (lun11,*)'after anl1',
     $           lff,li,lf,sum,alp,alm,aa1
          enddo
          if (lpril.ne.0)
     $     write (lun11,*)'after anl1',sum,alp,alm,aa1
          nnz=idat1(np1i-1+4)
          tbig=t*1.e+4
          call erc(nll,nu,tbig,nnz,se,sd,sum,lun11,lpril)
c ***** check if ans1 and ans2 are correct or inverted
          ans1=se*(2*lf+1)*aa1/sum
          ans2=sd*(2*li+1)*aa1/sum
          if ((nf.gt.ni).or.(lf.gt.li)) then
           atmp=ans1
           ans1=ans2
           ans2=atmp
          endif
          if (lpril.ne.0)
     $     write (lun11,*)'after erc',se,sd,ans1,ans2
        endif
      endif
c
      ans1=ans1*xnx
      ans2=ans2*xnx
      go to 9000
c
 64   continue
c     hydrogenic pi xsections, bautista format 
      lprisv=lpri
      idest1=idat1(np1i+nidt-2)
      ett=abs(rlev(1,nlevp)-rlev(1,idest1))
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=64:',(rdat1(np1r-1+mm),mm=1,5)
      if (ett.le.1.e-5) go to 9000
      zzz=float(idat1(np1i+2))
      enn=float(idat1(np1i))
      eth=ett
      nb1=nbinc(eth,epi,ncn2)
      gglo=rlev(2,idest1)
      swrat=gglo
      idest2=nlevp
      ekt=t*(0.861707)
      ll=nb1
      lorb=idat1(np1i+1)
      ic=idat1(np1i+2)
      nq=idat1(np1i)
      mm=0
      lfastl=1
      do while (ll.le.nphint) 
        mm=mm+1
        epii=epi(ll)
        e=epii
        eth=ett
        erel=max(0.,(e-eth)/13.605692)
        call hphotx(erel,ic,nq,xsec,lun11,lpri)
        sg(ll)=xsec(lorb+1)*(1.e-18)
        stmpp(mm)=xsec(lorb+1)
        etmpp(mm)=erel
        call enxt(ett,nb1,lpri,epi,ncn2,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        enddo
      lprib=0
      if (lpri.gt.1) lprib=lpri           
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,xpx,opakab,
     $ opakc,lprib,epi,ncn2,bremsa,t,swrat,xnx,lfastl,lun11)
      lprim=0
      ntmp=ll-nb1
      temp=t*1.e+4
      ntmp=mm
      call milne(temp,ntmp,etmpp,stmpp,eth/13.6,ans2,lun11,lprim)
      ans2=ans2*swrat
      lpri=lprisv
      go to 9000
c
c
 65   continue
c     effective charge to be used in coll. ion. 
      tz=t*1.e+4
      idest1=idat1(np1i+nidt-2)
      idest2=nlevp
      ggup=rlev(2,nlevp)
      gglo=rlev(2,1) 
      eth=max(0.,rlev(1,nlevp)-rlev(1,idest1))
      ekt=0.861707*t
c      if (eth/ekt.gt.50.) go to 9000      
      call szirco(idat1(np1i),tz,rdat1(np1r),cii)
      ans1=cii*xnx
c     note that rinf has exponential removed
      rinf=(2.08e-22)*gglo/ggup/t/tsq
      ans2=ans1*rinf*expo(eth/ekt)
      go to 9000
c
 66   continue
c     Like type 69 but, data in fines tructure
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat1(np1i+1)).lt.rlev(1,idat1(np1i))) then
        idest2=idat1(np1i)
        idest1=idat1(np1i+1)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      elin=rdat1(np1r)
      if (elin.le.1.e-24) go to 9000
      elin=12398.41/elin
      ekt=0.861707*t
      delt=12398.41/elin/ekt
c      if (delt.gt.50.) go to 9000
      hij=elin*1.e-8
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/elin)
      call calt66(temp,np1r,rdat1,nrdt,gamma)
      cijpp=gamma
      cji=(8.626e-8)*cijpp/tsq/ggup
        exptmp=expo(-delt)
        cij=cji*ggup*exptmp/gglo
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpri.ge.1) then
        write (lun11,*)'ltyp=66',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdt,(rdat1(np1r-1+mm),mm=1,8),nind,jlo
        write (lun11,*)'       ',cij,cji,xnx,cijpp,exptmp
        endif
      elin=0.      
      go to 9000
c
 67   continue
c     Effective collision strengths from Keenan et al.
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat1(np1i+1)).lt.rlev(1,idat1(np1i))) then
        idest2=idat1(np1i)
        idest1=idat1(np1i+1)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      elin=abs(rdat1(np1r))
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/elin)
      call calt67(temp,np1r,rdat1,gamma)
      cijpp=gamma
      cijpp=max(0.,cijpp)
      cji=(8.626e-8)*cijpp/tsq/ggup
        exptmp=expo(-delt)
        cij=cji*ggup*exptmp/gglo
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=69',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdt,(rdat1(np1r-1+mm),mm=1,8),nind,jlo
        write (lun11,*)'       ',cij,cji,xnx,cijpp,exptmp
        endif
      elin=0.      
      go to 9000
c
 68   continue
c     coll. strength He-like ions by Zhang & Sampason 
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat1(np1i+1)).lt.rlev(1,idat1(np1i))) then
        idest2=idat1(np1i)
        idest1=idat1(np1i+1)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      elin=12398.41/abs(eeup-eelo+1.e-24)
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/elin)
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=68',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdt,(rdat1(np1r-1+mm),mm=1,8),nind,jlo
        endif
      call calt68(temp,np1r,np1i,rdat1,idat1,gamma)
      cijpp=gamma
      cijpp=max(cijpp,0.)
      cji=(8.626e-8)*cijpp/tsq/ggup
      ekt=0.861707*t
      delt=12398.41/elin/ekt
        exptmp=expo(-delt)
        cij=cji*ggup*exptmp/gglo
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpri.gt.1) then
        write (lun11,*)'       ',cij,cji,xnx,cijpp,exptmp
        endif
      elin=0.      
      go to 9000
c
 69   continue
c     Kato & Nakazaki (1996) fit to Helike coll. strgt
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat1(np1i+1)).lt.rlev(1,idat1(np1i))) then
        idest2=idat1(np1i)
        idest1=idat1(np1i+1)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      elin=12398.41/abs(eeup-eelo+1.e-24)
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      m=nrdt
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
c      temp=max(temp,2.8777e+6/elin)
      call calt69(temp,m,np1r,rdat1,gamma,lpri,lun11)
      cijpp=gamma
      cijpp=max(cijpp,0.)
      cji=(8.626e-8)*cijpp/tsq/ggup
      ekt=0.861707*t
      delt=12398.41/elin/ekt
        exptmp=expo(-delt)
        cij=cji*ggup*exptmp/gglo
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=69',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdt,(rdat1(np1r-1+mm),mm=1,8),nind,jlo
        endif
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpri.gt.1) then
        write (lun11,*)'       ',cij,cji,xnx,cijpp,exptmp
        endif
      elin=0.      
      go to 9000
c
 70   continue 
c     Coefficients for phot x-section of suplevels  
c      lfastl=lfast
      lfastl=3
      temp=t*1.e+4
      ans3=0.
      ans4=0.
      den=xpx
      m=1000
      lpric=0
c      if (lpri.ge.1) lpric=2
      mlion=npar(ml)
      idest1=idat1(np1i+nidt-2)
      idest1=min(idest1,nlev-1)
      idest2=nlev+idat1(np1i-1+nidt-3)-1
      idest2=max(idest2,nlev)
      ggup=rlev(2,nlevp)
      ett=abs(rlev(1,idest1)-rlev(1,nlevp))
      if (lpric.ge.1)
     $ write (lun11,*)'rlev:',idest1,nlevp,rlev(1,idest1),rlev(1,nlevp)
      if (idest2.gt.nlevp) then
        jkk3=jkion+1
        if (lpric.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        ndtmp=npfi(13,jkk3)
        if (lpric.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        mllz=npar(ndtmp)
        iltmp=0
        nptmp=mllz
        do while ((ndtmp.ne.0).and.(iltmp.ne.(idest2-nlevp+1))
     $      .and.(nptmp.eq.mllz)) 
           mlm=ndtmp-1
           call drd(ltyp2,lrtyp2,lcon2,
     $       nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $       nptrs,0,lun11)
           iltmp=idat1(np1i2+nidt2-2)
           if (lpric.gt.1) write (lun11,*)nidt2,iltmp,ndtmp
           ndtmp=npnxt(ndtmp)           
           nptmp=0
           if (ndtmp.ne.0) nptmp=npar(ndtmp)
           enddo
         ggup=rdat1(np1r2+1)
         ett=abs(rlev(1,idest1)+rdat1(np1r2))
         endif
       if (lpric.ge.1)
     $    write (lun11,*) ndtmp,iltmp,idest2,ggup,ett
      xkt=ett/(0.861707*t)
      nb1=nbinc(ett,epi,ncn2)
      if (lforce.ne.1) then
        xkto=vsav(4,ml)
        tq=ee1exp(xkt)/max(1.e-24,ee1exp(xkto))
        ans1=rates(1,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans2=rates(2,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        ans3=rates(3,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans4=rates(4,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        if (lpric.gt.1) write (lun11,*)'type 53 scaling:',
     $    ml,vsav(2,ml),r19,tq,xnx,vsav(3,ml),xkt,xkto,tst,
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml),ett,t
        go to 9000
        endif
      vsav(2,ml)=r19
      vsav(1,ml)=bremsint(nb1)
      vsav(3,ml)=xnx
      vsav(4,ml)=xkt
      mlm=mlion-1
      call drd(ltyp2,lrtyp2,lcon2,
     $  nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $  nptrs,0,lun11)
      ist=idat1(np1i2)
      ic=ist
      eth=ett
      gglo=rlev(2,idest1)
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      if (lpric.ne.0) then
        write (lun11,*)'type 70 data:',idat1(np1i),idat1(np1i+nidt-1),t,
     $           xnx,eth,gglo,ggup,swrat
        call dprinto(ndesc,nrdesc,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,rdat1,idat1,kdat1,lun11) 
        endif
      ettry=ett/13.6
      call calt70(temp,den,ettry,ic,m,np1r,np1i,rdat1,idat1,
     1             ntmp,etmpp,stmpp,rec,al,lun11,lpric)
      if (lpric.ne.0) write (lun11,*)'after  calt70:',rec,stmpp(1)
      crit53=0.01
      do mm=1,ntmp
        stmpp(mm)=stmpp(mm)*1.e-18
        stmpp(mm)=max(stmpp(mm),0.)
        enddo
      call phint53hunt(stmpp,etmpp,ntmp,ett,ans1,ans2d,ans3d,ans4s,
     $ lpric,epi,ncn2,bremsa,t,swrat,xnx,crit53,lfastl,lun11)
      if (ans2d.le.1.e-36) then
        ans1=0.
        ans2=0.
        go to 9000
        endif
      scale=rec*xnx/ans2d
      ans1=ans1*scale
c     does the swrat not belong?
c      ans2=rec*xnx*swrat
      ans2=rec*xnx
c      ans2=ans2d
      tm=t*1.e4
      q2=2.07e-16*xnx*(tm**(-1.5))
      rs=q2/swrat
      ans1o=ans1
c      ans1=min(ans1,ans2/rs)
      if (lpric.ge.2)
     $ write (lun11,*)'type 70 limit:',ans2,rs,swrat,
     $   xnx,tm,q2,ans1o,ans1,scale,rec
c
c     testing superlevel phot.
c      ans1=0.
c
      rates(1,ml)=ans1
      rates(2,ml)=ans2
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      if (lpric.ge.1) write (lun11,*)'rates:',
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml)
      go to 9000
c
 71   continue
c     Transition rates from superlevel to spect. lvls 
      temp=t*1.e+4
      lpril=0
      den=xpx
      m=1000
      if (lpril.ne.0)
     $  write (lun11,*)'before calt71:',rdat1(np1r),
     $    rdat1(np1r+1),rdat1(np1r+2)
      call calt71(temp,den,ic,m,np1r,np1i,rdat1,idat1,
     $            wav,aij,lun11,lpril)
      idest1=idat1(np1i-1+nidt-3)
      idest2=idat1(np1i+nidt-3)
      if ((idest1.le.0).or.(idest1.gt.nlev).or.
     $   (idest2.le.0).or.(idest2.gt.nlev)) go to 9000
      if (lpril.ne.0)
     $ write (lun11,*)idest1,idest2,aij,wav,ml
      ans1=aij
c
c
      ans2=0.
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      nelin=npar(nilin)
      elin=wav
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
      flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
      mlm=nelin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      a=rdat1(np1r2+1)
      elammu=elin*1.e-4     
      ans1=aij*(ptmp1+ptmp2)
c     special fudge for ca i and ca ii   
      if ((idat1(np1i-1+6).eq.96).or.(idat1(np1i-1+6).eq.97))
     $ ans1=min(ans1,1.e+10)
c
      vtherm=((vturb*1.e+5)**2+(1.29e+6/sqrt(a/t))**2)**(0.5)
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
      sigvtherm=sigma
      ener=12398.41/abs(elin)
      nb1=nbinc(ener,epi,ncn2)
      ans2=0.
c      ans2=sigvtherm*bremsa(nb1)*vtherm/3.e+10
      if (elin.gt.0.99e+9) then 
         ans2=0.
         sigvtherm=0.
         endif
      ans1=ans1+ans2*ggup/(1.e-36+gglo)
      opakab=sigvtherm
      ans3=ans2*ener*ergsev
      ans4=ans1*ener*ergsev
      if (elin.gt.0.1) then
        dele=12398.41/(elin+1.e-24)
        ans4=ans1*dele*(1.602197e-12)
        endif
c      ans4=0.
      if (lpril.ne.0)
     $ write (lun11,*)' ',vtherm,ans2,ans4,flin
      go to 9000
c
 72   continue
c     Autoinization rates (in s^-1) for satellite lvls
      lpril=0
      idest1=idat1(np1i-1+nidt-3)
      idest2=idat1(np1i+nidt-3)
      temp=t*1.e+4
      call calt72(temp,np1r,rdat1,nrdt,rate,lun11,lpril)  
      ans1=rate*xnx
      ans2=0.
      ggup=rlev(2,nlevp)
      gglo=rlev(2,1) 
c     note that rinf has exponential removed
      rinf=(2.08e-22)*gglo/ggup/t/tsq
      dele=rdat1(np1r+1)
      ans2=rate*xnx*rinf*xnx*expo(dele/temp)
      go to 9000
c
 75   continue
c     Autoinization rates (in s^-1) for satellite lvls
c        now including final ion stage
      lpril=0
      idest3=idat1(np1i+nidt-1)
      idest4=idat1(np1i+nidt-3)
      idest2=idat1(np1i+nidt-2)+nlev-1
      idest1=idat1(np1i-1+nidt-3)
      idest1=max(idest1,1)
      idest2=max(idest2,1)
      temp=t*1.e+4
      call calt72(temp,np1r,rdat1,nrdt,rate,lun11,lpril)  
      ans1=rate*xnx
      ans2=0.
      go to 9000
c
 73   continue
c     Fit to coll. strengths satellite lvls Helike ion
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat1(np1i+1)).lt.rlev(1,idat1(np1i))) then
        idest2=idat1(np1i)
        idest1=idat1(np1i+1)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      elin=abs(rdat1(np1r))
      hij=elin*1.e-8
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      if (elin.le.1.e-24) go to 9000
      m=1000
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/elin)
      crate=0.
      call calt73(temp,np1r,np1i,rdat1,idat1,crate)
c      write (lun11,*)'type 73 calc:',
c     $  (rdat1(np1r-1+lk),lk=1,7),(idat1(np1i-1+lk),lk=1,4),crate,
c     $  gglo,ggup
      cijpp=crate/gglo
      cijpp=max(cijpp,0.)
      cji=(8.626e-8)*cijpp/tsq/ggup
        exptmp=expo(-delt)
       cij=cji*ggup*exptmp/gglo
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=69',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdt,(rdat1(np1r-1+mm),mm=1,8),nind,jlo
        write (lun11,*)'       ',cij,cji,xnx,cijpp,exptmp
        endif
      elin=0.      
      go to 9000
c
 74   continue
c     Delta functions to add to phot. x-sections  DR 
      temp=t*1.e+4
      den=xpx
      m=1000
      rec=0.
      lprisv=lpri
c      if (lpri.ge.1) lpri=2      
      if (lpri.gt.1) write (lun11,*)'type 74 data:',den,temp,
     $ (rdat1(np1r-1+mm),mm=1,nrdt),(idat1(np1i-1+mm),mm=1,nidt)
      call calt74(temp,ncn2,epi,bremsa,nrdt,np1r,rdat1,rate,
     $       alpha) 
      idest1=idat1(np1i+nidt-2)
      idest2=nlevp
      idest3=idat1(np1i+nidt-1)
      idest4=idest3+1
      gglo=rlev(2,idest1)
      ggup=rlev(2,idest2)
      if (lpri.gt.1) write (lun11,*)'returning from calt74:',
     $  rate,alpha,idest1,idest2,gglo,ggup
      ans1=rate
      alpha=alpha*gglo/ggup
      ans2=alpha
      lpri=lprisv
      go to 9000
c
 81   continue
c     bhatia Fe XIX
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat1(np1i+1)).lt.rlev(1,idat1(np1i))) then
        idest2=idat1(np1i)
        idest1=idat1(np1i+1)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      elin=12398.41/abs(eeup-eelo+1.e-24)
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      m=nrdt
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/elin)
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=75',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdt,(rdat1(np1r-1+mm),mm=1,8),nind,jlo
        endif
      cijpp=rdat1(np1r)
      cijpp=max(cijpp,0.)
      cji=(8.626e-8)*cijpp/tsq/ggup
      ekt=0.861707*t
      delt=12398.41/elin/ekt
        exptmp=expo(-delt)
        cij=cji*ggup*exptmp/gglo
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpri.gt.1) then
        write (lun11,*)'       ',cij,cji,xnx,cijpp,exptmp
        endif
      elin=0.      
      go to 9000
c
 76   continue
c     2 photon decay (just  like 50)
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      lpril=lpri
      aij=rdat1(np1r)
      eeup=rlev(1,idest1)
      eelo=rlev(1,idest2)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         endif
      elin=12398.41/abs(eeup-eelo)
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      nelin=npar(nilin)
      if ((nilin.le.0).or.(nelin.le.0)) go to 9000
      flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
      mlm=nelin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      a=rdat1(np1r2+1)
      elammu=elin*1.e-4     
c      if (flin.le.1.e-10) flin=1.
      ans1=aij
      vtherm=((vturb*1.e+5)**2+(1.29e+6/sqrt(a/t))**2)**(0.5)
      ans2=0.
      ans4=aij*ergsev*12398.41/abs(elin)
      ansar2=0.
      em2ph=aij
      lskp=1
      emax=12398.41/elin
      nbmx=nbinc(emax,epi,ncn2)
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=76:',
     $  ml,nilin,nelin,elin,flin,rdat1(np1r),gglo,ggup,a,vtherm,ans2
     $   ,nbmx
        rcemsum=0.       
        lfastl=0
        ll=1+lskp
        do while (ll.le.nbmx)
          ansar2o=ansar2
          ansar2=epi(ll)*epi(ll)*max(0.,(epi(nbmx)-epi(ll)))
          rcemsum=rcemsum+(ansar2+ansar2o)
     $                   *(epi(ll)-epi(ll-lskp))/2.
          call enxt(epi(1),nb1,lpril,epi,ncn2,t,lfastl,lun11,
     $                  ll,lskp,nphint,lrcalc)
          ll=ll+lskp
          enddo
c        rcemsum=(emax**3)/12.
        rctmp1=0.
        rctmp2=0.
        ll=2
        do while (ll.le.nbmx) 
          ansar2=epi(ll)*epi(ll)*max(0.,(epi(nbmx)-epi(ll)))
          ansar2=ansar2*em2ph*emax/(1.e-24+rcemsum)
          rctmp1=abund2*ansar2*ptmp1/12.56
          rctmp2=abund2*ansar2*ptmp2/12.56
          rccemis(1,ll)=rccemis(1,ll)+rctmp1
          rccemis(2,ll)=rccemis(2,ll)+rctmp2
          call enxt(epi(1),nb1,lpril,epi,ncn2,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
          ll=ll+nskp
          enddo
        if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=76:',
     $  ml,nilin,nelin,elin,flin,rdat1(np1r+2),gglo,ggup,a,vtherm,ans2
        ans4=aij*ergsev*12398.41/abs(elin)
        go to 9000
c
 77   continue
c     coll rates from 71 
c     Transition rates from superlevel to spect. lvls 
c      go to 9000
      den=xpx
      m=1000
      clu=0.
      cul=0.
      idest1=idat1(np1i-1+nidt-3)
      idest2=idat1(np1i+nidt-3)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      eup=rlev(1,idest2)
      elo=rlev(1,idest1)
      wav=12398.41/(eup-elo+1.e-24)
      ekt=0.861707*t
      delt=wav/ekt
      lprit=0
c      if (lpri.ne.0) lprit=1
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/wav)
      call calt77(lprit,lun11,temp,den,m,np1r,np1i,rdat1,idat1,cul,clu)
      ans1=clu
      ans2=cul
      go to 9000
c
 78   continue

      go to 9000
c
 79   continue
c     fluorescence lines
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      elin=abs(rdat1(np1r))
      flin=rdat1(np1r+1)
c      if (flin.le.1.e-10) flin=1.
      eeup=rlev(1,idest1)
      eelo=rlev(1,idest2)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         endif
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
      a=rdat1(np1r-1+5)
      hij=elin*1.e-8
      elammu=elin*1.e-4     
      aij=(6.67e+7)*gglo*flin/ggup/elammu/elammu
c     this is a fudge to avoid badnumerics from fine structure.
      if (flin.le.1.01e-12) aij=1.e+5
      if (elin.ge.1.e+9) aij=1.e+5
      ans1=aij*(ptmp1+ptmp2)
      ans4=aij*(ptmp1+ptmp2)*ergsev*12398.41/abs(elin)
      vtherm=((vturb*1.e+5)**2+(1.29e+6/sqrt(a/t))**2)**(0.5)
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
      sigvtherm=sigma
c     notice that opakab does not have abundance in
      opakab=sigvtherm
c      ans2=(0.02655)*flin*elin*(1.e-8)/vtherm
      ans2=0.
      go to 9000
c
 80   continue
c Collisional ionization rates gnd of Fe and Ni  
      go to 9000
c
 82   continue
c     Fe UTA rad rates
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
c     nb check this out:  no bound-bound decays from continuum
      if ((idest1.le.0).or.(idest1.ge.nlev)
     $  .or.(idest2.le.0).or.(idest2.ge.nlev))
     $      go to 9000
      gflin=rdat1(np1r+2)
      aij=rdat1(np1r-1+4)
      eeup=rlev(1,idest1)
      eelo=rlev(1,idest2)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         endif
      elin=abs(rdat1(np1r))
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      nelin=npar(nilin)
      if ((nilin.le.0).or.(nelin.le.0)) go to 9000
c      flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
      flin=gflin
      mlm=nelin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      a=rdat1(np1r2+1)
      vtherm=((vturb*1.e+5)**2+(1.29e+6/sqrt(a/t))**2)**(0.5)
      ener=12398.41/elin
      dele=ener*vtherm/3.e+10
      delev=vtherm/(elin*(1.e-8))
      delea=rdat1(np1r-1+6)*(4.14e-15)
      elammu=elin*1.e-4     
      ans1=aij*(ptmp1+ptmp2)
      sigma=(0.02655)*flin/delev
c      sigvtherm=(0.02655)*flin*elin*(1.e-8)/3.e+10
      sigvtherm=sigma
      jkkl=nplini(ml)                  
      if (jkkl.le.0) go to 9000
      ml3=nplin(jkkl)
      if (ml3.le.0) go to 9000
      mlm=ml3-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r,nidt,np1i,nkdt,np1k,mlm,
     $  nptrs,0,lun11)
      elin=abs(rdat1(np1r))
      ener=12398.41/abs(elin)
      nb1=nbinc(ener,epi,ncn2)
      ans2=sigvtherm*bremsa(nb1)*vtherm/3.e+10
c
c     turning off rex
      ans2=0.
c
c      ans4=ans1*ener*ergsev
c     notice that opakab does not have abundance in
      opakab=sigvtherm
      lfasto=2
      ans3=ans2*ener*ergsev
c     this is a cheat.  there is still an error in the 82/83 data that 
c       makes some fluorescence emission
c     this test should prevent calculation when called from func2
c     since abund1 will be zero
      opakb1=sigvtherm*abund1
c      lpriu=lpri
      lpriu=0
      rcem1=0.
      rcem2=0.
      if (opakb1.gt.1.e-34)
     $ call linopac(lpriu,lun11,opakb1,ans2,sigvtherm,vtherm,bremsa,
     $               rcem1,rcem2,elin,vturb,t,a,delea,epi,ncn2,
     $               opakc,opakscatt,rccemis,fline,lfasto)
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=82:',
     $  ml,nilin,nelin,elin,flin,rdat1(np1r+2),gglo,ggup,a,vtherm,ans2,
     $  idest1,idest2,idest3,idest4,nlev,sigvtherm,bremsa(nb1),nb1
      go to 9000
c
c
 83   continue
c     Fe UTA level data
      go to 9000
c
 84   continue
      lprisv=lpri
      lpril=0
      go to 9000
c      if (lpri.ge.1) lpril=2
      if (lpril.gt.1) write (lun11,*)'ltyp=84',ml,npar(ml)
      if (lpril.gt.1) write (lun11,*)(rdat1(np1r-1+jj),jj=1,nrdt)
      if (lpril.gt.1) write (lun11,*)(idat1(np1i-1+jj),jj=1,nidt)
      if (lpril.gt.1) write (lun11,*)(kdat1(np1k-1+jj),jj=1,nkdt)
      if (ml.le.0) go to 9000
      lfastl=lfast
      nilin=npar(ml)
      idest3=idat1(np1i+nidt-1)
      idest4=idest3+1
      idest1=idat1(np1i+nidt-2)
      idest2=1
      ntmp=nrdt/2-1  
      ett2=rdat1(np1r)
      ett=rdat1(np1r+2)*13.605692
      ediff=rdat1(np1r-1+2*ntmp)*13.605692-ett2
      scal2=rdat1(np1r+1)
      do ml2=1,ntmp
        etmpp(ml2)=rdat1(np1r-1+2*ml2+1)-rdat1(np1r+2)
        stmpp(ml2)=rdat1(np1r-1+2*ml2+2)*1.e-18*scal2
        stmpp(ml2)=max(stmpp(ml2),0.)
        if (lpril.gt.1) write (lun11,*)ml2,etmpp(ml2),stmpp(ml2)
        enddo
      ett=ett2-(rdat1(np1r-1+2*ntmp+1)-rdat1(np1r+2))*13.6
      ntmp2=nptmpdim
      call phextrap(etmpp,stmpp,ntmp,ntmp2,ett,ncn2,lpri,lun11)
      nb1=nbinc(ett,epi,ncn2)
      numcon2=max(2,ncn2/50)
c         numcon2=200
      nphint=ncn2-numcon2
      nphint=max(nphint,nb1+1)
      if (lpril.gt.1) 
     $ write (lun11,*)'ltyp=84:',ett,ett2,ediff,ntmp,
     $  etmpp(1),stmpp(1),etmpp(ntmp+1),stmpp(ntmp+1),nb1,nphint
      if (nb1.ge.nphint-1) go to 9000
      if (lpril.gt.1)
     $ write (lun11,*)ett,nb1,bremsint(nb1),ml,vsav(1,ml),tst,
     $  lforce       
       lprib=0
       lprib=lpril
c       call phint5384(stmpp,etmpp,ntmp,ett,ans1,ans2,ans3,ans4,
c     $   abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
c     $   opakc,rccemis,lprib,epi,ncn2,bremsa,t,trad,swrat,xnx,crit53,
c     $    lfast,lun11)
      ans1=0.
      ans3=0.
      ans4=0.
      ans2=0.
      rates(1,ml)=ans1
      rates(2,ml)=ans2
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      lpri=lprisv
      go to 9000

c
 85   continue
      lprisv=lpri
      lpril=0
c      if (lpri.ge.1) lpril=2
      if (lpril.gt.1) write (lun11,*)'ltyp=85',ml,npar(ml)
      if (lpril.gt.1) write (lun11,*)(rdat1(np1r-1+jj),jj=1,nrdt)
      if (lpril.gt.1) write (lun11,*)(idat1(np1i-1+jj),jj=1,nidt)
      if (lpril.gt.1) write (lun11,*)(kdat1(np1k-1+jj),jj=1,nkdt)
      if (ml.le.0) go to 9000
      lfastl=1
      nilin=npar(ml)
      idest3=idat1(np1i+nidt-1)
      idest4=idest3+1
      idest1=idat1(np1i+nidt-2)
      idest2=1
      ett2=rdat1(np1r+1)*13.605692
      nmin=idat1(np1i)
      jkk=idest3
      zc=dfloat(jkk-114)
      eion=dble(rdat1(np1r+1))
      kdim=ncn2
      far=dble(rdat1(np1r+2))
      gam=dble(rdat1(np1r-1+4))
      scal=dble(rdat1(np1r-1+5))
      call pexs(nmin,kdim,zc,eion,far,gam,scal,
     +                etmp8,stmp8,ierr,lpril,lun11)
      do mm=1,ncn2
        stmpp(mm)=sngl(stmp8(mm))*1.e-18
        enddo
      call phintfo(stmpp,ett2*0.8,ans1,ans2,ans3,ans4,
     $ abund1,abund2,xpx,opakab,
     $ opakc,lpril,epi,ncn2,bremsa,t,swrat,xnx,lfastl,lun11)
      opakab=0.
      ans4=0.
      ans2=0.
      rates(1,ml)=ans1
      rates(2,ml)=ans2
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      lpri=lprisv
      go to 9000

 86   continue
c     iron auger data
c     this statement causes pileup of populations in some superlevels.
c      if (idat1(np1i-1+nidat-1).ne.idat1(np1i-1+nidat)+1) go to 9000
      ans1=rdat1(np1r+1)
      ans2=0.
      idest1=idat1(np1i-1+nidt-3)
c      idest2=nlevp
      idest2=nlevp+idat1(np1i-1+nidt-4)-1
      idest3=idat1(np1i+nidt-1)
      idest4=idest3+1
      go to 9000
c
 87   continue
      go to 9000
c
 88   continue
c     op inner shell photoexcitation 
      lprisv=lpri
      idest1=idat1(np1i+nidt-2)
c      idest2=idat1(np1i+nidt-3)
      idest2=nlevp
c      if (lpri.ge.1) lpri=2
      lunsv=lun11
      if (lpri.gt.1) write (lun11,*)'ltyp=88,idest1=',idest1,idest2
      if ((idest1.ge.nlevp).or.(idest1.le.0)) go to 9000
      if (ml.le.0) go to 9000
      eth=rlev(4,idest1)-rlev(1,idest1)
      ett=eth
      nilin=npar(ml)
      if (lpri.gt.1) write (lun11,*)'nilin=',nilin,ml
      if (nilin.le.0) go to 9000
      ntmp=nrdt/2
      do ml2=1,ntmp
        etmpp(ml2)=rdat1(np1r-1+2*ml2-1)
        stmpp(ml2)=rdat1(np1r-1+2*ml2)*1.e-18
        stmpp(ml2)=max(stmpp(ml2),0.)
        enddo
      ntmp2=nptmpdim
      call phextrap(etmpp,stmpp,ntmp,ntmp2,ett,ncn2,lpri,lun11)
c      ett=ett+max(0.,13.605692*etmpp(1))
      if (lpri.gt.1) write (lun11,*)'ett=',ett,etmpp(1)
      if (ett.le.0.) go to 9000
      nb1=nbinc(ett,epi,ncn2)
      tst=abs(bremsint(nb1)/max(1.e-24,vsav(1,ml))-1.)
      xkt=ett/(0.861707*t)
      r19=rr/1.e+19
      if (lforce.ne.1) then
        xkto=vsav(4,ml)
        tq=ee1exp(xkt)/max(1.e-24,ee1exp(xkto))
        ans1=rates(1,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans2=rates(2,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        ans3=rates(3,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans4=rates(4,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        if (lpri.gt.1) write (lun11,*)'type 49 scaling:',
     $    ml,vsav(2,ml),r19,tq,xnx,vsav(3,ml),xkt,xkto,tst,
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml),ett,t
        go to 9000
        endif
      vsav(2,ml)=r19
      vsav(1,ml)=bremsint(nb1)
      vsav(3,ml)=xnx
      vsav(4,ml)=xkt
      mlm=nilin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdti,np1r2,nidti,np1i2,nkdti,np1k2,mlm,
     $  nptrs,0,lun11)
      emax=etmpp(ntmp)*13.6+eth
      gglo=rlev(2,idest1)
      ggup=rlev(2,idest2)
      idest3=idat1(np1i-1+nidti)
      idest4=idest3+1
      if (lpri.gt.1) write (lun11,*)'before phint53',gglo,ggup
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      if (lpri.gt.1) then
        write (lun11,*)'type 88 data:',idat1(np1i),idat1(np1i+nidt-1),
     $           t,xnx,eth,gglo,ggup,swrat
        call dprinto(ndesc,nrdesc,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,rdat1,idat1,kdat1,lun11) 
        endif
c      if ((lpri.ge.1).and.(idest1.le.4).and.(jkion.eq.29)
c     $    .and.(abund1.gt.1.e-34))  then
c        lun99=99
c        write (lun99,*)'type 88 data:', idest1, idest2, 
c     $           eth,gglo,ggup,swrat, abund1,abund2
c        call dprinto(ndesc,nrdesc,lcon,
c     $          nrdt,rdat,nidt,idat,nkdt,kdat,lun99) 
c        do mm=1,ncn2
c          opaksv(mm)=opakc(mm)
c          opakc(mm)=0.
c          enddo
c        endif
      lprib=lpri
      lprib=0
      if (lpri.gt.1) lprib=lpri
      rnist=rniss(idest1)*exp(-ett/(0.861707)/t)/rniss(nlevp)
      call phint53(stmpp,etmpp,ntmp,ett,ans1,ans2,ans3,ans4,
     $  abund1,abund2,ptmp1,ptmp2,xpx,opakab,rnist,
     $  opakc,rccemis,lprib,epi,ncn2,bremsa,t,swrat,xnx,
     $  lfast,lun11)
c      if ((lpri.ge.1).and.(idest1.le.4).and.(jkion.eq.29)
c     $    .and.(abund1.gt.1.e-34))  then
c        nhit=0
c        do mm=1,ncn2
c          if ((opakc(mm).gt.1.e-34).and.(epi(mm).gt.500.)
c     $        .and.(epi(mm).lt.800.)) then
c            write (lun99,919)mm,epi(mm),opakc(mm),
c     $        opakc(mm)/max(1.e-34,abund1)/xpx,opaksv(mm)+opakc(mm)
c            nhit=1
c            endif  
c          opakc(mm)=opaksv(mm)+opakc(mm)
c          enddo
c        if (nhit.eq.0) write (lun99,*)'no cross section'
c        endif
      if (lpri.gt.1) then
        npr=nb1
        write (lun11,*)'bautista threshold xsection:',
     $         npr,ett,eth,rdat1(np1r),sg(npr),ans2,swrat
        endif
      rates(1,ml)=ans1
      ans2=0.
      ans4=0.
      rates(2,ml)=ans2
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      if (lpri.gt.1) write (lun11,*)'rates:',
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml)
      lun11=lunsv
      lpri=lprisv
      go to 9000
c
 89   continue
      go to 9000
c
 90   continue
      go to 9000
c
 91   continue
c     a values from atomdb.  same as 50.
      go to 50
      go to 9000
c
 92   continue
c     collision strengths from atomdb
c 
      lpril=0
c      if (lpri.ge.1) lpril=2     
      if (lpril.gt.1) write (lun11,*)'ltyp=92',ml,npar(ml)
      if (lpril.gt.1) write (lun11,*)(rdat1(np1r-1+jj),jj=1,nrdt)
      if (lpril.gt.1) write (lun11,*)(idat1(np1i-1+jj),jj=1,nidt)
      if (lpril.gt.1) write (lun11,*)(kdat1(np1k-1+jj),jj=1,nkdt)
c
c     
c     general stuff
      lctype=idat1(np1i+3-1)
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      tmin=rdat1(np1r)
      tmax=rdat1(np1r+1)
      do mml=1,20
        tstr(mml)=rdat1(np1r+1+mml)
        cstr(mml)=rdat1(np1r+21+mml)
        enddo
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      elin=12398.41/abs(eeup-eelo+1.e-24)
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      temp=t*1.e+4
      eij=abs(eeup-eelo)
      eijkev=eij/1.e+3
      tk=t*1.e+4
      mlm=npar(ml)-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      nistage=idat1(np1i2)
      mlm=npar(mlm)-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      nzel=idat1(np1i2)
      if (lpril.gt.1)
     $ write (lun11,*)'before calc_maxwell_rates',lctype,tmin,tmax,
     $   eijkev,tk,zzz,gglo,ggup
      call calc_maxwell_rates(lun11,lpril,lctype,tmin,tmax, 
     $  Tstr,cstr, eijkev,  tk, nzel,  gglo,  ggup,  cij, cji) 
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpril.gt.1) then
        write (lun11,*)'type 92 data',lctype,cij,cji,xnx
        endif
       go to 9000
c
c      old code for 92 not used
c      delt=12398.41/elin/ekt
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
c      eijry=eij/13.605692
c      tsq=sqrt(t)
c      cijpp=0.
c      temp=max(temp,2.8777e+6/elin)
c      if (lctype.eq.11) then
cc       chianti type 1 (dere et al. 1997)
c        cijpp=upsil(1,eijry,cijpp,cstr(1),cstr(2),
c     $      cstr(3),cstr(4),cstr(5),tk)
c        endif
c      if (lctype.eq.12) then
cc       chianti type 2 (dere et al. 1997)
c        cijpp=upsil(2,eijry,cijpp,cstr(1),cstr(2),
c     $      cstr(3),cstr(4),cstr(5),tk)
c        endif
c      if (lctype.eq.13) then
cc       chianti type 3 (dere et al. 1997)
c        cijpp=upsil(3,eijry,cijpp,cstr(1),cstr(2),
c     $      cstr(3),cstr(4),cstr(5),tk)
c        endif
c      if (lctype.eq.14) then
cc       chianti type 4 (dere et al. 1997)
c        cijpp=upsil(4,eijry,cijpp,cstr(1),cstr(2),
c     $      cstr(3),cstr(4),cstr(5),tk)
c        endif
c      if (lctype.eq.31) then
cc       sampson goett and clark 1983 type 1      
c        if (nrdt.lt.7) go to 9000
c        y=eij/ekt
c        aa=rdat1(np1r+2)
c        co=rdat1(np1r+3)
c        cr=rdat1(np1r+4)
c        crp=rdat1(np1r+5)
c        rr=rdat1(np1r+6)
c        sig=rdat1(np1r+6)
c        z2s=rdat1(np1r+7)
c        zeff=float(idat1(np1i+2))-sig
c        if (y.gt.40.)  go to 9000
c        call expint(y,em1)
c        e1=em1/y*exp(-y)
c        if (y*a+y.le.80) then
c            call eint(y*a+y,ee1,ee2,ee3)
c          else
c            ee1=0.
c            ee2=0.
c            ee3=0.
c          endif
c        er=0.
c        er1=0.
c        if (rr.eq.1.) then
c          er=ee1
c          er1=ee2
c          endif
c        if (rr.eq.2.) then
c          er=ee2
c          er1=ee3
c          endif
c        if (y*a+y.le.40) then
c            qij=co*exp(-y)+1.55*z2s*e1+y*exp(y*a)*(cr*er/(a+1.)**(rr-1.)
c     #      +cr1*er1/(a+1.)**rr)
c          else
c            qij=co*exp(-y)+1.55*z2s*e1
c          endif
c        cijpp=qij*exp(y)/zeff/zeff
c        endif
c      if (lctype.eq.32) then
cc       sampson goett and clark 1983 type 2
c        endif
c      if (lctype.eq.33) then
cc       sampson goett and clark 1983 type 3 
c        endif
c      if (lctype.eq.41) then
cc       kato and nakazaki 1989 type 1 
c        call calt66(temp,np1r+2,rdat1,gamma)
c        cijpp=gamma
c        endif
c      if (lctype.eq.42) then
cc       kato and nakazaki 1989 type 3
c        go to 9000
c        endif
c      if (lctype.gt.100) then
c        ncase=int(lctype/50)
cc       ltype=100 -->ncase=2
cc       ltype=150 -->ncase=3
cc       ltype=200 -->ncase=4
cc       ltype=250 -->ncase=5
cc       ltype=300 -->ncase=6
cc       ltype=350 -->ncase=7
cc       ltype=400 -->ncase=8
cc       ltype=450 -->ncase=9
cc       ltype=500 -->ncase=10
cc       ltype=550 -->ncase=11
cc       ltype=600 -->ncase=12
cc       ltype=650 -->ncase=13
cc       ltype=700 -->ncase=14
cc       ltype=750 -->ncase=15
cc       ltype=800 -->ncase=16
cc       ltype=850 -->ncase=17
cc       ltype=900 or greater -->ncase=18
cc       don't do qs
c        if ((ncase.ge.6).and.(ncase.le.9)) stop 'ncase=6-9'
cc        if ((ncase.ge.6).and.(ncase.le.9)) go to 9000
c        if (ncase.ge.14) stop 'ncase=14'
cc        if (ncase.ge.14) go to 9000
cc
c        npts=lctype-50*ncase
c        if ((tk.le.tmin).or.(tk.ge.tmax)) go to 9000
c        mm=1
c        do while ((tk.lt.tstr(mm)).and.(mm.lt.npts))
c          mm=mm+1
c          enddo
c        mm=max(mm-1,1)
c        cijpp=cstr(mm)+(cstr(mm+1)-cstr(mm))*(tk-tstr(mm))
c     $                 /(tstr(mm+1)-tstr(mm)+1.e-38)
c        endif
c
c      cji=(8.626e-8)*cijpp/tsq/ggup
c      ekt=0.861707*t
c      delt=12398.41/elin/ekt
c      exptmp=expo(-delt)
c      cij=cji*ggup*exptmp/gglo
c
 93   continue
      go to 9000
c     op pi xsections
      lprisv=lpri
      if (nrdt.gt.3) go to 533
      idest1=idat1(np1i+nidt-2)
      idest2=nlevp+idat1(np1i-1+nidt-3)-1
      if (lpri.gt.1) write (lun11,*)'idest1=',idest1,idest2,nlevp,ml
      if ((idest1.ge.nlevp).or.(idest1.le.0)) go to 9000
      if (ml.le.0) go to 9000
      eth=rlev(4,idest1)-rlev(1,idest1)
      eexc=rlev(1,idest1)
      ett=eth
      nilin=npar(ml)
      if (lpri.gt.1) write (lun11,*)'nilin=',nilin,ml
      if (nilin.le.0) go to 9000
      ntmp=nrdt/2
c      ett=ett+max(0.,13.605692*etmpp(1))
      if (lpri.gt.1) write (lun11,*)'ett=',ett,etmpp(1)
      if (ett.le.0.) go to 9000
      nb1=nbinc(ett,epi,ncn2)
      tst=abs(bremsint(nb1)/max(1.e-24,vsav(1,ml))-1.)
      xkt=ett/(0.861707*t)
      r19=rr/1.e+19
c      if ((tst.le.0.01).and.(lforce.ne.1)) then
      if (lforce.ne.1) then
        xkto=vsav(4,ml)
        tq=ee1exp(xkt)/max(1.e-24,ee1exp(xkto))
        ans1=rates(1,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans2=rates(2,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        ans3=rates(3,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans4=rates(4,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        if (lpri.gt.1) write (lun11,*)'type 93 scaling:',
     $    ml,vsav(2,ml),r19,tq,xnx,vsav(3,ml),xkt,xkto,tst,
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml),ett,t
        go to 9000
        endif
      vsav(2,ml)=r19
      vsav(1,ml)=bremsint(nb1)
      vsav(3,ml)=xnx
      vsav(4,ml)=xkt
      mlm=nilin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      gglo=rlev(2,idest1)
      ggup=rlev(2,nlevp)
      idest3=idat1(np1i+nidt-1)
      idest4=idest3+1
      if (idest2.gt.nlevp) then
        jkk3=jkion+1
        if (lpri.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        ndtmp=npfi(13,jkk3)
        if (lpri.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        if (ndtmp.le.0) go to 9000
        mllz=npar(ndtmp)
        iltmp=0
        do while ((ndtmp.ne.0).and.(iltmp.ne.(idest2-nlevp+1))
     $      .and.(npar(ndtmp).eq.mllz)) 
           mlm=ndtmp-1
           call drd(ltyp2,lrtyp2,lcon2,
     $       nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $       nptrs,0,lun11)
           iltmp=idat1(np1i2+nidt2-2)
           if (lpri.gt.1) write (lun11,*)nidt2,iltmp,ndtmp
           ndtmp=npnxt(ndtmp)     
           if (ndtmp.le.0) go to 9000                 
           enddo
c        NB fix to excited level PI and rec
         ett=ett+rdat1(np1r2)
         eth=ett
         ggup=rdat1(np1r2+1)
         if (lpri.gt.1)
     $    write (lun11,*) ndtmp,iltmp,idest2,ggup,ett
         endif
      if (lpri.gt.1) write (lun11,*)'before phint53',eexc,eth,lfast
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      if (lpri.gt.1) then
        write (lun11,*)'type 93 data:',idat1(np1i),idat1(np1i+nidt-1),
     $           t,xnx,eth,gglo,ggup,swrat
        call dprinto(ndesc,nrdesc,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,rdat1,idat1,kdat1,lun11) 
        endif
      lprib=0
      if (lpri.gt.1) lprib=lpri
      sth=1.e-18*rdat1(np1r+1)
      alph=rdat1(np1r+2)
      e1=rdat1(np1r)
      lfastl=1
      call phint53pl(sth,e1,alph,ett,ans1,ans2,ans3,ans4,
     $  abund1,abund2,ptmp1,ptmp2,xpx,opakab,
     $  opakc,rccemis,lprib,epi,ncn2,bremsa,t,swrat,xnx,
     $  lfastl,lun11)
      if (lpri.gt.1) then
        npr=nb1
        write (lun11,*)'bautista threshold xsection:',
     $         npr,ett,eth,rdat1(np1r),sg(npr),ans2,swrat
        endif
      rates(1,ml)=ans1
      rates(2,ml)=ans2
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      if (lpri.gt.1) write (lun11,*)'rates:',
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml)
      lpri=lprisv
      go to 9000
c
 94   continue
      go to 9000
c     op pi xsections
c     old version
      lprisv=lpri
c      if (lpri.ge.1) lpri=2
      if (nrdt.gt.3) go to 499
      idest1=idat1(np1i+nidt-2)
      idest4=idat1(np1i+nidt-3)
      idest2=nlevp+idat1(np1i+nidt-4)-1
      if (lpri.gt.1) write (lun11,*)'idest1=',idest1,idest2
      if ((idest1.ge.nlevp).or.(idest1.le.0)) go to 9000
      if (ml.le.0) go to 9000
      eth=rlev(4,idest1)-rlev(1,idest1)
      ett=eth
      nilin=npar(ml)
      if (lpri.gt.1) write (lun11,*)'nilin=',nilin,ml
      if (nilin.le.0) go to 9000
      if (lpri.gt.1) write (lun11,*)'ett=',ett,etmpp(1)
      if (ett.le.0.) go to 9000
      nb1=nbinc(ett,epi,ncn2)
      tst=abs(bremsint(nb1)/max(1.e-24,vsav(1,ml))-1.)
      xkt=ett/(0.861707*t)
      r19=rr/1.e+19
c      if ((tst.le.0.01).and.(lforce.ne.1)) then
      if (lforce.ne.1) then
        xkto=vsav(4,ml)
        tq=ee1exp(xkt)/max(1.e-24,ee1exp(xkto))
        ans1=rates(1,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans2=rates(2,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        ans3=rates(3,ml)*vsav(2,ml)*vsav(2,ml)/r19/r19
        ans4=rates(4,ml)*tq*xnx/max(1.e-24,vsav(3,ml))
        if (lpri.gt.1) write (lun11,*)'type 94 scaling:',
     $    ml,vsav(2,ml),r19,tq,xnx,vsav(3,ml),xkt,xkto,tst,
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml),ett,t
        go to 9000
        endif
      vsav(2,ml)=r19
      vsav(1,ml)=bremsint(nb1)
      vsav(3,ml)=xnx
      vsav(4,ml)=xkt
      mlm=nilin-1
      call drd(ltyp,lrtyp,lcon,
     $  nrdt,np1r2,nidt,np1i2,nkdt,np1k2,mlm,
     $  nptrs,0,lun11)
      ntmp=nrdt/2
      gglo=rlev(2,idest1)
      ggup=rlev(2,nlevp)
      idest3=idat1(np1i+nidt-1)
      idest4=idest3+1
      if (idest2.gt.nlevp) then
        jkk3=jkion+1
        if (lpri.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        ndtmp=npfi(13,jkk3)
        if (lpri.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        mllz=npar(ndtmp)
        nptmp=mllz
        do while ((ndtmp.ne.0).and.(iltmp.ne.(idest2-nlevp+1))
     $      .and.(nptmp.eq.mllz)) 
           mlm=ndtmp-1
           call drd(ltyp2,lrtyp2,lcon2,
     $       nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $       nptrs,0,lun11)
           iltmp=idat1(np1i2+nidt2-2)
           if (lpri.gt.1) write (lun11,*)nidt2,iltmp,ndtmp
           ndtmp=npnxt(ndtmp)           
           nptmp=0
           if (ndtmp.ne.0) nptmp=npar(ndtmp)
           enddo
         ggup=rdat1(np1r2+1)
         if (lpri.gt.1)
     $    write (lun11,*) ndtmp,iltmp,idest2,ggup
         endif
      if (lpri.gt.1) write (lun11,*)'before phint53'
      if (ggup.le.1.e-24) then
        write (lun11,*) 'ggup error'
        return
        endif
      swrat=gglo/ggup
      if (lpri.gt.1) then
        write (lun11,*)'type 94 data:',idat1(np1i),idat1(np1i+nidt-1),
     $           t,xnx,eth,gglo,ggup,swrat
        call dprinto(ndesc,nrdesc,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,rdat1,idat1,kdat1,lun11) 
        endif
      lprib=0
      if (lpri.gt.1) lprib=lpri
      sth=1.e-18*rdat1(np1r+1)
      alph=rdat1(np1r+2)
      e1=rdat1(np1r)
      lfastl=1
      call phint53pl(sth,e1,alph,ett,ans1,ans2,ans3,ans4,
     $  abund1,abund2,ptmp1,ptmp2,xpx,opakab,
     $  opakc,rccemis,lprib,epi,ncn2,bremsa,t,swrat,xnx,
     $  lfastl,lun11)
      if (lpri.gt.1) then
        npr=nb1
        write (lun11,*)'bautista threshold xsection:',
     $         npr,ett,eth,rdat1(np1r),sg(npr),ans2,swrat
        endif
      rates(1,ml)=ans1
      rates(2,ml)=ans2
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      if (lpri.gt.1) write (lun11,*)'rates:',
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml)
      lpri=lprisv
      go to 9000
c
 95   continue
c     bryans ci rates
      idest4=idat1(np1i+nidt-1)+1
      idest3=idat1(np1i+nidt-1)      
      if (nrdesc.eq.5) then
         idest1=idat1(np1i)
         if (nidt.ge.3) then
           idest2=nlevp-1+idat1(np1i+1)
           else
           idest2=nlevp
           endif
        else
         idest2=1
         idest1=1
        endif
      ee=rdat1(np1r)
      tmin=rdat1(np1r+1)
      nspline=(nrdt-2)/2
      ekt=0.861707*t
      tt=ekt/ee
c     constant is ln(2)
      xx=1.-(0.693147)/log(tt+2.)
      mm=1
      do while ((mm.lt.nspline).and.(xx.gt.rdat1(np1r+1+mm)))
        if (lpri.gt.1) write (lun11,*)mm,xx,rdat1(np1r+1+mm)        
        mm=mm+1
        enddo
c      this illustrates the storage scheme for the splines
c      do mm=1,nspline
c        tspline(mm)=rdat1(np1r+1+mm)
c        vspline(mm)=rdat1(np1r+1+nspline+mm)
c        enddo
c     linear interpolation
      rho=(rdat1(np1r+1+nspline+mm-1)
     $  +(xx-rdat1(np1r+1+mm-1))*
     $    (rdat1(np1r+1+nspline+mm)-rdat1(np1r+1+nspline+mm-1))
     $    /(rdat1(np1r+1+mm)-rdat1(np1r+1+mm-1)))
c      dere equation 7
      call eint(1./tt,e1,e2,e3)
      citmp1=1.e-6*e1*rho/sqrt(tt*ee**3)
      ans1=citmp1*xnx
      ans2=0.
      idest1=1
c      idest2=1
      ggup=rlev(2,nlevp)
      gglo=rlev(2,idest1)
c     note that rinf has exponential removed
      tsq=sqrt(t)
      rinf=(2.08e-22)*gglo/ggup/t/tsq
      ans2=ans1*rinf*xnx/expo(-1./tt)
c      idest1=idat1(np1i+nidt-2)
      idest4=idat1(np1i+nidt-1)+1
      idest3=idat1(np1i+nidt-1)      
      go to 9000
c
96    continue
      go to 9000
c
97    continue
c     ci rate in terms of upsilon
c     to do fexxiv --> fexxv 1s2s(3S) ci from patrick
      if (lpri.gt.1)
     $  write (lun11,*)'type 97:',ndesc,lcon,nrdt,nidt,nkdt,
     $  ml,(rdat1(np1r+mm-1),mm=1,nrdt),(idat1(np1i+mm-1),mm=1,nidt),
     $  (kdat1(np1k+mm-1),mm=1,nkdt)
      idest4=idat1(np1i+nidt-1)+1
      idest3=idat1(np1i+nidt-1)      
      if (nrdesc.eq.5) then
         idest1=idat1(np1i)
         if (nidt.ge.3) then
           idest2=nlevp-1+idat1(np1i+1)
           else
           idest2=nlevp
           endif
        else
         idest2=1
         idest1=1
        endif
      eth=rlev(4,idest1)-rlev(1,idest1)
      if (idest2.gt.nlevp) then
        jkk3=jkion+1
        if (lpri.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        ndtmp=npfi(13,jkk3)
        if (lpri.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        if (ndtmp.le.0) go to 9000
        mllz=npar(ndtmp)
        iltmp=0
        do while ((ndtmp.ne.0).and.(iltmp.ne.(idest2-nlevp+1))
     $      .and.(npar(ndtmp).eq.mllz)) 
           mlm=ndtmp-1
           call drd(ltyp2,lrtyp2,lcon2,
     $       nrdt2,np1r2,nidt2,np1i2,nkdt2,np1k2,mlm,
     $       nptrs,0,lun11)
           iltmp=idat1(np1i2+nidt2-2)
           if (lpri.gt.1) write (lun11,*)nidt2,iltmp,ndtmp
           ndtmp=npnxt(ndtmp)     
           if (ndtmp.le.0) go to 9000                 
           enddo
c        NB fix to excited level PI and rec
         ett=ett+rdat1(np1r2)
         eth=ett
         ggup=rdat1(np1r2+1)
         if (lpri.gt.1)
     $    write (lun11,*) ndtmp,iltmp,idest2,ggup,ett
         endif
      nspline=nrdt/2
      ekt=0.861707*t
      xx=ekt
      mm=1
      do while ((mm.lt.nspline).and.(ekt.gt.rdat1(np1r+mm-1)))
        if (lpri.gt.1) write (lun11,*)mm,xx,rdat1(np1r+mm-1)        
        mm=mm+1
        enddo
      do mm5=1,nspline
        rdattmp(mm5)=rdat1(np1r+mm5+nspline-1)
c        NB a fudge for Fe XXIV --> Fe XXV
c        if ((jkion.eq.349).and.(idest1.eq.1).and.(idest2.gt.53)) then
c          rdattmp(mm5)=rdat1(np1r+mm5+nspline-1)*0.5
c          endif
        enddo
      cijpp=rdattmp(mm)+(xx-rdat1(np1r+mm-1))*
     $    (rdattmp(mm+1)-rdattmp(mm))
     $    /(rdat1(np1r+mm)-rdat1(np1r+mm-1))
      gglo=rlev(2,idest1)
      cji=(8.626e-8)*cijpp/tsq
     $      /ggup
      delt=eth/ekt
      exptmp=expo(-delt)
      cij=cji*ggup*exptmp/gglo
c
c     NB a fudge which boosts this works
c      if (idest2.eq.nlevp+1) cij=cij*10.
c      
      ans1=cij*xnx
c     note that rinf has exponential removed
      rinf=(2.08e-22)*gglo/ggup/t/tsq
      ans2=ans1*rinf*xnx/exptmp
c      idest1=idat1(np1i+nidt-2)
      idest4=idat1(np1i+nidt-1)+1
      idest3=idat1(np1i+nidt-1)      
      if (lpri.gt.1)
     $ write (lun11,*)'ltype=97:',cij,cji,cijpp,mm,rdattmp(mm),
     $ rdat1(np1r+mm-1),xx,idest1,idest2,gglo,ggup,delt,exptmp,
     $  eth,ekt,ans1,ans2
c
      go to 9000
c
99    continue
      go to 9000
c
98    continue
c     line rates, col, burgess and tully for chianti
      lpril=0
c      if (lpri.ge.1) lpril=2
      idest1=idat1(np1i)
      idest2=idat1(np1i+1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         eeup=rlev(1,idest2)
         eelo=rlev(1,idest1)
         endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      c=rdat1(np1r+2)
      eijry=rdat1(np1r)
      eij=eijry*13.605692
      elin=12398.41/eij
      hij=elin*1.e-8
      ntem=(nrdt-3)/2
      if (lpril.gt.1)
     $ write (lun11,*)'type 98 data:',elin,ntem
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      if (lpril.gt.1)
     $ write (lun11,*)elin,ekt,delt
      do mm=1,ntem
        tstr(mm)=rdat1(np1r+2+mm)
        cstr(mm)=rdat1(np1r+2+ntem+mm)
        if (lpril.gt.1) write (lun11,*)mm,tstr(mm),cstr(mm)
        enddo
      tk=t*1.e+4
      tk=max(tk,2.8777e+6/elin)
      ik=idat1(np1i+nidt-2)
c     whats going on here
c      cijpp=gglo*upsiln(ik,eijry,c,ntem,cstr,tstr,tk,lpril,lun11)
      cijpp=upsiln(ik,eijry,c,ntem,cstr,tstr,tk,lpril,lun11)
      ekt=0.861707*t
      delt=12398.41/elin/ekt
      cji=(8.626e-8)*cijpp/tsq
     $      /ggup
      exptmp=expo(-delt)
      cij=cji*ggup*exptmp/gglo
      if (lpril.gt.1)
     $ write (lun11,*)'ltyp=98',c,p1,p2,p3,p4,p5,ik,
     $    eij,idest1,idest2,cij,cji,xnx,cijpp,exptmp,delt,gglo,ggup
      ans1=cij*xnx
      ans2=cji*xnx
      elin=0.
      lpril=0
      go to 9000

c      
 9000 continue
c
      call remtms(time2)
c      write (lun11,*)'ndesc=',ndesc
!jg      tucalc(ndesc)=tucalc(ndesc)+abs(time2-time1)
!jg      ncall(ndesc)=ncall(ndesc)+1
c
      if (lpri.gt.1)
     $ write (lun11,9931)krdesc(nrdesc),kdesc(ndesc),ndesc,ans1,ans2,
     $     ans3,ans4,idest1,idest2,rdat1(np1r)
 9931 format (1x,'in ucalc :',a28,a56,i4,4x,4(1pe10.2),2i4,3(1pe10.2))
c
      return
      end
