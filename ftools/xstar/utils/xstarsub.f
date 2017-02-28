      program xstarsub
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
      parameter (ncomp=101)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     line luminosities
      dimension elum(2,nnnl),elumo(2,nnnl)
c     line emissivities
      dimension rcem(2,nnnl)
c     line opacities
      dimension oplin(nnnl)
      dimension fline(ncn)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c      continuum lum
      dimension zrems(3,ncn),zremso(3,ncn),
     $          zremsz(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      dimension rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      dimension opakc(ncn)
c     level populations
      dimension xilev(nnml),bilev(nnml),rniss(nnml)
      dimension cemab(2,nnml),opakab(nnml)
      dimension elumab(2,nnml),elumabo(2,nnml)
      dimension tauc(2,nnml)
c     ion abundances
      dimension xii(nni)
c     heating and cooling
      dimension htt(nni),cll(nni)
      dimension rrrt(nni),pirt(nni)
      dimension nlevs(nni)
c     element abundances
      dimension abel(13),abcosmic(13),ababs(13)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
      dimension decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
c
c
C     Warning!!  #11 & #7 must be run before #5 since #5 changes
C                the variable zrtmp
C                Also make sure that if you change the number of 
C                entries in nlprnt, that you also update nlnprnt
C                and the dimension statement for nlprnt
      dimension nlprnt(15)
      data nlnprnt/6/,nlprnt/11,22,1,19,23,24,5,10,16,14,4,6,15,21,7/
c
      data abcosmic/1.,0.1,3.54e-4,9.33e-5,7.41e-4,1.20e-4,
     $        3.80e-5,3.55e-5,2.14e-5,3.31e-6,2.29e-6,3.16e-5,
     $        1.78e-6/
c 
      character(133) tmpst
      character(256) datafil3,datafil4,datafile
c
C     storing info for parameters
      character(20) parname(38)
      character(10) partype(38)
      real parms(38)
      character(30) parcomm(38)
      character(80) kmodelname
      character(16) knam
      character(80) spectype, specfile
      integer nparms, specunit, nloopctl, lunlog
      real poptol
      logical ex3,ex4

C     Parameter Names
C
      data parname/'cfrac','temperature',
     $   'lcpres','pressure','density','spectrum',
     $   'spectrum_file','spectun','trad',
     $   'rlrad38','column','rlogxi',
     $   'nsteps','niter','lwrite',
     $   'lprint','lstep',
     $   'habund','heabund','cabund',
     $   'nabund','oabund','neabund',
     $   'mgabund','siabund','sabund',
     $   'arabund','caabund','feabund',
     $   'niabund','emult','taumax','xeemin',
     $   'critf','vturbi','npass','modelname',
     $   'loopcontrol'/
      data partype/'real','real',
     $    'integer','real','real','string',
     $    'string','integer','real',
     $    'real','real','real',
     $    'integer','integer','integer',
     $    'integer','integer',
     $    'real','real','real',
     $    'real','real','real',
     $    'real','real','real',
     $    'real','real','real',
     $    'real','real','real',
     $    'real','real','real','integer','string',
     $    'integer'/
      data parcomm/' ','Units of 10**4 K',
     $     '1=yes, 0=no','dynes/cm**2','cm**(-3)',' ',
     $     ' ','0=energy, 1=photons','or alpha',
     $     '/10**38 erg/sec','cm**(-2)',' ',
     $     ' ',' ','1=yes, 0=no',
     $     '1=yes, 0=no',' ',
     $     ' ',' ',' ',
     $     ' ',' ',' ',
     $     ' ',' ',' ',
     $     ' ',' ',' ',
     $     ' ',' ',' ',
     $     ' ',' ',' ',
     $     ' ',' ',
     $     ' '/
      nparms=38
c
      lunu=20
c
      open (unit=5,file='xin116no.dat',status='unknown')
c
      lun11=6
      lpri=0
      call ener(epi)
      call rread1(tp,xlum,lwri,lpri,r,t,xpx,p,lcdd,numrec,npass,
     $ nnmax,nlimd,rmax,xpxcol,xi,zeta,lfix,zremsz,epi,
     $ lunlog,abel,cfrac,emult,taumax,xeemin,spectype,specfile,
     $ specunit,kmodelname,nloopctl,critf,vturbi)
      lunlog=6
      call xstarsetup(lnerrd,nlimd,lforce,lfpi,
     $       lpri,lprid,lunlog,tinf,critf,
     $       t,tp,r,delr,xee,xpx,abel,ababs,cfrac,xlum,p,lcdd,
     $       epi,bremsa,bremsint,
     $       decomp,ecomp,sxcomp,
     $       zrems,zremsz,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,nlevs,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,
     $       ntotit,
     $       xii,rrrt ,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $       xilev,bilev,rniss,nmat,elum,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
     $       nlin,elin)
c

c       step thru radius zones
        kk=1
        jkp=0
 808      jkp=jkp+1

c          
          delr=0.
          ectt=13.6
          jk=jkp
          if (jk.gt.1) 
     $         call step(ectt,emult,epi,opakc,rccemis,fline,
     $           zrems,lpris,delr,dpthc,r,
     $           xpxcol,xcol,xpx,taumax,numrec,lunlog)
          rdel=rdel+delr
          r=r+delr
          xcol=xcol+xpx*delr
c
          r19=r*(1.e-19)
          xi=xlum/r19/r19/xpx
          zeta=alog10(xi)
          call trnfrc(lpri,lunlog,ldir,
     $      r,xpxcol,xpx,
     $      epi,zremsz,dpthc,rccemis,opakc,
     $      zrems,fline,bremsa,bremsint)
          lfpi=2
          lforce=1
c
          call xstarcalc(lnerrd,nlimd,lforce,lfpi,
     $       lpri,lprid,lunlog,tinf,vturbi,critf,
     $       t,tp,r,delr,xee,xpx,ababs,cfrac,xlum,p,lcdd,
     $       epi,bremsa,bremsint,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npni,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       ntotit,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $       xilev,bilev,rniss,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab)
          call pprint(9,jk,tp,xlum,lwri,lpri,r,t,xpx,p,lcdd,
     $       numrec,npass,nnmax,nlimd,rmax,xpxcol,xi,zeta,lfix,
     $       zremsz,epi,abel,cfrac,emult,taumax,xeemin,
     $       spectype,specfile,specunit,kmodelname,nloopctl,
     $       nparms,parname,partype,parms,parcomm,
     $       lunlog,tinf,xcol,vturbi,critf,
     $       delr,rdel,enlum,xee,ababs,
     $       bremsa,tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $       ntotit,lnerrd,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
     $       elumab,elum,zrems)
          call pprint(10,jk,tp,xlum,lwri,lpri,r,t,xpx,p,lcdd,
     $       numrec,npass,nnmax,nlimd,rmax,xpxcol,xi,zeta,lfix,
     $       zremsz,epi,abel,cfrac,emult,taumax,xeemin,
     $       spectype,specfile,specunit,kmodelname,nloopctl,
     $       nparms,parname,partype,parms,parcomm,
     $       lunlog,tinf,xcol,vturbi,critf,
     $       delr,rdel,enlum,xee,ababs,
     $       bremsa,tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $       ntotit,lnerrd,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
     $       elumab,elum,zrems)
c
          call stpcut(ldir,lpri,lunlog,vturbi,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $        epi,opakc,oplin,opakab,delr,t,
     $        dpthc,tau0,tauc)
          call trnfrn(lpri,lunlog,vturbi,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $       r,delr,
     $       epi,
     $       tau0,dpthc,tauc,
     $       rcem,rccemis,brcems,opakc,opakab,cemab,
     $       zrems,zremso,elumab,elumabo,elum,elumo,zremsz,
     $       etoti,etotc,etotl)
c
C        All done looping over the radial zones
c         if ((kk.eq.1).and.((xcol.lt.xpxcol).or.(jkp.ne.2*int(jkp/2)))
         if ((kk.eq.1).and.(xcol.lt.xpxcol).and.(xee.gt.xeemin)
     $         .and.(numrec.gt.0)) go to 808
         if ((kk.gt.1).and.(jkp.lt.numrec)) go to 808
c
          call pprint(15,jk,tp,xlum,lwri,lpri,r,t,xpx,p,lcdd,
     $       numrec,npass,nnmax,nlimd,rmax,xpxcol,xi,zeta,lfix,
     $       zremsz,epi,abel,cfrac,emult,taumax,xeemin,
     $       spectype,specfile,specunit,kmodelname,nloopctl,
     $       nparms,parname,partype,parms,parcomm,
     $       lunlog,tinf,xcol,vturbi,critf,
     $       delr,rdel,enlum,xee,ababs,
     $       bremsa,tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $       ntotit,lnerrd,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
     $       elumab,elum,zrems)
          call pprint(6,jk,tp,xlum,lwri,lpri,r,t,xpx,p,lcdd,
     $       numrec,npass,nnmax,nlimd,rmax,xpxcol,xi,zeta,lfix,
     $       zremsz,epi,abel,cfrac,emult,taumax,xeemin,
     $       spectype,specfile,specunit,kmodelname,nloopctl,
     $       nparms,parname,partype,parms,parcomm,
     $       lunlog,tinf,xcol,vturbi,critf,
     $       delr,rdel,enlum,xee,ababs,
     $       bremsa,tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $       ntotit,lnerrd,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
     $       elumab,elum,zrems)

      stop
      end
      subroutine xstarsetup(lnerrd,nlimd,lforce,lfpi,
     $       lpri,lprid,lunlog,tinf,critf,
     $       t,tp,r,delr,xee,xpx,abel,ababs,cfrac,xlum,p,lcdd,
     $       epi,bremsa,bremsint,
     $       decomp,ecomp,sxcomp,
     $       zrems,zremsz,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,nlevs,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,
     $       ntotit,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $       xilev,bilev,rniss,nmat,elum,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
     $       nlin,elin)
c     
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
      parameter (ncomp=101)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     line luminosities
      dimension elum(2,nnnl),elumo(2,nnnl)
c     line emissivities
      dimension rcem(2,nnnl)
c     line opacities
      dimension oplin(nnnl)
      dimension fline(ncn)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c      continuum lum
      dimension zrems(3,ncn),zremso(3,ncn),
     $          zremsz(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      dimension rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      dimension opakc(ncn)
c     level populations
      dimension xilev(nnml),bilev(nnml),rniss(nnml)
      dimension cemab(2,nnml),opakab(nnml)
      dimension elumab(2,nnml),elumabo(2,nnml)
      dimension tauc(2,nnml)
c     ion abundances
      dimension xii(nni)
c     heating and cooling
      dimension htt(nni),cll(nni)
      dimension rrrt(nni),pirt(nni)
      dimension nlevs(nni)
c     element abundances
      dimension abel(13),abcosmic(13),ababs(13)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
      dimension decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
c
c
C     Warning!!  #11 & #7 must be run before #5 since #5 changes
C                the variable zrtmp
C                Also make sure that if you change the number of 
C                entries in nlprnt, that you also update nlnprnt
C                and the dimension statement for nlprnt
      dimension nlprnt(15)
      data nlnprnt/6/,nlprnt/11,22,1,19,23,24,5,10,16,14,4,6,15,21,7/
c
      data abcosmic/1.,0.1,3.54e-4,9.33e-5,7.41e-4,1.20e-4,
     $        3.80e-5,3.55e-5,2.14e-5,3.31e-6,2.29e-6,3.16e-5,
     $        1.78e-6/
c 
      character(133) tmpst
      character(256) datafil3,datafil4,datafile
c
C     storing info for parameters
      character(20) parname(38)
      character(10) partype(38)
      real parms(38)
      character(30) parcomm(38)
      character(80) kmodelname
      character(16) knam
      character(80) spectype, specfile
      integer nparms, specunit, nloopctl, lunlog
      real poptol
      logical ex3,ex4

C     Parameter Names
C
      data parname/'cfrac','temperature',
     $   'lcpres','pressure','density','spectrum',
     $   'spectrum_file','spectun','trad',
     $   'rlrad38','column','rlogxi',
     $   'nsteps','niter','lwrite',
     $   'lprint','lstep',
     $   'habund','heabund','cabund',
     $   'nabund','oabund','neabund',
     $   'mgabund','siabund','sabund',
     $   'arabund','caabund','feabund',
     $   'niabund','emult','taumax','xeemin',
     $   'critf','vturbi','npass','modelname',
     $   'loopcontrol'/
      data partype/'real','real',
     $    'integer','real','real','string',
     $    'string','integer','real',
     $    'real','real','real',
     $    'integer','integer','integer',
     $    'integer','integer',
     $    'real','real','real',
     $    'real','real','real',
     $    'real','real','real',
     $    'real','real','real',
     $    'real','real','real',
     $    'real','real','real','integer','string',
     $    'integer'/
      data parcomm/' ','Units of 10**4 K',
     $     '1=yes, 0=no','dynes/cm**2','cm**(-3)',' ',
     $     ' ','0=energy, 1=photons','or alpha',
     $     '/10**38 erg/sec','cm**(-2)',' ',
     $     ' ',' ','1=yes, 0=no',
     $     '1=yes, 0=no',' ',
     $     ' ',' ',' ',
     $     ' ',' ',' ',
     $     ' ',' ',' ',
     $     ' ',' ',' ',
     $     ' ',' ',' ',
     $     ' ',' ',' ',
     $     ' ',' ',
     $     ' '/
      nparms=38

c
c
      call remtms(t1s)
c
c     opening message
      if (lpri.ne.0) 
     $ write (lunlog,*)'xstar version 2.1kn7'

c
c
c     Test if atomic database files are available.  Abort if not.
      call getenv('LHEA_DATA', datafile)
      datafil4 = datafile(1:lenact(datafile))//'/atdb.fits'
      datafil3 = datafile(1:lenact(datafile))//'/coheat.dat'
      inquire(file=datafil3,exist=ex3)
      inquire(file=datafil4,exist=ex4)
      if (.not.(ex3 .and. ex4 )) then
         write(tmpst,*)'xstar: One or more of the Atomic Database files'
         write(lunlog,*)tmpst
         call xwrite(tmpst,10)
         write(tmpst,*)'xstar: are missing.'
         write(lunlog,*)tmpst
         call xwrite(tmpst,10)
         write(tmpst,*)'xstar: ',datafil4(1:lenact(datafil4))
         write(lunlog,*)tmpst
         call xwrite(tmpst,10)
         write(tmpst,*)'xstar: ',datafil3(1:lenact(datafil3))
         write(lunlog,*)tmpst
         call xwrite(tmpst,10)
         write(tmpst,*)'Program aborting...'
         write(lunlog,*)tmpst
         call xwrite(tmpst,10)
         close(lunlog)
         stop 'xstar: Missing Atomic Data files.  Aborting...'
      endif
c
c
      tread=0.
      trates1=0.
      thcor=0.
      trates2=0.
      theat=0.
      do kl=1,ntyp
         tucalc(kl)=0.
         ncall(kl)=0
         enddo
c
c     default parameter values
      emult=0.75
      taumax=20.
      xeemin=0.01
c
      lprisv=lpri
      lpri=0
      write(lunlog,*)'Atomic Abundances'
      write(lunlog,*)'Element      Solar    Hydrogen'
      do ll=1,13
        ababs(ll)=abel(ll)*abcosmic(ll)
c        write(lunlog,9990)parname(17+ll),abel(ll),ababs(ll)
        enddo
      write(lunlog,*)' '
 9990 format(A10,2(E12.4))
      call ispcg2(zremsz,epi,enlum,lpri,lunlog)
      xee=1.21
      xi=10.**zeta
c
c
c     read in
      write (lunlog,*)'Loading Atomic Database...'
      write (tmpst,*)'Loading Atomic Database...'
      call xwrite(tmpst,10)

      call readtbl(nptrs,np1r,np1i,np1k,np2,ndat2,
     &                   rdat1,idat1,kdat1,nidat1,datafil4,lunlog)
c
c
c
      lun25=25
      open(unit=lun25,file=datafil3,status='unknown')
      rewind(lun25)
      read (lun25,*)idum1,idum2
      do mm=1,ncomp
        do ll=1,ncomp
          read (lun25,*)lld,mmd,sxcomp(mm),edum,
     $            ecomp(ll),decomp(mm,ll)
c          read (lun25,*)lld,mmd,edum1,edum,
c     $            edum2,edum3
c          write (6,*)lld,mmd,ll,mm,edum1,edum,
c     $            edum2,edum3
c          sxcomp(mm)=edum1
c          ecomp(ll)=edum2
c          decomp(mm,ll)=edum3
          enddo
        enddo
c
c
c     Initialize the database
      write (lunlog,*)'initializng database...'
      write (tmpst,*)'initializng database...'
      call xwrite(tmpst,10)      
      call setptrs(lunlog,lpri,
     $ idat1,rdat1,kdat1,nptrs,np2,
     $ npnxt,npfi,npar,npfirst,nplin,
     $ nplini,npcon,npconi,npilev,npilevi,
     $ npconi2,nlevs,nlsvn,ncsvn,ababs)
c
c
c     set up and initialize
      rdel = 0.
      tinf=0.099
      call init(
     $       lpri,lunlog,tinf,
     $       t,r,delr,xlum,enlum,xee,xpx,abel,cfrac,
     $       epi,bremsa,bremsint,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $       ntotit,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
     $       elumab,elumabo,elum,elumo,zrems,zremso,zremsz,
     $       rates,vsav,idrates,fline)
c      
c
      nmat=nd
c
      return
      end
      subroutine xstarcalc(lnerrd,nlimd,lforce,lfpi,
     $       lpri,lprid,lunlog,tinf,vturbi,critf,
     $       t,tp,r,delr,xee,xpx,ababs,cfrac,xlum,p,lcdd,
     $       epi,bremsa,bremsint,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npni,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       ntotit,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $       xilev,bilev,rniss,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab)
c     
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
      parameter (ncomp=101)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npni(ndat2,nni),npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nni,nd),npilevi(nnml)
c     line luminosities
      dimension elum(2,nnnl),elumo(2,nnnl)
      dimension elumab(2,nnml),elumabo(2,nnml)
c     line emissivities
      dimension rcem(2,nnnl)
c     line opacities
      dimension oplin(nnnl)
      dimension fline(ncn)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c     continuum lum
      dimension zrems(3,ncn),zremso(3,ncn),
     $          zremsz(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      dimension rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      dimension opakc(ncn)
c     level populations
      dimension xilev(nnml),bilev(nnml),rniss(nnml)
      dimension cemab(2,nnml),opakab(nnml)
      dimension tauc(2,nnml)
c     ion abundances
      dimension xii(nni)
c     heating and cooling
      dimension htt(nni),cll(nni)
      dimension rrrt(nni),pirt(nni)
      dimension nlevs(nni)
c     element abundances
      dimension abel(13),abcosmic(13),ababs(13)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
      dimension decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
c
      character(133) tmpst
c
      character(20) parname(33)
      character(10) partype(33)
      real parms(33)
      character(30) parcomm(33),kmodelname
      character(16) knam
      character(8) spectype, specfile
      integer nparms, specunit, nloopctl, lunlog
c
      call dsec(lnerrd,nlimd,lfpi,
     $       lpri,lprid,lunlog,tinf,vturbi,critf,
     $       t,tp,r,delr,xee,xpx,ababs,cfrac,xlum,p,lcdd,
     $       epi,bremsa,bremsint,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       ntotit,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,fline)
c
c
      return
      end
      subroutine uclgsi(kdum,iresult,ierr)
      character*(*) kdum
      read (5,*)iresult
c      write (6,*)'in uclgsi, iresult=',iresult
      return
      end
      subroutine uclgsr(kdum,result,ierr)
      character*(*) kdum
      read (5,*)result
c      write (6,*)'in uclgsr, result=',result
      return
      end
      subroutine uclgst(kdum,kresult,ierr)
      character*(*) kdum,kresult
      character(80) kres2
      character(1) ktmp
      read (5,'(a80)')kres2
      do ll=1,80
        read (kres2(ll:ll),'(a1)')ktmp
        write(kresult(ll:ll),'(a1)')ktmp
        enddo
      return
      end
      subroutine xwrite(string,ll)
      character(120) string
      write (6,*)string
      return
      end
      subroutine getlun(lun)
c
      common /lpass/lunu
c
      lunu=lunu+1
      lun=lunu
c
      return
      end
      INTEGER FUNCTION LENACT(CBUF)
      CHARACTER CBUF*(*)
C---
C Function to return the active length of a character string, not
C counting any trailing blanks.  N.B. an all blank string will
C return ZERO as the length.
C---
C CBUF    I    String whose length is to be measured.
C---
C 1988-Jun-13 - Standard Fortran version [AFT]
C---
      INTEGER   I
C---
      DO 190 I=LEN(CBUF),1,-1
         IF(CBUF(I:I).NE.' ') THEN
            LENACT=I
            RETURN
         END IF
  190 CONTINUE
      LENACT=0
      RETURN
      END

c *********************************************************************
      SUBROUTINE ALP(N,TEMP,IC,AL)
C
C SUBROUTINE ALP GENERATES THE N RECOMBINATION COEFFTS FOR EACH TERM
C OF HYDROGENIC ATOMS IN LEVEL N.
C A FIVE POINT GAUSSIAN INTEGRATION FORMULA IS USED.
C
C          IC=IONIC CHARGE
C        TEMP=TEMPERATURE OF ELECTRON GAS
C       AL(J)=RECOMBINATION COEFFT TO ORBITAL L=J-1 OF LEVEL N
C
C 
      DIMENSION C(5),T(5),AL(200)
      real GU(200),GL(200),AN(200),F1,F2,AS,AT,P,Q
      C(1)=0.23692689
      C(2)=0.47862867
      C(3)=0.56888889
      C(4)=C(2)
      C(5)=C(1)
      T(1)=-0.90617985
      T(2)=-0.53846931
      T(3)=0.0
      T(4)=-T(2)
      T(5)=-T(1)
      NMLEV=50
c      write (lun11,*)'in erc:'
C
      H=0.0001/FLOAT(N)
      Y=157803.*IC*IC/TEMP
      A2=0.
C
      DO 5 L=1,2*NMLEV
      AN(L)=0.
 5     CONTINUE
C
      H=H*2.
      A1=A2
      A2=A2+H
c     P=(A2-A1)/2.
      p=h/2.
      Q=(A2+A1)/2.
C
      R=P*T(1)+Q
c      write (lun11,*)'before call gull1:',n,r
      CALL GULL1(N,R,GU,GL,lpri,lun11)
c      write (lun11,*)'after call gull1:',n,r,gu,gl
C
      DUM=(1.+N*N*R)
      DUM3=DUM**3.
      F1=DUM3*EXP(GU(1))*EXP(-R*Y)*Y
      AT=P*C(1)*F1*SQRT(Y)*IC
      A2=0.
      H=H/2.
C
 100   H=H*2.
      DO 50 J=1,4
      A1=A2
      A2=A2+H
      P=(A2-A1)/2.
      Q=(A2+A1)/2.
C 
      DO 10 I=1,5
      R=P*T(I)+Q
c      write (lun11,*)'before call gull1:',n,r
      CALL GULL1(N,R,GU,GL,lpri,lun11)
c      write (lun11,*)'after call gull1:',n,r,gu,gl
C
      as=0.
      DO 20 L=N,1,-1
      DUM=(1.+N*N*R)
      DUM3=DUM**3.
      F1=DUM3*EXP(GU(L))*EXP(-R*Y)*Y*L
      F2=DUM3*EXP(GL(L))*EXP(-R*Y)*Y*(L-1)
      AS=P*C(I)*(F1+F2)*SQRT(Y)*IC
      AN(L)=AN(L)+AS
 20    CONTINUE
      RAT=AS/AT
      IF(RAT.LT.1.E-6) GO TO 80
 10    CONTINUE
 50    CONTINUE
      GO TO 100
C
 80    DO 45 L=1,N
      AN(L)=5.6260E-15*AN(L)/FLOAT(N*N)
      AL(L)=(AN(L))
 45    CONTINUE
C      
      RETURN
      END

c-----------------------------------------------------------------
      subroutine amcol(n,l,temp,ic,z1,rm,ne,sum,ecm,cn)
c subroutine amcol determines the rate of angular momentum changing
c collisions in hydrogenic atoms due to collisions with ions.
c the codes are based on the method of hummer & storey (1987)
c
c        z1=charge of colliding ion
c        ic=ionic charge of hydrogenic atom
c        rm=reduced mass of colliding system in units of electron mass
c        ne=electron number density
c        sum = sum of spontaneous transitions out of level n,l
c        cn = transition rate for nl -> nl-1
c        cn(nl -> nl-1) = cn
c        cn(nl -> nl+1) = cn*(2.*l+1)/(2.*l-1)
c
c
      real ne
c
      pc1=1.181+alog10(temp/ne)
      pc2=pc1
      pc3=pc1
      dnl=6.*z1/ic*z1/ic*n*n*(n*n-l*l-l-1)
      if(sum.ne.0.) pc2=10.95+alog10(temp/rm/sum/sum)
      if(ecm.ne.0.) pc3=alog10(temp/rm/ecm/ecm)-11.22
      pc=min(pc1,pc2,pc3)
      qnl=9.933e-6*sqrt(rm/temp)*dnl*(11.538+alog10(temp/dnl/rm)+pc)
c
      den=l*(n*n-l*l)+(l+1)*(n*n-(l+1)*(l+1))
      cn=qnl*l*(n*n-l*l)/den
c
      return
      end
      subroutine amcrs(n,l,temp,ic,z1,rm,ne,sum,ecm,psi,il,cn,lun11)
c the angular momentum changing collision rates are calculated using
c either the pengelly & seaton (1964) formula (amcol) or the impact
c parameter method of seaton (1962) (impact) if the energy levels are
c non-degenerate.  the ps routine is used if the ratio of amcol/impact
c is greater than 0.94 since this code is faster.  ** beware - there
c may be problems if ne is too large ( > 1.e+7).  pc1 will be used in
c amcol rather than pc3 and the change will not occur.
c
c     n = principal quantum number of initial state
c     l = orbital quantum number of initial state
c     temp = temperature in kelvin
c     ic = ionic charge of target particle
c     z1 = charge of incident particle
c     rm = mass of incident particle in units of electron mass me
c     ne = electron number density
c     sum = total spontaneous transition rate out of n,l
c     cn = transition rate for nl -> nl-1
c     ecm = energy difference between nl and nl-1
c     psi = see notes for defn
c     il = flag to decide whether to use impact or amcol
c     cn = transition rate for nl -> nl-1
c
c
      real ne
c
      en=real(n)
      dnl=6.*z1/ic*z1/ic*n*n*(n*n-l*l-l-1)
      rho1=0.72/sum
      if(ecm.ne.0.) rho1=min(rho1,5.946e-12/ecm)
      rhom=3.929e11*rho1*temp/sqrt(dnl)/rm
      if(rhom.lt.10.) go to 30
      call amcol(n,l,temp,ic,z1,rm,ne,sum,ecm,cn)
      cn=0
c mab il=0
        il=0
        if(ecm.ne.0.) then
          if(il.eq.0) then
c     write (lun11,*)'call impact 1',en,l,temp,ic,z1,rm,ecm,psi
          call impact(en,l,temp,ic,z1,rm,ecm,psi,cr)
          rat=cn/cr
          cn=cr
          if(rat.gt. 0.94) il=1
          endif
        endif
c     go to 40
c
 30    if(ecm.eq.0.) then
      call velimp(n,l,temp,ic,z1,rm,ne,sum,cn)
      else
c     write (lun11,*)'call impact 2',en,l,temp,ic,z1,rm,ecm,psi
      call impact(en,l,temp,ic,z1,rm,ecm,psi,cn)
      endif

c      if(ne.gt.1.e14) then
c     call impact(en,l,temp,ic,z1,rm,ecm,psi,cn)
c      endif
c
 40    continue
c
      return
      end
      subroutine anl1(ni,nf,lf,iq,alm,alp,lpri,lun11)
c this subroutine is used to calculate the values of the
c spontaneous transition rates for electric dipole transitions from
c level ni,lf+1 and ni,lf-1 to level nf,lf.
c the transition probabilities (a values) are calculated
c using the gordon (1929) formula.
c        iq=ionic charge
c
      real y1,y2,x1,x2,x3,x4,x5,t
c
      alm=0.
c
c **** for case a set lower limit of nf=1, for case b nf starts at 2
c
      do 40 li=lf-1,lf+1,2
      if(li.lt.0) go to 40
      if(lf.gt.li) go to 100
      n=ni
      np=nf
      l=li
      go to 101
 100   n=nf
      np=ni
      l=lf
 101   continue
c
       call dfact(n+l,x1)
       call dfact(np+l-1,x2)
       call dfact(2*l-1,x3)
       call dfact(n-l-1,x4)
       call dfact(np-l,x5)
      ia1=-n+l+1
      ia2=ia1-2
      ib=-np+l
      ic=2*l
      x=-4.*n*np/((n-np)*(n-np))
       call hgf(ia1,ib,ic,x,y1)
       call hgf(ia2,ib,ic,x,y2)
      rev=abs(n-np)
      rn=float(n+np)
      t=(l+1)*log((4.*n*np))+(rn-2*l-2)*log((rev))
      t=t-log(4.e0)-rn*log((rn))
      y1=abs((y1-y2*(rev/rn)**2))
      y1=log(y1)+t
      t=2.*y1+x1+x2-2.*x3-x4-x5
      t=expo(t)
      an=2.6761e09*iq**4*max(li,lf)*(t)/(2.*li+1)
         dum=(1./nf/nf-1./ni/ni)**3.
      an=dum*an
      if(li.lt.lf) alm=an
      if(li.gt.lf) alp=an
c
 40    continue
c
      if (lpri.gt.1) then
        write (lun11,*)'in anl1:',li,lf,t,ni,nf,iq
        write (lun11,*) rn,n,np,rev,y1,y2,x
        write (lun11,*) ia1,ia2,ib,ic,x1,x2,x3,x4,x5,l,an
        endif
c
      return
      end
      subroutine augcmp(amat,m,nnzp,ndim,pirt,rrrt,lun11,lpri)
c
      dimension amat(ndim,ndim),pirt(ndim),rrrt(ndim)
c
      nnz=nnzp-1
      pirt(1)=amat(1,2)
      do ik = 2,nnz
            prod = 1.
            pirt(ik) = amat(ik,ik+1)
            ikm1 = ik - 1
            do  jk = 1,ikm1
               kk = ik - jk
               temp = -amat(kk,kk+1)/pirt(kk)
               if ( lpri.ge.1 ) write (lun11,*) ik,jk,kk,
     $             amat(kk,kk+1),pirt(kk),prod,temp
               if ( prod.lt.1.e+17 ) prod = prod*temp
               pirt(ik) = pirt(ik) + amat(kk,ik+1)*prod
               enddo
            enddo
c
c
      return
      end

c---------------------------------------------------------------------
      subroutine bkhsgo(sg,et,d,b,na,a,epi,t,lpri,lfast,lun11)
c
c
c
c
c     this routine does the work in computing cross sections by the
c     method of barfield, et. al.
c
c
      parameter (ncn=9999)
c
c
c
      dimension sg(ncn),b(na),a(11,na),epi(ncn)
c
c
      if (lpri.gt.1) write (lun11,*)'in bkhsgo:'
     $      ,na,b,t
      lprisv=lpri
c
      jj = 1
      yy=0.
      tmp=0.
      nb1=max(1,nbinc(et,epi))
      do j=1,nb1
         sg(j)=0.
         enddo
      i=nb1
 100     continue
         epii = epi(i)
         if (lpri.gt.1) write (lun11,*)i,epii,et 
         xx = epii*(1.e-3) - d
         if ( xx.le.0. ) goto 100
         if (lpri.gt.1) write (lun11,*)d,xx,jj
         if ( xx.ge.b(jj) ) jj = jj + 1
         if ( jj.gt.na ) go to 20
         xx = amax1(xx,0.)
         yy = alog10(xx)
         tmp = 0.
         do  lk = 1,11
             kk = 12 - lk
             tmp = a(kk,jj) + yy*tmp
             if (lpri.gt.1)
     $                 write (lun11,*)lk,kk,yy,tmp,a(kk,jj)
             enddo
         tmp = min(amax1(-50.,tmp),24.)
         sgtmp = exp10(tmp-24.)
         sg(i)=sgtmp
         if (lpri.gt.1)
     $             write (lun11,*)i,epii,xx,
     $               tmp,sgtmp
        call enxt(et,nb1,lpri,epi,t,lfast,lun11,
     $                  i,nskp,nphint,lrcalc)
        i=i+nskp
        if (i.le.nphint) go to 100
 20      continue
      if (i.lt.nphint) then
        do j=i,nphint
          sg(j)=0.
          enddo
        endif
c
      if (lpri.gt.1) write (lun11,*)'leaving bkhsgo'
      lpri=lprisv
c
      return
      end
      subroutine bnchmrk2(jj,jk,numrec,
     $       lpri,lun11,tinf,
     $       t,r,delr,rdel,xlum,enlum,xee,xpx,p,abel,cfrac,
     $       epi,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $       ntotit,lnerrd,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,
     $       xilev,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
     $       elum,zrems,zremsz)
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     line luminosities
      dimension elum(2,nnnl)
c     line emissivities
      dimension rcem(2,nnnl)
c     line opacities
      dimension oplin(nnnl)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c     continuum lum
      dimension zrems(3,ncn),
     $          zremsz(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum emissivities
      dimension rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      dimension opakc(ncn)
c     level populations
      dimension xilev(nnml)
      dimension cemab(2,nnml),opakab(nnml)
      dimension tauc(2,nnml)
c     ion abundances
      dimension xii(nni)
c     heating/cooling
      dimension htt(nni),cll(nni)
      dimension rrrt(nni),pirt(nni)
      dimension abel(13)
c     temporaries for dread
      dimension rdat(20000),idat(20000)
c
      character(100) kinam1
      character(100) kinamsv(500)
      character(1) kdat(20000),kblnk
c
      dimension elinsv(500)
c
c
      dimension elnprnt(10000)
      dimension elnsv(2,75,10),
     $   nlnsv(10),nbmpt(75,10),nlnsv2(10),fluxrl(75),lllsv(75)
c
      data kblnk/' '/
c
      data nlnsv(2)/30/
      data (elnsv(1,ml,2),elnsv(2,ml,2),nbmpt(ml,2),ml=1,13)/
     $    4858., 4868.,24,
     $    5874., 5878.,1,
     $    2325., 2330.,2,
     $    1334., 1336.,0,
     $    1906., 1910.,3,
     $    6545., 6550.,5,
     $    6580., 6590.,5,
     $    6295., 6305.,0,
     $    6360., 6370.,0,
     $    7320., 7325.,0,
     $    7330., 7335.,0,
     $    3725., 3731.,7,
     $    516800., 518500.,8/
      data (elnsv(1,ml,2),elnsv(2,ml,2),nbmpt(ml,2),ml=14,30)/
     $    882000., 883400.,9,
     $    4950., 4965.,10,
     $    5000., 5010.,10,
     $    4360., 4365.,10,
     $    258500., 259500.,0,
     $    127500., 128500.,11,
     $    154500., 157500.,12,
     $    3865., 3871.,13,
     $    3965., 3970.,13,
     $    6713., 6720.,14,
     $    6728., 6733.,14,
     $    4065., 4078.,0,
     $    180000., 197500.,15,
     $    334500., 345000.,16,
     $    9530., 9535.,17,
     $    9065., 9072.,17,
     $    104500., 105500.,18/
      data nlnsv(1)/32/
      data (elnsv(1,ml,1),elnsv(2,ml,1),nbmpt(ml,1),ml=1,16)/
     $    4858., 4868.,14,
     $    1214., 1217.,0,
     $    5874., 5878.,0,
     $    2325., 2330.,0,
     $    1334., 1336.,0,
     $    1906., 1910.,0,
     $    6545., 6550.,2,
     $    6580., 6590.,2,
     $    127500., 128500.,4,
     $    2053380., 2053390.,0,
     $    6295., 6305.,0,
     $    6360., 6370.,0,
     $    7320., 7325.,0,
     $    7330., 7335.,0,
     $    3725., 3731.,3,
     $    517500., 518500.,0/
      data (elnsv(1,ml,1),elnsv(2,ml,1),nbmpt(ml,1),ml=17,32)/
     $    882000., 883400.,0,
     $    4950., 4965.,0,
     $    5000., 5010.,0,
     $    4360., 4365.,0,
     $    258500., 259500.,0,
     $    127500., 128500.,0,
     $    154500., 155500.,0,
     $    3865., 3871.,0,
     $    3965., 3970.,0,
     $    6713., 6720.,5,
     $    6728., 6733.,5,
     $    3.3e+5, 3.4e+5,7,
     $    180000., 197500.,6,
     $    9530., 9535.,8,
     $    9065., 9072.,8,
     $    104500., 105500.,0/
      data nlnsv(3)/32/
      data (elnsv(1,ml,3),elnsv(2,ml,3),nbmpt(ml,3),ml=1,16)/
     $    4858., 4868.,1,
     $    1214., 1217.,0,
     $    5874., 5878.,2,
     $    2325., 2330.,3,
     $    1334., 1336.,4,
     $    1906., 1910.,5,
     $    6545., 6550.,6,
     $    6580., 6590.,6,
     $    1218025., 1218028.,12,
     $    2053380., 2053390.,0,
     $    6295., 6305.,0,
     $    6360., 6370.,0,
     $    7320., 7325.,8,
     $    7330., 7335.,8,
     $    3725., 3731.,9,
     $    517500., 518500.,10/
      data (elnsv(1,ml,3),elnsv(2,ml,3),nbmpt(ml,3),ml=17,32)/
     $    882000., 883400.,0,
     $    4950., 4965.,11,
     $    5000., 5010.,11,
     $    4360., 4365.,11,
     $    258500., 259500.,0,
     $    127500., 128500.,12,
     $    154500., 155500.,13,
     $    3865., 3871.,14,
     $    3965., 3970.,14,
     $    6713., 6720.,0,
     $    6728., 6733.,0,
     $    4065., 4078.,0,
     $    180000., 197500.,15,
     $    9530., 9535.,16,
     $    9065., 9072.,16,
     $    104500., 105500.,17/
      data nlnsv(4)/72/
      data (elnsv(1,ml,4),elnsv(2,ml,4),nbmpt(ml,4),ml=1,15)/
     $   4858., 4868.,1,
     $   6560., 6564., 0,
     $   1214., 1217., 0,
     $   5874., 5878., 2,
     $   4680., 4690., 3,
     $   2325., 2330., 4,
     $   1906., 1910., 5,
     $   1545., 1555., 6,
     $   5195., 5205., 0,
     $   6545., 6550., 7,
     $   6580., 6590., 7,
     $   1745., 1755., 8,
     $   5.7e+05, 5.71E+05, 9,
     $   1485., 1490., 10,
     $   1235., 1245.,11/
      data (elnsv(1,ml,4),elnsv(2,ml,4),nbmpt(ml,4),ml=16,32)/
     $   6295., 6305., 12,
     $   3725., 3731., 13,
     $   4950., 4965., 14,
     $   5000., 5010., 14,
     $   5.165E+05, 5.175E+05,16, 
     $   4360., 4365., 15,
     $   258500., 259500., 17,
     $   1400., 1408., 18,
     $   1217., 1219., 19,
     $   1213., 1214.5, 19,
     $   127500., 128500.,0, 
     $   154500., 156500., 20,
     $   3865., 3871., 21,
     $   3965., 3970., 21,
     $   2420., 2428., 22,
     $   241500., 242500.,0, 
     $   3420., 3430., 23/
      data (elnsv(1,ml,4),elnsv(2,ml,4),nbmpt(ml,4),ml=33,49)/
     $   3340., 3350., 0,
     $   2795., 2800., 25,
     $   44950., 45500., 26,
     $   55950., 56050., 0,
     $   347950., 348150., 27,
     $   2328., 2352., 28,
     $   1880., 1885., 29,
     $   1890., 1896., 29,
     $   1392., 1395., 30,
     $   1402., 1404., 30,
     $   6713., 6720., 31,
     $   6728., 6733., 31,
     $   4065., 4078., 0,
     $   186950., 187050.,32, 
     $   9530., 9535., 33,
     $   9065., 9072., 33,
     $   104500., 105500.,34/
      data (elnsv(1,ml,4),elnsv(2,ml,4),nbmpt(ml,4),ml=50,72)/
     $   6.984e+4,6.986e+4,35,
     $   8.98e+4, 9.0e+4,36,
     $   7135.,7137.,37,
     $   7750.,7752.,37,
     $   4710.,4712.,38,
     $   4739.,4741.,38,
     $   1.306e+5,1.308e+5,39,
     $   7005.,7007.,40,
     $   6434.,6436.,40,
     $   4.52e+04,4.53e+04,41,
     $   2.599e+05,2.601e+05,42,
     $   1.257e+04,1.259e+04,43,
     $   8623.,8625.,44,
     $   5157.,5166.,45,
     $   2.293e+05,2.295e+05,46,
     $   5274.,5276.,47,
     $   4661.,4663.,48,
     $   2829.,2837.,49,
     $   1.955e+05,1.957e+05,50,
     $   5146.,5148.,51,
     $   9.50e+04,9.52e+04,52,
     $   6088.,6090.,53,
     $   5159.,5161.,54/
      data nlnsv(5)/49/
      data (elnsv(1,ml,5),elnsv(2,ml,5),nbmpt(ml,5),ml=1,15)/
     $   4858., 4868.,1,
     $   6560., 6564., 0,
     $   1214., 1217., 0,
     $   5874., 5878., 2,
     $   4680., 4690., 3,
     $   2325., 2330., 0,
     $   1906., 1910., 4,
     $   1545., 1555., 5,
     $   5195., 5205., 0,
     $   6545., 6550., 6,
     $   6580., 6590., 6,
     $   1745., 1755., 0,
     $   5.7e+05, 5.71E+05, 7,
     $   1485., 1490., 0,
     $   1235., 1245.,0/
      data (elnsv(1,ml,5),elnsv(2,ml,5),nbmpt(ml,5),ml=16,32)/
     $   6295., 6305., 0,
     $   3725., 3731., 8,
     $   4950., 4965., 9,
     $   5000., 5010., 9,
     $   5.165E+05, 5.175E+05,10, 
     $   4360., 4365., 0,
     $   258500., 259500., 11,
     $   1400., 1408., 0,
     $   1217., 1219., 0,
     $   1213., 1214.5, 0,
     $   127500., 128500.,0, 
     $   154500., 156500., 12,
     $   3865., 3871., 13,
     $   3965., 3970., 13,
     $   2420., 2428., 0,
     $   241500., 242500.,0, 
     $   3420., 3430., 0/
      data (elnsv(1,ml,5),elnsv(2,ml,5),nbmpt(ml,5),ml=33,49)/
     $   3340., 3350., 0,
     $   2795., 2800., 14,
     $   44950., 45500., 0,
     $   55950., 56050., 0,
     $   347950., 348150., 0,
     $   2328., 2352., 0,
     $   1880., 1885., 15,
     $   1890., 1896., 15,
     $   1392., 1395., 0,
     $   1402., 1404., 0,
     $   6713., 6720., 0,
     $   6728., 6733., 0,
     $   4065., 4078., 0,
     $   186950., 187050.,0, 
     $   9530., 9535., 16,
     $   9065., 9072., 16,
     $   104500., 105500.,17/
      data nlnsv(6)/49/
      data (elnsv(1,ml,6),elnsv(2,ml,6),nbmpt(ml,6),ml=1,15)/
     $   4858., 4868.,1,
     $   6560., 6564., 0,
     $   1214., 1217., 0,
     $   5874., 5878., 2,
     $   4680., 4690., 3,
     $   2325., 2330., 0,
     $   1906., 1910., 4,
     $   1545., 1555., 5,
     $   5195., 5205., 0,
     $   6545., 6550., 0,
     $   6580., 6590., 0,
     $   1745., 1755., 0,
     $   5.7e+05, 5.71E+05, 0,
     $   1485., 1490., 0,
     $   1235., 1245.,0/
      data (elnsv(1,ml,6),elnsv(2,ml,6),nbmpt(ml,6),ml=16,32)/
     $   6295., 6305., 0,
     $   3725., 3731., 0,
     $   4950., 4965., 6,
     $   5000., 5010., 6,
     $   5.165E+05, 5.175E+05,7, 
     $   4360., 4365., 0,
     $   258500., 259500., 8,
     $   1400., 1408., 0,
     $   1217., 1219., 0,
     $   1213., 1214.5, 0,
     $   127500., 128500.,0, 
     $   154500., 156500., 9,
     $   3865., 3871., 10,
     $   3965., 3970., 10,
     $   2420., 2428., 11,
     $   241500., 242500.,0, 
     $   3420., 3430., 0/
      data (elnsv(1,ml,6),elnsv(2,ml,6),nbmpt(ml,6),ml=33,49)/
     $   3340., 3350., 0,
     $   2795., 2800., 0,
     $   44950., 45500., 0,
     $   55950., 56050., 0,
     $   347950., 348150., 0,
     $   2328., 2352., 0,
     $   1880., 1885., 0,
     $   1890., 1896., 0,
     $   1392., 1395., 0,
     $   1402., 1404., 0,
     $   6713., 6720., 0,
     $   6728., 6733., 0,
     $   4065., 4078., 0,
     $   186950., 187050.,0, 
     $   9530., 9535., 12,
     $   9065., 9072., 12,
     $   104500., 105500.,13/
      data nlnsv(7)/29/
      data (elnsv(1,ml,7),elnsv(2,ml,7),nbmpt(ml,7),ml=1,14)/
     $    4858., 4868., 1,
     $    1214., 1217., 2,
     $    5874., 5878., 3,
     $    4680., 4690., 4,
     $    1635., 1645., 5,
     $    1906., 1910., 6,
     $    1545., 1555., 7,
     $    5195., 5205., 0,
     $    6545., 6550., 8,
     $    6580., 6590., 8,
     $    1745., 1755., 9,
     $    1485., 1490., 10,
     $    6295., 6305., 11,
     $    630000., 635000., 12/
      data (elnsv(1,ml,7),elnsv(2,ml,7),nbmpt(ml,7),ml=15,29)/
     $    3725., 3731., 13,
     $    4950., 4965., 14,
     $    5000., 5010., 14,
     $    4360., 4365., 15,
     $   1400., 1408., 16,
     $   154500., 155500., 17,
     $    3865., 3871., 18,
     $   2420., 2428., 19,
     $   3420., 3430., 20,
     $    2795., 2800., 21,
     $    6713., 6720., 22,
     $    6728., 6733., 22,
     $    9530., 9535., 23,
     $   186950., 187050.,24, 
     $    104500., 105500., 25/
c
      scfac=1.
      crtt=0.004
      ergsev=1.602197e-12
c
      nbmk=jj
      nbb=nbmk
      lprii=0
      write (lun11,*)'benchmark number ',nbb
      nlnprnt=nlnsv2(nbb)
      nlnprs=1
      if (nbb.le.2)  nlnprnt=nlnsv2(nbb)+1
      if (nbb.le.2) nlnprs=nlnprnt
      nlnprnt2=nlnsv(nbb)
c
      lprii=1
c
c     first get the current version
      do 820 ll1=1,nlnprnt2
         ll2=ll1
         elnprnt(ll1)=elnsv(1,ll1,nbb)
         elnprnt(ll1+1)=elnsv(2,ll1,nbb)
         if ((elnprnt(ll1).le.1.e-24).or.(elnprnt(ll1+1).le.1.e-24))
     $           go to 820
         ecen=(elnprnt(ll1)+elnprnt(ll1+1))/2.
         ediff=elnprnt(ll1+1)-elnprnt(ll1)
         eww=0.
         ebar=0.
         asym=0.                            
         fluxbs=0.
         fluxbf=0.
         flxmx=0.
         lllsv(ll2)=1
c         if (lprii.ne.0) write (lun11,*)'ll1=',ll1,
c     $     elnprnt(ll1),elnprnt(ll1+1)
         do 821 lll=1,nlsvn
            j=lll
            ml=nplin(j)
            call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
            elin=rdat(1)
            nilin=npar(ml)
            ergsev=1.602197e-12
            ener=ergsev*(12398.54)/amax1(elin,1.e-24)
            etst=ener/ergsev
c           if ((etst.lt.elimdb(1)).or.(etst.gt.elimdb(2))) go to 147
            ml=nilin
            call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
            do ktt=1,nkdt
              write (kinam1(ktt:ktt),'(a1)')kdat(ktt)
              enddo
            do ktt=nkdt+1,100
              write (kinam1(ktt:ktt),'(a1)')kblnk
              enddo
            elmtp=elum(1,j)
            elmtpb=elum(2,j)
            if (elin.le.1.e-8) go to 821 
            if ((elin.gt.elnprnt(ll1+1)).or.
     $          (elin.lt.elnprnt(ll1))) go to 821
            ener=12398.54/elin
            tmpflxf=elmtp
     $          *(1.e+19/r)*(1.e+19/r)/12.56       
            fluxbf=fluxbf+tmpflxf
            tmpflux=(elmtp+elmtpb)
     $          *(1.e+19/r)*(1.e+19/r)/12.56       
            if (tmpflux.lt.flxmx) go to 3083
              flxmx=tmpflux
              lllsv(ll2)=lll
              kinamsv(ll2)=kinam1
              elinsv(ll2)=elin
3083          continue
c            if (lprii.ne.0) then
c              write (lun11,9924)j,elin,kinam1,elmtp,elmtpb,tau0(1,j),
c     $              tau0(2,j),tmpflux
c              write (lun11,*)'ll2,llk,lll=',ll2,ll1,lll
c             endif
9924        format (1h ,i8,1pe12.4,1x,a8,5(1pe12.4))
            fluxbs=fluxbs+tmpflux
            if (ll1.eq.1) scfac=tmpflux
            fluxrl(ll2)=tmpflux/(scfac+1.e-18)
            fluxrl(ll2)=min(fluxrl(ll2),1.e+6)        
c            frac=(elmtp+elmtpb)/(etotc+1.e-18)
            asym=elmtp/amax1(1.e-24,elmtp+elmtpb)
            nbltp=nbinc(ener,epi)
            ewtmp=0.
            if ((nbltp.gt.0).and.(nbltp.lt.ncn))
     $       ewtmp=(elmtp+elmtpb)/
     $         amax1(1.e-24,zrems(1,nbltp)+zrems(2,nbltp))
            eww=eww+ewtmp
            ebar=ebar+tmpflux*elin
c            write (lun11,9825)ll1,kinam1,elmtmpin(lll),tmpflux
821         continue
         if (ll1.eq.1) scfac=fluxbs
         fluxrl(ll2)=fluxbs/(scfac+1.e-18)                                   
         fluxrl(ll2)=min(fluxrl(ll2),1.e+6)   
c         if (lprii.ne.0)
c     $     write (lun11,*)'ll2,fluxrl:',ll2,fluxrl(ll2)
         asym=fluxbf/amax1(1.e-24,fluxbs)
         ebar=ebar/amax1(fluxbs,1.e-24)     
         r19=r*1.e-19
         eltmp=fluxbs*12.56*r19*r19 
         write (lun11,9924)ll1,elinsv(ll2),kinamsv(ll2),
     $            fluxbs,asym,eltmp,fluxrl(ll2)
 9285    format (1h ,i4,1x,a8,
     $          1x,f11.2,12x,4(1pe11.3))
9825     format (1h ,i4,1x,a8,
     $          1x,f11.2,1x,1pe11.3,1x,f9.0,1pe11.3,1x,f9.2)       
820      continue                                                      
c
c
      return
      end
      subroutine bnchmrk(jj,jk,numrec,
     $       lpri,lun11,tinf,
     $       t,r,delr,rdel,xlum,enlum,xee,xpx,p,abel,cfrac,
     $       epi,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $       ntotit,lnerrd,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,
     $       xilev,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
     $       elum,zrems,zremsz)
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     line luminosities
      dimension elum(2,nnnl)
c     line emissivities
      dimension rcem(2,nnnl)
c     line opacities
      dimension oplin(nnnl)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c     continuum lum
      dimension zrems(3,ncn),
     $          zremsz(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum emissivities
      dimension rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      dimension opakc(ncn)
c     level populations
      dimension xilev(nnml)
      dimension cemab(2,nnml),opakab(nnml)
      dimension tauc(2,nnml)
c     ion abundances
      dimension xii(nni)
c     heating/cooling
      dimension htt(nni),cll(nni)
      dimension rrrt(nni),pirt(nni)
      dimension abel(13)
c     temporaries for dread
      dimension rdat(20000),idat(20000)
c
      character(100) kinam1
      character(100) kinamsv(500)
      character(1) kdat(20000)
c
      dimension elinsv(500)
c
c
      dimension elnprnt(10000)
      dimension bmdat(15,50,10),elnsv(2,50,10),eldat(50,10),
     $   nbmdat(10),fbsv(50),sig(50),err(50),sigsm(50),nsm(50),
     $   nlnsv(10),nbmpt(50,10),nlnsv2(10),fluxrl(50),lllsv(50)
c
      character(8) klbm(50,10)
c
c     'table3meudonhiiregion,
c     ',,meu,lex,1,2,3,4,5,6,8,9,10,11
       data nlnsv2(1)/13/
c     table 2: cool h ii region:  mean,1,2,3,5,6,8,9,10,11,0.,0./
       data nbmdat(1)/10/
       data klbm(14,1),eldat(14,1),bmdat(1,14,1),bmdat(2,14,1),
     $ bmdat(3,14,1),bmdat(4,14,1),bmdat(5,14,1),bmdat(6,14,1),
     $ bmdat(7,14,1),bmdat(8,14,1),bmdat(9,14,1),bmdat(10,14,1),
     $ bmdat(11,14,1),bmdat(12,14,1)
     $ /'l(hb)',0.,4.93,4.99,4.98,4.93,4.85,
     $  4.83,4.93,4.94,5.01,4.91,0.,0./
       data klbm(2,1),eldat(2,1),bmdat(1,2,1),bmdat(2,2,1),
     $ bmdat(3,2,1),bmdat(4,2,1),bmdat(5,2,1),bmdat(6,2,1),
     $ bmdat(7,2,1),bmdat(8,2,1),bmdat(9,2,1),bmdat(10,2,1),
     $ bmdat(11,2,1),bmdat(12,2,1)
     $ /'[n ii]',6584,0.85,0.82,0.91,0.82,0.97,0.82,
     $  0.84,0.84,0.83,0.83,0.,0./
       data klbm(3,1),eldat(3,1),bmdat(1,3,1),bmdat(2,3,1),
     $ bmdat(3,3,1),bmdat(4,3,1),bmdat(5,3,1),bmdat(6,3,1),
     $ bmdat(7,3,1),bmdat(8,3,1),bmdat(9,3,1),bmdat(10,3,1),
     $ bmdat(11,3,1),bmdat(12,3,1)
     $ /'[o iii]',3727,1.18,1.11,1.16,1.22,1.32,1.14,
     $  1.21,1.24,1.14,1.11,0.,0./
       data klbm(4,1),eldat(4,1),bmdat(1,4,1),bmdat(2,4,1),
     $ bmdat(3,4,1),bmdat(4,4,1),bmdat(5,4,1),bmdat(6,4,1),
     $ bmdat(7,4,1),bmdat(8,4,1),bmdat(9,4,1),bmdat(10,4,1),
     $ bmdat(11,4,1),bmdat(12,4,1)
     $ /'[ne ii]',1.28e+05,0.31,0.36,0.35,0.29,0.29,0.29,0.29,
     $  0.35,0.29,0.29,0.,0./
       data klbm(5,1),eldat(5,1),bmdat(1,5,1),bmdat(2,5,1),
     $ bmdat(3,5,1),bmdat(4,5,1),bmdat(5,5,1),bmdat(6,5,1),
     $ bmdat(7,5,1),bmdat(8,5,1),bmdat(9,5,1),bmdat(10,5,1),
     $ bmdat(11,5,1),bmdat(12,5,1)
     $ /'[sii]',6720,0.57,0.69,0.64,0.55,0.61,0.52,0.52,
     $  0.6,0.45,0.58,0.,0./
       data klbm(6,1),eldat(6,1),bmdat(1,6,1),bmdat(2,6,1),
     $ bmdat(3,6,1),bmdat(4,6,1),bmdat(5,6,1),bmdat(6,6,1),
     $ bmdat(7,6,1),bmdat(8,6,1),bmdat(9,6,1),bmdat(10,6,1),
     $ bmdat(11,6,1),bmdat(12,6,1)
     $ /'[s iii]',1.87e+05,0.32,0.26,0.27,0.36,0.17,0.37,
     $  0.37,0.33,0.4,0.3,0.,0./
       data klbm(7,1),eldat(7,1),bmdat(1,7,1),bmdat(2,7,1),
     $ bmdat(3,7,1),bmdat(4,7,1),bmdat(5,7,1),bmdat(6,7,1),
     $ bmdat(7,7,1),bmdat(8,7,1),bmdat(9,7,1),bmdat(10,7,1),
     $ bmdat(11,7,1),bmdat(12,7,1)
     $ /'[s iii]',3.40e+05,0.52,0.43,0.47,0.6,0.27,0.61,
     $  0.62,0.54,0.67,0.51,0.,0./
       data klbm(8,1),eldat(8,1),bmdat(1,8,1),bmdat(2,8,1),
     $ bmdat(3,8,1),bmdat(4,8,1),bmdat(5,8,1),bmdat(6,8,1),
     $ bmdat(7,8,1),bmdat(8,8,1),bmdat(9,8,1),bmdat(10,8,1),
     $ bmdat(11,8,1),bmdat(12,8,1)
     $ /'[s iii]',9532,0.55,0.4,0.48,0.55,0.64,0.6,0.56,
     $  0.49,0.62,0.58,0.,0./
       data klbm(9,1),eldat(9,1),bmdat(1,9,1),bmdat(2,9,1),
     $ bmdat(3,9,1),bmdat(4,9,1),bmdat(5,9,1),bmdat(6,9,1),
     $ bmdat(7,9,1),bmdat(8,9,1),bmdat(9,9,1),bmdat(10,9,1),
     $ bmdat(11,9,1),bmdat(12,9,1)
     $ /'l(total)',0.,21.2,20.3,21.3,21.7,20.7,21,21.8,
     $  21.7,22.1,20.6,0.,0./
       data klbm(10,1),eldat(10,1),bmdat(1,10,1),bmdat(2,10,1),
     $ bmdat(3,10,1),bmdat(4,10,1),bmdat(5,10,1),bmdat(6,10,1),
     $ bmdat(7,10,1),bmdat(8,10,1),bmdat(9,10,1),bmdat(10,10,1),
     $ bmdat(11,10,1),bmdat(12,10,1)
     $ /'t(in)',0.,6793,6860,6952,6749,6980,6870,6747,
     $  6230,6912,6838,0.,0./
       data klbm(11,1),eldat(11,1),bmdat(1,11,1),bmdat(2,11,1),
     $ bmdat(3,11,1),bmdat(4,11,1),bmdat(5,11,1),bmdat(6,11,1),
     $ bmdat(7,11,1),bmdat(8,11,1),bmdat(9,11,1),bmdat(10,11,1),
     $ bmdat(11,11,1),bmdat(12,11,1)
     $ /'t(h+)',0.,6744,6690,6740,6742,6950,6660,6742,
     $  6770,6720,6681,0.,0./
       data klbm(12,1),eldat(12,1),bmdat(1,12,1),bmdat(2,12,1),
     $ bmdat(3,12,1),bmdat(4,12,1),bmdat(5,12,1),bmdat(6,12,1),
     $ bmdat(7,12,1),bmdat(8,12,1),bmdat(9,12,1),bmdat(10,12,1),
     $ bmdat(11,12,1),bmdat(12,12,1)
     $ /'<he+>/<h+>',0.,0.054,0,0.041,0.044,0.068,0.048,
     $  0.034,0.055,0.09,0.,0.,0./
       data klbm(13,1),eldat(13,1),bmdat(1,13,1),bmdat(2,13,1),
     $ bmdat(3,13,1),bmdat(4,13,1),bmdat(5,13,1),bmdat(6,13,1),
     $ bmdat(7,13,1),bmdat(8,13,1),bmdat(9,13,1),bmdat(10,13,1),
     $ bmdat(11,13,1),bmdat(12,13,1)
     $ /'r(out) e18',0.,8.96,9,8.93,8.94,9,8.93,9,8.87,9.02,8.97,0.,0./
c    table 3 meudon hii region, meu,lex,1,2,3,4,5,6,8,9,10,11/
       data nbmdat(2)/12/
       data nlnsv2(2)/23/
       data klbm(24,2),eldat(24,2),bmdat(1,24,2),bmdat(2,24,2),     
     $ bmdat(3,24,2),bmdat(4,24,2),bmdat(5,24,2),bmdat(6,24,2),     
     $ bmdat(7,24,2),bmdat(8,24,2),bmdat(9,24,2),bmdat(10,24,2),     
     $ bmdat(11,24,2),bmdat(12,24,2)/     
     $ 'h i',4862.,2.06,2.03,1.96,2.06,2.04,1.86,2.02,2.02,
     $2.05,2.10,2.11,2.09/
       data klbm(1,2),eldat(1,2),bmdat(1,1,2),bmdat(2,1,2),
     $ bmdat(3,1,2),bmdat(4,1,2),bmdat(5,1,2),bmdat(6,1,2),
     $ bmdat(7,1,2),bmdat(8,1,2),bmdat(9,1,2),bmdat(10,1,2),
     $ bmdat(11,1,2),bmdat(12,1,2)
     $ /'he i',5876,0.116,0.116,0.125,0.109,
     $ 0.119,0.11,0.101,0.116,0.,0.125,0.115,0.12/
       data klbm(2,2),eldat(2,2),bmdat(1,2,2),bmdat(2,2,2),
     $ bmdat(3,2,2),bmdat(4,2,2),bmdat(5,2,2),bmdat(6,2,2),
     $ bmdat(7,2,2),bmdat(8,2,2),bmdat(9,2,2),bmdat(10,2,2),
     $ bmdat(11,2,2),bmdat(12,2,2)
     $ /'c ii',2326,0.17,0.16,0.07,0.19,0.17,0.16,0.16,0.14,
     $  0.18,0.28,0.12,0.14/
       data klbm(3,2),eldat(3,2),bmdat(1,3,2),bmdat(2,3,2),
     $ bmdat(3,3,2),bmdat(4,3,2),bmdat(5,3,2),bmdat(6,3,2),
     $ bmdat(7,3,2),bmdat(8,3,2),bmdat(9,3,2),bmdat(10,3,2),
     $ bmdat(11,3,2),bmdat(12,3,2)
     $ /'c iii]',1909,0.051,0.06,0.05,0.059,0.059,
     $0.027,0.078,0.065,0.076,0.082,0.077,0.071/
       data klbm(4,2),eldat(4,2),bmdat(1,4,2),bmdat(2,4,2),
     $ bmdat(3,4,2),bmdat(4,4,2),bmdat(5,4,2),bmdat(6,4,2),
     $ bmdat(7,4,2),bmdat(8,4,2),bmdat(9,4,2),bmdat(10,4,2),
     $ bmdat(11,4,2),bmdat(12,4,2)
     $ /'[n ii]',1.22e+06,0.,0.031,0.032,0.033,0.,0.,0.,0.036,
     $  0.031,0.03,0.037,0.034/
       data klbm(5,2),eldat(5,2),bmdat(1,5,2),bmdat(2,5,2),
     $ bmdat(3,5,2),bmdat(4,5,2),bmdat(5,5,2),bmdat(6,5,2),
     $ bmdat(7,5,2),bmdat(8,5,2),bmdat(9,5,2),bmdat(10,5,2),
     $ bmdat(11,5,2),bmdat(12,5,2)
     $ /'[n ii]',6584,0.73,0.79,0.61,0.88,0.74,0.94,0.87,0.78,
     $  0.73,0.78,0.81,0.75/
       data klbm(6,2),eldat(6,2),bmdat(1,6,2),bmdat(2,6,2),
     $ bmdat(3,6,2),bmdat(4,6,2),bmdat(5,6,2),bmdat(6,6,2),
     $ bmdat(7,6,2),bmdat(8,6,2),bmdat(9,6,2),bmdat(10,6,2),
     $ bmdat(11,6,2),bmdat(12,6,2)
     $ /'[n iii]',5.70e+05,0.3,0.27,0.16,0.27,0.29,0.,0.26,
     $  0.3,0.3,0.17,0.27,0.39/
       data klbm(7,2),eldat(7,2),bmdat(1,7,2),bmdat(2,7,2),
     $ bmdat(3,7,2),bmdat(4,7,2),bmdat(5,7,2),bmdat(6,7,2),
     $ bmdat(7,7,2),bmdat(8,7,2),bmdat(9,7,2),bmdat(10,7,2),
     $ bmdat(11,7,2),bmdat(12,7,2)
     $ /'[o ii]',3727,2.01,2.16,1.5,2.19,2.14,2.56,2.3,2.11,
     $  2.26,2.41,2.2,1.95/
       data klbm(8,2),eldat(8,2),bmdat(1,8,2),bmdat(2,8,2),
     $ bmdat(3,8,2),bmdat(4,8,2),bmdat(5,8,2),bmdat(6,8,2),
     $ bmdat(7,8,2),bmdat(8,8,2),bmdat(9,8,2),bmdat(10,8,2),
     $ bmdat(11,8,2),bmdat(12,8,2)
     $ /'[o iii]',5.18e+05,1.1,1.07,1.1,1.04,1.11,1.04,0.99,
     $  1.08,1.08,1.23,1.04,0.97/
       data klbm(9,2),eldat(9,2),bmdat(1,9,2),bmdat(2,9,2),
     $ bmdat(3,9,2),bmdat(4,9,2),bmdat(5,9,2),bmdat(6,9,2),
     $ bmdat(7,9,2),bmdat(8,9,2),bmdat(9,9,2),bmdat(10,9,2),
     $ bmdat(11,9,2),bmdat(12,9,2)
     $ /'[o iii]',8.84e+05,1.2,1.23,1.3,1.07,1.28,0.,1.16,
     $  1.25,1.26,1.42,1.2,1.14/
       data klbm(10,2),eldat(10,2),bmdat(1,10,2),bmdat(2,10,2),
     $ bmdat(3,10,2),bmdat(4,10,2),bmdat(5,10,2),bmdat(6,10,2),
     $ bmdat(7,10,2),bmdat(8,10,2),bmdat(9,10,2),bmdat(10,10,2),
     $ bmdat(11,10,2),bmdat(12,10,2)
     $ /'[o iii]',5007,2.03,2.06,2.3,1.93,1.96,1.47,2.29,2.17,
     $  2.1,2.23,2.22,1.89/
       data klbm(11,2),eldat(11,2),bmdat(1,11,2),bmdat(2,11,2),
     $ bmdat(3,11,2),bmdat(4,11,2),bmdat(5,11,2),bmdat(6,11,2),
     $ bmdat(7,11,2),bmdat(8,11,2),bmdat(9,11,2),bmdat(10,11,2),
     $ bmdat(11,11,2),bmdat(12,11,2)
     $ /'[n iii]',1.28e+05,0.21,0.22,0.26,0.23,0.19,0.23,0.22,
     $  0.2,0.2,0.22,0.22,0.2/
       data klbm(12,2),eldat(12,2),bmdat(1,12,2),bmdat(2,12,2),
     $ bmdat(3,12,2),bmdat(4,12,2),bmdat(5,12,2),bmdat(6,12,2),
     $ bmdat(7,12,2),bmdat(8,12,2),bmdat(9,12,2),bmdat(10,12,2),
     $ bmdat(11,12,2),bmdat(12,12,2)
     $ /'[ne iii]',1.55e+05,0.44,0.38,0.37,0.43,0.43,0.47,0.37,
     $  0.42,0.42,0.22,0.34,0.38/
       data klbm(13,2),eldat(13,2),bmdat(1,13,2),bmdat(2,13,2),
     $ bmdat(3,13,2),bmdat(4,13,2),bmdat(5,13,2),bmdat(6,13,2),
     $ bmdat(7,13,2),bmdat(8,13,2),bmdat(9,13,2),bmdat(10,13,2),
     $ bmdat(11,13,2),bmdat(12,13,2)
     $ /'[ne iii]',3869,0.096,0.086,0.085,0.103,
     $  0.086,0.071,0.1,0.079,0.087,0.081,0.087,0.078/
       data klbm(14,2),eldat(14,2),bmdat(1,14,2),bmdat(2,14,2),
     $ bmdat(3,14,2),bmdat(4,14,2),bmdat(5,14,2),bmdat(6,14,2),
     $ bmdat(7,14,2),bmdat(8,14,2),bmdat(9,14,2),bmdat(10,14,2),
     $ bmdat(11,14,2),bmdat(12,14,2)
     $ /'[s ii]',6720,0.14,0.2,0.24,0.23,0.16,0.25,0.22,0.17,
     $  0.13,0.21,0.15,0.21/
       data klbm(15,2),eldat(15,2),bmdat(1,15,2),bmdat(2,15,2),
     $ bmdat(3,15,2),bmdat(4,15,2),bmdat(5,15,2),bmdat(6,15,2),
     $ bmdat(7,15,2),bmdat(8,15,2),bmdat(9,15,2),bmdat(10,15,2),
     $ bmdat(11,15,2),bmdat(12,15,2)
     $ /'[s iii]',1.87e+05,0.55,0.55,0.56,0.48,0.56,0.53,0.5,0.55,
     $  0.58,0.58,0.58,0.55/
       data klbm(16,2),eldat(16,2),bmdat(1,16,2),bmdat(2,16,2),
     $ bmdat(3,16,2),bmdat(4,16,2),bmdat(5,16,2),bmdat(6,16,2),
     $ bmdat(7,16,2),bmdat(8,16,2),bmdat(9,16,2),bmdat(10,16,2),
     $ bmdat(11,16,2),bmdat(12,16,2)
     $ /'[s iii]',3.40e+05,0.93,0.89,0.91,0.82,0.89,0.,0.81,
     $  0.88,0.94,0.92,0.92,0.91/
       data klbm(17,2),eldat(17,2),bmdat(1,17,2),bmdat(2,17,2),
     $ bmdat(3,17,2),bmdat(4,17,2),bmdat(5,17,2),bmdat(6,17,2),
     $ bmdat(7,17,2),bmdat(8,17,2),bmdat(9,17,2),bmdat(10,17,2),
     $ bmdat(11,17,2),bmdat(12,17,2)
     $ /'[s iii]',9532,1.25,1.29,1.16,1.27,1.23,1.15,1.48,1.27,
     $  1.3,1.31,1.32,1.46/
       data klbm(18,2),eldat(18,2),bmdat(1,18,2),bmdat(2,18,2),
     $ bmdat(3,18,2),bmdat(4,18,2),bmdat(5,18,2),bmdat(6,18,2),
     $ bmdat(7,18,2),bmdat(8,18,2),bmdat(9,18,2),bmdat(10,18,2),
     $ bmdat(11,18,2),bmdat(12,18,2)
     $ /'[s iv]',1.05e+05,0.39,0.34,0.22,0.37,0.42,0.35,0.36,
     $  0.41,0.33,0.26,0.38,0.27/
       data klbm(19,2),eldat(19,2),bmdat(1,19,2),bmdat(2,19,2),
     $ bmdat(3,19,2),bmdat(4,19,2),bmdat(5,19,2),bmdat(6,19,2),
     $ bmdat(7,19,2),bmdat(8,19,2),bmdat(9,19,2),bmdat(10,19,2),
     $ bmdat(11,19,2),bmdat(12,19,2)
     $ /'l(total)',0.,24.1,24.2,21.7,24.1,24.1,17.4,24.8,24.3,
     $  24.6,26.4,25.5,24.1/
       data klbm(20,2),eldat(20,2),bmdat(1,20,2),bmdat(2,20,2),
     $ bmdat(3,20,2),bmdat(4,20,2),bmdat(5,20,2),bmdat(6,20,2),
     $ bmdat(7,20,2),bmdat(8,20,2),bmdat(9,20,2),bmdat(10,20,2),
     $ bmdat(11,20,2),bmdat(12,20,2)
     $ /'t(in)',0.,7992,7552,7630,7815,7741,
     $  8057,7670,7650,7399,6530,7582,7445/
       data klbm(21,2),eldat(21,2),bmdat(1,21,2),bmdat(2,21,2),
     $ bmdat(3,21,2),bmdat(4,21,2),bmdat(5,21,2),bmdat(6,21,2),
     $ bmdat(7,21,2),bmdat(8,21,2),bmdat(9,21,2),bmdat(10,21,2),
     $ bmdat(11,21,2),bmdat(12,21,2)
     $ /'t(h+)',0.,7378,8034,7880,8064,8047,
     $  7879,8000,8060,8087,8220,8191,7913/
       data klbm(22,2),eldat(22,2),bmdat(1,22,2),bmdat(2,22,2),
     $ bmdat(3,22,2),bmdat(4,22,2),bmdat(5,22,2),bmdat(6,22,2),
     $ bmdat(7,22,2),bmdat(8,22,2),bmdat(9,22,2),bmdat(10,22,2),
     $ bmdat(11,22,2),bmdat(12,22,2)
     $ /'<he+>/<h+>',0.,0,0.77,0,0.71,0.77,
     $  0.69,0.76,0.75,0.83,0.86,0.79,0.77/
       data klbm(23,2),eldat(23,2),bmdat(1,23,2),bmdat(2,23,2),
     $ bmdat(3,23,2),bmdat(4,23,2),bmdat(5,23,2),bmdat(6,23,2),
     $ bmdat(7,23,2),bmdat(8,23,2),bmdat(9,23,2),bmdat(10,23,2),
     $ bmdat(11,23,2),bmdat(12,23,2)
     $ /'r(out)',0.,1.45,1.48,1.43,1.46,1.46,1.61,1.47,1.46,
     $  1.46,1.46,1.49,1.47/
c     table 4 blister hii region   mean,1,2,3,4,5,6,8,9,10,11,0./
       data nbmdat(3)/11/
       data nlnsv2(3)/22/
       data klbm(1,3),eldat(1,3),bmdat(1,1,3),bmdat(2,1,3),
     $ bmdat(3,1,3),bmdat(4,1,3),bmdat(5,1,3),bmdat(6,1,3),
     $ bmdat(7,1,3),bmdat(8,1,3),bmdat(9,1,3),bmdat(10,1,3),
     $ bmdat(11,1,3),bmdat(12,1,3)
     $ /' i(hb)',0.,4.62,4.6,4.59,4.81,3.89,
     $  4.69,4.67,4.7,4.85,4.58,4.78,0./
       data klbm(2,3),eldat(2,3),bmdat(1,2,3),bmdat(2,2,3),
     $ bmdat(3,2,3),bmdat(4,2,3),bmdat(5,2,3),bmdat(6,2,3),
     $ bmdat(7,2,3),bmdat(8,2,3),bmdat(9,2,3),bmdat(10,2,3),
     $ bmdat(11,2,3),bmdat(12,2,3)
     $ /'he i',5876,0.12,0.12,0.13,0.11,0.11,
     $  0.12,0.12,0.,0.12,0.11,0.12,0./
       data klbm(3,3),eldat(3,3),bmdat(1,3,3),bmdat(2,3,3),
     $ bmdat(3,3,3),bmdat(4,3,3),bmdat(5,3,3),bmdat(6,3,3),
     $ bmdat(7,3,3),bmdat(8,3,3),bmdat(9,3,3),bmdat(10,3,3),
     $ bmdat(11,3,3),bmdat(12,3,3)
     $ /'c ii',2326,0.18,0.06,0.14,0.2,0.3,
     $  0.1,0.15,0.23,0.35,0.11,0.16,0./
       data klbm(4,3),eldat(4,3),bmdat(1,4,3),bmdat(2,4,3),
     $ bmdat(3,4,3),bmdat(4,4,3),bmdat(5,4,3),bmdat(6,4,3),
     $ bmdat(7,4,3),bmdat(8,4,3),bmdat(9,4,3),bmdat(10,4,3),
     $ bmdat(11,4,3),bmdat(12,4,3)
     $ /'c ii',1335,0.09,0.002,0.17,0.14,0.02,
     $  0.13,0.16,0.,0.01,0.02,0.13,0./
       data klbm(5,3),eldat(5,3),bmdat(1,5,3),bmdat(2,5,3),
     $ bmdat(3,5,3),bmdat(4,5,3),bmdat(5,5,3),bmdat(6,5,3),
     $ bmdat(7,5,3),bmdat(8,5,3),bmdat(9,5,3),bmdat(10,5,3),
     $ bmdat(11,5,3),bmdat(12,5,3)
     $ /'c iii]',1909,0.17,0.13,0.22,0.17,
     $  0.08,0.18,0.15,0.2,0.25,0.14,0.23,0./
       data klbm(6,3),eldat(6,3),bmdat(1,6,3),bmdat(2,6,3),
     $ bmdat(3,6,3),bmdat(4,6,3),bmdat(5,6,3),bmdat(6,6,3),
     $ bmdat(7,6,3),bmdat(8,6,3),bmdat(9,6,3),bmdat(10,6,3),
     $ bmdat(11,6,3),bmdat(12,6,3)
     $ /'[n ii]',6584,0.87,0.67,0.58,0.94,
     $  1.48,0.74,0.9,0.87,0.92,0.82,0.83,0./
       data klbm(7,3),eldat(7,3),bmdat(1,7,3),bmdat(2,7,3),
     $ bmdat(3,7,3),bmdat(4,7,3),bmdat(5,7,3),bmdat(6,7,3),
     $ bmdat(7,7,3),bmdat(8,7,3),bmdat(9,7,3),bmdat(10,7,3),
     $ bmdat(11,7,3),bmdat(12,7,3)
     $ /'[n iii]',5.70e+04,0.031,0.032,0.035,0.033,0.,
     $  0.033,0.032,0.034,0.014,0.032,0.033,0./
       data klbm(8,3),eldat(8,3),bmdat(1,8,3),bmdat(2,8,3),
     $ bmdat(3,8,3),bmdat(4,8,3),bmdat(5,8,3),bmdat(6,8,3),
     $ bmdat(7,8,3),bmdat(8,8,3),bmdat(9,8,3),bmdat(10,8,3),
     $ bmdat(11,8,3),bmdat(12,8,3)
     $ /'[o iii]',7330,0.12,0.06,0.1,0.13,0.19,
     $  0.09,0.12,0.14,0.15,0.08,0.1,0./
       data klbm(9,3),eldat(9,3),bmdat(1,9,3),bmdat(2,9,3),
     $ bmdat(3,9,3),bmdat(4,9,3),bmdat(5,9,3),bmdat(6,9,3),
     $ bmdat(7,9,3),bmdat(8,9,3),bmdat(9,9,3),bmdat(10,9,3),
     $ bmdat(11,9,3),bmdat(12,9,3)
     $ /'[o iii]',3727,0.88,0.53,0.73,0.98,
     $  1.39,0.69,0.86,1.04,1.04,0.73,0.86,0./
       data klbm(10,3),eldat(10,3),bmdat(1,10,3),bmdat(2,10,3),
     $ bmdat(3,10,3),bmdat(4,10,3),bmdat(5,10,3),bmdat(6,10,3),
     $ bmdat(7,10,3),bmdat(8,10,3),bmdat(9,10,3),bmdat(10,10,3),
     $ bmdat(11,10,3),bmdat(12,10,3)
     $ /'[o iii]',5.18e+04,0.29,0.29,0.31,
     $  0.29,0.26,0.28,0.28,0.28,0.32,0.28,0.27,0./
       data klbm(11,3),eldat(11,3),bmdat(1,11,3),bmdat(2,11,3),
     $ bmdat(3,11,3),bmdat(4,11,3),bmdat(5,11,3),bmdat(6,11,3),
     $ bmdat(7,11,3),bmdat(8,11,3),bmdat(9,11,3),bmdat(10,11,3),
     $ bmdat(11,11,3),bmdat(12,11,3)
     $ /'[o iii]',5007,4.13,4.5,4.74,3.9,
     $  3.28,4.4,3.9,3.96,4.51,4.16,3.98,0./
       data klbm(12,3),eldat(12,3),bmdat(1,12,3),bmdat(2,12,3),
     $ bmdat(3,12,3),bmdat(4,12,3),bmdat(5,12,3),bmdat(6,12,3),
     $ bmdat(7,12,3),bmdat(8,12,3),bmdat(9,12,3),bmdat(10,12,3),
     $ bmdat(11,12,3),bmdat(12,12,3)
     $ /'[n iii]',1.28e+04,0.36,0.45,
     $  0.32,0.33,0.44,0.35,0.33,0.35,0.36,0.37,0.35,0./
       data klbm(13,3),eldat(13,3),bmdat(1,13,3),bmdat(2,13,3),
     $ bmdat(3,13,3),bmdat(4,13,3),bmdat(5,13,3),bmdat(6,13,3),
     $ bmdat(7,13,3),bmdat(8,13,3),bmdat(9,13,3),bmdat(10,13,3),
     $ bmdat(11,13,3),bmdat(12,13,3)
     $ /'[ne iii]',1.55e+04,0.98,0.93,
     $  1.24,1.07,1.09,0.96,1.04,1,0.59,0.92,0.97,0./
       data klbm(14,3),eldat(14,3),bmdat(1,14,3),bmdat(2,14,3),
     $ bmdat(3,14,3),bmdat(4,14,3),bmdat(5,14,3),bmdat(6,14,3),
     $ bmdat(7,14,3),bmdat(8,14,3),bmdat(9,14,3),bmdat(10,14,3),
     $ bmdat(11,14,3),bmdat(12,14,3)
     $ /'[ne iii]',3869,0.33,0.33,0.48,0.32,
     $  0.31,0.35,0.26,0.29,35,0.31,0.31,0./
       data klbm(15,3),eldat(15,3),bmdat(1,15,3),bmdat(2,15,3),
     $ bmdat(3,15,3),bmdat(4,15,3),bmdat(5,15,3),bmdat(6,15,3),
     $ bmdat(7,15,3),bmdat(8,15,3),bmdat(9,15,3),bmdat(10,15,3),
     $ bmdat(11,15,3),bmdat(12,15,3)
     $ /'[s iii]',1.87e+04,0.35,0.37,0.31,
     $  0.34,0.37,0.31,0.33,0.35,0.37,0.34,0.39,0./
       data klbm(16,3),eldat(16,3),bmdat(1,16,3),bmdat(2,16,3),
     $ bmdat(3,16,3),bmdat(4,16,3),bmdat(5,16,3),bmdat(6,16,3),
     $ bmdat(7,16,3),bmdat(8,16,3),bmdat(9,16,3),bmdat(10,16,3),
     $ bmdat(11,16,3),bmdat(12,16,3)
     $ /'[s iii]',9532,1.53,1.52,1.41,1.46,
     $  1.62,1.51,1.42,1.53,1.61,1.42,1.82,0./
       data klbm(17,3),eldat(17,3),bmdat(1,17,3),bmdat(2,17,3),
     $ bmdat(3,17,3),bmdat(4,17,3),bmdat(5,17,3),bmdat(6,17,3),
     $ bmdat(7,17,3),bmdat(8,17,3),bmdat(9,17,3),bmdat(10,17,3),
     $ bmdat(11,17,3),bmdat(12,17,3)
     $ /'[s iv]',1.05e+04,0.46,0.26,0.54,
     $  0.52,0.42,0.51,0.53,0.43,0.36,0.5,0.51,0./
       data klbm(18,3),eldat(18,3),bmdat(1,18,3),bmdat(2,18,3),
     $ bmdat(3,18,3),bmdat(4,18,3),bmdat(5,18,3),bmdat(6,18,3),
     $ bmdat(7,18,3),bmdat(8,18,3),bmdat(9,18,3),bmdat(10,18,3),
     $ bmdat(11,18,3),bmdat(12,18,3)
     $ /'i(total)',0.,50.3,47.1,52.6,52.4,
     $  44.2,50.4,49.4,50.3,54.9,47.4,52.9,0./
       data klbm(19,3),eldat(19,3),bmdat(1,19,3),bmdat(2,19,3),
     $ bmdat(3,19,3),bmdat(4,19,3),bmdat(5,19,3),bmdat(6,19,3),
     $ bmdat(7,19,3),bmdat(8,19,3),bmdat(9,19,3),bmdat(10,19,3),
     $ bmdat(11,19,3),bmdat(12,19,3)
     $ /'t(in)',0.,7989,8300,8206,7582,
     $  0.,8200,8200,7366,7740,8122,8189,0./
       data klbm(20,3),eldat(20,3),bmdat(1,20,3),bmdat(2,20,3),
     $ bmdat(3,20,3),bmdat(4,20,3),bmdat(5,20,3),bmdat(6,20,3),
     $ bmdat(7,20,3),bmdat(8,20,3),bmdat(9,20,3),bmdat(10,20,3),
     $ bmdat(11,20,3),bmdat(12,20,3)
     $ /'t(h+)',0.,8263,8170,8324,8351,
     $  0.,8310,8200,8328,8220,8217,8250,0./
       data klbm(21,3),eldat(21,3),bmdat(1,21,3),bmdat(2,21,3),
     $ bmdat(3,21,3),bmdat(4,21,3),bmdat(5,21,3),bmdat(6,21,3),
     $ bmdat(7,21,3),bmdat(8,21,3),bmdat(9,21,3),bmdat(10,21,3),
     $ bmdat(11,21,3),bmdat(12,21,3)
     $ /'<he+>/<h+>',0.,0.85,0.,0.94,0.78,
     $  0.,0.93,0.79,0.84,0.86,0.85,0.84,0./
       data klbm(22,3),eldat(22,3),bmdat(1,22,3),bmdat(2,22,3),
     $ bmdat(3,22,3),bmdat(4,22,3),bmdat(5,22,3),bmdat(6,22,3),
     $ bmdat(7,22,3),bmdat(8,22,3),bmdat(9,22,3),bmdat(10,22,3),
     $ bmdat(11,22,3),bmdat(12,22,3)
     $ /'ar',0.,0.,2.9,2.88,3.08,0.,2.93,
     $           2.98,3.09,3.1,2.67,3.03,0./
c     table 6 high ionization pn,  mean,1,2,3,4,5,6,10,11,0.,0.,0./
       data nbmdat(5)/9/
       data nlnsv2(5)/22/
       data klbm(1,5),eldat(1,5),bmdat(1,1,5),bmdat(2,1,5),
     $ bmdat(3,1,5),bmdat(4,1,5),bmdat(5,1,5),bmdat(6,1,5),
     $ bmdat(7,1,5),bmdat(8,1,5),bmdat(9,1,5),bmdat(10,1,5),
     $ bmdat(11,1,5),bmdat(12,1,5)
     $ /' l(hb)',0.,5.85,5.67,6.05,5.96,6.02,
     $      5.65,5.74,5.72,6.02,0.,0.,0./
       data klbm(2,5),eldat(2,5),bmdat(1,2,5),bmdat(2,2,5),
     $ bmdat(3,2,5),bmdat(4,2,5),bmdat(5,2,5),bmdat(6,2,5),
     $ bmdat(7,2,5),bmdat(8,2,5),bmdat(9,2,5),bmdat(10,2,5),
     $ bmdat(11,2,5),bmdat(12,2,5)
     $ /'he i',5876,0.12,0.12,0.13,0.13,
     $           0.1,0.12,0.13,0.13,0.13,0.,0.,0./
       data klbm(3,5),eldat(3,5),bmdat(1,3,5),bmdat(2,3,5),
     $ bmdat(3,3,5),bmdat(4,3,5),bmdat(5,3,5),bmdat(6,3,5),
     $ bmdat(7,3,5),bmdat(8,3,5),bmdat(9,3,5),bmdat(10,3,5),
     $ bmdat(11,3,5),bmdat(12,3,5)
     $ /'he ii',4686,0.081,0.096,0.08,0.087,
     $  0.039,0.085,0.092,0.09,0.083,0.,0.,0./
       data klbm(4,5),eldat(4,5),bmdat(1,4,5),bmdat(2,4,5),
     $ bmdat(3,4,5),bmdat(4,4,5),bmdat(5,4,5),bmdat(6,4,5),
     $ bmdat(7,4,5),bmdat(8,4,5),bmdat(9,4,5),bmdat(10,4,5),
     $ bmdat(11,4,5),bmdat(12,4,5)
     $ /'c iii]',1909,0.83,0.9,0.6,0.6,
     $          0.89,0.99,0.89,1.03,0.74,0.,0.,0./
       data klbm(5,5),eldat(5,5),bmdat(1,5,5),bmdat(2,5,5),
     $ bmdat(3,5,5),bmdat(4,5,5),bmdat(5,5,5),bmdat(6,5,5),
     $ bmdat(7,5,5),bmdat(8,5,5),bmdat(9,5,5),bmdat(10,5,5),
     $ bmdat(11,5,5),bmdat(12,5,5)
     $ /'c iv',1549,0.34,0.24,0.35,0.29,0.45,
     $           0.4,0.37,0.32,0.29,0.,0.,0./
       data klbm(6,5),eldat(6,5),bmdat(1,6,5),bmdat(2,6,5),
     $ bmdat(3,6,5),bmdat(4,6,5),bmdat(5,6,5),bmdat(6,6,5),
     $ bmdat(7,6,5),bmdat(8,6,5),bmdat(9,6,5),bmdat(10,6,5),
     $ bmdat(11,6,5),bmdat(12,6,5)
     $ /'[n ii]',6584,0.12,0.12,0.11,0.11,
     $  0.14,0.15,0.12,0.12,0.12,0.,0.,0./
       data klbm(7,5),eldat(7,5),bmdat(1,7,5),bmdat(2,7,5),
     $ bmdat(3,7,5),bmdat(4,7,5),bmdat(5,7,5),bmdat(6,7,5),
     $ bmdat(7,7,5),bmdat(8,7,5),bmdat(9,7,5),bmdat(10,7,5),
     $ bmdat(11,7,5),bmdat(12,7,5)
     $ /'[n iii]',5.70e+04,0.39,0.27,0.37,
     $            0.,0.,0.4,0.41,0.4,0.48,0.,0.,0./
       data klbm(8,5),eldat(8,5),bmdat(1,8,5),bmdat(2,8,5),
     $ bmdat(3,8,5),bmdat(4,8,5),bmdat(5,8,5),bmdat(6,8,5),
     $ bmdat(7,8,5),bmdat(8,8,5),bmdat(9,8,5),bmdat(10,8,5),
     $ bmdat(11,8,5),bmdat(12,8,5)
     $ /'[o ii]',3727,0.29,0.32,0.22,0.24,
     $  0.35,0.35,0.26,0.32,0.27,0.,0.,0./
       data klbm(9,5),eldat(9,5),bmdat(1,9,5),bmdat(2,9,5),
     $ bmdat(3,9,5),bmdat(4,9,5),bmdat(5,9,5),bmdat(6,9,5),
     $ bmdat(7,9,5),bmdat(8,9,5),bmdat(9,9,5),bmdat(10,9,5),
     $ bmdat(11,9,5),bmdat(12,9,5)
     $ /'[o iii]',5007,11.5,12.1,10,
     $  10.1,12.7,12.2,11.7,11.9,11.2,0.,0.,0./
       data klbm(10,5),eldat(10,5),bmdat(1,10,5),bmdat(2,10,5),
     $ bmdat(3,10,5),bmdat(4,10,5),bmdat(5,10,5),bmdat(6,10,5),
     $ bmdat(7,10,5),bmdat(8,10,5),bmdat(9,10,5),bmdat(10,10,5),
     $ bmdat(11,10,5),bmdat(12,10,5)
     $ /'[o iii]',5.20e+04,2.02,2.03,1.88,1.96,2.39,
     $   1.95,2.02,2.02,1.94,0.,0.,0./
       data klbm(11,5),eldat(11,5),bmdat(1,11,5),bmdat(2,11,5),
     $ bmdat(3,11,5),bmdat(4,11,5),bmdat(5,11,5),bmdat(6,11,5),
     $ bmdat(7,11,5),bmdat(8,11,5),bmdat(9,11,5),bmdat(10,11,5),
     $ bmdat(11,11,5),bmdat(12,11,5)
     $ /'[o iv]',2.60e+04,0.79,0.76,0.68,
     $  0.8,1.09,0.71,0.86,0.77,0.67,0.,0.,0./
       data klbm(12,5),eldat(12,5),bmdat(1,12,5),bmdat(2,12,5),
     $ bmdat(3,12,5),bmdat(4,12,5),bmdat(5,12,5),bmdat(6,12,5),
     $ bmdat(7,12,5),bmdat(8,12,5),bmdat(9,12,5),bmdat(10,12,5),
     $ bmdat(11,12,5),bmdat(12,12,5)
     $ /'[ne iii]',1.55e+04,1.35,1.35,1.3,1.32,
     $  1.55,1.3,1.35,1.34,1.31,0.,0.,0./
       data klbm(13,5),eldat(13,5),bmdat(1,13,5),bmdat(2,13,5),
     $ bmdat(3,13,5),bmdat(4,13,5),bmdat(5,13,5),bmdat(6,13,5),
     $ bmdat(7,13,5),bmdat(8,13,5),bmdat(9,13,5),bmdat(10,13,5),
     $ bmdat(11,13,5),bmdat(12,13,5)
     $ /'[ne iii]',3869,1.03,1.15,1.02,0.92,
     $      1.02,1.13,0.89,1.11,1,0.,0.,0./
       data klbm(14,5),eldat(14,5),bmdat(1,14,5),bmdat(2,14,5),
     $ bmdat(3,14,5),bmdat(4,14,5),bmdat(5,14,5),bmdat(6,14,5),
     $ bmdat(7,14,5),bmdat(8,14,5),bmdat(9,14,5),bmdat(10,14,5),
     $ bmdat(11,14,5),bmdat(12,14,5)
     $ /'mg ii',2798,0.13,0.34,0.1,0.07,0.14,0.05,0.1,0.1,0.11,0.,0.,0./
       data klbm(15,5),eldat(15,5),bmdat(1,15,5),bmdat(2,15,5),
     $ bmdat(3,15,5),bmdat(4,15,5),bmdat(5,15,5),bmdat(6,15,5),
     $ bmdat(7,15,5),bmdat(8,15,5),bmdat(9,15,5),bmdat(10,15,5),
     $ bmdat(11,15,5),bmdat(12,15,5)
     $ /'si iii]',1892,0.15,0.1,0.09,
     $        0.1,0.37,0.15,0.13,0.,0.11,0.,0.,0./
       data klbm(16,5),eldat(16,5),bmdat(1,16,5),bmdat(2,16,5),
     $ bmdat(3,16,5),bmdat(4,16,5),bmdat(5,16,5),bmdat(6,16,5),
     $ bmdat(7,16,5),bmdat(8,16,5),bmdat(9,16,5),bmdat(10,16,5),
     $ bmdat(11,16,5),bmdat(12,16,5)
     $ /'[s iii]',1.87e+04,0.34,0.45,
     $  0.24,0.32,0.,0.26,0.28,0.36,0.49,0.,0.,0./
       data klbm(17,5),eldat(17,5),bmdat(1,17,5),bmdat(2,17,5),
     $ bmdat(3,17,5),bmdat(4,17,5),bmdat(5,17,5),bmdat(6,17,5),
     $ bmdat(7,17,5),bmdat(8,17,5),bmdat(9,17,5),bmdat(10,17,5),
     $ bmdat(11,17,5),bmdat(12,17,5)
     $ /'[s iii]',9532,1.13,1.4,0.81,
     $                  0.92,0.,1.02,0.85,1.1,1.77,0.,0.,0./
       data klbm(18,5),eldat(18,5),bmdat(1,18,5),bmdat(2,18,5),
     $ bmdat(3,18,5),bmdat(4,18,5),bmdat(5,18,5),bmdat(6,18,5),
     $ bmdat(7,18,5),bmdat(8,18,5),bmdat(9,18,5),bmdat(10,18,5),
     $ bmdat(11,18,5),bmdat(12,18,5)
     $ /'[s iv]',1.05e+04,2.05,1.66,2.19,
     $  2.21,2.1,2.11,2.35,2.31,1.46,0.,0.,0./
       data klbm(19,5),eldat(19,5),bmdat(1,19,5),bmdat(2,19,5),
     $ bmdat(3,19,5),bmdat(4,19,5),bmdat(5,19,5),bmdat(6,19,5),
     $ bmdat(7,19,5),bmdat(8,19,5),bmdat(9,19,5),bmdat(10,19,5),
     $ bmdat(11,19,5),bmdat(12,19,5)
     $ /'l(total)',0.,133,133,122,120,140,132,131,134,134,0.,0.,0./
       data klbm(20,5),eldat(20,5),bmdat(1,20,5),bmdat(2,20,5),
     $ bmdat(3,20,5),bmdat(4,20,5),bmdat(5,20,5),bmdat(6,20,5),
     $ bmdat(7,20,5),bmdat(8,20,5),bmdat(9,20,5),bmdat(10,20,5),
     $ bmdat(11,20,5),bmdat(12,20,5)
     $ /'t(in)',0.,1.48,1.45,1.48,1.42,1.83,1.4,1.45,1.36,1.44,0.,0.,0./
       data klbm(21,5),eldat(21,5),bmdat(1,21,5),bmdat(2,21,5),
     $ bmdat(3,21,5),bmdat(4,21,5),bmdat(5,21,5),bmdat(6,21,5),
     $ bmdat(7,21,5),bmdat(8,21,5),bmdat(9,21,5),bmdat(10,21,5),
     $ bmdat(11,21,5),bmdat(12,21,5)
     $ /'t(h+)',0.,1.05,1.07,1.01,
     $               1.01,1.03,1.14,1.05,1.06,1.03,0.,0.,0./
       data klbm(22,5),eldat(22,5),bmdat(1,22,5),bmdat(2,22,5),
     $ bmdat(3,22,5),bmdat(4,22,5),bmdat(5,22,5),bmdat(6,22,5),
     $ bmdat(7,22,5),bmdat(8,22,5),bmdat(9,22,5),bmdat(10,22,5),
     $ bmdat(11,22,5),bmdat(12,22,5)
     $ /'<he+>/<h+>',0.,0.92,0.,0.92,0.92,0.,
     $         0.92,0.91,0.92,0.92,0.,0.,0./
c     table 5 meudon planetary nebula,  
c             meu,lex,1,2,3,4,5,6,10,11,0.,0./
       data nbmdat(4)/10/
       data nlnsv2(4)/39/
       data klbm(1,4),eldat(1,4),bmdat(1,1,4),bmdat(2,1,4),
     $ bmdat(3,1,4),bmdat(4,1,4),bmdat(5,1,4),bmdat(6,1,4),
     $ bmdat(7,1,4),bmdat(8,1,4),bmdat(9,1,4),bmdat(10,1,4),
     $ bmdat(11,1,4),bmdat(12,1,4)
     $ /' l(hb)',0.,2.6,2.53,2.06,2.63,
     $            2.68,2.35,2.73,2.68,2.3,2.8,0.,0./
       data klbm(2,4),eldat(2,4),bmdat(1,2,4),bmdat(2,2,4),
     $ bmdat(3,2,4),bmdat(4,2,4),bmdat(5,2,4),bmdat(6,2,4),
     $ bmdat(7,2,4),bmdat(8,2,4),bmdat(9,2,4),bmdat(10,2,4),
     $ bmdat(11,2,4),bmdat(12,2,4)
     $ /'he i',5876,0.11,0.09,0.09,0.11,
     $  0.1,0.05,0.1,0.11,0.09,0.11,0.,0./
       data klbm(3,4),eldat(3,4),bmdat(1,3,4),bmdat(2,3,4),
     $ bmdat(3,3,4),bmdat(4,3,4),bmdat(5,3,4),bmdat(6,3,4),
     $ bmdat(7,3,4),bmdat(8,3,4),bmdat(9,3,4),bmdat(10,3,4),
     $ bmdat(11,3,4),bmdat(12,3,4)
     $ /'he ii',4686,0.33,0.41,0.4,0.32,
     $  0.33,0.81,0.35,0.32,0.43,0.34,0.,0./
       data klbm(4,4),eldat(4,4),bmdat(1,4,4),bmdat(2,4,4),
     $ bmdat(3,4,4),bmdat(4,4,4),bmdat(5,4,4),bmdat(6,4,4),
     $ bmdat(7,4,4),bmdat(8,4,4),bmdat(9,4,4),bmdat(10,4,4),
     $ bmdat(11,4,4),bmdat(12,4,4)
     $ /'c ii]',2326,0.38,0.27,0.2,0.33,
     $  0.43,0.12,0.27,0.3,0.22,0.32,0.,0./
       data klbm(5,4),eldat(5,4),bmdat(1,5,4),bmdat(2,5,4),
     $ bmdat(3,5,4),bmdat(4,5,4),bmdat(5,5,4),bmdat(6,5,4),
     $ bmdat(7,5,4),bmdat(8,5,4),bmdat(9,5,4),bmdat(10,5,4),
     $ bmdat(11,5,4),bmdat(12,5,4)
     $ /'c iii]',1909,1.7,2.14,2.4,1.82,
     $  1.66,2.92,1.72,1.87,3.14,1.63,0.,0./
       data klbm(6,4),eldat(6,4),bmdat(1,6,4),bmdat(2,6,4),
     $ bmdat(3,6,4),bmdat(4,6,4),bmdat(5,6,4),bmdat(6,6,4),
     $ bmdat(7,6,4),bmdat(8,6,4),bmdat(9,6,4),bmdat(10,6,4),
     $ bmdat(11,6,4),bmdat(12,6,4)
     $ /'c iv',1549,1.64,2.51,2.6,2.44,
     $  2.05,1.44,2.66,2.18,4.74,1.94,0.,0./
       data klbm(7,4),eldat(7,4),bmdat(1,7,4),bmdat(2,7,4),
     $ bmdat(3,7,4),bmdat(4,7,4),bmdat(5,7,4),bmdat(6,7,4),
     $ bmdat(7,7,4),bmdat(8,7,4),bmdat(9,7,4),bmdat(10,7,4),
     $ bmdat(11,7,4),bmdat(12,7,4)
     $ /'[n ii]',6584,1.44,1.49,1.43,1.59,
     $  1.45,1.69,1.47,1.44,1.47,1.38,0.,0./
       data klbm(8,4),eldat(8,4),bmdat(1,8,4),bmdat(2,8,4),
     $ bmdat(3,8,4),bmdat(4,8,4),bmdat(5,8,4),bmdat(6,8,4),
     $ bmdat(7,8,4),bmdat(8,8,4),bmdat(9,8,4),bmdat(10,8,4),
     $ bmdat(11,8,4),bmdat(12,8,4)
     $ /'n iii]',1749,0.11,0.12,0.16,0.13,
     $  0.13,0.01,0.11,0.13,0.16,0.16,0.,0./
       data klbm(9,4),eldat(9,4),bmdat(1,9,4),bmdat(2,9,4),
     $ bmdat(3,9,4),bmdat(4,9,4),bmdat(5,9,4),bmdat(6,9,4),
     $ bmdat(7,9,4),bmdat(8,9,4),bmdat(9,9,4),bmdat(10,9,4),
     $ bmdat(11,9,4),bmdat(12,9,4)
     $ /'[n iii]',5.70e+04,0.,0.13,0.11,
     $  0.12,0.13,0.,0.13,0.13,0.13,0.14,0.,0./
       data klbm(10,4),eldat(10,4),bmdat(1,10,4),bmdat(2,10,4),
     $ bmdat(3,10,4),bmdat(4,10,4),bmdat(5,10,4),bmdat(6,10,4),
     $ bmdat(7,10,4),bmdat(8,10,4),bmdat(9,10,4),bmdat(10,10,4),
     $ bmdat(11,10,4),bmdat(12,10,4)
     $ /'n iv]',1487,0.12,0.2,0.22,
     $  0.2,0.15,0.2,0.21,0.19,0.26,0.17,0.,0./
       data klbm(11,4),eldat(11,4),bmdat(1,11,4),bmdat(2,11,4),
     $ bmdat(3,11,4),bmdat(4,11,4),bmdat(5,11,4),bmdat(6,11,4),
     $ bmdat(7,11,4),bmdat(8,11,4),bmdat(9,11,4),bmdat(10,11,4),
     $ bmdat(11,11,4),bmdat(12,11,4)
     $ /'n v',1240,0.09,0.21,0.17,0.18,
     $  0.12,0.34,0.23,0.15,0.38,0.15,0.,0./
       data klbm(12,4),eldat(12,4),bmdat(1,12,4),bmdat(2,12,4),
     $ bmdat(3,12,4),bmdat(4,12,4),bmdat(5,12,4),bmdat(6,12,4),
     $ bmdat(7,12,4),bmdat(8,12,4),bmdat(9,12,4),bmdat(10,12,4),
     $ bmdat(11,12,4),bmdat(12,12,4)
     $ /'[o i]',6300,0.15,0.14,0.17,0.15,
     $  0.12,0.15,0.14,0.14,0.16,0.13,0.,0./
       data klbm(13,4),eldat(13,4),bmdat(1,13,4),bmdat(2,13,4),
     $ bmdat(3,13,4),bmdat(4,13,4),bmdat(5,13,4),bmdat(6,13,4),
     $ bmdat(7,13,4),bmdat(8,13,4),bmdat(9,13,4),bmdat(10,13,4),
     $ bmdat(11,13,4),bmdat(12,13,4)
     $ /'[o ii]',3727,2.23,2.28,2.35,
     $  2.23,2.27,0.,2.31,2.18,2.5,2.14,0.,0./
       data klbm(14,4),eldat(14,4),bmdat(1,14,4),bmdat(2,14,4),
     $ bmdat(3,14,4),bmdat(4,14,4),bmdat(5,14,4),bmdat(6,14,4),
     $ bmdat(7,14,4),bmdat(8,14,4),bmdat(9,14,4),bmdat(10,14,4),
     $ bmdat(11,14,4),bmdat(12,14,4)
     $ /'[o iii]',5007,20.9,20.7,21.8,21.1,
     $  21.4,19.8,19.4,21.1,20.2,20.9,0.,0./
       data klbm(15,4),eldat(15,4),bmdat(1,15,4),bmdat(2,15,4),
     $ bmdat(3,15,4),bmdat(4,15,4),bmdat(5,15,4),bmdat(6,15,4),
     $ bmdat(7,15,4),bmdat(8,15,4),bmdat(9,15,4),bmdat(10,15,4),
     $ bmdat(11,15,4),bmdat(12,15,4)
     $ /'[o iii]',4363,0.16,0.16,0.18,0.16,
     $  0.16,0.19,0.14,0.16,0.16,0.15,0.,0./
       data klbm(16,4),eldat(16,4),bmdat(1,16,4),bmdat(2,16,4),
     $ bmdat(3,16,4),bmdat(4,16,4),bmdat(5,16,4),bmdat(6,16,4),
     $ bmdat(7,16,4),bmdat(8,16,4),bmdat(9,16,4),bmdat(10,16,4),
     $ bmdat(11,16,4),bmdat(12,16,4)
     $ /'[o iii]',5.20e+04,1.43,1.34,
     $  1.39,1.42,1.44,0.96,1.4,1.46,1.26,1.41,0.,0./
       data klbm(17,4),eldat(17,4),bmdat(1,17,4),bmdat(2,17,4),
     $ bmdat(3,17,4),bmdat(4,17,4),bmdat(5,17,4),bmdat(6,17,4),
     $ bmdat(7,17,4),bmdat(8,17,4),bmdat(9,17,4),bmdat(10,17,4),
     $ bmdat(11,17,4),bmdat(12,17,4)
     $ /'[o iv]',2.60e+04,3.62,3.92,3.9,
     $  3.52,3.98,4.48,3.32,3.86,5.01,3.33,0.,0./
       data klbm(18,4),eldat(18,4),bmdat(1,18,4),bmdat(2,18,4),
     $ bmdat(3,18,4),bmdat(4,18,4),bmdat(5,18,4),bmdat(6,18,4),
     $ bmdat(7,18,4),bmdat(8,18,4),bmdat(9,18,4),bmdat(10,18,4),
     $ bmdat(11,18,4),bmdat(12,18,4)
     $ /'o iv]',1403,0.13,0.27,0.36,0.2,
     $  0.23,0.18,0.26,0.33,0.41,0.15,0.,0./
       data klbm(19,4),eldat(19,4),bmdat(1,19,4),bmdat(2,19,4),
     $ bmdat(3,19,4),bmdat(4,19,4),bmdat(5,19,4),bmdat(6,19,4),
     $ bmdat(7,19,4),bmdat(8,19,4),bmdat(9,19,4),bmdat(10,19,4),
     $ bmdat(11,19,4),bmdat(12,19,4)
     $ /'o v]',1218,0.09,0.24,0.,0.2,0.11,0.35,0.29,0.19,0.33,0.,0.,0./
       data klbm(20,4),eldat(20,4),bmdat(1,20,4),bmdat(2,20,4),
     $ bmdat(3,20,4),bmdat(4,20,4),bmdat(5,20,4),bmdat(6,20,4),
     $ bmdat(7,20,4),bmdat(8,20,4),bmdat(9,20,4),bmdat(10,20,4),
     $ bmdat(11,20,4),bmdat(12,20,4)
     $ /'[ne iii]',1.55e+04,2.51,2.49,2.67,
     $  2.75,2.76,0.72,2.8,2.81,2.71,2.74,0.,0./
       data klbm(21,4),eldat(21,4),bmdat(1,21,4),bmdat(2,21,4),
     $ bmdat(3,21,4),bmdat(4,21,4),bmdat(5,21,4),bmdat(6,21,4),
     $ bmdat(7,21,4),bmdat(8,21,4),bmdat(9,21,4),bmdat(10,21,4),
     $ bmdat(11,21,4),bmdat(12,21,4)
     $ /'[ne iii]',3869,2.59,2.63,3.2,3.33,
     $  2.27,0.88,2.74,2.44,3.35,2.86,0.,0./
       data klbm(22,4),eldat(22,4),bmdat(1,22,4),bmdat(2,22,4),
     $ bmdat(3,22,4),bmdat(4,22,4),bmdat(5,22,4),bmdat(6,22,4),
     $ bmdat(7,22,4),bmdat(8,22,4),bmdat(9,22,4),bmdat(10,22,4),
     $ bmdat(11,22,4),bmdat(12,22,4)
     $ /'ne iv]',2423,0.56,0.95,1.05,0.72,
     $  0.74,1.64,0.91,0.74,1.19,0.63,0.,0./
       data klbm(23,4),eldat(23,4),bmdat(1,23,4),bmdat(2,23,4),
     $ bmdat(3,23,4),bmdat(4,23,4),bmdat(5,23,4),bmdat(6,23,4),
     $ bmdat(7,23,4),bmdat(8,23,4),bmdat(9,23,4),bmdat(10,23,4),
     $ bmdat(11,23,4),bmdat(12,23,4)
     $ /'[ne v]',3426,0.73,0.9,0.79,0.74,0.6,
     $  2.29,0.73,0.61,0.81,0.63,0.,0./
       data klbm(24,4),eldat(24,4),bmdat(1,24,4),bmdat(2,24,4),
     $ bmdat(3,24,4),bmdat(4,24,4),bmdat(5,24,4),bmdat(6,24,4),
     $ bmdat(7,24,4),bmdat(8,24,4),bmdat(9,24,4),bmdat(10,24,4),
     $ bmdat(11,24,4),bmdat(12,24,4)
     $ /'[ne v]',2.42e+04,1.67,0.88,1.2,0.94,
     $  0.76,1.16,0.81,0.99,0.25,0.95,0.,0./
       data klbm(25,4),eldat(25,4),bmdat(1,25,4),bmdat(2,25,4),
     $ bmdat(3,25,4),bmdat(4,25,4),bmdat(5,25,4),bmdat(6,25,4),
     $ bmdat(7,25,4),bmdat(8,25,4),bmdat(9,25,4),bmdat(10,25,4),
     $ bmdat(11,25,4),bmdat(12,25,4)
     $ /'mg ii',2798,1.48,1.56,2.5,2.33,
     $  1.6,0.63,1.22,1.17,1.15,1.92,0.,0./
       data klbm(26,4),eldat(26,4),bmdat(1,26,4),bmdat(2,26,4),
     $ bmdat(3,26,4),bmdat(4,26,4),bmdat(5,26,4),bmdat(6,26,4),
     $ bmdat(7,26,4),bmdat(8,26,4),bmdat(9,26,4),bmdat(10,26,4),
     $ bmdat(11,26,4),bmdat(12,26,4)
     $ /'[mg iv]',4.50e+03,0.09,0.12,0.11,
     $  0.12,0.13,0.,0.,0.12,0.14,0.,0.,0./
       data klbm(27,4),eldat(27,4),bmdat(1,27,4),bmdat(2,27,4),
     $ bmdat(3,27,4),bmdat(4,27,4),bmdat(5,27,4),bmdat(6,27,4),
     $ bmdat(7,27,4),bmdat(8,27,4),bmdat(9,27,4),bmdat(10,27,4),
     $ bmdat(11,27,4),bmdat(12,27,4)
     $ /'[si ii]',3.48e+04,0.13,0.18,0.14,0.16,
     $  0.26,0.,0.19,0.17,0.15,0.16,0.,0./
       data klbm(28,4),eldat(28,4),bmdat(1,28,4),bmdat(2,28,4),
     $ bmdat(3,28,4),bmdat(4,28,4),bmdat(5,28,4),bmdat(6,28,4),
     $ bmdat(7,28,4),bmdat(8,28,4),bmdat(9,28,4),bmdat(10,28,4),
     $ bmdat(11,28,4),bmdat(12,28,4)
     $ /'si ii]',2335,0.11,0.23,0.23,0.15,0.,
     $           0.53,0.16,0.16,0.,0.15,0.,0./
       data klbm(29,4),eldat(29,4),bmdat(1,29,4),bmdat(2,29,4),
     $ bmdat(3,29,4),bmdat(4,29,4),bmdat(5,29,4),bmdat(6,29,4),
     $ bmdat(7,29,4),bmdat(8,29,4),bmdat(9,29,4),bmdat(10,29,4),
     $ bmdat(11,29,4),bmdat(12,29,4)
     $ /'si iii]',1892,0.2,0.68,0.79,0.39,
     $               0.32,1.95,0.46,0.45,0.,0.4,0.,0./
       data klbm(30,4),eldat(30,4),bmdat(1,30,4),bmdat(2,30,4),
     $ bmdat(3,30,4),bmdat(4,30,4),bmdat(5,30,4),bmdat(6,30,4),
     $ bmdat(7,30,4),bmdat(8,30,4),bmdat(9,30,4),bmdat(10,30,4),
     $ bmdat(11,30,4),bmdat(12,30,4)
     $ /'si iv',1397,0.15,0.15,0.1,0.2,
     $  0.15,0.03,0.21,0.17,0.,0.16,0.,0./
       data klbm(31,4),eldat(31,4),bmdat(1,31,4),bmdat(2,31,4),
     $ bmdat(3,31,4),bmdat(4,31,4),bmdat(5,31,4),bmdat(6,31,4),
     $ bmdat(7,31,4),bmdat(8,31,4),bmdat(9,31,4),bmdat(10,31,4),
     $ bmdat(11,31,4),bmdat(12,31,4)
     $ /'[s ii]',6720,0.39,0.33,0.24,0.21,
     $  0.45,0.08,0.33,0.43,0.41,0.51,0.,0./
       data klbm(32,4),eldat(32,4),bmdat(1,32,4),bmdat(2,32,4),
     $ bmdat(3,32,4),bmdat(4,32,4),bmdat(5,32,4),bmdat(6,32,4),
     $ bmdat(7,32,4),bmdat(8,32,4),bmdat(9,32,4),bmdat(10,32,4),
     $ bmdat(11,32,4),bmdat(12,32,4)
     $ /'[s iii]',1.87e+04,0.49,0.53,0.6,
     $  0.48,0.49,0.,0.46,0.49,0.6,0.55,0.,0./
       data klbm(33,4),eldat(33,4),bmdat(1,33,4),bmdat(2,33,4),
     $ bmdat(3,33,4),bmdat(4,33,4),bmdat(5,33,4),bmdat(6,33,4),
     $ bmdat(7,33,4),bmdat(8,33,4),bmdat(9,33,4),bmdat(10,33,4),
     $ bmdat(11,33,4),bmdat(12,33,4)
     $ /'[s iii]',9532,2.09,1.91,2.31,2.04,1.89,
     $  0.36,2.05,1.87,2.34,2.42,0.,0./
       data klbm(34,4),eldat(34,4),bmdat(1,34,4),bmdat(2,34,4),
     $ bmdat(3,34,4),bmdat(4,34,4),bmdat(5,34,4),bmdat(6,34,4),
     $ bmdat(7,34,4),bmdat(8,34,4),bmdat(9,34,4),bmdat(10,34,4),
     $ bmdat(11,34,4),bmdat(12,34,4)
     $ /'[s iv]',1.05e+04,1.92,1.84,1.58,1.92,
     $  2.21,0.93,1.81,1.98,2.36,1.94,0.,0./
       data klbm(35,4),eldat(35,4),bmdat(1,35,4),bmdat(2,35,4),
     $ bmdat(3,35,4),bmdat(4,35,4),bmdat(5,35,4),bmdat(6,35,4),
     $ bmdat(7,35,4),bmdat(8,35,4),bmdat(9,35,4),bmdat(10,35,4),
     $ bmdat(11,35,4),bmdat(12,35,4)
     $ /'l(total)',0.,129,132,114,139,136,105,135,136,130,142,0.,0./
       data klbm(36,4),eldat(36,4),bmdat(1,36,4),bmdat(2,36,4),
     $ bmdat(3,36,4),bmdat(4,36,4),bmdat(5,36,4),bmdat(6,36,4),
     $ bmdat(7,36,4),bmdat(8,36,4),bmdat(9,36,4),bmdat(10,36,4),
     $ bmdat(11,36,4),bmdat(12,36,4)
     $ /'t(in)',0.,0.,1.8,0.,1.83,1.78,1.63,1.84,1.78,1.95,1.81,0.,0./
       data klbm(37,4),eldat(37,4),bmdat(1,37,4),bmdat(2,37,4),
     $ bmdat(3,37,4),bmdat(4,37,4),bmdat(5,37,4),bmdat(6,37,4),
     $ bmdat(7,37,4),bmdat(8,37,4),bmdat(9,37,4),bmdat(10,37,4),
     $ bmdat(11,37,4),bmdat(12,37,4)
     $ /'t(h+)',0.,0.,1.26,0.,1.22,1.21,1.32,1.35,1.21,1.29,1.2,0.,0./
       data klbm(38,4),eldat(38,4),bmdat(1,38,4),bmdat(2,38,4),
     $ bmdat(3,38,4),bmdat(4,38,4),bmdat(5,38,4),bmdat(6,38,4),
     $ bmdat(7,38,4),bmdat(8,38,4),bmdat(9,38,4),bmdat(10,38,4),
     $ bmdat(11,38,4),bmdat(12,38,4)
     $ /'<he+>/<h+>',0.,0.,0.69,0.,0.74,
     $       0.74,0.,0.71,0.71,0.6,0.68,0.,0./
       data klbm(39,4),eldat(39,4),bmdat(1,39,4),bmdat(2,39,4),
     $ bmdat(3,39,4),bmdat(4,39,4),bmdat(5,39,4),bmdat(6,39,4),
     $ bmdat(7,39,4),bmdat(8,39,4),bmdat(9,39,4),bmdat(10,39,4),
     $ bmdat(11,39,4),bmdat(12,39,4)
     $ /'r(out) e17',0.,0.,4.02,0.,4.04,4.04,0.,
     $         4.07,4.07,3.83,4.08,0.,0./
c     table 7 low ionization pn, mean,1,2,3,5,6,10,11,0.,0.,0.,0./
       data nbmdat(6)/8/ 
       data nlnsv2(6)/17/
       data klbm(1,6),eldat(1,6),bmdat(1,1,6),bmdat(2,1,6),
     $ bmdat(3,1,6),bmdat(4,1,6),bmdat(5,1,6),bmdat(6,1,6),
     $ bmdat(7,1,6),bmdat(8,1,6),bmdat(9,1,6),bmdat(10,1,6),
     $ bmdat(11,1,6),bmdat(12,1,6)
     $ /'l(h a)',0.,5.41,5.2,5.56,5.52,
     $                 5.35,5.41,5.38,5.44,0.,0.,0.,0./
       data klbm(2,6),eldat(2,6),bmdat(1,2,6),bmdat(2,2,6),
     $ bmdat(3,2,6),bmdat(4,2,6),bmdat(5,2,6),bmdat(6,2,6),
     $ bmdat(7,2,6),bmdat(8,2,6),bmdat(9,2,6),bmdat(10,2,6),
     $ bmdat(11,2,6),bmdat(12,2,6)
     $ /'he i',5876,0.13,0.12,0.14,0.12,0.14,0.12,0.12,0.15,0.,0.,0.,0./
       data klbm(3,6),eldat(3,6),bmdat(1,3,6),bmdat(2,3,6),
     $ bmdat(3,3,6),bmdat(4,3,6),bmdat(5,3,6),bmdat(6,3,6),
     $ bmdat(7,3,6),bmdat(8,3,6),bmdat(9,3,6),bmdat(10,3,6),
     $ bmdat(11,3,6),bmdat(12,3,6)
     $ /'he ii',4686,0.088,0.095,0.082,
     $  0.088,0.088,0.088,0.091,0.086,0.,0.,0.,0./
       data klbm(4,6),eldat(4,6),bmdat(1,4,6),bmdat(2,4,6),
     $ bmdat(3,4,6),bmdat(4,4,6),bmdat(5,4,6),bmdat(6,4,6),
     $ bmdat(7,4,6),bmdat(8,4,6),bmdat(9,4,6),bmdat(10,4,6),
     $ bmdat(11,4,6),bmdat(12,4,6)
     $ /'c iii]',1909,1.17,1.53,0.78,0.81,1.25,1.41,1.43,1,0.,0.,0.,0./
       data klbm(5,6),eldat(5,6),bmdat(1,5,6),bmdat(2,5,6),
     $ bmdat(3,5,6),bmdat(4,5,6),bmdat(5,5,6),bmdat(6,5,6),
     $ bmdat(7,5,6),bmdat(8,5,6),bmdat(9,5,6),bmdat(10,5,6),
     $ bmdat(11,5,6),bmdat(12,5,6)
     $ /'c iv',1549,1.38,1.2,1.34,1.31,1.54,1.43,1.51,1.32,0.,0.,0.,0./
       data klbm(6,6),eldat(6,6),bmdat(1,6,6),bmdat(2,6,6),
     $ bmdat(3,6,6),bmdat(4,6,6),bmdat(5,6,6),bmdat(6,6,6),
     $ bmdat(7,6,6),bmdat(8,6,6),bmdat(9,6,6),bmdat(10,6,6),
     $ bmdat(11,6,6),bmdat(12,6,6)
     $ /'[o iii]',5007,14.3,16,12.7,13.1,
     $             15.,14.5,14.6,14.3,0.,0.,0.,0./
       data klbm(7,6),eldat(7,6),bmdat(1,7,6),bmdat(2,7,6),
     $ bmdat(3,7,6),bmdat(4,7,6),bmdat(5,7,6),bmdat(6,7,6),
     $ bmdat(7,7,6),bmdat(8,7,6),bmdat(9,7,6),bmdat(10,7,6),
     $ bmdat(11,7,6),bmdat(12,7,6)
     $ /'[o iii]',5.20e+04,0.26,0.28,0.26,
     $  0.26,0.26,0.27,0.,0.26,0.,0.,0.,0./
       data klbm(8,6),eldat(8,6),bmdat(1,8,6),bmdat(2,8,6),
     $ bmdat(3,8,6),bmdat(4,8,6),bmdat(5,8,6),bmdat(6,8,6),
     $ bmdat(7,8,6),bmdat(8,8,6),bmdat(9,8,6),bmdat(10,8,6),
     $ bmdat(11,8,6),bmdat(12,8,6)
     $ /'[o iv]',2.60e+04,0.22,0.24,0.21,
     $  0.23,0.21,0.21,0.22,0.22,0.,0.,0.,0./
       data klbm(9,6),eldat(9,6),bmdat(1,9,6),bmdat(2,9,6),
     $ bmdat(3,9,6),bmdat(4,9,6),bmdat(5,9,6),bmdat(6,9,6),
     $ bmdat(7,9,6),bmdat(8,9,6),bmdat(9,9,6),bmdat(10,9,6),
     $ bmdat(11,9,6),bmdat(12,9,6)
     $ /'[ne iii]',1.55e+04,1.11,1.14,1.09,
     $  1.1,1.1,1.12,1.12,1.1,0.,0.,0.,0./
       data klbm(10,6),eldat(10,6),bmdat(1,10,6),bmdat(2,10,6),
     $ bmdat(3,10,6),bmdat(4,10,6),bmdat(5,10,6),bmdat(6,10,6),
     $ bmdat(7,10,6),bmdat(8,10,6),bmdat(9,10,6),bmdat(10,10,6),
     $ bmdat(11,10,6),bmdat(12,10,6)
     $ /'[ne iii]',3869,1.44,1.66,1.39,1.27,
     $  1.48,1.44,1.45,1.38,0.,0.,0.,0./
       data klbm(11,6),eldat(11,6),bmdat(1,11,6),bmdat(2,11,6),
     $ bmdat(3,11,6),bmdat(4,11,6),bmdat(5,11,6),bmdat(6,11,6),
     $ bmdat(7,11,6),bmdat(8,11,6),bmdat(9,11,6),bmdat(10,11,6),
     $ bmdat(11,11,6),bmdat(12,11,6)
     $ /'ne iv]',2423,0.1,0.1,0.11,0.11,0.1,0.08,0.08,0.1,0.,0.,0.,0./
       data klbm(12,6),eldat(12,6),bmdat(1,12,6),bmdat(2,12,6),
     $ bmdat(3,12,6),bmdat(4,12,6),bmdat(5,12,6),bmdat(6,12,6),
     $ bmdat(7,12,6),bmdat(8,12,6),bmdat(9,12,6),bmdat(10,12,6),
     $ bmdat(11,12,6),bmdat(12,12,6)
     $ /'[s iii]',9532,0.52,0.65,0.23,0.37,
     $      0.26,0.42,0.76,0.99,0.,0.,0.,0./
       data klbm(13,6),eldat(13,6),bmdat(1,13,6),bmdat(2,13,6),
     $ bmdat(3,13,6),bmdat(4,13,6),bmdat(5,13,6),bmdat(6,13,6),
     $ bmdat(7,13,6),bmdat(8,13,6),bmdat(9,13,6),bmdat(10,13,6),
     $ bmdat(11,13,6),bmdat(12,13,6)
     $ /'[s ii]',1.05e+04,1.43,1.2,
     $  1.32,1.57,1.32,1.84,1.38,1.37,0.,0.,0.,0./
       data klbm(14,6),eldat(14,6),bmdat(1,14,6),bmdat(2,14,6),
     $ bmdat(3,14,6),bmdat(4,14,6),bmdat(5,14,6),bmdat(6,14,6),
     $ bmdat(7,14,6),bmdat(8,14,6),bmdat(9,14,6),bmdat(10,14,6),
     $ bmdat(11,14,6),bmdat(12,14,6)
     $ /'l(total)',0.,120,126,109,112,122,124,122,121,0.,0.,0.,0./
       data klbm(15,6),eldat(15,6),bmdat(1,15,6),bmdat(2,15,6),
     $ bmdat(3,15,6),bmdat(4,15,6),bmdat(5,15,6),bmdat(6,15,6),
     $ bmdat(7,15,6),bmdat(8,15,6),bmdat(9,15,6),bmdat(10,15,6),
     $ bmdat(11,15,6),bmdat(12,15,6)
     $ /'t(in)',0.,1.78,0.,1.83,1.79,1.76,1.73,1.76,1.81,0.,0.,0.,0./
       data klbm(16,6),eldat(16,6),bmdat(1,16,6),bmdat(2,16,6),
     $ bmdat(3,16,6),bmdat(4,16,6),bmdat(5,16,6),bmdat(6,16,6),
     $ bmdat(7,16,6),bmdat(8,16,6),bmdat(9,16,6),bmdat(10,16,6),
     $ bmdat(11,16,6),bmdat(12,16,6)
     $ /'t(h+)',0.,1.16,0.,1.11,1.11,1.28,1.14,1.2,1.14,0.,0.,0.,0./
       data klbm(17,6),eldat(17,6),bmdat(1,17,6),bmdat(2,17,6),
     $ bmdat(3,17,6),bmdat(4,17,6),bmdat(5,17,6),bmdat(6,17,6),
     $ bmdat(7,17,6),bmdat(8,17,6),bmdat(9,17,6),bmdat(10,17,6),
     $ bmdat(11,17,6),bmdat(12,17,6)
     $ /'<he.>/ch.>',0.,0.91,0.,0.91,0.91,
     $      0.91,0.91,0.9,0.9,0.,0.,0.,0./
c     table 8 nlr cloud,mean,1,2,4,5,6,11,
       data nbmdat(7)/7/
       data nlnsv2(7)/29/
      data klbm(30,7),eldat(30,7),bmdat(1,30,7),bmdat(2,30,7),
     $ bmdat(3,30,7),bmdat(4,30,7),bmdat(5,30,7),bmdat(6,30,7),
     $ bmdat(7,30,7),bmdat(8,30,7),bmdat(9,30,7),bmdat(10,30,7),
     $ bmdat(11,30,7),bmdat(12,30,7)
     $ /'6',0.,0.,0.,0.,1.06,1.37,1.43,1.34,0.,0.,0.,0.,0./
       data klbm(1,7),eldat(1,7),bmdat(1,1,7),bmdat(2,1,7),
     $ bmdat(3,1,7),bmdat(4,1,7),bmdat(5,1,7),bmdat(6,1,7),
     $ bmdat(7,1,7),bmdat(8,1,7),bmdat(9,1,7),bmdat(10,1,7),
     $ bmdat(11,1,7),bmdat(12,1,7)
     $ /'lyoc',1216,34.2,38.3,32.1,37,32.4,31.5,34.2,0.,0.,0.,0.,0./
       data klbm(2,7),eldat(2,7),bmdat(1,2,7),bmdat(2,2,7),
     $ bmdat(3,2,7),bmdat(4,2,7),bmdat(5,2,7),bmdat(6,2,7),
     $ bmdat(7,2,7),bmdat(8,2,7),bmdat(9,2,7),bmdat(10,2,7),
     $ bmdat(11,2,7),bmdat(12,2,7)
     $ /'he i',5876,0.12,0.11,0.13,0.14,0.12,0.13,0.13,0.,0.,0.,0.,0./
       data klbm(3,7),eldat(3,7),bmdat(1,3,7),bmdat(2,3,7),
     $ bmdat(3,3,7),bmdat(4,3,7),bmdat(5,3,7),bmdat(6,3,7),
     $ bmdat(7,3,7),bmdat(8,3,7),bmdat(9,3,7),bmdat(10,3,7),
     $ bmdat(11,3,7),bmdat(12,3,7)
     $ /'he ii',4686,0.24,0.25,0.25,0.,
     $               0.25,0.23,0.24,0.,0.,0.,0.,0./
       data klbm(4,7),eldat(4,7),bmdat(1,4,7),bmdat(2,4,7),
     $ bmdat(3,4,7),bmdat(4,4,7),bmdat(5,4,7),bmdat(6,4,7),
     $ bmdat(7,4,7),bmdat(8,4,7),bmdat(9,4,7),bmdat(10,4,7),
     $ bmdat(11,4,7),bmdat(12,4,7)
     $ /'he ii',1640,1.6,1.6,1.74,1.49,1.53,1.56,1.67,0.,0.,0.,0.,0./
       data klbm(5,7),eldat(5,7),bmdat(1,5,7),bmdat(2,5,7),
     $ bmdat(3,5,7),bmdat(4,5,7),bmdat(5,5,7),bmdat(6,5,7),
     $ bmdat(7,5,7),bmdat(8,5,7),bmdat(9,5,7),bmdat(10,5,7),
     $ bmdat(11,5,7),bmdat(12,5,7)
     $ /'ciii]',1909,2.82,2.9,2.99,2.45,2.87,2.83,2.9,0.,0.,0.,0.,0./
       data klbm(6,7),eldat(6,7),bmdat(1,6,7),bmdat(2,6,7),
     $ bmdat(3,6,7),bmdat(4,6,7),bmdat(5,6,7),bmdat(6,6,7),
     $ bmdat(7,6,7),bmdat(8,6,7),bmdat(9,6,7),bmdat(10,6,7),
     $ bmdat(11,6,7),bmdat(12,6,7)
     $ /'civ',1549,3.18,2.7,3.85,2.28,3.69,3.17,3.36,0.,0.,0.,0.,0./
       data klbm(7,7),eldat(7,7),bmdat(1,7,7),bmdat(2,7,7),
     $ bmdat(3,7,7),bmdat(4,7,7),bmdat(5,7,7),bmdat(6,7,7),
     $ bmdat(7,7,7),bmdat(8,7,7),bmdat(9,7,7),bmdat(10,7,7),
     $ bmdat(11,7,7),bmdat(12,7,7)
     $ /'[nii]',6584,2.33,1.4,3.2,1.21,3.1,2.67,2.4,0.,0.,0.,0.,0./
       data klbm(8,7),eldat(8,7),bmdat(1,8,7),bmdat(2,8,7),
     $ bmdat(3,8,7),bmdat(4,8,7),bmdat(5,8,7),bmdat(6,8,7),
     $ bmdat(7,8,7),bmdat(8,8,7),bmdat(9,8,7),bmdat(10,8,7),
     $ bmdat(11,8,7),bmdat(12,8,7)
     $ /'niii]',1749,0.19,0.24,0.24,0.01,0.22,0.22,0.22,0.,0.,0.,0.,0./
       data klbm(9,7),eldat(9,7),bmdat(1,9,7),bmdat(2,9,7),
     $ bmdat(3,9,7),bmdat(4,9,7),bmdat(5,9,7),bmdat(6,9,7),
     $ bmdat(7,9,7),bmdat(8,9,7),bmdat(9,9,7),bmdat(10,9,7),
     $ bmdat(11,9,7),bmdat(12,9,7)
     $ /'niv]',1487,0.2,0.2,0.23,0.12,0.22,0.21,0.21,0.,0.,0.,0.,0./
       data klbm(10,7),eldat(10,7),bmdat(1,10,7),bmdat(2,10,7),
     $ bmdat(3,10,7),bmdat(4,10,7),bmdat(5,10,7),bmdat(6,10,7),
     $ bmdat(7,10,7),bmdat(8,10,7),bmdat(9,10,7),bmdat(10,10,7),
     $ bmdat(11,10,7),bmdat(12,10,7)
     $ /'[oi]',6300,1.61,2.2,1.61,1.41,1.67,1.31,1.46,0.,0.,0.,0.,0./
       data klbm(11,7),eldat(11,7),bmdat(1,11,7),bmdat(2,11,7),
     $ bmdat(3,11,7),bmdat(4,11,7),bmdat(5,11,7),bmdat(6,11,7),
     $ bmdat(7,11,7),bmdat(8,11,7),bmdat(9,11,7),bmdat(10,11,7),
     $ bmdat(11,11,7),bmdat(12,11,7)
     $ /'[o ii]',6.30e+04,1.12,0.25,1.13,0.,0.,1.44,1.64,0.,0.,0.,0.,0./
       data klbm(12,7),eldat(12,7),bmdat(1,12,7),bmdat(2,12,7),
     $ bmdat(3,12,7),bmdat(4,12,7),bmdat(5,12,7),bmdat(6,12,7),
     $ bmdat(7,12,7),bmdat(8,12,7),bmdat(9,12,7),bmdat(10,12,7),
     $ bmdat(11,12,7),bmdat(12,12,7)
     $ /'[oiii]',3727,1.72,1.6,1.44,3.18,1.58,1.3,1.2,0.,0.,0.,0.,0./
       data klbm(13,7),eldat(13,7),bmdat(1,13,7),bmdat(2,13,7),
     $ bmdat(3,13,7),bmdat(4,13,7),bmdat(5,13,7),bmdat(6,13,7),
     $ bmdat(7,13,7),bmdat(8,13,7),bmdat(9,13,7),bmdat(10,13,7),
     $ bmdat(11,13,7),bmdat(12,13,7)
     $ /'oiii]',1663,0.56,0.35,0.63,0.,0.61,0.57,0.63,0.,0.,0.,0.,0./
       data klbm(14,7),eldat(14,7),bmdat(1,14,7),bmdat(2,14,7),
     $ bmdat(3,14,7),bmdat(4,14,7),bmdat(5,14,7),bmdat(6,14,7),
     $ bmdat(7,14,7),bmdat(8,14,7),bmdat(9,14,7),bmdat(10,14,7),
     $ bmdat(11,14,7),bmdat(12,14,7)
     $ /'[oiii]',5007,33.1,31.4,34.5,31.1,33,32.8,36,0.,0.,0.,0.,0./
       data klbm(15,7),eldat(15,7),bmdat(1,15,7),bmdat(2,15,7),
     $ bmdat(3,15,7),bmdat(4,15,7),bmdat(5,15,7),bmdat(6,15,7),
     $ bmdat(7,15,7),bmdat(8,15,7),bmdat(9,15,7),bmdat(10,15,7),
     $ bmdat(11,15,7),bmdat(12,15,7)
     $ /'[oiii]',4363,0.32,0.3,0.34,0.,
     $             0.31,0.3,0.33,0.,0.,0.,0.,0./
       data klbm(16,7),eldat(16,7),bmdat(1,16,7),bmdat(2,16,7),
     $ bmdat(3,16,7),bmdat(4,16,7),bmdat(5,16,7),bmdat(6,16,7),
     $ bmdat(7,16,7),bmdat(8,16,7),bmdat(9,16,7),bmdat(10,16,7),
     $ bmdat(11,16,7),bmdat(12,16,7)
     $ /'oiv',1403,0.36,0.49,0.3,0.,0.36,0.42,0.25,0.,0.,0.,0.,0./
       data klbm(17,7),eldat(17,7),bmdat(1,17,7),bmdat(2,17,7),
     $ bmdat(3,17,7),bmdat(4,17,7),bmdat(5,17,7),bmdat(6,17,7),
     $ bmdat(7,17,7),bmdat(8,17,7),bmdat(9,17,7),bmdat(10,17,7),
     $ bmdat(11,17,7),bmdat(12,17,7)
     $ /'[neiii]',1.55e+04,1.89,1.5,2.01,0.,
     $        1.94,2.05,1.95,0.,0.,0.,0.,0./
       data klbm(18,7),eldat(18,7),bmdat(1,18,7),bmdat(2,18,7),
     $ bmdat(3,18,7),bmdat(4,18,7),bmdat(5,18,7),bmdat(6,18,7),
     $ bmdat(7,18,7),bmdat(8,18,7),bmdat(9,18,7),bmdat(10,18,7),
     $ bmdat(11,18,7),bmdat(12,18,7)
     $ /'[ne iii]',3869,1.91,1.9,2.51,0.84,
     $              2.16,1.72,2.34,0.,0.,0.,0.,0./
       data klbm(19,7),eldat(19,7),bmdat(1,19,7),bmdat(2,19,7),
     $ bmdat(3,19,7),bmdat(4,19,7),bmdat(5,19,7),bmdat(6,19,7),
     $ bmdat(7,19,7),bmdat(8,19,7),bmdat(9,19,7),bmdat(10,19,7),
     $ bmdat(11,19,7),bmdat(12,19,7)
     $ /'[ne iv]',2423,0.44,0.52,0.42,
     $            0.,0.47,0.41,0.38,0.,0.,0.,0.,0./
       data klbm(20,7),eldat(20,7),bmdat(1,20,7),bmdat(2,20,7),
     $ bmdat(3,20,7),bmdat(4,20,7),bmdat(5,20,7),bmdat(6,20,7),
     $ bmdat(7,20,7),bmdat(8,20,7),bmdat(9,20,7),bmdat(10,20,7),
     $ bmdat(11,20,7),bmdat(12,20,7)
     $ /'[nev]',3426,0.52,0.59,0.55,0.,0 53,0.44,0.5,0.,0.,0.,0.,0./
       data klbm(21,7),eldat(21,7),bmdat(1,21,7),bmdat(2,21,7),
     $ bmdat(3,21,7),bmdat(4,21,7),bmdat(5,21,7),bmdat(6,21,7),
     $ bmdat(7,21,7),bmdat(8,21,7),bmdat(9,21,7),bmdat(10,21,7),
     $ bmdat(11,21,7),bmdat(12,21,7)
     $ /'mgii',2798,1.78,3.5,1.72,1.48,1.23,1.12,1.61,0.,0.,0.,0.,0./
       data klbm(22,7),eldat(22,7),bmdat(1,22,7),bmdat(2,22,7),
     $ bmdat(3,22,7),bmdat(4,22,7),bmdat(5,22,7),bmdat(6,22,7),
     $ bmdat(7,22,7),bmdat(8,22,7),bmdat(9,22,7),bmdat(10,22,7),
     $ bmdat(11,22,7),bmdat(12,22,7)
     $ /'[siii]',3.48e+04,0.9,1,0.96,0.,
     $           1.07,0.96,0.52,0.,0.,0.,0.,0./
       data klbm(23,7),eldat(23,7),bmdat(1,23,7),bmdat(2,23,7),
     $ bmdat(3,23,7),bmdat(4,23,7),bmdat(5,23,7),bmdat(6,23,7),
     $ bmdat(7,23,7),bmdat(8,23,7),bmdat(9,23,7),bmdat(10,23,7),
     $ bmdat(11,23,7),bmdat(12,23,7)
     $ /'[sii]',6720,1.33,2.4,1.01,1.58,0.93,0.99,1.1,0.,0.,0.,0.,0./
       data klbm(24,7),eldat(24,7),bmdat(1,24,7),bmdat(2,24,7),
     $ bmdat(3,24,7),bmdat(4,24,7),bmdat(5,24,7),bmdat(6,24,7),
     $ bmdat(7,24,7),bmdat(8,24,7),bmdat(9,24,7),bmdat(10,24,7),
     $ bmdat(11,24,7),bmdat(12,24,7)
     $ /'[s iii]',9532,1.88,1.6,2.15,1.73,
     $               2.06,1.67,2.08,0.,0.,0.,0.,0./
       data klbm(25,7),eldat(25,7),bmdat(1,25,7),bmdat(2,25,7),
     $ bmdat(3,25,7),bmdat(4,25,7),bmdat(5,25,7),bmdat(6,25,7),
     $ bmdat(7,25,7),bmdat(8,25,7),bmdat(9,25,7),bmdat(10,25,7),
     $ bmdat(11,25,7),bmdat(12,25,7)
     $ /'[siii]',1.87e+04,0.49,0.36,0.61,0.,
     $             0.57,0.52,0.37,0.,0.,0.,0.,0./
       data klbm(26,7),eldat(26,7),bmdat(1,26,7),bmdat(2,26,7),
     $ bmdat(3,26,7),bmdat(4,26,7),bmdat(5,26,7),bmdat(6,26,7),
     $ bmdat(7,26,7),bmdat(8,26,7),bmdat(9,26,7),bmdat(10,26,7),
     $ bmdat(11,26,7),bmdat(12,26,7)
     $ /'[siv]',1.05e+04,1.05,0.86,
     $  1.24,1.23,0.82,0.94,1.22,0.,0.,0.,0.,0./
       data klbm(27,7),eldat(27,7),bmdat(1,27,7),bmdat(2,27,7),
     $ bmdat(3,27,7),bmdat(4,27,7),bmdat(5,27,7),bmdat(6,27,7),
     $ bmdat(7,27,7),bmdat(8,27,7),bmdat(9,27,7),bmdat(10,27,7),
     $ bmdat(11,27,7),bmdat(12,27,7)
     $ /'i(total) e0',0.,125,131,128,92,128,131,133,0.,0.,0.,0.,0./
       data klbm(28,7),eldat(28,7),bmdat(1,28,7),bmdat(2,28,7),
     $ bmdat(3,28,7),bmdat(4,28,7),bmdat(5,28,7),bmdat(6,28,7),
     $ bmdat(7,28,7),bmdat(8,28,7),bmdat(9,28,7),bmdat(10,28,7),
     $ bmdat(11,28,7),bmdat(12,28,7)
     $ /'t(in) e4',0.,1.7,1.71,1.7,0.,1.72,1.68,1.68,0.,0.,0.,0.,0./
       data klbm(29,7),eldat(29,7),bmdat(1,29,7),bmdat(2,29,7),
     $ bmdat(3,29,7),bmdat(4,29,7),bmdat(5,29,7),bmdat(6,29,7),
     $ bmdat(7,29,7),bmdat(8,29,7),bmdat(9,29,7),bmdat(10,29,7),
     $ bmdat(11,29,7),bmdat(12,29,7)
     $ /'t(h+) e4',0.,1.17,0.,1.24,1.12,
     $         1.06,1.2,1.23,0.,0.,0.,0.,0./
      data (elnsv(1,ml,2),elnsv(2,ml,2),nbmpt(ml,2),ml=1,13)/
     $    4858., 4868.,24,
     $    5874., 5878.,1,
     $    2325., 2330.,2,
     $    1334., 1336.,0,
     $    1906., 1910.,3,
     $    6545., 6550.,5,
     $    6580., 6590.,5,
     $    6295., 6305.,0,
     $    6360., 6370.,0,
     $    7320., 7325.,0,
     $    7330., 7335.,0,
     $    3725., 3731.,7,
     $    516800., 518500.,8/
      data (elnsv(1,ml,2),elnsv(2,ml,2),nbmpt(ml,2),ml=14,30)/
     $    882000., 883400.,9,
     $    4950., 4965.,10,
     $    5000., 5010.,10,
     $    4360., 4365.,10,
     $    258500., 259500.,0,
     $    127500., 128500.,11,
     $    154500., 157500.,12,
     $    3865., 3871.,13,
     $    3965., 3970.,13,
     $    6713., 6720.,14,
     $    6728., 6733.,14,
     $    4065., 4078.,0,
     $    180000., 197500.,15,
     $    334500., 345000.,16,
     $    9530., 9535.,17,
     $    9065., 9072.,17,
     $    104500., 105500.,18/
      data nlnsv(2)/30/
      data nlnsv(1)/32/
      data (elnsv(1,ml,1),elnsv(2,ml,1),nbmpt(ml,1),ml=1,16)/
     $    4858., 4868.,14,
     $    1214., 1217.,0,
     $    5874., 5878.,0,
     $    2325., 2330.,0,
     $    1334., 1336.,0,
     $    1906., 1910.,0,
     $    6545., 6550.,2,
     $    6580., 6590.,2,
     $    127500., 128500.,4,
     $    2053380., 2053390.,0,
     $    6295., 6305.,0,
     $    6360., 6370.,0,
     $    7320., 7325.,0,
     $    7330., 7335.,0,
     $    3725., 3731.,3,
     $    517500., 518500.,0/
      data (elnsv(1,ml,1),elnsv(2,ml,1),nbmpt(ml,1),ml=17,32)/
     $    882000., 883400.,0,
     $    4950., 4965.,0,
     $    5000., 5010.,0,
     $    4360., 4365.,0,
     $    258500., 259500.,0,
     $    127500., 128500.,0,
     $    154500., 155500.,0,
     $    3865., 3871.,0,
     $    3965., 3970.,0,
     $    6713., 6720.,5,
     $    6728., 6733.,5,
     $    3.3e+5, 3.4e+5,7,
     $    180000., 197500.,6,
     $    9530., 9535.,8,
     $    9065., 9072.,8,
     $    104500., 105500.,0/
      data nlnsv(3)/32/
      data (elnsv(1,ml,3),elnsv(2,ml,3),nbmpt(ml,3),ml=1,16)/
     $    4858., 4868.,1,
     $    1214., 1217.,0,
     $    5874., 5878.,2,
     $    2325., 2330.,3,
     $    1334., 1336.,4,
     $    1906., 1910.,5,
     $    6545., 6550.,6,
     $    6580., 6590.,6,
     $    1218025., 1218028.,12,
     $    2053380., 2053390.,0,
     $    6295., 6305.,0,
     $    6360., 6370.,0,
     $    7320., 7325.,8,
     $    7330., 7335.,8,
     $    3725., 3731.,9,
     $    517500., 518500.,10/
      data (elnsv(1,ml,3),elnsv(2,ml,3),nbmpt(ml,3),ml=17,32)/
     $    882000., 883400.,0,
     $    4950., 4965.,11,
     $    5000., 5010.,11,
     $    4360., 4365.,11,
     $    258500., 259500.,0,
     $    127500., 128500.,12,
     $    154500., 155500.,13,
     $    3865., 3871.,14,
     $    3965., 3970.,14,
     $    6713., 6720.,0,
     $    6728., 6733.,0,
     $    4065., 4078.,0,
     $    180000., 197500.,15,
     $    9530., 9535.,16,
     $    9065., 9072.,16,
     $    104500., 105500.,17/
      data nlnsv(4)/49/
      data (elnsv(1,ml,4),elnsv(2,ml,4),nbmpt(ml,4),ml=1,15)/
     $   4858., 4868.,1,
     $   6560., 6564., 0,
     $   1214., 1217., 0,
     $   5874., 5878., 2,
     $   4680., 4690., 3,
     $   2325., 2330., 4,
     $   1906., 1910., 5,
     $   1545., 1555., 6,
     $   5195., 5205., 0,
     $   6545., 6550., 7,
     $   6580., 6590., 7,
     $   1745., 1755., 8,
     $   5.7e+05, 5.71E+05, 9,
     $   1485., 1490., 10,
     $   1235., 1245.,11/
      data (elnsv(1,ml,4),elnsv(2,ml,4),nbmpt(ml,4),ml=16,32)/
     $   6295., 6305., 12,
     $   3725., 3731., 13,
     $   4950., 4965., 14,
     $   5000., 5010., 14,
     $   5.165E+05, 5.175E+05,16, 
     $   4360., 4365., 15,
     $   258500., 259500., 17,
     $   1400., 1408., 18,
     $   1217., 1219., 19,
     $   1213., 1214.5, 19,
     $   127500., 128500.,0, 
     $   154500., 156500., 20,
     $   3865., 3871., 21,
     $   3965., 3970., 21,
     $   2420., 2428., 22,
     $   241500., 242500.,0, 
     $   3420., 3430., 23/
      data (elnsv(1,ml,4),elnsv(2,ml,4),nbmpt(ml,4),ml=33,49)/
     $   3340., 3350., 0,
     $   2795., 2800., 25,
     $   44950., 45500., 26,
     $   55950., 56050., 0,
     $   347950., 348150., 27,
     $   2328., 2352., 28,
     $   1880., 1885., 29,
     $   1890., 1896., 29,
     $   1392., 1395., 30,
     $   1402., 1404., 30,
     $   6713., 6720., 31,
     $   6728., 6733., 31,
     $   4065., 4078., 0,
     $   186950., 187050.,32, 
     $   9530., 9535., 33,
     $   9065., 9072., 33,
     $   104500., 105500.,34/
      data nlnsv(5)/49/
      data (elnsv(1,ml,5),elnsv(2,ml,5),nbmpt(ml,5),ml=1,15)/
     $   4858., 4868.,1,
     $   6560., 6564., 0,
     $   1214., 1217., 0,
     $   5874., 5878., 2,
     $   4680., 4690., 3,
     $   2325., 2330., 0,
     $   1906., 1910., 4,
     $   1545., 1555., 5,
     $   5195., 5205., 0,
     $   6545., 6550., 6,
     $   6580., 6590., 6,
     $   1745., 1755., 0,
     $   5.7e+05, 5.71E+05, 7,
     $   1485., 1490., 0,
     $   1235., 1245.,0/
      data (elnsv(1,ml,5),elnsv(2,ml,5),nbmpt(ml,5),ml=16,32)/
     $   6295., 6305., 0,
     $   3725., 3731., 8,
     $   4950., 4965., 9,
     $   5000., 5010., 9,
     $   5.165E+05, 5.175E+05,10, 
     $   4360., 4365., 0,
     $   258500., 259500., 11,
     $   1400., 1408., 0,
     $   1217., 1219., 0,
     $   1213., 1214.5, 0,
     $   127500., 128500.,0, 
     $   154500., 156500., 12,
     $   3865., 3871., 13,
     $   3965., 3970., 13,
     $   2420., 2428., 0,
     $   241500., 242500.,0, 
     $   3420., 3430., 0/
      data (elnsv(1,ml,5),elnsv(2,ml,5),nbmpt(ml,5),ml=33,49)/
     $   3340., 3350., 0,
     $   2795., 2800., 14,
     $   44950., 45500., 0,
     $   55950., 56050., 0,
     $   347950., 348150., 0,
     $   2328., 2352., 0,
     $   1880., 1885., 15,
     $   1890., 1896., 15,
     $   1392., 1395., 0,
     $   1402., 1404., 0,
     $   6713., 6720., 0,
     $   6728., 6733., 0,
     $   4065., 4078., 0,
     $   186950., 187050.,0, 
     $   9530., 9535., 16,
     $   9065., 9072., 16,
     $   104500., 105500.,17/
      data nlnsv(6)/49/
      data (elnsv(1,ml,6),elnsv(2,ml,6),nbmpt(ml,6),ml=1,15)/
     $   4858., 4868.,1,
     $   6560., 6564., 0,
     $   1214., 1217., 0,
     $   5874., 5878., 2,
     $   4680., 4690., 3,
     $   2325., 2330., 0,
     $   1906., 1910., 4,
     $   1545., 1555., 5,
     $   5195., 5205., 0,
     $   6545., 6550., 0,
     $   6580., 6590., 0,
     $   1745., 1755., 0,
     $   5.7e+05, 5.71E+05, 0,
     $   1485., 1490., 0,
     $   1235., 1245.,0/
      data (elnsv(1,ml,6),elnsv(2,ml,6),nbmpt(ml,6),ml=16,32)/
     $   6295., 6305., 0,
     $   3725., 3731., 0,
     $   4950., 4965., 6,
     $   5000., 5010., 6,
     $   5.165E+05, 5.175E+05,7, 
     $   4360., 4365., 0,
     $   258500., 259500., 8,
     $   1400., 1408., 0,
     $   1217., 1219., 0,
     $   1213., 1214.5, 0,
     $   127500., 128500.,0, 
     $   154500., 156500., 9,
     $   3865., 3871., 10,
     $   3965., 3970., 10,
     $   2420., 2428., 11,
     $   241500., 242500.,0, 
     $   3420., 3430., 0/
      data (elnsv(1,ml,6),elnsv(2,ml,6),nbmpt(ml,6),ml=33,49)/
     $   3340., 3350., 0,
     $   2795., 2800., 0,
     $   44950., 45500., 0,
     $   55950., 56050., 0,
     $   347950., 348150., 0,
     $   2328., 2352., 0,
     $   1880., 1885., 0,
     $   1890., 1896., 0,
     $   1392., 1395., 0,
     $   1402., 1404., 0,
     $   6713., 6720., 0,
     $   6728., 6733., 0,
     $   4065., 4078., 0,
     $   186950., 187050.,0, 
     $   9530., 9535., 12,
     $   9065., 9072., 12,
     $   104500., 105500.,13/
      data nlnsv(7)/29/
      data (elnsv(1,ml,7),elnsv(2,ml,7),nbmpt(ml,7),ml=1,14)/
     $    4858., 4868., 1,
     $    1214., 1217., 2,
     $    5874., 5878., 3,
     $    4680., 4690., 4,
     $    1635., 1645., 5,
     $    1906., 1910., 6,
     $    1545., 1555., 7,
     $    5195., 5205., 0,
     $    6545., 6550., 8,
     $    6580., 6590., 8,
     $    1745., 1755., 9,
     $    1485., 1490., 10,
     $    6295., 6305., 11,
     $    630000., 635000., 12/
      data (elnsv(1,ml,7),elnsv(2,ml,7),nbmpt(ml,7),ml=15,29)/
     $    3725., 3731., 13,
     $    4950., 4965., 14,
     $    5000., 5010., 14,
     $    4360., 4365., 15,
     $   1400., 1408., 16,
     $   154500., 155500., 17,
     $    3865., 3871., 18,
     $   2420., 2428., 19,
     $   3420., 3430., 20,
     $    2795., 2800., 21,
     $    6713., 6720., 22,
     $    6728., 6733., 22,
     $    9530., 9535., 23,
     $   186950., 187050.,24, 
     $    104500., 105500., 25/
c
      scfac=1.
      crtt=0.004
      ergsev=1.602197e-12
c
      nbmk=jj
      nbb=nbmk
      lprii=0
      write (lun11,*)'benchmark number ',nbb
      nlnprnt=nlnsv2(nbb)
      nlnprs=1
      if (nbb.le.2)  nlnprnt=nlnsv2(nbb)+1
      if (nbb.le.2) nlnprs=nlnprnt
      nlnprnt2=nlnsv(nbb)
      r19=r*1.e-19
c
c     first get the current version
      do 820 ll1=1,nlnprnt2
         ll2=ll1
         elnprnt(ll1)=elnsv(1,ll1,nbb)
         elnprnt(ll1+1)=elnsv(2,ll1,nbb)
         if ((elnprnt(ll1).le.1.e-24).or.(elnprnt(ll1+1).le.1.e-24))
     $           go to 820
         ecen=(elnprnt(ll1)+elnprnt(ll1+1))/2.
         ediff=elnprnt(ll1+1)-elnprnt(ll1)
         eww=0.
         ebar=0.
         asym=0.                            
         fluxbs=0.
         fluxbf=0.
         flxmx=0.
         lllsv(ll2)=1
         if (lprii.ne.0) write (lun11,*)'ll1=',ll1,
     $     elnprnt(ll1),elnprnt(ll1+1)
         do 821 lll=1,nlsvn
            j=lll
            ml=nplin(j)
            call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
            elin=rdat(1)
            nilin=npar(ml)
            ergsev=1.602197e-12
            ener=ergsev*(12398.54)/amax1(elin,1.e-24)
            etst=ener/ergsev
c           if ((etst.lt.elimdb(1)).or.(etst.gt.elimdb(2))) go to 147
            ml=nilin
            call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
            do ktt=1,nkdt
              write (kinam1(ktt:ktt),'(a1)')kdat(ktt)
              enddo
            elmtp=elum(1,j)
            elmtpb=elum(2,j)
            if (elin.le.1.e-8) go to 821 
            if ((elin.gt.elnprnt(ll1+1)).or.
     $          (elin.lt.elnprnt(ll1))) go to 821
            ener=12398.54/elin
            tmpflxf=elmtp
     $          *(1.e+19/r)*(1.e+19/r)/12.56       
            fluxbf=fluxbf+tmpflxf
            tmpflux=(elmtp+elmtpb)
     $          *(1.e+19/r)*(1.e+19/r)/12.56       
            if (tmpflux.lt.flxmx) go to 3083
              flxmx=tmpflux
              lllsv(ll2)=lll
              kinamsv(ll2)=kinam1
              elinsv(ll2)=elin
3083          continue
            if (lprii.ne.0) then
              write (lun11,9924)j,elin,kinam1,elmtp,elmtpb,tau0(1,j),
     $              tau0(2,j),tmpflux
              write (lun11,*)'ll2,llk,lll=',ll2,ll1,lll
             endif
9924     format (1h ,i8,e12.4,1x,a8,5e12.4)
            fluxbs=fluxbs+tmpflux
            if (ll1.eq.1) scfac=tmpflux
            fluxrl(ll2)=tmpflux/(scfac+1.e-18)
            fluxrl(ll2)=min(fluxrl(ll2),1.e+6)        
c            frac=(elmtp+elmtpb)/(etotc+1.e-18)
            asym=elmtp/amax1(1.e-24,elmtp+elmtpb)
            nbltp=nbinc(ener,epi)
            ewtmp=0.
            if ((nbltp.gt.0).and.(nbltp.lt.ncn))
     $       ewtmp=(elmtp+elmtpb)/
     $         amax1(1.e-24,zrems(1,nbltp)+zrems(2,nbltp))
            eww=eww+ewtmp
            ebar=ebar+tmpflux*elin
821         continue
         if (ll1.eq.1) scfac=fluxbs
         fluxrl(ll2)=fluxbs/(scfac+1.e-18)                                   
         fluxrl(ll2)=min(fluxrl(ll2),1.e+6)   
         if (lprii.ne.0)
     $     write (lun11,*)'ll2,fluxrl:',ll2,fluxrl(ll2)
         asym=fluxbf/amax1(1.e-24,fluxbs)
         ebar=ebar/amax1(fluxbs,1.e-24)     
         eltmp=fluxbs*12.56*r19*r19 
820      continue                                                      
c
c     put them together and print out
c     step through the benchmark lines
      do 1039 ll1=1,nlnprnt
         ll2=ll1
         fffrl=0.
c        step through the printed lines
         do 1040 ll3=1,nlnprnt2
            if (nbmpt(ll3,nbb).ne.ll2) go to 1040
            fffrl=fluxrl(ll3)+fffrl
            ll3sv=ll3
 1040       continue
c         if (fffrl.le.1.e-24) go to 1039
         fdd=1.
         if (nbb.eq.2) fdd=scfac*12.56*r19*r19*10.
         if (nbb.eq.1) fdd=scfac*12.56*r19*r19*100.
         if (nbb.eq.4) fdd=scfac*12.56*r19*r19*1000.
         if (nbb.eq.5) fdd=scfac*12.56*r19*r19*10000.
         if (nbb.eq.6) fdd=scfac*12.56*r19*r19*10000.
         if (nbb.eq.7) fdd=scfac
         if (nbb.eq.3) fdd=scfac
         if (ll1.eq.nlnprs) fffrl=fffrl*fdd
         bmdat(6,ll2,nbb)=fffrl
 1039    continue
c
c     set up averages over the lexington results
      nlnprnt=nlnsv(nbb)
      do 1013 ll1=1,nlnprnt
         ll2=ll1
         fbar=0.
         nbar=0
         if (lprii.eq.1) write (lun11,*)'line number ',ll1,ll2
         do 1011 ml1=3,nbmdat(nbb)
c            if (bmdat(ml1,ll2,nbb).le.1.e-24) go to 1011
            if (ml1.eq.6) go to 1011
            fbar=fbar+bmdat(ml1,ll2,nbb)
            nbar=nbar+1
            if (lprii.eq.1) 
     $        write (lun11,*)'table data',ml1,bmdat(ml1,ll2,nbb),fbar
 1011       continue
         fbar=fbar/float(max0(1,nbar))
         if (nbar.le.0) fbar=0.
         fbsv(ll2)=fbar
         sigsum=0.
         nbar=0
         do 1014 ml1=3,nbmdat(nbb)
c            if (bmdat(ml1,ll2,nbb).le.1.e-24) go to 1014
            if (ml1.eq.6) go to 1014
            sigsum=sigsum+(bmdat(ml1,ll2,nbb)-fbar)**2
            nbar=nbar+1
            if (lprii.eq.1) 
     $        write (lun11,*)'table disp',ml1,bmdat(ml1,ll2,nbb),
     $        fbar,sigsum
 1014       continue
         sig(ll2)=sqrt(sigsum/float(max0(nbar,1)))
         if (nbar.le.0) sig(ll2)=0.
         if (lprii.eq.1) write (lun11,*)'average and disp:',
     $               ll2,fbsv(ll2),sig(ll2)
 1013    continue
c
c
c
      nlnprnt=nlnsv2(nbb)
      nlnprs=1
      nbmtmp=0
      if (nbb.le.2)  nlnprnt=nlnsv2(nbb)+1
      if (nbb.le.2) nlnprs=nlnprnt
      nlnprnt2=nlnsv(nbb)
      if (lprii.ne.0)
     $  write (lun11,*)'nlnprnt,nlnprnt2:',nlnprnt,nlnprnt2
c     step through the benchmark lines
      do 1019 ll1=1,nlnprnt
         ll2=ll1
         fffrl=0.
c        step through the printed lines
         do 1020 ll3=1,nlnprnt2
            if (nbmpt(ll3,nbb).ne.ll2) go to 1020
            fffrl=fluxrl(ll3)+fffrl
            if (lprii.ne.0)
     $       write (lun11,*)'stepping through constituents:',
     $       ll3,lllsv(ll3),elinsv(ll3),kinamsv(ll3),
     $              fluxrl(ll3)
            ll3sv=ll3
 1020       continue
         if (lprii.ne.0)
     $    write (lun11,*)'fffrl=',ll1,fffrl,ll3sv
         if (fffrl.le.1.e-24) go to 1019
         fdd=1.
         if (nbb.eq.2) fdd=scfac*12.56*r19*r19*10.
         if (nbb.eq.1) fdd=scfac*12.56*r19*r19*100.
         if (nbb.eq.4) fdd=scfac*12.56*r19*r19*1000.
         if (nbb.eq.5) fdd=scfac*12.56*r19*r19*10000.
         if (nbb.eq.6) fdd=scfac*12.56*r19*r19*10000.
         if (nbb.eq.7) fdd=scfac
         if (nbb.eq.3) fdd=scfac
         if (ll1.eq.nlnprs) fffrl=fffrl*fdd
         nbmtmp=nbmdat(nbb)+1
         do 1012 ml1=3,nbmtmp
c            if (ml1.eq.6) go to 1012
            if (ml1.eq.nbmtmp) go to 1015
            if (bmdat(ml1,ll2,nbb).le.1.e-24) go to 1012
            err(ml1)=(bmdat(ml1,ll2,nbb)-fbsv(ll2))
     $                 /amax1(1.e-24,sig(ll2))
            if (lprii.ne.0)
     $       write (lun11,*)'ml1),bmdat,err:',
     $          ml1,bmdat(ml1,ll2,nbb),fbsv(ll2),sig(ll2),err(ml1)
            go to 1016
 1015       continue
            err(ml1)=(fffrl-fbsv(ll2))
     $                 /amax1(1.e-24,sig(ll2))
            if (lprii.ne.0)
     $       write (lun11,*)'ml1,bmdat,err:',
     $          ml1,fffrl,fbsv(ll2),sig(ll2),err(ml1)
 1016       continue
            sigsm(ml1)=sigsm(ml1)+err(ml1)*err(ml1)
            err(ml1)=min(amax1(err(ml1),-9.99),9.99)
            nsm(ml1)=nsm(ml1)+1
 1012       continue
          write (lun11,9901) ll1,klbm(ll1,nbb),eldat(ll1,nbb),
     $    err(nbmtmp),fbsv(ll2),sig(ll2),fffrl
 9901     format (1h ,i4,1x,a8,f10.2,
     $      f5.2,1x,3(1pe10.2))
 1019    continue
      write (lun11,*)(sigsm(mm),mm=1,nbmtmp)
c
      return
      end
      subroutine bremem(lpri,lun11,xee,xpx,t,epi,brcems)
c
c     this routine computes emissivities due to thermal bremsstrahlung.
c
      parameter (ncn=9999)
c
      dimension epi(ncn),brcems(ncn)
c
c      data cc/8.223e-15/
      data cc/1.032e-13/
c
      lskp=1
c
      ekt = t*(0.861707)
      t6 = t/100.
c
      lprisv=lpri
      if (lpri.gt.0) write (lun11,*)'in bremem',t
c
      numcon=ncn
      do 100 kl = 1,numcon,lskp
         brcems(kl) = 0.
 100  continue
c
      xnx=xpx*xee
      enz2=(1.4)*xnx
      zz=1.
      do 110 kk = 1,numcon,lskp
                  temp = epi(kk)/ekt
                  gam = zz*zz*(0.158)/t6
                  gau = 1.
                  if ( temp.lt.100. ) gau = fbg(temp,gam)
                  brtmp = cc*xnx*enz2*gau*expo(-temp)/sqrt(t)
                  brcems(kk) = brcems(kk) + brtmp
                  if ( lpri.gt.0 ) write (lun11,99001) kk,
     &                 zz,enz2,gam,temp,gau,brtmp
 110              continue
c
      lpri=lprisv
c
      return
99001 format (' ',i4,6e12.4)
      end
c----------------------------------------------------------------
c----------------------------------------------------------------
            subroutine calt57(te,den,e,ep,n,cion,crec,lun11,lpri)
c  temp   temperature in K
c  den    electron density in cm-3
c  e      level energy in eV (first real in type 6)
c  ep     ionization potential in eV (forth real in type 6)
c  n      level's principal quatum number (first integer in type 6)
c  cion   ionization rate in s-1.cm+3
c  crec   3-body recombination rate in s-1.cm+6. THIS VALUE MUST BE
c         MUTIPLIED BY (stat. weigth level/stat. weigth recombining
c          level)
c
c
       rk=1.16058e+4
       cb=13.598*1.6021e-19/1.3805e-23
c       write (lun11,*)'entering calt57:',rno,den,rc,tmin,te,
c     $   ep,e,n
       rio=(ep-e)/13.6
       rc=sqrt(rio)*n 
       if (den.gt.1.e+18) then
        print*,'density too high for cics'
        stop
       endif
       tmin=3.8e+4*rc*sqrt(rc)
       temp=te
       if (te.lt.tmin) then
        temp=tmin
       endif
       rno=SQRT(1.8887E+8*rc/den**0.3333)
       rno2=(1.814e26*(rc**6)/2./den)**0.13333
       rno=min(rno,rno2)
       if (int(rno).gt.n) then
        call irc(n,temp,rc,rno,ciono)
c       write (lun11,*)'in calt57:',rno,den,rc,tmin,te,
c     $   ciono
c
c extrapolates to actual temperature below Tmin
c
        cion=0.
        crec=0.
        if (te.lt.tmin) then
         beta=.25*(sqrt((100.*rc+91.)/(4.*rc+3.))-5.)
         wte=(log(1.+te/cb/rio))**(beta/(1.+te/cb*rio))
         wtm=(log(1.+tmin/cb/rio))**(beta/(1.+tmin/cb*rio))
         call eint(rio/te*cb,ete,e2,e3)
         if (ete.lt.1.e-20) return
         call eint(rio/tmin*cb,etm,e2,e3)
c         write (lun11,*)'in calt57:',
c     $   n,temp,tmin,rc,rno,cion,te,ete,etm,wte,wtm,cion
         cion=ciono*sqrt(tmin/te)*ete/(etm+1.e-30)*wte/(wtm+1.e-30)
        else
         cion=ciono
        endif
c
        if (cion.le.1.e-34) return
c
c        se=log(cion)
c        sr=se-36.1136-1.5*log(te)+(13.6*
c     c                 rc*rc*(1./float(n*n)-1./rno/rno)*rk/te)
c        write (lun11,*)te,n,rno,rk,te,cion,se,sr
        cion=cion/float(n*n)
c        crec=expo(sr)/float(n*n)
        crec=cion*(2.0779e-16)*
     $    expo(13.6*rc*rc*(1./float(n*n)-1./rno/rno)*rk/te)
     $              /te**(1.5)/float(n*n)
       endif
        return
        end
c *********************************************************************
       subroutine calt60_62(temp,m,idata,dtype,itype,Upsilon)
c
c  This rutine takes the coefficients in data type 60 and 62 (reals
c  as dtype and integers as itype) and return effective collision
c  strengths according to fits by Callaway (1994).
c  "temp" is the temperature in Kelvin and "m" is the number of
c  reals in dtype. "idata" is the data type either 60 or 62.
c
c *********************************************************************

        dimension dtype(m),itype(4)
        t1=temp*6.33652e-6
        if (temp.gt.1.e9) t1=6.33652e+3
        de=1./float(itype(1))**2-1./float(itype(2))**2
        tmax=4.*de
        tmax=1.
        tt=t1
        if (t1.gt.tmax) tt=tmax
        if (idata.eq.60) then
         rat=0.
         do i=1,m-2
          rat=rat+dtype(i+2)*(tt**(i-1))
         enddo
         Upsilon=rat
        else
         rat=0.
         do i=1,m-5
          rat=rat+dtype(i+2)*(tt**(i-1))
         enddo
         Upsilon=rat+dtype(m-2)*log(dtype(m-1)*tt)*exp(-dtype(m)*tt)
        endif
c
         if (t1.gt.tt) then
          upsilon=Upsilon*(1.+log(t1/tmax)/(log(t1/tmax)+1.))
         endif
c
      return
      end
      subroutine calt66(temp,dtype66,gamma)
c
c   Takes coefficients in data type 66 and returns effective collision
c    strenghts for He-like ions according to Kato & Nakazaki (1989)
c    eq. (6).
c
       dimension dtype66(18)
       eboltz=1.160443e+04
       dele=dtype66(1)
       y=dele/temp*eboltz
c
       if (y.lt.1.e-20) then
        print*,'error in calt66. y too low. y=',y
        stop
       endif
       if (y.gt.1.e+20) then
        gamma=0.
        return
       endif
c
       if (y.gt.77.) y=77.       
       call expint(y,em1)
       a=dtype66(2)
       b=dtype66(3)
       c=dtype66(4)
       d=dtype66(5)
       e=dtype66(6)
       gam1=y*((a/y+c)+d*.5*(1.-y))+em1*(b-c*y+d*y*y*.5+e/y)
       dele=dtype66(7)
       y=dele/temp*eboltz
       if (y.gt.77.) y=77.       
       call expint(y,em1)
       a=dtype66(8)
       b=dtype66(9)
       c=dtype66(10)
       d=dtype66(11)
       e=dtype66(12)
       gam2=y*((a/y+c)+d*.5*(1.-y))+em1*(b-c*y+d*y*y*.5+e/y)
       dele=dtype66(13)
       y=dele/temp*eboltz
       if (y.gt.77.) y=77.       
       call expint(y,em1)
       a=dtype66(14)
       b=dtype66(15)
       c=dtype66(16)
       d=dtype66(17)
       e=dtype66(18)
       gam3=y*((a/y+c)+d*.5*(1.-y))+em1*(b-c*y+d*y*y*.5+e/y)
       gamma=gam1+gam2+gam3
       return
       end

       subroutine calt67(temp,dtype67,gamma)
c
c   Takes coefficients in data type 67 and returns effective collision
c    strenghts for He-like ions according to Keenan, McCann, & Kingston
c    (1987) eq. (2)
c
       dimension dtype67(3)
       tp=log10(temp)
       gamma=dtype67(1)+dtype67(2)*tp+dtype67(3)*tp*tp
       return
       end

       subroutine calt67o(temp,dtype67,gamma)
c
c   Takes coefficients in data type 67 and returns effective collision 
c    strenghts for He-like ions according to Keenan, McCann, & Kingston
c    (1987) eq. (2)
c
       dimension dtype67(3)
       gamma=dtype67(1)+dtype67(2)*temp+dtype67(3)*temp*temp
       return
       end
       subroutine calt68(temp,dtype68,itype68,gamma)
c
c   Takes coefficients in data type 68 and returns effective collision 
c    strenghts for He-like ions according to Sanpson & Zhang.
c
       dimension dtype68(3),itype68(4)
       z=float(itype68(3))
       tt=log10(temp/z/z/z)
       gamma=dtype68(1)+dtype68(2)*tt+dtype68(3)*tt*tt     
       return
       end
       subroutine calt69(temp,m,dtype69,gamma)
c
c   Takes coefficients in data type 69 and returns effective collision
c    strenghts for He-like ions according to Kato & Nakazaki (1989)
c    eq. (6). m is the dimension of dtype69
c
       dimension dtype69(m)
       eboltz=1.160443e+04
       dele=dtype69(1)
       y=dele/temp*eboltz
c
       if (y.lt.1.e-20) then
        print*,'error in calt69. y too low. y=',y
        stop
       endif
       if (y.gt.1.e+20) then
        gamma=0.
        return
       endif
c
       if (y.gt.77.) y=77.
       if (y.lt.5.e-2) y=5.e-2
       call expint(y,em1)
       a=dtype69(2)
       b=dtype69(3)
       c=dtype69(4)
       d=dtype69(5)
       e=dtype69(6)
       if (m.eq.6) then
        gamma=y*((a/y+c)+d*.5*(1.-y))+em1*(b-c*y+d*y*y*.5+e/y)
c        write(2,*)temp,y,em1,gamma
       else
        p=dtype69(7)
        q=dtype69(8)
        x1=dtype69(9)
        call expint(y*x1,em1)
        gnr=a/y+c/x1+d*.5*(1./x1/x1-y/x1)+e/y*log(x1)+
     #   em1/y/x1*(b-c*y+d*y*y*.5+e/y)
        gnr=gnr*y*exp(y*(1.-x1))
        gr=p*(1.+1./y)*(1.-exp(y*(1.-x1))*(x1+1/y)/(1.+1./y)) +
     #     q*(1.-exp(y*(1.-x1)))
        gamma=gnr+gr
       endif
       return
       end

c *********************************************************************
      subroutine calt70(temp,den,eth,ic,m,dtype70,itype70,
     1                  nx,xe,xs,rec,al,lun11,lpri)
c 
c  This rutine takes the coefficients in data type 70 (dtype70 reals
c  in itype70 integers) and returns the recombination rate (in s-1cm-3)
c  and the correstpondent phot. x-section for the superlevel. m is the 
c  dimension of dtype70. nx is the number of points in the x-section
c  xe() contains the photon energy in Ry and xx() is the x-section 
c  in Mb.
c  temp, den, and ic are the temperature, electron density 
c  and effective charge of the ion respectively.
c  eth is the threshold energy for the superlevel in Ry.
c  
c *********************************************************************

      dimension dtype70(m),itype70(9),xe(99999),xs(99999)            
c alpf: fitting coef. for hydrogenic recombination n=12,l=0
      dimension alpf(3)
      data alpf/-7.1094841E-02,-9.0274535E-02,-14.26129/

      if (lpri.gt.1)
     $  write (lun11,*)'in calt70:',temp,den,eth,ic,m,dtype70(1),
     $                  itype70(1)
      rne=log10(den)
      rte=log10(temp)
      nden=itype70(1)
      ntem=itype70(2)
      nxs=itype70(3)
      if (nden.gt.1) then
      if (rne.gt.dtype70(nden)) then
       write (lun11,*)'DENSITY TOO HIGH AT SUPREC'
       write (lun11,*)'z=',ic,' temp=',temp,' Ne=',den
       stop
      endif
      if (rne.le.dtype70(1)) then 
       in=1
      else
       in=int(rne/dtype70(nden)*nden)-1
       if (in.ge.nden) in=in-1
 5     in=in+1
       if (in.lt.nden .and. rne.ge.dtype70(in+1)) goto 5                  
       if (rne.lt.dtype70(in)) then  
        in=in-2
        goto 5
       endif
      endif
      else
       in=1
      endif
      if (rte.lt.dtype70(nden+1)) then
       it=1
      else
       dt=(dtype70(nden+ntem)-dtype70(nden+1))/float(ntem)
       it=int((rte-dtype70(nden+1))/dt)
 6     it=it+1
       if (it.ge.ntem) then
        it=ntem-1
       else
        if (rte.ge.dtype70(nden+it+1)) goto 6
        if (rte.lt.dtype70(nden+it)) then
         it=it-2
         goto 6
        endif
       endif
      endif
      kt1=nden+ntem+(in-1)*ntem+it
      rm=(dtype70(kt1+1)-dtype70(kt1))/(dtype70(nden+it+1)-
     #    dtype70(nden+it))
      rec1=dtype70(kt1)+rm*(rte-dtype70(nden+it))
      if (nden.gt.1) then
       kt1=kt1+ntem
       rm=(dtype70(kt1+1)-dtype70(kt1))/(dtype70(nden+it+1)-
     #    dtype70(nden+it))
       rec2=dtype70(kt1)+rm*(rte-dtype70(nden+it))
       rm=(rec2-rec1)/(dtype70(in+1)-dtype70(in))
       rec=rec1+rm*(rne-dtype70(in))
      else
       rec=rec1
      endif
      rec=exp10(rec)
c
      i1=ntem*nden+ntem+nden
      do i=1,nxs
       xe(i)=dtype70(i1+(i-1)*2+1)
       xs(i)=dtype70(i1+(i-1)*2+2)
      if (lpri.gt.1)
     $  write (lun11,*)i,xe(i),xs(i)
      enddo
      lprim=0
      call milne(temp,nxs,xe,xs,eth,al,lun11,lprim)
      scale=rec/(1.e-34+al)
      if (lpri.gt.1)
     $ write (lun11,*)'in calt70:',rec,al,scale,xs(1),nxs,eth
      crit=1.e-6
      imax=nxs
      do i=1,nxs
       xs(i)=xs(i)*scale
       xs(i)=min(xs(i),1.e+6)
       if (xs(i).gt.xs(1)*crit) imax=i
      if (lpri.gt.1)
     $  write (lun11,*)i,xe(i),xs(i)
      enddo
      nxs=imax
      nx=nxs
      return
      end
      
      subroutine calt71(temp,den,ic,m,dtype71,itype71,wav,aij,lun11,
     $                  lpri)    
c 
c  This rutine takes the coefficients in data type 71 (dtype71 reals
c  in itype71 integers) and returns the radiative transition prbability
c  (in s-1) from the superlevels to the spectroscopic level given by
c  itype71(3).
c  The wavelength for the transition is also given in wav
c  temp, den, and ic are the temperature, electron density 
c  and effective charge of the ion respectively.
c  
c *********************************************************************

      dimension dtype71(m),itype71(6)
      rne=log10(den)
      rte=log10(temp)
      nden=itype71(1)
      ntem=itype71(2)
      if (lpri.ne.0) write (lun11,*)'in calt71:',nden,ntem

      if (nden.eq.1 .and. ntem.eq.1) then
        if (dtype71(3).gt.30.) then
          dtmp=log10(dtype71(3))
        else
          dtmp=(dtype71(3))
        endif
       wav=dtype71(4)
       aij=10.**dtmp
c       aij=min(aij,1.e+12)
       if (lpri.ne.0) write (lun11,*)'early return',aij,wave
       return
      endif
      if (rne.gt.dtype71(nden)) then
c       print*,'DENSITY TOO HIGH AT CALT71'  
c       print*,'z=',ic,' temp=',temp,' Ne=',den,nden,dtype71(nden)
        rne=min(rne,dtype71(nden))
      endif
      if (rte.gt.(dtype71(nden+ntem)+1.)) then
       rte=dtype71(nden+ntem)+1.
      endif
      if (rte.lt.(dtype71(nden+1)-1.)) then
       rte=dtype71(nden+1)-1.
      endif
c
      wav=dtype71(nden*ntem+nden+ntem+1)
      if (rne.le.dtype71(1)) then 
       in=1
      else
       in=0
 5     in=in+1
       if (rne.ge.dtype71(in+1).and.in.lt.nden) goto 5                  
      endif
      if (rte.lt.dtype71(nden+1)) then
       it=1
      else
       it=0
 6     it=it+1
       if (it.ge.ntem) then
        it=ntem-1
       else
        if (rte.ge.dtype71(nden+it+1)) goto 6
       endif
      endif
      kt1=nden+ntem+(in-1)*ntem+it
      rm=(dtype71(kt1+1)-dtype71(kt1))/(dtype71(nden+it+1)-
     #    dtype71(nden+it))
      rec1=dtype71(kt1)+rm*(rte-dtype71(nden+it))
      kt1=kt1+ntem
      rm=(dtype71(kt1+1)-dtype71(kt1))/(dtype71(nden+it+1)-
     #    dtype71(nden+it))
      rec2=dtype71(kt1)+rm*(rte-dtype71(nden+it))
c
      rm=(rec2-rec1)/(dtype71(in+1)-dtype71(in))
      rec=rec1+rm*(rne-dtype71(in))
      aij=10.**rec
c      aij=min(aij,1.e+12)
      if (lpri.ne.0) write (lun11,*)'late return',rm,rec2,
     $       rec1,rec,aij,wav
c
      return
      end
c *********************************************************************
       subroutine calt72(temp,dtype72,rate)  
c
c   Takes coefficients in data type 72 and returns capture rates      
c   (in s^-1) for DR through satellite levels considered explicitly.
c
       dimension dtype72(3) 
       eboltzk=1.160443e+04
       dele=dtype72(2)
       s=4.141292e-22/(temp**1.5)
       rate=s*expo(-dele/temp)*dtype72(1)*dtype72(3)/2.*.5
       return
       end
       subroutine calt73(temp,dtype73,itype73,crate)
c
c   Takes coefficients in data type 73 and returns excitation rate times
c   the statistical weight of the lower level (w_i C(i,j) in s^-1
c   cm^3).
c
       dimension dtype73(7),itype73(4)
       boltzk=1.578876e+05
       const=5.46538e-11  
       z=float(itype73(3))
       y=z*z*dtype73(1)*boltzk/temp
c
c
       gam=0.
       if (dtype73(2).ge. 0.1) gam=-.2
       if (dtype73(2).gt. 0.01 .and. dtype73(2).lt. 0.1) gam=0.
       if (dtype73(2).le. 0.01) gam=0.2
       zeff=float(itype73(3))-gam
       z2s=dtype73(2)
       a=dtype73(3)
       co=dtype73(4)
       cr=dtype73(5)
       cr1=dtype73(6)
       r=dtype73(7)
       if (y.gt.40.) then
        crate=0.
        return
       endif 
        call expint(y,em1)
        e1=em1/y*exp(-y)
       if (y*a+y.le.80) then
        call eint(y*a+y,ee1,ee2,ee3)
       else
        ee1=0.
        ee2=0.
        ee3=0.
       endif
       er=0.
       er1=0.
       if (r.eq.1.) then   
        er=ee1
        er1=ee2
       endif
       if (r.eq.2.) then
        er=ee2
        er1=ee3
       endif
       if (y*a+y.le.40) then
        qij=co*exp(-y)+1.55*z2s*e1+y*exp(y*a)*(cr*er/(a+1.)**(r-1.)
     #      +cr1*er1/(a+1.)**r)
       else
        qij=co*exp(-y)+1.55*z2s*e1
       endif
       crate=qij*boltzk/temp*sqrt(temp)/zeff/zeff*const
       if (crate.lt.0.) crate=0.
       return
       end
       subroutine calt74(temp,np,xse,xss,nd,dtype74,itype74,rate,alpha,
     $                   lpri,lun11)
c
c   Takes coefficients in data type 74 and any given radiation flux
c   array given in xse(i) (energiesin eV) and xss(i) (flux values)
c   and return the the analytic integral over resonances in the
c   cross sections represented by delta functions.
c   The routine also returns the DR recombination coefficient (in
c   s-1cm-3) for the given value of temp (in Kelvins). alpha MUST
c   be mutiplied by the stadistical of the recombined state and
c   divided by that of the recombining state.
c
c   np is the number of points xse() and nd is the number of real
c   values in dtype74()
c
       dimension dtype74(nd),itype74(8)
       dimension xse(np),xss(np)
       te=temp*1.38066e-16
       ryk=4.589343e+10
       ry=13.598
       m=(nd-1)/2

c
       xt=dtype74(1)
       x=dtype74(2)
       hgh=dtype74(2+m)
       if (lpri.gt.1) write (lun11,*)'entering calt74:',xt,x,hgh,te,m
       alpha=0.
       if (x/ryk/te.lt.40.) then
        alpha=exp(-x/ryk/te)*(x+xt)*(x+xt)*hgh
       endif
       if (lpri.gt.1) write (lun11,*)'alpha0=',alpha
       do i=2,m
        x=dtype74(1+i)*ry
        hgh=dtype74(1+i+m)
        x=x/ry
        if (x/ryk/te.lt.40.) then
         alpha=alpha+exp(-x/ryk/te)*(x+xt)*(x+xt)*hgh
        if (lpri.gt.1)
     $   write (lun11,*)'m,alpha:',m,x,hgh,x/ry/te,alpha
        endif
       enddo
        factor=213.9577e-9
        alpha=alpha*factor/(te**1.5)/ryk/ryk
       if (lpri.gt.1) write (lun11,*)'alpha:',alpha,factor
c
       if (xse(np).lt.(x+xt)*ry) then
        rate=0.e0
        return
       endif
c
       x=(dtype74(2)+dtype74(1))*ry
       i=np/2
  5    if (xse(i).ge.x) then
        i=i-1
        goto 5
       endif
       i=i-1
  10   i=i+1
       if(xse(i).lt.x.and.xse(i+1).ge.x)then
        ipos=i
       else
        goto 10
       endif
c
       rm=(xss(ipos+1)-xss(ipos))/(xse(ipos+1)-xse(ipos))
       xsec=xss(ipos)+rm*(x-xse(ipos))
       rate=xsec*dtype74(2+m)
       do 50 i=2,m
        x=(dtype74(1+i)+dtype74(1))*ry
        hgh=dtype74(1+i+m)
        ip=ipos
 20     if (xse(ip).lt.x) then
         ip=ip+1
         goto 20
        endif
        ip=ip-2
 30     ip=ip+1
        if (ip.gt.np) goto 50
        ipos=ip
        rm=(xss(ipos+1)-xss(ipos))/(xse(ipos+1)-xse(ipos))
        xsec=xss(ipos)+rm*(x-xse(ipos))
        rate=rate+xsec*hgh
 50     continue
c
        rate=rate*4.752e-22
c       if (alpha.lt.1.e-37) rate=0.
       return
       end
      
c *********************************************************************
      subroutine calt77(lpri,lun11,temp,den,ic,m,
     $                       dtype77,itype77,cul,clu)
c 
c  This rutine takes the coefficients in data type 77 (dtype77 reals
c  and itype77 integers) and returns the collisional transition rates 
c  (in s-1) from the superlevel (cul) and to the superlevel (clu) from
c  the spectroscopic level given by itype77(3).
c  The wavelength for the transition is also given in wav
c  temp, den, and ic are the temperature, electron density 
c  and effective charge of the ion respectively.
c  
c *********************************************************************

      dimension dtype77(m),itype77(6)
      rne=log10(den)
      rte=log10(temp)
      nden=itype77(1)
      ntem=itype77(2)
      if (rne.gt.dtype77(nden)) then
c       print*,'DENSITY TOO HIGH AT CALT77'  
c       print*,'z=',ic,' temp=',temp,' Ne=',den,nden,dtype77(nden)
       rne=dtype77(nden)
      endif
      if (rte.gt.(dtype77(nden+ntem)+1.)) then
c       print*,'TEMPERATURE TOO HIGH AT CALT77'   
c       print*,'z=',ic,' temp=',temp,' Ne=',den
         rte=(dtype77(nden+ntem)+1.)
      endif
      if (rte.lt.(dtype77(nden+1)-1.)) then
       rte=dtype77(nden+1)-1.
      endif
c
      wav=dtype77(nden*ntem+nden+ntem+1)
      if (rne.le.dtype77(1)) then
       in=1
      else
       in=0
 5     in=in+1
       if (rne.ge.dtype77(in+1).and.in.lt.nden) goto 5
      endif
      if (rte.lt.dtype77(nden+1)) then
       it=1
      else
       it=0
 6     it=it+1
       if (it.ge.ntem) then
        it=ntem-1
       else
        if (rte.ge.dtype77(nden+it+1)) goto 6
       endif
      endif
c
      kt1=nden+ntem+(in-1)*ntem+it
      div=dtype77(nden+it+1)-dtype77(nden+it)
      rm=(dtype77(kt1+1)-dtype77(kt1))/(div+1.e-36)
      rec1=dtype77(kt1)+rm*(rte-dtype77(nden+it))
      kt1=kt1+ntem
      rm=(dtype77(kt1+1)-dtype77(kt1))/(dtype77(nden+it+1)-
     #    dtype77(nden+it)+1.e-36)
      rec2=dtype77(kt1)+rm*(rte-dtype77(nden+it))
c
      rm=(rec2-rec1)/(dtype77(in+1)-dtype77(in)+1.e-36)
      rec=rec1+rm*(rne-dtype77(in))
      if (lpri.ne.0) write (lun11,*)'in calt77:',
     $ temp,den,nden,ntem,rte,rne,wav,in,it,div,rm,
     $ rec1,rec2,rec
c      cul=10.**rec
      cul=exp10(rec)
c
      nl=itype77(3)
      k=0
  7   k=k+1 
      nl1=k*(k-1)/2+1
      nl2=(k+1)*k/2+1
      if (nl.ge.nl2) goto 7
      il=nl-nl1
      gg=float(2*il+1)*2.
      xt=1.43817e+8/wav/temp
      if (xt.lt.100) then
       clu=cul*expo(-xt)/gg
      else
       clu=0.e0
      endif
c
      return
      end
       subroutine calt80(te,ic,m,d80,epot,rion,r3b)
c
c  Takes coefficients in data type 80 and returns collisional 
c  ionization from ground state and 3-body recombination.
c  This is used for iron and nickel ions only.
c   te: temp. in K
c   ic: ion numbers (for example ic =17 for FeXVII)
c   m:  dimension of d80 array
c   d80: type 80 coefficients
c   epot: ionization energy in eV
c   rion: ionization rate in s^{-1}cm^3
c   r3b:  3-body recombination rate in s^{-1}cm^6. IT MUST BE
c         MULTIPLIED BY (stat. weight of level /Stat. weight of
c                          continuum). 

       dimension d80(12),cion(9)
       rc=2.07e-16
       do i=1,m-3
        cion(i)=d80(i+3)
       enddo
       tmin=d80(2)
       tmax=d80(3)
       scale=d80(1)
       ri=ratri(te,ic,m-3,cion,tmin,tmax,epot)
       ri=ri*scale 
       rc=ri*rc/sqrt(te**3)*exp(epot*1.1606e4/te)
       rion=ri
       r3b=rc
       return


       end 
      subroutine chisq4(a,n,np,bn,b,err,lun11,lprii)
c
      parameter (ndl=2400,nd=ndl+1)
c
      real a(np,np),b(np)
      real bn(np)
c
c        check the solution
         lpri=lprii
c         if (lprii.ge.1) lpri=3
         if (lpri.gt.2) write (lun11,*)'checking the solution'
         err=0.
         do  ll2=1,n
          sum=0.
          tmpmx=-1.e+36
          do  mm=1,n
            btmp=bn(mm)
            tmp=a(ll2,mm)*max(0.,btmp)
            tmpmx=max(tmpmx,abs(tmp))
            sum=sum+tmp
            if (lpri.gt.2) write (lun11,*)'   ',mm,btmp,
     $                            a(ll2,mm),tmp,sum,err
            enddo
          sum=sum-b(ll2)
          tmpmx=max(tmpmx,1.e-37)
          tmpp=sum/tmpmx
          if (abs(tmpp).lt.1.e+16)
     $     err=err+tmpp*tmpp
          if (lpri.gt.2) write (lun11,9246)ll2,bn(ll2),b(ll2),
     $             tmpmx,sum,err
 9246     format (1h ,i4,5e12.4)
          enddo
c
         if (lpri.gt.2)
     $    write (lun11,*)'leaving chisq4',err
c
      return
      end
      subroutine chisq(a,n,np,bn,b,err,lun11,lpri)
c
      parameter (ndl=2400,nd=ndl+1)
c
      real*8 a(np,np),b(np)
      real*8 bn(np)
c
c        check the solution
         if (lpri.gt.2) write (lun11,*)'checking the solution'
         err=0.
         do  ll2=1,n
          sum=0.
          tmpmx=-1.e+36
          do  mm=1,n
            btmp=bn(mm)
            tmp=a(ll2,mm)*max(0.,btmp)
            tmpmx=max(tmpmx,tmp)
            sum=sum+tmp
            if (lpri.gt.2) write (lun11,*)'   ',mm,btmp,
     $                            a(ll2,mm),tmp,sum
            enddo
          sum=sum-b(ll2)
          err=err+sum*sum
          if (lpri.gt.2) write (lun11,9246)ll2,bn(ll2),b(ll2),
     $             tmpmx,sum,err
 9246     format (1h ,i4,4e12.4,2i4)
          enddo
c
         if (lpri.gt.2)
     $    write (lun11,*)'leaving leqt'
c
      return
      end
      function cmpfnc(decomp,ecomp,sxcomp,ee,sxx,lun11,lpri)
c
      parameter (ncomp=101)
c
      dimension decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
c
      eetp=ee
      sxtp=sxx
      nc2=ncomp
      if (eetp.gt.1.e-4) then
        call hunt(ecomp,nc2,eetp,mm,0,lun11)
        call hunt(sxcomp,nc2,sxtp,ll,0,lun11)
c       if ((mm.gt.1).and.(ll.gt.1)) then
               mm=max(2,min(ncomp,mm))
               ll=max(2,min(ncomp,ll))
               mmm1 = mm - 1
               llm1 = ll - 1
               ddedsx = (decomp(ll,mm)-decomp(llm1,mm)
     $                  +decomp(ll,mmm1)-decomp(llm1,mmm1))
     $                     /(2.*(sxcomp(ll)-sxcomp(llm1)))
               ddede = (decomp(ll,mm)-decomp(ll,mmm1)
     $                  +decomp(llm1,mm)-decomp(llm1,mmm1))
     &                 /(2.*(ecomp(mm)-ecomp(mmm1)))
               dele = ee - ecomp(mmm1)
               delsx = sxx - sxcomp(llm1)
               cmpfnc = ddedsx*delsx + ddede*dele + decomp(llm1,mmm1)
        else
               cmpfnc = 4.*sxx - ee
        endif
c      if (lpri.ne.0)
c     $ write (lun11,*)'in cmpfnc',ee,sxx,ll,mm,cmpfnc
c
      return
      end
      subroutine cndnse(am,bv,nd,nl4,ikey,amo,bvo,nl3,ndir,lpric,
     $                  lun11,crit,crit2)
c
c     this routine condenses the rate matrices, taking out 
c       rows and columns whose coefficients are all small
c
c
      dimension am(nd,nd),bv(nd),amo(nd,nd),bvo(nd),ikey(nd)
c
c      data crit/1.e-34/,crit2/1.e-30/
c
      lpri=lpric
c      lpri=0
      if (lpri.gt.1) write (lun11,*)'in cndnse:',ndir,nd,nl4,nl3
c
      if (ndir.lt.0) go to 1009
c
c     find maximum element
      tstmx=0
      klmx=0
      jkmx=0
      do 1 jk=1,nl3
        bvo(jk)=bv(jk)
        do 2 kl=1,nl3
           amo(jk,kl)=am(jk,kl)
           tst=abs(am(jk,kl))
           if (tst.lt.tstmx) go to 2
              tstmx=tst
              klmx=kl
              jkmx=jk
 2         continue
 1      continue
      if (lpri.ge.2)
     $ write (lun11,*)'maximum element',jkmx,klmx,am(jkmx,klmx),tstmx
c
c     step through rows and find which are negligible
      ikey(1)=1
      ikey(2)=1
      do 3 jk=3,nl3
        ikey(jk)=1
        if (lpri.ge.2)
     $   write (lun11,*)'testing row',jk
        kl=0
 4         kl=kl+1
           tst=abs(amo(jk,kl))
           if (lpri.ge.2)
     $      write (lun11,*)kl,amo(jk,kl),tst
           if ((tst.lt.tstmx*crit).and.(kl.lt.nl3)) go to 4
        if (lpri.ge.2)
     $   write (lun11,*)'finishing test',kl,nl3,tst
       if (kl.lt.nl3) go to 3
       ikey(jk)=0
 3     continue
      do 33 jk=3,nl3
        if (ikey(jk).ne.0) then
          if (lpri.ge.2)
     $     write (lun11,*)'testing row',jk
          tst=0.
          do kl=1,nl3
            if (kl.ne.jk) then
              tst=max(tst,abs(amo(jk,kl)/(amo(jk,jk)+1.e-30)))
              if (lpri.ge.2)
     $        write (lun11,*)kl,amo(jk,kl),tst
              endif
            enddo
          if (lpri.ge.2)
     $    write (lun11,*)'finishing test',nl3,tst
         tst2=abs(bvo(jk)/(amo(jk,jk)+1.e-30))
         if ((tst.gt.crit2).or.(tst2.gt.crit)) go to 33
         ikey(jk)=0
         endif
 33    continue
      if (lpri.ge.2) write (lun11,*)'key vector'
      if (lpri.ge.2) write (lun11,91)
     $  (ikey(m3),m3=1,nl3)
 91   format (1x,40i2)
c
c      do mm=1,nl3
c        ikey(mm)=1
c        enddo
c
c
c     step through and omit levels from new matrix
      jkk=0
      do 5 jk=1,nl3
        if (ikey(jk).eq.0) go to 5
        jkk=jkk+1
        bv(jkk)=bvo(jk)
        kll=0
        do 6 kl=1,nl3
           if (ikey(kl).eq.0) go to 6
           kll=kll+1
           am(jkk,kll)=amo(jk,kl)
 6         continue
 5      continue
      nl4=jkk
c      write (lun11,*)'in cndnse:',nl3,nl4
c
      if (lpri.le.1) return
      nlp=min0(nl3,9)
      write (lun11,*)'old matrix',nl3,nlp
      do ll=1,nl3
        write (lun11,*)'b(i)=',bvo(ll)
        do ml=1,nl3
          if (abs(amo(ll,ml)).gt.1.e-34)
     $     write (lun11,9902)ll,ml,amo(ll,ml)
 9902     format (1h ,2i4,9e9.1)
          enddo
        enddo
      nlp=min0(9,nl4)
      write (lun11,*)'new matrix',nl4,nlp
      do ll=1,nl4
        write (lun11,*)'b(i)=',bv(ll)
        do ml=1,nl4
          if (abs(am(ll,ml)).gt.1.e-34)
     $     write (lun11,9902)ll,ml,(am(ll,ml))
          enddo
        enddo
c
      return
c
 1009 continue
c
c     step through and put back right hand sides
      jkk=0
      do 7 jk=1,nl3
        bvo(jk)=0.
        if (ikey(jk).eq.0) go to 7
        jkk=jkk+1
        bvo(jk)=bv(jkk)
 7    continue
c
c
      return
      end
      subroutine cndnsen(am,bv,nd,nl4,ikey,amo,bvo,nl3,ndir,lpric,
     $                  lun11,crit,crit2)
c
c     this routine condenses the rate matrices, taking out 
c       rows and columns whose coefficients are all small
c
c
      dimension am(nd,nd),bv(nd),amo(nd,nd),bvo(nd),ikey(nd)
c
c
      lpri=lpric
c      lpri=0
      if (lpri.ne.0) lpri=2
      if (lpri.gt.1) write (lun11,*)'in cndnse:',ndir,nd,nl4,nl3
c
      if (ndir.lt.0) go to 1009
c
c     find maximum element
      tstmx=0
      klmx=0
      jkmx=0
      do jk=1,nl3
        bvo(jk)=bv(jk)
        do kl=1,nl3
           amo(jk,kl)=am(jk,kl)
           tstmx=max(tstmx,abs(am(jk,kl)))
           enddo
         enddo
      if (lpri.ge.1)
     $ write (lun11,*)'maximum element',jkmx,klmx,am(jkmx,klmx),tstmx
c
c     step through rows and find which are negligible
      do 3 jk=1,nl3
        ikey(jk)=1
        if (lpri.ge.2)
     $   write (lun11,*)'testing row',jk
        tstmx2=0.
        do kl=1,nl3
           tst=abs(amo(kl,jk))
           tstmx2=max(tst,tstmx2)
           enddo
        if (tstmx2.lt.tstmx*crit) ikey(jk)=0
        if (lpri.ge.1)
     $   write (lun11,*)'testing row',jk,tstmx2,ikey(jk)
 3     continue
      if (lpri.ge.2) write (lun11,*)'key vector'
      if (lpri.ge.2) write (lun11,91)
     $  (ikey(m3),m3=1,nl3)
 91   format (1x,40i2)
c
c
c     step through and omit levels from new matrix
      jkk=0
      do 5 jk=1,nl3
        if (ikey(jk).eq.0) go to 5
        jkk=jkk+1
        bv(jkk)=bvo(jk)
        kll=0
        do 6 kl=1,nl3
           if (ikey(kl).eq.0) go to 6
           kll=kll+1
           am(jkk,kll)=amo(jk,kl)
 6         continue
 5      continue
      nl4=jkk
c      write (lun11,*)'in cndnse:',nl3,nl4
c
      if (lpri.le.1) return
      nlp=min0(nl3,9)
      write (lun11,*)'old matrix',nl3,nlp
      do ll=1,nl3
        write (lun11,*)'b(i)=',bvo(ll)
        do ml=1,nl3
          if (abs(amo(ll,ml)).gt.1.e-34)
     $     write (lun11,9902)ll,ml,amo(ll,ml)
 9902     format (1h ,2i4,9e9.1)
          enddo
        enddo
      nlp=min0(9,nl4)
      write (lun11,*)'new matrix',nl4,nlp
      do ll=1,nl4
        write (lun11,*)'b(i)=',bv(ll)
        do ml=1,nl4
          if (abs(am(ll,ml)).gt.1.e-34)
     $     write (lun11,9902)ll,ml,(am(ll,ml))
          enddo
        enddo
c
      return
c
 1009 continue
c
c     step through and put back right hand sides
      jkk=0
      do 7 jk=1,nl3
        bvo(jk)=0.
        if (ikey(jk).eq.0) go to 7
        jkk=jkk+1
        bvo(jk)=bv(jkk)
 7    continue
c
c
      return
      end
      subroutine comp2(lpri,lun11,epi,bremsa,t,
     $  r,decomp,ecomp,sxcomp,cmp1,cmp2)
c
c
c
c
c     this subroutine computes the heating - cooling due to compton
c     scattering.  the rate is returned in the common block coheat.
c
c
      parameter (ncn=9999)
      parameter (ncomp=101)
c
      dimension decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
c
c
      dimension bremsa(ncn),epi(ncn)
c
c
      data c2/8.219e-06/
      data emc2/5.11e+5/,sigth0/6.65e-25/,ergsev/1.602197e-12/
c
      lprisv=lpri
c      lpri=2
      if (lpri.ge.1) write (lun11,*)'in comp2'
c
      sigth0 = 6.65e-25
      c1=1.95639e-6
      tmp1 = 0.
      tmp2 = 0.
      c2 = 0.
      r19=r/1.e+19
      fpr2=r19*r19
c
c
      ekt = t*0.861707
      xx = emc2/(ekt+1.e-10)
      sxx = 1./xx
      zrmstp = bremsa(1)
      eee = epi(1)
      ee = eee/emc2
      tmp1 = zrmstp*cmpfnc(decomp,ecomp,sxcomp,ee,sxx,lun11,lpri)
      sum1 = 0.
      sum2 = 0.
      sum3 = 0.
      numcon=ncn
      do 100 kl = 2,numcon
         tmp1o = tmp1
         eeeo = eee
         eeo = ee
         eee = epi(kl)
         ee = eee/emc2
         zrmstp = bremsa(kl)
         tmp1 = zrmstp*cmpfnc(decomp,ecomp,sxcomp,ee,sxx,lun11,lpri)
         sum1 = sum1 + (tmp1+tmp1o)*(eee-eeeo)/2.
         sum2 = sum2 + (bremsa(kl)+bremsa(kl-1))*(eee-eeeo)/2.
         sum3 = sum3 + (bremsa(kl)*ee+bremsa(kl-1)*eeo)*(eee-eeeo)/2.
         if (lpri.ne.0) write (lun11,*)kl,eee,ee,zrmstp,tmp1,sum1
 100  continue
      ans = sum1
      cfake=sum2*sigth0
      hfake=sum3*sigth0
      cohc = -ans*sigth0
      cmp1=hfake
      cmp2=(-cohc+hfake)/ekt
      if (lpri.ne.0)
     $ write (lun11,*)'cmp1,cmp2:',cmp1,cmp2,cfake,hfake
c
      lpri=lprisv
c
      return
      end
      subroutine comp(lpri,lun11,epi,bremsa,r,cmp1,cmp2)
c
c
c
c
c     this subroutine computes the heating - cooling due to compton
c     scattering.  the rate is returned in the common block coheat.
c
c
      parameter (ncn=9999)
c
      dimension bremsa(ncn),epi(ncn)
c
c
      data c2/8.219e-06/
c
      lprisv=lpri
c      lpri=3
      if (lpri.ge.1) write (lun11,*)'in comp'
c
      sigth = 6.65e-25
      c1=1.95639e-6
      tmp1 = 0.
      tmp2 = 0.
      c2 = 0.
      r19=r/1.e+19
      fpr2=r19*r19
c
c     due to continuum.
      fac1 = sigth*bremsa(1)*epi(1)*(1.-c2*epi(1))
      fac3 = sigth*bremsa(1)*4.
      numcon=ncn
      do 100 i = 2,numcon
         delt = epi(i) - epi(i-1)
         fac2 = sigth*bremsa(i)*epi(i)*(1.-c2*epi(i))
         tmp1 = tmp1 + (fac1+fac2)*delt/2.
         fac1 = fac2
         fac4 = sigth*bremsa(i)*4.
         tmp2 = tmp2 + (fac3+fac4)*delt/2.
         fac3 = fac4
         if ( lpri.gt.2 ) write (lun11,99001) i,epi(i),bremsa(i),
     &                           fac1,fac3,tmp1,tmp2
 100  continue
c
      ebar = tmp1*4./(1.e-30+tmp2)
      if ( lpri.gt.2 ) write (lun11,*) 'ebar=',ebar
c
c
      if (lpri.gt.2)  write (lun11,*)c1,tmp1,tmp2
      cmp1 = c1*tmp1
      cmp2 = c1*tmp2
      if (lpri.gt.2) write (lun11,*)cmp1,cmp2
      lpri=lprisv
c
      return
99001 format (' ',i4,6e12.4)
      end
**==dbwk.spg  processed by SPAG 4.50J  at 17:36 on 21 Sep 2001
      subroutine dbwk(linst,lpri,lun11,idat1,rdat1,kdat1,nptrs,np2,
     &                npnxt,npfi,npar,npfirst,nplin,nplini,npcon,npconi,
     &                npilev,npilevi,npconi2,nlevs,nlsvn,ncsvn,abel)
c
c     this program manipulates the database
c
c     functions include:
c        read in and write out
c        add a record (on end)                      (instruction 2)
c        delete a record                            (instruction 3)
c        print all records of certain type          (instruction 4)
c        exit                                       (instruction 5)
c        create key vectors linking certain records (instruction 7)
c        sort records by ion number and data type   (instruction 8)
c
c     data structures are:
c      data: the database arrays (integer, real, character)
c       idat1(nidat1)
c       rdat1(nrdat1),
c       kdat1(nkdat1)
c     descriptions of database entries, and pointers
c       nptrs(nptt,ndat2)
c         nptrs(2,nx)=data type
c         nptrs(3,nx)=rate type
c         nptrs(4,nx)=continuation flag
c                       (n=number of continuations to come)
c         nptrs(5,nx)=number of reals
c         nptrs(6,nx)=number of integers
c         nptrs(7,nx)=number of characters
c         nptrs(8,nx)=pointer to reals
c         nptrs(9,nx)=pointer to integers
c         nptrs(10,nx)=pointer to characters
c
c       pointers:
c       next record:
c         npnxt(ndat2)
c       parent record (=ion header or element header)
c         npar(ndat2)
c       first record of a given rate type
c         npfirst(ntyp)
c       first record of rate type ntyp for ion nni
c         npfi(ntyp,nni)
c       pointer for line data from array containing luminosities
c         nplin(nnnl)
c       (inverse) pointer for line data to array containing luminosities
c          from database array
c         nplini(ndat2)
c       pointer for continuum data (pi xsection) from array containing luminosities
c         npcon(nnml)
c       pointer to abundance array to first level of element nni
c         npconi2(ndat2)
c       (inverse) pointer for continuum data (pi xsection) from array containing
c           luminosities
c         npconi(ndat2)
c
c       local variables used to construct pointers
c       nel(30)
c       nion(200)
c       mlold(50)
c       rdat(100)
c       idat(100)
c       kdat(100)
c       nemap(13)
c       nimap(168)
c       npnxt2(ndat2)
c       npfirst2(ntyp)
c       mlold2(50)
c
      parameter (ntyp=90)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,nptt=10,
     &           ndat2=100000)
      parameter (ndl=2400,nd=ndl+1)
c
c     master data
      dimension idat1(nidat1) , rdat1(nrdat1) , nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
      dimension npnxt(ndat2) , npar(ndat2) , npfirst(ntyp)
      dimension npfi(ntyp,nni)
      dimension nplin(nnnl) , nplini(ndat2) , npcon(nnml)
      dimension npilev(nd,nni) , npilevi(nnml)
      dimension npconi2(ndat2)
      dimension npconi(ndat2)
      dimension nel(nl) , nion(nni) , mlold(ntyp)
      dimension rdat(20000) , idat(20000)
      character(1) kdat(20000)
      dimension nemap(nl) , nimap(nni)
      dimension luse(nni) , nlines(nni) , nlevs(nni)
      dimension nptrt(ndat2)
      dimension abel(13),melpt(13)
c
      lprisv=lpri
c
      write (lun11,*) 'np2=' , np2
      ml = 0
      mlp = 0
 100  call dread(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,ml,
     &           idat1,rdat1,kdat1,nptrs,0,lun11)
      mlp = mlp + 1
      nptrt(mlp) = ml
c        call dprinto(ltyp,lrtyp2,lcon,
c     $  lrdat,rdat,lidat,idat,lkdat,kdat,8)
      if ( ml.le.np2 ) goto 100
      np2 = mlp - 1
      write (lun11,*) 'np2=' , np2
c
c
      if ( linst.eq.11 ) then
         write (lun11,*) 'linst=11' , np2
         mlt = 1
 150     ml = nptrt(mlt)
         write (lun11,*) 'ml,mlt:' , ml , mlt
         ltyp = nptrs(2,ml)
         call dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &              ml-1,idat1,rdat1,kdat1,nptrs,lprid,lun11)
         call dprinto(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &                lun11)
         mlt = mlt + 1
         if ( mlt.lt.np2 ) goto 150
      endif
c
      if ( linst.eq.12 ) then
         write (lun11,*) 'linst=11' , np2
         mlt = 1
 200     ml = nptrt(mlt)
         write (lun11,*) 'ml,mlt:' , ml , mlt
         ltyp = nptrs(2,ml)
         call dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &              ml-1,idat1,rdat1,kdat1,nptrs,lprid,lun11)
         call dprints(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &                lun11)
         mlt = mlt + 1
         if ( mlt.lt.np2 ) goto 200
      endif
c
 
c       add record
      if ( linst.eq.2 ) then
         call dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &              np2,idat1,rdat1,kdat1,nptrs,0,lun11)
         call dprinto(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &                lun11)
c           call dprintn(ltyp,lrtyp,lcon,
c     $        lrdat,rdat,lidat,idat,lkdat,kdat,np1,np2)
      endif
c
c       delete record
      if ( linst.eq.3 ) then
         read (5,*) ndel
         do ml = ndel , np2 - 1
            call dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &                 ml,idat1,rdat1,kdat1,nptrs,0,lun11)
            call dprinto(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,
     &                   kdat,lun11)
c             call dprintn(ltyp,lrtyp,lcon,
c     $        lrdat,rdat,lidat,idat,lkdat,kdat,nptr1(ml-1),ml-1)
         enddo
         np2 = np2 - 1
      endif
c
c       print out records sorted by isoseq
      if ( linst.eq.9 ) then
c       special loop for element data
         ml = npfirst(11)
 250     call dread(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &              ml-1,idat1,rdat1,kdat1,nptrs,0,lun11)
         call dprinto(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &                lun11)
         ml = npnxt(ml)
         if ( ml.ne.0 ) goto 250
         do mls = 1 , 28
            write (lun11,*) 'isosequence=' , mls
            ml12 = npfirst(12)
            do mli = 1 , nni
               if ( nion(mli).ne.0 ) then
                  mlel = npar(ml12)
                  call dread(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,
     &                       lkdat,kdat,mlel-1,idat1,rdat1,kdat1,nptrs,
     &                       0,lun11)
                  nnz = idat(1)
                  call dread(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,
     &                       lkdat,kdat,ml12-1,idat1,rdat1,kdat1,nptrs,
     &                       0,lun11)
                  nnion = idat(1)
                  iso = nnz + 1 - nnion
                  lomiti = 0
c            if ((iso.ge.3).and.(iso.le.8)) lomiti=1
c            write (lun11,*)mls,mli,mlel,nnz,nnion,iso
                  if ( iso.eq.mls ) then
c              write (lun11,*)'ion number=',mli
                     call dprinto(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,
     &                            idat,lkdat,kdat,lun11)
                     do mll = 1 , ntyp
                        lomita = 0
                        if ( (lomiti.eq.1) .and. 
     &                       ((mll.eq.3) .or. (mll.eq.4) .or. (mll.eq.5)
     &                       .or. (mll.eq.2) .or. (mll.eq.2) .or. 
     &                       (mll.eq.13) .or. (mll.eq.7)) ) lomita = 1
                        if ( (mll.ne.12) .and. (mll.ne.11) .and. 
     &                       (lomita.ne.1) ) then
                           ml1 = npfi(mll,mli)
                           if ( ml1.ne.0 ) then
c                  write (lun11,*)'rate type=',mll,' ',krdesc(mll),ml1
                              ml = ml1
                              mllz = npar(ml)
 252                          call dread(ltyp,lrtyp2,lcon,lrdat,rdat,
     &                           lidat,idat,lkdat,kdat,ml-1,idat1,rdat1,
     &                           kdat1,nptrs,0,lun11)
                              call dprinto(ltyp,lrtyp2,lcon,lrdat,rdat,
     &                           lidat,idat,lkdat,kdat,lun11)
                              ml = npnxt(ml)
                              if ( (ml.ne.0) .and. (npar(ml).eq.mllz) )
     &                             goto 252
                           endif
                        endif
                     enddo
                  endif
                  ml12 = npnxt(ml12)
               endif
            enddo
         enddo
      endif
c
c
c       print out records sorted by ion
      if ( linst.eq.8 ) then
         do mli = 1 , nni
            luse(mli) = 0
         enddo
         mlel1 = npfirst(11)
 300     call dread(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &              mlel1-1,idat1,rdat1,kdat1,nptrs,0,lun11)
         call dprinto(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &                lun11)
         do mli = 1 , nni
c            write (lun11,*)'mli,nion(mli)',mli2,nimap(mli2),
c     $                             nion(mli),mli,npfirst(12)
            ml12 = npfirst(12)
            if ( ml12.ne.0 ) then
 310           mlel2 = npar(ml12)
c              write (lun11,*)mlel2,ml12
               call dread(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,lkdat,
     &                    kdat,ml12-1,idat1,rdat1,kdat1,nptrs,0,lun11)
c              write (lun11,*)mlel2,mlel1,mli,idat(lidat),ml12
               if ( (mlel2.eq.mlel1) .and. (mli.eq.idat(lidat)) .and. 
     &              (luse(mli).ne.1) ) then
                  luse(mli) = 1
                  call dprinto(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,
     &                         lkdat,kdat,lun11)
                  do mlk = 1 , 13
                     if ( (mlk.ne.12) .and. (mlk.ne.11) ) then
                        ml = npfirst(mlk)
                        if ( ml.ne.0 ) then
 312                       call dread(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,
     &                                idat,lkdat,kdat,ml-1,idat1,rdat1,
     &                                kdat1,nptrs,0,lun11)
                           if ( mli.eq.idat(lidat) )
     &                          call dprinto(ltyp,lrtyp2,lcon,lrdat,
     &                          rdat,lidat,idat,lkdat,kdat,lun11)
                           ml = npnxt(ml)
                           if ( ml.ne.0 ) goto 312
                        endif
                     endif
                  enddo
               endif
               ml12 = npnxt(ml12)
               if ( ml12.ne.0 ) goto 310
            endif
         enddo
         mlel1 = npnxt(mlel1)
         if ( mlel1.ne.0 ) goto 300
      endif
c
c
c       print out all records
      if ( linst.eq.6 ) then
c           write (lun11,*)'linst=6',np2
         ml = 0
 350     ltyp = nptrs(2,ml)
         lprid = 0
         call dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,ml,
     &              idat1,rdat1,kdat1,nptrs,lprid,lun11)
         write (lun11,*) 'ml=' , ml
c             write (lun11,*)ltyp,lrtyp,lcon,
c     $        lrdat,rdat,lidat,idat,lkdat,kdat,nptrs(3,ml),ml
         call dprinto(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &                lun11)
         if ( ml.lt.np2 ) goto 350
      endif
c
c       set up pointers
      if ( linst.eq.7 ) then
c
         do kk = 1 , ntyp
            mlold(kk) = 0
         enddo
c
c            write (lun11,*)'linst=7'
c
c            pointer structure
c     type    desc         nr  ni  nk      daught  par
c     1       rr, a&p      2   1   0               14
c     2       hcx          4   1   0               14
c     3       ai           2   1   0               14
c     4       line dat 1   2   3   0        5      14
c     5       line dat 2   4   3   0                4
c     6       lev dat  1   4   3   0               14
c     7       dr a&p       5   1   0               14
c     8       dr a&r       0   0   0               14
c     9       hecx         4   1   0               14
c     10      lev dat 2    0   2  30                6
c     11      2 ph         2   2   0               14
c     12      pixc, bpl    5   2   0               14
c     13      el           2   2  30       14       0
c     14      ion          1   2   8       all     13
c     15      pixc bkh 1   5   1   0       20      14
c     16      pixc bkh     0   0   0               14
c     17      cx: cota     4   3   0               14
c     18      rr: cota     3   1   0               14
c     19      pixc hullac  0   0   0               14
c     20      pixc bkh 2   5   1   0       21      15
c     21      pixc bkh 3   4   4  11               20
c     22      dr stroey    5   1   0               14
c     23      pixc clark   5   2   0       24      14
c     24      pixc clark 2 4   4   0               23
c     25      ci r&s       0   0   0               14
c     26      ci cota      2   2   0               14
c
         lprisv = lpri
         lpri = 1
c           lun11=6
c
         call remtms(tt0)
c          first zero the pointers
         do ml = 1 , ntyp
            npfirst(ml) = 0
            do ll = 1 , nni
               npfi(ml,ll) = 0
            enddo
         enddo
         do ml = 1 , nl
            nemap(ml) = 0
            nel(ml) = 0
         enddo
         do ml = 1 , nni
            nion(ml) = 0
            nimap(ml) = 0
         enddo
         do ml = 1 , np2
            npar(ml) = 0
            npnxt(ml) = 0
         enddo
c          first step through and find all the elements
         call remtms(tt1)
         tzero = abs(tt1-tt0)
         if ( lpri.ne.0 ) write (lun11,*) 'the element pointers:' , 
     &                           tzero
c        first sort the element abundances
         lsrt=0
         do mml=1,nl
           melpt(mml)=mml
           enddo
         niter=0
         do while (lsrt.eq.0)
           lsrt=1
           niter=niter+1
           do mml=1,nl-1
             if (abel(melpt(mml)).lt.abel(melpt(mml+1))) then
               melptmp=melpt(mml)
               melpt(mml)=melpt(mml+1)
               melpt(mml+1)=melptmp
               lsrt=0
               endif
             enddo
           enddo
         mml=1
         do while ((abel(melpt(mml)).gt.1.e-12).and.(mml.lt.nl))
           mml=mml+1
           enddo
         mmlelmx=mml-1
         do mml=1,mmlelmx
           mlcomp=melpt(mml)
           nenxt=0
           do ml=1,np2
             lrtyp = nptrs(3,ml)
             if ( lrtyp.eq.11 ) then
               call dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,
     &                    kdat,ml-1,idat1,rdat1,kdat1,nptrs,0,lun11)
               nenxt2 = idat(lidat)
               nenxt = nenxt + 1
               if ( mlcomp.eq.nenxt2 ) then
                 if ( npfirst(lrtyp).eq.0 ) then
                     npfirst(lrtyp) = ml
                   else
                     npnxt(mlo) = ml
                   endif
                 mlo = ml
                 nemap(nenxt2) = nenxt
                 if ( lpri.gt.1 ) write (lun11,*) ml,nenxt,mlo,nenxt2
                 nel(nenxt) = ml
                 endif
               endif
             enddo
           enddo
c             write (lun11,*)'npfirst(11)=',npfirst(11)
c          next step through and put in the ion pointers:
         call remtms(tt2)
         t1 = abs(tt2-tt1)
         tt1 = tt2
         if ( lpri.ne.0 ) write (lun11,*) 'the ion pointers:' , t1
         ninxt = 0
         do ml = 1 , np2
            lrtyp = nptrs(3,ml)
            if ( lrtyp.eq.12 ) then
               call dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,
     &                    kdat,ml-1,idat1,rdat1,kdat1,nptrs,0,lun11)
               ninxt2 = idat(3)
c               write (lun11,*)idat(2),idat(3)
               netmp = nemap(idat(2))
c               write (lun11,*)netmp
               if ( (netmp.gt.0) .and. (netmp.le.nl) ) then
                  ninxt = ninxt + 1
                  nion(ninxt) = ml
                  nimap(ninxt2) = ninxt
                  npar(ml) = nel(netmp)
                  if ( lpri.gt.1 ) write (lun11,*) ml , ninxt , netmp , 
     &                 nel(netmp)
               endif
            endif
         enddo
         if ( lpri.gt.1 ) then
            write (lun11,*) 'the ion map:'
            do mml = 1 , 168
               write (lun11,*) mml , nimap(mml)
            enddo
         endif
c          next step through and put in pointers for others
         call remtms(tt2)
         t1 = abs(tt2-tt1)
         tt1 = tt2
         if ( lpri.ne.0 ) write (lun11,*) 'the other pointers:' , t1
         if ( lpri.gt.1 ) write (lun11,*) 'np2=' , np2
         do ml = 1 , np2
            lrtyp = nptrs(3,ml)
            if ( (lrtyp.ne.11) .and. (lrtyp.ne.12) .and. (lrtyp.gt.0) )
     &           then
c               write (lun11,*)'ml,nptrs(3,ml):',ml,nptrs(3,ml)
               call dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,
     &                    kdat,ml-1,idat1,rdat1,kdat1,nptrs,0,lun11)
               if ( lidat.ne.0 ) then
                  ninxt = idat(lidat)
c                 write (lun11,*)ml,lrtyp,ninxt
                  if ( ninxt.gt.0 ) then
c                   write (lun11,*)nimap(ninxt)
                     if ( nimap(ninxt).gt.0 ) then
c                     write (lun11,*)nion(nimap(ninxt))
                        npar(ml) = nion(nimap(ninxt))
                        if ( lpri.gt.1 ) write (lun11,*) ml , ltyp , 
     &                       lrtyp , lidat , ninxt , nion(nimap(ninxt))
     &                       , npar(ml)
                     endif
                  endif
               endif
            endif
         enddo
c          now the next pointers
         call remtms(tt2)
         t1 = abs(tt2-tt1)
         tt1 = tt2
         if ( lpri.ne.0 ) write (lun11,*) 'filling next pointers' , t1
         ml = 0
 400     ml = ml + 1
         mlo = ml
         lcon = nptrs(4,ml)
         if ( lcon.ne.0 ) then
 420        ml = ml + 1
            lcon = nptrs(4,ml)
            if ( (lcon.ne.0) .and. (ml.lt.np2) ) goto 420
         endif
         lrtyp = nptrs(3,mlo)
         if ( (lrtyp.ne.0) .and. (npar(ml).ne.0) ) then
            mltyp = mlold(lrtyp)
            if ( lpri.gt.1 ) write (lun11,*) ml , mlo , lrtyp , mltyp , 
     &                              nptrs(4,ml)
            if ( mltyp.ne.0 ) npnxt(mltyp) = mlo
            mlold(lrtyp) = mlo
         endif
         if ( ml.lt.np2 ) goto 400
c          now the first pointers
         do kl = 1 , ntyp
            if ( kl.ne.11 ) npfirst(kl) = 0
         enddo
         ml = 0
 450     ml = ml + 1
         lrtyp = nptrs(3,ml)
         if ( (lrtyp.ne.0) .and. (npar(ml).ne.0) ) then
            if ( npfirst(lrtyp).eq.0 ) npfirst(lrtyp) = ml
         endif
         if ( ml.lt.np2 ) goto 450
         if ( lpri.gt.1 ) then
            write (lun11,*) 'the next pointers'
            do ll = 1 , ntyp
               ml = npfirst(ll)
               write (lun11,*) 'type=' , ll , ml
               if ( ml.ne.0 ) then
 455              mln = npnxt(ml)
                  write (lun11,*) ml , mln
                  ml = mln
                  if ( mln.ne.0 ) goto 455
               endif
            enddo
         endif
c          now the next ion pointers
         call remtms(tt2)
         t1 = abs(tt2-tt1)
         tt1 = tt2
         if ( lpri.ne.0 ) write (lun11,*) 'the next ion pointers' , t1
         jkk = 0
         do jkkl = 1 , nni
            jkk2 = nimap(jkkl)
            if ( jkk2.ne.0 ) then
               jkk = jkk + 1
c             jkk=jkk2
               do kk = 1 , ntyp
                  npfi(kk,jkk) = 0
               enddo
               mlion = nion(jkk)
               if ( lpri.gt.1 ) write (lun11,*) 'jkk=' , jkk , 
     &                                 nimap(jkkl) , mlion
               do kk = 1 , ntyp
                  mlold(kk) = 0
               enddo
c             write (lun11,*)npar(1032)
               do mlt = 1 , ntyp
                  ml = npfirst(mlt)
                  if ( ml.ne.0 ) then
 456                 lrtyp = nptrs(3,ml)
                     lcon = nptrs(4,ml)
c                 write (lun11,*)ml,lrtyp,mlion,npar(ml)
c                 write (lun11,*)jkk,mlt
                     if ( (mlion.eq.npar(ml)) .and. (lrtyp.ne.0) ) then
                        mltyp = mlold(lrtyp)
c                     npni(mltyp,jkk)=ml
c                     write (lun11,*)mltyp,ml,jkk
                        if ( mltyp.le.0 ) npfi(lrtyp,jkk) = ml
                        mlold(lrtyp) = ml
                     endif
                     ml = npnxt(ml)
                     if ( ml.ne.0 ) goto 456
                  endif
               enddo
               if ( lpri.gt.1 ) then
                  do kk = 1 , ntyp
                     if ( npfi(kk,jkk).ne.0 ) then
                        write (lun11,*) jkk , kk , npfi(kk,jkk)
                        ml = npfi(kk,jkk)
                        mlz = npar(ml)
 458                    ml = npnxt(ml)
                        write (lun11,*) '   ' , ml
                        if ( (ml.ne.0) .and. (mlz.eq.npar(ml)) )
     &                       goto 458
                     endif
                  enddo
               endif
            endif
         enddo
c          now the line pointers
         jkkl = 0
         jkk = 0
         call remtms(tt2)
         t1 = abs(tt2-tt1)
         tt1 = tt2
         if ( lpri.ne.0 ) write (lun11,*) 'the line pointers' , t1
         do mm = 1 , nni
            nlines(mm) = 0
            nlevs(mm) = 0
         enddo
         do ml = 1 , np2
            ltyp = nptrs(2,ml)
            lrtyp = nptrs(3,ml)
            if ( ((lrtyp.eq.4) .or. (lrtyp.eq.14) .or. (lrtyp.eq.9))
     &           .and. (npar(ml).ne.0) ) then
               jkkl = jkkl + 1
               nplin(jkkl) = ml
               nplini(ml) = jkkl
               mlpar = npar(ml)
               call dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,
     &                    kdat,mlpar-1,idat1,rdat1,kdat1,nptrs,0,lun11)
               ilv = idat(lidat)
               if ( (ilv.gt.0) .and. (ilv.le.nni) ) nlines(ilv)
     &              = nlines(ilv) + 1
               if ( lpri.gt.1 ) write (lun11,*) jkkl , ml , nplin(jkkl)
     &                                 , npar(ml)
            endif
         enddo
         nlsvn = jkkl
c          now the continuum pointers
c          note that problems may occur if pi xsections aren't ordered
c           the same as levels.
         call remtms(tt2)
         t1 = abs(tt2-tt1)
         tt1 = tt2
         if ( lpri.ne.0 ) write (lun11,*) 'the continuum pointers' , t1
         jkkl = 0
         ml = 1
         do jkk = 1 , nni
            do jxx = 1 , 2
               if ( jxx.eq.1 ) then
                  ml = npfi(7,jkk)
               else
                  ml = npfi(1,jkk)
               endif
               if ( lpri.gt.1 ) write (lun11,*) jkk , ml , nimap(jkk)
               if ( ml.ne.0 ) then
                  mllz2 = npar(ml)
                  do while ((ml.ne.0).and.(npar(ml).eq.mllz2))
c                    now go looking for associated level
                     call dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,
     &                             idat,lkdat,kdat,ml-1,idat1,rdat1,
     &                             kdat1,nptrs,0,lun11)
                     if ( lidat.gt.1 ) then
                        nlvtmp1 = idat(lidat-1)
                        mll = npfi(13,jkk)
                        mllz = npar(mll)
                        lfnd=0
                        do while ((mll.ne.0).and.(lfnd.eq.0)
     $                                  .and.(npar(mll).eq.mllz))
                           call dread(ltyp,lrtyp,lcon,lrdat,rdat,
     &                              lidat,idat,lkdat,kdat,mll-1,idat1,
     &                              rdat1,kdat1,nptrs,0,lun11)
                           nlvtmp2 = idat(lidat-1)
                           if ( lpri.gt.1 ) write (lun11,*)
     &                                 'nlvtmp1,nlvtmp2,mll:' , 
     &                                nlvtmp1 , nlvtmp2 , mll
                           if ( nlvtmp1.eq.nlvtmp2 ) then
                              jkkl = jkkl + 1
                              npcon(jkkl) = ml
                              npconi(mll) = jkkl
                              npconi2(ml) = jkkl
                              mlpar = npar(ml)
                              call dread(ltyp,lrtyp,lcon,lrdat,
     &                                 rdat,lidat,idat,lkdat,kdat,
     &                                 mlpar-1,idat1,rdat1,kdat1,nptrs,
     &                                 0,lun11)
                              ilv = idat(lidat)
                              if ( (ilv.gt.0) .and. (ilv.le.nni) )
     &                           nlevs(jkk)=max(nlevs(jkk),nlvtmp2)
                              lfnd=1
                              if ( lpri.gt.1 ) write (lun11,*)
     &                                 jkkl , ml , npcon(jkkl) , 
     &                                 npar(ml) , npconi2(ml) , 
     &                                 npconi(mll) , mll
                           else
                              mll = npnxt(mll)
                           endif
                        enddo
                        ml1 = ml
                        ml = npnxt(ml)
                     endif
                  enddo
               endif
            enddo
         enddo
         ncsvn = jkkl
         call remtms(tt2)
         t1 = abs(tt2-tt1)
         tt1 = tt2
         if (lpri.ne.0)
     $       write (lun11,*) 'ion, #lines, #levels' , t1
         if ( lpri.gt.1) then
            nltot = 0
            nvtot = 0
            do mm = 1 , nni
               write (lun11,*) mm , nlines(mm) , nlevs(mm)
               nltot = nltot + nlines(mm)
               nvtot = nvtot + nlevs(mm)
            enddo
            write (lun11,*) 'totals:' , nltot , nvtot
         endif
c          now the ion level pointers
         call remtms(tt2)
         t1 = abs(tt2-tt1)
         tt1 = tt2
         if ( lpri.ne.0 ) write (lun11,*) 'the ion level pointers' , t1
         jkkl = 0
         do jkk = 1 , nni
            ml = npfi(13,jkk)
            if ( ml.ne.0 ) then
               mlz = npar(ml)
               if ( lpri.gt.1 ) write (lun11,*) jkk , ml , nimap(jkk)
               kkl = 0
 470           if ( ml.gt.0 ) then
                  if ( npar(ml).eq.mlz ) then
                     kkl = kkl + 1
                     jkkl = jkkl + 1
                     call dread(ltyp,lrtyp,lcon,nrdt,rdat,nidt,idat,
     &                          nkdt,kdat,ml-1,idat1,rdat1,kdat1,nptrs,
     &                          0,lun11)
                     npilevi(jkkl) = kkl
                     npilev(kkl,jkk) = jkkl
                     if ( lpri.gt.1 ) write (lun11,*) jkkl , ml , 
     &                    idat(nidt-1) , kkl , jkk
                     ml = npnxt(ml)
                     goto 470
                  endif
               endif
            endif
         enddo
         call remtms(tt2)
         t1 = abs(tt2-tt1)
         tt1 = tt2
         if ( lpri.ne.0 ) write (lun11,*) 'nlsvn=' , nlsvn , 
     &                           ', ncsvn=' , ncsvn , t1
c         print out the pointers
         if ( lpri.gt.1 ) then
            write (lun11,*) 'the pointers'
            do ml = 1 , np2
               write (lun11,*) ml , nptrs(3,ml) , npar(ml) , npnxt(ml)
            enddo
         endif
      endif
c
      lpri = lprisv
c
      return
99001 format (1x,'ml,nptrs(3,ml):',11i6)
      end
      subroutine deletefile(filename,status)

c     a simple little routine to delete a fits file

      integer status,unit,blocksize
      character*(*) filename

c     simply return if status is greater than zero
      if (status .gt. 0)return

c     get an unused logical unit number to use to open the fits file
 1    call ftgiou(unit,status)

c     try to open the file, to see if it exists
 2    call ftopen(unit,filename,1,blocksize,status)

      if (status .eq. 0)then
c         file was opened;  so now delete it 
 3        call ftdelt(unit,status)
      else if (status .eq. 103)then
c         file doesn't exist, so just reset status to zero and clear errors
          status=0
 4        call ftcmsg
      else
c         there was some other error opening the file; delete the file anyway
          status=0
 5        call ftcmsg
          call ftdelt(unit,status)
      end if

c     free the unit number for later reuse
 6    call ftfiou(unit, status)
      end
      real*8 function dexpo(x)
c
      real*8 x
c
c      x8=x
c      dexpo=dexpo(dmin1(dmax1(x8,-600.d0),600.d0))
c      dexpo=expo(min(max(x8,-600.0),600.0))
c      expo=dexpo
      dexpo=dexp(min(max(x,-200.d0),200.d0))
c
      return
      end

      subroutine dfact(n,x)
c to calculate the factorial of an integer n.  the output x is
c the natural log of n factorial, in double precision.
c
         real x
      x=0.
      if(n.eq.0) go to 100
      do 10 i=1,n
      x=x+log(float(i))
 10    continue
c
 100   continue
      return
      end

c-------------------------------------------------------------------
      subroutine dget(ltype,ans)
c
c     this routine gets the next record of a certain type
c
c
c
      return
      end
      subroutine dprint(ltyp,lrtyp,lcon,
     $  lrdat,rdat,lidat,idat,lkdat,kdat,
     $  np1r,np1i,np1k,np2,
     $  idat1,rdat1,kdat1,nptrs,lpri,lun11)
c
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /noop/lnoop
c
       dimension idat1(nidat1),rdat1(nrdat1),kdat1(nkdat1),
     $      nptrs(nptt,ndat2)
c
      dimension rdat(20000),idat(20000)
      character(1) kdat(20000)
      character(1) kdat1
c

      lprisv=lpri
      if (lpri.ne.0)
     $ write (lun11,*)'in dprint, np1,np2=',np1,np2
      if (np2.ge.ndat2) 
     $    stop 'data index error'
      nptrs(1,np2)=1
      nptrs(2,np2)=ltyp
      nptrs(3,np2)=lrtyp
      nptrs(4,np2)=lcon
      nptrs(5,np2)=lrdat
      nptrs(6,np2)=lidat
      nptrs(7,np2)=lkdat
      nptrs(8,np2)=np1r
      nptrs(9,np2)=np1i
      nptrs(10,np2)=np1k
      if (lpri.ne.0) then
        write (lun11,*)'in dprint:',np2,np1,ltyp,lrtyp,lrdat,lidat,lkdat
        write (lun11,*)'          ',lcon,np1r,np1i,np1k
        endif
      np2=np2+1
      if (lrdat.eq.0) go to 3801
        do 303 ml=1,lrdat
           rdat1(np1r)=rdat(ml)
c           write (6,*)ml,np1r,rdat1(np1r),rdat(ml)
           np1r=np1r+1
 303       continue
 3801   continue
      if (lidat.eq.0) go to 3802
        do 302 ml=1,lidat
           idat1(np1i)=idat(ml)           
c           write (6,*)ml,np1i,idat1(np1i),idat(ml)
           np1i=np1i+1
 302       continue
 9825   format (10i6)
 3802   continue
      if (lkdat.eq.0) go to 3803
        do 301 ml=1,lkdat
            kdat1(np1k)=kdat(ml)
c            write (6,*)ml,np1k,kdat1(np1k),kdat(ml)
            np1k=np1k+1
 301      continue
 3803   continue
c
      if ((np1k.gt.nkdat1).or.(np1i.gt.nidat1).or.(np1r.gt.nrdat1)
     $   .or.(np2.gt.ndat2)) then
        write (lun11,*)'dprint index error,',np1k,np1i,np1r,np2
        stop
        endif
c
      lpri=lprisv
c
      return
      end
      subroutine dprinto2(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,lun11)
c
c
      dimension rdat(20000),idat(20000)
      character(1) kblnk,kperc,kdat(20000)
c
      data kblnk/' '/,kperc/'%'/
c
      write (lun11,*)ltyp,lrtyp,lcon,nrdat,nidat,nkdat,
     $  (rdat(mm),mm=1,nrdat),(idat(mm),mm=1,nidat),
     $  kblnk,(kdat(mm),mm=1,nkdat),kblnk,kperc
c
      return
      end
      subroutine dprinto(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,lun11)
c
c
      dimension rdat(20000),idat(20000)
      character(1) kdat(20000)
      character(20000) kdtt
      character(1) kblnk,ktst,kperc,kdtt2(20000),ktsto
c
      data kblnk/' '/,kperc/'%'/
c
c      write (lun11,*)ltyp,lrtyp,lcon,nrdat,nidat,nkdat,
c     $  (rdat(mm),mm=1,nrdat),(idat(mm),mm=1,nidat),
c     $  kblnk,(kdat(mm),mm=1,nkdat),kblnk,kperc
c      return
c
      write (kdtt(1:18),9823)ltyp,lrtyp,lcon
      write (kdtt(19:37),9823)nrdat,nidat,nkdat
c
      lsp=0
      ml2=0
      if (lsp.eq.1) go to 9009
      nkd=38
      nkd2=39
      if (nrdat.eq.0) go to 3801
c        nkd2=nkd+nrdat*13
        nkd2=nkd
        do 303 ml=1,nrdat
           ml2=nkd+(ml-1)*13
           if (ml2.le.10000) then
             write (kdtt(ml2:ml2+12),'(1pe13.5)')rdat(ml)
             nkd2=nkd2+13
             endif
 303       continue
 3801   continue
      nkd=nkd2
      write (kdtt(nkd:nkd),'(a1)')kblnk
      nkd=nkd2+1
      if (nidat.eq.0) go to 3802
        nkd2=nkd+nidat*6
        do 302 ml=1,nidat
           ml2=nkd+(ml-1)*6
           write (kdtt(ml2:ml2+5),'(i6)')idat(ml)
           ml2=ml2+6
 302       continue
 9825   format (10i6)
 3802   continue
      nkd=nkd2
      if (nkdat.eq.0) go to 3803
        write (kdtt(nkd:nkd),'(a1)')kblnk
        nkd=nkd+1
        nkd2=nkd+nkdat
c        write (lun11,*)nkd,nkdat,nkd2,(kdat(mm),mm=1,nkdat)
        do 301 ml=1,nkdat
          ml2=nkd+ml-1
          write (kdtt(ml2:ml2),'(a1)')kdat(ml)
           ml2=ml2+1
 301      continue
 9827   format (30a1)
 3803   continue
 9823    format (3i6)
c       write (lun11,*)'before write:'
c       write (lun11,*)kdtt
      ml2=ml2-1
c
c
      ll2=0
c     remove spaces
      ktst=kperc
      do 3301 ll=1,ml2
         ktsto=ktst
         read(kdtt(ll:ll),'(a1)')ktst
c         if ((ktst.eq.kblnk).and.(ktsto.eq.kblnk)) go to 3301
         ll2=ll2+1
         kdtt2(ll2)=ktst
 3301    continue
c
      write (lun11,911)(kdtt2(mm),mm=1,ll2),kblnk,kperc
 911  format (20000a1)
c
      return
c
 9009 continue
      call dprints(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,lun11)
c
c
      return
      end
      subroutine dprints2(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,lun11)
c
c
      dimension rdat(20000),idat(20000)
      character(1) kdat(20000)
      character(20000) kdtt
      character(1) kblnk,ktst,kperc,kdtt2(20000)
c
      data kblnk/' '/,kperc/'%'/
c
c      do 101 ll=1,20000
c         write (kdtt(ll:ll),'(a1)')kblnk
c 101     continue
      write (kdtt(1:18),9823)ltyp,lrtyp,lcon
      write (kdtt(19:37),9823)nrdat,nidat,nkdat
c
      nkd=38
      nkd2=37
      rtmp=rdat(1)
      if (1.gt.nrdat) rtmp=0.
      ml2=nkd2
      write (kdtt(ml2:ml2+12),'(1pe13.5)')rtmp
      ml2=nkd+13
      write (kdtt(ml2-1:ml2-1),'(a1)')kblnk
      rtmp=0.
      if (2.le.nrdat) rtmp=rdat(nrdat)
      write (kdtt(ml2:ml2+12),'(1pe13.5)')rtmp
      nkd2=nkd+2*13
      nkd=nkd2
c
      write (kdtt(nkd:nkd),'(a1)')kblnk
      nkd=nkd2+1
      ml2=nkd 
      itmp=idat(1)
      if (1.gt.nidat) itmp=0
      ml2=nkd2
      write (kdtt(ml2:ml2+5),'(i6)') itmp
      ml2=ml2+6
      itmp=idat(nidat)
      if (1.gt.nidat) itmp=0
      write (kdtt(ml2:ml2+5),'(i6)') itmp
      nkd2=nkd+2*6-1
        write (kdtt(nkd:nkd),'(a1)')kblnk
      nkd=nkd2
 9825   format (10i6)
c
      nkd=nkd2
      ml2=nkd
      if (nkdat.eq.0) go to 3803
        write (kdtt(nkd:nkd),'(a1)')kblnk
        nkd=nkd+1
        nkd2=nkd+nkdat
c        write (lun11,*)nkd,nkdat,nkd2,(kdat(mm),mm=1,nkdat)
        do 301 ml=1,nkdat
          ml2=nkd+ml-1
          write (kdtt(ml2:ml2),'(a1)')kdat(ml)
           ml2=ml2+1
 301      continue
 9827   format (30a1)
 3803   continue
 9823    format (4i6)
c       write (lun11,*)'before write:'
c       write (lun11,*)kdtt
      ml2=ml2-1
c
c
      ll2=0
c     remove spaces
      ktst=kperc
      do 3301 ll=1,ml2
c         ktsto=ktst
         read(kdtt(ll:ll),'(a1)')ktst
c         if ((ktst.eq.kblnk).and.(ktsto.eq.kblnk)) go to 3301
         ll2=ll2+1
         kdtt2(ll2)=ktst
 3301    continue
c
      write (lun11,911)(kdtt2(mm),mm=1,ll2),kblnk,kperc
 911  format (20000a1)
c
c
c
      return
      end
      subroutine dprints(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,lun11)
c
c
      dimension rdat(20000),idat(20000)
      character(1) kdat(20000)
      character(20000) kdtt
      character(1) kblnk,ktst,kperc,kdtt2(20000)
c
      data kblnk/' '/,kperc/'%'/
c
c      do 101 ll=1,20000
c         write (kdtt(ll:ll),'(a1)')kblnk
c 101     continue
      write (kdtt(1:18),9823)ltyp,lrtyp,lcon
      write (kdtt(19:37),9823)nrdat,nidat,nkdat
c
      nkd=38
      nkd2=37
      rtmp=rdat(1)
      if (1.gt.nrdat) rtmp=0.
      ml2=nkd2
      write (kdtt(ml2:ml2+12),'(1pe13.5)')rtmp
      ml2=nkd+13
      write (kdtt(ml2-1:ml2-1),'(a1)')kblnk
      rtmp=rdat(2)
c      rtmp=rdat(nrdat)
      if (2.gt.nrdat) rtmp=0.
      write (kdtt(ml2:ml2+12),'(1pe13.5)')rtmp
      nkd2=nkd+2*13
      nkd=nkd2
c
      write (kdtt(nkd:nkd),'(a1)')kblnk
      nkd=nkd2+1
      ml2=nkd 
      itmp=idat(1)
      if (1.gt.nidat) itmp=0
      ml2=nkd2
      write (kdtt(ml2:ml2+5),'(i6)') itmp
      ml2=ml2+6
      itmp=0
      if (nidat.gt.1) itmp=idat(nidat-1)
      write (kdtt(ml2:ml2+5),'(i6)') itmp
      ml2=ml2+6
      itmp=idat(nidat)
      if (1.gt.nidat) itmp=0
      write (kdtt(ml2:ml2+5),'(i6)') itmp
      nkd2=nkd+3*6-1
        write (kdtt(nkd:nkd),'(a1)')kblnk
      nkd=nkd2
 9825   format (10i6)
c
      nkd=nkd2
      ml2=nkd
      if (nkdat.eq.0) go to 3803
        write (kdtt(nkd:nkd),'(a1)')kblnk
        nkd=nkd+1
        nkd2=nkd+nkdat
c        write (lun11,*)nkd,nkdat,nkd2,(kdat(mm),mm=1,nkdat)
        do 301 ml=1,nkdat
          ml2=nkd+ml-1
          write (kdtt(ml2:ml2),'(a1)')kdat(ml)
           ml2=ml2+1
 301      continue
 9827   format (30a1)
 3803   continue
 9823    format (4i6)
c       write (lun11,*)'before write:'
c       write (lun11,*)kdtt
      ml2=ml2-1
c
c
      ll2=0
c     remove spaces
      ktst=kperc
      do 3301 ll=1,ml2
c         ktsto=ktst
         read(kdtt(ll:ll),'(a1)')ktst
c         if ((ktst.eq.kblnk).and.(ktsto.eq.kblnk)) go to 3301
         ll2=ll2+1
         kdtt2(ll2)=ktst
 3301    continue
c
      write (lun11,911)(kdtt2(mm),mm=1,ll2),kblnk,kperc
 911  format (20000a1)
c
c
c
      return
      end
      subroutine dread(ltyp,lrtyp,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &                 np2,idat1,rdat1,kdat1,nptrs,lpri,lun11)
c
      parameter (ntyp=90)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,nptt=10,
     &           ndat2=100000)
c
c
c     master data
      dimension idat1(nidat1) , rdat1(nrdat1) , nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
c
      dimension rdat(20000) , idat(20000)
c
      character(1) kdat(20000)
c
c      call remtms(tt0)
c
      if ( lpri.ne.0 ) write (lun11,*) 'in dread, np1,np2=' , np2 , 
     &                                 ltyp , ntyp
 
c      if ((ltyp.le.0).or.(ltyp.gt.ntyp))
c     $    stop 'data typing error'
      mlr = 0
      mli = 0
      mlk = 0
      nrd = 0
      lconsv = 0
      lcon=1
      do while (lcon.ne.0)
        np2 = np2 + 1
        nrd = nrd + 1
        np1 = nptrs(1,np2)
        ltyp = nptrs(2,np2)
        lrdat = nptrs(5,np2)
        lidat = nptrs(6,np2)
        lkdat = nptrs(7,np2)
        lrtyp = nptrs(3,np2)
        lcon = nptrs(4,np2)
        np1r = nptrs(8,np2)
        np1i = nptrs(9,np2)
        np1k = nptrs(10,np2)
        if ( lpri.ne.0 ) write (lun11,*) 'in dread:' , np2 , np1 , ltyp , 
     &                                 lrtyp , lrdat , lidat
        if ( lpri.ne.0 ) write (lun11,99001) lkdat , lcon , np1r , np1i , 
     &                        np1k
        if ( lrdat.ne.0 ) then
          do ml = 1 , lrdat
            rdat(mlr+ml) = rdat1(np1r+ml-1)
            if ( lpri.ne.0 ) write (lun11,*) mlr , np1r , rdat1(np1r) , 
     &                              rdat(mlr)
            enddo
          npr1=np1r+lrdat-1
          mlr=mlr+lrdat
          if ( lpri.ne.0 ) write (lun11,*) 'rdat=' , 
     &                           (rdat(mm),mm=1,lrdat) , np2
          endif
        if ( lidat.ne.0 ) then
          do ml = 1 , lidat
            if ( lpri.ne.0 ) write (lun11,*) mli , np1i , idat1(np1i) , 
     &                              idat(mli) , np2
            idat(mli+ml) = idat1(np1i+ml-1)
            enddo
          mli = mli + lidat
          np1i = np1i + lidat-1
          if ( lpri.ne.0 ) write (lun11,*) 'idat=' , 
     &                           (idat(mm),mm=1,lidat)
          endif
        if ( lkdat.ne.0 ) then
          do ml = 1 , lkdat
            kdat(mlk+ml) = kdat1(np1k+ml-1)
c           write (lun11,*)mlk,np1k,kdat1(np1k),kdat(mlk),np2
            enddo
          mlk = mlk + lkdat
          np1k = np1k + lkdat-1
          if ( lpri.ne.0 ) write (lun11,*) 'kdat=' , 
     &                           (kdat(mm),mm=1,lkdat)
          endif
        enddo
c      np2=np2-nrd+1
      lidat = mli
      lrdat = mlr
      lkdat = mlk
      lcon = 0
c
c     the last pointer has to point to the next empty space.
c      nptr1(np2+1)=np1+1
c
c      call remtms(tt1)
c      tread = tread + abs(tt1-tt0)
c
      if ( lpri.ne.0 ) write (lun11,*) 'leaving dread' , np2
c
      return
99001 format (8x,5i8)
99002 format (10i6)
      end
      subroutine dreado(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,lerr,mml)
c
c
      dimension rdat(20000),idat(20000)
      character(1) kdat(20000)
      character(4096) kdtt
      character(256) kdtb,kblnk256
      character(1) kblnk,ktst,kperc,ktsto
c
      data kblnk/' '/,kperc/'%'/
      data kblnk256/'                                                  
     $                                                                 
     $                                                                 
     $                                                                 
     $      '/   
c
c      write (6,*)'in dreado'
      lerr=0
      llb=0
      ldon=0
      do ll=1,4096
        write (kdtt(ll:ll),'(a1)')kblnk 
        enddo
 104  continue
      read (5,911)kdtt
 911  format (a4096)
c      write (6,*)'kdtt=',kdtt
      read (kdtt(1:1),'(a1)')ktst
      if (ktst.eq.kperc) ldon=1
c      write (6,*)'ktst,ldon:',ktst,ldon
      read (kdtt(2:2),'(a1)')ktst
      if (ktst.eq.kperc) ldon=ldon+1
c       write (6,*)ktst,ldon
      if (ldon.eq.2) lerr=2
      if (ldon.eq.2) return
      ll2=4096
 101    continue
        read (kdtt(ll2:ll2),'(a1)')ktst
        if (ktst.ne.kblnk) go to 102
        ll2=ll2-1
        if (ll2.gt.1) go to 101
 102    continue
      ll2=ll2+1
      ll=0
      llb=0
      nrdat=0
      nidat=0
      nkdat=0
c     up to here everything is old
c
c     now try a fancy scan
c       this is a modal loop
c         mode 0: none of the others
c         mode 1: looking for a blank
c         mode 2: found blank, args<6
c         mode 3: found blank, reading reals
c         mode 4: found blank, reading integers
c         mode 5: found blank, reading chars
      nrtmp=0
      nitmp=0
      nktmp=0
      nrdat=0
      nidat=0
      nkdat=0
      llb2=0
      nkdt2=0
      ktst=kblnk
 1021   ll=ll+1
        ktsto=ktst
        read (kdtt(ll:ll),'(a1)')ktst
        if (ktst.eq.kperc) go to 1032
        if ((ktst.eq.kblnk).and.(ktsto.ne.kblnk)
     $        .and.(ll.gt.1)) then
c         mode >1
          llb=llb+1       
          if (llb.le.6) then
c           mode=2
            go to (1022,1023,1024,1025,1026,1027),llb
 1022         continue
              read (kdtb,*)ltyp
              go to 1039
 1023         continue
              read (kdtb,*)lrtyp
              go to 1039
 1024         continue
              read (kdtb,*)lcon
              go to 1039
 1025         continue
              read (kdtb,*)nrdat
              go to 1039
 1026         continue
              read (kdtb,*)nidat
              go to 1039
 1027         continue
              read (kdtb,*)nkdat
              go to 1039
 1039         continue           
          else          
            if ((nrdat.ne.0).and.(llb.le.6+nrdat)) then
c             mode=3
              nrtmp=nrtmp+1
              read (kdtb,*)rdat(nrtmp)
              endif
            if ((nidat.ne.0).and.(llb.le.6+nrdat+nidat)
     $            .and.(llb.gt.6+nrdat)) then
c             mode=4
              nitmp=nitmp+1
              read (kdtb,*)idat(nitmp)
c              write (6,*)'mode=4',nitmp,kdtb,idat(nitmp)
              endif
          endif
        llb2=0
        kdtb=kblnk256
        else
          if (ktst.ne.kblnk) then
c           mode 1
            llb2=llb2+1
c            write (6,*)'mode=1',llb2,ktst
            write (kdtb(llb2:llb2),'(a1)')ktst
            if ((nkdat.ne.0).and.(llb.ge.6+nrdat+nidat).and.
     $        (llb.le.6+nrdat+nidat+nkdat)) then
c             mode=5
              nktmp=nktmp+1
              kdat(nktmp)=ktst
c              write (6,*)'mode=5',nktmp,kdat(nktmp)
              endif
            endif
          endif
        if (ll.le.ll2-1) go to 1021
 1032   continue
c
        nkdat=nktmp
c
c
c        write (6,*)ltyp,lrtyp,lcon,nrdat,nidat,nkdat
c        write (6,'(10(1pe10.3))')(rdat(mm),mm=1,nrdat)
c        write (6,'(10i5)')(idat(mm),mm=1,nidat)
c        write (6,'(80a1)')(kdat(mm),mm=1,nkdat)
c
      return
      end
      subroutine dsec(lnerr,nlim,lfpi,
     $       lpri,lppri,lun11,tinf,vturbi,critf,
     $       t,trad,r,delr,xee,xpx,abel,cfrac,xlum,p,lcdd,
     $       epi,bremsa,bremsint,
     $       zrems,zremso,
     $       elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       ntotit,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,fline)
c
c
c
c
c     this routine solves for temperature and electron density by the
c     double secant method
c
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
      parameter (ncomp=101)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
      dimension zrems(3,ncn),zremso(3,ncn)
      dimension elum(2,nnnl),elumo(2,nnnl)
c     line emissivities
      dimension rcem(2,nnnl)
c     line opacities
      dimension oplin(nnnl)
      dimension fline(ncn)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      dimension rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      dimension opakc(ncn)
c     level populations
      dimension xilev(nnml),bilev(nnml),rniss(nnml)
c     ion abundances
      dimension xii(nni)
      dimension rrrt(nni),pirt(nni)
      dimension cemab(2,nnml),opakab(nnml)
      dimension elumab(2,nnml),elumabo(2,nnml)
      dimension tauc(2,nnml)
      dimension htt(nni),cll(nni)
      dimension nlevs(nni)
c     element abundances
      dimension abel(13)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
      dimension decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
c
      character(133) tmpst
c      character(30) kmodelname
c
c
      if (lpri.ne.0) write (lun11,*)'in dsec'
c
      crite=1.e-03
      crith=1.e-02
      critt=2.e-09
c
      ntotit=0
      nnt = 0
      nntt=0
      lnerr = 0
      lppri0 = lppri
      nlimt =max(nlim,0)
      nlimx=abs(nlim)
      nlimx = min(nlim,15)
      nlimtt=max0(nlimt,1)
      nlimxx=max0(nlimx,1)
      if (lpri.ne.0)
     $ write (lun11,*)'nlimtt,nlimxx,lppri--',nlimtt,nlimxx,lppri
      fact = 1.2
      facx = 1.2
      epst = crith
      epsx = crite
      epstt = critt
      to = 1.e+30
      tl = 0.
      th = 0.
      xeel = 0.
      xeeh = 0.
      elctrl = 0.
      elctrh = 0.
      hmctth = 0.
      hmcttl = 0.
c
      iht = 0
      ilt = 0
      iuht = 0
      iult = 0
c
 100  nnx = 0
      t=max(t,tinf)
      if (t.lt.tinf*1.01) then
          nlimt=0
          nlimtt=0
          nlimx=0
          nlimxx=0
        else
          nlimt =max(nlim,0)
          nlimx=abs(nlim)
          nlimtt=nlimt
          nlimx=min(15,nlimx)
          nlimxx=min(15,nlimx)
        endif
c      if (t.lt.tinf) return
      nnxx=0
      ihx = 0
      ilx = 0
 200  continue
      if ( lppri.ne.0 ) then
        write (lun11,99001) 
     $   nnx,xee,xeel,xeeh,elcter,elctrl,elctrh,
     $   nnt,t,tl,th,hmctot,hmcttl,hmctth
        write (tmpst,99001) 
     $   nnx,xee,xeel,xeeh,elcter,elctrl,elctrh,
     $   nnt,t,tl,th,hmctot,hmcttl,hmctth
        call xwrite(tmpst,10)
        endif
      call func(lpri,lun11,ntotit,tinf,lfpi,vturbi,critf,
     $       t,trad,r,delr,xee,xpx,abel,cfrac,xlum,p,lcdd,
     $       epi,bremsa,bremsint,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,fline)
      if ( lppri.ne.0 ) then
        write (lun11,99001) 
     $   nnx,xee,xeel,xeeh,elcter,elctrl,elctrh,
     $   nnt,t,tl,th,hmctot,hmcttl,hmctth
        write (tmpst,99001) 
     $   nnx,xee,xeel,xeeh,elcter,elctrl,elctrh,
     $   nnt,t,tl,th,hmctot,hmcttl,hmctth
        call xwrite(tmpst,10)
99001   format (' in dsec -- ',i4,6(1pe9.2),i4,6(1pe9.2))
        endif
      ntotit=ntotit+1
      nnx = nnx + 1
      nnxx=nnxx+1
      if (nnxx.ge.nlimxx) go to 300
      tst=abs(elcter)
      if (tst.lt.epsx) go to 300
      if ( elcter.lt.0 ) then
            ihx = 1
            xeeh = xee
            elctrh = elcter
            if ( ilx.ne.1 ) then
               xee = xee*facx
               goto 200
               endif
         else
            ilx = 1
            xeel = xee
            elctrl = elcter
            if ( ihx.ne.1 ) then
               xee = xee/facx
               goto 200
               endif
         endif
         xee = (xeel*elctrh-xeeh*elctrl)/(elctrh-elctrl)
         goto 200
c
c
 300  continue
      nntt=nntt+1
      nnt = nnt + 1
      if ( abs(hmctot).le.epst )  goto 500
      if (nntt.ge.nlimtt) go to 500
      if ( nnt.lt.nlimt ) then
         if ( hmctot.lt.0 ) then
            iht = 1
            th = t
            hmctth = hmctot
            iuht = 1
            if ( iult.eq.0 ) hmcttl = hmcttl/2.
            iult = 0
            if ( ilt.ne.1 ) then
               t = t/fact
               goto 100
            endif
         else
            ilt = 1
            tl = t
            hmcttl = hmctot
            iult = 1
            if ( iuht.eq.0 ) hmctth = hmctth/2.
            iuht = 0
            if ( iht.ne.1 ) then
               t = t*fact
               goto 100
            endif
         endif
         testt = abs(1.-t/to)
         if ( testt.lt.epstt ) then
            lnerr = -2
            if ( lppri.ne.0 ) then
               write (lun11,99004)
               write (lun11,99006) nnt,t,tl,th,hmctot,hmcttl,
     &                         hmctth
            endif
            goto 500
         else 
            to = t
            t = (tl*hmctth-th*hmcttl)/(hmctth-hmcttl)
            goto 100
         endif
      endif
c
 400  lnerr = 2
      write (lun11,99002)
      write (lun11,99006) nnt,t,tl,th,hmctot,hmcttl,hmctth
c
 500  if ( lppri.ne.0 ) write (lun11,99007) test,epst,hmctot
      lppri = lppri0
c
c
      return
99002 format (' ','**** note: in dsec --  too many iterations **** ')
99003 format (' ',' electrons ',i4,6(1pe16.8))
99004 format (' ',' warrning -- dsec not converging ')
99005 format (' ',' temperature --',i4,7(1pe16.8))
99006 format (' ',' temperature ',i4,6(1pe16.8))
99007 format (' ',' finishing dsec -- test,epst,hmctot',3(1pe16.8))
      end
      function ee1exp(x)
c
c     this routine computes the first exponential integral.
c
      if ( x.ge.1. ) then
        ee1exp=(1./x)*(0.250621+x*(2.334733+x))/(1.68153+x*(3.330657+x))
        return
      endif
c
      ee1exp = (-alog(x)-0.57721566+
     &      x*(0.99999193+x*(-0.24991055+x*(0.05519968+
     &      x*(-0.00976004+x*0.0010707857)))))*exp(x)
c
      return
      end
      function ee1expo(x)
c
c     this routine computes the first exponential integral.
c
      if ( x.ge.1. ) then
      ee1expo=(1./x)*(0.250621+x*(2.334733+x))/(1.68153+x*(3.330657+x))
      return
      endif
c
      ee1expo = (-alog(x)-0.57721566+
     &      x*(0.99999193+x*(-0.24991055+x*(0.05519968+
     &      x*(-0.00976004+x*0.0010707857)))))*expo(x)
c
      return
      end

c-------------------------------------------------------------------
       subroutine eint(t,e1,e2,e3)
c  returns the values of the exponential integral function of order
c  1, 2, and 3
       e1=0.
       e2=0.
       e3=0.
c       if (t.gt.50.) return
       call expint(t,ss)
       e1=ss/t/expo(t)
       e2=exp(-t)-t*e1
       e3=0.5*(expo(-t)-t*e2)
       return
       end
      subroutine ener(epi)
c
c
c
      parameter (ncn=9999)
c
      dimension epi(ncn)
c
c
      numcon = ncn
      if (numcon.lt.4) stop 'in ener: numcon error'
      numcon2=max(2,ncn/50)
c      numcon2=200
      numcon3=numcon-numcon2
      ebnd1=0.1
      ebnd2=4.e+4
c      ebnd1=1.
c      ebnd2=1.e+2
      ebnd2o=ebnd2
      dele=(ebnd2/ebnd1)**(1./float(numcon3-1))
      epi(1)=ebnd1
      do ll=2,numcon3
        epi(ll)=epi(ll-1)*dele
        enddo
      ebnd2=1.e+6
      ebnd1=ebnd2o
      dele=(ebnd2/ebnd1)**(1./float(numcon2-1))
      do ll2=1,numcon2
        ll=ll2+numcon3
        epi(ll)=epi(ll-1)*dele
        enddo
      return
c
      end
      function ensc(tautz,aaaa)
      if (tautz.ge.5.) go to 1
      ensc=tautz*((6.5)*tautz-log(max(tautz,1.e-30)))/(1.+2.*tautz)
      ensc=max(ensc,1.)
      go to 2
1     continue
      ensc=tautz*(1.6+(1.5)/(1.+(0.2)*aaaa*tautz))
2     continue
      return
      end
      subroutine enxt(eth,nb1,lpri,epi,t,lfast,lun11,
     $                  jk,nskp,nphint,lrcalc)
c
      parameter (ncn=9999)
c
      dimension epi(ncn)
c
      data ergsev/1.602197e-12/
      data bk/1.38062e-16/
c
       lprisv=lpri
       if (lpri.gt.2)
     $  write (lun11,*)'in enxt:',eth,nb1,t,lfast,jk,lpri,
     $                    epi(1),epi(ncn),ncn
      tm=t*1.e4
      bktm=bk*tm/ergsev
      if (lfast.le.2) then
         numcon2=max(2,ncn/50)
         nphint=ncn-numcon2
         nskp=1
         nskp2=1
      elseif (lfast.eq.3) then
         nphint=nbinc(max(3.*eth,eth+3.*bktm),epi)
         nphint=max(nphint,nb1+1)
         nskp=max(1,int((nphint-nb1)/16))
         nskp2=nskp
c         nphint=ncn
c         nskp=1
c         nskp2=1
      else
        nphint=nbinc(1.e+4,epi)
        nskp=1
        nint=ncn
c       nint=1
        nint2=ncn
        nskp2=1
        endif
      nskp1=nskp
c      if (jk.gt.ncn) return
      epii=epi(jk)
      exptst=(epii-eth)/bktm
      if (exptst.lt.3.) then
         lrcalc=1
         nskp=nskp1
       else
         lrcalc=0
         nskp=nskp2
       endif
       nphint=max(nphint,nb1+nskp)
       numcon=ncn
       numcon2=max(2,ncn/50)
       numcon3=numcon-numcon2
       nphint=min(nphint,numcon3)
       if (lpri.ge.2)
     $  write (lun11,*)'in enxt:',eth,nb1,t,lfast,jk,nskp,
     $   nphint,lrcalc
c
c
      return
      end
      subroutine erc(n,m,t,ic,se,sd,a,lun11,lpri)
c erc calculates the excitation rate, se [cm**3/s],  for atomic
c transitions between lower state n and upper state m in hydrogen
c due to electron collisions.  the energy loss rate, sl [ev*cm**3/s],
c from the electron gas is also determined.  (cf. johnson,1972)
c sd = deexcitation rate;   sg = energy gained by electron gas
c sm is a quantity symmetrical in n and m, used in models
c ***  the quantity em1 is required from subr. expint in this program
c
c
      if (lpri.ne.0)
     $ write(lun11,*)'erc',n,m,t,ic,se,sd,a
      sm=0.
      if(ic.ne.1) then
       if (ic.lt.10) then
        ym=157803.*ic*ic/t/m/m
        if (ym.gt.40.) then
         sd=0.
         se=0.
         return
        endif
        if (lpri.ne.0)
     $   write (lun11,*)'before impactn:',
     $       n,m,t,ic,a,sm     
        call impactn(n,m,t,ic,a,sm,lun11,lpri)
        if (lpri.ne.0)
     $   write (lun11,*)'after impactn:',
     $       n,m,t,ic,a,sm     
        ym=157803.*ic*ic/t/m/m
        xn=(1./n/n-1./m/m)
        yn=157803.*ic*ic*xn/t
        s=sm/n/n/expo(ym)
        sd=s*n/m*n/m
        if (yn.lt.40.) then
         se=s*expo(-yn)
        else
         se=0.e0
        endif
       else
        if (lpri.ne.0)
     $   write (lun11,*)'calling szcoll:',
     $       n,m,t,se,ic
        call szcoll(n,m,t,se,ic,lun11)           
        ym=157803.*ic*ic/t/m/m
        xn=(1./n/n-1./m/m)
        yn=157803.*ic*ic*xn/t
        sd=se*n*n/m/m*expo(yn)
       endif
      else
      xn=(1./n/n-1./m/m)
      f=-1.2456e-10*a/xn/xn
      xn=(1./n/n-1./m/m)
      yn=157803.*xn/t
      ym=157803./t/m/m
      z=1.94*xn*n**0.43+yn
      if(n.eq.1) z=yn+0.45*xn
      dif=z-yn
c
      if (lpri.ne.0)
     $ write(lun11,*)'before expint:',yn,e1y,z,e1z
       call expint(yn,e1y)
       call expint(z,e1z)
      if (lpri.ne.0)
     $ write(lun11,*)'after expint:',yn,e1y,z,e1z
      e2=(1.-e1y)/yn-expo(-dif)*(1.-e1z)/z
      rn=float(n)
      rm=float(m)
      ann=-2.*f*m*m/xn/n/n
      bn=(4.-18.63/n+36.24/n/n-28.09/(n**3))/n
      if(n.eq.1) bn=-0.603
      bnn=(1.+4./(xn*n*n*3)+bn/(rn**4*xn*xn))*4./(rm**3*xn*xn)
      if (lpri.ne.0)
     $ write(lun11,*)e2,rn,rm,ann,bn,bnn
c
      s=ann*((1./yn+0.5)*e1y/yn-(1./z+0.5)*e1z*expo(-dif)/z)
      s=s+e2*(bnn-ann*log(2./xn))
      s=1.095e-10*yn*yn*sqrt(t)*s/xn
      sm=s*n*n*expo(ym)
      if (lpri.ne.0)
     $  write(lun11,*)s,sm,yn,xn,t,dif
       sd=s*n/m*n/m
       se=s*expo(-yn)
      if (lpri.ne.0)
     $  write(lun11,*)'erc=',se,sd
c      sg=13.60*sq*xn
c      sl=13.60*s*xn
        endif

      if (lpri.ne.0)
     $  write(lun11,*)'erc return:',se,sd

c
      return
      end
      function exint1(x,jump)
c      implicit real*8 (a-h,o-z)
c     r. moore october 1976
c      jump=1    exint1=e1(x)
c      jump=2    exint1=expo(x)*e1(x)
c      jump=3    exint1=x*expo(x)*e1(x)
      if(x.ge.1.) go to   30
      exint1 =((((((((7.102452d-7*x-1.766345d-6)*x+2.928433d-5)*x
     1  -.0002335379d0)*x+.001664156d0)*x-.01041576d0)*x+.05555682d0)*x
     2  -.2500001d0)*x+.9999999d0)*x-log(x)-.57721566490153d0
      go to (9999,  10,  20),jump
   10 exint1=expo(x)*exint1
      return
   20 exint1=x*expo(x)*exint1
      return
   30 x2=x*x
      x3=x2*x
      x4=x3*x
      exint1=(x4+8.5733287401d0*x3+18.059016973d0*x2+8.6347608925d0*x
     1  +.2677737343d0) /(x4+9.5733223454d0*x3+25.6329561486d0*x2
     2  +21.0996530827d0*x+3.9584969228d0)
      go to (  40,  50,9999),jump
   40 exint1=exint1*expo(-x)/x
      return
   50 exint1=exint1/x
 9999 return
      end

      function exp10(x) 
      exp10=expo(2.30259*x)
      return                                                            
      end
       subroutine expint(x,em1)
c expint is a subroutine to calculate the value of e1, the exponential
c integral or em1=x*expo(x)*e1 at the point x.  the polynomial
c expressions that are used come from abromowitz and stegen
c
      if(x.le.1.) go to 100
c
      b1=9.5733223454
      b2=25.6329561486
      b3=21.0996530827
      b4=3.9584969228
      c1=8.5733287401
      c2=18.0590169730
      c3=8.6347608925
      c4=0.2677737343
      em1=x**4+c1*x**3+c2*x*x+c3*x+c4
      em1=em1/(x**4+b1*x*x*x+b2*x*x+b3*x+b4)
c      e1=em1/x/expo(x)
      go to 200
c
 100   continue
      a0=-0.57721566
      a1=0.99999193
      a2=-0.24991055
      a3=0.05519968
      a4=-0.00976004
      a5=0.00107857
      if (x.gt.0)then
      e1= a0+a1*x+a2*x*x+a3*x**3+a4*x**4+a5*x**5-log(x)
      else
      e1=-a0+a1*x+a2*x*x+a3*x**3+a4*x**4+a5*x**5-log(-x)
      endif
      em1=e1*x*expo(x)
c
 200   continue
      return
      end
c-------------------------------------------------------------------
      function expo(x) 
c      crit=200.
c      expo=exp(x)
c      return
      crit=60.   
c      if (x.lt.-crit) then
c        expo=1.e-34
c      else
c        xtmp=min(x,crit)
        expo=exp(amin1(max(x,-crit),crit))
c      endif
c
      return                                                            
      end
c-----------------------------------------------------------------
      subroutine fact8(n,x)
c to calculate the factorial of an integer n.  the output x is
c the natural log of n factorial.
c
      implicit real*8 (a-h,o-z)
      x=0.
      if(n.eq.0) go to 100
      do 10 i=1,n
      x=x+dlog(dfloat(i))
 10    continue
c
 100   continue
      return
      end

c-------------------------------------------------------------------
      subroutine fact(n,x)
c to calculate the factorial of an integer n.  the output x is
c the natural log of n factorial.
c
      x=0.
      if(n.eq.0) go to 100
      do 10 i=1,n
      x=x+log(float(i))
 10    continue
c
 100   continue
      return
      end

c-------------------------------------------------------------------
      function fbg(u,gam)
c
c     this function computes the free-free gaunt factor
c      u=h nu/kt
c      gam=z**2 ry/kt
c         z=charge of scattering ion
c         ry=rydberg constant
c         kt=kt, etc.
c 
c
      real a,a1,a2,a3,ai,ak,born,fbg,g1,g2,gam,
     &     gam1,gam2,gam3,p,power,t,u,u1,u2
      real u4
      integer m,m1,n
c
c      real*8 t,ai,ak,u4
      dimension a(6,7,3),gam2(6),gam3(6)
      dimension a1(6,7),a2(6,7),a3(6,7)
c
      equivalence (a1(1,1),a(1,1,1)),(a2(1,1),a(1,1,2)),
     &             (a3(1,1),a(1,1,3))
c
      data gam2/.7783,1.2217,2.6234,4.3766,20.,70./
      data gam3/1.,1.7783,3.,5.6234,10.,30./
      data a1/1.001,1.004,1.017,1.036,1.056,1.121,1.001,
     &     1.005,1.017,1.046,1.073,1.115,.9991,1.005,
     &     1.030,1.055,1.102,1.176,.9970,1.005,1.035,
     &     1.069,1.134,1.186,.9962,1.004,1.042,1.100,
     &     1.193,1.306,.9874,.9962,1.047,1.156,1.327,
     &     1.485,.9681,.9755,.8363,1.208,1.525,1.955/
      data a2/.30290,.16160,.04757,.01300,.00490,-.00320,
     &     .49050,.21550,.08357,.02041,.00739,.00029,
     &     .65400,.28330,.08057,.03257,.00759,-.00151,
     &     1.0290,.39100,.12660,.05149,.01274,.00324,
     &     .95690,.48910,.17640,.05914,.01407,-.00024,
     &     1.2690,.75790,.32600,.10770,.02800,.00548,
     &     1.3270,1.0170,1.3980,.20500,.06050,.00187/
      data a3/ - 1.3230,-.25400,-.01571,-.001000,-.000184,
     &     .00008,-4.7620,-.33860,-.03571,-.001786,-.000300,
     &     .00001,-8.3490,-.42060,-.02571,-.003429,-.000234,
     &     .00005,-13.231,-.59000,-.04571,-.005714,-.000445,
     &     -.00004,-7.6720,-.68520,-.06430,-.005857,-.000420,
     &     .00004,-7.1430,-.99470,-.12000,-.010070,-.000851,
     &     -.00004,-3.1750,-1.1160,-.84140,-.018210,-.001729,
     &     .00023/
c
 
      gam1 = gam*1000.
      if ( gam1.gt.100. ) then
         power = -.134/(gam**.2097)
         fbg = 1.5*(3.*u)**power
         return
      else
         u2 = u**2
c
c*****compute born approximation gaunt factor
c
         u1 = u/2.
         t = u1/3.75
         u4 = u1/2.
         if ( u1.gt.2. ) then
c
            ak = 1.2533141 - .07832358/u4 + .02189568/u4**2 - 
     &           .01062446/u4**3 + .00587872/u4**4 - .00251540/u4**5 + 
     &           .00053208/u4**6
            ak = ak/(expo(u1)*sqrt(u1))
         else
            ai = 1.0 + 3.5156229*t**2 + 3.0899424*t**4 + 
     &           1.2067492*t**6 + 0.2659732*t**8 + 0.0360768*t**10 + 
     &           0.0045813*t**12
            ak = -1.*log(u4)*ai - .57721566 + .42278420*u4**2 + 
     &           .23069758*u4**4 + .0348859*u4**6 + .00262698*u4**8 + 
     &           .00010750*u4**10 + .0000074*u4**12
         endif
         born = .5513*expo(u1)*ak
c
c*****compute polymonial factor to multiply born approximation
c
         m=0
         n=0
         if ( gam1.ge.1. ) then
            if ( u.ge..003 ) then
               if ( u.le..03 ) n = 1
               if ( (u.le..3) .and. (u.gt..03) ) n = 2
               if ( (u.le.1.) .and. (u.gt..3) ) n = 3
               if ( (u.le.5.) .and. (u.gt.1.) ) n = 4
               if ( (u.le.15.) .and. (u.gt.5.) ) n = 5
               if ( u.gt.15. ) n = 6
               if ( gam1.le.1.7783 ) m = 1
               if ( (gam1.le.3.) .and. (gam1.gt.1.7783) ) m = 2
               if ( (gam1.le.5.6234) .and. (gam1.gt.3.) ) m = 3
               if ( (gam1.le.10.) .and. (gam1.gt.5.6234) ) m = 4
               if ( (gam1.le.30.) .and. (gam1.gt.10.) ) m = 5
               if ( (gam1.le.100.) .and. (gam1.gt.30.) ) m = 6
               m1 = m + 1
               g1 = (a(n,m,1)+a(n,m,2)*u+a(n,m,3)*u2)*born
               g2 = (a(n,m1,1)+a(n,m1,2)*u+a(n,m1,3)*u2)*born
               p = (gam1-gam3(m))/gam2(m)
               fbg = (1.0-p)*g1 + p*g2
               return
            endif
         endif
      endif
      fbg = born
      return
      end
c---------------------------------------------------------------------
      function ff2(x,lun11)
      dimension q(15),p(15)
      data q/1.,2.1958e+2,2.0984e+4,1.1517e+6,4.0349e+7,
     $      9.4900e+8,1.5345e+10,1.7182e+11,1.3249e+12,
     $      6.9071e+12,2.3531e+13,4.9432e+13,5.7760e+13,
     $      3.0225e+13,3.3641e+12/
      data p/1.,2.1658e+2,2.0336e+4,1.0911e+6,3.7114e+7,
     $       8.3963e+8,1.2889e+10,1.3449e+11,9.4002e+11,
     $       4.2571e+12,1.1743e+13,1.7549e+13,1.0806e+13,
     $       4.9776e+11,0./
      xprod=1.
c      write (lun11,*)'in ff2:',x
      pp=0.
      qq=0.
      do j=1,15
        ptst=1./xprod
        if ((ptst.lt.1.e+20).and.(xprod.lt.1.e+24/x)) then
          pp=pp+p(j)/xprod
          qq=qq+q(j)/xprod
          xprod=xprod*x
c          write (lun11,*)j,xprod,pp,qq,ptst
          endif
        enddo
      ff2=pp/(1.e-20+qq)/x/x
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C   File Name:    fheader.f
C   Author:       W.T. Bridgman
C   Date:         January 1999
C   Abstract:     Routines for writing a stardard primary FITS
C                 header for XSTAR output files.
C          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      subroutine fheader(unit,knam,mdlname,status)

C     Create a FITS file with an empty primary header
C     nrhs columns and nrows row
C
C     Parameters:
C        unit    integer            File unit number
C        knam    char*16            File name to create
C        mdlname char*30            Model name for this run
C        status  integer            Returned status code
C 
      character(16) knam, filename
      character(30) mdlname
      integer unit, status

      integer bitpix,naxis,naxes(2),group
      logical simple,extend
      integer blocksize

      status=0
c
      filename=knam

C     Delete the file if it already exists, so we can recreate it
      call deletefile(filename,status)

C     Get an unused Logical Unit Number to use to open the FITS file
      call ftgiou(unit,status)

C     open the FITS file, with write access
      blocksize=1
      call ftinit(unit,filename,blocksize,status)
      if (status .gt. 0)call printerror(lun11,status)

C     initialize parameters for primary array
      simple=.true.
      bitpix=16
      naxis=0
      naxes(1)=0
      naxes(2)=0
      extend=.true.
      group=1
C     write the required primary header keywords
      call ftphpr(unit,simple,bitpix,naxis,naxes,0,group,extend,status)
      if (status .gt. 0)call printerror(lun11,status)

C     now add additional keywords
      call ftpcom(unit,'***********************************',status)
      call ftpkys(unit,'CREATOR','XSTAR v2.1',
     $ 'Program which generated this file',status)
      if (status .gt. 0)call printerror(lun11,status)

C     Extract the system date
      call ftpdat(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

C     Save run-specific information
      call ftpkys(unit,'MODEL',mdlname,'Model name for this run',status)
      if (status .gt. 0)call printerror(lun11,status)

      return
      end
      subroutine find53(stmpp,etmpp,ntmp,efnd,sg,jlo,lun11,lpri)
c
      dimension stmpp(ntmp),etmpp(ntmp)
c
c      lpri=0
c
c      if ((efnd.ge.etmpp(1)).and.(efnd.le.etmpp(ntmp))) then
      if (lpri.ne.0) write (lun11,*)'in find53:',efnd,ntmp,
     $    etmpp(1),etmpp(ntmp),stmpp(1),stmpp(ntmp)
      if ((efnd.ge.0.).and.(efnd.le.etmpp(ntmp))) then
        call hunt(etmpp,ntmp,efnd,jlo,0,lun11)
        ml2=max(jlo,1)
        ml2=min(ml2,ntmp-1)
        mlp=ml2+1
        if (mlp.eq.ntmp) then
            alg1=alog(max(stmpp(mlp),1.e-26)/max(stmpp(ml2),1.e-26))
            alg2=alog(max(etmpp(mlp),1.e-26)/max(etmpp(ml2),1.e-26))
            algtmp=alg1/alg2
            sg=stmpp(ml2)*(efnd/etmpp(ml2))**algtmp
          else
            del1=(efnd-etmpp(ml2))/(etmpp(mlp)-etmpp(ml2))
            del2=(efnd-etmpp(mlp))/(etmpp(mlp)-etmpp(ml2))
            sg=-stmpp(ml2)*del2+stmpp(mlp)*del1
          endif
         sg=max(0.,sg)
              if (lpri.ne.0)
     $         write (lun11,*)ll,epii,sg,ml2,stmpp(ml2),stmpp(mlp),
     $              del1,del2,efnd,etmpp(ml2)
         else
              sg=0.
         endif
c
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Close the file & release the unit number
C
      subroutine fitsclose(lun11,unit,status)
C
C     Parameters:
C        unit    integer            File unit number
C        status  integer            Returned status code
      implicit none
      integer unit, status,lun11

      call ftclos(unit, status)
      call ftfiou(unit, status)
      if (status .gt. 0)call printerror(lun11,status)

      return
      end
      subroutine fparmlist(unit,hdunum,mdlname,npar,parname,partype,
     $                    parval,parcomm,nloopctl,status,lun11)
c
c     parameters:
c        unit    integer            file unit number
c        hdunum  integer            number of last hdu written
c        mdlname char*30            model name for this run
c        npar    integer            number of parameters passed
c        parname char*20(999)       parameter name
c        partype char*10(999)       parameter type
c        parval  real(999)          parameter values converted to reals
c        parcomm char*30(999)       parameter comments & string values
c        nloopctl integer           loop control parameter 
c        status  integer            returned status code
c
      implicit none
c     passed parameters
      character(30) mdlname
      integer unit, status, hdunum, npar, nloopctl
      character(20) parname(999)
      character(10) partype(999)
      real parval(999)
      character(30) parcomm(999)
c     parameter info
      integer idat1(999)
      integer lun11

      integer tfields,nrows,varidat
      character(16) ttype(5),tform(5),tunit(5)
      integer colnum,frow,felem,hdutype
      integer ll
      character(30) extname

      data tform/'1I','20A','1E','10A','30A'/
      data ttype/'index','parameter','value','type','comment'/
      data tunit/' ',' ',' ',' ',' '/

      nrows=npar
      varidat=0

c     move to the last hdu (hdunum) in the file
      call ftmahd(unit,hdunum,hdutype,status)
      if (status .gt. 0)call printerror(lun11,status)

c     append a new empty extension after the last hdu
      call ftcrhd(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

c     define parameters for the binary table (see the above data statements)
      tfields=5

c     build extension name
      extname='PARAMETERS'
C      if(nloopctl.gt.0) then
C          write(ktmp2,'(i4.4)')nloopctl
C          extname='parameters_' // ktmp2
C          endif
      
c     write the required header parameters for the binary table
      call ftphbn(unit,nrows,tfields,ttype,tform,tunit,extname,
     $              varidat,status)
      if (status .gt. 0)call printerror(lun11,status)

c     save run-specific information
      call ftpkys(unit,'MODEL',mdlname,'model name for this run',status)
      if (status .gt. 0)call printerror(lun11,status)

c     set 'global' parameters for writing fits columns
      frow=1
      felem=1

c     column  1  (index)
      colnum=1
      do ll=1,nrows
         idat1(ll)=ll
         enddo
      call ftpclj(unit,colnum,frow,felem,nrows,idat1,status) 
      if (status .gt. 0)call printerror(lun11,status)

c     column  2  (parameter name)
      colnum=2
      call ftpcls(unit,colnum,frow,felem,nrows,parname,status)
      if (status .gt. 0)call printerror(lun11,status)

c     column  3  (parameter value)
      colnum=3
      call ftpcle(unit,colnum,frow,felem,nrows,parval,status)
      if (status .gt. 0)call printerror(lun11,status)

c     column  4 (parameter type)
      colnum=4
      call ftpcls(unit,colnum,frow,felem,nrows,partype,status)
      if (status .gt. 0)call printerror(lun11,status)

c     column  5 (parameter comment)
      colnum=5
      call ftpcls(unit,colnum,frow,felem,nrows,parcomm,status)
      if (status .gt. 0)call printerror(lun11,status)

c----------------------------------------------------------------
c     compute checksums
      call ftpcks(unit,status)
      if (status .gt. 0)call printerror(lun11,status)
      return
      end
      subroutine fstepr2(unit,hdunum,radin,radout,temp,pres,nrhdim,
     $                idat1,rdat1,kdat1,nptrs,npnxt,npfi,
     $                npfirst,npar,npcon,npconi,npilev,npilevi,npconi2,
     $                nplin,nlsvn,rcem,oplin,tau0,poptol,nloopctl,
     $                nzone,lun11,status)
C
C     Append a FITS extension binary table containing 
C     nrhs columns and at most nrhdim rows
C
C     Parameters:
C        unit    integer            File unit number
C        hdunum  integer            Number of last HDU written
C        radin   real               inner radius of shell
C        radout  real               outer radius of shell
C        temp    real               temperature of shell
C        pres    real               pressure in shell
C        nrhdim  integer            Maximum number of rows
C        idat1   integer(nidat1)    Needed by the atomic database
C        rdat1   real(nidat1)       Needed by the atomic database
C        kdat1   char*nidat1        Needed by the atomic database
C        nptrs                      Needed by the atomic database
C        npnxt                      Needed by the atomic database
C        npfi                       Needed by the atomic database
C        npfirst                    Needed by the atomic database
C        npcon                      Needed by the atomic database
C        npconi                     Needed by the atomic database
C        npcon2                     Needed by the atomic database
C        xilev   real(nrhdim)       Fractional level population array
C        cemab   real(2,nrhdim)     Recombination emission 
C        opakab  real(nrhdim)       Opacity
C        tauc    real(2,nrhdim)     Optical depth
C        poptol  real               Tolerance for population level
C        nloopctl integer           Loop control variable
C        nzone   integer            Pass number through iteration process
C        status  integer            Returned status code
C
      implicit none
      integer ntyp, ncn, nnnl, nnml, nni, nl, ndl, nd, mllz
      integer nidat1, nrdat1, nkdat1, ndat2, nptt,nrhdim

      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)

C     Allocation for passed parameters
      real tau0(2,nrhdim), rcem(2,nrhdim)
      real rdat1(nrdat1), poptol
      real radin, radout, temp, pres
      integer unit,hdunum, nrows, status, nloopctl, nzone
      integer idat1(nidat1),nptrs(nptt,ndat2)
c     line opacities
      real oplin(nnnl)

c     pointers to master data 
      integer npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni),npar(ndat2)
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)
      integer nplin(nnnl)

C     Internal work areas
      real rwrk1(500)
      integer ntptr(500)
      character(10) kion(500)
      character(20) klevl(500),klevu(500),kblnk20
      integer tfields,varidat
      character(16) ttype(10),tform(10),tunit(10)
      integer colnum,frow,felem,hdutype,ll, ltyp
      integer lrtyp, lcon, nrdt, nidt, mm, lun11, lpril,lpri
      integer jkk, nlev
      integer nlplmx,ln,lnn,ml,nilin2,nlpl,lmm,kltmpn,kltmpo,
     $         llo,lup,llofnd,lupfnd,nlevmx,nlines,nlsvn,
     $         k,kl2,kk,nilin,lm,mltype
      real eliml,elimh,elin,elmmtpp,elcomp
      character(30) extname, ktmp2
      character(1) kdat1(nkdat1)

C     Database manipulation quantities
      real rdat(300)
      integer idat(300), nkdt
c      integer lpridread
      character(1) kdat(20000),kblnk,kdtmp(200)
      integer kltmp(500)
      real elsv(500)

      data kblnk/' '/
      data kblnk20/'                    '/
c
      data tform/'1J','1E','8A','20A','20A','1E','1E','1E',
     $ '1E','1E'/

      data ttype/'index','wavelength','ion',
     $ 'lower_level','upper_level','emis_inward', 
     $ 'emis_outward','opacity','tau_in','tau_out'/

      data tunit/' ','A',' ',' ',' ','erg/cm^3/s',
     $ 'erg/cm^3/s','/cm',' ',' '/

      varidat=0
c
      lpri=0
      lpril=lpri
c

C     Move to the last HDU (hdunum) in the file
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Moving to end-of-FITS file'
      call ftmahd(unit,hdunum,hdutype,status)
      if (status .gt. 0)call printerror(lun11,status)

C     append a new empty extension after the last HDU
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr: Create the new extension'
      call ftcrhd(unit,status)
      if (status .gt. 0)call printerror(lun11,status)
      
C----------------------------------------------------------------
C
C     Extracting data from the Atomic Database here
C
      if (lpri.ne.0)
     $ write (lun11,*)' '
c      lpril=0
      kltmpo=0
c
c     print important lines
      if (lpri.ne.0)
     $ write (lun11,*)'emission line luminosities (erg/sec/10**38))'
         nlplmx=500
c
         eliml=0.1
         elimh=1.0e10
c
c        find the strongest lines.
         do  lm=1,nlplmx
            kltmp(lm)=0
            enddo
c
         nlpl=1
         do 559 lnn=1,nlsvn
c
            ln=lnn
            ml=nplin(ln)
            call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
            elin=rdat(1)
c            if (lrtyp.eq.14)  elin=rdat(nrdt)
            if (lrtyp.eq.14)  go to 559
            nilin=npar(ml)
            call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,nilin-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
            nilin2=idat(nidt)
            elmmtpp=(rcem(2,ln)+rcem(1,ln))/2.
            if (lpril.ne.0)
     $       write (lun11,*)lnn,elin,nilin,elmmtpp,ln,ml
            if ((ln.lt.0).or.(ln.gt.nnnl)) go to 559
            if ((elin.le.eliml).or.(elin.ge.elimh)) 
     $           go to 559
            if (elin.ge.8.9e+4) go to 559
            if (elmmtpp.le.1.e-34) go to 559
            if ((nilin2.le.0).or.(nilin2.ge.nni)) 
     $          go to 559
c
            lmm=0
560            lmm=lmm+1
               if (lmm.ge.nlpl) go to 558
               kl2=kltmp(lmm)
               elcomp=(rcem(2,kl2)+rcem(1,kl2))/2.
               if (elmmtpp.ge.elcomp) go to 558
               go to 560
558         continue
c
            if (lpril.ne.0)
     $       write (lun11,8516)ln,elin,elmmtpp
8516        format (1h ,i4,2e12.4)
            kltmpo=ln
            do  k=lmm,nlplmx
c               if (lpril.ne.0)
c     $          write (lun11,*)'in 557 loop',k,kltmp(k),kltmpo
               kltmpn=kltmp(k)
               kltmp(k)=kltmpo
               kltmpo=kltmpn
               enddo
           if (lpril.ne.0)
     $          write (lun11,*)'done with 557 loop',lm
c
            nlpl=max0(nlpl,min(nlplmx,lmm+1),1)
c
559         continue
c
         kltmp(nlpl)=kltmpo
c
c
         nlpl=nlpl-1
c
         if (lpri.ne.0)
     $    write (lun11,959)
959      format (1x,'index, ion, wavelength, transmitted, reflected')
         do  kk=1,nlpl
           if (lpril.ne.0)
     $       write (lun11,*)'kk=',kk
           ln=kltmp(kk)
           ml=nplin(ln)
           ntptr(kk)=ln
           klevl(kk)=kblnk20
           klevu(kk)=kblnk20
           if ((ln.ne.0).and.(ml.ne.0)) then
             if (lpril.ne.0)
     $       write (lun11,*)'   ',ln,ml
             call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
             llo=idat(1)
             lup=idat(2)
             elsv(kk)=rdat(1)
             nilin=npar(ml)
             call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,nilin-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
             do mm=1,nkdt
               kdtmp(mm)=kdat(mm)
               enddo
             do mm=nkdt+1,9
              kdtmp(mm)=kblnk
              enddo  
             nilin=idat(3)
             write(kion(kk),'(10a1)')(kdtmp(mm),mm=1,10)
             jkk=idat(nidt)
             ml=npfi(13,jkk)
             if (ml.ne.0) then
               mllz=npar(ml)
               lupfnd=0
               llofnd=0
 2943          continue
                 if (lpri.ne.0)
     $           write (lun11,*)ml,nptrs(2,ml),mltype,jkk
                 call dread(ltyp,lrtyp,lcon,
     $           nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $           idat1,rdat1,kdat1,nptrs,0,lun11)
                 nlevmx=nlevmx+1
                 nlev=idat(nidt-1)
                 if (lpri.ne.0)
     $            call dprinto(ltyp,lrtyp,lcon,
     $            nrdt,rdat,nidt,idat,nkdt,kdat,lun11)
                 if (lpri.ne.0)
     $           write (lun11,*)nlev,llo,lup,nlevmx,llofnd,lupfnd
                 if (nlev.eq.llo) then
                   do mm=1,20
                     if (mm.le.nkdt) then
                         write(ktmp2(mm:mm),'(a1)')kdat(mm)
                       else
                         write(ktmp2(mm:mm),'(a1)')kblnk
                       endif
                     enddo
c                   write (lun11,*)kk,ktmp2 
                   klevl(kk)=ktmp2
                   llofnd=1
                   endif
                 if (nlev.eq.lup) then
                   do mm=1,20
                     if (mm.le.nkdt) then
                         write(ktmp2(mm:mm),'(a1)')kdat(mm)
                       else
                        write(ktmp2(mm:mm),'(a1)')kblnk
                       endif
                     enddo
                   klevu(kk)=ktmp2
                   lupfnd=1
                   endif
                 ml=npnxt(ml)
                 if ((ml.ne.0).and.(npar(ml).eq.mllz)
     $            .and.((llofnd.ne.1).or.(lupfnd.ne.1))) go to 2943
               if (lpril.ne.0) then
                 write (lun11,*)ml,nilin,npar(ml)
                 write (lun11,9955)kk,ln,(kdtmp(mm),mm=1,8),elsv(kk),
     $             rcem(1,ln),rcem(2,ln)
                 write (lun11,*)klevu(kk)
                 write (lun11,*)klevl(kk)
                 endif
 9955          format (1x,2i8,1x,8a1,3(1pe11.3))
               endif
             endif 
           enddo
c
c      if (nlpl.le.0) return
      nlpl=max(nlpl,1)
c

C     End of atomic database extraction
C----------------------------------------------------------------
C     define parameters for the binary table (see the above data statements)
      nrows=nlpl
      if (lpri.ne.0)
     $ write (lun11,*)'before header write'
      tfields=10
C     Build extension name
      extname='XSTAR_RADIAL'
      if(nloopctl.gt.0) then
          write(ktmp2,'(I4.4)')nloopctl
          extname='XSTAR_RADIAL_' // ktmp2
          endif
      
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr: Write table headers'
C     write the required header parameters for the binary table
      call ftphbn(unit,nrows,tfields,ttype,tform,tunit,extname,
     $              varidat,status)
      if (status .gt. 0)call printerror(lun11,status)

      if (lpri.ne.0)
     $ write (lun11,*)'fstepr: Add some more keywords'

C     Write some model parameters in the extension header
      call ftpcom(unit,'***********************************',status)
      if (status .gt. 0)call printerror(lun11,status)

      call ftpcom(unit,'Model Keywords',status)
      if (status .gt. 0)call printerror(lun11,status)

C     Write values to 3 decimal places
      call ftpkye(unit,'RINNER',radin,3,'[cm] Inner shell radius',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      call ftpkye(unit,'ROUTER',radout,3,'[cm] Outer shell radius',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      call ftpkye(unit,'TEMPERAT',temp,3,'[10**4K] Shell Temperature',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      call ftpkye(unit,'PRESSURE',pres,3,'[dynes/cm**2] Shell Pressure',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      if (lpri.ne.0)
     $ write (lun11,*)'after header write'
C-------------------------------------------------------------------
C     Step through the columns and write them to the file
C
C     set 'global' parameters for writing FITS columns
      frow=1
      felem=1


C     column  1  (Line number)
      colnum=1
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum,nlpl
      nlines=nlpl
      call ftpclj(unit,colnum,frow,felem,nlines,ntptr,status) 
      if (status .gt. 0)call printerror(lun11,status)
 

C     column  2  (wavelength)
      colnum=2
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,elsv,status)  
      if (status .gt. 0)call printerror(lun11,status)
      if (status .gt. 0) stop
 

C     column  3  (Ion)
      colnum=3
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nlines,kion,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  4 (lower Level Designation)
      colnum=4
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nlines,klevl,status)
      if (status .gt. 0)call printerror(lun11,status) 

C     column  5 (Level Designation)
      colnum=5
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nlines,klevu,status)
      if (status .gt. 0)call printerror(lun11,status) 

C----------------------------------------------------------------

C     column  6
      colnum=6
      do ll=1,nlines
         rwrk1(ll)=0.
         if (ntptr(ll).ne.0) 
     $      rwrk1(ll)=rcem(1,ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)  
      if (status .gt. 0)call printerror(lun11,status)
 
C     column  7
      colnum=7
      do ll=1,nlines
         rwrk1(ll)=0.
         if (ntptr(ll).ne.0) 
     $    rwrk1(ll)=rcem(2,ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)  
      if (status .gt. 0)call printerror(lun11,status)
 

C     column  8
      colnum=8
      do ll=1,nlines
         rwrk1(ll)=0.
         if (ntptr(ll).ne.0) 
     $    rwrk1(ll)=oplin(ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)  
      if (status .gt. 0)call printerror(lun11,status)
 

C     column  9
      colnum=9
      do ll=1,nlines
         rwrk1(ll)=0.
         if (ntptr(ll).ne.0) 
     $    rwrk1(ll)=tau0(1,ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)  
      if (status .gt. 0)call printerror(lun11,status)
 
C     column  10
      colnum=10
      do ll=1,nlines
         rwrk1(ll)=0.
         if (ntptr(ll).ne.0) 
     $    rwrk1(ll)=tau0(2,ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write (lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nlines,rwrk1,status)  
      if (status .gt. 0)call printerror(lun11,status)
 

c----------------------------------------------------------------
C     Compute checksums
      call ftpcks(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Write Data for each radial zone to an individual extension
C
      subroutine fstepr(unit,hdunum,radin,radout,temp,pres,nrhdim,
     $                idat1,rdat1,kdat1,nptrs,npnxt,npfi,
     $                npfirst,npar,npcon,npconi,npilev,npilevi,npconi2,
     $                xilev,cemab,opakab,tauc,poptol,nloopctl,
     $                nzone,lun11,status)
C
C     Append a FITS extension binary table containing 
C     nrhs columns and at most nrhdim rows
C
C     Parameters:
C        unit    integer            File unit number
C        hdunum  integer            Number of last HDU written
C        radin   real               inner radius of shell
C        radout  real               outer radius of shell
C        temp    real               temperature of shell
C        pres    real               pressure in shell
C        nrhdim  integer            Maximum number of rows
C        idat1   integer(nidat1)    Needed by the atomic database
C        rdat1   real(nidat1)       Needed by the atomic database
C        kdat1   char*nidat1        Needed by the atomic database
C        nptrs                      Needed by the atomic database
C        npnxt                      Needed by the atomic database
C        npfi                       Needed by the atomic database
C        npfirst                    Needed by the atomic database
C        npcon                      Needed by the atomic database
C        npconi                     Needed by the atomic database
C        npcon2                     Needed by the atomic database
C        xilev   real(nrhdim)       Fractional level population array
C        cemab   real(2,nrhdim)     Recombination emission 
C        opakab  real(nrhdim)       Opacity
C        tauc    real(2,nrhdim)     Optical depth
C        poptol  real               Tolerance for population level
C        nloopctl integer           Loop control variable
C        nzone   integer            Pass number through iteration process
C        status  integer            Returned status code
C
      integer ntyp, ncn, nnnl, nnml, nni, nl, ndl, nd, mllz
      integer nidat1, nrdat1, nkdat1, ndat2, nptt,nrhdim

      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)

C     Allocation for passed parameters
      real xilev(nrhdim), opakab(nrhdim)
      real tauc(2,nrhdim), cemab(2,nrhdim)
      real rdat1(nrdat1), poptol
      real radin, radout, temp, pres
      integer unit,hdunum, nrows, status, nloopctl, nzone
      integer idat1(nidat1),nptrs(nptt,ndat2)

c     pointers to master data 
      integer npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni),npar(ndat2)
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)

C     Internal work areas
      real rwrk1(nnml), elev(nnml)
      integer ntptr(nnml)
      integer natomic(nnml), mllev(nnml),nupper(nnml)
      character(10) kion(nnml)
      character(20) klev(nnml)
      integer tfields,varidat
      character(16) ttype(13),tform(13),tunit(13)
      integer colnum,frow,felem,hdutype,ll, klel, mlel, jk, ltyp
      integer lrtyp, lcon, nrdt, nidt, mmlv, mm, lun11, lpril,lpri
      integer mllel, klion, mlion, jkk, kl, nlev
      integer mt2, mlleltp, nnz, mltype, mll, mlc, nions
      character(30) extname, ktmp2
      character(1) kdat1(nkdat1)

C     Database manipulation quantities
      real rdat(20000), xeltp,rdatl(20000)
      integer idat(20000), nkdt
c      integer lpridread
      character(1) kdat(20000),kdati(20000),kdatl(20000)
     $  ,kblnk,kblnk20000(20000)

      data kblnk/' '/
c
      data tform/'1J','1I','1E','8A','1I','20A','1E','1E','1E',
     $ '1E','1E','1E','1I'/

      data ttype/'index','ion_index','e_excitation','ion',
     $ 'atomic_number','ion_level','population','emiss_inward', 
     $ 'emiss_outward','opacity','tau_in','tau_out','upper index'/

      data tunit/' ',' ','eV',' ',' ',' ',' ','erg/cm^3/s',
     $ 'erg/cm^3/s',' ',' ',' ',' '/

      varidat=0
c
c
      lpril=0
      lpri=lpril

C     Move to the last HDU (hdunum) in the file
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Moving to end-of-FITS file'
      call ftmahd(unit,hdunum,hdutype,status)
      if (status .gt. 0)call printerror(lun11,status)

C     append a new empty extension after the last HDU
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Create the new extension'
      call ftcrhd(unit,status)
      if (status .gt. 0)call printerror(lun11,status)
      
C----------------------------------------------------------------
C
C     Extracting data from the Atomic Database here
C
c       lpril=0
C      First look for element data (jk is element index)
       klel=11
       mlel=npfirst(klel)
       nions=0
C      initialize line counter
       mmlv=0
       jk=0
 1040    jk=jk+1
         mt2=mlel-1
         call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mt2,
     $      idat1,rdat1,kdat1,nptrs,lpril,lun11)
         mllel=idat(nidt)
         xeltp=rdat(1)
         nnz=idat(1)
C        ignore if the abundance is small
         if (xeltp.lt.1.e-10) then
            jkk=jkk+nnz
            go to 1079
            endif
c        now step thru ions (jkk is ion index)
         klion=12
         mlion=npfirst(klion)
         jkk=0
         kl=0
 1141      continue
           jkk=jkk+1
C          retrieve ion name from kdati
           call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdati,mlion-1,
     $      idat1,rdat1,kdat1,nptrs,lpril,lun11)
C          if not accessing the same element, skip to the next element       
           mlleltp=idat(nidt-1)
           if (lpri.ne.0) write (lun11,*)idat(nidt),mlleltp,mllel
           if (mlleltp.ne.mllel) go to 1049

           kl=kl+1
           do mm=nkdt+1,8
              kdati(mm)=kblnk
              enddo  

           mltype=7
           mlrdesc=mltype
           ml=npfi(mltype,jkk)
           mllz=npar(ml)
           do while ((ml.ne.0).and.(npar(ml).eq.mllz))
             call dread(ltyp,lrtyp,lcon,
     $         nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $         idat1,rdat1,kdat1,nptrs,0,lun11)
             idest1=idat(nidt-1)
             idest2=idat(nidt-3)
             kkkl=npconi2(ml)           
             if ((kkkl.ne.0).and.(kkkl.le.ndat2)
     $         .and.(idest1.gt.0)) then
c               write (lun11,*)'continuum number:',kkkl,idest1,ml,mllz
               ml2=npfi(13,jkk)
               do while ((idest1.ne.itst).and.(mllz.eq.npar(ml2))
     $                 .and.(npnxt(ml2).ne.0))
                 do mm=1,20
                   kdatl(mm)=kblnk
                   enddo
                 call dread(ltyp,lrtyp,lcon,
     $             nrdt,rdatl,nidt,idat,nkdtl,kdatl,ml2-1,
     $             idat1,rdat1,kdat1,nptrs,0,lun11)
                 itst=idat(nidt-1)
c                 write (lun11,*)ml2,idest1,itst,mllz,npar(ml2)
                 ml2=npnxt(ml2)
                 enddo
               if (idest1.eq.itst) then
C                Extract level label & energy
                 nions=nions+1
                 mllev(nions)=idat(nidt-1)
C                Note that rwrk1 must be written to the file before
C                it is overwritten in subsequent columns
                 mmlv=npilev(itst,jkk)
                 rwrk1(nions)=xilev(mmlv)
                 elev(nions)=rdatl(1)
                 ntptr(nions)=kkkl
                 natomic(nions)=nnz
                 nupper(nions)=idest2
                 write(kion(nions),'(10a1)')(kdati(mm),mm=1,10)
                 write(klev(nions),'(20a1)')(kdatl(mm),mm=1,20)
                 if (lpri.ne.0) write (lun11,*)nions,xilev(mmlv),
     $                         rdat(1),nnz,mmlv,kkkl
                 endif
               endif
             ml=npnxt(ml)
             enddo

 1049      continue
           mlion=npnxt(mlion)
C          Go to next ion
           if ((mlion.ne.0).and.(kl.lt.nnz)) go to 1141

 1079    continue
         mlel=npnxt(mlel)
C        Go to next element
         if (mlel.ne.0) go to 1040
c

C     End of atomic database extraction
C----------------------------------------------------------------
C     define parameters for the binary table (see the above data statements)
      nrows=nions
      tfields=13
C     Build extension name
      extname='XSTAR_RADIAL'
      if(nloopctl.gt.0) then
          write(ktmp2,'(I4.4)')nloopctl
          extname='XSTAR_RADIAL_' // ktmp2
          endif
      
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Write table headers'
C     write the required header parameters for the binary table
      call ftphbn(unit,nrows,tfields,ttype,tform,tunit,extname,
     $              varidat,status)
      if (status .gt. 0)call printerror(lun11,status)

      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Add some more keywords'

C     Write some model parameters in the extension header
      call ftpcom(unit,'***********************************',status)
      if (status .gt. 0)call printerror(lun11,status)

      call ftpcom(unit,'Model Keywords',status)
      if (status .gt. 0)call printerror(lun11,status)

C     Write values to 3 decimal places
      call ftpkye(unit,'RINNER',radin,3,'[cm] Inner shell radius',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      call ftpkye(unit,'ROUTER',radout,3,'[cm] Outer shell radius',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      call ftpkye(unit,'TEMPERAT',temp,3,'[10**4K] Shell Temperature',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

      call ftpkye(unit,'PRESSURE',pres,3,'[dynes/cm**2] Shell Pressure',
     $ status)
      if (status .gt. 0)call printerror(lun11,status)

C-------------------------------------------------------------------
C     Step through the columns and write them to the file
C
C     set 'global' parameters for writing FITS columns
      frow=1
      felem=1

C     column  1  (Line number)
      colnum=1
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpclj(unit,colnum,frow,felem,nions,ntptr,status) 
      if (status .gt. 0)call printerror(lun11,status)
 
C     column  2 (Level number of this ion)
      colnum=2
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpclj(unit,colnum,frow,felem,nions,mllev,status)  
      if (status .gt. 0)call printerror(lun11,status)

C     column  3  (Energy)
      colnum=3
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nions,elev,status)  
      if (status .gt. 0)call printerror(lun11,status)
 

C     column  4  (Ion)
      colnum=4
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nions,kion,status)
      if (status .gt. 0)call printerror(lun11,status)


C     column  5  (Atomic Number)
      colnum=5
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpclj(unit,colnum,frow,felem,nions,natomic,status)  
      if (status .gt. 0)call printerror(lun11,status)
 

C     column  6 (Level Designation)
      colnum=6
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcls(unit,colnum,frow,felem,nions,klev,status)
      if (status .gt. 0)call printerror(lun11,status) 

C----------------------------------------------------------------
C     column 7 (Level population)
C     rwrk1 can be safely overwritten after this step

      colnum=7
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nions,rwrk1,status)  
      if (status .gt. 0)call printerror(lun11,status)

C     column  8 (inward recombination emissivity)
      colnum=8
      do ll=1,nions
         rwrk1(ll)=cemab(1,ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nions,rwrk1,status)  
      if (status .gt. 0)call printerror(lun11,status)
 
C     column  9 (outward recombination emissivity)
C     Remap ordering from input table to atomic data ordering
      colnum=9
      do ll=1,nions
         rwrk1(ll)=cemab(2,ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nions,rwrk1,status)  
      if (status .gt. 0)call printerror(lun11,status)
 

C     column  10 (opacity)
C     Remap ordering from input table to atomic data ordering
      colnum=10
      do ll=1,nions
         rwrk1(ll)=opakab(ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nions,rwrk1,status)  
      if (status .gt. 0)call printerror(lun11,status)
 

C     column  11 (inward optical depth)
C     Remap ordering from input table to atomic data ordering
      colnum=11
      do ll=1,nions
         rwrk1(ll)=tauc(1,ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nions,rwrk1,status)  
      if (status .gt. 0)call printerror(lun11,status)

C     column 12 (outward optical depth)
C     Remap ordering from input table to atomic data ordering
      colnum=12
      do ll=1,nions 
         rwrk1(ll)=tauc(2,ntptr(ll))
         enddo
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpcle(unit,colnum,frow,felem,nions,rwrk1,status)  
      if (status .gt. 0)call printerror(lun11,status)

C     column  13 (upper level index)
      colnum=13
      if (lpri.ne.0)
     $ write(lun11,*)'fstepr: Writing Column ',colnum
      call ftpclj(unit,colnum,frow,felem,nions,nupper,status)  
      if (status .gt. 0)call printerror(lun11,status)
 
c----------------------------------------------------------------
C     Compute checksums
      call ftpcks(unit,status)
      if (status .gt. 0)call printerror(lun11,status)

      return
      end
      subroutine func1(jkk,kl,nnz,lpri,lun11,lfpi,vturbi,
     $       t,trad,r,xee,xpx,xh1,xh0,
     $       epi,bremsa,bremsint,xiin,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,
     $       rlev,ilev,
     $          nlpt,iltp,nlev,klev,
     $       pirt,rrrt2)
c
c     this routine calculates rates affecting ion balance
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     energy bins
      dimension epi(ncn)
c     continuum optical depths
      dimension bremsa(ncn),bremsint(ncn)
      dimension rlev(10,nd),ilev(10,nd),
     $          nlpt(nd),iltp(nd)
      dimension opakc(ncn),rccemis(2,ncn)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
      dimension xiin(nni)
      dimension pirt(29)
c
      character(1) kdat(20000),klev(100,nd)
      character(48) kdesc2
c
      dimension rdat(20000),idat(20000)
c
c      if (lpri.ne.0)
c     $  write (lun11,*)'in func1, inputs:',t,
c     $         xee,xpx,xnx
c
c     zero temporaries
      tsq=sqrt(t)
      nnzp=nnz+1
      do mm=1,nnzp
        pirt(mm)=0.
        enddo
      rrrt2=0.
      nlev=0
c     now find level data
c     step thru types
      nlevmx=0
      mltype=13
      ml=npfi(mltype,jkk)
      mllz=npar(ml)
c     step thru records of this type
      do while ((ml.ne.0).and.(npar(ml).eq.mllz)) 
        call dread(ltyp,lrtyp,lcon,
     $    nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $    idat1,rdat1,kdat1,nptrs,0,lun11)
        nlevmx=nlevmx+1
        nlev=idat(nidt-1)
 9101   format (1x,'level quantities:',4i6,4(1pe12.5),3i6,8a1)
c       if (lpri.ne.0) write (lun11,9101)
c     $   ml,nlev,ltyp,lrtyp,(rdat(mm),mm=1,4),idat(1),idat(2),
c     $   idat(3),(kdat(mm),mm=1,8)
        if ((nlev.gt.0).and.(nlev.le.nd)) then
          nlpt(nlev)=ml
          iltp(nlev)=ltyp
          do  lk=1,nrdt
            rlev(lk,nlev)=rdat(lk)
            enddo
          do lk=1,nidt
            ilev(lk,nlev)=idat(lk)
            enddo
          do lk=1,nkdt
            klev(lk,nlev)=kdat(lk)
            enddo
          endif
        mlo=ml
        ml=npnxt(ml)
        enddo
      nlev=nlevmx
c     now find all the rates affecting this ion
c     step thru types
      mltype=1
      do while (mltype.lt.ntyp) 
        mlrdesc=mltype
        ml=npfi(mltype,jkk)
        if (((mlrdesc.eq.6).or.(mlrdesc.eq.1).or.(mlrdesc.eq.8)
     $     .or.(mlrdesc.eq.15).or.(mlrdesc.eq.7)).and.(ml.ne.0)) then
          mllz=npar(ml)
          do while ((ml.ne.0).and.(npar(ml).eq.mllz)) 
c           step thru records of this type
            if (nptrs(3,ml).eq.mlrdesc) then
              call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
              if (.not.((mlrdesc.eq.1).and.(ltyp.eq.53)) 
     $          .and.(.not.((mlrdesc.eq.7).and.(idat(nidt-1).ne.1))))
     $           then
c                calculate rates
                 lpriu=lpri
                 lforce=1
                 abund1=0.
                 abund2=0.
                 ptmp1=0.
                 ptmp2=0.
                 call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $                 nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $                 ans3,ans4,idest1,idest2,idest3,idest4,
     $                 abund1,abund2,ptmp1,ptmp2,xpx,opakb1,
     $                      delr,
     $                 opakc,rccemis,lpriu,kdesc2,
     $                 r,t,trad,tsq,xee,xh1,xh0,
     $                 epi,bremsa,bremsint,xii,
     $                 rlev,ilev,nlpt,iltp,nlev,klev,lfpi,lun11,
     $                 idat1,rdat1,kdat1,nptrs,np2,
     $                 npar,npnxt,npfi,npfirst,
     $                 nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $                 npconi2,ncsvn,rates,vsav,idrates,lforce)
                if (idest1.ne.0) then
                  if ((lrtyp.eq.1).or.((lrtyp.eq.7).and.(idest1.eq.1))
     $               .or.(lrtyp.eq.15)) then
                    llo=idest3
                    lup=idest4-idest3
                    pirt(kl+lup)=pirt(kl+lup)+ans1
                    if (lpri.ge.1)
     $                write (lun11,9001)jkk,lrtyp,ltyp,llo,lup+idest3,
     $                      ml,ans1,pirt(kl+lup)
 9001                format (1x,6i6,' ion ',1pe10.3,14x,1pe10.3)
                    endif
                  if ((lrtyp.eq.6).or.(lrtyp.eq.8)) then
                    rrrt2=rrrt2+ans1
                    if (lpri.ge.1)
     $                write (lun11,9002)jkk,lrtyp,ltyp,llo,ml,
     $                      ans1,rrrt2
 9002               format (1x,4i6,6x,i6,' ion ',14x,
     $                                1pe10.3,14x,1pe10.4)
                    endif
                  endif
                endif
              endif
            ml=npnxt(ml)
            enddo
          endif
        mltype=mltype+1
        enddo
c
c
      return
      end
      subroutine func2a(jkk,kl,ilimh,lpriz,lun11,lfpi,vturbi,
     $       t,trad,r,xee,xpx,xh1,xh0,cfrac,
     $       epi,bremsa,bremsint,xiin,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,
     $       rniss,rlev,ilev,
     $          nlpt,iltp,nlev,klev,ajis,cjis,ipmat,ipsv,
     $         rrrtot)
c
c     this routine calculates rates affecting level populations
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension tauc(2,nnml)
      dimension bremsa(ncn),bremsint(ncn)
      dimension xiin(nni)
      dimension rlev(10,nd),ilev(10,nd),
     $          nlpt(nd),iltp(nd)
      dimension opakc(ncn),rccemis(2,ncn)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
      dimension ipsv(29)
c
      character(1) kdat(20000),klev(100,nd),kblnk
      character(48) kdesc2
c
      data kblnk/' '/
c
      dimension rdat(20000),idat(20000)
      dimension rniss(nd)
      dimension ajis(nd,nd),cjis(nd,nd)
c
c      if (lpri.ne.0)
c     $  write (lun11,*)'in func2a, inputs:',t,
c     $         xee,xpx,xnx
c      if (lpri.ne.0) write (lun11,*)'in func2a:',ndo,ipsv
c

      mltype=1
      ml=npfi(mltype,jkk)
      if (ml.le.0) return
      mllz=npar(ml)
      call dread(ltyp,lrtyp,lcon,
     $   nrdt,rdat,nidt,idat,nkdt,kdat,mllz-1,
     $   idat1,rdat1,kdat1,nptrs,0,lun11)
      jkk2=idat(nidt)
      lpri=lpriz
      tsq=sqrt(t)
      if (kl.gt.0) then
        jkinit=jkk-kl+1
        do jktmp=jkinit,jkk-1
          iptmp=ipsv(kl-(jkk-jktmp))
c          if (lpri.ne.0) write (lun11,*)'iptmp=',jktmp,jkk,kl,iptmp
          if (iptmp.ge.0) then
            ml=npfi(mltype,jktmp)
            mlrdesc=mltype
            if (ml.eq.0) go to 30422
              mllz=npar(ml)
c             step thru records of this type
 3043           continue
                if (nptrs(3,ml).ne.mlrdesc) go to 30422
                  call dread(ltyp,lrtyp,lcon,
     $            nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $            idat1,rdat1,kdat1,nptrs,0,lun11)
c                 this is a cheat.  should creat new rate type for Auger, and one for total pi rate.
                  if ((nidt.lt.5).or.(ltyp.eq.53)) go to 3044
                    idesti=idat(nidt-2)
                    if (idesti.ne.jkk2) go to 3044
c                     calculate rates
                      lpriu=lpri              
                      lforce=1
                      abund1=0.
                      abund2=0.
                      ptmp1=0.
                      ptmp2=0.
                      call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $                  nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $                  ans3,ans4,idest1,idest2,idest3,idest4,
     $                  abund1,abund2,ptmp1,ptmp2,xpx,opakb1,
     $                         delr,
     $                  opakc,rccemis,lpriu,kdesc2,
     $                  r,t,trad,tsq,xee,xh1,xh0,
     $                  epi,bremsa,bremsint,xii,
     $                  rlev,ilev,nlpt,iltp,nlev,klev,lfpi,lun11,
     $                  idat1,rdat1,kdat1,nptrs,np2,
     $                  npar,npnxt,npfi,npfirst,
     $                  nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $                  npconi2,ncsvn,rates,vsav,idrates,lforce)
                      llo=idest1+iptmp
                      lup=idest2+ipmat
                      if (kl.eq.ilimh) lup=min(nlev+ipmat,lup)
c                     this is a cheat because I may have erred in the indexing
                      idest3=max(1,idat(nidt-3)-1)
                      if ((idest3.ge.1).and.
     $                  (llo.ne.0).and.(llo.le.nd).and.
     $                  (lup.ne.0).and.(lup.le.nd)) then
c                       ajis(llo,lup)=ajis(llo,lup)+airtmp*ptmp
                        ajis(llo,llo)=ajis(llo,llo)-ans1
c                       ajis(lup,lup)=ajis(lup,lup)-airtmp*ptmp
                        ajis(lup,llo)=ajis(lup,llo)+ans1
                        cjis(llo,lup)=cjis(llo,lup)-ans3*xpx
                        if (lpri.ge.1) then
                          do mn=nkdt+1,20
                            kdat(mn)=kblnk
                            enddo
                          write (lun11,9001)jkk,lrtyp,ltyp,idest1,
     $                    idest2,llo,lup,ml,ans1,ans2,ajis(llo,lup),
     $                    ajis(lup,llo),ajis(llo,llo),ajis(lup,lup),
     $                    (kdat(mmn),mmn=1,20)
 9001                     format (1x,8i6,' auger',6(1pe10.3),20a1)
                          endif
                        endif
 3044                 continue
                  ml=npnxt(ml)
                if ((ml.ne.0).and.(npar(ml).eq.mllz)) go to 3043
30422         continue
            endif
          enddo
        endif
c
c
c        if (lpri.ne.0) 
c     $            write (lun11,*)'rrrtt=',rrrtot,rrrtot2
c
c
      return
      end
      subroutine func2(jkk,kl,ilimh,lpriz,lun11,lfpi,vturbi,
     $       t,trad,r,xee,xpx,xh1,xh0,cfrac,
     $       epi,bremsa,bremsint,xii,
     $       tau0,dpthc,tauc,
     $       oplin,opakc,opakab,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,
     $       rniss,rlev,ilev,nmat,
     $          nlpt,iltp,nlev,klev,ajis,cjis,ipmat,
     $         rrrtot)
c
c     this routine calculates rates affecting level populations
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     line opacities
      dimension oplin(nnnl)
      dimension opakab(nnml)
c     continuum opacities
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension tauc(2,nnml)
      dimension bremsa(ncn),bremsint(ncn)
      dimension xii(nni)
      dimension rlev(10,nd),ilev(10,nd),
     $          nlpt(nd),iltp(nd)
      dimension opakc(ncn),rccemis(2,ncn)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
c
      character(1) kdat(20000),klev(100,nd),kblnk
      character(48) kdesc2
c
      data kblnk/' '/
c
      dimension rdat(20000),idat(20000)
      dimension rniss(nd)
      dimension ajis(nd,nd),cjis(nd,nd)
c
c
c      if (lpri.ne.0)
c     $  write (lun11,*)'in func2, inputs:',t,
c     $         xee,xpx,xnx
c      if (lpri.ne.0) write (lun11,*)'in func2:',ndo
c
c
      lpri=lpriz
      tsq=sqrt(t)
c          zero temporaries
           pirt=0.
           rrrt3=rrrtot
           rrrtot=0.
           rrrtot2=0.
           nlev=0
           do ml1=1,nmat
              rniss(ml1)=0.
              do ll=1,100
                klev(ll,ml1)=kblnk
                enddo
             enddo
c           now find level data
c          step thru types
           nlevmx=0
           mltype=13
           ml=npfi(mltype,jkk)
           mllz=npar(ml)
c          step thru records of this type
 2943      continue
              call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
              nlev=idat(nidt-1)
              nlevmx=max(nlevmx,nlev)
              if ((nlev.gt.0).and.(nlev.le.nd)) then
                nlpt(nlev)=ml
                iltp(nlev)=ltyp
c                if (lpri.ne.0) write (lun11,9101)
c     $            ml,nlev,ltyp,lrtyp,(rdat(mm),mm=1,4),idat(1),idat(2),
c     $            idat(3),(kdat(mm),mm=1,8)
 9101           format (1x,'level quantities:',4i6,4(1pe12.5),3i6,8a1)
                do  lk=1,nrdt
                  rlev(lk,nlev)=rdat(lk)
                  enddo
                do lk=1,nidt
                  ilev(lk,nlev)=idat(lk)
                  enddo
                do lk=1,nkdt
                  klev(lk,nlev)=kdat(lk)
                  enddo
                endif
              ml=npnxt(ml)
              if ((ml.ne.0).and.(npar(ml).eq.mllz)) go to 2943
           nlev=nlevmx
c
c          now find the rates affecting this ion
           mltype=0
 3042      mltype=mltype+1
             mlrdesc=mltype
             if (((mlrdesc.ge.10).and.(mlrdesc.le.13))
     $          .or.(mlrdesc.le.0))
     $              go to 30422
             ml=npfi(mltype,jkk)
             if (ml.eq.0) go to 30422
               mllz=npar(ml)
c              step thru records of this type
 3043          continue
               if (nptrs(3,ml).ne.mlrdesc) go to 30422
               call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
c              calculate rates
               lpriu=lpri              
               lforce=1
               abund1=0.
               abund2=0.
               ptmp1=1.
               ptmp2=0.
                 if (lrtyp.eq.7) then
                   kkkl=npconi2(ml)
                   if ((kkkl.gt.0).and.(kkkl.le.ndat2)) then
                     tau1=tauc(1,kkkl)
                     tau2=tauc(2,kkkl)
                     ptmp1=
     $                  pescv(tau1)*(1.-cfrac)
                     ptmp2=pescv(tau2)*(1.-cfrac)
     $                  +2.*pescv(tau1+tau2)*cfrac
                     ptmp=(ptmp1+ptmp2)
                     call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $                 nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $                 ans3,ans4,idest1,idest2,idest3,idest4,
     $                 abund1,abund2,ptmp1,ptmp2,xpx,opakb1,
     $                         delr,
     $                 opakc,rccemis,lpriu,kdesc2,
     $                 r,t,trad,tsq,xee,xh1,xh0,
     $                 epi,bremsa,bremsint,xii,
     $                 rlev,ilev,nlpt,iltp,nlev,klev,lfpi,lun11,
     $                 idat1,rdat1,kdat1,nptrs,np2,
     $                 npar,npnxt,npfi,npfirst,
     $                 nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $                 npconi2,ncsvn,rates,vsav,idrates,lforce)
                     llo=idest1+ipmat
                     lup=idest2+ipmat
                     if (kl.eq.ilimh) lup=min(nlev+ipmat,lup)
                     llol=idest1
                     lupl=idest2
                     if ((idest1.gt.0).and.(idest2.gt.0).and.
     $                (llo.le.nd).and.(lup.le.nd)) then
c
                       airtmp=ans2
                       rrrt3=rrrt3-airtmp
                       ajis(llo,lup)=ajis(llo,lup)+airtmp
                       ajis(llo,llo)=ajis(llo,llo)-ans1
                       ajis(lup,lup)=ajis(lup,lup)-airtmp
                       ajis(lup,llo)=ajis(lup,llo)+ans1
                       cjis(llo,lup)=cjis(llo,lup)-ans3*xpx
                       cjis(lup,llo)=cjis(lup,llo)+ans4*xpx
                       rrrtot=rrrtot+airtmp
                       if (idest2.le.nlev)
     $                  rrrtot2=rrrtot2+airtmp
c
                       if ((lpri.ge.1))
     $                  write (lun11,9004)jkk,lrtyp,ltyp,idest1,idest2,
     $                  llo,lup,ml,ans1,ans2,ans3,ans4,
     $                  ajis(llo,lup),ajis(lup,llo),
     $                  ajis(llo,llo),ajis(lup,lup),ptmp
9004                   format(1x,8i6,' level',9(1pe10.3),7(1pe10.3))
                       endif
                     endif
                   endif
                 if ((lrtyp.eq.5).or.(lrtyp.eq.40)) then
                   call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $                 nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $                 ans3,ans4,idest1,idest2,idest3,idest4,
     $                 abund1,abund2,ptmp1,ptmp2,xpx,opakb1,
     $                         delr,
     $                 opakc,rccemis,lpriu,kdesc2,
     $                 r,t,trad,tsq,xee,xh1,xh0,
     $                 epi,bremsa,bremsint,xii,
     $                 rlev,ilev,nlpt,iltp,nlev,klev,lfpi,lun11,
     $                 idat1,rdat1,kdat1,nptrs,np2,
     $                 npar,npnxt,npfi,npfirst,
     $                 nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $                 npconi2,ncsvn,rates,vsav,idrates,lforce)
                   llo=idest1+ipmat
                   lup=idest2+ipmat
                   if (kl.eq.ilimh) lup=min(nlev+ipmat,lup)
                   llol=idest1
                   lupl=idest2
                   if ((llo.ne.0).and.(lup.ne.0).and.
     $                 (llo.le.nd).and.(lup.le.nd)) then
                     ajis(llo,lup)=ajis(llo,lup)+ans2
                     ajis(llo,llo)=ajis(llo,llo)-ans1
                     ajis(lup,lup)=ajis(lup,lup)-ans2
                     ajis(lup,llo)=ajis(lup,llo)+ans1
                     if (lpri.ge.1)
     $                write (lun11,9004)jkk,lrtyp,ltyp,idest1,idest2,
     $                llo,lup,ml,ans1,ans2,ans3,ans4,
     $                ajis(llo,lup),ajis(lup,llo),
     $                ajis(llo,llo),ajis(lup,lup)
                     endif
                   endif
                 if ((lrtyp.eq.3).or.(lrtyp.eq.23)) then
                   call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $                 nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $                 ans3,ans4,idest1,idest2,idest3,idest4,
     $                 abund1,abund2,ptmp1,ptmp2,xpx,opakb1,
     $                         delr,
     $                 opakc,rccemis,lpriu,kdesc2,
     $                 r,t,trad,tsq,xee,xh1,xh0,
     $                 epi,bremsa,bremsint,xii,
     $                 rlev,ilev,nlpt,iltp,nlev,klev,lfpi,lun11,
     $                 idat1,rdat1,kdat1,nptrs,np2,
     $                 npar,npnxt,npfi,npfirst,
     $                 nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $                 npconi2,ncsvn,rates,vsav,idrates,lforce)
                   if ((idest1.gt.0).and.(idest2.gt.0).and.
     $               (idest1.le.nlev).and.(idest2.le.nlev).and.
     $               (idest1+ipmat.le.nd).and.(idest2+ipmat.le.nd)) then
                     e1=rlev(1,idest1)
                     e2=rlev(1,idest2)                      
                     if ((e1/(1.e-34+e2)-1.).lt.0.01) then
                       lup=idest2+ipmat
                       llo=idest1+ipmat
                       lupl=idest2
                       llol=idest1
                     else
                       lup=idest1+ipmat
                       llo=idest2+ipmat
                       lupl=idest1
                       llol=idest2
                     endif
                   if (kl.eq.ilimh) lup=min(nlev+ipmat,lup)
                   ajis(llo,lup)=ajis(llo,lup)+ans2
                   ajis(llo,llo)=ajis(llo,llo)-ans1
                   ajis(lup,lup)=ajis(lup,lup)-ans2
                   ajis(lup,llo)=ajis(lup,llo)+ans1
                   if (lpri.ge.1)
     $              write (lun11,9004)jkk,lrtyp,ltyp,idest1,idest2,
     $                llo,lup,ml,ans1,ans2,ans3,ans4,
     $                ajis(llo,lup),ajis(lup,llo),
     $                ajis(llo,llo),ajis(lup,lup)
                     endif
                   endif
                 if ((lrtyp.eq.4).or.(lrtyp.eq.14).or.(lrtyp.eq.9)) then
                   call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $                 nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $                 ans3,ans4,idest1,idest2,idest3,idest4,
     $                 abund1,abund2,ptmp1,ptmp2,xpx,opakb1,
     $                         delr,
     $                 opakc,rccemis,lpriu,kdesc2,
     $                 r,t,trad,tsq,xee,xh1,xh0,
     $                 epi,bremsa,bremsint,xii,
     $                 rlev,ilev,nlpt,iltp,nlev,klev,lfpi,lun11,
     $                 idat1,rdat1,kdat1,nptrs,np2,
     $                 npar,npnxt,npfi,npfirst,
     $                 nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $                 npconi2,ncsvn,rates,vsav,idrates,lforce)
                   if ((idest1.gt.0).and.(idest2.gt.0).and.
     $               (idest1.le.nlev).and.(idest2.le.nlev).and.
     $               (idest2+ipmat.le.nd).and.(idest1+ipmat.le.nd)) then
                     e1=rlev(1,idest1)
                     e2=rlev(1,idest2)
                     if (e1.lt.e2) then
                       lup=idest2+ipmat
                       llo=idest1+ipmat
                       lupl=idest2
                       llol=idest1
                     else
                       lup=idest1+ipmat
                       llo=idest2+ipmat
                       lupl=idest1
                       llol=idest2
                     endif
                     if (kl.eq.ilimh) lup=min(nlev+ipmat,lup)
                     jkkl=nplini(ml)
                     if ((jkkl.lt.nnnl).and.(jkkl.gt.0)) then
                       tau1=tau0(1,jkkl)
                       tau2=tau0(2,jkkl)
                       ptmp1=
     $                  pescl(tau1)*(1.-cfrac)
                       ptmp2=pescl(tau2)*(1.-cfrac)
     $                        +2.*pescl(tau1+tau2)*cfrac
                       ans1=ans1*(ptmp1+ptmp2)
                       ans4=ans4*(ptmp1+ptmp2)
c                      these terms represent radiative exctiation.
c                        must turn them off to attain LTE.
                       ajis(lup,llo)=ajis(lup,llo)+ans2
                       ajis(llo,llo)=ajis(llo,llo)-ans2
                       ajis(lup,lup)=ajis(lup,lup)-ans1
                       ajis(llo,lup)=ajis(llo,lup)+ans1
                       cjis(llo,lup)=cjis(llo,lup)-ans3*xpx
                       cjis(lup,llo)=cjis(lup,llo)+ans4*xpx
                       if ((lpri.ge.1))
     $                  write (lun11,9004)jkk,lrtyp,ltyp,idest1,idest2,
     $                   llo,lup,ml,ans1,ans2,ans3,ans4,
     $                   ajis(lup,llo),ajis(llo,lup),
     $                   ajis(llo,llo),ajis(lup,lup)
     $                  ,tau1,tau2,jkkl,ptmp1,ptmp2
 9009                  format (1x,8i6,' level',6(1pe10.3),i6,2(1pe10.3))
                       endif
                     endif
                 endif
               ml=npnxt(ml)
               if ((ml.ne.0).and.(npar(ml).eq.mllz)) go to 3043
30422          continue
             if (mltype.lt.ntyp) go to 3042
c
          if (lpri.ne.0) 
     $            write (lun11,*)'rrrtt=',rrrtot,rrrtot2
c
c
      return
      end
      subroutine func2i(jkk,lpri,lun11,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npfi,npar,npnxt,nlev)
c
c     this routine counts the levels for each ion
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npfi(ntyp,nni)
      dimension npar(ndat2),npnxt(ndat2)
c
      character(1) kdat(20000)
c
      dimension rdat(20000),idat(20000)
c
c
c          now find level data
c          step thru types
           nlevmx=0
           mltype=13
           ml=npfi(mltype,jkk)
           mllz=npar(ml)
c          step thru records of this type
 2943      continue
              call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
              nlevmx=nlevmx+1
              nlev=idat(nidt-1)
              ml=npnxt(ml)
              if ((ml.ne.0).and.(npar(ml).eq.mllz)) go to 2943
           nlev=nlevmx
c
      return
      end
      subroutine func2l(jkk,kl,lpri,lun11,lfpi,vturbi,
     $       t,trad,r,xee,xpx,xh1,xh0,cfrac,
     $       epi,bremsa,bremsint,xiin,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,
     $       rniss,rlev,ilev,
     $          nlpt,iltp,nlev,klev,ipmat,
     $         rrrtot)
c
c     this routine calculates rates affecting level populations
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension tauc(2,nnml)
      dimension bremsa(ncn),bremsint(ncn)
      dimension xiin(nni)
      dimension rlev(10,nd),ilev(10,nd),
     $          nlpt(nd),iltp(nd)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
c
      character(1) kdat(20000),klev(100,nd),kblnk
c
      data kblnk/' '/
c
      dimension rdat(20000),idat(20000)
      dimension rniss(nd)
c
c
c      if (lpri.ne.0)
c     $  write (lun11,*)'in func2, inputs:',t,
c     $          xee,xpx,xnx
c      if (lpri.ne.0) write (lun11,*)'in func2:',ndo
c
c
      tsq=sqrt(t)
c           now find level data
c          step thru types
           nlevmx=0
           mltype=13
           ml=npfi(mltype,jkk)
           mllz=npar(ml)
c          step thru records of this type
           do while ((ml.ne.0).and.(npar(ml).eq.mllz)) 
              call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
              nlev=idat(nidt-1)
              nlevmx=max(nlevmx,nlev)
              if ((nlev.gt.0).and.(nlev.le.nd)) then
                nlpt(nlev)=ml
                iltp(nlev)=ltyp
 9101           format (1x,'level quantities:',4i6,4(1pe12.5),3i6,8a1)
c                if (lpri.ne.0) write (lun11,9101)
c     $            ml,nlev,ltyp,lrtyp,(rdat(mm),mm=1,4),idat(1),idat(2),
c     $            idat(3),(kdat(mm),mm=1,8)
                do  lk=1,nrdt
                  rlev(lk,nlev)=rdat(lk)
                  enddo
                do lk=1,nidt
                  ilev(lk,nlev)=idat(lk)
                  enddo
                do lk=1,nkdt
                  klev(lk,nlev)=kdat(lk)
                  enddo
                do lk=nkdt+1,20
                  klev(lk,nlev)=kblnk
                  enddo
                endif
              ml=npnxt(ml)
              enddo
           nlev=nlevmx
           call levwk(rniss,bb,lpri,rlev,ilev,
     $            nlpt,iltp,nlev,klev,t,xee,xpx,lun11)

c
      return
      end
      subroutine func3(jkk,jkkl,lpri,lun11,lfpi,vturbi,
     $       t,trad,r,delr,xee,xpx,xh1,xh0,cfrac,xlum,
     $       epi,bremsa,bremsint,xiin,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,
     $       rlev,ilev,
     $          nlpt,iltp,nlev,klev,
     $       xintp,xeltp,rrcor,htt,cll,cllines,clcont,rrrt,
     $       xilev,ipmat,ipmatsv,rniss,
     $       rcem,oplin,rccemis,opakc,cemab,opakab,fline)


c
c     this routine calculates rates affecting emission and 
c        absorption
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     line emissivities
      dimension rcem(2,nnnl)
c     line opacities
      dimension oplin(nnnl)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      dimension rccemis(2,ncn)
c     continuum opacities
      dimension opakc(ncn)
      dimension fline(ncn)
c     level populations
      dimension xilev(ndl)
c     ion abundances
      dimension rlev(10,nd),ilev(10,nd),
     $          nlpt(nd),iltp(nd)
      dimension rniss(nd)
      dimension cemab(2,nnml),opakab(nnml)
      dimension tauc(2,nnml)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
      dimension xiin(nni)
c
      character(1) kdat(20000),klev(100,nd),kblnk
      character(48) kdesc2
c
      dimension rdat(20000),idat(20000)
      dimension ebar(ncn)
c
      data kblnk/' '/
c
      ergsev=1.602197e-12
c      
      lfpi3=2
c
      abundtot=0.
      rrrt=0.
      tsq=sqrt(t)
      r19=r*(1.e-19)
      ebar(ncn)=0.
      tmpp=0.
      do llm=2,ncn
        ll=ncn+1-llm
        tmpo=tmpp
        tmpp=bremsa(ll)/epi(ll)**3
        ebar(ll)=ebar(ll+1)+
     $         (tmpp+tmpo)
     $         *(epi(ll+1)-epi(ll))/2.
        enddo
      flxx=xlum/r19/r19
      do ll=1,ncn
        ebar(ll)=ebar(ll)*(epi(ll)**3)/flxx          
        enddo
c
c      lpri=2
c
      xnx=xpx*xee
      htt=0.
      cll=0.
c     now find level data
c     step thru types
      nlevmx=0
      mltype=13
      ml=npfi(mltype,jkk)
      mllz=npar(ml)
c     step thru records of this type
      do while ((ml.ne.0).and.(npar(ml).eq.mllz)) 
        call dread(ltyp,lrtyp,lcon,
     $    nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $    idat1,rdat1,kdat1,nptrs,0,lun11)
        nlev=idat(nidt-1)
        nlevmx=max(nlevmx,nlev)
c        if (lpri.ne.0) write (lun11,9101)
c     $    ml,nlev,ltyp,lrtyp,(rdat(mm),mm=1,4),idat(1),idat(2),
c     $    idat(3),idat(nidt),(kdat(mm),mm=1,8)
 9101     format (1x,'level quantities:',4i6,4(1pe12.5),4i6,8a1)
        if ((nlev.gt.0).and.(nlev.le.nd)) then
          nlpt(nlev)=ml
          iltp(nlev)=ltyp
          do  lk=1,nrdt
            rlev(lk,nlev)=rdat(lk)
            enddo
          do lk=1,nidt
            ilev(lk,nlev)=idat(lk)
            enddo
          do lk=1,nkdt
            klev(lk,nlev)=kdat(lk)
            enddo
          do lk=nkdt+1,20
            klev(lk,nlev)=kblnk
            enddo
          endif
        ml=npnxt(ml)
        enddo
      nlev=nlevmx           
      if (lpri.ne.0) then
        write (lun11,*)'level populations:'
        do mm=1,nlev
          write (lun11,9022)mm,(klev(ml,mm),ml=1,20),
     $      rlev(1,mm),rlev(2,mm),
     $      xilev(mm+ipmat),rniss(mm+ipmat)
 9022     format (i4,20a1,4(1pe10.3))
          enddo
        endif
c
c     now do other  rates
      lpriu=lpri
      mltype=9
      mlrdesc=mltype
      ml=npfi(mltype,jkk)
      if (ml.ne.0) then
        mllz=npar(ml)
        do while ((ml.ne.0).and.(npar(ml).eq.mllz))
          call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
          idest1=idat(1)
          idest2=idat(2)
          jkkl=nplini(ml)                  
          if ((rdat(1).gt.0.01).and.(jkkl.ne.0)
     $      .and.(idest1.gt.0).and.(idest2.gt.0).and.
     $      (idest1.lt.nlev).and.(idest2.lt.nlev)) then
            e1=rlev(1,idest1)
            e2=rlev(1,idest2)
            eth=abs(e2-e1)
            if (e1.lt.e2) then
                lup=idest2+ipmat
                llo=idest1+ipmat
              else
                lup=idest1+ipmat
                llo=idest2+ipmat
              endif
            abund1=xilev(llo)*xpx*xeltp
            abund2=xilev(lup)*xpx*xeltp
            tau1=tau0(1,jkkl)
            tau2=tau0(2,jkkl)
            ptmp1=
     $        pescl(tau1)*(1.-cfrac)
            ptmp2=pescl(tau2)*(1.-cfrac)+2.*pescl(tau1+tau2)*cfrac
            lforce=1
            call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $        nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $        ans3,ans4,idest1,idest2,idest3,idest4,
     $        abund1,abund2,ptmp1,ptmp2,xpx,opakb1,
     $             delr,
     $        opakc,rccemis,lpriu,kdesc2,
     $        r,t,trad,tsq,xee,xh1,xh0,
     $        epi,bremsa,bremsint,xii,
     $        rlev,ilev,nlpt,iltp,nlev,klev,lfpi3,lun11,
     $        idat1,rdat1,kdat1,nptrs,np2,
     $        npar,npnxt,npfi,npfirst,
     $        nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $        npconi2,ncsvn,rates,vsav,idrates,lforce)
            cemtmp1=abund2*ptmp1*ans4
            cemtmp2=abund2*ptmp2*ans4
            rcsum=ans4*abund2
            cll=cll+rcsum
            clcont=clcont+rcsum
            if ((lpri.ge.1))
     $        write (lun11,9002)jkk,lrtyp,ltyp,idest1,idest2,
     $        llo,lup,ml,ans1,ans2,ans3,ans4,
     $        cemtmp1+cemtmp2,opakb1,eth,
     $        jkkl,cll,htt,lforce
            endif
          ml=npnxt(ml)
          enddo
        endif
c
c     now do other  rates
      mltype=7
      mlrdesc=mltype
      ml=npfi(mltype,jkk)
      mllz=npar(ml)
      do while ((ml.ne.0).and.(npar(ml).eq.mllz))
        call dread(ltyp,lrtyp,lcon,
     $    nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $    idat1,rdat1,kdat1,nptrs,0,lun11)
        idest1=idat(nidt-1)
        idest2=nlev+idat(nidt-3)-1
        kkkl=npconi2(ml)           
c        if (lpri.ge.1) write (lun11,*)idest1,kkkl
        if ((kkkl.ne.0).and.(kkkl.le.ndat2)
     $     .and.(idest1.gt.0)) then
          llo=idest1+ipmat
          lup=idest2+ipmat
          eth=rlev(4,idest1)-rlev(1,idest1)
          abund1=xilev(llo)*xeltp
          abund2=xilev(lup)*xeltp
          if (lup.gt.ipmatsv) then
            lup=min(lup,ipmatsv)
            abund2=0.
            endif
c          if (lpri.ge.1)write (lun11,*)lup,ipmatsv,xilev(llo),llo,
c     $           ipmat,xilev(1+ipmat)
          if ((lup.le.ipmatsv).and.
     $      (xilev(llo)/(1.e-36+xilev(1+ipmat)).gt.1.e-24)) then
            tau1=tauc(1,kkkl)
            tau2=tauc(2,kkkl)
            ptmp1=
     $        pescv(tau1)*(1.-cfrac)
            ptmp2=pescv(tau2)*(1.-cfrac)+2.*pescv(tau1+tau2)*cfrac
            lforce=1
            call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $        nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $        ans3,ans4,idest1,idest2,idest3,idest4,
     $        abund1,abund2,ptmp1,ptmp2,xpx,opakab(kkkl),
     $                  delr,
     $        opakc,rccemis,lpriu,kdesc2,
     $        r,t,trad,tsq,xee,xh1,xh0,
     $        epi,bremsa,bremsint,xii,
     $        rlev,ilev,nlpt,iltp,nlev,klev,lfpi3,lun11,
     $        idat1,rdat1,kdat1,nptrs,np2,
     $        npar,npnxt,npfi,npfirst,
     $        nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $        npconi2,ncsvn,rates,vsav,idrates,lforce)
            htsum=ans3*xpx*abund1
            rcsum=ans4*xpx*abund2
            rrrt=rrrt+xilev(lup)*(ptmp1+ptmp2)*ans2
            abundtot=abundtot+xilev(lup)
            cemtmp1=abund2*xpx*ptmp1*ans4*rrcor
            cemtmp2=abund2*xpx*ptmp2*ans4*rrcor                 
            cemab(1,kkkl)=cemab(1,kkkl)+cemtmp1
            cemab(2,kkkl)=cemab(2,kkkl)+cemtmp2
            htt=htt+htsum
            clcont=clcont+rcsum
            cll=cll+rcsum
            if (lpri.ge.1)
     $        write (lun11,9002)jkk,lrtyp,ltyp,
     $          idest1,idest2,llo,lup,ml,ans1,ans2,
     $          ans3,ans4,cemab(1,kkkl)+cemab(2,kkkl),
     $          opakab(kkkl),eth,
     $          kkkl,cll,htt,lforce
 9002         format (1x,8i6,' h-c ',
     $          7(1pe10.3),i6,2(1pe10.3),i4)
            endif
          endif
        ml=npnxt(ml)
        enddo
c
      mltype=42
      mlrdesc=mltype
      ml=npfi(mltype,jkk)
      if (ml.ne.0) then
        mllz=npar(ml)
        do while ((ml.ne.0).and.(npar(ml).eq.mllz))
          call dread(ltyp,lrtyp,lcon,
     $    nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $    idat1,rdat1,kdat1,nptrs,0,lun11)
          idest1=idat(nidt-1)
          idest2=idat(nidt-2)
          if (idest1.gt.0) then
            llo=idest1+ipmat
            lup=idest2+ipmat
            eth=rlev(4,idest1)-rlev(1,idest1)
            abund1=xilev(llo)*xeltp
            abund2=xilev(lup)*xeltp
            if (lup.gt.ipmatsv) then
              abund2=0.
              lup=ipmatsv
              endif
            if ((lup.le.ipmatsv).and.
     $      (xilev(llo)/(1.e-36+xilev(1+ipmat)).gt.1.e-24)) then
              lforce=1
              call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $        nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $        ans3,ans4,idest1,idest2,idest3,idest4,
     $        abund1,abund2,ptmp1,ptmp2,xpx,opakbb,
     $                  delr,
     $        opakc,rccemis,lpriu,kdesc2,
     $        r,t,trad,tsq,xee,xh1,xh0,
     $        epi,bremsa,bremsint,xii,
     $        rlev,ilev,nlpt,iltp,nlev,klev,lfpi3,lun11,
     $        idat1,rdat1,kdat1,nptrs,np2,
     $        npar,npnxt,npfi,npfirst,
     $        nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $        npconi2,ncsvn,rates,vsav,idrates,lforce)
              htsum=ans3*xpx*abund1
              htt=htt+htsum
              czzz=0.
              if (lpri.ge.1)
     $        write (lun11,9002)jkk,lrtyp,ltyp,
     $          idest1,idest2,llo,lup,ml,ans1,ans2,
     $          ans3,ans4,czzz,opakbb,eth,kkkl,cll,htt,lforce
              endif
            endif
          ml=npnxt(ml)
          enddo
        endif
c
      mltype=1
      mlrdesc=mltype
      ml=npfi(mltype,jkk)
      mllz=npar(ml)
      do while ((ml.ne.0).and.(npar(ml).eq.mllz))
        call dread(ltyp,lrtyp,lcon,
     $    nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $    idat1,rdat1,kdat1,nptrs,0,lun11)
        if (.not.((mlrdesc.eq.1).and.(ltyp.eq.53))
     $      .and.(.not.((mlrdesc.eq.7).and.(idat(nidt-1).ne.1)))) 
     $      then
          if (nidt.gt.3) then
            idest1=idat(nidt-1)
            idest2=nlev+idat(nidt-3)-1
            kkkl=npconi2(ml)           
c            if (lpri.ne.0) write (lun11,*)'kkkl=',kkkl,idest1,ltyp
            if ((kkkl.ne.0).and.(kkkl.le.ndat2)
     $        .and.(idest1.gt.0)) then
              llo=idest1+ipmat
              lup=idest2+ipmat
c              eth=rlev(4,idest1)-rlev(1,idest1)
              eth=rdat(1)
              abund1=xilev(llo)*xeltp
              abund2=xilev(lup)*xeltp
              if (lup.gt.ipmatsv) then
                abund2=0.
                lup=ipmatsv
                endif
c              if (lpri.ne.0) write (lun11,*)lup,ipmatsv,llo,xilev(llo),
c     $               xilev(1+ipmat)
              if ((lup.le.ipmatsv).and.
     $          (xilev(llo)/(1.e-36+xilev(1+ipmat)).gt.1.e-24)) then
                tau1=tauc(1,kkkl)
                tau2=tauc(2,kkkl)
                ptmp1=
     $          pescv(tau1)*(1.-cfrac)
                ptmp2=pescv(tau2)*(1.-cfrac)+2.*pescv(tau1+tau2)*cfrac
                lforce=1
                call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $             nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $             ans3,ans4,idest1,idest2,idest3,idest4,
     $             abund1,abund2,ptmp1,ptmp2,xpx,opakab(kkkl),
     $                  delr,
     $             opakc,rccemis,lpriu,kdesc2,
     $             r,t,trad,tsq,xee,xh1,xh0,
     $             epi,bremsa,bremsint,xii,
     $             rlev,ilev,nlpt,iltp,nlev,klev,lfpi3,lun11,
     $             idat1,rdat1,kdat1,nptrs,np2,
     $             npar,npnxt,npfi,npfirst,
     $             nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $             npconi2,ncsvn,rates,vsav,idrates,lforce)
                htsum=ans3*xpx*abund1
                rrrt=rrrt+xilev(lup)*(ptmp1+ptmp2)*ans2
                abundtot=abundtot+xilev(lup)
                htt=htt+htsum
                if (lpri.ge.1)
     $            write (lun11,9002)jkk,lrtyp,ltyp,
     $            idest1,idest2,llo,lup,ml,ans1,ans2,
     $            ans3,ans4,cemab(1,kkkl)+cemab(2,kkkl),
     $            opakab(kkkl),eth,
     $            kkkl,cll,htt,lforce
                endif
              endif
            endif
          endif
        ml=npnxt(ml)
        enddo
c
c
      mltype=4
      mlrdesc=mltype
      ml=npfi(mltype,jkk)
      if (ml.ne.0) then
        mllz=npar(ml)
        do while ((ml.ne.0).and.(npar(ml).eq.mllz))
             call dread(ltyp,lrtyp,lcon,
     $            nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $            idat1,rdat1,kdat1,nptrs,0,lun11)
             idest1=idat(1)
             idest2=idat(2)
             jkkl=nplini(ml)                  
             if ((rdat(1).gt.0.01).and.(jkkl.ne.0)
     $          .and.(idest1.gt.0).and.(idest2.gt.0).and.
     $          (idest1.lt.nlev).and.(idest2.lt.nlev)) then
               e1=rlev(1,idest1)
               e2=rlev(1,idest2)
               if (e1.lt.e2) then
                   lup=idest2+ipmat
                   llo=idest1+ipmat
                 else
                   lup=idest1+ipmat
                   llo=idest2+ipmat
                 endif
c               if (lpri.ne.0) write (lun11,*)idest1,idest2
               abund1=xilev(llo)*xpx*xeltp
               abund2=xilev(lup)*xpx*xeltp
               tau1=tau0(1,jkkl)
               tau2=tau0(2,jkkl)
               ptmp1=
     $                pescl(tau1)*(1.-cfrac)
               ptmp2=pescl(tau2)*(1.-cfrac)+2.*pescl(tau1+tau2)*cfrac
               lpriu=lpri
               lforce=1
               call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $           nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $           ans3,ans4,idest1,idest2,idest3,idest4,
     $           abund1,abund2,ptmp1,ptmp2,xpx,opakb1,
     $               delr,
     $           opakc,rccemis,lpriu,kdesc2,
     $           r,t,trad,tsq,xee,xh1,xh0,
     $           epi,bremsa,bremsint,xii,
     $           rlev,ilev,nlpt,iltp,nlev,klev,lfpi3,lun11,
     $           idat1,rdat1,kdat1,nptrs,np2,
     $           npar,npnxt,npfi,npfirst,
     $           nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $           npconi2,ncsvn,rates,vsav,idrates,lforce)
c
c
               
               rcem(1,jkkl)=abund2*ans4*ptmp1
               rcem(2,jkkl)=abund2*ans4*ptmp2
c               if (lpri.ne.0) write (lun11,*)jkkl,tau0(1,jkkl),
c     $           tau0(2,jkkl),ptmp1,ptmp2,rcem(1,jkkl),rcem(2,jkkl),
c     $           oplin(jkkl),nbtmp,opakc(nbtmp),abund2,pesc,pbbs,
c     $           xilev(lup),xpx,llo,lup
               cll=cll+rcem(1,jkkl)+rcem(2,jkkl)
               htt=htt+abund1*ans3
               cllines=cllines+rcem(1,jkkl)+rcem(2,jkkl)
               ml3=nplin(jkkl)
               if (ml3.ne.0) then
                 call dread(ltyp,lrtyp,lcon,
     $             nrdt,rdat,nidt,idat,nkdt,kdat,ml3-1,
     $             idat1,rdat1,kdat1,nptrs,0,lun11)
                 elin=rdat(1)
                 ener=12398.54/elin
                 nb1=nbinc(ener,epi)
                 oplin(jkkl)=abund1*opakb1
cc                 if (oplin(jkkl).gt.1.e-24)
cc     $            fline(nb1)=fline(nb1)+abund2*ans4/oplin(jkkl)
cc     $                /(epi(nb1+1)-epi(nb1))/ergsev
                 fline(nb1)=fline(nb1)+abund2*ans4*(ptmp2)
     $                /(epi(nb1+1)-epi(nb1))/ergsev
                 if ((lpri.ge.1))
     $             write (lun11,9002)jkk,lrtyp,
     $               ltyp,idest1,idest2,
     $               llo,lup,ml,ans1,ans2,ans3,ans4,
     $               rcem(1,jkkl)+rcem(2,jkkl),oplin(jkkl),
     $               rdat(1),jkkl,cll,htt,lforce
                 endif
               endif
             ml=npnxt(ml)
             enddo
        endif
c
c
           mltype=14
           mlrdesc=mltype
           ml=npfi(mltype,jkk)
           mllz=npar(ml)
           do while ((ml.ne.0).and.(npar(ml).eq.mllz))
             call dread(ltyp,lrtyp,lcon,
     $            nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $            idat1,rdat1,kdat1,nptrs,0,lun11)
             idest1=idat(nidt-3)
             idest2=idat(nidt-2)
             if ((idest1.gt.0).and.(idest2.gt.0).and.
     $          (idest1.lt.nlev).and.(idest2.lt.nlev)) then
               e1=rlev(1,idest1)
               e2=rlev(1,idest2)
               eth=abs(e2-e1)
               if (e1.lt.e2) then
                   lup=idest2+ipmat
                   llo=idest1+ipmat
                 else
                   lup=idest1+ipmat
                   llo=idest2+ipmat
                 endif
               abund1=xilev(llo)*xpx*xeltp
               abund2=xilev(lup)*xpx*xeltp
               tau1=tau0(1,jkkl)
               tau2=tau0(2,jkkl)
               ptmp1=
     $                pescl(tau1)*(1.-cfrac)
               ptmp2=pescl(tau2)*(1.-cfrac)+2.*pescl(tau1+tau2)*cfrac
               lpriu=lpri
               lforce=1
               call ucalc(ltyp,lrtyp,ml,lcon,jkk,vturbi,
     $           nrdt,rdat,nidt,idat,nkdt,kdat,ans1,ans2,
     $           ans3,ans4,idest1,idest2,idest3,idest4,
     $           abund1,abund2,ptmp1,ptmp2,xpx,opakb1,
     $               delr,
     $           opakc,rccemis,lpriu,kdesc2,
     $           r,t,trad,tsq,xee,xh1,xh0,
     $           epi,bremsa,bremsint,xii,
     $           rlev,ilev,nlpt,iltp,nlev,klev,lfpi3,lun11,
     $           idat1,rdat1,kdat1,nptrs,np2,
     $           npar,npnxt,npfi,npfirst,
     $           nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $           npconi2,ncsvn,rates,vsav,idrates,lforce)
               rcemm=abund2*ans4
               rccemis(1,3)=rccemis(1,3)+
     $                  rcemm/(epi(4)-epi(3)+1.e-24)/ergsev
c               rccemis(2,1)=rccemis(2,1)+
c     $                  rcemm/(epi(2)-epi(1)+1.e-24)/ergsev
c               if (lpri.ne.0) write (lun11,*)jkkl,tau0(1,jkkl),
               cll=cll+rcemm
               clcont=clcont+rcemm
               if ((lpri.ge.1))
     $              write (lun11,9002)jkk,lrtyp,ltyp,
     $               idest1,idest2,llo,lup,ml,ans1,ans2,
     $               ans3,ans4,rcemm,opakb1,eth,
     $               kkkl,cll,htt,lforce
 9001            format (1x,8i6,' h-c ',5(1pe10.3),26x,
     $             2(1pe10.3),i4)
               endif
             ml=npnxt(ml)
             enddo
c
      rrrt=rrrt/max(1.e-24,abundtot)
c
      return
      end
      subroutine func(lpri,lun11,niter,tinf,lfpi,vturbi,critf,
     $       t,trad,r,delr,xee,xpx,abel,cfrac,xlum,p,lcdd,
     $       epi,bremsa,bremsint,
     $       zrems,zremso,
     $       elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       xiin,rrrts,pirts,htt,cll,httot,cltot,hmctot,elcter,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilevt,bilevt,rnist,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,fline)

c
c     this routine steps through data and calculates
c     new version attempts to avoid rates for unabundant ions
c
c     with data structures designed for Lucy's iterative method
c       nsup is a pointer from level n to superlevel N
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
      parameter (ncomp=101)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
      dimension zrems(3,ncn),zremso(3,ncn)
      dimension elum(2,nnnl),elumo(2,nnnl)
c     line emissivities
      dimension rcem(2,nnnl)
c     line opacities
      dimension oplin(nnnl)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      dimension rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      dimension opakc(ncn)
      dimension fline(ncn)
c     level populations
      dimension xilevt(nnml),bilevt(nnml),rnist(nnml)
c     ion abundances
      dimension xiin(nni)
      dimension rrrts(nni),pirts(nni)
      dimension cemab(2,nnml),opakab(nnml)
      dimension elumab(2,nnml),elumabo(2,nnml)
      dimension tauc(2,nnml)
      dimension rlev(10,nd),ilev(10,nd),
     $          nlpt(nd),iltp(nd)
      dimension htt(nni),cll(nni)
      dimension nlevs(nni)
c     element abundances
      dimension abel(13)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
      dimension decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
c
      character(1) kdat(20000),klev(100,nd),kblnk
c
      data kblnk/' '/
c
      dimension rdat(20000),idat(20000)
      dimension rrrt(29),pirt(29),xin(29),xitmp(29)
      dimension ajis(nd,nd),cjis(nd,nd)
      dimension xilev(nd),rniss(nd)
      dimension rrcor(nni),pirtt(29)
      dimension bmat(nd),bmatl(nd)
      dimension rnisl(nd)
      dimension ipsv(29),x(nd),nsup(nd)
c      dimension zwork(nd),ipvt(nd)
c
      lprif=lpri
c      lpritp=lpri
      lpritp=0
      if (lprif.ne.0)
     $  write (lun11,*)'in func, inputs:',t,
     $         xee,xpx,xnx,lcdd,p
c
       if (lcdd.ne.1) 
     $   xpx = p/1.38e-12/amax1(t,1.e-34)
c
      xnx=xpx*xee
      xh0=xpx*xiin(1)
      xh1=xpx*(1.-xiin(1))
c      xh0=xpx*max(0.,1.-xee)
c      xh1=xpx-xh0
c
c      zero emissivitiesd and opacities
       do ll=1,nni
         htt(ll)=0.
         cll(ll)=0.
         xiin(ll)=0.
         enddo
       do ll=1,29
         rrrt(ll)=0.
         pirt(ll)=0.
         enddo
       do ll=1,nnnl
         rcem(2,ll)=0.
         rcem(1,ll)=0.
         oplin(ll)=0.
         enddo
       do ll=1,ncn
         rccemis(1,ll)=0.
         rccemis(2,ll)=0.
         opakc(ll)=0.
         enddo
       do ll=1,nnml
         xilevt(ll)=0.
         bilevt(ll)=0.
         cemab(1,ll)=0.
         cemab(2,ll)=0.
         opakab(ll)=0.
         enddo
       elcter=0.
       httot=0.
       cltot=0.
       clcont=0.
       cllines=0.
       do ll=1,ncn
         fline(ll)=0.
         enddo
c
c      now calculate.  first step thru elements
       ndo=nd
       jkk=0
       jkkl=0
       klel=11
       mlel=npfirst(klel)
       jk=0
       httot=0.
       cltot=0.
       httot2=1.e-36
       cltot2=1.e-36
       crith=1.e-36
       do while ((mlel.ne.0)
     $       .and.((httot2/(httot+1.e-36).gt.crith)
     $          .or.(cltot2/(cltot+1.e-36).gt.crith)))
         call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mlel-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
         mllel=idat(nidt)
         if (lprif.ne.0) 
     $     write (lun11,9339)(kdat(mm),mm=1,nkdt)
 9339      format (1x, ' element:',12a1)
         jk=mllel
         nnz=idat(1)
         nnzp=nnz+1
         xeltp=abel(jk)
         httot2=1.e-36
         cltot2=1.e-36
c
c        now step thru ions first pass: func1
         if (lprif.ne.0) write (lun11,*)' first pass'
         klion=12
         mlion=npfirst(klion)
         jkk=0
         kl=0
         do while ((mlion.ne.0).and.(kl.lt.nnz)) 
           jkk=jkk+1
           call dread(ltyp,lrtyp,lcon,
     $        nrdt,rdat,nidt,idat,nkdt,kdat,mlion-1,
     $        idat1,rdat1,kdat1,nptrs,0,lun11)
           mleltp=npar(mlion)
           if (mleltp.eq.mlel) then
             kl=kl+1
             if (lprif.ne.0) 
     $          write (lun11,9338)(kdat(mm),mm=1,nkdt)
9338         format (1x, ' ion:',8a1)
             if (lprif.ne.0) write (lun11,9328)
             call func1(jkk,kl,nnz,
     $             lpri,lun11,lfpi,vturbi,
     $             t,trad,r,xee,xpx,xh1,xh0,
     $             epi,bremsa,bremsint,xiin,
     $             idat1,rdat1,kdat1,nptrs,np2,
     $             npar,npnxt,npfi,npfirst,
     $             nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $             npconi2,ncsvn,rates,vsav,idrates,
     $             rlev,ilev,
     $             nlpt,iltp,nlev,klev,
     $             pirtt,rrrtt)
             rrcor(jkk)=1.               
             pirtsum=0.
             do mm=kl,nnzp
                 pirtsum=pirtsum+pirtt(mm)
                 enddo
             pirt(kl)=pirtsum
             rrrt(kl)=rrrtt
             endif
           mlion=npnxt(mlion)
           enddo
c
c        do ion balance
         kl2=kl
         call istruc(pirt,rrrt,xin,kl2,lpritp,lun11)
         xisum=0.
         ldir=+1   
         do mm=1,kl
             kl1=mm
             xiin(jkk-kl+kl1)=xin(kl1)   
             xisum=xisum+xin(kl1)
             enddo 
         xipp=1.-xisum
         klp=kl+1
         xin(klp)=xipp
         iliml=0
         ilimh=0
         imax=0
         ximax=0.
         do mm=1,klp
           if (xin(mm).gt.ximax) then
             ximax=xin(mm)
             imax=mm
             endif
           enddo
         llp=imax
         llm=imax
         iliml=imax
         ilimh=imax
         lp=0
         lm=0
         if (imax.ne.klp) then
             lsumt=nlevs(jkk-kl+imax)
           else
             lsumt=0
           endif
         ndtmp=nd
         mmt=0
 1513      mmt=mmt+1
           lsum=lsumt
           iliml=min(iliml,llm)
           ilimh=max(ilimh,llp)
           if ((llp.ge.klp).or.(xeltp*xin(llp).lt.critf)) then
             ldir=-1 
             lp=1
             endif
           if ((llm.le.1).or.(xeltp*xin(llm).lt.critf)) then
             ldir=+1
             lm=1
             endif
           if ((lm.eq.1).or.(lp.eq.1)) go to 1514
           if (xin(llp+1).gt.xin(llm-1)) then
               ldir=+1
             else
               ldir=-1
             endif
 1514      continue
           if ((lp.eq.1).and.(lm.eq.1)) ldir=0
           if (ldir.eq.+1) then
               llp=llp+1
               if (llp.ne.klp) then
                   lsumt=lsum+nlevs(jkk-kl+llp)
                 else
                   lsumt=lsum
                 endif
               endif
           if (ldir.eq.-1) then
               llm=llm-1
               lsumt=lsum+nlevs(jkk-kl+llm)
               endif
          ilimh=max(ilimh-1,iliml+1)
c
c           write (lun11,*)mmt,llp,llm,ldir,lsumt,lsum,lp,lm,
c     $           iliml,ilimh
           if ((lsumt.lt.ndtmp).and.(ldir.ne.0))
     $             go to 1513
         if (lprif.ne.0) then
             write (lun11,*)'ion fractions:',iliml,ilimh,lsum
             write (lun11,*)'ion, pi rate,    rec rate,   fraction'
             do mm=1,kl
               write (lun11,9023)mm,pirt(mm),rrrt(mm),xin(mm)
 9023          format (1x,i4,3(1pe10.2))
               enddo
             endif
c
c        now step thru ions for second pass
         if (lprif.ne.0) write (lun11,*)' second pass'
 9328    format ('     ion    process     d1    d2 ',
     $     '    rec use    ans1      ans2  ',
     $ '   ionization  recombination')
 9329    format ('     ion    process     d1    d2 ',
     $ '             rec use     ans1      ans2     ans3       ans4  ' ,
     $     '  aji(lo,up) aji(up,lo)',
     $     ' aji(lo,lo) aji(up,up)')
 9330    format ('     ion    process     d1    d2 ',
     $  '             rec use     ans1      ans2     ans3       ans4 ',
     $  '  emiss.    opac.     energy   rec   heat      cool    ')
c
         if (lprif.ne.0) write (lun11,*)'zeroing:',nmat
         do ml1=1,nmat
           bmat(ml1)=0.
           nsup(ml1)=0
           do ml2=1,nmat
             ajis(ml1,ml2)=0.
             cjis(ml1,ml2)=0.
             enddo
           enddo
         klion=12
         mlion=npfirst(klion)
         jkk=0
         kl=0
         ipmat=0
         nsp=1
         do while ((mlion.ne.0).and.(kl.lt.nnz)) 
           jkk=jkk+1  
           call dread(ltyp,lrtyp,lcon,
     $        nrdt,rdat,nidt,idat,nkdt,kdat,mlion-1,
     $        idat1,rdat1,kdat1,nptrs,0,lun11)
           mleltp=npar(mlion)
           jkktmp=idat(nidt)
           if (mleltp.eq.mlel) then
             kl=kl+1
             ipsv(kl)=-1
             do mm=nkdt+1,8
               kdat(mm)=kblnk
               enddo
             if (lprif.ne.0) 
     $         write (lun11,9338)(kdat(mm),mm=1,nkdt)
             if (lprif.ne.0) write (lun11,9329)
             if ((kl.ge.iliml).and.(kl.le.ilimh)) then
               if (lpri.ne.0) write (lun11,*)'ipmat=',ipmat
               ipsv(kl)=ipmat
               rrrtt=rrrt(kl)
               call func2(jkk,kl,ilimh,
     $               lpri,lun11,lfpi,vturbi,
     $               t,trad,r,xee,xpx,xh1,xh0,cfrac,
     $               epi,bremsa,bremsint,xin,
     $               tau0,dpthc,tauc,
     $               oplin,opakc,opakab,
     $               idat1,rdat1,kdat1,nptrs,np2,
     $               npar,npnxt,npfi,npfirst,
     $               nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $               npconi2,ncsvn,rates,vsav,idrates,
     $               rniss,rlev,ilev,nmat,
     $               nlpt,iltp,nlev,klev,
     $               ajis,cjis,ipmat,rrrtt)
               call func2a(jkk,kl,ilimh,
     $               lpri,lun11,lfpi,vturbi,
     $               t,trad,r,xee,xpx,xh1,xh0,cfrac,
     $               epi,bremsa,bremsint,xin,
     $               tau0,dpthc,tauc,
     $               idat1,rdat1,kdat1,nptrs,np2,
     $               npar,npnxt,npfi,npfirst,
     $               nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $               npconi2,ncsvn,rates,vsav,idrates,
     $               rniss,rlev,ilev,
     $               nlpt,iltp,nlev,klev,
     $               ajis,cjis,ipmat,ipsv,rrrtt)
c              set up superlevel pointers
               nsup(1+ipmat)=nsp
               nsp=nsp+1
               do mm=2,nlev-1
                 nsup(mm+ipmat)=nsp
                 enddo
               nsp=nsp+1
               if (lpri.gt.2) write (lun11,*)'initial populations'
               do mm=1,nlev
                 x(mm+ipmat)=0.
                 mmtmp=npilev(mm,jkk)
                 xilast=0.
                 if (mmtmp.gt.0) then
                   x(mm+ipmat)=xilevt(mmtmp)
                   if (lpri.gt.2) write (lun11,*)mm,jkk,mmtmp,
     $                              xilevt(mmtmp)
                   xilast=xilevt(mmtmp)
                   endif
                 enddo
               ipmat=ipmat+nlev-1
               if (ipmat.gt.nd) stop 'ipmat too large.  
     $                                Increase critf value.'
               endif
             endif
           mlion=npnxt(mlion)
           enddo
c
         nsup(ipmat+1)=nsp
         xsum=0.
         do mm=1,ipmat
           xsum=xsum+x(mm)
           enddo
c         x(ipmat+1)=max(1.-xsum,0.)
         x(ipmat+1)=xilast
c        put in number conservation        
         ipmat=ipmat+1
c
         nmat=ipmat
         lprim=lpri
c         if (lpri.ge.1) lprim=3
         call remtms(tt1)
         nitmx=100
         nitmx2=100
         call msolvelucy(ajis,cjis,nsup,nsp,ipmat,bmat,x,
     $      ht,cl,nit,nit2,nit3,nitmx,nitmx2,lun11,lprim)
         if (lfpi.gt.2) then
           httot=httot+ht*xeltp
           cltot=cltot+cl*xeltp
           httot2=ht*xeltp
           cltot2=cl*xeltp
           htt(jkk)=ht*xeltp
           cll(jkk)=cl*xeltp
           endif
c         call chisq4(ajis,ipmat,nd,x,bmat,err,lun11,lpri)
         call remtms(tt2)
         if (lpri.gt.0) 
     $    write (lun11,*)'after msolvelucy',err,abs(tt2-tt1),
     $                   nit,nit2,nit3,ht,cl
         do mm=1,ipmat
           bmatl(mm)=x(mm)
           enddo
c
c        now calculate lte abundances
c        step thru ions
         if (lprif.ne.0) write (lun11,*)' calculating lte abundances'
         ipmatsv=ipmat
         ipmat=0
         mlion=npfirst(klion)
         jkk=jkk-nnz
         kl=0
         xintpsum=0.
         do while ((mlion.ne.0).and.(kl.lt.nnz)) 
           ltyp=klion     
           call dread(ltyp,lrtyp,lcon,
     $        nrdt,rdat,nidt,idat,nkdt,kdat,mlion-1,
     $        idat1,rdat1,kdat1,nptrs,0,lun11)
           mleltp=npar(mlion)
           if (mleltp.eq.mlel) then
             kl=kl+1
             jkk=jkk+1
             if ((kl.ge.iliml).and.(kl.le.ilimh)) then
               call func2l(jkk,kl,
     $             lpri,lun11,lfpi,vturbi,
     $             t,trad,r,xee,xpx,xh1,xh0,cfrac,
     $             epi,bremsa,bremsint,xin,
     $             tau0,dpthc,tauc,
     $             idat1,rdat1,kdat1,nptrs,np2,
     $             npar,npnxt,npfi,npfirst,
     $             nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $             npconi2,ncsvn,rates,vsav,idrates,
     $             rniss,rlev,ilev,
     $             nlpt,iltp,nlev,klev,
     $             ipmat,rrrtt)
               nlevm=nlev-1
               rnisum=0.
               do mm=1,nlevm
                 rnisum=rnisum+rniss(mm)
                 enddo
               rniss2=rniss(nlev)
               rrrt(kl)=pirt(kl)*rnisum/(rniss2+1.e-28)
               endif
             endif
           mlion=npnxt(mlion)
           enddo
         call istruc(pirt,rrrt,xitmp,kl2,lpritp,lun11)
         mlion=npfirst(klion)
         jkk=jkk-nnz
         kl=0
         do while ((mlion.ne.0).and.(kl.lt.nnz))
           ltyp=klion     
           call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mlion-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
           mleltp=npar(mlion)
           if (mleltp.eq.mlel) then
             kl=kl+1
             jkk=jkk+1
             if ((kl.ge.iliml).and.(kl.le.ilimh)) then
               call func2l(jkk,kl,
     $               lpri,lun11,lfpi,vturbi,
     $               t,trad,r,xee,xpx,xh1,xh0,cfrac,
     $               epi,bremsa,bremsint,xin,
     $               tau0,dpthc,tauc,
     $               idat1,rdat1,kdat1,nptrs,np2,
     $               npar,npnxt,npfi,npfirst,
     $               nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $               npconi2,ncsvn,rates,vsav,idrates,
     $               rniss,rlev,ilev,
     $               nlpt,iltp,nlev,klev,
     $               ipmat,rrrtt)
               nlevm=nlev-1
               do mm=1,nlevm
                 rnisl(mm+ipmat)=rniss(mm)
     $                *(xitmp(kl)+xitmp(kl+1))
c                 write (lun11,*)mm,rniss(mm),xitmp(kl),xitmp(kl+1)
                 enddo
               ipmat=ipmat+nlev-1
               endif
             endif
           mlion=npnxt(mlion)
           enddo
c
c        step thru ions
         if (lprif.ne.0) write (lun11,*)' third pass',ipmat
         mlion=npfirst(klion)
         ipmat=ipmat+1
         ipmatsv=ipmat
         ipmat=0
         jkk=jkk-nnz
         kl=0
         xintpsum=0.
         do while ((mlion.ne.0).and.(kl.lt.nnz))
           ltyp=klion     
           call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mlion-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
           mleltp=npar(mlion)
           if (mleltp.eq.mlel) then
             kl=kl+1
             jkk=jkk+1
             call func2i(jkk,
     $         lpri,lun11,
     $         idat1,rdat1,kdat1,nptrs,np2,
     $         npfi,npar,npnxt,nlev)
             pirts(jkk)=pirt(kl)
             rrrts(jkk)=rrrt(kl)
             if ((kl.ge.iliml).and.(kl.le.ilimh)) then
               if (lprif.ne.0) 
     $          write (lun11,9338)(kdat(mm),mm=1,nkdt)
               if (lprif.ne.0) write (lun11,9330)
               nlevm=nlev-1
c              retrieve saved abundances
               xintp=0.
               if (lpri.ne.0) write (lun11,*)'level populations:'
               do mm=1,nlevm
                 xilev(mm)=bmatl(mm+ipmat)
                 xintp=xintp+xilev(mm)
                 mmtmp=npilev(mm,jkk)
                 if (mmtmp.gt.0) then
                   rnist(mmtmp)=rnisl(mm+ipmat)
                   xilevt(mmtmp)=bmatl(mm+ipmat)
c                   if (lpri.ne.0) write (lun11,*)mm,mmtmp,
c     $                                  xilevt(mmtmp),jkk
                   endif
                 enddo
               xiin(jkk)=xintp
               xin(kl)=xintp
               mmtmp=npilev(nlev,jkk)
               xilevt(mmtmp)=bmatl(ipmat+nlev)
               rnist(mmtmp)=rnisl(ipmat+nlev)
c              this is an ungraceful solution to this problem
               xintp2=bmatl(ipmat+nlev)
               if (kl.lt.nnz)
     $            xintp2=xintp2
     $               +bmatl(ipmat+nlev+1)
     $               +bmatl(ipmat+nlev+2)
               xilev(nlev)=xintp2
               rrrts(jkk)=pirts(jkk)*xintp/(xintp2+1.e-28)
               if (lpri.ne.0) write (lun11,*)kl,nnz,ipmat,nlev,
     $           bmatl(ipmat+nlev),bmatl(ipmat+nlev+1),
     $           bmatl(ipmat+nlev+2)
               if (lpri.ne.0) write (lun11,*)nlev,xilev(nlev),
     $              jkk,pirts(jkk),rrrts(jkk),xintp,xintp2
               xintp=1.
c              now do heating-cooling
c              now find all the rates affecting this ion
               if (lpri.ne.0) write (lun11,*)'ipmat=',ipmat
c
c
               if (lfpi.le.2) then
                 call func3(jkk,jkkl,
     $             lpri,lun11,lfpi,vturbi,
     $             t,trad,r,delr,xee,xpx,xh1,xh0,cfrac,xlum,
     $             epi,bremsa,bremsint,xiin,
     $             tau0,dpthc,tauc,
     $             idat1,rdat1,kdat1,nptrs,np2,
     $             npar,npnxt,npfi,npfirst,
     $             nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $             npconi2,ncsvn,rates,vsav,idrates,
     $             rlev,ilev,
     $              nlpt,iltp,nlev,klev,
     $             xintp,xeltp,rrcor(jkk),httmp,cltmp,cllines,clcont,
     $                 rrrtdum,
     $             bmatl,ipmat,ipmatsv,rnisl,
     $             rcem,oplin,rccemis,opakc,cemab,opakab,fline)
                 cll(jkk)=cltmp
                 cltot=cltot+cltmp
                 htt(jkk)=httmp
                 httot=httot+httmp
                 cltot2=cltot2+cltmp
                 httot2=httot2+httmp
                 endif
c
               ipmat=ipmat+nlev-1
               endif
             endif
c
           mlion=npnxt(mlion)
           enddo
c
         xisum=0.
         do kl1=1,kl
           xisum=xisum+xin(kl1)
           enelec=float(kl1-1)
           elcter=elcter+xin(kl1)*enelec*xeltp
           enddo
         enelec=float(kl)
         elcter=elcter+max(0.,1.-xisum)*enelec*xeltp
c
         mlel=npnxt(mlel)

         enddo
c
       lpril=0
       call comp2(lpril,lun11,epi,bremsa,t,
     $   r,decomp,ecomp,sxcomp,cmp1,cmp2)
c       call comp(lpri,lun11,epi,bremsa,r,cmp1,cmp2)
       call bremem(lpril,lun11,xee,xpx,t,epi,brcems)
       call heatt(jkk,jkkl,lpri,lun11,lfpi,
     $       t,trad,r,delr,xee,xpx,abel,xh1,xh0,cfrac,xlum,
     $       epi,bremsa,bremsint,xiin,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,
     $       rlev,ilev,
     $          nlpt,iltp,nlev,klev,
     $       zrems,zremso,
     $       elumab,elumabo,elum,elumo,
     $       xintp,xeltp,rrcor,htt,cll,rrrt,
     $       xilev,ipmat,ipmatsv,rniss,
     $       rcem,oplin,rccemis,opakc,cemab,opakab,fline,
     $       brcems,cmp1,cmp2,httot,cltot,hmctot,
     $             cllines,clcont,htcomp,clcomp,clbrems)
c
       elcter=xee-elcter
c     
      if (lprif.ne.0) write (lun11,*)'leaving func'
c
c
      return
      end
      subroutine fwrtascii(unit,extname,rdati,ncol,nrhdim,
     $                      nidat1,klabs, kform, kunits,lun11)

c     write an ascii table extension containing 
c     ncol columns and nidat1 rows
c
c     parameters:
c        unit    integer            file unit number
c        extname char*30            name of the ascii extension
c        rdati   real(ncol*nidat1)  data array
c        ncol    integer            number of columns
c        nrhdim  integer            maximum number of rows & columns
c        nidat1  integer            actual number of rows
c        klabs   char*16(ncol)      column labels
c        kform   char*16(ncol)      column numeric format
c        kunits  char*15(ncol)      column units
c 
c     modifications:
c        1998/12/17, wtb: fix fits keyword format problem.  enhanced
c                    parameter list for more flexibility.
c        1999/01/04, wtb: added file creation date, model name, creator
c                    code & checksum
c        1999/01/25, wtb: convert this routine so it just writes an
c                    ascii table extension.
c 
      implicit none
      integer nrhmx,ncn,nrhmx1
      parameter (nrhmx1=999)
      parameter (nrhmx=2999)
      parameter (ncn=9999)

c     passed parameters
      real rdati(nrhmx1,nrhmx),rdat(nrhmx)
      character(16) klabs(nrhmx), kform(nrhmx), kunits(nrhmx)
      integer ncol, nrhdim, nidat1
      character(30) extname

      integer unit, status, tfields, nrows, rowlen, verbose,lun11
      integer nspace,tbcol(nrhmx),colnum,frow,felem,kk,ll

      status=0
      verbose=0
      tfields=ncol
      nrows=nidat1
      nspace=1
      rowlen=0
      tbcol(1)=0
c     append a new empty extension onto the end of the primary array
      call ftcrhd(unit,status)

      if(verbose.gt.0) write(6,*)'fwrtascii: writing header table'
c     write the required header parameters for the ascii table
      call ftphtb(unit,rowlen,nrows,tfields,klabs,tbcol,kform,kunits,
     &            extname,status)
      if (status .gt. 0)call printerror(lun11,status)
c
c     map each column to a 1-d array before writing to the file
      do kk=1,tfields
        if(verbose.gt.0) write(6,*)'fwrtascii: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nidat1
          rdat(ll)=rdati(kk,ll)
          enddo
        if(verbose.gt.0) write(6,*)'fwrtascii: writing column ',kk
        call ftpcle(unit,colnum,frow,felem,nrows,rdat,status)  
        enddo
      if (status .gt. 0)call printerror(lun11,status)

c     compute checksums
      if(verbose.gt.0) write(6,*)'fwrtascii: writing checksum'
      call ftpcks(unit,status)
c     check for any error, and if so print out error messages
      if (status .gt. 0)call printerror(lun11,status)
      end
c *********************************************************************
      subroutine grcoef(temp,den,ic,nq,grc,gnc,lun11)      
c 
c  this rutine takes the cascade coefficients data as read from     
c  xh.xstar and returns the cascade down coefficients in (cm^3 s^-1)
c  for the conditions t=temp and ne=den. nq is the principal
c  quantum number (n) of the level considered.
c     ic      : nuclear charge of the ion (z)
c     grc(36) : data values read from xh.xstar
c     gnc     : cascade coefficient given in return
c  
c *********************************************************************

      dimension grc(50)
      ener=1.5782e+5*ic*ic/float(nq*nq)
      if (den.lt.grc(1)) then                  
       id=10
       tt=log10(temp)
       rflex=-grc(id+1)/grc(id+2)*.5
       if (tt.le.rflex .and. grc(id+2).lt.0.) then
        print*,'temp=',temp,'ne=',den,'z=',ic,'n=',nq  
        print*,'temperature too low for grcoef'
        stop
       endif
       if (tt.ge.rflex .and. grc(id+2).gt.0.) then
       print*,'temp=',temp,'ne=',den,'z=',ic,'n=',nq  
        print*,'temperature too high for grcoef'
        stop
       endif
       gnc=10.**(grc(id)+grc(id+1)*tt+grc(id+2)*tt*tt)
       return
      endif
      if (den.gt.grc(9)) then
        print*, 'density too high (routine grcoef) !'
        stop
      endif
      in=0
 5    in=in+1
      if (den.gt.grc(in+1)) goto 5

      tt=log10(temp)
      id=10+(in-1)*3
c
       rflex=-grc(id+1)/grc(id+2)*.5
       rfley=-grc(id+4)/grc(id+5)*.5
       if ((tt.le.rflex .and. grc(id+2).lt.0.) .or. 
     c     (tt.le.rfley .and. grc(id+5).lt.0.)) then
        print*,'temp=',temp,'ne=',den,'z=',ic,'n=',nq  
        print*,'temperature too low for grcoef'
        stop
       endif
       if ((tt.ge.rflex .and. grc(id+2).gt.0.) .or. 
     c     (tt.ge.rfley .and. grc(id+5).gt.0.)) then
       print*,'temp=',temp,'ne=',den,'z=',ic,'n=',nq  
        print*,'temperature too high for grcoef'
        stop
       endif
c
      cnl=(grc(id)+grc(id+1)*tt+grc(id+2)*tt*tt)
      cdl=(grc(id+3)+grc(id+4)*tt+grc(id+5)*tt*tt)
      ne0=log10(den)
      ne1=log10(grc(in))
      ne2=log10(grc(in+1))  
      mn=(cdl-cnl)/(ne2-ne1)
      gnc=10.**(cnl+mn*(ne0-ne1))
      return
      end
c--------------------------------------------------------------------
      SUBROUTINE GULL1(N,RS,GUS,GLS,lpri,lun11)
C
C THIS SUBROUTINE CALCULATES THE VALUE OF |G(N,L;R,L')|**2
C GIVEN N THE PRINCIPAL QN AND R FOR ALL L=[O,N-1] AND
C L'=L+1 OR L'=L-1.  REF BURGESS (1964), BROCKELHURST (1971)
C
      REAL*8 CN,CLU,CLL,G0,GU(100),GL(100),FN,PI,S,r
      DIMENSION GUS(100),GLS(100)
      R=DBLE(RS)
      PI=2.*DACOS(0.D+0)
      N1=2*N-1
      CALL FACT(N1,F1)
      G0=0.5*DLOG(PI/2.)+ALOG(8.*N)+N*ALOG(4.*N)-F1
        IF(R.EQ.0.) THEN
        GU(N)=G0-2.*N
        ELSE
      S=DSQRT(R)
      GU(N)=G0-2.*DATAN(N*S)/S-0.5*DLOG(1.-DEXP(-2.*PI/S))
        END IF
      GU(N)=DEXP(GU(N))
C
      FN=1.D-300/GU(N)
      GU(N)=GU(N)*FN
C
      IF(N.EQ.1) GO TO 40
      GU(N-1)=(2.*N-1.)*(1.+N*N*R)*N*GU(N)
      GL(N)=(1.+N*N*R)*GU(N)/(2.*N)
      GL(N-1)=(2.*N-1.)*(4.+(N-1)*(1.+N*N*R))*GL(N)
C
      DO 10 L=N-1,3,-1
      GU(L-1)=(4*N*N-4*L*L+L*(2*L-1)*(1+N*N*R))*GU(L)
      GU(L-1)=GU(L-1)-4*N*N*(N-L)*(N+L)*(1+(L+1)*(L+1)*R)*GU(L+1)
      GL(L-1)=(4*N*N-4*(L-1)*(L-1)+(L-1)*(2*L-1)*(1+N*N*R))*GL(L)
      GL(L-1)=GL(L-1)-4*N*N*(N-L)*(N+L)*(1+(L-1)*(L-1)*R)*GL(L+1)
 10    CONTINUE
      GL(1)=0.
      GU(1)=(4*N*N-16+6*(1+N*N*R))*GU(2)
      GU(1)=GU(1)-4*N*N*(N-2)*(N+2)*(1+9*R)*GU(3)
C
      CN=LOG(FLOAT(N))-N*LOG(4.*N*N)-(2.*N+4)*LOG(1.+N*N*R)
      GU(1)=CN+dLOG(1.d0+R)+2.*DLOG(GU(1))-2.*DLOG(FN)
      CLU=CN+dLOG(1.d0+R)
      CLL=CN
C
      DO 30 L=1,N-1
      CLU=CLU+DLOG(DBLE(4*N*N*(N-L)*(N+L)*(1+(L+1)*(L+1)*R)))
      CLL=CLL+DLOG(DBLE(4*N*N*(N-L)*(N+L)*(1+(L-1)*(L-1)*R)))
      GU(L+1)=CLU+2.*DLOG(GU(L+1))-2.*DLOG(FN)
      GL(L+1)=CLL+2.*DLOG(GL(L+1))-2.*DLOG(FN)
 30    CONTINUE
      GO TO 60
C
 40    GL(1)=0.
      GU(1)=2.*DLOG(GU(1))-ALOG(4.)-5.*dLOG(1.d0+R)-2.*DLOG(FN)
      if (lpri.ne.0)
     $ write (lun11,*)'in gull1:',r,fn,gu(1),gu(2),gl(1),gl(2)
c converts results to single precision to give in return
       do l=1,100
        gus(l)=sngl(gu(l))
        gls(l)=sngl(gl(l))
       enddo
      
 60    RETURN
      END
      subroutine heatt(jkk,jkkl,lpri,lun11,lfpi,
     $       t,trad,r,delr,xee,xpx,abel,xh1,xh0,cfrac,xlum,
     $       epi,bremsa,bremsint,xiin,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,
     $       rlev,ilev,
     $          nlpt,iltp,nlev,klev,
     $       zrems,zremso,
     $       elumab,elumabo,elum,elumo,
     $       xintp,xeltp,rrcor,htt,cll,rrrt,
     $       xilev,ipmat,ipmatsv,rniss,
     $       rcem,oplin,rccemis,opakc,cemab,opakab,fline,
     $       brcems,cmp1,cmp2,httot,cltot,hmctot,
     $             cllines,clcont,htcomp,clcomp,clbrems)
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nni,nd),npilevi(nnml)
      dimension zrems(3,ncn),zremso(3,ncn)
      dimension elum(2,nnnl),elumo(2,nnnl)
      dimension abel(13)
c     line emissivities
      dimension rcem(2,nnnl)
c     line opacities
      dimension oplin(nnnl)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      dimension rccemis(2,ncn)
c     continuum opacities
      dimension opakc(ncn)
c     level populations
      dimension xilev(ndl)
c     ion abundances
      dimension rlev(10,nd),ilev(10,nd),
     $          nlpt(nd),iltp(nd)
      dimension rniss(nd)
      dimension cemab(2,nnml),opakab(nnml)
      dimension elumab(2,nnml),elumabo(2,nnml)
      dimension tauc(2,nnml)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
      dimension xiin(nni)
      dimension brcems(ncn),fline(ncn)
c
      character(1) kdat(20000),klev(100,nd),kblnk,kdati(20000)
c
      dimension rdat(20000),idat(20000)
c
      data kblnk/' '/
c
      data ergsev/1.602197e-12/
c     
      xnx=xpx*xee
      lprisv=lpri
      if (lpri.ge.1) lpri=2
      if (lpri.gt.1) write (lun11,*)'in heatt',httot,cltot,delr,r
      if (lpri.gt.1) write (lun11,*)ncsvn
      numcon=ncn
      r19=r*(1.e-19)
      fpr2=12.56*r19*r19
      r19=r*(1.e-19)
      fpr2=12.56*r19*r19
      
      clbrems=0.
      lskp=1
      tmp2=0.
      do kl=1,numcon
        tmp2o=tmp2
        tmp2 =brcems(kl)
        if ( kl.ge.2 ) clbrems=clbrems+(tmp2+tmp2o)
     &                *(epi(kl)-epi(kl-lskp))*ergsev/2.
        enddo
c
      if (lfpi.le.2) then
        httot=0.
        cltot=0.
        do kl=1,numcon
          optpp=opakc(kl)
          optp2=max(1.e-34,optpp)
          deltau=optp2*delr
c         for outward only
          epiio=epii
          epii=epi(kl)
          tmpho=tmph
          tautmp=optp2*delr
          fac=delr
          if (tautmp.gt.0.1)
     $      fac=(1.-expo(-tautmp))/(1.e-24+optp2)
          tmph=bremsa(kl)*optp2
          tmpco=tmpc
c          tmpc=(rccemis(1,kl)+fline(kl))
          tmpc1=rccemis(1,kl)
          tmpc2=rccemis(2,kl)
          tmpc=tmpc1+tmpc2
          zrems(1,kl)=max(0.,zremso(1,kl)
     $                +(-tmph+tmpc+fline(kl))*fac*fpr2)
          zrems(2,kl)=zremso(2,kl)+tmpc1*fac*fpr2
          zrems(3,kl)=zremso(3,kl)+tmpc2*fac*fpr2
          if (kl.gt.1) then
            httot=httot+(tmph+tmpho)
     $       *(epii-epiio)*ergsev/2.
            cltot=cltot+(tmpc+tmpco)
     $       *(epii-epiio)*ergsev/2.
            endif
          if (lpri.ge.1) write (lun11,9009)kl,epii,optpp,
     $     bremsa(kl),tmph,tmpc,fline(kl),httot,cltot
     $       ,((-tmph+tmpc)*fac*fpr2),zremso(1,kl)
 9009     format (1x,i4,14(1pe12.4))
          enddo
        if (lpri.ge.1)
     $   write (lun11,*)'continuum heating, cooling:',
     $       httot,cltot
        clcont=cltot
        do jkk=1,nlsvn
          jk=jkk
          ml=nplin(jk)
          call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
          elin=rdat(1)
          if ((elin.lt.1.e+8).and.(elin.gt.1.)) then
            nilin=npar(ml)
            ergsev=1.602197e-12
            ener=ergsev*(12398.54)/amax1(elin,1.e-34)
            etst=ener/ergsev
            tmpc1=rcem(1,jk)
            tmpc2=rcem(2,jk)
            tmpc=tmpc1+tmpc2
            cltot=cltot+tmpc
            nblin=nbinc(ener,epi)
            optp2=opakc(nblin)
            tmph=elumo(1,jk)*optp2/fpr2
            fac=delr          
            elum(1,jk)=max(0.,elumo(1,jk)+(-tmph+tmpc1)*fac*fpr2)
            elum(2,jk)=max(0.,elumo(2,jk)+(-tmph+tmpc2)*fac*fpr2)
            if (lpri.ge.1)
     $       write (lun11,*)jk,nilin,etst,
     $        rcem(1,jk),rcem(2,jk),delr,fpr2,cltot,
     $        elumo(1,jk),elum(1,jk)
            endif
          enddo      
c
c       calculate rrc luminosities
C       First look for element data (jk is element index)
        if (lpri.ge.3) write (lun11,*)'rrc print:'
        klel=11
        mlel=npfirst(klel)
        jk=0
        do while (mlel.ne.0)
          jk=jk+1
          mt2=mlel-1
          call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mt2,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
          mllel=idat(nidt)
          xeltp=rdat(1)
          xeltp=abel(mllel)
          nnz=idat(1)
          if (lpri.ge.3) 
     $      write (lun11,*)'element:',jk,mlel,mllel,nnz,
     $                  (kdat(mm),mm=1,nkdt)
C         ignore if the abundance is small
          if (xeltp.lt.1.e-10) then
              jkk=jkk+nnz
            else
c             now step thru ions (jkk is ion index)
              klion=12
              mlion=npfirst(klion)
              jkk=0
              kl=0
              do while ((mlion.ne.0).and.(kl.lt.nnz))
                jkk=jkk+1
C               retrieve ion name from kdati
                call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdati,mlion-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
C               if not accessing the same element, skip to the next element
                mlleltp=idat(nidt-1)
                if (mlleltp.eq.mllel) then
                  kl=kl+1
                  if (lpri.ge.3)
     $              write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $                          (kdati(mm),mm=1,nkdt)
c                 now find level data
c                 step thru types
                  nlevmx=0
                  mltype=13
                  ml=npfi(mltype,jkk)
                  mllz=npar(ml)
c                 step thru records of this type
                  do while ((ml.ne.0).and.(npar(ml).eq.mllz))
                    call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
                    nlev=idat(nidt-1)
                    nlevmx=max(nlevmx,nlev)
                    if ((nlev.gt.0).and.(nlev.le.nd)) then
                      do  lk=1,nrdt
                        rlev(lk,nlev)=rdat(lk)
                        enddo
                      do lk=1,nidt
                        ilev(lk,nlev)=idat(lk)
                        enddo
                      do lk=1,nkdt
                        klev(lk,nlev)=kdat(lk)
                        enddo
                      do lk=nkdt+1,20
                        klev(lk,nlev)=kblnk
                        enddo
                      endif
                    ml=npnxt(ml)
                    enddo
                  nlev=nlevmx
                  mltype=7
                  mlrdesc=mltype
                  ml=npfi(mltype,jkk)
                  mllz=npar(ml)
                  do while ((ml.ne.0).and.(npar(ml).eq.mllz))
c                   step thru records of this type
                    call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
                    kkkl=npconi2(ml)              
                    idest1=idat(nidt-1)
                    if ((kkkl.gt.0).and.(kkkl.le.ndat2)
     $                .and.(cemab(1,kkkl).gt.1.e-36)) then
                      eth=rlev(4,idest1)-rlev(1,idest1)
                      tmpc=(cemab(1,kkkl)+cemab(2,kkkl))
                      optp2=0.
                      fac=delr
                      tmph=elumabo(1,kkkl)*optp2/fpr2
                      elumab(1,kkkl)=max(0.,elumabo(1,kkkl)
     $                            +(-tmph+tmpc)*fac*fpr2/2.)
                      elumab(2,kkkl)=max(0.,elumabo(2,kkkl)
     $                            +(-tmph+tmpc)*fac*fpr2/2.)
                      if (lpri.ge.3)
     $                  write (lun11,*)kkkl,eth,idest1,
     $                    cemab(1,kkkl),cemab(2,kkkl),delr,fpr2,
     $                    elumabo(1,kkkl),elumab(1,kkkl)
                      endif
                    ml=npnxt(ml)
                    enddo
                  endif
C               Go to next ion
                mlion=npnxt(mlion)
                enddo
            endif
          mlel=npnxt(mlel)
C         Go to next element
          enddo
       cllines=cltot-clcont
       if (lpri.ge.1)
     $   write (lun11,*)'line cooling',cltot
        endif
c
      ekt = t*(0.861707)
      htcomp = cmp1*xnx*ergsev
      clcomp = ekt*cmp2*xnx*ergsev
      httot=htcomp+httot
      cltot=clcomp+cltot+clbrems
      hmctot=(httot-cltot)
     $   /((abs(httot)+abs(cltot))/2.)
      if (lpri.ge.1) write (lun11,9953)htcomp,clcomp,cmp1,cmp2,
     $   clbrems,httot,cltot,hmctot
 9953 format (1h , ' compton heating, cooling=',8e12.4)
      lpri=lprisv
c
c
c      write (lun11,*)'in heatt:',httot,clcont,cllines,cltot,hmctot
c     
      lpri=lprisv
c
      return
      end
      subroutine hgf(ia,ib,ic,x,hyp)
c subroutine hgf calculates the value, hyp, of the
c hypergeometric fn at x for constants ia,ib,ic
c
c     real*8 ser,hyp
c
      ser=1.
      hyp=1.
      i=-ia
      j=-ib
      i=min(i,j)
      do 10 n=0,i
      ser=ser*(ia+n)*(ib+n)*x/((n+1.)*(ic+n))
      hyp=hyp+ser
 10    continue
c
      return
      end
       subroutine hphotx(ener,ic,nq,xsec,lun11,lpri)
c  ener   is the photon energy in ryds with respect to the ionization
c         threshold
c  xsec   is an array containing the cross section in mb (18^{-18} cm^2)
c         for all l=[0,nq-1]
c  ic     ion charge
c  np     principal quantum number
c  ll     angular momentum number
c        real*8 en,cons,r,rk,theta1,theta2,gu,gl
        dimension gu(100),gl(100),xsec(100)
        cons=.54492d0*acos(0.)
c        write (lun11,*)'in hphotx:',ener,ic,nq,cons
        en=ener
c        r=dsqrt(en)
        r=sqrt(en)
        rk=r/ic/ic
        if (lpri.ne.0)
     $   write (lun11,*)'before call gull1:',nq,rk,r,en
        call gull1(nq,rk*rk,gu,gl,lpri,lun11)
c        call gull1(nq,rk,gu,gl,lun11)
       do lm=0,nq-1
        theta1=(1.+nq*nq*rk*rk)*expo(gu(lm+1))
        theta2=(1.+nq*nq*rk*rk)*expo(gl(lm+1))
        if (lpri.ne.0)
     $   write (lun11,*)'after call gull1:',lm,gu(lm+1),gl(lm+1),
     $           theta1,theta2
        xsec(lm+1)=cons*((lm+1)*theta1+lm*theta2)/(2.*lm+1.)
        xsec(lm+1)=float(nq*nq)/float(ic*ic)*xsec(lm+1)
       enddo
       return
       end
      subroutine hunt(xx,n,x,jlo,lpri,lun11)
      data nintmx/1000/
      dimension xx(n)
      logical ascnd
      nint=0
      ascnd=xx(n).gt.xx(1)
      if (lpri.gt.1) write (lun11,*)'in hunt',xx(1),xx(n),n,x
      if(jlo.le.0.or.jlo.gt.n)then
        jlo=1
        jhi=n+1
        if (lpri.gt.1) write (lun11,*)'initializing',jlo,jhi
        go to 3
      endif
      inc=1
      if(x.ge.xx(jlo).eqv.ascnd)then
1       jhi=jlo+inc
        if (lpri.gt.1) write (lun11,*)'hunt up ',jlo,jhi,xx(jlo),xx(jhi)
        if(jhi.gt.n)then
          jhi=n+1
        else if(x.ge.xx(jhi).eqv.ascnd)then
          jlo=jhi
          inc=inc+inc
          if (lpri.gt.1) write (lun11,*)'double the increment',inc
          go to 1
        endif
      else
        jhi=jlo
2       jlo=jhi-inc
        if (lpri.gt.1) write (lun11,*)'hunt down ',jlo,jhi,xx(jlo),
     $                        xx(jhi)
        if(jlo.lt.1)then
          jlo=0
        else if(x.lt.xx(jlo).eqv.ascnd)then
          jhi=jlo
          inc=inc+inc
          if (lpri.gt.1) write (lun11,*)'double the increment',inc
          go to 2
        endif
      endif
3     continue
      jlo=min(jlo,n)
      jlo=max(jlo,1)
      nint=nint+1
      if ((jhi-jlo.eq.1).or.(nint.gt.nintmx)) return
      if (lpri.gt.1) write (lun11,*)'bisection phase',jlo,jhi,jm,xx(jm)
      jm=(jhi+jlo)/2
      if(x.gt.xx(jm).eqv.ascnd)then
        jlo=jm
      else
        jhi=jm
      endif
      go to 3
      end
      subroutine huntf(xx,n,x,jlo,lpri,lun11)
c
c     this version of hunt assumes equally spaced data in log
c
      dimension xx(n)
      xtmp=max(x,xx(2))
      jlo=int((n-1)*alog(xtmp/xx(1))/alog(xx(n)/xx(1)))+1
c      write (6,*)'in huntf:',n,x,xx(1),xx(n),jlo
      jlo=max(1,jlo)
      jlo=min(n,jlo)
      return
      end
c---------------------------------------------------------------------
      subroutine impact(en,l,temp,ic,z1,rm,ecm,psi,cr)
c impact parameter collision cross-sections using the method of seaton.
c
      real*8 b,xsi,phi,bo,xsw,phw
      tk=8.617e-5*temp
      inc=1
      jm=90*inc
c
      cr=0.
      fi=0.
      wo=0.
      b=10.
      ev=abs(ecm)/8065.48
c
      po=(3.*en*en-l*(l+1))/2./ic
c
c strong coupling
c
 21    del=b/100./inc
      do 20 j=1,jm
      b=b-del
      call impcfn(b,xsi,phi)
c     write (lun11,*)b,xsi,phi
      w=ic*rm*ev/b*sqrt(2.*xsi*psi)
      wi=w+ecm/8065.48/2.
c     write (lun11,*)wi/tk
      if(wi/tk.ge.100.) go to 13
      if(wi.le.0.) go to 20
c
c weak coupling
c
      bo=po*ev/2./w*sqrt(wi*rm/13.60)
      call impcfn(bo,xsw,phw)
c
c the minimum of the weak and strong coupling x-sections is used
      ff=min(xsi/2.+phi,phw)
      ff=ff*exp(-wi/tk)
c
      crinc=(fi+ff)/2.*(wi-wo)
      cr=crinc+cr
      if(cr.lt.1.e-20) go to 20
      fi=ff
      wo=wi
      if(crinc/cr.lt.1.e-5) go to 13
c
 20    continue
      go to 21
 13       cr=6.900e-5*z1*z1*sqrt(rm/temp)*psi*cr/tk
c
c
      return
      end
c---------------------------------------------------------------
      subroutine impactn(n,m,temp,ic,amn,cmm,lun11,lpri)
c impact parameter collision cross-sections using the method of seaton.
c impactn.ftn calculates the electron collisional excitation rate for
c transitions between principal quantum number n and m in hydrogenic
c atoms with ionic charge ic.  it is assumed that rm=1 and z1=1.
c cmm is the symmetrical quantity used in the models.
c
c
      real*8 b,xsi,phi,bo,xsw,phw
      if (lpri.ne.0)
     $ write (lun11,*)'in impactn:',n,m,temp,ic,amn
      cmm=0.

      xm=157888.*ic*ic/temp/m/m
      if(xm.gt.60) return
      rm=1.
      z1=1.
      tk=8.617e-5*temp
      inc=1
      jm=90*inc
c
      ecm=109737.*ic*ic*(1./n/n-1./m/m)
      ecm3=ecm**3
      ecm=-ecm
      psi=1.644e+5*amn/ecm3
       po=(5.*n*n+1)/4./ic
c
      cr=0.
      fi=0.
      wo=0.
      b=10.
      ev=abs(ecm)/8065.48
c
      if (lpri.ne.0)
     $ write (lun11,*)xm,tk,ecm,ecm3,psi,po,ev
c
c strong coupling
c
 21    del=b/100./inc
      do 20 j=1,jm
      b=b-del
      call impcfn(b,xsi,phi)
      w=ic*rm*ev/b*sqrt(2.*xsi*psi)
      wi=w+ecm/8065.48/2.
      if (lpri.ne.0)
     $ write (lun11,*)'in 20 loop:',j,b,xsi,phi,w,wi
      if(wi/tk.ge.100.) go to 13
      if(wi.le.0.) go to 20
c
cc weak coupling
cc
       bo=po*ev/2./w*sqrt(wi*rm/13.60)
       call impcfn(bo,xsw,phw)
cc
cc the minimum of the weak and strong coupling x-sections is used
       ff=min(xsi/2.+phi,phw)
c
c only the strong coupling calculation is used
      ff=xsi/2.+phi
      ff=ff*exp(-wi/tk)
c
      crinc=(fi+ff)/2.*(wi-wo)
      cr=crinc+cr
      if(cr.lt.1.e-20) go to 20
      fi=ff
      wo=wi
      if (lpri.ne.0)
     $ write (lun11,*)'weak coupling:',bo,xsw,phw,ff,crinc,cr
      if((crinc/cr.lt.1.e-5).and.(crinc.gt.1.e-7)) go to 13
c
 20    continue
      go to 21
 13       cr=6.900e-5*z1*z1*sqrt(rm/temp)*psi*cr/tk
      cmm=cr*m*m*exp(xm)
c
      if (lpri.ne.0)
     $ write (lun11,*)'done with impactn:',cr,cmm
c
      return
      end
c--------------------------------------------------------------------
      subroutine impcfn(x,xsi,phi)
c
c data for functions used in the impact parameter method are generated
c using polynomials fitted to seaton's (1962) values using least square
c
      implicit real*8 (a-h,o-z)
      dimension a(6),b(6)
c
      pi=2.*dacos(0.d0)
      a(1)=0.9947187
      a(2)=0.6030883
      a(3)=-2.372843
      a(4)=1.864266
      a(5)=-0.6305845
      a(6)=8.1104480e-02
      b(1)=0.2551543
      b(2)=-0.5455462
      b(3)=0.3096816
      b(4)=4.2568920e-02
      b(5)=-2.0123060e-02
      b(6)=-4.9607030e-03
c
      if(x.gt.2.) go to 25
      xsi=0.
      phi=0.
      do 20 n=1,6
      xsi=xsi+a(n)*x**(n-1)
      y=log(x)
      phi=phi+b(n)*y**(n-1)
 20    continue
      if(x.eq.1.) phi=b(1)
      if(x.lt.0.05) then
      xsi=1.0+0.01917/0.05*x
      y=log(1.1229/x)
      phi=y+x*x/4.*(1.-2*y*y)
      endif
      go to 30
c
 25    xsi=pi*x*dexp(-2*x)*(1.+0.25/x+1./32./x/x)
      phi=pi/2.*dexp(-2*x)*(1.+0.25/x-3./32./x/x)
c
 30    continue
c
      return
      end
c---------------------------------------------------------------------
      subroutine init(
     $       lpri,lun11,tinf,
     $       t,r,delr,xlum,enlum,xee,xpx,abel,cfrac,
     $       epi,bremsa,bremsint,
     $       tau0,dpthc,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $       ntotit,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,
     $       rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
     $       elumab,elumabo,elum,elumo,zrems,zremso,zremsz,
     $       rates,vsav,idrates,fline)
c
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     line luminosities
      dimension elum(2,nnnl),elumo(2,nnnl)
c     line emissivities
      dimension rcem(2,nnnl)
c     line opacities
      dimension oplin(nnnl)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
      dimension fline(ncn)
c     continuum lum
      dimension zrems(3,ncn),zremso(3,ncn),
     $          zremsz(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      dimension rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      dimension opakc(ncn)
c     level populations
      dimension xilev(nnml)
      dimension cemab(2,nnml),opakab(nnml)
      dimension elumab(2,nnml),elumabo(2,nnml)
      dimension tauc(2,nnml)
c     ion abundances
      dimension xii(nni)
c     heating/cooling
      dimension htt(nni),cll(nni)
      dimension rrrt(nni),pirt(nni)
      dimension abel(13)
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
c
      httot=0.
      cltot=0.
      cllines=0.
      clcont=0.
      htcomp=0.
      clcomp=0.
      clbrems=0.
c
      do i = 1,ncn
         rccemis(1,i)=0.
         rccemis(2,i)=0.
         brcems(i)=0.
         fline(i)=0.
         zrems(1,i)=0.
         zrems(2,i)=0.
         zrems(3,i)=0.
         zremso(1,i)=0.
         zremso(2,i)=0.
         zremso(3,i)=0.
         bremsint(i)=0.
         bremsa(i)=0.
         dpthc(1,i) = 0.
c         dpthc(2,i)=1.e+10
         dpthc(2,i)=0.
         opakc(i)=0.
         enddo
      do  i = 1,nnnl
         rcem(1,i)=0.
         rcem(2,i)=0.
c         tau0(2,i)=1.e+10
         tau0(2,i)=0.
         elum(1,i)=0.
         elum(2,i)=0.
         elumo(1,i)=0.
         elumo(2,i)=0.
         tau0(1,i) = 0.
         oplin(i)=0.
         enddo
      do i=1,nni
         xii(i)=0.
         htt(i)=0.
         cll(i)=0.
         rrrt(i)=0.
         pirt(i)=0.
         enddo
      do i = 1,nnml
         elumab(1,i)=0.
         elumab(2,i)=0.
         elumabo(1,i)=0.
         elumabo(2,i)=0.
         cemab(1,i)=0.
         cemab(2,i)=0.
         opakab(i)=0.
         xilev(i)=1.
         tauc(1,i) = 0.
c         tauc(2,i) =1.e+10
         tauc(2,i) =0.
         enddo
      do i=1,ndat2
         do j=1,4
           rates(j,i)=0.
           vsav(j,i)=0.
           enddo
         idrates(1,i)=0
         idrates(2,i)=0
         enddo
c
c
c
      return
      end
       subroutine intin2(x1,x2,x0,t,ri2,ri3,lun11)
       ryd=2.17987e-11
       bk1=7.2429e+15
       ryk=ryd*bk1
       s1=x1*ryk/t
       s2=x2*ryk/t
       s0=x0*ryk/t
       del=bk1/t
c       write (lun11,*)'in intin:',x1,x2,x0,s1,s2,s0,del
       if ((s1-s0).lt.40) then
       ri2=exp(s0-s1)*(s1*s1+2.*s1+2.)-exp(s0-s2)*(s2*s2+2.*s2+2.)
       else
       ri2=0.
       endif
       ri2=ri2/(del**3)
       rr=(exp(s0-s1)*(x1**3)-exp(s0-s2)*(x2**3))*(ryd**3)
       ri3=(rr+3.*ri2)/del
c       write (lun11,*)ri2,rr,ri3
       return
       end
c---------------------------------------------------------------------
       subroutine intin(x1,x2,x0,t,ri2,ri3,lun11,lpri)
       implicit real*8 (a-h,o-z)
       ryk=7.2438d+15
       s1=x1*ryk/t 
       s2=x2*ryk/t 
       s0=x0*ryk/t 
       del=ryk/t        
       if (lpri.gt.1)
     $  write (lun11,*)'in intin:',s1,s2,s0,del
       if ((s1-s0).lt.40.) then
c           ri2=dexpo(s0-s1)*(s1*s1+2.*s1+2.)-dexpo(s0-s2)*(s2*s2+2.*s2+2.)
           ri2=exp(s0-s1)*((s1*s1+2.*s1+2.)-exp(s1-s2)*(s2*s2+2.*s2+2.))
     $              /(del**1.5)
           if (lpri.gt.1)
     $     write (lun11,*)'ri2=',ri2
           if ((s0.lt.1.e-3).and.(s2.lt.1.e-3).and.(s1.lt.1.e-3))
     $       ri2=0.
         else
           ri2=0.
         endif
c       ri2=ri2/(del**1.5)
       if (lpri.gt.1)
     $     write (lun11,*)'ri2=',ri2
c       rr=dexpo(s0-s1)*(s1**3)-dexpo(s0-s2)*(s2**3)
       rr=exp(s0-s1)*((s1**3)-exp(s1-s2)*(s2**3))
       if (lpri.gt.1)
     $     write (lun11,*)'rr=',rr
       ri3=(rr/(del**1.5)+3.*ri2)/del
       if (lpri.gt.1)
     $  write (lun11,*)'in intin:',s1,s2,s0,del,ri2,rr,ri3 
       return
       end
      subroutine invert(lpri,lun11,lun,lun2,numrec,epi)
c
c     this routine inverts the order
c
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
c
c
      dimension epi(ncn)
c     line optical depths
      dimension tau0(2,nnnl)
c     continuum optical depths
      dimension dpthc(2,ncn)
      dimension tauc(2,nnml)
c
      lprisv=lpri
      lpri=0
      nlyc=nbinc(13.7,epi)
      nry=nlyc+1
      if (lpri.ne.0)
     $ write (lun11,*)'in invert',lun,lun2,numrec
c
      rewind(lun2)
      do kk=1,numrec
        kkm=numrec+1-kk
        rewind(lun)
        do ll=1,kkm
          lpriu=0
          call unsavd(0,
     $       lpriu,lun,lun11,tinf,
     $       t,r,rdel,xcol,xee,xpx,
     $       tau0,dpthc,tauc,epi)
          enddo
        if (lpri.ne.0)
     $   write (lun11,*)kk,r,t,rdel,kkm,dpthc(1,nry),dpthc(2,nry)
        call savd(0,
     $       lpri,lun2,lun11,tinf,
     $       t,r,rdel,xcol,xee,xpx,
     $       tau0,dpthc,tauc)
        enddo
      lpri=lprisv
c
      return
      end
      subroutine ioneqm(z,a,s,n,m,l,lpri,lun11)
c
      implicit real*8 (a-h,o-z)
c
c      real a,delt,eps,pg,pl,q,s,sumg,suml,tst,z
      integer i,j,jk,jmax,k,l,ll,lpri,m,mmn,
     &        mmx,n
c
c
c
c     solves a system of ionization equations, attempting
c     to avoid overflow problems.
c
c
c
      dimension z(m),a(m),s(n),q(29)
c
      data eps/1.e-6/
      data delt/1.e-28/
c
      lprisv=lpri
      if (lpri.ge.1) lpri=2
      if (lpri.ge.2) write (lun11,*)'in ioneqm'
c
c     initialize
      do 100 jk = 1,n
         s(jk) = 0.
 100  continue
c
      if ( lpri.ge.2 ) write (lun11,99001)
c
c     form naive ratio
      do 200 j = 1,m
         q(j) = a(j)/(z(j)+delt)
 200  continue
c
c     step thru and search for max. q value
      jk = l
 300  jk = jk + 1
      if ( (jk.lt.n) .and. (q(jk-1).lt.1.) ) goto 300
      jmax = jk
c
      if ( lpri.ge.2 ) write (lun11,99002) n,m,l,jmax
c
c     step forwards
      suml = 0.
      if ( jmax.ne.n ) then
         pl = 1.
         mmx = jmax - 1
 350     mmx = mmx + 1
         pl = pl/(q(mmx)+delt)
         suml = suml + pl
         tst = pl/(suml+delt)
         if ( lpri.ge.2 ) write (lun11,99003) mmx,q(mmx),suml,pl
         if ( (tst.gt.eps) .and. (mmx.lt.m) ) goto 350
      endif
c
c     step backwards
      sumg = 0.
      if ( jmax.ne.l ) then
         pg = 1.
         mmn = jmax
 400     mmn = mmn - 1
         pg = pg*q(mmn)
         sumg = sumg + pg
         tst = pg/(sumg+delt)
         if ( lpri.ge.2 ) write (lun11,99004) mmn,q(mmn),sumg,pg
         if ( (tst.gt.eps) .and. (mmn.gt.l) ) goto 400
      endif
c
c
      s(jmax) = 1./(1.+suml+sumg)
      if ( jmax.ne.n ) then
         do 450 j = jmax,mmx
            s(j+1) = s(j)/(q(j)+delt)
 450     continue
      endif
c
      if ( jmax.ne.l ) then
         k = jmax - mmn
         do 500 i = 1,k
            j = jmax - i
            s(j) = s(j+1)*q(j)
 500     continue
      endif
c
      if ( lpri.ge.2 ) write (lun11,99005) (ll,q(ll),s(ll),ll=l,n)
c
      lpri=lprisv
c
      return
99001 format (' ',' in ioneqm ')
99002 format (' ',' n,m,l,jmax --',4i4)
99003 format (' ','in greater than loop, j,q,sum,p --',i4,3e12.4)
99004 format (' ','in less than loop, j,q,sum,p --',i4,3e12.4)
99005 format (' ',i4,2e12.4)
      end
      SUBROUTINE IRC(N,T,RC,RNO,SE)
C IRC CALCULATES THE EXCITATION RATE, SE [cm**3/s], FOR IONIZATION
C OF HYDROGEN ATOMS FROM STATE N DUE TO ELECTRON COLLISIONS, ASSUMING
C THE CONTINUUM STARTS AT LEVEL RNO.  THE
C ENERGY LOSS RATE, ELOST [eV*cm**3/s], IS ALSO DETERMINED.
C CIN IS THE 3-BODY RECOMBINATION RATE, DETERMINED FROM CNI BY
C DETAILED BALANCE.
C REF. JOHNSON (1972)
C
         CII=0.
      IF(RC.NE.1.) THEN                          ! MAB
       CALL SZIRC(N,T,RC,rno,SE)
       RETURN
      ENDIF
      IC=int(rc)
c
      XO=1.-N*N/RNO/RNO
      YN=XO*157803./(T*N*N)
      IF(N-2) 100,200,300
 100   AN=1.9603*N*(1.133/3./XO**3-0.4059/4./XO**4+0.07014/5./XO**5)
      BN=2./3.*N*N/XO*(3.+2./XO-0.603/XO/XO)
      RN=0.45
      GO TO 400
C
 200   AN=1.9603*N*(1.0785/3./XO**3-0.2319/4./XO**4+0.02947/5./XO**5)
      BN=(4.-18.63/N+36.24/(N*N)-28.09/(N*N*N))/N
      BN=2./3.*N*N/XO*(3.+2./XO+BN/XO/XO)
      RN=0.653
      GO TO 400
C
 300   G0=(0.9935+0.2328/N-0.1296/(N*N))/3./XO**3
      G1=-(0.6282-0.5598/N+0.5299/(N*N))/(N*4.)/XO**4
      G2=(0.3887-1.181/N+1.470/(N*N))/(N*N*5.)/XO**5
      AN=1.9603*N*(G0+G1+G2)
      BN=(4.-18.63/N+36.24/(N*N)-28.09/(N*N*N))/N
      BN=(3.+2./XO+BN/XO/XO)*2.*N*N/3./XO
      RN=1.94*N**(-1.57)
C
 400   CONTINUE
      RN=RN*XO
      ZN=RN+YN
      CALL EXPINT(YN,EY)
      CALL EXPINT(ZN,EZ)
      SE=AN*(EY/YN/YN-EXP(-RN)*EZ/ZN/ZN)
      EY=1.+1./YN-EY*(2./YN+1.)
      EZ=EXP(-RN)*(1.+1./ZN-EZ*(2./ZN+1.))
      SE=SE+(BN-AN*LOG(2.*N*N/XO))*(EY-EZ)
      SE=SE*SQRT(T)*YN*YN*N*N*1.095E-10/XO
      CII=SE*N*N
c MAB
c     CII=CII/100.
C      CNI=SE*EXP(-YN)
c      CIN=4.144219E-16*N*N/T/SQRT(T)*SE
C      ELOST=SE*13.60/(N*N)
C
      RETURN
      END
      subroutine ispcg2(zremsz,epi,enlum,lpri,lun11)
c
c
c     this subroutine calculates number luminosity
c
      parameter (ncn=9999)
c
      dimension zremsz(ncn),epi(ncn)
c
c
      if (lpri.ge.1) write (lun11,*)'in ispec2'
c
      numcon=ncn
      sum2 = 0.
      sum3 = 0.
      sum4 = 0.
      do 100 jk = 1,numcon
         if ( epi(jk).ge.13.6 ) then
            sum2 = sum2+(zremsz(jk)/epi(jk)+zremsz(jk-1)/epi(jk-1))
     &             *(epi(jk)-epi(jk-1))/2.
           if ( epi(jk).le.24.48 )
     $        sum3 = sum3+(zremsz(jk)/epi(jk)+zremsz(jk-1)/epi(jk-1))
     &             *(epi(jk)-epi(jk-1))/2.

         endif
         if ((epi(jk).ge.24.48).and.(epi(jk).le.54.4)) 
     $     sum4 = sum4+(zremsz(jk)/epi(jk)+zremsz(jk-1)/epi(jk-1))
     &             *(epi(jk)-epi(jk-1))/2.
         if (lpri.ne.0) 
     $    write (lun11,*)jk,epi(jk),zremsz(jk),sum2
 100  continue
      enlum = sum2
      write (lun11,*)'U(1-1.8),U(1.8-4):',sum3,sum4
c
c
      return
      end
      subroutine ispec4(tp,xlum,epi,zremsz,lpri,lun11)
c
c
c     this subroutine generates the initial spectrum.
c      power law spectrum
c     brems stores the flux to be used
c
c
      parameter (ncn=9999)
c
      dimension epi(ncn),zremsz(ncn)
c
c
c
      data ergsev/1.602197e-12/
c
      dimension zremsi(ncn)
c
      numcon=ncn
      ecut=0.01
      sum=0.
      lprisv=lpri
      if (lpri.ge.1) write (lun11,*)'in ispec4',tp,xlum
      do i=1,numcon
         zremsi(i)=1.e-24
         if (epi(i).gt.ecut)
     $    zremsi(i)=epi(i)**tp
         if (lpri.gt.1) write (lun11,*)i,epi(i),zremsi(i)
         if ((epi(i).ge.13.6).and.(epi(i).lt.1.36e+4)) 
     $    sum=sum+(zremsi(i)+zremsi(i-1))*(epi(i)-epi(i-1))/2.
         enddo
c
      const=xlum/sum/ergsev
      do 134 i=1,numcon
         zremsz(i)=zremsz(i)+zremsi(i)*const
         if (lpri.ge.1) 
     $        write (lun11,*)i,epi(i),zremsi(i),const,zremsz(i)
 134     continue
      lpri=lprisv
c
      return
      end
      subroutine ispec(tp,xlum,epi,zremsz,lpri,lun11)
c
c
c     this subroutine generates the initial spectrum.
c     brems stores the flux to be used
c
      parameter (ncn=9999)
c
      dimension epi(ncn),zremsz(ncn)
c
      data ergsev/1.602197e-12/
c
      dimension zremsi(ncn)
c
      numcon=ncn
      ekt=1000.*(0.861707)*tp
      sum=0.
      lprisv=lpri
      if (lpri.ge.1) write (lun11,*)'in ispec',tp,xlum
      do 132 i=1,numcon
         zremsi(i)=expo(-epi(i)/ekt)
         if (lpri.gt.1) write (lun11,*)i,epi(i),zremsi(i)
         if ((epi(i).lt.13.6).or.(epi(i).gt.1.36e+4)
     $        .or.(i.le.1)) go to 132
         sum=sum+(zremsi(i)+zremsi(i-1))*(epi(i)-epi(i-1))/2.
132      continue
c
      const=xlum/sum/ergsev
      do 134 i=1,numcon
         zremsz(i)=zremsi(i)*const
         if (lpri.ge.1) 
     $        write (lun11,*)i,epi(i),zremsi(i),const,zremsz(i)
 134     continue
      lpri=lprisv
c
      return
      end
      subroutine ispecg(eptmp,zrtmp,nret,epi,zremsz,xlum,
     $                  lpri,lun11)
c
c
c
c
c     this subroutine generates the initial spectrum.
c     brems stores the flux to be used
c     generic renormalization
c
      parameter (ncn=9999)
c
      dimension zremsi(9999),eptmp(99999),zrtmp(99999)
      dimension zremsz(ncn),epi(ncn)
c
      data ergsev/1.602197e-12/
c
c        linear interpolation in log
      jlo = 0
c     if (lpri.ge.1) write (lun11,*)'in ispecg:',nret
c     if ( lpri.gt.2 ) write (lun11,*) (ll,eptmp(ll),zrtmp(ll),ll=1,nret)
      numcon=ncn
      do 300 kl = 1,numcon
         x = epi(kl)
         zremsi(kl) = 0.
         epmx = amax1(eptmp(1),eptmp(nret))
         epmn = min(eptmp(1),eptmp(nret))
         if ( lpri.gt.2 ) write (lun11,*) kl,x,epmx,epmn
         if ( (x.le.epmx) .and. (x.ge.epmn) ) then
            call hunt(eptmp,nret,x,jlo,lpri,lun11)
            jlo = max0(jlo,1)
            zr1 = alog10(amax1(zrtmp(jlo+1),1.e-34))
            zr2 = alog10(amax1(zrtmp(jlo),1.e-34))
            ep1 = alog10(amax1(eptmp(jlo+1),1.e-34))
            ep2 = alog10(amax1(eptmp(jlo),1.e-34))
            alx = alog10(x)
            alx = amax1(alx,ep2)
            alx = min(alx,ep1)
            aly = (zr1-zr2)*(alx-ep2)/(ep1-ep2+1.e-34) + zr2
            y = exp10(aly)
            zremsi(kl) = y
            if ( lpri.gt.2 ) write (lun11,*) kl,x,jlo,zr1,zr2,
     &                              ep1,ep2,y
         endif
 300  continue
c
      sum = 0.
      tmp = zremsi(1)
      if ( lpri.gt.2 ) write (lun11,*) ' in ispecg'
      do 400 jk = 2,ncn
         tmpo = tmp
         tmp = zremsi(jk)
         if ( lpri.gt.2 ) write (lun11,*) jk,epi(jk),tmp,tmpo,sum
         if ( (epi(jk).ge.13.6) .and. (epi(jk).le.1.36e+4) ) then
            sum = sum + (tmp+tmpo)*(epi(jk)-epi(jk-1))/2.
         endif
 400  continue
      sum = sum*ergsev
      const = xlum/sum
      do 500 jk = 1,ncn
         if ( lpri.gt.2 ) write (lun11,*) jk,epi(jk),zremsz(jk),
     &                                zremsi(jk)
         zremsz(jk) = zremsz(jk) + zremsi(jk)*const
 500  continue
c
c
      return
      end
      subroutine ispecgg(xlum,epi,zremsz,
     $               lpri,lun11)
c
c
c     this subroutine generates the initial spectrum.
c     renormalization
c
c
      parameter (ncn=9999)
c
      dimension epi(ncn),zremsz(ncn)
c
c
c
      data ergsev/1.602197e-12/
c
      numcon=ncn
      sum=0.
      if (lpri.gt.1) write (lun11,*)'in ispecgg',xlum
      do 132 i=1,numcon
         if (lpri.gt.1) write (lun11,*)i,epi(i),zremsz(i)
         if ((epi(i).lt.13.6).or.(epi(i).gt.1.36e+4)
     $        .or.(i.le.1)) go to 132
         sum=sum+(zremsz(i)+zremsz(i-1))*(epi(i)-epi(i-1))/2.
132      continue
c
      const=xlum/sum/ergsev
      do 134 i=1,numcon
         zremsz(i)=zremsz(i)*const
         if (lpri.gt.1) 
     $        write (lun11,*)i,epi(i),const,zremsz(i)
 134     continue
c
      return
      end
      subroutine istruc(zeff,alpha,xitp,nnz,lpri,lun11)
c
      real zeff(29),alpha(29),xitp(29)
      real*8 z8(29),a8(29),x8(29)
c
c
      if (lpri.ne.0) 
     $ write (lun11,*)'ion rates:',nnz
      do mm=1,nnz
          z8(mm)=zeff(mm)
          a8(mm)=alpha(mm) 
          if (lpri.ne.0) 
     $     write (lun11,9901)zeff(mm),alpha(mm)
9901      format (1x,2(1pe11.3))
          enddo
c
      nnzp1 = nnz + 1
      ill=1
      call ioneqm(z8,a8,x8,nnzp1,nnz,ill,lpri,lun11)
c
      xisum=0.
      do mm=1,nnz
          xitp(mm)=x8(mm)
          xisum=xisum+xitp(mm)
          enddo
      xitp(nnz+1)=max(0.,1.-xisum)
c
      return
      end
      subroutine leqt2f(a,m,n,np,b,idgt,wkarea,ier,lun11,lpri)
c
      parameter (ndl=2400,nd=ndl+1)
c
      dimension a(np,np),b(np)
      dimension indx(nd)
      real*8 ao(nd,nd),bo(nd)
      real*8 an(nd,nd),bn(nd),d
c
c     n had better be less than nd
c
      do 1 jk=1,n
        bo(jk)=(b(jk))
        bn(jk)=(b(jk))
        do 1 kl=1,n
           an(jk,kl)=(a(jk,kl))
           ao(jk,kl)=(a(jk,kl))
 1         continue
c
      npp=nd
      if (lpri.gt.1)
     $ write (lun11,*)'before ludcmp',n,npp,np
      call ludcmp(an,n,npp,indx,d,lun11,lpri)
      npp=nd
      if (lpri.gt.1)
     $ write (lun11,*)'after ludcmp',n,npp
      call lubksb(an,n,npp,indx,bn,lun11,lpri)
      if (lpri.gt.1)
     $ write (lun11,*)'after lubksb'
      npp=nd
      call mprove(ao,an,n,npp,indx,bo,bn,lun11,lpri)
      if (lpri.gt.2)
     $ write (lun11,*)'after mprove',n,npp,np
c
c        check the solution
         if (lpri.gt.2) write (lun11,*)'checking the solution'
         errmx=0.
         do  ll2=1,n
          sum=0.
          tmpmx=0.
          mmmx=0
          mmmxo=0
          do  mm=1,n
            btmp=bn(mm)
            tmp=a(ll2,mm)*max(0.,btmp)
            if (abs(tmp).ge.tmpmx) then
              mmmxo=mmmx
              mmmx=mm
              tmpmx=max(tmpmx,abs(tmp))
              endif
            sum=sum+tmp
            enddo
          sum=sum-b(ll2)
          err=sum/amax1(1.e-24,tmpmx)
          errmx=max(errmx,abs(err))
          if (lpri.gt.2) write (lun11,9246)ll2,bn(ll2),tmpmx,sum,err,
     $                                     mmmx,mmmxo
 9246     format (1h ,i4,4e12.4,2i4)
          enddo
c
      do 2 jk=1,n
         if (lpri.gt.2)
     $    write (lun11,*)jk,b(jk)
         if (bn(jk).lt.1.d-36) bn(jk)=0.
         if (bn(jk).gt.1.d+36) bn(jk)=1.e+36
         b(jk)=(bn(jk))
 2       continue
c
         if (lpri.gt.2)
     $    write (lun11,*)'leaving leqt'
c
      return
      end
      subroutine levwk(rniss,bb,lpri,rlev,ilev,
     $          nlpt,iltp,nlev,klev,t,xee,xpx,lun11)

c
      parameter (ndl=2400,nd=ndl+1)
c
c
      data ergsev/1.602197e-12/
      data bk/1.38062e-16/
c
      character(1) klev(100,nd)
c
      dimension rniss(20000)
      dimension rlev(10,nd),ilev(10,nd),
     $        nlpt(nd),iltp(nd)
c     
      lprisv=lpri
      lpri=0
      xnx=xpx*xee
      bb=1.
      tm=t*1.e4
      ekt=t*0.861707
      tev3s2=ekt*sqrt(ekt)
      bktm=bk*tm/ergsev
      q2=2.07e-16*xnx*(tm**(-1.5))
      emltlv=rlev(2,nlev)
c     this is a factor which i dont understand
c      q2=q2*25.3
      rs=q2/emltlv
      ethion=rlev(1,nlev)
      if (lpri.ne.0)
     $ write (lun11,9902)tm,ekt,tev3s2,bktm,q2,
     $    emltlv,rs,ethion,xnx
 9902 format (1x,'in levwk',8(1pe11.3))
      rniss(nlev)=1.
      do 1 ll=1,nlev-1
        eexlv=rlev(1,ll)
        emltlv=rlev(2,ll)
c        explev=expo(-eexlv/bktm)
        ethsht=(ethion-eexlv)/bktm
        ethsht=amax1(ethsht,0.)
        explev2=expo(-ethsht)
        rniss(ll)=emltlv/(explev2/rs)
        bb=bb+rniss(ll)
        if (lpri.ne.0)
     $   write (lun11,9901)ll,eexlv,emltlv,ethsht,explev2,
     $        rniss(ll),rs,bb
 9901   format (1x,i4,7(1pe11.3))
 1      continue
        do ll=1,nlev
          rniss(ll)=rniss(ll)/bb
          enddo
       bb=1.
       lpri=lprisv
c
c
      return
      end
       subroutine lmatch(kstr1,kstr2,lfnd)
c
c      this routine looks for matches
c
       character(32) kstr1,kstr2
       character(1) ktst1,ktst2
c
       ldon1=0
       ldon2=0
       lfnd=0
       lm=27
 1       lm=lm-1
c
         if ((ldon1.eq.1).and.(ldon2.eq.1)) go to 2
         if (lm.lt.22) go to 2
         read (kstr1(lm:lm),'(a1)')ktst1
         read (kstr2(lm:lm),'(a1)')ktst2
         if (ktst1.ne.ktst2) return
         go to 1
c
 2     continue
       lfnd=1
c
       return
       end
      subroutine lubksb(a,n,np,indx,b,lun11,lpri)
c
      implicit real*8(a-h,o-z)
c
      integer i , ii , indx , j , ll , n , np
c
      dimension a(np,np) , indx(n) , b(np)
c
      if (lpri.gt.1)
     $ write (lun11,*)'in lubksb',n,np
      ii = 0
      do 100 i = 1 , n
         ll = indx(i)
c         write (lun11,*)'i,ll:',i,ll
         sum = b(ll)
         b(ll) = b(i)
         if ( ii.ne.0 ) then
            do 20 j = ii , i - 1
               if (lpri.gt.1)
     $          write (lun11,*)'i,j,ii:',i,j,ii,sum
               sum = sum - a(i,j)*b(j)
 20         continue
         elseif ( sum.ne.0. ) then
            ii = i
         endif
         if (lpri.gt.1)
     $    write (lun11,*)'i,sum:',i,sum
         b(i) = sum
 100  continue
      do 200 i = n , 1 , -1
         sum = b(i)
         if ( i.lt.n ) then
            do 120 j = i + 1 , n
               sum = sum - a(i,j)*b(j)
 120        continue
         endif
         b(i) = sum/a(i,i)
 200  continue
      return
      end
      subroutine ludcmp(a,n,np,indx,d,lun11,lpri)
      parameter (ndl=2400,nd=ndl+1,tiny=1.0d-20)
      implicit real*8 (a-h,o-s,u-z)
      dimension a(np,np),indx(n),vv(nd)
      if (lpri.gt.1)
     $ write (lun11,*)'in ludcmp:'
      d=1.
      do 12 i=1,n
        aamax=0.
        do 11 j=1,n
          if (abs(a(i,j)).gt.aamax) aamax=abs(a(i,j))
          if (lpri.gt.1) write (lun11,*)i,j,a(i,j),aamax
11      continue
        if (aamax.eq.0.) then
           if (lpri.gt.1)
     $      write (lun11,9902)         
           stop
9902       format (1h ,'singular matrix.' )
         end if
        vv(i)=1./aamax
12    continue
      do 19 j=1,n
        if (j.gt.1) then
          do 14 i=1,j-1
            sum=a(i,j)
            if (i.gt.1)then
              do 13 k=1,i-1
                sum=sum-a(i,k)*a(k,j)
13            continue
              a(i,j)=sum
            endif
14        continue
        endif
        aamax=0.
        imax=0
        do 16 i=j,n
          sum=a(i,j)
          if (j.gt.1)then
            do 15 k=1,j-1
              sum=sum-a(i,k)*a(k,j)
15          continue
            a(i,j)=sum
          endif
          dum=vv(i)*abs(sum)
          if (dum.ge.aamax) then
            imax=i
            aamax=dum
          endif
16      continue
        if (j.ne.imax)then
          do 17 k=1,n
            dum=a(imax,k)
            a(imax,k)=a(j,k)
            a(j,k)=dum
17        continue
          d=-d
          vv(imax)=vv(j)
        endif
        if (lpri.gt.1)
     $   write (lun11,*)'j,imax:',j,imax
        indx(j)=imax
        if(j.ne.n)then
          if(a(j,j).eq.0.)a(j,j)=tiny
          dum=1./a(j,j)
          do 18 i=j+1,n
            a(i,j)=a(i,j)*dum
18        continue
        endif
19    continue
      if(a(n,n).eq.0.)a(n,n)=tiny
      return
      end
       SUBROUTINE milne2(temp4,nt,x4,y4,eth4,alpha4,lun11,lpri)
C
c       x    = array of energies in Ry with respect to the threshold
c       y    = array of cross sections in Mb
c       nt   = number of points in topbase arrays
c       eth  = threshold energy in Ry
c       alpha= recombination rate for level n,lo
c
c
c
      real*8 x4(nt),y4(4,nt),alpha4(4)
      real*8 eth4,temp4
      real*8  kb, temp,x(20000),y(4,2000),eth,alpha(4),em,cc,st,
     $    ry,s1,s2,v1,v2,rb,ra,ri2,ri3

      

      ry=2.17896d-11
      kb=1.3805d-16
      em=9.109d-28
      cc=3.d+10
C
c
       temp=temp4
       eth=eth4
       do i=1,nt
         x(i)=x4(i)
         do j=1,4
           y(j,i)=y4(j,i)
           enddo
         enddo
c
       st=(x(1)+eth)*ry
       do j=1,4
         alpha(j)=0.
         enddo
       if (lpri.gt.1)
     $   write (lun11,*)'in milne:',temp,nt,eth,x(1),y(1,1)
       crit=0.01
       do i=2,nt
        s1=(x(i-1)+eth)*ry
        s2=(x(i)+eth)*ry
        call intin(s1,s2,st,temp,ri2,ri3,lun11,lpri)
        do j=1,4
          v1=y(j,i-1)
          v2=y(j,i)
          rb=(v2-v1)/(s2-s1+1.e-24)
          ra=v2-rb*s2
          alpha(j) = alpha(j) + (ra*ri2 + rb*ri3)
          if (lpri.gt.1) write (lun11,*)i,x(i),y(j,i),
     $     s1,s2,v1,v2,ra,rb,ri2,ri3,alpha(j)
          enddo
        enddo
c     alpha=sum*.79788*2.4917e+25/(temp**1.5)
      do j=1,4
        alpha(j)=alpha(j)*.79788*40.4153
        if (lpri.gt.1) 
     $    write (lun11,*)'alpha=',alpha(j)
 100    format(i3,1x,6(e12.6,1x))
        alpha4(j)=alpha(j)
        enddo
      return
      end
       SUBROUTINE milne(temp4,nt,x4,y4,eth4,alpha4,lun11,lpri)
C
c       x    = array of energies in Ry with respect to the threshold
c       y    = array of cross sections in Mb
c       nt   = number of points in topbase arrays
c       eth  = threshold energy in Ry
c       alpha= recombination rate for level n,lo
c
c
c
        dimension x4(nt),y4(nt)
      real*8  kb, temp,x(20000),y(20000),eth,alpha,em,cc,st,ry,
     $    sum,s1,s2,v1,v2,rb,ra,ri2,ri3
    
c      lpri=2     
c
      ry=2.17896d-11
      kb=1.3805d-16
      em=9.109d-28
      cc=3.d+10
C
c
       temp=temp4
       eth=eth4
       do i=1,nt
         x(i)=x4(i)
         y(i)=y4(i)
         enddo
c
       st=(x(1)+eth)*ry
       sumo=1.
       sum=0.
       if (lpri.gt.1)
     $   write (lun11,*)'in milne:',temp,nt,eth,x(1),y(1)
       i=1
       crit=0.01
       do while ((abs(sum-sumo).gt.crit*sum).and.(i.lt.nt))
         i=i+1
         s1=(x(i-1)+eth)*ry
         s2=(x(i)+eth)*ry
         v1=y(i-1)
         v2=y(i)
         if (lpri.gt.1) write (lun11,*)'i=',i,x(i),y(i),
     $     s1,s2,v1,v2
         if ((v1.ne.0.).or.(v2.ne.0.)) then
           rb=(v2-v1)/(s2-s1+1.e-24)
           ra=v2-rb*s2
           call intin(s1,s2,st,temp,ri2,ri3,lun11,lpri)
           sumo=sum
           sum = sum + (ra*ri2 + rb*ri3)
           if (lpri.gt.1) write (lun11,*)i,x(i),y(i),
     $     s1,s2,v1,v2,ra,rb,ri2,ri3,sum
           endif
         enddo
c      alpha=sum*.79788*2.4917e+25/(temp**1.5)
       alpha=sum*.79788*40.4153
       if (lpri.gt.1) 
     $    write (lun11,*)'alpha=',alpha,sum
 100   format(i3,1x,6(e12.6,1x))
       alpha4=alpha
       return
       end
      subroutine mprove(a,alud,n,np,indx,b,x,lun11,lpri)
      implicit real*8 (a-h,o-z)
      parameter (nmax=2001)
      dimension a(np,np),alud(np,np),indx(n),b(n),x(n),r(nmax)
c      real*8 sdp
      if (lpri.gt.1)
     $ write (lun11,*)'in mprove:'
      do 12 i=1,n
        sdp=-b(i)
        do 11 j=1,n
          sdp=sdp+(a(i,j))*(x(j))
11      continue
        r(i)=sdp
12    continue
      call lubksb(alud,n,np,indx,r,lun11,lpri)
      do 13 i=1,n
        x(i)=x(i)-r(i)
13    continue
      return
      end
      subroutine msolvelucy(ajis,cjis,nsup,nspmx,ipmat,bmat,x,
     $         ht,cl,niter,nit2,nit3,nitmx,nitmx2,lun11,lpri)
c
      parameter (ndl=2400,nd=ndl+1)
c
c     solves lucy iteration
c
      dimension ajis(nd,nd),cjis(nd,nd)
      dimension x(nd),bmat(nd),nsup(nd),xo(nd),xoo(nd)
      dimension bmatsup(nd),ajissup(nd,nd),xsup(nd),rr(nd)
      dimension cjissup(nd,nd)
c
      lerr=0
      lsimp=0
      if (lsimp.eq.1) then
c        put in number conservation
         do mm=1,ipmat
           ajis(ipmat,mm)=1.
           bmat(mm)=0.
           enddo
        bmat(ipmat)=1.
        lpril=0
        call leqt2f(ajis,1,ipmat,nd,bmat,idgt,wkarea,ier,
     $                      lun11,lpril)
        do mm=1,ipmat
          x(mm)=bmat(mm)
          enddo
        return
        endif
c
c       step thru levels, and form calculate superlevel quantities
      lprisv=lpri
      call remtms(tt1)
c      lpri=0
c      write (lun11,*)'in msolvelucy',lpri
      crit=1.e-5
      crit2=1.e-5
      diff=1.
      niter=0
      nit3=0
      do while ((diff.gt.crit).and.(niter.lt.nitmx))
        niter=niter+1
        if (lpri.gt.1) write (lun11,*)'iteration=',niter
        if (lpri.gt.1) write (lun11,*)'initial populations:'
        do mm=1,ipmat
          xo(mm)=x(mm)
          if (lpri.gt.1) write (lun11,*)mm,x(mm),nsup(mm)
          enddo
        do mm=1,nspmx
          xsup(mm)=0.
          bmatsup(mm)=0.
          do nn=1,nspmx
            ajissup(mm,nn)=0.
            cjissup(mm,nn)=0.
            enddo
          enddo
        do mm=1,ipmat
          nsp=nsup(mm)
          xsup(nsp)=xsup(nsp)+x(mm)          
          enddo
          call remtms(tt2)
         if (lpri.gt.1) 
     $    write (lun11,*)'before constucting matrix',abs(tt2-tt1)
        if (lpri.gt.1) 
     $      write (lun11,*)'constucting the condensed matirx:'
        ngood=0
        do mm=1,ipmat
          nspm=nsup(mm)
          rr(mm)=x(mm)/(1.e-36+xsup(nspm))
          if (xsup(nspm).le.1.e-36) rr(mm)=1.
          enddo
        do mm=1,ipmat
          nspm=nsup(mm)        
          do nn=1,ipmat
            nspn=nsup(nn)
            if ((nspn.ne.nspm)
     $         .and.((abs(ajis(mm,nn)).gt.1.e-36)
     $           .or.(abs(ajis(nn,mm)).gt.1.e-36))) then
              ajissup(nspm,nspn)=ajissup(nspm,nspn)
     $            +(ajis(mm,nn))*rr(nn)
              ajissup(nspm,nspm)=ajissup(nspm,nspm)
     $            -(ajis(nn,mm))*rr(mm)
              cjissup(nspm,nspn)=cjissup(nspm,nspn)
     $            +(cjis(mm,nn))*rr(mm)
              ngood=ngood+1
              if (lpri.gt.1) write (lun11,*)mm,nn,nspm,nspn,
     $              rr(mm),rr(nn),ajis(mm,nn),ajis(nn,mm),
     $              ajissup(nspm,nspn),ajissup(nspm,nspm)
              endif
            enddo
          
          enddo
          call remtms(tt2)
         if (lpri.gt.1) 
     $    write (lun11,*)'after constucting matrix',abs(tt2-tt1),
     $                       ngood
        if (lpri.gt.1) then
          if (lpri.gt.1) write (lun11,*)'the condensed populations:'
          do nsp=1,nspmx
            write (lun11,*)nsp,xsup(nsp)
            enddo
          if (lpri.gt.1) write (lun11,*)'the condensed matrix:'
          do nspm=1,nspmx
            do nspn=1,nspmx
              if (abs(ajissup(nspm,nspn)).gt.1.e-37)
     $         write (lun11,*)nspm,nspn,ajissup(nspm,nspn)
              enddo
            enddo
          endif
c        put in number conservation
c         nspcon=1
         nspcon=nspmx
         do mm=1,nspmx
           ajissup(nspcon,mm)=1.
           bmatsup(mm)=0.
           enddo
        bmatsup(nspcon)=1.
        lpril=0
        call remtms(tt1)
        if (lpri.gt.2) 
     $    write (lun11,*)'before leqt',abs(tt2-tt1)
        call leqt2f(ajissup,1,nspmx,nd,bmatsup,idgt,wkarea,ier,
     $                      lun11,lpril)
         call remtms(tt2)
         if (lpri.gt.2) 
     $    write (lun11,*)'after leqt',abs(tt2-tt1)
        if (lpri.gt.2) write (lun11,*)'the new condensed populations:'
        do mm=1,nspmx
          xsup(mm)=bmatsup(mm)
          if (lpri.gt.2) write (lun11,*)mm,xsup(mm)
          enddo
        if (lpri.gt.2) write (lun11,*)'new populations'
        do mm=1,ipmat
          nsp=nsup(mm)
          x(mm)=rr(mm)*xsup(nsp)
          if (lpri.gt.2) write (lun11,*)mm,nsp,rr(mm),x(mm)
          enddo
        nit2=0
        diff2=10.
        do while ((nit2.lt.nitmx2).and.(diff2.gt.crit2))
          nit2=nit2+1
          nit3=nit3+1
          if (lpri.gt.2) write (lun11,*)'new new populations'
          call remtms(tt2)
         if (lpri.gt.2) 
     $    write (lun11,*)'in diff2 loop',abs(tt2-tt1)
          do mmm=1,ipmat
           mm=ipmat+1-mmm
            riu=0.
            rui=0.
            ril=0.
            rli=0.
            do nn=mm+1,ipmat
              riu=riu+abs(ajis(nn,mm))
              rui=rui+abs(ajis(mm,nn))*x(nn)
              enddo
            do nn=1,mm-1
              ril=ril+abs(ajis(nn,mm))
              rli=rli+abs(ajis(mm,nn))*x(nn)
              enddo          
            xoo(mm)=x(mm)
            x(mm)=(rli+rui)/(ril+riu+1.e-34)
c            x(mm)=(rli+rui)/abs(ajis(mm,mm)+1.e-24)
            if (lpri.gt.2) write (lun11,*)mm,riu,rui,ril,rli,x(mm),
     $                                     xoo(mm)
            enddo
          xsum=0.
          do mm=1,ipmat
            xsum=xsum+x(mm)
            enddo
          if (lpri.gt.2) write (lun11,*)'new and old populations',ll,
     $                      xsum
          do mm=1,ipmat
            x(mm)=x(mm)/xsum
            enddo
          m2=1
          diff2=0.
          tst=0.
          do while ((diff2.lt.1.e+3).and.(x(m2).gt.1.e-24)
     $           .and.(m2.le.ipmat).and.(tst.lt.1.e+3))
            tst=xoo(m2)/x(m2)
            diff2=diff2+(tst-1.)*(tst-1.)
            if (lpri.gt.2) write (lun11,*)m2,x(m2),xoo(m2),xo(m2),
     $            diff2
            m2=m2+1
            enddo
          if (lpri.gt.2) write (lun11,*) 'diff2=',diff2,nit2
          enddo
        diff=0.
        m2=1
        do while ((m2.le.ipmat).and.(diff.lt.1.e+3))
          tst=xo(m2)*(1.e-30)
          if ((x(m2).gt.tst).and.(diff.lt.1.e+10))
     $     diff=diff+(min(1.e+10,(xo(m2)/(x(m2)+1.e-34)-1.)))**2
          if (lpri.gt.2) write (lun11,*)m2,x(m2),xo(m2),
     $            diff
          m2=m2+1
          enddo
        if (lpri.gt.2) write (lun11,*) 'diff=',diff
      enddo
c
      if (lpri.gt.2) write (lun11,*)'heating-cooling:'
      cl=0.
      ht=0.
      do mm=1,ipmat
        do nn=1,ipmat
          if (cjis(mm,nn).gt.0.) then
              cl=cl+x(mm)*cjis(mm,nn)
            else
              ht=ht-x(mm)*cjis(mm,nn)
            endif
          if ((lpri.gt.2).and.(abs(cjis(mm,nn)).gt.1.e-34))
     $         write (lun11,*)mm,nn,x(mm),cjis(mm,nn),ht,cl
          enddo
        enddo
      go to 9090
      if (lpri.gt.2) write (lun11,*)'heating-cooling superlevels:'
      clp=0.
      htp=0.
      do mm=1,nspmx
        do nn=1,nspmx
          if (cjissup(mm,nn).gt.0.) then
              clp=clp+xsup(mm)*cjissup(mm,nn)
            else
              htp=htp-xsup(mm)*cjissup(mm,nn)
            endif
          if ((lpri.gt.2).and.(abs(cjissup(mm,nn)).gt.1.e-34))
     $         write (lun11,*)mm,nn,xsup(mm),cjissup(mm,nn),htp,clp
          enddo
        enddo
      ht=htp
      cl=clp
 9090 continue
c
      lpri=lprisv
c
      return
      end

 

      function nbinc(e,epi)
c
c
c     this function bins the continuum
c     lines between   epi(i) and   epi(i+1) are put in bin number i.
c     energies between 0 and   epi(1) are put in bin number 50.
c
c
      parameter (ncn=9999)
c
c
      dimension epi(ncn)
c
c      call hunt(epi,ncn,e,jlo,0,lun11)
      numcon = ncn
      numcon2=max(2,ncn/50)
c      numcon2=200
      numcon3=numcon-numcon2
      call huntf(epi,numcon3,e,jlo,0,lun11)
      nbinc=jlo
      return
      end
c ***********************************************************************
      subroutine newqij(tem,keq,ncoef,gcoef,gij,error) 
c
c returns the value of gamma(i,j) (gij) at a given t=tem. keq is the  
c type of equation, qmd(ncoef) contains the coefficients read in.
c if 'error=true' t is out of the range of the range of the gamma
c equation and no value is returned in gij.
c
c ***********************************************************************

       dimension gcoef(ncoef)
       logical error

       error=.false.

       if (keq.eq.60) then
        tt=tem*6.333e-6
        tmin=gcoef(1)
        tmax=gcoef(2)
        if (tt.ge.tmin .and. (tt.le.tmax .or. tmax.eq.0.)) then
         gam=0.
         do 25 k=2,ncoef-1
 25      gam=gam+gcoef(k+1)*(tt**k)
         gij=gam
        else
         error=.true. 
        endif
        return
       endif
       if (keq.eq.59) then
        tt=tem*6.333e-6
        tmin=gcoef(1)
        tmax=gcoef(2)
        if (tt.ge.tmin .and. (tt.le.tmax .or. tmax.eq.0.)) then
         gam=0.
         do 30 k=2,ncoef-3 
 30      gam=gam+gcoef(k+1)*(tt**k)
         gam=gam+gcoef(ncoef-2)*log(tt*gcoef(ncoef-1))*
     1     expo(-gcoef(ncoef)*tt)
         gij=gam
        else
         error=.true.
        endif
        return
       endif
       end
      function nfindgd(n,x,x0)
c
c     this function find the grid number which the input parameter lies in.
c
      dimension x(n)
c      write (lun11,*)'in nfindgd',n,x0,x
      nfindgd=0
      if(x0.lt.x(1)) then
        nfindgd=1
        return
      end if
      if(x0.ge.x(n)) then
        nfindgd=n-1
        return
      end if
      do 10 i=1,n-1
c        write (lun11,*)'in nfindgd ',i,x(i),x0
        if(x0.ge.x(i).and.x0.lt.x(i+1)) then
          nfindgd=i
          return
        end if
10    continue
      return
      end
      function pescl(tau)
c
c     this routine calculates escape probability for a line transition
c
c     inputs: optical depths-- tau for a line transition 
c
c
      data pi/3.1415927/
c
      tauw=1.e5
c     *** need to determine tauw from line profiles?***
c
      if(tau.lt.1.0) then
        if(tau.lt.1.e-5) then
          pescl=1.0
          go to 10
        end if
        aa=2.0*tau
        pescl=(1.0-expo(-aa))/aa
        go to 10
      end if
      bb=0.5*sqrt(log(tau))/(1.0+tau/tauw)
      pescl=1.d0/(tau*sqrt(pi)*(1.2+bb))
10    continue
      pescl=pescl/2.0
      return
      end
      function pescv(tau)
c
c     this routine calculates escape probability for 
c       continuum
c
c     inputs: optical depths-- tau, energy
c
c
c      pescv=expo(-tau)/2.
c      return
c
      pescv=0.5
      return
c      if (e.lt.13.6) return
c     fudge because of too much case b
c      taubar=tau/200.
c      taubar=tau/5.
      taubar=tau*100.
      pescv=1./(taubar+1.)
c      pescv=expo(-taubar)
c     fudge because of numerical problem with large tau.
      eps=1.e-6
      pescv=max(pescv,eps)
      pescv=pescv/2.
      return
      end
c   ROUTINE PEXS
c
      subroutine pexs(nmin,kdim,zc,eion,far,gam,scal,
     +                e,axs,ierr,lpri,lun11)
c
c     Compute photoexcitation cross-section assuming one
c     Rydberg serie converging to a threshold.
c
c     nmin = starting princ. quant. num. of the serie
c     zc = effective charge, Z-Ne+1
c     eion = threshold energy in Ry
c     far = oscillator strength of the serie member with
c            n=nmin
c     gam = resonance width in Ry
c     e = external energy grid in Ry
c     kdim = dimension of the external energy grid
c     axs = cross section
c     scal = scaling factor
c     ierr = error indicator (=0: OK; .ne.0:error)
c
c
      implicit real*8 (a-h,o-z)
      parameter(pi=3.14159,nmax=30)
      dimension x(nmax),a(nmax),e(*),axs(*)
      data x,a/nmax*0.,nmax*0./
c
      if (lpri.ne.0) write (lun11,*)'in pexs',nmin,kdim,zc,
     $        eion,far,gam,scal
c
      ierr=0
      if(nmin.ge.nmax) then
       ierr=1
       return
      endif
      nres=nmax
      do 10 n=nmin,nmax
c
c   energy of the resonance ...
c
       x(n)=-(zc/n)**2
c
c   area of the resonance ...
c
       a(n)=8.06725*far*(nmin**3)/(n**3)
c
c   search for unresolved limit in term of member ...
c
       if(n.gt.nmin) then
        del=x(n)-x(n-1)
         res=gam/2.
        if(del.gt.res) nres=n
       endif

   10 continue
c
c   define shifted energy range ...
c
      xmin=x(nmin)-30*gam
      xres=x(nres)
      jmin=1
      jmax=kdim
      jres=jmax
      do 20 i=1,kdim
        axs(i)=0.
       e(i)=e(i)-eion
       if(i.gt.1.and.e(i-1).le.xmin.and.e(i).gt.xmin) 
     +    jmin=i-1
       if(i.gt.1.and.e(i-1).le.xres.and.e(i).gt.xres)
     +    jres=i-1
       if(i.gt.1.and.e(i-1).lt.0..and.e(i).ge.0.)
     +    jmax=i-1
   20 continue
      if(jmin.eq.jmax) jmax=jmin+1
      do 30 ii=nmin,nmax
       do 30 jj=jmin,jres
c
c   constant-width Lorentzian resonances ...
c
        axtp=a(ii)/pi*gam/2.
     +  /((e(jj)-x(ii))**2+(gam/2.)**2)
c
c   near-threshold pill-up (oscill. strength conservation)
c
     +  +a(ii)/pi*gam/2./((abs(e(jj))-x(ii))**2
     +   +(gam/2.)**2)
        axs(jj)=axs(jj)+axtp
c      if (lpri.ne.0) write (lun11,*)'30 loop',jj,e(jj),axtp,axs(jj)
   30 continue
      do 40 kk=1,kdim
c
c   near-threshold extrapolation ...
c
       if(kk.ge.jres.and.kk.le.jmax) axs(kk)=axs(jres)
c
c   scaling of the xs ...
c
       axs(kk)=scal*axs(kk)
c
c   return to the "usual" energy grid ...
c 
       e(kk)=e(kk)+eion
       if (lpri.ne.0)
     $  write (lun11,*)kk,e(kk),axs(kk)
   40 continue
c
c
      return
c
      end
      subroutine phint5384(stmpp,etmpp,ntmp,ethi,pirt,rrrt,piht,rrcl,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lpri,epi,bremsa,t,trad,swrat,xnx,crit,
     $ lfast,lun11)
c
c
c     this routine does the integration over the spectrum as required by
c     photo.  
c     this is my version from 11/1/99 which performs successive bisections
c      g stands for good
c
      parameter (ncn=9999)
c
      dimension bremsa(ncn),epi(ncn),stmpp(ntmp),etmpp(ntmp)
      dimension rccemis(2,ncn),opakc(ncn)
c
      data ergsev/1.602197e-12/
      data bk/1.38062e-16/
c
      data delt/1.e-28/
c
      eth=ethi
c
      lprisv=lpri
      if (lpri.gt.1) write (lun11,*)'in phint5384:',
     $      eth,xnx,swrat,t,nb1,
     $      stmpp(1),etmpp(1),ntmp,lfast,abund1
      ekt=t*0.861707
c
      tm=t*1.e4
      bktm=bk*tm/ergsev
      tsq = sqrt(t)
      rnist=(5.216e-21)*swrat/t/tsq
c
      ekt=t*0.861707
      q2=2.07e-16*(tm**(-1.5))
      rs=q2
      ethsht=eth/bktm
      ethsht=amax1(ethsht,0.)
      explev2=expo(-ethsht)
      rniss=swrat/(explev2/rs)
      rat3=rniss
c
      numcon2=max(2,ncn/50)
      nphint=ncn-numcon2
c
      opakab=0.
      ener=ethi+etmpp(1)*(13.598)
      nb1=nbinc(ener,epi)
      kl=nb1
      if (kl.ge.nphint) return
c      if (epi(kl+1).lt.ethi) return
      sgtmp=stmpp(1)
      crit=0.005
      epii=epi(kl)
      jk=1
      do while (jk.le.ntmp)
        ener=ethi+etmpp(jk)*(13.598)
        sgtmp=stmpp(jk) 
        do while ((kl.lt.nphint).and.
     $     ((epi(kl).lt.ener).or.(jk.eq.ntmp))) 
          kl=kl+1
          tempro=tempr
          epiio=epii
          epii=epi(kl)
          if (jk.eq.ntmp) then
              sgtp=sgtmp*(epii/ener)**(-3)
            else
              sgtp=sgtmp
            endif
          ansar1=sgtp
          optmp=abund1*ansar1*xpx
          if (kl.le.(nb1+1)) opakab=optmp
          opakc(kl)=opakc(kl)+optmp
          enddo
        if (lpri.gt.1) then
              write (lun11,*)jk,kl,ener,epi(kl),sgtmp,
     $                  sgtp,optmp
              endif
        jk=jk+1
        enddo
c
      if (lpri.gt.1) write (lun11,*)'in phint53:',eth,opakab
      lpri=lprisv
c
c
      return
      end
c      

      subroutine phint53(stmpp,etmpp,ntmp,ethi,pirt,rrrt,piht,rrcl,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lpri,epi,bremsa,t,trad,swrat,xnx,crit,
     $ lfast,lun11)
c
c
c     this routine does the integration over the spectrum as required by
c     photo.  
c     this is my version from 11/1/99 which performs successive bisections
c      g stands for good
c
      parameter (ncn=9999)
c
      dimension bremsa(ncn),epi(ncn),stmpp(ntmp),etmpp(ntmp)
      dimension rccemis(2,ncn),opakc(ncn)
c
      data ergsev/1.602197e-12/
      data bk/1.38062e-16/
c
      data delt/1.e-28/
c
      eth=ethi
c
c
      lprisv=lpri
      if (lpri.ge.1) write (lun11,*)'in phint53:',
     $      eth,xnx,swrat,t,nb1,
     $      stmpp(1),etmpp(1),ntmp,lfast,ptmp1,ptmp2
      ekt=t*0.861707
c
      tm=t*1.e4
      bktm=bk*tm/ergsev
      tsq = sqrt(t)
      rnist=(2.61e-21)*swrat/t/tsq
c
      if (lpri.ge.1) write (lun11,*)'in phint53:',
     $      eth,rnist,xnx,swrat,t,ndelt,nphint,nb1,
     $      stmpp(1),etmpp(1),ntmp,lfast
c
      numcon2=max(2,ncn/50)
      nphint=ncn-numcon2
c
      sumr = 0.
      sumh = 0.
      sumho=0.
      tempr = 0.  
      sumc = 0.
      sumi=0.
      tempi=0.
      atmp2=0.
      ansar2=0.
      rctmp1=0.
      rctmp2=0.
      htmp=0.
      ener=ethi+etmpp(1)*(13.598)
      nb1=nbinc(ener,epi)
      kl=nb1
      if (kl.ge.nphint) return
      if (epi(kl+1).lt.ethi) return
      sgtmp=stmpp(1)
      rcsum=0.
      crit=0.005
      tempr=0.
      dels=0.
      epii=epi(kl)
      jk=0
      do while ((jk.lt.ntmp).and.
     $          (ethi+etmpp(jk+1)*(13.598).lt.epi(nphint-1))
     $    .and.(abs(sumh/(sumho+1.e-24)-1.).gt.1.e-6)) 
        jk=jk+1
        enero=ener
        ener=ethi+etmpp(jk)*(13.598)
        sgtmpo=sgtmp
        sgtmp=stmpp(jk) 
        if (lpri.ne.0) write (lun11,*)jk,ener,kl,epi(kl),kl,nphint
        if (epi(kl).lt.ener) then
          dels=(sgtmp-sgtmpo)/(ener-enero+1.e-24)
          sgmn=min(sgtmp,sgtmpo)
          sgmx=max(sgtmp,sgtmpo)
          do while ((kl.lt.nphint).and.(epi(kl).lt.ener)) 
            kl=kl+1
            sgtpo=sgtp
            sgtp=max(sgmn,
     $                    min(sgmx,sgtmpo+dels*(epi(kl)-enero)))
            ansar1=sgtp
            bremtmp=bremsa(kl)/(12.56)
            tempro=tempr
            epiio=epii
            epii=epi(kl)
            tempr=(12.56)*sgtp*bremtmp/epii
            deld = epii - epiio
            sumr = sumr + (tempr+tempro)*deld/2.
            sumho=sumh
            sumh=sumh+(tempr*epii+tempro*epiio)*deld/2.
            exptst=(epii-eth)/bktm
            if (exptst.lt.20.) then
              exptmp=expo(-exptst)
              bbnurj=epii*epii*epii*(1.571e+22)*2.
              bbnu=bbnurj/(expo(epii/bktm)-1.)
              tempi1=rnist*bbnurj*exptmp*sgtp/epii
              tempi2=rnist*bremtmp*exptmp*sgtp/epii              
c              tempi2=0.
              tempio=tempi
              Atmp2o=atmp2
              tempi=(tempi1+tempi2)*(ptmp1+ptmp2)
              atmp2=tempi*epii
              sumi = sumi + (tempi+tempio)*deld/2.
              sumc = sumc+(atmp2+atmp2o)*deld/2.
              rccemis(1,kl)=rccemis(1,kl)+abund2*atmp2*ptmp1*xpx*xnx
              rccemis(2,kl)=rccemis(2,kl)+abund2*atmp2*ptmp2*xpx*xnx
c              write (lun11,*)kl,epi(kl),sgtp,bbnurj,bremtmp,bbnu,
c     $                              atmp2,tempr,tempi,rat,rat3
              endif
            optmp=abund1*ansar1*xpx
            if (kl.le.(nb1+1)) opakab=optmp
            opakc(kl)=opakc(kl)+optmp
c            write (lun11,*)kl,epi(kl),ansar1(kl),dels,sgtmp,
c     $        sgtmpo,ener,enero,tst,sumr,tsti,sumi
            enddo
          endif
        if (lpri.ge.1) then
          write (lun11,901)jk,kl,ener,epi(kl),sgtmp,bremtmp,tempr,
     $                        tst,sumr
 901      format(1x,2i6,7(1pe11.3))
c          write (lun11,*)sgtmpo,enero,dels
c          write (lun11,*) tempr,sumr,sumh,tst
          endif
        enddo
c
      pirt = pirt + sumr
      rrrt = rrrt + xnx*sumi
      piht = piht + sumh*ergsev
      rrcl = rrcl + xnx*sumc*ergsev
         
      if (lpri.ge.1) write (lun11,*)'in phint53:',eth,pirt,rrrt
     $         ,piht,rrcl,npass
      lpri=lprisv
c
c
      return
      end
      subroutine phint53hunt(stmpp,etmpp,ntmp,ethi,pirt,rrrt,piht,rrcl,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lpri,epi,bremsa,t,trad,swrat,xnx,crit,
     $ lfast,lun11)
c
c
c     this routine does the integration over the spectrum as required by
c     photo.  
c     this is my version from 11/1/99 which performs successive bisections
c      g stands for good
c
      parameter (ncn=9999)
c
      dimension bremsa(ncn),epi(ncn),stmpp(ntmp),etmpp(ntmp)
c
c
      data ergsev/1.602197e-12/
      data bk/1.38062e-16/
c
      dimension ansar1(ncn),ansar2(ncn),luse(9999)
      dimension rccemis(2,ncn),opakc(ncn)
c
      data delt/1.e-28/
c
c
c     initialize.
       pirt =0.
       rrrt =0.
       piht=0.
       rrcl=0.
c
      nb1=nbinc(ethi,epi)+1
      eth=ethi
      numcon=ncn
      numcon2=max(2,ncn/50)
      numcon3=numcon-numcon2
      if (lpri.ge.1) write (lun11,*)'in phint53:',
     $      eth,rnist,xnx,swrat,t,ndelt,nphint,nb1,
     $      stmpp(1),etmpp(1),ntmp,lfast
      if (nb1.ge.numcon3) return
c
      tm=t*1.e4
      bktm=bk*tm/ergsev
      tsq = sqrt(t)
c      rnist=(81.93)*swrat/t/tsq
      rnist=(5.216e-21)*swrat/t/tsq
c
c     from levwk
      ekt=t*0.861707
c      tev3s2=ekt*sqrt(ekt)
      q2=2.07e-16*(tm**(-1.5))
c     another factor I don't understand
c      q2=q2*25.3
      rs=q2
      ethsht=eth/bktm
      ethsht=amax1(ethsht,0.)
      explev2=expo(-ethsht)
      rniss=swrat/(explev2/rs)
      rat3=rniss
c
      lprisv=lpri
c      lpri=2
c
c     first find range
      kl=numcon3/2
      lprie=0
c      call enxt(ethi,nb1,lprie,epi,t,lfast,lun11,
c     $                  kl,nskp,nphint,lrcalc)    
      emaxx=etmpp(ntmp)*(13.598)+eth
      nphint=nbinc(emaxx,epi)
      ndelt=nphint-nb1
      if (lpri.ge.1) write (lun11,*)'in phint53:',
     $      eth,rnist,xnx,swrat,t,ndelt,nphint,nb1,
     $      stmpp(1),etmpp(1),etmpp(ntmp),ntmp,lfast
      delt=float(ndelt)
      itmp=int(log(delt)/(0.69315)+0.5)
 1011 continue
      ndelt=2**itmp
      nphint=nb1+ndelt
      etst=0.
      if (nphint.le.numcon3) then
     $ etst=(epi(nphint)-eth)/13.598 
      if ((nphint.gt.numcon3).or.(etst.gt.etmpp(ntmp))) then
        itmp=itmp-1
        if (itmp.gt.1) go to 1011
        endif
      if (lpri.ge.1) write (lun11,*)'in phint53:',
     $      eth,rnist,xnx,swrat,t,ndelt,nphint,nb1,
     $      stmpp(1),etmpp(1),ntmp,lfast
c
      do kl=max(1,nb1-1),nphint
        luse(kl)=0
        enddo
c
c     now step thru successive approximations
c      nskp=ndelt*2
      nskp=ndelt
c      if (lfast.le.2) nskp=2
      sumr = 0.
      sumh = 0.
      sumc = 0.
      sumi=0.
      npass=0
 102    npass=npass+1
        nskp=max(1,nskp/2)
c
        sumro=sumr
        sumho=sumh
        sumio=sumi
        sumco=sumc
        sumr = 0.
        sumh = 0.
        sumc = 0.
        sumi=0.
        tempr = 0.  
        tempi=0.
        atmp2=0.
        jlo=1
        ener = epi(nb1)
        kl=nb1-1
        kl=max(kl,1)
 101      continue
          enero=ener
          epii=epi(kl)
          ener=epii
          bremtmp=bremsa(kl)/(25.3)
          tempio=tempi
          atmp2o=atmp2
          sgtmp=0.
          if (ener.ge.eth) then
            if (luse(kl).eq.0) then
                efnd=(ener-eth)/13.598
                lprif=lpri
c                if ((lpri.gt.1).and.(npass.le.1)) lprif=1
                call find53(stmpp,etmpp,ntmp,efnd,sgtmp,jlo,lun11,lprif)
                ansar1(kl)=sgtmp
                if (lprif.ge.1) write (lun11,*)'after find53:',
     $                  jlo,efnd,sgtmp
                exptst=(epii-eth)/bktm
                exptmp=expo(-exptst)
c                bbnurj=2.*epii*epii*epii*(1.571e+22)
                bbnurj=epii*epii*epii
                tempi1=rnist*bbnurj*sgtmp*exptmp*(1.571e+22)/epii
                tempi2=rnist*bremtmp*sgtmp*exptmp/epii              
c                tempi2=0.
                tempi=tempi1+tempi2
c                atmp2=tempi1*epii
                atmp2=tempi*epii
                ansar2(kl)=atmp2
              else
                sgtmp=ansar1(kl)
                atmp2=ansar2(kl)
                tempi=atmp2/epii
              endif
            endif
          tempro=tempr
          tempr=(25.3)*sgtmp*bremtmp/epii
          deld = ener - enero
          tst=(tempr+tempro)*deld/2.
          sumr = sumr + tst
          sumh=sumh+(tempr*ener+tempro*enero)*deld/2.
          tsti = (tempi+tempio)*deld/2.
          sumi = sumi + tsti
          sumc = sumc+(atmp2+atmp2o)*deld/2.
          if ((lpri.ge.1).and.(npass.lt.100)) then
c              rat1=bremtmp/(9.869416e+22*bbnu+1.e-36)
c              rat1=bremtmp/(bbnurj+1.e-36)
c              rat2=tempi/(1.e-36+tempr)
c              bbnu=bbnurj/(exp(epii/ekt)-1.+1.e-36)
c              rat4=bremtmp/(bbnu+1.e-36)
              write (lun11,*)kl,ener,luse(kl),sgtmp,bremtmp,atmp2
              write (lun11,*)kl,ener,bremtmp,bbnu,bbnurj
              write (lun11,*) tempr,tempi,rnist,exptmp,bbnurj
c              write (lun11,*) rat1,rat2,rat3,rat4
              write (lun11,*) sumr,sumi,sumh,sumc,sgtmp
              endif
          luse(kl)=1
          kl=kl+nskp
          if (kl.le.nphint) go to 101
c
c
        tst3=abs((sumio-sumi)/(sumio+sumi+1.e-34))
        tst1=abs((sumro-sumr)/(sumro+sumr+1.e-34))
        tst2=abs((sumho-sumh)/(sumho+sumh+1.e-34))
        tst4=abs((sumco-sumc)/(sumco+sumc+1.e-34))
        if (lpri.ge.1) write (lun11,*)'after pass:',npass,
     $     sumr,sumh,sumi,sumc,tst1,tst2,tst3,tst4,nskp
        if (((tst3.gt.crit).or.(tst1.gt.crit)
     $    .or.(tst2.gt.crit).or.(tst4.gt.crit).or.(sumi.le.1.e-34))
     $    .and.(nskp.gt.1)) go to 102
c
         pirt = pirt + sumr
         rrrt = rrrt + xnx*sumi
         piht = piht + sumh*ergsev
         rrcl = rrcl + xnx*sumc*ergsev
c
c
         sgt4=(5.67e-5)*1.e+16*t**4
         if (lpri.ge.1) write (lun11,*)'in phint53:',eth,pirt,rrrt
     $         ,piht,rrcl,npass,sumc,ergsev
c         if (lpri.gt.1) stop
         lpri=lprisv
c
c
c
      return
      end
      subroutine phintfo(sigc,ethi,pirt,rrrt,piht,rrcl,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lpri,epi,bremsa,t,trad,swrat,xnx,lfast,lun11)
c
c
c     this routine does the integration over the spectrum as required by
c     photo.  no call to opmilne.
c
      parameter (ncn=9999)
c
      dimension bremsa(ncn),epi(ncn)
      dimension rccemis(2,ncn),opakc(ncn)
c
c
      data ergsev/1.602197e-12/
      data bk/1.38062e-16/
c
      dimension sigc(ncn)
c
      data delt/1.e-34/,eps/1.e-2/
c
      nb1=nbinc(ethi,epi)
      eth=ethi
c
      tm=t*1.e4
      bktm=bk*tm/ergsev
      tsq = sqrt(t)
      rnist=(5.216e-21)*swrat/t/tsq
c
c     from levwk
      ekt=t*0.861707
      q2=2.07e-16*(tm**(-1.5))
      rs=q2
      ethsht=eth/bktm
      ethsht=amax1(ethsht,0.)
      explev2=expo(-ethsht)
      rniss=swrat/(explev2/rs)
      rat3=rniss
c
c     initialize.
      pirt =0.
      rrrt =0.
c
      lprisv=lpri
c
c     continuum
      tempro = 0.
      tempr = 0.
      sumr = 0.
      sumh = 0.
      sumc = 0.
      tempio = 0.
      tempi=0.
      sumi=0.
      htsum=0.
      rctmp1=0.
      rctmp2=0.
      htmp=0.
      sgtmp = 0.
      if (lpri.ge.1) write (lun11,*)'in phintf:',
     $      eth,rnist,xnx,swrat,t,abund1,abund2
      ener = epi(nb1)
      sgtmp=0.
      bbnu=0.
      sum3=0.
      kl=nb1
      atmp2=0.       
 101        continue
            enero=ener
            ener = epi(kl)
            epii=ener
            sgtmp = sigc(kl)
            if (kl.eq.nb1) sigth=sgtmp
            bremtmp=bremsa(kl)/(25.3)
            tempro=tempr
            tempr=(25.3)*sgtmp*bremtmp/epii
            deld = ener - enero
            tst=(tempr+tempro)*deld/2.
            ansar1=sgtmp
            sumr = sumr + tst
            temph=tempr*ener
            sumh=sumh+(tempr*ener+tempro*enero)*deld*ergsev/2.
            exptst=(epii-eth)/bktm
            exptmp=expo(-exptst)
            bbnurj=epii*epii*epii*(1.571e+22)
            tempi1=rnist*bbnurj*exptmp*sgtmp/epii
            tempi2=rnist*bremtmp*exptmp*sgtmp/epii
            tempi=tempi1+tempi2
            atmp2o=atmp2
            atmp2=tempi1*epii
            ansar2=atmp2
            tsti = (tempi+tempio)*deld/2.
            sumi = sumi + tsti
            sumc = sumc+(atmp2+atmp2o)*deld*ergsev/2.
            rctmp1o=rctmp1
            rctmp2o=rctmp2
            rctmp1=abund2*ansar2*ptmp1*xpx*xnx
            rctmp2=abund2*ansar2*ptmp2*xpx*xnx
c            rccemis(1,kl)=rccemis(1,kl)+rctmp1
c            rccemis(2,kl)=rccemis(2,kl)+rctmp2
            optmp=abund1*ansar1*xpx
            if (kl.le.nb1+1) opakab=optmp
            htmpo=htmp
            tautmp=optmp*delr
            tmpp=optmp
            if (tautmp.gt.0.1)
     $              tmpp=(1.-expo(-tautmp))/(1.e-24+delr)
            htmp=bremsa(kl)*tmpp
            htsum=htsum+(htmp+htmpo)
     $               *(epi(kl)-epi(kl-1))*(1.602197e-12)/2.
            opakc(kl)=opakc(kl)+optmp
            if (lpri.gt.1) then
              write (lun11,*)kl,ener,bremtmp,bbnu,bbnurj
              write (lun11,*) tempr,tempi,rnist,exptmp,bbnurj
c              rat1=bremtmp/(9.869416e+22*bbnu+1.e-26)
c              rat1=bremtmp/(bbnurj+1.e-26)
c              rat2=tempi/(1.e-26+tempr)
c              bbnu=bbnurj/(exp(epii/ekt)-1.+1.e-26)
c              rat4=bremtmp/(bbnu+1.e-26)
c              write (lun11,*) rat1,rat2,rat3,rat4
              write (lun11,*) sumr,sumi,sumh,sumc,sgtmp
              endif
            tempro = tempr
            tempio = tempi
            enero = ener
            tst=1.
            lprie=0
            call enxt(ethi,nb1,lprie,epi,t,lfast,lun11,
     $                  kl,nskp,nphint,lrcalc)    
            kl=kl+nskp
            if ((kl.le.nphint).and.
     $            ((lfast.le.2).or.(tst.gt.eps))) go to 101
c            if (kl.le.ncn) go to 101
c
         pirt = pirt + sumr
         rrrt = rrrt + xnx*sumi
         piht = piht + sumh
         rrcl = rrcl + xnx*sumc
         
         sgt4=(5.67e-5)*1.e+16*t**4
         if (lpri.ge.1)
     $    write (lun11,*)'in phintf:',eth,sigth,
     $     pirt,rrrt,piht,rrcl
         lpri=lprisv
c
c
c
      return
      end
      subroutine pprint(jj,jkstep,
     $ tp,xlum,lwri,lpri,r,t,xpx,p,lcdd,numrec,npass,
     $ nnmax,nlimd,rmax,xpxcol,xi,zeta,lfix,zremsz,epi,
     $ abel,cfrac,emult,taumax,xeemin,spectype,specfile,specunit,
     $ kmodelname,nloopctl,nparms,parname,partype,parms,parcomm,
     $ lun11,tinf,xcol,vturbi,critf,
     $ delr,rdel,enlum,xee,ababs,
     $ bremsa,tau0,dpthc,tauc,
     $ idat1,rdat1,kdat1,nptrs,np2,
     $ npar,npnxt,npfi,npfirst,
     $ nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $ npconi2,ncsvn,
     $ ntotit,lnerrd,
     $ xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $ xilev,bilev,rniss,
     $ rcem,oplin,rccemis,brcems,opakc,cemab,opakab,
     $ elumab,elum,zrems)
c
c     variable categories
c
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
c      zremsz,epi,bremsa,
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
C         2 - print input parameter list
C         7 - write detailed list of ionic abundances
C         9 - print short summary line of the radial zone
C        11 - write xout_abund1.fits
C        12 - append abundance values to the data array for xout_abund1.fits
C             Doesn't actually write the file, just accumulates values.
C        17 - print column headings for this pass
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
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     line luminosities
      dimension elum(2,nnnl)
c     line emissivities
      dimension rcem(2,nnnl)
c     line opacities
      dimension oplin(nnnl)
c     line optical depths
      dimension tau0(2,nnnl)
c     energy bins
      dimension epi(ncn)
c     continuum lum
      dimension zrems(3,ncn),
     $          zremsz(ncn)
c     continuum optical depths
      dimension dpthc(2,ncn)
c     continuum flux
      dimension bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      dimension rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      dimension opakc(ncn)
c     level populations
      dimension xilev(nnml),bilev(nnml),rniss(nnml)
      dimension cemab(2,nnml),opakab(nnml)
      dimension elumab(2,nnml)
      dimension tauc(2,nnml)
c     ion abundances
      dimension xii(nni)
c     heating/cooling
      dimension htt(nni),cll(nni)
      dimension rrrt(nni),pirt(nni)
      dimension abel(13),ababs(13)
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
c     temporaries for dread
      dimension rdat(20000),idat(20000)
      dimension rdat2(20000),idat2(20000)
c
      dimension xcoltmp(nni)
      dimension kltmp(5000),zrtmp(999,2999),
     $   zrtmpc(999,2999),zrtmph(999,2999)
c
      save zrtmp,zrtmpc,zrtmph
c
      character(20) parname(38)
      character(10) partype(38)
      real parms(38)
      character(30) parcomm(38),kmodelname
      character(8) spectype, specfile
      character(1) kdat(20000),kdati(20000),kdatl(20000),kdtmp(100),
     $  kblnk,klablo(20),klabup(20),kdat2(20000)
      integer nparms, specunit, nloopctl
      character(8) kabstring(13)
      character(100) kinam1
      character(133) tmpst
      character(16) knam,klabs(999),kunits(999),kform(999),ktmp,
     $              kblnk16
      character(1) klev(100,nd)
      integer klen, mmlv, unit, status
      dimension rlev(10,nd),ilv(10,nd),
     $          nlpt(nd),iltp(nd)
      dimension ldon(2)
      dimension optmp(ncn)
c
c
      data kblnk/' '/,kblnk16/'                '/
      data kabstring/'H abund=','Heabund=','C abund=','N abund=',
     $               'O abund=','Neabund=','Mgabund=','Siabund=',
     $               'S abund=','Arabund=','Caabund=','Feabund=',
     $               'Niabund='/
c
c
      xnx=xpx*xee
c
c
      if ((jj.le.0).or.(jj.gt.26)) return
c
      goto (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22, 
     $23,24,25,26),
     $  jj
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
1     continue
c
      write (lun11,*)' '
      lpril=0
c     print 500 strongest emission lines
      write (lun11,*)'emission line luminosities (erg/sec/10**38))'
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
        call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
        elin=rdat(1)
        if (lrtyp.ne.14) then
          nilin=npar(ml)
          call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,nilin-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
          nilin2=idat(nidt)
          elmmtpp=(elum(2,ln)+elum(1,ln))/2.
          if (lpril.ne.0)
     $       write (lun11,*)lnn,elin,nilin,elmmtpp,ln,ml
          if ((ln.gt.0).and.(ln.lt.nnnl)
     $     .and.(elin.ge.eliml).and.(elin.le.elimh) 
     $     .and.(elin.le.8.9e+4)
     $     .and.(elmmtpp.gt.1.e-34)
     $     .and.(nilin2.gt.0).and.(nilin2.le.nni)) 
     $        then
          lmm=0
560         lmm=lmm+1
            if (lmm.gt.nlpl) go to 558
            kl2=kltmp(lmm)
            if (kl2.ne.0) 
     $       elcomp=(elum(2,kl2)+elum(1,kl2))/2.
            if (lpril.ne.0)
     $      write (lun11,*)'in 560 loop:',lmm,kl2,elmmtpp,elcomp
            if (elmmtpp.ge.elcomp) go to 558
            go to 560
558       continue
          nlpl=max0(nlpl,min(nlplmx,nlpl+1),1)
          if (lpril.ne.0)
     $       write (lun11,8516)ln,elin,elmmtpp,lmm,nlpl,kl2,elcomp
 8516     format (1h ,i4,2e12.4,3i4,e12.4)
          kltmpo=ln
          do  k=lmm,nlplmx
            kl2=kltmp(k)
            if (kl2.ne.0)
     $       elcomp=(elum(2,kl2)+elum(1,kl2))/2.
            if ((lpril.ne.0).and.(kltmp(k).ne.0))
     $          write (lun11,*)'in 557 loop',k,kltmp(k),elcomp
            kltmpn=kltmp(k)
            kltmp(k)=kltmpo
            kltmpo=kltmpn
            enddo
          if (lpril.ne.0)
     $     write (lun11,*)'done with 557 loop',lm
          endif
          endif
        enddo
      kltmp(nlpl)=kltmpo
      nlpl=nlpl-1
      write (lun11,959)
 959  format (1x,'index, ion, wavelength, transmitted, reflected')
      do  kk=1,nlpl
        if (lpril.ne.0)
     $    write (lun11,*)'kk=',kk
        ln=kltmp(kk)
        if (ln.ne.0) then
          ml=nplin(ln)
          if (ml.ne.0) then
            if (lpril.ne.0)
     $      write (lun11,*)'   ',ln,ml
            call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
            elin=rdat(1)
            nilin=npar(ml)
            call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,nilin-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
            do mm=1,nkdt
              kdtmp(mm)=kdat(mm)
              enddo
            do mm=nkdt+1,9
              kdtmp(mm)=kblnk
              enddo  
            nilin=idat(3)
            if (lpril.ne.0)
     $      write (lun11,*)ml,nilin,npar(ml)
            write (lun11,9955)kk,ln,(kdtmp(mm),mm=1,9),elin,
     $      elum(1,ln),elum(2,ln)
 9955       format (1x,2i8,1x,9a1,3(1pe13.5))
            endif
          endif
        enddo
      write (lun11,993)
      return
c
 19   continue
c
c
      write (lun11,*)' '
      lpril=0
c     print 500 strongest recombination continua
      write (lun11,*)'recombination continuum luminosities',
     $  '(erg/sec/10**38))'
      write (lun11,*)'index, ion, level, energy (eV), RRC luminosity '
C     lpril is flag for printing debug information
      if (lpril.ne.0) then
        write (lun11,*)'raw data'
        do j=1,nnml
          if (xilev(j).gt.1.e-37)
     $     write (lun11,*)j,xilev(j),elumab(1,j)
          enddo
        endif
C      initialize line counter
       mmlv=0
       ipmat=0
C      First look for element data (jk is element index)
        klel=11
        mlel=npfirst(klel)
        jk=0
        do while (mlel.ne.0)
          jk=jk+1
          mt2=mlel-1
          call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mt2,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
          mllel=idat(nidt)
          xeltp=rdat(1)
          xeltp=abel(mllel)
          nnz=idat(1)
          if (lpril.ne.0) 
     $      write (lun11,*)'element:',jk,mlel,mllel,nnz,
     $                  (kdat(mm),mm=1,nkdt),xeltp
C         ignore if the abundance is small
          if (xeltp.lt.1.e-10) then
              jkk=jkk+nnz
            else
c             now step thru ions (jkk is ion index)
              klion=12
              mlion=npfirst(klion)
              jkk=0
              kl=0
              do while ((mlion.ne.0).and.(kl.lt.nnz))
                jkk=jkk+1
C               retrieve ion name from kdati
                call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdti,kdati,mlion-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
C               if not accessing the same element, skip to the next element
                mlleltp=idat(nidt-1)
                if (mlleltp.eq.mllel) then
                  kl=kl+1
                  if (lpril.ne.0)
     $              write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $                          (kdati(mm),mm=1,nkdti)
c                 now find level data
c                 step thru types
                  nlevmx=0
                  mltype=13
                  ml=npfi(mltype,jkk)
                  mllz=npar(ml)
c                 step thru records of this type
                  do while ((ml.ne.0).and.(npar(ml).eq.mllz))
                    call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
                    nlev=idat(nidt-1)
                    nlevmx=max(nlevmx,nlev)
                    if ((nlev.gt.0).and.(nlev.le.nd)) then
                      if (lpril.ne.0) write (lun11,*)
     $                  'level quantities:',
     $                  ml,nlev,ltyp,lrtyp,rdat(1),rdat(2)
                      do  lk=1,nrdt
                        rlev(lk,nlev)=rdat(lk)
                        enddo
                      do lk=1,nidt
                        ilv(lk,nlev)=idat(lk)
                        enddo
                      do lk=1,nkdt
                        klev(lk,nlev)=kdat(lk)
                        enddo
                      do lk=nkdt+1,20
                        klev(lk,nlev)=kblnk
                        enddo
                      endif
                    ml=npnxt(ml)
                    enddo
                  nlev=nlevmx
                  mltype=7
                  mlrdesc=mltype
                  ml=npfi(mltype,jkk)
                  mllz=npar(ml)
                  do while ((ml.ne.0).and.(npar(ml).eq.mllz))
c                   step thru records of this type
                    call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
                    kkkl=npconi2(ml)              
                    idest1=idat(nidt-1)
                    if ((kkkl.gt.0).and.(kkkl.le.ndat2)
     $                .and.(elumab(1,kkkl).gt.1.e-36)) then
                      eth=rlev(4,idest1)-rlev(1,idest1)
                      do mm=1,nkdti
                        kdtmp(mm)=kdati(mm)
                        enddo
                      do mm=nkdti+1,20
                        kdtmp(mm)=kblnk
                        enddo  
                      if (lpril.ne.0)
     $                 write (lun11,*)jkk,idest1,llo,abund1,
     $                 eth,elumab(1,kkkl),elumab(2,kkkl)
                      write (lun11,9293)kkkl,(kdtmp(mm),mm=1,20),
     $                        (klev(lk,idest1),lk=1,20),eth,
     $                        elumab(1,kkkl)
                      endif
                    ml=npnxt(ml)
                    enddo
                  endif
C               Go to next ion
                mlion=npnxt(mlion)
                enddo
            endif
          mlel=npnxt(mlel)
C         Go to next element
          enddo
      write (lun11,993)
c
      return
c
c
 23   continue
c
      write (lun11,*)' '
      lpril=0
c     print 500 strongest absoprtion lines
      write (lun11,*)'line depths'
      nlplmx=5000
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
        call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
        elin=rdat(1)
        if (lrtyp.ne.14) then
          nilin=npar(ml)
          call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,nilin-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
          nilin2=idat(nidt)
          elmmtpp=tau0(1,ln)
          if (lpril.ne.0)
     $       write (lun11,*)lnn,elin,nilin,nilin2,elmmtpp,ln,ml
          if ((ln.gt.0).and.(ln.lt.nnnl)
     $     .and.(elin.ge.eliml).and.(elin.le.elimh) 
     $     .and.(elin.le.8.9e+4)
     $     .and.(elmmtpp.gt.1.e-34)
     $     .and.(nilin2.gt.0).and.(nilin2.le.nni)) 
     $     then
          lmm=0
760         lmm=lmm+1
            if (lmm.gt.nlpl) go to 758
            kl2=kltmp(lmm)
            if (kl2.ne.0) 
     $       elcomp=tau0(1,kl2)
            if (lpril.ne.0)
     $      write (lun11,*)'in 560 loop:',lmm,kl2,elmmtpp,elcomp
            if (elmmtpp.ge.elcomp) go to 758
            go to 760
758       continue
          nlpl=max0(nlpl,min(nlplmx,nlpl+1),1)
          if (lpril.ne.0)
     $       write (lun11,8516)ln,elin,elmmtpp,lmm,nlpl,kl2,elcomp
          kltmpo=ln
          do  k=lmm,nlplmx
            kl2=kltmp(k)
            if (kl2.ne.0)
     $       elcomp=tau0(1,kl2)
            if ((lpril.ne.0).and.(kltmp(k).ne.0))
     $          write (lun11,*)'in 557 loop',k,kltmp(k),elcomp
            kltmpn=kltmp(k)
            kltmp(k)=kltmpo
            kltmpo=kltmpn
            enddo
          if (lpril.ne.0)
     $     write (lun11,*)'done with 557 loop',lm
          endif
          endif
        enddo
      if (nlpl.le.1) return      
      kltmp(nlpl)=kltmpo
      nlpl=nlpl-1
      write (lun11,959)
      do  kk=1,nlpl
        ln=kltmp(kk)
        if (lpril.ne.0)
     $    write (lun11,*)'kk=',kk,ln
        if ((ln.gt.0).and.(ln.le.np2)) then
          ml=nplin(ln)
          if (ml.ne.0) then
            if (lpril.ne.0)
     $      write (lun11,*)'   ',ln,ml
            call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
            elin=rdat(1)
            nilin=npar(ml)
            if ((nilin.gt.0).and.(nilin.lt.np2)) then
              call dread(ltyp,lrtyp,lcon,
     $        nrdt,rdat,nidt,idat,nkdt,kdat,nilin-1,
     $        idat1,rdat1,kdat1,nptrs,0,lun11)
              do mm=1,nkdt
                kdtmp(mm)=kdat(mm)
                enddo
              do mm=nkdt+1,9
                kdtmp(mm)=kblnk
                enddo  
              nilin=idat(3)
              if (lpril.ne.0)
     $        write (lun11,*)ml,nilin,npar(ml)
              if ((ln.gt.0).and.(ln.lt.nnnl))
     $         write (lun11,9955)kk,ln,(kdtmp(mm),mm=1,9),elin,
     $        tau0(1,ln),tau0(2,ln)
              endif   
            endif
          endif
        enddo
      write (lun11,993)
c
c
      return
c
 24   continue
c
      write (lun11,*)' '
      lpril=0
c     print 500 strongest absorption edges
      write (lun11,*)'continuum depths'
      write (lun11,*)'index, ion, level, energy (eV), depth '
C     lpril is flag for printing debug information
      if (lpril.ne.0) then
        write (lun11,*)'raw data'
        do j=1,nnml
          if (xilev(j).gt.1.e-37)
     $     write (lun11,*)j,xilev(j),tauc(1,j)
          enddo
        endif
C      initialize line counter
       mmlv=0
       ipmat=0
C      First look for element data (jk is element index)
       klel=11
       mlel=npfirst(klel)
       jk=0
 2040    jk=jk+1
         mt2=mlel-1
         call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mt2,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
         mllel=idat(nidt)
         xeltp=rdat(1)
         xeltp=ababs(mllel)
         nnz=idat(1)
         if (lpril.ne.0) 
     $      write (lun11,*)'element:',jk,mlel,mllel,nnz,
     $                  (kdat(mm),mm=1,nkdt)
C        ignore if the abundance is small
         if (xeltp.lt.1.e-10) then
            jkk=jkk+nnz
            go to 2079
            endif
c        now step thru ions (jkk is ion index)
         klion=12
         mlion=npfirst(klion)
         jkk=0
         kl=0
 2141      continue
           jkk=jkk+1
C          retrieve ion name from kdati
           call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdati,mlion-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
C          if not accessing the same element, skip to the next element
           mlleltp=idat(nidt-1)
           if (mlleltp.ne.mllel) go to 2049
           nkdti=min(19,nkdt)
           do mm=1,nkdt
             kdtmp(mm)=kdati(mm)
             enddo
           do mm=nkdt+1,20
             kdtmp(mm)=kblnk
             enddo  
           kl=kl+1
           if (lpril.ne.0)
     $      write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $                          (kdati(mm),mm=1,nkdt)
c          now find level data
c          step thru types
           nlevmx=0
           mltype=13
           ml=npfi(mltype,jkk)
           mllz=npar(ml)
c          step thru records of this type
 2949      continue
              call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
              nlev=idat(nidt-1)
              nlevmx=max(nlevmx,nlev)
              if ((nlev.gt.0).and.(nlev.le.nd)) then
                nlpt(nlev)=ml
                iltp(nlev)=ltyp
                if (lpril.ne.0) write (lun11,*)'level quantities:',
     $            ml,nlev,ltyp,lrtyp,rdat(1),rdat(2)
                do  lk=1,nrdt
                  rlev(lk,nlev)=rdat(lk)
                  enddo
                do lk=1,nidt
                  ilv(lk,nlev)=idat(lk)
                  enddo
                do lk=1,nkdt
                  klev(lk,nlev)=kdat(lk)
                  enddo
                do lk=nkdt+1,20
                  klev(lk,nlev)=kblnk
                  enddo
                endif
              ml=npnxt(ml)
              if ((ml.ne.0).and.(npar(ml).eq.mllz)) go to 2949
           nlev=nlevmx
           if (lpril.ne.0) then
             write (lun11,*)'level populations:',ipmat
             do mm=1,nlev
               write (lun11,9022)mm,(klev(lk,mm),lk=1,20),
     $          rlev(1,mm),rlev(2,mm),rlev(3,mm),rlev(4,mm),
     $                xilev(mm+ipmat),rniss(mm+ipmat)
 9022          format (i4,20a1,6(1pe10.3))
               enddo
             endif
           mltype=7
           mlrdesc=mltype
           ml=npfi(mltype,jkk)
           if (ml.ne.0) then
             mllz=npar(ml)
             do while ((ml.ne.0).and.(npar(ml).eq.mllz)) 
c              step thru records of this type
               if (lpril.ne.0)
     $          write (lun11,*)ml,nptrs(2,ml)
               call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
               kkkl=npconi2(ml)              
               idest1=idat(nidt-1)
               llo=idest1+ipmat
               if (lpril.ne.0)
     $          write (lun11,*)ml,nptrs(2,ml),nptrs(3,ml),
     $                         kkkl,tauc(1,kkkl)
               if ((kkkl.gt.0).and.(kkkl.le.ndat2)
     $              .and.(tauc(1,kkkl).gt.1.e-36)) then
                  abund1=xilev(llo)
                  tau1=tauc(1,kkkl)
                  tau2=tauc(2,kkkl)
                  ett=abs(rlev(4,idest1)-rlev(1,idest1))
c                 now add on eth for upper level
                  nlevp=nlev+1
                  idest2=nlevp+idat(nidt-3)-1
                  if (lpril.ge.1) write (lun11,*)idest1,idat(nidt-3),
     $                 nlevp,idest2
                  if (idest2.gt.nlevp) then
                    jkk3=jkk+1
                    if (lpril.ge.1)
     $                write (lun11,*)jkk3,ndtmp,nlevp,idest2
                    ndtmp=npfi(13,jkk3)
                    if (lpril.ge.1)
     $               write (lun11,*)jkk3,ndtmp,nlevp,idest2
                    mllz2=npar(ndtmp)
                    iltmp=0
                    do while ((ndtmp.ne.0)
     $               .and.(iltmp.ne.(idest2-nlevp+1))
     $               .and.(npar(ndtmp).eq.mllz2)) 
                      call dread(ltyp2,lrtyp2,lcon2,
     $                 nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,ndtmp-1,
     $                 idat1,rdat1,kdat1,nptrs,0,lun11)
                      iltmp=idat2(nidt2-1)
                      if (lpril.ge.1) write (lun11,*)nidt2,iltmp,ndtmp
                      ndtmp=npnxt(ndtmp)     
                      enddo
                    ett=ett+rdat2(1)
                    if (lpril.ge.1)
     $               write (lun11,*) ndtmp,iltmp,idest2,ett
                    endif
                  do mm=1,nkdti
                    kdtmp(mm)=kdati(mm)
                    enddo
                  do mm=nkdti+1,20
                    kdtmp(mm)=kblnk
                    enddo  
                  if (lpril.ne.0)
     $             write (lun11,*)jkk,idest1,idest2,llo,abund1,
     $                 ett,tauc(1,kkkl),tauc(2,kkkl)
 9293             format (1x,i6,1x,(40a1),3(1pe13.5))
                  write (lun11,9293)kkkl,(kdtmp(mm),mm=1,20),
     $              (klev(lk,idest1),lk=1,20),ett,
     $              tauc(1,kkkl)
                  endif
               ml=npnxt(ml)
               if (lpril.ne.0) write (lun11,*)'ml=',ml,npar(ml),mllz
               enddo
             endif
           mltype=1
           mlrdesc=mltype
           ml=npfi(mltype,jkk)
           if (ml.ne.0) then
             mllz=npar(ml)
             do while ((ml.ne.0).and.(npar(ml).eq.mllz)) 
c              step thru records of this type
               call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
               kkkl=npconi2(ml)              
               idest1=idat(nidt-1)
               llo=idest1+ipmat
               if (lpril.ne.0)
     $          write (lun11,*)ml,nptrs(2,ml),nptrs(3,ml),
     $                         kkkl,tauc(1,kkkl)
               if ((kkkl.gt.0).and.(kkkl.le.ndat2)
     $              .and.(tauc(1,kkkl).gt.1.e-36)) then
                  abund1=xilev(llo)
                  tau1=tauc(1,kkkl)
                  tau2=tauc(2,kkkl)
                  if (ltyp.eq.59) then
                      eth=rdat(1)
                    else
                      ett=abs(rlev(1,idest1)-rlev(4,idest1))
                      idest2=nlevp+idat(nidt-3)-1
                      nlevp=nlev+1
                      if (lpri.ge.1) write (lun11,*)idest1,idat(nidt-3),
     $                  nlevp,idest2
                      if (idest2.gt.nlevp) then
                        jkk3=jkk+1
                        if (lpri.ge.1)
     $                    write (lun11,*)jkk3,ndtmp,nlevp,idest2
                        ndtmp=npfi(13,jkk3)
                        if (lpri.ge.1)
     $                   write (lun11,*)jkk3,ndtmp,nlevp,idest2
                        mllz2=npar(ndtmp)
                        iltmp=0
                        do while ((ndtmp.ne.0)
     $                   .and.(iltmp.ne.(idest2-nlevp+1))
     $                   .and.(npar(ndtmp).eq.mllz2)) 
                          call dread(ltyp2,lrtyp2,lcon2,
     $                     nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,ndtmp-1,
     $                     idat1,rdat1,kdat1,nptrs,0,lun11)
                          iltmp=idat2(nidt2-1)
                          if (lpri.ge.1) write (lun11,*)nidt2,
     $                                          iltmp,ndtmp
                          ndtmp=npnxt(ndtmp)     
                          enddo
                        ett=ett+rdat2(1)
                        if (lpri.ge.1)
     $                   write (lun11,*) ndtmp,iltmp,idest2,ett
                        endif
                    endif
                  do mm=1,nkdti
                    kdtmp(mm)=kdati(mm)
                    enddo
                  do mm=nkdti+1,20
                    kdtmp(mm)=kblnk
                    enddo  
                  if (lpril.ne.0)
     $             write (lun11,*)jkk,idest1,llo,abund1,
     $                 eth,tauc(1,kkkl),tauc(2,kkkl)
                  write (lun11,9293)kkkl,(kdtmp(mm),mm=1,20),
     $              (klev(lk,idest1),lk=1,20),eth,
     $              tauc(1,kkkl)
                  endif
               ml=npnxt(ml)
               enddo
             endif
           ipmat=ipmat+nlev
 2049      continue
           mlion=npnxt(mlion)
C          Go to next ion
           if ((mlion.ne.0).and.(kl.lt.nnz)) go to 2141
 2079    continue
         mlel=npnxt(mlel)
C        Go to next element
         if (mlel.ne.0) go to 2040
      write (lun11,993)
c
      return
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
C     Print list of input parameters
c
 2    continue
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
      do j=1,13
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
      write (lun11,'(1x)')
      return

c      r19=r*(1.e-19)
c      uu1=enlum/(12.56*xpx*r19*r19)/3.e+10
c      alguu1=alog10(amax1(1.e-34,uu1))
c      skse=xlum/(xpx*r19*r19)
c      zeta=alog10(max(1.e-34,skse))
c      ecc=2.998e+10
c      ekt=t*(0.861707)*ergsev
c      sksec=skse/12.56/((1.+xee)*ekt*ecc)
c      zetac=alog10(max(1.e-34,sksec))
c      enn0=xpx
c      nlyc=nbinc(13.7,epi)
c      nry=nlyc+1
c      egam=zremsz(nry)/(2.*12.56*enn0*ecc*r19*r19+1.e-34)
c      nry=nbinc(13.6,epi)+1
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
      parms(3)=lcdd
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
      do j=1,13
         parms(17+j)=abel(j)
         enddo
      parms(31)=emult     
      parms(32)=taumax     
      parms(33)=xeemin     
      parms(34)=critf     
      parms(35)=vturbi     
      parms(36)=npass
      parms(37)=0.0
      parcomm(37)=kmodelname
      parms(38)=nloopctl      
c
      return
c
4     continue
c
c
      write (lun11,*)
     $ 'continuum opacity and emissivities (/cm**3/sec/10**38)'
      write (lun11,*)'channel, energy, opacity, sigma*e**3, rec. em.,'
     $,' brem. em., comp. em.'
      opsum=0. 
      tstar=t
      ekkr=xnx*(6.65e-25)
      ekkr=max(1.e-20,ekkr)
      optpp=max(opakc(1),ekkr)
      fstr=0.
      rsum1=0.
      rsum2=0.
      numcon=ncn
      do 135 kl=2,numcon
         
         sgtmp=(opakc(kl)*(epi(kl)/1000.)**3)/amax1(1.e-34,xpx)
         if ((kl.gt.1).and.(epi(kl).gt.100.))
     $    opsum=opsum+(opakc(kl)+opakc(kl-1))*(epi(kl)-epi(kl-1))/2. 
         i=kl
         tmp = epi(i)*1.16/tstar
         crayj = 1./tmp
         fstro = fstr
         if ( tmp.le.50. ) then
            if ( tmp.gt.1.e-4 ) crayj = 1./(expo(tmp)-1.)
            crayj=crayj*crayj
c            fstr= cconst*tmp*crayj*epi(i)**3/tstar
            fstr= tmp*crayj*epi(i)**3/tstar
         endif
         optppo=optpp
         optpp=max(opakc(kl),ekkr)
         delte=epi(kl)-epi(kl-1)
         rsum1=min(1.e+20,rsum1+(fstr/optpp+fstro/optppo)*delte/2.)
         rsum2=min(1.e+20,rsum2+(fstr+fstro)*delte/2.)
         write (lun11,967)kl,epi(kl),opakc(kl)+optmp(kl),sgtmp,
     $            rccemis(1,kl),rccemis(2,kl), brcems(kl)
967      format (1h ,i6,6(1pe13.5))
135      continue
      write (lun11,*)'opsum cont=',opsum    
      rssmn=rsum2/rsum1
c      ens1=rssmn/(t*1.e+4)**(-3.5)/xpx/xpx/1.66e-24
      write (lun11,*)'rosseland mean opacity=',t,rssmn
      write (lun11,993)
c
c
      return
c
C
5     continue
c
      elsum=0.
      do jlk=1,nlsvn
         ln=jlk
         elsum=elsum+elum(1,ln)+elum(2,ln)
         enddo
      sumtmp1=0.
      sumtmp2=0. 
      ergsev=1.602197e-12
      r19=r*1.e-19
      fpr2=12.56*r19*r19
      tmp1=zremsz(1)*(1.-expo(-dpthc(1,1)))
      tmp2=zrems(2,1)+zrems(3,1)
      do jk=2,ncn
         tmp1o=tmp1
         tmp1=zremsz(jk)*(1.-expo(-dpthc(1,jk)))
         sumtmp1=sumtmp1+(tmp1+tmp1o)*(epi(jk)-epi(jk-1))*ergsev/2.
         tmp2o=tmp2
         tmp2=zrems(2,jk)+zrems(3,jk)
         sumtmp2=sumtmp2+(tmp2+tmp2o)*(epi(jk)-epi(jk-1))*ergsev/2.  
         enddo
      err=(sumtmp1-sumtmp2-elsum)/(sumtmp1+1.e-24)
      write (lun11,9981)sumtmp1,sumtmp2,elsum,err
 9981 format (1x,'energy sums: abs, cont, line, err:',4(1pe13.5))

c      fpr2dr=12.56*delr*r19*r19
c      write (lun11,*),httot,cltot,fpr2dr,httot/(sumtmp1+1.e-24),
c     $  cltot/(elsum+sumtmp2+1.e-24)
c
c
      return

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
6     continue
c
      lun11sv=lun11
c      call getlun(lun11)
c      open(unit=lun11,file='xout_contlum.lis',status='unknown')
c
      write (lun11,*)'continuum luminosities (/sec/10**38) and depths'
      write (lun11,*)'channel,energy,inc.,trn. lum.,back. em. lum.,'
     $,'fwd. em. lum.,backward depth,forward depth'
      numcon=ncn
      do 136 kl=1,numcon
         write (lun11,968)kl,epi(kl),zremsz(kl),
     $    zrems(1,kl),zrems(2,kl),zrems(3,kl),dpthc(1,kl),dpthc(2,kl)
968      format (1h ,i6,7(1pe13.5))
136      continue
      write (lun11,993)
c
      lun11=lun11sv
c
      return
c
c
8     continue
c
c
      write (lun11,*)'line list'
      write (lun11,9943)
9943  format (1x,'     wave          element  ion  glo          gup   '
     $ ,'      fij')
      do jlk=1,nlsvn
         j=jlk
         ml=nplin(j)
         call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
c         call dprinto(ltyp,lrtyp,lcon,
c     $          nrdt,rdat,nidt,idat,nkdt,kdatl,lun11)
         if (lrtyp.ne.14) then
           elin=rdat(1)
           nilin=npar(ml)
           ergsev=1.602197e-12
           ener=ergsev*(12398.54)/amax1(elin,1.e-34)
           etst=ener/ergsev
           idest1=idat(1)
           idest2=idat(2)
           aij=rdat(3)
           elin=abs(rdat(1))
c           write (lun11,*)'aij,elin:',aij,elin
c
c          find ion data
           ml=nilin
           call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
           do ktt=1,nkdt
              write (kinam1(ktt:ktt),'(a1)')kdat(ktt)
              enddo
           do ktt=nkdt+1,9
              write (kinam1(ktt:ktt),'(a1)')kblnk
              enddo
           nii=idat(nidt-2)
           mlel=npar(nilin)
           call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,mlel-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
           nzz=idat(1)
           jkkion=idat(nidt)
c           write (lun11,*)'ion data:',kinam1,jkkion
c        
c          find level data
           lfnd=0
           gglo=0.
           ggup=0.
           do lk=1,20
             klablo(lk)=kblnk
             klabup(lk)=kblnk
             enddo
           mltype=13
           ml=npfi(mltype,jkkion)
           mllz=npar(ml)
c          step thru records of this type
 2843      continue
c             write (lun11,*)ml,nptrs(2,ml)
             call dread(ltyp,lrtyp,lcon,
     $         nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $         idat1,rdat1,kdat1,nptrs,0,lun11)
c             write (lun11,*)nkdt
             nkdt=min(nkdt,20)
             nlev=idat(nidt-1)
             nlevmx=max(nlevmx,nlev)
             if (nlev.eq.idest1) then
               eelo=rdat(1)
               gglo=rdat(2)
               do lk=1,nkdt
                 klablo(lk)=kdat(lk)
                 enddo
               do lk=nkdt+1,20
                 klablo(lk)=kblnk
                 enddo
               lfnd=lfnd+1
               endif
             if (nlev.eq.idest2) then
               eeup=rdat(1)
               ggup=rdat(2)
               do lk=1,nkdt
                 klabup(lk)=kdat(lk)
                 enddo
               do lk=nkdt+1,20
                 klabup(lk)=kblnk
                 enddo
               lfnd=lfnd+1
               endif
c             write (lun11,*)'level:',nlev,rdat(1),rdat(2),lfnd,kdat
             ml=npnxt(ml)
             if ((ml.ne.0).and.(npar(ml).eq.mllz).and.(lfnd.lt.2)) 
     $                  go to 2843
           if (lfnd.ge.2) then
             flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
             write (lun11,9944)j,elin,kinam1,aij,flin,gglo,ggup,
     $             klablo,klabup
c             write (lun11,9945)elin,nzz,nii,gglo,ggup,flin
9944         format (1h ,i9,e12.4,1x,a9,4(1pe12.4),1x,20a1,1x,20a1)
 9945        format (1h ,1pe13.5,2i8,3(1pe13.5))
             endif
           endif
         enddo
      write (lun11,993)
c
      return
c
c
10    continue
c
c
      write (lun11,993)
      write (lun11,*)'ion abundances and thermal rates (erg/sec)'
      write (lun11,947)
947   format (1x,'index, ion, abundance, recombination, ionization,',
     $' heating, cooling: ')
      klion=12
      mlion=npfirst(klion)
      lk=0
 1041    continue
         lk=lk+1
         ltyp=klion     
         call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mlion-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
         do mm=1,nkdt
             kdtmp(mm)=kdat(mm)
             enddo
         do mm=nkdt+1,9
             kdtmp(mm)=kblnk
             enddo  
         nell=npar(mlion)
         call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,nell-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
         if ((idat(nidt).gt.0).and.(idat(nidt).le.nl)) then
           abundel=ababs(idat(nidt))
           if (abundel.gt.1.e-15) 
     $      write (lun11,9046)lk,(kdtmp(mm),mm=1,9),
     $      xii(lk),rrrt(lk),pirt(lk),htt(lk),cll(lk)
9046       format (1x,i4,1x,9a1,5(1pe16.8))
           endif
         mlion=npnxt(mlion)          
         if (mlion.ne.0) go to 1041
      write (lun11,*)'total heating, cooling:',
     $            httot,cltot
      write (lun11,*)'partial heating rates: photo,compton',
     $            httot-htcomp,htcomp
      write (lun11,*)'partial cooling rates: rec,lines,brems,compton',
     $            clcont,cllines,clcomp,clbrems
      write (lun11,993)
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
      call fheader(unit,knam,kmodelname,status)

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
        call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mlion-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
c
c       get element abundance
        nelin=npar(mlion)
        ml=nelin
        call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
        mllel=idat(nidt)
        xeltp=ababs(mllel)
c
c       go back to ion data
        call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mlion-1,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)

C       Compute string length from character array by searching backwards 
C       for first non-blank character
        klen=0
        do mm=nkdt+1,9
          kdat(mm)=kblnk
          enddo
        do mm=1,9
          if(kdat(10-mm).ne.' '.and.klen.eq.0) then 
             klen=10-mm
          endif
          enddo
c       write (lun11,*)'kdat:',(kdat(mm),mm=1,9)
c       write (lun11,*)'klen:',klen
C       Replace ' ' in ion names to '_' to match FITS standard
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
c          write (lun11,*)jkl,lk,zrtmp(2,jkl),zrtmp(8+lk,jkl),
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
      call fwrtascii(unit,'ABUNDANCES                               ',
     $  zrtmp,8+lk,999,numrec,klabs,kform,kunits,lun11)
c     calculate columns
      numrec1=1      
      do lkk=1,lk
        zrtmp(8+lkk,numrec1)=xcoltmp(lkk)
        enddo
      do lkk=1,8
        zrtmp(lkk,numrec1)=0.
        enddo
c
      call fwrtascii(unit,'COLUMNS                                   ',
     $ zrtmp,8+lk,999,numrec1,klabs,kform,kunits,lun11)
c
      klabs(8+lk+1)='compton'
      kform(8+lk+1)='E11.3'
      kunits(8+lk+1)=' '
      klabs(8+lk+2)='total'
      kform(8+lk+2)='E11.3'
      kunits(8+lk+2)=' '
      call fwrtascii(unit,'HEATING                                   ',
     $ zrtmph,8+lk+2,999,numrec,klabs,kform,kunits,lun11)
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
     $ zrtmpc,8+lk+3,999,numrec,klabs,kform,kunits,lun11)
c
      call fitsclose(lun11,unit,status)
c
      return

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Add ionic abundances info in this radial zone to array for 
C     eventual inclusion in xout_abund1.fits
C     Modifies zrtmp
C
12    continue
      ergsev=1.602197e-12
      r19=r*(1.e-19)
c      write (lun11,*)enlum,xpx,r,xlum
      uu1=enlum/(12.56*xpx*r19*r19)/3.e+10
c      write (lun11,*)uu1
      alguu1=alog10(amax1(1.e-34,uu1))
      skse=xlum/(xpx*r19*r19)
      zeta=alog10(max(1.e-34,skse))
      ecc=2.998e+10
      ekt=t*(0.861707)*ergsev
c      sksec=skse/(12.56*((1.+xee)*ekt+pradl/(1.e-34+xpx))*ecc)
      sksec=skse/12.56/((1.+xee)*ekt*ecc)
      zetac=alog10(max(1.e-34,sksec))
      enn0=xpx
      nlyc=nbinc(13.7,epi)
      nry=nlyc+1
      egam=zremsz(nry)/(2.*12.56*enn0*ecc*r19*r19+1.e-34)
      nry=nbinc(13.6,epi)+1
C     Copy the values for radial zone jkstep
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
        zrtmpc(8+lk,jkstep)=htt(lk)
        zrtmph(8+lk,jkstep)=cll(lk)
        mlion=npnxt(mlion)
        enddo
      zrtmph(8+lk+1,jkstep)=htcomp
      zrtmph(8+lk+2,jkstep)=httot
      zrtmpc(8+lk+1,jkstep)=clcomp
      zrtmpc(8+lk+2,jkstep)=clbrems
      zrtmpc(8+lk+3,jkstep)=cltot
c
      return
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

13    continue
c
c
993   format (1h )
c
      return
c
14    continue
c
      write (lun11,900)
900   format ('line opacities and emissivities',
     $ ' (erg/cm**3/sec/10**38)')
      write (lun11,915)
915   format (1x,'index,wavelength,energy,ion,opacity,rec. em.,',
     $'coll. em.,fl. em.,di. em.,cx. em.')
      opsum=0.
      vvthermsc=1.3e+6*sqrt(t)/3.e+10    
      do 50 jlk=1,nlsvn
         j=jlk
         ml=nplin(j)
         call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
         if (lrtyp.eq.14) go to 50
         elin=rdat(1)
         nilin=npar(ml)
         ergsev=1.602197e-12
         ener=ergsev*(12398.54)/amax1(elin,1.e-34)
         etst=ener/ergsev
c         if ((etst.lt.elimdb(1)).or.(etst.gt.elimdb(2))) go to 50
         ml=nilin
         call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
c         write (lun11,*)nkdt,(kdat(mm),mm=1,10)
         do ktt=1,nkdt
              write (kinam1(ktt:ktt),'(a1)')kdat(ktt)
              enddo
         do ktt=nkdt+1,9
              write (kinam1(ktt:ktt),'(a1)')kblnk
              enddo
         write (lun11,904)j,elin,etst,kinam1,oplin(j),rcem(1,j),
     $                      rcem(2,j)
904      format (1h ,i9,2(1pe13.5),1x,a9,6(1pe13.5))
         if (etst.lt.100.) go to 50
         opsum=opsum+oplin(j)*etst*vvthermsc        
50       continue
       write (lun11,*)'opsum line=',opsum    
      write (lun11,993)
c
      return
c
 15   continue
c
      write (lun11,*)'line luminosities (erg/sec/10**38) and depths'
      write (lun11,9923)
9923  format (1x,' line, wavelength, ion, back. em. lum.,fwd. em. lum.,',
     $'backward depth, forward depth')
      do 147 jlk=1,nlsvn
         j=jlk
         ml=nplin(j)
         call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
         if (lrtyp.eq.14) go to 147
         elin=rdat(1)
         nilin=npar(ml)
         ergsev=1.602197e-12
         ener=ergsev*(12398.54)/amax1(elin,1.e-34)
         etst=ener/ergsev
         ilevup=idat(2)
         ilevlo=idat(1)       
c         if ((etst.lt.elimdb(1)).or.(etst.gt.elimdb(2))) go to 147
         jkko=jkk
         jkk=idat(nidt)
         if (jkk.ne.jkko) then
c          get level data
           nlevmx=0
           mltype=13
           ml=npfi(mltype,jkk)
           if (ml.ne.0) then
             mllz=npar(ml)
c            step thru records of this type
             do while ((ml.ne.0).and.(npar(ml).eq.mllz)) 
               call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
               nlev=idat(nidt-1)
               nlevmx=max(nlevmx,nlev)
c               write (lun11,9101)
c     $        ml,nlev,ltyp,lrtyp,(rdat(mm),mm=1,4),idat(1),idat(2),
c     $        idat(3),idat(nidt),(kdat(mm),mm=1,8)
 9101          format (1x,'level quantities:',4i6,4(1pe12.5),4i6,8a1)
               if ((nlev.gt.0).and.(nlev.le.nd)) then
                 nlpt(nlev)=ml
                 iltp(nlev)=ltyp
                 do  lk=1,nrdt
                   rlev(lk,nlev)=rdat(lk)
                   enddo
                 do lk=1,nidt
                   ilv(lk,nlev)=idat(lk)
                   enddo
                 do lk=1,nkdt
                   klev(lk,nlev)=kdat(lk)
                   enddo
                 do lk=nkdt+1,20
                   klev(lk,nlev)=kblnk
                   enddo
                 endif
               ml=npnxt(ml)
               enddo
             endif
           endif
         ml=nilin
         call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
         do ktt=1,nkdt
              write (kinam1(ktt:ktt),'(a1)')kdat(ktt)
              enddo
         do ktt=nkdt+1,9
              write (kinam1(ktt:ktt),'(a1)')kblnk
              enddo
         elmtp=elum(1,j)
         elmtpb=elum(2,j)
         write (lun11,9924)j,elin,kinam1,
     $    elmtp,elmtpb,tau0(1,j), tau0(2,j)
9924      format (1h ,i9,1pe13.5,1x,a9,1x,4(1pe13.5))
147      continue
      write (lun11,993)
c
      return
c
c
c
 18   continue
c
      write (lun11,*)'line wavelengths and levels'
      do jlk=1,nlsvn
         j=jlk
         ml=nplin(j)
         call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
         elin=rdat(1)
         nilin=npar(ml)
         ergsev=1.602197e-12
         ener=ergsev*(12398.54)/amax1(elin,1.e-34)
         etst=ener/ergsev
         ilevup=idat(2)
         ilevlo=idat(1)       
         jkko=jkk
         jkk=idat(nidt)
         if (jkk.ne.jkko) then
c          get level data
           nlevmx=0
           mltype=13
           ml=npfi(mltype,jkk)
           mllz=npar(ml)
c          step thru records of this type
           do while ((ml.ne.0).and.(npar(ml).eq.mllz)) 
             call dread(ltyp,lrtyp,lcon,
     $        nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $        idat1,rdat1,kdat1,nptrs,0,lun11)
             nlev=idat(nidt-1)
             nlevmx=max(nlevmx,nlev)
             if ((nlev.gt.0).and.(nlev.le.nd)) then
               nlpt(nlev)=ml
               iltp(nlev)=ltyp
               do  lk=1,nrdt
                 rlev(lk,nlev)=rdat(lk)
                 enddo
               do lk=1,nidt
                 ilv(lk,nlev)=idat(lk)
                 enddo
               do lk=1,nkdt
                 klev(lk,nlev)=kdat(lk)
                 enddo
               do lk=nkdt+1,20
                 klev(lk,nlev)=kblnk
                 enddo
               endif
             ml=npnxt(ml)
             enddo
           endif
         ml=nilin
         call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
         do ktt=1,nkdt
              write (kinam1(ktt:ktt),'(a1)')kdat(ktt)
              enddo
         do ktt=nkdt+1,9
              write (kinam1(ktt:ktt),'(a1)')kblnk
              enddo
         elmtp=elum(1,j)
         elmtpb=elum(2,j)
         write (lun11,9929)j,elin,kinam1,
     $    (klev(mm,ilevlo),mm=1,20),(klev(mm,ilevup),mm=1,20),
     $    rlev(1,ilevlo),rlev(1,ilevup),rlev(2,ilevlo),rlev(2,ilevup),
     $    rlev(3,ilevlo),rlev(3,ilevup),
     $    ilv(1,ilevlo),ilv(1,ilevup),ilv(2,ilevlo),ilv(2,ilevup),
     $    ilv(3,ilevlo),ilv(3,ilevup)
 9929    format (1h ,i9,1pe13.5,1x,a9,1x,2(20a1,1x),6(1pe13.5),
     $          6i6)
         enddo
      write (lun11,993)
c
c
      return
c
 20   continue
c
      return
c
 21   continue
c
      write (lun11,*)' level opacities and emissivities'

      jkk=0
      npi=0
c
        do j=1,nnml
c
         npc=npcon(j)
c
c        get continuum data
         mll=npc
         if (mll.le.0) go to 9343
         write (lun11,*)nptrs(1,mll),nptrs(2,mll),mll
         call dread(ltypc,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdati,npc-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
         ilevt=idat(nidt-1)
c
c        get ion data
         npio=npi
         npi=npar(npc)
         if (npio.ne.npi) jkk=jkk+1
         mll=npi
         write (lun11,*)nptrs(1,mll),nptrs(2,mll),mll
         call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdti,kdati,npi-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
         ethi=rdat(1)
c
c        get level data
         mltype=13
         mll=npfi(mltype,jkk)
         if (mll.le.0) go to 9343
           mllz=npar(mll)
 3943      continue
           write (lun11,*)nptrs(1,mll),nptrs(2,mll),mll
           call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdatl,mll-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
           mllev=idat(nidt-1)
           mllo=mll
           mll=npnxt(mll)
           if ((mllev.ne.ilevt).and.(mll.ne.0).and.(npar(mll).eq.mllz)) 
     $            go to 3943
         if ((mll.ne.0).and.(mllev.eq.ilevt).and.(npar(mll).eq.mllz)) 
     $            then
           write (lun11,*)j,npc,ilev,npi,jkk,ethi,mll
           write (lun11,*)ilev,mllev
           ethc=ethi-rdat(1)

           do mm=1,nkdti
              kdtmp(mm)=kdati(mm)
              enddo
           do mm=nkdti+1,20
              kdtmp(mm)=kblnk
              enddo  
           do mm=1,nkdt
              kdtmp(9+mm)=kdatl(mm)
              enddo
           do mm=nkdt+1,20
              kdtmp(9+mm)=kblnk
              enddo  
c
           mlcu=j
           write (lun11,969)j,ethc,(kdtmp(mm),mm=1,9),
     $         (kdtmp(mm),mm=10,29),mllev,
     $         cemab(1,mlcu),cemab(2,mlcu),opakab(mlcu),
     $         tauc(1,mlcu),tauc(2,mlcu),ltypc,npc
c           if (mlcu.ge.1965) stop
 969       format (1x,i6,1x,1pe11.3,1x,9a1,1x,20a1,
     $           i6,5(1pe13.5),2i6)
c
           endif
c       
          enddo
c
 9343     continue
c
c
      return
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Write level populations
C
7     continue
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
C      initialize line counter
       mmlv=0
       ipmat=0
C      First look for element data (jk is element index)
        klel=11
        mlel=npfirst(klel)
        jk=0
        do while (mlel.ne.0)
          jk=jk+1
          mt2=mlel-1
          call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mt2,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
          mllel=idat(nidt)
          xeltp=rdat(1)
          xeltp=abel(mllel)
          nnz=idat(1)
          if (lpril.ne.0) 
     $      write (lun11,*)'element:',jk,mlel,mllel,nnz,
     $                  (kdat(mm),mm=1,nkdt),xeltp
C         ignore if the abundance is small
          if (xeltp.lt.1.e-10) then
              jkk=jkk+nnz
            else
c             now step thru ions (jkk is ion index)
              klion=12
              mlion=npfirst(klion)
              jkk=0
              kl=0
              do while ((mlion.ne.0).and.(kl.lt.nnz))
                jkk=jkk+1
C               retrieve ion name from kdati
                call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdti,kdati,mlion-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
C               if not accessing the same element, skip to the next element
                mlleltp=idat(nidt-1)
                if (mlleltp.eq.mllel) then
                  kl=kl+1
                  if (lpril.ne.0)
     $              write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $                          (kdati(mm),mm=1,nkdti)
                  do mm=nkdti+1,20
                    kdati(mm)=kblnk
                    enddo
                  call func2l(jkk,kl,
     $             lpril,lun11,lfpi,vturbi,
     $             t,trad,r,xee,xpx,xh1,xh0,cfrac,
     $             epi,bremsa,bremsint,xii,
     $             tau0,dpthc,tauc,
     $             idat1,rdat1,kdat1,nptrs,np2,
     $             npar,npnxt,npfi,npfirst,
     $             nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $             npconi2,ncsvn,rates,vsav,idrates,
     $             rniss,rlev,ilv,
     $             nlpt,iltp,nlev,klev,
     $             ipmat,rrrtt)
                  do mm2=1,nlev
                    mmtmp=npilev(mm2,jkk)
                    kkkl=mmtmp
c                    write (lun11,*)mm2,jkk,mmtmp
                    eth=rlev(1,mm2)
                    if (xilev(kkkl).gt.1.e-34)
     $               write (lun11,9296)kkkl,(kdati(mm),mm=1,20),
     $                (klev(lk,mm2),lk=1,20),eth,xilev(kkkl)
 9296                 format (1x,i6,1x,(40a1),7(1pe13.5))
                    enddo
                  endif
C               Go to next ion
                mlion=npnxt(mlion)
                enddo
            endif
          mlel=npnxt(mlel)
C         Go to next element
          enddo
      write (lun11,993)
c
      return
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Print a short summary line of the radial calculation
C
9     continue
c
      elsum=0.
      do jlk=1,nlsvn
         ln=jlk
         ml=nplin(ln)
         call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
         elin=rdat(1)
         nilin=npar(ml)
         ergsev=1.602197e-12
         ener=ergsev*(12398.54)/amax1(elin,1.e-34)
         etst=ener/ergsev
c         if ((etst.gt.epi(1)).and.(etst.lt.epi(ncn-1))) then
           elsum=elsum+elum(1,ln)+elum(2,ln)
c           endif
         enddo
      sumtmp1=0.
      sumtmp2=0. 
      ergsev=1.602197e-12
      r19=r*1.e-19
      fpr2=12.56*r19*r19
      tmp1=zremsz(1)*(1.-expo(-dpthc(1,1)))
      tmp2=zrems(1,1)
      do jk=2,ncn
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
      alguu1=alog10(amax1(1.e-34,uu1))
      skse=xlum/(xpx*r19*r19)
      zeta=alog10(max(1.e-34,skse))
      ecc=2.998e+10
      ekt=t*(0.861707)*ergsev
      sksec=skse/12.56/((1.+xee)*ekt*ecc)
      zetac=alog10(max(1.e-34,sksec))
      enn0=xpx
      nlyc=nbinc(13.7,epi)
      nry=nlyc+1
      egam=zremsz(nry)/(2.*12.56*enn0*ecc*r19*r19+1.e-34)
      nry=nbinc(13.6,epi)+1
c      write (lun11,9969)r,rdel,zeta,xee,xpx,t,hmctot,
c     $ dpthc(1,nry),dpthc(2,nry),ntotit,lnerrd
9969  format (1x,9(1pe10.3),2i3)
      tmp1=alog10(r)
      tmp2=alog10(max(1.e-36,min(99.,rdel/r)))
      tmp2c=alog10(max(xcol,1.e-10))
      tmp3=alog10(xpx)
      tmp4=alog10(t)+4.
      tmp5=alog10(max(dpthc(1,nry),1.e-10))
      tmp6=alog10(max(dpthc(2,nry),1.e-10))
      tmp7=min(99.99,max(-99.99,hmctot*100.))
      tmp8=min(99.99,max(-99.99,terr*100.))
      write (tmpst,9889)tmp1,tmp2,tmp2c,zeta,xee,tmp3,tmp4,tmp7,
     $ tmp8,tmp5,tmp6,ntotit
      write (lun11,9889)tmp1,tmp2,tmp2c,zeta,xee,tmp3,tmp4,tmp7,
     $ tmp8,tmp5,tmp6,ntotit
 9889  format (1x,11(1x,f6.2),i3)
      call xwrite(tmpst,10)
c
c
      return
c
 16   continue
c
      write (lun11,993)
      write (lun11,*)'times:',tread,tloop,tfunc,trates1,thcor,trates2,
     $          theat
      ttot=0.
      do ll=1,ntyp
        ttmpi=tucalc(ll)/max(1,ncall(ll))
        ttot=ttot+tucalc(ll)
        write (lun11,9892)ll,ncall(ll),tucalc(ll),ttmpi
 9892   format (1x,2i8,2(1pe11.3))
        enddo
      write (lun11,*)'total ucalc=',ttot
      write (lun11,993)
c
      return
c
 17   continue
c
c
      
      write (lun11,993)
      write (tmpst,993)
      call xwrite(tmpst,10)
      klabs(1)='log(r)'
      klabs(2)='delr/r'
      klabs(3)='log(N)'
      klabs(4)='log(xi)'
      klabs(5)='x_e   '
      klabs(6)='log(n)'
      klabs(7)='log(t)'
      klabs(8)='h-c(%)'
      klabs(9)='h-c(%)'
      klabs(10)='log(tau)'
c      klabs(10)='ntotit'
      write (lun11,9979)(klabs(mm),mm=1,10)
      write (tmpst,9979)(klabs(mm),mm=1,10)
9979  format (2x,9(1x,a6),(1x,a9))
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
      write (lun11,9979)(klabs(mm),mm=1,10)
      write (tmpst,9979)(klabs(mm),mm=1,10)
      call xwrite(tmpst,10)
 9989 format (2x,11a6)
c
c
      return
c
 22   continue
c
c      write (lun11,993)
      ergsev=1.602197e-12
      r19=r*(1.e-19)
c      write (lun11,*)enlum,xpx,r,xlum
      uu1=enlum/(12.56*xpx*r19*r19)/3.e+10
c      write (lun11,*)uu1
      enlumx=0.
      nb1=nbinc(100.,epi)
      nb10=nbinc(10000.,epi)
c      write (lun11,*)'nb1=',nb1,nb10
      do kl=nb1,nb10
c        write (lun11,*)kl,epi(kl),zremsz(kl),enlumx
        enlumx=enlumx+(zremsz(kl)/epi(kl)+zremsz(kl-1)/epi(kl-1))
     $                *(epi(kl)-epi(kl-1))/2.
        enddo
      uux=enlumx/(12.56*xpx*r19*r19)/3.e+10
      alguux=alog10(amax1(1.e-34,uux))
      alguu1=alog10(amax1(1.e-34,uu1))
      skse=xlum/(xpx*r19*r19)
      zeta=alog10(max(1.e-34,skse))
      ecc=2.998e+10
      ekt=t*(0.861707)*ergsev
c      sksec=skse/(12.56*((1.+xee)*ekt+pradl/(1.e-34+xpx))*ecc)
      sksec=skse/12.56/((1.+xee)*ekt*ecc)
      zetac=alog10(max(1.e-34,sksec))
      enn0=xpx
      nlyc=nbinc(13.7,epi)
      nry=nlyc+1
      egam=zremsz(nry)/(2.*12.56*enn0*ecc*r19*r19+1.e-34)
      nry=nbinc(13.6,epi)+1
9968  format (1x,' log(Xi)=',1pe11.3, ' log(u1)=',1pe11.3,
     $ ' log(ux)=',1pe11.3,' gamma=',1pe11.3, ' rdel=',1pe11.3)
 9965 format (1x,' r=',1pe11.3,' t=',1pe11.3,' log(xi)=',1pe11.3,
     $ ' n_e=',1pe11.3,' n_p=',1pe11.3)
9966  format (1x,'httot=',1pe11.3,' cltot=',1pe11.3,
     $      'taulc=',1pe11.3,'taulcb=',1pe11.3)
      write(lun11,9965)r,t,zeta,xnx,xpx
      write(lun11,9966)httot,cltot,dpthc(1,nry),dpthc(2,nry)
      write(lun11,9968)zetac,alguu1,alguux,egam,rdel
c
      return
c
 25   continue
c
 26   continue
c
c
      return
      end
      subroutine printerror(lun11,status)

c     print out the fitsio error messages to the user

      integer status
      character errtext*30,errmessage*80


c     check if status is ok (no error); if so, simply return
      if (status .le. 0)return

c     get the text string which describes the error
 1    call ftgerr(status,errtext)
      write (lun11,*)'fitsio error status =',status,': ',errtext

c     read and print out all the error messages on the fitsio stack
 2    call ftgmsg(errmessage)
      do while (errmessage .ne. ' ')
          write (lun11,*)errmessage
          call ftgmsg(errmessage)
      end do
      end

c
***********************************************************************
c
      function ratri ( t,iz,m,rion,tmin,tmax,epot)         
c
c*******************************************************************************
c
c     use recomended rates by pindzola etal
c     ri(i) is the ionization rate from ions with i electrons,
c     involving both direct ionization and excition autoionization.
c
c     routine CHEB and data from Pindzola et al.
c
      
c     real t,twox,prev2,prev
      dimension rion(m)
      rio=epot
      cb=8.6164e-5                     
       sum1=0.
       tev=t*8.6173468e-05
       if (tev.lt.tmin) tev=tmin   
       if (tev.gt.tmax) tev=tmax
        eminl=log(tmin)
        emaxl=log(tmax)
        k=m
        cheb=rion(k)
        xx=log(tev)
        k=k-1
        if (k.eq.0) stop 'error in ratri'
        xnorm=(xx-eminl-(emaxl-xx))/(emaxl-eminl)
        twox=2.*xnorm
        prev2=0.
250     prev=cheb
        if(k.eq.1) goto 310
        cheb=rion(k)+twox*prev-prev2
        prev2=prev
        k=k-1
        goto 250
310     cheb=.5*rion(1)+xnorm*prev-prev2
        ri=exp(cheb)

       tev=t*8.6173468e-05
       if (tev.lt.tmin.or. tev.gt.tmax) then
        rc=float(iz)
        beta=.25*(sqrt((100.*rc+91.)/(4.*rc+3.))-5.)
         wte=(log(1.+t*cb/rio))**(beta/(1.+t*cb/rio))
         if (tev.lt.tmin) then
          tm=rion(8)/8.6173468e-05
         else
          tm=rion(9)/8.6173468e-05
         endif
         wtm=(log(1.+tm*cb/rio))**(beta/(1.+tm*cb/rio))
         tx=rio/(t*cb)
         if (tx.gt.100.) tx=100.
         call eint(tx,ete,e2,e3)
         call eint(rio/(tm*cb),etm,e2,e3)
         ri=ri*sqrt(tm/t)*ete/etm*wte/wtm
         write(11,*)rio/(t*cb),rio/(tm*cb),ete/etm,wte/wtm
       endif
      ratri=ri
      return 
      end
       subroutine rdflo(flo,qry,ios,lpri,ind)
 
c       reads integer from file param corresponding to character string
c       'char'. if no integer there, prompts user for parameter ...
c
       character(1) qtmp
       character(72) qry
       character(1) chtst(16)
c
       data chtst/'0','1','2','3','4','5','6','7','8','9','+','-',
     $            ' ','.','e','e'/
c
       lun11=6
       if (lpri.gt.2) write (lun11,*) 'in rdflo',ind
       if (lpri.gt.2) write (lun11,*)qry
       ist=1
c       ind=index(qry,' ')
       if (lpri.gt.2) write (lun11,9901)ind
9901   format (1x,' ind=',i4)
c
c      scan for e and dot
9904   format (1h ,i4,2x,a12)
c       kk=ind
       kk=0
       lfnde=0
       lfndd=0
       lnon=0
       lfndm=0
       lfnd2=0
102    kk=kk+1
       if (kk.ge.72) return
       read (qry(kk:kk),'(a1)') qtmp
       if (qtmp.eq.'.') lfndd=kk
       if ((qtmp.eq.'-').and.(lfndd.eq.0)) lfndm=kk
       if ((qtmp.eq.'e').or.(qtmp.eq.'e')) lfnde=kk
       lfnd=0
       lfnd2o=lfnd2
       lfnd2=0       
       do 3011 ll=1,16
         if (qtmp.eq.chtst(ll)) lfnd=1
         if ((qtmp.eq.chtst(ll)).and.(ll.ne.13)) lfnd2=1         
 3011    continue
       if ((lfnd2.eq.1).and.(lfnd2o.eq.0)) 
     $      lfndb=kk
       if (lfnd.eq.0) lnon=1
       if (kk.lt.ind) go to 102
       if (lpri.gt.2) write (lun11,9905)lfnde,lfndd
9905   format (1x,' e and d switches',2i4)
       if (lfndd.eq.0) go to 10
       if (lnon.ne.0) go to 1011
       lneg=1
       if (lfndm.ne.0) lneg=-1
       read (qry(lfndb:lfndb),'(a1)') qtmp
       if ((qtmp.eq.'+').or.(qtmp.eq.'-'))lfndb=lfndb+1
c
c
c      scan for mantissa
       ind2=ind
       kk=lfndb
       idec=lfndd
       if (lpri.gt.2) write (lun11,*)'scanning mantissa'
       if (lfnde.ne.0) ind2=lfnde-1
       if (lpri.gt.2) write (lun11,*)'ind,ind2,lfnde,lfndb:',
     $    ind,ind2,lfnde,lfndb
       sum=0.
       iexp=1-(kk-idec+1)
       kk=kk-1
104       kk=kk+1
          if (kk.eq.idec) go to 301
          iexp=iexp-1
          read (qry(kk:kk),'(i1)') itmp
          sum=sum+float(itmp)*10.**iexp
301       continue
          if (lpri.gt.2) write (lun11,9907)kk,itmp,iexp,sum
9907      format (1x,' kk,itmp,iexp,sum ',3i8,e12.4)
          if (kk.lt.ind2) go to 104
       flo=sum*float(lneg)
       ios=0
       if (lfnde.eq.0) return
c
c      scan for exponent
       if (lpri.gt.2) write (lun11,*)'scanning exponent'
       kk=lfnde+1
       lneg=1
       sum2=0.
       read (qry(kk:kk),'(a1)') qtmp
       if (qtmp.ne.'-') go to 1104
          kk=kk+1
          lneg=-1
 1104     continue
       if (qtmp.eq.'+') kk=kk+1
       iexp=1-(kk-ind)
       kk=kk-1
 105      kk=kk+1
          iexp=iexp-1
          read (qry(kk:kk),'(i1)') itmp
          sum2=sum2+float(itmp)*10.**iexp
          if (lpri.gt.2) write (lun11,9907)kk,itmp,iexp,sum2
          if (kk.lt.ind) go to 105
       flo2=sum2*float(lneg)
       flo=flo*10.**flo2
       if (lpri.gt.2) write (lun11,*)'returning:',flo2,flo
       ios=0

       ios=0
       return
c
 10    continue
       ios=-1
       return
c
 1011  continue
       ios=999
c
       return
       end
       subroutine rdint(lo,qry,ios,lpri,ind)
 
c       reads integer from file param corresponding to character string
c       'char'. if no integer there, prompts user for parameter ...
c
       character(1) qtmp,qtmpo
       character(72) qry
       character(1) chtst(15)
c
       data chtst/'0','1','2','3','4','5','6','7','8','9','+','-',
     $           ' ','.','e'/
c
       lun11=6
       if (lpri.gt.2) write (lun11,*) 'in rdint',ind
       if (lpri.gt.2) write (lun11,*)qry
       ist=1
c       ind=index(qry,' ')
       if (lpri.gt.2) write (lun11,9901)ind
9901   format (1x,' ind=',i4)
c
c      scan for e and dot
9904   format (1h ,i4,2x,a12)
       kk=0
       lfndm=0
       lnon=0
       lfnd2=0
102    kk=kk+1
       if (kk.ge.72) return
       qtmpo=qtmp
       read (qry(kk:kk),'(a1)') qtmp
       if (qtmp.eq.'-') lfndm=kk
       lfnd=0
       lfnd2o=lfnd2
       lfnd2=0       
       do 3011 ll=1,13
         if (qtmp.eq.chtst(ll)) lfnd=1
         if ((qtmp.eq.chtst(ll)).and.(ll.le.10)) lfnd2=1         
 3011    continue

       lfndb=0
       if ((lfnd2.eq.1).and.(lfnd2o.eq.0)) 
     $      lfndb=kk
       if (lfnd.eq.0) lnon=1
       if (kk.lt.ind) go to 102
       if (lnon.ne.0) go to 1011
       lneg=1
       if (lfndm.ne.0) lneg=-1
c
c
c
c      scan for mantissa
       kk=lfndb
       ind2=ind
       idec=ind+1
       if (lpri.gt.2) write (lun11,*)'scanning mantissa'
       isum=0
       iexp=-1-(kk-idec-1)
       kk=kk-1
104       kk=kk+1
          iexp=iexp-1
          read (qry(kk:kk),'(i1)') itmp
          isum=isum+itmp*10**iexp
301       continue
          if (lpri.gt.2) write (lun11,9907)kk,itmp,iexp,isum
9907      format (1x,' kk,itmp,iexp,sum ',4i8)
          if (kk.lt.ind2) go to 104
       lo=isum*lneg
       ios=0
       return
c
c 10    continue
c       ios=-1
c       return
c
 1011  continue
       ios=999
c
       return
       end
      subroutine readtbl(nptrs,np1r,np1i,np1k,np2,npdat2,
     &                   rdat1,idat1,kdat1,nidat1,filename,lun11)

c       written by Ke Zhang, Nov. 9, 2001

      integer idat1(nidat1)
      real rdat1(nidat1)
      character(1) kdat1(nidat1)
      integer nptrs(10,npdat2),ntptr(1000000)

      real nulle
      integer nullj
      character filename*256,nullstr*1
      character credate*63, comment*50
      logical anynull

      integer status,unit,readwrite,blocksize,hdutype
      integer row,col

      status=0
      nullstr=' '
      nullj=0
      nulle=0.

c get an unused logical unit number and open the fits file
      call ftgiou(unit,status)
      readwrite=0
      call ftopen(unit,filename,readwrite,blocksize,status)

c Read the primary header & get the file creation date
      call ftmahd(unit,1,hdutype,status)
      call ftgkys(unit,'DATE',credate,comment,status)
      if(status .gt.0) call printerror(lun11,status)
      write(lun11,*)'Atomic Data Version: ',credate(1:lenact(credate))

      col=1
      row=1

c move to the next extension : POINTERS
      call ftmrhd(unit,1,hdutype,status)
          
c read LENGTH keywords: total records #
      call ftgkyj(unit,'LENGTH',np2,comment,status)

c read POINTERS data
      call ftgcvj(unit,col,row,1,10*np2,nullj,
     &               ntptr,anynull,status)
      do i=1,np2
        do j=1,10
          nptrs(j,i)=ntptr(10*(i-1)+j)
        enddo
      enddo


c move to the next extension : REALS
      call ftmrhd(unit,1,hdutype,status)

c read LENGTH keywords: total reals #
      call ftgkyj(unit,'LENGTH',np1r,comment,status)

c read REAL data
      call ftgcve(unit,col,row,1,np1r,nulle,
     &               rdat1,anynull,status)


c move to the next extension : INTEGERS
      call ftmrhd(unit,1,hdutype,status)

c read LENGTH keywords: total integers #
      call ftgkyj(unit,'LENGTH',np1i,comment,status)

c read INTEGER data
      call ftgcvj(unit,col,row,1,np1i,nullj,
     &               idat1,anynull,status)


c move to the next extension : CHARS
      call ftmrhd(unit,1,hdutype,status)

c read LENGTH keywords: total chars #
      call ftgkyj(unit,'LENGTH',np1k,comment,status)

c read CHAR data
      call ftgcvb(unit,col,row,1,np1k,nullstr,
     &             kdat1,anynull,status)


c close the file and free the unit number
      call ftclos(unit, status)
      call ftfiou(unit, status)

c check for any error, and if so print out error messages
      if (status .gt. 0) call printerror(lun11,status)

      return
      end
      subroutine remtms(ct)
c
      real ct
      real a(2)
c
c
c
       ct=etime(a)
c       ct=0.
c      write (lun11,*)'in remtms:',ct,a
c
      return
      end
      subroutine rread1(tp,xlum,lwri,lpri,r,t,xpx,p,lcdd,numrec,npass,
     $ nnmax,nlimd,rmax,xpxcol,xi,zeta,lfix,zremsz,epi,
     $ lun11,abel,cfrac,emult,taumax,xeemin,stringst,stringst2,lerg,
     $ kmodelname,nloopctl,critf,vturbi)
c
c
c     this routine handles reading of the input data.
c
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
c                                                                       
      dimension zremsz(ncn)
      dimension epi(ncn)
      dimension eptmp(99999),zrtmp(99999),abel(13)
c                                                                       
      character(133) tmpst
      character(8) stringst
      character(80) stringsl,kblnk80,stringst2
      character(30) kmodelname
      integer nloopctl
c
      data kblnk80/
     $'                                                                 
     $                '/
c
c
            ierr=0
            call uclgsr('cfrac',cfrac,ierr)
c
c           temperature
            call uclgsr('temperature',t,ierr)
c
c           pressure/density switch
            call uclgsi('lcpres',lcdd2,ierr)
            lcdd=1-lcdd2
c
c           pressure
            call uclgsr('pressure',p,ierr)
c
c           density
            call uclgsr('density',xpx,ierr)
c
c           spectrum
 
c            lpri=3
            do mm=1,ncn
              zremsz(mm)=0.
              enddo
            stringsl=kblnk80                                                  
            stringst=kblnk80                                                  
            stringst2=kblnk80           
            lerg=0                                       
            call uclgst('spectrum',stringsl,ierr)
            xlum=1.
            read (stringsl(1:8),'(a8)')stringst
            if ((stringst.eq.'file    ').or.(stringst.eq.'file')) then
              stringsl=kblnk80                                                  
              call uclgst('spectrum_file',stringsl,ierr)
              read (stringsl(1:80),'(a80)')stringst2
              call getlun(lun13)
              open (unit=lun13,file=stringst2,status='unknown')
              call uclgsi('spectun',lerg,ierr)
               read (lun13,*)nenergy
               numcon2 = nenergy
               do ll=1,nenergy
                  read (lun13,*)eptmp(ll),zrtmp(ll)
                  if (lerg.eq.1) zrtmp(ll)=zrtmp(ll)*eptmp(ll)
                  enddo
               call ispecg(eptmp,zrtmp,numcon2,epi,zremsz,xlum,
     $                  lpri,lun11)
               if ( lpri.gt.2 ) write (lun11,*) 'atable spectrum:'
               if ( lpri.gt.2 ) write (lun11,*) 'energy grid:' , 
     &                                 (eptmp(ii),ii=1,numcon2)
               if ( lpri.gt.2 ) write (lun11,*) 'spectrum:' , 
     &                                 (zrtmp(ii),ii=1,numcon2)
            elseif ((stringst.eq.'bbody').or.(stringst.eq.'bbody   ')) 
     $           then
               call uclgsr('trad',tp,ierr)
               call starf(tp,xlum,epi,zremsz,lpri,lun11)
            elseif ((stringst.eq.'pow').or.(stringst.eq.'pow     ')) 
     $           then
c               write (lun11,*)'calling ispec4'
               call uclgsr('trad',tp,ierr)
               call ispec4(tp,xlum,epi,zremsz,lpri,lun11)
            elseif ((stringst.eq.'brems').or.(stringst.eq.'brems   ')) 
     $           then
               call uclgsr('trad',tp,ierr)
               call ispec(tp,xlum,epi,zremsz,lpri,lun11)
            else 
              write (tmpst,*)'error: invalid input spectrum specified.'
              call xwrite(tmpst,10)
              stop
            endif

c
c            luminosity
            call uclgsr('rlrad38',xlum,ierr)
            lprisv=lpri
            lpri=1
            call ispecgg(xlum,epi,zremsz,
     $               lpri,lun11)
            lpri=lprisv
c
c           column density
            call uclgsr('column',xpcol,ierr)
c
c           ionization parameter
            call uclgsr('rlogxi',zeta,ierr)
c
c           number of steps
            call uclgsi('nsteps',numrec,ierr)
c
c           number of iterations
            call uclgsi('niter',nlimd,ierr)
c
c           write switch
            call uclgsi('lwrite',lwri,ierr)
c
c           print switch
            call uclgsi('lprint',lpri,ierr)
c
c           step size choice
            call uclgsi('lstep',lfix,ierr)
c
c           abundances
            call uclgsr('habund',abel(1),ierr)
            call uclgsr('heabund',abel(2),ierr)
            call uclgsr('cabund',abel(3),ierr)
            call uclgsr('nabund',abel(4),ierr)
            call uclgsr('oabund',abel(5),ierr)
            call uclgsr('neabund',abel(6),ierr)
            call uclgsr('mgabund',abel(7),ierr)
            call uclgsr('siabund',abel(8),ierr)
            call uclgsr('sabund',abel(9),ierr)
            call uclgsr('arabund',abel(10),ierr)
            call uclgsr('caabund',abel(11),ierr)
            call uclgsr('feabund',abel(12),ierr)
            call uclgsr('niabund',abel(13),ierr)
c
c
            call uclgsi('npass',npass,ierr)
C           Test if npass is even.  If it is, change it to next lowest odd
            if(mod(npass,2) .eq. 0) then
              write(lun11,*)'rread1: npass should always be odd.'
              write(lun11,*)'rread1: resetting to ',npass-1
              npass=npass-1
            endif

            stringsl=kblnk80                                                  
            call uclgst('modelname',stringsl,ierr)
            read (stringsl(1:30),'(a30)')kmodelname
c
c
c           step parameters
            call uclgsr('emult',emult,ierr)
            call uclgsr('taumax',taumax,ierr)
c
c           min xee
            call uclgsr('xeemin',xeemin,ierr)
c
c           critf
            call uclgsr('critf',critf,ierr)
c
c           vturbi
            call uclgsr('vturbi',vturbi,ierr)
c
            call uclgsi('loopcontrol',nloopctl,ierr)
c
c
            ccc = 3.e+10
            xlum2=xlum
            xpxcol=xpcol
            xi=10.**zeta
            if (lcdd.ne.1) then
               xpx = p/1.38e-12/amax1(t,1.e-34)
               r19 = sqrt(xlum2/12.56/ccc/amax1(1.e-34,p*xi))
            else
               r19 = sqrt(xlum2/amax1(1.e-34,xpx*xi))
            endif               
            rmax = xpxcol/(amax1(xpx,1.e-36))
            r = r19*(1.e+19)
c
c
      return
      end
      subroutine savd(ldir,
     $       lpri,lun,lun11,tinf,
     $       t,r,rdel,xcol,xee,xpx,
     $       tau0,dpthc,tauc)
c
c     this routine  saves only depths for iterative calculation
c
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
c
c     line optical depths
      dimension tau0(2,nnnl)
c     continuum optical depths
      dimension dpthc(2,ncn)
      dimension tauc(2,nnml)
c
c
      if (lpri.ne.0) write (lun11,*)'in savd',rdel,t,tauc(1,25)
      write (lun)t
      write (lun)r
      write (lun)rdel
      write (lun)xcol
      write (lun)xee
      write (lun)xpx
      write (lun)tau0
      write (lun)dpthc
      write (lun)tauc
c
      return
      end
      subroutine setptrs(lun11,lpri,
     $ idat1,rdat1,kdat1,nptrs,np2,
     $ npnxt,npfi,npar,npfirst,nplin,
     $ nplini,npcon,npconi,npilev,npilevi,
     $ npconi2,nlevs,nlsvn,ncsvn,abel)
c
c     this program set the pointers of the database
c       Written by Ke Zhang, Oct.8 2001

c
c     data structures are:
c      data: the database arrays (integer, real, character)
c       idat1(nidat1)
c       rdat1(nrdat1),
c       kdat1(nkdat1)
c     descriptions of database entries, and pointers
c       nptrs(nptt,ndat2)
c         nptrs(2,nx)=data type
c         nptrs(3,nx)=rate type
c         nptrs(4,nx)=continuation flag 
c                       (n=number of continuations to come)
c         nptrs(5,nx)=number of reals
c         nptrs(6,nx)=number of integers
c         nptrs(7,nx)=number of characters
c         nptrs(8,nx)=pointer to reals
c         nptrs(9,nx)=pointer to integers
c         nptrs(10,nx)=pointer to characters
c
c       pointers:
c       next record:
c         npnxt(ndat2)
c       parent record (=ion header or element header)
c         npar(ndat2)
c       first record of a given rate type
c         npfirst(ntyp)
c       first record of rate type ntyp for ion nni
c         npfi(ntyp,nni)
c       pointer for line data from array containing luminosities
c         nplin(nnnl)
c       (inverse) pointer for line data to array containing luminosities
c          from database array
c         nplini(ndat2)
c       pointer for continuum data (pi xsection) from array containing luminosities
c         npcon(nnml)
c       pointer to abundance array to first level of element nni
c         npconi2(ndat2)
c       (inverse) pointer for continuum data (pi xsection) from array containing 
c           luminosities
c         npconi(ndat2)
c
c
      parameter (ntyp=90)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
      parameter (ndl=2400,nd=ndl+1)
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
      dimension npnxt(ndat2),npar(ndat2),
     $      npfirst(ntyp)
      dimension npnxt2(ndat2)
      dimension npfi(ntyp,nni)
      dimension nplin(nnnl),nplini(ndat2),npcon(nnml)
      dimension npilev(nd,nni),npilevi(nnml)
      dimension npconi2(ndat2)
      dimension npconi(ndat2)
      dimension nlevs(nni)
      dimension abel(13),melpt(13)
      DIMENSION rdat(20000) , idat(20000)
      character(1) kdat(20000)
c
      dimension mlold(ntyp)

c            pointer structure
c     type    desc         nr  ni  nk      daught  par
c     1       rr, a&p      2   1   0               14
c     2       hcx          4   1   0               14
c     3       ai           2   1   0               14
c     4       line dat 1   2   3   0        5      14
c     5       line dat 2   4   3   0                4
c     6       lev dat  1   4   3   0               14
c     7       dr a&p       5   1   0               14
c     8       dr a&r       0   0   0               14
c     9       hecx         4   1   0               14
c     10      lev dat 2    0   2  30                6
c     11      2 ph         2   2   0               14
c     12      pixc, bpl    5   2   0               14
c     13      el           2   2  30       14       0
c     14      ion          1   2   8       all     13
c     15      pixc bkh 1   5   1   0       20      14
c     16      pixc bkh     0   0   0               14
c     17      cx: cota     4   3   0               14
c     18      rr: cota     3   1   0               14
c     19      pixc hullac  0   0   0               14
c     20      pixc bkh 2   5   1   0       21      15
c     21      pixc bkh 3   4   4  11               20
c     22      dr stroey    5   1   0               14
c     23      pixc clark   5   2   0       24      14
c     24      pixc clark 2 4   4   0               23
c     25      ci r&s       0   0   0               14
c     26      ci cota      2   2   0               14
c


c the main data index
      indx=1

c     fist an experimental print
c      do itmp=1,np2
c        CALL DREAD(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
c     &          itmp-1,Idat1,Rdat1,Kdat1,Nptrs,0,Lun11)
c        write (lun11,*)'itmp=',itmp
c        call dprinto(ltyp,lrtyp2,lcon,
c     $  lrdat,rdat,lidat,idat,lkdat,kdat,lun11)
c        enddo
c      stop

c the ion index
      iion=1

c the level index
      ilev=1

c the continum index
      icon=1

c the line index
      iline=1

c initialize pointers

      do i=1,ntyp
        npfirst(i)=0
      enddo

      do i=1,nni
        do j=1,ntyp
          npfi(j,i)=0
        enddo
        nlevs(i)=0
      enddo

      do i=1,ndat2
        npnxt(i)=0
      enddo

      do iel2=1,nl
        iel=iel2
        if (abel(iel).lt.1.e-15) then
c
c        write (lun11,*)'iel=',iel2,iel,abel(iel)
c  pass by elements that has neglectable abundance
          indx=indx+1
          do while((nptrs(3,indx).ne.11).and.(indx.lt.np2))
            indx=indx+1
          enddo

        else

c  register element record
c  npfirst,npnxt,npar,mlold

          if (npfirst(11).eq.0) then 
            npfirst(11)=indx
          else 
            npnxt(mlold(11))=indx
          endif

c          write (lun11,*)'registering element:',iel,abel(iel),indx,
c     $                         mlold(11)
          mlold(11)=indx
          npar(indx)=0
          indx=indx+1

c  go through ions

          lrtp=nptrs(3,indx)
c          write (lun11,*)'lrtp=',lrtp,indx
          do while(lrtp.eq.12)

c
c          write(lun11,*) iel,iion

c  register ion record
c  npfirst,npnxt,npar,mlold

            if (npfirst(12).eq.0) then
              npfirst(12)=indx
            else
              npnxt(mlold(12))=indx     
            endif
c            write (lun11,*)'npfirst(12)=',npfirst(12),indx
            mlold(12)=indx
            npar(indx)=mlold(11)
            indx=indx+1

c  level records, rate type 13
c  npfirst,npnxt,npar,mlold,npfi,npilev,npilevi

            if (nptrs(3,indx).eq.13) then
              npfi(13,iion)=indx
              iilev=1
              if (npfirst(13).eq.0) then
                npfirst(13)=indx
              else
                npnxt(mlold(13))=indx
              endif
c              write (lun11,*)'filling npilev:'
              do while(nptrs(3,indx).eq.13)
                npar(indx)=mlold(12)
                npnxt(indx)=indx+1
                npilev(iilev,iion)=ilev
                npilevi(ilev)=iilev
c                write (lun11,*)ilev,iilev,indx,iion
                iilev=iilev+1
                ilev=ilev+1
                indx=indx+1
              enddo
              mlold(13)=indx-1
              npnxt(indx-1)=0
            endif


            do i=1,2
              if (i.eq.1) then
                lrtp=7
              else
                lrtp=1
              endif
              if (nptrs(3,indx).eq.lrtp) then
                npfi(lrtp,iion)=indx
                if (npfirst(lrtp).eq.0) then
                  npfirst(lrtp)=indx
                else
                  npnxt(mlold(lrtp))=indx
                endif
c                write (lun11,*)'npconi loop'
                do while(nptrs(3,indx).eq.lrtp)
c                 npcon points from the array of continuum emissivities 
c                    to the photoionization data
c                 npconi points from the levels to the arrays of 
c                    array of continuum emissivities
c                 npconi2 points from the photoionization data 
c                    to the array of continuum emissivities 
c                    (inverse if npcon)
c                 icon is the index of the continuum emissivity array 
c                    element
c                 indx is the index of the photoionization data
                  npar(indx)=mlold(12)
                  npnxt(indx)=indx+1
                  npcon(icon)=indx
c                  write (lun11,*)'index into continuum  array:',
c     $                icon
c                  write (lun11,*)'index of photoionization element:',
c     $                indx
c                 now search for the level that goes with this 
c                    photoionization data
                  mltmpn=npfi(13,iion)
                  mlfnd=0
                  nclev=idat1(nptrs(6,indx)+nptrs(9,indx)-2)
                  if (nclev.gt.nlevs(iion)) nlevs(iion)=nclev
                  mltst=nclev
c                  write (lun11,*)'searching for level:'
                  do while ((mlfnd.ne.mltst).and.(mltmpn.ne.0)
     $                  .and.(npar(mltmpn).eq.npar(indx)))
                     mltmp=mltmpn
                     mlfnd=idat1(nptrs(6,mltmp)+nptrs(9,mltmp)-2)
                     mltmpn=npnxt(mltmp)
c                     write (lun11,*)mltmp,mlfnd,mltmpn,npar(mltmpn),
c     $                               npar(indx),nclev
                     enddo
                  npconi2(indx)=icon
c                  npconi(icon)=npfi(13,iion)-1+nclev
                  if (mltmp.ne.0) then
                    npconi(mltmp)=icon
                    endif
c                  write (lun11,*)indx,npar(indx),icon,nclev,
c     $                nptrs(3,indx),lrtp,mltmp
                  indx=indx+1
                  icon=icon+1
                enddo
                mlold(lrtp)=indx-1
                npnxt(indx-1)=0
              endif
           enddo
c
c  lines data and lines pointers, rate type 4, 9 & 14
c  npfirst,npnxt,npar,mold,npfi,nplin,nplini

c            write (lun11,*)'nplin,nplini:'
            do i=1,3
              if (i.eq.1) then
                lrtp=4
              elseif (i.eq.2) then
                lrtp=9
              else
                lrtp=14
              endif
              if (nptrs(3,indx).eq.lrtp) then
                npfi(lrtp,iion)=indx
                if (npfirst(lrtp).eq.0) then
                  npfirst(lrtp)=indx
                else
                  npnxt(mlold(lrtp))=indx
                endif
                do while(nptrs(3,indx).eq.lrtp)
                  npar(indx)=mlold(12)
                  npnxt(indx)=indx+1
                  nplin(iline)=indx
                  nplini(indx)=iline
c                  write (lun11,*)indx,iline
                  indx=indx+1
                  iline=iline+1
                enddo
                mlold(lrtp)=indx-1
                npnxt(indx-1)=0
              endif
            enddo

c  pointers for rate types 6,8,3,5,40
c  npfirst,npnxt,npar,mold,npfi

            do i=1,5
              if (i.eq.1) then
                lrtp=6
              elseif (i.eq.2) then
                lrtp=8
              elseif (i.eq.3) then
                lrtp=3
              elseif (i.eq.4) then
                lrtp=5              
              else
                lrtp=40
              endif
              if (nptrs(3,indx).eq.lrtp) then
                npfi(lrtp,iion)=indx
                if (npfirst(lrtp).eq.0) then
                  npfirst(lrtp)=indx
                else
                  npnxt(mlold(lrtp))=indx
                endif
                do while(nptrs(3,indx).eq.lrtp)
                  npar(indx)=mlold(12)
                  npnxt(indx)=indx+1
                  indx=indx+1
                enddo
                mlold(lrtp)=indx-1
                npnxt(indx-1)=0
              endif
            enddo
            
c  pointers for other rate types
c  npfirst,npnxt,npar,mold,npfi

            lrtp=nptrs(3,indx)
            do while((lrtp.ne.12).and.(lrtp.ne.11).and.(lrtp.ne.0))
              npar(indx)=mlold(12)
              if (npfirst(lrtp).eq.0) then
                npfirst(lrtp)=indx
              else
                npnxt(mlold(lrtp))=indx
              endif
              mlold(lrtp)=indx
              if (npfi(lrtp,iion).eq.0) npfi(lrtp,iion)=indx
c              write (lun11,*)iion,lrtp,indx,npfi(lrtp,iion)
              indx=indx+1
              lrtp=nptrs(3,indx)
            enddo

c  ionization data and continum pointers, rate type 7 & 1
c  npfirst,npnxt,npar,mlold,npfi,npcon,npconi,npconi2,nlevs


            iion=iion+1
              
          enddo
        endif
      enddo

      nlsvn=iline-1
      ncsvn=icon-1
c
      go to 9000
c
c     sort the element abundances
      lsrt=0
      do mml=1,nl
        melpt(mml)=mml
      enddo
      niter=0
      do while (lsrt.eq.0)
        lsrt=1
        niter=niter+1
        do mml=1,nl-1
          if (abel(melpt(mml)).lt.abel(melpt(mml+1))) then
            melptmp=melpt(mml)
            melpt(mml)=melpt(mml+1)
            melpt(mml+1)=melptmp
            lsrt=0
          endif
        enddo
      enddo
c
c
c     now redo the element pointers
c     zero the new next pointers
      do mml=1,np2
        npnxt2(mml)=0
        enddo
      npfirst2=0
      mllo=0
c     step thru elements
      do mml=1,nl
        mlloo=mllo
        mll=npfirst(11)
        itst=0
        do while ((mll.ne.0).and.(itst.ne.melpt(mml)))
          CALL DREAD(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
     &          mll-1,Idat1,Rdat1,Kdat1,Nptrs,0,Lun11)
          itst=idat(lidat)
          mllo=mll
          mll=npnxt(mll)          
          enddo
        if (mllo.ne.0) then
          if (npfirst2.eq.0) then
              npfirst2=mllo
            else
              npnxt2(mlloo)=mllo
            endif
          endif
        enddo
      npnxt2(mlloo)=0
      npfirst(11)=npfirst2
      do mml=1,np2
        if ((npnxt2(mml).ne.0).or.(mml.eq.mlloo)) then
          npnxt(mml)=npnxt2(mml) 
          endif
        enddo
c

 9000 continue

c
c     now print stuff sorted
c      ntptmp=11
c        mll=npfirst(ntptmp)
c        write (lun11,*)'ntptmp=',ntptmp
c        do while (mll.ne.0)
c          CALL DREAD(ltyp,lrtyp2,lcon,lrdat,rdat,lidat,idat,lkdat,kdat,
c     &          mll-1,Idat1,Rdat1,Kdat1,Nptrs,0,Lun11)
c          write (lun11,*)'mll=',mll
c          call dprinto(ltyp,lrtyp2,lcon,
c     $       lrdat,rdat,lidat,idat,lkdat,kdat,lun11)
c          mll=npnxt(mll)
c          enddo
c
c
      return
      end
      subroutine spline(x,y,n,yp1,ypn,y2)
      parameter (nmax=9999)
      dimension x(n),y(n),y2(n),u(nmax)
      if (yp1.gt..99e30) then
        y2(1)=0.
        u(1)=0.
      else
        y2(1)=-0.5
        u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
      endif
      do 11 i=2,n-1
        sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
        p=sig*y2(i-1)+2.
        y2(i)=(sig-1.)/p
        u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))
     *      /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
11    continue
      if (ypn.gt..99e30) then
        qn=0.
        un=0.
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do 12 k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k)
12    continue
      return
      end
       function splinem(p1,p2,p3,p4,p5,x)
c
c 5-point spline interpolation of y(x), for x in the range (0,1)
c knot values p1=y(0), p2=y(1/4), p3=y(1/2), p4=y(3/4), p5=y(1)
c
       s=1./30.
       s2=32.*s*(19.*p1-43.*p2+30.*p3-7.*p4+p5)
       s3=160.*s*(-p1+7.*p2-12.*p3+7.*p4-p5)
       s4=32.*s*(p1-7.*p2+30.*p3-43.*p4+19.*p5)
       if (x.gt.0.25) goto 1
       x0=x-0.125
       t3=0.0
       t2=0.5*s2
       t1=4.*(p2-p1)
       t0=0.5*(p1+p2)-0.015625*t2
       goto 4
 1     if (x.gt.0.5) goto 2
       x0=x-0.375
       t3=20.*s*(s3-s2)
       t2=0.25*(s2+s3)
       t1=4.*(p3-p2)-0.015625*t3
       t0=0.5*(p2+p3)-0.015625*t2
       goto 4
 2     if (x.gt.0.75) goto 3
       x0=x-0.625
       t3=20.*s*(s4-s3)
       t2=0.25*(s3-s4)
       t1=4.*(p4-p3)-0.015625*t3
       t0=0.5*(p3+p4)-0.015625*t2
       goto 4
 3     x0=x-0.875
       t3=0.0
       t2=0.5*s4
       t1=4.*(p5-p4)
       t0=0.5*(p4+p5)-0.015625*t2
 4     splinem=t0+x0*(t1+x0*(t2+x0*t3))
       return
       end
      subroutine splint(xa,ya,y2a,n,x,y,klo,khi,ncall)
      dimension xa(n),ya(n),y2a(n)
      ncall=ncall+1
      khi=khi*2
      khi=min0(khi,n)
      if (ncall.gt.1) go to 101
      klo=1
      khi=n
101   continue
1     if (khi-klo.gt.1) then
        k=(khi+klo)/2
        if(xa(k).gt.x)then
          khi=k
        else
          klo=k
        endif
      goto 1
      endif
      h=xa(khi)-xa(klo)
c      write (lun11,9831)x,klo,khi,xa(klo),xa(khi),ya(klo),ya(khi),y2a(klo),
c     $            y2a(khi),h
c9831  format (1h , ' in splint ',e12.4,2i4,7e12.4)
      if (h.eq.0.) go to 9001
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h
      y=a*ya(klo)+b*ya(khi)+
     *      ((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
c      write (lun11,9832)a,b,y
c9832  format (1h ,15x,3e12.4)
      return
9001  continue
      print 9002
9002  format (1h , ' in splint. bad xa input ')
      stop
      end
      subroutine starf(tp,xlum,epi,zremsz,lpri,lun11)
c
c
c     this subroutine generates the initial spectrum.
c      optically thin bremsstrahlung spectrum
c     brems stores the flux to be used
c
c
      parameter (ncn=9999)
c
      dimension epi(ncn),zremsz(ncn)
c
c
c
      data ergsev/1.602197e-12/
c
      dimension zremsi(ncn)
c
      numcon=ncn
      del=1.
      q=7.49e+08*del*xlum/(1.e-37+tp)
      xkt=1.16e-03/(1.e-37+tp)
      sum=0.
      lprisv=lpri
c      lpri=2
      if (lpri.gt.1) write (lun11,*)'in starf',tp,xlum,q,xkt
      do 132 i=1,numcon
         tempp=epi(i)*xkt
         zremsi(i)=0.
c         zremsi(i)=(3.1415e+22)*epi(i)**3/expo(tempp)
         if (tempp.lt.1.e-3) then
             zremsi(i)=epi(i)**3/tempp
           else
             if (tempp.lt.50.) 
     $         zremsi(i)=epi(i)**3/(expo(tempp)-1.)
             if (tempp.gt.50.) zremsi(i)=xlum/epi(i)/ergsev/1.e+37
           endif
         zremsi(i)=(3.1415e+22)*zremsi(i)
         if (lpri.gt.1) write (lun11,*)i,epi(i),zremsi(i)
         if ((epi(i).lt.13.6).or.(epi(i).gt.1.36e+4)
     $        .or.(i.le.1)) go to 132
         sum=sum+(zremsi(i)+zremsi(i-1))*(epi(i)-epi(i-1))/2.
132      continue
c
      const=xlum/sum/ergsev
      sum2=0.
      do 134 i=1,numcon
         zremsz(i)=zremsz(i)+zremsi(i)*const
         if (i.gt.1)
     $    sum2=sum2+(zremsz(i)+zremsz(i-1))*(epi(i)-epi(i-1))/2.
         if (lpri.gt.1) 
     $        write (lun11,*)i,epi(i),zremsi(i),const,zremsz(i)
 134     continue
      sum2=sum2*ergsev
c      write (lun11,*)'normalization:',sum2
      lpri=lprisv
c
      return
      end
      subroutine step(ectt,emult,epi,opakc,rccemis,fline,
     $  zrems,lpri,delr,dpthc,r,
     $  xpxcol,xcol,xpx,taumax,numrec0,lun11)
c
c
c
c     this routine claculates step sizes
c
c
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
c
      dimension opakc(ncn),epi(ncn),dpthc(2,ncn)
      dimension rccemis(2,ncn)
      dimension zrems(3,ncn)
      dimension fline(ncn)
c
c
      lprisv=lpri
c      lpri=1
      if (lpri.ge.1) write (lun11,*)'in step',taumax
c
c
      rmax=xpxcol/xpx
c      delr=rmax/float(max(1,numrec0))
      delr=min(rmax,r/numrec0)
      r19=r*(1.e-19)
      fpr2=12.56*r19*r19
      klmn = 1
      do  kl = 1,ncn
         optp2 = amax1(opakc(kl),1.e-24)
         dell=max(optp2*zrems(1,kl),
     $    (rccemis(1,kl)+rccemis(2,kl)+fline(kl))*fpr2)
         tst = emult*zrems(1,kl)/(abs(dell)+1.e-24)
         tst = emult/optp2
         if ((epi(kl).gt.ectt).and.(dpthc(1,kl).le.taumax)
     $     .and.(zrems(1,kl).gt.1.e-12)) then
            if ( tst.lt.delr ) klmn = kl
            delr = min(delr,tst)
            endif
         if (lpri.ne.0) write (lun11,*)kl,epi(kl),opakc(kl),zrems(1,kl),
     $          rccemis(1,kl),rccemis(2,kl),fline(kl),dell,tst,
     $          dpthc(1,kl),delr
         enddo
      delrmn=(xpxcol-xcol)/xpx
      delr=min(delr,delrmn)
      if ( lpri.ge.1 ) write (lun11,*)'in step',emult,
     &   delr,epi(klmn),ectt,rmax,xpxcol,xpx
      lpri=lprisv
c
c
      return
      end
      subroutine stpcut(ldirt,lpri,lun11,vturbi,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $      epi,opakc,oplin,opakab,delr,t,
     $      dpthc,tau0,tauc)
c
c
c     this routine updates.  calculates depths, etc.
c
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
      parameter (ntyp=90)
      parameter (ndl=2400,nd=ndl+1)
c
c     master data
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
      dimension epi(ncn),opakc(ncn),dpthc(2,ncn)
c     line opacities
      dimension oplin(nnnl)
c     line optical depths
      dimension tau0(2,nnnl)
      dimension opakab(nnml)
      dimension tauc(2,nnml)
      dimension optmp(ncn),optp2(ncn)
c     temporaries for dread
      dimension rdat(20000),idat(20000)
c
      dimension ldon(2)
c
      character(1) kdat(20000)
c
      if (lpri.ne.0)
     $ write (lun11,*)'in stpcut:',delr,nlsvn
c
      do llk=1,ncn
        optp2(llk)=0.
        optmp(llk)=0.
        enddo
      bbb=vturbi
      dpcrit=1.e-10
      eliml=1.
      elimh=1.0e6
      do  lnn=1,nlsvn
c        lnn=258
        ln=lnn
        ml=nplin(ln)
        call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
c       get damping parameter for fe 2-3 uta
        delea=0.
        if (ltyp.eq.82) delea=rdat(6)*(4.14e-15)
c       get damping parameter for type 50 data
        if (ltyp.eq.50) delea=rdat(3)*(4.14e-15)
        elin=rdat(1)
        lup=idat(2)
        nilin=npar(ml)
        ml=nilin
        call dread(ltyp,lrtyp,lcon,
     $       nrdt,rdat,nidt,idat,nkdt,kdat,nilin-1,
     $       idat1,rdat1,kdat1,nptrs,0,lun11)
        nilin=idat(3)
c       get nuclear mass       
        nelin=npar(ml)
        ml=nelin
        call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
        aatmp=rdat(2)
        if (lpri.ne.0)
     $   write (lun11,*)ln,elin,aatmp,nilin,nelin,lup
        if ((ln.gt.0).and.(ln.le.nnnl) 
     $    .and.(elin.gt.eliml).and.(elin.lt.elimh) 
     $    .and.(aatmp.gt.0.1) 
     $    .and.(nilin.gt.0).and.(nilin.le.nni))
     $    then

c         line parameters
          etmp=12398.54/elin
          nbtmp=nbinc(etmp,epi)
          nbtmpp=min(nbtmp+1,ncn)
          nbtmpm=max(nbtmp-1,1)
          if (lpri.ne.0)
     $     write (lun11,*)ln,elin,elmmtpp,nilin,egam,lup

c         find associated type 86 data
          iion=0
          idat(nidt)=0
          if (lpri.ne.0)
     $     write (lun11,*)'searching for ion'
          do while ((idat(nidt).ne.nilin).and.(iion.lt.nni))
            iion=iion+1
            nitmp=npfi(13,iion)
            call dread(ltyp,lrtyp,lcon,
     $            nrdt,rdat,nidt,idat,nkdt,kdat,nitmp-1,
     $           idat1,rdat1,kdat1,nptrs,0,lun11)
            if (lpri.ne.0)
     $        write (lun11,*)iion,idat(nidt),nilin,nitmp
            enddo
c         get damping parameter for iron auger damped lines
          ndtmp=npfi(41,iion)
          if (ndtmp.ne.0) then
            if (lpri.ne.0)
     $       write (lun11,*)'  found ion',lup,ndtmp
            if (ndtmp.gt.0) then
              mllz=npar(ndtmp)
              call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ndtmp-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
              iltmp=idat(2)
              do while ((ndtmp.ne.0).and.(lup.ne.iltmp)
     $          .and.(npar(ndtmp).eq.mllz)) 
                call dread(ltyp,lrtyp,lcon,
     $            nrdt,rdat,nidt,idat,nkdt,kdat,ndtmp-1,
     $            idat1,rdat1,kdat1,nptrs,0,lun11)
                iltmp=idat(2)
                if (lpri.ne.0)
     $           write (lun11,*)'   ',nidt,iltmp,ndtmp
                ndtmp=npnxt(ndtmp)     
                enddo
              endif
            if (lup.eq.iltmp) delea=rdat(3)*(4.14e-15)
            endif
c
c         cheat for narrow line plot
c         delea=0.        
c
c         thermal width quantities
          vth=(1.2e+1)*sqrt(t/aatmp)
          vturb=max(bbb,vth)
          e0=(12398.54)/amax1(elin,1.e-34)
          deleturb=e0*(vturb/3.e+5)
          deleth=e0*(vth/3.e+5)
          optpp=oplin(ln)
c     $      *deleth/max(deleth,deleturb)
c     $      *(vth/3.e+5)
          dele=deleth+deleturb
          aasmall=delea/(1.e-24+dele)/12.56
          ml1=nbinc(e0,epi)
          if (lpri.ne.0)
     $     write (lun11,*)'optpp,dpcrit*opakc(ml1),ml1,deleth,delea:',
     $                    optpp,dpcrit*opakc(ml1),ml1,deleth,delea
          delet=(epi(ml1)-etmp)/dele
          if ((optpp.gt.dpcrit*opakc(ml1)).and.(abs(delet).lt.100.)) 
     $      then
c           apply binned opacity
            if (aasmall.gt.1.e-6) then
                 profile=voigte(delet,aasmall)
              else
                 profile=expo(-delet*delet)/1.772
              endif 
            tst=optpp*profile
            optmp(ml1)=optmp(ml1)+tst
            if (lpri.ne.0) write (lun11,*)ml1,epi(ml1),epi(ml1+1),
     $                              dilfac,optmp(ml1),tst
            mlc=0
            ldir=1
            ldon(1)=0
            ldon(2)=0
            mlmmax=0
            mlmmin=ncn
            do while ((ldon(1)*ldon(2).eq.0).and.(mlc.lt.100)) 
              mlc=mlc+1
              do ij=1,2
                ldir=-ldir
                if (ldon(ij).ne.1) then
                  mlm=ml1+ldir*mlc
                  if ((mlm.le.ncn).and.(mlm.ge.1)) then
                    delet=(epi(mlm)-etmp)/dele
                    if (aasmall.gt.1.e-6) then
                        profile=voigte(abs(delet),aasmall)
                      else
                        profile=expo(-delet*delet)/1.772
                      endif 
                    tst=optpp*profile
                    optmp(mlm)=optmp(mlm)+tst
                    optp2(mlm)=tst
                    mlmmin=min(mlm,mlmmin)
                    mlmmax=max(mlm,mlmmax)
                    if (lpri.ne.0) write (lun11,*) mlm,epi(mlm),ij,
     $               delet,sqrt(aasmall),profile,tst,optmp(mlm)
                    endif
                  if ((tst.lt.dpcrit*opakc(mlm))
     $                 .or.(mlm.le.1).or.(mlm.ge.ncn)
     $                 .or.(mlc.gt.100).or.(abs(delet).gt.200.)) 
     $                  ldon(ij)=1
                  endif
                enddo                 
              enddo
            mlmmax=max(mlmmax-1,mlmmin)
            sum=0.
            do mltml=mlmmin,mlmmax
              sum=sum+(optp2(mltml)+optp2(mltml+1))
     $                *(epi(mltml+1)-epi(mltml))/2.
              enddo
            if (lpri.ne.0) write (lun11,*)'lsum=',sum
            endif      
          endif
        enddo
c
c     calculate continuum depths
      lind=1
      if (ldirt.gt.0) lind=2
      dpthmx=0.
      indmx=0
      if (lpri.ne.0) write (lun11,*)'dpthc:',delr
      do  i = 1,ncn
c         opakc(i)=opakc(i)+optmp(i)
         optpp=min(optmp(i),1.e+3/(1.e-24+delr))
         dpthtmp=(opakc(i)+optpp)*delr
         dpthc(lind,i) = dpthc(lind,i) + dpthtmp
         if (lpri.ne.0) write (lun11,*)i,epi(i),opakc(i),optmp(i),
     $             dpthc(lind,i),dpthtmp
         if (dpthtmp.gt.dpthmx) then
           dpthmx=dpthtmp
           indmx=i
           endif
         enddo
      nlyc=nbinc(13.7,epi)
      nry=nlyc+1
c      write (lun11,*)'in stpcut',indmx,dpthmx,epi(indmx)
c
c     calculate line depths
      do  i = 1,nlsvn
         tau0(lind,i) = tau0(lind,i) + oplin(i)*delr
c         write (lun11,*)i,lind,oplin(i),delr,tau0(lind,i)
         enddo
c
c     calculate level depths
c      write (lun11,*)'in stpcut:',delr,lind
      do i = 1,ncsvn
         tauc(lind,i) = tauc(lind,i) + opakab(i)*delr
c         write (lun11,*)i,opakab(i),tauc(lind,i)
         enddo
c
c
      return
      end
       subroutine szcoll(ni,nj,tt,rate,ic,lun11)   
c     calculates electron impact excitation rates from semiempirical 
c     formula (eq.35) from smpson & zhang (1988, apj 335, 516)
c       real*8 abethe(11), hbethe(11), rbethe(11)
c       real*8 fvg1(5),fvg2(5),fvg3(5)
       real abethe(11), hbethe(11), rbethe(11)
       real fvg1(5),fvg2(5),fvg3(5)
       data(abethe(i),i=1,11)/ 1.30, 0.59, 0.38, 0.286, 0.229, 0.192,
     1       0.164, 0.141, 0.121, 0.105, 0.100 /
       data(hbethe(i),i=1,11)/ 1.48, 3.64, 5.93, 8.32, 10.75, 12.90,
     1       15.05, 17.20, 19.35, 21.50, 2.15 /
       data(rbethe(i),i=1,11)/ 1.83, 1.60, 1.53, 1.495, 1.475, 1.46,
     1       1.45, 1.45, 1.46, 1.47, 1.48 /
       data(fvg1(i),i=1,5)/ 1.133, 1.0785, 0.9935, 0.2328, -0.1296/
       data(fvg2(i),i=1,5)/ -0.4059, -0.2319, 0.6282, -0.5598,0.5299/
       data(fvg3(i),i=1,5)/ 0.07014, 0.02947, 0.3887, -1.181, 1.47/
       boltz=1.38066d-16
       eion=1.578203d+5
       const=8.63d-6   
c
       rn2=(float(ni)/float(nj))**2
       delnn=(1./float(ni*ni)-1./float(nj*nj))**2
c  computes fvalue as in johnson 1972)
       g1=0.
       g2=0.
       g3=0.
       if (ni.eq.1 ) then
        g1=fvg1(1)
        g2=fvg2(1)
        g3=fvg3(1)
       endif
       if (ni.eq.2 ) then
        g1=fvg1(2)
        g2=fvg2(2)
        g3=fvg3(2)
       endif
       if (ni.ge.3) then
        g1=fvg1(3)+fvg1(4)/ni+fvg1(5)/ni/ni
        g2=(fvg2(3)+fvg2(4)/ni+fvg2(5)/ni/ni)/ni*(-1.)
        g3=(fvg3(3)+fvg3(4)/ni+fvg3(5)/ni/ni)/ni/ni
       endif
       xx=1.-rn2
       gaunt=g1+g2/xx+g3/xx/xx
       fnn=1.9603*gaunt/(xx**3)*ni/(nj**3)
c
       if (ni.lt.11) then
         an=abethe(ni)
         hn=hbethe(ni)
         rrn=rbethe(ni)
       else  
         an=abethe(11)/float(ni)
         hn=hbethe(11)*float(ni)
         rrn=rbethe(11)
       endif
       ann=fnn*4.*(ni**4)/(1.-rn2)
       dnn=ann*hn*((1.-rn2)**rrn - an*rn2) 
c       write (lun11,*)ann,hn,rn2,rrn,an,fnn,dnn
       cnn=1.12*ni*ann*(1.-rn2)
       if ((nj-ni).eq.1) cnn=cnn*expo(-0.006*((ni-1)**6)/ic)
       yy=eion*ic*ic*(1./float(ni*ni)-1./float(nj*nj))/tt
       call eint(yy,eint1,e2,e3)
       rate=const/sqrt(tt)/ni/ni/ic/ic*(dnn*expo(-yy)+(ann+
     1   yy*(cnn-dnn))*eint1)
c       write (lun11,*)'in szcoll:',const,ni,nj,tt,ic,dnn,yy,
c     $     ann,cnn,dnn,eint1
       return
       end
      subroutine szirc(nn,T,rz,rno,cii)
c     calculates electron impact ionizition rates from semiempirical
c     formula (eq.35) from Smpson & Zhang (1988, ApJ 335, 516)
       real abethe(11), hbethe(11), rbethe(11)
       DATA(abethe(i),i=1,11)/ 1.134, 0.603, 0.412, 0.313, 0.252,
     1       0.211, 0.181, 0.159, 0.142, 0.128, 1.307 /
       DATA(hbethe(i),i=1,11)/ 1.48, 3.64, 5.93, 8.32, 10.75, 12.90,
     1       15.05, 17.20, 19.35, 21.50, 2.15 /
       DATA(rbethe(i),i=1,11)/ 2.20, 1.90, 1.73, 1.65, 1.60, 1.56,
     1       1.54, 1.52, 1.52, 1.52, 1.52 /
       Boltz=1.38066e-16
       Eion=2.179874e-11
       const=4.6513e-3
C
       rc=float(int(rno))
       if (nn.lt.11) then
         an=abethe(nn)
         hn=hbethe(nn)
         rrn=rbethe(nn)
       else
         an=abethe(11)/float(nn)
         hn=hbethe(11)*float(nn)
         rrn=rbethe(11)
       endif
       tt= T*Boltz
       rn=float(nn)
c      yy=rz*rz/(rn*rn)*Eion/tt
       yy=rz*rz*Eion/tt*(1./rn/rn-1./rc/rc-.25*(1./(rc-1.)**2-
     c    1./rc/rc))
       call eint(yy,e1,e2,e3)
       cii=const*sqrt(tt)*(rn**5)/(rz**4)*an*yy* (
     1   e1/rn-(exp(-yy)-yy*e3)/(3.*rn)+(yy*e2-2.*yy*e1+exp(-yy))*
     2   3.*hn/rn/(3.-rrn)+(e1-e2)*3.36*yy)

c       print*,YY,1./yy,cii
C      give result
       end
c ----------------------------------------------------------
        subroutine szirco(nn,t,rz,cii,lun11)
c     calculates electron impact ionizition rates from semiempirical
c     formula (eq.35) from smpson & zhang (1988, apj 335, 516)
       real abethe(11), hbethe(11), rbethe(11)
       data(abethe(i),i=1,11)/ 1.134, 0.603, 0.412, 0.313, 0.252,
     1       0.211, 0.181, 0.159, 0.142, 0.128, 1.307 /
       data(hbethe(i),i=1,11)/ 1.48, 3.64, 5.93, 8.32, 10.75, 12.90,
     1       15.05, 17.20, 19.35, 21.50, 2.15 /
       data(rbethe(i),i=1,11)/ 2.20, 1.90, 1.73, 1.65, 1.60, 1.56,
     1       1.54, 1.52, 1.52, 1.52, 1.52 /
       boltz=1.38066e-16
       eion=2.179874e-11
       const=4.6513e-3
c
       if (nn.lt.11) then
         an=abethe(nn)
         hn=hbethe(nn)
         rrn=rbethe(nn)
       else
         an=abethe(11)/float(nn)
         hn=hbethe(11)*float(nn)
         rrn=rbethe(11)
       endif
       tt= t*boltz
       rn=float(nn)
c       rz=float(nz)
       yy=rz*rz/(rn*rn)*eion/tt
       call eint(yy,e1,e2,e3)
       term1=e1/rn-(expo(-yy)-yy*e3)/(3.*rn)
       term2=(yy*e2-2.*yy*e1+expo(-yy))*3.*hn/rn/(3.-rrn)
       term3=(e1-e2)*3.36*yy
       cii=const*sqrt(tt)*(rn**5)/(rz**4)*an*yy* (
     1   term1+term2+term3)
c       write (lun11,*)'in szirc:',nn,t,an,hn,rrn,rn,yy,e1,e2,e3,term1,
c     $    term2,term3,cii
c      give result
       end
c ----------------------------------------------------------
      subroutine trnfrc(lpri,lun11,ldir,
     $      r,xpxcol,xpx,
     $      epi,zremsz,dpthc,rccemis,opakc,
     $      zrems,fline,bremsa,bremsint)
c
      parameter (ncn=9999)
c
      dimension epi(ncn),zremsz(ncn),dpthc(2,ncn),bremsa(ncn)
      dimension bremsint(ncn),rccemis(2,ncn),opakc(ncn),
     $          zrems(3,ncn),fline(ncn)
c
c
      lprisv=lpri
      r19=r/(1.e+19)
      fpr2=(12.56)*r19*r19
      ncnm=ncn-1
      bremsa(ncn)=0.
      bremsint(ncn)=0.
      nb1=nbinc(13.7,epi)
      sum=0.
      rmax=xpxcol/xpx
      if (lpri.ne.0) write (lun11,*)'in trnfrc:',rmax,xpxcol,xpx
      do 1 jkp=1,ncnm
         jk=ncnm+1-jkp
c
c        for outward only
c
         if (ldir.lt.0) then
             bremsa(jk)=zrems(1,jk)/fpr2
           else
             bremsa(jk)=zremsz(jk)*expo(-dpthc(1,jk))/fpr2
           endif
         sumtmp=(bremsa(jk)+bremsa(jk+1))*(epi(jk+1)-epi(jk))/2.
         bremsint(jk)=bremsint(jk+1)+sumtmp
         sum=sum+sumtmp
         if (lpri.ne.0) write (lun11,*)jk,epi(jk),dpthc(1,jk),
     $       zremsz(jk),opakc(jk),
     $       zremsz(jk)*expo(-dpthc(1,jk))/fpr2,bremsa(jk)
1        continue
      do jk=1,ncn
         bremsint(jk)=bremsint(jk)/max(1.e-36,sum)
         enddo
c
      return
      end
      subroutine trnfrn(lpri,lun11,vturbi,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,
     $       r,delr,
     $       epi,
     $       tau0,dpthc,tauc,
     $       rcem,rccemis,brcems,opakc,opakab,cemab,
     $       zrems,zremso,elumab,elumabo,elum,elumo,zremsz,
     $       etoti,etotc,etotl)
c
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
      parameter (ntyp=90)
      parameter (ndl=2400,nd=ndl+1)
c
c     master data
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nni,nd),npilevi(nnml)
      dimension epi(ncn)
      dimension cemab(2,nnml),opakab(nnml)
      dimension elumab(2,nnml),elumabo(2,nnml)
      dimension rccemis(2,ncn),opakc(ncn)
      dimension rcem(2,nnnl),brcems(ncn)
      dimension tau0(2,nnnl)
      dimension dpthc(2,ncn)
      dimension tauc(2,nnml)
      dimension zrems(3,ncn),zremso(3,ncn),
     $          zremsz(ncn)
      dimension elum(2,nnnl),elumo(2,nnnl)
c
      data ergsev/1.602197e-12/
c
c     
c     transfer continuum
      lprisv=lpri
      if (lpri.ge.1) write (lun11,*)'in trnfrn'
      numcon=ncn
      do kl=1,numcon
        do ll=1,3
          zremso(ll,kl) = zrems(ll,kl)
          enddo
        enddo
c
c     transfer lines
      do jkk=1,nlsvn
        jk=jkk
        do ll=1,2
          elumo(ll,jk)=elum(ll,jk)
          if (lpri.ge.1) write (lun11,*)jk,elum(1,jk),elumo(1,jk)
          enddo
        enddo
c
c     transfer RRCs
      do jkk=1,ncsvn
        jk=jkk
        do ll=1,2
          elumabo(ll,jk)=elumab(ll,jk)
          if (lpri.ge.1) write (lun11,*)jk,elumab(1,jk),elumabo(1,jk)
          enddo
        enddo
c
c
      return
      end
       subroutine ucalc(ndesc,nrdesc,ml,lcon,jkion,vturbi,
     $   nrdat,rdat,nidat,idat,nkdat,kdat,ans1,ans2,
     $   ans3,ans4,idest1,idest2,idest3,idest4,
     $   abund1,abund2,ptmp1,ptmp2,xpx,opakab,
     $         delr,
     $   opakc,rccemis,lpriu,kdesc2,
     $   rr,t,trad,tsq,xee,xh1,xh0,
     $   epi,bremsa,bremsint,xiin,
     $   rlev,ilev,nlpt,iltp,nlev,klev,lfast,lun11,
     $          idat1,rdat1,kdat1,nptrs,np2,
     $     npar,npnxt,npfi,npfirst,
     $     nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,ncsvn,rates,vsav,idrates,lforce)
c
      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
     $               ,tucalc(ntyp),ncall(ntyp),theat
c
      data ergsev/1.602197e-12/
      data pi/3.1415927/,c/2.997925e10/,luse8/0/
c
      character(1) kdat(20000),klev(100,nd),kdat2(20000)
      character(48) kdesc(ntyp),kdesc2
      character(28) krdesc(ntyp)
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c     $      ,np1r,np1i,np1k,np2
      character(1) kdat1(nkdat1)
c     pointers to master data 
      dimension npar(ndat2),npnxt(ndat2),
     $      npfirst(ntyp)
      dimension npfi(ntyp,nni)
c     pointers to line data 
      dimension nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      dimension npcon(nnml),npconi2(ndat2),npconi(ndat2)
      dimension npilev(nd,nni),npilevi(nnml)
c     the saved rates
      dimension rates(4,ndat2),idrates(2,ndat2)
      dimension vsav(4,ndat2)
      dimension
     $   epi(ncn),bremsa(ncn),bremsint(ncn),
     $   rlev(10,nd),ilev(10,nd),nlpt(nd),iltp(nd)
      dimension aa(11),aaa(11,10),bbb(10),sg(ncn)
      dimension rdat(20000),idat(20000)
      dimension rdat2(20000),idat2(20000)
      dimension rstorey(5),dcfe(8),defe(8),alhe(2)
      dimension etmpp(99999),stmpp(99999),ttmp(400),xsec(100)
      dimension rccemis(2,ncn),opakc(ncn)
      dimension rccems2(2,ncn)
      dimension xiin(nni),stmpe(99999),dum1(ncn)
      real*8 zc,eion,far,gam,scal,etmp8(ncn),stmp8(ncn)
c
      save aa,bb,ddd,ett,ggup,gglo,hij,
     $         swrat,elin,pi,c,ergsev,etmp8,luse8
c
      data krdesc(1)/'ground state ionization        '/
      data krdesc(2)/'level ionization/recombination '/
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
      data kdesc(37)/'                                                '/
      data kdesc(38)/'                                                '/
      data kdesc(39)/'                                                '/
      data kdesc(40)/'                                                '/
      data kdesc(41)/'                                                '/
      data kdesc(42)/'                                                '/
      data kdesc(43)/'                                                '/
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
c
      call remtms(time1)
c
      xnx=xpx*xee
c      write (6,*)'in ucalc:',ml,ndesc,nrdesc,lpriu,lun11
c
c
      lpri=lpriu
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc:',ndesc,lcon,nrdat,nidat,nkdat,
     $  ml,(rdat(mm),mm=1,nrdat),(idat(mm),mm=1,nidat),
     $  (kdat(mm),mm=1,nkdat)
       if (lpri.gt.1) write (lun11,*)'in ucalc, inputs:',
     $   t,xee,xpx,xnx
c
      vturb=vturbi
c      vturb=0.
c
      kdesc2=kdesc(ndesc)
c
      if (luse8.eq.0) then
        luse8=1
        do mm=1,ncn
          etmp8(mm)=epi(mm)/13.598
          enddo
        endif
      nlevp=nlev
      ans1=0.
      ans2=0.
      ans3=0.
      ans4=0.
      idest1=0
      idest2=0
      idest3=idat(nidat)
      idest4=idat(nidat)+1
      opakab=0.
c
      go to (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,
     $  17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,
     $  36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,
     $  56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,
     $  76,77,78,79,80,81,82,83,84,85,86,87,88), ndesc
c
c
c     rr, a&p formula
1     continue
c      write (lun11,*)'in ucalc, ndesc=1'
      arad=rdat(1)
      eta=rdat(2)
      rrrt=arad/t**eta
      ans1=rrrt*xnx
      idest1=1
      idest2=0
c      write (lun11,*)'in ucalc, ndesc=1'
      go to 9000
c
c     h charge exchange recombination
 2    continue
      aax=rdat(1)
      bbx=rdat(2)
      ccx=rdat(3)
      ddx=rdat(4)      
      rate=aax*expo(log(t)*bbx)*(1.+ccx*expo(ddx*t))*(1.e-9)
      ans1=rate*xh0
      ans2=0.
      idest1=1
      idest2=nlevp
      go to 9000
c      beth=rdat(1)
c      alh(1)=rdat(2)
c      alh(2)=rdat(3)
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
      cai=rdat(1)
      eai=rdat(2)
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
      idest1=idat(1)
      idest2=idat(2)
      elin=rdat(1)
      flin=rdat(2)
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
      a=rdat(5)
      hij=elin*1.e-8
      elammu=elin*1.e-4     
      aij=(6.67e+7)*gglo*flin/ggup/elammu/elammu
c     this is a fudge to avoid badnumerics from fine structure.
      if (flin.le.1.01e-12) aij=1.e+5
      if (elin.ge.1.e+9) aij=1.e+5
      ans1=aij
      ans4=aij*ergsev*12398.54/abs(elin)
      vtherm=max(vturb*1.e+5,1.3e+6/sqrt(a/t))
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
c      sigvtherm=(0.02655)*flin*elin*(1.e-8)/3.e+10
      sigvtherm=sigma
      ener=12398.54/abs(elin)
      nb1=nbinc(ener,epi)
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
      idest1=idat(2)
      idest2=idat(1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      ans1=0.
      ans2=0.
      ggup=rlev(2,idat(2))
      gglo=rlev(2,idat(1))
      hij=elin*1.e-8
      ekt=t*(0.861707)
      eex=abs(rlev(1,idat(1))-rlev(1,idat(2)))
      ans2=(8.629e-8)*rdat(5)*t**rdat(6)/ggup
      exptmp=expo(-eex/ekt)
      ans1=(8.629e-8)*rdat(5)*t**rdat(6)*exptmp/gglo
c      write (lun11,*)'ltyp=5',idest1,idest2,elin,flin,ggup,gglo,
c     $       ans2,eex,ekt,exptmp,ans1
      go to 9000
c      write (lun11,*)'in ucalc, ltyp=17',idat(1),idat(2),ggup,
c     $          gglo,eex,ans2,ans1
c
c     level quantities, partition function
 6    continue      
      go to 9000
c
c     dr, a&p formula
 7    continue
      adi=rdat(1)
      bdi=rdat(2)
      t0=rdat(3)
      t1=rdat(4)
      ap=1.
      dirt=adi*ap*(1.e-06)*expo(-t0/t)
     $  *(1.+bdi*expo(-t1/t))/(t*sqrt(t))
      ans1=dirt*xnx
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
        dcfe(n)=rdat(n)
        defe(n)=rdat(n+4)
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
      aax=rdat(1)
      bbx=rdat(2)
      ccx=rdat(3)
      ddx=rdat(4)
      rate=aax*t**bbx*(1.+ccx*expo(ddx*t))*(1.e-9)
      ans2=rate*xh0
      ans1=0.
      idest1=1
      idest2=nlevp
      if (nidat.gt.1) then
        idest1=idat(1)
        idest2=nlevp+idat(2)-1
        ans2=ans2/6.
        endif
c      if (jkion.eq.18) idest1=3
      go to 9000
      bethe=rdat(1)
      alhe(1)=rdat(2)
      alhe(2)=rdat(3)
      ntcs=2
      if (t.lt.1.) ntcs=1
      hecxrt = bethe*t**alhe(ntcs)
      xhe1 = xiin(2)*xpx
      xhe1=0.
      ans1=hecxrt*xhe1
      idest1=1
      idest2=0
      go to 9000
c
c
 10   continue
c     charge transfer ionzation as used in func2, for level rates    
      aax=rdat(1)
      bbx=rdat(2)
      ccx=rdat(3)
      ddx=rdat(4)
      eex=rdat(5)
      rate=aax*t**bbx*(1.+ccx*expo(ddx*t))
     $             *expo(-eex/t)*(1.e-9)
      ans1=rate*xh1
      ans2=0.
      idest1=1
      idest2=nlevp
      go to 9000
c
 11   continue
      idest2=idat(1)
      idest1=idat(2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      dele=abs(rlev(1,idest2)-rlev(1,idest1))
      ggl=rdat(3)
      ggu=rdat(4)
      ans1=(6.669e+15)*rdat(2)*ggl/(ggu*rdat(1)*rdat(1))
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
c      if (lpri.gt.1) write (lun11,*)(rdat(jj),jj=1,nrdat)
c      if (lpri.ne.0) write (lun11,*)(idat(jj),jj=1,nidat)
c      if (lpri.ne.0) write (lun11,*)(kdat(jj),jj=1,nkdat)
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      ntmp=nrdat/2
      do ml2=1,ntmp+1
        etmpp(ml2)=rdat(2*ml2-1)
        stmpp(ml2)=rdat(2*ml2)*1.e-18
        enddo
      call dread(ltyp2,lrtyp2,lcon2,
     $          nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,nilin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      idest1=idat(nidat-1)
      idest2=idat(nidat-2)-idat(nidat)
      if (lpri.gt.1)
     $ write (lun11,*)ml,nilin,rdat(1),idest1
      ett=rdat2(1)
      if (lpri.gt.1)
     $ write (lun11,*)'ett=',ett
      nb1=nbinc(ett,epi)
      gglo=rlev(2,1)
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) stop 'ggup error'
      swrat=gglo/ggup
      d=rdat(2)
      do mm=1,11
        aa(mm)=rdat(3+mm)
        enddo
c      aa(1)=min(max(aa(1),-6.),6.)
      ekt=t*(0.861707)
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=15:',lcon,
     $               nrdat,nidat,nkdat
      if (lpri.gt.1)
     $ write (lun11,891)(rdat(mm),mm=1,nrdat)
 891  format (1x,10(1pe10.3))
      if (lpri.gt.1)
     $ write (lun11,892)(idat(mm),mm=1,nidat)
 892  format (1x,10(i6))
      if (lpri.gt.1)
     $ write (lun11,893)(kdat(mm),mm=1,nkdat)
 893  format (1x,100a1)
      na=idat(nidat-5)
      nsh=idat(nidat-4)
      do lk=1,na
        ll=idat(9*lk-5)
        if (lpri.gt.1)
     $   write (lun11,*)'ll=',ll,lk
        lz=15*(lk-1)
        ett=rdat(1+lz)
        ddd=rdat(2+lz)
        bb=rdat(3+lz)
        aa(1)=rdat(4+lz)
        aa(2)=rdat(5+lz)
        if (lpri.gt.1)
     $   write (lun11,*)'ltest=2',ett,ddd,bb,aa(1)
        do 1011 mml=1,5
          aa(2+mml)=rdat(mml+5+lz)
 1011     continue
        do 1012 mml=1,4
          aa(7+mml)=rdat(mml+10+lz)
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
      call bkhsgo(sg,ett,ddd,bbb,na,
     $         aaa,epi,t,lprib,lfast,lun11)
      lprib=0
c      if (lpri.gt.1) lprib=lpri      
      gglo=rlev(2,1)
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) stop 'ggup error'
      swrat=gglo/ggup
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,lfast,lun11)
      if (lpri.ge.1) then
        npr=nbinc(ett,epi)
        write (lun11,*)'bkh threshold xsection:',
     $         npr,ett,sg(npr)
        endif
      lpri=lprisv
      go to 9000
c
 16   continue
      ekt=t*(0.861707)
      njj=int(nrdat/5)
c      write (lun11,*)'ltyp=16:',idat(nidat)
      csum=0.
      csum2=0.
      do mm=1,njj
        mm5=5*(mm-1)
        eth=rdat(mm5+1)
        a=rdat(mm5+2)
        b=rdat(mm5+3)
        c=rdat(mm5+4)
        d=rdat(mm5+5)
        xx=eth/ekt
c        write (lun11,*)'xx=',xx
        em1=ee1expo(xx)
        f1=em1/xx
c        write (lun11,*)'before ff2:',f1,em1,xx
        f2=ff2(xx,lun11)
c        write (lun11,*)xxma,b,c,d,em1,f1,f2
        fi=a*(1.-xx*f1)+b*(1.+xx-xx*(2.+xx)*f1)
     $   +c*f1+d*xx*f2
        fi=max(fi,0.)
        csum=csum+fi*expo(-xx)/xx
        csum2=csum2+fi/xx
c        write (lun11,*)mm,mm5,a,b,c,d,xx,f1,fi,csum
        enddo
      citmp1=csum*(6.69e-7)/(ekt)**(1.5)
      ans1=citmp1*xnx
      citmp2=csum2*(6.69e-7)/(ekt)**(1.5)
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
      go to 9000
c
 17   continue
c     line rates, col
      ans1=0.
      ans2=0.
c      go to 9000
      hij=elin*1.e-8
c      write (lun11,*)'ltyp=4',idest1,idest2,elin,flin,ggup,gglo
      ekt=t*(0.861707)
      idest1=idat(2)
      idest2=idat(1)
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      if (eelo.gt.eeup) then
        idest1=idat(1)
        idest2=idat(2)
        eeup=rlev(1,idest2)
        eelo=rlev(1,idest1)
        endif
      eex=eeup-eelo
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      ans2=(8.629e-8)*rdat(1)*t**rdat(2)/ggup
      ans1=0.
      exptmp=expo(-eex/ekt)
      exptmp=1.
      if (ekt.gt.eex/20.) 
     $ ans1=ans2*ggup*exptmp/gglo
      if (lpri.gt.1)
     $ write (lun11,*)'in ucalc, ltyp=17',idat(1),idat(2),ggup,
     $          gglo,eex,ans2,ans1
      go to 9000
c
 18   continue
      aarec=rdat(1)
      bbrec=rdat(2)
      ccrec=rdat(3)
      ttz=rdat(4)
      algt=alog10(t/(1.e-32+ttz))+4.
      algt=max(algt,3.5)
      algt=min(algt,7.5)
      idest1=idat(1)
      ans1=exp10(aarec+bbrec*(algt-ccrec)**2)/t/1.e+4
      ans1=ans1*xnx
c      ans1=0.
      ans2=0.
      idest2=0
      go to 9000
c
 19   continue
      etkh=rdat(5)
      enelec=1.
      eth=etkh
      nb1=nbinc(eth,epi)
      idest1=idat(1)
      idest2=nlevp
      ggup=rlev(2,nlevp)
      gglo=rlev(2,idest1)
      swrat=gglo/ggup
      ekt=t*(0.861707)
      lm=nb1
 1912   continue
         bbb2=epi(lm)/amax1(etkh,1.e-30)
         etmp=log(bbb2)
         alppp=rdat(1)+etmp*(rdat(2)+etmp*
     $         (rdat(3)+etmp*rdat(4)))
         ppp=expo(alppp)
         sg(lm)=(1.e-18)*enelec*ppp*(13.606)/etkh
         call enxt(eth,nb1,lpri,epi,t,lfast,lun11,
     $                  lm,nskp,nphint,lrcalc)
        lm=lm+nskp
        if (lm.le.nphint) go to 1912
      call phintfo(sg,eth,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,lfast,lun11)
      go to 9000
c
 20   continue
c     charge transfer ionzation as used in func1, for total rate    
      aax=rdat(1)
      bbx=rdat(2)
      ccx=rdat(3)
      ddx=rdat(4)
      eex=rdat(5)
      rate=aax*t**bbx*(1.+ccx*expo(ddx*t))
     $             *expo(-eex/t)*(1.e-9)
      ans1=rate*xh1
      ans2=0.
      idest1=1
      idest2=nlevp
      idest3=idat(nidat)
      idest4=idest3+1
      go to 9000
c
 21   continue
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
        rstorey(kl)=rdat(kl)
 221    continue
      if (rstorey(5).lt.0.) go to 9000
      t3s2=t**(-1.5)
      dirtemp=
     $   (1.e-12)*(rstorey(1)/t+rstorey(2)
     $   +t*(rstorey(3)+t*rstorey(4)))*t3s2
     $   *expo(-rstorey(5)/t)
      dirtemp=max(dirtemp,0.)
      if (lpri.gt.1) write (lun11,*)'in ucalc, ltyp=22:',
     $   ndesc,lcon,nrdat,nidat,nkdat,
     $  ml,(rdat(mm),mm=1,nrdat),(idat(mm),mm=1,nidat),
     $  (kdat(mm),mm=1,nkdat),dirtemp,xnx
      ans1=dirtemp*xnx
      idest1=1
      idest2=0
      go to 9000
c
 23   continue
      lfastl=lfast
      lprisv=lpri
c      lpri=2
      if (lpri.gt.1)
     $ write (lun11,*)'in ucalc, 23:',rdat(1),rdat(2),rdat(3),rdat(4),
     $          rdat(5),swrat,idat(1),idat(2),idat(3),idat(4),idat(5)
      ett=rlev(1,idat(1))
      eth=ett
      nb1=nbinc(eth,epi)
      gglo=rlev(2,idat(1))
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) stop 'ggup error'
      swrat=gglo/ggup
      ekt=t*(0.861707)
      jkk2=idat(nidat)
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      jkk3=0
      ndtmp=npfirst(12)
 1512   continue
        jkk3=jkk3+1
        call dread(ltyp2,lrtyp2,lcon2,
     $          nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,ndtmp-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
        jkk=idat2(nidt2)
        ndtmp=npnxt(ndtmp)
        if ((jkk.ne.jkk2).and.(ndtmp.ne.0)) go to 1512
      if (ndtmp.le.0) go to 9000
      zzz=float(idat2(1))
      ndtmp=npfi(13,jkk3)
      mllz=npar(ndtmp)
      if (lpri.gt.1) write (lun11,*)jkk,jkk2,jkk3,zzz,ndtmp
 1511   continue
        call dread(ltyp2,lrtyp2,lcon2,
     $          nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,ndtmp-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
        iltmp=idat2(nidt2-1)
        ndtmp=npnxt(ndtmp)
        if ((ndtmp.ne.0).and.(iltmp.ne.idat(1))
     $      .and.(npar(ndtmp).eq.mllz)) go to 1511
      nprn=idat(1)
c      write (lun11,*)'ind=23:',ml,nilin,zzz,jkk,nprn,sg0   
      enn=float(nprn)
      if ((enn.le.1.e-24).or.(zzz.le.1.e-24).or.(ett.le.1.e-6))
     $  go to 9000
      sg0=6.3e-18*enn/zzz/zzz
      if (lpri.gt.1) 
     $ write (lun11,*)'ind=23:',ml,nilin,zzz,jkk,nprn,sg0,ett
      ll=nb1     
 1611   continue
        epii=epi(ll)
        sg(ll)=sg0*(epii/ett)**(-3)
        call enxt(ett,nb1,lpri,epi,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        if (ll.le.nphint) go to 1611
      lprib=0
c      if (lpri.gt.1) lprib=lpri
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,lfast,lun11)
      lpri=lprisv
c      ans2=ans2*xnx
      idest1=idat(nidat-1)
      idest2=nlevp
      go to 9000
c
 24   continue
      go to 9000
c
 25   continue
      if (nrdesc.eq.5) then
          idest2=nlevp
        else
          idest2=1
c          go to 9000
        endif
      e=rdat(1)
      a=rdat(2)
      b=rdat(3)
      c=rdat(4)
      d=rdat(5)
      cion = 0.
      chir = (t*1.e+4)/(11590.*e)
      citmp1=cion
      ans1=citmp1*xnx
      ans2=0.
      idest1=1
c      idest2=1
      if ( chir.le..0115 ) go to 9000
      chi = amax1(chir,0.1)
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
      gglo=rlev(2,nidat-1) 
c     note that rinf has exponential removed
      rinf=(2.08e-22)*gglo/ggup/t/tsq
      ans2=ans1*rinf*xnx
      ans1=ans1*chitmp
c      idest1=idat(nidat-1)
      idest4=idat(nidat)+1
      idest3=idat(nidat)      
      go to 9000
c
 26   continue
      go to 9000
c      ekt=t*(0.861707)
c      idest1=idat(1)
c      gglo=rlev(2,idest1)
c      edelt=abs(rlev(1,idest1)-rlev(1,nlev))
c      exptmp=expo(-edelt/ekt)
c      ans1=(4.1416e-9)*rdat(1)*t**rdat(2)/gglo
c      ggup=rlev(2,nlev)
c      rinf=(2.08e-22)*gglo/ggup/t/tsq
c      ans2=ans1*rinf
c      ans1=ans1*exptmp
c      write (lun11,*)'ltyp=26',idest1,gglo,ggup,
c     $   edelt,rdat(1),rdat(2),ans1
c      idest2=nlev
c      go to 9000
c
 27   continue
c      go to 9000
      lprisv=lpri
c      ett=rdat(2)
      lfastl=2
c      lpri=2
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
      nb1=nbinc(eth,epi)
      gglo=rlev(2,idat(1))
      swrat=gglo
      ekt=t*(0.861707)
      ll=nb1
 1913   continue
        epii=epi(ll)
        e=epii
        eth=ett
        zap = e/eth - 1.
        y = e/eth
        yy=sqrt(zap)
        yy=amax1(yy,1.e-04)
        fh2lke=((6.3e-18)/rdat(1)/rdat(1))
     $   *y**(-4)*expo(4.-4.*atan(yy)/yy)
     $   /(1.-expo(-6.2832/yy))
c        fh2lke=((6.3e-18)/rdat(1)/rdat(1))*y**(-3)
        sg(ll)=fh2lke
        if (lpri.ge.3) write (lun11,*)ll,epii,zap,y,yy,fh2lke
        call enxt(ett,nb1,lpri,epi,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        if (ll.le.nphint) go to 1913
      lprib=0
      if (lpri.gt.1) lprib=1
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,lfast,lun11)
      lpri=lprisv
      go to 9000
c
 28   continue
c     line rates, col
      idest1=idat(1)
      idest2=idat(2)
      if (rlev(1,idat(2)).lt.rlev(1,idat(1))) then
        idest2=idat(1)
        idest1=idat(2)
        endif
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      nind=5
      if (nrdat.ge.12) then
        do ll=1,4
          ttmp(ll)=rdat(nrdat-4+ll)
          enddo
        jlo=0
        call hunt(ttmp,4,t,jlo,0,lun11)     
        nind=nrdat-8+jlo      
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      elin=rdat(1)
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
c      nind=nrdat-2
      cijpp=rdat(nind)
      ekt=0.861707*t
      delt=12398.54/elin/ekt
      cji=(8.626e-8)*cijpp/tsq/ggup
      cij=0.
      exptmp=expo(-delt)
      cij=cji*ggup*exptmp/tsq/gglo
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=28',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdat,(rdat(mm),mm=1,8),nind,jlo
        write (lun11,*)'       ',cij,cji,xnx,cijpp,exptmp
        endif
      elin=0.
      go  to 9000
c
 29   continue
      go to 9000
c      anstmp=rdat(2)*(8.626e-8)/tsq
c      ans2=anstmp*(2.08e-22)*(rdat(3)/rdat(4))/t/tsq
c      ans1=0.
c      delt=rdat(1)/t
c      if (delt.lt.50.) then
c         exptmp=1.
c         exptmp=expo(-delt)
c         ans1=anstmp*exptmp
c         endif
c      idest1=idat(2)
c      idest2=nlev
c      write (lun11,*)'ltyp=29',ans1,ans2,(rdat(ii),ii=1,4),anstmp,xnx
c      go to 9000
c
 30   continue
c      write (lun11,*)'ltyp=30',idat(1)
        nmx=idat(1)
        t6=t/100.
        zeff=float(nmx)
        beta=zeff*zeff/(6.34*t6)
        yy=beta
        vth=(3.10782e+7)*sqrt(t)
c       fudge factor makes the 2 expressions join smoothly
        ypow=min(1.,(0.06376)/yy/yy)
        fudge=0.9*(1.-ypow)+(1./1.5)*ypow
        phi1=(1.735+alog(yy)+1./6./yy)*fudge/2.
        phi2=yy*(-1.202*alog(yy)-0.298)
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
      idest1=idat(2)
      idest2=idat(1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      elin=rdat(1)
      flin=rdat(2)
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
c      a=rdat(5)
      ans1=0.
      ans2=0.
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      nelin=npar(nilin)
c      write (lun11,*)'in ucalc, ind=31:',
c     $  ml,nilin,nelin,elin,flin
      if ((nilin.le.0).or.(nelin.le.0)) go to 9000
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nelin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      a=rdat2(2)
      hij=elin*1.e-8
      aij=(0.02655)*flin*8.*pi/hij/hij*gglo/(1.e-24+ggup)
      ans1=aij
      vtherm=max(vturb*1.e+5,1.3e+6/sqrt(a/t))
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
c      sigvtherm=(0.02655)*flin*elin*(1.e-8)/3.e+10
      sigvtherm=sigma
      ener=12398.54/abs(elin)
      nb1=nbinc(ener,epi)
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
c      write (lun11,*)'ltyp=31',idest1,idest2,elin,flin,ggup,gglo
      go to 9000
c
 32   continue  
      idest1=idat(1)
      gglo=rdat(4)
      ans1=0.
      ans2=0.
      go to 9000
c      if (gglo.lt.1.e-24) go to 9000
c      ekt=t*(0.861707)
c      edelt=rdat(3)
c      ans1=(4.1416e-9)*rdat(1)*t**rdat(2)*expo(-edelt/ekt)
c     $        /gglo
c      write (lun11,*)'ltyp=26',idest1,gglo,edelt,rdat(1),rdat(2),ans1
c      idest2=nlev
c      go to 9000
c
 33   continue
c     line rates, col
      idest1=idat(2)
      idest2=idat(1)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      ggup=rlev(2,idat(2))
      gglo=rlev(2,idat(1))
      elin=rdat(1)
      hij=elin*1.e-8
c      write (lun11,*)'ltyp=4',idest1,idest2,elin,flin,ggup,gglo
      if (elin.le.1.e-24) go to 9000
      nind=4
      cijpp=rdat(nind)
      ekt=0.861707*t
      delt=12398.54/elin/ekt
      exptmp=expo(-delt)
      cij=(8.626e-8)*cijpp*exptmp/tsq/gglo
      cji=(8.626e-8)*cijpp/tsq/ggup
      ans1=cij*xnx
      ans2=cji*xnx
c      write (lun11,*)'ltyp=28',cij,cji,xnx,cijpp
      go to 9000
c
 34   continue
c     line rates, coll and rad
c           write (lun11,*)'level data'
c           do 1906 ll=1,nlev
c             write (lun11,*)ll,(rlev(mm,ll),mm=1,3),
c     $          (ilev(mm,ll),mm=1,3),(klev(mm,ll),mm=1,3)
c 1906        continue
      idest1=idat(1)
      idest2=idat(2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      elin=rdat(1)
      aij=rdat(2)
      eeup=rlev(1,idest1)
      eelo=rlev(1,idest2)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         endif
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
c      ggup=rdat(4)
c      gglo=rdat(3)
      a=rdat(5)
      hij=elin*1.e-8
      elammu=elin*1.e-4     
      flin=aij*hij*hij*ggup/((0.02655)*8.*pi*gglo)
      flin=aij*hij*hij*ggup/((0.667274)*gglo)
      ans1=aij
      vtherm=max(vturb*1.e+5,1.3e+6/sqrt(a/t))
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
      sigvtherm=sigma
      ener=12398.54/abs(elin)
      nb1=nbinc(ener,epi)
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
c      if (lpri.ne.0)
c     $ write (lun11,*)'ltyp=34',idest1,idest2,elin,flin,ggup,gglo,
c     $                         a,aij,hij,pi
      go to 9000
c
 35   continue
      lprisv=lpri
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=15:',lcon
      ett=rdat(1)
      if (ett.le.(1.e-24)) go to 9000
      ntmp=(nrdat-1)/2
      do ml2=1,ntmp
        etmpp(ml2)=rdat(1+2*ml2)
        stmpp(ml2)=rdat(2*ml2)
        enddo
      nb1=nbinc(ett,epi)
      gglo=rlev(2,1)
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) stop 'ggup error'
      swrat=gglo/ggup
      numcon2=max(2,ncn/50)
      nphint=ncn-numcon2
      idest1=idat(6)
      idest2=idat(5)-idat(7)
      ekt=t*(0.861707)
      jlo=0
      ll=nb1
 1914     continue
          epii=epi(ll)
          efnd=(epii-ett)/13.598
          call hunt(etmpp,ntmp,efnd,jlo,0,lun11)
          ml2=jlo
          mlp=ml2+1
          del1=(efnd-etmpp(ml2))/(etmpp(mlp)-etmpp(ml2))
          del2=(efnd-etmpp(mlp))/(etmpp(mlp)-etmpp(ml2))
          sg(ll)=-stmpp(ml2)*del2+stmpp(mlp)*del1
c          if (lpri.gt.1)
c     $    write (lun11,*)ll,epii,sg(ll),ml2,stmpp(ml2),stmpp(mlp),
c     $              del1,del2
          call enxt(ett,nb1,lpri,epi,t,lfast,lun11,
     $                  ll,nskp,nphint,lrcalc)
          ll=ll+nskp
          if (ll.le.nphint) go to 1914
      lprib=0
      if (lpri.gt.1) lprib=lpri           
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,lfast,lun11)
      if (lpri.gt.1) then
        npr=nbinc(ett,epi)+2
        write (lun11,*)'bkh threshold xsection:',
     $         npr,ett,sg(npr)
        endif
      lpri=lprisv
      go to 9000

c
 36   continue
      lprisv=lpri
c      lpri=2
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=36:',(idat(mm),mm=1,5)
      idest1=idat(nidat-1)
      ett=rlev(1,nlevp)-rlev(1,idest1)
      idest2=nlevp
      if (ett.le.1.e-5) go to 9000
      eth=ett
      nb1=nbinc(eth,epi)
      gglo=rlev(2,idest1)
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) stop 'ggup error'
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
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nilin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      nistage=idat2(1)
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nelin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      nzel=idat2(1)
      nq=idat(1)
      nq=min(10,nq)
      zz=float(nzel-nistage+1)
      sgth=(6.3e-18)*nq*nq/zz/zz
      if (lpri.gt.1) write (lun11,*)nb1,nq,nzel,nistage,zz,
     $                              ett,sgth,idest1,gglo,ggup
      ll=nb1
 1916   continue
        epii=epi(ll)
        sg(ll)=sgth*(epii/ett)**(-3)
        call enxt(ett,nb1,lpri,epi,t,lfast,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        if (ll.le.nphint) go to 1916
      lprib=0
      if (lpri.gt.1) lprib=lpri
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,lfast,lun11)
      lpri=lprisv
      go to 9000
c
 37   continue
      go to 9000
c
 38   continue
      go to 9000
c
 39   continue
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
c     op pi xsections
c     old version
      lprisv=lpri
c      if (lpri.ge.1) lpri=2
      idest1=idat(nidat-1)
      idest4=idat(nidat-2)
      idest2=nlevp+idat(nidat-3)-1
      if (lpri.gt.2) write (lun11,*)'idest1=',idest1,idest2
      if ((idest1.ge.nlevp).or.(idest1.le.0)) go to 9000
      if (ml.le.0) go to 9000
      eth=rlev(4,idest1)-rlev(1,idest1)
      ett=eth
      nilin=npar(ml)
      if (lpri.gt.2) write (lun11,*)'nilin=',nilin,ml
      if (nilin.le.0) go to 9000
      ntmp=nrdat/2
      do ml2=1,ntmp+1
        etmpp(ml2)=rdat(2*ml2-1)
        stmpp(ml2)=rdat(2*ml2)*1.e-18
        stmpp(ml2)=max(stmpp(ml2),0.)
        enddo
      etst=etmpp(ntmp)*13.598+ett
      if (etst.gt.0.) then
        eeemlt=1.2
        eeemlt3=eeemlt**3
        if (lpri.gt.2) write (lun11,*)'etst=',etst,idest1
        if ((etst.lt.1.5e+4).and.(idest1.le.3)) then
          ml3=ntmp
          do while (etst.lt.1.5e+4)
            ml3=ml3+1
            etst=eeemlt*etst
            stmpp(ml3)=stmpp(ml3-1)/eeemlt3
            einv=(etst-ett)/13.598
            etmpp(ml3)=einv
            stmpe(ml3)=stmpp(ml3)*(etmpp(ml3)*13.598+ett)
            if (lpri.gt.2) write (lun11,*)ml3,etst,stmpp(ml3),einv
            enddo
          ntmp=ml3
          endif
        endif
c      ett=ett+max(0.,13.598*etmpp(1))
      if (lpri.gt.2) write (lun11,*)'ett=',ett,etmpp(1)
      if (ett.le.0.) go to 9000
      nb1=nbinc(ett,epi)
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
        if (lpri.gt.2) write (lun11,*)'type 49 scaling:',
     $    ml,vsav(2,ml),r19,tq,xnx,vsav(3,ml),xkt,xkto,tst,
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml),ett,t
        go to 9000
        endif
      vsav(2,ml)=r19
      vsav(1,ml)=bremsint(nb1)
      vsav(3,ml)=xnx
      vsav(4,ml)=xkt
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nilin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      emax=etmpp(ntmp)*13.6+eth
      gglo=rlev(2,idest1)
      ggup=rlev(2,nlevp)
      idest3=idat(nidat)
      idest4=idest3+1
      if (idest2.gt.nlevp) then
        jkk3=jkion+1
        if (lpri.gt.2)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        ndtmp=npfi(13,jkk3)
        if (lpri.gt.2)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        mllz=npar(ndtmp)
 4923   continue
           call dread(ltyp2,lrtyp2,lcon2,
     $          nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,ndtmp-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
           iltmp=idat2(nidt2-1)
           if (lpri.gt.2) write (lun11,*)nidt2,iltmp,ndtmp
           ndtmp=npnxt(ndtmp)           
           if ((ndtmp.ne.0).and.(iltmp.ne.(idest2-nlevp+1))
     $      .and.(npar(ndtmp).eq.mllz)) go to 4923
         ggup=rdat2(2)
         if (lpri.gt.2)
     $    write (lun11,*) ndtmp,iltmp,idest2,ggup
         endif
      if (lpri.gt.2) write (lun11,*)'before phint53'
      if (ggup.le.1.e-24) stop 'ggup error'
      swrat=gglo/ggup
      if (lpri.gt.2) then
        write (lun11,*)'type 49 data:',idat(1),idat(nidat),t,xnx,
     $           eth,gglo,ggup,swrat
        call dprinto(ndesc,nrdesc,lcon,
     $          nrdat,rdat,nidat,idat,nkdat,kdat,lun11) 
        endif
      lprib=lpri
      lprib=0
      crit53=0.01
      call phint53(stmpp,etmpp,ntmp,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,crit53,
     $    lfast,lun11)
c      call milne(t*1.e+4,ntmp,etmpp,stmpp,ett/13.598,alphamilne,
c     $   lun11,lpri)
c      alphamilne=alphamilne*1.e+18*xnx*swrat
c      if (lpri.ne.0) write (lun11,*)'milne rate=',alphamilne
      if (lpri.gt.2) then
        npr=nb1
        write (lun11,*)'bautista threshold xsection:',
     $         npr,ett,eth,rdat(1),sg(npr),ans2,swrat
        endif
      rates(1,ml)=ans1
      rates(2,ml)=ans2
c      rates(2,ml)=alphamilne
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      if (lpri.gt.2) write (lun11,*)'rates:',
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml)
      lpri=lprisv
      go to 9000
c
 50   continue
c     op line rad. rates 
      idest1=idat(1)
      idest2=idat(2)
c     nb check this out:  no bound-bound decays from continuum
      if ((idest1.le.0).or.(idest1.ge.nlev)
     $  .or.(idest2.le.0).or.(idest2.ge.nlev))
     $      go to 9000
c      gflin=rdat(2)
      aij=rdat(3)
c      aij=min(aij,1.e+10)
      eeup=rlev(1,idest1)
      eelo=rlev(1,idest2)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         endif
      elin=abs(rdat(1))
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      nelin=npar(nilin)
      if ((nilin.le.0).or.(nelin.le.0)) go to 9000
      flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nelin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      a=rdat2(2)
      vtherm=max(vturb*1.e+5,1.3e+6/sqrt(a/t))
      ener=12398.54/elin
      dele=ener*vtherm/3.e+10
      elammu=elin*1.e-4     
      ans1=aij
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
c      sigvtherm=(0.02655)*flin*elin*(1.e-8)/3.e+10
      sigvtherm=sigma
      jkkl=nplini(ml)                  
      if (jkkl.le.0) go to 9000
      ml3=nplin(jkkl)
      if (ml3.le.0) go to 9000
      call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,ml3-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
      elin=rdat(1)
      ener=12398.54/abs(elin)
      nb1=nbinc(ener,epi)
      ans2=0.
c      ans2=sigvtherm*bremsa(nb1)*vtherm/3.e+10
      if ((elin.gt.0.99e+9).or.(nrdesc.eq.9)) then 
         ans2=0.
         sigvtherm=0.
         endif
      ans1=ans1+ans2*ggup/(1.e-36+gglo)
      opakab=sigvtherm
      ans3=ans2*ener*ergsev
      ans4=ans1*ener*ergsev
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=50:',
     $  ml,nilin,nelin,elin,flin,rdat(3),gglo,ggup,a,vtherm,ans2,
     $  idest1,idest2,idest3,idest4,nlev,sigvtherm,bremsa(nb1),nb1
      if ((nrdesc.ne.9).or.(lfast.gt.2)) go to 9000
c
c       special for 2 photon
        ansar2=0.
        em2ph=aij
        lskp=1
        emax=ener
        nbmx=nbinc(emax,epi)
        if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=50:',
     $  ml,nilin,nelin,elin,flin,rdat(1),gglo,ggup,a,vtherm,ans2
     $   ,nbmx
        rcemsum=0.       
        lfastl=0        
        ll=2
 2298     continue
          ansar2o=ansar2
          ansar2=epi(ll)*epi(ll)*amax1(0.,(epi(nbmx)-epi(ll)))
          rcemsum=rcemsum+(ansar2+ansar2o)
     $                   *(epi(ll)-epi(ll-lskp))/2.
          call enxt(epi(1),nb1,0,epi,t,lfastl,lun11,
     $                  ll,lskp,nphint,lrcalc)
          ll=ll+lskp
          if (ll.le.nbmx) go to 2298
        rctmp1=0.
        rctmp2=0.
        ll=2
 2297     continue
          ansar2=epi(ll)*epi(ll)*amax1(0.,(epi(nbmx)-epi(ll)))
          ansar2=ansar2*em2ph*emax/(1.e-24+rcemsum)
          rctmp1o=rctmp1
          rctmp1=abund2*ansar2*ptmp1
          rctmp2o=rctmp2
          rctmp2=abund2*ansar2*ptmp2
          rccemis(1,ll)=rccemis(1,ll)+rctmp1
          rccemis(2,ll)=rccemis(2,ll)+rctmp2
c          if (lpri.gt.2) write (6,*)'2 photon:',ll,epi(ll),ansar2
          call enxt(epi(1),nb1,0,epi,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
          ll=ll+nskp
          if (ll.le.nbmx) go to 2297
      go to 9000
c
 51   continue
c     line rates, col, burgess and tully from manuel
      idest1=idat(3)
      idest2=idat(2)
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
      eijry=rdat(1)
      eij=eijry*13.598
      elin=12398.54/eij
      hij=elin*1.e-8
c      if (lpri.ne.0)
c     $ write (lun11,*)'type 51 data:',elin
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=12398.54/elin/ekt
      if (lpri.gt.1)
     $ write (lun11,*)elin,ekt,delt
c      if (delt.gt.50.) go to 9000            
      c=rdat(2)
      p1=rdat(3)
      p2=rdat(4)
      p3=rdat(5)
      p4=rdat(6)
      p5=rdat(7)
      tk=t*1.e+4
c      tk=max(tk,(1.e+4)*12398.54/elin/(0.861707)/50.)
      tk=max(tk,2.8777e+6/elin)
      ik=idat(1)
      cijpp=upsil(ik,eijry,c,p1,p2,p3,p4,p5,tk)
      ekt=0.861707*t
      delt=12398.54/elin/ekt
      cji=(8.626e-8)*cijpp/tsq/ggup
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
      go to 59
c
 53   continue
c     op pi xsections
c     old version
      lprisv=lpri
c      if (lpri.ge.1) lpri=2
      idest1=idat(nidat-1)
      idest2=nlevp+idat(nidat-3)-1
      if (lpri.gt.2) write (lun11,*)'idest1=',idest1,idest2,nlevp,ml
      if ((idest1.ge.nlevp).or.(idest1.le.0)) go to 9000
      if (ml.le.0) go to 9000
      eth=rlev(4,idest1)-rlev(1,idest1)
      eexc=rlev(1,idest1)
      ett=eth
      nilin=npar(ml)
      if (lpri.gt.2) write (lun11,*)'nilin=',nilin,ml
      if (nilin.le.0) go to 9000
      ntmp=nrdat/2
c      ett=ett+max(0.,13.598*etmpp(1))
      if (lpri.gt.2) write (lun11,*)'ett=',ett,etmpp(1)
      if (ett.le.0.) go to 9000
      nb1=nbinc(ett,epi)
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
        if (lpri.gt.2) write (lun11,*)'type 53 scaling:',
     $    ml,vsav(2,ml),r19,tq,xnx,vsav(3,ml),xkt,xkto,tst,
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml),ett,t
        go to 9000
        endif
      vsav(2,ml)=r19
      vsav(1,ml)=bremsint(nb1)
      vsav(3,ml)=xnx
      vsav(4,ml)=xkt
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nilin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      emax=etmpp(ntmp)*13.6+eth
      gglo=rlev(2,idest1)
      ggup=rlev(2,nlevp)
      idest3=idat(nidat)
      idest4=idest3+1
      if (idest2.gt.nlevp) then
        jkk3=jkion+1
        if (lpri.gt.2)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        ndtmp=npfi(13,jkk3)
        if (lpri.gt.2)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        if (ndtmp.le.0) go to 9000
        mllz=npar(ndtmp)
        iltmp=0
        do while ((ndtmp.ne.0).and.(iltmp.ne.(idest2-nlevp+1))
     $      .and.(npar(ndtmp).eq.mllz)) 
           call dread(ltyp2,lrtyp2,lcon2,
     $          nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,ndtmp-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
           iltmp=idat2(nidt2-1)
           if (lpri.gt.2) write (lun11,*)nidt2,iltmp,ndtmp
           ndtmp=npnxt(ndtmp)     
           if (ndtmp.le.0) go to 9000                 
           enddo
c        NB fix to excited level PI and rec
         ett=ett+rdat2(1)
         eth=ett
         ggup=rdat2(2)
         if (lpri.gt.2)
     $    write (lun11,*) ndtmp,iltmp,idest2,ggup,ett
         endif
      sscal=1.
      do ml2=1,ntmp+1
        etmpp(ml2)=rdat(2*ml2-1)
        stmpp(ml2)=rdat(2*ml2)*1.e-18*sscal
        stmpp(ml2)=max(stmpp(ml2),0.)
        stmpe(ml2)=stmpp(ml2)*(etmpp(ml2)*13.598+ett)
        enddo
c      go to 9093
      etst=etmpp(ntmp)*13.598+ett
      etst=-1.
      if (etst.gt.0.) then
        eeemlt=1.2
        eeemlt3=eeemlt**3
        if (lpri.ge.3) write (lun11,*)'etst=',etst,idest1
        if ((etst.lt.1.5e+4).and.(idest1.le.3)) then
          ml3=ntmp
          do while (etst.lt.1.5e+4)
            ml3=ml3+1
            etst=eeemlt*etst
            stmpp(ml3)=stmpp(ml3-1)/eeemlt3
            einv=(etst-ett)/13.598
            etmpp(ml3)=einv
            stmpe(ml3)=stmpp(ml3)*(etmpp(ml3)*13.598+ett)
            if (lpri.ge.3) write (lun11,*)ml3,etst,stmpp(ml3),einv
            enddo
          ntmp=ml3
          endif
        endif
 9093 continue
      if (lpri.gt.2) write (lun11,*)'before phint53',eexc,eth,lfast
      if (ggup.le.1.e-24) stop 'ggup error'
      swrat=gglo/ggup
      if (lpri.gt.2) then
        write (lun11,*)'type 53 data:',idat(1),idat(nidat),t,xnx,
     $           eth,gglo,ggup,swrat
        call dprinto(ndesc,nrdesc,lcon,
     $          nrdat,rdat,nidat,idat,nkdat,kdat,lun11) 
        endif
      lprib=lpri
      lprib=0
      crit53=0.01
      if ((eexc/eth.lt.0.05).or.(lfast.le.2)) then
        call phint53(stmpp,etmpp,ntmp,ett,ans1,ans2,ans3,ans4,
     $   abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $   opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,crit53,
     $    lfast,lun11)
        endif
      if (lfast.gt.2) then
        call milne(t*1.e+4,ntmp,etmpp,stmpp,ett/13.598,alphamilne,
     $   lun11,lpri)
        alphamilne=alphamilne*1.e+18*xnx*swrat
        call milne(t*1.e+4,ntmp,etmpp,stmpe,ett/13.598,ccmilne,
     $   lun11,lpri)
        ans2=alphamilne
        ccmilne=ccmilne*1.e+18*xnx*swrat*(1.602197e-12)
        ans4=ccmilne
c        if (lpri.ne.0) write (lun11,*)'milne rate=',alphamilne
        endif
      if (lpri.gt.2) then
        npr=nb1
        write (lun11,*)'bautista threshold xsection:',
     $         npr,ett,eth,rdat(1),sg(npr),ans2,swrat
        endif
      rates(1,ml)=ans1
      rates(2,ml)=ans2
c      rates(2,ml)=alphamilne
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      if (lpri.gt.2) write (lun11,*)'rates:',
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml)
      lpri=lprisv
      go to 9000
c
 54   continue
      idest1=idat(nidat-3)
      idest2=idat(nidat-2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      ans3=0.
      ans4=0.
      lprisv=lpri
c      if (lpri.ge.1) lpri=2
      if (lpri.gt.1) write (lun11,*)'type 54 data:',
     $  idat(nidat-3),idat(nidat-2)
      if (rlev(1,idest2).lt.rlev(1,idest1)) then
        itmp=idest2
        idest2=idest1
        idest1=itmp
        endif
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      elin=12398.54/abs(eeup-eelo+1.e-24)
      hij=elin*1.e-8
      ekt=0.861707*t
      delt=12398.54/elin/ekt
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
      iq=idat(nidat-1)
      if (lpri.gt.1)
     $ write (lun11,*)'before anl1:',ni,nf,li,lf,iq,idest1,idest2,
     $  eelo,eeup,idat(nidat-3),idat(nidat-2)
      call anl1(ni,nf,lf,iq,alm,alp,lpri,lun11)
      ans1=alp
      if (li.lt.lf) ans1=alm
      lpri=lprisv
      go to 9000
c
 55   continue
      lprisv=lpri
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=55:',(idat(mm),mm=1,5)
      idest1=idat(nidat-1)
      ett=rlev(1,nlevp)-rlev(1,idest1)
      idest2=nlevp
      if (ett.le.1.e-5) go to 9000
      eth=ett
      nb1=nbinc(eth,epi)
      gglo=rlev(2,idest1)
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) stop 'ggup error'
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
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nilin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      nistage=idat2(1)
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nelin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      nzel=idat2(1)
      zz=float(nzel-nistage)
      sgth=(6.3e-18)/zz/zz
      ll=nb1
 1016   continue
        epii=epi(ll)
        sg(ll)=sgth*(epii/ett)**(-3)
        call enxt(ett,nb1,lpri,epi,t,lfast,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        if (ll.le.nphint) go to 1016
      lprib=0
      if (lpri.gt.1) lprib=lpri
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,lfast,lun11)
      lpri=lprisv
      go to 9000
c
 56   continue
      idest1=idat(1)
      idest2=idat(2)  
c      go to 9000
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat(2)).lt.rlev(1,idat(1))) then
        idest2=idat(1)
        idest1=idat(2)
        endif
      lprisv=lpri
c      lpri=2
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      dele=abs(eeup-eelo)
      if (dele.le.1.e-16) go to 9000
      ntmp=nrdat/2
      do kl=1,ntmp
        ttmp(kl)=rdat(kl)
        enddo
      tfnd=alog10(t*1.e+4)
      jlo=0
      call hunt(ttmp,ntmp,tfnd,jlo,0,lun11)
      jlo=min(jlo,ntmp-1)
      nind=ntmp+jlo
      if (lpri.gt.1) write (lun11,*)'type 56:',
     $  idest1,idest2,ggup,gglo,dele,jlo,nind,
     $  rdat(nind),rdat(nind+1),tfnd,ttmp(jlo+1),ttmp(jlo)
      cijpp=(rdat(nind+1)-max(1.e-36,rdat(nind)))
     $   *(tfnd-ttmp(jlo))/(ttmp(jlo+1)-ttmp(jlo)+1.e-24)
     $     +max(1.e-36,rdat(nind))
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
      lpri=0
      tz=t*1.e+4
      idest1=idat(nidat-1)
      idest2=nlevp
c      write (lun11,*)'in ucalc at 57:',idest1,idat(1),rdat(1)
      if ((idat(1).le.0).or.(idest1.le.1).or.(idest1.gt.nlevp)) 
     $        go to 9000
      d57=rdat(1)
      i57=idat(1)
      eth=max(0.,rlev(1,nlevp)-rlev(1,idest1))
      ekt=0.861707*t
c      tz=max(tz,(1.e+4)*eth/(0.861707)/50.)
c      tz=max(tz,2.320975e+02*eth)
      e1=rlev(1,idest1)
      ep=rlev(4,idest1)
      if (ep.le.0.) go to 9000
      call calt57(tz,xnx,e1,ep,i57,cion,crec,lun11,lpri)  
      if (lpri.ge.1)
     $ write (lun11,*)'ltype=57:',cion,crec,gglo,ggup,nlevp,idest1,rinf,
     $  eth,ekt,ans1,ans2
      ans1=cion*xnx
      ggup=rlev(2,nlevp)
      gglo=rlev(2,idest1) 
c     note that rinf has exponential removed
      rinf=gglo/(1.e-36+ggup)
      ans2=crec*rinf*xnx*xnx
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
      if (lpril.gt.1) write (lun11,*)(rdat(jj),jj=1,nrdat)
      if (lpril.gt.1) write (lun11,*)(idat(jj),jj=1,nidat)
      if (lpril.gt.1) write (lun11,*)(kdat(jj),jj=1,nkdat)
      if (ml.le.0) go to 9000
      lfastl=lfast
      nilin=npar(ml)
      idest3=idat(nidat)
      idest4=idat(nidat-2)
      idest1=idat(nidat-1)
      idest2=idat(nidat-3)-1
      idest2=max(idest2,1)
      if ((nilin.le.0).or.(nilin.gt.np2)) go to 9000
      call dread(ltyp2,lrtyp2,lcon2,
     $          nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,nilin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      if (lpril.gt.1)
     $ write (lun11,*)ml,nilin,rdat(1),idest1,rdat2(1),nlevp
      ett=rdat2(1)
      if ((idest1.gt.nlevp).or.(idest1.le.0)) go to 9000
      if (ml.le.0) go to 9000
      if (ett.le.0.) go to 9000
      nb1=nbinc(ett,epi)
      numcon2=max(2,ncn/50)
      nphint=ncn-numcon2
      nphint=max(nphint,nb1+1)
      if (nb1.ge.nphint-1) go to 9000
      ett=rdat(1)
      nb1=nbinc(ett,epi)
      if (nb1.ge.(ncn-1)) go to 9000
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
      if (lpril.gt.1) write (lun11,*)nlevp,ggup
      if (ggup.le.1.e-24) stop 'ggup error'
      swrat=gglo/ggup
      e0=rdat(2)
      ett=rdat(1)
      nb1=nbinc(ett,epi)       
      if (lpril.gt.1)
     $ write (lun11,*)'ett=',ett,nb1,nphint
      if (nb1.ge.(nphint-1)) go to 9000
c      if ((bremsint(nb1).lt.1.e-20).and.(lpril.gt.1)) 
c     $    write (lun11,*)'skipping 59',
c     $         nb1,bremsint(nb1)
      if (bremsint(nb1).lt.1.e-20) go to 9000
      s0=rdat(3)
      ya=rdat(4)
      pp=rdat(5)
      yw=rdat(6)
      ywsq=yw*yw
      l2=idat(3)
      qq=5.5+l2-pp/2.
      if (lpril.gt.1) write (lun11,*)'qq=',
     $   l2,qq,ya,ywsq,pp,yw,s0
      ll=nb1
 1086   continue
        epii=epi(ll)
        yy=epii/e0
        ff=((yy-1.)*(yy-1.)+ywsq)*(yy**(-qq))
     $      *(1.+sqrt(yy/ya))**(-pp)
        sg(ll)=s0*ff*(1.e-18)
        if (lpril.gt.1) write (lun11,*)ll,epii,sg(ll),
     $    yy,qq,ff
        call enxt(ett,nb1,0,epi,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        if (ll.le.nphint) go to 1086
      ekt=t*(0.861707)
      lprib=0
c      if (lpril.gt.1) lprib=lpril
      gglo=rlev(2,1)
      ggup=rlev(2,nlevp)
      if (ggup.le.1.e-24) stop 'ggup error'
      swrat=gglo/ggup
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccems2,lprib,epi,bremsa,t,trad,swrat,xnx,lfast,lun11)
      if (lpril.gt.1) then
        npr=nb1
        write (lun11,*)'verner threshold xsection:',
     $         npr,ett,sg(npr),opakab
        endif
      ans4=0.
      ans2=0.
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
      idest1=idat(1)
      idest2=idat(2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat(2)).lt.rlev(1,idat(1))) then
        idest2=idat(1)
        idest1=idat(2)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      dele=abs(rlev(1,idest2)-rlev(1,idest1))
      if (dele.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=dele/ekt
      temp=t*1.e+4
      temp=max(temp,0.02*dele*1.e+4/(0.861707))
      call calt60_62(temp,nrdat,ndesc,rdat,idat,cijpp)
c      cijpp=cijpp/2./2.
      cji=(8.626e-8)*cijpp/tsq/(1.e-16+ggup)
      exptmp=expo(-delt)
      cij=cji*ggup*exptmp/(1.e-16+gglo)
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=60',idest1,idest2,temp,flin,ggup,gglo
        write (lun11,*)'       ',nrdat,(rdat(mm),mm=1,8),jlo
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
      idest1=idat(nidat-3)
      idest2=idat(nidat-2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      elin=12398.54/abs(eeup-eelo+1.e-24)
      hij=elin*1.e-8
      ekt=0.861707*t
      delt=12398.54/elin/ekt
      if (delt.gt.50.) go to 9000
      ni=ilev(1,idest1)
      li=ilev(3,idest1)
      nf=ilev(1,idest2)
      lf=ilev(3,idest2)
      sum=0.
      iq=idat(nidat-1)
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
            call anl1(ni,nn,lii-1,iq,alm,alp,lpri,lun11)
c              write (lun11,*)'li=1',ni,nn,lii-1,iq,alp
            sum=sum+alp
           endif
           if (nn.gt.lii+1) then
            if (lpril.ne.0)
     $        write (lun11,*)'nn=li+1',ni,nn,lii+1,iq
            call anl1(ni,nn,lii+1,iq,alm,alp,lpri,lun11)
            sum=sum+alm
           endif
          enddo
          if (lpril.ne.0)
     $     write (lun11,*)'after anl1',sum
          ecm=abs(rlev(1,idest1)-rlev(1,idest2))*8059.9
          ecm=0.
          nnz=idat(4)
          tbig=t*1.e+4
          z1=1.
          rm=1800.
          il=0
          psi=0.75/nnz/nnz*lii/(2*lii+1)*ni*ni*(ni*ni-lii*lii)
          if (lpril.ne.0)
     $     write (lun11,*)'before amcrs',ecm,ni,lii,sum
          call amcrs(ni,lii,tbig,nnz,z1,rm,xnx,sum,ecm,psi,il,cn,lun11)
          cno=cn
          iz=idat(4)
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
c        go to 9097
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
          nnz=idat(4)
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
 9097   continue
      endif
      ans1=ans1*xnx
      ans2=ans2*xnx
      go to 9000
c
 64   continue
c     hydrogenic pi xsections, bautista format 
      lprisv=lpri
      idest1=idat(nidat-1)
      ett=abs(rlev(1,nlevp)-rlev(1,idest1))
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=64:',(rdat(mm),mm=1,5)
      if (ett.le.1.e-5) go to 9000
      zzz=float(idat(3))
      enn=float(idat(1))
      eth=ett
      nb1=nbinc(eth,epi)
      gglo=rlev(2,idest1)
      swrat=gglo
      idest2=nlevp
      ekt=t*(0.861707)
      ll=nb1
      lorb=idat(2)
      ic=idat(3)
      nq=idat(1)
      mm=0
 1919 continue
        mm=mm+1
        epii=epi(ll)
        e=epii
        eth=ett
        erel=max(0.,(e-eth)/13.598)
        call hphotx(erel,ic,nq,xsec,lun11,lpri)
        sg(ll)=xsec(lorb+1)*(1.e-18)
        stmpp(mm)=xsec(lorb+1)
        etmpp(mm)=erel
        call enxt(ett,nb1,lpri,epi,t,lfast,lun11,
     $                  ll,nskp,nphint,lrcalc)
        ll=ll+nskp
        if (ll.le.nphint) go to 1919
      lprib=0
      if (lpri.gt.1) lprib=lpri           
      call phintfo(sg,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,lfast,lun11)
      ntmp=ll-nb1
      if (lfast.ge.4) then
        temp=t*1.e+4
        ntmp=mm
        call milne(temp,ntmp,etmpp,stmpp,eth/13.6,ans2,lun11,lprim)
c      write (lun11,*)'ans2=',ans2,xnx,swrat
        ans2=ans2*swrat
        endif
      lpri=lprisv
      go to 9000
c
c
 65   continue
c     effective charge to be used in coll. ion. 
      tz=t*1.e+4
      idest1=idat(nidat-1)
      idest2=nlevp
      ggup=rlev(2,nlevp)
      gglo=rlev(2,1) 
      eth=max(0.,rlev(1,nlevp)-rlev(1,idest1))
      ekt=0.861707*t
c      if (eth/ekt.gt.50.) go to 9000      
      call szirco(idat(1),tz,rdat(1),cii,lun11)
      ans1=cii*xnx
c     note that rinf has exponential removed
      rinf=(2.08e-22)*gglo/ggup/t/tsq
      ans2=ans1*rinf*expo(eth/ekt)
      go to 9000
c
 66   continue
c     Like type 69 but, data in fines tructure
      idest1=idat(1)
      idest2=idat(2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat(2)).lt.rlev(1,idat(1))) then
        idest2=idat(1)
        idest1=idat(2)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      elin=rdat(1)
      if (elin.le.1.e-24) go to 9000
      elin=12398.54/elin
      ekt=0.861707*t
      delt=12398.54/elin/ekt
c      if (delt.gt.50.) go to 9000
      hij=elin*1.e-8
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/elin)
      call calt66(temp,rdat,gamma)
      cijpp=gamma
      cji=(8.626e-8)*cijpp/tsq/ggup
        exptmp=expo(-delt)
        cij=cji*ggup*exptmp/gglo
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=69',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdat,(rdat(mm),mm=1,8),nind,jlo
        write (lun11,*)'       ',cij,cji,xnx,cijpp,exptmp
        endif
      elin=0.      
      go to 9000
c
 67   continue
c     Effective collision strengths from Keenan et al.
      idest1=idat(1)
      idest2=idat(2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat(2)).lt.rlev(1,idat(1))) then
        idest2=idat(1)
        idest1=idat(2)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      elin=rdat(1)
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=12398.54/elin/ekt
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/elin)
      call calt67(temp,rdat,gamma)
      cijpp=gamma
      cijpp=max(0.,cijpp)
      cji=(8.626e-8)*cijpp/tsq/ggup
        exptmp=expo(-delt)
        cij=cji*ggup*exptmp/gglo
      ans1=cij*xnx
      ans2=cji*xnx
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=69',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdat,(rdat(mm),mm=1,8),nind,jlo
        write (lun11,*)'       ',cij,cji,xnx,cijpp,exptmp
        endif
      elin=0.      
      go to 9000
c
 68   continue
c     coll. strength He-like ions by Zhang & Sampason 
      idest1=idat(1)
      idest2=idat(2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat(2)).lt.rlev(1,idat(1))) then
        idest2=idat(1)
        idest1=idat(2)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      elin=12398.54/abs(eeup-eelo+1.e-24)
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=12398.54/elin/ekt
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/elin)
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=68',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdat,(rdat(mm),mm=1,8),nind,jlo
        endif
      call calt68(temp,rdat,idat,gamma)
      cijpp=gamma
      cijpp=max(cijpp,0.)
      cji=(8.626e-8)*cijpp/tsq/ggup
      ekt=0.861707*t
      delt=12398.54/elin/ekt
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
      idest1=idat(1)
      idest2=idat(2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat(2)).lt.rlev(1,idat(1))) then
        idest2=idat(1)
        idest1=idat(2)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      elin=12398.54/abs(eeup-eelo+1.e-24)
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=12398.54/elin/ekt
      m=nrdat
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/elin)
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=69',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdat,(rdat(mm),mm=1,8),nind,jlo
        endif
      call calt69(temp,m,rdat,gamma)
      cijpp=gamma
      cijpp=max(cijpp,0.)
      cji=(8.626e-8)*cijpp/tsq/ggup
      ekt=0.861707*t
      delt=12398.54/elin/ekt
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
 70   continue 
c     Coefficients for phot x-section of suplevels  
c      lfastl=lfast
      lfastl=3
      temp=t*1.e+4
      den=xpx
      m=1000
      lpric=0
c      if (lpri.ge.1) lpric=2
      mlion=npar(ml)
      idest1=idat(nidat-1)
      idest1=min(idest1,nlev-1)
      idest2=nlev+idat(nidat-3)-1
      idest2=max(idest2,nlev)
      ggup=rlev(2,nlevp)
      ett=abs(rlev(1,idest1)-rlev(1,nlevp))
      if (idest2.gt.nlevp) then
        jkk3=jkion+1
        if (lpric.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        ndtmp=npfi(13,jkk3)
        if (lpric.gt.1)
     $    write (lun11,*)jkk3,ndtmp,nlevp,idest2
        mllz=npar(ndtmp)
 1513   continue
           call dread(ltyp2,lrtyp2,lcon2,
     $          nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,ndtmp-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
           iltmp=idat2(nidt2-1)
           if (lpric.gt.1) write (lun11,*)nidt2,iltmp,ndtmp
           ndtmp=npnxt(ndtmp)           
           if ((ndtmp.ne.0).and.(iltmp.ne.(idest2-nlevp+1))
     $      .and.(npar(ndtmp).eq.mllz)) go to 1513
         ggup=rdat2(2)
         ett=abs(rlev(1,idest1)+rdat2(1))
         endif
       if (lpric.ge.1)
     $    write (lun11,*) ndtmp,iltmp,idest2,ggup,ett
      xkt=ett/(0.861707*t)
      nb1=nbinc(ett,epi)
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
      call dread(ltyp2,lrtyp2,lcon2,
     $          nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,mlion-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      ist=idat2(1)
      ic=ist
      eth=ett
      ethi=eth
      gglo=rlev(2,idest1)
      if (ggup.le.1.e-24) stop 'ggup error'
      swrat=gglo/ggup
      if (lpric.ne.0) then
        write (lun11,*)'type 70 data:',idat(1),idat(nidat),t,xnx,
     $           eth,gglo,ggup,swrat
        call dprinto(ndesc,nrdesc,lcon,
     $          nrdat,rdat,nidat,idat,nkdat,kdat,lun11) 
        endif
      ettry=ett/13.6
      call calt70(temp,den,ettry,ic,m,rdat,idat,
     1             ntmp,etmpp,stmpp,rec,al,lun11,lpric)
      if (lpric.ne.0) write (lun11,*)'after  calt70:',rec,stmpp(1)
      crit53=0.01
      do mm=1,ntmp
        stmpp(mm)=stmpp(mm)*1.e-18
        stmpp(mm)=max(stmpp(mm),0.)
        enddo
      call phint53hunt(stmpp,etmpp,ntmp,ett,ans1,ans2d,ans3d,ans4s,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ dum1,dum1,lpric,epi,bremsa,t,trad,swrat,xnx,crit53,
     $   lfast,lun11)
      if (ans2d.le.1.e-36) then
        ans1=0.
        ans2=0.
        go to 9000
        endif
      scale=rec*xnx/ans2d
      ans1=ans1*scale
      ans2=rec*xnx*swrat
c      ans2=ans2d
      tm=t*1.e4
      q2=2.07e-16*xnx*(tm**(-1.5))
      rs=q2/swrat
      ans1o=ans1
c      ans1=min(ans1,ans2/rs)
c      write (lun11,*)'type 70 limit:',ans2,rs,swrat,
c     $   xnx,tm,q2,ans1o,ans1
c
c     testing superlevel phot.
c      ans1=0.
cc
c     testing superlevel phot.
c      if (idat(nidat).eq.18)
c     $ ans2=ans2*4.
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
     $ write (lun11,*)'before calt71:',rdat(1),rdat(2),rdat(3)
      call calt71(temp,den,ic,m,rdat,idat,wav,aij,lun11,lpril)
      idest1=idat(nidat-3)
      idest2=idat(nidat-2)
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
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nelin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      a=rdat2(2)
      elammu=elin*1.e-4     
      ans1=aij
c     special fudge for ca i and ca ii   
      if ((idat(6).eq.96).or.(idat(6).eq.97))
     $ ans1=min(ans1,1.e+10)
c
      vtherm=max(vturb*1.e+5,1.3e+6/sqrt(a/t))
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
      sigvtherm=sigma
      ener=12398.54/abs(elin)
      nb1=nbinc(ener,epi)
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
        dele=12398.54/(elin+1.e-24)
        ans4=ans1*dele*(1.602197e-12)
        endif
c      ans4=0.
      if (lpril.ne.0)
     $ write (lun11,*)' ',vtherm,ans2,ans4,flin
      go to 9000
c
 72   continue
c     Autoinization rates (in s^-1) for satellite lvls
      idest1=idat(nidat-3)
      idest2=idat(nidat-2)
      temp=t*1.e+4
      call calt72(temp,rdat,rate)  
      ans1=rate*xnx
      ans2=0.
      ggup=rlev(2,nlevp)
      gglo=rlev(2,1) 
c     note that rinf has exponential removed
      rinf=(2.08e-22)*gglo/ggup/t/tsq
      dele=rdat(2)
      ans2=rate*xnx*rinf*xnx*expo(dele/temp)
      go to 9000
c
 75   continue
c     Autoinization rates (in s^-1) for satellite lvls
c        now including final ion stage
      idest3=idat(nidat)
      idest4=idat(nidat-2)
      idest2=idat(nidat-1)+nlev-1
      idest1=idat(nidat-3)
      idest1=max(idest1,1)
      idest2=max(idest2,1)
      temp=t*1.e+4
      if (nrdat.lt.3) rdat(3)=1.
      call calt72(temp,rdat,rate)  
      ans1=rate*xnx
      ans2=0.
      go to 9000
c
 73   continue
c     Fit to coll. strengths satellite lvls Helike ion
      idest1=idat(1)
      idest2=idat(2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat(2)).lt.rlev(1,idat(1))) then
        idest2=idat(1)
        idest1=idat(2)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      elin=rdat(1)
      hij=elin*1.e-8
      ekt=0.861707*t
      delt=12398.54/elin/ekt
      if (elin.le.1.e-24) go to 9000
      m=1000
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/elin)
      crate=0.
      call calt73(temp,rdat,idat,crate)
c      write (lun11,*)'type 73 calc:',
c     $  (rdat(lk),lk=1,7),(idat(lk),lk=1,4),crate,
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
        write (lun11,*)'       ',nrdat,(rdat(mm),mm=1,8),nind,jlo
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
     $ (rdat(mm),mm=1,nrdat),(idat(mm),mm=1,nidat)
      call calt74(temp,ncn,epi,bremsa,nrdat,rdat,idat,rate,
     $       alpha,lpri,lun11) 
      idest1=idat(nidat-1)
      idest2=nlevp
      idest3=idat(nidat)
      idest4=idest3+1
      gglo=rlev(2,idest1)
      ggup=rlev(2,idest2)
      if (lpri.gt.1) write (lun11,*)'returning from calt74:',
     $  rate,alpha,idest1,idest2,gglo,ggup
      ans1=rate
      alpha=alpha*gglo/ggup
       if ((rate.lt.0.).and.(lpri.gt.1)) then
         write (lun11,*)'the spectrum:'
         do mm=1,ncn
           write (lun11,*)mm,epi(mm),bremsa(mm)
           enddo
         endif
      ans2=alpha
      lpri=lprisv
      go to 9000
c
 81   continue
c     bhatia Fe XIX
      idest1=idat(1)
      idest2=idat(2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      if (rlev(1,idat(2)).lt.rlev(1,idat(1))) then
        idest2=idat(1)
        idest1=idat(2)
        endif
      ggup=rlev(2,idest2)
      gglo=rlev(2,idest1)
      eeup=rlev(1,idest2)
      eelo=rlev(1,idest1)
      elin=12398.54/abs(eeup-eelo+1.e-24)
      hij=elin*1.e-8
      if (elin.le.1.e-24) go to 9000
      ekt=0.861707*t
      delt=12398.54/elin/ekt
      m=nrdat
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/elin)
      if (lpri.gt.1) then
        write (lun11,*)'ltyp=75',idest1,idest2,elin,flin,ggup,gglo
        write (lun11,*)'       ',nrdat,(rdat(mm),mm=1,8),nind,jlo
        endif
      cijpp=rdat(1)
      cijpp=max(cijpp,0.)
      cji=(8.626e-8)*cijpp/tsq/ggup
      ekt=0.861707*t
      delt=12398.54/elin/ekt
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
      idest1=idat(1)
      idest2=idat(2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      lpril=lpri
      aij=rdat(1)
      eeup=rlev(1,idest1)
      eelo=rlev(1,idest2)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         endif
      elin=12398.54/abs(eeup-eelo)
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      nelin=npar(nilin)
      if ((nilin.le.0).or.(nelin.le.0)) go to 9000
      flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
c      flin=rdat(2)
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nelin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      a=rdat2(2)
c      a=rdat(5)
      elammu=elin*1.e-4     
c      if (flin.le.1.e-10) flin=1.
      ans1=aij
      vtherm=max(vturb*1.e+5,1.3e+6/sqrt(a/t))
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
      sigvtherm=sigma
      ener=12398.54/abs(elin)
      nb1=nbinc(ener,epi)
      ans2=0.
      ans4=aij*ergsev*12398.54/abs(elin)
      if (lfast.gt.2) go to 9000
        ansar2=0.
        em2ph=aij
        lskp=1
        emax=12398.54/elin
        nbmx=nbinc(emax,epi)
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=76:',
     $  ml,nilin,nelin,elin,flin,rdat(1),gglo,ggup,a,vtherm,ans2
     $   ,nbmx
        lfastl=0
        rcemsum=0.       
        lfastl=0
        ll=1+lskp
 298      continue
          ansar2o=ansar2
          ansar2=epi(ll)*epi(ll)*amax1(0.,(epi(nbmx)-epi(ll)))
          rcemsum=rcemsum+(ansar2+ansar2o)
     $                   *(epi(ll)-epi(ll-lskp))/2.
          call enxt(epi(1),nb1,lpril,epi,t,lfastl,lun11,
     $                  ll,lskp,nphint,lrcalc)
          ll=ll+lskp
          if (ll.le.nbmx) go to 298
c        rcemsum=(emax**3)/12.
        rctmp1=0.
        rctmp2=0.
        ll=2
 297      continue
c          if (lpri.gt.2) write (6,*)'2 photon:',ll,epi(ll),ansar2
          ansar2=epi(ll)*epi(ll)*amax1(0.,(epi(nbmx)-epi(ll)))
          ansar2=ansar2*em2ph*emax/(1.e-24+rcemsum)
          rctmp1o=rctmp1
          rctmp1=abund2*ansar2*ptmp1
          rctmp2o=rctmp2
          rctmp2=abund2*ansar2*ptmp2
          rccemis(1,ll)=rccemis(1,ll)+rctmp1
          rccemis(2,ll)=rccemis(2,ll)+rctmp2
          call enxt(epi(1),nb1,lpril,epi,t,lfastl,lun11,
     $                  ll,nskp,nphint,lrcalc)
          ll=ll+nskp
          if (ll.le.nbmx) go to 297
        if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=76:',
     $  ml,nilin,nelin,elin,flin,rdat(3),gglo,ggup,a,vtherm,ans2
        ans4=aij*ergsev*12398.54/abs(elin)
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
      idest1=idat(nidat-3)
      idest2=idat(nidat-2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      eup=rlev(1,idest2)
      elo=rlev(1,idest1)
      wav=12398.54/(eup-elo+1.e-24)
      ekt=0.861707*t
      delt=wav/ekt
      lprit=0
      temp=t*1.e+4
c      temp=max(temp,(1.e+4)*12398.54/elin/(0.861707)/50.)
      temp=max(temp,2.8777e+6/wav)
      call calt77(lprit,lun11,temp,den,ic,m,rdat,idat,cul,clu)
      ans1=clu
      ans2=cul
      go to 9000
c
 78   continue

      go to 9000
c
 79   continue
c     fluorescence lines
      idest1=idat(1)
      idest2=idat(2)
      if ((idest1.le.0).or.(idest1.gt.nlev)
     $  .or.(idest2.le.0).or.(idest2.gt.nlev))
     $      go to 9000
      elin=rdat(1)
      flin=rdat(2)
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
      a=rdat(5)
      hij=elin*1.e-8
      elammu=elin*1.e-4     
      aij=(6.67e+7)*gglo*flin/ggup/elammu/elammu
c     this is a fudge to avoid badnumerics from fine structure.
      if (flin.le.1.01e-12) aij=1.e+5
      if (elin.ge.1.e+9) aij=1.e+5
      ans1=aij
      vtherm=max(vturb*1.e+5,1.3e+6/sqrt(a/t))
      sigma=(0.02655)*flin*elin*(1.e-8)/vtherm
      sigvtherm=sigma
      ener=12398.54/abs(elin)
      nb1=nbinc(ener,epi)
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
c      ans2=(0.02655)*flin*elin*(1.e-8)/vtherm
      go to 9000
c
 80   continue
c Collisional ionization rates gnd of Fe and Ni  
      go to 9000
c
 82   continue
c     Fe UTA rad rates
      idest1=idat(1)
      idest2=idat(2)
c     nb check this out:  no bound-bound decays from continuum
      if ((idest1.le.0).or.(idest1.ge.nlev)
     $  .or.(idest2.le.0).or.(idest2.ge.nlev))
     $      go to 9000
      gflin=rdat(3)
      aij=rdat(6)
      eeup=rlev(1,idest1)
      eelo=rlev(1,idest2)
      if (eeup.lt.eelo) then
         itmp=idest1
         idest1=idest2
         idest2=itmp
         endif
      elin=rdat(1)
      ggup=rlev(2,idest1)
      gglo=rlev(2,idest2)
      if (ml.le.0) go to 9000
      nilin=npar(ml)
      if (nilin.le.0) go to 9000
      nelin=npar(nilin)
      if ((nilin.le.0).or.(nelin.le.0)) go to 9000
c      flin=(1.e-16)*aij*ggup*elin*elin/((0.667274)*gglo)
      flin=gflin
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nelin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      a=rdat2(2)
      vtherm=max(vturb*1.e+5,1.3e+6/sqrt(a/t))
      ener=12398.54/elin
      dele=ener*vtherm/3.e+10
      delev=vtherm/(elin*(1.e-8))
      delea=rdat(6)
      elammu=elin*1.e-4     
      ans1=aij
      sigma=(0.02655)*flin/(delev)
      sigvtherm=(0.02655)*flin*elin*(1.e-8)/3.e+10
      sigvtherm=sigma
      jkkl=nplini(ml)                  
      if (jkkl.le.0) go to 9000
      ml3=nplin(jkkl)
      if (ml3.le.0) go to 9000
      call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,ml3-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
      elin=rdat(1)
      ener=12398.54/abs(elin)
      nb1=nbinc(ener,epi)
      ans2=0.
c      ans2=sigvtherm*bremsa(nb1)*vtherm/3.e+10
      if (elin.gt.0.99e+9) then 
         ans2=0.
         sigvtherm=0.
         endif
      ans1=ans1+ans2*ggup/(1.e-36+gglo)
      ans3=ans2*ener*ergsev
      ans4=ans1*ener*ergsev
      opakab=sigvtherm
      if (lpri.gt.1)
     $  write (lun11,*)'in ucalc, ind=82:',
     $  ml,nilin,nelin,elin,flin,rdat(3),gglo,ggup,a,vtherm,ans2,
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
c      if (lpri.ge.1) lpril=2
      if (lpril.gt.1) write (lun11,*)'ltyp=84',ml,npar(ml)
      if (lpril.gt.1) write (lun11,*)(rdat(jj),jj=1,nrdat)
      if (lpril.gt.1) write (lun11,*)(idat(jj),jj=1,nidat)
      if (lpril.gt.1) write (lun11,*)(kdat(jj),jj=1,nkdat)
      if (ml.le.0) go to 9000
      lfastl=lfast
      nilin=npar(ml)
      idest3=idat(nidat)
      idest4=idest3+1
      idest1=idat(nidat-1)
      idest2=1
      ntmp=(nrdat)/2-1
      ett2=rdat(1)
      ett=rdat(3)*13.598
      ediff=rdat(2*(ntmp))*13.598-ett2
      ediffry=ediff/13.598
      scal=rdat(2)
      do ml2=1,ntmp
        etmpp(ml2)=rdat(2*ml2+1)-rdat(3)
        stmpp(ml2)=rdat(2*ml2+2)*1.e-18*scal
        stmpp(ml2)=max(stmpp(ml2),0.)
        if (lpril.gt.1) write (lun11,*)ml2,etmpp(ml2),stmpp(ml2)
        enddo
      ett=ett2-(rdat(2*ntmp+1)-rdat(3))*13.6
      nb1=nbinc(ett,epi)
      numcon2=max(2,ncn/50)
c         numcon2=200
      nphint=ncn-numcon2
      nphint=max(nphint,nb1+1)
      if (lpril.gt.1) 
     $ write (lun11,*)'ltyp=84:',ett,ett2,ediff,scal,ntmp,
     $  etmpp(1),stmpp(1),etmpp(ntmp+1),stmpp(ntmp+1),nb1,nphint
      if (nb1.ge.nphint-1) go to 9000
      if (lpril.gt.1)
     $ write (lun11,*)ett,nb1,bremsint(nb1),ml,vsav(1,ml),tst,
     $  lforce       
c       lprib=0
       lprib=lpril
       call phint5384(stmpp,etmpp,ntmp,ett,ans1,ans2,ans3,ans4,
     $   abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $   opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,crit53,
     $    lfast,lun11)
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
      if (lpril.gt.1) write (lun11,*)(rdat(jj),jj=1,nrdat)
      if (lpril.gt.1) write (lun11,*)(idat(jj),jj=1,nidat)
      if (lpril.gt.1) write (lun11,*)(kdat(jj),jj=1,nkdat)
      if (ml.le.0) go to 9000
      lfastl=lfast
      nilin=npar(ml)
      idest3=idat(nidat)
      idest4=idest3+1
      idest1=idat(nidat-1)
      idest2=1
      ett2=rdat(2)*13.598
      nmin=idat(1)
      jkk=idest3
      zc=float(jkk-114)
      eion=rdat(2)
      kdim=ncn
      far=rdat(3)
      gam=rdat(4)
      scal=rdat(5)
      call pexs(nmin,kdim,zc,eion,far,gam,scal,
     +                etmp8,stmp8,ierr,lpril,lun11)
      do mm=1,ncn
        stmpp(mm)=stmp8(mm)*1.e-18
        enddo
      call phintfo(stmpp,ett2*0.8,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccems2,lpril,epi,bremsa,t,trad,swrat,xnx,lfast,lun11)
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
      ans1=rdat(2)
      ans2=0.
      idest1=idat(nidat-3)
      idest2=nlevp
      idest3=idat(nidat)
      idest4=idest3+1
      go to 9000
c
 87   continue
      go to 9000
c
 88   continue
c     op inner shell photoexcitation 
      lprisv=lpri
      idest1=idat(nidat-1)
c      idest2=idat(nidat-2)
      idest2=nlevp
      if (lpri.gt.1) write (lun11,*)'ltyp=88,idest1=',idest1,idest2
      if ((idest1.ge.nlevp).or.(idest1.le.0)) go to 9000
      if (ml.le.0) go to 9000
      eth=rlev(4,idest1)-rlev(1,idest1)
      ett=eth
      nilin=npar(ml)
      if (lpri.gt.1) write (lun11,*)'nilin=',nilin,ml
      if (nilin.le.0) go to 9000
      ntmp=nrdat/2
      do ml2=1,ntmp+1
        etmpp(ml2)=rdat(2*ml2-1)
        stmpp(ml2)=rdat(2*ml2)*1.e-18
        stmpp(ml2)=max(stmpp(ml2),0.)
        enddo
c      ett=ett+max(0.,13.598*etmpp(1))
      if (lpri.gt.1) write (lun11,*)'ett=',ett,etmpp(1)
      if (ett.le.0.) go to 9000
      nb1=nbinc(ett,epi)
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
      call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat2,nidt,idat2,nkdt,kdat2,nilin-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
      emax=etmpp(ntmp)*13.6+eth
      gglo=rlev(2,idest1)
      ggup=rlev(2,idest2)
      idest3=idat(nidat)
      idest4=idest3+1
      if (lpri.gt.1) write (lun11,*)'before phint53',gglo,ggup
      if (ggup.le.1.e-24) stop 'ggup error'
      swrat=gglo/ggup
      if (lpri.gt.1) then
        write (lun11,*)'type 88 data:',idat(1),idat(nidat),t,xnx,
     $           eth,gglo,ggup,swrat
        call dprinto(ndesc,nrdesc,lcon,
     $          nrdat,rdat,nidat,idat,nkdat,kdat,lun11) 
        endif
c      lprib=lpri
      lprib=0
      crit53=0.01
      lfastl=2
      call phint53(stmpp,etmpp,ntmp,ett,ans1,ans2,ans3,ans4,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,delr,
     $ opakc,rccemis,lprib,epi,bremsa,t,trad,swrat,xnx,crit53,
     $    lfastl,lun11)
      if (lpri.gt.1) then
        npr=nb1
        write (lun11,*)'bautista threshold xsection:',
     $         npr,ett,eth,rdat(1),sg(npr),ans2,swrat
        endif
      rates(1,ml)=ans1
      ans2=0.
      ans4=0.
      rates(2,ml)=ans2
      rates(3,ml)=ans3
      rates(4,ml)=ans4
      if (lpri.gt.1) write (lun11,*)'rates:',
     $    rates(1,ml),rates(2,ml),rates(3,ml),rates(4,ml)
      lpri=lprisv
      go to 9000
c


c
 9000 continue
c
      call remtms(time2)
c      write (lun11,*)'ndesc=',ndesc
      tucalc(ndesc)=tucalc(ndesc)+abs(time2-time1)
      ncall(ndesc)=ncall(ndesc)+1
c
      if (lpri.gt.1)
     $   write (lun11,9931)kdesc(ndesc),ndesc,ans1,ans2,
     $     ans3,ans4,idest1,idest2,rdat(1)
 9931 format (1x,'in ucalc :',a56,i4,4x,4(1pe10.2),2i4,3(1pe10.2))
c
      return
      end
      subroutine unsavd(ldir,
     $       lpri,lun,lun11,tinf,
     $       t,r,rdel,xcol,xee,xpx,
     $       tau0,dpthc,tauc,epi)
c
c     this routine  saves only depths for iterative calculation
c
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
c
c     line optical depths
      dimension tau0(2,nnnl)
c     continuum optical depths
      dimension dpthc(2,ncn)
      dimension tauc(2,nnml)
      dimension epi(ncn)
c
      dimension tau0d(2,nnnl),dpthcd(2,ncn),taucd(2,nnml)
c
      read (lun)t
      read (lun)r
      read (lun)rdel
      read (lun)xcol
      read (lun)xee
      read (lun)xpx 
      read (lun)tau0d
      read (lun)dpthcd
      read (lun)taucd
      lind1=1
      lind2=2
      if (ldir.gt.0) lind2=1
      if (ldir.lt.0) lind1=2    
      do ll=lind1,lind2
        do kl=1,nnnl
          tau0(ll,kl)=tau0d(ll,kl)
          enddo
        do kl=1,ncn
          dpthc(ll,kl)=dpthcd(ll,kl)
          enddo
        do kl=1,nnml
          tauc(ll,kl)=taucd(ll,kl)
          enddo
        enddo
      nlyc=nbinc(13.7,epi)
      nry=nlyc+1
      if (lpri.ne.0) write (lun11,*)'in unsavd',rdel,t,tauc(1,1),
     $  tauc(2,1),ldir,dpthc(1,nry),dpthc(2,nry),tau0(1,1),tau0(2,1),
     $  lind1,lind2
c
      return
      end
c-------------------------------------------------------------------
       function upsil(k,eij,c,p1,p2,p3,p4,p5,t)
c  t = electron temperature in Kelvin
c  p# = spline knot values
c  c = abscissa scale parameter
c  k = transition type
c  eij = transition energy (Ryd)
c
       e=abs(t/(1.57888e5*eij))                  !<<<<<< CORRECTED LINE
       if ((k.eq.1).or.(k.eq.4.)) x=log((e+c)/c)/log(e+c)
       if ((k.eq.2).or.(k.eq.3)) x=e/(e+c)
       y=splinem(p1,p2,p3,p4,p5,x)
       if (k.eq.1) y=y*log(e+2.71828)
       if (k.eq.3) y=y/(e+1)
       if (k.eq.4) y=y*log(e+c)
       upsil=y     
       return
       end
      subroutine velimp(n,l,temp,ic,z1,rm,ne,sum,cn)
c
c impact parameter collision rate calculated following the method of
c pengelly & seaton (1964) but using the lowest cross-section at every
c velocity.
c ********** note that cn is the rate for nl -> nl-1 and hence l > 0 *
c      cne(l+1)=cn
c      cen(l)=cn*(2.*l+1)/(2.*l-1)
c
c
c
      real ne
c
      cn=0.
      if((l.eq.0).or.(sum.eq.0.)) go to 50
      den=l*(n*n-l*l)+(l+1)*(n*n-(l+1)*(l+1))
      dnl=6.*z1/ic*z1/ic*n*n*(n*n-l*l-l-1)
      pi=2.*acos(0.)
      pa=0.72/sum
      pd=6.90*sqrt(temp/ne)
      alfa=3.297e-12*rm/temp
      b=1.157*sqrt(dnl)
      bb=b*b
c
      va=pd/pa
      vd=b/pd
      vb=sqrt(va*vd)
c
           ava=alfa*va*va
           avb=alfa*vb*vb
           avd=alfa*vd*vd
      ea=0.
      ed=0.
      xa=expo(-ava)
      xb=expo(-avb)
      xd=expo(-avd)
           if(ava.lt.50.) call expint(ava,ea)
c           call expint(ava,ea)
      ea=ea/ava*xa
           call expint(avb,eb)
      eb=eb/avb*xb
           if(avd.lt.50.) call expint(avd,ed)
c           call expint(avd,ed)
      ed=ed/avd*xd
c
      if(va.gt.vd) then
      if(avb.gt.1.e-3) then
      cn=sqrt(pi*alfa)*(pa*pa*(2./alfa/alfa-xb*(vb**4+2.*vb*vb/alfa+
     #2./alfa/alfa))+bb*xb+2.*bb*eb-bb*ea)
           else
      cn=sqrt(pi*alfa)*bb*(1.+avb*(1./3.-avb/4.)+2.*eb-ea)
                endif
c
           else
      if(ava.gt.1.e-3) then
      ca=sqrt(pi*alfa)*pa*pa*(2./alfa/alfa-xa*(va**4+2.*va*va/alfa+
     #2./alfa/alfa))
           else
      ca=sqrt(pi*alfa)*pd*pd*va**4*alfa*(1/3.-ava/4.+ava*ava/10.)
                 endif
c
      cad=sqrt(pi*alfa)*pd*pd/alfa*(xa*(1.+ava)-xd*(1.+avd))
      cd=sqrt(pi*alfa)*bb*(xd+ed)
      cn=ca+cad+cd
                 endif
c
      cn=cn*l*(n*n-l*l)/den
c
 50      return
      end
C
C
C     ****************************************************************
C
C
      function voigte(vs,a)
c     =====================
c
c  computes a voigt function  h = h(a,v)
c  a=gamma/(4*pi*dnud)   and  v=(nu-nu0)/dnud.  this  is  done after
c  traving (landolt-b\rnstein, p. 449).
c
      real*4 vs,a
      PARAMETER (UN=1., TWO=2.)
      dimension ak(19),a1(5)
      data ak      /-1.12470432, -0.15516677,  3.28867591, -2.34357915,
     ,  0.42139162, -4.48480194,  9.39456063, -6.61487486,  1.98919585,
     , -0.22041650, 0.554153432, 0.278711796,-0.188325687, 0.042991293,
     ,-0.003278278, 0.979895023,-0.962846325, 0.532770573,-0.122727278/
      data sqp/1.772453851/,sq2/1.414213562/
c
      v = abs(vs)
      u = a + v
      v2 = v*v
      if (a.eq.0.0) go to 140
      if (a.gt.0.2) go to 120
      if (v.ge.5.0) go to 121
c
      ex=0.
      if(v2.lt.100.)ex = exp(-v2)
      k = 1
c
  100 quo = un
      if (v.lt.2.4) go to 101
      quo = un/(v2 - 1.5)
      m = 11
      go to 102
c
  101 m = 6
      if (v.lt.1.3) m = 1
  102 do 103 i=1,5
         a1(i) = ak(m)
         m = m + 1
  103 continue
      h1 = quo*(a1(1) + v*(a1(2) + v*(a1(3) + v*(a1(4) + v*a1(5)))))
      if (k.gt.1) go to 110
c
c a le 0.2  and v lt 5.
c
      h = h1*a + ex*(un + a*a*(un - two*v2))
      voigte=h
      return
c
  110 pqs = two/sqp
      h1p = h1 + pqs*ex
      h2p = pqs*h1p - two*v2*ex
      h3p = (pqs*(un - ex*(un - two*v2)) - two*v2*h1p)/3. + pqs*h2p
      h4p = (two*v2*v2*ex - pqs*h1p)/3. + pqs*h3p
      psi = ak(16) + a*(ak(17) + a*(ak(18) + a*ak(19)))
c
c 0.2 lt a le 1.4  and  a + v le 3.2
c
      h = psi*(ex + a*(h1p + a*(h2p + a*(h3p + a*h4p))))
      voigte=h
      return
c
  120 if (a.gt.1.4.or.u.gt.3.2) go to 130
      ex=0.
      if(v2.lt.100.)ex = exp(-v2)
      k = 2
      go to 100
c
c a le 0.2  and  v ge 5.
c
  121 h = a*(15. + 6.*v2 + 4.*v2*v2)/(4.*v2*v2*v2*sqp)
      voigte=h
      return
c
  130 a2 = a*a
      u = sq2*(a2 + v2)
      u2 = un/(u*u)
c
c a gt 1.4  or  a + v gt 3.2
c
      h = sq2/sqp*a/u*(1. + u2*(3.*v2 - a2) +
     ,        u2*u2*(15.*v2*v2 - 30.*v2*a2 + 3.*a2*a2))
      voigte=h
      return
c
c a eq 0.
c
  140 h=0.
      if(v2.lt.100.)h=exp(-v2)
      voigte=h
      return
      end
      FUNCTION VOIGT(V,AGAM)
C     ======================
C
C     Voigt function
C     Procedure after Matta and Reichel, 1971, Math.Comp. 25, 339.

      DIMENSION HN(12),EN(12)
      PARAMETER (PI=3.141592653589793D0, M=12, HH=0.5D0, UN=1.D0)
      PARAMETER (PISQ=1.77245385090551D0,PISQ1=UN/PISQ)
      DATA ICOMP /0/
      SAVE EN,HN,PH,HP,ICOMP
C
C     Initialization of auxiliary quantities
C
      IF(ICOMP.NE.0) GO TO 20
      HP=HH*PISQ1
      PH=PI/HH
      DO 10 I=1,M
         XI=I
         U=XI*XI*HH*HH
         EN(I)=EXP(-U)
   10    HN(I)=4.D0*U
      ICOMP=1
   20 CONTINUE
C
C     Main term
C
      AGAM1=UN/AGAM
      X=V*AGAM1
      T=0.25D0*AGAM1*AGAM1
      X2=X*X
      X4=4.D0*X2
      S1=UN+X2
      S2=UN-X2
      U0=0.
      DO 30 I=1,M
         S0=HN(I)*T
         U=EN(I)/((S2+S0)*(S2+S0)+X4)
         U0=U0+U*(S1+S0)
   30 CONTINUE
      S2=UN/S1
      U0=HP*(S2+2.D0*U0)
C
C     Correction term
C
      IF(T.LT.0.25D0/PH/PH) GO TO 50
      U=X/2.D0/T
      A=COS(U)
      B=SIN(U)
      TSQ1=UN/SQRT(T)
      S1=PH*TSQ1
      S2=S1*X
      C=EXP(-S1)-COS(S2)
      D=SIN(S2)
      T4=0.25D0/T
      U=EXP(-X2*T4-S1+T4)*PISQ*TSQ1/(C*C+D*D)
      U0=U0+U*(A*C-B*D)
   50 VOIGT=U0*AGAM1*PISQ1
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Write extension containing the spectra for
C     this particular model.
C
C     Modifications:
C       04/01/1999,WTB: Disabled appending loop control value to 
C               extension name due to changes in xstar2xspec design
C
      subroutine writespectra2(lun11,lpri,nparms,parname,partype,parval,
     $       parcomm,epi,dpthc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,elum,tau0,kmodelname,nloopctl)

      implicit none

      integer ntyp, ncn, nnnl, nnml, nni, nl, ndl, nd
      integer nidat1, nrdat1, nkdat1, nptt, ndat2

      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
c      common /times/tread,tloop,tfunc,trates1,thcor,trates2
c     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     passed parameters
      character(30) kmodelname
      integer nparms, nloopctl, lun11
      character(20) parname(38)
      character(10) partype(38)
      real parval(38)
      character(30) parcomm(38)

c     master data 
      integer idat1(nidat1),nptrs(nptt,ndat2)
      real rdat1(nrdat1)

      character(1) kdat1(nkdat1)
c     pointers to master data 
      integer npar(ndat2),npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni)
c     pointers to line data 
      integer nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)
c     line luminosities
      real elum(2,nnnl)
c     energy bins
      real epi(ncn)
c     continuum optical depths
      real dpthc(2,ncn)
c     line optical depths
      real tau0(2,nnnl)
c
      real rtmp(ncn)
c
      character(16) knam,klabs(9),kunits(9),kform(9),kblnk16
      character(30) extname
      integer unit,istatus, nilin, nkdt,nidt,lcon,lrtyp,ltyp,ml,status
      integer nkdt2,nrdt2,nidt2
      integer nlsvn, ln, ll, lnn, nrdt,mllz
      integer tbcol(9), nrows, nspace, rowlen
      integer np2, kk
      integer frow, felem, colnum, tfields, verbose,mm
      real eliml, elimh, elmmtpp,elin,ergsev
C     Internal work areas
      integer ntptr(nnnl)
      character(10) kion(nnnl)
      character(20) klevl(nnnl),klevu(nnnl)
      character(16) ttype(9),tform(9),tunit(9)
      integer lpri
      integer jkk, nlev
      integer nlplmx,nilin2,nlpl,lmm,kltmpn,kltmpo,
     $         llo,lup,llofnd,lupfnd,nlevmx,
     $         k,kl2,lm,kk2
      real elcomp
      character(30) ktmp2

C     Database manipulation quantities
      real rdat(20000)
      integer idat(20000)
      character(1) kdat(20000),kblnk,kdtmp(200)
      real rdat2(20000)
      integer idat2(20000)
      character(1) kdat2(20000)
      integer kltmp(1000)
      real elsv(1000)
      integer klion,mlion,mliono,nitmp

      data kblnk/' '/
c
      data tform/'1J','1E','9A','20A','20A','1E','1E','1E',
     $ '1E'/

      data ttype/'index','wavelength','ion',
     $ 'lower_level','upper_level','lum_inward', 
     $ 'lum_outward','tau_in','tau_out'/

      data tunit/' ','A',' ',' ',' ','erg/cm^3/s',
     $ 'erg/cm^3/s',' ',' '/
c
      data kblnk16/'                '/,ergsev/1.602197e-12/
c

      verbose=lpri
      eliml=0.1
      elimh=1.0e10
c
c
c     open and prepare the fits file for spectral data
      if(verbose.gt.0) write (lun11,*)'writespectra2: opening header',
     $  kmodelname
      knam='xout_lines1.fits'
      call fheader(unit,knam,kmodelname,istatus)
      if(istatus.gt.0) call printerror(lun11,istatus)

c     write extension of parameter values
      if(verbose.gt.0) 
     $ write (lun11,*)'writespectra2: write parameter list'
      call fparmlist(unit,1,kmodelname,nparms,parname,partype,parval,
     $               parcomm,nloopctl,istatus,lun11)
      if(istatus.gt.0) call printerror(lun11,istatus)
      if(verbose.gt.0) 
     $  write (lun11,*)'writespectra2: building data tables'

c
c     build spectra data tables      
      if (verbose.gt.0) write (lun11,*)' '
      kltmpo=0
      lpri=0
      if (verbose.gt.0) 
     $  write (lun11,*)'emission line luminosities (erg/sec/10**38))'
      nlplmx=1000
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
        call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
        elin=rdat(1)
        if (lrtyp.ne.14) then
          nilin=npar(ml)
          call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,nilin-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
          nilin2=idat(nidt)
          elmmtpp=(elum(2,ln)+elum(1,ln))/2.
          if (verbose.gt.0) 
     $         write (lun11,*)lnn,elin,nilin,elmmtpp,ln,ml
          if ((ln.gt.0).and.(ln.lt.nnnl)
     $         .and.(elin.ge.eliml).and.(elin.le.elimh) 
     $         .and.(elin.le.8.9e+4)
     $         .and.(elmmtpp.gt.1.e-34)
     $         .and.(nilin2.gt.0).and.(nilin2.le.nni)) 
     $           then
            lmm=0
560           lmm=lmm+1
              if (lmm.ge.nlpl) go to 558
              kl2=kltmp(lmm)
              if (kl2.ne.0) 
     $        elcomp=(elum(2,kl2)+elum(1,kl2))/2.
              if (elmmtpp.ge.elcomp) go to 558
              go to 560
558         continue
            nlpl=max0(nlpl,min(nlplmx,nlpl+1),1)
            if (verbose.gt.0) 
     $       write (lun11,8516)ln,elin,elmmtpp
8516        format (1h ,i4,2e12.4)
            kltmpo=ln
            do  k=lmm,nlplmx
              kl2=kltmp(k)
              if (kl2.ne.0)
     $         elcomp=(elum(2,kl2)+elum(1,kl2))/2.
              if ((lpri.ne.0).and.(kltmp(k).ne.0))
     $         write (lun11,*)'in 557 loop',k,kltmp(k),elcomp
              kltmpn=kltmp(k)
              kltmp(k)=kltmpo
              kltmpo=kltmpn
              enddo
            if (verbose.gt.0) 
     $       write (lun11,*)'done with 557 loop',lm
            endif
          endif
        enddo
      kltmp(nlpl)=kltmpo
      nlpl=nlpl-1
      if (verbose.gt.0) 
     $    write (lun11,959)
959   format (1x,'index, ion, wavelength, transmitted, reflected')
      kk2=0
      do  kk=1,nlpl
        ln=kltmp(kk)
        if (ln.ne.0) then
          ml=nplin(ln)
          if (ml.ne.0) then
            if (verbose.gt.0) 
     $        write (lun11,*)'   ',ln,ml
            call dread(ltyp,lrtyp,lcon,
     $        nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $        idat1,rdat1,kdat1,nptrs,0,lun11)
            llo=idat(1)
            lup=idat(2)
            elsv(kk)=rdat(1)
            nilin=npar(ml)
            call dread(ltyp,lrtyp,lcon,
     $        nrdt,rdat,nidt,idat,nkdt,kdat,nilin-1,
     $        idat1,rdat1,kdat1,nptrs,0,lun11)
            do mm=1,nkdt
              kdtmp(mm)=kdat(mm)
              enddo
            do mm=nkdt+1,10
              kdtmp(mm)=kblnk
              enddo  
            nilin=idat(nidt)
            write(kion(kk),'(10a1)')(kdtmp(mm),mm=1,10)
            jkk=idat(nidt)
            klion=12
            mlion=npfirst(klion)
            jkk=0
            nitmp=0
            do while ((mlion.ne.0).and.(nitmp.ne.nilin))
C             retrieve ion name from kdati
              jkk=jkk+1
              call dread(ltyp,lrtyp,lcon,
     $          nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,mlion-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
              nitmp=idat2(nidt2)
              mliono=mlion
              mlion=npnxt(mlion)
              enddo

            ml=npfi(13,jkk)
            if (verbose.gt.0) 
     $         write (lun11,*)'ml=',ml,jkk,kion(kk)
            if (ml.ne.0) then
              mllz=npar(ml)
              lupfnd=0
              llofnd=0
 2943         continue
                call dread(ltyp,lrtyp,lcon,
     $            nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $            idat1,rdat1,kdat1,nptrs,0,lun11)
                nlevmx=nlevmx+1
                nlev=idat(nidt-1)
                if (nlev.eq.llo) then
                  do mm=1,20
                    if (mm.le.nkdt) then
                        write(ktmp2(mm:mm),'(a1)')kdat(mm)
                      else
                        write(ktmp2(mm:mm),'(a1)')kblnk
                      endif
                    enddo
                  klevl(kk)=ktmp2
                  llofnd=1
                  endif
                if (nlev.eq.lup) then
                  do mm=1,20
                    if (mm.le.nkdt) then
                        write(ktmp2(mm:mm),'(a1)')kdat(mm)
                      else
                        write(ktmp2(mm:mm),'(a1)')kblnk
                      endif
                    enddo
                  klevu(kk)=ktmp2
                  lupfnd=1
                  endif
                ml=npnxt(ml)
                if ((ml.ne.0).and.(npar(ml).eq.mllz)
     $           .and.((llofnd.ne.1).or.(lupfnd.ne.1))) go to 2943
              kk2=kk2+1
              ntptr(kk2)=ln
              if (verbose.gt.0) then
                write (lun11,*)ml,nilin,npar(ml)
                write (lun11,9955)kk,ln,(kdtmp(mm),mm=1,9),elsv(kk),
     $               elum(1,ln),elum(2,ln)
                write (lun11,*)klevu(kk)
                write (lun11,*)klevl(kk)
                endif
 9955           format (1x,2i8,1x,9a1,3(1pe11.3))
              endif
            endif
          endif
        enddo
c      if (nlpl.le.0) return
      nlpl=max(nlpl,1)
c
      nlpl=kk2
c
      if (verbose.gt.0) then
        do kk=1,nlpl
          write (lun11,*)kk,ntptr(kk)
          enddo
        endif
c
c     write the spectral data to the extension
      do mm=1,9
        kunits(mm)=kblnk16
        klabs(mm)=kblnk16
        kform(mm)=kblnk16
        enddo
      klabs(1)='index           '
      kform(1)='I6'
      kunits(1)='  '
      klabs(2)='ion             '
      kform(2)='A9'
      kunits(2)=' '
      klabs(3)='lower_level     '
      kform(3)='A20'
      kunits(3)='  '
      klabs(4)='upper_level     '
      kform(4)='A20'
      kunits(4)='  '
      klabs(5)='wavelength      '
      kform(5)='F10.2'
      kunits(5)='A'
      klabs(6)='emit_inward     '
      kform(6)='E11.3'
      kunits(6)='erg/s/10**38'
      klabs(7)='emit_outward    '
      kform(7)='E11.3'
      kunits(7)='erg/s/10**38'
      klabs(8)='depth_inward    '
      kform(8)='E11.3'
      kunits(8)='  '
      klabs(9)='depth_outward   '
      kform(9)='E11.3'
      kunits(9)='  '
c     build extension name
      extname='XSTAR_LINES'
C      if(nloopctl.gt.0) then
C          write(ktmp2,'(i4.4)')nloopctl
C          extname='xstar_spectra_' // ktmp2
C          endif
      if(verbose.gt.0) 
     $   write (lun11,*)'writespectra2: writing spectral data'

c     append a new empty extension onto the end of the primary array
      status=0
      call ftcrhd(unit,status)
      if(verbose.gt.0) 
     $    write (lun11,*)'writespectra2: writing header table'

      tfields=9
      nrows=nlpl
      nspace=1
      rowlen=0
      do mm=1,9
        tbcol(mm)=0
        enddo

c     write the required header parameters for the ascii table
      status=0
      call ftphtb(unit,rowlen,nrows,tfields,klabs,tbcol,kform,kunits,
     &            extname,status)
      if (status .gt. 0)call printerror(lun11,status)
      status=0
c
c     map each column to a 1-d array before writing to the file
      kk=1
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpclj(unit,colnum,frow,felem,nrows,ntptr,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=2
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcls(unit,colnum,frow,felem,nrows,kion,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=3
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcls(unit,colnum,frow,felem,nrows,klevl,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=4
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcls(unit,colnum,frow,felem,nrows,klevu,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=5
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,elsv,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=6
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nrows
          rtmp(ll)=elum(1,ntptr(ll))
          enddo
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rtmp,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=7
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nrows
          rtmp(ll)=elum(2,ntptr(ll))
          enddo
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rtmp,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=8
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nrows
          rtmp(ll)=tau0(1,ntptr(ll))
          enddo
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rtmp,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=9
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nrows
          rtmp(ll)=tau0(2,ntptr(ll))
          enddo
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rtmp,status)  
        if (status .gt. 0)call printerror(lun11,status)

c     compute checksums
      if(verbose.gt.0) write (lun11,*)'writespectra2: writing checksum'
      status=0
      call ftpcks(unit,status)
c     check for any error, and if so print out error messages
      if (status .gt. 0)call printerror(lun11,status)

      if(verbose.gt.0) write (lun11,*)'writespectra2: closing file'
      call fitsclose(lun11,unit,istatus)
c
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Write extension containing the spectra for
C     this particular model.
C
C     Modifications:
C       04/01/1999,WTB: Disabled appending loop control value to 
C               extension name due to changes in xstar2xspec design
C
      subroutine writespectra3(lun11,lpri,nparms,parname,partype,parval,
     $       parcomm,epi,dpthc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,elum,zrems,zremsz,kmodelname,nloopctl)

      implicit none

      integer ntyp, ncn, nnnl, nnml, nni, nl, ndl, nd
      integer nidat1, nrdat1, nkdat1, nptt, ndat2

      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
c      common /times/tread,tloop,tfunc,trates1,thcor,trates2
c     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     passed parameters
      character(30) kmodelname
      integer nparms, nloopctl, lun11
      character(20) parname(38)
      character(10) partype(38)
      real parval(38)
      character(30) parcomm(38)

c     master data 
      integer idat1(nidat1),nptrs(nptt,ndat2)
      real rdat1(nrdat1)

      character(1) kdat1(nkdat1)
c     pointers to master data 
      integer npar(ndat2),npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni)
c     pointers to line data 
      integer nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)
c     line luminosities
      real elum(2,nnnl)
c     energy bins
      real epi(ncn)
c     continuum lum
      real zrems(3,ncn),zremsz(ncn)
c     continuum optical depths
      real dpthc(2,ncn)
c     temporaries for dread
c
      real zrtmp(5,ncn), rtmp(ncn)
c
      character(16) knam,klabs(5),kunits(5),kform(5),kblnk16
c      character(30) ktmp2
      character(30) extname
      integer unit,istatus, kl,lpri
      integer nlsvn, ll, numcon
      integer np2, tbcol(5), nrows, nspace, rowlen, kk
      integer frow, felem, colnum, tfields, status, verbose,mm
      real eliml, elimh, ergsev
c
      data kblnk16/'                '/,ergsev/1.602197e-12/
c
      verbose=lpri
      eliml=0.1
      elimh=1.0e10
c
c
c     open and prepare the fits file for spectral data
      if(verbose.gt.0) write (lun11,*)'writespectra3: opening header',
     $  kmodelname
      knam='xout_cont1.fits'
      call fheader(unit,knam,kmodelname,istatus)
      if(istatus.gt.0) call printerror(lun11,istatus)

c     write extension of parameter values
      if(verbose.gt.0) 
     $  write (lun11,*)'writespectra: write parameter list'
      call fparmlist(unit,1,kmodelname,nparms,parname,partype,parval,
     $               parcomm,nloopctl,istatus,lun11)
      if(istatus.gt.0) call printerror(lun11,istatus)
      if(verbose.gt.0) 
     $  write (lun11,*)'writespectra: building data tables'

c     build spectra data tables
      numcon=ncn
      do ll=1,ncn
        zrtmp(4,ll)=0.
        zrtmp(5,ll)=0.
        enddo
      do kl=1,numcon
         zrtmp(4,kl)=zrtmp(4,kl)+zrems(2,kl)
         zrtmp(5,kl)=zrtmp(5,kl)+zrems(3,kl)
         zrtmp(3,kl)=zremsz(kl)*exp(-(dpthc(1,kl)))
c         write (lun11,968)kl,epi(kl),zremsz(kl),
c     $          zrtmp1(kl),zrtmp2(kl)
         zrtmp(2,kl)=zremsz(kl)
         zrtmp(1,kl)=epi(kl)
         enddo

c     write the spectral data to the extension
      do mm=1,5
        kunits(mm)=kblnk16
        klabs(mm)=kblnk16
        kform(mm)=kblnk16
        enddo
      klabs(1)='energy          '
      kform(1)='E11.3'
      kunits(1)='eV'
      klabs(2)='incident        '
      kform(2)='E11.3'
      kunits(2)='erg/s/erg'
      klabs(3)='transmitted     '
      kform(3)='E11.3'
      kunits(3)='erg/s/erg'
      klabs(4)='emit_inward     '
      kform(4)='E11.3'
      kunits(4)='erg/s/erg'
      klabs(5)='emit_outward    '
      kform(5)='E11.3'
      kunits(5)='erg/s/erg'
c     build extension name
      extname='XSTAR_SPECTRA'
C      if(nloopctl.gt.0) then
C          write(ktmp2,'(i4.4)')nloopctl
C          extname='xstar_spectra_' // ktmp2
C          endif
      if(verbose.gt.0) 
     $  write (lun11,*)'writespectra: writing spectral data'
c      call writespectra(unit,ktmp1,zrtmp,5,999,ncn,
c     $                 klabs,kform,kunits)

c     append a new empty extension onto the end of the primary array
      status=0
      call ftcrhd(unit,status)
      if(verbose.gt.0) 
     $   write (lun11,*)'writespectra: writing header table'

      tfields=5
      nrows=ncn
      nspace=1
      rowlen=0
      do mm=1,5
      tbcol(mm)=0
      enddo

c     write the required header parameters for the ascii table
      status=0
      call ftphtb(unit,rowlen,nrows,tfields,klabs,tbcol,kform,kunits,
     &            extname,status)
      if (status .gt. 0)call printerror(lun11,status)
      status=0
c
c     map each column to a 1-d array before writing to the file
      do kk=1,tfields
        if(verbose.gt.0) 
     $    write (lun11,*)'writespectra: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nrows
          rtmp(ll)=zrtmp(kk,ll)
          enddo
        if(verbose.gt.0) 
     $    write (lun11,*)'writespectra: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rtmp,status)  
        if (status .gt. 0)call printerror(lun11,status)
        enddo

c     compute checksums
      if(verbose.gt.0) write (lun11,*)'writespectra: writing checksum'
      call ftpcks(unit,status)
c     check for any error, and if so print out error messages
      if (status .gt. 0)call printerror(lun11,status)

      if(verbose.gt.0) write (lun11,*)'writespectra: closing file'
      call fitsclose(lun11,unit,istatus)
c
      return
      end
      subroutine writespectra4(lun11,lpri,nparms,parname,partype,parval,
     $       parcomm,epi,dpthc,abel,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,elumab,tauc,kmodelname,nloopctl)

      implicit none

      integer ntyp, ncn, nnnl, nnml, nni, nl, ndl, nd
      integer nidat1, nrdat1, nkdat1, nptt, ndat2

      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
c      common /times/tread,tloop,tfunc,trates1,thcor,trates2
c     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     passed parameters
      character(30) kmodelname
      integer nparms, nloopctl, lun11
      character(20) parname(38)
      character(10) partype(38)
      real parval(38)
      character(30) parcomm(38)

c     master data 
      integer idat1(nidat1),nptrs(nptt,ndat2)
      real rdat1(nrdat1)

      character(1) kdat1(nkdat1)
c     pointers to master data 
      integer npar(ndat2),npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni)
c     pointers to line data 
      integer nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)
      real elumab(2,nnml),tauc(2,nnml)
c     energy bins
      real epi(ncn)
c     continuum optical depths
      real dpthc(2,ncn)
      real abel(13)
c     temporaries for dread
      real rdat(20000)
      integer idat(20000)
      real rlev(10,nd)
      integer ilv(10,nd)
c
      real rsv1(10000),rsv2(10000),rsv3(10000),rsv4(10000),rsv5(10000)
      integer ntptr(10000)
c
      character(8) kdtmpi(10000),kdtmp8
      character(20) kdtmpl(10000),kdtmp20
      character(1) klev(100,nd),kblnk
      character(1) kdat(20000),kdati(20000)
      character(16) knam,klabs(8),kunits(8),kform(8),kblnk16
      character(30) extname
      integer unit,istatus,kl,nkdt,nidt,lcon,lrtyp,ltyp,ml,nkdti
      integer nlsvn,nrdt
      integer np2, tbcol(8), nrows, nspace, rowlen, kk
      integer frow, felem, colnum, tfields, status, verbose,mm
      real ergsev,eth,xeltp,abund1
      integer lpri,klel,mlel,jk,mt2,mllel,nnz,jkk,klion,mlion,
     $        mlleltp,nlevmx,mltype,mllz,nlev,lk,mlrdesc,kkkl,idest1,
     $        kksv,llo
c
      data kblnk/' '/
      data kblnk16/'                '/,ergsev/1.602197e-12/
c

      verbose=0
c     open and prepare the fits file for spectral data
      if(verbose.gt.0) write (lun11,*)'writespectra: opening header',
     $  kmodelname
      knam='xout_rrc1.fits'
      call fheader(unit,knam,kmodelname,istatus)
      if(istatus.gt.0) call printerror(lun11,istatus)

c     write extension of parameter values
      if(verbose.gt.0) 
     $     write (lun11,*)'writespectra: write parameter list'
      call fparmlist(unit,1,kmodelname,nparms,parname,partype,parval,
     $               parcomm,nloopctl,istatus,lun11)
      if(istatus.gt.0) call printerror(lun11,istatus)
      if(verbose.gt.0) 
     $  write (lun11,*)'writespectra: building data tables'

c     build spectra data tables
      write (lun11,*)' '
      lpri=verbose
c     print 500 strongest recombination continua
c      write (lun11,*)'recombination continuum luminosities',
c     $  '(erg/sec/10**38))'
c      write (lun11,*)'ion, level, energy (eV), RRC luminosity '
C     lpril is flag for printing debug information
C      initialize line counter
      kksv=0
C      First look for element data (jk is element index)
        klel=11
        mlel=npfirst(klel)
        jk=0
        do while (mlel.ne.0)
          jk=jk+1
          mt2=mlel-1
          call dread(ltyp,lrtyp,lcon,
     $      nrdt,rdat,nidt,idat,nkdt,kdat,mt2,
     $      idat1,rdat1,kdat1,nptrs,0,lun11)
          mllel=idat(nidt)
          xeltp=rdat(1)
          xeltp=abel(mllel)
          nnz=idat(1)
          if (lpri.ne.0) 
     $      write (lun11,*)'element:',jk,mlel,mllel,nnz,
     $                  (kdat(mm),mm=1,nkdt)
C         ignore if the abundance is small
          if (xeltp.lt.1.e-10) then
              jkk=jkk+nnz
            else
c             now step thru ions (jkk is ion index)
              klion=12
              mlion=npfirst(klion)
              jkk=0
              kl=0
              do while ((mlion.ne.0).and.(kl.lt.nnz))
                jkk=jkk+1
C               retrieve ion name from kdati
                call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdti,kdati,mlion-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
C               if not accessing the same element, skip to the next element
                mlleltp=idat(nidt-1)
                if (mlleltp.eq.mllel) then
                  kl=kl+1
                  if (lpri.ne.0)
     $              write (lun11,*)'  ion:',kl,jkk,mlion,mlleltp,
     $                          (kdati(mm),mm=1,nkdti)
c                 now find level data
c                 step thru types
                  nlevmx=0
                  mltype=13
                  ml=npfi(mltype,jkk)
                  mllz=npar(ml)
c                 step thru records of this type
                  do while ((ml.ne.0).and.(npar(ml).eq.mllz))
                    call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
                    nlev=idat(nidt-1)
                    nlevmx=max(nlevmx,nlev)
                    if ((nlev.gt.0).and.(nlev.le.nd)) then
                      if (lpri.ne.0)write (lun11,*)'level quantities:',
     $                ml,nlev,ltyp,lrtyp,rdat(1),rdat(2)
                      do  lk=1,nrdt
                        rlev(lk,nlev)=rdat(lk)
                        enddo
                      do lk=1,nidt
                        ilv(lk,nlev)=idat(lk)
                        enddo
                      do lk=1,nkdt
                        klev(lk,nlev)=kdat(lk)
                        enddo
                      do lk=nkdt+1,20
                        klev(lk,nlev)=kblnk
                        enddo
                      endif
                    ml=npnxt(ml)
                    enddo
                  nlev=nlevmx
                  mltype=7
                  mlrdesc=mltype
                  ml=npfi(mltype,jkk)
                  mllz=npar(ml)
                  do while ((ml.ne.0).and.(npar(ml).eq.mllz))
c                   step thru records of this type
                    call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
                    kkkl=npconi2(ml)              
                    idest1=idat(nidt-1)
                    if ((kkkl.gt.0).and.(kkkl.le.ndat2)
     $                .and.(elumab(1,kkkl).gt.1.e-36)) then
                      kksv=kksv+1
                      eth=rlev(4,idest1)-rlev(1,idest1)
                      ntptr(kksv)=kkkl
                      rsv1(kksv)=eth
                      rsv2(kksv)=elumab(1,kkkl)
                      rsv3(kksv)=elumab(2,kkkl)
                      rsv4(kksv)=tauc(1,kkkl)
                      rsv5(kksv)=tauc(2,kkkl)
                      do mm=1,nkdti
                        write (kdtmp8(mm:mm),'(a1)')kdati(mm)
                        enddo
                      do mm=nkdti+1,8
                        write (kdtmp8(mm:mm),'(a1)')kblnk
                        enddo
                      kdtmpi(kksv)=kdtmp8
                      do mm=1,20
                        write (kdtmp20(mm:mm),'(a1)')klev(mm,idest1)
                        enddo
                      kdtmpl(kksv)=kdtmp20
                      if (lpri.ne.0)
     $                 write (lun11,*)jkk,idest1,llo,abund1,
     $                 eth,elumab(1,kkkl),elumab(2,kkkl)
                      if (lpri.ne.0)
     $                 write (lun11,9293)kdtmpi(kksv),
     $                        (klev(lk,idest1),lk=1,20),eth,
     $                        elumab(1,kkkl)
 9293                 format(1x,20a1,20a1,2(1pe11.3))
                      endif
                    ml=npnxt(ml)
                    enddo
                  endif
C               Go to next ion
                mlion=npnxt(mlion)
                enddo
            endif
          mlel=npnxt(mlel)
C         Go to next element
          enddo

c     write the spectral data to the extension
      do mm=1,8
        kunits(mm)=kblnk16
        klabs(mm)=kblnk16
        kform(mm)=kblnk16
        enddo
      klabs(1)='index          '
      kform(1)='I6'
      kunits(1)='  '
      klabs(2)='ion            '
      kform(2)='A9'
      kunits(2)='  '
      klabs(3)='level          '
      kform(3)='A20'
      kunits(3)='  '
      klabs(4)='energy         '
      kform(4)='E11.3'
      kunits(4)='eV'
      klabs(5)='emit_outward    '
      kform(5)='E11.3'
      kunits(5)='erg/s'
      klabs(6)='emit_inward     '
      kform(6)='E11.3'
      kunits(6)='erg/s'
      klabs(7)='depth_outward   '
      kform(7)='E11.3'
      kunits(7)='  '
      klabs(8)='depth_inward    '
      kform(8)='E11.3'
      kunits(8)='  '
c     build extension name
      extname='XSTAR_SPECTRA'
C      if(nloopctl.gt.0) then
C          write(ktmp2,'(i4.4)')nloopctl
C          extname='xstar_spectra_' // ktmp2
C          endif
      if(verbose.gt.0) 
     $   write (lun11,*)'writespectra: writing spectral data'

c     append a new empty extension onto the end of the primary array
      status=0
      call ftcrhd(unit,status)
      if(verbose.gt.0) 
     $    write (lun11,*)'writespectra: writing header table'

      tfields=8
      nrows=kksv
      nspace=1
      rowlen=0
      do mm=1,8
      tbcol(mm)=0
      enddo

c     write the required header parameters for the ascii table
      status=0
      call ftphtb(unit,rowlen,nrows,tfields,klabs,tbcol,kform,kunits,
     &            extname,status)
      if (status .gt. 0)call printerror(lun11,status)
      status=0
c
c     map each column to a 1-d array before writing to the file
      kk=1
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpclj(unit,colnum,frow,felem,nrows,ntptr,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=2
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcls(unit,colnum,frow,felem,nrows,kdtmpi,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=3
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcls(unit,colnum,frow,felem,nrows,kdtmpl,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=4
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rsv1,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=5
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rsv2,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=6
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rsv3,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=7
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rsv4,status)  
        if (status .gt. 0)call printerror(lun11,status)
      kk=8
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra2: building column ',kk
        frow=1
        felem=1
        colnum=kk
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra2: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rsv5,status)  
        if (status .gt. 0)call printerror(lun11,status)

c     compute checksums
      if(verbose.gt.0) write (lun11,*)'writespectra: writing checksum'
      status=0
      call ftpcks(unit,status)
c     check for any error, and if so print out error messages
      if (status .gt. 0)call printerror(lun11,status)

      if(verbose.gt.0) write (lun11,*)'writespectra: closing file'
      call fitsclose(lun11,unit,istatus)
c
      return
      end
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Write extension containing the spectra for
C     this particular model.
C
C     Modifications:
C       04/01/1999,WTB: Disabled appending loop control value to 
C               extension name due to changes in xstar2xspec design
c       051/17/2003 TK added auger damping
C
      subroutine writespectra(lun11,lpri,nparms,
     $       parname,partype,parval,parcomm,t,vturbi,epi,dpthc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,elum,zrems,zremsz,kmodelname,nloopctl)

      integer ntyp, ncn, nnnl, nnml, nni, nl, ndl, nd
      integer ndat2,nptt, nidat1, nrdat1, nkdat1

      parameter (ntyp=90)
      parameter (ncn=9999)
      parameter (nnnl=80000,nnml=80000,nni=168,nl=13)
      parameter (ndl=2400,nd=ndl+1)
      parameter (nidat1=1800000,nrdat1=1800000,nkdat1=1800000,
     $           nptt=10,ndat2=100000)
c
c      common /times/tread,tloop,tfunc,trates1,thcor,trates2
c     $               ,tucalc(ntyp),ncall(ntyp),theat
c
c     passed parameters
      character(30) kmodelname
      integer nparms, nloopctl, lun11
      character(20) parname(38)
      character(10) partype(38)
      real parval(38)
      character(30) parcomm(38)

c     master data 
      integer idat1(nidat1),nptrs(nptt,ndat2)
      real rdat1(nrdat1)

      character(1) kdat1(nkdat1)
c     pointers to master data 
      integer npar(ndat2),npnxt(ndat2),npfirst(ntyp)
      integer npfi(ntyp,nni)
c     pointers to line data 
      integer nplin(nnnl),nplini(ndat2)
c     pointers to line data 
      integer npcon(nnml),npconi2(ndat2),npconi(ndat2)
      integer npilev(nd,nni),npilevi(nnml)
c     line luminosities
      real elum(2,nnnl)
c     energy bins
      real epi(ncn)
c     continuum lum
      real zrems(3,ncn),zremsz(ncn)
c     continuum optical depths
      real dpthc(2,ncn)
c     temporaries for dread
      real rdat(20000)
      integer idat(20000)
c
      real zrtmp(5,ncn), rtmp(ncn)
c
      character(1) kdat(20000),kdat2(20000)
      character(16) knam,klabs(5),kunits(5),kform(5),kblnk16
      character(30) extname
      integer unit,istatus, kl, nilin, nkdt,nidt,lcon,lrtyp,ltyp,ml
      integer nlsvn, ln, ll, numcon, lnn, nbtmp, nrdt, nbtmpp
      integer nbtmpm, np2, tbcol(5), nrows, nspace, rowlen, kk
      integer frow, felem, colnum, tfields, status, verbose,mm
      integer nb1,nb2,lup,ndtmp,mllz,nkdt2,iion,nitmp,nbtmpt,
     $        ltyp2,lrtyp2,lcon2,nrdt2,nidt2,idat2(20000),iltmp

      real eliml, elimh, elmmtpp, dele, etmp, elin,ergsev,aasmall
      real dele2,egam,profile,deler,delea,rdat2(20000),vturbi,aatmp,bbb
c
      data kblnk16/'                '/,ergsev/1.602197e-12/
c
c     externally defined functions
      integer nbinc

      verbose=lpri
      eliml=0.1
      elimh=1.e+5
      elimh=min(elimh,8.9e+4) 
c
c

c     open and prepare the fits file for spectral data
      if(verbose.gt.0) write (lun11,*)'writespectra: opening header',
     $  kmodelname
      knam='xout_spect1.fits'
      call fheader(unit,knam,kmodelname,istatus)
      if(istatus.gt.0) call printerror(lun11,istatus)

c     write extension of parameter values
      if(verbose.gt.0) 
     $     write (lun11,*)'writespectra: write parameter list'
      call fparmlist(unit,1,kmodelname,nparms,parname,partype,parval,
     $               parcomm,nloopctl,istatus,lun11)
      if(istatus.gt.0) call printerror(lun11,istatus)
      if(verbose.gt.0) 
     $  write (lun11,*)'writespectra: building data tables'

c     build spectra data tables
      numcon=ncn
      bbb=vturbi
      do ll=1,ncn
        zrtmp(4,ll)=0.
        zrtmp(5,ll)=0.
        enddo
      do  lnn=1,nlsvn
         ln=lnn
         ml=nplin(ln)
         call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
         elin=rdat(1)
         egam=rdat(3)
         lup=idat(2)
         nilin=npar(ml)
         call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,nilin-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
         nelin=npar(nilin)
         nilin=idat(3)
c        get nuclear mass       
         ml=nelin
         call dread(ltyp,lrtyp,lcon,
     $          nrdt,rdat,nidt,idat,nkdt,kdat,ml-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
         aatmp=rdat(2)
         elmmtpp=(elum(2,ln)+elum(1,ln))/2.
         if (lpri.ne.0)
     $     write (lun11,*)ln,elin,elmmtpp,nilin,nelin,egam,
     $              lup,aatmp
         if (((ln.gt.0).and.(ln.le.nnnl)) 
     $    .and.((elin.gt.eliml).and.(elin.lt.elimh)) 
     $    .and.(elmmtpp.gt.1.e-34).and.(aatmp.gt.1.e-24)
     $    .and.((nilin.gt.0).and.(nilin.le.nni)))
     $    then

c          line parameters
           etmp=12398.54/elin
           nbtmp=nbinc(etmp,epi)
           etmp=epi(nbtmp)
           nbtmpp=min(nbtmp+1,ncn)
           nbtmpm=max(nbtmp-1,1)
           dele=(epi(nbtmpp)-epi(nbtmpm))*ergsev/2.

c          find associated type 86 data
           iion=0
           idat(nidt)=0
           if (lpri.ne.0)
     $      write (lun11,*)'searching for ion'
           do while ((idat(nidt).ne.nilin).and.(iion.lt.nni))
             iion=iion+1
             nitmp=npfi(13,iion)
             call dread(ltyp,lrtyp,lcon,
     $               nrdt,rdat,nidt,idat,nkdt,kdat,nitmp-1,
     $               idat1,rdat1,kdat1,nptrs,0,lun11)
           if (lpri.ne.0)
     $        write (lun11,*)iion,idat(nidt),nilin,nitmp
             enddo
           ndtmp=npfi(41,iion)
           delea=0.
           if (ndtmp.ne.0) then
           if (lpri.ne.0)
     $        write (lun11,*)'  found ion',lup,ndtmp
             if (ndtmp.gt.0) then
               mllz=npar(ndtmp)
               call dread(ltyp2,lrtyp2,lcon2,
     $          nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,ndtmp-1,
     $          idat1,rdat1,kdat1,nptrs,0,lun11)
               iltmp=idat2(2)
               do while ((ndtmp.ne.0).and.(lup.ne.iltmp)
     $          .and.(npar(ndtmp).eq.mllz)) 
                 call dread(ltyp2,lrtyp2,lcon2,
     $            nrdt2,rdat2,nidt2,idat2,nkdt2,kdat2,ndtmp-1,
     $            idat1,rdat1,kdat1,nptrs,0,lun11)
                 iltmp=idat2(2)
                 if (lpri.ne.0)
     $            write (lun11,*)'   ',nidt2,iltmp,ndtmp
                 ndtmp=npnxt(ndtmp)     
                 enddo
               endif
             if (lup.eq.iltmp) delea=rdat2(3)*(4.14e-15)
             endif
c
c          cheat for narrow line plot
c           delea=0.        
c
c          thermal width quantities
           vth=(1.2e+1)*sqrt(t/aatmp)
           vturb=max(bbb,vth)
           e0=(12398.54)/amax1(elin,1.e-34)
           deleturb=e0*(vturb/3.e+5)
           deleth=e0*(vth/3.e+5)
           dele=deleth+deleturb
           deler=egam*(4.14e-15)
           dele2=delea+deler+dele
           aasmall=(delea+deler)/(1.e-36+dele)/12.56
           nb1=nbinc(etmp-5.*dele2,epi)
           nb2=nbinc(etmp+5.*dele2,epi)
           if (lpri.ne.0)
     $      write (lun11,*)deler,delea,dele2,nb1,nb2,deleth,
     $                       aatmp,aasmall
           do nbtmpt=nb1,nb2
             if (nb1.ne.nb2) then
                 delet=(epi(nbtmpt)-etmp)/dele
c                 if ((aasmall.gt.1.e-12).and.(nbtmpt.ne.nbtmp)) then
                 if (aasmall.gt.1.e-12) then
                     profile=voigte(delet,aasmall)
                   else
                     profile=expo(-delet*delet)/1.772
                   endif 
                 profile=profile/dele/(1.602197e-12)
               else
                 profile=1./(epi(nb1+1)-epi(nb1))/1.602197e-12
               endif
             zrtmp(4,nbtmpt)=zrtmp(4,nbtmpt)+elum(1,ln)*profile
             zrtmp(5,nbtmpt)=zrtmp(5,nbtmpt)+elum(2,ln)*profile   
             if (lpri.ne.0) write (lun11,*)nbtmpt,epi(nbtmpt),delet,
     $         profile,elum(1,ln),elum(2,ln),
     $         zrtmp(4,nbtmpt),zrtmp(5,nbtmpt)
             enddo

           endif

         enddo

      do kl=1,numcon
         if (lpri.ne.0) write (lun11,*)kl,epi(kl),zrems(2,kl),
     $          zrems(3,kl),zrtmp(4,kl),zrtmp(5,kl),dpthc(1,kl),
     $          zremsz(kl)
         zrtmp(4,kl)=zrtmp(4,kl)+zrems(2,kl)
         zrtmp(5,kl)=zrtmp(5,kl)+zrems(3,kl)
         zrtmp(3,kl)=zremsz(kl)*max(1.e-10,expo(-(dpthc(1,kl))))
         zrtmp(2,kl)=zremsz(kl)
         zrtmp(1,kl)=epi(kl)
         enddo

c     write the spectral data to the extension
      do mm=1,5
        kunits(mm)=kblnk16
        klabs(mm)=kblnk16
        kform(mm)=kblnk16
        enddo
      klabs(1)='energy          '
      kform(1)='E11.3'
      kunits(1)='eV'
      klabs(2)='incident        '
      kform(2)='E11.3'
      kunits(2)='erg/s/erg'
      klabs(3)='transmitted     '
      kform(3)='E11.3'
      kunits(3)='erg/s/erg'
      klabs(4)='emit_inward     '
      kform(4)='E11.3'
      kunits(4)='erg/s/erg'
      klabs(5)='emit_outward    '
      kform(5)='E11.3'
      kunits(5)='erg/s/erg'
c     build extension name
      extname='XSTAR_SPECTRA'
C      if(nloopctl.gt.0) then
C          write(ktmp2,'(i4.4)')nloopctl
C          extname='xstar_spectra_' // ktmp2
C          endif
      if(verbose.gt.0) 
     $   write (lun11,*)'writespectra: writing spectral data'

c     append a new empty extension onto the end of the primary array
      status=0
      call ftcrhd(unit,status)
      if(verbose.gt.0) 
     $    write (lun11,*)'writespectra: writing header table'

      tfields=5
      nrows=ncn
      nspace=1
      rowlen=0
      do mm=1,5
      tbcol(mm)=0
      enddo

c     write the required header parameters for the ascii table
      status=0
      call ftphtb(unit,rowlen,nrows,tfields,klabs,tbcol,kform,kunits,
     &            extname,status)
      if (status .gt. 0)call printerror(lun11,status)
      status=0
c
c     map each column to a 1-d array before writing to the file
      do kk=1,tfields
        if(verbose.gt.0) 
     $      write (lun11,*)'writespectra: building column ',kk
        frow=1
        felem=1
        colnum=kk
        do ll=1,nrows
          rtmp(ll)=zrtmp(kk,ll)
          enddo
        if(verbose.gt.0) 
     $     write (lun11,*)'writespectra: writing column ',kk
        status=0
        call ftpcle(unit,colnum,frow,felem,nrows,rtmp,status)  
        if (status .gt. 0)call printerror(lun11,status)
        enddo

c     compute checksums
      if(verbose.gt.0) write (lun11,*)'writespectra: writing checksum'
      status=0
      call ftpcks(unit,status)
c     check for any error, and if so print out error messages
      if (status .gt. 0)call printerror(lun11,status)

      if(verbose.gt.0) write (lun11,*)'writespectra: closing file'
      call fitsclose(lun11,unit,istatus)
c
      return
      end
