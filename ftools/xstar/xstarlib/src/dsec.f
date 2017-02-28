      subroutine dsec(lnerr,nlim,
     $       lpri,lppri,lun11,tinf,vturbi,critf,
     $       t,trad,r,delr,xee,xpx,abel,cfrac,p,lcdd,
     $       epi,ncn2,bremsa,bremsint,
     $       zrems,zremso,
     $       elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       ntotit,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,opakscatt,cemab,
     $       cabab,opakab,fline,flinel)
c
c     this routine solves for temperature and electron density by the
c     double secant method
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
c
c
c     global xstar data
c     master data
      integer idat1(nidat1)
      real*8 rdat1(nrdat1)
      integer nptrs(nptt,ndat2)
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
      real*8 elum(3,nnnl),elumo(3,nnnl)
c     line emissivities
      real*8 rcem(2,nnnl)
c     line opacities
      real*8 oplin(nnnl)
      real*8 fline(2,nnnl),flinel(ncn)
c     line optical depths
      real*8 tau0(2,nnnl)
c     energy bins
      real*8 epi(ncn)
c      continuum lum
      real*8 zrems(4,ncn),zremso(4,ncn)
c     continuum flux
      real*8 bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      real*8 rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      real*8 opakc(ncn),opakscatt(ncn)
c     level populations
      real*8 xilev(nnml),bilev(nnml),rniss(nnml)
      real*8 cemab(2,nnml),cabab(nnml),opakab(nnml)
      real*8 elumab(2,nnml),elumabo(2,nnml)
      real*8 tauc(2,nnml)
c     ion abundances
      real*8 xii(nni)
c     heating and cooling
      real*8 htt(nni),cll(nni)
      real*8 rrrt(nni),pirt(nni)
      integer nlevs(nni)
c     element abundances
      real*8 abel(nl)
c     the saved rates
      real*8 rates(4,ndat2)
      integer idrates(2,ndat2)
      real*8 vsav(4,ndat2)
c     compton heating data
      real*8 decomp(ncomp,ncomp),ecomp(ncomp),sxcomp(ncomp)
c     state variables
      real*8 p,r,t,xpx,delr
c     heating-cooling variables
      real*8 httot,cltot,htcomp,clcomp,clbrems,elcter,cllines,
     $     clcont,hmctot
c     input parameters
      real*8 trad,tinf
      real*8 cfrac,critf,vturbi,xee
      integer lcdd,ncn2,lpri,lun11,np2,nlim
c     variables associated with thermal equilibrium solution
      integer nmat,ntotit
c     temporary for xwrite
      character(133) tmpst
      integer nlsvn,ncsvn
c
c     local variables
      integer nnt,nntt,lnerr,lppri0,lppri,nlimt,nlimx,nnxx,
     $        nlimtt,nlimxx,iht,ilt,iuht,iult,ihx,ilx,nnx
      real*8 crite,crith,critt,fact,facx,epst,epsx,epstt,to,
     $     tl,th,xeel,xeeh,elctrl,elctrh,hmctth,hmcttl,tst,
     $     testt
c
c
      if (lpri.ne.0) write (lun11,*)'in dsec'
c
      crite=1.e-03
c      crite=1.e-06
      crith=1.e-02
c      crith=5.e-03
c      crith=1.e-05
      critt=2.e-09
c
      ntotit=0
      nnt = 0
      nntt=0
      lnerr = 0
      lppri0 = lppri
      nlimt =max(nlim,0)
      nlimx=abs(nlim)
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
      xeeh = 1.
      elctrl = 1.
      elctrh = -1.
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
          nlimxx=nlimx
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
      call func(lpri,lun11,vturbi,critf,
     $       t,trad,r,delr,xee,xpx,abel,cfrac,p,lcdd,
     $       epi,ncn2,bremsa,bremsint,
     $       zrems,zremso,elumab,elumabo,elum,elumo,
     $       decomp,ecomp,sxcomp,
     $       tau0,tauc,
     $       idat1,rdat1,kdat1,nptrs,np2,
     $       npar,npnxt,npfi,npfirst,
     $       nplin,nplini,nlsvn,npcon,npconi,npilev,npilevi,
     $       npconi2,nlevs,ncsvn,rates,vsav,idrates,
     $       xii,rrrt,pirt,htt,cll,httot,cltot,hmctot,elcter,
     $         cllines,clcont,htcomp,clcomp,clbrems,
     $       xilev,bilev,rniss,nmat,
     $       rcem,oplin,rccemis,brcems,opakc,opakscatt,cemab,
     $       cabab,opakab,fline,flinel)
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
      tst=abs(elcter)/max(1.e-10,xee)
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
      lnerr = 2
      write (lun11,99002)
      write (lun11,99006) nnt,t,tl,th,hmctot,hmcttl,hmctth
c
 500  if ( lppri.ne.0 ) write (lun11,99007) testt,epst,hmctot
      lppri = lppri0
c
      return
99002 format (' ','**** note: in dsec --  too many iterations **** ')
99004 format (' ',' warrning -- dsec not converging ')
99006 format (' ',' temperature ',i4,6(1pe16.8))
99007 format (' ',' finishing dsec -- test,epst,hmctot',3(1pe16.8))
      end
