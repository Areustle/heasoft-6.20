      subroutine step(ectt,emult,epi,ncn2,opakc,rccemis,fline,
     $  zrems,lpri,delr,dpthc,r,
     $  xpxcol,xcol,xpx,taumax,numrec0,lun11)
c
c
c
c     this routine claculates step sizes
c
      implicit none
c
      include './PARAM'
c
      real*8 opakc(ncn),epi(ncn),dpthc(2,ncn)
      real*8 rccemis(2,ncn)
      real*8 zrems(4,ncn)
      real*8 fline(2,nnnl)
      integer lpri,lprisv,kl,klmn,ncn2,numrec0,lun11
      real*8 taumax,emult,xpx,xcol,xpxcol,ectt,delr,r,optp2,dell,tst,
     $     delrmn,fpr2,r19,rmax
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
      do  kl = 1,ncn2
         optp2 = max(opakc(kl),1.e-24)
         dell=max(optp2*zrems(1,kl),
     $    (rccemis(1,kl)+rccemis(2,kl))*fpr2)
         tst = emult*zrems(1,kl)/(abs(dell)+1.e-24)
         tst = emult/optp2
         if ((epi(kl).gt.ectt).and.(dpthc(1,kl).le.taumax)
     $     .and.(zrems(1,kl).gt.1.e-12)) then
            if ( tst.lt.delr ) klmn = kl
            delr = min(delr,tst)
            endif
         if (lpri.ne.0) write (lun11,*)kl,epi(kl),opakc(kl),zrems(1,kl),
     $          rccemis(1,kl),rccemis(2,kl),fline(1,kl),dell,tst,
     $          dpthc(1,kl),delr
         enddo
      delrmn=(xpxcol-xcol)/xpx
      delr=min(delr,delrmn)
      if ( lpri.ge.1 ) write (lun11,*)'in step',emult,
     &   delr,epi(klmn),ectt,rmax,xpxcol,xpx,xcol,delrmn
      lpri=lprisv
c
c
      return
      end
