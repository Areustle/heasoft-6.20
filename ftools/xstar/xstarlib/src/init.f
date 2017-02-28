      subroutine init(lunlog,bremsa,bremsint,tau0,dpthc,tauc,
     $   xii,rrrt,pirt,htt,cll,httot,cltot,
     $   cllines,clcont,htcomp,clcomp,clbrems,
     $   xilev,rcem,oplin,rccemis,brcems,opakc,opakscatt,
     $   cemab,cabab,opakab,elumab,elumabo,elum,elumo,
     $   zrems,zremso,rates,vsav,idrates,fline,flinel)
c
c     this routine initializes everything
c     author:  T. Kallman
c
      implicit none
c
      include './PARAM'
c
c     line luminosities
      real*8 elum(3,nnnl),elumo(3,nnnl)
c     line emissivities
      real*8 rcem(2,nnnl)
c     line opacities
      real*8 oplin(nnnl)
c     line optical depths
      real*8 tau0(2,nnnl)
      real*8 fline(2,nnnl),flinel(ncn)
c     continuum lum
      real*8 zrems(4,ncn),zremso(4,ncn)
c     continuum optical depths
      real*8 dpthc(2,ncn)
c     continuum flux
      real*8 bremsa(ncn),bremsint(ncn)
c     continuum emissivities
      real*8 rccemis(2,ncn),brcems(ncn)
c     continuum opacities
      real*8 opakc(ncn),opakscatt(ncn)
c     level populations
      real*8 xilev(nnml)
      real*8 cemab(2,nnml),cabab(nnml),opakab(nnml)
      real*8 elumab(2,nnml),elumabo(2,nnml)
      real*8 tauc(2,nnml)
c     ion abundances
      real*8 xii(nni)
c     heating/cooling
      real*8 htt(nni),cll(nni)
      real*8 rrrt(nni),pirt(nni)
      real*8 rates(4,ndat2)
      integer idrates(2,ndat2)
      real*8 vsav(4,ndat2)
      real*8 httot,cltot,cllines,clcont,htcomp,clcomp,clbrems
      integer i,j,lunlog
      character(133) tmpst
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
         flinel(i)=0.
         zrems(1,i)=0.
         zrems(2,i)=0.
         zrems(3,i)=0.
         zrems(4,i)=0.  !jg
         zremso(1,i)=0.
         zremso(2,i)=0.
         zremso(3,i)=0.
         zremso(4,i)=0. !jg
         bremsint(i)=0.
         bremsint(i)=0.
         bremsa(i)=0.
         dpthc(1,i) = 0.
         dpthc(2,i)=0.
c         dpthc(2,i)=1.e+10
         opakc(i)=0.
         opakscatt(i)=0.
         enddo
       do  i = 1,nnnl
         fline(1,i)=0.
         fline(2,i)=0.
         rcem(1,i)=0.
         rcem(2,i)=0.
         elum(1,i)=0.
         elum(2,i)=0.
         elum(3,i)=0.
         elumo(1,i)=0.
         elumo(2,i)=0.
         elumo(3,i)=0.
c         tau0(2,i)=1.e+20
c         tau0(1,i) = 1.e+20
         tau0(2,i)=0.
         tau0(1,i) = 0.
         oplin(i)=0.
         enddo
c      write (lunlog,*)'NB no backward escape'
c      write (tmpst,*)'NB no backward escape'
c      call xwrite(tmpst,10)
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
         cabab(i)=0.
         cemab(1,i)=0.
         cemab(2,i)=0.
         opakab(i)=0.
c         xilev(i)=1.
         xilev(i)=0.
         tauc(1,i) = 0.
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
      return
      end
