      subroutine phint53pl(sth,e1,alph,ethi,pirt,rrrt,piht,rrcl,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,
     $ opakc,rccemis,lpri,epi,ncn2,bremsa,t,swrat,xnx,
     $ lfast,lun11)
c
c
c     this routine does the integration over the spectrum as required by
c     photo.
c     uses power law piecewise analytic integrals, assuming \sigma~e^{-3}
c        for recombination integrals.
c
      implicit none
c
      include './PARAM'
c
      real*8  bremsa(ncn),epi(ncn)
      real*8 rccemis(2,ncn),opakc(ncn)
      real*8 sth,e1,alph
      integer lpri,ncn2,lfast,lun11
      real*8 ethi,pirt,rrrt,piht,rrcl,abund1,abund2,ptmp1,ptmp2,xpx,
     $     opakab,t,swrat,xnx
      real*8 eth,ergsev,bk,tm,bktm,ener,sgtmp,epii,sgtp,optmp,
     $     sumr,sumh,sumi,sumc,tempi,atmp2,
     $     bremtmp,tempr,sumho,exptst,rnist,tsq,ethsht,temphp,
     $     bremtmpp,bbnurjp,exptmpp,rctmp2,rctmp1,wwih,wwic,wwir,
     $     sgtpp,tempcp,tempc,temph,tempip,temprp,wwicp,wwiip,wwii,
     $     wwihp,epiip,wwirp
      integer lprisv,numcon2,nphint,nb1,kl,klmax,nbinc
c
      data ergsev/1.602197e-12/
      data bk/1.38062e-16/
c
      eth=ethi
      lprisv=lpri
c
      tm=t*1.e4
      bktm=bk*tm/ergsev
      tsq = sqrt(t)
      rnist=(2.61e-21)*swrat/t/tsq
c
      if (lpri.ge.1) write (lun11,*)'in phint53pl:',
     $      eth,xnx,swrat,t,
     $      lfast,ptmp1,ptmp2
     $      ,abund1,abund2
      ethsht=eth/bktm
      ethsht=max(ethsht,0.)
c
      numcon2=max(2,ncn2/50)
      nphint=ncn2-numcon2
c
      sumr = 0.
      sumh = 0.
      sumho = 0.
      sumc = 0.
      sumi=0.
      ener=ethi+13.605692*e1
      nb1=nbinc(ener,epi,ncn2)
      do while ((epi(nb1).lt.ener).and.(nb1.lt.nphint))
        nb1=nb1+1
        enddo
      if (lpri.ge.1) write (lun11,*)'nb1=',nb1,ener,nphint
      if (nb1.ge.nphint) return
c      if (epi(kl+1).lt.ethi) return
      tempr=0.
      rctmp1=0.
      rctmp2=0.
c
c     preliminary setup
      sgtpp=sth
      bremtmpp=bremsa(nb1)/(12.56)
      epiip=epi(nb1)
      temprp=(12.56)*sgtpp*bremtmpp/epiip
      temphp=temprp*epiip
      exptst=(epiip-eth)/bktm
      exptmpp=exp(-exptst)
      bbnurjp=epiip*epiip*epiip*(1.571e+22)*2.
      tempip=rnist*(bremtmpp+bbnurjp*(ptmp1+ptmp2))
     $  *sgtpp*exptmpp/epiip
      tempcp=tempip*epiip
      klmax=ncn2
c
      kl=nb1
      epii=epi(kl)
      if (lpri.ne.0) write (lun11,*)'kl=',kl,klmax,sumh,sumho
      do while ((kl.lt.klmax)
     $    .and.(abs(sumh/(sumho+1.e-24)-1.).gt.1.e-6))
c
c       the basics
        sgtmp=max(0.,sth*(epi(kl)/ener)**alph)
        sgtp=sgtmp
        sgtpp=sth*(epi(kl+1)/ener)**alph
        bremtmp=bremsa(kl)/(12.56)
        bremtmpp=bremsa(kl+1)/(12.56)
        epii=epi(kl)
        epiip=epi(kl+1)
c
c       pi rate
        tempr=temprp
        temprp=(12.56)*sgtpp*bremtmpp/epiip
        wwir=(epiip-epii)/2.
        wwirp=wwir
        sumr = sumr + (tempr*wwir+temprp*wwirp)
c
c       heat
        temph=temphp
        temphp=temprp*epiip
        wwih=wwir
        wwihp=wwih
        sumho=sumh
        sumh = sumh + (temph*wwih+temphp*wwihp)
c
c       rec
        exptst=(epiip-eth)/bktm
        if (exptst.lt.30.) then
          bremtmpp=bremsa(kl+1)/(25.3)
          exptmpp=exp(-exptst)
          bbnurjp=epiip*epiip*epiip*(1.571e+22)*2.
          tempi=tempip
          tempip=rnist*(bremtmpp+bbnurjp*(ptmp1+ptmp2))
     $        *sgtpp*exptmpp/epiip
          wwii=wwir
          wwiip=wwir
          sumi = sumi + (tempi*wwii+tempip*wwiip)
c
c         cool
          tempc=tempcp
          tempcp=tempip*epiip
          wwic=wwir
          wwicp=wwir
          sumc = sumc+tempc*wwic+tempcp*wwicp
c
          atmp2=tempc
          rctmp1=abund2*atmp2*ptmp1*xpx*xnx
          rctmp2=abund2*atmp2*ptmp2*xpx*xnx
          rccemis(1,kl)=rccemis(1,kl)+rctmp1
          rccemis(2,kl)=rccemis(2,kl)+rctmp2
c
          endif
c
c       emiss and opac
c       the emission must be fudged to get the right cooling with a
c         trapezoid integration.
        optmp=abund1*xpx*sgtp
        if (kl.le.(nb1+1)) opakab=optmp
        opakc(kl)=opakc(kl)+optmp
c
c       print
         if ((lpri.ge.1).and.(abs(rctmp1+rctmp2).gt.1.e-24)
     $         .and.(ener.gt.6000.))
     $      write (lun11,901)kl,epii,sgtp,bremtmp,
     $         tempr,temprp,wwir,wwirp,sumr,
     $         temph,temphp,wwih,wwihp,sumh,
     $         tempi,tempip,wwii,wwiip,sumi,
     $         tempc,tempcp,wwic,wwicp,sumc
     $         ,rctmp1,rctmp2
 901       format(1x,'found something',i6,25(1pe11.3))
        if (sgtp.lt.0.) then
          write (6,*) 'phint error'
          return
          endif
c
        kl=kl+1
        enddo
c
      pirt = pirt + sumr
      rrrt = rrrt + xnx*sumi
      piht = piht + sumh*ergsev
      rrcl = rrcl + xnx*sumc*ergsev

      if (lpri.ge.1) write (lun11,*)'in phint53:',eth,pirt,rrrt
     $         ,piht,rrcl
      lpri=lprisv
c
c
      return
      end
