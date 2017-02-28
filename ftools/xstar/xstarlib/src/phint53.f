      subroutine phint53(stmpp,etmpp,ntmp,ethi,pirt,rrrt,piht,rrcl,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,rnist,
     $ opakc,rccemis,lpri,epi,ncn2,bremsa,t,swrat,xnx,
     $ lfast,lun11)
c
c
c     this routine does the integration over the spectrum as required by
c     photo.
c     uses power law piecewise analytic integrals, assuming \sigma~e^{-3}
c        for recombination integrals.
c     special version for photemis
c
      implicit none
c
      integer ntmp
c
      include './PARAM'
c
      real*8  bremsa(ncn),epi(ncn),stmpp(ntmp),etmpp(ntmp)
      real*8 rccemis(2,ncn),opakc(ncn)
      real*8 sgbar(ncn),bremsint(ncn)
      integer lpri,ncn2,lfast,lun11
      real*8 ethi,pirt,rrrt,piht,rrcl,abund1,abund2,ptmp1,ptmp2,xpx,
     $     opakab,t,swrat,xnx,crit
      real*8 eth,ergsev,bk,tm,bktm,ener,sgtmp,epii,sgtp,optmp,
     $     sumr,sumh,sumi,sumc,tempi,
     $     bremtmp,tempr,sumho,exptst,
     $     atmp2,rnist,tsq,ethsht,optmp2,
     $     wwir,wwih,bbnurjp,e2t,e2,e1,e1o,bremtmpp,
     $     epiip,e2o,tempc,tempcp,exptmpp,rctmp2,rctmp1,
     $     s2,s2to,s2o,s2t,sgtpp,sum,tempip,temphp,temprp,temph,
     $     wwirp,wwicp,wwic,wwii,wwihp,wwiip,enermx,exptsto
      integer lprisv,numcon2,nphint,nb1,kl,jk,nbn,klmax,nbinc
c
      logical done
c
      data ergsev/1.602197e-12/
      data bk/1.38062e-16/
c
c
c     internal to this routine we use the threshold binned
c      eth=ethi
      nb1=nbinc(ethi,epi,ncn2)
      eth=epi(nb1)
      lprisv=lpri
c
      tm=t*1.e4
      bktm=bk*tm/ergsev
      tsq = sqrt(t)
c      rnist=(2.61e-21)*swrat/t/tsq
c
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
      ener=eth+etmpp(1)*(13.605692)
      nb1=nbinc(ener,epi,ncn2)
      if (lpri.ge.1)
     $  write (lun11,*)'in phint53:',
     $      eth,xnx,swrat,t,
     $      stmpp(1),etmpp(1),ntmp,lfast,ptmp1,ptmp2
     $      ,abund1,abund2,rnist,crit
      do while ((epi(nb1).lt.ener).and.(nb1.lt.nphint))
        nb1=nb1+1
        enddo
      nb1=nb1-1
c      if (lpri.ge.1) write (lun11,*)'nb1=',nb1,ener,nphint
      if (nb1.ge.nphint) return
c      if (epi(kl+1).lt.ethi) return
      tempr=0.

c     step through cross section, map onto kl grid
      jk=1
      enermx=eth+etmpp(ntmp)*(13.605692)
      nbn=nbinc(enermx,epi,ncn2)
      nbn=max(nbn,min(nb1+1,ncn2-1))
      ener=eth+etmpp(jk)*(13.605692)
      sgtmp=stmpp(1)
      sgbar(max(1,nb1-1))=0.
      kl=nb1
      jk=1
      e1=epi(kl)
      e2=eth+etmpp(jk)*(13.605692)
      s2=stmpp(jk)
      if (e1.lt.e2) then
        kl=kl+1
        e1=epi(kl)
        endif
      e1o=e2
      sum=0.
      done=.false.
      do while (.not.done)
c       step through where jk grid is finer
c        if (lpri.ne.0) write (lun11,*)'mapping loop:',jk,kl,e2,e1
        do while ((e2.lt.e1).and.(jk.lt.(ntmp-1)))
          jk=jk+1
          e2o=e2
          s2o=s2
          e2=eth+etmpp(jk)*(13.605692)
          s2=stmpp(jk)
          sum=sum+(s2+s2o)*(e2-e2o)/2.
c          if (lpri.ne.0) write (lun11,*)'jk loop',jk,e2,s2,e2o,s2o,sum
          enddo
c       kl bin exceeds jk bin, subtract off extra
        sum=sum-(s2+s2o)*(e2-e2o)/2.
c       now interpolate to find value at kl bin
        e2t=e1
        if (e2-e2o.gt.1.e-8) then
             s2t=s2o+(s2-s2o)*(e2t-e2o)/(e2-e2o+1.e-24)
           else
             s2t=s2o
           endif
        s2to=s2t
c       now update sum at kl bin
        sum=sum+(s2t+s2o)*(e2t-e2o)/2.
c       save
        if (abs(e1-e1o).gt.1.e-34) then
            sgbar(kl)=sum/(e1-e1o)
          else
            sgbar(kl)=0.
          endif
c        if (lpri.ne.0) write (lun11,*)'saving:',kl,e1,sgbar(kl),sum,s2t
        e1o=e1
c       increment kl
        kl=kl+1
        e1=epi(kl)
c       step through where kl grid is finer
        do while ((e1.lt.e2).and.(kl.lt.ncn2))
          e2t=e1
          if (e2-e2o.gt.1.e-8) then
              s2t=s2o+(s2-s2o)*(e2t-e2o)/(e2-e2o)
            else
              s2t=s2o
            endif
          s2to=s2t
          sum=(s2t+s2to)*(e1-e1o)/2.
          sgbar(kl)=sum/(e1-e1o)
c          if (lpri.ne.0) write (lun11,*)'kl loop',kl,e1,
c     $                                   sgbar(kl),sum,s2t
          e1o=e1
          kl=kl+1
          e1=epi(kl)
          enddo
c       update sum for remaining bit
        sum=(s2+s2t)*(e2-e2t)/2.
c        if (lpri.ne.0) write (lun11,*)'testing for done:',kl,nphint,
c     $                                                    jk,ntmp
        if ((kl.gt.nphint-1).or.(jk.ge.ntmp-1))
     $       done=.true.
        enddo
      klmax=kl-1
c
c
c     preliminary setup
      sgtpp=sgbar(nb1)
      bremtmpp=bremsa(nb1)/(12.56)
      epiip=epi(nb1)
      temprp=(12.56)*sgtpp*bremtmpp/epiip
      temphp=temprp*epiip
      exptst=(epiip-eth)/bktm
      exptmpp=exp(-exptst)
      bbnurjp=epiip*epiip*epiip*(1.571e+22)*2.
      tempip=rnist*(bremtmpp+bbnurjp)
     $  *sgtpp*exptmpp/epiip*(ptmp1+ptmp2)
      tempcp=tempip*epiip
c
      kl=nb1
      epii=epi(kl)
c      if (lpri.ne.0) write (lun11,*)'kl=',kl,klmax,sumh,sumho
      rctmp1=0.
      rctmp2=0.
      do while (kl.lt.klmax)
c
c       the basics
        sgtmp=max(0.,sgbar(kl))
        sgtp=sgtmp
        sgtpp=sgbar(kl+1)
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
        exptsto=exptst
        exptst=(epiip-eth)/bktm
        if ((exptsto.lt.200.).and.(lfast.ge.2)) then
          bremtmpp=bremsa(kl+1)/(12.56)
          exptmpp=exp(-exptst)
          bbnurjp=epiip*epiip*epiip*(1.571e+22)*2.
          tempi=tempip
          tempip=rnist*(bremtmpp+bbnurjp)
     $        *sgtpp*exptmpp*12.56/epiip
          atmp2=tempip*epiip
          tempip=tempip*(ptmp1+ptmp2)
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
          rctmp1=abund2*atmp2*ptmp1*xpx/12.56
          rccemis(1,kl)=rccemis(1,kl)+rctmp1
          rctmp2=abund2*atmp2*ptmp2*xpx/12.56
          rccemis(2,kl)=rccemis(2,kl)+rctmp2
c
          endif
c
c       emiss and opac
c       the emission must be fudged to get the right cooling with a
c         trapezoid integration.
        optmp=abund1*xpx*sgtp
        opakc(kl)=opakc(kl)+optmp
c
        if (kl.le.(nb1+2)) then
          optmp2=rnist*exptmpp*sgtp*abund2*(ptmp1+ptmp2)*xpx
          opakab=optmp-optmp2
          if (lpri.ge.1)
     $     write (lun11,*)'optmp,optmp2',optmp,optmp2
          endif
c
c       print
        if (lpri.ge.1) then
          write (lun11,*)jk,kl,epi(kl),kl,nphint
          write (lun11,901)jk,kl,epi(kl),sgtp,bremtmp,tempr,sumr
     $                     ,exptsto,tempi,sumi,tempip,wwir
 901      format(1x,2i6,10(1pe11.3))
          endif
c     $      write (lun11,901)kl,epii,sgtp,bremtmp,
c     $         tempr,temprp,wwir,wwirp,sumr,
c     $         temph,temphp,wwih,wwihp,sumh,
c     $         tempi,tempip,wwii,wwiip,sumi,
c     $         tempc,tempcp,wwic,wwicp,sumc
c     $         ,rctmp1,rctmp2,exptst
c 901       format(1x,'found something',i6,25(1pe11.3))
        if (sgtp.lt.0.) then
          write (6,*) 'phint error'
          return
          endif
c
        kl=kl+1
        enddo
c
      pirt = pirt + sumr
      rrrt = rrrt + sumi
      piht = piht + sumh*ergsev
      rrcl = rrcl + sumc*ergsev

      if (lpri.ge.1) write (lun11,*)'in phint53:',eth,pirt,rrrt
     $         ,piht,rrcl
      lpri=lprisv
c
      do kl=nb1,nbn
        sgbar(kl)=0.
        enddo
c
c
      return
      end
