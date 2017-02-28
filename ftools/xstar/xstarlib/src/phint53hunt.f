      subroutine phint53hunt(stmpp,etmpp,ntmp,ethi,pirt,rrrt,piht,rrcl,
     $ lpri,epi,ncn2,bremsa,t,swrat,xnx,crit,lfast,lun11)
c
c
c     this routine does the integration over the spectrum as required by
c     photo.
c     this is my version from 11/1/99 which performs successive bisections
c      g stands for good
c     author:  T. Kallman
c
      implicit none
c
      integer ntmp
c
      include './PARAM'
c
      real*8  bremsa(ncn),epi(ncn),stmpp(ntmp),etmpp(ntmp)
      real*8 ansar1(ncn),ansar2(ncn)
      integer luse(ncn)
      integer lpri,ncn2,lfast,lun11
      real*8 ethi,pirt,rrrt,piht,rrcl,
     $     t,swrat,xnx,crit
      real*8 eth,ergsev,bk,tm,bktm,ener,epii,
     $     sumr,sumh,sumi,sumc,tempi,enero,
     $     bremtmp,tempr,tempro,deld,sumho,exptst,
     $     tempi1,tempi2,tempio,atmp2,rnist,atmp2o,bbnurj,delt,
     $     exptmp,emaxx,efnd,ethsht,etst,sumro,sumio,tst3,tst2,
     $     tst,sumco,tst4,tst1,tsq,tsti,sgtmp
      integer lprisv,numcon2,nphint,nb1,kl,itmp,jlo,lprif,
     $     nskp,ndelt,npass,numcon,numcon3,nbinc
c
c
      data ergsev/1.602197e-12/
      data bk/1.38062e-16/
      data delt/1.e-28/
c
c     initialize.
       pirt =0.
       rrrt =0.
       piht=0.
       rrcl=0.
c
      nb1=nbinc(ethi,epi,ncn2)+1
      eth=ethi
      numcon=ncn2
      numcon2=max(2,ncn2/50)
      numcon3=numcon-numcon2
      if (lpri.ge.1) write (lun11,*)'in phint53:',
     $      eth,xnx,swrat,t,nb1,
     $      stmpp(1),etmpp(1),ntmp,lfast
      if (nb1.ge.numcon3) return
c
      tm=t*1.e4
      bktm=bk*tm/ergsev
      tsq = sqrt(t)
      rnist=(5.216e-21)*swrat/t/tsq
c
c     from levwk
      ethsht=eth/bktm
      ethsht=max(ethsht,0.)
c
      lprisv=lpri
c
c     first find range
      kl=numcon3/2
      emaxx=etmpp(ntmp)*(13.605692)+eth
      nphint=nbinc(emaxx,epi,ncn2)
      ndelt=nphint-nb1
      if (lpri.ge.1) write (lun11,*)'in phint53:',
     $      eth,rnist,xnx,swrat,t,ndelt,nphint,nb1,
     $      stmpp(1),etmpp(1),etmpp(ntmp),ntmp,lfast
      ndelt=max(ndelt,1)
      delt=float(ndelt)
      itmp=int(log(delt)/(0.69315)+0.5)
 1011 continue
      ndelt=2**itmp
      nphint=nb1+ndelt
      etst=0.
      if (nphint.le.numcon3)
     $ etst=(epi(nphint)-eth)/13.605692
c      write (lun11,*)ndelt,nphint,etst,numcon3,etmpp(ntmp),ntmp,itmp
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
      nskp=ndelt
      sumr = 0.
      sumh = 0.
      sumc = 0.
      sumi=0.
      npass=0
      do while (((tst3.gt.crit).or.(tst1.gt.crit)
     $    .or.(tst2.gt.crit).or.(tst4.gt.crit).or.(sumi.le.1.e-24))
     $    .and.(nskp.gt.1)) 
        npass=npass+1
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
        do while (kl.le.nphint) 
          enero=ener
          epii=epi(kl)
          ener=epii
          bremtmp=bremsa(kl)/(25.3)
          tempio=tempi
          atmp2o=atmp2
          sgtmp=0.
          if (ener.ge.eth) then
            if (luse(kl).eq.0) then
                efnd=(ener-eth)/13.605692
                lprif=0
                if (lpri.gt.1) lprif=1
                call find53(stmpp,etmpp,ntmp,efnd,sgtmp,jlo,lun11,lprif)
                ansar1(kl)=sgtmp
                if (lprif.ge.1) write (lun11,*)'after find53:',
     $                  jlo,efnd,sgtmp
                exptst=(epii-eth)/bktm
                exptmp=exp(-exptst)
                bbnurj=epii*epii*epii
                tempi1=rnist*bbnurj*sgtmp*exptmp*(1.571e+22)/epii
                tempi2=rnist*bremtmp*sgtmp*exptmp/epii
c                tempi2=0.
                tempi=tempi1+tempi2
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
          if ((lpri.ge.1).and.(npass.le.1)) then
              write (lun11,*)kl,ener,luse(kl),sgtmp,bremtmp,atmp2
              write (lun11,*)kl,ener,bremtmp,bbnurj
              write (lun11,*) tempr,tempi,rnist,exptmp,bbnurj
              write (lun11,*) sumr,sumi,sumh,sumc,sgtmp
              endif
          luse(kl)=1
          kl=kl+nskp
          enddo
c
c
        tst3=abs((sumio-sumi)/(sumio+sumi+1.e-24))
        tst1=abs((sumro-sumr)/(sumro+sumr+1.e-24))
        tst2=abs((sumho-sumh)/(sumho+sumh+1.e-24))
        tst4=abs((sumco-sumc)/(sumco+sumc+1.e-24))
        if (lpri.ge.1) write (lun11,*)'after pass:',npass,
     $     sumr,sumh,sumi,sumc,tst1,tst2,tst3,tst4,nskp
        enddo
c
         pirt = pirt + sumr
         rrrt = rrrt + xnx*sumi
         piht = piht + sumh*ergsev
         rrcl = rrcl + xnx*sumc*ergsev
c
c
         if (lpri.ge.1) write (lun11,*)'in phint53:',eth,pirt,rrrt
     $         ,piht,rrcl,npass,sumc,ergsev
         lpri=lprisv
c
      return
      end
