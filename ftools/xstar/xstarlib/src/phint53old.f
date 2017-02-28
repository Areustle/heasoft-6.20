      subroutine phint53old(stmpp,etmpp,ntmp,ethi,pirt,rrrt,piht,rrcl,
     $ abund1,abund2,ptmp1,ptmp2,xpx,opakab,rnist,
     $ opakc,rccemis,lpri,epi,ncn2,bremsa,t,swrat,xnx,
     $ lfast,lun11)
c
c
c     this routine does the integration over the spectrum as required by
c     photo.
c     this is my version from 11/1/99 which performs successive bisections
c      g stands for good
c
      implicit none
c
      integer ntmp
c
      include './PARAM'
c
      real*8  bremsa(ncn),epi(ncn),stmpp(ntmp),etmpp(ntmp)
      real*8 rccemis(2,ncn),opakc(ncn)
      integer lpri,ncn2,lfast,lun11
      real*8 ethi,pirt,rrrt,piht,rrcl,abund1,abund2,ptmp1,ptmp2,xpx,
     $     opakab,t,swrat,xnx
      real*8 eth,ergsev,bk,tm,bktm,ener,sgtmp,epii,sgtp,ansar1,optmp,
     $     sumr,sumh,sumi,sumc,tempi,atmp2,enero,sgtmpo,dels,
     $     sgmn,sgmx,bremtmp,tempr,tempro,epiio,deld,sumho,exptst,
     $     tempi1,tempi2,tempio,rnist,tsq,tempi1o,tempi2o,optmp2,
     $     atmp2o,bbnurj,exptmp,atmp1,atmp1o,bbnu,sss,xee,rcctot,
     $     rccemiso,expo
      integer lprisv,numcon2,nphint,nb1,kl,jk,
     $     nbinc,jkp
c
      data ergsev/1.602197e-12/
      data bk/1.38062e-16/
c
c     some constants
      eth=ethi
      lprisv=lpri
      tm=t*1.e4
      bktm=bk*tm/ergsev
      tsq = sqrt(t)
c     this constant has 4 pi built in
c      rnist=(2.61e-21)*swrat/t/tsq
c
c      print input parameters
      if (lpri.ge.1) write (lun11,*)'in phint53:',
     $      eth,rnist,xnx,swrat,t,
     $      stmpp(1),etmpp(1),ntmp,lfast,abund1,abund2
     $      ,ptmp1,ptmp2
c
c     limits of energy indeces
      numcon2=max(2,ncn2/50)
      nphint=ncn2-numcon2
c
c     zero temporaries
      sumr = 0.
      sumh = 0.
      sumho=0.
      tempr = 0.
      sumc = 0.
      sumi=0.
      tempi=0.
      atmp1=0.
      atmp2=0.
c
c     starting values for integration
      ener=ethi+etmpp(1)*(13.605692)
      nb1=nbinc(ener,epi,ncn2)
      kl=nb1
      if (kl.ge.nphint) return
      if (epi(kl+1).lt.ethi) return
      sgtmp=stmpp(1)
      dels=0.
      epii=epi(kl)
      jk=0
      rcctot=0.
      jkp=min(jk+1,ntmp)
c
c     step through cross section indeces
      do while ((jk.lt.ntmp).and.
     $          (ethi+etmpp(jkp)*(13.605692).lt.epi(nphint-1)))
c     $    .and.(abs(sumh/(sumho+1.e-24)-1.).gt.1.e-6))
c
c       get cross section dependent quantities
        jk=jk+1
        enero=ener
        ener=ethi+etmpp(jk)*(13.605692)
        sgtmpo=sgtmp
        sgtmp=stmpp(jk)
c
c       test for whether cross section grid energy
c         is greater than master energy grid energy
        if (epi(kl+1).lt.ener) then
c
c         calculate derivitive for interpolation
          dels=(sgtmp-sgtmpo)/(ener-enero+1.e-24)
          sgmn=min(sgtmp,sgtmpo)
          sgmx=max(sgtmp,sgtmpo)
c
c         step through photon energies
          do while ((kl.lt.nphint).and.(epi(kl+1).lt.ener))
c
c           the cross section
            kl=kl+1
            sgtp=max(sgmn,min(sgmx,sgtmpo+dels*(epi(kl)-enero)))
            ansar1=sgtp
c
c           the photoionization rate
            bremtmp=bremsa(kl)/(12.56)
            tempro=tempr
            epiio=epii
            epii=epi(kl)
            tempr=(12.56)*sgtp*bremtmp/epii
            deld = epii - epiio
            sumr = sumr + (tempr+tempro)*deld/2.
c
c           the photoionization heating rate
            sumho=sumh
            sumh=sumh+(tempr*epii+tempro*epiio)*deld/2.
c
c           skip if lfast=1
            if (lfast.gt.1) then
c
c             quantities associated with recombination
c             this expression causes trouble when the threshold is not 
c              at eth
c              exptst=(-eth+epii)/bktm
c             this is a possible fix
              exptst=(-(eth+max(0.,etmpp(1)*(13.605692)))+epii)/bktm
              exptmp=0.
c
c             test for exponential in rrc
              if (exptst.lt.200.) then
c
                exptmp=expo(-exptst)
                bbnurj=epii*epii*epii*(1.571e+22)*2.
                tempi1=rnist*bbnurj*exptmp*sgtp*12.56/epii
                tempi2=rnist*bremtmp*exptmp*sgtp*12.56/epii
                tempi1=tempi1*(ptmp1+ptmp2)
                tempi2=tempi2*(ptmp1+ptmp2)
                tempi1o=tempi1
                tempi2o=tempi2
                Atmp1o=atmp1
                Atmp2o=atmp2
c               the old way with stimulated recombination treated as emission
c                tempi=(tempi1+tempi2)*(ptmp1+ptmp2)
                atmp1=tempi1*epii
                atmp2=tempi2*epii
c
c               the recombination rate
                sumi = sumi + (tempi1+tempi1o+tempi2+tempi2o)*deld/2.
c
c               the recombination cooling rate
                sumc = sumc+(atmp1+atmp1o+atmp2+atmp2o)*deld/2.
c
c               the rrc emissivity
                rccemis(1,kl)=rccemis(1,kl)+
     $                 abund2*atmp1*ptmp1*xpx/12.56
                rccemis(2,kl)=rccemis(2,kl)+
     $                abund2*atmp1*ptmp2*xpx/12.56
c
c               total rrc emissivity
                if (kl.gt.1)
     $           rcctot=rcctot+(rccemis(1,kl)+rccemis(1,kl-1)+
     $                       rccemis(2,kl)+rccemis(2,kl-1))
     $              *(epi(kl)-epi(kl-1))*(1.602197e-12)*12.56/2.
c
c               end of test for exponential in rrc
                endif
c
              if (kl.le.(nb1+1)) then
                optmp=abund1*ansar1*xpx
c               need to take out the 4 pi
                optmp2=rnist*exptmp*sgtp*abund2*(ptmp1+ptmp2)*xpx
                opakab=optmp-optmp2
                endif

c             test for lfast=3
              if (lfast.gt.2) then
c
c               opacity
                optmp=abund1*ansar1*xpx
c               need to take out the 4 pi
                optmp2=rnist*exptmp*sgtp*abund2*(ptmp1+ptmp2)*xpx
                if (kl.le.(nb1+1)) opakab=optmp-optmp2
c               new way treating stimulated recombination as negative absorption
                opakc(kl)=opakc(kl)+max(0.,optmp-optmp2)
c
c               the source function and photon occupation number
                bbnu=bbnurj/(expo(epii/bktm)-1.)
c               for a successful comparison must write s in sr^-1
                sss=(rccemis(1,kl)+rccemis(2,kl))/(1.e-34+opakc(kl))
c
c               end of test for lfast=3
                endif
c
c             end of test for lfast=1
              endif
c
c           diagnostic print
            if (lpri.ge.1)
     $        write (lun11,902)kl,jk,epi(kl),sgtp,bbnurj,bremtmp,bbnu,
     $          atmp2,tempr,tempi1,tempi2,sumi,optmp,optmp2,
     $          opakc(kl),rccemis(1,kl),rccemis(2,kl),
     $          sss,sss/(1.e-34+bbnu),
     $          rcctot,rcctot/abund2/xpx,sumc*ergsev,exptst,exptmp
 902        format (1x,2i6,23(1pe12.4))
c
c           end of loop over master energy grid
            enddo
c
c         end of test for energy grid points
          endif
c
c       end of loop over cross section grid
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
c
      return
      end
