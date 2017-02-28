       subroutine binemis(lun11,lpri,xlum,
     $       t,vturbi,epi,ncn2,dpthc,
     $       idat1,rdat1,kdat1,nptrs,
     $       npar,npnxt,npfi,
     $       nplin,nlsvn,
     $       eliml,elimh,elum,zrems,zremsz,ilsv,
     $       zrtmp,ewsv,elsv,nlsv)
c
C
      implicit none
c
      include './PARAM'
      integer nbtpp
      parameter (nbtpp=ncn)
c
c
c     passed parameters
      integer lun11
c     master data 
      integer idat1(nidat1),nptrs(nptt,ndat2)
      real*8 rdat1(nrdat1)
      character(1) kdat1(nkdat1)
c     pointers to master data 
      integer npar(ndat2),npnxt(ndat2)
      integer npfi(ntyp,nni)
c     pointers to line data 
      integer nplin(nnnl)
c     line luminosities
      real*8 elum(3,nnnl)
c     energy bins
      real*8 epi(ncn)
c     continuum lum
      real*8 zrems(4,ncn),zremsz(ncn)
c     continuum optical depths
      real*8 dpthc(2,ncn)
      real*8 zrtmp(6,ncn)
      integer kl, nilin, nkdt,nidt,lcon,lrtyp,ltyp,ml
      integer nlsvn, ln, ll, numcon, lnn, nbtmp, nrdt
      integer  verbose
      integer lup,ndtmp,mllz,nkdt2,iion,nitmp,
     $        ltyp2,lrtyp2,lcon2,nrdt2,nidt2,iltmp,mlpar
      real*8 eliml, elimh, elmmtpp, dele, etmp, elin, aasmall
      real*8 dele2,egam,profile,deler,delea,rdat2(20000),vturbi,aatmp
      real*8 optpp,e00,deleepi,etptst,tst,sume,zrsum1,zrsum2,deletpp,
     $        deleused,tmpe,zrtp2,zrtp1,bbb,deleused2,xlum
      integer ml1,mlmin,mlmax,ij,mlm,ldir,mloff,ml1m,ml1mn,mlc,ncut,
     $        ml1mx,ml2,np1k,np1i,np1r,np1i2,ml1max,ml1min
c     arrays containing printout info
      integer ilsv(nnnl)
      real*8 ewsv(nnnl),elsv(nnnl)
      real*8 etpp(nbtpp),zrtpp2(2,nbtpp),zrtmps(2,nbtpp)
      integer ldon(2)
c
      integer lpri,ncn2,nlsv,nelin
      real*8 t,delet,deleturb,deleth,e0,vth,vturb,dpcrit
      real*8 zro
c
c     externally defined functions
      integer nbinc
      real*8 voigte
c
      data dpcrit/1.e-6/
c
      verbose=lpri
c
c     open and prepare the fits file for spectral data
      if(verbose.gt.0) write (lun11,*)'in binemis:'
c
      nlsv=0
c
c     build spectra data tables
      numcon=ncn2
      bbb=vturbi
      do ll=1,ncn2
        zrtmp(4,ll)=0.
        zrtmp(5,ll)=0.
        zrtmps(1,ll)=0.
        zrtmps(2,ll)=0.
        zrtpp2(1,ll)=0.
        zrtpp2(2,ll)=0.
        enddo
      do  lnn=1,nlsvn
c
        ln=lnn
        ml=nplin(ln)
        call drd(ltyp,lrtyp,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,ml-1,
     $          nptrs,0,lun11)
        elin=abs(rdat1(np1r))
        egam=rdat1(np1r+2)
        lup=idat1(np1i+1)
        nilin=npar(ml)
        call drd(ltyp,lrtyp,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,nilin-1,
     $          nptrs,0,lun11)
        nelin=npar(nilin)
        nilin=idat1(np1i+2)
c       get nuclear mass       
        ml=nelin
        call drd(ltyp,lrtyp,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,ml-1,
     $          nptrs,0,lun11)
        aatmp=rdat1(np1r+1)
        elmmtpp=(elum(2,ln)+elum(1,ln))/2.
        if ((lpri.ne.0).and.(elmmtpp.gt.1.e-37))
     $     write (lun11,*)ln,elin,elmmtpp,nilin,nelin,egam,
     $              lup,aatmp
        if (((ln.gt.0).and.(ln.le.nnnl)) 
     $    .and.((elin.gt.eliml).and.(elin.lt.elimh)) 
     $    .and.(elmmtpp.gt.1.e-8*xlum).and.(aatmp.gt.1.e-24)
     $    .and.((nilin.gt.0).and.(nilin.le.nni))
     $    .and.(ltyp.ne.76))
     $       then
c
c         line parameters
          etmp=12398.54/elin
          nbtmp=nbinc(etmp,epi,ncn2)
c
          nlsv=nlsv+1
          ilsv(nlsv)=ln
          ewsv(nlsv)=-elmmtpp/max(1.e-34,zremsz(nbtmp))
          elsv(nlsv)=elmmtpp
          if (lpri.ne.0)
     $      write (lun11,*)'nlsv,ilsv(nlsv),elsv(nlsv):',
     $                       nlsv,ilsv(nlsv),elsv(nlsv)
c
c         find associated type 86 data
          iion=1
          nitmp=npfi(13,iion)
          call drd(ltyp,lrtyp,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,nitmp-1,
     $          nptrs,0,lun11)
          if (lpri.ne.0)
     $      write (lun11,*)'searching for ion'
          do while ((idat1(np1i-1+nidt).ne.nilin).and.(iion.lt.nni))
            iion=iion+1
            nitmp=npfi(13,iion)
            call drd(ltyp,lrtyp,lcon,
     $          nrdt,np1r,nidt,np1i,nkdt,np1k,nitmp-1,
     $          nptrs,0,lun11)
            if (lpri.ne.0)
     $        write (lun11,*)iion,idat1(np1i-1+nidt),nilin,nitmp
            enddo
          ndtmp=npfi(41,iion)
          delea=0.
          if (ndtmp.gt.0) then
            if (lpri.ne.0)
     $        write (lun11,*)'  found ion',lup,ndtmp
            mllz=npar(ndtmp)
            call drd(ltyp2,lrtyp2,lcon2,
     $          nrdt,np1r,nidt,np1i2,nkdt,np1k,ndtmp-1,
     $         nptrs,0,lun11)
            iltmp=idat1(np1i2+1)
            mlpar=mllz
            do while ((ndtmp.ne.0).and.(lup.ne.iltmp)
     $         .and.(mlpar.eq.mllz)) 
               call drd(ltyp2,lrtyp2,lcon2,
     $           nrdt,np1r,nidt,np1i2,nkdt,np1k,ndtmp-1,
     $          nptrs,0,lun11)
              iltmp=idat1(np1i2+1)
              if (lpri.ne.0)
     $           write (lun11,*)'   ',nidt2,iltmp,ndtmp
              ndtmp=npnxt(ndtmp)     
              mlpar=0
              if (ndtmp.ne.0) mlpar=npar(ndtmp)
              enddo
            endif
          if (lup.eq.iltmp) then
            delea=rdat2(3)*(4.14e-15)
            egam=rdat2(4)
            endif
c
c         cheat for narrow line plot
c         delea=0.        
c
c         a list of all the deles
c           delea=auger natural width in eV
c           deleturb=turbulent width
c           deleth=thermal Doppler width
c           dele=thermal+turbulent width
c           deler=radiative natural width
c           dele2=total width, natural + Doppler
c           deletpp=goal of resolution of internal grid=dele/8
c           deleepi=xstar grid spacing
c           deleused=spacing of internal grid=deleepi/int(deleepi/depetpp)
c           delet=energy offset from line center in units of dele (local)
c
c         thermal width quantities
          vth=(1.2e+1)*sqrt(t/aatmp)
          vturb=max(bbb,vth)
          e0=(12398.42)/max(elin,1.e-24)
          deleturb=e0*(vturb/3.e+5)
          deleth=e0*(vth/3.e+5)
c         old expression
c          dele=deleth+deleturb
c         new expression
          dele=sqrt(deleth*deleth+deleturb*deleturb)
          deler=egam*(4.14e-15)
          dele2=delea+deler+dele
          aasmall=(delea+deler)/(1.e-36+dele)/12.56
c
          ml1=nbtmp
          if (lpri.ge.1) write (lun11,*) 
     &   'e0,elin,elum1,elum2,ml1,deleth,delea:',
     &    e0,elin,elum(1,ln),elum(2,ln),ml1,deleth,delea
c
c         calculate profile on temporary grid
          e00=epi(ml1)
          etmp=e0
c         deleepi is the grid spacing of the epi grid
c         deletpp is the physical energy spacing needed 
c           for an accurate integration of the voigt profile
c         ncut is the ratio of these two quantities, 
c           used for rebinning the calculated voigt profile
          deleepi=epi(ml1+1)-epi(ml1)
          deletpp=dele
          ncut=int(deleepi/deletpp)
          ncut=max(ncut,1)
          ncut=min(ncut,nbtpp/10)
          deleused=deleepi/float(ncut)
          mlc=0
          ldir=1
          ldon(1)=0
          ldon(2)=0
          mlmin=nbtpp
          mlmax=1
          ml1min=ncn+1
          ml1max=0
          ml2=nbtpp/2
          if (lpri.ne.0) write (lun11,*)'ncut=',ncut,deleused,deletpp,
     $                                    deleepi
c
c         calculate profile at continuum bin closest to line center
          delet=(e00-etmp)/dele
          if (aasmall.gt.1.e-9) then
              profile=voigte(abs(delet),aasmall)/1.772
            else
              profile=exp(-delet*delet)/1.772
            endif 
          profile=profile/dele/(1.602197e-12)
          etpp(ml2)=e00
          zrtpp2(1,ml2)=elum(1,ln)*profile
          zrtpp2(2,ml2)=elum(2,ln)*profile
          tst=1.
c
c         now put profile on temporary grid
c         work outward in both directions from line center
          do while ((ldon(1)*ldon(2).eq.0).and.(mlc.lt.nbtpp/2)) 
c
            mlc=mlc+1
c
c           alternate directions
            do ij=1,2
              ldir=-ldir
c
c             test to see if done in this direction
              if (ldon(ij).ne.1) then
c
c               index into temporary grid
                mlm=ml2+ldir*mlc

                etptst=e00+float(ldir*mlc)*deleused
c
c               test to see if within allowed range
                if ((mlm.lt.nbtpp).and.(mlm.gt.1)
     $            .and.(etptst.gt.0.).and.(etptst.lt.epi(ncn2))) then
c
c                 calculate index extremes for later use
c                 ml1m is index into epi grid
c                 ml1min and ml1max are extremes of ml1m
c                 mlmin and mlmax are extremes of mlm
                  mlmin=min(mlm,mlmin)
                  mlmax=max(mlm,mlmax)
c
c                 store energy binc
                  etpp(mlm)=e00+float(ldir*mlc)*deleused
c
c                 calculate profile
                  delet=(etpp(mlm)-etmp)/dele
                  if (aasmall.gt.1.e-9) then
                      profile=voigte(abs(delet),aasmall)/1.772
                    else
                      profile=exp(-delet*delet)/1.772
                    endif 
                  profile=profile/dele/(1.602197e-12)
c
                  zrtpp2(1,mlm)=elum(1,ln)*profile
                  zrtpp2(2,mlm)=elum(2,ln)*profile
                  tst=profile
c
c                 print
                  if (lpri.ne.0) write (lun11,*) 'first write',
     $               mlm,etpp(mlm),ij,
     $               deleused,delet,mlmin,mlmax,ml1,
     $               mlc,mloff,mod(mloff,ncut),profile,zrtpp2(2,mlm)
c
c                 end of test for within range
                  endif
c
c               test to see if done in this direction:
c                 profile not too small
c                 index within range
c                 energy within range
c                 within specified number of doppler widths (50)
                if (((tst.lt.dpcrit)
     $               .or.(mlm.le.1).or.(mlm.ge.nbtpp)
     $               .or.(etptst.le.0.).or.(etptst.ge.epi(ncn2))
     $               .or.(mlc.gt.nbtpp)
     $               .or.(abs(delet).gt.max(50.,200.*aasmall)))
     $               .and.(ml1min.lt.ml1-2).and.(ml1max.gt.ml1+2)
     $               .and.(ml1min.ge.1).and.(ml1max.le.ncn))
     $                ldon(ij)=1
c
c               end of test for done in this direction
                endif
c
c             end of loop over directions
              enddo
c
c           end of loop over energies
            enddo
c
c         store into continuum bins
          sume=0.
          zrsum1=0.
          zrsum2=0.
          ml1min=nbinc(etpp(mlmin),epi,ncn2)
          ml1max=nbinc(etpp(mlmax),epi,ncn2)
          ml1m=ml1min
          if (lpri.ne.0) write (lun11,*)'renormalizing profile',
     $       ml2,mlmin,mlmax,ml1,ml1min,ml1max
          mlmin=max(mlmin,2)        
          mlmax=min(mlmax,nbtpp)        
c          ml1m=nbinc(etpp(mlmin),epi,ncn2)
          ml1mx=1
          ml1mn=ncn2
c
c         step through temp grid bins     
c         and  sum over intervals
          do mlm=mlmin+1,mlmax
c
            tmpe=abs(etpp(mlm)-etpp(mlm-1))
            sume=sume+tmpe
            zrsum1=zrsum1+(zrtpp2(1,mlm)+zrtpp2(1,mlm-1))*tmpe/2.
            zrsum2=zrsum2+(zrtpp2(2,mlm)+zrtpp2(2,mlm-1))*tmpe/2.
c            if (lpri.ne.0) write (lun11,*)mlm,etpp(mlm),ml1m,epi(ml1m),
c     $         sume,zrsum1,zrsum2
c
c           test to see if you have reached epi grid boundary
            if (etpp(mlm).gt.epi(ml1m)) then 
c
c             store current sum
              if (mlm.eq.mlmax) ml1m=max(1,ml1m-1)
              if (sume.gt.1.d-24) then
                zrtp2=zrsum2/sume
                zrtp1=zrsum1/sume
                do while ((etpp(mlm).gt.epi(ml1m)).and.(ml1m.lt.ncn2)) 
                  zrtmps(1,ml1m)=zrtp1
                  zrtmps(2,ml1m)=zrtp2
                  if (lpri.ne.0) write (lun11,*)mlm,ml1m,
     $               epi(ml1m),epi(ml1m+1),etpp(mlm),zrtmps(2,ml1m)
                  ml1m=ml1m+1        
                  enddo
                endif
c
c             reset interval sums
              zrsum2=0.
              zrsum1=0.
              sume=0.
c
c             end of test for epi bin boundary
              endif
c
c           end of rebinning loop
            enddo
c
          do ml1m=ml1min,ml1max
            zrtmp(5,ml1m)=zrtmp(5,ml1m)+zrtmps(2,ml1m)
            zrtmp(4,ml1m)=zrtmp(4,ml1m)+zrtmps(1,ml1m)
            if (lpri.ne.0) write (lun11,*)ml1m,
     $           epi(ml1m),zrtmps(1,ml1m),zrtmp(4,ml1m)
            enddo
c
          endif
        enddo

c

      if (lpri.ne.0) write (lun11,*)'after first binemis loop'
      do kl=1,numcon
         if (lpri.ne.0) write (lun11,*)kl,epi(kl),zrems(2,kl),
     $          zrems(3,kl),zrtmp(4,kl),zrtmp(5,kl),dpthc(1,kl),
     $          zremsz(kl)
         zrtmp(4,kl)=zrtmp(4,kl)+zrems(2,kl)
         zrtmp(5,kl)=zrtmp(5,kl)+zrems(3,kl)
         zrems(2,kl)=zrtmp(4,kl)
         zrems(3,kl)=zrtmp(5,kl)
         zrtmp(3,kl)=zremsz(kl)*exp(-dpthc(1,kl))
         zrtmp(2,kl)=zremsz(kl)
         zrtmp(1,kl)=epi(kl)
         zrtmp(6,kl)=zrems(4,kl)
         enddo
c

      return
      end
