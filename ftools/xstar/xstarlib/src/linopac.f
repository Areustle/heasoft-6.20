      subroutine linopac(lprie,lun11,optpp,ans2,sigvtherm,vtherm,bremsa,
     $                   rcem1,rcem2,elin,vturbi,t,aatmp,delea,epi,ncn2,
     $                   opakc,opakscatt,rccemis,fline,lfast)
c
      implicit none
c
c     this routine puts line opacity into continuum bins
c     author:  T. Kallman
c 
c
      include './PARAM'
      integer nbtpp
      parameter (nbtpp=20000)
c
      real*8 epi(ncn),opakc(ncn),dpthc(2,ncn),opakscatt(ncn)
     $       ,bremsa(ncn)
      integer ldon(2)
      real*8  rccemis(2,ncn),fline(2,nnnl)
c
      real*8  prftmp,sum,rcem1,rcem2,ans2,sigvtherm,vtherm
c
      real*8 vturbi,delr,t,eliml,elimh,
     $  dpcrit,bbb,optpp,delea,aatmp,elin,etmp,vth,
     $  vturb,deleturb,deleth,dele,aasmall,
     $  deleused,deleepi,delet,deletpp,e00,dpthmx,
     $  dpthtmp,e0,tst,opsum,optmpo,profile,optp2,
     $  tmpopmx,tmpopo,etptst,opsv4,sum2,optmp2,optmp2o
      integer ldirt,lpri,lun11,
     $  ncn2,llk,lnn,ln,ml,lup,nilin,nelin,
     $  nbtmp,iion,nitmp,ndtmp,mllz,
     $  iltmp,i,ij,lind,
     $  lcon,ldir,mlm,ml1m,ltyp,lrtyp,ml1,ml2,ml1min,
     $  ml1max,mlc,mloff,mlmin,mlmax,ncut,nidt,
     $  nkdt,nrdt,np2,ncsvn,lprie
      integer nbinc,mlpar,lfast
      real*8 voigte
      integer np1i,np1r,np1k
c
c     temporary grid for use in calculating profile
      real*8 etpp(nbtpp),optpp2(nbtpp),optmp(ncn)
c
c      real*8  tmpew,tmpewo,tmpop,tmpe,sum,sume
      real*8 tmpew,tmpewo,tmpop,tmpe,sume,rnormchk,ergsev
c
      data dpcrit/1.e-6/,ergsev/1.602197e-12/
c
c
      lpri=lprie
c      lpri=0
c
c     test whether line is in range
      if ((elin.gt.1.e+8).or.(elin.lt.1.)) return
c
c     for scattering model, add in line opacity
      bbb=vturbi
      elin=abs(elin)
c     thermal width quantities
      vth=(1.29E+1)*sqrt(t/aatmp)
      vturb=bbb
c      e0=(12398.41)/max(elin,1.E-24)
      e0=(12398.41)/max(elin,1.E-24)
      if (e0.le.epi(1)) return
      deleturb=e0*(vturb/3.E+5)
      deleth=e0*(vth/3.E+5)
c     old expression
c     dele=deleth+deleturb
c     new expression
      dele=sqrt(deleth*deleth+deleturb*deleturb)
      aasmall=delea/(1.E-24+dele)/12.56
c
c     continuum bin for line
      ml1=nbinc(e0,epi,ncn2)
      ml1=max(min(ncn-1,ml1),2)
c
c     here is what we do to get the heating right
      prftmp=2./(epi(ml1+1)-epi(ml1-1))
      opsv4=optpp*dele
c
c     print line quantities
      if (lpri.ge.1) write (lun11,*) 
     &   'e0,optpp,dpcrit*opakc(ml1),ml1,deleth,delea:',
     &    e0,optpp, dpcrit*opakc(ml1),ml1,deleth,delea
      if (lpri.ge.1) write (lun11,*)optpp,prftmp,opsv4,dele,
     $       opsv4*prftmp,rcem1,rcem2
c
c     test for simple calculation
      if (lfast.gt.2) then
c
c         single bin calculation
          opakc(ml1)=opakc(ml1)+opsv4*prftmp
          opakscatt(ml1)=opakscatt(ml1)+opsv4*prftmp
          rccemis(1,ml1)=rccemis(1,ml1)+rcem1*prftmp/ergsev/12.56
          rccemis(2,ml1)=rccemis(2,ml1)+rcem2*prftmp/ergsev/12.56
          return
c
c       full profile calculation
        else
c
c         calculate profile on temporary grid
c         set up temporary grid
          e00=epi(ml1)
          etmp=e0
c         deleepi is the grid spacing of the epi grid
c         deletpp is the physical energy spacing needed 
c           for an accurate integration of the voigt profile
c         ncut is the ratio of these two quantities, 
c           used for rebinning the calculated voigt profile
          deleepi=epi(ml1+1)-epi(ml1)
c         expanding step to make broader lines
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
          if (lpri.ge.1) write (lun11,*)'ncut=',ncut,deleused,deletpp,
     $                                  deleepi
c
c         calculate profile at continuum bin closest to line center
          delet=(e00-etmp)/dele
          if (aasmall.gt.1.e-6) then
              profile=voigte(abs(delet),aasmall)/1.772
            else
              profile=exp(-delet*delet)/1.772
            endif 
          etpp(ml2)=e00
          optpp2(ml2)=optpp*profile
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
c
c               energy of temporary grid point
                etptst=e00+float(ldir*mlc)*deleused
c
c               test to see if within allowed range
                if ((mlm.le.nbtpp).and.(mlm.ge.1)
     $           .and.(etptst.gt.0.).and.(etptst.lt.epi(ncn2))) then
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

c                 calculate profile
                  delet=(etpp(mlm)-etmp)/dele
                  if (aasmall.gt.1.e-9) then
                      profile=voigte(abs(delet),aasmall)/1.772
                    else
                      profile=exp(-delet*delet)/1.772
                    endif 
c
c                 calculate opacity
                  optpp2(mlm)=optpp*profile
c                  tst=optpp2(mlm)*delr
                  tst=profile
c
c                 print
                  if (lpri.ge.1) write (lun11,*) 'first write',
     $             mlm,etpp(mlm),ij,
     $             deleused,delet,mlmin,mlmax,ml1,
     $             mlc,mloff,mod(mloff,ncut),profile,optpp2(mlm),
     $             tst
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
          sum=0.
          opsum=0.
          tmpop=0.
          tmpopmx=0.
          sume=0.
          ml1min=nbinc(etpp(mlmin),epi,ncn2)
          ml1max=nbinc(etpp(mlmax),epi,ncn2)
          ml1m=ml1min
          if (lpri.ge.1) write (lun11,*)'renormalizing profile',
     $       ml2,mlmin,mlmax,ml1m,ml1min,ml1max
          tmpew=0.
          mlmin=max(mlmin,2)        
          mlmax=min(mlmax,nbtpp)   
c
c         step through temp grid bins     
c         and  sum over intervals
          do mlm=mlmin+1,mlmax
c
            tmpopo=tmpop
            tmpop=optpp2(mlm)
            tmpopmx=max(tmpopmx,tmpop)
            tmpe=abs(etpp(mlm)-etpp(mlm-1))
c
c           update interval sum             
            sume=sume+tmpe
            opsum=opsum+(tmpop+tmpopo)*tmpe/2.
c
c           test to see if you have reached epi grid boundary
            if (etpp(mlm).gt.epi(ml1m)) then                  
c
c             store current sum
              optmpo=opakc(ml1m)
              if (sume.gt.1.d-34) then
                optp2=opsum/sume
               do while ((etpp(mlm).gt.epi(ml1m)).and.(ml1m.lt.ncn2))  
                  opakc(ml1m)=opakc(ml1m)+optp2
c                 print
                  if (lpri.ge.1) write (lun11,*)mlm,ml1m,
     $             epi(ml1m),epi(ml1m+1),etpp(mlm),opakc(ml1m),
     $               optmpo,optpp2(mlm),opsum,sume
                  ml1m=ml1m+1
                  enddo
                endif
c
c             reset interval sums
              tmpopmx=0.
              opsum=0.
              sume=0.
c
c             end of test for epi bin boundary
              endif
c
c           end of rebinning loop
            enddo
 9000     continue
c
c         norm check
c          rnormchk=ewsv(nlsv)/optpp/dele/(1.e-34+delr)
c          if (lpri.ne.0) write (lun11,*)'norm check',nilin,elin,optpp,
c     $          dele,aasmall,ewsv(nlsv),rnormchk
c
c       end of test for fast calculation
        endif
c
c
      return
      end
