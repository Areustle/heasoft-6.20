c
c     this program translates ascii xout177p.lis to binary xplot.out
c
      parameter (nidat1=1100000,nrdat1=1100000,nkdat1=1100000,
     $           nptt=10,ndat2=100000)
      parameter (ntyp=70)
      parameter (nnnl=1100000,nnml=1100000,nni=168,nl=13)
c
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
      common /times/tread,tloop,tfunc,trates1,tlcalc,trates2
c
c     master data
      dimension rdat(2000),idat(2000)
      dimension ipass(ndat2*nptt)
      character(1) kdat(2000)
      character(1) kdat1(nkdat1)
      character(10) knam
c
c
      dimension a(2)
c
      equivalence (ipass(11),nptrs(1,1))
c
       ct=etime(a)
c      go to 9098
      np1r=1
      np1i=1
      np1k=1
      np2=1
      mml=0
 1      continue
        mml=mml+1
c        write (6,*)'mml=',mml
        call dreado(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,lerr,mml)
c        write (6,*)'after dreado:',ltyp,lrtyp,lcon,
c     $  nrdat,nidat,nkdat,mml,idat(nidat),
c     $  np1r,np1i,np1k,np2
c        write (6,9101)(rdat(mm),mm=1,nrdat)
 9101   format (1x,10(1pe10.3))
c        write (6,9102)(idat(mm),mm=1,nidat)
 9102   format (1x,10i6)
c        write (6,9103)(kdat(mm),mm=1,nkdat)
 9103   format (1x,132a1)
        if (lerr.ne.0) go to 2
        call dprint(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,
     $  np1r,np1i,np1k,np2,
     $  idat1,rdat1,kdat1,nptrs,lpri,lun11)
        go to 1
 2    continue
 9098 continue
c
c
c     write out
      ndat1=nidat1     
      knam='atdat.fits'
      call writeimage(knam)
      call writebintabl3(idat1,rdat1,kdat1,ndat1,knam)
      write (6,*)'first write ok'
      ipass(1)=np1i
      ipass(2)=np1r
      ipass(3)=np1k
      ipass(4)=np2
c      write (6,901)((nptrs(mm,nn),mm=1,10),nn=1,2)
 901  format (10i6)
c      write (6,901)(ipass(mm),mm=1,30)
c      write (6,*)'before second write',np2
      knam='aptrs.fits'
      call writeimage(knam)
      ndt2=ndat2*nptt
      call writebintabl1(ipass,ndt2,knam)
      write (6,*)'second write ok'

      go to 9099
      open(unit=9,file='atdat.dat',form='unformatted')
      write (9)idat1
      write (9)rdat1
      write (9)kdat1
      write (9)nptrs,np1i,np1r,np1k,np2
 9099 continue
        write (6,*)'data read',np1i,np1r,np1k,np2
       ct2=etime(a)
      ttt=abs(ct-ct2)
      write (6,*)'total time=',ttt
c
      stop
      end
      subroutine dread(ltyp,lrtyp,lcon,
     $  lrdat,rdat,lidat,idat,lkdat,kdat,np2,
     $  idat1,rdat1,kdat1,nptrs,lpri,lun11)
c
      parameter (ntyp=70)
      parameter (nidat1=1100000,nrdat1=1100000,nkdat1=1100000,
     $           nptt=10,ndat2=100000)
c
      common /times/tread,tloop,tfunc,trates1,thcor,trates2
c
c     master data 
      dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
      character(1) kdat1(nkdat1)
c
      dimension rdat(2000),idat(2000)
c
      character(1) kdat(2000)
c
      call remtms(tt0)
c
      if (lpri.ne.0)
     $ write (lun11,*)'in dread, np1,np2=',np2,ltyp,ntyp

c      if ((ltyp.le.0).or.(ltyp.gt.ntyp))
c     $    stop 'data typing error'
      mlr=0
      mli=0
      mlk=0
      nrd=0
 101    continue
        np2=np2+1
        nrd=nrd+1
        np1=nptrs(1,np2)
        ltyp=nptrs(2,np2)
        lrdat=nptrs(5,np2)
        lidat=nptrs(6,np2)
        lkdat=nptrs(7,np2)
        lrtyp=nptrs(3,np2)
        lcon=nptrs(4,np2)
        np1r=nptrs(8,np2)
        np1i=nptrs(9,np2)
        np1k=nptrs(10,np2)
        if (lpri.ne.0)
     $   write (lun11,*)'in dread:',np2,np1,ltyp,lrtyp,lrdat,lidat
        if (lpri.ne.0)
     $   write (lun11,982)lkdat,lcon,np1r,np1i,np1k
 982    format (8x,5i8)
        if (lrdat.ne.0) then
        do ml=1,lrdat
           mlr=mlr+1
           rdat(mlr)=rdat1(np1r)
           np1r=np1r+1
c           write (lun11,*)mlr,np1r,rdat1(np1r),rdat(mlr)
           enddo
        if (lpri.ne.0)
     $   write (lun11,*)'rdat=',(rdat(mm),mm=1,lrdat),np2
        endif
       if (lidat.ne.0) then
         do ml=1,lidat
           mli=mli+1           
           idat(mli)=idat1(np1i)
           np1i=np1i+1
c           write (lun11,*)mli,np1i,idat1(np1i),idat(mli),np2
           enddo
 9825   format (10i6)
        if (lpri.ne.0)
     $   write (lun11,*)'idat=',(idat(mm),mm=1,lidat)
        endif
       if (lkdat.ne.0) then
        do ml=1,lkdat
           mlk=mlk+1           
           kdat(mlk)=kdat1(np1k)
           np1k=np1k+1
c           write (lun11,*)mlk,np1k,kdat1(np1k),kdat(mlk),np2
           enddo
        if (lpri.ne.0)
     $   write (lun11,*)'kdat=',(kdat(mm),mm=1,lkdat)
        endif
        if (lcon.ne.0) go to 101
c      np2=np2-nrd+1
      lidat=mli
      lrdat=mlr
      lkdat=mlk
      lcon=0
c
c     the last pointer has to point to the next empty space.
c      nptr1(np2+1)=np1+1
c
c
      call remtms(tt1)
      tread=tread+abs(tt1-tt0)
c
      if (lpri.ne.0) write (lun11,*)'leaving dread',np2
c
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
      subroutine dreado(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,lerr,mml)
c
      save leno,lenoo
c
      dimension rdat(2000),idat(2000)
      character(1) kdat(2000)
      character(100000) kdtt,kdtt2
      character(256) kdtb,kblnk256
      character(1) kblnk,ktst,kperc,knum,ktsto
c
      data kblnk/' '/,kperc/'%'/,knum/'#'/
      data kblnk256/'                                                  
     $                                                                 
     $                                                                 
     $                                                                 
     $      '/   
      data leno/100000/
      data lenoo/100000/
c

c      write (6,*)'in dreado'
      lerr=0
      llb=0
c      do ll=1,leno
c        write (kdtt(ll:ll),'(a1)')kblnk 
c        enddo
 104  continue
c      read (5,*)kdtt
      read (5,911)kdtt
 911  format (a256)
      read (kdtt(1:1),'(a1)')ktst
      if (ktst.eq.kperc) ldon=1
      read (kdtt(2:2),'(a1)')ktst
      if (ktst.eq.kperc) ldon=ldon+1
      if (ldon.eq.2) lerr=2
      if (ldon.eq.2) return
      ll2=ll2+1
      ll2=lenact(kdtt)
      read (kdtt(ll2:ll2),'(a1)')ktst
 1822 continue
      if (ktst.ne.kperc) then
        ll2=ll2+1
c        do ll=1,lenoo
c          write (kdtt2(ll:ll),'(a1)')kblnk 
c          enddo
        read (5,911)kdtt2
c        read (5,*)kdtt2
        ll3=lenact(kdtt2)
        lenoo=ll3
        do llt=1,ll3
          read (kdtt2(llt:llt),'(a1)')ktst
          ll2=ll2+1
          write (kdtt(ll2:ll2),'(a1)')ktst
          enddo
        go to 1822
        endif
      leno=ll2
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
      nktmp=1
      kdat(nktmp)=kblnk
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
c
c
c
c
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
c       lpri=2
       if (lpri.gt.2) write (6,*) 'in rdflo',ind
       if (lpri.gt.2) write (6,*)qry
       ist=1
c       ind=index(qry,' ')
       if (lpri.gt.2) write (6,9901)ind
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
       if (lpri.gt.2) write (6,9905)lfnde,lfndd
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
       if (lpri.gt.2) write (6,*)'scanning mantissa'
       if (lfnde.ne.0) ind2=lfnde-1
       if (lpri.gt.2) write (6,*)'ind,ind2,lfnde,lfndb:',
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
          if (lpri.gt.2) write (6,9907)kk,itmp,iexp,sum
9907      format (1x,' kk,itmp,iexp,sum ',3i8,e12.4)
          if (kk.lt.ind2) go to 104
       flo=sum*float(lneg)
       ios=0
       if (lfnde.eq.0) return
c
c      scan for exponent
       if (lpri.gt.2) write (6,*)'scanning exponent'
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
          if (lpri.gt.2) write (6,9907)kk,itmp,iexp,sum2
          if (kk.lt.ind) go to 105
       flo2=sum2*float(lneg)
       flo=flo*10.**flo2
       if (lpri.gt.2) write (6,*)'returning:',flo2,flo
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
      subroutine rdflo2(flo,qry,ios,lpri,ind)
 
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
       if (lpri.gt.2) write (6,*) 'in rdflo',ind
       if (lpri.gt.2) write (6,*)qry
       ist=1
c       ind=index(qry,' ')
       if (lpri.gt.2) write (6,9901)ind
9901   format (1x,' ind=',i4)
c
c      scan for e and dot
9904   format (1h ,i4,2x,a12)
       kk=ind
       lfnde=0
       lfndd=0
       lnon=0
102    kk=kk+1
       read (qry(kk:kk),'(a1)') qtmp
       if (qtmp.eq.'.') lfndd=kk
       if ((qtmp.eq.'e').or.(qtmp.eq.'e')) lfnde=kk
       lfnd=0
       do 3011 ll=1,16
         if (qtmp.eq.chtst(ll)) lfnd=1
 3011    continue
       if ((qtmp.ne.' ').and.(kk.lt.lfnde+3)) go to 102
       if (lfnd.eq.0) lnon=1
c       if (kk.lt.ind) go to 102
       if (lpri.gt.2) write (6,9905)lfnde,lfndd
9905   format (1x,' e and d switches',2i4)
       if (lfndd.eq.0) go to 10
       if (lnon.ne.0) go to 1011
c
c
c      decide if negative
       idec=lfndd
       lneg=1
       kk=ind+1
       read (qry(kk:kk),'(a1)') qtmp
       if (qtmp.ne.'-') go to 1103
          kk=kk+1
          lneg=-1
1103      continue
       if (qtmp.eq.'+') kk=kk+1
c
c
c      scan for mantissa
       ind2=ind
       if (lpri.gt.2) write (6,*)'scanning mantissa',
     $          kk,idec,lfnde
       if (lfnde.ne.0) ind2=lfnde-1
       sum=0.
       iexp=-1-(kk-idec-1)
       kk=kk-1
104       kk=kk+1
          if (kk.eq.idec) go to 301
          iexp=iexp-1
          read (qry(kk:kk),'(i1)') itmp
          sum=sum+float(itmp)*10.**iexp
301       continue
          if (lpri.gt.2) write (6,9907)kk,itmp,iexp,sum
9907      format (1x,' kk,itmp,iexp,sum ',3i8,e12.4)
          if (kk.lt.ind2) go to 104
       flo=sum*float(lneg)
       ios=0
       if (lfnde.eq.0) return
c
c      scan for exponent
       if (lpri.gt.2) write (6,*)'scanning exponent'
       ind3=lfnde+1
       kk=ind3
       lneg=1
       sum2=0.
       read (qry(kk:kk),'(a1)') qtmp
       write (6,*)qtmp,kk
       if (qtmp.ne.'-') go to 1104
          kk=kk+1
          lneg=-1
 1104     continue
       if (qtmp.eq.'+') kk=kk+1
       iexp=3-(kk-ind3)
       kk=kk-1
 105      kk=kk+1
          iexp=iexp-1
          read (qry(kk:kk),'(i1)') itmp
          sum2=sum2+float(itmp)*10.**iexp
          if (lpri.gt.2) write (6,9907)kk,itmp,iexp,sum2
          if (kk.lt.ind3+2) go to 105
       flo2=sum2*float(lneg)
       flo=flo*10.**flo2
       ind=kk+1
       if (lpri.gt.2) write (6,*)'returning:',flo2,flo
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
       subroutine rdflon(flo,qry,ios,lpri,ind)
 
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
       if (lpri.gt.2) write (6,*) 'in rdflo',ind
       if (lpri.gt.2) write (6,*)qry
       ist=1
c       ind=index(qry,' ')
       if (lpri.gt.2) write (6,9901)ind
9901   format (1x,' ind=',i4)
c
c      scan for e and dot
9904   format (1h ,i4,2x,a12)
       kk=0
       lfnde=0
       lfndd=0
       lnon=0
102    kk=kk+1
       if (kk.ge.72) return
       read (qry(kk:kk),'(a1)') qtmp
       if (qtmp.eq.'.') lfndd=kk
       if ((qtmp.eq.'e').or.(qtmp.eq.'e')) lfnde=kk
       lfnd=0
       do 3011 ll=1,16
         if (qtmp.eq.chtst(ll)) lfnd=1
 3011    continue
       if (lfnd.eq.0) lnon=1
       if (kk.lt.ind) go to 102
       if (lpri.gt.2) write (6,9905)lfnde,lfndd
9905   format (1x,' e and d switches',2i4)
       if (lfndd.eq.0) go to 10
       if (lnon.ne.0) go to 1011
c
c
c      decide if negative
       idec=lfndd
       lneg=1
       kk=1
       read (qry(kk:kk),'(a1)') qtmp
       if (qtmp.ne.'-') go to 1103
          kk=kk+1
          lneg=-1
1103      continue
       if (qtmp.eq.'+') kk=kk+1
c
c
c      scan for mantissa
       ind2=ind
       if (lpri.gt.2) write (6,*)'scanning mantissa'
       if (lfnde.ne.0) ind2=lfnde-1
       sum=0.
       iexp=-1-(kk-idec-1)
       kk=kk-1
104       kk=kk+1
          if (kk.eq.idec) go to 301
          iexp=iexp-1
          read (qry(kk:kk),'(i1)') itmp
          sum=sum+float(itmp)*10.**iexp
301       continue
          if (lpri.gt.2) write (6,9907)kk,itmp,iexp,sum
9907      format (1x,' kk,itmp,iexp,sum ',3i8,e12.4)
          if (kk.lt.ind2) go to 104
       flo=sum*float(lneg)
       ios=0
       if (lfnde.eq.0) return
c
c      scan for exponent
       if (lpri.gt.2) write (6,*)'scanning exponent'
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
          if (lpri.gt.2) write (6,9907)kk,itmp,iexp,sum2
          if (kk.lt.ind) go to 105
       flo2=sum2*float(lneg)
       flo=flo*10.**flo2
       if (lpri.gt.2) write (6,*)'returning:',flo2,flo
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
c       lpri=2
       if (lpri.gt.2) write (6,*) 'in rdint',ind
       if (lpri.gt.2) write (6,*)qry
       ist=1
c       ind=index(qry,' ')
       if (lpri.gt.2) write (6,9901)ind
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
       if (lpri.gt.2) write (6,*)'scanning mantissa'
       isum=0
       iexp=-1-(kk-idec-1)
       kk=kk-1
104       kk=kk+1
          iexp=iexp-1
          read (qry(kk:kk),'(i1)') itmp
          isum=isum+itmp*10**iexp
301       continue
          if (lpri.gt.2) write (6,9907)kk,itmp,iexp,isum
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
       subroutine rdint2(lo,qry,ios,lpri,ind)
 
c       reads integer from file param corresponding to character string
c       'char'. if no integer there, prompts user for parameter ...
c
       character(1) qtmp
       character(72) qry
       character(1) chtst(15)
c
       data chtst/'0','1','2','3','4','5','6','7','8','9','+','-',
     $           ' ','.','e'/
c
       if (lpri.gt.2) write (6,*) 'in rdint',ind
       if (lpri.gt.2) write (6,*)qry
       ist=1
c       ind=index(qry,' ')
       if (lpri.gt.2) write (6,9901)ind
9901   format (1x,' ind=',i4)
c
c      scan for e and dot
9904   format (1h ,i4,2x,a12)
       kk=ind
       lnon=0
102    kk=kk+1
       if (kk.ge.72) return
       read (qry(kk:kk),'(a1)') qtmp
       lfnd=0
       do 3011 ll=1,13
         if (qtmp.eq.chtst(ll)) lfnd=1
 3011    continue
       if (lfnd.eq.0) lnon=1
       if (qtmp.ne.' ') go to 102
c       if (kk.lt.ind) go to 102
       if (lnon.ne.0) go to 1011
c
c
c      decide if negative
       lneg=1
       kk=ind
       read (qry(kk:kk),'(a1)') qtmp
       if (qtmp.ne.'-') go to 1103
          kk=kk+1
          lneg=-1
1103      continue
       if (qtmp.eq.'+') kk=kk+1
c
c
c      scan for mantissa
       ind2=ind
       idec=ind+1
       if (lpri.gt.2) write (6,*)'scanning mantissa'
       isum=0
       iexp=-1-(kk-idec-1)
       kk=kk-1
104       kk=kk+1
          iexp=iexp-1
          read (qry(kk:kk),'(i1)') itmp
          isum=isum+itmp*10**iexp
301       continue
          if (lpri.gt.2) write (6,9907)kk,itmp,iexp,isum
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
      subroutine dprint(ltyp,lrtyp,lcon,
     $  lrdat,rdat,lidat,idat,lkdat,kdat,
     $  np1r,np1i,np1k,np2,
     $  idat1,rdat1,kdat1,nptrs,lpri,lun11)
c
      parameter (nidat1=1100000,nrdat1=1100000,nkdat1=1100000,
     $           nptt=10,ndat2=100000)
c
      common /noop/lnoop
c
       dimension idat1(nidat1),rdat1(nrdat1),
     $      nptrs(nptt,ndat2)
c
      dimension rdat(2000),idat(2000)
      character(1) kdat(2000)
      character(1) kdat1(nkdat1)
      character(1) kblnk,kperc
c
      data kblnk/' '/,kperc/'%'/
c

      lprisv=lpri
      lpri=0
      if (lpri.ne.0)
     $ write (6,*)'in dprint, np1,np2=',np1,np2
      if (np2.ge.ndat2) 
     $    stop 'data index error'
      nptrs(1,np2)=np1+1
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
        write (6,*)'in dprint:',np2,np1,ltyp,lrtyp,lrdat,lidat,lkdat
        write (6,*)'          ',lcon,np1r,np1i,np1k
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
c      write (6,*)np1k,np1i,np1r,np2
c      write (6,*)nkdat1,nidat1,nrdat1,ndat2
      if ((np1k.gt.nkdat1).or.(np1i.gt.nidat1).or.(np1r.gt.nrdat1)
     $   .or.(np2.gt.ndat2)) then
        write (6,*)'dprint index error,',np1k,np1i,np1r,np2
        write (6,*)'dprint index error,',nkdat1,nidat1,nrdat1,ndat2
        stop
        endif
c
c      call dprinto(ltyp,lrtyp,lcon,
c     $  lrdat,rdat,lidat,idat,lkdat,kdat,6)
c
      lpri=lprisv
c
      return
      end
      subroutine dprinto(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,lun11)
c
c
      dimension rdat(2000),idat(2000)
      character(1) kdat(2000)
      character(20000) kdtt
      character(1) kblnk,ktst,kperc,kdtt2(20000)
c
      data kblnk/' '/,kperc/'%'/
c
      write (lun11,*)ltyp,lrtyp,lcon,nrdat,nidat,nkdat,
     $  (rdat(mm),mm=1,nrdat),(idat(mm),mm=1,nidat),
     $  kblnk,(kdat(mm),mm=1,nkdat),kblnk,kperc
      return
c
      write (kdtt(1:18),9823)ltyp,lrtyp,lcon
      write (kdtt(19:37),9823)nrdat,nidat,nkdat
c
      lsp=0
      if (lsp.eq.1) go to 9009
      nkd=38
      nkd2=37
      if (nrdat.eq.0) go to 3801
        nkd2=nkd+nrdat*13
        do 303 ml=1,nrdat
           ml2=nkd+(ml-1)*13
           write (kdtt(ml2:ml2+12),'(1pe13.5)')rdat(ml)
           ml2=ml2+13
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
      return
c
 9009 continue
      call dprints(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,lun11)
c
c
      return
      end
      subroutine dprints(ltyp,lrtyp,lcon,
     $  nrdat,rdat,nidat,idat,nkdat,kdat,lun11)
c
c
      dimension rdat(2000),idat(2000)
      character(1) kdat(2000)
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
      rtmp=rdat(max(1,nrdat))
      if (2.gt.nrdat) rtmp=0.
      write (kdtt(ml2:ml2+12),'(1pe13.5)')rtmp
      nkd2=nkd+2*13
      nkd=nkd2
c
      write (kdtt(nkd:nkd),'(a1)')kblnk
      nkd=nkd2+1
      ml2=mkd 
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
      subroutine writeascii(knam,rdati,nrhs,nrhdim,nidat1,klabs)

c     create an ascii table containing 3 columns and 6 rows

      parameter (nrhmx=999)
c
      real rdati(nrhdim,nidat1),rdat(5000)
      character(16) knam,klabs(nrhs)
      character(16) ttypet(nrhmx),tformt(nrhmx),tunitt(nrhmx)

      integer status,unit,readwrite,blocksize,tfields,nrows,rowlen
      integer nspace,tbcol(nrhmx),colnum,frow,felem
      character filename*16,extname*16
      character(16) ttype(3),tform(3),tunit(3)
      data tform/'a8','i6','e11.3'/
      data ttype/'charcter','integer','real'/
      data tunit/' ',' ',' '/

 1    status=0
c
      lpri=0
      if (lpri.ne.0)
     $ write (6,*)'entering writeascii:',nrhs,nrhdim,nidat1,
     $  rdati(1,100),rdati(2,100),rdati(3,100)

c     get an unused logical unit number to use to open the fits file
 2    call ftgiou(unit,status)

c     open the fits file, with write access
 3    readwrite=1
      filename=knam
      call ftopen(unit,filename,readwrite,blocksize,status)
      if (lpri.ne.0)
     $ write (6,*)'after ftopen:',status

c     append a new empty extension onto the end of the primary array
 4    call ftcrhd(unit,status)
      if (lpri.ne.0)
     $ write (6,*)'after ftchrd:',status

c     define parameters for the ascii table (see the above data statements)
      tfields=nrhs
      nrows=nidat1
      extname='xstar_spectra'
      
      do kk=1,nrhs
        ttypet(kk)=klabs(kk)
        tformt(kk)=tform(3)
        tunitt(kk)=tunit(3)
        enddo
c
c     calculate the starting position of each column, and the total row length
      nspace=1
 5    call ftgabc(tfields,tformt,nspace,rowlen,tbcol,status)
      if (lpri.ne.0)
     $ write (6,*)'after ftgabc:',status,rowlen,tbcol

c     write the required header parameters for the ascii table
 6    call ftphtb(unit,rowlen,nrows,tfields,ttypet,tbcol,tformt,tunitt,
     &            extname,status)
      if (lpri.ne.0)
     $ write (6,*)'after ftphbn:',status
c
      do kk=1,nrhs
c      
        frow=1
        felem=1
        colnum=kk
        do ll=1,nidat1
          rdat(ll)=rdati(kk,ll)
          enddo
        call ftpcle(unit,colnum,frow,felem,nrows,rdat,status)  
        if (lpri.ne.0)
     $   write (6,*)'after ftpcle:',kk,status,rdat(100)
c
        enddo
c
c     close the fits file and free the unit number
 8    call ftclos(unit, status)
      call ftfiou(unit, status)

c     check for any error, and if so print out error messages
 9    if (status .gt. 0)call printerror(status)
      end
      subroutine writebintabl3(idat1,rdat1,kdat1,nidat1,filename)

c     create a binary table containing 3 columns and 6 rows

      integer idat1(nidat1)
      real rdat1(nidat1)
      character(1) kdat1(nidat1)
      integer status,unit,readwrite,blocksize,hdutype,tfields,nrows
      integer varidat,colnum,frow,felem
      character filename*10,extname*16
      character(16) ttype(3),tform(3),tunit(3)
      data ttype/'charcter','integer','real'/
      data tform/'1a','1j','1e'/
      data tunit/' ',' ',' '/

 1    status=0
c     name of the fits file to append the ascii table to:



c     get an unused logical unit number to use to open the fits file
 2    call ftgiou(unit,status)

c     open the fits file, with write access
 3    readwrite=1
      call ftopen(unit,filename,readwrite,blocksize,status)
c      write (6,*)'after ftopen:',status

c     move to the last (2nd) hdu in the file
 4    call ftmahd(unit,1,hdutype,status)
c      write (6,*)'after ftmahd:',status

c     append/create a new empty hdu onto the end of the file and move to it
 5    call ftcrhd(unit,status)
c      write (6,*)'after ftcrhd:',status

c     define parameters for the binary table (see the above data statements)
      tfields=3
      nrows=nidat1
      extname='xstar_data'
      varidat=0
      
c     write the required header parameters for the binary table
 6    call ftphbn(unit,nrows,tfields,ttype,tform,tunit,
     &            extname,varidat,status)
c      write (6,*)'after ftphbn:',status
c
c     extract the system date
      call ftpdat(unit,status)
      if (status .gt. 0)call printerror(status)
c
c
c     write names to the first column, diameters to 2nd col., and density to 3rd
      frow=1
      felem=1
      colnum=1
 7    call ftpcls(unit,colnum,frow,felem,nrows,kdat1,status)
c      write (6,*)'after ftpcls:',status
      colnum=2
      call ftpclj(unit,colnum,frow,felem,nrows,idat1,status)  
c      write (6,*)'after ftpclj:',status
      colnum=3
      call ftpcle(unit,colnum,frow,felem,nrows,rdat1,status)  
c      write (6,*)'after ftpcle:',status

c     close the fits file and free the unit number
 8    call ftclos(unit, status)
c      write (6,*)'after ftclos:',status
      call ftfiou(unit, status)
c      write (6,*)'after ftfiou:',status

c     check for any error, and if so print out error messages
 9    if (status .gt. 0)call printerror(status)
      return
      end
      subroutine writebintabl1(idat1,nidat1,filename)

c     create a binary table containing garbage

      integer idat1(nidat1)
      integer status,unit,readwrite,blocksize,hdutype,tfields,nrows
      integer varidat,colnum,frow,felem
      character filename*10,extname*16
      character(16) ttype(1),tform(1),tunit(1)
      data ttype/'integer'/
      data tform/'1j'/
      data tunit/' '/

 1    status=0
c     name of the fits file to append the ascii table to:
c      filename='aptrs.fits'

c     get an unused logical unit number to use to open the fits file
c      write (6,*)'before ftgiou:',nidat1,idat1(1)
 2    call ftgiou(unit,status)
c      write (6,*)'after ftgiou:',status

c     open the fits file, with write access
 3    readwrite=1
      call ftopen(unit,filename,readwrite,blocksize,status)
c      write (6,*)'after ftopen:',status

c     move to the last (2nd) hdu in the file
 4    call ftmahd(unit,1,hdutype,status)
c      write (6,*)'after ftmahd:',status

c     append/create a new empty hdu onto the end of the file and move to it
 5    call ftcrhd(unit,status)
c      write (6,*)'after ftcrhd:',status

c     define parameters for the binary table (see the above data statements)
      tfields=1
      nrows=nidat1
      extname='xstar_ptrs'
      varidat=0
      
c     write the required header parameters for the binary table
 6    call ftphbn(unit,nrows,tfields,ttype,tform,tunit,
     &            extname,varidat,status)
c      write (6,*)'after ftphbn:',status

c     write names to the first column, diameters to 2nd col., and density to 3rd
      frow=1
      felem=1
      colnum=1
 7    call ftpclj(unit,colnum,frow,felem,nrows,idat1,status)
c      write (6,*)'after ftpcls:',status

c     close the fits file and free the unit number
 8    call ftclos(unit, status)
c      write (6,*)'after ftclos:',status
      call ftfiou(unit, status)
c      write (6,*)'after ftfiou:',status

c     check for any error, and if so print out error messages
 9    if (status .gt. 0)call printerror(status)
      return
      end
      subroutine readtable

c     read and print data values from an ascii or binary table

      integer status,unit,readwrite,blocksize,hdutype,ntable
      integer felem,nelems,nullj,diameter,nfound,irow,colnum
      real nulle,density
      character filename*40,nullstr*1,name*8,ttype(3)*10
      logical anynull

 1    status=0

c     get an unused logical unit number to use to open the fits file
 2    call ftgiou(unit,status)

c     open the fits file previously created by writeimage
      filename='atestfilez.fits'
      readwrite=0
 3    call ftopen(unit,filename,readwrite,blocksize,status)

c     loop twice, first reading the ascii table, then the binary table
 4    do ntable=1,2

c         move to the next extension
 5        call ftmrhd(unit,1,hdutype,status)

          print *,' '
          if (hdutype .eq. 1)then
              print *,'extension ',ntable,' is an ascii table.'
          else if (hdutype .eq. 2)then
              print *,'extension ',ntable,' is a binary table.'
          end if

c         read the ttypen keywords, which give the names of the columns
 6        call ftgkns(unit,'ttype',1,3,ttype,nfound,status)
          write(*,2000)ttype
2000      format(8x,3a10)

c         read the data, one row at a time, and print them out
          felem=1
          nelems=1
          nullstr=' '
          nullj=0
          nulle=0.
          do irow=1,6
              colnum=1
 7            call ftgcvs(unit,colnum,irow,felem,nelems,nullstr,name,
     &                    anynull,status)
              colnum=2
 8            call ftgcvj(unit,colnum,irow,felem,nelems,nullj,diameter,
     &                    anynull,status)
              colnum=3
 9            call ftgcve(unit,colnum,irow,felem,nelems,nulle,density,
     &                    anynull,status)
              write(*,2001)irow,name,diameter,density
2001          format(i4,a10,i10,f10.2)
          end do
      end do

c     close the file and free the unit number
 10   call ftclos(unit, status)
      call ftfiou(unit, status)

      return
      end
      subroutine writeimage(filename)

c     create a fits primary array containing a 2-d image

      integer status,unit,blocksize,bitpix,naxis,naxes(2)
      character filename*10
      logical simple,extend

 1    status=0

c     delete the file if it already exists, so we can then recreate it
 2    call deletefile(filename,status)

c     get an unused logical unit number to use to open the fits file
 3    call ftgiou(unit,status)

c     create the new empty fits file
      blocksize=1
 4    call ftinit(unit,filename,blocksize,status)

c     initialize primary header keywords
      simple=.true.
      bitpix=16
      naxis=0
      naxes(1)=0
      naxes(2)=0
      extend=.true.

c     write the required header keywords
 5    call ftphpr(unit,simple,bitpix,naxis,naxes,0,1,extend,status)

c     Extract the system date
      call ftpdat(unit,status)
      if(status .gt. 0) call printerror(status)

c     Add general info
      call ftpkys(unit,'CREATOR','BINTRAN',
     $ 'Program which generated this file', status)
      if (status .gt. 0) call printerror(status)
      call ftpcom(unit,'This file is part of the atomic database',
     $ status)
      call ftpcom(unit,'for the XSTAR spectral modeling program',status)
      if (status .gt. 0) call printerror(status)

c     close the file and free the unit number
 8    call ftclos(unit, status)
      call ftfiou(unit, status)

c     check for any error, and if so print out error messages
 9    if (status .gt. 0)call printerror(status)
      return
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
      subroutine printerror(status)

c     print out the fitsio error messages to the user

      integer status
      character errtext*30,errmessage*80

c     check if status is ok (no error); if so, simply return
      if (status .le. 0)return

c     get the text string which describes the error
 1    call ftgerr(status,errtext)
      print *,'fitsio error status =',status,': ',errtext

c     read and print out all the error messages on the fitsio stack
 2    call ftgmsg(errmessage)
      do while (errmessage .ne. ' ')
          print *,errmessage
          call ftgmsg(errmessage)
      end do
      end
