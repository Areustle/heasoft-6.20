      subroutine rdflo(flo,qry,ios,lpri,ind)

c       reads integer from file param corresponding to character string
c       'char'. if no integer there, prompts user for parameter ...
c      author:  T. Kallman
c
       implicit none
       
       real*8 sum, float, flo, sum2, flo2

       integer lun11, lpri, ind, kk, lfnde, lfndd
       integer lnon, lfndm, lfnd2, lfnd, lfnd2o, ll
       integer lfndb, lneg, ind2, idec, iexp, itmp, ios

       character(1) qtmp
       character(72) qry
       character(1) chtst(16)
c
       data chtst/'0','1','2','3','4','5','6','7','8','9','+','-',
     $            ' ','.','e','e'/
c
       lun11=6
       if (lpri.gt.2) write (lun11,*) 'in rdflo',ind
       if (lpri.gt.2) write (lun11,*)qry
c
c      scan for e and dot
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
       if (lpri.gt.2) write (lun11,9905)lfnde,lfndd
9905   format (1x,' e and d switches',2i4)
       if (lfndd.eq.0) go to 10
       if (lnon.ne.0) go to 1011
       lneg=1
       if (lfndm.ne.0) lneg=-1
       read (qry(lfndb:lfndb),'(a1)') qtmp
       if ((qtmp.eq.'+').or.(qtmp.eq.'-'))lfndb=lfndb+1
c
c      scan for mantissa
       ind2=ind
       kk=lfndb
       idec=lfndd
       if (lpri.gt.2) write (lun11,*)'scanning mantissa'
       if (lfnde.ne.0) ind2=lfnde-1
       if (lpri.gt.2) write (lun11,*)'ind,ind2,lfnde,lfndb:',
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
          if (lpri.gt.2) write (lun11,9907)kk,itmp,iexp,sum
9907      format (1x,' kk,itmp,iexp,sum ',3i8,e12.4)
          if (kk.lt.ind2) go to 104
       flo=sum*float(lneg)
       ios=0
       if (lfnde.eq.0) return
c
c      scan for exponent
       if (lpri.gt.2) write (lun11,*)'scanning exponent'
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
          if (lpri.gt.2) write (lun11,9907)kk,itmp,iexp,sum2
          if (kk.lt.ind) go to 105
       flo2=sum2*float(lneg)
       flo=flo*10.**flo2
       if (lpri.gt.2) write (lun11,*)'returning:',flo2,flo
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
