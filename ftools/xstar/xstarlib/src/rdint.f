       subroutine rdint(lo,qry,ios,lpri,ind)

c       reads integer from file param corresponding to character string
c       'char'. if no integer there, prompts user for parameter ...
c      author:  T. Kallman
c
       implicit none

       integer lun11, lpri, ind, kk
       integer lnon, lfndm, lfnd2, lfnd, lfnd2o, ll
       integer lfndb, lneg, ind2, idec, iexp, itmp, ios
       integer isum, lo

       character(1) qtmp
       character(72) qry
       character(1) chtst(15)
c
       data chtst/'0','1','2','3','4','5','6','7','8','9','+','-',
     $           ' ','.','e'/
c
       lun11=6
       if (lpri.gt.2) write (lun11,*) 'in rdint',ind
       if (lpri.gt.2) write (lun11,*)qry
c
c      scan for e and dot
       kk=0
       lfndm=0
       lnon=0
       lfnd2=0
102    kk=kk+1
       if (kk.ge.72) return
       read (qry(kk:kk),'(a1)') qtmp
       if (qtmp.eq.'-') lfndm=kk
       lfnd=0
       lfnd2o=lfnd2
       lfnd2=0
       do 3011 ll=1,13
         if (qtmp.eq.chtst(ll)) lfnd=1
         if ((qtmp.eq.chtst(ll)).and.(ll.le.10)) lfnd2=1
 3011    continue

       lfndb=0
       if ((lfnd2.eq.1).and.(lfnd2o.eq.0))
     $      lfndb=kk
       if (lfnd.eq.0) lnon=1
       if (kk.lt.ind) go to 102
       if (lnon.ne.0) go to 1011
       lneg=1
       if (lfndm.ne.0) lneg=-1
c
c      scan for mantissa
       kk=lfndb
       ind2=ind
       idec=ind+1
       if (lpri.gt.2) write (lun11,*)'scanning mantissa'
       isum=0
       iexp=-1-(kk-idec-1)
       kk=kk-1
104       kk=kk+1
          iexp=iexp-1
          read (qry(kk:kk),'(i1)') itmp
          isum=isum+itmp*10**iexp
          if (lpri.gt.2) write (lun11,9907)kk,itmp,iexp,isum
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
