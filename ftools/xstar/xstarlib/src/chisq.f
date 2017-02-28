      subroutine chisq(ajisb,cjisb,indb,nindb,
     $   ipmat,x,lun11,lpri)
c
      include './PARAM'
c
      real*8 ajisb(2,ndb),cjisb(ndb)
      integer indb(2,ndb),ll1mx(nd),ll2mx(nd)
      real*8 x(nd),bmat(nd),xo(nd),sum(nd),
     $       term1mx(nd),term2mx(nd),term1,term2
c
c        check the solution
         write (lun11,*)'checking the solution'
         err=0.
        do mm=1,ipmat
          sum(mm)=0.
          term1mx(mm)=0.
          term2mx(mm)=0.
          ll1mx(mm)=0
          ll2mx(mm)=0
          enddo
        write (lun11,*)'rate equations'
        do ll=1,nindb
          mm=min(ipmat,indb(1,ll))
          nn=min(ipmat,indb(2,ll))
          if (mm.ne.nn) then
            term1=ajisb(1,ll)*x(nn)
            term2=ajisb(2,ll)*x(mm)
            sum(mm)=sum(mm)+term1-term2
            if (term1.gt.term1mx(mm)) then
               term1mx(mm)=term1
               ll1mx(mm)=ll
               endif
            if (term2.gt.term2mx(mm)) then
               term2mx(mm)=term2
               ll2mx(mm)=ll
               endif
            write (lun11,*)ll,mm,nn,ajisb(1,ll),ajisb(2,ll),
     $         x(nn),x(mm),sum(mm),term1,term2
 9246       format (1h ,i4,4e12.4,2i4)
            endif
          enddo
        write (lun11,*)'sums'
        do mm=1,ipmat
          write (lun11,*)mm,x(mm),sum(mm),term1mx(mm),
     $       indb(2,ll1mx(mm)),term2mx(mm),indb(2,ll2mx(mm))
          enddo
c
         if (lpri.gt.2)
     $    write (lun11,*)'leaving leqt'
c
      return
      end
