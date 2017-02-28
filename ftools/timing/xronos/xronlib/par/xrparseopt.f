      subroutine xrparseopt(cfile,cfil,copt,iopt,mopt,dopt,ierr)
      implicit none
      integer nfil,iyrsta,iopt(15),mopt(15),ierr
      character(160) cfile,cfil
      character(10) copt(15)
      double precision dopt(15)
C     LOCAL
      integer k,extnum,isptr,iblank,lenact,ib
      logical last_blank
      external lenact
      character(160) tfile
      character(8) extopt

c     decode filename and options of infile
      cfil = ' '
      iblank = index(cfile,' ')
      cfil=cfile(1:iblank-1)
      do k=1,15
         copt(k)='          '
      enddo
      
C     strip FTOOLS style extension off and turn it into an RT option
      call xrstrext(cfil,tfile,extnum,ierr)
      cfil=tfile
      if(extnum.ne.-99) then
         call ftkeyn('RT',extnum,extopt,ierr)
         cfile=cfile(1:lenact(cfile))//' '//extopt
      endif
      
c     break cfile on white space into copt         
      k=1
      isptr=1
      last_blank=.true.
      do ib=iblank+1,160
         if(cfile(ib:ib).ne.' ') then
            if(k.gt.15) then
               call xwrite(
     $ 'WARNING: You have specified too many options for the file:',4)
               call xwrite('Maximum options: 10',4)
               call xwrite('Extra options ignored.',4)
               goto 100
            endif
            copt(k)(isptr:isptr)=cfile(ib:ib)
            last_blank=.false.
            isptr=isptr+1
         else
            if(.not.last_blank) then
               k=k+1
               isptr=1
            endif
            last_blank=.true.
         endif
      enddo
 100  continue
      CALL xrdecopt(copt, iopt,mopt, dopt, ierr)
      return
      end
