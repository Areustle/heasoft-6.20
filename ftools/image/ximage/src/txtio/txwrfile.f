      subroutine txwrfile(filename, status)
      implicit none
c
c  Write contents of txtio buffer into text file
c
c  I  filename  (s)  Text file location
c  O  status    (i)  Error flag (0 = OK)
c
      character*(*) filename
      integer status

      include 'txtio.inc'
c
c  Local variables
c
      integer i, lun, ndig, LENACT

      if ( status.ne.0 ) return
      ndig = 0

      if ( txtqfile.ne.filename ) then
         call XWRITE(' txwrfile: Buffer improperly filled', 10)
         status = -1
         return
      endif

      call getlun(lun)
c     open(unit=lun, file=filename, status='new', iostat=status)
      call openwr(lun,filename,'unknown',' ',' ',0,0,status)
      if ( status.ne.0 ) then
         call XWRITE(' Could not open text file', 10)
         call frelun(lun)
         return
      endif

      do i = 1, NQCom
         write(lun, '(a)', IOSTAT=status) QCOm(i)(1:LENACT(QCOm(i)))
         if ( status.ne.0 ) goto 100
      enddo

      do i = 1, NQPts
         call wrqdat(lun, ndig, QBUf(i), QERr, NQRows, NQCol)
      enddo

 100  close(lun)
      call frelun(lun)

      return
      end
