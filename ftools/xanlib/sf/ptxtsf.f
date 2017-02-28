      SUBROUTINE PTXTSF(IUNIT, NREC, BUFFER, LENBUF)
      INTEGER   IUNIT, NREC, LENBUF
      character(1)      BUFFER(LENBUF)
C---
C SF subroutine to print the remaining auxiliary records of a
C package as text on the standard output device.
C---
C IUNIT    I    Input unit
C NREC     I    The no. of remaining records
C BUFFER   I/R  A buffer used for IO
C LENBUF   I    Size of the buffer
C IERRSF   I/R  SF error flag
C IERRIO     R  IO error
C---
C 1989-Aug-21 - Modified to use XWRITE - [AFT]
C 1985-Mar-10 - rashafer
C---
      character cbuf*255
      integer*4 irec, i, lr, lc
C---
      irec=0
      do while(irec.ne.nrec)
         read(iunit) lr,(buffer(i),i=1,min(lenbuf,lr))
         if(lr.gt.0) then
            irec=irec+1
            lc=min(lenbuf,lr)
            if(lr.gt.lc) write(*,*) 'PTXTSF: Record too full:',lr,
     &        'some text lost'
            cbuf=' '
            do i = 1, lc
               write(cbuf(i+1:i+1),'(a1)') buffer(i)
            end do
            call xwrite(cbuf(:lc+1),5)
         else
            irec=nrec
            if(irec.lt.0) then
               write(*,*) 'PTXTSF: Badly terminated package'
                backspace(iunit)
            end if
         end if
      end do
      return
      end
