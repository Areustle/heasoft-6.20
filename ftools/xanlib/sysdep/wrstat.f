      SUBROUTINE wrstat(value)

      INTEGER value(5)

c **      **    fwj haberl      23 oct 1986
c **      ** subroutine to write statistics onto terminal
c      value(1) - Elapsed real time in 10msec tics
c      value(2) - elapsed CPU time in 10msec tics
c      value(3) - buffered I/O count
c      value(4) - direct I/O count
c      value(5) - page fault count

      INCLUDE 'writebuf.inc'

      INTEGER i
      REAL rvalue(2)
      character(30) par(5)
      character(8) unit

      DATA unit/' seconds'/
      DATA par/'  Elapsed real time','  Elapsed CPU time',
     &       '  Buffered I/O count','  Direct I/O count',
     &       '  Page fault count'/

      DO i = 1, 2
         rvalue(i) = value(i)/100.
      ENDDO

      DO i = 1, 2
         WRITE(wrtstr,'(a,f10.2,a)') par(i), rvalue(i), unit
         CALL xwrite(wrtstr,5)
      ENDDO
      DO i = 3, 5
         WRITE(wrtstr,'(a,i10)') par(i), value(i)
         CALL xwrite(wrtstr,5)
      ENDDO

      RETURN
      END

