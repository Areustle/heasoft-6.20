c
      SUBROUTINE xrdhms(tewi, swi, iwi, imod)
c
c  ls 9/4/88 to convert a time in 3 different formats
c
c     I/R  tewi = time in days (format 1)
c     I/R  swi  = secs of the day (together with iwi(1) is format 2)
c     I/R  iwi  = day,hour,min,sec,msec  (format 3)
c       I  imod = 1,2,3 if in input time is given in format 1,2,3
c
      INTEGER*4 imod, iwi(5)
      REAL*8 tewi, swi, dum
c  convert any input format to format 1
      GOTO (1, 2, 3), imod
 2    tewi = iwi(1)
      tewi = tewi + swi/86400.D0
      GOTO 1
 3    dum = iwi(5)
      dum = dum/86400.D+3
      tewi = dum
      dum = iwi(4)
      dum = dum/86400.D0
      tewi = tewi + dum
      dum = iwi(3)
      dum = dum/1440.D0
      tewi = tewi + dum
      dum = iwi(2)
      dum = dum/24.D0
      tewi = tewi + dum + iwi(1)
 1    CONTINUE
c  convert format 1 to format 2
      iwi(1) = tewi + 1.157D-12
      dum = tewi - iwi(1)
      swi = dum*86400.D0
c  and to format 3
      iwi(2) = swi/3600.D0
      dum = swi - iwi(2)*3600.D0
      iwi(3) = dum/60.D0
      dum = dum - iwi(3)*60D0
      iwi(4) = dum
      dum = dum - iwi(4)
c +0.5d0
      dum = dum*1000.D0
c modified on 7/2/89
      iwi(5) = nint(dum)
c modified on 12/2/89
c  reset in case of 1000 msec
c reset msec
      IF (iwi(5).EQ.1000) THEN
         iwi(5) = 0
         iwi(4) = iwi(4) + 1
c reset sec
         IF (iwi(4).EQ.60) THEN
            iwi(4) = 0
            iwi(3) = iwi(3) + 1
         ENDIF
c reset min
         IF (iwi(3).EQ.60) THEN
            iwi(3) = 0
            iwi(2) = iwi(2) + 1
         ENDIF
c reset hour
         IF (iwi(2).EQ.24) THEN
            iwi(2) = 0
            iwi(1) = iwi(1) + 1
         ENDIF
      ENDIF
c
c     write(5,*) tewi,swi,iwi
      RETURN
      END
c
