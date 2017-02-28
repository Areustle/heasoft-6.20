      SUBROUTINE GETSEED(Seed)
      implicit none
c
c get a seed number
c
      REAL*4 dummy , GETRAN
      INTEGER*4 itime(3) , date(3) , Seed , modulus
      character(8) systime
c
c  initialize seed using current date and time
c
      CALL GETTIM(systime)
      READ (systime(1:2),'(i2)') itime(1)
      READ (systime(4:5),'(i2)') itime(2)
      READ (systime(7:8),'(i2)') itime(3)
CvMSJ - 3/2/98 - IDATE has been replaced with more portable
C                routine, XIDATE, for the benefit of g77
C     CALL IDATE(date(1),date(2),date(3))
      CALL XIDATE(date)
c
c  calculate seed and make sure it is a large, odd integer
c
      Seed = (date(1)*date(2)*date(3)
     &       +(itime(1)**2+itime(2)**2+itime(3)**2))*1234
      modulus = MOD(Seed,2)
      IF ( modulus.EQ.0 ) Seed = Seed - 1
c
c  initialize random number generator for the first time
c  seed value will automatically been updated after each call to ran
c
      dummy = GETRAN(Seed)
c
      RETURN
      END
