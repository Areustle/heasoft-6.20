CH    SUBROUTINE SCNCHK          PDP 11/70 DU0:[300,147]SCNCHK.FTN;10
CH                               TO MICROVAX 3/1/88
CH
C  Jesse Allen  23 Jan 1996  Removed implicit typing
C  Lorraine Breedon 21 Oct 1998 changed anglim(4,2) to anglim(2)
C  Lorraine Breedon 21 Oct 1998 eliminate LIM common block

      SUBROUTINE SCNCHK(J,A,ANGLIM,Q)

      implicit none

C Common block declarations

      real anglim(2)
c      COMMON/LIM/ANGLIM

      character(40) taskname
      common /TASK/ taskname

C Local variables

      logical q

      integer j

      real a, al, au, dtr

      DATA DTR/.017453295D0/


      Q=.TRUE.
      A=A/DTR
      AL=ANGLIM(1)
      AU=ANGLIM(2)
      IF((AL.LT.AU).AND.((A.LT.AL).OR.(A.GT.AU)))Q=.FALSE.
      IF((AL.GT.AU).AND.((A.LT.AL).AND.(A.GT.AU)))Q=.FALSE.

      RETURN

      END
