C
       SUBROUTINE SHFTIM( SHf, Yr, Day, Hr, Mn, Sec)
       IMPLICIT NONE

       integer*4 SHF
       INTEGER  Yr, Day, Hr, Mn, Sec

       integer*4 Tarr(5)

       CALL TIMKA(SHF, Tarr)
       Yr = Tarr(1)
       Day= Tarr(2)
       Hr = Tarr(3)
       Mn = Tarr(4)
       Sec = Tarr(5)

       RETURN
       END
