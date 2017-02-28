C edicmn.inc
C
C IUP..IEOF are the user-defined control keys.
C IFTYPE is the number returned by FORTYP.
C ICEDIT is <>0 if line editing has been switched on.
C
      INTEGER   IUP, IDOWN, ILEFT, IRIGHT, IBEG, IEND
      INTEGER   IWRITE, IERASE, IEOF, IFTYPE, ICEDIT
C
      COMMON/EDICMN/ IUP, IDOWN, ILEFT, IRIGHT, IBEG, IEND,
     :          IWRITE, IERASE, IEOF, IFTYPE, ICEDIT
