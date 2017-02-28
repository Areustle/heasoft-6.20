      SUBROUTINE CalFix(CodeNam, Date, Time, MaxRet, SLen, FileName,
     X                  ExtNo, Online, NRet, NFound, Status)
C
C
C
      IMPLICIT NONE
      CHARACTER*(*) CodeNam, Date, Time
      INTEGER MaxRet, SLen
      CHARACTER*(*) FileName(MaxRet), Online(MaxRet)
      INTEGER ExtNo(MaxRet), NRet, NFound, Status
C
C
C
      NFound = 0
      CALL GTCALF(0, 'XTE', 'PCA', '-', '-', CodeNam, Date, Time, Date,
     X            Time, '-', MaxRet, FileName, ExtNo, OnLine, NRet,
     X            NFound, Status)
C
      RETURN
      END
