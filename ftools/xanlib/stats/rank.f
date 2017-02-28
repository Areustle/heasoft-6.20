      SUBROUTINE RANK(N,INDX,IRANK)
      integer*4 INDX(*),IRANK(*), j, n
      DO 11 J=1,N
        IRANK(INDX(J))=J
11    CONTINUE
      RETURN
      END
