
      SUBROUTINE xgrnge(string, iparse, nrange, rngnam, descr, rnglw,
     &                  rnghi, idefrg, qsing, qextrm, qintgr, iflag, 
     &                  idelim)

      INTEGER   iparse, nrange
      REAL      rnglw(nrange), rnghi(nrange)
      INTEGER   idefrg(nrange)
      INTEGER   iflag, idelim
      CHARACTER*(*) string
      CHARACTER*(*) rngnam(nrange), descr
      LOGICAL   qsing, qintgr(2, nrange), qextrm(2, nrange)

c Subroutine to get a general range (see the XPRNGE subroutine
c for the details of what constitutes a general string).


c  Error Conditions 0 - A range parsed (or
c  the field was skipped
c  -1 EOF on corrections
c   1 End of string reached
c   2 Infinite skip

      INTEGER ibeg, iend

      LOGICAL qskip, qdone

      CALL xgtarg(string, iparse, ibeg, iend, qskip, iflag, idelim)

      IF ( iflag .NE. 0 ) RETURN
      IF ( qskip ) RETURN

      qdone=.false.

      DO WHILE( .NOT.qdone )

         CALL xprnge(string(ibeg:iend), nrange, rngnam, descr, rnglw,
     &               rnghi, idefrg, .true., qsing, qextrm, qintgr, 
     &               iflag)

         IF ( iflag .LT. 0 ) RETURN

         qdone = iflag .EQ. 0
         IF ( .NOT.qdone ) THEN
            CALL xinfix('Replace entire argument:',string,iparse,iflag)
            IF ( iflag .NE. 0 ) THEN
               iflag = -1
               RETURN
            ENDIF
         ENDIF

      ENDDO

      RETURN
      END
