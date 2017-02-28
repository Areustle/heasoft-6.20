**==CHQUAL.spg  processed by SPAG 3.09I  at 09:41 on 20 Aug 1992
      SUBROUTINE CHQUAL(Qualifiers,Nq,Quals,Nqual,Status)
c
c  subroutine checks for a '?' qualifier, if found it returns status=1
c  and lists the available qualifiers
c
      INTEGER*4 Nq , Nqual , Status , j , l , LENACT , ipos , istart , 
     &          len
      INTEGER*4 lenst
      character(68) comment
      CHARACTER*(*) Qualifiers(*) , Quals(*)
      LOGICAL*4 found , extra_long , question
c
      INCLUDE 'estec.inc'
c
      Status = 0
c
      IF ( Nq.EQ.0 ) THEN
         RETURN
      ELSEIF ( Nq.EQ.-1 ) THEN
c
         question = .FALSE.
         lenst = LENACT(Zstring) - 1
         IF ( lenst.GT.0 ) THEN
            DO 20 j = 1 , lenst
               IF ( Zstring(j:j+1).EQ.'/?' ) question = .TRUE.
 20         CONTINUE
         ENDIF
         IF ( question ) THEN
            WRITE (Zwrite,99001)
            CALL XWRITE(Zwrite,10)
            Status = 1
         ELSE
            RETURN
         ENDIF
      ENDIF
c
      found = .FALSE.
      DO 100 j = 1 , Nq
         IF ( Qualifiers(j)(1:1).EQ.'?' ) THEN
            found = .TRUE.
            GOTO 200
         ENDIF
 100  CONTINUE
c
 200  IF ( .NOT.found ) RETURN
c
c ? specified
c
      Status = 1
      IF ( Nqual.EQ.0 ) THEN
         WRITE (Zwrite,99002)
         CALL XWRITE(Zwrite,10)
         WRITE (Zwrite,99001)
         CALL XWRITE(Zwrite,10)
         RETURN
      ENDIF
c
      CALL SORTQL(Quals,Nqual)
c
      WRITE (Zwrite,99002)
      CALL XWRITE(Zwrite,10)
      WRITE (Zwrite,99003)
      CALL XWRITE(Zwrite,10)
      WRITE (Zwrite,99002)
      CALL XWRITE(Zwrite,10)
c
      comment = ' '
      ipos = 0
      istart = 1
c
      DO 300 j = 1 , Nqual
         IF ( Quals(j)(1:1).NE.'?' ) THEN
            DO WHILE ( .TRUE. )
               ipos = ipos + 1
c
               extra_long = .FALSE.
               l = LENACT(Quals(j))
               IF ( l.GT.15 ) THEN
                  len = 30
                  l = 29
                  IF ( ipos.EQ.4 ) THEN
                     extra_long = .TRUE.
                     GOTO 205
                  ENDIF
                  ipos = ipos + 1
               ELSE
                  len = 15
               ENDIF
c
               comment(istart:istart+l) = '/' // Quals(j)(1:l)
               istart = istart + len
c
c Ziqin Pan, Jun 20,2005
c Move 205 CONTINUE out of if then block
c
 205           CONTINUE
               IF ( ipos.EQ.4 .OR. j.EQ.Nqual ) THEN
c 205              WRITE (Zwrite,99004) comment
                  WRITE (Zwrite,99004) comment
                  CALL XWRITE(Zwrite,10)
                  comment = ' '
                  istart = 1
                  ipos = 0
                  IF ( extra_long ) GOTO 220
               ENDIF
               GOTO 300
 220        ENDDO
         ENDIF
 300  CONTINUE
c
      WRITE (Zwrite,99002)
      CALL XWRITE(Zwrite,10)
c
      RETURN
99001 FORMAT (' This command has no qualifiers')
99002 FORMAT (' ')
99003 FORMAT (' Allowed qualifiers are:')
99004 FORMAT (1X,A)
      END
