**==DOQUAL.spg  processed by SPAG 3.09I  at 09:42 on 20 Aug 1992
      SUBROUTINE DOQUAL(Quals,Qualifier,Lq,Nqual,Nq,Status)
c
c routine to get the qualifers from the parse string and
c check if they are valid	nick 4.5.90
c from original code by andy, arvind etc...
c tidy up 4 dec 1991 Nick
c
c import
c quals - array of valid qualifers
c nqual - number of valid qualifiers
c
c export
c qualifier - output array of found qualifiers
c lq - array of ?
c nq - number of found qualifiers
c
      INCLUDE 'estec.inc'
c
      INTEGER*4 Nqual , jj , Status , lm1 , Nq , jqual , llen , klen
      INTEGER*4 lm2 , lm3 , lcx , LENACT
      CHARACTER*(*) Quals(Nqual) , Qualifier(Nqual)
      character(256) string , par , value
      INTEGER*4 Lq(Nqual)
c
c start by getting parsed commands
c
      Status = 0
      lcx = LENACT(Zstring)
      CALL GETCMD(Zstring,lcx,string,lm1,par,lm2,value,lm3,Status)
c
      DO 100 jj = 1 , Nqual
         Qualifier(jj) = ' '
         Lq(jj) = 0
 100  CONTINUE
c
      Status = 0
      CALL GTQUAL(string,lm1,Nq,Qualifier,Lq,Status)
      IF ( Nq.GT.Nqual ) THEN
         WRITE (*,*) ' Error: too many qualifiers'
         Status = 1
         RETURN
      ENDIF
c
c check to see if qualifier is a '?' - if it is list available qualifier
c and then return
c
      CALL CHQUAL(Qualifier,Nq,Quals,Nqual,Status)
      IF ( Status.NE.0 ) RETURN
c
c check qualifier are valid
c
      IF ( Nq.GT.0 ) THEN
c
         DO 150 jj = 1 , Nq
c
            Status = 0
            CALL QLMTCH(Quals,Nqual,Qualifier(jj),jqual,Status)
            IF ( Status.NE.0 ) THEN
               llen = INDEX(Qualifier(jj),' ')
               klen = INDEX(Qualifier(jj),'=')
               IF ( klen.GT.0 .AND. klen.LT.llen ) llen = klen
               llen = llen - 1
               IF ( llen.LE.0 ) llen = 1
               Zwrite = ' Error: Unmatched/ambiguous qualifier (' //
     &                  Qualifier(jj)(1:llen) //
     &                  ') - please reenter command'
               CALL XWRITE(Zwrite,5)
               RETURN
            ENDIF
 150     CONTINUE
      ENDIF
c
      RETURN
      END
