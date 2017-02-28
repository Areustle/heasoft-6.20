C   12/12/86 612161120  MEMBER NAME  SWITCH   (FORT)     M  FORTRAN
C+
C (Filename) SWITCH
C (Purpose ) Trun on/off switches with COM like user-interface
C (Author  ) A.Shirahashi, Univ. of Tokyo
C (Date    ) 12-Dec-1986
C (History ) 16-Dec-1986, 'ALL' option added
C
C (Arguments) QUEST  : a character string which describes the theme
C             NVAL   : # of variables to be modified
C             HELP   : an array of help text
C             VARTYP : dummy argument, should be 'L'
C             ARRAY  : logical array
C-
      SUBROUTINE SWITCH( QUEST,NVAL,NAMES,HELP,VARTYP,ARRAY )
C
C ARG
C     (Input)
      CHARACTER * ( * )  QUEST
      INTEGER   *   4    NVAL
      CHARACTER * ( * )  NAMES(NVAL)
      CHARACTER * ( * )  HELP(NVAL)
      CHARACTER * ( * )  VARTYP
C     (Output)
      LOGICAL   *   4    ARRAY(NVAL)
C
C CONST
      CHARACTER * 1  SQ
      PARAMETER( SQ = '''' )
C
C COMMON
       include 'comode.inc' ! 't#tp.com.fort'
C
C
C VAR
      CHARACTER * 512  VARFRM
      CHARACTER *  16  TOKEN
      CHARACTER *   5  VAL
      LOGICAL   *   4  SELECT(MAXENT)
      LOGICAL   *   4  VALUES(MAXENT)
      LOGICAL   *   4  MODIFY
C
C BEGIN
C     ... set work array
      DO 1 J = 1, NVAL
        VALUES(J) = ARRAY(J)
        SELECT(J) = .FALSE.
1     CONTINUE
      MODIFY = .FALSE.
C
80    CONTINUE
C
C     ... show thema
      IF( .NOT. BATCH ) CALL CM_CLR
      WRITE( 6,'(1H ,A,/)' ) QUEST
C
C     ... check display field length
      LMAX = 0
      DO 10 J = 1, NVAL
        L = LENRD( NAMES( J ) )
        IF( L .GT. LMAX ) LMAX = L
10    CONTINUE
C
      NC = 80 / ( LMAX + 9 )
      NR = ( NVAL + NC - 1 ) / NC + 1
C
      IV = 0
      VARFRM(1:5) = '(1H ,'
      IF = 5
      DO 20 J = 1, NR
        DO 30 K = 1, NC
          IV = IV + 1
          IF( VALUES(IV) ) THEN
            VAL = SQ//'ON '//SQ
          ELSE
            VAL = SQ//'OFF'//SQ
          END IF
          LX = LMAX - LENRD( NAMES(IV) ) + 3
          WRITE( VARFRM(IF+1:),1000 ) SQ,SQ,VAL,LX
1000      FORMAT( 'A,',A1,' = ',A1,',',A5,',',I2,'X,' )
          IF = IF + 18
          IF( IV .GE. NVAL ) GOTO 40
30      CONTINUE
        VARFRM(IF+1:IF+6) = '/,1H ,'
        IF = IF + 6
20    CONTINUE
C
40    CONTINUE
      VARFRM(IF:IF) = ')'
C
C     ... show variables
      WRITE( 6,VARFRM(1:IF) ) (NAMES(J)(1:LENRD(NAMES(J))),J=1,NVAL)
C
      IF( MODIFY ) THEN
        WRITE( 6,'(/,1H ,A,A)' ) COMPRM,': Type OK to Accept'
      ELSE
        WRITE( 6,'(/,1H ,A,A)' ) COMPRM,': Press <ENTER> to Accept'
      END IF
C
90    CONTINUE
C
      CALL TXTRD( '?'//COMPRM//'>',TOKEN )
      LT = LENRD( TOKEN )
      CALL CLstrupc( LT,TOKEN )
C
      IF( MODIFY .AND. TOKEN(1:LT).EQ.'OK' ) THEN
        GOTO 800
      ELSE IF( MODIFY .AND. LT.EQ.0 ) THEN
        GOTO 80
      ELSE IF( .NOT.MODIFY .AND. LT.EQ.0 ) THEN
        GOTO 800
      ELSE IF( TOKEN(1:LT).EQ.'ON' ) THEN
        DO 50 J = 1, NVAL
          IF( SELECT(J) ) VALUES(J) = .TRUE.
          SELECT(J) = .FALSE.
50      CONTINUE
        MODIFY = .TRUE.
        GOTO 80
      ELSE IF( TOKEN(1:LT).EQ.'OFF' ) THEN
        DO 60 J = 1, NVAL
          IF( SELECT(J) ) VALUES(J) = .FALSE.
          SELECT(J) = .FALSE.
60      CONTINUE
        MODIFY = .TRUE.
        GOTO 80
      ELSE IF( TOKEN(1:LT).EQ.'ALL' ) THEN
        DO 65 J = 1, NVAL
          SELECT(J) = .TRUE.
65      CONTINUE
      ELSE
        NMATCH = 0
        DO 70 J = 1, NVAL
          LN = LENRD( NAMES(J) )
          IF( LT.EQ.LN .AND. TOKEN(1:LT).EQ.NAMES(J)(1:LN) ) THEN
            JMATCH = J
            NMATCH = 1
            GOTO 71
          ELSE IF( TOKEN(1:LT).EQ.NAMES(J)(1:LT) ) THEN
            JMATCH = J
            NMATCH = NMATCH + 1
          END IF
70      CONTINUE
71      CONTINUE
        IF( NMATCH.EQ.0 ) THEN
          WRITE( 6,'(1H ,A,A)' ) COMPRM,': No Such Answer'
        ELSE IF( NMATCH.GE.2 ) THEN
          WRITE( 6,'(1H ,A,A)' ) COMPRM,': Ambiguous Answer'
        ELSE IF( NMATCH.EQ.1 ) THEN
          SELECT( JMATCH ) = .TRUE.
        END IF
      END IF
C
      GOTO 90
C
800   CONTINUE
      DO 100 J = 1, NVAL
        ARRAY(J) = VALUES(J)
100   CONTINUE
      RETURN
C
      END
