C   11/11/86 612041907  MEMBER NAME  COMTEST  (FORT)     M  FORTRAN
C
      Implicit NONE
      INTEGER * 4  NVAL
      INTEGER * 4  VALUES(5)
      Real * 4  RVALUE(5)
      Real * 8  DVALUE(5)
      CHARACTER * 30 HELP ( 5 )
      CHARACTER * 5  NAMES ( 5 )
      INTEGER * 4    LANS(0:5)
      CHARACTER * 3  ANS
      INTEGER * 4  ICTRL
C Data
      Data  NVAL   / 5 /
      Data  VALUES / 1, 2, 3, 4, 5 /
      Data  RVALUE / 1.0,2.0,3.0,4.0,5.0 /
      Data  DVALUE / 1.0D0,2.0D0,3.0D0,4.0D0,5.0D0 /
      Data  NAMES / 'ONE', 'TWO', 'THREE', 'FOUR', 'FIVE' /
      Data  HELP   / 'korewa ichi', 'ni', 'san', 'yon', 'go desuyo'/
      Data  ICTRL / 1 /
C
      CALL INICOM ( 'COM' )
ccc      PRINT *, 'BATCH = ', BATCH
      CALL HLPLIB ( 'TRSI.COM.HELP' )
C
1     CONTINUE
      CALL MODVAL ( '*** MODVAL (I) ***',
     &              NVAL, NAMES, '\\COM', 'I', VALUES )
      PRINT *, 'Value (I) = ', VALUES
      Call Modval( '*** MODVAL (R) ***',
     &             nval, names, '\\COM', 'R', rvalue )
      Print *, 'Value (R) = ', rvalue
      Call Modval( '*** MODVAL (D) ***',
     &             nval, names, '\\COM', 'D', dvalue )
      Print *, 'Value (D) = ', dvalue
C
      Call Chval( '*** Chval (R) ***',
     &            nval, names, '\\COM', 'R', rvalue )
C
C      DO 10 I = 1, 10000
C        IF ( ICOMER .EQ. 2 ) GOTO 20
C        IF ( MOD ( I, 100 ) .EQ. 0 ) PRINT *, 'EVENT = ', I
C 10   CONTINUE
C
      CALL INQUIR ( '*** INQUIR ***', NVAL, NAMES, HELP, 1, LANS )
ccc      PRINT *, 'BATCH = ', BATCH
      PRINT *, 'LANS = ', LANS
      IF ( LANS(1) .EQ. 5 ) STOP
      CALL SHOWIT ( '*** SHOWIT ***', NVAL, NAMES, HELP )
ccc      PRINT *, 'BATCH = ', BATCH
      CALL INQUIR ( '*** INQUIR ***', NVAL, NAMES, HELP, 2, LANS )
ccc      PRINT *, 'BATCH = ', BATCH
      PRINT *, 'LANS = ', LANS
      IF ( LANS(1) .EQ. 5 ) STOP
      CALL SHOWIT ( '*** SHOWIT ***', NVAL, NAMES, HELP )
ccc      PRINT *, 'BATCH = ', BATCH
      GOTO 1
20    CONTINUE
      CALL TELLIT ( 'CTRL-C PRESSED........')
      ICTRL = ICTRL + 1
      IF ( ICTRL .GT. 10 ) STOP
      CALL TXTRD ( '?ARE YOU STOP YOUR PROCESS ', ANS )
      IF ( ANS(1:1) .EQ. 'N' ) GOTO 1
      STOP
      END
