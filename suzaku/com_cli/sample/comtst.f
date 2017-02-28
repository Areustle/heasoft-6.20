C   11/11/86 701091822  MEMBER NAME  COMTST   (FORT)     M  FORTRAN
C
      Implicit NONE
      INTEGER * 4  NVAL
      INTEGER   * 4  VALUEI( 5 )
      REAL      * 4  VALUER( 5 )
      LOGICAL   * 4  VALUEL( 5 )
      CHARACTER * 5  VARTYP
      CHARACTER * 30 HELP ( 5 )
      CHARACTER * 5  NAMES ( 5 )
      EQUIVALENCE( VALUEI,VALUER,VALUEL )
C Data
      Data  NVAL   / 5 /
      Data  VARTYP / 'IIRRL' /
      Data  NAMES / 'ONE', 'TWO', 'THREE', 'FOUR', 'FIVE' /
      DATA  HELP   / 'korewa ichi', 'ni', 'san', 'yon', 'go desuyo'/
C
      CALL INICOM ( 'COM' )
C
      VALUEI( 1 ) = 1
      VALUEI( 2 ) = 2
      VALUER( 3 ) = -121.0
      VALUER( 4 ) = 0.0
      VALUEL( 5 ) = .TRUE.
      CALL CHVAL( 'CHVAL',NVAL,NAMES,HELP,VARTYP,VALUEI )
      STOP
      END
