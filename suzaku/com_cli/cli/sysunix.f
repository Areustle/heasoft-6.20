C
C File: syssun.c
C Description: system dependent I/O routines for UNIX (DEC,DG)
C Date: 6-Jul-1992
C
C History:
C     29-Sep-1994, PROMPT -> CLprom
C     26-Feb-1997 Y.ISHISAKI, CLprom fix for OSF1
C
      Subroutine CLprom( string )
      Implicit None
C input
      Character * (*)  string
C local
      Integer  status
C begin
      Call CLsyso(6,'SYS$OUTPUT',' ',status)
ccc      Write( *,'(1H$A)' ) string     ! do not work well on OSF1
      Write( *,'(1X,A,$)' ) string
      Call Clsyso(6,' ',' ',status)
C
      Return
      End
C
C
      Subroutine CLputL( Lun,String )
      Implicit None
C local
      Integer  Lun
      Character * (*)  String
C begin
      If( Lun.eq.6 ) Then
ccc        Print *, String
        Write( *,'(1X,A)' ) String
      Else
        Write(Lun,'(1X,A)') String
      End If
      Return
      End
