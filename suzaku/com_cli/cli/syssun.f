C
C File: syssun.c
C Description: system dependent I/O routines for UNIX (SUN)
C Date: 6-Jul-1992
C
C History: 29-Sep-1994, PROMPT -> CLprom
C
      Subroutine CLprom( STRING )
      Implicit None
C input
      Character * (*)  STRING
C local
      Integer  status
C begin
      Call CLsyso(6,'SYS$OUTPUT',' ',status)
      Write( *,'(A,$)' ) STRING
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
      Write(Lun,'(A)') String
      Return
      End
