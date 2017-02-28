C+
C File: clopen.for
C Description: Open/Close Command File for CLI
C Author:A.Shirahashi, Univ. of Tokyo
C Date: 6-Nov-1986
C Note: THIS MODULE IS MACHINE DEPENDENT.
C
C Public: CLopen, CLclos
C
C History: CFopen/CLclos -> CLopen/CLclos
C     27-Aug-1992, append mode
C                  remove CLALOC, CLFREE
C     25-Feb-1997 Y.ISHISAKI, Character * 132->256 Name1, Fulnam in CLopen
C     08-Jul-1998 Y.ISHISAKI, check if file exist after CLowrt
C     02-Mar-2005 Y.ISHISAKI, use CLaddext() instead of FILTYP()
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C
      Subroutine CLopen( NAME,LUN,MODE,IERR,DEFNAM )
      Implicit None
C common
      Include 'clidef.inc'
C input
      Character * ( * )  NAME, MODE
      Integer  LUN
      Character * ( * )  DEFNAM
C output
      Integer  IERR
C local
      Character * (LINSIZ)  Name1, Fulnam
      Character * 1  Cmode
      Integer  L, LNAME, status
      Logical * 4  LEXIST
C function
      Integer * 4  Lenrd
C begin
       LNAME = LENRD( NAME )
       If( LNAME .eq. 0 ) Goto 911
       NAME1(:LNAME) = NAME(:LNAME)
       NAME1(LNAME+1:) = ' '
       L = LNAME
       FULNAM(:L) = NAME1(:LNAME)
       FULNAM(L+1:) = ' '
       Call CLaddext(FULNAM,DEFNAM,L)
C
       Cmode = Mode(1:1)
       Call CLstrupc( 1,Cmode )
C
C      ... for debug
C      Write( *,* ) 'FULNAM:<',FULNAM(1:L),'>'
C      Write( *,* ) 'DEFNAM:<',DEFNAM,'>'
C
       If( Cmode .eq. 'R' ) Then
         INQUIRE( FILE=FULNAM(1:L),EXIST=LEXIST,ERR=901 )
         If( .not. LEXIST ) Goto 901
cc         Open(UNIT=LUN,FILE=FULNAM(1:L),ERR=901,STATUS='OLD',READONLY)
         Open(UNIT=LUN,FILE=FULNAM(1:L),ERR=901,STATUS='OLD')
       Else If( Cmode .eq. 'W' ) Then
         Call CLowrt(Fulnam(:L))
         INQUIRE( FILE=Fulnam(:L),EXIST=LEXIST,ERR=901 )
         If( LEXIST ) Goto 901
         If( Lun.eq.6 ) THen
           Call CLsyso(6,Fulnam(:L),'NEW',status)
         Else
           Call CLsyso(Lun,Fulnam(:L),'W',status)
           If( status.lt.0 ) Goto 901
C           Open( UNIT=LUN,FILE=FULNAM(1:L),
C     &           FORM='FORMATTED',STATUS='NEW',ERR=901 )
         End If
       Else If( Cmode .eq. 'A' ) Then
         Inquire( FILE=Fulnam(1:L),EXIST=Lexist,ERR=901 )
         If( Lexist ) Then
           If( Lun.eq.6 ) Then
             Call CLsyso(6,Fulnam(:L),'APPEND',status)
           Else
             Call CLsyso(Lun,Fulnam(:L),'A',status)
             If( status.lt.0 ) Goto 901
C             Open( UNIT=Lun, FILE=Fulnam(:L),
C     &             FORM='FORMATTED',STATUS='OLD',ACCESS='APPEND',
C     &             ERR=901 )
           End If
         Else
           If( Lun.eq.6 ) THen
             Call CLsyso(6,Fulnam(:L),'NEW',status)
           Else
             Call CLsyso(Lun,Fulnam(:L),'W',status)
             If( status.lt.0 ) Goto 901
C             Open( UNIT=LUN,FILE=FULNAM(1:L),
C     &             FORM='FORMATTED',STATUS='NEW',ERR=901 )
           End If
         End If
       Else
         Goto 902
       End If
C
C .... Sucessful Completion
      Ierr = 0
      Goto 1000
C .... File Open Failure
 901  If( Mode(1:1).eq.Cmode ) Then
        Call CLIerr( 2,'can''t open '''//FULNAM(:L)//'''' )
      End If
      Ierr = -1
      Goto 1000
 911  If( Mode(1:1).eq.Cmode ) Then
        Call CLIerr( 2,'invalid filename specification' )
      End If
      Ierr = -1
      Goto 1000
C .... Access Mode Error
 902  Ierr = -2
      Goto 1000
C
 1000 Continue
C     CALL ERRSTR( 131,ERR131 )
      Return
C
      End
C
C
      Subroutine CLCLOS( LUN,IERR )
      Implicit None
C
C .... Close Command File for CLI
C      by A.Shirahashi, 10-NOV-1986
C
C input
      Integer LUN
C output
      Integer Ierr
C begin
      IERR = 0
      CLOSE( UNIT=LUN,ERR=900 )
      Return
 900  IERR = -1
      Return
C
      End
C
C
