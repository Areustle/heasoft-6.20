C   04/12/86 810282314  MEMBER NAME  QOPEN    (FORT)     M  FORTRAN
C
C File: qopen.f
C Description: Inquire filename on console and open it
C Author:  A.Shirahahsi, Univ. of Tokyo
C Date: 4-Dec-1986
C
C Public: Qopen, XOPEN, CLISTC, FINDIT, OPENIT
C
C THIS MODULE IS HEAVILY MACHINE DEPENDENT.
C
C HISTORY:
C     05-Sep-1987, full screen version
C     13-Mar-1988, FB format
C     20-Sep-1988, return SELECT
C     29-Sep-1988, VBS format
C     16-Jun-1992, DIRECT access file
C     02-Jul-1992, fix a bug for negative LUN, DIRECT=recl
C     08-Jul-1998 Y.ISHISAKI, check if file exist after CLowrt
C     20-Feb-2005 Y.ISHISAKI, insert 'Implicit None'
C     20-Feb-2005 Y.ISHISAKI, Use IntrdX instead of Intrd in CLISTC()
C     20-Feb-2005 Y.ISHISAKI, check also '?' in XOPEN(), CLISTC()
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C     02-Feb-2006 Y.ISHISAKI, LISTC() moved from miscunix.f to CLISTC()
C
CC*********************************************************************
C*
C*  LOGICAL FUNCTION QOPEN( PROMPT,LUN,DSNAME,MODE,FRM,STS,
C*                          /ICONT/,/SELECT/ )
C*
C*
C*  (Purpose) Inquire filename on console and open it
C*
C*  (Input)  CHARACTER * (*)  PROMPT   prompt string, CLI convention
C*           INTEGER   *   4  LUN      logical unit number
C*                                     if LUN < 0, QOPEN will NOT open
C*                                     specified dataset actually.
C*           CHARACTER * (*)  MODE     'READ', 'WRITE', 'BOTH', 'DIRECT'
C*           CHARACTER * (*)  FRM      'UNFORMATTED', 'FORMATTED'
C*           CHARACTER * (*)  STS      'OLD', 'NEW', 'SHR', 'UNKNOWN'
C*
C*  (Input/Output)
C*           CHARACTER * (*)  DSNAME   (In)  default dataset name
C*                                     (Out) name of opened dataset
C*           INTEGER   *  4   ICONT    Context switch
C*                                     must be 0 for first call
C*
C*  (Output)
C*           CHARACTER * (*)  SELECT   Command string used to select
C*                                     this dataset.
C*
C*  (Function Value) .True. for successful completion
C*                   .False. for can't open
C*
C*  (Screen mode)  If first character of PROMPT or DSNAME is '$',
C*
C*  (Relation)  Calls XOPEN, YOPEN
C*
CC**********************************************************************
C
C     Logical Function QOPEN( PROMPT,LUN,DSNAME,MODE,FRM,STS,
C    &                        ICONT )
C
      Logical Function Qopen( Prompt,Lun,Dsname,Mode,Frm,Sts )
      Implicit None
C
C ARG
C (Input)
      Character * (*)  PROMPT
      Integer   *   4  LUN
      Character * (*)  MODE, FRM, STS
C (Input/Output)
      Character * (*)  DSNAME
      Integer   *   4  ICONT
C
C VAR
      Integer * 4  ILOG, IMODE, LDS, NARGC, IICONT
      SAVE  IMODE
C
C FUNC
      Integer * 4  CLILOG, LENRD
      Logical * 4  XOPEN
C
C BEGIN
C     NARGC = NARGET()
      NARGC = 6
      If( NARGC .ge. 7 ) Then
        IICONT = ICONT
      Else
        IICONT = 0
      End If
C
C ... have CLI to stop logging
      ILOG = CLILOG( 0 )
C
      Call TXTRD( PROMPT,DSNAME )
      ILOG = CLILOG( 0 )
      IMODE = 0
      QOPEN = XOPEN( LUN,DSNAME,MODE,FRM,STS )
C
      If( ILOG.eq.1 ) Then
        LDS = LENRD( DSNAME )
        Call CLWLOG( DSNAME(:LDS) )
      End If
C
C ... restore CLI logging mode
      ILOG = CLILOG( 1 )
C
      If( NARGC .ge. 7 ) Then
        If( IMODE .eq. 0 ) Then
          ICONT = 1
        Else
          ICONT = 2
        End If
      End If
C
C
      Return
      End
C
CC**********************************************************************
C
C  LOGICAL FUNCTION XOPEN(LUN,DSNAME,MODE,FRM,STS )
C
C  (Purpose) Open dataset
c            when '*' is included in the filename, datasets are searched
C            that match the given name and inquire on the console which
C            dataset to be selected.
C
C (Input)    LUN     logical unit number
C                    if LUN < 0, XOPEN will NOT open the dataset
C            MODE    'READ' / 'WRITE' / 'DIRECT'
C            FRM     'UNFORMATTED' / 'FORMATTED'
C            STS     'OLD' / 'NEW' / 'SHR' / 'UNKNOWN'
C (In/Out)   DSNAME  (In)  dataset name to be opened
C                    (Out) dataset name to have been opened
C (Output)   Function return value  .True.  = Succesful Completion
C                                   .False. = Can't Open
C
C (Author)   A.Shirahasi, Univ. of Tokyo
C (Date)     18-Nov-1986
C (Update)   3-Dec-1986, bug fixed
C            4-Dec-1986, change for QOPEN
C
C           16-Jun-1992, DIRECT access file
C            2-Jul-1992, fix a bug for negative LUN, DIRECT=recl
C
CC**********************************************************************
C
        Logical Function XOPEN( LUN,DSNAME,MODE,FRM,STS )
        Implicit None
C common
      include 'clidef.inc'
C
C ARG
        Integer   * 4    LUN
        Character * (*)  DSNAME, MODE, FRM, STS
C
C CONST
        Character * 1  SQ
        Parameter( SQ = '''' )
C
C VAR
        Character * (LINSIZ)  FULNAM
        Integer * 4  LDS, IERR
C
C FUNC
        Logical * 4  OPENIT
        Integer * 4  LENRD
C
C BEGIN
C        If( LUN.lt.0 ) Then
C          XOPEN = .TRUE.
C          Return
C        End If
C
C
        LDS = LENRD( DSNAME )
        If( LDS .eq. 0 ) Goto 9000
C       Call CLstrupc( LDS,DSNAME )
C
        Fulnam = Dsname
C
C       FULNAM = ' '
C       If( DSNAME(1:1) .eq. '.' ) Then
C         Call GETUID( FULNAM(1:4) )
C         FULNAM(5:) = DSNAME
C       Else
C         FULNAM = DSNAME
C       End If
C
        If ( INDEX(FULNAM,'*').gt.0 .OR. INDEX(FULNAM,'?').gt.0 ) Then
          Call CLISTC( FULNAM,LENRD(FULNAM),IERR )
          If( IERR .ne. 0 ) Goto 9000
        End If
C
        If( OPENIT( LUN,FULNAM,MODE,FRM,STS ) ) Then
          DSNAME = FULNAM
          XOPEN = .TRUE.
          Goto 9900
        End If
C
9000    XOPEN = .FALSE.
        Goto 9900
C
9900    Continue
        Return
        End
C
C
      Subroutine CLISTC( NAME,LNAME,IERR )
      Implicit None
C common
      include 'clidef.inc'
C
C ARG
      Character * (*)  NAME
      Integer          LNAME, IERR
C
C CONST
      Integer * 4  MAXDSN
      Parameter( MAXDSN = 100 )
C
C VAR
      Character * (LINSIZ)  DSNAME( MAXDSN )
      Integer  J, K, L, NO
C
C FUNCTION
      Integer  Lenrd
C
C BEGIN
 100  Continue
      Call FINDIT( NAME,LNAME,DSNAME,MAXDSN,NO )
      If( NO.le.0 ) Goto 9000
C
      Write( 6,'(1X)' )
      Do J = 1, NO
        L = Lenrd(DSNAME(J))
        Write( 6,'(1X,I3,1X,A)' ) J,DSNAME(J)(:L)
      End Do
      Write( 6,'(1X)' )
C ... always inquire on console
      Call CLSTAR( 1 )
      Call INTRDX( '?Select number or input new file name',K,NAME )
      If ( Index('#0123456789+-', NAME(1:1)) .eq. 0 ) Then
        If ( INDEX(NAME,'*').gt.0 .OR. INDEX(NAME,'?').gt.0 ) Then
          Goto 100
        Else
          IERR = 0
          Return
        End If
      End If
      If( K .le. 0 .or. K .gt. NO ) Goto 9000
      L = Lenrd(DSNAME(K))
      NAME = DSNAME(K)(:L)
      IERR = 0
      Return
9000  IERR = -1
      Return
      End
C
C
      Subroutine FINDIT( NAME,LNAME,DSNAME,MAXDSN,NDSN )
      Implicit None
C
C ARG
C (Input)
      Character * (*)  NAME
      Integer   * 4    LNAME, MAXDSN
C (Output)
      Character * (*)  DSNAME( MAXDSN )
      Integer   * 4    NDSN
C VAR
      Integer   * 4    DIRP
C
C FUNCTION
      Integer  cli__OPENDIR, cli__READDIR
C
C BEGIN
      NDSN = 0
      DIRP = cli__OPENDIR( NAME(:LNAME) )
      If( DIRP.ne.0 ) Then
        Do While ( NDSN .lt. MAXDSN )
          If ( 0 .eq. cli__READDIR( DIRP, DSNAME(NDSN+1) ) ) Goto 900
          NDSN = NDSN + 1
        End Do
 900    Continue
      End If
      Call cli__CLOSEDIR( DIRP )
C
      Return
      End
C
C
      Logical Function OPENIT( LUN,DSN,MODE,FRM,STS )
      Implicit None
C common
      include 'clidef.inc'
C
C (Update)
C     10-Feb-88, Ask member name for PO file without member name.
C                Note: DSN may be overwritten in this routine.
C     28-Sep-88, Can create a new member in an old PO dataset.
C
C     16-Jun-92, DIRECT access file
C      2-Jul-92, DIRECT=recl
C      3-Jul-92, LIST
C      9-Oct-92, ask overwrite in case of LUN<0
C
C ARG
C (Input)
      Integer * 4  LUN
      Character * (*)  DSN, MODE, FRM, STS
C
C CONST
      Character * 1  SQ
      Parameter( SQ = '''' )
C
C VAR
      Character * (LINSIZ)  DSNAME
      Character *  6  KMODE
      Character * 11  KFRM
      Character *  7  KSTS
      Character *  4  CC
      Logical         LEXIST
      Integer   *  4  LDSN, LFRM, LE, Reclen
C
C FUNC
      Integer * 4  LENRD
C
C BEGIN
C     LDSN = LENRD( DSN )
C     Write(*,*) 'OPENIT: LUN=',LUN,' DSN=',DSN(:LDSN)
C     Write(*,*) 'MODE,FRM,STS=',MODE,',',FRM,',',STS
C      If( LUN .lt. 0 ) Then
C        OPENIT = .TRUE.
C        Return
C      End If
C
      DSNAME = DSN
      LDSN = LENRD( DSNAME )
C
      If( MODE(1:1).eq.'R' .or. MODE(1:1).eq.'r' ) Then
        KMODE = 'READ'
      Else If( MODE(1:1).eq.'W' .or. MODE(1:1).eq.'w' ) Then
        KMODE = 'WRITE'
      Else If( MODE(1:1).eq.'B' .or. MODE(1:1).eq.'b' ) Then
        KMODE = 'BOTH'
      Else If( MODE(1:1).eq.'D' .or. MODE(1:1).eq.'d' ) Then
        KMODE = 'DIRECT'
        LE = Index(MODE,'=')
        IF( LE.gt.0 ) Then
          Read(Mode(LE+1:),*) RecLen
        Else
          RecLen = 1024
        End If
      Else
        Goto 9000
      End If
C
      LFRM = LENRD( FRM )
      If( FRM(1:1).eq.'U' .or. FRM(1:1).eq.'u' ) Then
        KFRM = 'UNFORMATTED'
      Else If( FRM(1:1).eq.'F' .or. FRM(1:1).eq.'f' ) Then
        KFRM = 'FORMATTED'
        CC = 'NONE'
      Else If( FRM(1:1).eq.'L' .or. FRM(1:1).eq.'l' ) Then
        KFRM = 'FORMATTED'
        CC = 'LIST'
      Else
        Goto 9000
      End If
C
      If( STS(1:1).eq.'N' .or. STS(1:1).eq.'n' ) Then
        KSTS = 'NEW'
      Else If( STS(1:1).eq.'O' .or. STS(1:1).eq.'o' ) Then
        KSTS = 'OLD'
      Else If( STS(1:1).eq.'S' .or. STS(1:1).eq.'s' ) Then
        KSTS = 'SHR'
      Else If( STS(1:1).eq.'U' .or. STS(1:1).eq.'u' ) Then
        KSTS = 'UNKNOWN'
      Else
        Goto 9000
      End If
C
C     If( (IDSN.eq.1 .or. IDSN.eq.3) .and. KSTS(1:3).eq.'NEW' ) Then
C       KSTS = 'OLD'
C     End If
C
C     Write(*,*) 'KMODE=',KMODE,',KFRM=',KFRM,',KSTS=',KSTS
      If( KMODE(1:1).eq.'R' ) Then
        INQUIRE( FILE=DSNAME(:LDSN),EXIST=LEXIST,ERR=9000 )
        If( .not.LEXIST ) Goto 9000
        If( Lun.gt.0 ) Then
          OPEN( UNIT=LUN,FILE=DSNAME(:LDSN)
     &          ,FORM=KFRM,STATUS=KSTS,ERR=9000 )
        End If
      Else If( KMODE(1:1).eq.'W' .or.
     &         KMODE(1:1).eq.'B' .or.
     &         KMODE(1:1).eq.'D' ) Then
          If( KSTS(1:3).eq.'NEW' ) Then
C ... delete a file to be overwritten. (for UNIX)
            Call CLowrt( Dsname(:LDSN) )
            INQUIRE( FILE=DSNAME(:LDSN),EXIST=LEXIST,ERR=9000 )
            If( LEXIST ) Goto 9000
          End If
          If( KMODE(1:1).eq.'W' ) Then
            If( KFRM(1:1).eq.'U' ) Then
              If( Lun.gt.0 ) Then
                OPEN( UNIT=LUN,FILE=DSNAME(:LDSN),
     &                FORM=KFRM,STATUS=KSTS,ERR=9000 )
              End If
            Else If( KFRM(1:1).eq.'F' ) Then
              If( Lun.gt.0 ) Then
                OPEN( UNIT=LUN,FILE=DSNAME(:LDSN),Form=KFRM,
     &                STATUS=KSTS,ERR=9000 )
              End If
            End If
          Else If( KMODE(1:1).eq.'D' ) Then
C           Write(*,*) '...trying to open direct access file.'
            If( Lun.gt.0 ) Then
              OPEN( UNIT=LUN,FILE=DSNAME(:LDSN),ACCESS='DIRECT',
     &              Recl=RecLen,FORM=KFRM,STATUS=KSTS,ERR=9000 )
            End If
          Else
            If( Lun.gt.0 ) Then
              OPEN( UNIT=LUN,FILE=DSNAME(:LDSN),
     &              FORM=KFRM,STATUS=KSTS,ERR=9000 )
            End If
          End If
      End If
C
      OPENIT = .TRUE.
      Return
C
9000  OPENIT = .FALSE.
C     Write(*,*) 'but failed.'
C     PRINT *, DSNAME
      Return
      End
