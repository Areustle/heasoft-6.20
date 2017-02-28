C
C File: opnrd.f
C Description: Collection of input redirection routines
C
C Public:  OPNRD, LUNRD
C
C History:
C     19-Jun-1992, OPNRD, LUYNRD returns status
C     25-Feb-1997 Y.ISHISAKI, set file name in FILIST common in Opnrd & Lunrd
C     25-Feb-1997 Y.ISHISAKI, check LPTRSV in Opnrd & Lunrd
C     25-Feb-1997 Y.ISHISAKI, reset FILINE in Opnrd & Lunrd
C     26-Feb-2005 Y.ISHISAKI, do not change Opt_DEBUG (formerly ISNGL) status
C     28-May-2005 Y.ISHISAKI, Character * 256 -> Character * (LINSIZ)
C
C ---------
C   OPNRD       ... redirect input to indirect file (1st word) with arguments
C ---------
      Integer Function Opnrd( LINE )
      Implicit None
C input
      Character * (*)  LINE
C common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
C local
      Integer  LUN, Lfile, Ierr
      Character * (LINSIZ)  File
C begin
      If ( LPTRSV.gt.0 ) LUNPTR = LPTRSV
      If( LUNPTR .ge. MAXNST ) Then
        Call CLIERR( 2,'command files nest too deep' )
        Opnrd = -1
        Return
      End If
C
ccc      Opt_DEBUG = 0
      Call CLgetlun( LUN )
      LUNPTR = LUNPTR + 1
      LUNLST(LUNPTR) = LUN
      FILINE(LUNPTR) = 0
      Call CLargs(LINE, File, Lfile)
      Call CLOPEN( File(:Lfile),LUN,'R',IERR,'.com' )
      If( IERR .ne. 0 ) Then
        If( LUNPTR .gt. 0 ) Then
          LUNPTR = LUNPTR - 1
        End If
        Call CLfreelun( LUN )
        Opnrd = -1
      Else
        Opnrd = 0
      End If
C
      Return
      End
C
C ---------
C   LUNRD       ... redirect input to LUN with no arguments
C ---------
      Integer Function Lunrd( LUN )
      Implicit None
C input
      Integer  LUN
C common
      include 'clidef.inc'
      include 'clflag.inc'
      include 'clunit.inc'
C local
      Logical  YesNo
      Integer  Ldsn
      Character * (LINSIZ)  DSNAME
C function
      Integer  Lenrd
C begin
      If ( LPTRSV.gt.0 ) LUNPTR = LPTRSV
      If( LUNPTR .ge. MAXNST ) Then
        Call CLIERR( 2,'command files nest too deep' )
        Lunrd = -1
        Return
      End If
C
      Inquire( Unit=LUN,Opened=YesNo )
      If( YesNo ) Then
C
        DSNAME = '*LUN'
        Call CLitoa(LUN, DSNAME(5:))
        Ldsn = Lenrd(DSNAME)
ccc        Write (*,*) 'Lunrd: DSNAME='//DSNAME(:Ldsn)
C
        LUNPTR = LUNPTR + 1
        LUNLST(LUNPTR) = LUN
ccc        FILIST(LUNPTR) = '*** LUNRD ***'
        FILINE(LUNPTR) = 0
        Call CLargs(DSNAME(:Ldsn), ' ', Ldsn)
        Call CLmarklun(LUN)
        Lunrd = 0
      Else
        Call CLIERR( 2,'unit not yet opened' )
        Lunrd = -1
      End If
C
      Return
      End
C
