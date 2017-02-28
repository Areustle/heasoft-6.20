C
C File: clidef.inc
C Description: constant definitions for CLI
C
C History:
C     27-Jul-1992, add CONTMARK
C     19-Nov-1994, add BQUOTE
C     27-May-1998 Y.ISHISAKI, Parameter -> Common for g77
C
C const
      Integer  LINSIZ
      Parameter( LINSIZ = 1024 )
C
      COMMON / CLIDEF /
     &     IBSLA,ICRET,IQUOTE,IBQUOTE,ISPACE,ITAB,ICONTMARK,
     &     INULL,ILINEFEED
C
      Integer * 4
     &     IBSLA,ICRET,IQUOTE,IBQUOTE,ISPACE,ITAB,ICONTMARK,
     &     INULL,ILINEFEED
C
      Character * 1
     &      BSLA, CRET, QUOTE, BQUOTE, SPACE, TAB, CONTMARK,
     &      NULL, LINEFEED
C
      EQUIVALENCE( IBSLA,BSLA )
      EQUIVALENCE( ICRET,CRET )
      EQUIVALENCE( IQUOTE,QUOTE )
      EQUIVALENCE( IBQUOTE,BQUOTE )
      EQUIVALENCE( ISPACE,SPACE )
      EQUIVALENCE( ITAB,TAB )
      EQUIVALENCE( ICONTMARK,CONTMARK )
      EQUIVALENCE( INULL,NULL )
      EQUIVALENCE( ILINEFEED,LINEFEED )
C
ccc      Parameter( BSLA = CHAR(92) )     ! back slash
ccc      Parameter( CRET = CHAR(13) )     ! carriage return
ccc      Parameter( QUOTE = CHAR(39) )    ! single quote
ccc      Parameter( BQUOTE = CHAR(96) )   ! back quote
ccc      Parameter( SPACE = CHAR(32) )    ! white space
ccc      Parameter( TAB = CHAR(9) )       ! tab
ccc      Parameter( CONTMARK = CHAR(92) ) ! continulation line mark
C
C     for Emacs
C     ;;; Local Variables: ***
C     ;;; mode:Fortran ***
C     ;;; End: ***
