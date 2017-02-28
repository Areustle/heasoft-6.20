C   18/12/86 810101754  MEMBER NAME  @UNITRD  (FORT)     M  FORTRAN
C+
C File: clunit.inc (formerly unitrd.inc)
C Description: Common for CLI on Logical Unit Number handling
C
C History:
C     16-Nov-1989, add FILIST, FILINE and FILBUF
C     25-Feb-1997 Y.ISHISAKI, FILINE -> FILINE(0:LUNMAX)
C     20-Feb-2005 Y.ISHISAKI, add LUNBASE, define MAXNST = LUNMAX - 1
C     21-Feb-2005 Y.ISHISAKI, add MAXLABEL, MAXLOOP
C     22-Feb-2005 Y.ISHISAKI, remove IRD,IWR,HELSET,TTCHAN,TLUN
C     23-Feb-2005 Y.ISHISAKI, add ARGLST, remove FILIST
C     26-Feb-2005 Y.ISHISAKI, separate string into UNITRD2 for --pedantic
C     26-Feb-2005 Y.ISHISAKI, LOGFILE declaration moved from cllog.f
C     26-Feb-2005 Y.ISHISAKI, renamed from 'unitrd.inc' -> 'clunit.inc'
C     27-Feb-2005 Y.ISHISAKI, add LARGS for each length of ARGLST
C     01-Mar-2005 Y.ISHISAKI, add SEARCH_PATH, LSEARCH_PATH
C     02-Mar-2005 Y.ISHISAKI, add add @.sub, @.endsub support
C-
      Integer  LUNBASE, LUNMIN, LUNMAX,
     &     MAXNST, MAXLABEL, MAXLOOP, MAXSUB
      Parameter( LUNBASE = 80 ) ! actually use (LUNBASE+1) - (LUNBASE+LUNMAX)
                                !   --> modified to use CLgetlun(), since 2.00
      Parameter( LUNMIN = 21 )  ! LUNMIN - LUNBASE are used by CLgetlun
      Parameter( LUNMAX = 8 )   ! maximum number of LUNs consumed inside CLI
      Parameter( MAXNST = LUNMAX )      ! maximum allowed nesting for com file
      Parameter( MAXLABEL = 100 )       ! maximum number of labels
      Parameter( MAXLOOP = LUNMAX )     ! maximum loop nesting
      Parameter( MAXSUB = 10 )          ! maximum number of subroutines
C
      Common /CLUNIT1/ KLUN,
     &     LUNIN, LUNPTR, LUNLST(0:LUNMAX), LPTRSV,
     &     FILINE(0:LUNMAX),
     &     NLABEL, LENLBL(MAXLABEL), LUNLBL(MAXLABEL), POSLBL(MAXLABEL),
     &     LOOP_DEPTH, LOOP_READ_DEPTH, LOOP_LUN(MAXLOOP),
     &     LARGS(0:LUNMAX), LSEARCH_PATH,
     &     NSUB, SUB_LUN(MAXSUB), SUB_DEFLUN(MAXSUB), LSUB_NAME(MAXSUB),
     &     LSUB_DEFARG(MAXSUB), FLAG_SUB_READING
C
      Common /CLUNIT2/  ARGLST(0:LUNMAX), FILBUF, LOGFILE,
     &     LABELS(MAXLABEL),
     &     LOOP_CMD(MAXLOOP), LOOP_STAT(MAXLOOP),
     &     SEARCH_PATH,
     &     SUB_NAME(MAXSUB), SUB_DEFARG(MAXSUB)
C
      Integer * 4
     &     KLUN,        ! logical unit number used for command input logging
     &     LUNIN,       ! logical unit number of current input
     &     LUNPTR,      ! pointer to current input for LUNLST, 0 for console
     &     LUNLST,      ! 0-LUNMAX array of logical unit number
     &     LPTRSV,      ! LUNPTR save area during command suspend
     &     FILINE,      ! 0-LUNMAX array of line number currently reading
     &     LOOP_DEPTH,  ! current while/repeat nest level
     &     LOOP_READ_DEPTH,     ! while/repeat nest level in reading loop body
     &     LOOP_LUN,    ! 1-MAXLOOP array of LUN for temporary loop file
     &     LARGS        ! 0-LUNMAX array of line length of arguments
C
      Character * (LINSIZ)
     &     ARGLST,      ! 0-LUNMAX array of command procedure argument
     &     FILBUF,      ! most recent conetent of the line being read
     &     LOGFILE      ! current log file, if ILOG=1
C
      Character * 1
     &     LOOP_CMD,    ! 1-... array of kind of loop, W:while, R:repeat
     &     LOOP_STAT    ! 1-... array of loop status, s:next start, r:running
C
      Integer * 4
     &     NLABEL,      ! number of labels defined so far
     &     LENLBL,      ! 1-MAXLABEL array of string length of labels
     &     LUNLBL,      ! 1-... array of LUN where each label is defined
     &     POSLBL       ! 1-... array of file position indicator of labels
C
      Character * 64
     &     LABELS       ! 1-... array of label string
C
      Character * (LINSIZ)
     &     SEARCH_PATH  ! command procedure search path
C
      Integer * 4
     &     LSEARCH_PATH ! string length of SEARCH_PATH
C
      Integer * 4
     &     NSUB,        ! number of subroutines defined so far
     &     SUB_LUN,     ! 1-MAXSUB array of LUN of subroutine
     &     SUB_DEFLUN,  ! 1-MAXSUB array of LUN where subroutine is defined
     &     LSUB_NAME,   ! 1-MAXSUB array of string length of SUB_NAME
     &     LSUB_DEFARG, ! 1-MAXSUB array of string length of SUB_DEFARG
     &     FLAG_SUB_READING     ! 0:normal, 1:reading subroutine
C
      Character * 64
     &     SUB_NAME     ! 1-MAXSUB array of subroutine names
C
      Character * (LINSIZ) ! 1-MAXSUB array of file name
     &     SUB_DEFARG   !       in which subroutine is defined
C
C     for Emacs
C     ;;; Local Variables: ***
C     ;;; mode:Fortran ***
C     ;;; End: ***
