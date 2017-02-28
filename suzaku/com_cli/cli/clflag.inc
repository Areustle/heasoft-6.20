C   08/01/87 804201909  MEMBER NAME  @FLAGRD  (FORT)     M  FORTRAN
C+
C File: clflag.inc (formerly, flagrd.inc)
C Author: A.Shirahashi, Univ. of Tokyo
C
C History:
C     27-May-1987, Add ICON
C     01-Dec-1987, Add IEOF
C     26-Sep-1989, Add IMEM and NMEM
C     01-Apr-1994, add Opt_AT, Opt_ECHO, Opt_EOF
C     11-Feb-1997 Y.ISHISAKI, add ISKIP
C     21-Feb-2005 Y.ISHISAKI, add FLAG_COMMENT, FLAG_IF, IBREAK
C     22-Feb-2005 Y.ISHISAKI, renamed from 'flagrd.inc' into 'clflag.inc'
C     22-Feb-2005 Y.ISHISAKI, remove ICTZ, IDEBUG, IFF, LOPNRD
C     26-Feb-2005 Y.ISHISAKI, add Opt_ALIAS/DOLLAR/PERCENT/BSLASH/INCO/HISTORY
C     26-Feb-2005 Y.ISHISAKI, separate string into FLAGRD2 for --pedantic
C     26-Feb-2005 Y.ISHISAKI, rename ISNGL -> Opt_DEBUG
C     26-Feb-2005 Y.ISHISAKI, IPNT, IFLQ, IFCR moved to clitxt.inc
C     26-Feb-2005 Y.ISHISAKI, rename ICON -> ICONCAT
C     02-Mar-2005 Y.ISHISAKI, add IF_LUN
C-
      Integer  MAXIF
      Parameter( MAXIF = 8 )     ! maximum if nesting
C
      COMMON /CLFLAG1/ IFLQ, IFCR, ICOM, ILOG, INHLOG, ICONCAT,
     &     ISKIP, FLAG_COMMENT, IF_DEPTH, IF_LUN(MAXIF),
     &     ISKIP_BEFORE_COMMENT,
     &     Opt_AT, Opt_ECHO, Opt_BREAK, Opt_DEBUG, Opt_ALIAS,
     &     Opt_DOLLAR, Opt_PERCENT, Opt_BSLASH,
     &     Opt_INCO, Opt_HISTORY, Opt_EOF
C
      COMMON /CLFLAG2/  FLAG_IGN_ELSE(MAXIF), ISKIP_BEFORE_IF(MAXIF)
C
      INTEGER * 4
     &     IFLQ,        ! 0:next character will be read from internal buffer
     &                  ! 1:read from file, note that this is read only status
     &     IFCR,        ! 1:normal, 0:don't read next line. Next input will
     &                  !   be an empty string if internal buffer is exhasuted.
     &     ICOM,        ! 1:next input will be read from console
     &     ILOG,        ! 1:logging command input are enabled
     &     INHLOG,      ! 1:temporary stop logging command input
     &     ICONCAT,     ! 1:line concatenation in in progress, start prompt '_'
     &     ISKIP,       ! 1:skipping input due to if or comment, echo with '-'
     &     ISKIP_BEFORE_COMMENT,        ! ISKIP status before entering comment
     &     FLAG_COMMENT,                ! 1:flag inside comment block @/* @*/
     &     IF_DEPTH,    ! nesting level of if block
     &     IF_LUN       ! 1-MAXIF array of LUN where if block is defined
C
      Character * 1  FLAG_IGN_ELSE      ! 'z':evaluate elif/else
     &                                  ! 'x':not evaluate elif/else
     &                                  ! 'e':else comes, wait for endif
C
      Character * 1  ISKIP_BEFORE_IF    ! 'z':ISKIP=0, 'x':ISKIP=1
C
      Integer * 4
     &     Opt_AT,      ! enable the interpretation of '@'
     &     Opt_ECHO,    ! enable echo, equivalent to '@+'
     &     Opt_BREAK,   ! capture SIGINT to suspend command procedure
     &     Opt_DEBUG,   ! enable single stepping mode of command procedure
     &     Opt_ALIAS,   ! enable alias substitution in KEYRD
     &     Opt_DOLLAR,  ! enable $(symbol) substitution
     &     Opt_PERCENT, ! enable %1-%9 subst. (for backward compatibility)
     &     Opt_BSLASH,  ! enable concatenation using '\' at the end of line
     &     Opt_INCO,    ! enable inline comments, @! or ! comment
     &     Opt_HISTORY, ! enable command line history ![-]n in console mode
     &     Opt_EOF      ! not implemented, besides CLIopt()
C
C     for Emacs
C     ;;; Local Variables: ***
C     ;;; mode:Fortran ***
C     ;;; End: ***
