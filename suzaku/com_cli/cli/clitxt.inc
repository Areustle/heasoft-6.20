C   30/11/87 712032212  MEMBER NAME  @CLITXT  (FORT)     M  FORTRAN
C
C File: clitxt.inc
C Description: common variables for CLI prompt
C
C HISTORY:
C     26-Feb-2005 Y.ISHISAKI, add 'c' on head for each variables
C     26-Feb-2005 Y.ISHISAKI, add cLINE, cLAST
C     26-Feb-2005 Y.ISHISAKI, IPNT moved from clflag.inc
C     16-May-2012 Y.ISHISAKI, merge CLITXT2 to CLITXT1 for g95 on MacOSX
C
      COMMON / CLITXT1 /
     &     cQUEST, cANSWE, cQUECOM, cQUES, cLINE,
     &     cLAST, IPNT
ccc      COMMON / CLITXT2 / cLAST, IPNT
C
      CHARACTER * (LINSIZ)
     &     cQUEST,      ! latest question (or prompt) on ARGRD
     &     cANSWE,      ! latest default answer on ARGRD
     &     cQUECOM,     ! additional prompt for temporary reading from console
     &     cQUES,       ! current prompt string
     &     cLINE        ! current input line buffer for next reading
C
      Integer * 4
     &     cLAST,       ! length of cLINE
     &     IPNT         ! pointer to internal read buffer cLINE, 0 for empty
C
C     for Emacs
C     ;;; Local Variables: ***
C     ;;; mode:Fortran ***
C     ;;; End: ***
