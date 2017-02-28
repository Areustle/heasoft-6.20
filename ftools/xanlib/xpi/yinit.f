**==yinit.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
*
* These are the subroutines which will capture the command line and replace
* the xparse routines
*
* yinit clears out all of the arrays
*
      SUBROUTINE YINIT
 
      IMPLICIT NONE
 
      INCLUDE 'yaccfor.inc'
      INCLUDE 'tbl.inc'
      INTEGER i
 
      SCOm = ' '
      SLIne = ' '
      DO 100 i = 1 , MAXPARS
         SPArs(i) = ' '
         SVAl(i) = ' '
 100  CONTINUE
      NPArs = 0
      COMval = .FALSE.
      CPAr = 1
 
      DO 200 i = 1 , TBLPMAX
         TBLpstat(i) = .TRUE.
 200  CONTINUE
      RETURN
      END
