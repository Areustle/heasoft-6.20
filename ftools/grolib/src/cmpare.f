C=======================================================================
      SUBROUTINE CMPARE(VEC1,N1,VEC2,N2,SELCT)
C=======================================================================
C* VEC1(N1), VEC2(N2) - arrays to be compared
C* SELCT   -  .TRUE.  if the arrays are equal with respect to the sel.
C*                    criteria
C* SELCT   -  .FALSE. if the arrays are not equal with respect to the
C*                    selection criteria
C=======================================================================
C+ ISSUE: 2   STARTED: 21 JAN 1988    PROGRAMMER: C. VON MONTIGNY
C+            UPDATED: 24 MAR 1988    BY CVM
C+ $Id: cmpare.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C+ 2.0	E.S.Panduranga	06/21/91	Moved source from IBM to SUN.
C+					Stripped off trailing blanks.
C+					Declare undeclared variables.
C+ $Log: cmpare.f,v $
C+ Revision 3.2  2013/05/21 19:08:27  irby
C+ Change character*n to character(n) to silence warnings: "Obsolescent
C+ feature: Old-style character length".
C+
C+ Revision 3.1  2002/04/16 20:32:07  irby
C+ Additions to libgro - previously these codes existed in the following
C+ libraries:
C+
C+   libsenstv
C+   libsysutil
C+   libutil
C+   libftio
C+
c Revision 1.1  1996/08/15  17:27:23  programs
c Initial revision
c
c Revision 2.1  1991/09/09  17:42:41  nancy
c First controlled version on the Sun.
c
C=======================================================================

Cesp  ! Declaring variables undeclared on the IBM !
      integer	n1, n2, nelem, icheck, i

      INTEGER*2 VEC1(N1),VEC2(N2)
      LOGICAL SELCT

      character(80)	id
      common	/id/	id
      id='$Id: cmpare.f,v 3.2 2013/05/21 19:08:27 irby Exp $'
C-----------------------------------------------------------------------
      NELEM=N1
      IF(N2.LT.N1) NELEM=N2
      SELCT=.FALSE.
      ICHECK=0
      DO 10 I=1,NELEM
      IF(VEC1(I).EQ.VEC2(I)) ICHECK=ICHECK+1
   10 CONTINUE
      IF(ICHECK.EQ.NELEM)SELCT=.TRUE.
C-----------------------------------------------------------------------
      RETURN
      END
