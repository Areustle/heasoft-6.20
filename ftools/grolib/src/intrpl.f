C=======================================================================
      SUBROUTINE INTRPL(YX,X,X1,X2,Y1,Y2)
C=======================================================================
C* Linear interpolation at point X between the two knots X1 and X2.
C* y1=f(x1), y2=f(x2): result  yx=f(x)
C*
C*
C=======================================================================
C+ ISSUE: 2   STARTED: 24 JUN 1988    PROGRAMMER: C. VON MONTIGNY
C+            UPDATED: 13 Aug 1990    BY  CVM
C+ $Id: intrpl.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C=======================================================================
C%  CHANGES:
C%  13 Aug 1990 BY CVM: underflow suppressed
C+ 2.0	E.S.Panduranga	06/21/91	Moved source from IBM to SUN.
C+					Stripped off trailing blanks.
C+					Declare undeclared variables.
C+					Commented call to xuflow.
C+ $Log: intrpl.f,v $
C+ Revision 3.2  2013/05/21 19:08:27  irby
C+ Change character*n to character(n) to silence warnings: "Obsolescent
C+ feature: Old-style character length".
C+
C+ Revision 3.1  2002/04/16 20:32:12  irby
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
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Cesp  ! declaring variables undeclared on IBM !
      real	yx, x, x1, x2, y1, y2, slope

      character(80)	id
      common	/id/	id
      id = '$Id: intrpl.f,v 3.2 2013/05/21 19:08:27 irby Exp $'

Cesp  ! The following is not available on the SUN !
Cesp  ! Check later if an equivalent function is available !
Cesp  call xuflow(0)

      SLOPE= (Y2-Y1)/(X2-X1)
      YX= SLOPE* (X-X1) + Y1
      RETURN
      END
