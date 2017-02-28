      subroutine FITAZI (y)
C=======================================================================
C* Function:
C* makes a line fit through a number of datapoints
C*
C* Method:
C*
C* Input/output:
C*   y(3)   - real*4     ; 3-dim. array for y-values
C*
C=======================================================================
C+ ISSUE:      STARTED: 01 Jul 1991    PROGRAMMER: C. VON MONTIGNY
C+             UPDATED: 01 Jul 1991    BY  CVM
C+ $Id: fitazi.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C+ 2.0	E.S.Panduranga	09/04/91	Moved source from IBM to SUN.
C+					Stripped off trailing blanks.
C+					Declare undeclared variables.
C+ $Log: fitazi.f,v $
C+ Revision 3.2  2013/05/21 19:08:27  irby
C+ Change character*n to character(n) to silence warnings: "Obsolescent
C+ feature: Old-style character length".
C+
C+ Revision 3.1  2002/04/16 20:32:08  irby
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
Cesp  Declaring undeclared variables
      integer	n, i
      parameter (n=3)
      real*4      x(n),y(n)
      real*4      sum0,sum1,sum2,sum3,sum4
      real*4      a,b,zb,nb,r2,zr2,nr2
      data x /0.,22.5,45./

      character(80)	id

      save

      common	/id/	id
      id = '$Id: fitazi.f,v 3.2 2013/05/21 19:08:27 irby Exp $'

      sum0=0.
      sum1=0.
      sum2=0.
      sum3=0.
      sum4=0.
      DO i = 1,n
         sum0= sum0 + x(i)
         sum1= sum1 + y(i)
         sum2= sum2 + x(i)*y(i)
         sum3= sum3 + x(i)*x(i)
         sum4= sum4 + y(i)*y(i)
      END DO

c ---> calculate slope b of line:
      zb = sum2 - (sum0*sum1)/n
      nb = sum3 - (sum0*sum0)/n
      b = zb/nb

c ---> calculate y-axis cut of line:
      a = (sum1 - b * sum0)/n

c ---> calculate r square:
      zr2 = b * (sum2 - zb)
      nr2 = sum4 - (sum1*sum1)/n
      r2 = zr2/nr2

c ---> calculate new y-axis values:
      DO i = 1 , n
         y(i) = a + b*x(i)
      END DO
C-----------------------------------------------------------------------
      return
      end

