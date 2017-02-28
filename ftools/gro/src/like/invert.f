c
c        SUBROUTINE invert(A)
c
c
C  $Id: invert.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++  Spec: Use numerical recipes LU decomposition and substitution to
C	invert NoptxNopt matrix A
C=======================================================================
C LIKE Version: 4.8 DELIVERED: November 8th 1993, Programmer J.R. MATTOX
C+            Updated: 12 Jul 1991    by  JRM
C=======================================================================
C  $Log: invert.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:33  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  20:18:10  jae
c added lines for LOC
c
c Revision 5.1  1996/02/29  20:48:13  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:38  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE invert(A)

C     Common blocks used:
      INCLUDE '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
      DIMENSION A(3,3),Y(3,3),INDX(3)


      id = '$Id: invert.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='INVERT'

      if (jae_invert) write(*,'("In routine ",a)') LOC

      do i=1,Nopt
         do j=1,Nopt
            Y(i,j)=0.
         enddo
         Y(i,i)=1.
      enddo

c     decompose matrix
      call LUDCMP(A,Nopt,3,INDX,D)
      if (signal.ne.' ') return

      do j=1,Nopt
         call LUBKSB(A,Nopt,3,INDX,Y(1,j))
      enddo

      do i=1,Nopt
         do j=1,Nopt
            A(i,j)=Y(i,j)
         enddo
      enddo

      RETURN
      END
