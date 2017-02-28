C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPDIV(MAP1,MAP2,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: mapdiv.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++    mapdiv = proc(map1,map2) modifies (map1)
C++           effect: map1 is divided by map2, result stored in map1.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:     by  JRM
C=======================================================================
C  $Log: mapdiv.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:37  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:50  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:01  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE MAPDIV(MAP1,MAP2,IMAPWIDTH,IMAPHEIGHT)

C     Common blocks used:
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/errrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
C     
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
      REAL MAP1(IMAPWIDTH,IMAPHEIGHT),MAP2(IMAPWIDTH,IMAPHEIGHT)
      logical opps

      id = '$Id: mapdiv.f,v 1.2 2013/05/21 19:08:26 irby Exp $'

      opps=.false.

C     Step through bins:
      DO 10 J = 1,CTLMSZ2
         DO 10 I = 1,CTLMSZ1
            if (MAP2(I,J).ne.0.0) then
               MAP1(I,J) = MAP1(I,J) / MAP2(I,J)
            else
               if (MAP1(I,J).ne.0.0) then
                  MAP1(I,J) = 0.
                  if (.not.opps) then
                     write(lu(1),'("MAPDIV:Request to divide by ",
     *                    "zero, I,J",2i4)')I,J
                     opps=.true.
                  endif
               endif
            endif
 10      CONTINUE

         RETURN
         END
c
