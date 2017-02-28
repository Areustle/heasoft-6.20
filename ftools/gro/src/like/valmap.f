C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      SUBROUTINE VALMAP(VALUE,MAP,IL,IB,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: valmap.f,v 1.2 2013/05/21 19:08:27 irby Exp $
C=======================================================================
C*     write value to map at specified pixel (longitude IL, lat. IB)
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated:     by  JRM
C=======================================================================
C  $Log: valmap.f,v $
C  Revision 1.2  2013/05/21 19:08:27  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:45  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:41:11  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:54:15  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:57:01  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE VALMAP(VALUE,MAP,IL,IB,IMAPWIDTH,IMAPHEIGHT)
C
C  Common blocks used:
C
      INCLUDE '../COMMON/errrep.copy'
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'

      save
	
      character(80) id
      REAL MAP(IMAPWIDTH,IMAPHEIGHT)

      common /id/id


      id = '$Id: valmap.f,v 1.2 2013/05/21 19:08:27 irby Exp $'
      LOC='VALMAP'


      if (jae_valmap) write(*,'("In routine ",a)') LOC

      MAP(IL,IB)=VALUE

      RETURN
      END
