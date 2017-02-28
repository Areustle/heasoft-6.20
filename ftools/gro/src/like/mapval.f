C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      FUNCTION MAPVAL(MAP,IL,IB,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: mapval.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*   RETURNS MAP VALUES AT SPECIFIED PIXEL (LONGITUDE IL, LAT. IB)
C*    mapval must be declared real in calling routine
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:     by  JRM
C=======================================================================
C  $Log: mapval.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:40  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:16:23  jae
c added COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:52:12  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:28  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      FUNCTION MAPVAL(MAP,IL,IB,IMAPWIDTH,IMAPHEIGHT)
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'

      save

      character(80) id
      common /id/id
      REAL MAP(IMAPWIDTH,IMAPHEIGHT)
      REAL MAPVAL

      id = '$Id: mapval.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='MAPVAL'


      if (jae_mapval) write(*,'("In routine ",a)')LOC

      MAPVAL=MAP(IL,IB)

      RETURN
      END
c
