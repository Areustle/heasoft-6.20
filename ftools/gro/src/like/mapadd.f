C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPADD(BIAS,map,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: mapadd.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++    MAPADD = proc(MAP,BIAS_FACTOR) modifies (MAP)
C++            effect: BIAS_FACTOR is added to each bin of MAP
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: mapadd.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:37  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/07/31  19:06:30  jae
c addedadded COMMON cnfrep.copy and lines for LOC
c
c Revision 5.1  1996/02/29  20:51:45  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:53  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE MAPADD(BIAS,map,IMAPWIDTH,IMAPHEIGHT)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
        
      save
      
      character(80) id
      common /id/id
c     INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
      REAL MAP(IMAPWIDTH,IMAPHEIGHT)
      REAL BIAS                 !   The bias factor

      id = '$Id: mapadd.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='MAPADD'

      if(jae_mapadd)write(*,'("In routine ",a)') LOC

      DO 10 IL = 1,CTLMSZ1
         DO 10 IB=1,CTLMSZ2
            MAP(IL,IB) =  MAP(IL,IB) + BIAS
 10      CONTINUE
         
         RETURN
         END
c
