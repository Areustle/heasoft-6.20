C=======================================================================
C        function FUNC_limit(X)
C
C
C  $Id: func_lim.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C++	Called by golden.
C
C
C
C  $Log: func_lim.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:31  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:54  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:03  jae
c Subroutine Module for like V5.00
c
C

      function FUNC_limit(X)

C     Common blocks used:
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      
      save

      character(80) id
      common /id/id

      id = '$Id: func_lim.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='FUNC_lim'


      Counts=X
      CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)

      if(signal.ne.' ')then
         FUNC_limit=1.e10
         signal=' '
      else
         FUNC_limit=abs(lnL-target_lnL)
      endif

      return
      END
