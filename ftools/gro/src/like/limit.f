C        subroutine LIMIT(AX,CX,flag)
C
C
C  $Id: limit.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C	Effect: Used in derivation of upper limits; derives the number 
C	of counts for which -lnL=target_lnL
C
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     AX,CX           initial search range,
c     logical flag    !true find upper limit, false find lower
c
C=======================================================================
c  LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated:    by  JRM
C
C=======================================================================
C  $Log: limit.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/04/18 19:34:10  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:36  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:40  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:54:46  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine LIMIT(AX,CX,flag)

C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'

      save

      character(80) id
      common /id/id
      logical flag              !true find upper limit, false find lower

      id = '$Id: limit.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
      LOC='LIMIT'


c     check CX limit
      nloop=0
 10   Counts=CX
      CALL HOOD(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
c     check to insure no negative counts  model for any pixel
      if(signal.ne.' ')then
c     negative counts  model for at least one pixel
         signal=' '
         goto 20
      endif
      if(lnL.lt.target_lnL)then
c     CX not far enough for requested target_lnL
         nloop=nloop+1
         if(nloop.gt.100)then
            signal='L'
            sigmsg='LIMIT:Stuck in loop.'
            return
         endif
         delta=abs(CX)
         if(delta.lt.2.)delta=5. !reasonable size
         if(flag)then
            CX=CX+delta
         else
            CX=CX-delta
         endif
         goto 10
      endif

 20   BX=(AX+CX)/2.
      if(Verbose)
     &     write(6,'("Verbose(LIMIT):counts range:",2f7.1)') AX,CX
      
      call golden(AX,BX,CX,Counts_limit)
      if(signal.ne.' ') return
      
      return
      END
