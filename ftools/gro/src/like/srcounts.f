C        subroutine srcounts
C
C
C  $Id: srcounts.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C	effect: srcounts finds the counts estimate (negative values
C	allowed)
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C  $Log: srcounts.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:43  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:53:27  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:27  jae
c Subroutine Module for like V5.00
c
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine srcounts
C
C  Common blocks used:
C
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'

      save

      character(80) id
      common /id/id

      id = '$Id: srcounts.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='srcounts'
c
c     Guess at initial bounds:
c
      if (Counts.gt.0.5) then   ! use extant positive Counts estimate
         p0=Counts/2.
         p2=Counts*2.
      else                      ! start from scratch
         p0=0.
         p2=100.
      endif
c
c     Verify initial bounds:
c
      loop=0
 10   continue
      Counts=p0
      call CHOOD(map,bmap,gasmap,emap,tL0,XL0,CTLMSZ1,CTLMSZ2)
      
      if (xl0.ge.0.) then       ! minimum occurs at a smaller value of P
         p2=p0
         if (p0.gt.1.e-1) then
            p0=p0/1.5
         elseif (p0.lt.-1.e-1) then
            p0=p0*1.5
         else                   ! p0~=0, go negative
            p0=-1.
         endif

         loop=loop+1
         if (loop.gt.100) then
            write(sigmsg,'("SRCOUNTS: divergence in choice of p0: ",
     &           f5.1)') p0
            SIGNAL = '0'
            RETURN
         endif
         goto 10                ! try again
      endif
c
c     OK, best fit requires P > p0; how how about p2
c
      loop=0
 20   continue
      Counts=p2
      call CHOOD(map,bmap,gasmap,emap,tL0,XL2,CTLMSZ1,CTLMSZ2)
      
      if (XL2.le.0.) then       ! minimum occurs at a greater value of P
         if (p2.gt.1.e-1) then
            p2=p2*1.5
         elseif (p2.lt.-1.e-1) then
            p2=p2/1.5
         else                   ! p2~=0, go positive
            p2=1.
         endif

         loop=loop+1
         if (loop.gt.100) then
            write(sigmsg,'("SRCOUNTS: divergence in choice of p2: ",
     &           f5.1)') p2
            SIGNAL = '0'
            RETURN
         endif
         goto 20                ! try again
      endif
c
c     OK, best fit requires P < p2; do minimization
c
      P1=(P0+P2)/2.
C
C     The dbrent  routine  finds the minimum of L
C     
      lnL=DBRENT(p0,p1,p2,Cnts)
      Counts=Cnts
      if (signal.ne.' ') return
      
      RETURN
      END
c
