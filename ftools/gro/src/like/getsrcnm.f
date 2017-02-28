c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C        SUBROUTINE GETSRCNM()
C
C
C  $Id: getsrcnm.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C*
C=============================================================================
C*      effect: Calculates a GRO J2000 name for a source
C*              sets srcN variable to GRO style name for location srcL,srcB
C*                      
C=============================================================================
C LIKE Version: 5.0 DELIVERED: March 25th 1994, Programmer J.A. ESPOSITO
C+              UPDATED:     by  JAE
C=============================================================================
C* Subroutine Argument List
C* ^^^^^^^^^^^^^^^^^^^^^^^^
C* None
c=============================================================================
C  $Log: getsrcnm.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/04/18 19:34:09  irby
C  Bug fixes for OSF build:
C
C  - change "dreal" to acceptable replacement "dble"
C  - changed null string comparisons '' to ' ' - this may not be the best
C    solution.
C  - removed malloc.h from like.h (unnecessary, and not available under Darwin)
C  - Makefile generated using: mkmk version 1.81
C
C  Revision 1.1  2002/04/16 20:27:31  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:48:02  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:37:17  jae
c Subroutine Module for like V5.00
c
c
c
c=============================================================================
c
c   NOTE: this routine will retain a non-GRO J2000 name if the name has no 
C*        leading spaces.  Source names of the form GRO Jxxxxpddmm will be 
C*        changed if srcN has any of the values srcN(1:1)=' ', srcN(1:1)='.' 
C*        or srcN(1:3)='GRO' where x = {0-9}, p={+,-},d={0-9},mm=(0-9)
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
      SUBROUTINE GETSRCNM()

C     Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/likrep.copy'

      save

      character(80) id
      common /id/id
      character(1) pj


      id = '$Id: getsrcnm.f,v 1.3 2013/05/21 19:08:25 irby Exp $'
      LOC='getsrcnm'
c
c---> Check form of present name, return if indicated by name style.
c
      if (srcN(1:3).eq.'GRO'.or.srcN.eq.' '.or.srcN(1:1).eq.' '.or.
     &     srcN(1:1).eq.'.') then
         goto 1
      else
         return
      endif
c
 1    tmpL = srcL
      tmpB = srcB
c
c---> if galactic coordinates are in use, get appropriate Celestial values
c
      if (tmpL.lt.0)tmpL = tmpL + 360
      if (coord_sys.eq.'G') then
         IRET = 0
         CALL CELGALD('GC',tmpaL,tmpaB,tmpL,tmpB,IRET)
         tmpL = tmpaL
         tmpB = tmpaB
      endif
c
c---> Convert Celestial RA degrees to HHMM format where H=hours and M=minutes
C---> Convert Celestial DEC degrees to DD format where D=degrees
c
      tmpL = 24.*tmpL/360.
      itmpLhr = tmpL
      itmpLmn = (tmpL - float(itmpLhr))*60
      itmpBd = abs(tmpB)
      itmpBmn = 60*(abs(tmpB)-itmpBd)

      if (tmpB.lt.0) then
         pj ='-'
      else
         pj = '+'
      endif
      write(srcN,'("GRO J",i2.2,i2.2,A1,i2.2,i2.2)',
     &     err = 99)itmpLhr,itmpLmn,pj,itmpBd,itmpBmn
c     print *,srcN

      return

 99   signal ='S'
      SIGMSG = '--Error in write to srcN--'

      if (jae_getsrcnm) then
         print *,' error in getsrcnm '
         print *,' RA Hrs: ',itmpLhr
         print *,' RA Min: ',itmpLmn
         print *,'DEC Deg: ',itmpBd
         print *,'DEC Min: ',itmpBmn
         print *,'RA(hr),DEC(deg): ',tmpL,tmpB
      endif

      return
      end
c
