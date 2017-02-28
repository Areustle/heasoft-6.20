C       SUBROUTINE FITERR(aveL,aveB,Nbound,a,b,phi,Rc)
C
C
C  $Id: fiterr.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++            effect: 
C++	Ellipse fit parameters, a major 
C++	axis, b minor axis, phi position angle of major axis, RMS deviation
C++	of ellipse fit from region boundry. a,b,phi in radian. nmap,xrrmap
C++	are used for scratch space.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C  $Log: fiterr.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/12/26 17:28:04  irby
C  Change negative exponents to e.g. -1*number instead of -number for f90
C  compatibility (and put in parens (-1) where necessary).
C
C  Revision 1.1  2002/04/16 20:27:30  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:40  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:52  jae
c Subroutine Module for like V5.00
c
C%   Changes:
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE FITERR(aveL,aveB,Nbound,a,b,phi,Rc)

C     Common blocks included
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/likrep.copy'
      INCLUDE '../COMMON/errrep.copy'
      INCLUDE '../COMMON/nmprep.copy'
      INCLUDE '../COMMON/xrrrep.copy'

      save

      character(80) id
      common /id/id
      REAL SIG(509),coef(3),W(3)
      data pi /3.1415926/


      id = '$Id: fiterr.f,v 1.3 2013/05/21 19:08:25 irby Exp $'

c     work in celestial coordinates in radians
      if (coord_sys.eq.'C') then
         theaveL=aveL*PI180
         theaveB=aveB*PI180
         do n=1,Nbound
	    TS_fine1(n)=TS_fine1(n)*PI180
	    TS_fine2(n)=TS_fine2(n)*PI180
         enddo
      else
c     convert to celestial coordinates
         IRET=0
         tempL=aveL*PI180
         tempB=aveB*PI180
         call CELGAL('GC',theaveL,theaveB,tempL,tempB,IRET)
         if (IRET.ne.0) then
            sigmsg='ERRMAP:CELGAL error'
            signal='C'
            RETURN
         endif
         do n=1,Nbound
	    if (verbose)print *,'l,b',n,TS_fine1(n),TS_fine2(n)
	    tempL=TS_fine1(n)*PI180
	    tempB=TS_fine2(n)*PI180
	    call CELGAL('GC',tmpL,tmpB,tempL,tempB,IRET)
	    TS_fine1(n)=tmpL
	    TS_fine2(n)=tmpB
	    if (IRET.ne.0) then
               sigmsg='ERRMAP:CELGAL error'
               signal='C'
               RETURN
	    endif
         enddo
      endif

      if (abs(theaveB/PI180)+Rc.gt.85.) then
         sigmsg='ERRMAP:source is near the celestial pole'
         signal='P'
         call error(0,loc)
         print *,'Eliptical fit may be in error.'       
      endif
      
c     convert to polar coordinates centered on mean position
      cosb=cos(theaveB)
      do n=1,Nbound
         tmpL=TS_fine1(n)
         tmpB=TS_fine2(n)
         diffL=tmpL-theaveL
c     check for wrap around
         if (diffL.lt.-pi) then
	    diffL=diffL+2.*pi
         elseif (diffL.gt.pi) then
	    diffL=diffL-2.*pi
         endif
         TS_fine1(n)=atan2((diffL)*cosb,tmpB-theaveB)
         TS_fine2(n)=1./(((diffL)*cosb)**2+(tmpB-theaveB)**2)
         SIG(n)=1.
         if (verbose)print *,'ra,dec,theta,r',
     &        n,tmpL/PI180,tmpB/PI180,
     &        TS_fine1(n)/PI180,(TS_fine2(n)**(-1.*0.5))/PI180
      enddo
      
c     fit r^-2(t)=coef1*cos^2(t)+coef2*sin^2(t)+coef3*2sin(t)cos(t)
      call SVDFIT(TS_fine1,TS_fine2,SIG,Nbound,
     &     coef,3,nmap,xrrmap,W,Nbound,3,CHISQ)

      if (signal.ne.' ') return
c     print *,coef
      
c     derive ellipse parameters from coef
      phi=0.5*atan(2.*coef(3)/(coef(1)-coef(2)))

      if (phi.lt.0.) then
         if (coef(3).lt.0.) then
	    phi=phi+pi
         else
	    phi=phi+pi/2.
         endif
      elseif (phi.gt.0.) then
         if (coef(3).lt.0.) then
	    phi=phi+pi/2.
         endif
      endif

      f=(cos(phi)**4-sin(phi)**4)
      d=(coef(1)*cos(phi)**2-coef(2)*sin(phi)**2)

      if (f/d.le.0.) then
c     print *,'f,d',f,d
         signal='Z'
         sigmsg='ERRMAP:a hyperbola fits better than an ellipse!.'
         return
      else
         a=sqrt(f/d)
      endif

      d=(coef(2)*cos(phi)**2-coef(1)*sin(phi)**2)

      if (f/d.le.0.) then
c     print *,'f,d',f,d
         signal='Z'
         sigmsg='ERRMAP:a hyperbola fits better than an ellipse!.'
         return
      else
         b=sqrt(f/d)
      endif

      if (b.gt.a) then
c     major and minor axes interchanged
         temp=a
         a=b
         b=temp
         phi=phi+pi/2.
         if (phi.gt.pi) phi=phi-pi
      endif

      return
      END
