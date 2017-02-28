c       SUBROUTINE MAPERR(medium,tmpmap,map_dim,org1,org2,scl,xlmax,
c     & xmaxl,xmaxb,aveL,aveB,deltaTS,Iconf,a,b,phi,RMS)
c
C
C  $Id: maperr.f,v 1.3 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++            effect: the error region map is displayed.
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C  $Log: maperr.f,v $
C  Revision 1.3  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2002/12/26 17:28:04  irby
C  Change negative exponents to e.g. -1*number instead of -number for f90
C  compatibility (and put in parens (-1) where necessary).
C
C  Revision 1.1  2002/04/16 20:27:38  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:51:51  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:03  jae
c Subroutine Module for like V5.00
c
C%   Changes:
c	jrm 12/29/93 add 'catalog point is off fine map' check & correction
C 	Installation of RCS lines: 23 AUG 1995 by JAE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE MAPERR(medium,tmpmap,map_dim,org1,org2,scl,xlmax,
     &     xmaxl,xmaxb,aveL,aveB,deltaTS,Iconf,a,b,phi,RMS)

c common blocks included
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/ctlrep.copy'
      INCLUDE '../COMMON/likrep.copy'
      INCLUDE '../COMMON/errrep.copy'
      INCLUDE '../COMMON/maprep.copy'
      INCLUDE '../COMMON/nmprep.copy'
      INCLUDE '../COMMON/xrrrep.copy'

      save
      
      character(80) id
      common /id/id
      REAL tmpmap(map_dim,map_dim)
      real tr(6),c(10),x(1),y(1)
      character(70) TITLE
      character(80) DTEXT1
      character(24)    medium

      id = '$Id: maperr.f,v 1.3 2013/05/21 19:08:26 irby Exp $'
c     set up the geometry vector
      diameter=float(map_dim)*scl
      tr(1) = org1-scl  
      tr(2) = scl
      tr(3) = 0.
      tr(4) = org2-scl 
      tr(5) = 0.
      tr(6) = scl

c     now set up the contour levels for 50%, 68%, 95%, 99% confidence
      c(1)=xlmax-1.4
      c(2)=xlmax-2.3
      c(3)=xlmax-6.
      c(4)=xlmax-9.1
      
      CALL PGBEGIN(0,medium,1,1)
      CALL PGENV(org1-scl+diameter,org1-scl,
     &     org2-scl,org2-scl+diameter,0,0)
      call PGSCH(1.2)
      call PGSLW(2)
      TITLE='EGRET position probability for '//srcN
      if(coord_sys.eq.'C') then
         call pglabel('right ascension','declination',TITLE)
      else
         call pglabel('galactic longitude','galactic latitude',TITLE)
      endif
c     plot 50%, 68%, 95%, 99% confidence contours
      call PGSLS(2)
      call pgcont(tmpmap,map_dim,map_dim,1,map_dim,1,map_dim,c,-4,tr)
c     plot the analysis confidence contour
      c(1)=xlmax-deltaTS
      call PGSLS(1)
      call pgcont(tmpmap,map_dim,map_dim,1,map_dim,1,map_dim,c,-1,tr)
      
      txtbgn=org1-scl+0.97*diameter
      CALL PGtext(txtbgn,org2-scl+0.05*diameter,
     &     'Counts map:'//mapdoc(1)(1:60))
      write(DTEXT1,
     &     "('Ranal',f5.1,'deg;',
     &     ' Counts',f8.1,'; Gmult',f7.3,'; Gbias',f7.3)")
     &     Ranal,Counts,Gmult,Gbias
      CALL PGtext(txtbgn,org2-scl+0.02*diameter,DTEXT1)

      CALL PGtext(txtbgn,org2-scl+0.96*diameter,
     &     'Dashed contours are 50%, 68%, 95%, 99% confidence.')
      write(DTEXT1,
     &     "('Solid is the ',I2,
     &     '% confidence contour which is analyzed in detail.')")
     &     Iconf
      CALL PGtext(txtbgn,org2-scl+0.93*diameter,DTEXT1)
      if(srcN(1:5).eq.'GRO J'.or.srcN(1:3).eq.'2CG')then
c     an unidentified source
         x(1)=srcL
         y(1)=srcB
         call PGPOINT(1,x,y,3)
              CALL PGtext(txtbgn,org2-scl+0.90*diameter,
     &        'The mean position (which is the catalog position) is '//
     &        'marked with '//CHAR(3)//';')
           else
c     an identified source
c     mean point within analysis confidence contour
              x(1)=aveL
              y(1)=aveB
              call PGPOINT(1,x,y,23)
c     catalog point
              x(1)=srcL
c     check the longitude 
              if(x(1).gt.org1+(float(map_dim)+0.5)*scl)then
                 if(x(1)-360..lt.org1+(float(map_dim)+0.5)*scl)then
                    x(1)=x(1)-360.
                 else
c     catalog point is off fine map
                    CALL PGtext(txtbgn,org2-scl+0.90*diameter,
     &                   'The catalog position is off the map; '//
     &                   'the mean position is marked with '//
     *                   CHAR(23)//';')
                    goto 170
                 endif
              endif
              
              y(1)=srcB
              call PGPOINT(1,x,y,15)
              CALL PGtext(txtbgn,org2-scl+0.90*diameter,
     &             'The catalog position is marked with '//CHAR(15)//
     &             '; the mean position with '//CHAR(23)//';')
 170          continue
           endif
           x(1)=xmaxl
           y(1)=xmaxb
           call PGPOINT(1,x,y,2)
           write(DTEXT1,
     &          "('max TS (',f7.1,') with ',A1)")xlmax,CHAR(2)
           CALL PGtext(txtbgn,org2-scl+0.87*diameter,DTEXT1)
           
           if(phi.ge.0)then 
c     ellipse has been fit

              if(coord_sys.eq.'G') then
                 CALL PGtext(txtbgn,org2-scl+0.11*diameter,
     &                'The elliptical fit is shown with dotted line; '//
     &                'meridian is dot-dash.')
              else
                 CALL PGtext(txtbgn,org2-scl+0.11*diameter,
     &                'The elliptical fit is shown with dotted line.')
              endif
              f=60./PI180
              write(DTEXT1,
     &             "('Fit:a,b',2f6.1,'arcmin;',
     &             ' pos ang',f7.1,'deg; RMS dev',f7.2,'arcmin')")
     &             a*f,b*f,phi/PI180,RMS*f
              CALL PGtext(txtbgn,org2-scl+0.08*diameter,DTEXT1)
              
c     draw elliptical fit
              if(coord_sys.eq.'C') then
                 theaveL=aveL*PI180
                 theaveB=aveB*PI180
              else
c     convert to celestial coordinates
                 IRET=0
                 tempL=aveL*PI180
                 tempB=aveB*PI180
                 call CELGAL('GC',theaveL,theaveB,tempL,tempB,IRET)
                 if(IRET.ne.0)then
                    sigmsg='MAPERR:CELGAL error'
                    signal='C'
                    RETURN
                 endif
              endif
              a2=a**2
              b2=b**2
              cosb=cos(theaveB)
              factor=PI180*3.6
              do i=1,100   
                 theta=float(i)*factor
                 epsilon=theta-phi
                 Rfit=((cos(epsilon)**2)/a2+(sin(epsilon)**2)/b2)**
     *                (-1.*0.5)
                 TS_fine1(i)=theaveL+Rfit*sin(theta)/cosb
                 TS_fine2(i)=theaveB+Rfit*cos(theta)
c     back to map coordinate system in degrees
                 if(coord_sys.eq.'C') then
                    TS_fine1(i)=TS_fine1(i)/PI180
                    TS_fine2(i)=TS_fine2(i)/PI180
                 else
                    call CELGAL('CG',TS_fine1(i),TS_fine2(i),tmpL,tmpB,
     *                   IRET)
                    if(IRET.ne.0)then
                       sigmsg='MAPERR:CELGAL error'
                       signal='C'
                       RETURN
                    endif
                    TS_fine1(i)=tmpL/PI180
                    TS_fine2(i)=tmpB/PI180
                    if(TS_fine1(i).gt.org1+(float(map_dim)+0.5)*scl)then
                       TS_fine1(i)=TS_fine1(i)-360.
                    elseif(TS_fine1(i).lt.org1-scl/2.)then
                       TS_fine1(i)=TS_fine1(i)+360.
                    endif
                 endif
              enddo
              call PGSLS(4)
              call PGLINE(100,TS_fine1,TS_fine2)
              if(coord_sys.eq.'G') then
c     draw meridian
                 do i=1,21   
                    TS_fine1(i)=theaveL
                    TS_fine2(i)=theaveB+float(i-1)*a/20.
                    call CELGAL('CG',TS_fine1(i),TS_fine2(i),tmpL,tmpB,
     *                   IRET)
                    if(IRET.ne.0)then
                       sigmsg='MAPERR:CELGAL error'
                       signal='C'
                       RETURN
                    endif
                    TS_fine1(i)=tmpL/PI180
                    TS_fine2(i)=tmpB/PI180
                    if(TS_fine1(i).gt.org1+(float(map_dim)+0.5)*scl)then
                       TS_fine1(i)=TS_fine1(i)-360.
                    elseif(TS_fine1(i).lt.org1-scl/2.)then
                       TS_fine1(i)=TS_fine1(i)+360.
                    endif
                 enddo
                 call PGSLS(3)
                 call PGLINE(20,TS_fine1,TS_fine2)
              endif 
           endif                !ellipse has been fit clause
           call pgend
           END
