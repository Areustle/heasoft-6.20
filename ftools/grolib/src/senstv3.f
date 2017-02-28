      SUBROUTINE SENSTV3(NBRENR,IMOD,XINCL,AZIM,EFFSAR,EFSENS,caltbl)
C=======================================================================
C* Version: 2.3                        Date: 21 Nov 1994                
C*
C* Function:
C*   This subroutine returns the effective sensitive area values for one  
C*   arbitrary incidence direction, one triggermode combination and a set 
C*   of energy ranges.
C*
C* Method:                    
C*   Variable 'caltbl' included in order to select between 'old Calibration
C*   files' (caltbl=0), 'Vertical only' .and. 'PMT C43 off' (caltbl=5)
C*   as well as 'ALL open' .and. PMT C43 off' (caltbl=10) as well as both
C*   'Fanmodes' (caltbl=15 and 20). 
C*   The value of 'caltbl' determines whether the calibration files contain  
C*   3 azimuth angles or 16. In case of 3 azimuth angles symmetry is 
C*   assumed and used for calculating the sensitive area for any arbitrary 
C*   incidence angle. If there are 16 azimuth angles then symmetry can
C*   not be assumed and the sensitive area at any incidence angle must be
C*   interpolated by using the 16 azimuths.
C*
C* Input:
C*   NBRENR           - integer*4; number of energy intervals (max=10).
C*   IMOD             - integer*4; viewing mode 
C*   XINCL            - real*4; arbitrary inclination angle
C*   AZIM             - real*4; arbitrary azimuth angle
C*   EFFSAR           - real*4; array containig the effective sensitive
C*    (NBRENR,74,9,16)          area as returned by SENSTV2.
C*   CALTBL           - integer*4; indicator which kind of calibration 
C*                                 files is used (3 or 16 azimuths, resp)
C*				           0 = old calibration files (azimuth=3)
C*				           5 = Vertical only, C43 off (azimuth=16)
C*				          10 = All open, C43 off (azimuth=16)
C*					  15 = Fanmode, along x-axis
C*					  20 = Fanmode, along y-axis
C*
C* Output:
C*   EFSENS           - real*4; array containing the effective sensitive
C*    (NBRENR)                  area for the arbitrary incidence angle 
C*                              and energy intervals.
C*
C* Calls:
C*   MAP    - subroutine which maps the viewing mode and the arbitrary 
C*            incidence angle back onto the calibrated sector of angles. 
C*   INTRPL - linearly interpolates between two points
C*
C* Common blocks: 
C*                none
C*
C=======================================================================
C+ ISSUE: 2   STARTED: 14 DEC 1988    PROGRAMMER: C. VON MONTIGNY
C+            UPDATED: 05 Jan 1996    BY  CVM
C+ $Id: senstv3.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C=======================================================================
C%  CHANGES:
C%  12 JUL 1990 BY CVM: MAP OF INCOMMING PHOTON ONTO CALIB. SECTOR INCL.
C%  20 JUL 1990 BY CVM: changed according to new calfil files
C%  13 AUG 1990 BY CVM: output removed. Subroutine MAP +INTRPL attached
C%  14 AUG 1990 BY CVM: argument list changed according to documentation
C+ 2.0	E.S.Panduranga	06/21/91	Moved source from IBM to SUN.
C+					Stripped off trailing blanks.
C+					Declare undeclared variables.
C+			09/05/91	Merged changes made by Corrina
C+ $Log: senstv3.f,v $
C+ Revision 3.2  2013/05/21 19:08:27  irby
C+ Change character*n to character(n) to silence warnings: "Obsolescent
C+ feature: Old-style character length".
C+
C+ Revision 3.1  2002/04/16 20:32:13  irby
C+ Additions to libgro - previously these codes existed in the following
C+ libraries:
C+
C+   libsenstv
C+   libsysutil
C+   libutil
C+   libftio
C+
c Revision 1.1  1997/01/23  17:01:50  programs
c Initial revision
c
c Revision 1.2  1996/08/15  17:58:33  programs
c implemented FAN-modes
c
c Revision 2.1  1991/09/09  17:42:41  nancy
c First controlled version on the Sun.
c
C%  21 Nov 1994 BY CVM: -variable CALTBL included 
C%                      -dimension of array EFSENS increased to 16 azim. 
C%                      -array and data values for IAZIM increased 
C%  12 Dec 1994 BY CVM:  take care of new fov-mode imod=75 
C%  17 Jan 1995 BY CVM:  handle azimuth angles between 337.5 and  
C%                       360. degrees correctly. 
C%  15 May 1995 BY CVM:  mapping of direction modes also for new 
C%                       calibration files! 
C%
C%  05 Oct 1995 BY CVM:  fanmode included
C%  05 Jan 1996 BY CVM:  comments updated
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Cesp  ! declaring variables undefined on IBM !
      integer	nimod, nincl, nazim, nbrenr, imod, imodnw
      integer	ia, ia1, ia2, iaz, iaz1, iaz2, ie, caltbl
      real	xincl, azim, theta2, phi2, a1, a2, az1, az2
      real	ef1, ef2, val1, val2, valf

      PARAMETER(NIMOD=74,NINCL=9)
      REAL IANGLE(9),IAZIM(17)
      REAL EFSENS(NBRENR),EFFSAR(NBRENR,NIMOD,NINCL,16)

      save

      DATA IANGLE/0.,5.,10.,15.,20.,25.,30.,35.,40./ 
      DATA IAZIM/0.,22.5,45.,67.5,90.,112.5,135.,157.5,180.,202.5,225.,
     $ 247.5,270.,292.5,315.,337.5,360./ 
      data iaz1/0/, iaz2/0/, ia1/0/, ia2/0/

      character(80)	id
      common	/id/	id
      id = '$Id: senstv3.f,v 3.2 2013/05/21 19:08:27 irby Exp $'
C ----------------------------------------------------------------------
C                               MAP INCOMMING PHOTON ONTO CALIBRATION
C                               SECTOR
      CALL MAP ( XINCL,AZIM,IMOD,THETA2,PHI2,IMODNW)
      		!returns  theta2 always equal xincl !!!

      IF (caltbl .eq. 0) THEN
	 nazim=3
      ELSE                           
         nazim=16
C                                    
         if (caltbl .eq. 5) then 
             phi2=azim   !TAKE Azimuth AS IS
             imodnw=74  !remember: MODE 75 has been put
                        !into the place for mode 74 in 
         	        !the effsar array, in order to 
         		!avoid increasing the dimension
         		!for this anyway very large array.
         else if (caltbl .ge. 15) then
             imodnw=imodnw-13  !see above. similar for fanmodes
         endif
      END IF    
      IF (phi2.ge.360.)  phi2=0.
      IF (XINCL.gt. 40.) theta2=40.
C ----------------------------------------------------------------------
C                                    DETERMINE ADJACENT ANGLES OF
C                                    INCLINATION ANGLE THETA2

      DO 10 IA=1,8
        IF(THETA2.GE.IANGLE(IA) .AND. THETA2.LE.IANGLE(IA+1)) THEN
              A1=IANGLE(IA)
              A2=IANGLE(IA+1)
             IA1=IA
             IA2=IA+1
              GOTO 100
        END IF
  10  CONTINUE
C                                    DETERMINE ADJACENT ANGLES OF
C                                    AZIMUTH ANGLE PHI2
 100  DO 20 IAZ=1,nazim
        IF(PHI2.GE.IAZIM(IAZ) .AND. PHI2.LE.IAZIM(IAZ+1)) THEN
              AZ1=IAZIM(IAZ)
              AZ2=IAZIM(IAZ+1)
             IAZ1=IAZ
             IAZ2=IAZ+1
             if (IAZ2 .ge. 17) IAZ2=1 
C                                    ! use entry for 0deg. azimuth.
              GOTO 200
        END IF
  20  CONTINUE
C                                    INTERPOLATE EFF. SENS. AREA
 200  CONTINUE
      DO 30 IE=1,NBRENR
         EF1=EFFSAR(IE,IMODNW,IA1,IAZ1)
         EF2=EFFSAR(IE,IMODNW,IA2,IAZ1)

C                                       in inclination angles
           CALL INTRPL(VAL1,THETA2,A1,A2,EF1,EF2)

         EF1=EFFSAR(IE,IMODNW,IA1,IAZ2)
         EF2=EFFSAR(IE,IMODNW,IA2,IAZ2)
C                                       in azimuth angles
           CALL INTRPL(VAL2,THETA2,A1,A2,EF1,EF2)
C                                     FINAL VALUE
           CALL INTRPL(VALF,PHI2,AZ1,AZ2,VAL1,VAL2)

         IF (XINCL.lt. 40.) then
             EFSENS(IE)=VALF
         ELSE IF (XINCL.ge.40. .and. XINCL.le.60) then
             EFSENS(ie)= VALF*(1 - 0.9*(XINCL-40.)/20.)
         ELSE IF (XINCL.gt. 60.) then
             EFSENS(IE)=VALF* 0.1
         END IF

  30  CONTINUE
C-----------------------------------------------------------------------
      RETURN
      END
