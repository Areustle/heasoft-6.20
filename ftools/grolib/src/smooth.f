      SUBROUTINE smooth (TASCCO, EFFSAR, nbrenr, nmod,nincl,nazim,ierr)
C=======================================================================
C* Function: attempt to correct the sensivity contour lines for TASC in
C*           and TASC not in coincidence
C*
C*
C* Method:
C*
C* Input:
C*   TASCCO - integer*4  ; Flag for TASC in/TASC out
C*   EFFSAR - real*4     ; array for eff. senst. areas (see SENSTV prog)
C*  (nbrenr,nmod,nincl,nazim)
C*   NBRENR - integer*4  ; total number of energy intervals
C*   NMOD   - integer*4  ; total number of field-of-view modes
C*   NINCL  - integer*4  ; total number of inclination angles
C*   NAZIM  - integer*4  ; total number of azimuth angles
C*
C* Output:
C*   EFFSAR - real*4     ; array for eff. senst. areas (corrected)
C*  (nbrenr,nmod,nincl,nazim)
C*   NBRENR - integer*4  ; total number of energy intervals
C*   NMOD   - integer*4  ; total number of field-of-view modes
C*   NINCL  - integer*4  ; total number of inclination angles
C*   NAZIM  - integer*4  ; total number of azimuth angles
C*   ierr   - integer*4  ; not in use
C*
C*
C* Calls:
C*   FITAZI - subroutine to calculate a fit line through three points
C*
C=======================================================================
C+ ISSUE:      STARTED:                PROGRAMMER: C. VON MONTIGNY
C+             UPDATED: 12 Jan 1995    BY  CVM
C+ $Id: smooth.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C+ 2.0	E.S.Panduranga	09/04/91	Moved source from IBM to SUN.
C+					Stripped off trailing blanks.
C+					Declare undeclared variables.
C+					Fitazi subroutine put in another file.
C+ $Log: smooth.f,v $
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
c Revision 1.1  1996/08/15  17:27:23  programs
c Initial revision
c
c Revision 2.1  1991/09/09  17:42:41  nancy
c First controlled version on the Sun.
c
C+ $Id: smooth.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C+ 2.2  C.v.Montigny   01/12/95         constrain averaging to 3 azimuths!
C+
C=======================================================================
      real*4     effsar(nbrenr,nmod,nincl,nazim) ,y(3)
      integer*4  TASCCO

Cesp  Declaring undeclared variables
      integer	nbrenr, nmod, nazim, ierr, imod, nincl, ienr, i
      integer	iazim, ik
      real	average

      save

      character(80)	id
      common	/id/	id
C      id = '$Id: smooth.f,v 3.2 2013/05/21 19:08:27 irby Exp $'
      id = '$Id: smooth.f,v 3.2 2013/05/21 19:08:27 irby Exp $'

      do 20 imod=2,nmod
      do 20 ienr=1,nbrenr
            IF (tascco.eq.1) THEN 
Cesp	      !calculate average of azimuth values
Cesp	      ! for 5 and 10 deg inclination
              DO 25 i = 2 , 3
              average=alog10(effsar(ienr,imod,i,1))+
     &        alog10(effsar(ienr,imod,i,2))
     &                            +alog10(effsar(ienr,imod,i,3))
              average=average/3
              average=10.**average
              do 25 iazim=1,3
                    effsar(ienr,imod,i,iazim)= average
  25          continue
            ELSE IF (tascco.eq.0) THEN
Cesp	      !fit a line through azimuth val.
              write(6,'('' smooth:'',3f9.2)') (effsar(ienr,imod,3,ik),ik
     &               =1,3)
              DO 28 i = 2 , 3
Cesp	      ! for 5 and 10 deg inclination
              y(1)=alog10(effsar(ienr,imod,i,1))
              y(2)=alog10(effsar(ienr,imod,i,2))
              y(3)=alog10(effsar(ienr,imod,i,3))
              call FITAZI (y)            
Cesp	      ! fit a line through y-values
              effsar(ienr,imod,i,1) =10.**y(1)
              effsar(ienr,imod,i,2) =10.**y(2)
              effsar(ienr,imod,i,3) =10.**y(3)
  28          CONTINUE
            END IF
  20  continue
      return
      end
C=======================================================================
