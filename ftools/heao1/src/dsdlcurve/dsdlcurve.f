**==dsdlce.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998

CH   Jesse Allen (1.0.0 19 Apr 1998) Original working version
CH  Lorraine Breedon (1.1.0 26 Nov 1998) Remove COMMON blocks..pass variables
CH                                       via subroutine arguements\
CH                                       Remove subroutine dsdkst.f. Instead
CH                                       put code in DSDLCURVE.f and 
CH                                       DSDLCURVEINIT.f \
CH                                       Move MATch data array from dsclwt.f
CH                                       to here \
CH                                       Improve output diagnostic messages \

c This routine reads heao1 scanning data and creates scan files centered at
c chosen targets using selected scalar rates from the MED and HED3
c detectors.


      SUBROUTINE DSDLCE
      IMPLICIT NONE

      character(80) logfile , imessage 
      character(100) message, rawfile
      INTEGER LENACT , ierr , parse

      LOGICAL clobber,anti_coeff,daily_effic,det_select
      character(25) title
      REAL*8 ra50 , dc
      REAL BETa , VWDay,sc_wt(8),anti_wt,DAYs(1200)
      INTEGER I3fov, ichat , lchat , tchat,IGPtot,IRNtot
      INTEGER anti_mode, sc_mode, ICLwt,MATch(2,6,6)
      INTEGER  IDWt , IVWoff,rawunit,i,det_no
      character(40) TASkname
      character(7) VERSION
      PARAMETER (VERSION='1.1.0')
      character(8) ratnam(6)
      
      DATA ratnam/'TOTAL   ' , 'HARD    ' , 'SOFT    ' , 'R15     ' , 
     &     'WTD. R15' , 'MED M1  '/
      DATA MATch/1 , 1 , 2 , 4 , 2 , 4 , 2 , 4 , 5 , 6 , 5 , 6 , 1 , 
     &     2 , 1 , 2 , 3 , 4 , 3 , 4 , 5 , 5 , 6 , 6 , 1 , 2 , 1 , 2 , 
     &     3 , 4 , 3 , 4 , 5 , 5 , 6 , 6 , 1 , 1 , 2 , 5 , 2 , 5 , 2 , 
     &     5 , 2 , 5 , 6 , 6 , 1 , 1 , 2 , 4 , 2 , 4 , 2 , 4 , 5 , 5 , 
     &     6 , 6 , 1 , 5 , 1 , 5 , 1 , 5 , 1 , 5 , 1 , 5 , 6 , 6/


C Dummy variables for log file.
      DATA imessage , parse/' ' , 0/

c
c Initialize variable

      ierr = 0
      message = ' '
      do i=1,1200
           DAYs(i) = 0.0
      enddo
      i=0
      
     

      TASkname = 'DSDLCURVE '//VERSION

C obtain input parameters
      CALL DSDLCURVEINIT(rawfile,title,ra50,dc,
     &                         BETa,IDWt,IVWoff,VWDay,ICLwt,
     &                         daily_effic,ratnam,
     &                         sc_mode, sc_wt, det_select,
     &                         det_no,anti_coeff,anti_mode, 
     &                         anti_wt, tchat,lchat,
     &                         clobber,ierr)
      IF ( ierr.NE.0 ) THEN
         message = ' Failure in attempting to retrieve input parameters'
         CALL XAERROR(message,1)
         return
      ENDIF
   
C read the day values in the raw database
      ierr = 0
      CALL READDAY(rawunit,rawfile,I3fov,IGPtot,IRNtot,DAYs,ierr)
      IF ( ierr.NE.0 ) THEN
         message = ' Failure in reading database '
         CALL XAERROR(message,1)
         return
      ENDIF


C open the log file if necessary tchat>=lchat
C reset the internal chatness to lchat
 
      CALL XCHATY(tchat,lchat)
      ichat = lchat


C Give useful information to terminal and log file
      message = ' '
      CALL XWRITE(message,ichat)
      i = INDEX(TASkname,' ')
      message = TASkname(:LENACT(TASkname))//' start scan file '
      CALL XWRITE(message,ichat)
      logfile = '+'//TASkname(1:i-1)//'.log'
      IF ( lchat.GE.tchat ) CALL SETLOG(imessage,parse,logfile,' ')
      i = INDEX(title,' ')
      message = ' '
      CALL XWRITE(message,ichat)
      WRITE (message,'('' OBJECT NAME '',A25)') title(1:i-1)
      CALL XWRITE(message,ichat)
      WRITE (message,'('' SOURCE RA AND DEC (1950) '',2F8.3)') ra50 , dc
      CALL XWRITE(message,ichat)
      WRITE (message,'('' DATABASE '',A70)') rawfile
      CALL XWRITE(message,ichat)
      WRITE (message,
     &'( '' BETA, DESIRED SCAN, VWDAY '',F8.3,I4,F7.1)') 
     & BETa, IVWoff, VWDay
      CALL XWRITE(message,ichat)
      WRITE (message,
     &'( '' WEIGHT BY DAILY EFFICIENCY ? '',L1)') 
     & daily_effic
      CALL XWRITE(message,ichat)
      IF (ICLwt .EQ. 0) THEN
         WRITE (message,
     &'( '' DISCOVERY SCALAR WEIGHT OPTION : keyboard input values'')')
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'( '' DISCOVERY SCALAR MODE '',I2)') sc_mode
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'( '' SCALAR WEIGHTS'',8F8.3)') (sc_wt(i),i=1,8)
         CALL XWRITE(message,ichat)
      ELSE
         WRITE (message,
     &'( '' DISCOVERY SCALAR WEIGHT OPTION : '',a8, '' RATE'')')  
     & ratnam(ICLwt)
         CALL XWRITE(message,ichat)
      ENDIF
      WRITE (message,
     &'( '' CHANGE ANTI-COINCIDENCE COEFF ? '',L1)') 
     & anti_coeff
      CALL XWRITE(message,ichat)
      IF (anti_coeff) THEN
         WRITE (message,
     &'( '' ANTI-COINCIDENCE COEFF MODE '',I2)') anti_mode
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'( '' ANTI-COINCIDENCE COEFF WEIGHT '',F8.3)') anti_wt
         CALL XWRITE(message,ichat)
      ENDIF
      message = ' '
      CALL XWRITE(message,ichat)



C Perform the main task
      ierr = 0
      CALL DSDLCURVEMAKE(rawunit,I3fov,title,ra50,dc,BETa,
     &     IDWt,IVWoff,VWDay,MATch,ICLwt,ratnam,sc_mode,sc_wt,
     &     det_select,det_no,anti_coeff,anti_mode,anti_wt,IRNtot,
     &     IGPtot,DAYs,ichat,clobber,ierr)

      message=' '
      CALL XWRITE(message,ichat)

      IF ( ierr.EQ.0 ) THEN
          WRITE (message,
     & '( '' **** DSDLCURVE '',A7, '' **** finished '')')
     & version
         CALL XWRITE(message,ichat)
      ELSE
         WRITE (message,
     & '( '' **** DSDLCURVE '',A7, ''...ERROR '')')
     & version
         CALL XWRITE(message,ichat)


      ENDIF

      END
