CH  Lorraine Breedon (1.0.0 14 Mar 1999) Original working version


c This routine reads OSO8 B/C pha raw fits data and generates spectra 
c (+ rates) output for a selected x-ray source. 

      SUBROUTINE OSOPHA
      IMPLICIT NONE

      character(80) logfile , imessage 
      INTEGER nrawfiles
      INTEGER LENACT , ierr , parse
      LOGICAL clobber,fov_check,ph_restriction
      character(25) title
      INTEGER maxlist,ch_start,ch_end
      character(7) VERSION
      PARAMETER (VERSION='1.0.0', maxlist=10, nrawfiles=100)
      character(160) message, rawphaf(nrawfiles),cont_srcelist,
     &      outpha,rawlist
      REAL src_Dc50, src_Ra50, cont_Ra50(maxlist), 
     &      cont_Dc50(maxlist),phase_st,phase_sp,
     &      int_low,int_high
      integer beginday,  endday, imsecs, imsece
      INTEGER i,j,detector,nsrces,nraw
      INTEGER lchan_min,lchan_max,hchan_min,hchan_max
      INTEGER tchat,lchat,ichat,integ_time
      DOUBLE PRECISION src_equi,period,ephemeris
      REAL bg,bgl,bgh
      character(40) TASkname
                   
     

C Dummy variables for log file.
      DATA imessage , parse/' ' , 0/

c
c Initialize variable

      ierr = 0
      message = ' '
      outpha = ' '
      title = ' '
      cont_srcelist = ' '
      i=0
      j=0
      nsrces=0
      do i=1,maxlist
           cont_Ra50(i)= 0.0
           cont_Dc50(i)= 0.0
      enddo
      do i=1,nrawfiles
           rawphaf(i)= ' '
      enddo    
    
      ph_restriction = .FALSE.
      period = 0.d0
      ephemeris = 0.d0
      phase_st = 0.0
      phase_sp = 0.0
      fov_check = .FALSE.

      TASkname = 'OSOPHA '//VERSION

C Get input parameters
      CALL OSOPHAGPAR(src_equi,maxlist,src_Ra50,src_Dc50,title,
     &                detector,rawlist,rawphaf,nraw,ph_restriction,
     &                period,ephemeris,
     &                phase_st,phase_sp,fov_check,cont_srcelist,
     &                nsrces,cont_Ra50,cont_Dc50,
     &                ch_start,ch_end,integ_time,
     &                int_low,int_high,
     &                lchan_min,lchan_max,
     &                hchan_min,hchan_max,
     &                beginday,imsecs,endday,imsece,
     &                bg,bgl,bgh,
     &                tchat,lchat,clobber,ierr)
      IF ( ierr.NE.0 ) THEN
         message = ' Failure in attempting to retrieve input parameters'
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
      message = TASkname(:LENACT(TASkname))//' start PHA + rates..'
      CALL XWRITE(message,ichat)
      logfile = '+'//TASkname(1:i-1)//'.log'
      IF ( lchat.GE.tchat ) CALL SETLOG(imessage,parse,logfile,' ')
      i = INDEX(title,' ')
      message = ' '
      CALL XWRITE(message,ichat)
      WRITE (message,'('' OBJECT NAME '',A25)') title(1:i-1)
      CALL XWRITE(message,ichat)
      WRITE (message,'('' SOURCE RA and DEC (1950) '',2F8.3)') 
     & src_Ra50, src_Dc50
      CALL XWRITE(message,ichat)
      WRITE (message,'('' DETECTOR '',I2)') detector
      CALL XWRITE(message,ichat)
      WRITE (message,'('' RAW DATA FILE LIST '',A70)') rawlist
      CALL XWRITE(message,ichat)
      WRITE (message,'('' PHASE RESTRICTION ? '',L1)') ph_restriction
      CALL XWRITE(message,ichat)
      
      IF (ph_restriction) then
         WRITE (message,
     &'('' PERIOD for PHASE RESTRICTION '',D8.3)') period
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'('' EPHEMERIS for PHASE RESTRICTION '',D8.3)') ephemeris
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'('' PHASE START '',F5.3)') phase_st
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'('' PHASE STOP '',F5.3)') phase_sp
         CALL XWRITE(message,ichat)
       ENDIF
     
       WRITE (message,
     &'('' FOV check for CONTAMINATING  SOURCES ? '',L1)') fov_check
         CALL XWRITE(message,ichat)
      
       IF (fov_check) then
         WRITE (message,
     &'('' LISTFILE of CONTAMINATING SOURCES  '',A70)') cont_srcelist
         CALL XWRITE(message,ichat)
         WRITE (message,
     &'('' CONT. SRCE(S) (RA(1950) DEC(1950) dec. degrees) :'')')
        CALL XWRITE(message,ichat)
        message = ' '
        CALL XWRITE(message,ichat)
        do j=1,nsrces
             WRITE (message,
     &'(''     '',2F8.3)') cont_Ra50(j),cont_Dc50(j)
             CALL XWRITE(message,ichat)
        enddo
        message = ' '
        CALL XWRITE(message,ichat)
      ENDIF
      WRITE (message,
     &'('' START and END channels for LC creation '',2I4)')
     & ch_start,ch_end
      CALL XWRITE(message,ichat)
      WRITE (message,
     &'('' Integration time (no. raw data records to integrate) '',I5)')
     & integ_time
      CALL XWRITE(message,ichat)
      WRITE (message,
     &'('' INTENSITY CUT (cnts/sec) '',2F12.3)') int_low,int_high
      CALL XWRITE(message,ichat)
      WRITE (message,
     &'('' CHANNEL BOUNDARIES '',4(I2,X))') 
     & lchan_min,lchan_max,hchan_min,hchan_max
      CALL XWRITE(message,ichat)
      WRITE (message,
     &'('' Mean BROAD band BKGD level (c/s) '',F12.3)') 
     & bg
      CALL XWRITE(message,ichat)
      WRITE (message,
     &'('' Mean SOFT band BKGD level (c/s) '',F12.3)') 
     & bgl
      CALL XWRITE(message,ichat)
      WRITE (message,
     &'('' Mean HARD band BKGD level (c/s) '',F12.3)') 
     & bgh
      CALL XWRITE(message,ichat)
      message = ' '
      CALL XWRITE(message,ichat)
      message = ' OBSERVATION START and STOP TIMES : '
      CALL XWRITE(message,ichat)
      WRITE (message,
     &'('' START day (of 1975) and MILLISECS of that day :'',I4,I12)')
     & beginday,imsecs
      CALL XWRITE(message,ichat)
      WRITE (message,
     &'('' END day (of 1975) and MILLISECS of that day : '',I4,I12)')
     & endday,imsece
      CALL XWRITE(message,ichat)
      message = ' '
      CALL XWRITE(message,ichat)

      

C Perform the main task
      ierr = 0
      CALL OSOPHAMAKE_BC(src_Ra50,src_Dc50,title,outpha,
     &                detector,rawphaf,nraw,ph_restriction,period,
     &                ephemeris,phase_st,phase_sp,
     &                fov_check,nsrces,
     &                cont_Ra50,cont_Dc50,
     &                ch_start,ch_end,integ_time,
     &                int_low,int_high,
     &                lchan_min,lchan_max,
     &                hchan_min,hchan_max,
     &                beginday,imsecs,endday,imsece,
     &                bg,bgl,bgh,
     &                ichat,clobber,ierr)
     

      message=' '
      CALL XWRITE(message,ichat)

      IF ( ierr.EQ.0 ) THEN
          WRITE (message,
     & '( '' **** OSOPHA '',A7, '' **** finished '')')
     & version
         CALL XWRITE(message,ichat)
      ELSE
         WRITE (message,
     & '( '' **** OSOPHA '',A7, ''...ERROR '')')
     & version
         CALL XWRITE(message,ichat)
      ENDIF


 999  continue    
      END
