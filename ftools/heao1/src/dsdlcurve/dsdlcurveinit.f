**==dsdlcurveinit.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998

CH  Lorraine Breedon (1.0.0 24 Nov 1998) Remove nested IF block structure \
CH                                       Move XPI calls from dsdkst.f into
CH                                       here \
CH                                       Obtain raw database filename from 
CH                                       input list file rather than from 
CH                                       I3fov value \
CH                                       Input source coords as string and 
CH                                       and for any equinox \
CH                                         
      SUBROUTINE DSDLCURVEINIT(rawfile,Title,Ra50,Dc50,
     &                         BETa,IDWt,IVWoff,VWDay,ICLwt,
     &                         daily_effic,ratnam,
     &                         sc_mode, sc_wt, 
     &                         det_select,det_no,anti_coeff,anti_mode, 
     &                         anti_wt, Tchat,Lchat,
     &                         Clobber,Ierr)
c
c
c read parameters in the parameter file for program dsdlcurve

c rawfile     O  ch*100  raw data filename
c Title       O  ch*25   rootname of output scan light curve
c Ra50        O  r*8     Source RA (in hh mm ss.s or degrees)
c Dc50        O  r*8     Source dec (in dd mm ss.s)
c BETa        O  r*4     parameter related to the no of days of data
c                        to be utilised. Default is approx BETa*5.7 days
c IDWt        O  i       Integer flag for daily efficiency weighting 
c                        (0=no ; 1=yes)
c IVWoff      O  i       Value determines which 6 month scan to use
c                        (0=1st scan ; 1=2nd scan ; 2=3rd scan)
c VWDay       O  r*4     Day of 1977 beyond which a scan of the source
c                        was made. Fixed at day 230. 
c ICLwt       O  i       Weight options for discovery scalars
c daily_effic O  l       Logical flag for daily efficiency weighting
c ratnam      O  ch*8    Array for all the types of RATE data available
c sc_mode     O  i       Discovery scalar mode for which to apply
c                        color weights
c sc_wt       O  r*4     Color weights for a given mode
c anti_coeff  O  l       Logical flag for anti-coincidence coefficient wting
c anti_mode   O  i       Discovery scalar mode for which to apply
c                        anti-coincidence coefficient weighting
c anti_wt     O  r*4     anti-coincidence coefficient weighting
c Tchat       O  i       terminal chattiness
c Lchat       O  i       level of chattines to output log file
c Clobber     O  l       Flag to overwrite scan output if it already exists
c Ierr        O  i       Output error status flag

      IMPLICIT NONE

      
      CHARACTER*(*) Title
      LOGICAL Clobber,anti_coeff,daily_effic,det_select
      REAL*8 Dc50 , Ra50
      INTEGER Ierr, Tchat, Lchat, equi
      INTEGER ICLwt,sc_mode,anti_mode,ICLwt1
      REAL BETa , VWDay,sc_wt(8),anti_wt
      INTEGER  IDWt , IVWoff ,lenact,filelen,det_no
      CHARACTER*(*) rawfile
      character(160) infile,message
      character(40) decstr, rastr
      DOUBLE PRECISION equinox,radeg,decdeg,dtr
      REAL sra,sdec

      character(100) errm
      CHARACTER*(*) ratnam(6)

      DATA dtr/.0174532925D0/


      
      errm = ' '
      message=' '
      rawfile=' '
      Ierr=0
      IDWt=0
      det_no=0
      



C get coord stuff
      call uclgsd('equinox', equinox, Ierr)
      if (Ierr .ne. 0) then
         errm = 'Error reading EQUINOX'
         CALL XAERROR(errm,1)
         return
      endif 

      call uclgst('ra', rastr, Ierr)
      if (Ierr .ne. 0) then
         errm = 'Error reading RA'
         CALL XAERROR(errm,1)
         return
      endif 

      call uclgst('dec', decstr, Ierr)
      if (Ierr .ne. 0) then
         errm = 'Error reading DEC' 
         CALL XAERROR(errm,1)
         return
      endif 

C Convert Right Ascension and Declination inputs into B1950.0 coordinates
C in degrees.
 
      equi = INT(equinox)
      CALL PARSERA(rastr,equi,radeg,Ierr)
      IF ( Ierr.NE.0 ) THEN
         errm = '  parsera: Error traslating RA string'
         CALL XAERROR(errm,1)
         return
        ENDIF
      CALL PARSEDEC(decstr,equi,decdeg,Ierr)
      IF ( Ierr.NE.0 ) THEN
         errm = '  parsdec: Error traslate DEC string'
         CALL XAERROR(errm,1)
         return
      ENDIF
 
C Note the code is hardwired to assume input from parameters was in
C FK4.  However, the difference between FK4 and FK5 systems is well
C below the resolving power of the A2 instrument.
 
      sra = radeg*dtr
      sdec = decdeg*dtr
      IF ( equinox.NE.1950.0D0 ) CALL SLA_PRECES('FK4',equinox,1950.0D0,
     &     sra,sdec)
C  returned values from sla routine are sra, sdec (in radians)
C ..so now convert back to decimal degrees
     
      Dc50 = sdec/dtr
      Ra50 = sra/dtr

      
c input database filename (+path)
      CALL UCLGST('infile',infile,Ierr)
      IF (Ierr.NE.0) THEN
         errm = 'Unable to get INFILE parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF


C Determine the database file
      filelen=LENACT(infile)
      rawfile = infile(:filelen)


c input title
      CALL UCLGST('rootname',Title,Ierr)
      IF (Ierr.NE.0) THEN
         errm = 'Unable to get ROOTNAME parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF
   

c input beta

      CALL UCLGSR('beta',BETa,Ierr)
      IF (Ierr.NE.0) THEN
         errm = 'Unable to get BETA parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF

      IF ( BETa.LT..01 ) THEN
         BETa = 0.6
         WRITE (message,
     &'('' BETA < 0.01 therefore set default = 0.6 '')')
         CALL XWRITE(message,5)
      ENDIF



c input vwday
c110   CALL UCLGSR('vwday',VWDay,Ierr)
c      IF (Ierr.NE.0) THEN
c         errm = 'Unable to get VWDAY parameter'
c         CALL XAERROR(errm,1)
c         return
c      ENDIF

c      IF ( VWDay .LT. 235 .OR. VWDay .GT. 721) THEN
c           WRITE (message,
c     &'('' WARNING : Invalid VWDay selection ...please re-enter '')')
c         CALL XWRITE(message,5)
c         goto 110
c      ENDIF

       VWDay = 230.0

c weight by daily efficiency ?
      CALL UCLGSB('daily_effic',daily_effic,Ierr)
      IF (Ierr.NE.0) THEN
         errm = 'Unable to get DAILY_EFFIC parameter'
         CALL XAERROR(errm,1)
         return
      ELSE
         IF (daily_effic) IDWt = 1
      ENDIF

 
c input ivwoff
125   CALL UCLGSI('ivwoff',IVWoff,Ierr)
      IF (Ierr.NE.0) THEN
         errm = 'Unable to get IVWOFF parameter'
         CALL XAERROR(errm,1)
         return
      ELSEIF (IVWoff.LT. 0 .OR. IVWoff .GT. 2) THEN
         WRITE (message,
     &'('' WARNING: Invalid IVWoff selection ...please re-enter '')')
         CALL XWRITE(message,5)
        goto 125
      ENDIF
       WRITE (message,
     &'('' Options for setting up weights for discovery scalars : '')')
      CALL XWRITE(message,5)
      WRITE (message,
     &'(''  0 use standard values for TOTAL RATE  '')')
      CALL XWRITE(message,5)
      WRITE (message,
     &'(''  1 use standard values for HARD RATE '')')
      CALL XWRITE(message,5)
      WRITE (message,
     &'(''  2 use standard values for SOFT RATE '')')
      CALL XWRITE(message,5)
      WRITE (message,
     &'(''  3 use standard values for R15 RATE '')')
      CALL XWRITE(message,5)
      WRITE (message,
     &'(''  4 use standard values for WTD R15 RATE '')')
      CALL XWRITE(message,5)
      WRITE (message,
     &'(''  5 use standard values for MED M1 RATE '')')
      CALL XWRITE(message,5)
      WRITE (message,
     &'(''  6 to enter values from keyboard '')')
      CALL XWRITE(message,5)
      WRITE (message,
     &'('' IT IS STRONGLY ADVISED TO USE THE STANDARD VALUES '')')
      CALL XWRITE(message,5)





c Weights for discovery scalars 
      CALL UCLGSI('iclwt1',ICLwt1,Ierr)
      IF (Ierr.NE.0) THEN
          errm = 'Unable to get ICLWT1'
         CALL XAERROR(errm,1)
         return
      ENDIF

      IF (ICLwt1 .EQ. 6 ) THEN
         ICLwt = 0

C  enter discovery scalar weights from keyboard
         WRITE (message,
     &'('' The discovery scalers changed during the HEAO-1 mission.'')')
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' Each setting for the discovery scalar is called a mode.'')')
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' There were 6 scanning modes and different X-ray colors '')')
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' depending on the mode. The available colors are : '')')
         CALL XWRITE(message,5)
         message = ' '
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' MODE 1: HD3 M1,M2,-,-     MED M1,-,-,- '')')
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' MODE 2: HD3 M1,M2,-,-     MED M1,M2,-,- '')')
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' MODE 3: HD3 M1,M2,-,-     MED 1ACD,1B,2A,2B '')')
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' MODE 4: HD3 1AD,1B,1C,M2  MED 1ACD,1B,2A,2B '')')
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' MODE 5: HD3 1A,1B,1CD,M2  MED 1ACD,1B,2A,2B '')')
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' MODE 6: HD3 1A,1B,1CD,M2  MED 1A,1B,1CD,M2 '')')
         CALL XWRITE(message,5)
         message = ' '
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' There are 8 colors for each mode. '')')
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' You can weight the available colors as desired.'')')
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' You should choose the weights so that each mode '')')
         CALL XWRITE(message,5)
         WRITE (message,
     &'('' produces approximately the same summed rate. '')')
         CALL XWRITE(message,5)

c Enter mode 
150      CALL UCLGSI('sc_mode',sc_mode,Ierr)
         IF (Ierr.NE.0) THEN
            errm = 'Unable to get SC_MODE parameter'
            CALL XAERROR(errm,1)
            return
         ELSE  
            IF (sc_mode .LT. 1 .OR. sc_mode .GT. 6) THEN
                WRITE (message,
     &'('' WARNING : Invalid SC_MODE selection. Please re-enter...'')')
                CALL XWRITE(message,5)
                goto 150
            ELSE
C Enter weights
                CALL UCLGSR('sc_wt1',sc_wt(1),Ierr)
                IF (Ierr.NE.0) THEN
                   errm = 'Unable to get SC_WT1 parameter'
                   CALL XAERROR(errm,1)
                   return
                ENDIF
                CALL UCLGSR('sc_wt2',sc_wt(2),Ierr)
                IF (Ierr.NE.0) THEN
                   errm = 'Unable to get SC_WT2 parameter'
                   CALL XAERROR(errm,1)
                   return
                ENDIF
                CALL UCLGSR('sc_wt3',sc_wt(3),Ierr)
                IF (Ierr.NE.0) THEN
                   errm = 'Unable to get SC_WT3 parameter'
                   CALL XAERROR(errm,1)
                   return
                ENDIF
                CALL UCLGSR('sc_wt4',sc_wt(4),Ierr)
                IF (Ierr.NE.0) THEN
                   errm = 'Unable to get SC_WT4 parameter'
                   CALL XAERROR(errm,1)
                   return
                ENDIF
                CALL UCLGSR('sc_wt5',sc_wt(5),Ierr)
                IF (Ierr.NE.0) THEN
                   errm = 'Unable to get SC_WT5 parameter'
                   CALL XAERROR(errm,1)
                   return
                ENDIF
                CALL UCLGSR('sc_wt6',sc_wt(6),Ierr)
                IF (Ierr.NE.0) THEN
                   errm = 'Unable to get SC_WT6 parameter'
                   CALL XAERROR(errm,1)
                   return
                ENDIF
                CALL UCLGSR('sc_wt7',sc_wt(7),Ierr)
                IF (Ierr.NE.0) THEN
                   errm = 'Unable to get SC_WT7 parameter'
                   CALL XAERROR(errm,1)
                   return
                ENDIF
                CALL UCLGSR('sc_wt8',sc_wt(8),Ierr)
                IF (Ierr.NE.0) THEN
                   errm = 'Unable to get SC_WT8 parameter'
                   CALL XAERROR(errm,1)
                   return
                ENDIF
            ENDIF
         ENDIF
      ELSE
         ICLwt = ICLwt1 + 1
         WRITE (message,
     &'(''....  Using standard weights for '',A8,'' rate'')')
     & ratnam(ICLwt)
         CALL XWRITE(message,5)
      ENDIF

C Select MED or HD3 data only ? or defalt
      CALL UCLGSB('det_select',det_select,Ierr)
      IF (Ierr.NE.0) THEN
         errm = 'Unable to get DATA_SELECT parameter'
         CALL XAERROR(errm,1)
         return
      ELSE
         IF (.NOT. det_select) THEN
175          CALL UCLGSI('det_no',det_no,Ierr)
             IF (Ierr.NE.0) THEN
                errm = 'Unable to get DET_NO parameter'
                CALL XAERROR(errm,1)
                return
             ELSE
                IF (det_no .LT. 5 .OR. det_no .GT. 7) THEN
                   WRITE (message,
     &'('' WARNING : Invalid DET_NO selection. Please re-enter '')')
                   CALL XWRITE(message,5)
                   goto 175
                ENDIF
             ENDIF
          ENDIF
      ENDIF


     




C Change ANTI-COEFF ?
      CALL UCLGSB('anti_coeff',anti_coeff,Ierr)
      IF (Ierr.NE.0) THEN
         errm = 'Unable to get ANTI_COEFF parameter'
         CALL XAERROR(errm,1)
         return
      ELSE


         IF (anti_coeff) THEN
            IF (ICLwt1 .NE. 6 ) THEN
        
C  give info about modes
               WRITE (message,
     &'('' The discovery scalars changed during the HEAO-1 mission.'')')
               CALL XWRITE(message,5)
               WRITE (message,
     &'('' Each setting for the discovery scalar is called a mode.'')')
               CALL XWRITE(message,5)
               WRITE (message,
     &'('' There were 6 scanning modes throught the mission. '')')
               CALL XWRITE(message,5)
               WRITE (message,
     &'('' A change in mode represented a change in X-ray colors : '')')
               CALL XWRITE(message,5)
               message = ' '
               CALL XWRITE(message,5)
               WRITE (message,
     &'('' MODE 1: HD3 M1,M2,-,-     MED M1,-,-,- '')')
               CALL XWRITE(message,5)
               WRITE (message,
     &'('' MODE 2: HD3 M1,M2,-,-     MED M1,M2,-,- '')')
               CALL XWRITE(message,5)
               WRITE (message,
     &'('' MODE 3: HD3 M1,M2,-,-     MED 1ACD,1B,2A,2B '')')
               CALL XWRITE(message,5)
               WRITE (message,
     &'('' MODE 4: HD3 1AD,1B,1C,M2  MED 1ACD,1B,2A,2B '')')
               CALL XWRITE(message,5)
               WRITE (message,
     &'('' MODE 5: HD3 1A,1B,1CD,M2  MED 1ACD,1B,2A,2B '')')
               CALL XWRITE(message,5)
               WRITE (message,
     &'('' MODE 6: HD3 1A,1B,1CD,M2  MED 1A,1B,1CD,M2 '')')
               CALL XWRITE(message,5)
               message = ' '
               CALL XWRITE(message,5)
               WRITE (message,
     &'(''   The anti-coincidence coefficient is defined as the'')')
               CALL XWRITE(message,5)
               WRITE(message,
     &'(''   expected change in the count rate/s for a change in '')')
               CALL XWRITE(message,5)
               WRITE (message,
     &'(''   the anti-coincidence rate  of 1000/major frame. '')')
               CALL XWRITE(message,5)
               WRITE (message,
     &'(''   The coeff. may be different for different modes. '')')
               CALL XWRITE(message,5)
            ENDIF
   
C Enter mode
200         CALL UCLGSI('anti_mode',anti_mode,Ierr)
            IF (Ierr.NE.0) THEN
                errm = 'Unable to get ANTI_MODE parameter'
                CALL XAERROR(errm,1)
                return
            ELSEIF (anti_mode .LT. 1 .OR. anti_mode .GT. 6) THEN
                   WRITE (message,
     &'('' WARNING : Invalid ANTI_MODE selection. Please re-enter '')')
                   CALL XWRITE(message,5)
                   goto 200
            ENDIF
C check to see if scalar mode already selected
            IF (ICLwt1 .EQ. 6 ) THEN
                   IF (anti_mode .NE. sc_mode) THEN
                         WRITE (message,
     &'('' WARNING : Mode selection different to that selected  '')')
                         CALL XWRITE(message,5)
                         WRITE (message,
     &'('' previously for color weightings. Therefore resetting '')')
                         CALL XWRITE(message,5)
                         WRITE (message,
     &'('' to this previously selected mode = '',I3)') sc_mode
                         CALL XWRITE(message,5)
                         anti_mode=sc_mode
                   ENDIF
            ENDIF
           
C Enter weight
            CALL UCLGSR('anti_wt',anti_wt,Ierr)
            IF (Ierr.NE.0) THEN
                errm = 'Unable to get ANTI_WT parameter'
                CALL XAERROR(errm,1)
                return
            ENDIF
         ENDIF
      ENDIF

c terminal chat
      CALL UCLGSI('tchat',Tchat,Ierr)
      IF (Ierr.NE.0) THEN
         errm = 'Unable to get TCHAT parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF

c
c log chat
      CALL UCLGSI('lchat',Lchat,Ierr)
      IF (Ierr.NE.0) THEN
         errm = 'Unable to get LCHAT parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF

C clobber
      CALL UCLGSB('clobber',Clobber,Ierr)
      IF (Ierr.NE.0) THEN
         errm = 'Unable to get CLOBBER parameter'
         CALL XAERROR(errm,1)
         return
      ENDIF

  
      RETURN
      END
