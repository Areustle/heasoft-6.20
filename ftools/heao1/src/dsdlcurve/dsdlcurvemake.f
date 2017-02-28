**==dsdlcurvemake.spg  processed by SPAG 4.50J  at 16:32 on 17 Oct 1998

CH   Jesse Allen (2.0.0 19 Apr 1998) Original working version
CH  Lorraine Breedon (2.1.0 26 Nov 1998) Remove COMMON blocks..pass variables
CH                                       via subroutine arguements\
CH                                       Remove subroutine dsdkst.f. Instead
CH                                       put code in DSDLCURVE.f and 
CH                                       DSDLCURVEINIT.f \
CH                                       Replace dsdkst call with call to
CH                                       dsdlwt.f \
CH                                       Tidy up logical unit numbers \

c This routine creates scan files centered at
c chosen targets using selected scalar rates from the MED and HED3
c detectors.

      SUBROUTINE DSDLCURVEMAKE(rawunit,I3fov,Title,Ra50,Dc50,BETa,
     &     IDWt,IVWoff,VWDay,MATch,ICLwt,ratnam,sc_mode,sc_wt,
     &     det_select,det_no,anti_coeff,anti_mode,anti_wt,IRNtot,
     &     IGPtot,DAYs, ichat,clobber,Ierr)

c rawunit     I  i       logical unit no for raw datafile
c I3fov       I  i       Integer flag for instrument fov
c                        (0 for 1.5x3.0 deg fov ; 1 for 3.0x3.0 deg fov)
c Title       I  ch*25   rootname of output scan light curve
c Ra50        I  r*8     Source RA (decimal degrees) - 1950 equinox
c Dc50        I  r*8     Source dec (decimal degrees) - 1950 equinox
c BETa        I  r*4     parameter related to the no of days of data
c                        to be utilised. Default is approx BETa*5.7 days
c IDWt        I  i       Integer flag for daily efficiency weighting 
c                        (0=no ; 1=yes)
c IVWoff      I  i       Value determines which 6 month scan to use
c                        (0=1st scan ; 1=2nd scan ; 2=3rd scan)
c VWDay       I  r*4     Day of 1977 beyond which a scan of the source
c                        was made. Fixed at day 230. 
c ICLwt       I  i       Weight options for discovery scalars
c ratnam      I  ch*8    Array for all the types of RATE data available
c sc_mode     I  i       Discovery scalar mode for which to apply
c                        color weights
c sc_wt       I  r*4     Color weights for a given mode
c anti_coeff  I  l       Logical flag for anti-coincidence coefficient wting
c anti_mode   I  i       Discovery scalar mode for which to apply
c                        anti-coincidence coefficient weighting
c anti_wt     I  r*4     anti-coincidence coefficient weighting
c Irntot      I  i       Total no of data records in raw data file
c Igptot      I  i       Total no of rows in raw data file
C Days        I  r*4     Array containing the days of mission (of 1977)
c ichat       I  i       terminal/output log file chattiness
c Clobber     I  l       Flag to overwrite scan output if it already exists
c Ierr        O  i       Output error status flag


      IMPLICIT NONE

      INTEGER KEY ,  IDWt , ICLwt,det_no
      LOGICAL anti_coeff, clobber,det_select
      CHARACTER*(*) Title,ratnam(ICLwt)
      character(120) errm, message
      REAL*8 Ra50 , Dc50
      INTEGER ichat , Ierr, IVWoff,Fitsunit
      INTEGER I3fov, anti_mode, sc_mode,MATch(2,6,6)
      character(70) filename
      REAL BETa , VWDay,sc_wt(8),anti_wt
      
C
C*** Start of declarations inserted by SPAG
      REAL a1 , a13 , aday , alen , angl , anti , avday , aveff , bexp , 
     &     btim , COLwt(9,6), DAYs(1200) , dday , 
     &     deff , deltr , dt , dtim
 
      REAL dwt , dwt2 , eday , efexp , eff , eff1 , effprp , etim , 
     &     etim0 , ftim , prpang , prpday , prtlen , rtd , sday , 
     &     status , stim , stim0 , sumeff
 
      REAL sumexp , sxp , time , tmp , wt , xmax , xmin , ymax
 
      INTEGER i , iauto , ic1 , icnt , idn , idrec , ids , idx0 , idx1 , 
     &        iend , IGPtot 
 
      INTEGER irec , irn , IRNtot , isky , itmp , ITYp , iview , iwrt , 
     &        ixphi , ixplo , ixplo0 , j , j1  , jmod
 
      INTEGER jno , jrlen , jspin , mode , nbad ,
     &        nbin , ncnt , 
     &        nint ,  rawunit, mode_mismatch
C
C DSDISK.FOR
C    Version 1.0                     8/11/88       by Dr. F. Marshall
C
C   CALLS VIEW-MTRANS FOR RA/DEC TO DOY
C         DSDKST TO START AND GET WEIGHTS
C         DSCPRN(AND DSKLOK) COMPUTES NEEDED RECORD NO.
C         DSPLOT PLOTS ON SCREEN
C  	  DSDWT COMPUTES EFFICIENCY FOR PRESENT SCAN
C         DSEFF COMPUTES EFFECTIVE EXPOSURE
C	  SYSTMU TO WRITE DAY AND TIME
C
C DISK IS CREATED BY PROGRAM DSFET.
C  IT IS (ALMOST) A COPY OF DSSUM TAPE.
C    REC. LENGTH=2882 (NO APPENDED FN/RN)
C
C MOD-JUN-86 TO HAVE INT*4 KBUF TO PREVENT OVERFLOWS
C	             TO USE EFF1 IN OUTPUT FILE
C MOD-JUL-86 TO ALLOW FOR WEIGHTING BY DAILY EFFICIENCY
C 	        REQUIRED MAKING SUMS OF EXP, ANTI REAL*4
C MOD-JUL-86 TO ELIMINATE JBUF (USE UNKNOWN)
C MOD-AUG-86 TO WRITE OUT NUMBER OF EXPOSURES FOR EACH 12 HOURS
C MOD-AUG-86 TO USE NOMINAL SPIN-AXIS POSITION FOR ENTIRE 12 HOURS
C MOD-AUG-86 TO HAVE T-FWHM = 365.25/360. * THETA-FWHM
C MOD-AUG-86 TO CORRECT READING ONE EARLY RECORD IN DSCPRN
C MOD-AUG-86 TO ALLOW VARIABLE STARTING TIME FOR VIEW
C MOD-AUG-86 TO ALLOW DIFFERENT GROUPINGS FOR MOD AND MODE
C MOD-SEP-86 TO ALLOW RESTART AT END OF PROGRAM
C MOD-SEP-86 TO CHANGE DEFAULT WEIGHTS SO THAT R15=1 SOURCE
C    		PRODUCES 1 CPS
C MOD-JUL-87 TO USE MATCH TO DECIDE WHETHER TO ADD DATA FROM DIFFERENT
C	        MODES TOGETHER
C MOD-JUL-87 TO RUN OF MICRO-VAX CHANGED DAYS TO HAVE DIM OF 1200
C               CHANGED COLWT TO (9,6)
C MOD-SEP-87 TO INCLUDE SCAN ANGLE IN CALL TO DSPLOT
C MOD-FEB-88 TO USE EITHER 1.5 OR 3.0 DEG FOV DATA
C MOD-FEB-88 TO MAKE MORE STRUCTURED
C MOD-APR-88 TO LABEL WITH EITHER MED, HD3, OR COMBINED (DET.7)
C MOD-JUL-88 SO THAT WHEN IAUTO.NE.0 PROGRAM USES LOCAL MODE
C		FOR EACH SECTION OF TIME (MODE CAN CHANGE)
C MOD-AUG-88 TO CORRECT SMALL PROBLEM IN DSDWT (FOR FILES THAT
C		ONLY HAD SMALL AMOUNT OF TIME, DWT COULD BE
C		SLIGHTLY OFF)
C MOD-MAR-98 Reads FITS DSDISK raw data files and writes FITS scan files
C MOD-SEP-98 George Hilton, XPI interface
C
      REAL*4 dir(3,3) , aexp(100) , a2exp(100) , aanti(100) , rat(100) , 
     &       sig(100)
C
C AEXP IS WEIGHTED SUM OF EXPOSURES
C A2EXP IS SUM OF EXPOSURES WEIGHTED BY DWT**2
C AANTI IS WEIGHTED SUM OF ANTI RATE
 
      INTEGER*2 lbuf(1440) , jtyp
      REAL*8 spax(3,3)
C
C
      DATA rtd/57.2958/
C
c  Initialize to avoid warning
      dwt2 = 0.
      dday = 0.
      nint = 0
      ncnt = 0
c  --

C initialise
      do i=1,1440
           lbuf(i)=0
      enddo
      iend = 0
      nbin = 100
      mode_mismatch=0
      filename=' '


C AVERAGE EXPOSURE IS DETERMINED FROM ICNT= IXPLO TO IXPHI
      ixplo = 48
      ixphi = ixplo + 5
      ixplo0 = ixplo - 1
      jrlen = 2882
      CALL DSCLWT(ICLwt,ratnam,sc_mode,sc_wt,
     &            det_select,det_no,anti_coeff,anti_mode,
     &                  anti_wt,COLwt,Idn,ichat)
C
C PRPANG IS FWHM IN DAY DIRECTION
C USE AVERAGE OF HED3 AND MED
C
      prpang = 2.85
      prpday = BETa*prpang
C  ITYP IS TYPE OF RECORD 0-11
      ITYp = 11
      time = 232.
      iview = 0
      iauto = 0
      iwrt = 0
C
      CALL CREATESCAN(Fitsunit,filename,I3fov,Title,Ra50,Dc50,
     & clobber,status)
      IF ( status.NE.0 ) THEN
          WRITE (errm,
     &'('' Error creating scan file, status = '',I2)') status
          CALL XAERROR(errm,1)
         return
      ENDIF
C
C END MOD 5 Apr 1998
      prpday = BETa*prpang
C END MOD 06 Oct 1997
      isky = 0
      iview = iview + IVWoff
      IF ( iview.LT.0 .OR. iview.GT.2 ) GOTO 800
C END MOD 06 Oct 1997
C
  
C END MOD 06 Oct 1997
      IF ( Ra50.LT.0. ) THEN
         sday = -Ra50
         eday = Dc50
         aday = 0.5*(sday+eday)
         alen = eday - sday
      ELSE
         CALL VIEW(VWDay,iview+1,Ra50,Dc50,aday,angl)
         IF ( angl.LT.0. ) angl = angl + 360.
         a1 = 1./COS(angl/rtd)
         a1 = ABS(a1)
         a13 = prpang*a1
         alen = AMIN1(prpday*a1,10.)
         IF ( iauto.NE.0 ) alen = AMIN1(a13,15.)
         itmp = 2.*(aday-0.08-alen) + .5
C THE 0.08 IS TO COMPENSATE FOR FACT THAT TAPE HAS DATA FROM
C   E.G., .08 TO .58
         sday = itmp/2.
         itmp = 2.*(aday-0.08+alen) + .5
         eday = itmp/2. + .1
      ENDIF

      IF ( iauto.NE.0 ) THEN
C  SET INTERVAL SO THAT PRTLEN OF ALEN IS COVERED
         nint = iauto
         IF ( iauto.EQ.1 ) nint = 6
         IF ( iauto.EQ.2 ) nint = 12
         prtlen = 0.67 + nint/100.
         itmp = 4.*prtlen*alen/nint + 0.999
         dday = itmp/2.
         itmp = (aday+0.25)*2.
         sday = itmp/2. - dday*nint/2.
         eday = sday + dday + 0.1
         ncnt = 0
      ENDIF
      effprp = ABS(360.*COS(angl/rtd)/365.25/prpang)

C EFFPRP IS THE INVERSE OF THE FWHM IN DAYS PERP. TO SCAN DIRECTION
C IDX1 IS BIN NO. OF 1ST BIN TO BE ACCUMULATED
C 1ST BIN OVERALL RUNS FROM SA 0. TO 0.25
      idx1 = (angl-12.375)*4. + 1.
      IF ( idx1.LT.1 ) idx1 = idx1 + 1440
      IF ( idx1.GT.1440 ) idx1 = idx1 - 1440
      idx0 = idx1 - 1
 

      eday = AMIN1(eday,DAYs(IGPtot))
      sday = AMAX1(sday,DAYs(1))


      avday = 0.5*(sday+eday)
      IF ( eday.GT.sday ) THEN
         IF ( Ra50.GE.0. ) sday = AMAX1(sday,DAYs(1))
         dt = sday - time

      
C
C  COMPUTE RECORD FOR DESIRED DAY.
         CALL DSCPRN(sday,irn,IGPtot,DAYs)

    
         irn = irn - 1
C WILL INCREMENT IRN BEFORE EACH READ
         ITYp = 10
         GOTO 200
      ENDIF

100   IF (sday .GT. eday) THEN
         WRITE (errm,
     &'('' ERROR : start day for scan > max day value for data !!'')')
          CALL XAERROR(errm,1)
      ENDIF
      GOTO 700
 

      
C
C WHEN IAUTO.NE.0 WE LOOP BACK TO HERE
 200  avday = 0.5*(sday+eday)
    

      jspin = 0
C DETERMINE THE DESIRED MODE BY AVERAGE DAY
      mode = 1
      IF ( avday.GT.246.06 ) mode = 2
      IF ( avday.GT.248.06 ) mode = 3
      IF ( avday.GT.305.0 ) mode = 4
      IF ( avday.GT.321.8 ) mode = 5
      IF ( avday.GT.615.0 ) mode = 6
C
 
      WRITE (message,
     &'('' DAY RANGE when SRCE in FOV:'',2F8.2, '' SCAN ANGLE:'',F8.2)') 
     & sday , eday , angl
      CALL XWRITE(message,ichat)
       message=' '
      CALL XWRITE(message,ichat)

      WRITE (message,
     &'('' The discovery scalar modes changed throught the mission:'')')
      CALL XWRITE(message,ichat)
       WRITE (message,
     &'(''    Day GT 246.06 ; Mode = 2 '')')
      CALL XWRITE(message,ichat)
       WRITE (message,
     &'(''    Day GT 248.06 ; Mode = 3 '')')
      CALL XWRITE(message,ichat)
       WRITE (message,
     &'(''    Day GT 305.00 ; Mode = 4 '')')
      CALL XWRITE(message,ichat)
       WRITE (message,
     &'(''    Day GT 321.08 ; Mode = 5 '')')
      CALL XWRITE(message,ichat)
       WRITE (message,
     &'(''    Day GT 615.00 ; Mode = 6 '')')
      CALL XWRITE(message,ichat)
      WRITE (message,
     &'('' Hence Mode for day range when srce in FOV = '', I3)') 
     & mode
      CALL XWRITE(message,ichat)
       message=' '
      CALL XWRITE(message,ichat)



C
C END MOD 06 Oct 1997
      DO i = 1 , 100
         rat(i) = 0.
         sig(i) = 0.
         aexp(i) = 0.
         a2exp(i) = 0.
         aanti(i) = 0.
      ENDDO
      sumexp = 0.
      sumeff = 0.
      irec = 0
C IREC COUNTS (SORT OF) THE NUMBER OF DATA RECORDS USED
      nbad = 0
C
C WE RETURN HERE AFTER EACH 1/2 DAY OF DATA
 300  IF ( ITYp.EQ.0 ) GOTO 500
C


C Read raw data -
C determine start and end times of a given row using the irn 
C (record number) value. 
 400  irn = irn + 1
      IF ( irn.GT.IRNtot ) GOTO 600
      CALL READRAW(rawunit,irn,jtyp,stim,etim,dir,lbuf,status)
       ITYp = 0
 500  jno = 0
      mode_mismatch=0
      IF ( jtyp.EQ.0 ) THEN
         time = stim
         IF ( etim.EQ.0. ) etim = stim + 0.4
         IF ( .NOT.(stim.GT.200. .AND. stim.LT.1000. .AND. 
     &        etim.GT.200. .AND. etim.LT.1000. .AND. 
     &        (irec.LT.1 .OR. stim.GE.sday)) ) THEN
            nbad = nbad + 1
C
            WRITE (errm,
     &'(" Error reading raw data file at record number",I10,2F10.2)') 
     & irn , stim , etim
            CALL XAERROR(errm,1)
            Ierr=1
            return
 
C
         ENDIF
C NOW CHECK IF MODE FOR PRESENT DAY IS THE DESIRED MODE
         jmod = 1
         IF ( stim.GT.246. ) jmod = 2
         IF ( stim.GT.248. ) jmod = 3
         IF ( stim.GT.305. ) jmod = 4
         IF ( stim.GT.321.7 ) jmod = 5
         IF ( stim.GT.615. ) jmod = 6
         IF ( stim.LT.sday ) jno = 1
C
         IF ( stim.LT.sday ) THEN
            WRITE(errm,
     &'(" ERROR : problem reading raw data file - ")')
           CALL XAERROR(errm,1)
            WRITE(errm,
     &'(" Start time in data < start day of scan ")')
           CALL XAERROR(errm,1)
            WRITE(errm,
     &'("Stim, Etim, Record number",2F8.2,I6)')
     & stim , etim , irn
           CALL XAERROR(errm,1)  
           Ierr=1
           return 
         ENDIF
         IF ( ICLwt.NE.0 ) THEN
C ARE USING STANDARD RATE - demand exact same modes
            IF (.NOT. anti_coeff) THEN
               IF ( jmod.LT.MATch(1,mode,ICLwt) .OR. 
     &           jmod.GT.MATch(2,mode,ICLwt) ) THEN
                 jno = 1
                 mode_mismatch=1
               ENDIF
            ELSE
               IF ( jmod.LT.MATch(1,mode,ICLwt) .OR. 
     &           jmod.GT.MATch(2,mode,ICLwt) ) THEN
                 jno = 1
                 mode_mismatch=1
               ELSEIF ((mode_mismatch .EQ. 0) .AND. 
     &                 (mode .NE. anti_mode)) THEN
                    jno=1
                    mode_mismatch=2
               ENDIF
            ENDIF
         ELSE
C ARE USING USER INPUT RATE--DEMAND EXACT SAME MODES
            IF ( jmod.NE.mode ) THEN
               jno = 1
               mode_mismatch=3
 
            ELSEIF (mode.NE.sc_mode) THEN
               jno=1
               mode_mismatch=4
            ENDIF
         ENDIF
         IF ( etim.LE.eday ) THEN
            IF ( jno.EQ.0 ) THEN
               IF ( stim.LE.avday .OR. jspin.EQ.0 ) THEN
C   USE CURRENT SPIN AXIS UNTIL EXCEED AVERAGE DAY
                  jspin = 1
                  DO i = 1 , 3
                     DO j = 1 , 3
                        spax(i,j) = dir(i,j)
                     ENDDO
                  ENDDO
               ENDIF
               ftim = etim
               irec = irec + 1
               IF ( irec.EQ.1 ) btim = stim
C  ADD IN DATA
C  ADD EXPOSURE
               CALL DSDWT(aday,stim,etim,effprp,aveff)
               dt = etim - stim
               deff = aveff*dt
               eff = eff + deff
               efexp = efexp + dt
               dwt = 1.
               IF ( IDWt.NE.0 .AND. Ra50.GE.0. ) dwt = aveff
               dwt2 = dwt*dwt
C SAVE TIMES
               stim0 = stim
               etim0 = etim
            ENDIF
            bexp = 0.
            DO j = 1 , 11
C THERE ARE NOW 11 DATA RECORDS PER GROUP
               irn = irn + 1
               IF ( irn.GT.IRNtot ) GOTO 600
               CALL READRAW(rawunit,irn,jtyp,stim,etim,dir,lbuf,status)
               If (status .ne. 0) THEN
                  WRITE (errm,
     & '(" ERROR : Reading raw data file, status = ",I6)') status
                  CALL XAERROR(errm,1)
                   Ierr=1
                  return
               ENDIF
              
               IF ( jno.EQ.0 ) THEN
C
                  IF ( jtyp.NE.j ) THEN
 
                     WRITE (errm,
     & '(" ERROR : Read error in raw data file",2I6)') irn , jtyp
                     CALL XAERROR(errm,1)
                     Ierr=1
                     
 
C
C IF READ ERROR, DO NOT WRITE OUTPUT
                     iwrt = 1
                     return

C   ADD TO EXPOSURE
C SUM UP WEIGHTED COUNTS
                  ELSEIF ( j.LE.11 ) THEN
                     j1 = j - 1
                     wt = 1.
                     IF ( j.GT.1 .AND. j.LE.9 ) THEN
C ONLY DO WTS. FOR COLORS
                        wt = COLwt(j1,jmod)
                        wt = wt*dwt
                     ENDIF
                     IF ( wt.NE.0. ) THEN
C RE-START INDENTING
                        DO icnt = 1 , 100
                           ic1 = idx0 + icnt
                           IF ( ic1.GT.1440 ) ic1 = ic1 - 1440
                           IF ( j.EQ.11 ) THEN
   
C ADD UP ANTI'S
                               aanti(icnt) = aanti(icnt) + lbuf(ic1)*dwt
                           ELSEIF ( j.NE.1 .AND. (J.NE.10)) THEN
C ADD UP WEIGHTED COLORS
                              tmp = lbuf(ic1)*wt
                              rat(icnt) = rat(icnt) + tmp
                              sig(icnt) = sig(icnt) + tmp*wt

                           ELSEIF (J .EQ. 1) THEN
C ADD UP EXPOSURES
                                aexp(icnt) = aexp(icnt) + lbuf(ic1)*dwt
                                IF ( icnt.GE.ixplo .AND. icnt.LE.ixphi )
     &                             THEN
 
                                   bexp = bexp + lbuf(ic1)
                                   a2exp(icnt) = a2exp(icnt) + lbuf(ic1)
     &                              *dwt2
                                   IF ( icnt.EQ.ixphi ) THEN
                                    bexp = bexp/(ixphi-ixplo0)
 

                                    WRITE(message,
     &'("Times:",2F7.2," Mode:",I2," Daily_wt:",F7.3," N.exp:",F7.1)') 
     & stim0 , etim0 , jmod , dwt , bexp
                                    CALL XWRITE(message,ichat)
 
                                   ENDIF
C
                                   IF ( IDWt.EQ.0 ) THEN
C THESE ARE FOR IDWT=0
                                    sumexp = sumexp + lbuf(ic1)
                                    sumeff = sumeff + lbuf(ic1)*aveff
                                   ENDIF
                                ENDIF
                           ENDIF
                        ENDDO
C RESTART INDENTING
                     ENDIF
                  ENDIF
               ENDIF
            ENDDO
            ITYp = 11
            IF ( nbad.LT.13 ) GOTO 300
            
            GOTO 800
         ENDIF
      ELSE
C
         WRITE (errm,
     &'(" Problem reading raw data file - skipping to header",2I6)') 
     & irn , jtyp
         CALL XAERROR(errm,1)
         GOTO 400
      ENDIF

C Have read the raw datafile...now lets see if we have any data ...

 600  IF ( irec.LT.1 ) GOTO 100

C NOW COMPUTE EFFECTIVE EXPOSURE
      CALL DSEFF(IDWt,Ra50,ixplo,ixphi,aexp,a2exp,sumeff,sumexp,eff1)
C NOW MAKE SUMS INTO RATES
      dtim = ftim - btim
      ymax = 0.
      DO icnt = 1 , nbin
         sxp = aexp(icnt)*1.28
         IF ( sxp.GT.0.001 ) THEN
            rat(icnt) = rat(icnt)/sxp
C NOW CORRECT RATE FOR ANTI RATE
 
            anti = aanti(icnt)/sxp*100.
            deltr = (anti-11000.)*COLwt(9,mode)/1000.
            rat(icnt) = rat(icnt) - deltr
            sig(icnt) = SQRT(ABS(sig(icnt)))/sxp
         ENDIF
         ymax = AMAX1(ymax,rat(icnt)+sig(icnt))
      ENDDO
      xmin = (idx1-1)/4.
      xmax = xmin + nbin/4.

      IF ( iwrt.EQ.0 ) THEN
         idrec = idrec + 1
         KEY = 0
         IF ( iauto.NE.0 ) KEY = 1
         IF ( I3fov.EQ.0 ) THEN
            ids = 2
         ELSE
            ids = 1
         ENDIF
         status=0
         CALL WRITESCAN(Fitsunit,xmin,nbin,btim,ftim,rat,sig,angl,
     &        status)
         IF (status .NE. 0) THEN
            errm = 'Problem writing to scan file '
            CALL XAERROR(errm,1)
            Ierr=1
            return
         ENDIF

      ENDIF
 700  eff = 0.
      sxp = 0.
      IF ( iauto.NE.0 ) THEN
         ncnt = ncnt + 1
         IF ( ncnt.LE.(nint-1) ) THEN
            sday = sday + dday
            eday = eday + dday
            GOTO 200
         ENDIF
      ENDIF

      IF (mode_mismatch .EQ. 1) THEN
          WRITE (message,
     &'('' WARNING: Mode computed from stime of raw datafile ='',I3)')
     & jmod
          CALL XWRITE(message,ichat)
          WRITE (message,
     &'('' BUT Mode matched from days when srce in FOV and '')') 
          CALL XWRITE(message,ichat)
          WRITE (message,
     &'('' the selected standard RATE = '',I3)') 
     & MATch(1,mode,ICLwt)
          CALL XWRITE(message,ichat)
      ELSEIF (mode_mismatch .EQ. 2) THEN
          WRITE (message,
     &'('' WARNING : Mode computed from days when srce in FOV = '',I3)')
     & mode
          CALL XWRITE(message,ichat)
          WRITE (message,
     &'('' BUT selected discovery scalar Mode for ACC WT = '',I3)')
     &  anti_mode
          CALL XWRITE(message,ichat)

      ELSEIF (mode_mismatch .EQ. 3) THEN
          WRITE (message,
     &'('' WARNING: Mode computed from stime of raw datafile ='',I3)')
     & jmod
          CALL XWRITE(message,ichat)
          WRITE (message,
     &'('' BUT Mode computed from days when srce in FOV = '',I3)')
     & mode
          CALL XWRITE(message,ichat)

      ELSEIF (mode_mismatch .EQ. 4) THEN
           WRITE (message,
     &'('' WARNING: Mode computed from days when srce in FOV = '',I3)')
     & mode
           CALL XWRITE(message,ichat)
           WRITE (message,
     &'('' BUT selected discovery scalar Mode for COLOR WTS = '',I3)')
     &  sc_mode
           CALL XWRITE(message,ichat)

       ENDIF
       IF (mode_mismatch .NE. 0) THEN
            errm=' '
            CALL XAERROR(errm,1)
            WRITE (errm,
     &'('' ERROR: Mode mismatch !! '')')
           CALL XAERROR(errm,1)
            errm=' '
            CALL XAERROR(errm,1)
            WRITE (errm,
     &'('' .....deleting scan file : '',a40)') filename
           CALL XAERROR(errm,1)
            status = 0
            open(unit=Fitsunit, file=filename, status='old')
            close(Fitsunit, status='delete')
            Ierr=1
   
        ELSEIF (sday .GT. eday) THEN
            CALL XAERROR(errm,1)
            WRITE (errm,
     &'('' .....deleting scan file : '',a40)') filename
           CALL XAERROR(errm,1)
            status = 0
            open(unit=Fitsunit, file=filename, status='old')
            close(Fitsunit, status='delete')
            Ierr=1

                        
       ENDIF
 
 
c close raw fits file     
       
800   status=0
      CALL FTCLOS(rawunit,status)
      IF ( status.NE.0 ) THEN
         errm = ' Problem closing database file '
         CALL XAERROR(errm,1)
         return
      ENDIF
      CALL XFRLUN(rawunit,status)
      IF ( status.NE.0 ) THEN
         errm = ' Problem releasing unit number'
         CALL XAERROR(errm,1)
         return
      ENDIF


      RETURN
      END
