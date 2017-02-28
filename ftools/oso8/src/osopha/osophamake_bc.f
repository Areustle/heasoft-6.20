**==aa0001.spg  processed by SPAG 4.50J  at 16:38 on 15 Mar 1999

CH  Lorraine Breedon (1.0.0 14 Mar 1999) Original working version

c This routine creates spectra and lightcurves from OSO-8 B/C
c detector data for a selected source.

      SUBROUTINE OSOPHAMAKE_BC(src_Ra50,src_Dc50,title,filename,
     &                detector,rawphaf,nraw,ph_restriction,period,
     &                ephemeris,phase_st,phase_sp,
     &                fov_check,ncontsrces,
     &                cont_Ra50,cont_Dc50,
     &                ch_start,ch_end,integ_time,
     &                int_low,int_high,
     &                lchan_min,lchan_max,
     &                hchan_min,hchan_max,
     &                beginday,imsecs,endday,imsece,
     &                bg,bgl,bgh,
     &                ichat,clobber,status)


c PRIMARY VARIABLES :

c af                r*4      Nominal area of the detector.
c ang               r*4      Angle (degrees) between srce and satellite spin
c                               axis.
c angle_check       logicl   Logical indicating that angle between srce and 
c                            satellite spin axis has been determined.


c bb                r*4      Ratio of angle `ang' to nominal detector FOV.
c bbtest            r*4      Ratio of angle `test' to nominal detector FOV.
c bg                r*4      Mean broad band bkgd level (counts).
c bge               r*4      Error in bg.
c bgl               r*4      Mean soft band bkgd level (counts).
c bgh               r*4      Mean hard band bkgd level (counts).
c bbnrm             r*4      Detector area corrected for collimator response
c                            (for a given row = 40.96 sec integ).
c bnrm              r*4      Same as above.
c bnrmt, bknrmt     r*4      Total effective area for all valid data rows
c                            in output light-curve.
c 

c cont_ra50,svra    r*4      Contaminating source RA (degrees), 1950 epoch.
c cont_dec50,svdec  r*4      Contaminating source DEC (degrees), 1950 epoch.


c detector,jdet     i        Flag indicating detector in use (1=B ; 2=C).
c dtm               r*4      Dead-time correction factor (dimensionless)
c                            for a valid data row (value 0.0 - 1.0).
c dt                r*4      Overall dead-time factor for all the valid data
c                            rows (i.e. for the entire output light-curve).
c dstr              r        Error in broad band bsu'd count rate (c/s/det area)
c                            corrected for dt and coll. resp.


c ephas,ph_stop     r*4      User selected phase start value for phase
c                            restriction.
c esc               r*4      Error in count rate for a valid data row. 
c ephemeris,dref    dp       User selected ephemeris for phase restriction.

c fov_check         logicl   Logical for checking whether a contaminating
c                            source is in the detector FOV.


c ichs,ch_start     i        Lower channel threshold for ouput light-curve.
c iche,ch_end       i        Upper channel threshold for ouput light-curve.
c idays,beginday    i        User input start day (of 1975) of obsn.
c idaye,enday       i        User input end day (of 1975) of obsn.
c IDY,raw_idy       i        Day of 1975 for a given row in a raw data file.
c IMS,raw_ims       i        No. of milliseconds of IDY for a given row in
c                            a raw data file.
c idayss            i        Day of 1975 for the final valid row of a raw data 
c                            file.
c idss              i*4      Day of 1975 for the first valid row of a raw data 
c                            file.
c ieof              logicl   Logical indicating end-of-file for a raw data
c                            file.
c ifas              i        Flag for performing phase restriction (0=no;1=yes)
c imsecs,mss        i*4      User input number of milliseconds of obsn
c                            start day.
c imsece,mse        i*4      User input number of milliseconds of obsn
c                            end day.
c int_low,sct1      r*4      Low intensity threshold for PHA accumulation.
c int_high,sct2     r*4      High intensity threshold for PHA accumulation.



c jdst              i        Valid start day (of 1975) of obsn.
c jsst              i*4      No. milliseconds of valid start day.
c jded              i        Valid end day (of 1975) of obsn.
c jsed              i*4      No. milliseconds of valid end day.


c k                 i        Counter for the no. of contaminating sources.
c kchk,ncontsrces   i        No. contaminating srces which could possibly
c                            be in detctor FOV (max. value = 10).
c ksc               i*4      No. of valid raw data records (rows) in output
c                            light-curve.
c kst               i*4      Summed counts/channel (channels 1-63)
c                            over all valid data rows in output light-curve.
c                            


c lunpha            i        Logical unit number for output PHA file.
c lunrates          i        Logical unit number for output light-curve 
c                            (rates) file.
c
c msrc              i*4      Time (in msec) of centre of integration. 
c

c Nbins             i        Total number of channels in ouput PHA file.
c nf                i        Total number of input raw data files.
c nraw              i        Number of raw data files actually used.
c nt                i*4      Total number of counts in a given row in a raw
c                            data file over the entire channel band (1-63). 
c ns                i*4      Total number of counts in a given row in a raw data
c                            file over the broad band (ichs-iche).
c nsl               i*4      Total number of counts in a given row in a raw data
c                            file over the soft band (lchan_min-lchan_max).
c nsh               i*4      Total number of counts in a given row in a raw data
c                            file over the hard band (hchan_min-hchan_max).
c nst               i*4      Array of up to 63 elements for a given row in a raw
c                            data file. Each element containing counts/channel.
c nsc               i        Current number of valid data records (rows) in
c                            output light-curve.
c nc                i*4      No of raw data records (rows) being processed at a 
c                            given time.
c nint              i        Number of raw data records (rows) in a 40.96 sec
c                            integration.
c 
c
c pha               r*4      63-element array for each row of a raw data file. 
c                            Each element contains counts/channel.  
c ph_restriction    logicl   Logical flag for restricting phase of selected raw 
c                            data.  
c 
c
c qtm               logicl   Logical flag that time in a given row of raw
c                            data file is within the user input selected
c                            time window.
c qfint             logicl   ?
c qnorm             logicl   Logical flag that data is corrected for deadtime
c                            and collimator response.
c qfov              logicl   Logical flag to indicate that a contaminating srce
c                            is in FOV.
c 
c 
c rawopen           logicl   Logical flag indicating that a raw input data file
c                            is open.
c raw_unit          i        Logical unit number for current raw data file.
c raw_row           i        Current row being read in a raw data file.
c rawphaf           ch*160    Name of input raw data file.
c rates             logicl   Logical flag to indicate that output light-curve
c                            is completed.
c rates_row         i        Current data row being written to output 
c                            light-curve.         
c rates_out         ch*80    Name of output light-curve.
c ra,srce_Ra50      r*4      User input srce RA (1950 epoch).
c rdec,srce_Dc50    r*4      User input srce DEC (1950 epoch).
c
c
c SNRa,sunra        r        Sun RA in a given row of raw data file.
c SNDEc,sundec      r        Sun DEC in a given row of raw data file.
c SRA,spra          r        RA of satellite spin axis for given row in raw
c                            data file.
c SDEc,spdec        r        DEC of satellite spin axis for given row in raw
c                            data file.
c ss,sct            r        Bsu'd count rate (c/s/det area) for a valid row
c                            in raw data file (over broad band).
c sl,slt            r        Bsu'd counts for a valid row
c                            in raw data file (over soft band).
c sh,sht            r        Bsu'd counts for a valid row
c                            in raw data file (over hard band).
c sfas,phase_st     dp       User selected phase stop value for phase
c                            restriction.
c srat              r        Ratio of sht/slt.
c str               r        Broad band bsu'd count rate (c/s/det area) 
c                            corrected for dt and coll. resp. (over
c                            all valid data rows).
c stl               r        Soft band bsu'd count rate (c/s) corrected 
c                            for dt (over all valid data rows).
c sth               r        Hard band bsu'd count rate (c/s) corrected 
c                            for dt (over all valid data rows).
c strat             r        Ratio of sth/stl.
c
c
c tlp40             r        Integration time (B=40.96s ; C=40.96*0.317s).  

c BASIC ALGORITHM :

c A list of raw pha data files are input (each spanning an integral number of 
c days between the start and end of the mission = day 176 - ??? of 1975). The
c file list encompasses days in chronological order. Such days correspond to 
c when the source of interest is most likely to be in the detector FOV. The
c first raw data file is opened and each data row is read sequentially until
c the time value for a given row (in days of 1975 and millisecs of that day)
c is within the user input selected time window (logical variable QTM is then
c set to .TRUE.). If QTM still = .FALSE. when the end-of-file is reached, the
c file is closed, and the next raw data file in the list is opened and read. 
c Once QTM = .TRUE. for a given row in a file :
c
c The angle between the source coords and the spin axis pointing for that
c row is calculated. This angle is then used to determine the detector area
c corrected for the collimator response (bbnrm). 
c For bbnrm <= 5cm**2 - the data in the current row is ignored, instead
c                       the code continues onto the next row.
c For bbnrm > 5cm**2  - the pha data array (counts/channel) for that row is
c                       read, covering the broad band channel thresholds
c                       (ichs - iche). The total number of counts per
c                       40.96 sec integration in this channel range (corrected
c                       for dead-time and collimator response) + associated
c                       error is then written as an integral data point 
c                       (data row) to the output light-curve. 
c The above is conducted for all the remaining rows in all the remaining 
c raw data files until the time value for a row is greater than the user
c input selected obsn end time. All the valid data rows will thus
c have been written to the output light-curve. 
c In the process of producing the light-curve, the entire 1-63 channel pha
c data array for each valid data row is placed into memory, such that a 
c final summed pha array over all such rows can be calculated. This array,
c (after correcting for dead-time and the collimator response), is then the 
c output 1-63 channel PHA spectrum appropriate to the output light-curve. 


      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL*4 af , ang , bb , bbnrm , bbtest , bg , bge , bgh , bgl , 
     &     bknrmt , bnrm , bnrmt , detang , dstr , dt , dtm , esc , 
     &     esct , phnorm , ra
      REAL*4 rdec ,  sc , sce , sct , sct1 , sct2 , 
     &     SDEc , sh , sht , sl , slt , SNDec , SNRa , snt , spdec , 
     &     spra , SRA
      REAL*4 srat , ss , st , sth , stl , str , strat , sundec , 
     &     sunra , svdec , svra , test , tim  , tlp40 , 
     &     tstnrm 
C      INTEGER*2 h  
      INTEGER*4 i ,  iche , ichs , idaye , idays , idayss , ids , 
     &        jded , jdet , jdst , jfov , k , kchk  
      INTEGER*4 lunpha, nhr1 , nhr2 , nhr3 , nhr4 , nint ,  nsc
      LOGICAL qfint ,
     &          qfov , qnorm ,  qtm 
C*** End of declarations inserted by SPAG
      DOUBLE PRECISION tsec
      INTEGER*4 mss , mse , msrc , IDY , IMS ,  ntst , ntm , 
     &          msmf , idss
      INTEGER*4 kst , nst , nc , ksc , jsst , jsed , ns , nt , nsl , nsh
      DIMENSION nst(63) ,  sc(63) , sce(63) , 
     &          kst(63) 
      DIMENSION af(2) , detang(2) , svra(10) , svdec(10)
CC	ALTERED TO ADD C DET 8/12/81 JHS, TO CHECK SOURCES IN VIEW 2/86
CC

C variables introduced in major re-write (L.B)

      character*(*) filename, title,rawphaf(*)
      LOGICAL clobber,fov_check, ieof,rates,rawopen,angle_check,
     &         ph_restriction,phase_ok
      REAL*4 src_Dc50, src_Ra50, cont_Ra50(*), cont_Dc50(*),pha(63),
     &           int_low,int_high,phase_st,phase_sp,sfas,efas,fas
      INTEGER*4 lchan_min,lchan_max,hchan_min,
     &           hchan_max,integ_time
      INTEGER*4 ichat,ch_start,ch_end,nraw,nf,raw_row,ifas,ip
      INTEGER*4 beginday,  endday, imsecs, imsece,rates_row
      INTEGER*4 detector,ncontsrces,status,row,Nbins,lunrates
      INTEGER*4 rawunit , blocksize , hdutype,totrows
      character(80) comment
      character(160) message,errm,rates_out
      character(255) string
      integer*4 raw_isect,raw_idy,raw_ims
      DOUBLE PRECISION period,ephemeris,dper , dref , dtim
      real*4 offset,areatot,area
      
      real*8 rbnrm,rbb
      
      DATA af/23.363 , 
     &     149.649/ , detang/3.34 , 5.09/ , msmf/20480/ 

c  Initialize to avoid warning
      sh = 0.
      sl = 0.
      jfov = 0
      kchk = 0
      mss = 0
      mse = 0
      sfas = 0.
      efas = 0.
      dper = 0.d0
      dref = 0.d0
c  --

c initialise variables
      rbnrm=0.0d0
      rbb=0.0d0


      angle_check=.FALSE.
      rawopen=.FALSE.
      ieof = .FALSE.
      rates = .FALSE.
      phase_ok = .FALSE.      
      qfint = .TRUE.
      qnorm = .TRUE.      
      
      lunpha = 0
      lunrates = 0
      rawunit = 0
      row = 0
      rates_row = 0
      nf = 0
      ntst = 0
      nc = 0
      
C New initializations
      sct=0.0
      ss=0.0
      esc=0.0
      esct=0.0
      dtm=0.0
      bnrm=0.0
      bnrmt=0.0
      bge=0.0
      tlp40=0.0
      snt=0.0
      str = 0.0
      stl = 0.0
      sth = 0.0
      slt=0.0
      sht=0.0
      areatot=0.0
      bknrmt = 0.0
      area = 0.0

      idayss=0
      idays=0
      ids=0
      idss=0
      ksc=0
      ns = 0
      nt = 0
      nsl = 0
      nsh = 0
      raw_row=0
      msrc=0
      ntm=0
      nsc=0

      do i=1,63
         nst(i)=0
         kst(i)=0
      enddo

      IF (detector .eq. 1) then
         jdet=1
      ELSE
         jdet=2
      ENDIF

      IF ( jdet.EQ.1 ) tlp40 = 40.96
      IF ( jdet.EQ.2 ) tlp40 = 40.96*.317

      ra = src_Ra50
      rdec =  src_Dc50
      IF ( ra.EQ.0.0 .AND. rdec.EQ.0.0 ) qnorm = .FALSE.

C Phase restriction ?
      IF (ph_restriction) THEN
         ifas = 1
         dper = period
         dref = ephemeris
         sfas = phase_st
         efas = phase_sp
      ELSE
         ifas = 0
      ENDIF


C FOV check ?
      IF (fov_check) THEN
         jfov = 1
         kchk = ncontsrces
         do k=1,kchk
              svra(k) = cont_Ra50(k)
              svdec(k) =  cont_Dc50(k)
         enddo
      ENDIF

c set up channels for LC integration
      ichs = ch_start
      iche = ch_end
      nint = integ_time

     

C set up rate range for PHA accumulation
      sct1 = int_low
      sct2 = int_high

C set up channel boundaries 
      nhr1 = lchan_min
      nhr2 = lchan_max
      nhr3 = hchan_min
      nhr4 = hchan_max

C set background level (to CNTS in a tlp40 interval) ..check bge ???
      If (bg .ne. 0.0) then
         bge = SQRT(bg * tlp40) 
      else
         bge = 0.0
      endif
      bg = bg * tlp40
      bgl = bgl * tlp40
      bgh = bgh * tlp40


C Create header for output rates file here..
      CALL OSOCR_RATES_ABC(lunrates,rates_out,detector,title,ra,rdec,
     &                    clobber,status)
      IF ( status.NE.0 ) THEN
         WRITE (errm,
     & '( '' Problem creating rates file : '',a80)')
     &  rates_out
         call xaerror(errm,1)
         return
      ENDIF
      
C >>>>>>>>>>>>>>>>>>>>>>>>>>> the following is all the spectral processing

 300  ksc = 0
      bknrmt = 0.0
      area = 0.0
C BKNRMT WILL BE EFF AREA OF TOTAL HISTOGRAM DATA
      DO 400 i = 1 , 63
         kst(i) = 0
 400  CONTINUE
C KST WILL BE ACCUMULATED HISTOGRAM
C QTM SAYS WHETHER IN PROPER TIME INTERVAL

 500  qtm = .FALSE.
       
      IF (.NOT. rates) then
          idays = beginday
          mss = imsecs
          idaye = endday
          mse = imsece
      ELSE
          idays = 0
          idaye = 0
      ENDIF
     
      IF ( qfint ) idayss = idays
      qfint = .FALSE.
      IF ( idays.EQ.0 .AND. idaye.EQ.0 ) THEN
         IF ( ksc.NE.0 ) THEN
C BNRM IN NOW AVERAGE NORM
c            tim = ksc*tlp40
            tim = FLOAT(ksc)*tlp40
            snt = 0.0
c            bnrm = bknrmt/ksc
            bnrm = bknrmt/FLOAT(ksc)
            phnorm = tim*bnrm
           
C create header for output spectral file
               
               call OSOCR_SPEC(lunpha,filename,detector,title,ra,
     &                rdec,bg,bnrm,tim,phnorm,clobber,status)
                IF ( status.NE.0 ) THEN
                   WRITE (errm,
     & '( '' Error creating : '', a80)')
     &  filename
                   call xaerror(errm,1)
                   return
                ENDIF
            
C TIM IS TOTAL LIVE TIME OF HISTOGRAM
            DO 520 i = 1 , 63
               st = FLOAT(kst(i))
               sc(i) = st/tim
               IF ( st.GE.0.0 ) sce(i) = SQRT(st)/tim
C SC IS OBSERVED CTS/SEC
               snt = snt + sc(i)
 520        CONTINUE
            str = 0.0
            stl = 0.0
            sth = 0.0
            dt = 1.0 - snt*0.000550
            DO 540 i = ichs , iche
               str = str + sc(i)
 540        CONTINUE
C            dstr = SQRT(str*tim+bg*ksc)/(tim*bnrm)
            dstr = SQRT(str*tim+bg*FLOAT(ksc))/(tim*bnrm)
            str = (str-bg*dt/tlp40)/bnrm
            DO 560 i = nhr1 , nhr2
               stl = stl + sc(i)
 560        CONTINUE
            DO 580 i = nhr3 , nhr4
               sth = sth + sc(i)
 580        CONTINUE
            stl = stl - bgl*dt/tlp40
            sth = sth - bgh*dt/tlp40
            strat = sth/stl
            Nbins = 63

C give info to screen about PHA data

            message=' '
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' MEAN RATE (+error) for PHA data (c/s/detarea) :'',2F10.3)') 
     & str, dstr
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' HARDNESS RATIO for PHA  data :'',F8.3)') 
     & strat
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' PHA ACCUMULATION TIME (secs) :'',F10.3)') 
     & tim
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' NET AREA of detector (sq. cm) :'',F6.2)') 
     & bnrm
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' INTEGRATED RATE for PHA data (c/s) :'',F10.4)') 
     & snt
            CALL XWRITE(message,ichat)
            message=' '
            CALL XWRITE(message,ichat)

C write output to spectral FITS file
            call OSOWR_SPEC(lunpha,Nbins,dt,jdst,jsst,jded,
     &                     jsed,sc,sce,
     &                     status)
            IF ( status.NE.0 ) THEN
               WRITE (errm,
     & '( '' Problem writing to spectral file : '',a80)')
     &  filename
               call xaerror(errm,1)
               return
            ENDIF
         ENDIF
         goto 2000
      ENDIF
      
C >>>>>>>>>>>>>>>>>>>>>>>>>>> end of spectral processing 
      
 585  nf = nf + 1
      rawunit=0
      raw_row=0
      ieof=.FALSE.
      
C open the raw PHA file ; move to 1st extension and obtain NAXIS2
      CALL XGTLUN(rawunit,status)
      IF (Status .NE.0 ) THEN
         errm = ' Problem obtaining free unit number'
         CALL XAERROR(errm,1)
         return
      ENDIF

      CALL FTOPEN(rawunit,rawphaf(nf),0,blocksize,Status)
      IF ( status.NE.0 ) THEN
          WRITE (errm,
     &'('' Unable to open raw fits PHA file : '',a80)')
     & rawphaf(nf)
          CALL XAERROR(errm,1)
          return
       ENDIF
 
       CALL FTMRHD(rawunit,1,hdutype,status)
       IF ( status.NE.0 ) THEN
           WRITE (errm,
     &'('' Unable to move to 1st extension : '',a80)')
     & rawphaf(nf)
           CALL XAERROR(errm,1)
           return
       ENDIF
       CALL FTGKYJ(rawunit,'NAXIS2',totrows,comment,status)
       IF ( status.NE.0 ) THEN
          WRITE (errm,
     &'('' Unable to obtain NAXIS2 value : '',a80)')
     & rawphaf(nf)
          CALL XAERROR(errm,1)
          return
       ENDIF
       rawopen=.TRUE.

 590   raw_row=raw_row+1
     
 600   call OSO_RDRAW(rawunit,qtm,detector,raw_row,totrows,raw_isect,
     &                raw_idy,raw_ims,
     &                SRA,SDEc,SNRa,SNDec,pha,ieof,status)
       IF ( status.NE.0 ) THEN
          WRITE (errm,
     & '( '' Problem reading raw datafile : '',a80)')
     &  rawphaf(nf)
          call xaerror(errm,1)
          WRITE (errm,
     & '( '' at row, I7 '')')
     &  row
          call xaerror(errm,1)
          return
        ELSE
          IF (ieof) then
C reached end of raw data file
             CALL FTCLOS(rawunit,status)
             IF ( status.NE.0 ) THEN
                WRITE (errm,
     &'('' Unable to close raw fits PHA file : '',a80)')
     & rawphaf(nf)
                CALL XAERROR(errm,1)
                return
             ENDIF
   
             CALL XFRLUN(rawunit,status)
             IF ( status.NE.0 ) THEN
                WRITE (errm,
     &'('' Problem releasing unit no. for : '',a80)')
     & rawphaf(nf)
                CALL XAERROR(errm,1)
                return
             ENDIF
             rawopen=.FALSE.
             if (nf .lt. nraw) goto 585
          ENDIF
        ENDIF

        IDY = raw_idy
        IMS = raw_ims

        IF (.NOT. qtm .AND. .NOT. ieof) goto 800
        IF (.NOT. qtm .AND. ieof) then
           WRITE (errm,
     &'('' ERROR: Selected time window outside raw data time frame'')')
           CALL XAERROR(errm,1)
           WRITE (errm,
     &'('' Please check day numbers of raw data !!'')')
           CALL XAERROR(errm,1)
           WRITE (errm,
     & '( '' ...deleting rates file: '',a80)')
     &  rates_out
           call xaerror(errm,1)
           status=0
           open(unit=lunrates, file=rates_out, status='old')
           close(lunrates, status='delete')
           goto 2000
        ENDIF

 700   spra = SRA
       spdec = SDEc
       sunra = SNRa
       sundec = SNDec
             
      IF ( qtm ) THEN
         IF ( IDY.GT.idaye ) qtm = .FALSE.
         IF ( IDY.EQ.idaye .AND. IMS.GT.mse ) qtm = .FALSE.
         IF ( .NOT.qtm ) GOTO 1600
         IF ( ifas.EQ.1 ) THEN
c             dtim = DFLOT(IDY) + DFLOT(IMS)/8.64D7
            dtim = DBLE(IDY) + DBLE(IMS)/8.64D7
C convert dtim to MJD
             dtim = dtim + 42412.0D0
c              type *, dtim
             dtim = (dtim-dref)/dper
             ip = INT(dtim)
             fas = SNGL(dtim) - FLOAT(ip)
c             type *, ip,dtim,fas
             IF ( (sfas.LT.efas) .AND. (fas.LT.sfas .OR. fas.GT.efas) )
     &        GOTO 590
             IF ( (sfas.LE.efas) .OR. (fas.GE.sfas) 
     &                      .OR. (fas.LE.efas) ) then
                 phase_ok = .TRUE.
                 GOTO 710
             ENDIF
             GOTO 590
         ENDIF

 710     IF ( jfov.NE.0 ) THEN
            qfov = .FALSE.
            DO 720 k = 1 , kchk
               CALL ANGLE(svra(k),svdec(k),spra,spdec,test)
               IF ( test.LE.detang(jdet) ) THEN
                  bbtest = test/detang(jdet)
                  tstnrm = af(jdet)
     &                 *(ACOS(bbtest)-(bbtest
     &                 *SQRT(1.0-bbtest*bbtest)))
                  message= ' '
                  CALL XWRITE(message,ichat)
                  WRITE (message,
     &'('' WARNING : Contaminating srce in FOV (RA DEC) : '',2F8.3)') 
     & svra(k),svdec(k)
                  CALL XWRITE(message,ichat)
                  WRITE (message,
     &'('' .. Angle between srce and spin axis (degrees) : '',F8.3)') 
     & test
                  CALL XWRITE(message,ichat)
                  WRITE (message,
     &'('' .. EFFECTIVE AREA  (cm**2) : '',F8.3)') 
     & tstnrm
                  CALL XWRITE(message,ichat)
                  qfov = .TRUE.
               ENDIF
 720        CONTINUE
            IF ( qfov ) then
               WRITE (message,
     &'('' .. for DAY (of 1975) and MILLISECS of that day :'',I4,I12)')
     & IDY,IMS
               CALL XWRITE(message,ichat)
            ENDIF
         ENDIF
CC
         IF ( .NOT.qnorm ) THEN
            bnrm = 1.0
            GOTO 900
         ELSE
            CALL ANGLE(ra,rdec,spra,spdec,ang)
            angle_check=.TRUE.
            IF ( ang.GT.detang(jdet) ) ang = detang(jdet)
            bb = ang/detang(jdet)
            rbb=DBLE(bb)
            bnrm = af(jdet)*(ACOS(bb)-(bb*SQRT(1.0-bb*bb)))
            rbnrm = DBLE(af(jdet))*(DACOS(rbb)-(rbb*
     &           DSQRT(1.0d0-rbb*rbb)))
c            bnrm=SNGL(rbnrm)
            write(string,13) ang,bb,rbb,DACOS(rbb),DSQRT(1.0d0-rbb*rbb),bnrm,
     &           rbnrm
 13         format("ang,bb,rbb,DACOS(rbb),DSQRT(1.0d0-rbb*rbb),bnrm,
     &           rbnrm ", 7(F18.12,1X))
            CALL XWRITE(string,55)
            bnrm=SNGL(rbnrm)


C AF IS AREA*2/PI FOR B OR C
C ONLY KEEP IF AT LEAST 5 CM**2
            
            IF ( bnrm.LE. 5.0 ) GOTO 590
            GOTO 900
         ENDIF
      ENDIF
      
 800  IF ( IDY.LT.idays ) GOTO 590
      IF ( IDY.EQ.idays .AND. IMS.LT.mss ) GOTO 590
      qtm = .TRUE.
      GOTO 700

 900  msrc = IMS - msmf*jdet
c      type *, raw_row,spra,spdec

C TIME IN MSEC OF CENTER OF ACCUMULATION
C B DET ASSUMED TO END AT TIME,C TO END 20.48S BEFORE. NEEDS CHECK

      ids = IDY
      IF ( msrc.LT.0 ) THEN
         msrc = msrc + 86400000
         ids = ids - 1
      ENDIF
      ns = 0
      nt = 0
      nsl = 0
      nsh = 0
      DO 1000 i = 1 , 63
c         nt = nt + pha(i)
         nt = nt + INT(pha(i))
 1000 CONTINUE
c      dtm = 1. - nt*.000550/tlp40
      dtm = 1.0 - FLOAT(nt)*0.000550/tlp40
C DEAD TIME PER CT .000550 SEC .WANT DEAD TIME FOR CTS IN TLP40
      IF ( qnorm ) then
         area = bnrm
         bnrm = bnrm*dtm
      ENDIF 
      DO 1100 i = ichs , iche
c         ns = ns + pha(i)
         ns = ns + INT(pha(i))
 1100 CONTINUE
      DO 1200 i = nhr1 , nhr2
c         nsl = nsl + pha(i)
         nsl = nsl + INT(pha(i))
 1200 CONTINUE
      DO 1300 i = nhr3 , nhr4
c         nsh = nsh + pha(i)
         nsh = nsh + INT(pha(i))
 1300 CONTINUE
c      ss = (ns-bg*dtm)/(bnrm*tlp40)
      ss = (FLOAT(ns)-bg*dtm)/(bnrm*tlp40)
c      esc = SQRT(ns+bge)/(bnrm*tlp40)
      esc = SQRT(FLOAT(ns)+bge)/(bnrm*tlp40)
      write(string,11) ns,bg,dtm,bnrm,tlp40,bge
 11   format("ns,bg,dtm,bnrm,tlp40,bge ",I10,5(F18.12,1X))
      CALL XWRITE(string,55)
      sl = FLOAT(nsl) - bgl*dtm
      sh = FLOAT(nsh) - bgh*dtm
      IF ( nc.EQ.0 ) GOTO 1700
      ntm = msrc - ntst

C NTM IS DIFFERENCE IN AV TIME OF RECORD AND FIRST OF INTEGRATION
      IF ( ntm.LT.0 ) ntm = ntm + 86400000
      ntm = (ntm+1000)/40960
      IF ( ntm.GT.nc+1 ) GOTO 1600

C QUIT INTEGRATION IF A FRAME MISSED
      IF ( ntm.GE.nint ) GOTO 1600
 1400 sct = sct + ss/(esc*esc)
      write(string,12) sct,ss,esc
 12   format("sct,sc,esc ",3(F18.12,1X))
      CALL XWRITE(string,55)
      DO 1500 i = 1 , 63
         nst(i) = nst(i) + INT(pha(i))
 1500 CONTINUE
      nsc = nsc + 1
      bnrmt = bnrmt + bnrm
      
C TOTAL NORM FOR RATE ACCUM
      esct = esct + 1.0/(esc*esc)
      slt = slt + sl
      sht = sht + sh
      bbnrm = bnrm

      nc = nc + 1
      GOTO 590

 1600 IF (esct .eq. 0.0 ) then
          IF (.NOT. phase_ok ) then
             WRITE (errm,
     &'('' Calculated phase is OUTSIDE input phase restriction '')')
            CALL XAERROR(errm,1)
          ELSE
           IF (angle_check) then
             WRITE (errm,
     &'('' Source NOT in FOV during requested time window '')')
             CALL XAERROR(errm,1)
           ELSE
             WRITE (errm,
     &'('' ERROR: Selected time window outside raw data time frame'')')
             CALL XAERROR(errm,1)
             WRITE (errm,
     &'('' Please check day numbers of raw data !!'')')
             CALL XAERROR(errm,1)
           ENDIF
          ENDIF
          WRITE (errm,
     & '( '' ...deleting rates file: '',a80)')
     &  rates_out
          call xaerror(errm,1)
          status=0
          open(unit=lunrates, file=rates_out, status='old')
          close(lunrates, status='delete')
          goto 2000
       ENDIF
           
      sct = sct/esct

      IF ( sct.GE.sct1 .AND. sct.LE.sct2 ) THEN
         IF ( bknrmt.LE.0.0 ) THEN
            jdst = ids
            jsst = msrc
         ENDIF
         jded = ids
         jsed = msrc
         
         DO 1650 i = 1 , 63
            kst(i) = kst(i) + nst(i)
 1650    CONTINUE
         ksc = ksc + nsc
         bknrmt = bknrmt + bnrmt
         areatot = areatot + area
      ENDIF
      
      esct = 1.0/SQRT(esct)
      IF ( slt.EQ.0.0 ) slt = 9999.
      srat = sht/slt
      ntm = ntst + (nc-1)*msmf

C NOW NTM IS MSEC OF CENTER OF INTEGRATION
      IF ( ntm.GE.86400000 ) THEN
         idss = idss + 1
         ntm = ntm - 86400000
      ENDIF
c         offset = jdst * 86400.
         offset = FLOAT(jdst) * 86400.0
c         tsec = (idss-idayss)*86400. + ntm/1000. + offset
         tsec = (DBLE(idss)-DBLE(idayss))*86400.0d0 + DBLE(ntm)/1000.0d0
     &        + DBLE(offset)
         rates_row = rates_row + 1
C write to rates file 
         CALL OSOWR_RATES_BC(lunrates,rates_out,rates_row,tsec,
     &                      sct,esct,slt,sht,ns,srat,
     &                      bbnrm,area,status)
         IF ( status.NE.0 ) THEN
            WRITE (errm,
     & '( '' Problem writing to rates file : '',a80)')
     &  rates_out
            call xaerror(errm,1)
            return
         ENDIF
      
C TSEC IS SEC OF FIRST DAY

 1700 ntst = msrc
C HERE NTST IS SET AT BEGINNING OF INTEGRATION
      idss = ids
      sct = 0.0
      esct = 0.0
      slt = 0.0
      sht = 0.0
      DO 1800 i = 1 , 63
         nst(i) = 0
 1800 CONTINUE
      nsc = 0
      nc = 0
      bnrmt = 0.0

      IF ( qtm ) GOTO 1400
C if got to here ... rates data is completed . 
C Therefor finalise keywords in rates file 
     
      CALL OSOCL_RATES_BC(lunrates,rates_row,jdst,jsst,jded,jsed,
     &                    areatot,rates_out,status)
      IF ( status.NE.0 ) THEN
         WRITE (errm,
     & '( '' Problem updating rates file, : '',a80)')
     &  rates_out
         call xaerror(errm,1)
         return
      ENDIF

C Now ready to produce spectrum ..
      rates = .TRUE. 
      GOTO 500

 2000 continue 
      IF (rawopen) then   
         CALL FTCLOS(rawunit,status)
         IF ( status.NE.0 ) THEN
             WRITE (errm,
     &'('' Unable to close raw fits PHA file : '',a40)')
     & rawphaf(nf)
            CALL XAERROR(errm,1)
            return
          ENDIF
          CALL XFRLUN(rawunit,status)
          IF ( status.NE.0 ) THEN
                WRITE (errm,
     &'('' Problem releasing unit no. for : '',a40)')
     & rawphaf(nf)
                CALL XAERROR(errm,1)
                return
          ENDIF  
      ENDIF
      RETURN
      END
