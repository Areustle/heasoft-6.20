**==aa0001.spg  processed by SPAG 4.50J  at 16:17 on 18 Mar 1999
C       [300,105]PHAST.FTN       (SELECTED PHA)
C	PHART -- GENERATES RATES FROM OSO-8 A DET PHA DATA.
C.....RD1:[300,105]PHAST : [300,104]PHART ALTERED TO SELECT PHA
C.....BY INTENSITY THRESHOLDS ON THE RATE ('THRES1' AND 'THRES2'
C.....FOR 'SCT'. RATE WINDOW IS CHOSEN BY CHANNEL BOUNDARIES,
C.....AS BEFORE). IF INTENSITY CRITERION INVOKED, ( BY 'QPHSRT' ),
C.....ONLY SOURCE AND BKGRD PHA CORRESPONDING TO ALLOWED RATES KEPT.
C.....(PHA FOR ADJACENT BUT UNTESTED RATES ARE DISCARDED.)
C.....PROGRAM DEFAULTS TO ORIGINAL PROGRAM EXCEPT FOR ADDITIONAL
C.....PRINTOUT -- MORE INPUT VALUES AND VARIOUS RATES.
C
C.....VARIABLES ADDED FOR PRINTOUT CONVENIENCE ONLY :
C.....     QSTAR MARKS RATES WHICH MEET INTENSITY CRITERION;
C.....     ITRUN(2) GIVES PROGRAM RUN TIME.
C
C.....NEW VARIABLES INTRODUCED TO ALLOW INTERMEDIATE SUMMATION OF
C.....PHA DATA BEFORE TIME-INTEGRATED RATE TESTED AGAINST LIMITS :
C.....     NSTI(63),NSTIT(63),NBTI(63),NBTIT(63),ANRMI,ANRMTI.
C
C.....NEW VARS. INTRODUCED TO CALCULATE AND PRINT OUT NO. OF PTS.,
C.....UNWTED AND WTED AVER. RATES, ERRORS, AND DISPERSION :
C.....     NLC, USCT, UESCT, WSCT, WESCT, DUSCT.
C.....SAME AS ABOVE, FOR INTENSITY-SELECTED POINTS :
C.....     NSLC, SUSCT, SUESCT, SWSCT, SWESCT, SDUSCT.
C
C.....NEW VARS. INTRODUCED TO PRINT OUT VARIOUS RATES FROM FINAL
C.....PHA SUM -- WINDOW RATE, HARDNESS(/SOFTNESS) RATIO, AND ERRORS :
C.....     PRATE, ERATE, PNUM, ENUM, PDEN, EDEN, ERAT,
C.....     PHRAT, EHRAT, PSRAT, ESRAT.
C
C.....CHANGED TO ALLOW HRATE-FORMATTED RATE FILE FOR [300,136]ACF.
C.....(CHANGES LIFTED FROM CORRECTED VERSION OF [300,126]ORATES.)
C.....OPTION CHOSEN BY 'QHRATE', DEFAULTING TO ORIGINAL FORMAT.
C
C.....   PROGRAM DEBUGGED (?)  03-AUG-80    JLRS.
C

CH  Lorraine Breedon (1.0.0 14 Apr 1999) Original working FTOOL version

c This routine creates spectra and lightcurves from OSO-8 A
c detector data for a selected source.



      SUBROUTINE OSOPHAMAKE_A(src_Ra50,src_Dc50,title,filename,
     &                detector,rawphaf,nraw,ph_restriction,period,
     &                ephemeris,phase_st,phase_sp,
     &                fov_check,ncontsrces,
     &                cont_Ra50,cont_Dc50,
     &                ch_start,ch_end,integ_time,
     &                int_low,int_high,
     &                lchan_min,lchan_max,
     &                hchan_min,hchan_max,
     &                beginday,imsecs,endday,imsece,
     &                ichat,clobber,status)

      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL a , aaa , an , ang , anrm , anrmi , anrmt , anrmti , area , 
     &     b , bbb , bc , bce , bdec , bnrm , bnt , bpa , bra , bt , 
     &     dusct
      REAL eden , efas , ehrat , enum , erat , erate , erbpa , esc , 
     &     esct , esrat , fas , pden , per , phrat , pnum , prate , 
     &     psrat , ra , rdec , RPM
      REAL rrpm , sc , sce , sct , SDEc , sdusct , sfas , sh , sht , 
     &     sl , slt , SNDec , SNRa , snt , spd , SPDec , spr , SPRa , 
     &     SRA , srat
      REAL ss , st , suesct , SUNdec , SUNra , susct , swesct , swsct , 
     &     thres1 , thres2 , uesct , usct , wesct , wsct
      INTEGER*2 h , hdata , HTT
      INTEGER i , ia , ibr , iche , ichs , IDAta , idaye , idays , ids , 
     &        idsc , idsec , ierr , ifas , ip , isb , isd , 
     &        ISEd , iso , isr
      INTEGER iss , isso , ISSt , iy , j ,  jsed , jsst , 
     &        kiop , kn0 , kno , knob , ksed , ksst , l1 , l2 , l3 , l4
      INTEGER lr , lrtyp , nb , nbc , nbh , nbl , nbti , nbtit , nc , 
     &        nhr1 , nhr2 , nhr3 , nhr4 , ns , nsc , nsci , nsh , 
     &        nsl , nsti
      INTEGER nstit
      LOGICAL qang , qbg , qbgr , qdisc , qdsc , 
     &          qhrate , qphsrt , qpr , qsec , qsrc , qstar , qtm , qusn
C*** End of declarations inserted by SPAG
      DOUBLE PRECISION tsec , dper , dref , dtim
      INTEGER*4 mss , mse , msrc , idms , IDY , IMS , ISEct , ntst , 
     &          ntm , mint
      INTEGER*4 idss , kview , kback , kread
      INTEGER*4 ist , ibt , nst , nbt
      INTEGER*4  nlc , nslc
      DIMENSION qusn(6) , hdata(2310) , IDAta(77) , ia(6)
      DIMENSION an(256) , nst(63) , nbt(63) , sc(63) , bc(63) , sce(63)
     &          , bce(63)
      DIMENSION nsti(63) , nbti(63) , nstit(63) , nbtit(63)
      EQUIVALENCE (IDAta,IDY)
      DATA qusn/'P' , 'H' , 'A' , ' ' , ' ' , ' '/ 


C variables introduced in major re-write (L.B.)

      character*(*) filename, title,rawphaf(*)
      LOGICAL clobber,ieof,rates,rawopen,angle_check,
     &        ph_restriction,fov_check,qfov,time_check
      REAL src_Dc50, src_Ra50,pha(63),phase_st,phase_sp,
     &           int_low,int_high,cont_Ra50(*), cont_Dc50(*)
      REAL test, tstnrm
      INTEGER lchan_min,lchan_max,hchan_min,
     &           hchan_max,lunpha,ncontsrces,jfov,kchk
      INTEGER ichat,ch_start,ch_end,nraw,nf,raw_row
      integer beginday,  endday, imsecs, imsece,rates_row
      INTEGER detector,status,row,Nbins,lunrates,integ_time,k
      INTEGER rawunit , blocksize , hdutype,totrows,raw_idy,raw_ims
      character(80) comment
      character(160) message,errm,rates_out
      DOUBLE PRECISION period,ephemeris
      REAL svra(10) , svdec(10)
      character qform
      integer*4 raw_isect


c initialise variables
      RPM = 5.8
      idsec = 64
      ntst = 0
      nc = 0
      spr = 0.0
      spd = 0.0
      kview = 0
      kiop = 0
      knob = 0
      kn0 = 0
      kback = 0
      kread = 0
      qdisc = .TRUE.
      lrtyp = 0
      qhrate=.FALSE.
      qbg = .FALSE.
      qdsc = .TRUE.
      qbgr = .FALSE.
      qang = .FALSE.
      rawopen=.FALSE.
      ieof = .FALSE.
      rates = .FALSE.
      angle_check=.FALSE.
      time_check=.FALSE.
      qphsrt = .FALSE.
      lunpha = 0
      lunrates = 0
      rawunit = 0
      row = 0
      rates_row = 0
      nf = 0

C.....ALLOW FOR HRATE FORMAT.
C
      aaa = 0.0
      l1 = 1
      l2 = 0
      l3 = 0
      l4 = 0
      bbb = 0.0
      j = 1
      h = 1
      area = 263.0
      IF ( qhrate ) THEN
         DO 150 iy = 1 , 4
c            WRITE (6,ERR=100) iy , h , l1 , l2 , l3 , l4 , aaa , bbb
 150     CONTINUE
      ENDIF

 200  ra = src_Ra50
      rdec =  src_Dc50
      bra = 0.0
      bdec = 0.0
      type *, 'ra=',ra
      type *, 'rdec=',rdec


c set up channels for LC integration
      ichs = ch_start
      iche = ch_end
      mint = integ_time
      qpr = .TRUE.

C set up channel boundaries 
      nhr1 = lchan_min
      nhr2 = lchan_max
      nhr3 = hchan_min
      nhr4 = hchan_max

C set up rate range for PHA accumulation
      qphsrt = .FALSE.
      thres1 = int_low
      thres2 = int_high

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
 

      isb = 0
      iso = 0
      ibr = 0
      isr = 0   
      iss = 0
      ISSt = 0

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

 300  nsc = 0
      nbc = 0
      anrmt = 0.0
C
C.....INITIALIZE LC COUNTERS, AVER. RATE VARS., AND
C.....AND INTERMEDIATE VARIABLES.
C
      nlc = 0
      nslc = 0
      usct = 0
      uesct = 0
      wsct = 0
      wesct = 0
      susct = 0
      suesct = 0
      swsct = 0
      swesct = 0
      dusct = 0
      sdusct = 0
C
      nsci = 0
      anrmti = 0.0
      anrmi = 0.0
      DO 400 i = 1 , 63
         nsti(i) = 0
         nbti(i) = 0
         nstit(i) = 0
         nbtit(i) = 0
         nst(i) = 0
         nbt(i) = 0
 400  CONTINUE
 500  qsrc = .FALSE.
      qtm = .FALSE.
      qstar = .FALSE.
      qsec = .FALSE.

      IF (.NOT. rates) then
          idays = beginday
          mss = imsecs
          idaye = endday
          mse = imsece
      ELSE
          idays = 0
          idaye = 0
      ENDIF
       
      type *, 'HI(27)'

      IF ( idays.EQ.0 .AND. idaye.EQ.0 ) THEN
         IF ( nsc.NE.0 .AND. nbc.NE.0 ) THEN
            IF ( qdsc ) THEN
C create header for output spectral file
               call OSOCR_SPEC(lunpha,filename,detector,title,ra,
     &                          rdec,clobber,status)
                IF ( status.NE.0 ) THEN
                   WRITE (errm,
     & '( '' Error creating : '', a80)')
     &  filename
                   call xaerror(errm,1)
                   return
                ENDIF
            ENDIF

            snt = 0.0
            bnt = 0.0
            anrm = anrmt/nsc
            DO 520 i = 1 , 63
               st = nst(i)
               bt = nbt(i)
               sc(i) = st/nsc
               bc(i) = bt/nbc
               sce(i) = SQRT(st)/nsc
               bce(i) = SQRT(bt)/nbc
               snt = snt + sc(i)
               bnt = bnt + bc(i)
 520        CONTINUE
C
C.....USE FINAL PHA CHANNEL ENTRIES TO CALCULATE CHOSEN RATE,
C.....LOW ENERGY RATE, HIGH ENERGY RATE, HARDNESS RATIO, SOFTNESS
C.....RATIO, AND ERRORS.
C
            prate = 0
            erate = 0
            DO 540 i = ichs , iche
               prate = prate + sc(i) - bc(i)
               erate = erate + sce(i)*sce(i) + bce(i)*bce(i)
 540        CONTINUE
            prate = prate/anrm
            erate = SQRT(erate)/anrm
C
            pnum = 0
            enum = 0
            DO 560 i = nhr3 , nhr4
               pnum = pnum + sc(i) - bc(i)
               enum = enum + sce(i)*sce(i) + bce(i)*bce(i)
 560        CONTINUE
            pnum = pnum/anrm
            enum = enum/(anrm*anrm)
C
            pden = 0
            eden = 0
            DO 580 i = nhr1 , nhr2
               pden = pden + sc(i) - bc(i)
               eden = eden + sce(i)*sce(i) + bce(i)*bce(i)
 580        CONTINUE
            pden = pden/anrm
            eden = eden/(anrm*anrm)
C
            phrat = pnum/pden
            erat = SQRT((enum/(pnum*pnum))+(eden/(pden*pden)))
            ehrat = phrat*erat
C
            psrat = pden/pnum
            esrat = psrat*erat
C
            enum = SQRT(enum)
            eden = SQRT(eden)
C
C.....PRINT OUT THESE PHA RATES, RATIOS, AND ERRORS.
            
            message=' '
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' MEAN RATE (+error) for PHA data (c/s/det/chan) :'',2F10.3)') 
     & prate , erate
            CALL XWRITE(message,ichat)
            WRITE (message,
     &'('' HARDNESS RATIO for PHA  data :'',F8.3)') 
     & phrat
            CALL XWRITE(message,ichat)
            message=' '
            CALL XWRITE(message,ichat)

C write output to spectral FITS file
c            call OSOWR_SPEC(lunpha,Nbins,jdst,jsst,jded,jsed,sc,sce,
c     &                     status)
c            IF ( status.NE.0 ) THEN
c               WRITE (errm,
c     & '( '' Problem writing to spectral file : '',a80)')
c     &  filename
c               call xaerror(errm,1)
c               return
c            ENDIF

            IF ( qdsc ) THEN
               sc(1) = nsc
               bc(1) = nbc
            ENDIF
         ENDIF
         goto 2200
      ELSE
         type *, idays , mss , idaye , mse
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
       type *, '*** before RDRAW **'
     

 590   raw_row=raw_row+1
       

c       type *, 'qtm =',qtm
c      type *, 'raw_row=',raw_row

 600   call OSO_RDRAW(rawunit,qtm,detector,raw_row,totrows,raw_isect,
     &                raw_idy,raw_ims,
     &                SRA,SDEc,SNRa,SNDec,pha,ieof,status)

c               type *, SRA,SDEc,SNRa,SNDec
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
        ISEct = raw_isect

c      type *, 'HI(1)'

      IF ( .NOT.qtm .AND. .NOT. ieof) GOTO 800
c      type *, 'HI(2)'
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
           goto 2200
      ENDIF

 700  SPRa = SRA
      SPDec = SDEc
      SUNra = SNRa
      SUNdec = SNDec

c       type *, SPRa,SPDec,SUNra,SUNdec
c       type *, 'HI(3)'
      IF ( .NOT.qsec ) GOTO 1000
c        type *, 'HI(4)'
      IF ( qtm ) THEN
c          type *, 'HI(5)'
         IF ( IDY.GT.idaye ) qtm = .FALSE.
         IF ( IDY.EQ.idaye .AND. IMS.GT.mse ) qtm = .FALSE.
c         type *, 'HI(6)'
         IF ( .NOT.qtm ) type * , '**', ISEct, IDY , IMS, raw_row
         IF ( .NOT.qtm ) GOTO 2000
c          type *, 'HI(7)'
         IF ( ifas.EQ.1 ) THEN
            dtim = DFLOTJ(IDY) + DFLOTJ(IMS)/8.64D7
            dtim = (dtim-dref)/dper
            ip = dtim
            fas = dtim - ip
            IF ( (sfas.LT.efas) .AND. (fas.LT.sfas .OR. fas.GT.efas) )
     &        GOTO 590
            IF ( (sfas.LE.efas) .OR. (fas.GE.sfas) .OR. (fas.LE.efas) )
     &        GOTO 710
              GOTO 590
         ENDIF
 710     IF ( jfov.NE.0 ) THEN
           ISEd = ISEct
           isd = ISEct
           IF ( isd.LT.ISSt ) isd = isd + 256
           idsc = isd - ISSt
           IF ( idsc.GT.40 .AND. idsc.LE.64 ) THEN
              idsec = idsc
           ELSE
              ISSt = ISEd - idsec
              IF ( ISSt.LT.0 ) ISSt = ISSt + 256
           ENDIF


           IF ( (ISEd-iss).LT.isr .OR. (ISEd-iss).GE.idsec-isr ) THEN
            IF ( (iss-ISSt).LE.isr .OR. (iss-ISSt).GT.idsec-isr ) THEN
                ISSt = ISEct
                goto 590
            ENDIF
           ENDIF
           qfov = .FALSE.
           DO 720 k = 1 , kchk
               CALL ANGLE(svra(k),svdec(k),SPRa,SPDec,test)
               IF ( test.LE.10 ) THEN
                  CALL SDIST(ISSt,ISEd,RPM,SPRa,SPDec,SUNra,
     &                       SUNdec,ra,rdec,an)
                  CALL SNORM(ISSt , ISEd , RPM,an,tstnrm)
                  WRITE (message,
     &'('' WARNING : Contaminating srce in FOV (RA DEC) : '',2F8.3)') 
     & svra(k),svdec(k)
                  CALL XWRITE(message,ichat)
                  WRITE (message,
     &'('' .. Angle between srce and spin axis : '',F8.3)') 
     & test
                  CALL XWRITE(message,ichat)
                  WRITE (message,
     &'('' .. EFFECTIVE AREA  : '',F8.3)') 
     & tstnrm
                  CALL XWRITE(message,ichat)
                  qfov = .TRUE.
               ENDIF
 720       CONTINUE
           IF ( qfov ) then
               WRITE (message,
     &'(''..for DAY (of 1975) and MILLISECS of that day :'',I4,F12.3)')
     & IDY,IMS
            CALL XWRITE(message,ichat)
           ENDIF
         ENDIF

      Endif
      


      goto 900

 800  IF ( IDY.LT.idays ) GOTO 590
      IF ( IDY.EQ.idays .AND. IMS.LT.mss ) GOTO 590
      type *, 'HI(8)'
      type *, IDY, IMS
      qtm = .TRUE.
      GOTO 700

 900  ISEd = ISEct
      isd = ISEct
c      type *, 'helloooo'
c      type *, ISEd,isd,ISSt

      IF ( isd.LT.ISSt ) isd = isd + 256
      idsc = isd - ISSt
      IF ( idsc.GT.40 .AND. idsc.LE.64 ) THEN
         idsec = idsc
      ELSE
         ISSt = ISEd - idsec
         IF ( ISSt.LT.0 ) ISSt = ISSt + 256
      ENDIF
      
c       type *, 'HI(9)'
c        type *, 'raw_row=',raw_row
c       type *, 'ISEd = ',ISEd 
c       type *, 'ISSt= ',ISSt
c        type *, 'IDY=',IDY
c       type *, 'IMS=',IMS
      
c      type *, ISEd,iss,isr,idsec,ISSt
      IF ( (ISEd-iss).LT.isr .OR. (ISEd-iss).GE.idsec-isr ) THEN
         IF ( (iss-ISSt).LE.isr .OR. (iss-ISSt).GT.idsec-isr ) THEN
            IF ( qsrc ) THEN
               IF ( (ISEd-isb).GE.ibr .AND. (ISEd-isb).LT.idsec-ibr )
     &              GOTO 1400
               IF ( (isb-ISSt).GT.ibr .AND. (isb-ISSt).LE.idsec-ibr )
     &              GOTO 1400
               ISSt = ISEct
               GOTO 590
            ELSE
               IF ( .NOT.qsrc ) kno = kno + 1
c                IF ( kno.GT.15 ) PRINT * , ' NO SOURCE SECT OUT OF ' , 
c     &                                kno
               ISSt = ISEct
               GOTO 590
            ENDIF
         ENDIF
      ENDIF

        type *, 'HI(10)'    
       type *, 'raw_row=',raw_row
      type *, 'ISEd = ',ISEd 
      type *, 'ISSt= ',ISSt
      type *, 'IDY=',IDY
      type *, 'IMS=',IMS


      kno = 0
      IF ( (isd-ISSt).EQ.64 ) THEN
         rrpm = 60000./(IMS-msrc)
      ELSE
         per = 532480./FLOAT(isd-ISSt)
         rrpm = 60000./per
      ENDIF
      IF ( rrpm.GT.5.0 .AND. rrpm.LT.7.0 ) RPM = (9.*RPM+rrpm)/10.0
      msrc = IMS
      ids = IDY
      CALL ANGLE(SPRa,SPDec,spr,spd,ang)
      IF ( ang.LE..2 ) THEN
        type *, 'HI(10)'  
         
         IF ( idsec.GE.64 .AND. nc.NE.0 ) GOTO 1200
         type *, 'HI(11)'

         GOTO 1100
      ENDIF

      type *, 'HI(12)'

 1000 type *, 'HI THERE !!'
      type *, ISSt,ISEd,RPM,SPRa,SPDec,SUNra,SUNdec,ra,rdec
      CALL SDIST(ISSt,ISEd,RPM,SPRa,SPDec,SUNra,SUNdec,ra,rdec,an)
      CALL AZIMTH(SPRa,SPDec,SUNra,SUNdec,ra,rdec,a,b,RPM)

       type *, '---'
      type *, an,a,b,RPM

      iss = (b*256./360.) - 12 + iso
      IF ( iss.LT.0 ) iss = iss + 256
      IF ( iss.GT.256 ) iss = iss - 256
      isso = iss - iso
      IF ( isso.LT.0 ) isso = isso + 256
      IF ( qbg ) THEN
         CALL AZIMTH(SPRa,SPDec,SUNra,SUNdec,bra,bdec,a,b,RPM)
         isb = (b*256./360.) - 12
         IF ( isb.LT.0 ) isb = isb + 256
      ENDIF
      spr = SPRa
      spd = SPDec

      type *, '+++'
      type *, spr,spd,ISSt , ISEd , RPM,an,anrm
 1100 CALL SNORM(ISSt , ISEd , RPM,an,anrm)
      angle_check = .TRUE.
      IF (.NOT. qsec) then
         qsec = .TRUE.
         ISSt = ISEct
         goto 590
      ENDIF

       type *, 'HI(13)'

 1200 IF ( anrm.GT.10. ) THEN
         ns = 0
         kview = kview + 1

           type *, 'HI(14)'
         IF ( kview.LE.10 ) type *, ' SOURCE         ' , IDY , IMS , 
     &                            ISEct , anrm
         nsl = 0
         nsh = 0
C
         IF ( qphsrt ) THEN
C
C
C.....READ IN TEMPORARY SOURCE PHA AND NORM. THESE WILL BE
C.....KEPT ONLY IF NEAREST RATE ENTRY IS.
C
            DO 1220 i = 1 , 63
               nstit(i) = pha(i)
 1220       CONTINUE
            anrmi = anrm
         ELSE
C
C.....IF THE PHA SORTING OPTION IS NOT CHOSEN, DEFAULT TO
C.....COLLECTION OF ALL SOURCE PHA.
C
            type *, 'HI(15)'
            DO 1240 i = 1 , 63
               nst(i) = nst(i) + pha(i)
 1240       CONTINUE
            nsc = nsc + 1
            anrmt = anrmt + anrm
         ENDIF
C
C
         DO 1250 i = ichs , iche
            ns = ns + pha(i)
 1250    CONTINUE
         DO 1300 i = nhr1 , nhr2
            nsl = nsl + pha(i)
 1300    CONTINUE
         DO 1350 i = nhr3 , nhr4
            nsh = nsh + pha(i)
 1350    CONTINUE
         qsrc = .TRUE.
         ksst = ISSt
         ksed = ISEd
         ISSt = ISEct
         GOTO 590
      ENDIF
      ISSt = ISEct
      GOTO 590
C
 1400 nb = 0
      kback = kback + 1
      IF ( kback.LE.10 ) type * , ' BKGRD SECTOR   ' , IDY , IMS , 
     &                         ISEct
      nbl = 0
      nbh = 0
C
      type *, 'HI(16)'
      IF ( qphsrt ) THEN
C
C
C.....READ IN TEMPORARY BKGRD PHA. THESE WILL BE KEPT ONLY IF
C.....NEAREST RATE ENTRY IS.
C
         DO 1450 i = 1 , 63
            nbtit(i) = pha(i)
 1450    CONTINUE
      ELSE
C
C.....IF PHA SORTING OPTION NOT CHOSEN, DEFAULT TO COLLECTION
C.....OF ALL BKGRD PHA.
C
         DO 1500 i = 1 , 63
            nbt(i) = nbt(i) + pha(i)
 1500    CONTINUE
         nbc = nbc + 1
      ENDIF
C
C
      type *, 'HI(17)'

      DO 1600 i = ichs , iche
         nb = nb + pha(i)
 1600 CONTINUE
      DO 1700 i = nhr1 , nhr2
         nbl = nbl + pha(i)
 1700 CONTINUE
      DO 1800 i = nhr3 , nhr4
         nbh = nbh + pha(i)
 1800 CONTINUE
      idms = IMS - msrc
      IF ( idms.LT.0 ) idms = idms + 86400000
      IF ( idms.LT.20000 ) THEN
         ss = (ns-nb)/anrm
         knob = 0
         esc = SQRT(FLOAT(ns+nb))/anrm
         sl = nsl - nbl
         sh = nsh - nbh
         IF ( nc.EQ.0 ) GOTO 2100
         ntm = msrc - ntst
         IF ( ntm.LT.0 ) ntm = ntm + 86400000
         IF ( ntm.GT.mint ) GOTO 2000
      ELSE
         knob = knob + 1
         IF ( knob.GT.3 ) type * , ' NO BACKGROUND FOR DATA AT ' , 
     &                          IDY , IMS
         qsrc = .FALSE.
         GOTO 590
      ENDIF

      type *, 'HI(18)'
 1900 sct = sct + ss/(esc*esc)
      esct = esct + 1./(esc*esc)
C
      IF ( qphsrt ) THEN
C
C.....UPDATE INTERMEDIATE SUMS OF SOURCE AND BKGRD PHA'S,
C.....AND UPDATE NORM, PRIOR TO TESTING OF INTEGRATED RATE.
C.....SPIN COUNTER NC CAN SERVE AS INTERMEDIATE COUNTER.
C
         DO 1950 i = 1 , 63
            nsti(i) = nsti(i) + nstit(i)
            nbti(i) = nbti(i) + nbtit(i)
 1950    CONTINUE
         anrmti = anrmti + anrmi
      ENDIF
C
C
      type *, 'HI(19)'
      slt = slt + sl
      sht = sht + sh
      ist = ist + ns
      ibt = ibt + nb
      jsst = ksst
      jsed = ksed
      bnrm = anrm
      nc = nc + 1
      qsrc = .FALSE.
      ISSt = ISEct
      GOTO 590

 2000 IF (esct .eq. 0.0 .AND. kno .gt. 0) then

C  srce NOT in FOV during requested time window ..therefore shut down
          WRITE (errm,
     &'('' Source NOT in FOV during requested time window '')')
         CALL XAERROR(errm,1)
           WRITE (errm,
     & '( '' ...deleting rates file: '',a80)')
     &  rates_out
          call xaerror(errm,1)
          status=0
          open(unit=lunrates, file=rates_out, status='old')
          close(lunrates, status='delete')
          goto 2200

      ELSEIF (esct .eq. 0.0 .AND. .NOT. qtm) then
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
           goto 2200
      ENDIF

      type *, 'HI(20)'

      sct = sct/esct
      esct = 1./SQRT(esct)
C
C.....UPDATE COUNTER OF LC POINTS, AND RUNNING SUMS OF RATES,
C.....ERRORS, AND DISPERSIONS.
C
      nlc = nlc + 1
      usct = usct + sct
      dusct = dusct + (sct*sct)
      uesct = uesct + (esct*esct)
      wsct = wsct + sct/(esct*esct)
      wesct = wesct + 1./(esct*esct)
C
C.....IF (QPHSRT), COMPARE CHOSEN RATE TO CHOSEN LIMITS.
C
      IF ( qphsrt ) THEN
         IF ( (sct.LE.thres2) .AND. (sct.GE.thres1) ) THEN
            qstar = .TRUE.
C
C.....UPDATE COUNTER OF LC POINTS MEETING INTENSITY CRITERION,
C.....AND RUNNING SUMS OF THUS RESTRICTED RATES, ERRORS, AND
C.....DISPERSION.
C
            nslc = nslc + 1
            susct = susct + sct
            sdusct = sdusct + (sct*sct)
            suesct = suesct + (esct*esct)
            swsct = swsct + sct/(esct*esct)
            swesct = swesct + 1./(esct*esct)
C
C.....UPDATE NO. OF SOURCE AND BKGRD ENTRIES, TOTAL NORM, AND ALLOWED
C.....PHA ACCUMULATION. THEN ZERO INTERMEDIATE VARS. FOR NEXT GO.
C
            nsc = nsc + nc
            nbc = nbc + nc
            anrmt = anrmt + anrmti
            DO 2020 i = 1 , 63
               nst(i) = nst(i) + nsti(i)
               nbt(i) = nbt(i) + nbti(i)
 2020       CONTINUE
         ENDIF
C
         type *, 'HI(21)'
         anrmti = 0.0
         DO 2050 i = 1 , 63
            nsti(i) = 0
            nbti(i) = 0
 2050    CONTINUE
      ENDIF
C
C

      type *, 'HI(22)'

      IF ( slt.NE.0. ) srat = sht/slt
      IF ( slt.EQ.0. ) type *, 'ZERO SOFT COUNTS'
      st = FLOAT(ist)/nc
      bt = FLOAT(ibt)/nc
      ntm = ntst + (60000./RPM)*FLOAT(nc-1)/2.0
      IF ( ntm.GE.86400000 ) THEN
         idss = idss + 1
         ntm = ntm - 86400000
      ENDIF
      IF ( qdisc ) THEN
         tsec = (idss-idays)*86400. + ntm/1000.
CC
         IF ( lrtyp.NE.0 ) THEN
c            bpa = bt/263.
c            erbpa = SQRT(bt/nc)/263.
c            IF ( qpr ) WRITE (3,99036) idss , ntm , bpa , erbpa
c            IF ( .NOT.qhrate ) WRITE (6) tsec , bpa , erbpa
c            IF ( qhrate ) WRITE (6) idss , ntm , area , bpa , erbpa , l1
         ENDIF
CC
         IF ( lrtyp.NE.1 ) THEN
C
c            IF ( .NOT.qhrate ) WRITE (6) tsec , sct , esct
c            IF ( qhrate ) WRITE (6) idss , ntm , area , sct , esct , l1
         ENDIF
      ENDIF
C
      IF ( qpr ) THEN
         IF ( .NOT.qstar ) THEN
c            WRITE (3,99037) idss , ntm , sct , esct , srat , ichs , 
c     &                      iche , isso , isb , nc , st , bt , bnrm , 
c     &                      RPM , jsst , jsed , SUNra , SUNdec
c         ELSE
c            WRITE (3,99038) idss , ntm , sct , esct , srat , ichs , 
c     &                      iche , isso , isb , nc , st , bt , bnrm , 
c     &                      RPM , jsst , jsed , SUNra , SUNdec
            qstar = .FALSE.
         ENDIF
      ENDIF
      type *, 'HI(23)'

 2100 ntst = msrc
      idss = ids
      sct = 0.0
      esct = 0.0
      slt = 0.0
      sht = 0.0
      ist = 0
      ibt = 0
      nc = 0

      type *, 'HI(24)'

      IF ( qtm ) GOTO 1900

      type *, 'HI(25)'

C
C.....CALCULATE AND PRINTOUT AVER. RATES, NO. OF LC POINTS.
C
      usct = usct/nlc
      dusct = SQRT((dusct/(nlc-1))-(usct*usct))
      uesct = SQRT(uesct)/nlc
      wsct = wsct/wesct
      wesct = 1./SQRT(wesct)
      type *, 'no of lc points', nlc
      type *, 'unweighted av. rate + error ',usct , uesct
      type *, 'dispersion',dusct
      type *, 'weighted av. rate + error ',wsct , wesct
      IF ( qphsrt ) THEN
C
C.....IF INTENSITY RESTRICTED BY LIMITS, CALCULATE AND PRINT
C.....RESTRICTED AVER. RATES, ERRORS, AND NO. OF RESTRICTED
C.....LC POINTS.
C
         susct = susct/nslc
         sdusct = SQRT((sdusct/(nslc-1))-(susct*susct))
         suesct = SQRT(suesct)/nslc
         swsct = swsct/swesct
         swesct = 1./SQRT(swesct)
         type *, 'no of lc points within limits', nslc
         type *, 'unweighted av. rate + error ',susct , suesct
         type *, 'dispersion', sdusct
         type *, 'weighted av. rate + error ',swsct , swesct
      ENDIF
C
C
      type *, 'HI(26)'

      rates = .TRUE.
      GOTO 500

 2200 continue 
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

**==sdist.spg  processed by SPAG 4.50J  at 16:17 on 18 Mar 1999
CCCC
      SUBROUTINE SDIST(ISSt,ISEd,RPM,SPRa,SPDec,SUNra,SUNdec,
     &                 Ra,Rdec,S)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL b , fa , Ra , Rdec , RPM , S , xa , ya , z
      REAL SPRa,SPDec,SUNra,SUNdec
      INTEGER i , ISEd , ISSt , j
C*** End of declarations inserted by SPAG
      DIMENSION xa(2) , ya(2) , z(256) , S(256)
     

      type *, '** in sdist **'

      fa = 360./256.
      type *, SPRa,SPDec,SUNra,SUNdec,Ra,Rdec,xa(1),xa(2),RPM
      type *, '***'
      CALL AZIMTH(SPRa,SPDec,SUNra,SUNdec,Ra,Rdec,xa(1),xa(2),RPM)
      type *, SPRa,SPDec,SUNra,SUNdec,Ra,Rdec,xa(1),xa(2),RPM
      DO 100 i = 1 , 256
         z(i) = (fa*i) - xa(2)
         CALL CAAAB(5.0,z(i),xa(1),ya(1),ya(2))
        type *, 'oops'
         type *, z(i),xa(1),ya(1),ya(2)
         IF ( ya(1).GT.5.09 ) ya(1) = 5.09
         b = ya(1)/5.09
         z(i) = 0.6366*(ACOS(b)-(b*SQRT(1-b*b)))
         type *, 'z(i)=',z(i)
 100  CONTINUE
      DO 200 j = 1 , 244
         S(j) = z(j+12)
c         type *, 'S(j)=',S(j)
 200  CONTINUE
      DO 300 j = 1 , 12
         S(j+244) = z(j)
 300  CONTINUE
      RETURN
      END

**==snorm.spg  processed by SPAG 4.50J  at 16:17 on 18 Mar 1999
CCCC
      SUBROUTINE SNORM(ISSt , ISEd , RPM,S,Anrm)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL Anrm , ex , RPM , S
      INTEGER ISEd , ISSt , ist , j
C*** End of declarations inserted by SPAG

      DIMENSION S(256)
      ex = 0.0
C	CALCULATE SECTOR NORM
      ist = ISSt + 1
      IF ( ISEd.LT.ISSt ) THEN
         DO 50 j = ist , 256
            ex = ex + S(j)
 50      CONTINUE
         DO 100 j = 1 , ISEd
            ex = ex + S(j)
 100     CONTINUE
      ELSE
         DO 150 j = ist , ISEd
            ex = ex + S(j)
 150     CONTINUE
      ENDIF
      IF ( ex.GT.1.E-5 ) THEN
         Anrm = (263.2/256.)*(60./RPM)*ex
      ELSE
         Anrm = 0.0
      ENDIF
      RETURN
      END


**==azimth.spg  processed by SPAG 4.50J  at 15:39 on 16 Apr 1999
      SUBROUTINE AZIMTH(SPRa,SPDec,SUNra,SUNdec,
     &                  Sorra,Sordec,Spsor,Sunsor,Rpm)
      IMPLICIT NONE
C*** Start of declarations inserted by SPAG
      REAL b1 , b2 , c2 , Rpm , Sordec , Sorra , SPDec , SPRa , Spsor , 
     &     SUNdec , SUNra , Sunsor , t1 , t2 , t3
C*** End of declarations inserted by SPAG

      t1 = 90. - SPDec
      t2 = SUNra - SPRa
      t3 = 90. - SUNdec
      CALL CAAAB(t1,t2,t3,b1,b2)
      t2 = Sorra - SPRa
      t3 = 90. - Sordec
      CALL CAAAB(t1,t2,t3,Spsor,c2)
      Sunsor = c2 - (b2-22.9-2.45*Rpm)
      IF ( Sunsor.LT.0.0 ) Sunsor = Sunsor + 360.

      type *, 'Spsor=', Spsor
      type *, 'Sunsor=',Sunsor
      RETURN
      END
