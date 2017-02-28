
*+WTPCMKF
c    ------------------------------------------------------------
      subroutine wtpcmkf(ounit,telescope,instrume,detnam,extname,
     &                   time,mjdref,hduclas3,n_mkf,mag_ang,
     &                   lval,zen_ang,ses_ang,moon_ang,sun_ang,
     &                   sat_lon,sat_lat,vel_ang,rasat,decsat,
     &                   asp_qual,stt_qual,stat,ra_sc,dec_sc,
     &                   hkp_rows,miss,temp,press,hvolt,filpos,
     &                   mv_aco,xtransm,a1_al,sa_rate,
     &                   xacc,a1al_mv,a1_ah,ap,
     &                   hksta_rows,hv_sta,carr_sta,gas_sta,det_sta,
     &                   temp_sta,filt_sta,lv_sta,inst_sta,
     &                   tel_sta,cur_sta,obi_num,deadt,
     &                   nk_hist,hist,nk_comm,comms,ierr,chatter) 
c    ------------------------------------------------------------
c
c ___ DESCRIPTION _______________________________________________________ 
c                                                                         
c This routine writes a FITS MAKEFILTER extension in OGIP format for
c ROSAT PSPC  
c
c HDUCLASS = OGIP 
c HDUCLAS1 = TEMPORALDATA
c HDUCLAS2 = HK
c HDUVERS  = 1.0.0
c
c                                 
c NOTE : Assumes FITS file is open and has had the primary header written.
c        File is left open at end ... use FTCLOS to close file            
c                                 ... or FTCRHD to add another extension. 
c COLUMNS WRITTEN
c 
c TIME         Modified Julian Date
c MAG_ANG      Magnetic Zenith Angle
c LVAL         MacIlwain parameter
c ZEN_ANG      Zenith Angle
c SES_ANG      Sun-Earth-Satellite angle
c MOON_ANG     Angle between pointing direction and moon
c SUN_ANG      Angle between pointing direction and sun
c SAT_LON      Satellite longitude
c SAT_LAT      Satellite latitud
c VEL_ANG      Velocity vector-look direction (Ram) Angle
c RASAT        Satelite RA
c DESAT        Satelite DEC
c ASP_QUAL     Aspect quality ; 0,1-5 or 9 for gap
c STT_QUAL     Star tracker flag
c STATUS       Aspect Status
c RA_SC        Right ascension of spacecraft pointing 
c DEC_SC       Declination of spacecraft pointing 
c MISS         Missing housekeeping information
c TEMP         Temperature of Instrument
c PRESS        Pressure of Instrument
c HVOLT        Instrument high voltage
c FILPOS       Filter wheel position
c MV_ACO       MV anticoincidence rate
c XTRANSM      Transmitted X-ray rate
c A1_AL        events in anode A1 above low level threshold
c SA_RATE      SA_RATE 
c XACC         Accepted X-ray event
c A1AL_MV      A1 events above low level in antico
c A1_AH        A1 events above upper level threshold 
c AP           After pulse rate: (XE-AEXE/A1LL)
c HV_STA       Status of high voltage
c CARR_STA     Carrousal status
c GAS_STA      Gas system status
c DET_STA      Detector status
c TEMP_STA     Temperature status
c FILT_STA     Filter wheel postion
c LV_STA       Low voltage status
c INST_STA     Instrument in use
c TEL_STA      Telemetry status
c CURR_STA     Current status
c OBI_NUM      OBI Number
c DEADT        Deadtime correction
c                                                                         
c Keywords are ...
c
c TELESCOP    : Mission/Telescope name (if not known, pass 'UNKNOWN')
c INSTRUME    : Instrument/Detector name (if not known, pass 'UNKNOWN')
c DETNAM      : Detector name
c MJDREF      : Modified Julian Date referance
c HDUCLASS    : HDU 'family' keywords 
c DATE        : Current date
c
c History comments are written if passed to routine
c Comments are written if passed to routine
c
c ___ VARIABLES _________________________________________________________
c
      IMPLICIT NONE
      character*(*) telescope,instrume,extname
      character*(*) hduclas3,detnam
      integer nk_hist,chatter,nk_comm,ounit,ierr 
      character(80) hist(*),comms(*)
      integer n_mkf
      real*8 mag_ang(*),lval(*),zen_ang(*),ses_ang(*),time(*)
      real*8 moon_ang(*),sun_ang(*),sat_lon(*),sat_lat(*)
      real*8 vel_ang(*),mjdref,ap(*)
      real*8 asp_qual(*),stt_qual(*),rasat(*),decsat(*)
      integer stat(*)
      real*8 ra_sc(*),dec_sc(*),xacc(*),a1al_mv(*),a1_ah(*)
      integer hkp_rows(*),miss(*),temp(*),press(*)
      integer hvolt(*),filpos(*),cur_sta(*)
      real*8 mv_aco(*),xtransm(*),a1_al(*),sa_rate(*)
      real*4 deadt(*)
      integer hksta_rows(*),hv_sta(*),carr_sta(*),gas_sta(*)
      integer det_sta(*),temp_sta(*),lv_sta(*)
      integer inst_sta(*),filt_sta(*),tel_sta(*),obi_num(*)
      
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c ounit      int    : Fortran output unit
c nk_hist    int    : No of lines of history comments (max is maxhist)
c nk_comm    int    : No of lines of comments
c hist       char   : History array, containing comments
c comms      char   : Comment array
c telescop   char   : Mission/Telescope name
c instrume   char   : Instrument/Detector name
c hduclas3   char   : Approved values are PREDICTED, TOTAL or NET
c chatter    int    : chattiness flag for o/p (5 quite,10 normal,>20 silly)
c
c --- CALLED ROUTINES -----------------------------------------------------
c
c subroutine FTCRHD       : (FITSIO) Creates new FITS extension
c subroutine FTPHBN       : (FITSIO) Writes required header keywords
c subroutine FTBDEF       : (FITSIO) Defines the BINTABLE data structure
c subroutine FTPCLx       : (FITSIO) Writes the data, here ftpcle and 
c                           ftpclj, x is the datatype
c subroutine WT_FERRMSG   :  (CALLIB) Writes error text if required
c
c --- COMPILATION AND LINKING ---------------------------------------------
c
c Link with FITSIO and CALLIB
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------------
c
c Rehana Yusaf (1994 June) 1.0.0;
        character(5) version
        parameter ( version = '1.0.0' )
*-
c _________________________________________________________________________
c
c
c --- INTERNAL VARIABLES ---
c
      character(30) errstr,wrnstr,comm
      character(70) subinfo,exhist,errinfo
      integer frow,felem,status,tfields,i,nfields,var,col,row
      parameter (tfields=44)
      character(16) ttype(tfields),tform(tfields),tunit(tfields)
      character(40) tcomm(tfields)
      character(8) keywrd

c       --- USER INFO ---
c
        subinfo = ' ... using WTPCMKF Ver '//version
        IF (chatter.GE.15) THEN
          call fcecho(subinfo)
        ENDIF           
c
c       --- CREATE A NEW EXTENSION ---
c
        status = 0 
        errstr = 'ERROR : WTPCMKF Ver '//version//':'
        wrnstr = 'WARNING : WTPCMKF Ver '//version//':'
        call ftcrhd (ounit,status)
        errinfo = errstr//' creating new extension !'
        call wt_ferrmsg (status,errinfo)
        IF (status.NE.0) THEN
          ierr = 1
          return
        ENDIF

c
c       --- DEFINE COLUMN KEYWORDS ---
c

c       SETUP HEADER KEYWORDS DESCRIBING COLUMNS 

        nfields = 1
        ttype(nfields) = 'TIME'
        tform(nfields) = '1D'
        tunit(nfields) = 's'
        tcomm(nfields) = 'Time in secs'

        nfields = 2
        ttype(nfields) = 'MAG_ANG'
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Magnetic Zenith Angle'

        nfields = nfields + 1
        ttype(nfields) = 'LVAL'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'         
        tcomm(nfields) = 'MacIlwain L parameter'

        nfields = nfields + 1
        ttype(nfields) = 'ZEN_ANG'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Zenith Angle'

        nfields = nfields + 1
        ttype(nfields) = 'SES_ANG'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Sun-Earth-Satellite angle'

        nfields = nfields + 1
        ttype(nfields) = 'MOON_ANG'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Angle between pointing direction and moon'

        nfields = nfields + 1
        ttype(nfields) = 'SUN_ANG'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Angle between pointing direction and sun' 

        nfields = nfields + 1
        ttype(nfields) = 'SAT_LON'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Satellite longitude'

        nfields = nfields + 1
        ttype(nfields) = 'SAT_LAT'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Satellite latitude'

        nfields = nfields + 1
        ttype(nfields) = 'VEL_ANG'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Velocity vector-look direction (Ram) Angle'

        nfields = nfields + 1
        ttype(nfields) = 'RASAT'       
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Satellite RA'

        nfields = nfields + 1
        ttype(nfields) = 'DECSAT'       
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Satellite DEC'

        nfields = nfields + 1
        ttype(nfields) = 'ASP_QUAL'
        tform(nfields) = 'I'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Aspect Quality (0,1-5, or 9 for gap)'

        nfields = nfields + 1
        ttype(nfields) = 'STT_QUAL'        
        tform(nfields) = 'I'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Star tracker flag'

        nfields = nfields + 1
        ttype(nfields) = 'STATUS'
        tform(nfields) = 'I'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Aspect status'

        nfields = nfields + 1
        ttype(nfields) = 'RA_SC'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Right ascension of spacecraft pointing'

        nfields = nfields + 1
        ttype(nfields) = 'DEC_SC'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Declination of spacecraft pointing'

        nfields = nfields + 1
        ttype(nfields) = 'MISS'        
        tform(nfields) = 'I'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Missing housekeeping information'

        nfields = nfields + 1
        ttype(nfields) = 'TEMP'        
        tform(nfields) = 'I'
        tunit(nfields) = 'deg C'
        tcomm(nfields) = 'Temperature of instrument'

        nfields = nfields + 1
        ttype(nfields) = 'PRESS'        
        tform(nfields) = 'I'
        tunit(nfields) = 'MBARS'
        tcomm(nfields) = 'Pressure of instrument'

        nfields = nfields + 1
        ttype(nfields) = 'HVOLT'        
        tform(nfields) = 'I'
        tunit(nfields) = 'V'
        tcomm(nfields) = 'Instrument high voltage'

        nfields = nfields + 1
        ttype(nfields) = 'FILPOS'
        tform(nfields) = 'J'
        tunit(nfields) = '10**(-1) deg'
        tcomm(nfields) = 'Filter wheel position'

        nfields = nfields + 1
        ttype(nfields) = 'MV_ACO'        
        tform(nfields) = '1D'
        tunit(nfields) = 'count /s'
        tcomm(nfields) = 'MV anticoincidence rate'

        nfields = nfields + 1
        ttype(nfields) = 'XTRANSM'        
        tform(nfields) = '1D'
        tunit(nfields) = 'count /s'
        tcomm(nfields) = 'Transmitted X-ray rate'

        nfields = nfields + 1
        ttype(nfields) = 'A1_AL'        
        tform(nfields) = '1D'
        tunit(nfields) = 'count /s'
        tcomm(nfields) = 'events in anode A1 above low level threshold'

        nfields = nfields + 1
        ttype(nfields) = 'SA_RATE'        
        tform(nfields) = '1D'
        tunit(nfields) = 'count /s'
        tcomm(nfields) = 'SA_RATE'

        nfields = nfields + 1
        ttype(nfields) = 'XACC'
        tform(nfields) = '1D'
        tunit(nfields) = 'count /s'
        tcomm(nfields) = 'Accepted X-ray event rate'

        nfields = nfields + 1
        ttype(nfields) = 'A1AL_MV'
        tform(nfields) = '1D'
        tunit(nfields) = 'count /s'
        tcomm(nfields) = 'A1 events above low level in antico'

        nfields = nfields + 1
        ttype(nfields) = 'A1_AH'
        tform(nfields) = '1D'
        tunit(nfields) = 'count /s'
        tcomm(nfields) = 'A1 events above upper level threshold'

        nfields = nfields + 1
        ttype(nfields) = 'AP'
        tform(nfields) = '1D'
        tunit(nfields) = 'count /s'
        tcomm(nfields) = 'After pulse rate : (XE-AEXE)/A1LL'

        nfields = nfields + 1
        ttype(nfields) = 'HV_STA'        
        tform(nfields) = 'J'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Status of high voltage'

        nfields = nfields + 1
        ttype(nfields) = 'CARR_STA'        
        tform(nfields) = 'J'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Carrousal status'

        nfields = nfields + 1
        ttype(nfields) = 'GAS_STA'        
        tform(nfields) = 'J'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Gas system status'

        nfields = nfields + 1
        ttype(nfields) = 'DET_STA'        
        tform(nfields) = 'J'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Detector status'

        nfields = nfields + 1
        ttype(nfields) = 'TEMP_STA'        
        tform(nfields) = 'J'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Temperature Status'

        nfields = nfields + 1
        ttype(nfields) = 'FILT_STA'        
        tform(nfields) = 'J'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Filter Wheel position'

        nfields = nfields + 1
        ttype(nfields) = 'LV_STA'        
        tform(nfields) = '1J'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Low voltage status'

        nfields = nfields + 1
        ttype(nfields) = 'INST_STA'        
        tform(nfields) = '1J'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Instrument in use'

        nfields = nfields + 1
        ttype(nfields) = 'TEL_STA'        
        tform(nfields) = '1J'
        tunit(nfields) = 'NONE'
        tcomm(nfields) = 'Telemetry status'

        nfields = nfields + 1
        ttype(nfields) = 'CURR_STA'
        tform(nfields) = '1J'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Current status'


        nfields = nfields + 1
        ttype(nfields) = 'OBI_NUM'        
        tform(nfields) = '1J'
        tunit(nfields) = 'NONE'
        tcomm(nfields) = 'OBI number'

        nfields = nfields + 1
        ttype(nfields) = 'DEADT'       
        tform(nfields) = 'E'
        tunit(nfields) = 'NONE'
        tcomm(nfields) = 'Deadtime correction'

        status = 0
        var = 0
        call ftphbn(ounit,n_mkf,nfields,ttype,tform,tunit,
     &              extname,var,status)
        errinfo = errstr//' defining column keywords'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 3
          return
        ENDIF
c 
c - Fix up the Comments in the TTYPE keywords
c
        status = 0
        do i = 1, nfields
          write(keywrd,'(a5,i2)') 'TTYPE',i
          call crmvblk(keywrd)
          call ftmcom(ounit,keywrd,tcomm(i),status)
          if(status.NE.0) then
           subinfo = wrnstr // 'Problem altering '// keywrd
           call wt_ferrmsg(status,subinfo)
           status = 0
          endif
        enddo


c WRITING keywords


c TELESCOPE NAME

        status = 0
        comm = 'Name of Mission/Telescope'
        call ftpkys(ounit,'TELESCOP',telescope,comm,status)
        errinfo = errstr//' writing TELESCOPE keyword'
        IF (chatter.GE.9) THEN
          call wt_ferrmsg(status,errinfo)
        ENDIF

c INSTRUMENT NAME

        status = 0
        comm = 'Name of Instrument/Detector'
        call ftpkys(ounit,'INSTRUME',instrume,comm,status)
        errinfo = errstr//' writing INSTRUMENT keyword'
        IF (chatter.GE.9) THEN
          call wt_ferrmsg(status,errinfo)
        ENDIF

c DETNAM

        status = 0
        comm = 'Name of Detector'
        call ftpkys(ounit,'DETNAM',detnam,comm,status)
        errinfo = errstr//' writing DETNAM keyword'
        IF (chatter.GE.9) THEN
          call wt_ferrmsg(status,errinfo)
        ENDIF

c MJDREF

        status = 0
        comm = 'Modified Julien Date Referance'
        call ftpkyd(ounit,'MJDREF',mjdref,12,comm,status)
        errinfo = errstr//' writing MJDREF keyword'
        IF (chatter.GE.9) THEN
          call wt_ferrmsg(status,errinfo)
        ENDIF


c HDUCLASS and HDUVERS

        status = 0
        call ftpkys(ounit,'HDUCLASS','OGIP',
     &'format conforms to OGIP standard',status)
        errinfo = errstr//' Problem writing HDUCLASS keyword'
        call wt_ferrmsg(status,errinfo)

        status = 0
        call ftpkys(ounit,'HDUCLAS1','TEMPORALDATA',
     &'dataset is a response function',status)
        errinfo = errstr//' problem writing HDUCLAS1 keyword'
        call wt_ferrmsg(status,errinfo)

        status = 0
        call ftpkys(ounit,'HDUCLAS2','HK',
     &'dataset is a radial point response function',status)
        errinfo = errstr//' problem writing HDUCLAS2 keyword'
        call wt_ferrmsg(status,errinfo)

        status = 0
        call ftpkys(ounit,'HDUVERS','1.0.0',
     &'Version of family of formats',status)
        errinfo = errstr//' writing HDUVERS keyword'
        call wt_ferrmsg(status,errinfo)


c
c       --- ADDING OTHER COMMENTS ---
c
        status = 0
        call ftpdat(ounit,status)
        errinfo = errstr//' writing date'
        call wt_ferrmsg(status,errinfo)
        IF (nk_hist.GT.0) THEN
           do i=1,nk_hist
              status = 0
              call ftphis(ounit,hist(i),status)
              errinfo = errstr//' writing history'
              call wt_ferrmsg(status,errinfo)
           enddo
        ENDIF
        exhist = 'EXTENSION WRITTEN BY WTPCMKF Ver '
     &//version
        status = 0
        call ftphis(ounit,exhist,status)
        call wt_ferrmsg(status,errinfo)
        IF (nk_comm.GT.0) THEN
           do i=1,nk_comm
              status = 0
              call ftpcom(ounit,comms(i),status)
              errinfo = errstr//' writing comments'
              call wt_ferrmsg(status,errinfo)
           enddo
        ENDIF

        IF (chatter.GE.20) THEN
          subinfo = '   ... keywords have been written'
          call fcecho(subinfo)
        ENDIF

c
c --- DEFINE EXTENSION DATA STRUCTURE ---
c
        call ftbdef(ounit,nfields,tform,var,n_mkf,status)
        errinfo = errstr//' defining data structure'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 5
          return
        ENDIF
c
c       --- WRITE THE DATA ---
c

c TIME

        frow = 1
        felem = 1
        status = 0
        col = 1
        call ftpcld(ounit,col,frow,felem,n_mkf,time,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing Time Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c MAG_ANG

        frow = 1
        felem = 1
        status = 0
        col = 2
        call ftpcld(ounit,col,frow,felem,n_mkf,mag_ang,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing MAG_ANG Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c LVAL

        status = 0
        col = 3
        call ftpcld(ounit,col,frow,felem,n_mkf,lval,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing RL Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c ZEN_ANG

        status = 0
        col = 4
        call ftpcld(ounit,col,frow,felem,n_mkf,zen_ang,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing ZEN_ANG Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c SES_ANG

        status = 0
        col = 5
        call ftpcld(ounit,col,frow,felem,n_mkf,ses_ang,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SES_ANG Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c MOON_ANG

        status = 0
        col = 6
        call ftpcld(ounit,col,frow,felem,n_mkf,moon_ang,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing MOON_ANG Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c SUN_ANG

        status = 0
        col = 7
        call ftpcld(ounit,col,frow,felem,n_mkf,sun_ang,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SUN_ANG Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c SAT_LON

        status = 0
        col = 8
        call ftpcld(ounit,col,frow,felem,n_mkf,sat_lon,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SAT_LON Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c SAT_LAT

        status = 0
        col = 9
        call ftpcld(ounit,col,frow,felem,n_mkf,sat_lat,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SAT_LAT Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c VEL_ANG

        status = 0
        col = 10
        call ftpcld(ounit,col,frow,felem,n_mkf,vel_ang,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing VEL_ANG Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c RASAT

        status = 0
        col = 11
        call ftpcld(ounit,col,frow,felem,n_mkf,rasat,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing RASAT Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c DECSAT

        status = 0
        col = 12
        call ftpcld(ounit,col,frow,felem,n_mkf,decsat,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing DECSAT Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF


c ASP_QUAL

        status = 0
        col = 13
        call ftpcld(ounit,col,frow,felem,n_mkf,asp_qual,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing ASP_QUAL Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c STT_QUAL

        status = 0
        col = 14
        call ftpcld(ounit,col,frow,felem,n_mkf,stt_qual,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing STT_QUAL Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c STATUS

        status = 0
        col = 15
        call ftpclj(ounit,col,frow,felem,n_mkf,stat,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing STATUS Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c RA_SC

        status = 0
        col = 16
        call ftpcld(ounit,col,frow,felem,n_mkf,ra_sc,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing RA_SC Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c DEC_SC

        status = 0
        col = 17
        call ftpcld(ounit,col,frow,felem,n_mkf,dec_sc,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing DEC_SC Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c MISS

        status = 0
        col = 18
        do i=1,n_mkf
          row=hkp_rows(i)
          call ftpclj(ounit,col,i,felem,1,miss(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing MISS Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c TEMP

        status = 0
        col = 19 
        do i=1,n_mkf
          row = hkp_rows(i)
          call ftpclj(ounit,col,i,felem,1,temp(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing TEMP Data '
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c PRESS

        status = 0
        col = 20 
        do i=1,n_mkf
          row=hkp_rows(i)
          call ftpclj(ounit,col,i,felem,1,press(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing PRESS Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c HVOLT

        status = 0
        col = 21
        do i=1,n_mkf
          row=hkp_rows(i)
          call ftpclj(ounit,col,i,felem,1,hvolt(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing HVOLT Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c FILPOS

        status = 0
        col = 22
        do i=1,n_mkf
          row=hkp_rows(i)
          call ftpclj(ounit,col,i,felem,1,filpos(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing FILPOS Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c MV_ACO

        status = 0
        col = 23 
        call ftpcld(ounit,col,frow,felem,n_mkf,mv_aco,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing MV_ACO Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c XTRANSM

        status = 0
        col = 24 
        call ftpcld(ounit,col,frow,felem,n_mkf,xtransm,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing XTRANSM Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c A1_AL

        status = 0
        col = 25 
        call ftpcld(ounit,col,frow,felem,n_mkf,a1_al,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing A1_AL Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c SA_RATE

        status = 0
        col = 26 
        call ftpcld(ounit,col,frow,felem,n_mkf,sa_rate,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SA-RATE Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c XACC

        status = 0
        col = 27
        call ftpcld(ounit,col,frow,felem,n_mkf,xacc,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing XACC Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c A1AL_MV

        status = 0
        col = 28
        call ftpcld(ounit,col,frow,felem,n_mkf,a1al_mv,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing A1AL_MV Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c A1_AH

        status = 0
        col = 29
        call ftpcld(ounit,col,frow,felem,n_mkf,a1_ah,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing A1_AH Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c AP

        status = 0
        col = 30
        call ftpcld(ounit,col,frow,felem,n_mkf,ap,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing AP Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF


c HV_STA

        status = 0
        col = 31 
        do i=1,n_mkf
          row = hksta_rows(i)
          call ftpclj(ounit,col,i,felem,1,hv_sta(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing HV_STA Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c CARR_STA

        status = 0
        col = 32 
        do i=1,n_mkf
          row = hksta_rows(i)
          call ftpclj(ounit,col,i,felem,1,carr_sta(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing CARR_STA Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c GAS_STA

        status = 0
        col = 33 
        do i=1,n_mkf
          row = hksta_rows(i)
          call ftpclj(ounit,col,i,felem,1,det_sta(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing DET_STA Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c DET_STA

        status = 0
        col = 34 
        do i=1,n_mkf
          row = hksta_rows(i)
          call ftpclj(ounit,col,i,felem,1,det_sta(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing DET_STA Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c TEMP_STA

        status = 0
        col = 35 
        do i=1,n_mkf
          row = hksta_rows(i)
          call ftpclj(ounit,col,i,felem,1,temp_sta(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing TEMP_STA Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c FILT_STA

        status = 0
        col = 36 
        do i=1,n_mkf
           row = hksta_rows(i)
          call ftpclj(ounit,col,i,felem,1,filt_sta(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing LV_STA Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c LV_STA

        status = 0
        col = 37
        do i=1,n_mkf
          row = hksta_rows(i)
          call ftpclj(ounit,col,i,felem,1,lv_sta(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing LV_STA Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo 

c INST_STA

        status = 0
        col = 38 
        do i=1,n_mkf
         row = hksta_rows(i)
         call ftpclj(ounit,col,i,felem,1,inst_sta(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing INST_STA Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c TEL_STA

        status = 0
        col = 39 
        do i=1,n_mkf
          row = hksta_rows(i)
          call ftpclj(ounit,col,i,felem,1,tel_sta(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing TEL_STA Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c CURR_STA

        status = 0
        col = 40
        do i=1,n_mkf
          row = hksta_rows(i)
          call ftpclj(ounit,col,i,felem,1,cur_sta(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing TEL_STA Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo


c OBI_NUM

        status = 0
        col = 41 
        do i=1,n_mkf
          row = hksta_rows(i)
          call ftpclj(ounit,col,i,felem,1,obi_num(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing OBI_NUM Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c DEADT

        status = 0
        col = 42 
        call ftpcle(ounit,col,frow,felem,n_mkf,deadt,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing INST_STA Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

        IF ((chatter.GE.20).AND.(ierr.EQ.0)) THEN
          subinfo = '   ...Data has been written'
          call fcecho(subinfo)
        ENDIF

        IF ((chatter.GE.20).AND.(ierr.EQ.0)) THEN
          subinfo = ' ... MKF extension successfully written'
          call fcecho(subinfo)
        ENDIF
  100   FORMAT(I6)
        return
        end                      
c ----------------------------------------------------------------------
c       END OF WTPCMKF
c ----------------------------------------------------------------------

