
*+WTHRIMKF
c    ------------------------------------------------------------
      subroutine wthrimkf(ounit,telescope,instrume,detnam,extname,
     &             time,mjdref,hduclas3,n_mkf,sun_x,sun_y,sun_z,
     &             moon_x,moon_y,moon_z,lon_east,lat_nort,
     &             sat_x,sat_y,sat_z,gha,alt_sat,ses_ang,est_ang,
     &             obi_num,asp_qual,stt_qual,roan_sc,ra_sc,dec_sc,
     &             accepted,livt_cor,max_hkp,temp1,temp2,temp3,
     &             hkp_rows,asp_rows,nk_hist,hist,nk_comm,comms,
     &             ierr,chatter) 
c    ------------------------------------------------------------
c
c ___ DESCRIPTION _______________________________________________________ 
c                                                                         
c This routine writes a FITS MAKEFILTER extension in OGIP format for
c ROSAT HRI
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
      real*8 time(*),mjdref
      real*8 sun_x(*),sun_y(*),sun_z(*)
      real*8 moon_x(*),moon_y(*),moon_z(*)
      real*8 lon_east(*),lat_nort(*)
      integer sat_x(*),sat_y(*),sat_z(*)
      integer*2 obi_num(*),asp_qual(*),stt_qual(*)
      real*8 roan_sc(*),ra_sc(*),dec_sc(*)
      real*8 gha(*),alt_sat(*)
      real ses_ang(*),est_ang(*)

      real accepted(*),livt_cor(*)

      integer max_hkp
      real temp1(*),temp2(*),temp3(*)
      integer hkp_rows(*),asp_rows(*) 
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
c Rehana Yusaf (1994 OCT) 1.0.0;
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
      parameter (tfields=46)
      character(16) ttype(tfields),tform(tfields),tunit(tfields)
      character(40) tcomm(tfields)
      character(8) keywrd

c       --- USER INFO ---
c
        subinfo = ' ... using WTHRIMKF Ver '//version
        IF (chatter.GE.15) THEN
          call fcecho(subinfo)
        ENDIF           
c
c       --- CREATE A NEW EXTENSION ---
c
        status = 0 
        errstr = 'ERROR : WTHRIMKF Ver '//version//':'
        wrnstr = 'WARNING : WTHRIMKF Ver '//version//':'
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
        ttype(nfields) = 'SUN_X'
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'unit vector in ECF-frame, X component'

        nfields = nfields + 1
        ttype(nfields) = 'SUN_Y'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'         
        tcomm(nfields) = 'unit vector in ECF-frame, Y component'

        nfields = nfields + 1
        ttype(nfields) = 'SUN_Z'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'unit vector in ECF-frame, Z component'

        nfields = nfields + 1
        ttype(nfields) = 'MOON_X'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Moon unit vector in ECF-frame, X component'

        nfields = nfields + 1
        ttype(nfields) = 'MOON_Y'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Moon unit vector in ECF-frame, Y component'

        nfields = nfields + 1
        ttype(nfields) = 'MOON_Z'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Moon unit vector in ECF-frame, Z component' 

        nfields = nfields + 1
        ttype(nfields) = 'SAT_X'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Satellite position vector ECF-frame,'
     &//' X component'

        nfields = nfields + 1
        ttype(nfields) = 'SAT_Y'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Satellite position vector ECF-frame,'
     &//' Y component'

        nfields = nfields + 1
        ttype(nfields) = 'SAT_Z'        
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Satellite position vector ECF-frame,'
     &//' Z component'

        nfields = nfields + 1
        ttype(nfields) = 'LON_EAST'       
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Geodetic longitude east in ECF-frame '
     &//'of reference'

        nfields = nfields + 1
        ttype(nfields) = 'LAT_NORT'       
        tform(nfields) = '1D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Geodetic latitude north in ECF-frame'

        nfields = nfields + 1
        ttype(nfields) = 'ALT_SAT'
        tform(nfields) = 'D'
        tunit(nfields) = 'm'
        tcomm(nfields) = 'Satellite altitude above ellipsoid'

        nfields = nfields + 1
        ttype(nfields) = 'GHA'        
        tform(nfields) = 'D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Greenwich Hour Angle'

        nfields = nfields + 1
        ttype(nfields) = 'SES_ANG'
        tform(nfields) = 'E'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Sun-Earth_Satellite Angle'

        nfields = nfields + 1
        ttype(nfields) = 'EST_ANG'        
        tform(nfields) = '1E'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Earth-Sat-Target Angle'

        nfields = nfields + 1
        ttype(nfields) = 'OBI_NUM'
        tform(nfields) = '1I'
        tunit(nfields) = 'NONE'
        tcomm(nfields) = 'OBI Number'

        nfields = nfields + 1
        ttype(nfields) = 'ASP_QUAL'        
        tform(nfields) = '1I'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Aspect quality'

        nfields = nfields + 1
        ttype(nfields) = 'STT_QUAL'        
        tform(nfields) = 'I'
        tunit(nfields) = 'CODED'
        tcomm(nfields) = 'Star Tracker flag'

        nfields = nfields + 1
        ttype(nfields) = 'RA_SC'        
        tform(nfields) = 'D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'RA of SC pointing'

        nfields = nfields + 1
        ttype(nfields) = 'DEC_SC'        
        tform(nfields) = 'D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'DEC of SC pointing'

        nfields = nfields + 1
        ttype(nfields) = 'ROAN_SC'        
        tform(nfields) = 'D'
        tunit(nfields) = 'deg'
        tcomm(nfields) = 'Roll angle of SC pointing'

        nfields = nfields + 1
        ttype(nfields) = 'TEMP1'
        tform(nfields) = '1E'
        tunit(nfields) = 'DEG_C'
        tcomm(nfields) = 'Temperature 1'

        nfields = nfields + 1
        ttype(nfields) = 'TEMP2'        
        tform(nfields) = '1E'
        tunit(nfields) = 'DEG_C'
        tcomm(nfields) = 'Temperature 2'

        nfields = nfields + 1
        ttype(nfields) = 'TEMP3'        
        tform(nfields) = '1E'
        tunit(nfields) = 'DEG_C'
        tcomm(nfields) = 'Temperature 3'

        nfields = nfields + 1
        ttype(nfields) = 'ACCEPTED'        
        tform(nfields) = '1E'
        tunit(nfields) = 'count'
        tcomm(nfields) = 'Accepted X-ray rate'

        nfields = nfields + 1
        ttype(nfields) = 'LIVT_COR'        
        tform(nfields) = '1E'
        tunit(nfields) = 'NONE'
        tcomm(nfields) = 'Live-time correction factor'

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
          IF (i.LT.10) THEN
           write(keywrd,'(a5,i1)') 'TTYPE',i
          ELSE
           write(keywrd,'(a5,i2)') 'TTYPE',i
          ENDIF
          call crmvlbk(keywrd)
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

c SUN_X 

        frow = 1
        felem = 1
        status = 0
        col = 2
        call ftpcld(ounit,col,frow,felem,n_mkf,sun_x,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SUN_X Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c SUN_Y 

        status = 0
        col = 3
        call ftpcld(ounit,col,frow,felem,n_mkf,sun_y,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SUN_Y Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c SUN_Z 

        status = 0
        col = 4
        call ftpcld(ounit,col,frow,felem,n_mkf,sun_z,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SUN_Z Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c MOON_X 

        status = 0
        col = 5
        call ftpcld(ounit,col,frow,felem,n_mkf,moon_x,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing MOON_X Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c MOON_Y

        status = 0
        col = 6
        call ftpcld(ounit,col,frow,felem,n_mkf,moon_y,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing MOON_Y Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c MOON_Z 

        status = 0
        col = 7
        call ftpcld(ounit,col,frow,felem,n_mkf,moon_z,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing MOON_Z Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c SAT_X 

        status = 0
        col = 8
        call ftpclj(ounit,col,frow,felem,n_mkf,sat_x,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SAT_X Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c SAT_Y

        status = 0
        col = 9
        call ftpclj(ounit,col,frow,felem,n_mkf,sat_y,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SAT_Y Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c SAT_Z

        status = 0
        col = 10
        call ftpclj(ounit,col,frow,felem,n_mkf,sat_z,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SAT_Z Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c LON_EAST

        status = 0
        col = 11
        call ftpcld(ounit,col,frow,felem,n_mkf,lon_east,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing LON_EAST Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c LAT_NORT

        status = 0
        col = 12
        call ftpcld(ounit,col,frow,felem,n_mkf,lat_nort,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing LAT_NORT Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c ALT_SAT

        status = 0
        col = 13
        call ftpcld(ounit,col,frow,felem,n_mkf,alt_sat,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing ALT_SAT Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF


c GHA

        status = 0
        col = 14
        call ftpcld(ounit,col,frow,felem,n_mkf,gha,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing GHA Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c SES_ANG

        status = 0
        col = 15
        call ftpcle(ounit,col,frow,felem,n_mkf,ses_ang,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing SES_ANG Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c EST_ANG

        status = 0
        col = 16
        call ftpcle(ounit,col,frow,felem,n_mkf,est_ang,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing EST_ANG Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c OBI_NUM

        status = 0
        col = 17
        call ftpcli(ounit,col,frow,felem,n_mkf,obi_num,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing EST_ANG Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c ASP_QUAL 

        status = 0
        col = 18
        row = asp_qual(1)
        do i=1,n_mkf
          row = asp_rows(i)
          call ftpcli(ounit,col,i,felem,1,
     &                asp_qual(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing ASP_QUAL Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c STT_QUAL

        status = 0
        col = 19
        do i=1,n_mkf
          row = asp_rows(i)
          call ftpcli(ounit,col,i,felem,1,
     &                stt_qual(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing DEC_SC Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c RA_SC

        status = 0
        col = 20
        do i=1,n_mkf
          row = asp_rows(i)
          call ftpcld(ounit,col,i,felem,1,
     &               ra_sc(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing RA_SC Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c DEC_SC 

        status = 0
        col = 21
        do i=1,n_mkf
          row = asp_rows(i)
          call ftpcld(ounit,col,i,felem,1,
     &                dec_sc(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing DEC_SC Data '
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c ROAN_SC

        status = 0
        col = 22 
        do i=1,n_mkf
          row = asp_rows(i)
          call ftpcld(ounit,col,i,felem,1,
     &                roan_sc(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing ROAN_SC Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c TEMP1

        status = 0
        col = 23
        do i=1,n_mkf
          row=hkp_rows(i)
          call ftpcle(ounit,col,i,felem,1,temp1(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing TEMP1 Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c TEMP2

        status = 0
        col = 24
        do i=1,n_mkf
          row=hkp_rows(i)
          call ftpcle(ounit,col,i,felem,1,temp2(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing TEMP2 Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c TEMP3

        status = 0
        col = 25 
        do i=1,n_mkf
          row=hkp_rows(i)
          call ftpcle(ounit,col,i,felem,1,temp3(row),status)
          IF (status.NE.0) THEN
            subinfo = errstr//' Writing TEMP3 Data'
            call wt_ferrmsg(status,subinfo)
            ierr = 2
            return
          ENDIF
        enddo

c ACCEPTED

        status = 0
        col = 26 
        call ftpcle(ounit,col,frow,felem,n_mkf,accepted,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing ACCEPTED Data'
          call wt_ferrmsg(status,subinfo)
          ierr = 2
          return
        ENDIF

c LIVT_COR

        status = 0
        col = 27 
        call ftpcle(ounit,col,frow,felem,n_mkf,livt_cor,status)
        IF (status.NE.0) THEN
          subinfo = errstr//' Writing LIVT_COR Data'
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

