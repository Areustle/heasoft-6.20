
*+WTRPF1
c    ------------------------------------------------------------
      subroutine wtrpf1(ounit,extname,hduclas3,nrad,rad_lo,rad_hi,
     &                   radunit,ntheta,theta_lo,theta_hi,thetaunit,    
     &                   nenerg,energ_lo,energ_hi,energunit,    
     &                   rpsf,qerror,rpsf_err,rpsfunit,qarea,
     &                   area_wgt,hist,nk_hist,comms,     
     &                   nk_comm,telescop,instrume,maxrad,maxtheta,
     &                   ierr,chatter)             
c    ------------------------------------------------------------
c
c ___ DESCRIPTION _______________________________________________________ 
c                                                                         
c This routine writes a FITS extension in OGIP RPSF HDUVERS1 1.0.0 format:
c
c HDUCLASS = OGIP 
c HDUCLAS1 = RESPONSE
c HDUCLAS2 = RPRF
c HDUCLAS3 = PREDICTED 
c            TOTAL
c            NET     (background = 0)
c HDUVERS1 = 1.0.0
c HDUVERS2 = 1.0.1
c
c This routine can write one or more radial profile, at one or more
c theta, and energy values. The data is written as vectors for conciseness
c                                 
c NOTE : Assumes FITS file is open and has had the primary header written.
c        File is left open at end ... use FTCLOS to close file            
c                                 ... or FTCRHD to add another extension. 
c Columns written are ...                                                 
c                                                                         
c RAD_LO      : Lower edge of radial bins                        
c RAD_HI      : Upper edge of radial bins                       
c THETA_LO    : Lower bound for off_axis angle, if single value, it is
c               written as keyword                 
c               If value is not defined pass -99
c THETA_HI    : Upper bound for off_axis angle, if single value, it is
c               written as keyword              
c               If value is not defined pass -99                              
c ENERG_LO    : Low energy bound, if single value, it is
c               written as keyword                                 
c               If value is not defined pass -99
c ENERG_HI    : High energy bound,  if single value, it is
c               written as keyword              
c               If value is not defined pass -99                   
c RPSF        : Radial Point Spread function,
c               3D array-RPSF(irad,itheta,ien) 
c RPSF_ERR    : Error on RPSF, 3D array - RPSF_ERR(irad,itheta,ien)
c               if there is a consistent error it is written as keyword
c               if this is not supplied pass qerror as false
c               default value is 0.
c AREA_WGT    : Area weighting factor, 3D - AREA_WGT(irad,itheta,ien)
c               if this is not supplied pass qarea as false.
c               Default value is 1.0
c 
c Keywords are ...
c
c RPSFVER     : Routine version
c TELESCOP    : Mission/Telescope name (if not known, pass 'UNKNOWN')
c INSTRUME    : Instrument/Detector name (if not known, pass 'UNKNOWN')
c HDUCLASS    : HDU 'family' keywords 
c DATE
c
c History comments are written if passed to routine
c Comments are written if passed to routine
c
c ___ VARIABLES _________________________________________________________
c
      IMPLICIT NONE
      integer nrad,nenerg,ntheta,ounit,ierr,maxrad,maxtheta
      character*(*) radunit,thetaunit,energunit,rpsfunit
      character*(*) telescop,instrume,extname,hduclas3
      real rad_lo(maxrad),rad_hi(maxrad),area_wgt(maxrad,maxtheta,*)
      real theta_lo(maxtheta), theta_hi(maxtheta)
      real energ_lo(*),energ_hi(*)
      real rpsf(maxrad,maxtheta,*),rpsf_err(maxrad,maxtheta,*)
      integer nk_hist,chatter,nk_comm
      character(80) hist(*),comms(*)
      logical qarea,qerror
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c maxrad     int    : Array dimensions for radial profile data
c maxtheta   int    : Array dimensions for theta data
c rad_lo     real   : Array of lower edge of radial bins 
c rad_hi     real   : Array of upper edge of radial bins 
c theta_lo   real   : Array of lower theta bounds for radial profiles
c theta_hi   real   : Array of upper theta bounds for radial profiles
c energ_lo   real   : Array of lower energy bound
c energ_hi   real   : Array of upper energy bound
c radunit    char   : radial bins unit
c thetaunit  char   : Unit for theta angles
c energunit  char   : Unit for energy
c qarea      logical: False if area_wgt not passed
c qerror     logical: False if rpsf_err not passed
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
c Rehana Yusaf (1993 September) 1.0.0;
c Rehana Yusaf (1994 Jan 14) 1.0.1; Add HDUCLASS/HDUVERS keywords
        character(5) version
        parameter ( version = '1.0.1' )
*-
c _________________________________________________________________________
c
c
c --- INTERNAL VARIABLES ---
c
       integer status,nfields,i,sum1,nrows,k,nrpsf,charerr
       integer frow,felem,colnum,var,irad,itheta,ienerg,ncols
       real values(4096),key_val,farea
       integer fcstln, nrad_len,nth_len,nen_len,nrpsf_len
       parameter (ncols = 10)
       character(5) oclass
       character(16) ttype(ncols),tform(ncols)
       character(16) tdim(ncols),tunits(ncols)
       character(30) errstr,wrnstr
       character(70) subinfo,errinfo,exhist
       character(40) comm          
       character(6) charnum,ch_nrad,ch_nth,ch_nen
       character(8) unitform,dimkey
c
c Internals ...
c
c status     int    : Error flag
c nfields    int    : No. of data columns
c frow       int    : Beginning row number for writing columns
c felem      int    : beginning element number for writing columns
c colnum     int    : Column No.
c var        int    : size in bytes of the 'variable length data area'
c ttype      char   : Array of type of data (dimension nfields)
c tform      char   : Array of data format,real,int etc (dimension nfields)
c tunits     char   : Array of data units (dimension nfields)
c errstr     char   : Error string for this routine
c version    int    : Subroutine version
c oclass     int    : OGIP format keyword        
c
c
c       --- USER INFO ---
c
        subinfo = ' ... using WTRPF1 Ver '//version
        IF (chatter.GE.15) THEN
          call fcecho(subinfo)
        ENDIF           
c
c       --- CREATE A NEW EXTENSION ---
c
        status = 0 
        errstr = 'ERROR : WTRPF1 Ver '//version//':'
        wrnstr = 'WARNING : WTRPF1 Ver '//version//':'
        oclass = '1993a'
        call ftcrhd (ounit,status)
        errinfo = errstr//' creating new extension !'
        call wt_ferrmsg (status,errinfo)
        IF (status.NE.0) THEN
          ierr = 1
          return
        ENDIF
c
c       --- SET UP COLUMNS ---
c

c RAD_LO SETUP ...

        charerr = 0
        write(ch_nrad,100,IOSTAT=charerr) nrad
        IF (charerr.NE.0) THEN
          errinfo = errstr//' writing TFORM for RAD_LO'
          call fcecho(errinfo)
          ierr = 4
          return
        ENDIF
        call crmvlbk(ch_nrad)
        nrad_len = fcstln(ch_nrad)
        unitform = ch_nrad(1:nrad_len)//'E'
        tdim(1) = '('//ch_nrad(1:nrad_len)//')'
        ttype(1) = 'RAD_LO'
        tform(1) = unitform
        tunits(1)= radunit

c RAD_HI SETUP ...

        ttype(2) = 'RAD_HI'
        tform(2) = unitform
        tunits(2)= radunit
        tdim(2) = tdim(1)
        nfields = 2

c THETA_LO SETUP ...

          charerr = 0
          write(ch_nth,100,IOSTAT=charerr) ntheta
          IF (charerr.NE.0) THEN
            errinfo = errstr//' writing TFORM/TDIM for THETA_LO'
            call fcecho(errinfo)
            ierr = 4
            return
          ENDIF
          call crmvlbk(ch_nth)
          nth_len = fcstln(ch_nth)
          unitform = ch_nth(1:nth_len)//'E'  
        IF (ntheta.GT.1) THEN
          nfields = nfields + 1
          tdim(nfields) = '('//ch_nth(1:nth_len)//')'
          ttype(nfields) = 'THETA_LO'
          tform(nfields) = unitform
          tunits(nfields)= thetaunit

c THETA_HI SETUP ...

          nfields = nfields + 1
          ttype(nfields) = 'THETA_HI'
          tform(nfields) = unitform
          tunits(nfields)= thetaunit
          tdim(nfields) = tdim(nfields - 1)
        ENDIF

c ENERG_LO SETUP ...

          charerr = 0
          write(ch_nen,100,IOSTAT=charerr) nenerg
          IF (charerr.NE.0) THEN
            errinfo = errstr//' writing TFORM for ENERG_LO'
            call fcecho(errinfo)
            ierr = 4
            return
          ENDIF
          call crmvlbk(ch_nen)
          nen_len = fcstln(ch_nen)
          unitform = ch_nen(1:nen_len)//'E' 
        IF (nenerg.GT.1) THEN
          nfields = nfields + 1
          unitform = ch_nen(1:nen_len)//'E'
          tdim(nfields) = '('//ch_nen(1:nen_len)//')'
          ttype(nfields) = 'ENERG_LO'
          tform(nfields) = unitform
          tunits(nfields)= energunit

c ENERG_HI SETUP ...

          nfields = nfields + 1
          ttype(nfields) = 'ENERG_HI'
          tform(nfields) = unitform
          tunits(nfields)= energunit
          tdim(nfields) = tdim(nfields-1)
        ENDIF

c RPSF SETUP ...

        nfields = nfields + 1      
        nrpsf = nrad * ntheta * nenerg
        charerr = 0
        write(charnum,100,IOSTAT=charerr) nrpsf
        IF (charerr.NE.0) THEN
          errinfo = errstr//' writing TFORM for RPSF'
          call fcecho(errinfo)
          ierr = 4
          return
        ENDIF
        call crmvlbk(charnum)
        tdim(nfields)='('//ch_nrad(1:nrad_len)//','
     &//ch_nth(1:nth_len)//','//ch_nen(1:nen_len)//')'
        nrpsf_len = fcstln(charnum)
        unitform = charnum(1:nrpsf_len)//'E'
        ttype(nfields) = 'RPSF'
        tform(nfields) = unitform
        tunits(nfields)= rpsfunit

c RPSF_ERR SETUP ...

        nfields = nfields + 1
        ttype(nfields) = 'RPSF_ERR'
        tform(nfields) = unitform
        tunits(nfields)= rpsfunit
        tdim(nfields) = tdim(nfields-1)
        
c CHECK THAT ALL AREA WEGHTING FACTORS ARE NOT THE SAME - IF SO WRITE AS KEYWORD
     
        farea = 1.0
        sum1= 1
        IF (qarea) THEN
          farea = area_wgt(1,1,1)
          do irad=2,nrad
            do itheta=1,ntheta
              do ienerg=1,nenerg
                IF (area_wgt(irad,itheta,ienerg).EQ.farea) THEN
                  sum1 = sum1+1
                ENDIF
              enddo
            enddo
          enddo
        ENDIF
        nrpsf = nrad * ntheta * nenerg
        IF (sum1.EQ.nrpsf) THEN
          qarea = .false.
        ENDIF

        IF (qarea) THEN
          nfields = nfields + 1
          charerr = 0
          write(charnum,100,IOSTAT=charerr) nrpsf
          IF (charerr.NE.0) THEN
            errinfo = errstr//' writing TFORM for AREA_WGT'
            call fcecho(errinfo)
            ierr = 4
            return
          ENDIF
          call crmvlbk(charnum)
          i = index(charnum,' ')
          IF (i.NE.0) THEN
            unitform = charnum(1:i-1)//'E'
          ELSE
            unitform = charnum//'E'
          ENDIF       
          ttype(nfields) = 'AREA_WGT'
          tform(nfields) = unitform
          tunits(nfields)= ' '
          tdim(nfields)=tdim(nfields-1)
        ENDIF
          
c
c       --- WRITING REQUIRED KEYWORDS ---
c

c EXTENSION NAME

        status = 0
        nrows = 1
        call ftphbn(ounit,nrows,nfields,ttype,tform,tunits,
     &             extname,0,status)
        errinfo = errstr//' creating binary header'
        call wt_ferrmsg (status,errinfo)
        IF (status.NE.0) THEN
          ierr = 2
          return
        ENDIF

c TDIM - COLUMN DIMENSIONS 

        comm = ' Column dimension'
        do i=1,nfields
          write(charnum,100,IOSTAT=charerr) i
          call crmvlbk(charnum)
          dimkey = 'TDIM'//charnum
          status = 0
          call ftpkys(ounit,dimkey,tdim(i),comm,status)
          errinfo = errstr//' writing TDIM keyword'
          call wt_ferrmsg(status,errinfo)
        enddo

c TELESCOPE NAME

        status = 0
        comm = 'Name of Mission/Telescope'
        call ftpkys(ounit,'TELESCOP',telescop,comm,status)
        errinfo = errstr//' writing TELESCOPE keyword'
        call wt_ferrmsg(status,errinfo)

c INSTRUMENT NAME

        status = 0
        comm = 'Name of Instrument/Detector'
        call ftpkys(ounit,'INSTRUME',instrume,comm,status)
        errinfo = errstr//' writing INSTRUMENT keyword'
        call wt_ferrmsg(status,errinfo)

c HDUCLASS and HDUVERS

        status = 0
        call ftpkys(ounit,'HDUCLASS','OGIP',
     &'format conforms to OGIP standard',status)
        errinfo = errstr//' Problem writing HDUCLASS keyword'
        call wt_ferrmsg(status,errinfo)

        status = 0
        call ftpkys(ounit,'HDUCLAS1','RESPONSE',
     &'dataset is a response function',status)
        errinfo = errstr//' problem writing HDUCLAS1 keyword'
        call wt_ferrmsg(status,errinfo)

        status = 0
        call ftpkys(ounit,'HDUCLAS2','RPRF',
     &'dataset is a radial point response function',status)
        errinfo = errstr//' problem writing HDUCLAS2 keyword'
        call wt_ferrmsg(status,errinfo)

        IF (hduclas3.EQ.'PREDICTED') THEN
          comm = 'Predicted (theoretical) dataset'
        ELSEIF (hduclas3.EQ.'TOTAL') THEN
          comm = 'Total radial point spread function'
        ELSEIF (hduclas3.EQ.'NET') THEN
          comm = 'Source only radial point spread function'
        ELSE
          comm = 'WARNING This is NOT an OGIP-approved value'
        ENDIF

        status = 0
        call ftpkys(ounit,'HDUCLAS3',hduclas3,
     &comm,status)
        errinfo = errstr//' problem writing HDUCLAS2 keyword'
        call wt_ferrmsg(status,errinfo)

        status = 0
        call ftpkys(ounit,'HDUVERS1','1.0.0',
     &'Version of family of formats',status)
        errinfo = errstr//' writing HDUVERS1 keyword'
        call wt_ferrmsg(status,errinfo)
 
        status = 0
        call ftpkys(ounit,'HDUVERS2','1.0.1',
     &'Version of format',status)
        errinfo = errstr//' writing HDUVERS2 keyword'
        call wt_ferrmsg(status,errinfo)
 
c OGIP VERSION

        status = 0
        comm = 'OGIP FITS format version'
        call ftpkys(ounit,'RPSFVER',oclass,comm,status)
        errinfo = errstr//' writing FITS version keyword'
        call wt_ferrmsg(status,errinfo)

c IF NTHETA = 1 WRITE THETA_LO & THETA_HI AS KEYWORDS ...

        IF (ntheta.EQ.1) THEN
          IF (theta_lo(ntheta).EQ.(-99)) THEN
            comm = 'Value not defined'
          ELSE
            comm = 'Lower bound for off_axis angle (deg)'
          ENDIF

c IF NTHETA = 1 , DEFINED (NOT -99) THEN CONVERT TO STANDARD UNIT, deg
c THIS WILL LATER BE REPLACED WITH A UNIT CONVERTER

          IF (theta_lo(ntheta).NE.(-99)) THEN
            IF (thetaunit(1:6).EQ.'arcmin') THEN
              theta_lo(ntheta) = theta_lo(ntheta) * 60
            ELSEIF (thetaunit(1:6).EQ.'arcsec') THEN
              theta_lo(ntheta) = theta_lo(ntheta) * 60 * 60
            ENDIF
          ENDIF
          status = 0
          call ftpkye(ounit,'THETA_LO',theta_lo(ntheta),8,comm,status)     
          errinfo = errstr//' writing THETA_LO as keyword'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 4
            return
          ENDIF

c IF NTHETA = 1 , DEFINED (NOT -99) THEN CONVERT TO STANDARD UNIT, deg
c THIS WILL LATER BE REPLACED WITH A UNIT CONVERTER

          IF (theta_hi(ntheta).NE.(-99)) THEN
            IF (thetaunit(1:6).EQ.'arcmin') THEN
              theta_hi(ntheta) = theta_hi(ntheta) * 60
            ELSEIF (thetaunit(1:6).EQ.'arcsec') THEN
              theta_hi(ntheta) = theta_hi(ntheta) * 60 * 60
            ENDIF        
          ENDIF 
          IF (theta_hi(ntheta).EQ.(-99)) THEN
            comm = 'Value not defined'
          ELSE
            comm = 'Upper bound for off_axis angle (deg)'
          ENDIF
          status = 0
          call ftpkye(ounit,'THETA_HI',theta_hi(ntheta),8,comm,status)
          errinfo = errstr//' writing THETA_HI as keyword'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 4
            return
          ENDIF        
        ENDIF

c IF NENERG = 1 WRITE ENERG_LO & ENERG_HI AS KEYWORDS ...

        IF (nenerg.EQ.1) THEN
          IF (energ_lo(nenerg).EQ.(-99)) THEN
            comm = 'Value not defined'
          ELSE
            comm = 'Low energy bound (KeV)'
          ENDIF

c IF NENERG = 1 , DEFINED (NOT -99) THEN CONVERT TO STANDARD UNIT, KeV
c THIS WILL LATER BE REPLACED WITH A UNIT CONVERTER

          IF (energ_lo(nenerg).NE.(-99)) THEN
            IF (energunit(1:2).EQ.'eV') THEN
              energ_lo(nenerg) = energ_lo(nenerg) * 100
            ENDIF
          ENDIF            
          status = 0
          call ftpkye(ounit,'ENERG_LO',energ_lo(nenerg),8,comm,status)
          errinfo = errstr//' writing ENERG_LO as keyword'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 4
            return
          ENDIF
          IF (energ_hi(nenerg).EQ.(-99)) THEN
            comm = 'Value not defined'
          ELSE        
            comm = 'High energy bound '
          ENDIF

c IF NENERG = 1 , DEFINED (NOT -99) THEN CONVERT TO STANDARD UNIT, KeV
c THIS WILL LATER BE REPLACED WITH A UNIT CONVERTER

          IF (energ_hi(nenerg).NE.(-99)) THEN
            IF (energunit(1:2).EQ.'eV') THEN
              energ_hi(nenerg) = energ_hi(nenerg) * 100
            ENDIF
          ENDIF        
          status = 0
          call ftpkye(ounit,'ENERG_HI',energ_hi(nenerg),8,comm,status)
          errinfo = errstr//' writing ENERG_HI as keyword'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 4
            return
          ENDIF  
        ENDIF

c WRITE AREA_WGT AS KEYWORD - IF NOT COLUMN  

        IF (.NOT.qarea) THEN
          key_val = farea
          call ftpkye(ounit,'AREA_WGT',key_val,8,
     &                'Area Weighting Factor',status)
          errinfo = errstr//' writing AREA_WGT keyword'
          call wt_ferrmsg(status,errinfo)
        ENDIF

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
        exhist = 'EXTENSION WRITTEN BY WTRPF1 Ver '
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
c       --- DEFINE EXTENSION DATA STRUCTURE ---
c
        status = 0
        var = 0
        call ftbdef(ounit,nfields,tform,var,nrows,status)
        errinfo = errstr//' defining data'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 3
          return
        ENDIF
c
c       --- WRITE THE DATA ---
c
        frow = 1
        felem = 1

c RAD_LO DATA ...

        status = 0
        colnum = 1
        call ftpcle(ounit,colnum,frow,felem,nrad,rad_lo,status)
        errinfo = errstr//' writing RAD_LO data'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 4
          return
        ENDIF

c RAD_HI DATA ...

        status = 0     
        colnum = colnum + 1
        call ftpcle(ounit,colnum,frow,felem,nrad,rad_hi,status)
        errinfo = errstr//' writing RAD_HI data'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 4
          return
        ENDIF

c THETA_LO ...

        IF (ntheta.GT.1) THEN
          status = 0
          colnum = colnum + 1
          call ftpcle(ounit,colnum,frow,felem,ntheta,theta_lo,status)
          errinfo = errstr//' writing THETA_LO data'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 4
           return
          ENDIF    

c THETA_HI ...

          status = 0
          colnum = colnum + 1
          call ftpcle(ounit,colnum,frow,felem,ntheta,theta_hi,status)
          errinfo = errstr//' writing THETA_HI data'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 4
            return
          ENDIF
        ENDIF      

c ENERG_LO ...

        IF (nenerg.GT.1) THEN
          status = 0
          colnum = colnum + 1
          call ftpcle(ounit,colnum,frow,felem,nenerg,energ_lo,status)
          errinfo = errstr//' writing ENERG_LO data'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 4
            return
          ENDIF

c ENERG_HI ...

        
          status = 0
          colnum = colnum + 1
          call ftpcle(ounit,colnum,frow,felem,nenerg,energ_hi,status)
          errinfo = errstr//' writing THETA_HI data'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 4
            return
          ENDIF
        ENDIF      

c RPSF ...

        status = 0
        colnum = colnum + 1
        k = 0
        do irad=1, nrad
          do itheta=1,ntheta
             do ienerg=1,nenerg
                k = k+1
                values(k) = rpsf(irad,itheta,ienerg)
              enddo
           enddo
        enddo
        call ftpcle(ounit,colnum,frow,felem,nrpsf,values,status)
        errinfo = errstr//' writing RPSF data'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 4
          return
        ENDIF      

c RPSF_ERR ...

        status = 0
        colnum = colnum + 1
        k = 0
        do irad=1, nrad
          do itheta=1,ntheta
             do ienerg=1,nenerg
                k = k+1
                values(k) = rpsf_err(irad,itheta,ienerg)
              enddo
           enddo
        enddo 
        call ftpcle(ounit,colnum,frow,felem,nrpsf,values,status)
        errinfo = errstr//' writing RPSF_ERR data'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 4
          return
        ENDIF            

c AREA_WGT ...

        IF (qarea) THEN
          status = 0
          colnum = colnum + 1
          k = 0
          do irad=1, nrad
            do itheta=1,ntheta
               do ienerg=1,nenerg
                  k = k+1
                  values(k) = area_wgt(irad,itheta,ienerg)
               enddo
            enddo
          enddo 
          call ftpcle(ounit,colnum,frow,felem,nrpsf,values,status)
          errinfo = errstr//' writing RPSF data'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 4
            return
          ENDIF            
        ENDIF

        IF (chatter.GE.20) THEN
          subinfo = '   ...Data has been written'
          call fcecho(subinfo)
        ENDIF

        IF (chatter.GE.20) THEN
          subinfo = ' ... RPSF extension written'
          call fcecho(subinfo)
        ENDIF
  100   FORMAT(I6)
        return
        end                      
c ----------------------------------------------------------------------
c       END OF WTRPF1
c ----------------------------------------------------------------------

