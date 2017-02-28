
*+RDRPF1
c     ---------------------------------------------------------------
      subroutine rdrpf1(iunit,hduclas3,nrad,rad_lo,rad_hi,
     &                        radunit,ntheta,theta_lo,theta_hi,
     &                        thetaunit,nenerg,energ_lo,energ_hi,
     &                        energunit,rpsf,qerror,rpsf_err,
     &                        rpsfunit,qarea,area_wgt,telescop,
     &                        instrume,maxrad,maxtheta,ierr,chatter)
c     ---------------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads an OGIP standard 
c HDUCLAS1 RESPONSE
c HDUCLAS2 RPRF
c HDUCLAS3 PREDICTED,TOTAL or NET (bkgd = 0) 
c extension from a FITS file. 
c NOTE : Assumes file is already open and in correct position.
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c RAD_LO    : Lower edge of radial bins 
c RAD_HI    : Upper edge of radial bins 
c THETA_LO  : Lower bound for off_axis angle, if it is a single value it
c             is read from a keyword. If the value is not defined it is -99
c THETA_HI  : Upper bound for off_axis angle, if it is a single value it
c             is read from a keyword. If the value is not defined it is -99 
c ENERG_LO  : Low energy bound, if read from a keyword it is a single value
c             if it is not defined, it is -99
c ENERG_HI  : Upper energy bound, if read from a keyword it is a single value
c             if it is not defined, it is -99     
c RPSF      : Radial Point Spread function,
c             3D array - RPSF(irad,itheta,ien)
c RPSF_ERR  : Error on RPSF, 3D array - RPSF_ERR(irad,itheta,ien)
c             this is an optional column, if it is not present then
c             qerror is set to false
c AREA_WGT  : Area weighting factor, 3D - AREA_WGT(irad,itheta,ien)
c             this is an optional column, and if it is not present
c             then qarea is set to false
c
c Keywords read ...
c
c RPSFVER   : Routine version
c TELESCOP  : Mission/Telescope name, NOTE: if not present set to UNKNOWN
c INSTRUME  : Instrument/Detector name, NOTE: if not present set to UNKNOWN
c HDUCLAS3  : OGIP approved values are ... PREDICTED
c                                          TOTAL
c                                          NET (bkgd = 0)
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,nrad,ntheta,nenerg,chatter
      integer ierr,maxrad,maxtheta
      real rad_lo(maxrad),rad_hi(maxrad)
      real energ_lo(*),energ_hi(*)
      real theta_lo(maxtheta),theta_hi(maxtheta)
      real rpsf(maxrad,maxtheta,*),rpsf_err(maxrad,maxtheta,*)
      character*(*) radunit,thetaunit,energunit,rpsfunit,hduclas3
      real area_wgt(maxrad,maxtheta,*)
      character*(*) instrume,telescop
      logical qarea,qerror
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c maxrad     int    : Array dimensions
c maxtheta   int    : Array dimensions
c iunit      int    : Fortran unit number for file
c chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c nrad       int    : Counter for observed psf data 
c rad_lo     real   : Array of lower edge of observed radial bin
c rad_hi     real   : Array of upper edge of observed radial bin
c theta_lo   real   : Array of lower theta values for off_axis angle
c theta_hi   real   : Array of upper theta values for off_axis angle
c energ_lo   real   : Array of lower energy bound 
c energ_hi   real   : Array of upper energy bound
c rpsf       real   : Array of Radial Point Spread Function
c rpsf_err   real   : Array of errors on rpsf
c area_wgt   real   : Array of Area Weighting Factors
c qarea      logical: True if area_wgt present
c qerror     logical: True if rpsf_err present
c ierr       int    : Error flag, ierr = 0 okay
c                                 ierr = 1 error finding extension 
c                                 ierr = 2 Column/keyword number not found
c                                 ierr = 3 Error in reading data
c                                 ierr = 4 Mandatory keyword not found
c                                 ierr = 5 tunits or ttype not found
c
c --- CALLED ROUTINES -----------------------------------------------------
c
c subroutine FTMAHD      : FITSIO routine to move to extension header
c subroutine FTGKYs      : FITSIO routine to read extension header keyword,
c                          where the s is for string
c subroutine FTGKYj      : FITSIO routine to read extension header keyword,
c                          where the j, is for an integer
c subroutine FTGKNS      : FITSIO routine to read extension header keyword,
c                          where a rootstring is given, thus an array of
c                          keywords can be read
c subroutine FTGCVe      : FITSIO routine to read columns of data in an
c                          extension. The e indicates that it is real data. 
c
c subroutine FCECHO      : FTOOLS routine to write to screen
c subroutine WT_FERRMSG  : Writes FITSIO error text if required
c
c --- COMPILATION AND LINKING ---------------------------------------------
c
c Link with FTOOLS - FITSIO, CALLIB
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------------
c
c Rehana Yusaf (sept 1993) 1.0.0;
c Rehana Yusaf (1994 Jan 20) 1.1.0; Renamed from RD_RPSF1993a to RDRPF1
c                                   hduclas3 keyword is read
c                                   extname extension search removed,it is
c                                   assumed that the file is already at the
c                                   RPSF extension. FNDHDU and/or FNDEXT can
c                                   be used to find the RPSF extension
c  
c Rehana Yusaf (1994 Feb 9) 1.1.1;  Bugfix, status flag set to 0 after
c                                   hduclas3 keyword is read
c Banashree Mitra Seifert (1995 December 15) 2.0.0:
c                          . One bug found in filling 3D AREA_WGT and
c                            fixed 
c                          . Introduced screen display routines
c                            wtinfo,wterrm,wtferr
c ---------------------------------------------------------------------
       character(5) version
       parameter (version = '2.0.0' )
       character(7) subname
*-
c ________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(70) subinfo,errinfo
      character(40) comm,extinfo
      integer maxcol,nunits,ncols,irad,itheta,ien,k,nrows
      parameter (maxcol = 10)
      character(8) ttype(maxcol),rpsfver
      character(16) tunits(maxcol),tform(maxcol),tdim(maxcol)
      character(6) charnum,ch_nrad,ch_nth,ch_nen
      integer fcstln,nrad_len,nth_len,nen_len
      real enull,values(4096),rpsferr,areawgt
      integer status,htype,colnum
      integer felem,inull,frow,snum
      logical anyflg,foundcol,extfind,cnstarea,cnsterr     
      logical qtheta,qenerg

c INTEGER VALUES, USED FOR EXTRACTING NUMBERS FROM TFORM ...

       integer e_pos, charerr, nrad_lo, nrad_hi, nth_lo, nth_hi
       integer nen_lo,nen_hi,i
       integer nrpsf,ndims,nrpsf_nrad,nrpsf_nth,nrpsf_nen
       integer rpsf_dims(3),nrpsfe_nrad,nrpsfe_nth
       integer nrpsfe_nen, rpsferr_dims(3),narea,narea_nrad
       integer narea_nth,narea_nen,area_dims(3)
c
c Internals ...
c
c errstr     char   : Error text for this routine
c subinfo    char   : routine info
c errinfo    char   : More error information
c version    char   : Subroutine version
c enull      real   : Used to represent undefined values in FITS file
c status     int    : Error flag for FITSIO call
c iunit      int    : Fortran unit number for file
c block      int    : FITSIO record blocking factor
c htype      int    : Type of header unit in FITS file
c felem      int    : First pixel of element array in FITS file
c frow       int    : Beginning row No. in FITS file
c inull      int    : Used to represent undefined values in FITS file
c colnum     int    : No. of column
c i,j        int    : Counters for loops
c snum       int    : Value of first keyword sequence No.
c anyflg     logical: True if any data undefined in FITS file
c foundcol   logical: True if column found
c extfind    logical: True if extension found
c
c -------- USER INFO -------

         subname = 'rdrpf1'
         subinfo ='using '//subname //version
         call wtinfo(chatter,10,1,subinfo)
c       
c     --- MOVING TO DATA EXTENSION ---
c     --- READING KEYWORDS ---
c

c INITIALIZE

       nrpsf_nrad = 0
       nrpsf_nth = 0
       nrpsf_nen = 0
       nrpsfe_nrad = 0
       nrpsfe_nth = 0
       nrpsfe_nen = 0
       narea_nrad = 0
       narea_nth = 0
       narea_nen = 0

c READ NAXIS2 

       ierr = 0
       status = 0
       call ftgkyj(iunit,'NAXIS2',nrows,comm,status)
       errinfo = 'reading NAXIS2'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF
       snum = 1

c READ HDUCLAS3 

       status = 0
       call ftgkys(iunit,'HDUCLAS3',hduclas3,comm,status)
       errinfo = 'reading HDUCLAS3'
       call wtferr(subname,version,status,errinfo)

c READ TUNITS

       status = 0
       call ftgkns(iunit,'TUNIT',snum,maxcol,tunits,nunits,status)
       errinfo = 'reading units of data'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 5
         return
       ENDIF

c READ TTYPE (COLUMNS)

       status = 0
       call ftgkns(iunit,'TTYPE',snum,maxcol,ttype,ncols,status)
       errinfo = 'reading column names'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 5
         return
       ENDIF

c READ TFORM

       call ftgkns(iunit,'TFORM',snum,maxcol,tform,ncols,status)
       errinfo = 'reading column format'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 5
         return
       ENDIF        

c READ TDIM (COLUMN DIMENSIONS)

       call ftgkns(iunit,'TDIM',snum,maxcol,tdim,ncols,status)
       errinfo = 'reading column dimensions'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 5
         return
       ENDIF        

c DETERMINE IF THETA, ENERG, RPSF_ERR AND AREA_WGT COLUMNS ARE PRESENT

       qtheta = .false.
       qenerg = .false.
       qerror = .false.
       qarea = .false.
       ntheta = 1
       nenerg = 1
       do i=1,ncols
         IF (ttype(i).EQ.'THETA_LO') THEN
           qtheta = .true.
         ELSEIF (ttype(i).EQ.'THETA_HI') THEN
           qtheta = .true.
         ELSEIF (ttype(i).EQ.'ENERG_LO') THEN
           qenerg = .true.
         ELSEIF (ttype(i).EQ.'ENERG_HI') THEN
           qenerg = .true.
         ELSEIF (ttype(i).EQ.'RPSF_ERR') THEN
           qerror = .true.
         ELSEIF (ttype(i).EQ.'AREA_WGT') THEN
           qarea = .true.
         ENDIF
      enddo

c DETERMINE NRAD,NTHETA,NENERG & NRPSF VALUES BY PARSING TFORM ...

      do i=1,ncols

c RAD_LO TFORM ...

        IF (ttype(i).EQ.'RAD_LO') THEN
          charnum = tform(i)
          e_pos = index(charnum(1:),'E')
          IF (e_pos.EQ.1) THEN
            nrad_lo = 1
          ELSE
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nrad_lo
            IF (charerr.NE.0) THEN
              ierr = 6
              errinfo = 'parsing RAD_LO TFORM'
              call wterrm(subname,version,errinfo)
            ENDIF
          ENDIF

c RAD_HI TFORM ...

        ELSEIF (ttype(i).EQ.'RAD_HI') THEN
          charnum = tform(i)
          e_pos = index(charnum(1:),'E')
          IF (e_pos.EQ.1) THEN
            nrad_hi = 1
          ELSE
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nrad_hi
            IF (charerr.NE.0) THEN
              ierr = 6
              errinfo = 'parsing RAD_HI TFORM'
              call wterrm(subname,version,errinfo)
            ENDIF
          ENDIF                       

c THETA_LO TFORM ...

        ELSEIF (ttype(i).EQ.'THETA_LO') THEN
          charnum = tform(i)
          e_pos = index(charnum(1:),'E')
          IF (e_pos.EQ.1) THEN
            nth_lo = 1
          ELSE
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nth_lo
            IF (charerr.NE.0) THEN
              ierr = 6
              errinfo = 'parsing THETA_LO TFORM'
              call wterrm(subname,version,errinfo)
            ENDIF
          ENDIF    

c THETA_HI TFORM ...

        ELSEIF (ttype(i).EQ.'THETA_HI') THEN
          charnum = tform(i)
          e_pos = index(charnum(1:),'E')
          IF (e_pos.EQ.1) THEN
            nth_hi = 1
          ELSE
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nth_hi
            IF (charerr.NE.0) THEN
              ierr = 6
              errinfo = 'parsing THETA_HI TFORM'
              call wterrm(subname,version,errinfo)
            ENDIF
          ENDIF

c ENERG_LO TFORM ...
   
        ELSEIF (ttype(i).EQ.'ENERG_LO') THEN
          charnum = tform(i)
          e_pos = index(charnum(1:),'E')
          IF (e_pos.EQ.1) THEN
            nen_lo = 1
          ELSE
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nen_lo
            IF (charerr.NE.0) THEN
              ierr = 6
              errinfo = 'parsing ENERG_LO TFORM'
              call wterrm(subname,version,errinfo)
            ENDIF
          ENDIF

c ENERG_HI TFORM ...

        ELSEIF (ttype(i).EQ.'ENERG_HI') THEN
          e_pos = index(charnum(1:),'E')
          IF (e_pos.EQ.1) THEN
            nen_hi = 1
          ELSE
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nen_hi
            IF (charerr.NE.0) THEN
              ierr = 6
              errinfo = 'parsing ENERG_HI TFORM'
              call wterrm(subname,version,errinfo)
            ENDIF
          ENDIF   

c RPSF TFORM ...

        ELSEIF (ttype(i).EQ.'RPSF') THEN
          e_pos = index(charnum(1:),'E')
          IF (e_pos.EQ.1) THEN
            nrpsf = 1
          ELSE
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nrpsf
            IF (charerr.NE.0) THEN
              ierr = 6
              errinfo = 'parsing RPSF TFORM'
              call wterrm(subname,version,errinfo)
            ENDIF
          ENDIF                        
          call par_dim(tdim(i),ndims,rpsf_dims,ierr)
          IF (ierr.NE.0) THEN
            errinfo = 'parsing RPSF TDIM keyword'
            call wterrm(subname,version,errinfo)
            ierr = 6
            return
          ENDIF
          nrpsf_nrad = rpsf_dims(1)
          nrpsf_nth = rpsf_dims(2) 
          nrpsf_nen = rpsf_dims(3)                       

c RPSF_ERR TFORM ...

        ELSEIF (ttype(i).EQ.'RPSF_ERR') THEN
          e_pos = index(charnum(1:),'E')
          IF (e_pos.EQ.1) THEN
            rpsferr = 1
          ELSE
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)rpsferr
            IF (charerr.NE.0) THEN
              ierr = 6
              errinfo = 'parsing RPSF_ERR TFORM'
              call wterrm(subname,version,errinfo)
            ENDIF
          ENDIF     
          call par_dim(tdim(i),ndims,rpsferr_dims,ierr)
          IF (ierr.NE.0) THEN
            errinfo = 'parsing RPSF_ERR TDIM keyword'
            call wterrm(subname,version,errinfo)
            ierr = 6
            return
          ENDIF
          nrpsfe_nrad = rpsferr_dims(1)
          nrpsfe_nth = rpsferr_dims(2)
          nrpsfe_nen = rpsferr_dims(3)  

c AREA_WGT TFORM ...

        ELSEIF (ttype(i).EQ.'AREA_WGT') THEN
          e_pos = index(charnum(1:),'E')
          IF (e_pos.EQ.1) THEN
            narea = 1
          ELSE
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)narea
            IF (charerr.NE.0) THEN
              ierr = 6
              errinfo = 'parsing AREA_WGT TFORM'
              call wterrm(subname,version,errinfo)
            ENDIF
          ENDIF
          call par_dim(tdim(i),ndims,area_dims,ierr)
          IF (ierr.NE.0) THEN
            errinfo = 'parsing AREA_WGT TDIM keyword'
            call wterrm(subname,version,errinfo)
            ierr = 6
            return
          ENDIF
          narea_nrad = area_dims(1)
          narea_nth = area_dims(2)
          narea_nen = area_dims(3)  
        ENDIF
      enddo

c CHECK THAT TFORM AND TDIM VALUES MATCH - WARN OTHERWISE

      nrad = nrad_lo
      IF (nrad_lo.NE.nrad_hi) THEN
        errinfo = 'dimension of RAD_LO column is not'
     &//' equal to RAD_HI dimension !'
        call wtwarm(subname,version,chatter,5,errinfo)
      ENDIF

      IF (qtheta) THEN
        ntheta = nth_lo
        IF (nth_lo.NE.nth_hi) THEN
          errinfo = 'dimension of THETA_LO column is not'
     &//' equal to THETA_HI dimension !'
        call wtwarm(subname,version,chatter,5,errinfo)
        ENDIF
      ENDIF

      IF (qenerg) THEN
        nenerg = nen_lo
        IF (nen_lo.NE.nen_hi) THEN
          errinfo = 'dimension of ENERG_LO column is not'
     &//' equal to ENERG_HI dimension !'
        call wtwarm(subname,version,chatter,5,errinfo)
        ENDIF
      ENDIF

      IF (nrpsf_nrad.NE.nrad) THEN
        errinfo = 'the first dimension in the RPSF array'
     &//' is not equal to nrad !'
        call wtwarm(subname,version,chatter,5,errinfo)
      ENDIF

      IF (nrpsf_nth.NE.ntheta) THEN
        errinfo = 'the second dimension in the RPSF array'
     &//' is not equal to ntheta !'
        call wtwarm(subname,version,chatter,5,errinfo)
      ENDIF

      IF (nrpsf_nen.NE.nenerg) THEN
        errinfo = 'the third dimension in the RPSF array'
     &//' is not equal to nenerg !'
        call wtwarm(subname,version,chatter,5,errinfo)
      ENDIF

      IF ((nrad*ntheta*nenerg).NE.nrpsf) THEN
       errinfo = 'nrpsf is not equal to nrad*ntheta*nenerg'
        call wtwarm(subname,version,chatter,5,errinfo)
      ENDIF

      IF (qerror) THEN

        IF (nrpsfe_nrad.NE.nrad) THEN
          errinfo ='first dimension of RPSF_ERR should'
     &//' be equal to RAD dimension'
        call wtwarm(subname,version,chatter,5,errinfo)
        ENDIF

        IF (nrpsfe_nth.NE.ntheta) THEN
          errinfo ='second dimension of RPSF_ERR should'
     &//' be equal to THETA dimension'
        call wtwarm(subname,version,chatter,5,errinfo)
        ENDIF

        IF (nrpsfe_nen.NE.nenerg) THEN
          errinfo ='third dimension of RPSF_ERR should'
     &//' be equal to ENERGY dimension'
        call wtwarm(subname,version,chatter,5,errinfo)
        ENDIF

        IF ((nrad*ntheta*nenerg).NE.rpsferr) THEN
          errinfo='NRPSFERR should be equal to'
     &//' nrad*ntheta*nenerg'
        call wtwarm(subname,version,chatter,5,errinfo)
        ENDIF
      ENDIF

      IF (qarea) THEN

        IF (narea_nrad.NE.nrad) THEN
          errinfo ='first dimension of AREA_WGT should'
     &//' be equal to RAD dimension'
        call wtwarm(subname,version,chatter,5,errinfo)
        ENDIF

        IF (narea_nth.NE.ntheta) THEN
          errinfo ='second dimension of AREA_WGT should'
     &//' be equal to THETA dimension'
        call wtwarm(subname,version,chatter,5,errinfo)
        ENDIF

        IF (narea_nen.NE.nenerg) THEN
          errinfo ='third dimension of AREA_WGT should'
     &//' be equal to ENERGY dimension'
        call wtwarm(subname,version,chatter,5,errinfo)
        ENDIF

        IF ((nrad*ntheta*nenerg).NE.narea) THEN
          errinfo='NAREA should be equal to'
     &//' nrad*ntheta*nenerg'
        call wtwarm(subname,version,chatter,5,errinfo)
        ENDIF
      ENDIF

c READ RPSFVER

      status = 0
      call ftgkys(iunit,'RPSFVER',rpsfver,comm,status)
      errinfo = 'reading RPSFVER keyword'
      call wtferr(subname,version,status,errinfo)
      IF (rpsfver.NE.'1993a') THEN
        errinfo = 'RPSFVER :'//rpsfver
        call wtwarm(subname,version,chatter,5,errinfo)
        errinfo = 'this routine reads RPSF ver 1993a !'
        call wtwarm(subname,version,chatter,5,errinfo)
      ENDIF

c READ TELESCOP

       status = 0
       call ftgkys(iunit,'TELESCOP',telescop,comm,status)
       errinfo = 'reading TELESCOP keyword'
       call wtferr(subname,version,status,errinfo)
       IF (status.EQ.202) THEN
         telescop = 'UNKNOWN'
       ENDIF

c READ INSTRUME

       status = 0
       call ftgkys(iunit,'INSTRUME',instrume,comm,status)
       errinfo = 'reading INSTRUMENT keyword'
       call wtfwrn(subname,version,chatter,1,status,errinfo)
       IF (status.EQ.202) THEN
         instrume = 'UNKNOWN'
       ENDIF

c READ THETA_LO IF APPROPRIATE

      status = 0
      IF (.NOT.qtheta) THEN
        ntheta = 1
        call ftgkye(iunit,'THETA_LO',theta_lo(1),comm,status)
        errinfo = 'reading THETA_LO keyword '
        call wtferr(subname,version,status,errinfo)

c IF THETA_LO not present then set to default of -99 (not defined)

        IF (status.EQ.202) THEN
          theta_lo(1) = -99
        ENDIF

c STANDARD UNIT FOR THETA_LO ASSUMED

        thetaunit = 'deg'
      ENDIF

c READ THETA_HI IF APPROPRIATE

      status = 0
      IF (.NOT.qtheta) THEN
        call ftgkye(iunit,'THETA_LO',theta_hi(1),comm,status)
        errinfo = 'reading THETA_HI keyword '
        call wtferr(subname,version,status,errinfo)

c IF THETA_HI not present then set to default of -99 (not defined)

        IF (status.EQ.202) THEN
          theta_hi(1) = -99
        ENDIF        
      ENDIF  

c ENERG_LO 

      status = 0
      IF (.NOT.qenerg) THEN
        nenerg = 1
        call ftgkye(iunit,'ENERG_LO',energ_lo(1),comm,status)
        errinfo = 'reading ENERG_LO keyword '
        call wtferr(subname,version,status,errinfo)

c IF ENERG_LO not present then set to default of -99 (not defined)

        IF (status.EQ.202) THEN
          energ_lo(1) = -99
        ENDIF         

c STANDARD UNITS FOR ENERGY ASSUMED

        energunit = 'KeV'
      ENDIF            

c ENERG_HI

      status = 0
      IF (.NOT.qenerg) THEN
        call ftgkye(iunit,'ENERG_HI',energ_hi(1),comm,status)
        errinfo = 'reading ENERG_HI keyword '
        call wtferr(subname,version,status,errinfo)

c IF ENERG_HI not present then set to default of -99 (not defined)

        IF (status.EQ.202) THEN
          energ_hi(1) = -99
        ENDIF         
      ENDIF         

c RPSF_ERR

      IF (.NOT.qerror) THEN
        status = 0
        call ftgkye(iunit,'RPSF_ERR',rpsferr,comm,status)
        errinfo = 'reading RPSF_ERR keyword '
        call wtferr(subname,version,status,errinfo)
        IF (status.EQ.202) THEN
          rpsferr = 0.0
        ENDIF
      ENDIF   

c AREA_WGT 

      status = 0
      IF (.NOT.qarea) THEN
        call ftgkye(iunit,'AREA_WGT',areawgt,comm,status)
        errinfo = 'reading AREA_WGT keyword '
        call wtferr(subname,version,status,errinfo)

c IF AREA_WGT not present then default of 1.0 should be assumed

        IF (status.EQ.202) THEN
          areawgt = 1.0
        ENDIF
      ENDIF         

      subinfo = 'keywords have been read'
      call wtinfo(chatter,20,2,subinfo)
c
c --- READING DATA ---
c

c CHECK TO FIND FIND RAD_LO COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'RAD_LO',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='RAD_LO column not present in RPSF extension' 
          call wterrm(subname,version,errinfo)
          ierr = 2
          return
       ENDIF
       radunit = tunits(colnum)

c READING RAD_LO COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,nrad,enull,rad_lo,
     &             anyflg,status)
       errinfo = 'reading RAD_LO column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND RAD_HI COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'RAD_HI',colnum,status)
       IF (status.NE.0) THEN
          foundcol = .false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='RAD_HI column not present in RPSF extension' 
          call wterrm(subname,version,errinfo)
          ierr = 2
          return
       ENDIF

c READING RAD_HI COLUMN 

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,nrad,enull,rad_hi,
     &             anyflg,status)
       errinfo = 'reading RAD_HI column'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
          ierr = 3
          return
       ENDIF

c FIND THETA_LO COLUMN, IF QTHETA

       IF (qtheta) THEN
         foundcol=.true.
         status = 0
         call ftgcno(iunit,.false.,'THETA_LO',colnum,status)
         IF (status.NE.0) THEN
          foundcol = .false.
         ENDIF
         IF (.NOT.foundcol) THEN
            errinfo='THETA_LO column not present in '
     &//' RPSF extension'
         call wtferr(subname,version,status,errinfo)
            ierr = 2
            return
         ENDIF
         thetaunit = tunits(colnum)         
       ENDIF

c READING THETA_LO COLUMN

       IF (qtheta) THEN
         frow=1
         felem=1
         enull=0
         status=0
         call ftgcve(iunit,colnum,frow,felem,nrad,enull,theta_lo,
     &             anyflg,status)
         errinfo = 'reading THETA_LO column'
         call wtferr(subname,version,status,errinfo)
         IF (status.NE.0) THEN
            ierr = 3
            return
         ENDIF
       ENDIF

c FIND THETA_HI COLUMN, IF QTHETA 

       IF (qtheta) THEN
         foundcol=.true.
         status = 0
         call ftgcno(iunit,.false.,'THETA_HI',colnum,status)
         IF (status.NE.0) THEN
          foundcol = .false.
         ENDIF
         IF (.NOT.foundcol) THEN
            errinfo='THETA_HI column not present in '
     &//' RPSF extension'
            call wterrm(subname,version,errinfo)
            ierr = 2
            return
         ENDIF
       ENDIF     

c READING THETA_HI COLUMN

       IF (qtheta) THEN
         frow=1
         felem=1
         enull=0
         status=0
         call ftgcve(iunit,colnum,frow,felem,nrad,enull,theta_hi,
     &             anyflg,status)
         errinfo = 'reading THETA_HI column'
         call wtferr(subname,version,status,errinfo)
         IF (status.NE.0) THEN
            ierr = 3
            return
         ENDIF                       
       ENDIF

c CHECK TO FIND ENERG_LO COLUMN

       IF (qenerg) THEN
         foundcol=.true.
         status = 0
         call ftgcno(iunit,.false.,'ENERG_LO',colnum,status)
         IF (status.NE.0) THEN
           foundcol = .false.
         ENDIF
         IF (.NOT.foundcol) THEN
          errinfo='ENERG_LO column not present in '
     &//'RPSF extension' 
            call wterrm(subname,version,errinfo)
          ierr = 2
          return
         ENDIF    
         energunit = tunits(colnum)
       ENDIF

c READING ENERG_LO COLUMN

       IF (qenerg) THEN
         frow=1
         felem=1
         enull=0
         status=0
         call ftgcve(iunit,colnum,frow,felem,nrad,enull,energ_lo,
     &             anyflg,status)
         errinfo = 'reading ENERG_LO column'
         call wtferr(subname,version,status,errinfo)
         IF (status.NE.0) THEN
          ierr = 3
          return
         ENDIF
       ENDIF

c CHECK TO FIND ENERG_HI COLUMN

       IF (qenerg) THEN
         foundcol=.true.
         status = 0
         call ftgcno(iunit,.false.,'ENERG_HI',colnum,status)
         IF (status.NE.0) THEN
           foundcol = .false.
         ENDIF
         IF (.NOT.foundcol) THEN
          errinfo='ENERG_HI column not present in '
     &//'RPSF extension' 
          call wterrm(subname,version,errinfo)
          ierr = 2
          return
         ENDIF
       ENDIF

c READING ENERG_HI COLUMN

       IF (qenerg) THEN
         frow=1
         felem=1
         enull=0
         status=0
         call ftgcve(iunit,colnum,frow,felem,nrad,enull,energ_hi,
     &             anyflg,status)
         errinfo = 'reading ENERG_HI column'
         call wtferr(subname,version,status,errinfo)
         IF (status.NE.0) THEN
          ierr = 3
          return
         ENDIF
       ENDIF              

c FIND RPSF COLUMN NUMBER 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'RPSF',colnum,status)
       IF (status.NE.0) THEN
          foundcol = .false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='RPSF column not present in RPSF extension' 
          call wterrm(subname,version,errinfo)
          ierr = 2
          return
       ENDIF                
       rpsfunit = tunits(colnum)

c READING RPSF COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,nrad,enull,values,
     &             anyflg,status)
       errinfo = 'reading RPSF column'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
          ierr = 3
          return
       ENDIF            

c FILL 3D ARRAY RPSF(irad,itheta,ien)

       k = 0
       do irad=1,nrad
         do itheta=1,ntheta
           do ien=1,nenerg
              k = k+1
              rpsf(irad,itheta,ien) = values(k)
            enddo
         enddo
       enddo
                                               
c CHECK TO FIND RPSF_ERR COLUMN 

       IF (qerror) THEN
        foundcol=.true.
        status = 0
        call ftgcno(iunit,.false.,'RPSF_ERR',colnum,status)
        IF (status.NE.0) THEN
          foundcol = .false.
        ENDIF
        IF (.NOT.foundcol) THEN
          extinfo='RPSF_ERR column not present in RPSF extension' 
          call wterrm(subname,version,extinfo)
          ierr = 2
          return
        ENDIF

c READING RPSF_ERR COLUMN 

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,nrad,enull,values,
     &            anyflg,status)
       IF (status.NE.0) THEN
         extinfo = 'reading RPSF_ERR column'
         call wterrm(subname,version,extinfo)
         ierr = 3
         return
       ENDIF
      ENDIF

c FILL 3D ARRAY RPSF_ERR(irad,itheta,ien)

      cnsterr = .false.
      IF ((.NOT.qerror).AND.(rpsferr.NE.(0.0))) THEN
        cnsterr = .true.
      ENDIF
      k = 0
       do irad=1,nrad
         do itheta=1,ntheta
           do ien=1,nenerg
              k = k+1
              IF (cnsterr) THEN
                rpsf_err(irad,itheta,ien) = rpsferr
              ELSE
                rpsf_err(irad,itheta,ien) = values(k)
              ENDIF
            enddo
         enddo
      enddo                

c FIND AREA_WGT COLUMN NUMBER

      IF (qarea) THEN
       foundcol=.true.
       call ftgcno(iunit,.false.,'AREA_WGT',colnum,status)
       IF (status.NE.0) THEN
           foundcol = .false.
       ENDIF
       IF (.NOT.foundcol) THEN
          extinfo=' AREA_WGT column not present in RPSF extension' 
          call wtinfo(chatter,1,2,extinfo)
          ierr = 2
          return
       ENDIF                

c READING AREA_WGT COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,nrad,enull,values,
     &            anyflg,status)
       IF (status.NE.0) THEN
         extinfo = 'reading AREA_WGT column'
         call wterrm(subname,version,extinfo)
         ierr = 3
         return
       ENDIF                  
      ENDIF

c FILL 3D ARRAY AREA_WGT(irad,itheta,ien)

      cnstarea = .false.
c     IF ((.NOT.qarea).AND.(areawgt.NE.(1.0))) THEN
c  This previous line is fixed in this Version 1.2.0 in (next line)

      IF (.NOT.qarea) THEN
        cnstarea = .true.
        qarea = .true.
      ENDIF
      k = 0
       do irad=1,nrad
         do itheta=1,ntheta
           do ien=1,nenerg
              k = k+1
              IF (cnstarea) THEN
                area_wgt(irad,itheta,ien) = areawgt
              ELSE
                area_wgt(irad,itheta,ien) = values(k)
              ENDIF
            enddo
         enddo
      enddo

      subinfo = 'data has been read'
      call wtinfo(chatter,10,2,subinfo)
      return
      end
c ------------------------------------------------------------------------
c     END OF SUBROUTINE RDRPF1
c ------------------------------------------------------------------------


