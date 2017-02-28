*+RDEEF1
c -------------------------------------------------------------------
      subroutine rdeef1(iunit,hduclas3,nrad,rad_lo,rad_hi,
     >                        radunit,ntheta,theta_lo,theta_hi,
     >                        thetaunit,nenerg,energ_lo,energ_hi,
     >                        energunit,reef,qerror,reef_err,
     >                        reefunit,qarea,area_wgt,telescop,
     >                        instrume,maxrad,maxtheta,ierr,chatter)

c _--------------- DESCRIPTION -------------------------------------
c
c This subroutine reads an OGIP standard 
c HDUCLAS1 RESPONSE
c HDUCLAS2 REEF
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
c REEF      : Radial Encircled Energy Function
c             3D array - REEF(irad,itheta,ien)
c REEF_ERR  : Error on REEF, 3D array - REEF_ERR(irad,itheta,ien)
c             this is an optional column, if it is not present then
c             qerror is set to false
c
c Keywords read ...
c
c REEFVER   : Routine version
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
      real reef(maxrad,maxtheta,*),reef_err(maxrad,maxtheta,*)
      character*(*) radunit,thetaunit,energunit,reefunit,hduclas3
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
c reef       real   : Array of Radial Point Spread Function
c reef_err   real   : Array of errors on reef
c area_wgt   real   : Array of Area Weighting Factors
c qarea      logical: True if area_wgt present
c qerror     logical: True if reef_err present
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
c ----------------------- AUTHORS/MODifICATION HISTORY -----------------
c
c Banashree Mitra Seifert (December 1995); from rdrpf1.f
c              . screen dsplay subroutines used
c
c ---------------------------------------------------------------------
       character(5) version
       parameter (version = '1.0.0' )
*-
c ________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(70) subinfo,errinfo
      character(40) comm,extinfo
      integer maxcol,nunits,ncols,irad,itheta,ien,k,nrows
      parameter (maxcol = 10)
      character(8) ttype(maxcol)
      character(16) tunits(maxcol),tform(maxcol),tdim(maxcol)
      character(6) charnum
      real enull,values(4096),reeferr,areawgt
      integer status,colnum
      integer felem,frow,snum
      logical anyflg,foundcol,cnstarea,cnsterr     
      logical qtheta,qenerg

c INT .ge.  VALUES, USED FOR EXTRACTING NUMBERS FROM TFORM ...

       integer e_pos, charerr, nrad_lo, nrad_hi, nth_lo, nth_hi
       integer nen_lo,nen_hi,i
       integer nreef,ndims,nreef_nrad,nreef_nth,nreef_nen
       integer reef_dims(3),nreefe_nrad,nreefe_nth
       integer nreefe_nen, reeferr_dims(3),narea,narea_nrad
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
c      --- USER INFO ---
c -----------------------------------------------------------------------
       character(7) subname
       subname = 'rdeef1'

       subinfo ='using '//subname//version
       call wtinfo(chatter,10,1,subinfo)
       
c      --- MOVING TO DATA EXTENSION ---
c         --- READING KEYWORDS ---

c INITIALIZE

       nreef_nrad = 0
       nreef_nth = 0
       nreef_nen = 0
       nreefe_nrad = 0
       nreefe_nth = 0
       nreefe_nen = 0
       narea_nrad = 0
       narea_nth = 0
       narea_nen = 0

c READ NAXIS2 

       status = 0
       call ftgkyj(iunit,'NAXIS2',nrows,comm,status)
       errinfo = ' reading NAXIS2'
       call wtferr(subname,version,status,errinfo)
       if (status .ne. 0) then
         ierr = 4
         return
       endif
       snum = 1

c READ HDUCLAS3 

       status = 0
       call ftgkys(iunit,'HDUCLAS3',hduclas3,comm,status)
       errinfo = ' reading HDUCLAS3'
       call wtferr(subname,version,status,errinfo)

c READ TUNITS

       status = 0
       call ftgkns(iunit,'TUNIT',snum,maxcol,tunits,nunits,status)
       errinfo = ' reading units of data'
       call wtferr(subname,version,status,errinfo)
       if (status .ne. 0) then
         ierr = 5
         return
       endif

c READ TTYPE (COLUMNS)

       status = 0
       call ftgkns(iunit,'TTYPE',snum,maxcol,ttype,ncols,status)
       errinfo = ' reading column names'
       call wtferr(subname,version,status,errinfo)
       if (status .ne. 0) then
         ierr = 5
         return
       endif

c READ TFORM

       call ftgkns(iunit,'TFORM',snum,maxcol,tform,ncols,status)
       errinfo = ' reading column format'
       call wtferr(subname,version,status,errinfo) 
       if (status .ne. 0) then
         ierr = 5
         return
       endif        

c READ TDIM (COLUMN DIMENSIONS)

       call ftgkns(iunit,'TDIM',snum,maxcol,tdim,ncols,status)
       errinfo = ' reading column dimensions'
       call wtferr(subname,version,status,errinfo)
       if (status .ne. 0) then
         ierr = 5
         return
       endif        

c DETERMINE IF THETA, ENERG, REEF_ERR AND AREA_WGT COLUMNS ARE PRESENT

       qtheta = .false.
       qenerg = .false.
       qerror = .false.
       qarea = .false.
       ntheta = 1
       nenerg = 1
       do i=1,ncols
         if (ttype(i) .eq. 'THETA_LO') then
           qtheta = .true.
         elseif (ttype(i) .eq. 'THETA_HI') then
           qtheta = .true.
         elseif (ttype(i) .eq. 'ENERG_LO') then
           qenerg = .true.
         elseif (ttype(i) .eq. 'ENERG_HI') then
           qenerg = .true.
         elseif (ttype(i) .eq. 'REEF_ERR') then
           qerror = .true.
         elseif (ttype(i) .eq. 'AREA_WGT') then
           qarea = .true.
         endif
      enddo

c DETERMINE NRAD,NTHETA,NENERG & NREEF VALUES BY PARSING TFORM ...

      do i=1,ncols

c RAD_LO TFORM ...

        if (ttype(i) .eq. 'RAD_LO') then
          charnum = tform(i)
          e_pos = index(charnum(1:),'E')
          if (e_pos .eq. 1) then
            nrad_lo = 1
          else
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nrad_lo
            if (charerr .ne. 0) then
              ierr = 6
              errinfo = ' parsing RAD_LO TFORM'
              call wterrm(subname,version,errinfo)
            endif
          endif

c RAD_HI TFORM ...

        elseif (ttype(i) .eq. 'RAD_HI') then
          charnum = tform(i)
          e_pos = index(charnum(1:),'E')
          if (e_pos .eq. 1) then
            nrad_hi = 1
          else
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nrad_hi
            if (charerr .ne. 0) then
              ierr = 6
              errinfo = ' parsing RAD_HI TFORM'
              call wterrm(subname,version,errinfo)
            endif
          endif                       

c THETA_LO TFORM ...

        elseif (ttype(i) .eq. 'THETA_LO') then
          charnum = tform(i)
          e_pos = index(charnum(1:),'E')
          if (e_pos .eq. 1) then
            nth_lo = 1
          else
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nth_lo
            if (charerr .ne. 0) then
              ierr = 6
              errinfo = ' parsing THETA_LO TFORM'
              call wterrm(subname,version,errinfo)
            endif
          endif    

c THETA_HI TFORM ...

        elseif (ttype(i) .eq. 'THETA_HI') then
          charnum = tform(i)
          e_pos = index(charnum(1:),'E')
          if (e_pos .eq. 1) then
            nth_hi = 1
          else
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nth_hi
            if (charerr .ne. 0) then
              ierr = 6
              errinfo = ' parsing THETA_HI TFORM'
              call wterrm(subname,version,errinfo)
            endif
          endif

c ENERG_LO TFORM ...
   
        elseif (ttype(i) .eq. 'ENERG_LO') then
          charnum = tform(i)
          e_pos = index(charnum(1:),'E')
          if (e_pos .eq. 1) then
            nen_lo = 1
          else
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nen_lo
            if (charerr .ne. 0) then
              ierr = 6
              errinfo = ' parsing ENERG_LO TFORM'
              call wterrm(subname,version,errinfo)
            endif
          endif

c ENERG_HI TFORM ...

        elseif (ttype(i) .eq. 'ENERG_HI') then
          e_pos = index(charnum(1:),'E')
          if (e_pos .eq. 1) then
            nen_hi = 1
          else
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nen_hi
            if (charerr .ne. 0) then
              ierr = 6
              errinfo =' parsing ENERG_HI TFORM'
              call wterrm(subname,version,errinfo)
            endif
          endif   

c REEF TFORM ...

        elseif (ttype(i) .eq. 'REEF') then
          e_pos = index(charnum(1:),'E')
          if (e_pos .eq. 1) then
            nreef = 1
          else
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)nreef
            if (charerr .ne. 0) then
              ierr = 6
              errinfo = ' parsing REEF TFORM'
              call wterrm(subname,version,errinfo)
            endif
          endif                        
          call par_dim(tdim(i),ndims,reef_dims,ierr)
          if (ierr .ne. 0) then
              errinfo = ' parsing REEF TDIM keyword'
              call wterrm(subname,version,errinfo)
              ierr = 6
              return
          endif
          nreef_nrad = reef_dims(1)
          nreef_nth = reef_dims(2) 
          nreef_nen = reef_dims(3)                       

c REEF_ERR TFORM ...

        elseif (ttype(i) .eq. 'REEF_ERR') then
          e_pos = index(charnum(1:),'E')
          if (e_pos .eq. 1) then
            reeferr = 1
          else
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)reeferr
            if (charerr .ne. 0) then
              ierr = 6
              errinfo = ' parsing REEF_ERR TFORM'
              call wterrm(subname,version,errinfo)
            endif
          endif     
          call par_dim(tdim(i),ndims,reeferr_dims,ierr)
          if (ierr .ne. 0) then
              errinfo = ' parsing REEF_ERR TDIM keyword'
              call wterrm(subname,version,errinfo)
              ierr = 6
              return
          endif
          nreefe_nrad = reeferr_dims(1)
          nreefe_nth = reeferr_dims(2)
          nreefe_nen = reeferr_dims(3)  

c AREA_WGT TFORM ...

        elseif (ttype(i) .eq. 'AREA_WGT') then
          e_pos = index(charnum(1:),'E')
          if (e_pos .eq. 1) then
            narea = 1
          else
            read (charnum(1:e_pos-1),*,IOSTAT=charerr)narea
            if (charerr .ne. 0) then
              ierr = 6
              errinfo = ' parsing AREA_WGT TFORM'
              call wterrm(subname,version,errinfo)
            endif
          endif
          call par_dim(tdim(i),ndims,area_dims,ierr)
          if (ierr .ne. 0) then
              errinfo = ' parsing AREA_WGT TDIM keyword'
              call wterrm(subname,version,errinfo)
              ierr = 6
              return
          endif
          narea_nrad = area_dims(1)
          narea_nth = area_dims(2)
          narea_nen = area_dims(3)  
        endif
      enddo

c CHECK THAT TFORM AND TDIM VALUES MATCH - WARN OTHERWISE

      nrad = nrad_lo
      if (nrad_lo .ne. nrad_hi) then
          errinfo = 'dimension of RAD_LO column is not'
     &              //' equal to RAD_HI dimension !'
          call wtwarm(subname,version,chatter,5,errinfo)
      endif

      if (qtheta) then
          ntheta = nth_lo
          if (nth_lo .ne. nth_hi) then
             errinfo = 'dimension of THETA_LO column is not'
     &                //' equal to THETA_HI dimension !'
             call wtwarm(subname,version,chatter,5,errinfo)
          endif
      endif

      if (qenerg) then
          nenerg = nen_lo
          if (nen_lo .ne. nen_hi) then
              errinfo = ' Dimension of ENERG_LO column is not'
     &                  //' equal to ENERG_HI dimension !'

              call wtwarm(subname,version,chatter,5,errinfo)
          endif
      endif

      if (nreef_nrad .ne. nrad) then
          errinfo = ' The first dimension in the REEF array'
     &              //' is not equal to nrad !'

          call wtwarm(subname,version,chatter,5,errinfo)
      endif

      if (nreef_nth .ne. ntheta) then
          errinfo = ' The second dimension in the REEF array'
     &              //' is not equal to ntheta !'

          call wtwarm(subname,version,chatter,5,errinfo)
      endif

      if (nreef_nen .ne. nenerg) then
          errinfo = ' The third dimension in the REEF array'
     &              //' is not equal to nenerg !'

          call wtwarm(subname,version,chatter,5,errinfo)
      endif

      if ((nrad*ntheta*nenerg) .ne. nreef) then
          errinfo = ' nreef is not equal to nrad*ntheta*nenerg'
          call wtwarm(subname,version,chatter,5,errinfo)
      endif

      if (qerror) then

        if (nreefe_nrad .ne. nrad) then
            errinfo =' First dimension of REEF_ERR should'
     &               //' be equal to RAD dimension'
            call wtwarm(subname,version,chatter,5,errinfo)
        endif

        if (nreefe_nth .ne. ntheta) then
            errinfo =' Second dimension of REEF_ERR should'
     &               //' be equal to THETA dimension'
            call wtwarm(subname,version,chatter,5,errinfo)
        endif

        if (nreefe_nen .ne. nenerg) then
            errinfo =' Third dimension of REEF_ERR should'
     &               //' be equal to ENERGY dimension'
            call wtwarm(subname,version,chatter,5,errinfo)
        endif

        if ((nrad*ntheta*nenerg) .ne. reeferr) then
             errinfo='NREEFERR should be equal to'
     &               //' nrad*ntheta*nenerg'
            call wtwarm(subname,version,chatter,5,errinfo)
        endif
      endif

      if (qarea) then

        if (narea_nrad .ne. nrad) then
            errinfo =' First dimension of AREA_WGT should'
     &                //' be equal to RAD dimension'
            call wtwarm(subname,version,chatter,5,errinfo)
        endif

        if (narea_nth .ne. ntheta) then
            errinfo =' Second dimension of AREA_WGT should'
     &                //' be equal to THETA dimension'
            call wtwarm(subname,version,chatter,5,errinfo)
        endif

        if (narea_nen .ne. nenerg) then
            errinfo =' Third dimension of AREA_WGT should'
     &               //' be equal to ENERGY dimension'
            call wtwarm(subname,version,chatter,5,errinfo)
        endif

        if ((nrad*ntheta*nenerg) .ne. narea) then
             errinfo='NAREA should be equal to'
     &               //' nrad*ntheta*nenerg'
             call wtwarm(subname,version,chatter,5,errinfo)
        endif
      endif

c READ REEFVER
c
c      status = 0
c      call ftgkys(iunit,'REEFVER ',reefver,comm,status)
c
c READ TELESCOP

       status = 0
       call ftgkys(iunit,'TELESCOP',telescop,comm,status)
       errinfo = ' reading TELESCOP keyword'
       call wtferr(subname,version,status,errinfo)
       if (status .eq. 202) then
         telescop = 'UNKNOWN'
       endif

c READ INSTRUME

       status = 0
       call ftgkys(iunit,'INSTRUME',instrume,comm,status)
       errinfo = ' reading INSTRUMENT keyword'
       call wtferr(subname,version,status,errinfo)
       if (status .eq. 202) then
         instrume = 'UNKNOWN'
       endif

c READ THETA_LO if APPROPRIATE

      status = 0
      if (.NOT.qtheta) then
          ntheta = 1
          call ftgkye(iunit,'THETA_LO',theta_lo(1),comm,status)
          errinfo = ' reading THETA_LO keyword '
          call wtferr(subname,version,status,errinfo)

c if THETA_LO not present then set to default of -99 (not defined)

        if (status .eq. 202) then
          theta_lo(1) = -99
        endif

c STANDARD UNIT FOR THETA_LO ASSUMED

        thetaunit = 'deg'
      endif

c READ THETA_HI if APPROPRIATE

      status = 0
      if (.NOT.qtheta) then
          call ftgkye(iunit,'THETA_LO',theta_hi(1),comm,status)
          errinfo = ' reading THETA_HI keyword '
          call wtferr(subname,version,status,errinfo)

c if THETA_HI not present then set to default of -99 (not defined)

        if (status .eq. 202) then
          theta_hi(1) = -99
        endif        
      endif  

c ENERG_LO 

      status = 0
      if (.NOT.qenerg) then
          nenerg = 1
          call ftgkye(iunit,'ENERG_LO',energ_lo(1),comm,status)
          errinfo = ' reading ENERG_LO keyword '
          call wtferr(subname,version,status,errinfo)

c if ENERG_LO not present then set to default of -99 (not defined)

        if (status .eq. 202) then
          energ_lo(1) = -99
        endif         

c STANDARD UNITS FOR ENERGY ASSUMED

        energunit = 'KeV'
      endif            

c ENERG_HI

      status = 0
      if (.NOT.qenerg) then
          call ftgkye(iunit,'ENERG_HI',energ_hi(1),comm,status)
          errinfo = ' reading ENERG_HI keyword '
          call wtferr(subname,version,status,errinfo)

c if ENERG_HI not present then set to default of -99 (not defined)

        if (status .eq. 202) then
          energ_hi(1) = -99
        endif         
      endif         

c REEF_ERR

      if (.NOT.qerror) then
          status = 0
          call ftgkye(iunit,'REEF_ERR',reeferr,comm,status)
          errinfo = ' reading REEF_ERR keyword '
          call wtferr(subname,version,status,errinfo)
          if (status .eq. 202) then
              reeferr = 0.0
          endif
      endif   

c AREA_WGT 

      status = 0
      if (.NOT.qarea) then
          call ftgkye(iunit,'AREA_WGT',areawgt,comm,status)
          errinfo = ' reading AREA_WGT keyword '
          call wtferr(subname,version,status,errinfo)

c if AREA_WGT not present then default of 1.0 should be assumed

        if (status .eq. 202) then
          areawgt = 1.0
        endif
      endif         

      subinfo = 'Keywords have been read'
      call wtinfo(chatter,20,4,subinfo)

c --- READING DATA ---

c CHECK TO FIND FIND RAD_LO COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'RAD_LO',colnum,status)
       if (status.NE.0) then
         foundcol=.false.
       endif
       if (.NOT.foundcol) then
           errinfo='RAD_LO column not present in REEF extension' 
           call wterrm(subname,version,errinfo)
           ierr = 2
           return
       endif
       radunit = tunits(colnum)

c READING RAD_LO COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,nrad,enull,rad_lo,
     &             anyflg,status)
       errinfo = ' reading RAD_LO column '
       call wtferr(subname,version,status,errinfo)
       if (status .ne. 0) then
           ierr = 3
           return
       endif

c CHECK TO FIND RAD_HI COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'RAD_HI',colnum,status)
       if (status .ne. 0) then
           foundcol = .false.
       endif
       if (.NOT.foundcol) then
           errinfo='RAD_HI column not present in REEF extension' 
           call wtferr(subname,version,status,errinfo)
           ierr = 2
           return
       endif

c READING RAD_HI COLUMN 

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,nrad,enull,rad_hi,
     &             anyflg,status)
       errinfo = ' reading RAD_HI column'
       call wtferr(subname,version,status,errinfo)
       if (status .ne. 0) then
           ierr = 3
           return
       endif

c FIND THETA_LO COLUMN, if QTHETA

       if (qtheta) then
         foundcol=.true.
         status = 0
         call ftgcno(iunit,.false.,'THETA_LO',colnum,status)
         if (status .ne. 0) then
             foundcol = .false.
         endif
         if (.NOT.foundcol) then
             errinfo='THETA_LO column not present in '
     &               //' REEF extension'
             call wterrm(subname,version,errinfo)
             ierr = 2
             return
         endif
         thetaunit = tunits(colnum)         
       endif

c READING THETA_LO COLUMN

       if (qtheta) then
         frow=1
         felem=1
         enull=0
         status=0
         call ftgcve(iunit,colnum,frow,felem,nrad,enull,theta_lo,
     &               anyflg,status)
         errinfo = ' reading THETA_LO column'
         call wtferr(subname,version,status,errinfo)
         if (status .ne. 0) then
             ierr = 3
             return
         endif
       endif

c FIND THETA_HI COLUMN, if QTHETA 

       if (qtheta) then
         foundcol=.true.
         status = 0
         call ftgcno(iunit,.false.,'THETA_HI',colnum,status)
         if (status .ne. 0) then
             foundcol = .false.
         endif
         if (.NOT.foundcol) then
             errinfo='THETA_HI column not present in '
     &                //' REEF extension'
             call wterrm(subname,version,errinfo)
             ierr = 2
             return
         endif
       endif     

c READING THETA_HI COLUMN

       if (qtheta) then
         frow=1
         felem=1
         enull=0
         status=0
         call ftgcve(iunit,colnum,frow,felem,nrad,enull,theta_hi,
     &             anyflg,status)
         errinfo = ' reading THETA_HI column'
         call wtferr(subname,version,status,errinfo)
         if (status .ne. 0) then
             ierr = 3
             return
         endif                       
       endif

c CHECK TO FIND ENERG_LO COLUMN

       if (qenerg) then
         foundcol=.true.
         status = 0
         call ftgcno(iunit,.false.,'ENERG_LO',colnum,status)
         if (status .ne. 0) then
           foundcol = .false.
         endif
         if (.NOT.foundcol) then
             errinfo='ENERG_LO column not present in '
     &               //'REEF extension' 
             call wterrm(subname,version,errinfo)
          ierr = 2
          return
         endif    
         energunit = tunits(colnum)
       endif

c READING ENERG_LO COLUMN

       if (qenerg) then
         frow=1
         felem=1
         enull=0
         status=0
         call ftgcve(iunit,colnum,frow,felem,nrad,enull,energ_lo,
     &             anyflg,status)
         errinfo = ' reading ENERG_LO column'
         call wtferr(subname,version,status,errinfo)
         if (status .ne. 0) then
          ierr = 3
          return
         endif
       endif

c CHECK TO FIND ENERG_HI COLUMN

       if (qenerg) then
         foundcol=.true.
         status = 0
         call ftgcno(iunit,.false.,'ENERG_HI',colnum,status)
         if (status .ne. 0) then
           foundcol = .false.
         endif
         if (.NOT.foundcol) then
          errinfo='ENERG_HI column not present in '
     &             //'REEF extension' 
          call wterrm(subname,version,errinfo) 
          ierr = 2
          return
         endif
       endif

c READING ENERG_HI COLUMN

       if (qenerg) then
         frow=1
         felem=1
         enull=0
         status=0
         call ftgcve(iunit,colnum,frow,felem,nrad,enull,energ_hi,
     &             anyflg,status)
         errinfo = ' reading ENERG_HI column'
         call wtferr(subname,version,status,errinfo)
         if (status .ne. 0) then
          ierr = 3
          return
         endif
       endif              

c FIND REEF COLUMN NUMBER 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'REEF',colnum,status)
       if (status .ne. 0) then
          foundcol = .false.
       endif
       if (.NOT.foundcol) then
          errinfo='REEF column not present in REEF extension' 
          call wterrm(subname,version,errinfo)
          ierr = 2
          return
       endif                
       reefunit = tunits(colnum)

c READING REEF COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,nrad,enull,values,
     &             anyflg,status)
       errinfo = ' reading REEF column'
       call wtferr(subname,version,status,errinfo)
       if (status .ne. 0) then
          ierr = 3
          return
       endif            

c FILL 3D ARRAY REEF(irad,itheta,ien)

       k = 0
       do irad=1,nrad
         do itheta=1,ntheta
           do ien=1,nenerg
              k = k+1
              reef(irad,itheta,ien) = values(k)
            enddo
         enddo
       enddo
                                               
c CHECK TO FIND REEF_ERR COLUMN 

       if (qerror) then
        foundcol=.true.
        status = 0
        call ftgcno(iunit,.false.,'REEF_ERR',colnum,status)
        if (status .ne. 0) then
          foundcol = .false.
        endif
        if (.NOT.foundcol) then
          extinfo=' REEF_ERR column not present in REEF extension' 
          call wterrm(subname,version,extinfo)
          ierr = 2
          return
        endif

c READING REEF_ERR COLUMN 

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,nrad,enull,values,
     &            anyflg,status)
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           extinfo = ' READING REEF_ERR COLUMN'
           call wterrm(subname,version,extinfo)
         ierr = 3
         return
       endif
      endif

c FILL 3D ARRAY REEF_ERR(irad,itheta,ien)

      cnsterr = .false.
      if ((.NOT.qerror).AND.(reeferr .ne. (0.0))) then
        cnsterr = .true.
      endif
      k = 0
       do irad=1,nrad
         do itheta=1,ntheta
           do ien=1,nenerg
              k = k+1
              if (cnsterr) then
                reef_err(irad,itheta,ien) = reeferr
              else
                reef_err(irad,itheta,ien) = values(k)
              endif
            enddo
         enddo
      enddo                

c FIND AREA_WGT COLUMN NUMBER

      if (qarea) then
       foundcol=.true.
       call ftgcno(iunit,.false.,'AREA_WGT',colnum,status)
       if (status .ne. 0) then
           foundcol = .false.
       endif
       if (.NOT.foundcol) then
          extinfo=' AREA_WGT column not present in REEF extension' 
          call wterrm(subname,version,extinfo)
          ierr = 2
          return
       endif                

c READING AREA_WGT COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,nrad,enull,values,
     &            anyflg,status)
       call wtferr(subname,version,status,subinfo)
       if (status .ne. 0) then
           extinfo = ' READING AREA_WGT COLUMN'
           call wterrm(subname,version,extinfo)
           ierr = 3
           return
       endif                  
      endif

c FILL 3D ARRAY AREA_WGT(irad,itheta,ien)

      cnstarea = .false.
      if ((.NOT.qarea).AND.(areawgt .ne. (1.0))) then
        cnstarea = .true.
        qarea = .true.
      endif
      k = 0
       do irad=1,nrad
         do itheta=1,ntheta
           do ien=1,nenerg
              k = k+1
              if (cnstarea) then
                area_wgt(irad,itheta,ien) = areawgt
              else
                area_wgt(irad,itheta,ien) = values(k)
              endif
            enddo
         enddo
      enddo

      subinfo = 'Data has been read'
      call wtinfo(chatter,20,4,subinfo)

      return
      end
c ------------------------------------------------------------------------
c     end OF SUBROUTINE RDEEF1  
c ------------------------------------------------------------------------

