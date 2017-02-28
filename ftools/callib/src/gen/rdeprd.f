*+RDEPRD
c     -------------------------------------------------------------
      subroutine rdeprd(iunit,n_ep,max_ep,mjd_int,mjd_frac,
     &                  sun_x,sun_y,sun_z,
     &                  moon_x,moon_y,moon_z,
     &                  lon_east,lat_nort,sat_x,sat_y,sat_z,
     &                  gha,alt_sat,
     &                  chatter,ierr)
c     -------------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads a FITS RDF format EPHEM extension
c NOTE : Assumes file is already open  at the desired extension.
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c 
c MJD_INT : The integer part of UTC in Modified Julian Date (MJD)
c MJD_FRAC: The fractional portion of UTC in MJD
c SUN_X   : Unit vectors to the Sun in the ecf (Earth Centred Frame) 
c           x component  
c SUN_Y   : Unit vectors to the Sun in the ecf;y component 
c SUN_Z   : Unit vectors to the Sun in the ecf;z component 
c MOON_X  : Moon unit vector in ecf; x component
c MOON_Y  : Moon unit vector in ecf; y component
c MOON_Z  : Moon unit vector in ecf; z component
c LON_EAST: Geodetic longitude east in ECF-frame
c LON_NORT: Geodetic longitude north in ECF-frame
c ALT_SAT : Satellite altitude above elipsoid
c SAT_X   : Satellite position vector ,ECF frame, X component
c SAT_Y   : Satellite position vector ,ECF frame, Y component
c SAT_Z   : Satellite position vector ,ECF frame, Z component
c GHA     : Greenwich Hour Angle  
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_ep,n_ep,ierr,chatter
      integer mjd_int(max_ep)
      real*8 mjd_frac(max_ep),gha(max_ep)
      real*8 sun_x(max_ep),sun_y(max_ep),sun_z(max_ep) 
      real*8 moon_x(max_ep),moon_y(max_ep),moon_z(max_ep)
      real*8 lon_east(max_ep),lat_nort(max_ep),alt_sat(max_ep)
      integer sat_x(max_ep),sat_y(max_ep),sat_z(max_ep)
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c max_ep     int    : Array dimensions
c iunit      int    : Fortran unit number for file
c chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c n_ep       int i/r: Counter ephem data 
c ierr       int    : Error flag, ierr = 0 okay
c                                 ierr = 2 Column/keyword number not found
c                                 ierr = 3 Error in reading data
c                                 ierr = 4 Mandatory keyword not found
c
c --- CALLED ROUTINES -----------------------------------------------------
c
c subroutine FTMAHD      : FITSIO routine to move to extension header
c subroutine FTGKYj      : FITSIO routine to read extension header keyword,
c                          where the j, is for an integer
c subroutine FTGKNS      : FITSIO routine to read extension header keyword,
c                          where a rootstring is given, thus an array of
c                          keywords can be read
c subroutine FCECHO      : FTOOLS routine to write to screen
c subroutine WT_FERRMSG  : Writes FITSIO error text if required
c
c --- COMPILATION AND LINKING ---------------------------------------------
c
c Link with FTOOLS - FITSIO, CALLIB
c
c --- AUTHORS/MODIFICATION HISTORY ----------------------------------------
c
c Rehana Yusaf (1994 May 16) 1.0.0; original, primary intended use with 
c                                   PCFILT code
c Rehana Yusaf (1994 May 27) 1.0.1; mjd_int,mjd_frac, and gha are also
c                                   read
c
       character(5) version
       parameter (version = '1.0.1' )
*-
c ________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(30) errstr
      character(70) subinfo,errinfo
      character(40) comm
      integer status,colnum
      integer felem,frow,inull
      real*8 enull
      logical anyflg,foundcol
c
c --- USER INFO ---
c
       ierr = 0
       IF (chatter.GE.15) THEN
         subinfo =' ... using RDEPRD Ver '//version
         call fcecho(subinfo)
       ENDIF 
c
c --- READING KEYWORDS ---
c

c READ NAXIS2 

       status = 0
       call ftgkyj(iunit,'NAXIS2',n_ep,comm,status)
       errinfo = errstr//' reading NAXIS2'
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF
       IF (chatter.GE.20) THEN
          write(subinfo,'(A,i12)') 
     &		'   ... Number of records found = ', n_ep
          call fcecho(subinfo)
       ENDIF

c check that array dimensions are large enough 

       IF (n_ep.GT.max_ep) THEN
         errinfo = errstr//' MAX_EP array dimensions are too small !'
         call fcecho(errinfo)
         ierr = 5
         return
       ENDIF
c
c --- READING DATA ---
c
c CHECK TO FIND MJD_INT COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'MJD_INT',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'MJD_INT column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING MJD_INT COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_ep,inull,mjd_int,
     &             anyflg,status)
       errinfo = errstr//' reading mjd_int column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND MJD_FRAC COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'MJD_FRAC',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'MJD_FRAC column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING MJD_FRAC COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_ep,enull,mjd_frac,
     &             anyflg,status)
       errinfo = errstr//' reading mjd_frac column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND SUN_X COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'SUN_X',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'SUN_X column not present in EPHEM ext' 
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF      

c READING SUN_X COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_ep,enull,sun_x,
     &             anyflg,status)
       errinfo = errstr//' reading sun_x column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF              

c CHECK TO FIND SUN_Y COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'SUN_Y',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'SUN_Y column not present in EPHEM ext' 
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF       

c READING SUN_Y COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_ep,enull,sun_y,
     &             anyflg,status)
       errinfo = errstr//' reading SUN_Y column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND SUN_Z COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'SUN_Z',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'SUN_Z column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING SUN_Z COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_ep,enull,sun_z,
     &             anyflg,status)
       errinfo = errstr//' reading SUN_Z column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF                       

c CHECK TO FIND MOON_X COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'MOON_X',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'MOON_X column not present in EPHEM ext' 
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING MOON_X COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_ep,enull,moon_x,
     &             anyflg,status)
       errinfo = errstr//' reading MOON_X column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF
          
c CHECK TO FIND MOON_Y COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'MOON_Y',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'MOON_Y column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING MOON_Y COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_ep,enull,moon_y,
     &             anyflg,status)
       errinfo = errstr//' reading MOON_Y column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND MOON_Z COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'MOON_Z',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'MOON_Z column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING MOON_Z COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_ep,enull,moon_z,
     &             anyflg,status)
       errinfo = errstr//' reading MOON_Z column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND LON_EAST COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'LON_EAST',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'LON_EAST column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING LON_EAST COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_ep,enull,lon_east,
     &             anyflg,status)
       errinfo = errstr//' reading LON_EAST column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND LAT_NORT COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'LAT_NORT',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'LAT_NORT column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING LAT_NORT COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_ep,enull,lat_nort,
     &             anyflg,status)
       errinfo = errstr//' reading LAT_NORT column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND ALT_SAT COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'ALT_SAT',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'ALT_SAT column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING ALT_SAT COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_ep,enull,alt_sat,
     &             anyflg,status)
       errinfo = errstr//' reading ALT_SAT column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND SAT_X COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'SAT_X',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'SAT_X column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING SAT_X COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_ep,inull,sat_x,
     &             anyflg,status)
       errinfo = errstr//' reading sat_x column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND SAT_Y COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'SAT_Y',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'SAT_Y column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING SAT_Y COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_ep,inull,sat_y,
     &             anyflg,status)
       errinfo = errstr//' reading sat_y column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND SAT_Z COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'SAT_Z',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'SAT_Z column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING SAT_Z COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_ep,inull,sat_z,
     &             anyflg,status)
       errinfo = errstr//' reading sat_z column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND GHA COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'GHA',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'GHA column not present in EPHEM ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING GHA COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_ep,enull,gha,
     &             anyflg,status)
       errinfo = errstr//' reading gha column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

       IF (chatter.GE.20) THEN
         subinfo = '      ... data has been read'
         call fcecho(subinfo)
       ENDIF
       return
       end
c ------------------------------------------------------------------------
c     END OF SUBROUTINE RDEPRD 
c ------------------------------------------------------------------------

