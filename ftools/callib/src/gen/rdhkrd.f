
*+RDHKRD
c     ---------------------------------------------------------
      subroutine rdhkrd(iunit,n_hkp,max_hkp,hkp_time,miss,temp,
     &                  press,hvolt,filpos,chatter,ierr)
c     ---------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads a FITS RDF format HKP extension
c NOTE : Assumes file is already open. 
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c TIME    : Spacecaraft time of value change
c MISS    : Missing housekeeping information
c TEMP    : Temperature of instrument
c PRESS   : Pressure of instrument
c HVOLT   : Instrument high voltage
c FILPOS  : Filter wheel position
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_hkp,n_hkp,ierr,chatter
      real*8 hkp_time(max_hkp)
      integer miss(max_hkp),temp(max_hkp),press(max_hkp)
      integer hvolt(max_hkp),filpos
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c max_hkp    int    : Array dimensions
c iunit      int    : Fortran unit number for file
c chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c n_hkp      int    : Counter for HKP data 
c 
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
c Rehana Yusaf (May 16 1994) 1.0.0;
c Rehana Yusaf (June 9 1994) 1.0.1; FILPOS ni longer read
c Peter Wilson (Dec  4 1997) 1.0.2; enull declared as real*8
c
       character(5) version
       parameter (version = '1.0.2' )
*-
c _________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(30) errstr
      character(70) subinfo,errinfo
      character(40) comm
      integer status,colnum
      real*8 enull
      integer felem,inull,frow
      logical anyflg,foundcol

c
c      --- USER INFO ---
c
       ierr = 0
       IF (chatter.GE.15) THEN
         subinfo =' ... using RDHKRD Ver '//version
         call fcecho(subinfo)
       ENDIF 
c
c     --- READING KEYWORDS ---
c

c READ NAXIS2 

       status = 0
       call ftgkyj(iunit,'NAXIS2',n_hkp,comm,status)
       errinfo = errstr//' reading NAXIS2'
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF
       IF (chatter.GE.20) THEN
         write(subinfo,'(A,i12)')
     &  '   ... Number of records found = ',n_hkp
         call fcecho(subinfo)
       ENDIF

c check that array dimensions are large enough 

       IF (n_hkp.GT.max_hkp) THEN
         errinfo = errstr//' array dimensions are too small !'
         call fcecho(errinfo)
         ierr = 5
         return
       ENDIF
c
c --- READING DATA ---
c

c CHECK TO FIND TIME COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'TIME',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'TIME column not present in HKP ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING TIME COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_hkp,enull,hkp_time,
     &             anyflg,status)
       errinfo = errstr//' reading TIME column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND MISS COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'MISS    ',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'MISS column not present in HKP ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING MISS COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_hkp,inull,miss,
     &             anyflg,status)
       errinfo = errstr//' reading MISS column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND TEMP COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'TEMP',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'TEMP column not present in TEMP ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF      

c READING TEMP COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_hkp,inull,temp,
     &             anyflg,status)
       errinfo = errstr//' reading TEMP column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF              

c CHECK TO FIND PRESS COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'PRESS',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'PRESS column not present in PRESS ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF       

c READING PRESS COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_hkp,inull,press,
     &             anyflg,status)
       errinfo = errstr//' reading PRESS column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF                       

c CHECK TO FIND HVOLT COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'HVOLT',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'HVOLT column not present in HKP ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING HVOLT COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_hkp,inull,hvolt,
     &             anyflg,status)
       errinfo = errstr//' reading HVOLT column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF          

c CHECK TO FIND FILPOS COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'FILPOS',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'FILPOS column not present in HKP ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF


c READING FILPOS COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_hkp,inull,filpos,
     &             anyflg,status)
       errinfo = errstr//' reading FILPOS column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF 

       IF (chatter.GE.20) THEN
         subinfo = '      ... HKP data has been read'
         call fcecho(subinfo)
       ENDIF
       return
       end

c ------------------------------------------------------------------------
c     END OF SUBROUTINE RDHKRD 
c ------------------------------------------------------------------------


