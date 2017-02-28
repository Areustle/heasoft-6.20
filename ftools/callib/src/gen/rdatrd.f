*+RDATRD
c     -------------------------------------------------------------
      subroutine rdatrd(iunit,n_att,max_att,time,roan_cas,
     &                 ra_cas,dec_cas,chatter,ierr)
c     -------------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads a FITS RDF format Attitude (ASPECT) extension
c NOTE : Assumes file is already open  at the desired extension.
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c TIME       : Time of corected aspect in full seconds
c ROAN_CAS   : Corrected Roll angle
c RA_CAS     : X-translation in NS-system
c DEC_CAS    : Y-translation in NS-system
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_att,n_att,ierr,chatter
      real*8 time(max_att),roan_cas(max_att)
      real*8 ra_cas(max_att),dec_cas(max_att)
ccc      real*8 time(*),roan_cas(*)
ccc      real*8 ra_cas(*),dec_cas(*)
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c max_att    int    : Array dimensions
c iunit      int    : Fortran unit number for file
c chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c n_att      int i/r: Counter attitude data (IF NE 0 on entry then that 
c		      many attitude entries already assumed to have been 
c		      read in [from previous extensions])
c time       int    : Array of corrected aspect
c roan_cas   int    : Array of roll angle
c ra_cas     int    : Array of X-translation in NS-system
c dec_cas    int    : Array of Y-translation in NS-system 
c ierr       int    : Error flag, ierr = 0 okay
c                                 ierr = 107/207 error finding extension 
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
c Rehana Yusaf (Oct 1 1993) 1.0.0; rdatrd.f used as a basis
c Rehana Yusaf (Jan 11 1996) 1.0.1; add screen display routines
       character(5) version
       parameter (version = '1.0.1' )
       character(6) subname
       parameter (subname = 'rdatrd')
*-
c ________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(70) subinfo,errinfo
      character(40) comm
      character(8) extname
      integer status,colnum
      integer felem,frow
      real*8 enull
      logical anyflg,foundcol
c
c --- USER INFO ---
c
       ierr = 0
       subinfo =' using '//subname//' '//version
       call wtinfo(chatter,15,1,subinfo)
c
c --- READING KEYWORDS ---
c

c READ NAXIS2 

       status = 0
       call ftgkyj(iunit,'NAXIS2',n_att,comm,status)
       errinfo = ' reading NAXIS2'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF
       write(subinfo,'(A,i12)') 
     &		' Number of records found = ', n_att
       call wtinfo(chatter,20,2,subinfo)

c check that array dimensions are large enough 

       IF (n_att.GT.max_att) THEN
         errinfo = ' MAX_ATT array dimensions are too small !'
         call wterrm(subname,version,errinfo)
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
         errinfo=' TIME column not present in '//extname
         call fcecho(errinfo)
         ierr = 2
         return
       ENDIF


c READING TIME COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_att,enull,time,
     &             anyflg,status)
       errinfo = ' reading TIME column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND ROAN_CAS COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'ROAN_CAS',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='ROAN_CAS column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF      

c READING ROAN_CAS COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_att,enull,roan_cas,
     &             anyflg,status)
       errinfo = ' reading roan_cas column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF              

c CHECK TO FIND RA_CAS COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'RA_CAS',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='RA_CAS column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF       

c READING RA_CAS COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_att,enull,ra_cas,
     &             anyflg,status)
       errinfo = ' reading RA_CAS column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF                       

c CHECK TO FIND DEC_CAS COLUMN

       foundcol=.true.
       status = 0
       colnum=0
       call ftgcno(iunit,.false.,'DEC_CAS',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=' DEC_CAS column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF

c READING DEC_CAS COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_att,enull,dec_cas,
     &             anyflg,status)
       errinfo = ' reading DEC_CAS column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF          
       subinfo = ' data has been read'
       call wtinfo(chatter,20,2,subinfo)
       return
       end
c ------------------------------------------------------------------------
c     END OF SUBROUTINE RDATRD 
c ------------------------------------------------------------------------
