
*+RDGTI1
c     --------------------------------------------------------------
      subroutine rdgti1(iunit,n_gti,max_gti,start,stop,chatter,ierr)
c     --------------------------------------------------------------
c
c ___ DESCRIPTION __________________________________________________________
c
c This subroutine reads a FITS GTI file
c NOTE : Assumes file is already open and at correct extension
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c START : Accepted time period
c STOP  : End of accepted time period
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_gti,n_gti,ierr,chatter
      real*8 start(max_gti),stop(max_gti)
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c iunit      int    : Fortran unit number for file
c chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c ierr       int    : Error flag, ierr = 0 okay
c                                 ierr = 2 Column/keyword number not found
c                                 ierr = 3 Error in reading data
c start      int    : array of start times
c stop       int    : array of stop times
c n_gti      int    : Number of times
c max_gti    int    : Maximum array dimension
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
c Rehana Yusaf (Oct 1 1993) 2.0.0;
c Ian M George (2.0.0:1994 Mar 19) ripped out extension searching bit
c Rehana Yusaf (2.0.1:1996 Jan 16) improve screen display, add wtinfo and
c                                  friends.
c Peter Wilson (2.0.2:1997 Dec  8) enull declared as real*8
       character(5) version
       parameter (version = '2.0.2' )
       character(6) subname
       parameter (subname = 'rdgti1')
*-
c _________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(70) subinfo,errinfo
      character(40) comm
      real*8 enull 
      integer status,colnum
      integer felem,frow
      logical anyflg,foundcol
c
c      --- USER INFO ---
c
       ierr = 0
       subinfo =' using '//subname//' '//version
       call wtinfo(chatter,15,1,subinfo)
c
c     --- READING KEYWORDS ---
c

c READ NAXIS2 

       status = 0
       call ftgkyj(iunit,'NAXIS2',n_gti,comm,status)
       errinfo = ' reading NAXIS2'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF

c check that array dimensions are large enough 

       IF (n_gti.GT.max_gti) THEN
         errinfo = ' array dimensions are to small'
         call wterrm(subname,version,errinfo)
	 write(errinfo,'(a,i12)') ' GTI array size  : ',max_gti
         call wtinfo(chatter,0,1,errinfo)
	 write(errinfo,'(a,i12)') ' No. GTIs in data: ',n_gti
         call wtinfo(chatter,0,1,errinfo)
         ierr = 5
         return
       ENDIF
c
c --- READING DATA ---
c

c CHECK TO FIND  COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'START',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=' START column not found'
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF

c READING START COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_gti,enull,start,
     &             anyflg,status)
       errinfo = ' reading START column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND STOP COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'STOP',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='STOP column not found'
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF      

c READING STOP COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_gti,enull,stop,
     &             anyflg,status)
       errinfo = ' reading STOP column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF
      
       subinfo = ' GTIs read succesfully'
       call wtinfo(chatter,20,2,subinfo)
       return
       end
c ------------------------------------------------------------------------
c     END OF SUBROUTINE RDGTI1
c ------------------------------------------------------------------------

