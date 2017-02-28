
*+RHMVRD
c     -------------------------------------------------------------
      subroutine rhmvrd(iunit,n_evr,max_evr,time,livt_cor,
     &                  chatter,ierr)
c     -------------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads an HRI FITS RDF format EVRATE qualified 
c Event rate extension
c NOTE : Assumes file is already open. 
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c TIME      : SC clock time (secs)
c LIVT_COR  : Live-time correction factor
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_evr,n_evr,ierr,chatter
      real*8 time(max_evr)
      real livt_cor(max_evr)
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c max_evr    int    : Array dimensions
c iunit      int    : Fortran unit number for file
c chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c n_evr      int    : Counter event rate data 
c                     (IF NE 0 on entry then that many evr entries already
c                      aaumed to have been read in (fron prevoius ext') 
c 
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
c Rehana Yusaf (21 MArch 1994) 1.0.0; RDMVU0 used as basis
c Peter Wilson (Jan 19 1999) 1.0.1; Add a dnull variable to pass to ftgcvd
c
       character(5) version
       parameter (version = '1.0.1' )
*-
c _________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(30) errstr
      character(70) subinfo,errinfo
      character(40) comm
      character(8) extname
      integer status,colnum
      integer felem,frow
      real enull
      real*8 dnull
      logical anyflg,foundcol

c
c      --- USER INFO ---
c
       ierr = 0
       IF (chatter.GE.15) THEN
         subinfo =' ... using RHMVRD Ver '//version
         call fcecho(subinfo)
       ENDIF 
c
c     --- READING KEYWORDS ---
c

c READ NAXIS2 

       status = 0
       call ftgkyj(iunit,'NAXIS2',n_evr,comm,status)
       errinfo = errstr//' reading NAXIS2'
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF
       IF (chatter.GE.20) THEN
         write(subinfo,'(A,i12)')
     &  '   ... Number of records found = ',n_evr
         call fcecho(subinfo)
       ENDIF

c check that array dimensions are large enough 

       IF (n_evr.GT.max_evr) THEN
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
          errinfo=errstr//'TIME column not present in '//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING TIME COLUMN

       frow=1
       felem=1
       dnull=0.0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_evr,dnull,time,
     &             anyflg,status)
       errinfo = errstr//' reading TIME column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND LIVT_COR COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'LIVT_COR',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'LIVT_COR column not present in '
     &//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF      

c READING LIVT_COR COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,n_evr,enull,livt_cor,
     &             anyflg,status)
       errinfo = errstr//' reading MV_ACO column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF              

       IF (chatter.GE.20) THEN
         subinfo = '      ... EVR data has been read'
         call fcecho(subinfo)
       ENDIF
       return
       end

c ------------------------------------------------------------------------
c     END OF SUBROUTINE RHMVRD 
c ------------------------------------------------------------------------


