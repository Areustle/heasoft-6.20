
*+RDMVRD
c     -------------------------------------------------------------
      subroutine rdmvrd(iunit,n_evr,max_evr,time,mv_aco,
     &                  xtransm,a1_al,xacc,chatter,ierr)
c     -------------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads a FITS RDF format EVRATE Qualified 
c Event rate extension
c NOTE : Assumes file is already open. 
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c TIME      : Time of event rate
c MV_ACO    : MV anticoincident rate
c XTRANSM   : Transmitted X-ray rate
c A1_AL     : Events in A1 above low level threshold
c XACC      : Accepted X-ray rate
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_evr,n_evr,ierr,chatter
      real*8 time(max_evr)
      integer*4 mv_aco(max_evr),xtransm(max_evr)
      integer*4 a1_al(max_evr),xacc(max_evr)
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
c Rehana Yusaf (26 Jan 1995) 1.0.1; bug fix, XACC_CNT is used instead
c                                   of XACC
c Rehana Yusaf (28 dec 1995) 1.0.2; bug-fix, read XACC instead of XACC_CNT
c                                   add wtinfo and friends
c                                   
       character(5) version
       parameter (version = '1.0.2' )
       character(6) subname
       parameter (subname = 'rdmvrd')
*-
c _________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(70) subinfo,errinfo
      character(40) comm
      character(8) extname
      integer status,colnum
      integer felem,inull,frow
      real*8 enull
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
       call ftgkyj(iunit,'NAXIS2',n_evr,comm,status)
       errinfo = ' reading NAXIS2'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 1
         goto 100
       ENDIF  
       write(subinfo,'(A,i12)')
     &  ' Number of records found = ',n_evr
       call wtinfo(chatter,20,2,subinfo)

c check that array dimensions are large enough 

       IF (n_evr.GT.max_evr) THEN
         errinfo = ' array dimensions are too small !'
         call wterrm(subname,version,errinfo)
         ierr = 5
         return
       ENDIF
c
c --- READING DATA ---
c

c CHECK TO FIND ITI_EVR COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'TIME',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='TIME column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF

c READING TIME COLUMN

       frow=1
       felem=1
       enull=0.0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_evr,enull,time,
     &             anyflg,status)
       errinfo = ' reading TIME column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         goto 100
       ENDIF

c CHECK TO FIND MV_ACO COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'MV_ACO',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='MV_ACO column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          goto 100
       ENDIF      

c READING MV_ACO COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,mv_aco,
     &             anyflg,status)
       errinfo = ' reading MV_ACO column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         goto 100
       ENDIF              

c CHECK TO FIND XTRANSM COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'XTRANSM',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='XTRANSM column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          goto 100
       ENDIF       

c READING XTRANSM COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,xtransm,
     &             anyflg,status)
       errinfo = ' reading XTRANSM column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         goto 100
       ENDIF                       

c CHECK TO FIND A1_AL COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'A1_AL',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='A1_AL column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          goto 100
       ENDIF

c READING A1_AL COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,a1_al,
     &             anyflg,status)
       errinfo = ' reading A1_AL column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         goto 100
       ENDIF          

c CHECK TO FIND XACC COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'XACC',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='XACC_CNT column not present in EVRATE'
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          goto 100
       ENDIF

c READING XACC COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,xacc,
     &             anyflg,status)
       errinfo = ' reading XACC column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         goto 100
       ENDIF                    
       
  100  IF (ierr.NE.0) THEN
         subinfo = ' problem reading EVR dataset'
         call wtinfo(chatter,0,1,subinfo)
       ELSE                   
         subinfo = ' EVR data has been read'
         call wtinfo(chatter,10,3,subinfo)
       ENDIF
       return
       end

c ------------------------------------------------------------------------
c     END OF SUBROUTINE RDMVRD 
c ------------------------------------------------------------------------


