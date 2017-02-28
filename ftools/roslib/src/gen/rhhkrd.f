
*+RHHKRD
c     ---------------------------------------------------------
      subroutine rhhkrd(iunit,n_hkp,max_hkp,hkp_time,temp1,
     &                  temp2,temp3,chatter,ierr)
c     ---------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads a FITS HRI RDF format HKP extension
c NOTE : Assumes file is already open. 
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c TIME    : Spacecraft time of value change
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_hkp,n_hkp,ierr,chatter
      real*8 hkp_time(max_hkp)
      real temp1(max_hkp),temp2(max_hkp),temp3(max_hkp)
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
c Rehana Yusaf (Oct 28 1994) 1.0.0;
c
       character(5) version
       parameter (version = '1.0.0' )
*-
c _________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(30) errstr
      character(70) subinfo,errinfo
      character(40) comm
      integer status,colnum
      real enull
      integer felem,inull,frow
      logical anyflg,foundcol

c
c      --- USER INFO ---
c
       ierr = 0
       IF (chatter.GE.15) THEN
         subinfo =' ... using RHHKRD Ver '//version
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

c CHECK TO FIND TEMP1 COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'TEMP1',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'TEMP1 column not present in HKP ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING TEMP1 COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,n_hkp,enull,temp1,
     &             anyflg,status)
       errinfo = errstr//' reading TEMP1 column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND TEMP2 COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'TEMP2',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'TEMP2 column not present in HKP ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF      

c READING TEMP2 COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,n_hkp,enull,temp2,
     &             anyflg,status)
       errinfo = errstr//' reading TEMP2 column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF              

c CHECK TO FIND TEMP3 COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'TEMP3',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'TEMP3 column not present in HKP ext'
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF       

c READING TEMP3 COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,n_hkp,enull,temp3,
     &             anyflg,status)
       errinfo = errstr//' reading TEMP3 column '
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
c     END OF SUBROUTINE RHHKRD 
c ------------------------------------------------------------------------


