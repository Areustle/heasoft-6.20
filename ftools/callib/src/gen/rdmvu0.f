
*+RDMVU0
c     -------------------------------------------------------------
      subroutine rdmvu0(iunit,n_evr,max_evr,iti_evr,iac_evr,
     &                  iqe_evr,ia1_evr,iax_evr,chatter,ierr)
c     -------------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads a FITS US REV0 format EVRAT Qualified 
c Event rate extension
c NOTE : Assumes file is already open. 
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c ITI_EVR   : Time of event rate
c IAC_EVR   : MV anticoincident rate
c IQE_EVR   : Transmitted X-ray rate
c IA1_EVR   : Events in A1 above low level threshold
c IAX_EVR   : Accepted X-ray rate
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_evr,n_evr,ierr,chatter
      integer*4 iti_evr(max_evr)
      integer*4 iac_evr(max_evr),iqe_evr(max_evr)
      integer*4 ia1_evr(max_evr),iax_evr(max_evr)
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
c Rehana Yusaf (Oct 1 1993) 1.0.0;
c Rehana Yusaf (Jan 11 1996) 1.0.1; add wtinfo and friends
c
       character(5) version
       parameter (version = '1.0.1' )
       character(6) subname
       parameter (subname = 'rdmvu0')
*-
c _________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(70) subinfo,errinfo
      character(40) comm
      character(8) extname
      integer status,htype,colnum
      integer felem,inull,frow
      logical anyflg,foundcol,extfind,qfirst

c
c      --- USER INFO ---
c
       ierr = 0
       subinfo =' using '//subname//' '//version
       call wtinfo(chatter,15,1,subinfo)

       IF (n_evr.LE.0) THEN
         qfirst = .true.
       ELSE
         qfirst = .false.
       ENDIF
c       
c      --- MOVING TO DATA EXTENSION ---
c
       extfind = .false.
       do WHILE(.NOT.extfind)
         call ftmrhd(iunit,1,htype,status)
         extname = '  '
         call ftgkys(iunit,'EXTNAME',extname,comm,status)
         IF (extname.EQ.'EVRAT') THEN 
            extfind = .true.
         ELSE
            IF ((status.EQ.107).OR.(status.EQ.207)) THEN
              IF (qfirst) THEN
                errinfo = ' "EVRAT" EXTENSION NOT FOUND '
                call wterrm(subname,version,errinfo)
              ELSE
                subinfo = ' end of file encoutered'
                call wtinfo(chatter,20,2,subinfo)
              ENDIF
              ierr=status
              return
            ENDIF
         ENDIF
      enddo  
      IF (chatter.GE.20) THEN
        IF (qfirst) THEN
          subinfo = ' moved to '//extname//'extension'
        ELSE
          subinfo = ' found another '//extname//'extension'
        ENDIF
        call wtinfo(chatter,20,2,subinfo)
      ENDIF
c
c     --- READING KEYWORDS ---
c

c READ NAXIS2 

       status = 0
       call ftgkyj(iunit,'NAXIS2',n_evr,comm,status)
       errinfo = ' reading NAXIS2'
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
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
       call ftgcno(iunit,.false.,'ITI_EVR',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=' ITI_EVR column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF

c READING ITI_EVR COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,iti_evr,
     &             anyflg,status)
       errinfo = ' reading ITI_EVR column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND IAC_EVR COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'IAC_EVR',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=' IAC_EVR column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF      

c READING IAC_EVR COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,iac_evr,
     &             anyflg,status)
       errinfo = ' reading ITI_EVR column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF              

c CHECK TO FIND IQE_EVR COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'IQE_EVR',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=' IQE_EVR column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF       

c READING IQE_EVR COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,iqe_evr,
     &             anyflg,status)
       errinfo = ' reading IQE_EVR column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF                       

c CHECK TO FIND IA1_EVR COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'IA1_EVR',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=' IA1_EVR column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF

c READING IA1_EVR COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,ia1_evr,
     &             anyflg,status)
       errinfo = ' reading IA1_EVR column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF          

c CHECK TO FIND IAX_EVR COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'IAX_EVR',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=' IAX_EVR column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF

c READING IAX_EVR COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_evr,inull,iax_evr,
     &             anyflg,status)
       errinfo = ' reading IAX_EVR column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF                    
                          
       subinfo = ' EVR data has been read'
       call wtinfo(chatter,20,2,subinfo)
       return
       end

c ------------------------------------------------------------------------
c     END OF SUBROUTINE RDMVU0 
c ------------------------------------------------------------------------

