*+RHLTU0
c     -------------------------------------------------------------
      subroutine rhltu0(iunit,n_lt,max_lt,ltf_stime,
     &                 lt_corr_fact,chatter,ierr)
c     -------------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads a FITS HRI US REV0 format LTF extension
c NOTE : Assumes file is already open. 
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c LIF_START_TIME 
c LIVE_TIME_CORR_FACTOR
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_lt,n_lt,ierr,chatter
      real*8 ltf_stime(max_lt)
      real lt_corr_fact(max_lt)
c
c --- VARIABLE DIRECTORY --------------------------------------------------
c
c Arguments ...
c
c max_att    int    : Array dimensions
c iunit      int    : Fortran unit number for file
c chatter    int    : Chatter flag ( <5 quiet,>5 normal,>20 noisy)
c n_lt       int i/r: Counter ltf data (IF NE 0 on entry then that 
c		      many attitude entries already assumed to have been 
c		      read in [from previous extensions])
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
c Rehana Yusaf (Aug 29 1994) 1.0.0; written for HRIEXPMAP
c
       character(5) version
       parameter (version = '1.0.0' )
*-
c ________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(30) errstr,wrnstr
      character(70) subinfo,errinfo
      character(40) comm
      character(8) extname
      real enull
      integer status,htype,colnum
      integer felem,frow
      logical anyflg,foundcol,extfind, qfirst
c
c --- USER INFO ---
c
       ierr = 0
       IF (chatter.GE.15) THEN
         subinfo =' ... using RHLTU0 Ver '//version
         call fcecho(subinfo)
       ENDIF 
c
c --- IMG - set logical if this is first set of attitude data
c
       IF (n_lt.LE.0) THEN
	 qfirst = .true.
       ELSE
	 qfirst = .false.
       ENDIF
c       
c --- MOVING TO DATA EXTENSION ---
c
       errstr =' ERROR:RHLTU0 Ver '//version//':'
       wrnstr =' WARNING:RHLTU0 Ver '//version//':'
       extfind = .false.
       do WHILE(.NOT.extfind)
         status = 0
         call ftmrhd(iunit,1,htype,status)
         extname = '   '
         call ftgkys(iunit,'EXTNAME',extname,comm,status)
         IF (extname.EQ.'LTF') THEN 
            extfind = .true.
         ELSE
            IF ((status.EQ.107).OR.(status.EQ.207)) THEN
		IF (qfirst) then
                  errinfo = errstr//' NO "LTF" EXTENSION FOUND '
                  call fcecho(errinfo)
		ELSE
		   IF (chatter.GE.20) THEN
                     subinfo = ' ... end of file encountered'
                     call fcecho(subinfo)
		   ENDIF 
	        ENDIF
                ierr=status
                return
            ENDIF
         ENDIF
      enddo  
      IF (chatter.GE.20) THEN
	IF (qfirst) THEN
          subinfo = '   ... moved to '//extname//'extension'
	ELSE
          subinfo = '   ... found another '//extname//'extension'
	ENDIF
        call fcecho(subinfo)
      ENDIF
c
c --- READING KEYWORDS ---
c

c READ NAXIS2 

       status = 0
       call ftgkyj(iunit,'NAXIS2',n_lt,comm,status)
       errinfo = errstr//' reading NAXIS2'
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 4
         return
       ENDIF
       IF (chatter.GE.20) THEN
          write(subinfo,'(A,i12)') 
     &		'   ... Number of records found = ', n_lt
          call fcecho(subinfo)
       ENDIF

c check that array dimensions are large enough 

       IF (n_lt.GT.max_lt) THEN
         errinfo = errstr//' MAX_LT array dimensions are too small !'
         call fcecho(errinfo)
         ierr = 5
         return
       ENDIF
c
c --- READING DATA ---
c

c CHECK TO FIND LTF_START_TIME COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'LTF_START_TIME',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'LTF_START_TIME column not present in '
     &//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING LTF_START_TIME COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcvd(iunit,colnum,frow,felem,n_lt,enull,ltf_stime,
     &             anyflg,status)
       errinfo = errstr//' reading ASPECT_TIME column '
       call wt_ferrmsg(status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF


c CHECK TO FIND LIVE_TIME_CORR_FACTOR COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'LIVE_TIME_CORR_FACTOR',
     &colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=errstr//'LIVE_TIME_CORR_FACTOR '
     &//'column not present in '//extname
          call fcecho(errinfo)
          ierr = 2
          return
       ENDIF

c READING LIVE_TIME_CORR_FACTOR COLUMN

       frow=1
       felem=1
       enull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,n_lt,enull,
     &             lt_corr_fact,anyflg,status)
       errinfo = errstr//' reading LIVE_TIME_CORR_FACTOR column '
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
c     END OF SUBROUTINE RHLTU0 
c ------------------------------------------------------------------------


