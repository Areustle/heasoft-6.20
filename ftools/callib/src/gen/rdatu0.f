*+RDATU0
c     -------------------------------------------------------------
      subroutine rdatu0(iunit,n_att,max_att,it1_cas,it2_cas,
     &                 iro_cas,ixn_cas,iyn_cas,chatter,ierr)
c     -------------------------------------------------------------
c
c ___ DESCRIPTION _________________________________________________________
c
c This subroutine reads a FITS US REV0 format Attitude extension
c NOTE : Assumes file is already open. 
c        ... close file at end, using FTCLOS, or
c        ... read another extension
c
c Columns read are ...
c
c IT1_CAS    : Time of corected aspect in full seconds
c IT2_CAS    : Time of corrected aspect in sub-seconds
c IRO_CAS    : Corrected Roll angle
c IXN_CAS    : X-translation in NS-system
c IYN_CAS    : Y-translation in NS-system
c
c ___ VARIABLES ____________________________________________________________
c
      IMPLICIT NONE
      integer iunit,max_att,n_att,ierr,chatter
      integer*4 it1_cas(max_att),iro_cas(max_att)
      integer*4 ixn_cas(max_att)
      real iyn_cas(max_att)
      integer*4 it2_cas(max_att)
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
c it1_cas    int    : Array of corrected aspect
c it2_cas    int    : Array of sub-seconds of corrected aspect
c iro_cas    int    : Array of roll angle
c ixn_cas    int    : Array of X-translation in NS-system
c iyn_cas    real   : Array of Y-translation in NS-system 
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
c Georgie Boy  (1.0.1:93 Oct 07), minor cosmetics n stuff
c Rehana Yusaf (1.0.2:94 Mar 29) ; bug-fix, iyn_cas read as real not integer
c Rehana Yusaf (1.0.3:96 Jan 11) ; add screen display routines, wtinfo et al
       character(5) version
       parameter (version = '1.0.3' )
       character(6) subname
       parameter (subname = 'rdatu0')
*-
c ________________________________________________________________________
c
c --- INTERNAL VARIABLES ---
c
      character(70) subinfo,errinfo
      character(40) comm
      character(8) extname
      integer status,htype,colnum
      integer felem,inull,frow
      logical anyflg,foundcol,extfind, qfirst
c
c --- USER INFO ---
c
       ierr = 0
       subinfo =' using '//subname//' '//version
       call wtinfo(chatter,15,1,subinfo)
c
c --- IMG - set logical if this is first set of attitude data
c
       IF (n_att.LE.0) THEN
	 qfirst = .true.
       ELSE
	 qfirst = .false.
       ENDIF
c       
c --- MOVING TO DATA EXTENSION ---
c
       extfind = .false.
       do WHILE(.NOT.extfind)
         status = 0
         call ftmrhd(iunit,1,htype,status)
         extname = '   '
         call ftgkys(iunit,'EXTNAME',extname,comm,status)
         IF (extname.EQ.'CORAS') THEN 
            extfind = .true.
         ELSE
            IF ((status.EQ.107).OR.(status.EQ.207)) THEN
		IF (qfirst) then
                  errinfo = ' CORAS extension no found'
                  call wtferr(subname,version,status,errinfo)
		ELSE
                   subinfo = ' end of file encountered'
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

c CHECK TO FIND IT1_CAS COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'IT1_CAS',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='IT1_CAS column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF

c READING IT1_CAS COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_att,inull,it1_cas,
     &             anyflg,status)
       errinfo = ' reading IT1_CAS column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND IT2_CAS COLUMN 

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'IT2_CAS',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo='IT2_CAS column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF

c READING IT2_CAS COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_att,inull,it2_cas,
     &             anyflg,status)
       errinfo = ' reading IT2_CAS column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF

c CHECK TO FIND IRO_CAS COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'IRO_CAS',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=' IRO_CAS column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF      

c READING IRO_CAS COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_att,inull,iro_cas,
     &             anyflg,status)
       errinfo = ' reading IRO_CAS column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF              

c CHECK TO FIND IXN_CAS COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'IXN_CAS',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=' IXN_CAS column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF       

c READING IXN_CAS COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcvj(iunit,colnum,frow,felem,n_att,inull,ixn_cas,
     &             anyflg,status)
       errinfo = ' reading IXN_CAS column '
       call wtferr(subname,version,status,errinfo)
       IF (status.NE.0) THEN
         ierr = 3
         return
       ENDIF                       

c CHECK TO FIND IYN_CAS COLUMN

       foundcol=.true.
       status = 0
       call ftgcno(iunit,.false.,'IYN_CAS',colnum,status)
       IF (status.NE.0) THEN
         foundcol=.false.
       ENDIF
       IF (.NOT.foundcol) THEN
          errinfo=' IYN_CAS column not present in '//extname
          call wtferr(subname,version,status,errinfo)
          ierr = 2
          return
       ENDIF

c READING IYN_CAS COLUMN

       frow=1
       felem=1
       inull=0
       status=0
       call ftgcve(iunit,colnum,frow,felem,n_att,inull,iyn_cas,
     &             anyflg,status)
       errinfo = ' reading IYN_CAS column '
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
c     END OF SUBROUTINE RDATU0 
c ------------------------------------------------------------------------


