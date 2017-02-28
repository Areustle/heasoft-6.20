*+RDPHA2
c     -----------------------------------------------------------------
      subroutine rdpha2(iunit,maxpha,nchan,tlscop,instrum,detnam,
     &                       filter,detchan,texpos,qareasc,areascal,
     &                       qbacksc,backscal,
     &		             corscal,backfil,corfil,rmffil,arffil,xflt,
     &                       max_xflt,n_xflt,dmode,chantyp,phaversn,
     &                       hduclas2,fchan,chan,dtype,
     &                       ipha,pha,qerror,error,qsys,sysfrc,qqual,
     &                       qualty,qgroup,group,pois,ierr,chatter)
c     -----------------------------------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c
c This subroutine reads the SPECTRUM extension of a PHA file in 
c PHAVERSN = 1992a format.
c Currently the following formats are supported (see OGIP/92-007a)
c   HDUVERS1 = '1.0.0' 
c   HDUVERS1 = '1.1.0'
c   HDUVERS  = '1.1.0'
c   HDUVERS  = '1.2.0'
c
c NOTE : The file is assumed to be open. It is assumed that the file
c is at the desired SPECTRUM extension.
c Close file using FTCLOS or read another ext'
c
c KEYWORDS READ ...
c
C TELESCOP   : Telescope name
c INSTRUME   : Instrument name
c FILTER     : Filter in use
c POISSERR   : True if Poission errors apply
c EXPOSURE   : Exposure time
c CORRSCAL   : Correction scaling factor
c BACKFILE   : Associated background filename
c CORRFILE   : Associated correction filename
c RESPFILE   : Associated response filename
c ANCRFILE   : Associated ancillary response filename
c XFLTxxxx   : XSPEC filter description
c CHANTYPE   : Channel type - PHA or PI
c DETCHANS   : Number of possible detector channels
c DATAMODE   : Datamode - for ASCA SIS - BRIGHT,FAINT etc 
c PHAVERSN   : PHA file format version
c HDUCLAS2   : Describes SPECTRUM - BKG,TOTAL,NET
c TLMIN1     : First possible value of first column (Channel)
c              if TLMIN not found the default value 1 is assumed
c
c COLUMNS READ (if present) ...
c
c COUNTS/RATE : data in counts OR counts per sec
c STAT_ERR    : Statistical error on data
c SYS_ERR     : Systematic Error
c QUALITY     : Channel Quality flags
c GROUPING    : Channel grouping flags
c AREASCAL    : Area scaling factor
c BACKSCAL    : Background scaling factor
c
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      integer iunit,chatter,maxpha,nchan,dtype,ierr,n_xflt
      integer max_xflt
      character*(*) tlscop,instrum,filter,chantyp,xflt(*)
      character*(*) backfil,corfil,rmffil,dmode,hduclas2
      character*(*) arffil,detnam,phaversn
      integer group(*),qualty(*),fchan
      integer chan(*),ipha(*),detchan
      real error(*),sysfrc(*),pha(*)
      real areascal(*),backscal(*)
      real texpos,corscal
      logical qgroup,qqual,qerror,qsys,pois
      logical qareasc, qbacksc
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c Arguments (only those that are not described in keywords read)...
c
c iunit    int     : Fortran file unit number
c qareasc  logical : true if AREASCAL was read as a vector
c qbacksc  logical : true if BACKSCAL was read as a vector
c qgroup   logical : true if grouping flags have been set
c qerror   logical : true if statistical errors present
c qsys     logical : true if systematic errors present
c pois     logical : true if poisonian errors
c chatter  int     : chattiness flag (>20 verbose)
c maxpha   int     : Maximum array size
c nchan    int     : Number of channels available
c detchan  int     : Number of possible channels
c dtype    int     : Datatype, 1 - counts, 2 - rate
c ipha     int     : Array used for counts (if dtype = 1)
c pha      real    : Array used for rate  ( if dtype = 2)
c ierr     int     : error flag, 0 is okay ...
c                                1 = failed to find extension
c			         2 = failed to read primary header
c 				 3 = array sizes are not large enough
c				 4 = column not found
c				 5 = error reading data
c 				 6 = error reading keyword
c
c --- CALLED ROUTINES ------------------------------------------------
c
c FTMRHD      : (FITSIO) Move to extension
c FTGHBN      : (FITSIO) Get binary header info
c FTGKYx      : (FITSIO) Get keyword value of format x
c FTGCNO      : (FITSIO) Get column number
c FTGCVx      : (FITSIO) Reads data
c WT_FERRMSG  : (CALLIB) Writes fitsio and routine error message
c FCECHO      : Screen write
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c
c   kaa  3/19/01   modification of rdpha1.f version 1.3.3 for HDUVERS=1.2.0
c --------------------------------------------------------------------
      character(5) version
      parameter (version = '1.0.0')
*-
c --------------------------------------------------------------------
c 
c --- INTERNALS ---
c
      INTEGER MAXCOLS
      PARAMETER(MAXCOLS=20)
      character(6) subname
      parameter (subname='rdpha2')
      character(30) comm
      character(70) subinfo,errinfo
      integer status,colnum,i,tfields,ivar
      integer frow,felem,inull
      integer datacode,repeat,width
      real enull, rval
      character(20) ttype(MAXCOLS),tunit(MAXCOLS),tlmin
      character(5) tform(MAXCOLS)
      logical anyflg,qnotint
      integer lenact

	rval = 0.0
	ierr=0

c Initialize
	do i = 1, MAXCOLS
	  ttype(i) = ' '
	enddo

c
c --- USER INFO ---
c
      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,15,1,subinfo)
c
c --- READING KEYWORDS ---
c
      status = 0
      call ftghbn(iunit,MAXCOLS,nchan,tfields,ttype,tform,tunit,
     &            comm,ivar,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     &  ' reading binary header info')
        ierr = 2
        return
      ENDIF

      IF (nchan.GT.maxpha) THEN
        write(subinfo,'(A,i12)')
     &  'array sizes are too small, they should be ',nchan
        call wtinfo(chatter,0,1,subinfo)
        ierr = 3
        return
      ENDIF          
      write(subinfo,'(a,i5)')
     & ' Number of channels :',nchan
      call rmvexsp(subinfo,errinfo) 
      call wtinfo(chatter,20,2,errinfo)

c DETCHANS ...
 
      call ftgkyj(iunit,'DETCHANS',detchan,comm,status)
      call wtfwrn(subname,version,chatter,10,status,
     & ' reading DETCHANS')
      if(status .ne. 0) then
        call wtferr(subname,version,status,
     &  ' reading DETCHANS keyword')
        ierr = 2
        return
      endif
      if(detchan .ne. maxpha) then
        write(subinfo, '(I12)') detchan
        call rmvlbk(subinfo)
        subinfo='DETCHANS keyword is '//subinfo
        call wtwarm(subname,version,chatter,20,subinfo)
        write(subinfo, '(I12)') maxpha
        call rmvlbk(subinfo)
        subinfo='but at most '//subinfo(:lenact(subinfo))//
     &          ' values will be read'
        call wtinfo(chatter,20,1,subinfo)
      endif

c DETERMINE WHICH COLUMNS ARE PRESENT AND SET LOGICALS ACCORDINGLY

      dtype = 0
      qerror = .false.
      qsys = .false.
      qqual = .false.
      qgroup = .false.
      qareasc = .false.
      qbacksc = .false.
      do i=1,MAXCOLS
        IF (ttype(i).EQ.'COUNTS') THEN
          dtype = 1
        ELSEIF (ttype(i).EQ.'RATE') THEN
          dtype = 2
        ELSEIF (ttype(i).EQ.'STAT_ERR') THEN
          qerror = .true.
        ELSEIF (ttype(i).EQ.'SYS_ERR') THEN
          qsys = .true.
        ELSEIF (ttype(i).EQ.'QUALITY') THEN
          qqual = .true.
        ELSEIF (ttype(i).EQ.'GROUPING') THEN
          qgroup =.true.
        ELSEIF (ttype(i).EQ.'AREASCAL') THEN
          qareasc =.true.
        ELSEIF (ttype(i).EQ.'BACKSCAL') THEN
          qbacksc =.true.
        ENDIF
      enddo

      IF (dtype.EQ.0) THEN
        ierr = 3
        call wtferr(subname,version,status,
     &  ' reading primary header')
        return
      ENDIF

c TELESCOPE ...

      status = 0
      tlscop = 'UNKNOWN'
      call ftgkys(iunit,'TELESCOP',tlscop,comm,status)
      call wtfwrn(subname,version,chatter,20,status,
     & ' reading TELESCOP') 
      IF (status.EQ.225) THEN
        tlscop = 'UNKNOWN'
      ENDIF

c INSTRUME ...

      status = 0
      instrum = 'UNKNOWN'
      call ftgkys(iunit,'INSTRUME',instrum,comm,status)
      call wtfwrn(subname,version,chatter,20,status,
     & ' reading INSTRUME')
      IF (status.EQ.225) THEN
        instrum = 'UNKNOWN'
      ENDIF     

c PHAVERSN ...

      status = 0
      phaversn = '  '
      call ftgkys(iunit,'HDUVERS',phaversn,comm,status)
      call wtfwrn(subname,version,chatter,30,status,
     & ' reading HDUVERS/HDUVERS1/PHAVERSN')
      IF (phaversn.EQ.'   ') THEN
        status = 0
        call ftgkys(iunit,'HDUVERS1',phaversn,comm,status)
        call wtfwrn(subname,version,chatter,30,status,
     &  ' reading HDUVERS1')
      ENDIF
      IF (phaversn.EQ.'   ') THEN
        status = 0
        call ftgkys(iunit,'PHAVERSN',phaversn,comm,status)
        call wtfwrn(subname,version,chatter,30,status,
     &  ' reading PHAVERSN')
      ENDIF

c HDUCLAS2 ...

      hduclas2 = ' '
      call ftgkys(iunit,'HDUCLAS2',hduclas2,comm,status)
      call wtfwrn(subname,version,chatter, 30, status,
     & ' reading HDUCLAS2')
 
c DETNAM ...

      status = 0
      detnam = '  '
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      call wtfwrn(subname,version,chatter,20,status,
     & ' reading DETNAM keyword')
    
c FILTER ...

      status = 0
      filter = ' '
      call ftgkys(iunit,'FILTER',filter,comm,status)
      IF (chatter.GE.10) THEN
      call wtfwrn(subname,version,chatter,20,status,
     & ' reading FILTER keyword')
      ENDIF

c TEXPOS ...

      status = 0
      call ftgkye(iunit,'EXPOSURE',texpos,comm,status)
      IF (chatter.GE.10) THEN
        call wtfwrn(subname,version,chatter,10,status,
     &  ' reading EXPOSURE keyword')
      ENDIF
      IF (status.EQ.225) THEN
        texpos = -1
        call wtinfo(chatter,10,1,
     & ' Exposure time not found - exposure set to -1')
      ENDIF
        
c POISSERR ...

      status = 0
      pois = .false.
      call ftgkyl(iunit,'POISSERR',pois,comm,status)
      call wtfwrn(subname,version,chatter,15,status,
     & ' reading POISSERR keyword')
      IF (status.NE.0) THEN
        call wtinfo(chatter,10,1,
     & ' POISERR assumed to be false') 
      ENDIF

c CORRSCAL ...

      status = 0
      corscal = 1
      call ftgkye(iunit,'CORRSCAL',corscal,comm,status)
      call wtfwrn(subname,version,chatter,10,status,
     & ' reading CORRSCAL keyword')

c BACKFILE ...

      status = 0
      backfil = 'NONE'
      call ftgkys(iunit,'BACKFILE',backfil,comm,status)
      call wtfwrn(subname,version,chatter,10,status,
     & ' reading BACKFILE keyword')
      IF (backfil.EQ.'   ') THEN
         backfil = 'NONE'
      ENDIF 

c CORRFILE ...

      status = 0
      corfil = 'NONE'
      call ftgkys(iunit,'CORRFILE',corfil,comm,status)
      call wtfwrn(subname,version,chatter,10,status,
     & ' reading CORRFILE keyword')
      IF (corfil.EQ.'   ') THEN
        corfil = 'NONE'
      ENDIF

c RESPFILE ...

      status = 0
      rmffil ='NONE'
      call ftgkys(iunit,'RESPFILE',rmffil,comm,status)
      call wtfwrn(subname,version,chatter,10,status,
     & ' reading RESPFILE keyword')
      IF (rmffil.EQ.'   ') THEN
        rmffil = 'NONE'
      ENDIF

c ANCRFILE ...

      status = 0
      arffil = 'NONE'
      call ftgkys(iunit,'ANCRFILE',arffil,comm,status)
      call wtfwrn(subname,version,chatter,10,status,
     & ' reading ANCRFILE keyword')
      IF (arffil.EQ.'    ') THEN
        arffil = 'NONE'
      ENDIF

c CHANTYPE ...

      status = 0
      chantyp = 'UNKNOWN'
      call ftgkys(iunit,'CHANTYPE',chantyp,comm,status)
      call wtfwrn(subname,version,chatter,20,status,
     & ' reading CHANTYPE keyword')

c XSPEC FILTER ...

      status = 0
      call ftgkns(iunit,'XFLT',1,max_xflt,xflt,n_xflt,status)
      call wtfwrn(subname,version,chatter,30,status,
     & ' reading XFLT keyword')

c DATAMODE ...

      status = 0
      dmode = ' '
      call ftgkys(iunit,'DATAMODE',dmode,comm,status)
      call wtfwrn(subname,version,chatter,10,status,
     & ' reading DATAMODE keyword')

      call wtinfo(chatter,20,2,' keywords have been read')
c
c --- READ DATA ---
c

c CHANNEL ...

      frow = 1
      felem = 1
      status = 0
      call ftgcno(iunit,.false.,'CHANNEL',colnum,status)
      IF (status.NE.0) THEN
        status = 0
        call ftgcno(iunit,.false.,' CHANNEL',colnum,status)
      ENDIF
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     & ' finding CHANNEL column number')
        ierr = 4
        return
      ENDIF

      inull = 0
      status = 0
      call ftgcvj(iunit,colnum,frow,felem,nchan,inull,chan,
     &		  anyflg,status)
      IF (status.NE.0) THEN
        call wtferr(subname,version,status,
     & ' reading CHANNEL column')
        ierr = 5
        return
      ENDIF

c READ TLMINx keyword, where x is CHANNEL COLUMN NUMBER TO DETERMINE
c FIRST POSSIBLE CHANNEL VALUE

      write(tlmin,'(a,i12)') 'TLMIN',colnum
      call crmvblk(tlmin)
      status = 0
      call ftgkyj(iunit,tlmin,fchan,comm,status)
      IF (status.EQ.202) THEN
        IF (chan(1).EQ.0) THEN
          fchan = 0
        ELSE
          fchan = 1
        ENDIF
        call wtfwrn(subname,version,chatter,15,status,
     & ' TLMIN keyword for FCHAN value not found')
        IF (status.NE.0) THEN
          call wtinfo(chatter,15,1,' Default value of FCHAN is 1')
        ENDIF
      ENDIF

c COUNTS or RATE ...

      IF (dtype.EQ.1) THEN
        status = 0
        call ftgcno(iunit,.false.,'COUNTS',colnum,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' finding COUNTS column number')  
          ierr = 4
          return
        ENDIF

c Read the COUNTS column, checking the values are integer

        enull = 0.0
        status = 0
        frow = 1
        felem = 1
        call ftgcve(iunit,colnum,frow,felem,nchan,inull,pha,
     &            anyflg,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' reading COUNTS column')
          ierr = 5
          return
        ENDIF                   

        qnotint = .FALSE.
        DO i = 1, nchan
           ipha(i) = INT(pha(i))
           IF ( ABS(ipha(i)-pha(i)) .GT. 1e-5 ) qnotint = .TRUE.
        ENDDO
        IF ( qnotint ) THEN
           call wtinfo(chatter,0,1,
     & ' **WARNING: COUNTS column is non-integer and will be truncated')
        ENDIF

c RATE...

      ELSEIF (dtype.EQ.2) THEN
        status = 0
        call ftgcno(iunit,.false.,'RATE',colnum,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' finding RATE column number')
          ierr = 4
          return
        ENDIF
  
        enull = 0
        status = 0
        frow = 1
        felem = 1
        call ftgcve(iunit,colnum,frow,felem,nchan,enull,pha,
     &            anyflg,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' reading RATE column')
          ierr = 5
          return
        ENDIF            
      ENDIF

c STAT_ERR ...

      IF (qerror) THEN
        status = 0
        call ftgcno(iunit,.false.,'STAT_ERR',colnum,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' finding STAT_ERR column number')
          ierr = 4
          return
        ENDIF

        enull = 0
        status = 0
        frow = 1
        felem = 1
        call ftgcve(iunit,colnum,frow,felem,nchan,enull,error,
     &            anyflg,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' reading STAT_ERR column')
          ierr = 5
          return
        ENDIF             
      ELSEIF(.not. pois) then
        status = 0
        call ftgkye(iunit,'STAT_ERR',rval,comm,status)
        IF (status.NE.0) THEN
	  status = 0
	ELSE
	  do i = 1, maxpha
		error(i) = rval
	  enddo
	  qerror = .true.
        ENDIF
      ENDIF

c SYS_ERR ...

      IF (qsys) THEN
        status = 0
        call ftgcno(iunit,.false.,'SYS_ERR',colnum,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' finding SYS_ERR column number')
          ierr = 4
          return
        ENDIF

        enull = 0
        status = 0
        frow = 1
        felem = 1
        call ftgcve(iunit,colnum,frow,felem,nchan,enull,sysfrc,
     &            anyflg,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' reading SYS_ERR column')
          ierr = 5
          return
        ENDIF             
      ELSE
        status = 0
        call ftgkye(iunit,'SYS_ERR',rval,comm,status)
        IF (status.NE.0) THEN
	  status = 0
	ELSE
          IF ( rval .NE. 0. ) THEN
   	    do i = 1, maxpha
	      sysfrc(i) = rval
	    enddo
	    qsys = .true.
          ENDIF
        ENDIF
      ENDIF

c QUALITY ...

      IF (qqual) THEN
        status = 0
        call ftgcno(iunit,.false.,'QUALITY',colnum,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' finding QUALITY column number')
          ierr = 4
          return
        ENDIF

        inull = 0
        status = 0
        frow = 1
        felem = 1
        call ftgcvj(iunit,colnum,frow,felem,nchan,inull,qualty,
     &            anyflg,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' reading QUALITY column')
          ierr = 5
          return
        ENDIF             
      ELSE
        status = 0
        call ftgkyj(iunit,'QUALITY',ivar,comm,status)
        IF (status.NE.0) THEN
	  status = 0
	ELSE
	  do i = 1, maxpha
		qualty(i) = ivar
	  enddo
	  qqual = .true.
        ENDIF
      ENDIF

c GROUPING ...

      IF (qgroup) THEN
        status = 0
        call ftgcno(iunit,.false.,'GROUPING',colnum,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' finding GROUPING column number')
          ierr = 4
          return
        ENDIF

        inull = 0
        status = 0
        frow = 1
        felem = 1
        call ftgcvj(iunit,colnum,frow,felem,nchan,inull,group,
     &            anyflg,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' reading GROUPING column')
          ierr = 5
          return
        ENDIF             
      ENDIF

c AREASCAL ...

      IF (qareasc) THEN

        status = 0
        call ftgcno(iunit,.false.,'AREASCAL',colnum,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' finding AREASCAL column number')
          ierr = 4
          return
        ENDIF

        enull = 0.
        status = 0
        frow = 1
        felem = 1
        call ftgcve(iunit,colnum,frow,felem,nchan,enull,areascal,
     &            anyflg,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' reading AREASCAL column')
          ierr = 5
          return
        ENDIF             

c If the keyword only exists then replicate into areascal array

      ELSE

         status = 0
         areascal(1) = 1.
         call ftgkye(iunit,'AREASCAL',areascal(1),comm,status)
         call wtfwrn(subname,version,chatter,10,status,
     &               ' reading AREASCAL keyword')
         DO i = 2, nchan
            areascal(i) = areascal(1)
         ENDDO

      ENDIF

      status = 0


c BACKSCAL ...

      IF (qbacksc) THEN

        status = 0
        call ftgcno(iunit,.false.,'BACKSCAL',colnum,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' finding BACKSCAL column number')
          ierr = 4
          return
        ENDIF

        enull = 0.
        status = 0
        frow = 1
        felem = 1
        call ftgcve(iunit,colnum,frow,felem,nchan,enull,backscal,
     &            anyflg,status)
        IF (status.NE.0) THEN
          call wtferr(subname,version,status,
     &     ' reading BACKSCAL column')
          ierr = 5
          return
        ENDIF             

c If the keyword only exists then replicate into backscal array

      ELSE

         status = 0
         backscal(1) = 1.
         call ftgkye(iunit,'BACKSCAL',backscal(1),comm,status)
         call wtfwrn(subname,version,chatter,10,status,
     &               ' reading BACKSCAL keyword')
         DO i = 2, nchan
            backscal(i) = backscal(1)
         ENDDO

      ENDIF

      call wtinfo(chatter,20,2,
     & ' data has been read')

      return
      end
c -----------------------------------------------------------------------
c     END OF RDPHA2
c -----------------------------------------------------------------------
