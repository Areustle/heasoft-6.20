*+RDSSS1
c     -----------------------------------------------------------------
      subroutine rdsss1(iunit,maxpha,nchan,tlscop,instrum,detnam,
     &                       filter,object,detchan,texpos,areascal,
     &		             backscal,corscal,backfil,corfil,rmffil,arffil,
     &                       chantyp,hduclas2,fchan,chan,dtype,
     &                       ipha,pha,qerror,error,qsys,sysfrc,qqual,
     &                       qualty,pois,numint,expos,mepoc,
     &                       ierr,chatter)
c     -----------------------------------------------------------------
c --- DESCRIPTION -----------------------------------------------------
c
c This subroutine reads the SPECTRUM extension of an Einstein PHA file
c NOTE : The Einstein SSS spectra is NOT in the OGIP standard format for
c PHA extension. This subroutine was primarily written for a program that
c will convert Einstein SSS spectra to OGIP format.
c In the Einstein format any channels that did not produce usefull data
c have their corresponding quanties set equal to INDEF (IEEE NaN value),
c this routine only stores the usefull channels.
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
c AREASCAL   : Area scaling factor
c BACKSCAL   : Background scaling factor
c CORRSCAL   : Correction scaling factor
c BACKFILE   : Associated background filename
c CORRFILE   : Associated correction filename
c RESPFILE   : Associated response filename
c ANCRFILE   : Associated ancillary response filename
c CHANTYPE   : Channel type - PHA or PI
c DETCHANS   : Number of possible detector channels
c HDUCLAS2   : Describes SPECTRUM - BKG,TOTAL,NET
c TLMIN1     : First possible value of first column (Channel)
c              if TLMIN not found the default value 1 is assumed
c
c COLUMNS READ (if present) ...
c CHANNEL          : Channel numbers
c COUNTS/RATE      : data in counts OR counts per sec
c STAT_ERR_RATE    : Statistical error on data
c SYS_ERR_RATE     : Systematic Error
c DQF              : Channel Quality flags
c
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      integer iunit,chatter,maxpha,nchan,dtype,ierr
      character*(*) tlscop,instrum,filter,chantyp,object
      character*(*) backfil,corfil,rmffil,hduclas2
      character*(*) arffil,detnam
      integer qualty(*),fchan,numint
      integer chan(*),ipha(*),detchan,icurpha
      real error(*),sysfrc(*),pha(*),expos(*),mepoc(*)
      real texpos,areascal,backscal,corscal,curpha
      logical qqual,qerror,qsys,pois
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c Arguments (only those that are not described in keywords read)...
c
c iunit    int     : Fortran file unit number
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
c Rehana Yusaf (1.0.0: 1995 Jan 18)
      character(5) version
      parameter (version = '1.0.0')
*-
c --------------------------------------------------------------------
c 
c --- INTERNALS ---
c
      character(30) errstr,wrnstr,comm
      character(70) subinfo
      character(80) errinfo
      character(8) exname,mename
      integer status,colnum,i,tfields,ivar
      integer frow,felem,inull
      real enull
      character(20) ttype(6),tunit(6),tlmin
      character(5) tform(6)
      logical anyflg
      integer statcol,ratecol,chancol,syscol,qualcol
c
c --- USER INFO ---
c
      errstr = ' ERROR:RDSSS1 Ver '//version//':'
      wrnstr = ' WARNING:RDSSS1 Ver '//version//':'
      IF (chatter.GE.15) THEN
        subinfo = ' ... using RDSSS1 Ver '//version
        call fcecho(subinfo)
      ENDIF 
c
c --- READING KEYWORDS ---
c
      status = 0
      call ftghbn(iunit,6,nchan,tfields,ttype,tform,tunit,
     &            comm,ivar,status)
      errinfo = errstr//' reading binary header info'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 2
        return
      ENDIF

      IF (nchan.GT.maxpha) THEN
        write(subinfo,'(A,i12)')
     &  'array sizes are too small, they should be ',nchan
        errinfo = errstr//subinfo
        call fcecho(errinfo)
        ierr = 3
        return
      ENDIF          

c DETCHANS ...
 
      call ftgkyj(iunit,'DETCHANS',detchan,comm,status)
      IF (status.NE.0) THEN
        detchan = nchan
      ENDIF

c DETERMINE WHICH COLUMNS ARE PRESENT AND SET LOGICALS ACCORDINGLY

      dtype = 0
      qerror = .false.
      qsys = .false.
      qqual = .false.
      do i=1,6
        IF (ttype(i).EQ.'COUNTS') THEN
          dtype = 1
        ELSEIF (ttype(i).EQ.'RATE') THEN
          dtype = 2
        ELSEIF (ttype(i).EQ.'STAT_ERR_RATE') THEN
          qerror = .true.
        ELSEIF (ttype(i).EQ.'SYS_ERR_RATE') THEN
          qsys = .true.
        ELSEIF (ttype(i).EQ.'DQF') THEN
          qqual = .true.
        ENDIF
      enddo

      IF (dtype.EQ.0) THEN
        ierr = 3
        subinfo = errstr//' reading header'
        call fcecho(errinfo)
        return
      ENDIF

c TELESCOPE ...

      status = 0
      tlscop = 'UNKNOWN'
      call ftgkys(iunit,'TELESCOP',tlscop,comm,status)
      errinfo = errstr//' reading TELESCOP'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
      IF (status.EQ.225) THEN
        tlscop = 'UNKNOWN'
      ENDIF

c INSTRUME ...

      status = 0
      instrum = 'UNKNOWN'
      call ftgkys(iunit,'INSTRUME',instrum,comm,status)
      errinfo = errstr//' reading INSTRUME'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
      IF (status.EQ.225) THEN
        instrum = 'UNKNOWN'
      ENDIF     

c HDUCLAS2 ...

      hduclas2 = ' '
      call ftgkys(iunit,'HDUCLAS2',hduclas2,comm,status)
      errinfo = wrnstr//' reading HDUCLAS2'
      IF (chatter.GE.30) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
 
c DETNAM ...

      status = 0
      detnam = '  '
      call ftgkys(iunit,'DETNAM',detnam,comm,status)
      errinfo = wrnstr// ' reading DETNAM keyword'
      IF (chatter.GE.20) THEN
        call wt_ferrmsg(status,errinfo)
      ENDIF
    
c FILTER ...

      status = 0
      filter = ' '
      call ftgkys(iunit,'FILTER',filter,comm,status)
      IF (chatter.GE.30) THEN
        errinfo = wrnstr//' reading FILTER keyword'
        call wt_ferrmsg(status,errinfo)
      ENDIF
      IF (status.NE.0) THEN
        filter = 'NONE'
      ENDIF

c OBJECT

      status = 0
      object = ' '
      call ftgkys(iunit,'OBJECT',object,comm,status)
      IF (chatter.GE.30) THEN
        errinfo = wrnstr//' reading OBJECT keyword'
        call wt_ferrmsg(status,errinfo)
      ENDIF


c EXPOSURE ...

      status = 0
      call ftgkye(iunit,'EXPOSURE',texpos,comm,status)
      IF (chatter.GE.10) THEN
        errinfo = wrnstr//' reading EXPOSURE keyword'
        call wt_ferrmsg(status,errinfo)   
      ENDIF
      IF (status.EQ.225) THEN
        texpos = -1
        errinfo = wrnstr//' Exposure time not found'
        call fcecho(errinfo)
        errinfo = wrnstr//' Exposure set to -1'
        call fcecho(errinfo)
      ENDIF
        
c POISSERR ...

      status = 0
      pois = .false.
      call ftgkyl(iunit,'POISSERR',pois,comm,status)
      IF (chatter.GE.15) THEN
        errinfo = wrnstr//' reading POISSERR keyword'
        call wt_ferrmsg(status,errinfo)   
        errinfo = 'POISERR assumed to be false'
        call fcecho(errinfo)
      ENDIF

c AREASCAL ...

      status = 0
      areascal = 1
      call ftgkye(iunit,'EFFAREA',areascal,comm,status)
      IF (chatter.GE.30) THEN
        errinfo = wrnstr//' reading AREASCAL keyword'
        call wt_ferrmsg(status,errinfo)     
      ENDIF

c BACKSCAL ...

      status = 0
      backscal = 1
      call ftgkye(iunit,'BACKSCAL',backscal,comm,status)
      IF (chatter.GE.30) THEN
        errinfo = wrnstr//' reading BACKSCAL keyword'
        call wt_ferrmsg(status,errinfo)     
      ENDIF

c CORRSCAL ...

      status = 0
      corscal = 1
      call ftgkye(iunit,'CORRSCAL',corscal,comm,status)
      IF (chatter.GE.30) THEN
        errinfo = wrnstr//' reading CORRSCAL keyword'
        call wt_ferrmsg(status,errinfo)     
      ENDIF

c BACKFILE ...

      status = 0
      backfil = 'NONE'
      call ftgkys(iunit,'BACKFILE',backfil,comm,status)
      IF (chatter.GE.20) THEN
        errinfo = wrnstr//' reading BACKFILE keyword'
        call wt_ferrmsg(status,errinfo)     
      ENDIF
      IF (backfil.EQ.'   ') THEN
         backfil = 'NONE'
      ENDIF 

c CORRFILE ...

      status = 0
      corfil = 'NONE'
      call ftgkys(iunit,'CORRFILE',corfil,comm,status)
      IF (chatter.GE.20) THEN
        errinfo = wrnstr//' reading CORRFILE keyword'
        call wt_ferrmsg(status,errinfo) 
      ENDIF
      IF (corfil.EQ.'   ') THEN
        corfil = 'NONE'
      ENDIF

c RESPFILE ...

      status = 0
      rmffil ='NONE'
      call ftgkys(iunit,'RESPFILE',rmffil,comm,status)
      IF (chatter.GE.20) THEN
        errinfo = wrnstr//' reading RESPFILE keyword'
        call wt_ferrmsg(status,errinfo) 
      ENDIF
      IF (rmffil.EQ.'   ') THEN
        rmffil = 'NONE'
      ENDIF

c ANCRFILE ...

      status = 0
      arffil = 'NONE'
      call ftgkys(iunit,'ANCRFILE',arffil,comm,status)
      IF (chatter.GE.20) THEN
        errinfo = wrnstr//' reading ANCRFILE keyword'
        call wt_ferrmsg(status,errinfo) 
      ENDIF
      IF (arffil.EQ.'    ') THEN
        arffil = 'NONE'
      ENDIF

c CHANTYPE ...

      status = 0
      chantyp = 'UNKNOWN'
      call ftgkys(iunit,'CHANTYPE',chantyp,comm,status)
      IF (chatter.GE.20) THEN
        errinfo = wrnstr//' reading CHANTYPE keyword'
        call wt_ferrmsg(status,errinfo) 
      ENDIF

c NEXPOSE ...

      status = 0
      numint = 0
      call ftgkyj(iunit,'NEXPOSE',numint,comm,status) 
      IF (chatter.GE.20) THEN
        errinfo = errstr//' reading NEXPOSE keyword'
        call wt_ferrmsg(status,errinfo)
      ENDIF

c READ EXPOS1 ... EXPOSnumint and MEPOC1 ... MEPOCnumint

      status = 0
      IF (numint.GT.0) THEN
        do i=1,numint
          IF (i.LE.9) THEN
            write(exname,'(a,i1)') 'EXPOS',i
            write(mename,'(a,i1)') 'MEPOC',i
          ELSE
            write(exname,'(a,i2)') 'EXPOS',i
            write(mename,'(a,i2)') 'MEPOC',i
          ENDIF
          call ftgkye(iunit,exname,expos(i),comm,status)
          IF (chatter.GE.5) THEN
            errinfo = errstr//' reading '//exname
            call wt_ferrmsg(status,errinfo)
            status = 0
          ENDIF
          call ftgkye(iunit,mename,mepoc(i),comm,status) 
          IF (chatter.GE.5) THEN
            errinfo = errstr//' reading '//mename 
            call wt_ferrmsg(status,errinfo)
            status = 0
          ENDIF 
        enddo
      ENDIF

c
c --- GET COLUMN NUMBERS ---
c

c CHANNEL ...

      frow = 1
      felem = 1
      status = 0
      call ftgcno(iunit,.false.,'CHANNEL',chancol,status)
      errinfo = errstr//' finding CHANNEL column number'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
        ierr = 4
        return
      ENDIF

      inull = 0
      status = 0
      call ftgcvj(iunit,chancol,frow,felem,1,inull,chan,
     &		  anyflg,status)
      errinfo = errstr//' reading CHANNEL column'
      call wt_ferrmsg(status,errinfo)
      IF (status.NE.0) THEN
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
        IF (chatter.GE.15) THEN
          errinfo = wrnstr//
     &' TLMIN keyword for FCHAN value not found'
          call fcecho(errinfo)
          errinfo = ' ... Default value of FCHAN is 1 '
          call fcecho(errinfo)
        ENDIF
      ENDIF

c COUNTS or RATE COLUMN NUMBER ...

      IF (dtype.EQ.1) THEN
        status = 0
        call ftgcno(iunit,.false.,'COUNTS',ratecol,status)
        errinfo = errstr//' finding COUNTS column number'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 4
          return
        ENDIF
      ELSEIF (dtype.EQ.2) THEN
        status = 0
        call ftgcno(iunit,.false.,'RATE',ratecol,status)
        errinfo = errstr//' finding RATE column number'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 4
          return
        ENDIF
      ENDIF

c STAT_ERR_RATE COLUMN NUMBER ...

      IF (qerror) THEN
        status = 0
        call ftgcno(iunit,.false.,'STAT_ERR_RATE',statcol,status)
        errinfo = errstr//' finding STAT_ERR_RATE column number'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 4
          return
        ENDIF
      ENDIF

c SYS_ERR_RATE COLUMN NUMBER ...

      IF (qsys) THEN
        status = 0
        call ftgcno(iunit,.false.,'SYS_ERR_RATE',syscol,status)
        errinfo = errstr//' finding SYS_ERR_RATE column number'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 4
          return
        ENDIF
      ENDIF

c QUALITY COLUMN NUMBER ...

      IF (qqual) THEN
        call ftgcno(iunit,.false.,'DQF',qualcol,status)
        errinfo = errstr//' finding DQF column number'
        call wt_ferrmsg(status,errinfo)
        IF (status.NE.0) THEN
          ierr = 4
          return
        ENDIF
      ENDIF       

c READ DATA ...

      nchan = 0
      do i=1,detchan
        IF (dtype.EQ.1) THEN
          inull = 1
          status = 0
          felem = 1
          call ftgcvj(iunit,ratecol,i,felem,1,inull,icurpha,
     &            anyflg,status)
          errinfo = errstr//' reading COUNTS column'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 5
            return
          ENDIF
        ELSEIF (dtype.EQ.2) THEN
          enull = 1
          status = 0
          felem = 1
          call ftgcve(iunit,ratecol,i,felem,1,enull,curpha,
     &            anyflg,status)
          errinfo = errstr//' reading RATE column'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 5
            return
          ENDIF
        ENDIF
        IF (.NOT.anyflg) THEN
          nchan = nchan + 1
          IF (dtype.EQ.1) THEN
            ipha(nchan) = icurpha
          ELSE
            pha(nchan) = curpha
          ENDIF
          inull = 0
          status = 0
          call ftgcvj(iunit,chancol,i,felem,1,inull,chan(nchan),
     &            anyflg,status)
          errinfo = errstr//' reading CHANNEL column'
          call wt_ferrmsg(status,errinfo)
          IF (status.NE.0) THEN
            ierr = 5
            return
          ENDIF          
          IF (qerror) THEN
            enull = 0
            status = 0
            felem = 1
            call ftgcve(iunit,statcol,i,felem,1,enull,error(nchan),
     &            anyflg,status)
            errinfo = errstr//' reading STAT_ERR_RATE column'
            call wt_ferrmsg(status,errinfo)
            IF (status.NE.0) THEN
             ierr = 5
             return
            ENDIF 
          ENDIF
          IF (qsys) THEN
            enull = 0
            status = 0
            felem = 1
            call ftgcve(iunit,syscol,i,felem,1,enull,sysfrc(nchan),
     &            anyflg,status)
            errinfo = errstr//' reading SYS_ERR_RATE column'
            call wt_ferrmsg(status,errinfo)
            IF (status.NE.0) THEN
              ierr = 5
              return
            ENDIF
          ENDIF
          IF (qqual) THEN
            inull = 0
            status = 0
            felem = 1
            call ftgcvj(iunit,qualcol,i,felem,1,inull,qualty(nchan),
     &            anyflg,status)
            errinfo = errstr//' reading DQF column'
            call wt_ferrmsg(status,errinfo)
            IF (status.NE.0) THEN
              ierr = 5
              return
            ENDIF
          ENDIF
        ENDIF
      enddo
      IF (chatter.GE.20) THEN
        subinfo = '       ... data has been read'
        call fcecho(subinfo)
      ENDIF
      return
      end
c -----------------------------------------------------------------------
c     END OF RDSSS1
c -----------------------------------------------------------------------
