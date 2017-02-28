      subroutine update(infile,image,cfile,nfiles,outfile,minexpo,
     &     fracavg,exposure,status)

C EXOSAT Update Header
C modifies/appends keywords in Lightcurve or Image FITS headers
C  I     infile   Name of input FITS file
C  I/O   outfile  FITS filename which will be updated
C  I     image    Logical TRUE if outfile is an image FITS file
C  I     cfile    Array of FITS event filenames
C  I     minexpo  Fractional exposure cutoff
C  I     fracavg  Mean deadtime  correction
C  I     exposure Effective Exposure time corrected for deadtime
C

      implicit none

C Input/Output  variables
      CHARACTER*(*) infile,outfile
      character(160) cfile(100)
      LOGICAL image
      INTEGER*4 nfiles,status,telapse
      REAL*8 minexpo,exposure,tstop,tstart
      REAL*4 fracavg

C Local variables

      INTEGER*4 readwrite,iounit,blocksize,i
      REAL*4 equin
      REAL*8 dum
      CHARACTER errm*255,subname*50,context*160,comment*80
      CHARACTER errtxt*30,stat*15
      LOGICAL deadcor

      if(status.ne.0) return

      subname = 'update:'
      readwrite=1
      status=0
      equin=0.0
      stat=' '
      errtxt=' '
      CALL FTGIOU(iounit,status)
      CALL FTOPEN(iounit,outfile,readwrite,blocksize,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't open "//outfile//" for writing"
         goto 999
      endif
C
C Modify Primary Header
C
      CALL FTGKYE(iounit,'EQUINOX',equin,comment,status)
      if(status.eq.0) then
C If EQUINOX is present 
C Delete all keywords EQUINOX and replace with EQUINOX= 1950.0
         do while(status.eq.0)
            CALL FTDKEY(iounit,'EQUINOX',status)
            if(status.ne.0) then
               context=" "
            endif
         enddo
         status=0
C Add EQUINOX keyword
         CALL FTPKYE(iounit,'EQUINOX',1950.0,4,
     &        'Epoch of Coordinate System',status)
      endif
      status=0
C Modify TELAPSE = ( TSTOP - TSTART )
      CALL FTGKYJ(iounit,'TELAPSE',telapse,comment,status)
      if(status.eq.0) then
         CALL FTGKYD(iounit,'TSTOP',tstop,comment,status)
         CALL FTGKYD(iounit,'TSTART',tstart,comment,status)
         telapse=INT(tstop-tstart)
         comment='&'
         CALL FTMKYJ(iounit,'TELAPSE',telapse,comment,status)
      endif
      status=0
C Modify DEADAPP
      deadcor=.true.
      comment='&'
      CALL FTMKYL(iounit,'DEADAPP',deadcor,comment,status)      
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't write DEADAPP"
         goto 999
      endif
C Delete comments
      do while(status.eq.0)
         CALL FTDKEY(iounit,'COMMENT',status)
      enddo
      status=0
C Mean deadtime correction
      comment='Deadtime due to electronics'
      CALL FTMKYE(iounit,'DEADC',fracavg,4,comment,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't write DEADC"
         goto 999
      endif
C Exposure time is SUM(IntTime*fracexp)
      comment='Effective Exposure time (corrected for deadtime)'
      CALL FTMKYD(iounit,'EXPOSURE',exposure,8,comment,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't write EXPOSURE"
         goto 999
      endif
C Delete DATAMODE, ONTIME, LIVETIME from Primary Header
      CALL FTDKEY(iounit,'DATAMODE',status)
      if(status.ne.0) status=0
c      CALL FTDKEY(iounit,'ONTIME',status)
c      if(status.ne.0) status=0
      CALL FTDKEY(iounit,'LIVETIME',status)
      if(status.ne.0) status=0
C Display as Comments all the user inputs
      comment=' '
      comment="Input File: "//infile
      CALL RMVXBK(comment)
      CALL FTPCOM(iounit,comment,status)

      comment=' '
      comment="Output File: "//outfile
      CALL RMVXBK(comment)
      CALL FTPCOM(iounit,comment,status)


      comment=' '
      write(comment,*)minexpo
      comment="Minimum Exposure Cutoff: "//comment
      CALL RMVXBK(comment)
      CALL FTPCOM(iounit,comment,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)         
         context="Can't write min. expo. comment"
         goto 999
      endif
      do i=1,nfiles
         comment=' '
         comment="Event File: "//cfile(i)
         CALL RMVXBK(comment)
         CALL FTPCOM(iounit,comment,status)
         comment=' '
      enddo
C Update the Checksum and Datasum
      CALL FTPCKS(iounit,status)
      CALL FTPCKS(iounit,status)

      if(status.ne.0) then
         CALL FTGERR(status,errtxt)         
         context="Can't update checksum datasum"
         goto 999
      endif

C*********************************************
C Move to rate extension
C*********************************************
      CALL FTMNHD(iounit,2,'RATE',0,status)
      if(status.ne.0) then
C No extension is present
         CALL FTGERR(status,errtxt)
         context="Can't move to first extension "
         goto 999
      endif
C Modify DEADAPP keyword
      deadcor=.true.
      comment='&'
      CALL FTMKYL(iounit,'DEADAPP',deadcor,comment,status)      
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't write DEADAPP"
         goto 999
      endif
C Modify DEADC leyword
      comment='Deadtime due to electronics'
      CALL FTMKYE(iounit,'DEADC',fracavg,4,comment,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't write DEADC"
         goto 999
      endif
C Exposure time is SUM(IntTime*fracexp)
      comment='Effective Exposure time (corrected for deadtime)'
      CALL FTMKYD(iounit,'EXPOSURE',exposure,8,comment,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't write EXPOSURE"
         goto 999
      endif
C Modify TELAPSE = ( TSTOP - TSTART )
      CALL FTGKYJ(iounit,'TELAPSE',telapse,comment,status)
      if(status.eq.0) then
         CALL FTGKYD(iounit,'TSTOP',tstop,comment,status)
         CALL FTGKYD(iounit,'TSTART',tstart,comment,status)
         telapse=INT(tstop-tstart)
         comment='&'
         CALL FTMKYJ(iounit,'TELAPSE',telapse,comment,status)
      endif
      status=0
C Move to first record of header
      CALL FTGREC(iounit,1,comment,status)
C Delete some comments (Don't delete the Region file selection comments)
      if(.not.image) then
         i=0
         do while(status.eq.0.and.i.lt.19)
            i=i+1
            CALL FTDKEY(iounit,'COMMENT',status)
         enddo
         status=0
      endif


      CALL FTGKYE(iounit,'EQUINOX',equin,comment,status)
      if(status.eq.0) then
C If EQUINOX is present 
C Delete all keywords EQUINOX and replace with EQUINOX 1950.0
         do while(status.eq.0)
            CALL FTDKEY(iounit,'EQUINOX',status)
            if(status.ne.0) then
               context=" "
            endif
         enddo
         status=0
C        Add EQUINOX keyword
         CALL FTPKYE(iounit,'EQUINOX',1950.0,4,
     &        'Epoch of Coordinate System',status)
      endif
      status=0
      if(.not.image) then
         CALL FTGKYD(iounit,'MINEXP',dum,comment,status)
         if(status.ne.0) then
            status=0
            comment='Minimum exposure cutoff'
            CALL FTPKYD(iounit,'MINEXP',minexpo,4,comment,status)
            if(status.ne.0) then
               CALL FTGERR(status,errtxt)         
               context="Can't write MINEXP"
               goto 999
            endif
         else
            comment='Minimum exposure cutoff'
            CALL FTMKYD(iounit,'MINEXP',minexpo,4,comment,status)
            if(status.ne.0) then
               CALL FTGERR(status,errtxt)         
               context="Can't write MINEXP"
               goto 999
            endif
         endif
         
         CALL FTDKEY(iounit,'EXPMIN',status)
         status=0
      endif
C Delete DATAMODE, ONTIME, LIVETIME from RATE extension
      CALL FTDKEY(iounit,'DATAMODE',status)
      if(status.ne.0) status=0
c      CALL FTDKEY(iounit,'ONTIME',status)
c      if(status.ne.0) status=0
      CALL FTDKEY(iounit,'LIVETIME',status)
      if(status.ne.0) status=0
      if(.not.image) then
         CALL FTPHIS(iounit,'The RATE table was modified by CMADEAD to a
     &ccount for EXOSAT CMA ',status)
         CALL FTPHIS(iounit,'deadtime due to telemetry event losses and 
     &electronics',status)
      endif
C Update the Checksum and Datasum
      CALL FTPCKS(iounit,status)
      CALL FTPCKS(iounit,status)


C*****************************************
C Move to GTI extension
C*****************************************
      CALL FTMNHD(iounit,2,'GTI',0,status)
      if(status.ne.0) then
C No extension is present
         CALL FTGERR(status,errtxt)         
         context="Can't move to next extension"
         status=0
         CALL ftclos(iounit,status)
         CALL ftfiou(iounit,status)
         goto 1000
      endif
      deadcor=.true.
      comment='&'
      CALL FTMKYL(iounit,'DEADAPP',deadcor,comment,status)      
      if(status.ne.0) then
         context="Can't write DEADAPP"
         goto 999
      endif
      CALL FTGKYD(iounit,'MINEXP',dum,comment,status)
      if(status.ne.0) then
         status=0
         comment='Minimum exposure cutoff'
         CALL FTPKYD(iounit,'MINEXP',minexpo,4,comment,status)
         if(status.ne.0) then
            CALL FTGERR(status,errtxt)         
            context="Can't write MINEXP"
            goto 999
         endif
      else
         status=0
         comment='Minimum exposure cutoff'
         CALL FTMKYD(iounit,'MINEXP',minexpo,4,comment,status)
         if(status.ne.0) then
            CALL FTGERR(status,errtxt)         
            context="Can't write MINEXP"
            goto 999
         endif
      endif

      comment='Deadtime due to electronics'
      CALL FTMKYE(iounit,'DEADC',fracavg,4,comment,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't write DEADC"
         goto 999
      endif
C Exposure time is SUM(IntTime*fracexp)
      comment='Effective Exposure time (corrected for deadtime)'
      CALL FTMKYD(iounit,'EXPOSURE',exposure,8,comment,status)
      if(status.ne.0) then
         CALL FTGERR(status,errtxt)
         context="Can't write EXPOSURE"
         goto 999
      endif
C Modify TELAPSE = ( TSTOP - TSTART )
      CALL FTGKYJ(iounit,'TELAPSE',telapse,comment,status)
      if(status.eq.0) then
         CALL FTGKYD(iounit,'TSTOP',tstop,comment,status)
         CALL FTGKYD(iounit,'TSTART',tstart,comment,status)
         telapse=INT(tstop-tstart)
         comment='&'
         CALL FTMKYJ(iounit,'TELAPSE',telapse,comment,status)
      endif
      status=0
C Delete DATAMODE, ONTIME, LIVETIME from RATE extension
      CALL FTDKEY(iounit,'DATAMODE',status)
      if(status.ne.0) status=0
c      CALL FTDKEY(iounit,'ONTIME',status)
c      if(status.ne.0) status=0
      CALL FTDKEY(iounit,'LIVETIME',status)
      if(status.ne.0) status=0
C If EQUINOX is present and is equal to 2000.0 then change to 1950.0
      CALL FTGKYE(iounit,'EQUINOX',equin,comment,status)
      if(status.ne.0) then
C     EQUINOX is not present in this header
         status=0
      else
C     EQUINOX is present check if it is 2000.0
         if(equin.eq.2000.0) then
            comment='&'
            CALL FTMKYE(iounit,'EQUINOX',1950.0,4,comment,status)
         endif
      endif
C Update the Checksum and Datasum
      CALL FTPCKS(iounit,status)
      CALL FTPCKS(iounit,status)

      CALL ftclos(iounit,status)
      CALL ftfiou(iounit,status)
      if (status.eq.0) goto 1000
 999  continue
      CALL ftclos(iounit,status)
      CALL ftfiou(iounit,status)
      write(stat,*) status
      CALL RMVXBK(stat)
      errm=subname//' '//context//', '//errtxt//' '//stat
      CALL RMVXBK(errm)
      CALL xaerror(errm,5)
 1000 continue
      end
