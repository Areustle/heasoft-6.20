c
      SUBROUTINE chkfiles(cfile, nfiles, tstartlc, tstoplc, tspan,
     &     status)
c
c This program will check the list of event files that have been given.
c If any event file lies outside the TSTART or TSTOP of the input
c lightcurve then that file will not be included in the cmadead correction

C Also--- 
C The total time span of the event files need to be found and compared
C to lightcurve span. The number of bins that should be allocated for fracexp 
C should be the larger of the two (i.e. (total timespan)/timedel )
C

c
c  I/O cfile       character array containing event filenames
c  I/O  nfiles      number of event files
C  I  tstartlc    SHF start time of lightcurve or image
C  I  tstoplc     SHF stop time of lightcurve or image
C  O  tspan       Time span between start of first and end of last event file
c  I/O status
c 

      implicit none 


c Input/output variables 
      REAL*8 tstartlc, tstoplc, tstart, tstop,start1,stop1,tspan     
      INTEGER*4 nfiles,nfiles2,nfilemax,status
      PARAMETER (nfilemax=100) 
      character(160) cfile(nfilemax)
c
c Local variable
      INTEGER*4 i, unit,readwrite,blocksize,hdutype,nhdu
      REAL*8 nulld
      character(160) subname, evtfile
      character(255)  errm ,context,string
      character(160) cfile2(nfilemax)  
      PARAMETER (subname = 'chkfiles:')
      character comment*80
c
      if(status.ne.0) return

c  Initialize to avoid warning
      nfiles2 = 0
c  --

      nulld=0.0d0
      start1=852076800.0d0
      stop1=-378691200.0d0
      readwrite = 0
c

C Loop over all files in cfile
      DO i=1,nfiles
         evtfile=cfile(i)
C========================================================
C Open the event file
C========================================================


         CALL FTGIOU(unit,status)
         if(status.ne.0) then
            context= "ERROR: Can't get unit number"
            goto 999
         endif
         
         CALL FTOPEN(unit,evtfile,readwrite,blocksize,status)
         if(status.ne.0) then 
            context="ERROR: Can't find "//evtfile
            goto 999
         endif
C========================================================
C Go to the  EVENTS  extension
C========================================================
         nhdu=3
         CALL FTMAHD(unit,nhdu,hdutype,status)
         if(status.ne.0) then
            context="ERROR: Can't move to OBC_PACKET extension"
            goto 999
         endif

         CALL FTGKYD(unit,'TSTART',tstart,comment,status)
         if(status.ne.0) then
            context="ERROR: Can't get TSTART keyword"
            goto 999
         endif
         CALL FTGKYD(unit,'TSTOP',tstop,comment,status)
         if(status.ne.0) then
            context="ERROR: Can't get TSTOP keyword"
            goto 999
         endif

C========================================================
C Test to see if this event file should be used
C========================================================

         if( (tstart.lt.tstartlc).AND.(tstop.lt.tstartlc) .OR.
     &        (tstart.gt.tstoplc).AND.(tstop.gt.tstoplc) ) then
C Do not include this file
            context= "Warning:"//evtfile//
     & " not within time span of lightcurve. Ignored."
             errm=context
            CALL RMVXBK(errm)
            CALL xwrite(errm,5) 
         else
            nfiles2=nfiles2+1
            if(tstart.lt.start1) then
               start1=tstart
            endif
            if(tstop.gt.stop1) then
               stop1=tstop
            endif
            cfile2(nfiles2)=evtfile
         endif
      enddo

      write(string,10) start1
 10   format("First start time of event files is ",F13.1)
      call xwrite(string,30)

      write(string,20) stop1
 20   format("Last stop time of event files is ",F13.1)
      call xwrite(string,30)

      tspan=stop1-start1
      CALL ftclos(unit,status)
      CALL ftfiou(unit,status)
      do i=1,nfiles2
         cfile(i)=cfile2(i)
      enddo
      nfiles=nfiles2
      goto 1000
 999  continue 
      errm=subname//' '//context
      CALL RMVXBK(errm)
      call xaerror(errm, 5)
      CALL ftclos(unit,status)
      CALL ftfiou(unit,status)
 1000 continue
      return
      end
