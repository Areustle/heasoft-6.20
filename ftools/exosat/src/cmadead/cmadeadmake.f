      subroutine cmadeadmake(infile,evtfile,outfile,minexpo,
     &     ichat, clobber,ierr)
c
c
c INITialize files and parameters for program cmadead
c read parameters in the parameters file.
c
c  I  infile    (d)  lightcurve file 
c  I  evtfile   (d)  file containing names of event file 
c  O  outfile   (d)  deadtime corrected lightcurve file
C  I  minexpo   (d)  minimum exposure cutoff 
c  I  tchat     (i)  Chattiness terminal
c  I  lchat     (i)  Chattiness log 
c  I  ierr      (i)  Error return
c  I  clobber   (l)  Overwrite output file if it exists?
c
      IMPLICIT NONE
C Input 
       INTEGER*4 ichat, ierr
       REAL*8 minexpo
       CHARACTER*(*) infile, outfile, evtfile
       LOGICAL clobber
c 
c Local variable 
c dynamic memory

      INTEGER*4 time,rate,error,lcrows,tspanbin
      INTEGER*4 nfiles,status,i,naxis,extnum,npoint
      INTEGER*4 gndtime,shftime,prtime,bctime,bad,samples,fracexp
      INTEGER*4 row1,fracexpcnt,fracnt,npointsum,lchdu,lenact

      character(160) subname,cfile(100),subprog
      character(255)  errm ,context,extname,string

      PARAMETER (subname = 'cmadeadmake:')
      REAL fracavg,fracexpsum,fracsum
      REAL*8 expt,expos,inttime,start,tzero,tstart,tstop,tspan
      LOGICAL image,first
      include 'dmcommon.inc'
c      DATA disk /' '/
c
      extname="OBC_PACKET"
      errm = ' '
      fracexpsum=0.0
      fracsum=0.0
      fracexpcnt=0
      fracnt=0
      expt=0.0d0
      expos=0.0d0
      inttime=0.0
      image=.false.
      first=.true.
      status=0
      nfiles=0
      npoint=0
      npointsum=0


c
c Open the @ file. Get the names of all event files and number of files
c from evtfile and store in cfile(nfiles)
c
      subprog="GETFILES"
      CALL xwrite(subprog,15) 
      CALL getfiles(evtfile, cfile, nfiles, status)
      IF (status.ne.0) Then 
          context=' Problem in opening the filelist'
          errm= subname//' '//context
          GOTO 999
      ENDIF
      
      infile=infile(1:lenact(infile))

c
C Check to see if infile is an image or a lightcurve. If lightcurve what is 
C * the extension in which the lightcurve can be found, lchdu;
C * the integration time, inttime;
C * number of rows, lcrows; 
C * start time, tzero
c * and TSTART and TSTOP
C
      subprog="EXISITLC"
      CALL xwrite(subprog,15) 
      CALL exisitlc(infile,image,lchdu,lcrows,tstart,tstop,tzero,
     &     inttime,status)
      IF (status.ne.0) Then 
          context="Can't determine input file type or file not found"
          errm= subname//' '//context
          GOTO 999
      ENDIF
C
C I need to check event files to make sure they are not outside of 
C  TSTART and TSTOP. That is if TSTART *and* TSTOP (event) is > TSTOP(lc) or
C                               TSTART *and* TSTOP (event) is < TSTART(lc)
C  then the event file should not be used to correct the lightcurve
C
C Also: 
C The total time span of the event files need to be found and compared
C to lightcurve span. The number of bins that should be allocated for fracexp 
C should be the larger of the two (i.e. (total timespan)/binsize )
C
C Also: 
C The event files should be processed in time order. In case they aren't
C already they should be listed in this way. Use exshuffle as used in exomerge.
C
C tspan is the time span between start of first and end of last event file
C
      subprog="CHKFILES"
      CALL xwrite(subprog,15) 
      CALL chkfiles(cfile, nfiles, tstart, tstop, tspan,status)
      IF (status.ne.0) Then 
          context=' Problem in checking the event files'
          errm= subname//' '//context
          GOTO 999
      ENDIF
      tspanbin=INT(tspan/inttime)+nfiles
      write(string,20) tspanbin
 20   format("Event files span ",I6," bins")
      call xwrite(string,30)


C
C FOR IMAGES
C
      if(image) then
         string="Processing an image file"
         call xwrite(string,10) 
         subprog="EXCOPYIMG"
         CALL xwrite(subprog,15) 
c
c Copy infile image FITS file to outfile image FITS file
c
         CALL excopyimg(infile,outfile,clobber,status)
         IF (status.ne.0) Then 
            context=' Problem copying image FITS files'
            errm= subname//' '//context
            GOTO 999
         ENDIF
      else
C
C FOR LIGHTCURVES
C
         string="Processing a lightcurve"
         call xwrite(string,10) 
C 
c  Arrays for lightcurve data:  time, rate, error, fracexp. The length
c  determined by NAXIS2 keyword of lightcurve file
c        Dynamic Memory Allocation. 
C 
C Time and Rate arrays will be filled in exreadlc 
C        when reading the lightcurve
C Rate will be then updated in applycorre 
C        where it will be adjusted for fractional exposure fracexp
C Error will be filled in applycorre 
C        using the adjusted rates and integration time
c Fracexp will be filled in deadcor and adjusted against minexpo in 
C        applycorre

         if(tspanbin.gt.lcrows) then
            write(string,22) tspanbin
 22         format(
     &           "Allocating 4 arrays each with # of elements= ",
     &           I6)
            call xwrite(string,30)
            call mall1(tspanbin,time,rate,error,fracexp,1,status)            
C  Initialize the fracexp array 
            do i=1,tspanbin
               memd(fracexp+i-1)=0.0d0
            enddo
         else
            write(string,22) lcrows
            call xwrite(string,30)
            call mall1(lcrows,time,rate,error,fracexp,1,status)
C  Initialize the fracexp array
            do i=1,lcrows
               memd(fracexp+i-1)=0.0d0
            enddo
         endif
         IF (status.ne.0) Then 
            context=' Problem allocating memory for TIME, RATE'
            errm= subname//' '//context
            GOTO 999
         ENDIF
C
c reading the whole lightcurve. 
C Input is:
C        - from exisitlc -  lchdu,lcrows
C        - from user - infile
C Output is: 
C         time, rate, status
C
         subprog="EXREADLC"
         CALL xwrite(subprog,15) 
         call exreadlc(lchdu,lcrows,infile,memd(time),memr(rate),
     &        status)
         if(image) goto 100
         IF (status.ne.0) Then 
            context=' Problem reading infile'
            errm= subname//' '//context
            GOTO 999
         ENDIF
      endif
      row1=0
C****
C LOOP THROUGH EACH EVENT FILE
C****
      DO i=1,nfiles

         string="Reading "//cfile(i)
         call xwrite(string,10)
C
C Read event file cfile(i), extension OBC_PACKET, and return extension
c number and NAXIS2 to allow appropriate amount of dynamic memory allocation
c 
         subprog="EXTABLE"
         CALL xwrite(subprog,15) 
         CALL extable(cfile(i),extname,naxis,extnum,status)
         IF (status.ne.0) Then 
            context=' Problem in reading table from eventfile'
            errm= subname//' '//context
            GOTO 999
         ENDIF
C Dynamic memory allocation routine. All arrays will have length of naxis

         CALL mall2(naxis,gndtime,shftime,prtime,bctime,bad,samples,
     &        1,status)
         IF (status.ne.0) Then 
            context=' Problem allocating memory for OBC_PACKET arrays'
            errm= subname//' '//context
            GOTO 999
         ENDIF
C
C Compute the fractional exposure correction from cfile(i)
C
C If it is an image use the unbinned adima expp derived deadtime correction
C
C If it is a lightcurve use the integration time given by inttime
C and the IA/lcurv/exp3 derived deadcor routine
C
          if(image) then
             subprog="IMAGEDEAD"
             CALL xwrite(subprog,15) 
             CALL imagedead(naxis,extnum,cfile(i),
     &         memd(gndtime), memi(shftime),memd(prtime),
     &         memr(bctime),mems(bad),mems(samples),fracsum,expt,status)
             fracexpsum=fracexpsum+fracsum
             expos=expos+expt
          else
             subprog="DEADCOR"
             CALL xwrite(subprog,15) 
             write(string,24) row1
 24          format(
     &            "Last bin # of prev. event file before deadcor",
     &            I6)
             call xwrite(string,30)
             call deadcor(naxis,lcrows,memd(time),tzero,row1,inttime,
     &            extnum,cfile(i),start,npoint,memd(fracexp),
     &            memd(gndtime),memi(shftime),memd(prtime),memr(bctime),
     &            mems(bad),mems(samples),fracsum,expt,status)
             npointsum=npointsum+npoint
             fracexpsum=fracexpsum+fracsum
             expos=expos+expt
             IF (status.ne.0) Then 
                context=' Problem in exposure corrections'
                errm= subname//' '//context
                GOTO 999
             ENDIF
          endif
          row1=row1+npoint
C Dynamic memory deallocation routine

          CALL mall2(naxis,gndtime,shftime,prtime,bctime,bad,samples,
     &         2,status)
          IF (status.ne.0) Then 
             context='Problem deallocating memory for OBC_PACKET arrays'
             errm= subname//' '//context
             GOTO 999
          ENDIF
c
C About to use the next event file
c
          first=.false.
       ENDDO
C****
C END OF EVENT FILE LOOP
C****
 100   continue

C
C FOR IMAGES
C
       if(image) then
          fracavg=fracexpsum/float(nfiles)
          if(fracavg.gt.1.0) fracavg=1.0/fracavg

       else
C
C FOR LIGHTCURVES
C
c
c Compute the corrected count rate and error and build the corrected 
C lightcurve array
c
          subprog="APPLYCORRE"
          CALL xwrite(subprog,15) 
          call applycorre(lcrows,inttime,memr(rate),
     &         memr(error),memd(fracexp),minexpo,status)
          IF (status.ne.0) Then 
             context=' Problem correcting RATE'
             errm= subname//' '//context
             GOTO 999
          ENDIF
          write(string,26) fracexpsum,npointsum
 26       format("fracexpsum, npointsum", F9.3,1x,I6)
          call xwrite(string,30)

          fracavg=fracexpsum/float(npointsum)
c         
c Writes to output file all rows of lightcurve
c
          subprog="WRITEARRAY"
          CALL xwrite(subprog,15) 
          call writearray(infile,outfile,lcrows,inttime,
     &         memd(time),memr(rate),memr(error),memd(fracexp),
     &         minexpo,clobber,status)
          IF (status.ne.0) Then 
             context=' Problem writing outfile'
             errm= subname//' '//context
             GOTO 999
          ENDIF
C
C Deallocate memory
C          
          call mall1(lcrows,time,rate,error,fracexp,2,status)
          IF (status.ne.0) Then 
             context=' Problem deallocating memory for TIME, RATE'
             errm= subname//' '//context
             GOTO 999
          ENDIF

       endif
c
c update headers of the lightcurve or image file
c
       subprog="UPDATE"
       CALL xwrite(subprog,15) 
       call update(infile,image,cfile,nfiles,outfile,minexpo,fracavg,
     &      expos,status)
       IF (status.ne.0) Then 
          context='Problem updating keywords in outfile'
          errm= subname//' '//context
          GOTO 999
       ENDIF
       GOTO 1000

999   CONTINUE
      CALL RMVXBK(errm)
      call xerror(errm,5)
1000  CONTINUE
      RETURN
      END
