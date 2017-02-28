      subroutine exisitlc(infile,image,nhdu,lcrows,tstart,tstop,tzero,
     &     inttime,status)

C This routine will check whether the FITS file,infile
C is a lightcurve or an image and retrieve some keywords. 
C
C I   iounit   Logical Unit Number
C I/O image    false when no image found
C O   nhdu     CHDU number where lightcurve can be found
C O   lcrows   NAXIS2 for lightcurve
C O   tzero    value from TIMEZERO keyword
C O   tstart   
C O   tstop


      implicit none
      
      LOGICAL image
      INTEGER*4 i,nhdu,status,naxis,iounit,hdutype,readwrite,blocksize
      INTEGER*4 lcrows
      character*(*) infile
      character(80) string, string2
      REAL*8 inttime,tzero,tstart,tstop
      character subname*50,context*160,comment*80,errm*255
      CHARACTER errtxt*30,stat*15

      readwrite=0
      image=.false.
      subname="exisitlc: "
      errtxt=" "
      nhdu=0
      inttime=0.0
      CALL FTGIOU(iounit,status)
      if(status.ne.0) then
         context="Can't get unit number"
         goto 999
      endif

C========================================================
C Open the FITS file - READ
C========================================================
      CALL FTOPEN(iounit,infile,readwrite,blocksize,status)
      if(status.ne.0) then 
         CALL FTGERR(status,errtxt)
         context="Can't find "//infile
         goto 999
c      else
c           comment ="Opened "//infile//" successfully"
c           CALL RMVXBK(comment)
      endif

C
C     Is there anything in the primary header ?
C
      nhdu=1
      CALL FTMAHD(iounit,nhdu,hdutype,status)
      CALL FTGKYJ(iounit,'NAXIS',naxis,comment,status)
      if(naxis.eq.0) then
C        Primary HDU is empty. Maybe it is a lightcurve
         i=1
 10      continue
         i=i+1
C        Can I move to an extension?
         nhdu=i
         if(i.ge.10) then
            context="No LC extension"
            goto 999
         endif
         
         CALL FTMAHD(iounit,nhdu,hdutype,status)
C        No extension
         if(status.ne.0) then 
            context="ERROR: Primary HDU is empty no LC extension"
            goto 999
         else
            
C FITS file with extension
C What is the name of the extension?
C
            CALL FTGKYS(iounit,'EXTNAME',string,comment,status)
            CALL RMVBLK(string)
            comment= "Extension name "//string
            CALL FTGKYS(iounit,'HDUCLAS1',string2,comment,status)
            if(status.ne.0) status=0
            CALL RMVBLK(string2)
            comment = "HDUCLAS1 "//string2
            if((string.eq.'RATE').or.(string.eq.'COUNTS').or.
     &           (string2.eq.'LIGHTCURVE')) then
C
C This is a lightcurve Get NAXIS2, TSTART, TSTOP, TIMEDEL and TIMEZERO

C
C
C Get the number of rows LCROWS
C
               CALL FTGKYJ(iounit,'NAXIS2',lcrows,comment,status)
               if(status.ne.0) then
                  context="NAXIS2 of lightcurve not found"
                  goto 999
               endif
C
C Get TSTART and TSTOP 
C
               CALL FTGKYD(iounit,'TSTART',tstart,comment,status)
               if(status.ne.0) then
                  context="TSTART not found"
                  goto 999
               endif
               CALL FTGKYD(iounit,'TSTOP',tstop,comment,status)
               if(status.ne.0) then
                  context="TSTOP not found"
                  goto 999
               endif

C
C Now look for the Integration time, inttime
C
               CALL FTGKYD(iounit,'TIMEDEL',inttime,comment,status)
               if(status.ne.0) then
                  context="Integration time not found"
                  goto 999
               endif
C
C Get the number of rows LCROWS
C
               CALL FTGKYJ(iounit,'NAXIS2',lcrows,comment,status)
               if(status.ne.0) then
                  context="NAXIS2 of lightcurve not found"
                  goto 999
               endif
C
C Get TIMEZERO
C
               CALL FTGKYD(iounit,'TIMEZERO',tzero,comment,status)
               if(status.ne.0) then
                  context="Can't get TIMEZERO keyword"
                  goto 999
               endif
C Done. Get out of here.
               goto 1000
            else
               goto 10
            endif
         endif
         goto 10
      else
C    Primary HDU has data. Maybe there is a lightcurve in an extension
C
C    Get TSTART and TSTOP from primary HDU
C
         CALL FTGKYD(iounit,'TSTART',tstart,comment,status)
         if(status.ne.0) then
            context="TSTART not found"
            goto 999
         endif
         CALL FTGKYD(iounit,'TSTOP',tstop,comment,status)
         if(status.ne.0) then
            context="TSTOP not found"
            goto 999
         endif
         i=1
 30      continue
         i=i+1
C        Can I move to an extension?
         nhdu=i
         if(i.ge.10) then
            image=.true.
            context="None of the extensions are lightcurves"
            goto 999
         endif
         CALL FTMAHD(iounit,nhdu,hdutype,status)

         if(status.ne.0) then 
C This might be an image
            status=0
            image=.true.
            goto 900
         else
C
C           FITS file with extension
C           What is the name of the extension?
C
            CALL FTGKYS(iounit,'EXTNAME',string,comment,status)
            CALL RMVBLK(string)
            CALL FTGKYS(iounit,'HDUCLAS1',string2,comment,status)
            CALL RMVBLK(string2)
            
            if((string.eq.'RATE').or.(string.eq.'COUNTS').or.
     &           (string2.eq.'LIGHTCURVE')) then
C
C              This is a lightcurve 
C
C              Get the number of rows LCROWS
C
               CALL FTGKYJ(iounit,'NAXIS2',lcrows,comment,status)
               if(status.ne.0) then
                  context="NAXIS2 of lightcurve not found"
                  goto 999
               endif
C
C              Get TSTART and TSTOP 
C
               CALL FTGKYD(iounit,'TSTART',tstart,comment,status)
               if(status.ne.0) then
                  context="TSTART not found"
                  goto 999
               endif
               CALL FTGKYD(iounit,'TSTOP',tstop,comment,status)
               if(status.ne.0) then
                  context="TSTOP not found"
                  goto 999
               endif
C
C              Now look for the Integration time, inttime
C
               CALL FTGKYD(iounit,'TIMEDEL',inttime,comment,status)
               if(status.ne.0) then
                  context="Integration time not found"
                  goto 999
               else
                  goto 900
               endif
            else
               goto 30
            endif
         endif
         goto 30
      endif
 900  continue
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
      return
      end
 
