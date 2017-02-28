 
      SUBROUTINE SELECT_PHASE(File,Time,Ok)
      implicit none
 
* All times are in seconds, so we must convert the times in days in the
* window files to seconds.  All of them are relative to the launch time.
* File is the file to load, ' ' means reset and return
* time is the time to check
* ok is true if it came in true and time is within the proper phase
*
 
 
      LOGICAL*4 Ok
      CHARACTER*(*) File
      REAL*8 Time
 
      INCLUDE 'exwin.inc'
 
      INTEGER*4 lun
      LOGICAL*4 found
 
      REAL*8 ttime
      INTEGER i
      character(180) errstr
 
 
      IF ( File.EQ.' ' ) THEN
         winfilename = ' '
         Ok = Ok .AND. .TRUE.
         RETURN
      ENDIF
 
      IF ( File.NE.winfilename ) THEN
* we haven't read in the file yet
         winfilename = File
         CALL GETLUN(lun)
         CALL EXRDWIN(lun,6,6,5,File,twia,twio,pwi,pwia,pwio,fwia,fwio,
     &                ewia,ewio,nwi,nwito,*50)
         CALL FRELUN(lun)
 
* pwi(1) is the epoch for the phase window in days
* pwi(2) is the period for the phase window in secs (I don't think so, but
* it's not sensible otherwise!)
 
* Convert the times
         pwi(1) = pwi(1)*86400.D0
         GOTO 100
cvMSJ - 2/24/98 - g77 doesn't like concatenations of *(*) in call
C50      CALL XERROR(' Can not read window file '//File,5)
 50      errstr = ' Cannot read window file '//File
         CALL XERROR(errstr,5)
         CALL FRELUN(lun)
         RETURN
      ENDIF
 
 100  found = .FALSE.
 
      IF ( nwi(2).LE.0 ) RETURN
* Subtract off the epoch
 
      ttime = Time - pwi(1)
 
* Find out the number of periods this time indicates
 
      ttime = ttime/pwi(2)
 
* get the fractional part
 
      i = ttime
 
      ttime = ttime - i
 
      DO 200 i = 1 , nwi(2)
 
 
         found = (ttime.GE.pwia(i)) .AND. (ttime.LE.pwio(i))
 200  CONTINUE
 
      Ok = Ok .AND. found
 
 
      RETURN
      END
