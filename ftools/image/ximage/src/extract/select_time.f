      SUBROUTINE SELECT_TIME(File,Time,Ok,Timewin,Maxgti,Imaxtw,Mjd1)
      IMPLICIT NONE
 
* All times are in seconds, so we must convert the times in days in the
* window files to seconds.  All of them are relative to the launch time.
* file is the file to load, if ' ' reset saved file name and return
* time is the time to check
* ok is true if it came in true and the time was with one of the time windows
* Timewin is a real*2 of (2,maxgti) which holds 1 to imaxgti of good time
*    intervals
* mjd1 is the modified julian day of the beginning of the year.
 
      LOGICAL*4 Ok
      CHARACTER*(*) File
      INTEGER*4 Maxgti , Imaxtw
      REAL*8 Time , Timewin(2,Maxgti) , Mjd1
 
      INCLUDE 'exwin.inc'
      INCLUDE '../include/io.inc'
 
      INTEGER*4 lun, i
      LOGICAL*4 found
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
     &                ewia,ewio,nwi,nwito,*100)
         CALL FRELUN(lun)
 
* Convert the times to mjd from days from beginning of year, mjd1 is
* the modified julian day of the beginning of the year.
 
         call xwrite(' i  winstart winstop mjdstart mjdstop', 20)
         DO 50 i = 1 , nwi(1)
            Timewin(1,i) = twia(i) + Mjd1
            Timewin(2,i) = twio(i) + Mjd1
            write(ZWRite,*) i, twia(i), twio(i), Timewin(1,i),
     &                                           Timewin(2,i)
            call xwrite(ZWRite, 20)
            twia(i) = twia(i) + Mjd1
            twio(i) = twio(i) + Mjd1
 
 50      CONTINUE
         Imaxtw = nwi(1)
         GOTO 200
cvMSJ - 2/24/98 - g77 doesn't like concatenations of *(*) in call
C100     CALL XERROR(' Can not read window file '//File,5)
 100     errstr = ' Cannot read window file '//File
         CALL XERROR(errstr,5)
         CALL FRELUN(lun)
         RETURN
      ENDIF
 
 200  found = .FALSE.
      DO 300 i = 1 , nwi(1)
         IF ( .NOT.found ) found = (Time.GE.twia(i)) .AND. 
     &                             (Time.LE.twio(i))
 300  CONTINUE
 
      Ok = Ok .AND. found
 
 
      RETURN
      END
