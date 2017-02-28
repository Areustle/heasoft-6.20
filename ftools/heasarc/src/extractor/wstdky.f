
      SUBROUTINE wstdky(Lun, Status)

      IMPLICIT NONE

      INTEGER Lun, Status

c Write all the standard keywords to the current extension
C Arguments :
C      Lun        i          i: i/o unit for output file
C      Status     i          r: 0==OK
C
C Ning Gan (7/23/1998) The date-obs and date-end keywords are written 
C                      in the new date format.  

      INCLUDE 'extractor.inc'

      DOUBLE PRECISION mjdobs

      CHARACTER(72) contxt
      CHARACTER(20) user
      CHARACTER(255) filstr

      INTEGER LENACT
      EXTERNAL LENACT

      character(40) taskname
      COMMON /task/ taskname

C mission description keywords

      IF ( lenact(Telescope) .EQ. 0 ) Telescope = 'NONE'
      CALL FTUKYS(Lun,'telescop',Telescope,'Telescope (mission) name',
     &            Status)
      IF ( lenact(Datamode) .EQ. 0 ) Datamode = 'NONE'
      CALL FTUKYS(Lun,'datamode',Datamode,'Datamode',Status)
      IF ( lenact(Detector) .EQ. 0 ) Detector = 'NONE'
      CALL FTUKYS(Lun,'detnam',Detector,'Detector',Status)
      IF ( lenact(Instrument) .EQ. 0 ) Instrument = 'NONE'
      CALL FTUKYS(Lun,'instrume',Instrument,'Instrument name',Status)
      IF ( lenact(Filter) .NE. 0 .AND. Filter(1:4) .NE. 'NONE' ) THEN
         CALL FTUKYS(Lun,'filter',Filter,'Instrument filter in use',
     &               Status)
      ENDIF
      IF ( lenact(Object) .EQ. 0 ) Object = 'NONE'
      CALL FTUKYS(Lun,'object',Object,'Name of observed object',
     &              Status)
      contxt = 'Failed to write mission description keywords'
      IF ( Status .NE. 0 ) GOTO 999

C Then the exposure keywords
 
      CALL FTUKYD(Lun,'ONTIME',Expos,15,'On-source time',Status)
      IF ( deadc .NE. 1.0 ) THEN
         CALL FTUKYE(Lun,'DEADC',deadc,8,'Deadtime correction',Status)
         CALL FTUKYL(Lun,'DEADAPP',.FALSE.,
     &               'Has DEADC been applied to data',Status)
         CALL FTUKYD(Lun,'EXPOSURE',Expos*deadc,15,'Exposure time',
     &               Status)
         CALL FTUKYD(Lun,'LIVETIME',Expos*deadc,15,
     &               'Deadtime corrected on-source time', Status)
      ELSE
         CALL FTUKYD(Lun,'EXPOSURE',Expos,15,'Exposure time',Status)
         CALL FTUKYD(Lun,'LIVETIME',Expos,15,'On-source time', Status)
      ENDIF

      contxt = 'Failed to write exposure keywords'
      IF ( Status .NE. 0 ) GOTO 999

C Then the date keywords

      CALL FTUKYS(Lun,'DATE-OBS',csdate,
     &   'Start date of observations',Status)
      IF ( lenact(cstime) .GT. 0 ) THEN
         CALL FTUKYS(Lun,'TIME-OBS',cstime,
     &      'Start time of observations',Status)
      ENDIF

      CALL FTUKYS(Lun,'DATE-END',cedate,
     &   'End date of observations',Status)
      IF ( lenact(cetime) .GT. 0 ) THEN
         CALL FTUKYS(Lun,'TIME-END',cetime,
     &      'End time of observations',Status)
      ENDIF
 
      CALL FTUKYD(lun,'TSTART',tstart,15,'time start',status)
      CALL FTUKYD(lun,'TSTOP',tstop,15,'time stop',status)
      CALL FTUKYD(lun,'TELAPSE',(tstop-tstart),15,'elapsed time',status)

      IF ( TIMEUNIT .EQ. 'd' ) THEN
         mjdobs = tstart + emjdrfi + emjdrff
      ELSE
         mjdobs = tstart/86400d0 + emjdrfi + emjdrff
      ENDIF
      CALL FTUKYD(lun,'MJD-OBS', mjdobs,15, 
     &            'MJD of data start time', status)

      IF ( qmjdref ) THEN
         CALL FTUKYD(lun,'MJDREF', emjdrfi + emjdrff, 15, 
     &               'MJD reference day', status)
      ELSE
         CALL FTUKYJ(lun,'MJDREFI', emjdrfi,
     &               'MJD reference day', status)
         CALL FTUKYD(lun,'MJDREFF', emjdrff, 15, 
     &               'MJD reference (fraction of day)', status)
      ENDIF

      contxt = 'Failed to write date keywords'
      IF ( Status .NE. 0 ) GOTO 999

      CALL FTUKYS(lun,'TIMEREF',timeref,'reference time',status)
      contxt = 'Failed to write TIMEREF keyword'
      IF ( Status .NE. 0 ) GOTO 999
      CALL FTUKYS(lun,'TIMESYS',timesys,'time measured from',status)
      contxt = 'Failed to write TIMESYS keyword'
      IF ( Status .NE. 0 ) GOTO 999
      CALL FTUKYS(lun,'TIMEUNIT',timeunit,'unit for time keywords',
     &            status)
      contxt = 'Failed to write TIMEUNIT keyword'
      IF ( Status .NE. 0 ) GOTO 999

c The equinox and RADECSYS keywords

      CALL FTUKYE(Lun,'EQUINOX',equinox,3,
     &            'Equinox of celestial coord system',Status)
      CALL FTUKYS(lun,'RADECSYS',radecsys,'celestial coord system',
     &            status)

c Then other miscellaneous keywords
 
      CALL GETUSE(user,Status)
      Status = 0
      IF ( lenact(user) .EQ. 0 ) user = 'NONE'
      CALL FTUKYS(Lun,'USER',user,'User name of creator',Status)
 
      filstr = Infile
      CALL ADDFN(Lun,filstr,Status)
      IF ( Status .NE. 0 ) THEN
         contxt = 'Unable to add file names to GTI extension'
         CALL fcecho(contxt)
         Status = 0
      ENDIF

      CALL FTUKYS(lun,'creator',taskname,'Extractor',Status)
      CALL FTPDAT(lun,status)

      CALL FTUKYS(Lun,'origin','NASA/GSFC','origin of fits file',
     &              Status)

      IF ( scseqend .GT. 0 ) THEN
         CALL FTUKYJ(Lun, 'SCSEQEND', scseqend, 'SC seq end  (sec)', 
     &               Status)
      ENDIF
      IF ( numobi .GT. 0 ) THEN
         CALL FTUKYJ(Lun, 'NUM_OBIS', numobi, 
     &               'Number of obs intervals (OBIs)', Status)
      ENDIF

      contxt = 'Failed to write miscellaneous keywords'
      IF ( Status .NE. 0 ) GOTO 999

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(Status)
      ENDIF

      RETURN
      END


**==addfn.spg  processed by SPAG 4.50J  at 14:12 on 25 Oct 1995

*
      SUBROUTINE ADDFN(Lun,Fname,Status)

      IMPLICIT NONE
*
* This adds a bunch of keywords to an open fits file for the file names
*
* lun is the lun for the open fits file
* fname is the file name to add.  If the file name has an @ as the
*  first character then it opens that file and reads through it adding each
*  line as a file.  Otherwise, it just adds fname as a single file name
*
      INTEGER Lun
      CHARACTER*(*) Fname
      INTEGER Status


      INTEGER i , lunfn , ier , LENACT
      character(200) str , str1

      Status = 0
      i = 0
      lunfn = 0
      ier = 0
      str = ' '
      str1 = ' '


      IF ( Fname(1:1).EQ.'@' ) THEN
         CALL GETLUN(lunfn)
         str = Fname(2:LENACT(Fname))
         CALL OPENWR(lunfn,str,'old',' ',' ',0,1,ier)
         IF ( ier.NE.0 ) THEN
            CALL XWARN1(' Indirect file can not be opened to '//
     &                  'write to fits header ',5)
            CALL XWARN1(Fname,5)
            Status = ier
            RETURN
         ENDIF

         i = 1
         READ (lunfn,'(a)',IOSTAT=ier) str
         DO WHILE ( ier.EQ.0 )
            WRITE (str1,'(a,i3.3)',ERR=100) 'filin' , i
            CALL STRIPFN(str)
            CALL FTUKYS(Lun,str1,str,'Input file name',Status)
            READ (lunfn,'(a)',IOSTAT=ier) str
            i = i + 1
         ENDDO
         CLOSE (lunfn)
         CALL FRELUN(lunfn)
      ELSE
         CALL STRIPFN(Fname)
         CALL FTUKYS(Lun,'filin001',Fname,'Input file name',Status)
      ENDIF


      IF ( Status.EQ.0 ) RETURN


 100  CALL XWARN1(' Problem writing file names into fits header',5)

      END
