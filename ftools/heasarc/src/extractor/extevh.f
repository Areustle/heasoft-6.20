
      SUBROUTINE extevh(Infile1, Tdelta, Location, Mjdrfi1, Mjdrff1, 
     &                  Qmjdrf1, Deadc1, Equin, Bounds, Cproj, 
     &                  Ctime, Dmkey, Instru, Telesc, Filt, Datamd,
     &                  Detnam, Target, Tsys, Goodfwcs, Obsinf, Status)

      IMPLICIT NONE

      DOUBLE PRECISION Tdelta(2), Location(21), Mjdrff1

      REAL Deadc1, Equin

      INTEGER Bounds(12), Mjdrfi1, Obsinf(2), Status

      CHARACTER*(*) Infile1, Cproj(9), Tsys(3), Ctime(4), Dmkey(8)
      CHARACTER*(*) Instru, Telesc, Filt, Datamd, Detnam, Target

      LOGICAL Goodfwcs, Qmjdrf1

c Reads the event file header information

c Arguments :
c      Infile1        c         i: Event filename
c      Tdelta         d         r: TIMEDEL and TIMEPIXR
c      Location       d         r: WCS info : 1-2 are image CRPIX, 3-4 image
c                                  image CRVAL, 5-6 image CRDELT, 
c                                  7-8 image OPTIC, 9-10 WMAP CRDELT,
c                                  11-12 WMAP CRPIX, 13-14 WMAP OPTIC,
c                                  15-17 pointing position, 18 image CROTA,
c                                  19 pixel size in sky coordinates, 20-21
c                                  WMAP CRVAL
c      Mjdrfi1        i         r: Reference time (day)
c      Mjdrff1        d         r: Reference time (fraction of day)
c      Qmjdrf1        l         r: True if MJDREF read, false if MJDREFI and F
c      Deadc1         r         r: Deadtime correction
c      Equin          r         r: Equinox
c      Bounds         i         r: Valid range on ECOL (1-2), XCOLF (3-4), 
c                                  YCOLF (5-6), XCOLH (7-8), YCOLH(9-10),
c                                  GCOL(11-12).
c      Cproj          c         r: 1-2 are image CTYPE, 3-4 image CNAME,
c                                  5-6 WMAP CTYPE, 7-8 WMAP CNAME, 9 RADECSYS
c      Ctime          c         r: DATE-OBS, TIME-OBS, DATE-END, TIME-END
c      Dmkey          c         r: 1-2 are image MTYPE, 3-4 are image MFORM,
c                                  5-6 and 7-8 repeat for the WMAP.
c      Instru         c         r: Instrument in use
c      Telesc         c         r: Telescope in use
c      Filt           c         r: Filter in use
c      Datamd         c         r: Datamode for instrument
c      Detnam         c         r: Detector name
c      Target         c         r: Object being observed
c      Goodfwcs       l         r: if true then FITS WCS was read
c      Tsys           c         r: 1 is TIMEREF, 2 is TIMESYS, 3 is TIMEUNIT 
c      Obsinf         i         r: 1 is SCSEQEND, 2 is NUMOBI (these are
c                                  ROSAT specific)
c      Status         i         r: 0==OK

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'

      DOUBLE PRECISION dinput(MAXCOLS)
      DOUBLE PRECISION lbounds(MAXCOLS), ubounds(MAXCOLS)
      DOUBLE PRECISION djunk2

      REAL livetime, ontime

      INTEGER unit, blocksize, hdutype, nrows, tfields, varidat
      INTEGER nfound, Tpos
      INTEGER xif, yif, xih, yih, phai, xif1, yif1, gradei, timei
      INTEGER i, ii

      CHARACTER(255) contxt
      CHARACTER(70) comment
      CHARACTER(20) extname
      CHARACTER(5)  descr
      character(20) ttype(MAXCOLS), tunit(MAXCOLS), tform(MAXCOLS)
      character(10) cinput(MAXCOLS)

      INTEGER lenact
      EXTERNAL lenact

      Goodfwcs = .FALSE.

c Open the FITS file

      Status = 0
      CALL GETLUN(unit)
      CALL FTOPEN(unit,Infile1,0,blocksize,Status)
      IF ( Status.NE.0 ) THEN
         contxt = ' File '//Infile1(1:LENACT(Infile1))//' not found'
         CALL fcerr(contxt)
         CALL fcerrm(Status)
         CALL FRELUN(unit)
         RETURN
      ENDIF

c Read the basic mission keywords from the primary header
 
      CALL FTMAHD(unit,1,hdutype,Status)
  
      Instru = ' '
      CALL FTGKYS(unit,'INSTRUME',Instru,comment,Status)
      Status = 0
      Telesc = ' '
      CALL FTGKYS(unit,'TELESCOP',Telesc,comment,Status)
      Status = 0
      Filt = ' '
      CALL FTGKYS(unit,'FILTER',Filt,comment,Status)
      Status = 0
      Datamd = ' '
      CALL FTGKYS(unit,'DATAMODE',Datamd,comment,Status)
      Status = 0
      Detnam = ' '
      CALL FTGKYS(unit,'DETNAM',Detnam,comment,Status)
      Status = 0
      Target = ' '
      CALL FTGKYS(unit,'OBJECT',Target,comment,Status)
      Status = 0
 
*     Find the events table
 
      extname = ' '
      i = 2
      CALL UPC(Eventname)
      DO WHILE ( Status.EQ.0 .AND. extname.NE.Eventname )
         CALL FTMAHD(unit,i,hdutype,Status)
         IF ( hdutype.EQ.2 ) THEN
            CALL FTGHBN(unit,MAXCOLS,nrows,tfields,ttype,tform,tunit,
     &                  extname,varidat,Status)
            CALL UPC(extname)
         ENDIF
         i = i + 1
      ENDDO

      contxt = ' EVENTS table named '//Eventname//' not found'
      IF ( Status .NE. 0 ) GOTO 999

*     If we didn't get some of the basic mission keywords from the primary
*     header then try to get them from the event header.

      IF ( Instru .EQ. ' ' ) THEN
         CALL FTGKYS(unit,'INSTRUME',Instru,comment,Status)
         Status = 0
      ENDIF
      IF ( Telesc .EQ. ' ' ) THEN
         CALL FTGKYS(unit,'TELESCOP',Telesc,comment,Status)
         Status = 0
      ENDIF
      IF ( Filt .EQ. ' ' ) THEN
         CALL FTGKYS(unit,'FILTER',Filt,comment,Status)
         Status = 0
      ENDIF
      IF ( Datamd .EQ. ' ' ) THEN
         CALL FTGKYS(unit,'DATAMODE',Datamd,comment,Status)
         Status = 0
      ENDIF
      IF ( Detnam .EQ. ' ' ) THEN
         CALL FTGKYS(unit,'DETNAM',Detnam,comment,Status)
         Status = 0
      ENDIF
      IF ( Target .EQ. ' ' ) THEN
         CALL FTGKYS(unit,'OBJECT',Target,comment,Status)
         Status = 0
      ENDIF

*     get the date-obs, time-obs, and date-end, time-end keywords
*     start by setting them all to ' ' just in case the appropriate
*     keywords are not found. note that we check for the new format
*     where the date and time both appear in the date-* keywords.

      DO i = 1, 4
         Ctime(i) = ' '
      ENDDO

      CALL FTGKYS(unit,'date-obs',Ctime(1),comment,Status)
      IF ( Status.NE.0 ) THEN
         Status = 0
         CALL FTGKYS(unit,'date_obs',Ctime(1),comment,Status)
      ENDIF
      IF ( Status.NE.0 ) THEN
         CALL fcecho(' Unable to get date-obs/date_obs')
         Status = 0
      ENDIF
      Tpos = index(Ctime(1),'T')
      IF ( Tpos .EQ. 0 ) THEN
         CALL FTGKYS(unit,'time-obs',Ctime(2),comment,Status)
         IF ( Status.NE.0 ) THEN
            Status = 0
            CALL FTGKYS(unit,'time_obs',Ctime(2),comment,Status)
         ENDIF
         IF ( Status.NE.0 ) THEN
            CALL fcecho(' Unable to get time-obs/time_obs')
            Status = 0
         ENDIF
      ENDIF

      CALL FTGKYS(unit,'date-end',Ctime(3),comment,Status)
      IF ( Status.NE.0 ) THEN
         Status = 0
         CALL FTGKYS(unit,'date_end',Ctime(3),comment,Status)
      ENDIF
      IF ( Status.NE.0 ) THEN
         CALL fcecho(' Unable to get date-end/date_end')
         Status = 0
      ENDIF
      Tpos = index(Ctime(3),'T')
      IF ( Tpos .EQ. 0 ) THEN
         CALL FTGKYS(unit,'time-end',Ctime(4),comment,Status)
         IF ( Status.NE.0 ) THEN
            Status = 0
            CALL FTGKYS(unit,'time_end',Ctime(4),comment,Status)
         ENDIF
         IF ( Status.NE.0 ) THEN
            CALL fcecho(' Unable to get time-end/time_end')
            Status = 0
         ENDIF
      ENDIF

c Now get the TIMEDEL keyword. If keyword doesn't exist then return -999.
 
      CALL FTGKYD(unit,'timedel',Tdelta(1),comment,Status)
      IF ( Status .NE. 0 ) THEN
         Tdelta(1) = -999.d0
         Status = 0
      ENDIF

c Now get the TIMEPIXR keyword. If keyword doesn't exist then return -999.
 
      CALL FTGKYD(unit,'timepixr',Tdelta(2),comment,Status)
      IF ( Status .NE. 0 ) THEN
         Tdelta(2) = -999.d0
         Status = 0
      ENDIF

c Get the TIMEREF, TIMESYS, and TIMEUNIT keywords if they are present
c If we can't read TIMEUNIT then get instead look for TUNIT for the TIME
c column.

      DO i = 1, 3
         Tsys(i) = ' '
      ENDDO
      CALL FTGKYS(unit, 'timeref', Tsys(1), comment, Status)
      Status = 0
      CALL FTGKYS(unit, 'timesys', Tsys(2), comment, Status)
      Status = 0
      CALL FTGKYS(unit, 'timeunit', Tsys(3), comment, Status)
      IF ( Status .NE. 0 ) THEN
         Status = 0
         CALL FTGCNO(unit,.FALSE.,'TIME',timei,Status)
         IF ( Status .EQ. 0 ) THEN
            CALL FTGKNS(unit, 'tunit', timei, 1, Tsys(3), nfound, 
     &                  Status)
         ENDIF
      ENDIF
      Status = 0

c Find the column numbers for the event attributes that we need

      IF ( needener ) THEN
 
         CALL FTGCNO(unit,.FALSE.,Ecol,phai,Status)
         contxt = ' Column '//Ecol(1:LENACT(Ecol))//' Not found'
         IF ( Status.NE.0 ) GOTO 999

      ENDIF

      IF ( haveimco ) THEN

         CALL FTGCNO(unit,.FALSE.,Xcolf,xif,Status)
         contxt = ' Column '//Xcolf(1:LENACT(Xcolf))//' Not found'
         IF ( Status.NE.0 ) GOTO 999

         CALL FTGCNO(unit,.FALSE.,Ycolf,yif,Status)
         contxt = ' Column '//Ycolf(1:LENACT(Ycolf))//' Not found'
         IF ( Status.NE.0 ) GOTO 999

      ENDIF

      IF ( needwmco ) THEN

         CALL FTGCNO(unit,.FALSE.,Xcolh,xih,Status)
         contxt = ' Column '//Xcolh(1:LENACT(Xcolh))//' Not found'
         IF ( Status.NE.0 ) GOTO 999

         CALL FTGCNO(unit,.FALSE.,Ycolh,yih,Status)
         contxt = ' Column '//Ycolh(1:LENACT(Ycolh))//' Not found'
         IF ( Status.NE.0 ) GOTO 999

      ENDIF

      IF ( needgrade ) THEN
 
         CALL FTGCNO(unit,.FALSE.,Gcol,gradei,Status)
         contxt = ' Column '//Gcol(1:LENACT(Gcol))//' Not found'
         IF ( Status.NE.0 ) GOTO 999

      ENDIF

c Now find the legal minimum and maximum values in the columns that
c we are interested in. 
 
      DO i = 1 , MAXCOLS
         lbounds(i) = -999d0
         ubounds(i) = -999d0
      ENDDO

c First try to find the TLMIN, TLMAX keywords

      CALL FTGKND(unit,'tlmin',1,MAXCOLS,lbounds,nfound,Status)
      IF ( Status.NE.0 ) THEN
         WRITE(contxt,'(a,i4)') ' Failed to read TLMIN#, status = ', 
     &                          Status
         CALL fcecho(contxt)
         Status = 0
      ENDIF

      CALL FTGKND(unit,'tlmax',1,MAXCOLS,ubounds,nfound,Status)
      IF ( Status.NE.0 ) THEN
         WRITE(contxt,'(a,i4)') ' Failed to read TLMAX#, status = ', 
     &                          Status
         CALL fcecho(contxt)
         Status = 0
      ENDIF

c If either the X or Y columns are TIME then use the TSTART and TSTOP
c keywords to get the bounds.

      IF ( haveimco ) THEN

         IF ( Xcolf .EQ. 'TIME' ) THEN
            CALL FTGKYD(unit,'TSTART',lbounds(xif),comment,Status)
            contxt = 'Failed to read TSTART keyword'
            IF ( Status .NE. 0 ) GOTO 999
            CALL FTGKYD(unit,'TSTOP',ubounds(xif),comment,Status)
            contxt = 'Failed to read TSTOP keyword'
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF
 
         IF ( Ycolf .EQ. 'TIME' ) THEN
            CALL FTGKYD(unit,'TSTART',lbounds(yif),comment,Status)
            contxt = 'Failed to read TSTART keyword'
            IF ( Status .NE. 0 ) GOTO 999
            CALL FTGKYD(unit,'TSTOP',ubounds(yif),comment,Status)
            contxt = 'Failed to read TSTOP keyword'
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF

      ENDIF
 
c If TLMIN, TLMAX didn't work then try the back-up keywords
c first the PHA/PI column...

      IF ( needener ) THEN

         Bounds(1) = NINT(lbounds(phai))
         IF ( Bounds(1) .EQ. -999 ) Bounds(1) = 1
 
         Bounds(2) = NINT(ubounds(phai))
         IF ( Bounds(2) .EQ. -999 ) THEN
            CALL FTGKYJ(unit,Mphakey,Bounds(2),comment,Status)
            contxt = ' Keyword name '//Mphakey(1:LENACT(Mphakey))
     &                 //' not found for PHA/PI column'
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF

      ENDIF

c then the X, Y columns...

      IF ( haveimco ) THEN

         Bounds(3) = NINT(lbounds(xif))
         IF ( Bounds(3) .EQ. -999 ) Bounds(3) = 1
         Bounds(5) = NINT(lbounds(yif))
         IF ( Bounds(5) .EQ. -999 ) Bounds(5) = 1

         Bounds(4) = NINT(ubounds(xif))
         IF ( Bounds(4) .EQ. -999 ) THEN
            CALL FTGKYJ(unit,Xfkey,Bounds(4),comment,Status)
            contxt = ' Keyword name '//Xfkey(1:LENACT(Xfkey))
     &           //' not found for image X column'
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF

         Bounds(6) = NINT(ubounds(yif))
         IF ( Bounds(6) .EQ. -999 ) THEN
            CALL FTGKYJ(unit,Yfkey,Bounds(6),comment,Status)
            contxt = ' Keyword name '//Yfkey(1:LENACT(Yfkey))
     &           //' not found for image Y column'
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF

      ENDIF

c and if required the WMAP X, Y columns...

      IF ( needwmco ) THEN

         Bounds(7) = NINT(lbounds(xih))
         IF ( Bounds(7) .EQ. -999 ) Bounds(7) = 1
         Bounds(9) = NINT(lbounds(yih))
         IF ( Bounds(9) .EQ. -999 ) Bounds(9) = 1

         Bounds(8) = NINT(ubounds(xih))
         IF ( Bounds(8) .EQ. -999 ) THEN
            CALL FTGKYJ(unit,Xhkey,Bounds(8),comment,Status)
            contxt = ' Keyword name '//Xhkey(1:LENACT(Xhkey))
     &                 //' not found for WMAP X column'
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF

         Bounds(10) = NINT(ubounds(yih))
         IF ( Bounds(10) .EQ. -999 ) THEN
            CALL FTGKYJ(unit,Yhkey,Bounds(10),comment,Status)
            contxt = ' Keyword name '//Yhkey(1:LENACT(Yhkey))
     &                 //' not found for WMAP Y column'
            IF ( Status .NE. 0 ) GOTO 999
         ENDIF

      ENDIF

      IF ( needgrade ) THEN
         Bounds(11) = NINT(lbounds(gradei))
         Bounds(12) = NINT(ubounds(gradei))
      ENDIF

c Get the WCS keywords
 
      Goodfwcs = .TRUE.

c Get the optical axes for the image and WMAP axes

      DO i = 1 , MAXCOLS
         dinput(i) = -999.D0
      ENDDO
      CALL FTGKND(unit,'optic',1,MAXCOLS,dinput,nfound,Status)
      Status = 0

      Location(7) = dinput(xif)
      Location(8) = dinput(yif)
      IF ( needwmco ) THEN
         Location(13) = dinput(xih)
         Location(14) = dinput(yih)
      ENDIF

C Get the WCS info - initialize Cproj and Dmkey first since we won't 
C necessarily set all elements in the code that follows.

      DO i = 1, 8
         Cproj(i) = ' '
         Dmkey(i) = ' '
      ENDDO

      descr = 'image'
      CALL extwcs(unit, xif, yif, .FALSE., descr, Location(1), 
     &     Location(3), Location(5), Location(18), Cproj(1), 
     &     Cproj(3), Dmkey(1), Dmkey(3), Goodfwcs, Status) 

      IF ( needwmco ) THEN

         descr = 'wmap'
         CALL extwcs(unit, xih, yih, .FALSE., descr, Location(11), 
     &               Location(20), Location(9), djunk2, Cproj(5), 
     &               Cproj(7), Dmkey(5), Dmkey(7), Goodfwcs, Status)

      ENDIF

c Set xif1 and yif1 to be the columns with sky coordinates.

      DO i = 1 , MAXCOLS
         cinput(i) = ' '
      ENDDO
      CALL FTGKNS(unit,'tctyp',1,MAXCOLS,cinput,nfound,Status)

      IF ( Status.NE.0 .OR. nfound.EQ.0 ) THEN
         Status = 0
         CALL FTGKNS(unit,'cinput',1,MAXCOLS,cinput,nfound,Status)
      ENDIF
 
      Status = 0

      xif1 = -1
      yif1 = -1
      DO ii = 1 , MAXCOLS
         CALL UPC(cinput(ii))
         IF ( cinput(ii) .EQ. 'RA---TAN' ) xif1 = ii
         IF ( cinput(ii) .EQ. 'DEC--TAN' ) yif1 = ii
      ENDDO
      IF ( xif1 .EQ. -1 ) xif1 = xif
      IF ( yif1 .EQ. -1 ) yif1 = yif

c Set the pixel size in sky coordinates (mainly to handle the case when
c the image is in detector coordinates)

      DO i = 1 , MAXCOLS
         dinput(i) = -999.D0
      ENDDO
      CALL FTGKND(unit,'tcdlt',1,MAXCOLS,dinput,nfound,Status)
      IF ( Status.NE.0 ) THEN
         CALL fcecho(' tcdlt did not work')
         Status = 0
         Goodfwcs = .FALSE.
      ENDIF

      IF ( xif1 .GT. 0 .AND. dinput(xif1) .NE. -999.D0 ) THEN
         Location(19) = ABS(dinput(xif1))
      ELSEIF ( yif1 .GT. 0 .AND. dinput(yif1) .NE. -999.D0 ) THEN
         Location(19) = ABS(dinput(yif1))
      ELSE
         Location(19) = ABS(Location(6))
      ENDIF

c Get the nominal pointing position

      Location(15) = -999.d0
      CALL FTGKYD(unit,'ra_pnt',Location(15),comment,Status)
      Status = 0
      Location(16) = -999.d0
      CALL FTGKYD(unit,'dec_pnt',Location(16),comment,Status)
      Status = 0
      Location(17) = -999.d0
      CALL FTGKYD(unit,'pa_pnt',Location(17),comment,Status)
      Status = 0

c If we couldn't read the instrument and telescope from the primary header
c then have another go here. 
 
      IF ( Instru.EQ.' ' ) THEN
         CALL FTGKYS(unit,'INSTRUME',Instru,comment,Status)
         Status = 0
      ENDIF
      IF ( Telesc.EQ.' ' ) THEN
         CALL FTGKYS(unit,'TELESCOP',Telesc,comment,Status)
         Status = 0
      ENDIF
 
c  Get the reference time - note the different ways it could be stored.
c  Use an internal storage accurate to microseconds.

      Status = 0
      CALL FTGKYD(unit,'mjdref',Mjdrff1,comment,Status)
      IF ( Status .EQ. 0 ) THEN
         Mjdrfi1 = INT(Mjdrff1)
         Mjdrff1 = Mjdrff1 - Mjdrfi1
         Qmjdrf1 = .TRUE.
      ELSE
         Status = 0
         Qmjdrf1 = .FALSE.
         CALL FTGKYJ(unit,'xs-mjdrd',Mjdrfi1,comment,Status)
         CALL FTGKYD(unit,'xs-mjdrf',mjdrff1,comment,Status)
         IF ( Status.NE.0 ) THEN
            Status = 0
            CALL FTGKYJ(unit,'mjdrefi',mjdrfi1,comment,Status)
            CALL FTGKYD(unit,'mjdreff',Mjdrff1,comment,Status)
         ENDIF
         IF ( Status.NE.0 ) THEN
            Mjdrfi1 = 0
            mjdrff1 = 0.d0
         ENDIF
         Status = 0
      ENDIF

c If it is present then get the deadtime correction. If there is no DEADC
c or DTCOR keyword also check for LIVETIME and ONTIME and if they exist 
c calculate a deadtime correction from their ratio.

      Deadc1 = 1.0
      CALL FTGKYE(unit,'deadc',Deadc1,comment,Status)
      IF ( Status .NE. 0 ) THEN
         Status = 0
         CALL FTGKYE(unit,'dtcor',Deadc1,comment,Status)
      ENDIF
      IF ( Status .NE. 0 ) THEN
         Status = 0
         CALL FTGKYE(unit,'LIVETIME',livetime,comment,Status)
         CALL FTGKYE(unit,'ONTIME',ontime,comment,Status)
         IF ( Status .EQ. 0 .AND. ontime .NE. 0. ) THEN
            Deadc1 = livetime/ontime
         ENDIF
         Status = 0
      ENDIF

c If it is present get the equinox

      Equin = 0
      CALL FTGKYE(unit,'equinox',equin,comment,Status)
      Status = 0

c and the RADECSYS

      Cproj(9) = ' '
      CALL FTGKYS(unit,'RADECSYS',Cproj(9),comment,Status)      
      Status = 0

c If they are present get the SC sequence end time and the number of OBIs

      Obsinf(1) = -999
      CALL FTGKYJ(unit, 'SCSEQEND', Obsinf(1), comment, Status)
      Status = 0

      Obsinf(2) = -999
      CALL FTGKYJ(unit, 'NUM_OBIS', Obsinf(2), comment, Status)
      Status = 0

c  Close the file

      CALL FTCLOS(unit, Status)
      contxt = 'Failed to close file'
      CALL FRELUN(unit)

c  If there was an error then dump out diagnostics

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcecho(Infile1)
         CALL fcerr(contxt)
         CALL fcerrm(Status)
      ENDIF

      RETURN
      END

    
