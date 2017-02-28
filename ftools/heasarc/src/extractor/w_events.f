
      SUBROUTINE w_events(unit, evlun, plun, status)

      IMPLICIT NONE

      INTEGER unit, evlun, plun, status

c Subroutine to write the current lot of events into the output event
c file

c Arguments :
c      unit           i         i: I/O unit of current input events file
c      evlun          i         i: I/O unit of opened output events file
c      plun           i         i: I/O unit of opened file of pointers
c      status         i         r: 0==OK

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'
      INCLUDE 'keys.inc'

      INTEGER MAXELT
      PARAMETER(MAXELT=1000)

      DOUBLE PRECISION tscal1 , tzero1, dtmp(MAXELT)
      DOUBLE PRECISION crpix(2), crval(2), crdelt(2), crota
      DOUBLE PRECISION xff, yff, xsky, ysky

      INTEGER i, hdutype
      INTEGER repeat1, tnull1, ier, width
      INTEGER rowsize , rowbuffer(ROWBUFFERSIZE) , k
      INTEGER tran(MAXCOLS) , types(MAXCOLS) , evtdu
      INTEGER ipt(MAXCOLS), repeat(MAXCOLS)
      INTEGER otfields , varidat
      INTEGER itmp , nrows, tfields, nevents
      INTEGER xif, yif, ipx, ipy
 
      character(256) stmp(MAXELT)
      character(20) extname, ttype1, tunit1, datatype1, tdisp1
      character(20) ttype(MAXCOLS), tunit(MAXCOLS)
      character(20) ottype(MAXCOLS)
      character(10) tform(MAXCOLS), otform(MAXCOLS)
      character(80) comment, contxt
      character(10) itmpstr
      character(10) ctype(2), cname(2), mform(2), mtype(2)
      character(4) coordt, fcoordt

      LOGICAL anyf, exactmatch, first, zsmsg
      LOGICAL wcsgood, wcsmatch

      INTEGER LENACT
      EXTERNAL LENACT

      character(40) taskname
      COMMON / task / taskname
 
      SAVE first, zsmsg, nevents
      SAVE otfields, otform, rowsize

      DATA first, zsmsg /.TRUE., .FALSE./

      Status = 0

c Get the binary header arrays for the current input file.
c evtdu stores the hdu with the events
 
      CALL FTGHDN(unit,evtdu)

      CALL FTGHBN(unit,MAXCOLS,nrows,tfields,ttype,tform,tunit,
     &            extname,varidat,Status)
      contxt = 'Problem getting input file binary header'
      IF ( Status.NE.0 ) GOTO 999

c If the first time through then set up the output event file which was
c opened earlier

      IF ( first ) THEN

c copy the primary header of the input file into the output file

         CALL FTMAHD(unit,1,hdutype,Status)
         CALL W_EVENTSH(unit,Evlun,Eventkey,Status)
         CALL FTRDEF(Evlun,Status)
         contxt = 'Failure while writing event file primary header'
         IF ( status .NE. 0 ) GOTO 999
         CALL FTUKYS(Evlun,'creator',taskname,'Extractor',Status)
         CALL FTPDAT(Evlun,status)
         contxt = 'Failed to write date to output event file'
         IF ( Status .NE. 0 ) GOTO 999


c now repeat for the event extension header

         CALL FTMAHD(unit,evtdu,hdutype,Status)
         CALL FTCRHD(Evlun,Status)
         CALL W_EVENTSH(unit,Evlun,Eventkey,Status)
         CALL FTUKYD(Evlun,'TIMEZERO',0.D0,15,'Time Zero',Status)
         CALL FTBDEF(Evlun,tfields,tform,0,0,Status)
         CALL FTMKYJ(Evlun,'naxis2',0,'Number of rows in table',
     &               Status)
         CALL FTGKYJ(Evlun,'naxis1',rowsize,comment,Status)
         CALL FTRDEF(Evlun,Status)
         CALL FTUKYS(Evlun,'EXTNAME',Eventname,'Name',Status)
         contxt = 'Failure while writing event extension header'
         IF ( status .NE. 0 ) GOTO 999
         IF ( rowsize.GT.4*ROWBUFFERSIZE ) THEN
            CALL fcerr(' ROWBUFFERSIZE is too small, increase in '//
     &                 'expar.inc')
            RETURN
         ENDIF
         CALL FTRDEF(Evlun,Status)

* Get the binary arrays for the output file

         CALL FTGHBN(Evlun,MAXCOLS,nrows,otfields,ottype,otform,tunit,
     &               extname,varidat,Status)
         contxt = 'Problem getting output file binary header'
         IF ( Status.NE.0 ) GOTO 999
 
* Make sure that the output time unit, if blank, is 's'

         itmp = -1
         CALL FTGCNO(Evlun,.FALSE.,Tcol,itmp,Status)
         Status = 0

         IF ( itmp.NE.-1 ) THEN
            IF ( tunit(itmp).EQ.' ' ) THEN
               WRITE (itmpstr,'(a,i1)') 'tunit' , itmp
               CALL FTUKYS(Evlun,itmpstr,'s','Unit for time column',
     &                     Status)
            ENDIF
         ENDIF

         first = .FALSE.
         nevents = 0

      ENDIF

c Set up the translation table from current input file to the output
c file - this is necessary if there are multiple input files and they
c have columns in different orders

      IF ( tfields.NE.otfields ) CALL XWARN1(
     &     'Input file and output file have different # fields',5)
 
      DO k = 1 , tfields

         CALL FTGCNO(Evlun,.FALSE.,ttype(k),tran(k),Status)
         IF ( Status.NE.0 ) THEN
            WRITE(comment,'(a,a,a,i4)') 
     &          'Failed to find column # for ', ttype(k), 
     &          ', status = ', Status
            CALL fcecho(comment)
            tran(k) = 0
            Status = 0
         ENDIF

         CALL FTBNFM(tform(k),types(k),repeat(k),width,Status)
         IF ( Status.NE.0 ) THEN
            WRITE(comment,'(a,a,a,i4,a,i4)') 
     &         'Failed to parse ', tform(k), ' from column ', k, 
     &         ', status = ', Status
            CALL fcecho(comment)
            Status = 0
         ENDIF

         CALL FTGBCL(Evlun,tran(k),ttype1,tunit1,datatype1,repeat1,
     &               tscal1,tzero1,tnull1,tdisp1,Status)
         IF ( Status.NE.0 ) THEN
            WRITE(comment,'(a,i5,a,i4)')
     &             'Failed to get information for column ', k,
     &             ', status = ', Status
            CALL fcecho(comment)
            Status = 0
         ENDIF

         IF ( tscal1.EQ.0.D0 ) THEN
            comment = 'Scaling 0 for column '//ttype(k)
            IF ( .NOT.zsmsg ) THEN
               CALL XWARN1(comment,5)
               CALL XWARN1('Column will not be copied',5)
            ENDIF
            tran(k) = 0
            zsmsg = .TRUE.
         ENDIF

      ENDDO
 
      exactmatch = .TRUE.

      IF ( tfields .NE. otfields ) THEN
         exactmatch = .FALSE.
      ELSE
         DO k = 1 , tfields
            IF ( tform(k).NE.otform(k) .OR. tran(k).NE.k )
     &          exactmatch = .FALSE.
         ENDDO
      ENDIF
 
      IF ( .NOT.exactmatch ) THEN
         comment = ' NOTICE: Input file structure is not an exact match'
         CALL XWRITE(comment,5)
         comment = ' to the output events file, using slow method'
         CALL XWRITE(comment,5)
      ENDIF

c Get the WCS information for this file

      CALL FTGCNO(Unit,.FALSE.,Xcolf,xif,Status)
      contxt = ' Column '//Xcolf(1:LENACT(Xcolf))//' Not found'
      IF ( Status.NE.0 ) GOTO 999

      CALL FTGCNO(Unit,.FALSE.,Ycolf,yif,Status)
      contxt = ' Column '//Ycolf(1:LENACT(Ycolf))//' Not found'
      IF ( Status.NE.0 ) GOTO 999

      CALL extwcs(Unit, xif, yif, .TRUE., 'image', crpix, crval, 
     &            crdelt, crota, ctype, cname, mtype, mform,
     &            wcsgood, Status)
      contxt = 'Failed to read WCS data from current file'
      IF ( Status.NE.0 ) GOTO 999

c Set the coordtype used by the coordinate conversion routines

      coordt = ctype(1)(lenact(ctype)-3:lenact(ctype))
      fcoordt = fctype(1)(lenact(fctype)-3:lenact(fctype))

c Assume it is a tangent plane if not set

      IF ( coordt .NE. '-SIN' .AND. coordt .NE. '-TAN' .AND.
     &     coordt .NE. '-ARC' .AND. coordt .NE. '-NCP' .AND.
     &     coordt .NE. '-GLS' .AND. coordt .NE. '-MER' .AND.
     &     coordt .NE. '-AIT' ) coordt = '-TAN'

      IF ( fcoordt .NE. '-SIN' .AND. fcoordt .NE. '-TAN' .AND.
     &     fcoordt .NE. '-ARC' .AND. fcoordt .NE. '-NCP' .AND.
     &     fcoordt .NE. '-GLS' .AND. fcoordt .NE. '-MER' .AND.
     &     fcoordt .NE. '-AIT' ) fcoordt = '-TAN'

c Now check whether this matches that for the first event file

      wcsmatch = .TRUE.
      IF ( wcsgood .AND. (
     &     crval(1) .NE. fcrval(1) .OR. crval(2) .NE. fcrval(2) .OR.
     &     crpix(1) .NE. fcrpix(1) .OR. crpix(2) .NE. fcrpix(2) .OR.
     &     crdelt(1) .NE. fcrdelt(1) .OR. crdelt(2) .NE. fcrdelt(2) .OR.
     &     crota .NE. fcrota .OR. coordt .NE. fcoordt ) ) THEN
         wcsmatch = .FALSE.
      ENDIF

      IF ( .NOT.wcsmatch ) THEN
         comment = ' NOTICE: Event files have differing WCS values, '//
     &             'using slow method'
         CALL XWRITE(comment,5)
      ENDIF


c Loop round selected event files
 
      CALL XWRITE(' Writing events file',5)

      REWIND (plun)
      READ (plun,IOSTAT=ier) i

c Different options required for the loop round events - place the IF blocks
c outside the DO WHILE for optimum speed. First do the easy case of direct
c byte copy

      IF ( exactmatch .AND. wcsmatch ) THEN

         DO WHILE ( ier.EQ.0 )
            Nevents = Nevents + 1

            CALL FTGTBB(unit,i,1,rowsize,rowbuffer,Status)
            contxt = 'Error reading event files with ftgtbb'
            IF ( Status.NE.0 ) GOTO 999

            CALL FTPTBB(Evlun,Nevents,1,rowsize,rowbuffer,Status)
            contxt = 'Error writing event file with FTPTBB'
            IF ( Status.NE.0 ) GOTO 999

            contxt = 'Error copying event file columns'
            IF ( Status.NE.0 ) GOTO 999

            READ (plun,IOSTAT=ier) i

         ENDDO

c Now the case of not an exactmatch but a wcsmatch so we don't have to do the
c WCS conversion. This is the slow method. All types except strings are read
c as doubles. In order to handle vector columns we have to index into the
c dtmp or stmp arrays for the start position of each column.

      ELSEIF ( .NOT.exactmatch .AND. wcsmatch ) THEN

         DO WHILE ( ier.EQ.0 )
            Nevents = Nevents + 1

            DO k = 1, tfields
               IF ( k .EQ. 1 ) THEN
                  ipt(k) = 1
               ELSE
                  ipt(k) = ipt(k-1) + repeat(k-1)
               ENDIF
               IF ( types(k) .NE. 16 ) THEN
                  CALL FTGCVD(unit,k,i,1,repeat(k),1,0.D0,dtmp(ipt(k)),
     &                        anyf,Status)
               ELSE
                  CALL FTGCVS(unit,k,i,1,repeat(k),' ',stmp(ipt(k)),
     &                        anyf,Status)
               ENDIF
            ENDDO

            DO k = tfields , 1 , -1
               IF ( tran(k).NE.0 ) THEN
                  IF ( types(k) .NE. 16 ) THEN
                     CALL FTPCLD(Evlun,tran(k),Nevents,1,repeat(k),
     &                           dtmp(ipt(k)),Status)
                  ELSE
                     CALL FTPCLS(Evlun,tran(k),Nevents,1,repeat(k),
     &                           stmp(ipt(k)),Status)
                  ENDIF
               ENDIF
            ENDDO

            contxt = 'Error copying event file columns'
            IF ( Status.NE.0 ) GOTO 999

            READ (plun,IOSTAT=ier) i

         ENDDO

c Finally do the case where we have to do a WCS conversion. 

      ELSEIF ( .NOT.wcsmatch ) THEN

         DO WHILE ( ier.EQ.0 )
            Nevents = Nevents + 1

            DO k = 1, tfields
               IF ( k .EQ. 1 ) THEN
                  ipt(k) = 1
               ELSE
                  ipt(k) = ipt(k-1) + repeat(k-1)
               ENDIF
               IF ( types(k) .NE. 16 ) THEN
                  CALL FTGCVD(unit,k,i,1,repeat(k),0.D0,dtmp(ipt(k)),
     &                        anyf,Status)
               ELSE
                  CALL FTGCVS(unit,k,i,1,repeat(k),' ',stmp(ipt(k)),
     &                        anyf,Status)
               ENDIF
            ENDDO

c if required modify the sky position for the WCS - do this by shifting from
c pixels to sky then back to pixels

            ipx = ipt(xif)
            ipy = ipt(yif)
            xff = dtmp(ipx)
            yff = dtmp(ipy)
            CALL FTWLDP(dtmp(ipx),dtmp(ipy),crval(1),crval(2),
     &                  crpix(1),crpix(2),crdelt(1),crdelt(2),
     &                  crota,coordt,xsky,ysky,Status)
            CALL FTXYPX(xsky,ysky,fcrval(1),fcrval(2),fcrpix(1),
     &                  fcrpix(2),fcrdelt(1),fcrdelt(2),fcrota,
     &                  fcoordt,dtmp(ipx),dtmp(ipy),Status)
            IF ( Status .NE. 0 ) THEN
               dtmp(ipx) = xff
               dtmp(ipy) = yff
               Status = 0
            ELSE

c If the sky X and Y columns are integers then make sure that we used NINT
c because by default cfitsio will truncate the real number when writing using
c FTPCLD leading to a possible pixel shift

               IF ( types(xif) .EQ. 21 .OR. types(xif) .EQ. 41 )
     &           dtmp(ipx) = NINT(dtmp(ipx))
               IF ( types(yif) .EQ. 21 .OR. types(yif) .EQ. 41 )
     &           dtmp(ipy) = NINT(dtmp(ipy))

            ENDIF

c write the row out

            DO k = tfields , 1 , -1
               IF ( tran(k).NE.0 ) THEN
                  IF ( types(k) .NE. 16 ) THEN
                     CALL FTPCLD(Evlun,tran(k),Nevents,1,repeat(k),
     &                           dtmp(ipt(k)),Status)
                  ELSE
                     CALL FTPCLS(Evlun,tran(k),Nevents,1,repeat(k),
     &                           stmp(ipt(k)),Status)
                  ENDIF
               ENDIF
            ENDDO

            contxt = 'Error copying event file columns'
            IF ( Status.NE.0 ) GOTO 999

            READ (plun,IOSTAT=ier) i

         ENDDO

      ENDIF

      WRITE(contxt, '(i7, a)') Nevents, 
     &                         ' events written to the output file'
      CALL fcecho(contxt)
 
      CALL FTUKYJ(Evlun,'naxis2',Nevents,'Number of events',Status)
      CALL FTRDEF(Evlun,Status)
      contxt = 'Error modifying the number of events'
      IF ( Status.NE.0 ) GOTO 999
 

      CALL FTMRHD(Evlun,-1,hdutype,Status)
      CALL FTUKYJ(Evlun,'nevents',Nevents,'Number of events',Status) 
      CALL FTMRHD(Evlun,1,hdutype,Status)
      contxt = 'Error setting the number of events in primary header'
      IF ( Status.NE.0 ) GOTO 999


 999  CONTINUE
      IF ( status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(status)
      ENDIF
 
      RETURN
      END

c***********************************************************************
C*==w_eventsh.spg  processed by SPAG 4.50J  at 15:13 on  8 Mar 1995
C Output routines for the extractor
CWrite out an events header.  This writes out dummy values so that getevents
Ccan write out the data as we go along.
 
      SUBROUTINE W_EVENTSH(Inlun, Outlun, Eventkey, Status)

      IMPLICIT NONE
 
      INTEGER Inlun , Outlun, Status
      CHARACTER*(*) Eventkey
 
      INCLUDE 'expar.inc'
 
C ignore holds the keywords to ignore
 
      character(10) ignore(MAXIGNORE)
      character(80) inline, contxt
      INTEGER lun1 , iignore
      INTEGER i , j
      LOGICAL found
 
      character(40) taskname
      COMMON / task / taskname
 
      DO 100 i = 1 , MAXIGNORE
         ignore(i) = ' '
 100  CONTINUE
      inline = ' '
      lun1 = 0
      iignore = 0
      j = 0
      status = 0
      found = .FALSE.

      iignore = 0
      IF ( Eventkey.NE.' ' ) THEN
         CALL GETLUN(lun1)
         CALL OPENWR(lun1,Eventkey,'old',' ',' ',0,1,Status)
         contxt = ' Problem opening ignore keywords file: '//Eventkey
         IF ( Status .NE. 0 ) GOTO 999
 
         READ (lun1,*,IOSTAT=Status) inline
         DO WHILE ( Status .EQ. 0 )
            iignore = iignore + 1
            IF ( iignore.GT.MAXIGNORE ) THEN
               CALL XWARN1(' Too many ignored keywords: ',5)
               GOTO 150
            ENDIF
            ignore(iignore) = inline(1:10)
            READ (lun1,*,IOSTAT=Status) inline
         ENDDO
 150     CLOSE (lun1)
         CALL FRELUN(lun1)
      ENDIF
      Status = 0

      inline = ' '
      i = 1
      found = .FALSE.
      CALL FTGREC(Inlun,i,inline,Status)
      DO WHILE ( Status.EQ.0 .AND. inline(1:8).NE.'END     ' )
         DO 200 j = 1 , iignore
            IF ( .NOT.found ) found = (inline(1:8).EQ.ignore(j)(1:8))
 200     CONTINUE
         IF ( .NOT.found ) CALL FTPREC(Outlun,inline,status)
         i = i + 1
         CALL FTGREC(Inlun,i,inline,status)
      ENDDO
      Status = 0

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(Status)
      ENDIF

      RETURN
      END

c ***********************************************************************

      SUBROUTINE w_events_end(Evlun, Imaxgtis, Imaxccd, Gtis, 
     &                        Gtihdunms, Exposs, Qinreg, Area, Status)

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'
      INCLUDE 'keys.inc'

      DOUBLE PRECISION Gtis(3,MAXGTI,MAXCCD),Exposs(MAXCCD)
      DOUBLE PRECISION Area
      INTEGER Evlun, Imaxgtis(MAXCCD),Imaxccd, Status
      character(30) Gtihdunms(MAXCCD)
      LOGICAL Qinreg(0:MAXCCD-1)

c Subroutine to close out the output events file by writing the time
c keywords and the GTI extension. Also copies any extra extensions from
c the first input file. The output events file is assumed to be open.

c Arguments :
c    Evlun      i            i: Unit for output events file
c    Imaxgti    i            i: Actual number of GTIs
c    Gti        d            i: Good time intervals
c    Area       d            i: The size of the selected area in pixels
c    Qinreg     l            i: If true events were selected from this CCD
c    Status     i            r: O==OK


      INTEGER unit, hdutype, blocksize, i, j

      CHARACTER(80) contxt
      CHARACTER(255) infile1
      CHARACTER(20) extname
      CHARACTER(72) comment

      LOGICAL qcopy

      INTEGER lenact
      character(255) exevfl
      EXTERNAL lenact, exevfl

c Do the primary header time keywords

      CALL FTMAHD(Evlun,1,hdutype,Status)
      contxt = 'Failed to move to first extension'
      IF ( status .NE. 0 ) GOTO 999      

      CALL wstdky(Evlun, Status)

      CALL FTUKYD(Evlun,'TIMEZERO',0.D0,15,'Time Zero',Status)
 
      contxt = 'Unable to write keywords to primary header'
      IF ( Status.NE.0 ) GOTO 999

c Now do the events extension
 
      CALL FTMRHD(Evlun,1,hdutype,Status)
      contxt = 'Failed to move to second extension'
      IF ( status .NE. 0 ) GOTO 999      

      CALL wstdky(Evlun, Status)
      contxt = 'Unable to write keywords to events extension header'
      IF ( Status.NE.0 ) GOTO 999

c Write the data subspace keywords

      CALL w_dskeys(Evlun, Status)
      contxt = 'Failed to write the data subspace keywords'
      IF ( Status.NE.0 ) GOTO 999
             
      CALL FTUKYE(Evlun, 'NPIXSOU', SNGL(Area*binf*binf), 10,
     &        'Number of pixels in selected region',status)
      contxt = 'Failed to write NPIXSOU to events extension header'
      IF ( Status.NE.0 ) GOTO 999

c Write the GTI extensions on the end

      IF ( imaxccd .GT. 1 ) THEN
         DO i = 1, imaxccd
            IF ( Qinreg(NINT(Gtis(3,1,i))) ) THEN
               CALL w_gti(Evlun, Imaxgtis(i), MAXGTI,
     &                    Gtis(1,1,i), Gtihdunms(i), exposs(i),
     &                    NINT(Gtis(3,1,i)), evgtinm, Status)
            ENDIF
         ENDDO
      ELSE
         CALL w_gti(Evlun, Imaxgtis(1), MAXGTI,
     &              Gtis(1,1,1), Gtihdunms(1), exposs(1),
     &              NINT(Gtis(3,1,1)), evgtinm, Status)
      ENDIF
      contxt = 'Failure in W_GTI'
      IF ( Status.NE.0 ) GOTO 999
         
c If a region file was defined then write a region extension

      CALL w_regions(Evlun, Status)
      contxt = 'Failure in W_REGIONS'
      IF ( Status.NE.0 ) GOTO 999
      
c If required copy extra extensions from the first input event file

      IF ( copyall ) THEN

c Get the name of the first input event file and open it. If we can't
c open it then give up and jump to closing the output event file

         infile1 = exevfl(1, .FALSE., Status)
         CALL getlun(unit)
         CALL ftopen(unit, infile1, 0, blocksize, Status)
         IF ( Status.NE.0 ) THEN
            contxt = 'Failed to open '//infile1(:lenact(infile1))
            CALL fcecho(contxt)
            Status = 0
            GOTO 100
         ENDIF

c Loop round the input file extensions

         i = 2
         CALL FTMAHD(unit,i,hdutype,Status)
         DO WHILE ( status .EQ. 0 )

c Get the extension name and/or the HDUNAME. If they do not match the
c event or gti names or have HDUCLAS1='GTI' then create a new output HDU 
c and copy the current input HDU.

            qcopy = .TRUE.

            CALL ftgkys(unit, 'EXTNAME', extname,  comment, Status)
            IF ( extname .EQ. eventname ) qcopy = .FALSE.
            IF ( extname .EQ. evgtinm ) qcopy = .FALSE.

            CALL ftgkys(unit, 'HDUNAME', extname,  comment, Status)
            IF ( status .EQ. 0 ) THEN
               IF ( extname .EQ. gtihdunm ) qcopy = .FALSE.
               DO j = 1, imaxccd
                  IF ( extname .EQ. Gtihdunms(j) ) qcopy = .FALSE.
               ENDDO
            ELSE
               status = 0
            ENDIF

            CALL ftgkys(unit, 'HDUCLAS1', extname,  comment, Status)
            IF ( status .EQ. 0 ) THEN
               CALL upc(extname)
               IF ( extname(1:3) .EQ. 'GTI' ) qcopy = .FALSE.
            ELSE
               status = 0
            ENDIF

            IF ( qcopy ) THEN

               CALL ftcrhd(Evlun, Status)
               CALL ftcopy(unit, Evlun, 0, Status)
               contxt = 'Failed to copy HDU to new event file'
               IF ( status .NE. 0 ) GOTO 999

            ENDIF

c Move onto next input HDU

            i = i + 1
            CALL FTMAHD(unit,i,hdutype,Status)            

         ENDDO

         Status = 0

      ENDIF

 100  CONTINUE

c finally close the output event file

      CALL xclsfl(Evlun, Status)

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(Status)
      ENDIF

      RETURN
      END

