
C Routine to read the Mission DataBase to get some piece of information.
C The database is arranged for Mission:SubMission:Instrument:Datamode and 
C uses an inheritance structure such that any keyword is searched for 
C first at the Datamode level, then the Instrument, then the SubMission, 
C and finally the Mission eg the entry Mission:Instrument:x overrides the
C entry Mission:x. If a string is not detected then Status=-1 and Result=" ".

      SUBROUTINE XSL_MDBS(Mission, SubMission, Instrument, Datamode, 
     &                    Keyword, Result, Status)

      CHARACTER*(*) Mission, SubMission, Instrument, Datamode, Keyword 
      CHARACTER*(*) Result
      INTEGER Status

C Argument :
C   Mission     C*(*)          i: Mission in use
C   SubMission  C*(*)          i: SubMission in use - mainly used to
C                                 distinguish between ROSAT RDF and OLD format
C   Instrument  C*(*)          i: Instrument in use
C   Datamode    C*(*)          i: Datamode in use
C   Keyword     C*(*)          i: Keyword requested
C   Result      C*(*)          r: Value of the keyword
C   Status      I              r: 0==OK, -1==not found

      INCLUDE 'xsel.inc'

      INTEGER MAXSTR
      PARAMETER (MAXSTR=1000)

      INTEGER lun, ierr, i, j, lfil, istr, nstr
      INTEGER nkey

      character(255) key(4), testkey(4), filenm
      character(512) wrtstr
      character(2048) inline
      character(20) savmis, testmis
      character(2048) savstr(MAXSTR,2), teststr

      LOGICAL qdone

      INTEGER lenact, lchop
      EXTERNAL lenact, lchop

      SAVE savmis, savstr, nstr

      DATA savmis /'NONE'/

      Status = 0
      Result = ' '

      CALL UPC(Mission)

c Check whether this is a new mission. If so read the appropriate data
c from the MDB file.

      IF ( savmis .NE. Mission ) THEN

C Check whether there is an XSELECT_MDB environment variable defined. If
C so use this as the filename otherwise construct the filename using the
C FTOOLS variable.

         CALL TRLOG('XSELECT_MDB', 11, filenm, lfil)
         IF ( lfil .EQ. 0 ) THEN
            filenm = xslnam(:lenact(xslnam))//'.mdb'
            CALL ptend(xsldsk, xsldir, filenm)
         ENDIF

C Open the MDB file

         CALL getlun(lun)
         CALL openwr(lun, filenm, 'old', ' ', ' ', 0, 1, ierr)
         IF ( ierr .NE. 0 ) THEN
            wrtstr = 'Failed to open '//filenm
            CALL xwrite(wrtstr, 5)
            Status = 1
            RETURN
         ENDIF

C Loop through the file accumulating the savstr array

         nstr = 0
         READ(lun, '(a)', iostat=ierr) inline
         qdone = .FALSE.
         IF ( ierr .NE. 0 ) qdone = .TRUE.

         DO WHILE ( .NOT.qdone )

            testmis = inline(:lenact(Mission))
            CALL upc(testmis)

            IF ( testmis .EQ. Mission ) THEN
               IF ( nstr .LE. MAXSTR ) THEN
                  nstr = nstr + 1

C we have found a string we want to save so split it into the key and the value

                  teststr = inline(lchop(inline):lenact(inline))
                  i = index(teststr,' ')
                  j = index(teststr,'	')
                  IF ( j.NE.0 .AND. j.LT.i ) i = j
                  savstr(nstr,1) = teststr(:i-1)
                  CALL upc(savstr(nstr,1))
                  teststr = teststr(i:)
                  i = 1
                  DO WHILE ( ( teststr(i:i) .EQ. ' ' .OR.
     &                         teststr(i:i) .EQ. '	' ) .AND.
     &                         i .LT. lenact(teststr) )
                     i = i + 1
                  ENDDO
                  savstr(nstr,2) = teststr(i:lenact(teststr))
               ELSE
                  wrtstr = 'The number of entries in the mdb file '//
     &                     'exceeds the maximum allowed'
                  CALL XWRITE(wrtstr, 5)
                  wrtstr = 'Increase MAXSTR in xsel_mdb.f'
                  CALL XWRITE(wrtstr, 5)
                  qdone = .TRUE.
               ENDIF
            ENDIF

            IF ( .NOT.qdone ) THEN
               READ(lun, '(a)', iostat=ierr) inline
               IF ( ierr .NE. 0 ) qdone = .TRUE.
            ENDIF

         ENDDO

         CLOSE(lun)
         CALL frelun(lun)

         savmis = Mission

      ENDIF

C Set up the search key array. The Mission key matches by construction.

      nkey = 0
      IF ( lenact(SubMission) .GT. 0 .AND. 
     &     SubMission .NE. 'NONE' ) THEN
         nkey = nkey + 1
         key(nkey) = SubMission
      ENDIF
      IF ( lenact(Instrument) .GT. 0 .AND. 
     &     Instrument .NE. 'NONE' ) THEN
         nkey = nkey + 1
         key(nkey) = Instrument
      ENDIF
      IF ( lenact(Datamode) .GT. 0 .AND. 
     &     Datamode .NE. 'NONE' ) THEN
         nkey = nkey + 1
         key(nkey) = Datamode
      ENDIF


C Set up the strings to be tested

      DO i = 1, nkey+1
         testkey(i) = Mission(:lenact(Mission))
         DO j = 1, nkey-i+1
            testkey(i) = testkey(i)(:lenact(testkey(i)))//':'//
     &           key(j)(:lenact(key(j)))
         ENDDO
         testkey(i) = testkey(i)(:lenact(testkey(i)))//':'//
     &           Keyword(:lenact(Keyword))
      ENDDO

      DO i = 1, nkey+1
         CALL upc(testkey(i))
      ENDDO


C Loop through the savstr array looking for the one of the possible keys

      DO i = 1, nkey+1

         istr = 0
         DO WHILE ( istr .NE. nstr )

            istr = istr + 1

            IF ( testkey(i) .EQ. savstr(istr,1) ) THEN
               Result = savstr(istr,2)
               RETURN
            ENDIF
         ENDDO

      ENDDO

C We were not successful so return Status=-1 and Result=" "

      Status = -1

      RETURN
      END

c *************************************************************

      SUBROUTINE XSL_MDBJ(Mission, SubMission, Instrument, Datamode, 
     &                    Keyword, Result,Status)

      CHARACTER*(*) Mission, SubMission, Instrument, Datamode, Keyword
      INTEGER Result, Status

C Argument :
C   Mission     C*(*)          i: Mission in use
C   SubMission  C*(*)          i: SubMission in use - mainly used to
C                                 distinguish between ROSAT RDF and OLD format
C   Instrument  C*(*)          i: Instrument in use
C   Datamode    C*(*)          i: Datamode in use
C   Keyword     C*(*)          i: Keyword requested
C   Result      I              r: Value of the keyword
C   Status      I              r: 0==OK, -1==not found, other==error

      character(255) string

      INTEGER lenact
      EXTERNAL lenact

C get the argument as a string

      CALL XSL_MDBS(Mission, SubMission, Instrument, Datamode, Keyword, 
     &              string, Status)

C read an integer

      IF ( Status .EQ. 0 ) THEN

         IF ( lenact(string) .EQ. 1 ) THEN 
            READ(string(1:1), '(i1)', iostat=Status) Result
         ELSEIF ( lenact(string) .EQ. 2 ) THEN 
            READ(string(1:2), '(i2)', iostat=Status) Result
         ELSEIF ( lenact(string) .EQ. 3 ) THEN 
            READ(string(1:3), '(i3)', iostat=Status) Result
         ELSEIF ( lenact(string) .EQ. 4 ) THEN 
            READ(string(1:4), '(i4)', iostat=Status) Result
         ELSEIF ( lenact(string) .EQ. 5 ) THEN 
            READ(string(1:5), '(i5)', iostat=Status) Result
         ELSEIF ( lenact(string) .EQ. 6 ) THEN 
            READ(string(1:6), '(i6)', iostat=Status) Result
         ELSEIF ( lenact(string) .EQ. 7 ) THEN 
            READ(string(1:7), '(i7)', iostat=Status) Result
         ELSEIF ( lenact(string) .EQ. 8 ) THEN 
            READ(string(1:8), '(i8)', iostat=Status) Result
         ENDIF

      ENDIF

      IF ( Status .NE. 0 ) Result = 0

      RETURN
      END

c *************************************************************

      SUBROUTINE XSL_MDBE(Mission, SubMission, Instrument, Datamode, 
     &                    Keyword, Result,Status)

      CHARACTER*(*) Mission, SubMission, Instrument, Datamode, Keyword
      REAL Result
      INTEGER Status

C Argument :
C   Mission     C*(*)          i: Mission in use
C   SubMission  C*(*)          i: SubMission in use - mainly used to
C                                 distinguish between ROSAT RDF and OLD format
C   Instrument  C*(*)          i: Instrument in use
C   Datamode    C*(*)          i: Datamode in use
C   Keyword     C*(*)          i: Keyword requested
C   Result      R              r: Value of the keyword
C   Status      I              r: 0==OK, -1==not found, other==error

      character(255) string

      INTEGER lenact
      EXTERNAL lenact

C get the argument as a string

      CALL XSL_MDBS(Mission, SubMission, Instrument, Datamode, Keyword, 
     &              string, Status)

C read a real

      IF ( Status .EQ. 0 ) THEN
         READ(string(:lenact(string)), '(g15.0)', iostat=Status) Result
      ENDIF

      IF ( Status .NE. 0 ) Result = 0

      RETURN
      END

c *************************************************************

      SUBROUTINE XSL_MDBD(Mission, SubMission, Instrument, Datamode, 
     &                    Keyword, Result,Status)

      CHARACTER*(*) Mission, SubMission, Instrument, Datamode, Keyword
      DOUBLE PRECISION Result
      INTEGER Status

C Argument :
C   Mission     C*(*)          i: Mission in use
C   SubMission  C*(*)          i: SubMission in use - mainly used to
C                                 distinguish between ROSAT RDF and OLD format
C   Instrument  C*(*)          i: Instrument in use
C   Datamode    C*(*)          i: Datamode in use
C   Keyword     C*(*)          i: Keyword requested
C   Result      D              r: Value of the keyword
C   Status      I              r: 0==OK, -1==not found, other==error

      character(255) string

      INTEGER lenact
      EXTERNAL lenact

C get the argument as a string

      CALL XSL_MDBS(Mission, SubMission, Instrument, Datamode, Keyword, 
     &              string, Status)

C read a double precision

      IF ( Status .EQ. 0 ) THEN
         READ(string(:lenact(string)), '(g15.0)', iostat=Status) Result
      ENDIF

      IF ( Status .NE. 0 ) Result = 0

      RETURN
      END

c *************************************************************

      SUBROUTINE XSL_MDBB(Mission, SubMission, Instrument, Datamode, 
     &                    Keyword, Result,Status)

      CHARACTER*(*) Mission, SubMission, Instrument, Datamode, Keyword
      INTEGER Status
      LOGICAL Result

C Argument :
C   Mission     C*(*)          i: Mission in use
C   SubMission  C*(*)          i: SubMission in use - mainly used to
C                                 distinguish between ROSAT RDF and OLD format
C   Instrument    C*(*)        i: Instrument in use
C   Datamode    C*(*)          i: Datamode in use
C   Keyword     C*(*)          i: Keyword requested
C   Result      L              r: Value of the keyword
C   Status      I              r: 0==OK, -1==not found, other==error

      character(255) string

      INTEGER lenact
      EXTERNAL lenact

C get the argument as a string

      CALL XSL_MDBS(Mission, SubMission, Instrument, Datamode, Keyword, 
     &              string, Status)

C if the string is "yes" then return true, if "no" then return false.

      Result = .FALSE.
      IF ( Status .EQ. 0 ) THEN
         IF ( string(1:1) .EQ. 'y' .OR. 
     &        string(1:1) .EQ. 'Y' ) Result = .TRUE.
      ENDIF

      RETURN
      END

c *************************************************************

      SUBROUTINE XSL_MDB_INST(Mission, Submis, Instru, Obsno, Status)

      CHARACTER*(*) Mission, Submis, Instru
      INTEGER Obsno, Status

C Argument :
C   Mission     C*(*)          i: Mission in use
C   SubMission  C*(*)          i: SubMission in use
C   Instru      C*(*)        i/r: Instrument name - on return full name of 
C                                 matched Instrument
C   Obsno       I              r: Number of the detector, -5 if none found
C   Status      I              r: 0==OK, -1==not found, other==error

      INTEGER best, match, i, iflag, idelim, lenn, lenb, lene
      INTEGER idet

      LOGICAL qdone, qskip

      CHARACTER(255) detstr, outname

      INTEGER lenact
      EXTERNAL lenact

      Status = 0

      CALL upc(Instru)

C Get the instrument string for this mission

      CALL XSL_MDBS(Mission, Submis, ' ', ' ', 'instruments', detstr, 
     &              Status)

      best = 0
      idet = 0
      obsno = -5
      outname = 'NONE'
      lenn = 0
      qdone = .FALSE.

C Loop around the instruments in the string

      DO WHILE ( .NOT.qdone )

         CALL xgtarg(detstr, lenn, lenb, lene, qskip, iflag, idelim)
         IF ( iflag .NE. 0 ) GOTO 100
         idet = idet + 1

C Find how well the input name matches this Instrument

         match = 0
         DO i = lenb, lene
            IF ( Instru(i-lenb+1:i-lenb+1) .EQ. detstr(i:i) )  
     &           match = match + 1
         ENDDO

C If this the best match then set out the output name and obsno

         IF ( match .GT. best ) THEN
            best = match
            outname = detstr(lenb:lene)
            obsno = idet
         ENDIF

      ENDDO

 100  CONTINUE
      Instru = outname(:MIN(len(outname),len(Instru)))

      RETURN
      END

c *************************************************************

      SUBROUTINE XSL_MDB_MODE(Mission, Submis, Instru, Datamode, Status)

      CHARACTER*(*) Mission, Submis, Instru, Datamode
      INTEGER Status

C Argument :
C   Mission     C*(*)          i: Mission in use
C   SubMission  C*(*)          i: SubMission in use
C   Instru      C*(*)          i: Instrument in use
C   Datamode    C*(*)        i/r: Datamode name - on return full name of 
C                                 matched Datamode
C   Status      I              r: 0==OK, -1==not found, other==error

      INTEGER best, match, i, iflag, idelim, lenn, lenb, lene

      LOGICAL qdone, qskip

      character(2048) modstr, outname, updmode, upmods

      INTEGER lenact
      EXTERNAL lenact

      Status = 0

      updmode = Datamode
      CALL upc(updmode)

C Get the modes string for this mission and instrument

      CALL XSL_MDBS(Mission, Submis, Instru, ' ', 'modes', modstr, 
     &              Status)

      upmods = modstr
      CALL upc(upmods)

      best = 0
      outname = 'NONE'
      lenn = 0
      qdone = .FALSE.

C Loop around the instruments in the string

      DO WHILE ( .NOT.qdone )

         CALL xgtarg(modstr, lenn, lenb, lene, qskip, iflag, idelim)
         IF ( iflag .NE. 0 ) GOTO 100


C If we have an exact match then we are done

         IF ( upmods(lenb:lenn) .EQ. updmode(1:lenact(updmode)) )
     &       RETURN

C Find how well the input name matches this Datamode

         match = 0
         DO i = lenb, lene
            IF ( index(updmode,upmods(i:i)) .NE. 0 ) match = match + 1
         ENDDO

C If this the best match then set out the output name and obsno

         IF ( match .GT. best ) THEN
            best = match
            outname = modstr(lenb:lene)
         ENDIF

      ENDDO

 100  CONTINUE
      Datamode = outname(:MIN(len(outname),len(Datamode)))

      RETURN
      END



