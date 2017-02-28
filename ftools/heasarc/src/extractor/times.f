C*==addgti.spg  processed by SPAG 4.50J  at 14:12 on 25 Oct 1995


      SUBROUTINE ADDGTI(Gti,Imaxgti,Gtiw,Gtiname,Gtiread,Status)

      IMPLICIT NONE

      INCLUDE 'expar.inc'

      INTEGER Imaxgti , Status
      DOUBLE PRECISION Gti(3,MAXGTI), Gtiw(3,MAXGTI)
      CHARACTER*(*) Gtiname
      LOGICAL Gtiread


C     file is the file to load, if ' ' reset saved file name and return
C     time is the time to check
C     ok is true if it came in true and the time was with one of the time windows
C     Gti is a real*2 of (2,maxgti) which holds 1 to imaxgti of good time
C     intervals

      DOUBLE PRECISION gtic(2,MAXGTI)

      INTEGER imaxgtic
      INTEGER ier, i, type

      CHARACTER(255) filenm

      INCLUDE 'exwin.inc'

      INTEGER lenact, tmftyp
      CHARACTER(255) exnxtm
      EXTERNAL lenact, tmftyp, exnxtm

c Loop around the input time filter files
      i=0
      Gtiread = .false.
      Status = 0

      filenm = exnxtm(.true., ier)
      DO WHILE ( ier .EQ. 0 )

c Find out what type of file this.

         type = tmftyp(filenm, status)
         IF ( status .NE. 0 ) THEN
            status = 0
            GOTO 100
         ENDIF

c If this is the first file then write straight into the gti array

         IF ( .NOT.Gtiread ) THEN

            IF ( type .EQ. 1 ) THEN
               CALL ADDGTI1(filenm,Gti,MAXGTI,Imaxgti,Gtiname,status)
               Gtiread = .TRUE.
            ELSEIF ( type .EQ. 2 ) THEN
               nwi(1) = 0
               CALL XRRDWIN(1,filenm,twia,twio,pwi,pwia,pwio,fwia,
     &                      fwio,ewia,ewio,nwi,nwito,status)
               IF ( status .EQ. 0 .AND. nwi(1) .GT. 0 ) THEN
                  nwi(i) = MIN(nwi(i), MAXGTI)
                  DO i = 1 , nwi(1)
                     gti(1,i) = twia(i)*86400.D0
                     gti(2,i) = twio(i)*86400.D0
                  ENDDO
                  imaxgti = nwi(1)
                  Gtiread = .TRUE.
               ENDIF
            ELSEIF ( type .EQ. 3 ) THEN
               CALL ADDGTI2(filenm,Gti,MAXGTI,Imaxgti,status)
               Gtiread = .TRUE.
            ENDIF
            IF ( status .NE. 0 ) THEN
               status = 0
               GOTO 100
            ENDIF

            CALL EXTMGTI(Gti,MAXGTI,Imaxgti,status)
            CALL PRINTGTI(Gti,MAXGTI,Imaxgti,'First gti')

         ELSE

c If this is not the first time through then write into the gtic array
c and then AND into the gti array

            imaxgtic = 0
            DO i = 1 , MAXGTI
               gtic(1,i) = 0.0
               gtic(2,i) = 0.0
            ENDDO

            IF ( type .EQ. 1 ) THEN
               CALL ADDGTI1(filenm,Gtic,MAXGTI,Imaxgtic,Gtiname,status)
            ELSEIF ( type .EQ. 2 ) THEN
               nwi(1) = 0
               CALL XRRDWIN(1,filenm,twia,twio,pwi,pwia,pwio,fwia,
     &                      fwio,ewia,ewio,nwi,nwito,status)
               IF ( status .EQ. 0 .AND. nwi(1) .GT. 0 ) THEN
                  nwi(i) = MIN(nwi(i), MAXGTI)
                  DO i = 1 , nwi(1)
                     gtic(1,i) = twia(i)*86400.D0
                     gtic(2,i) = twio(i)*86400.D0
                  ENDDO
                  imaxgtic = nwi(1)
               ENDIF
            ELSEIF ( type .EQ. 3 ) THEN
               CALL ADDGTI2(filenm,Gtic,MAXGTI,Imaxgtic,status)
            ENDIF
            IF ( status .NE. 0 ) THEN
               status = 0
               GOTO 100
            ENDIF

            CALL EXTMGTI(gtic,MAXGTI,imaxgtic,status)
            CALL PRINTGTI(gtic,MAXGTI,imaxgtic,'Added gtic')

            CALL FINGTI(Gti,Imaxgti,gtic,imaxgtic,gtiw,MAXGTI,status)
            CALL PRINTGTI(Gti,MAXGTI,Imaxgti,'Anded gti')

         ENDIF

c Get the next time filter filename

 100     CONTINUE
         filenm = exnxtm(.false., ier)

      ENDDO

c do a final sort/merge on the gti

      CALL PRINTGTI(Gti,MAXGTI,Imaxgti,'gti before final merge')
      CALL EXTMGTI(Gti,MAXGTI,Imaxgti,status)
      CALL PRINTGTI(Gti,MAXGTI,Imaxgti,'gti after final merge')

      RETURN
      END
C*==addgti1.spg  processed by SPAG 4.50J  at 14:12 on 25 Oct 1995

      SUBROUTINE ADDGTI1(Gtifile,Gti,Maxgti1,Imaxgti,Gtiname,Status)

      IMPLICIT NONE

      INCLUDE 'expar.inc'

      INTEGER unit , Maxgti1 , Status , Imaxgti
      DOUBLE PRECISION Gti(3,Maxgti1)
      CHARACTER*(*) Gtiname , Gtifile

      INTEGER i

      INTEGER nrows , blocksize, hdutype

      character(80) errormsg
      LOGICAL anyf

      INTEGER starti , stopi, stat2
      INTEGER ilen , row
      LOGICAL qdone
      DOUBLE PRECISION timezero

      INTEGER LENACT
      EXTERNAL LENACT

      unit = 0
      i = 0
      nrows = 0
      errormsg = ' '
      anyf = .FALSE.
      starti = 0
      stopi = 0
      row = 0
      timezero = 0.0

c Attempt to construct the filename and extension name to open the file and
c move to the correct extension.

c If an extension is specified as part of the filename open the file and
c move to it

      qdone = .FALSE.
      CALL GETLUN(unit)
      Status = 0

      IF ( index(Gtifile,']').EQ.lenact(Gtifile) ) THEN

         CALL FTNOPN(unit,Gtifile,0,Status)
         IF ( Status.EQ.0 ) qdone = .TRUE.

      ELSEIF ( index(Gtifile,'+').NE.0 ) THEN

         ilen = index(Gtifile,'+')
         IF ( index(Gtifile(ilen+1:),'.') .EQ. 0 ) THEN
            CALL FTNOPN(unit,Gtifile,0,Status)
            IF ( Status.EQ.0 ) qdone = .TRUE.
         ENDIF
      
      ENDIF

c the extension was not specified so we have to find it - first try Gtiname

      Status = 0
      CALL FTOPEN(unit,Gtifile,0,blocksize,Status)
      IF ( Status .NE. 0 ) THEN
         errormsg = ' Failure opening '//Gtifile(1:LENACT(Gtifile))
         CALL fcerr(errormsg)
         CALL fcerrm(status)
         CALL FRELUN(unit)
         RETURN
      ENDIF

      IF (.NOT.qdone .AND. Gtiname.NE.' ' .AND. Gtiname.NE.'NONE') THEN

         CALL FTMNHD(unit,2,Gtiname,1,Status)
         IF ( Status.EQ.0 ) qdone = .TRUE.

      ENDIF

c If that didn't work try STDGTI

      IF ( .NOT.qdone ) THEN

         Status = 0
         CALL FTMNHD(unit,2,'STDGTI',1,Status)
         IF ( Status.EQ.0 ) qdone = .TRUE.

      ENDIF

c Finally just read the second HDU

      IF ( .NOT.qdone ) THEN

         Status = 0
         CALL FTMAHD(unit,2,hdutype,Status)
         IF ( Status.EQ.0 ) qdone = .TRUE.

      ENDIF

      IF ( .NOT.qdone ) THEN
         errormsg = ' Failure opening '//Gtifile(1:LENACT(Gtifile))
         CALL fcerr(errormsg)
         CALL fcerrm(status)
         Status = 0
         CALL FTCLOS(unit, status)
         CALL FRELUN(unit)
         RETURN
      ENDIF
         
C get the table information

      CALL FTGKYJ(unit,'NAXIS2',nrows,errormsg,Status)
      errormsg = 'Failed to read NAXIS2 in '//
     &           Gtifile(1:LENACT(Gtifile))
      IF ( Status .NE. 0 ) GOTO 999

      timezero = 0.D0
      CALL FTGKYD(unit,'timezero',timezero,errormsg,Status)
      IF ( Status.NE.0 ) THEN
         Status = 0
         timezero = 0.D0
      ENDIF
      CALL FTGCNO(unit,.FALSE.,'start',starti,Status)
      CALL FTGCNO(unit,.FALSE.,'stop',stopi,Status)
      errormsg = ' START or STOP columns not found for file '//
     &     Gtifile
      IF ( Status.NE.0 ) GOTO 999

      IF ( nrows+Imaxgti .GT. Maxgti1 ) THEN
         Status = -1
         errormsg = 'MAXGTI is too small'
         GOTO 999
       ENDIF

      DO i = Imaxgti + 1 , nrows + Imaxgti
         row = i - Imaxgti
         CALL FTGCVD(unit,starti,row,1,1,0.D0,Gti(1,i),anyf,Status)
         CALL FTGCVD(unit,stopi,row,1,1,0.D0,Gti(2,i),anyf,Status)
         Gti(1,i) = Gti(1,i) + timezero
         Gti(2,i) = Gti(2,i) + timezero
      ENDDO

      Imaxgti = Imaxgti + nrows

 999  CONTINUE
      
      IF ( Status.NE.0 ) THEN
         CALL fcerr(errormsg)
         CALL fcerrm(status)
      ENDIF

      stat2 = 0
      CALL FTCLOS(unit,stat2)
      CALL FRELUN(unit)

      RETURN
      END
C*==addgti2.spg  processed by SPAG 4.50J  at 14:12 on 25 Oct 1995

C
C     Addasctem


      SUBROUTINE ADDGTI2(Asctem,Gtic,Maxgti,Imaxgtic,Status)

      IMPLICIT NONE

      CHARACTER*(*) Asctem
      INTEGER Imaxgtic , Maxgti , i , lun1 , ier, Status
      DOUBLE PRECISION Gtic(3,Maxgti)
      character(80) errormsg

      i = 0
      lun1 = 0
      ier = 0
      errormsg = ' '
      Status = 0

      CALL GETLUN(lun1)
      CALL OPENWR(lun1,Asctem,'OLD',' ',' ',0,1,ier)
      IF ( ier.NE.0 ) THEN
         errormsg = ' File '//Asctem//' not able to be opened'
         CALL fcerr(errormsg)
         CALL fcerrm(ier)
         Status = ier
         RETURN
      ENDIF


      i = Imaxgtic - 1
      IF ( i.LT.0 ) i = 0
      DO WHILE ( .TRUE. )
         i = i + 1
         READ (lun1,*,ERR=100,END=200) Gtic(1,i) , Gtic(2,i)
      ENDDO

 100  CONTINUE
      errormsg = ' Error reading '//Asctem
      CALL fcerr(errormsg)
      Status = 1
      RETURN

 200  Imaxgtic = i - 1
      CLOSE (lun1)
      CALL FRELUN(lun1)

      RETURN
      END
C*==calcbinsize.spg  processed by SPAG 4.50J  at 14:12 on 25 Oct 1995

      FUNCTION CALCBINSIZE(Binval,Binlc,Gti,Imaxgti1,Status)

C Calculates the bin size of a bin starting at binval, with a max size of
C binlc, using only times between gti(1,i) and gti(2,i) for i=1,imaxgti.
C assumes that the gti are in ascending order of time.
C
C 6 cases, GTI at top, Bin at bottom, G1 is start of GTI, G2 is stop of GTI
C                      B1 is start of bin, B2 is stop of bin
C ==================================================
C 1
C G1-------------G2        true when G1.le.B1 & G2.ge.B1 & G2.le.B2
C      B2-----------B2     exposure is G2 - B1
C======================================================================
C 2
C G1-----G2                true when G2.le.B1
C            B1--------B2  No exposure
C======================================================================
C 3
C         G1----G2         true when B2.le.G1
C B1---B2                  We're done, no exposure
C======================================================================
C 4
C         G1-----------G2  true when G1.ge.B1 & G1.le.B2 & g2.ge.B2
C  B1-----------B2         B2 - G1
C======================================================================
C 5
C      G1-------G2         G1.ge.B1 & G1.le.B2 & G2.ge.B1 & G2.le.B2
C B1----------------B2     G2 - G1
C ======================================================================
C 6
C  G1-----------------G2   G1.le.B1 & G2.ge.B2
C      B1----B2            B2 - B1
C ======================================================================


      INTEGER Imaxgti1,Status
      DOUBLE PRECISION CALCBINSIZE , Binval , Binlc , Gti(3,Imaxgti1)

      DOUBLE PRECISION b1 , b2 , g1 , g2
      INTEGER i
      LOGICAL fail
      character(80) errstr

      fail = .FALSE.
      g1 = 0
      g2 = 0
      b1 = 0
      b2 = 0
      i = 0
      Status = 0

      b1 = Binval
      b2 = Binval + Binlc

      CALCBINSIZE = 0.D0
      
      DO 100 i = 1 , Imaxgti1
         fail = .FALSE.
         g1 = Gti(1,i)
         g2 = Gti(2,i)
C
C case 1
C
         IF ( g1.LE.b1 .AND. g2.GE.b1 .AND. g2.LE.b2 ) THEN
            fail = .TRUE.
            CALCBINSIZE = g2 - b1 + CALCBINSIZE
C
C case 2
C
         ELSEIF ( g2.LE.b1 ) THEN
C do nothing
            IF ( fail ) THEN
               CALL fcerr(' bugcheck:calcbinsize, two cases taken')
               WRITE (errstr,*) ' G1 = ' , g1
               CALL fcerr(errstr)
               WRITE (errstr,*) ' G2 = ' , g2
               CALL fcerr(errstr)
               WRITE (errstr,*) ' B1 = ' , b1
               CALL fcerr(errstr)
               WRITE (errstr,*) ' B2 = ' , b2
               CALL fcerr(errstr)
               Status = 2
               RETURN
            ENDIF
            fail = .TRUE.
C
C case 3
C
         ELSEIF ( b2.LE.g1 ) THEN
            IF ( fail ) THEN
               CALL fcerr(' bugcheck:calcbinsize, two cases taken')
               WRITE (errstr,*) ' G1 = ' , g1
               CALL fcerr(errstr)
               WRITE (errstr,*) ' G2 = ' , g2
               CALL fcerr(errstr)
               WRITE (errstr,*) ' B1 = ' , b1
               CALL fcerr(errstr)
               WRITE (errstr,*) ' B2 = ' , b2
               CALL fcerr(errstr)
               Status = 3
               RETURN
            ENDIF
            fail = .TRUE.
            GOTO 200
C
C case 4
C
         ELSEIF ( g1.GE.b1 .AND. g1.LE.b2 .AND. g2.GE.b2 ) THEN
            CALCBINSIZE = b2 - g1 + CALCBINSIZE
            IF ( fail ) THEN
               CALL fcerr(' bugcheck:calcbinsize, two cases taken')
               WRITE (errstr,*) ' G1 = ' , g1
               CALL fcerr(errstr)
               WRITE (errstr,*) ' G2 = ' , g2
               CALL fcerr(errstr)
               WRITE (errstr,*) ' B1 = ' , b1
               CALL fcerr(errstr)
               WRITE (errstr,*) ' B2 = ' , b2
               CALL fcerr(errstr)
               Status = 4
               RETURN
            ENDIF
            fail = .TRUE.
C
C case 5
C
         ELSEIF ( g1.GE.b1 .AND. g1.LE.b2 .AND. g2.GE.b1 .AND.
     &            g2.LE.b2 ) THEN
            CALCBINSIZE = CALCBINSIZE + g2 - g1
            IF ( fail ) THEN
               CALL fcerr(' bugcheck:calcbinsize, two cases taken')
               WRITE (errstr,*) ' G1 = ' , g1
               CALL fcerr(errstr)
               WRITE (errstr,*) ' G2 = ' , g2
               CALL fcerr(errstr)
               WRITE (errstr,*) ' B1 = ' , b1
               CALL fcerr(errstr)
               WRITE (errstr,*) ' B2 = ' , b2
               CALL fcerr(errstr)
               Status = 5
               RETURN
            ENDIF
            fail = .TRUE.
         ELSEIF ( g1.LE.b1 .AND. g2.GE.b2 ) THEN
            CALCBINSIZE = CALCBINSIZE + b2 - b1
            IF ( fail ) THEN
               CALL fcerr(' bugcheck:calcbinsize, two cases taken')
               WRITE (errstr,*) ' G1 = ' , g1
               CALL fcerr(errstr)
               WRITE (errstr,*) ' G2 = ' , g2
               CALL fcerr(errstr)
               WRITE (errstr,*) ' B1 = ' , b1
               CALL fcerr(errstr)
               WRITE (errstr,*) ' B2 = ' , b2
               CALL fcerr(errstr)
               Status = 5
               RETURN
            ENDIF
            fail = .TRUE.
         ENDIF
 100  CONTINUE

 200  RETURN
      END
C*==checkgti.spg  processed by SPAG 4.50J  at 14:12 on 25 Oct 1995

      SUBROUTINE CHECKGTI(Infile, Gtis, Maxgtis1, Imaxgtis,Maxccd1,
     &                    Imaxccd, Gtiname, Eventname, Qoverlap, Qgti)

      IMPLICIT NONE

      LOGICAL Qoverlap, Qgti

      INTEGER Maxgtis1 ,Maxccd1, Imaxgtis(Maxccd1),Imaxccd
      DOUBLE PRECISION Gtis(3,Maxgtis1,Maxccd1)
      CHARACTER*(*) Gtiname , Infile, Eventname

C Subroutine to read GTIs from the Infile[Gtiname] and check whether they
C overlap with the GTIs passed into the subroutine in the array Gtis. If it
C doesn't find an extension with GTIs on the event file then look in the
C event extension for the TSTART and TSTOP keywords - if that doesn't work
C then take the times of the first and last events.
C Arguments :
C    Infile      c         i: Filename to be read
C    Gti         d         i: Set of GTIs to be compared against
C    Maxgti1     i         i: The size of the Gti array
C    Imaxgti     i         i: The actual number of GTI in Gti
C    Gtiname     c         i: The extension name for Infile with the GTIs
C    Eventname   c         i: The extension name for Infile with the events
C    Qoverlap    l         r: True if there is any GTI overlap
C    Qgti        l         r: True if GTI are read from Infile

      INCLUDE 'expar.inc'

      DOUBLE PRECISION gti1(3,MAXGTI), gti2(3,MAXGTI), gtiw(3,MAXGTI) 
      DOUBLE PRECISION mintime1

      INTEGER imaxgti1 , imaxgti2
      INTEGER i , j, status
      INTEGER LENACT , itmp
      CHARACTER(80) errmsg
      character(30) hdunames(MAXCCD),hduname

      status = 0

      DO 100 i = 1 , MAXGTI
         gti1(1,i) = 0.0
         gti1(2,i) = 0.0
         gti2(1,i) = 0.0
         gti2(2,i) = 0.0
 100  CONTINUE
      mintime1 = 0.0
      j = 0
      itmp = 0
      errmsg = ' '

      Qoverlap = .FALSE.
      Qgti = .FALSE.

      imaxgti1 = 0

      CALL GETGTI(Infile,gti1,MAXGTI,imaxgti1,Gtiname,Eventname,
     &            0,mintime1,MAXCCD,hdunames,hduname,status)
      IF ( Status .NE. 0 ) RETURN
      IF ( imaxgti1 .GT. 0 ) Qgti = .TRUE.

      i = 1
      DO WHILE ( i.LE.imaxgti1 )
         IF ( gti1(1,i).GT.gti1(2,i) ) THEN
            errmsg = ' GTI Stop before Start in '//
     &               Infile(1:LENACT(Infile))
            CALL XWARN1(errmsg,5)
            WRITE (errmsg,'(1x,a,e20.15,a,e20.15)',IOSTAT=itmp) 
     &             'STOP ' , gti1(2,i) , ' is before START ' , 
     &             gti1(1,i)
            CALL XWARN1(errmsg,5)
            CALL XWARN1(' GTI ignored',5)
            DO 220 j = i + 1 , imaxgti1
               gti1(1,j-1) = gti1(1,j)
               gti1(2,j-1) = gti1(2,j)
 220        CONTINUE
            imaxgti1 = imaxgti1 - 1
         ENDIF
         i = i + 1
      ENDDO

      CALL EXTMGTI(gti1,MAXGTI,imaxgti1,status)
     
      DO j=1,imaxccd
         imaxgti2=Imaxgtis(j)
         DO 500 i = 1 , imaxgti2
            gti2(1,i) = Gtis(1,i,j)
            gti2(2,i) = Gtis(2,i,j)
            gti2(3,i) = Gtis(3,i,j)
 500     CONTINUE

         CALL FINGTI(gti2,imaxgti2,gti1,imaxgti1,gtiw,MAXGTI,status)

         IF ( imaxgti2 .GT. 0 ) THEN
            Qoverlap = .TRUE.
            GOTO 600
         ENDIF
      ENDDO
600   CONTINUE

      END
C*==extmgti.spg  processed by SPAG 4.50J  at 14:12 on 25 Oct 1995
c
c

      SUBROUTINE EXTMGTI(Gti,Maxgti,Imaxgti,Status)

      IMPLICIT NONE

C     sort/merge the gti(2,maxgti) where imaxgti is the current max

      INTEGER Maxgti , Imaxgti, Status
      DOUBLE PRECISION Gti(3,Maxgti)


      INTEGER i , j
      LOGICAL found

      i = 0
      j = 0
      found = .FALSE.
      Status = 0

      CALL EXSORT3(Imaxgti,Gti,Maxgti)

      DO 100 i = 1 , Imaxgti - 1
         IF ( Gti(1,i).GT.Gti(1,i+1)) THEN
            CALL fcerr('Sorting failed - exsort3')
            Status = 1
            RETURN
         ENDIF
 100  CONTINUE

C     Merge the overlaps.  `Do while (found)' forces `do 250' to repeat
C     until no more overlaps are found.

      found = .TRUE.
      DO WHILE ( found )
          found = .FALSE.
          DO 250 i = 1 , Imaxgti - 1
              IF ( .NOT.found ) THEN
                  IF ( Gti(2,i).GE.Gti(1,i+1)) THEN
                      found = .TRUE.
                      Gti(2,i) = MAX(Gti(2,i+1),Gti(2,i))
                      DO 205 j = i + 1 , Imaxgti - 1
                          Gti(1,j) = Gti(1,j+1)
                          Gti(2,j) = Gti(2,j+1)
                          Gti(3,j) = Gti(3,j+1)
  205                 CONTINUE
                      Imaxgti = Imaxgti - 1
                  ENDIF
              ENDIF
  250     CONTINUE
      ENDDO

C     Clean up the "bad" gti's where START > STOP
      i = 1
      DO WHILE ( i.LE.Imaxgti )
         IF ( Gti(1,i) .GT. Gti(2,i) ) THEN
            DO j = i+1, Imaxgti
               Gti(1,j-1) = Gti(1,j)
               Gti(2,j-1) = Gti(2,j)
               Gti(3,j-1) = Gti(3,j)
            enddo
            Imaxgti = Imaxgti - 1
         else
            i = i + 1
         ENDIF
      ENDDO

      RETURN
      END
C*==fingti.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995

      SUBROUTINE FINGTI(Gti1,Ngti1,Gti2,Ngti2,Gtiw,Maxgti,Status)

C     Build final merged gti.  Takes two gti lists (both sorted, merged, and
C     filtered) and ANDs them, ie. a given time will be within a GTI only if
C     it was inside GTIs from *both* input lists. The result is returned in
C     Gti1. Always use Gti1 to set the third element in the first index.
C
C     Arguments
C       Gti1           i/r:Array 1 of GTIs
C       Ngti1          i/r:Actual length of array 1
C       Gti2             i:Array 2 of GTIs
c       Ngti2            i:Actual length of array 2
c       Gtiw             w:Work array for GTIs
c       Maxgti           i:Allocated sizes of arrays
c       Status           r:Error status

      IMPLICIT NONE

      INTEGER Ngti1, Ngti2, Maxgti, Status
      DOUBLE PRECISION Gti1(3,Ngti1), Gti2(3,Ngti2)
      DOUBLE PRECISION Gtiw(3,2*Maxgti)

      INTEGER i, i1, i2, j, iout

      LOGICAL done, active

      Status = 0

c Check that input arrays really are sorted

      DO i = 2, Ngti1
         IF ( Gti1(1,i) .LT. Gti1(1,i-1) ) THEN
            CALL fcecho(
     & 'Unexpected error in FINGTI: input array 1 is out of order')
            Status = 1
            RETURN
         ENDIF
      ENDDO

      DO i = 2, Ngti2
         IF ( Gti2(1,i) .LT. Gti2(1,i-1) ) THEN
            CALL fcecho(
     & 'Unexpected error in FINGTI: input array 2 is out of order')
            Status = 1
            RETURN
         ENDIF
      ENDDO

c Special case of Ngti2 = 0

      IF ( Ngti2 .EQ. 0 ) RETURN

c Special case of Ngti1 = 0

      IF ( Ngti1 .EQ. 0 ) THEN
         
         Ngti1 = Ngti2
         DO i = 1, Ngti1
            DO j = 1, 3
               Gti1(j,i) = Gti2(j,i)
            ENDDO
         ENDDO

      ENDIF

c loop around accumulating the output GTIs in the Gtiw array

      iout = 0
      i1 = 1
      i2 = 1
      done = .FALSE.
      active = .FALSE.

      DO WHILE ( .NOT.done )

c     if active find the next end time then set to inactive

         IF ( active ) THEN

            Gtiw(2,iout) = MIN(Gti1(2,i1),Gti2(2,i2))
            active = .FALSE.
            IF ( Gti1(2,i1) .GT. Gti2(2,i2) ) THEN 
               i2 = i2 + 1
            ELSE
               i1 = i1 + 1
            ENDIF

c eliminate this GTI if it is zero length

            IF ( Gtiw(2,iout) .LE. Gtiw(1,iout) ) iout = iout - 1

            IF ( i1 .GT. Ngti1 .OR. i2 .GT. Ngti2 ) done = .TRUE.

         ELSEIF ( .NOT.active ) THEN

c if inactive then find the next time which is valid and set to active

            DO WHILE ( Gti1(2,i1) .LT. Gti2(1,i2) .AND. .NOT.done )
               i1 = i1 + 1
               IF ( i1 .GT. Ngti1 ) done = .TRUE.
            ENDDO

            DO WHILE ( Gti2(2,i2) .LT. Gti1(1,i1) .AND. .NOT.done )
               i2 = i2 + 1
               IF ( i2 .GT. Ngti2 ) done = .TRUE.
            ENDDO

            IF ( .NOT.done ) THEN
               iout = iout + 1
               Gtiw(1,iout) = MAX(Gti1(1,i1),Gti2(1,i2))
               active = .TRUE.
               Gtiw(3,iout) = Gti1(3,i1)
            ENDIF

         ENDIF

      ENDDO

c Copy Gtiw into Gti1

      Ngti1 = iout
      DO i = 1, Ngti1
         DO j = 1, 3
            Gti1(j,i) = Gtiw(j,i)
         ENDDO
      ENDDO

      RETURN
      END
      
C*==fingti.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995
c
c      SUBROUTINE FINGTI(Gti,Imaxgti,Gtic,Imaxgtic,Maxgti1,Status)
c
cC     Build final merged gti.  Takes two gti lists (both sorted, merged, and
cC     filtered) and ANDs them, ie. a given time will be within a GTI only if
cC     it was inside GTIs from *both* input lists.
c
c      implicit none
c
c      INCLUDE 'expar.inc'
c
c      INTEGER Imaxgti , Imaxgtic , Maxgti1, Status
c      DOUBLE PRECISION Gti(3,Maxgti1) , Gtic(3,Maxgti1)
c
c      INTEGER i, j, ilist, iout
c
c      LOGICAL active , good
c
cc Stuff for dynamically allocated gtilist1 and gtilist2 arrays
c
c      INCLUDE 'memblock.inc'
c
c      INTEGER igtilist1, igtilist2
c
c      DATA igtilist1, igtilist2 / 2*0 /
c
cc Get the memory for gtilist1 and gtilist2
c
c      CALL udmget(2*Imaxgti+2*Imaxgtic, 7, igtilist1, Status)
c      IF ( Status .NE. 0 ) THEN
c         CALL fcecho('Failed to get memory for gtilist1 in FINGTI')
c         RETURN
c      ENDIF
c      CALL udmget(2*Imaxgti+2*Imaxgtic, 4, igtilist2, Status)
c      IF ( Status .NE. 0 ) THEN
c         CALL fcecho('Failed to get memory for gtilist2 in FINGTI')
c         RETURN
c      ENDIF
c
c      DO 100 i = 0 , 2*Imaxgti+2*Imaxgtic-1
c         MEMD(igtilist1+i) = 0.0
c         MEMI(igtilist2+i) = 0
c 100  CONTINUE
c      i = 0
c      ilist = 0
c      iout = 0
c      active = .FALSE.
c      good = .FALSE.
c      Status = 0
c
c      IF ( Imaxgtic.LE.0 .OR. Imaxgti.LE.0 ) RETURN
c
cC     build a merged list
cC     gtilist1(i) holds the time
cC     gtilist2(i) holds the type.  0 for gti start, 1 for gti stop,
cC     2 for time selection start, and 3 for time selection stop
cC     Since the times are all sorted and merged then
cC     the end of the previous gti or time selection is guaranteed to be
cC     less than the beginning of the next gti or time selection.
cC     Filter out zero-length GTIs because the heapsort used in exsort2 is 
cc     not stable w.r.t. equal keys.
c
c      ilist = 0
c      DO 200 i = 1 , Imaxgti
c         IF ( Gti(2,i) .GT. Gti(1,i) ) THEN
c            ilist = ilist + 1
c            MEMD(igtilist1+ilist-1) = Gti(1,i)
c            MEMI(igtilist2+ilist-1) = 0
c            ilist = ilist + 1
c            MEMD(igtilist1+ilist-1) = Gti(2,i)
c            MEMI(igtilist2+ilist-1) = 1
c         ENDIF
c 200  CONTINUE
c
c      DO 300 i = 1 , Imaxgtic
c         IF ( Gtic(2,i) .GT. Gtic(1,i) ) THEN
c            ilist = ilist + 1
c            MEMD(igtilist1+ilist-1) = Gtic(1,i)
c            MEMI(igtilist2+ilist-1) = 2
c            ilist = ilist + 1
c            MEMD(igtilist1+ilist-1) = Gtic(2,i)
c            MEMI(igtilist2+ilist-1) = 3
c         ENDIF
c 300  CONTINUE
c
c
c      CALL EXSORT2(ilist,MEMD(igtilist1),MEMI(igtilist2),
c     &             2*Imaxgti+2*Imaxgtic)
c
c      DO 400 i = 1 , ilist - 1
c         IF ( MEMD(igtilist1+i-1).GT.MEMD(igtilist1+i) ) THEN
c            CALL fcerr('Sorting failed - exsort2')
c            Status = 1
c            RETURN
c         ENDIF
c 400  CONTINUE
c
c
cC     Now produce a merged gti in gti.
cC     This section is a finite-state machine that takes the combined, sorted
cC     time list (gtilist1) and the type codes and creates an ANDed GTI.
cC     If the two original lists are A and B, A means list A is on, !A means
cC     it's off, etc, and the numbers are the codes given above, then this
cC     FSM looks like:
cC
cC        (AB)  3->  <-2 (A!B)
cC
cC       1| ^            1| ^
cC        v |0            v |0
cC
cC       (!AB)  3->  <-2 (!A!B)
C
cC     A new GTI is begun on the transitions to state AB, and a currently
cC     open one is ended on the transtitions away from it.
c
c      active = .FALSE.
c      good = .FALSE.
c      iout = 0
c      DO 500 i = 1 , ilist
c         IF ( MEMI(igtilist2+i-1).EQ.0 ) THEN
cC
cC Its a gti start
cC
c            IF ( .NOT.(good) ) THEN
c               good = .TRUE.
c               IF ( active ) THEN
cC
cC we previously started a ts interval
cC
c                  iout = iout + 1
c                  Gti(1,iout) = MEMD(igtilist1+i-1)
c               ENDIF
c            ENDIF
c         ELSEIF ( MEMI(igtilist2+i-1).EQ.1 ) THEN
cC
cC gti stop
cC
c            IF ( good ) THEN
c               good = .FALSE.
cC
cC close out the interval
cC
c               IF ( active ) Gti(2,iout) = MEMD(igtilist1+i-1)
c            ENDIF
c         ELSEIF ( MEMI(igtilist2+i-1).EQ.2 ) THEN
cC
cC start of ts interval
cC
c            IF ( .NOT.(active) ) THEN
c               active = .TRUE.
c               IF ( good ) THEN
cC
cC start a ts interval
cC
c                  iout = iout + 1
c                  Gti(1,iout) = MEMD(igtilist1+i-1)
c               ENDIF
c            ENDIF
c         ELSEIF ( MEMI(igtilist2+i-1).EQ.3 ) THEN
cC
cC End of ts interval
cC
c            IF ( active ) THEN
c               active = .FALSE.
cC
cC close out the interval
cC
c               IF ( good ) Gti(2,iout) = MEMD(igtilist1+i-1)
c            ENDIF
c         ENDIF
c 500  CONTINUE
c
c      Imaxgti = iout
cC
cC     The FSM can leave zero-length GTIs, so clean them up.
cC
c      i = 1
c      DO WHILE ( i .LE. Imaxgti )
c          IF ( Gti(1,i) .GE. Gti(2,i) ) THEN
c              DO j = i+1, Imaxgti
c                  Gti(1,j-1) = Gti(1,j)
c                  Gti(2,j-1) = Gti(2,j)
c              enddo
c              Imaxgti = Imaxgti - 1
c          else
c              i = i + 1
c          ENDIF
c      ENDDO
c
cc If there are no net GTIs then create one with start = stop = 0 to help
cc tracking when there are multiple CCDs.
c
c      IF ( Imaxgti .EQ. 0 ) THEN
c         Imaxgti = 1
c         Gti(1,1) = 0.0
c         Gti(2,1) = 0.0
c      ENDIF
c
cc Release the memory for igtilist1/2
c
c      CALL udmfre(igtilist1, 7, Status)
c      IF ( Status .NE. 0 ) 
c     &   CALL fcecho('Failed to release memory for gtilist1 in FINGTI')
c      CALL udmfre(igtilist2, 4, Status)
c      IF ( Status .NE. 0 ) 
c     &   CALL fcecho('Failed to release memory for gtilist2 in FINGTI')
c
c      RETURN
c      END
C*==getgti.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995

      SUBROUTINE GETGTI(Infile, Gti, Maxgti1, Imaxgti, Gtiname, 
     &                  Eventname, Chatter, Mintime1, Maxccd1,
     &                  Hdunames, Hduname, Status)

      IMPLICIT NONE

      INTEGER Maxgti1 , Status , Imaxgti, Chatter,Maxccd1

      DOUBLE PRECISION Gti(3,Maxgti1) , Mintime1

      CHARACTER*(*) Gtiname , Infile, Eventname, Hdunames(Maxccd1)
      CHARACTER*(*) Hduname

C Subroutine to read GTIs from the Infile[Gtiname]. If it doesn't find an 
C extension with GTIs on the event file then look in the event extension 
C for the TSTART and TSTOP keywords - if that doesn't work then take the 
C times of the first and last events.
C Arguments :
C    Infile      c         i: Filename to be read
C    Gti         d       i/r: Set of GTIs read
C    Maxgti1     i         i: The size of the Gti array
C    Imaxgti     i       i/r: The actual number of GTI in Gti
C    Gtiname     c         i: The extension name for Infile with the GTIs
C    Eventname   c         i: The extension name for Infile with the events
C    Chatter     i         i: Chatter level at which to write diagnostics
C    Mintime1    d         r: The first good time
C    Hduname     c         r: The HDUNAME read from the file
C    Status      i         r: 0==OK

      INCLUDE 'expar.inc'

      DOUBLE PRECISION timezero

      INTEGER i, j, unit, hdutype, v, starti , stopi
      INTEGER nrows , tfields , blocksize,indexgti(MAXCCD),imaxnum
      INTEGER ccdid(MAXCCD), ngtiref, ccdnum(MAXCCD)

      character(20) gtiref(MAXCCD)
      character(20) ttype(MAXCOLS), tunit(MAXCOLS)
      character(10) tform(MAXCOLS)
      CHARACTER(255) errormsg
      CHARACTER(20)  extname

      LOGICAL anyf, qdone

      INTEGER lenact
      LOGICAL SUBSET
      EXTERNAL lenact, SUBSET

      unit = 0
      hdutype = 0
      extname = ' '
      nrows = 0
      tfields = 0
      blocksize = 0
      DO 100 i = 1 , MAXCOLS
         ttype(i) = ' '
         tunit(i) = ' '
         tform(i) = ' '
 100  CONTINUE
      errormsg = ' '
      v = 0
      anyf = .FALSE.
      starti = 0
      stopi = 0
      Hdunames(1) = 'GTI'
      Hduname='GTI'

      CALL GETLUN(unit)
      CALL FTOPEN(unit,Infile,0,blocksize,Status)
      errormsg = ' Failed to open '//Infile(1:LENACT(Infile))
      IF ( Status.NE.0 ) GOTO 999

c Move to the events extension to get the data subspace keywords

      hdutype = 2
      CALL FTMNHD(unit, hdutype, Eventname, 0, Status)
      errormsg = ' Failed to find events extension'
      IF ( Status.NE.0 ) GOTO 999

      imaxnum = 0

c First use the data subspace keywords to look for GTIs

      CALL getsubkeys(unit, Status)

      IF ( Status .EQ. 0 ) THEN

         CALL gtblrfs('TIME', gtiref, ccdnum, ngtiref, Status)

         IF ( Status .EQ. 0 .AND. ngtiref .GT. 0 ) THEN

c Find the GTI tables

            DO i = 1, ngtiref

               qdone = .FALSE.
               j = 0
               DO WHILE (.NOT.qdone)
                  j = j + 1
                  CALL FTMAHD(unit,j,hdutype,Status)
                  IF ( Status .EQ. 0 .AND. hdutype .EQ. 2 ) THEN
                     CALL FTGHBN(unit,MAXCOLS,nrows,tfields,ttype,
     &                           tform,tunit,extname,v,Status)
                     CALL FTGKYS(unit,'hduname',Hduname,errormsg,Status)
                     IF ( Status .NE. 0 ) THEN
                        Status = 0
                        Hduname = ' '
                     ENDIF
                     IF ( extname .EQ. gtiref(i) .OR.
     &                    Hduname .EQ. gtiref(i) ) THEN
                        imaxnum = imaxnum+1
                        indexgti(imaxnum) = j
                        IF ( Hduname .EQ. ' ' ) THEN
                           Hdunames(imaxnum) = extname
                        ELSE
                           Hdunames(imaxnum) = Hduname
                        ENDIF
                        ccdid(imaxnum) = ccdnum(i)
                        qdone = .TRUE.
                     ENDIF
                  ENDIF
                  IF ( Status .NE. 0 ) qdone = .TRUE.
               ENDDO
               Status = 0

            ENDDO

         ENDIF
      ENDIF

      Status = 0

C If we didn't find any GTIs then look for one called Gtiname, STDGTI,
C or GTI

      IF ( imaxnum .EQ. 0 ) THEN

         hdutype = 2
         extname = Gtiname
         CALL FTMNHD(unit, hdutype, extname, 0, Status)
         IF ( Status .NE. 0 ) THEN
            Status = 0
            extname = 'STDGTI'
            CALL FTMNHD(unit, hdutype, extname, 0, Status)
         ENDIF
         IF ( Status .NE. 0 ) THEN
            Status = 0
            extname = 'GTI'
            CALL FTMNHD(unit, hdutype, extname, 0, Status)
         ENDIF

         IF ( Status .EQ. 0 ) THEN
            imaxnum = imaxnum+1
            CALL FTGHDN(unit, indexgti(imaxnum))
            Hdunames(imaxnum) = extname
            ccdid(imaxnum) = 1
         ENDIF

      ENDIF

c if imaxnum is not zero then we found the GTI table so process it

      Status =0

      DO j = 1, imaxnum

         CALL FTMAHD(unit,indexgti(j),hdutype,Status)
         CALL FTGHBN(unit,MAXCOLS,nrows,tfields,ttype,
     &               tform,tunit,extname,v,Status)
         CALL UPC(extname)

         IF ( (nrows+imaxgti).GT.Maxgti1 ) THEN
            errormsg = ' maxgti is too small'
            Status = -1
            GOTO 999
         ENDIF

         timezero = 0.D0
         CALL FTGKYD(unit,'timezero',timezero,errormsg,Status)
         IF ( Status.NE.0 ) THEN
            Status = 0
            timezero = 0.D0
         ENDIF

         CALL FTGCNO(unit,.FALSE.,'start',starti,Status)
         CALL FTGCNO(unit,.FALSE.,'stop',stopi,Status)

         DO i = 1 + Imaxgti , nrows + Imaxgti
            CALL FTGCVD(unit,starti,i-Imaxgti,1,1,0.D0,Gti(1,i),anyf,
     &                  Status)
            CALL FTGCVD(unit,stopi,i-Imaxgti,1,1,0.D0,Gti(2,i),anyf,
     &                  Status)
            Gti(1,i) = Gti(1,i) + timezero
            Gti(2,i) = Gti(2,i) + timezero
            Gti(3,i) = ccdid(j)
         ENDDO
         imaxgti = nrows + imaxgti

c If there were no intervals in this GTI then add a zero length interval
c so we can keep track of the fact that this CCD is in use but has no
c good time

         IF ( nrows .EQ. 0 ) THEN
            imaxgti = imaxgti + 1
            Gti(1,imaxgti) = 0.0d0
            Gti(2,imaxgti) = 0.0d0
            Gti(3,imaxgti) = ccdid(j)
         ENDIF

      ENDDO

c If couldn't find a GTI extension then try to find an events extension

      IF ( imaxnum.EQ.0 ) THEN

         errormsg = ' Cannot find a gti extension named '//
     &    Gtiname(:lenact(Gtiname))//' in '//Infile(:lenact(Infile))
         CALL XWARN1(errormsg,Chatter)
         Status = 0

         hdutype = 2
         CALL FTMNHD(unit, hdutype, Eventname, 0, Status)

c If we can't find the events extension either then give up

         IF ( status .NE. 0 ) THEN
            errormsg = ' or an events extension named '//
     &                 eventname(:lenact(eventname))
            CALL XWARN1(errormsg,Chatter)
            errormsg = 'giving up...'
            CALL XWARN1(errormsg,Chatter)
            Status = 0
            GOTO 999
         ENDIF

c Look for TSTART and TSTOP keywords

         CALL FTGKYD(unit,'TSTART',Gti(1,Imaxgti+1),errormsg,Status)
         CALL FTGKYD(unit,'TSTOP',Gti(2,Imaxgti+1),errormsg,Status)
         Gti(3,Imaxgti+1)=1

         IF ( Status .EQ. 0 ) THEN

            errormsg = ' using TSTART and TSTOP from events extension'
            CALL XWARN1(errormsg,Chatter)
            nrows = 1
            imaxgti=imaxgti+nrows

         ELSEIF ( Status .NE. 0 ) THEN

            errormsg = 
     &       'cannot find TSTART and TSTOP either, giving up...'
            GOTO 999

         ENDIF

      ENDIF

c Set the new number of GTIs and start time

      DO i = 1 , Imaxgti
         Mintime1 = MIN(Mintime1,Gti(1,i))
      ENDDO

 999  CONTINUE

      IF ( Status .NE. 104 ) CALL FTCLOS(unit,Status)
      CALL FRELUN(unit)

      IF ( Status .NE. 0 ) THEN
         CALL fcerr(errormsg)
         CALL fcerrm(status)
      ENDIF

      RETURN
      END
C*==printgti.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995


      SUBROUTINE PRINTGTI(Gti,Maxgti,Imaxgti,Str)

      IMPLICIT NONE

      CHARACTER*(*) Str
      INTEGER Imaxgti , Maxgti
      DOUBLE PRECISION Gti(3,Maxgti)

C this is a place holder routine for debugging
      DOUBLE PRECISION djunk
      CHARACTER(255) sjunk
      djunk = Gti(1,Imaxgti)
      sjunk = Str

      RETURN
      END
c *********************************************************************

      FUNCTION SELECT_PHASE_SETUP(Mjdref, Emjdrff, Emjdrfi, Status)

      DOUBLE PRECISION Mjdref, Emjdrff
      INTEGER Emjdrfi, Status
      LOGICAL select_phase_setup

c Sets up the phase selection - returns true if there are phases

      INCLUDE 'exwin.inc'

      CHARACTER(255) file
      character(80) errormsg

      INTEGER ierr

      LOGICAL qfound

      INTEGER tmftyp, lenact
      CHARACTER(255) exnxtm
      EXTERNAL tmftyp, lenact, exnxtm

      Status = 0
      select_phase_setup = .FALSE.

c find the file to use ( we assume only one xronos window file has
c been specified ). If there is not xronos window file then set the
c number of phase bins to zero and return

      File = exnxtm(.true., ierr)
      qfound = .FALSE.
      DO WHILE ( ierr .EQ. 0 )
         IF ( tmftyp(File, status) .EQ. 2 ) THEN
            qfound = .TRUE.
            ierr = 1
         ELSE
            File = exnxtm(.false., ierr)
         ENDIF
      ENDDO

      IF ( .NOT.qfound ) THEN
         nwi(2) = 0
         RETURN
      ENDIF

c Read the file

      CALL XRRDWIN(1, File, twia, twio, pwi, pwia, pwio, fwia,
     &             fwio, ewia, ewio, nwi, nwito, status)
      IF ( status .NE. 0 ) THEN
         errormsg = ' Can not read window file '
     &        //File(1:MIN((LEN(errormsg)-26),lenact(File)))
         CALL fcerr(errormsg)
         CALL fcerrm(status)
         RETURN
      ENDIF

C pwi(1) is the epoch for the phase window in days
C pwi(2) is the period for the phase window in secs (I don't think so, but
C it's not sensible otherwise!) (well, maybe not.  Let's try days)

C Convert the times

      pwi(1) = pwi(1)*86400.D0
      pwi(1) = pwi(1) + Mjdref*86400.D0 - (Emjdrfi+Emjdrff)*86400.D0
      pwi(2) = pwi(2)*86400.D0

      select_phase_setup = ( nwi(2) .GT. 0 )

      END

c ***********************************************************************

      FUNCTION SELECT_PHASE(Time)

      IMPLICIT NONE

      LOGICAL Select_phase

      DOUBLE PRECISION Time

C All times are in seconds, so we must convert the times in days in the
C window files to seconds.  All of them are relative to the launch time.

      INCLUDE 'exwin.inc'

      LOGICAL found

      DOUBLE PRECISION ttime
      REAL ttime4
      INTEGER i

      INTEGER tmftyp
      EXTERNAL tmftyp

      found = .FALSE.

      Select_phase = .TRUE.

C no phase window

      IF ( nwi(2).EQ.0 ) RETURN

C Subtract off the epoch

      ttime = Time - pwi(1)

C Find out the number of periods this time indicates

      ttime = DMOD(ttime,pwi(2))
      ttime = ttime/pwi(2)
      IF ( ttime.LT.0.D0 ) ttime = ttime + 1.D0

      ttime4 = SNGL(ttime)

      DO 200 i = 1 , nwi(2)
         found = (ttime4.GE.pwia(i)) .AND. (ttime4.LE.pwio(i))
         IF ( found ) THEN
            Select_phase = found
            RETURN
         ENDIF
 200  CONTINUE

      Select_phase = found

      RETURN
      END

c ***********************************************************************

      SUBROUTINE CALC_EXP(Gti,Imaxgti,Maxgti,Secs)

      INTEGER Maxgti
      DOUBLE PRECISION Gti(3,Maxgti)
      INTEGER Imaxgti
      DOUBLE PRECISION Secs

      INCLUDE 'exwin.inc'

      INTEGER i, j
      DOUBLE PRECISION exp , dpstart , dpstop

      DOUBLE PRECISION EXPCORR
      EXTERNAL EXPCORR

      exp = 0.D0
      IF ( nwi(2).EQ.0 ) THEN

C no phase windows
         DO 250 i = 1 , Imaxgti
            exp = exp + Gti(2,i) - Gti(1,i)
 250     CONTINUE
      ELSE
         DO 300 i = 1 , Imaxgti
            DO 260 j = 1 , nwi(2)
               dpstart = pwia(j)
               dpstop = pwio(j)
               exp = exp + EXPCORR(Gti(1,i),Gti(2,i),pwi(1),pwi(2),
     &               dpstart,dpstop)
 260        CONTINUE
 300     CONTINUE
      ENDIF
      Secs = exp
      RETURN
      END
C*==select_time.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995
      FUNCTION SELECT_TIME(Time,Timedel,Gti,Maxgti,Imaxgti,Nbin,
     &                     Adjustgti)

      IMPLICIT NONE

      LOGICAL Select_time

      LOGICAL Adjustgti, foundit, first
      INTEGER Maxgti , Imaxgti , i , lasttry , step , Nbin
      DOUBLE PRECISION Time , Timedel , Gti(3,Maxgti)

      SAVE first
      DATA first / .TRUE. /

c On input Nbin is the GTI in which the last event lay. If the first
c time through then initialize Nbin to -1

      IF ( first ) THEN
         Nbin = -1
         first = .FALSE.
      ENDIF

      Select_time = .TRUE.

c Do a bunch of simple checks

c If there are no GTIs then must be false

      IF ( Imaxgti.EQ.0 ) THEN
         Select_time = .FALSE.
         RETURN
      ENDIF

c Check if event is before first GTI or after last
   
      IF ( (Time.LT.Gti(1,1)) .OR. (Time.GT.Gti(2,Imaxgti)) ) THEN
         Select_time = .FALSE.
         RETURN
      ENDIF

c If there is only one GTI. If this is the first event then adjust
c GTI for frame time if required.

      IF ( Imaxgti.EQ.1 ) THEN
         IF ( (Time.LT.Gti(1,1)) .OR. (Time.GT.Gti(2,1)) ) THEN
            Select_time = .FALSE.
         ELSE
            IF ( Nbin .LE. 0 .AND. Timedel .GT. 0.0 .AND. Adjustgti )
     &           CALL ADJUST_GTI(Time, Timedel, Gti(1,1))
            Nbin = 1
         ENDIF
         RETURN
      ENDIF

c First check whether the current event lies in the same GTI interval as
c the last one. Since events are usually pretty much in time order this
c may well be the case.

      IF ( Nbin .GT. 0 .AND. Nbin .LE. Imaxgti ) THEN
         IF ( (Time.GE.Gti(1,Nbin)) .AND. 
     &        (Time.LE.Gti(2,Nbin)) ) RETURN
      ENDIF

C New KOOL RULING DOODS O(log(N)) search.... Assuming I didn't screw it.

      foundit = .FALSE.
      lasttry = Imaxgti/2
      step = lasttry

      DO WHILE ( .NOT.foundit )
         i = lasttry

C are we at the last bin. This will be the first event in this GTI so
C adjust for frame time if required.

         IF ( i.EQ.Imaxgti ) THEN

            IF ( (Time.GE.Gti(1,i)) .AND. (Time.LE.Gti(2,i)) ) THEN
               IF ( Timedel .GT. 0.0 .AND. Adjustgti ) 
     &              CALL ADJUST_GTI(Time, Timedel, Gti(1,i))
               Nbin = i
               RETURN
            ENDIF

C are we at the right bin. This will be the first event in this GTI so
C adjust for frame time if required.

         ELSEIF ( (Time.GE.Gti(1,i)) .AND. (Time.LT.Gti(1,i+1)) ) THEN

            IF ( Time .GT. Gti(2,i) ) THEN
               Select_time = .FALSE.
            ELSE
               IF ( Timedel .GT. 0.0 .AND. Adjustgti ) 
     &              CALL ADJUST_GTI(Time, Timedel, Gti(1,i))
            ENDIF
            Nbin = i
            RETURN

         ENDIF

C Else time to move

         step = MAX(INT(step/2),1)
         IF ( Time.GT.Gti(1,i) ) THEN
            lasttry = lasttry + step
         ELSE
            lasttry = lasttry - step
         ENDIF

      ENDDO

      RETURN
      END

C *************************************************************************
      SUBROUTINE ADJUST_GTI(Time, Timedel, Gti)

      DOUBLE PRECISION Time, Timedel, Gti(3)

      INTEGER nframes
      DOUBLE PRECISION tfstart, tfend

      IF ( Timedel .LE. 0.0 ) RETURN

      tfstart = Time - Timedel/2
      tfend   = Time + Timedel/2

      nframes = NINT((tfstart - Gti(1))/timedel)
      IF ( nframes .GT. 0 ) THEN
         Gti(1) = tfstart - nframes*timedel
      ELSE
         Gti(1) = tfstart
      ENDIF

      nframes = NINT((Gti(2)-tfend)/timedel)
      IF ( nframes .GT. 0 ) THEN
         Gti(2) = tfend + nframes*timedel
      ELSE
         Gti(2) = tfend
      ENDIF

      RETURN
      END

**==exsort2.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995
 
      SUBROUTINE EXSORT2(N,Ra,Rb,Mn)

      IMPLICIT NONE

* heapsort of ra, switching rb at the same time
      INTEGER i , l , ir , j , Mn , N
      DOUBLE PRECISION Ra(Mn)
      INTEGER Rb(Mn)
      DOUBLE PRECISION rra
      INTEGER rrb
 
      i = 0
      l = 0
      ir = 0
      j = 0
      rra = 0.0
      rrb = 0
 
* Is there something to do?
      IF ( N.LT.2 ) RETURN
 
      l = N/2 + 1
      ir = N
      DO WHILE ( .TRUE. )
         IF ( l.GT.1 ) THEN
            l = l - 1
            rra = Ra(l)
            rrb = Rb(l)
         ELSE
            rra = Ra(ir)
            rrb = Rb(ir)
            Ra(ir) = Ra(1)
            Rb(ir) = Rb(1)
            ir = ir - 1
            IF ( ir.EQ.1 ) THEN
               Ra(1) = rra
               Rb(1) = rrb
               RETURN
            ENDIF
         ENDIF
         i = l
         j = l + l
         DO WHILE ( .TRUE. )
            IF ( j.LE.ir ) THEN
               IF ( j.LT.ir ) THEN
                  IF ( Ra(j).LT.Ra(j+1) ) j = j + 1
               ENDIF
               IF ( rra.LT.Ra(j) ) THEN
                  Ra(i) = Ra(j)
                  Rb(i) = Rb(j)
                  i = j
                  j = j + j
               ELSE
                  j = ir + 1
               ENDIF
               GOTO 50
            ENDIF
            Ra(i) = rra
            Rb(i) = rrb
            GOTO 100
 50         CONTINUE
         ENDDO
         GOTO 99999
 100     CONTINUE
      ENDDO
99999 CONTINUE
      END
**==exsort3.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995
 
      SUBROUTINE EXSORT3(N,Ra,Mn)

      IMPLICIT NONE

* heapsort of ra(1,i), switching ra(2,i) at the same tim<e
      INTEGER Mn , N
      DOUBLE PRECISION Ra(3,Mn)
      DOUBLE PRECISION rra , rrb, rrc
      INTEGER i , l , ir , j
 
      rra = 0.0
      rrb = 0.0
      rrc = 0.0
      i = 0
      l = 0
      ir = 0
      j = 0
 
* Is there something to do?
      IF ( N.LT.2 ) RETURN
      l = N/2 + 1
      ir = N
      DO WHILE ( .TRUE. )
         IF ( l.GT.1 ) THEN
            l = l - 1
            rra = Ra(1,l)
            rrb = Ra(2,l)
            rrc = Ra(3,1)
         ELSE
            rra = Ra(1,ir)
            rrb = Ra(2,ir)
            rrc = Ra(3,ir)
            Ra(1,ir) = Ra(1,1)
            Ra(2,ir) = Ra(2,1)
            Ra(3,ir) = Ra(3,1)
            ir = ir - 1
            IF ( ir.EQ.1 ) THEN
               Ra(1,1) = rra
               Ra(2,1) = rrb
               Ra(3,1) = rrc
               RETURN
            ENDIF
         ENDIF
         i = l
         j = l + l
         DO WHILE ( .TRUE. )
            IF ( j.LE.ir ) THEN
               IF ( j.LT.ir ) THEN
                  IF ( Ra(1,j).LT.Ra(1,j+1) ) j = j + 1
               ENDIF
               IF ( rra.LT.Ra(1,j) ) THEN
                  Ra(1,i) = Ra(1,j)
                  Ra(2,i) = Ra(2,j)
                  Ra(3,i) = Ra(3,j)
                  i = j
                  j = j + j
               ELSE
                  j = ir + 1
               ENDIF
               GOTO 50
            ENDIF
            Ra(1,i) = rra
            Ra(2,i) = rrb
            Ra(3,i) = rrc
            GOTO 100
 50         CONTINUE
         ENDDO
         GOTO 99999
 100     CONTINUE
      ENDDO
99999 CONTINUE
      END

**==expcorr.spg  processed by SPAG 4.50J  at 14:13 on 25 Oct 1995
 
 
c------------------------------------------------------------
      FUNCTION EXPCORR(Gtistart,Gtistop,Epoch,Period,Pstart,Pstop)
c ------------------------------------------------------------
c This subroutine takes an input gti start and stop time,
c and a set of phase windows specified by an epoch, a period and
c a phase start and stop (the latter two between 0 <-> 1), and returns
c the corrected exposure time.

      IMPLICIT NONE

      DOUBLE PRECISION EXPCORR

      DOUBLE PRECISION Gtistart , Gtistop , Epoch , Period
      DOUBLE PRECISION Pstart , Pstop , dleft , dright , ditemp
 
      DOUBLE PRECISION tstart , tstop , tdelta , pdelta , pleft , pright
      DOUBLE PRECISION remleft , remrig
      INTEGER itemp
 
      dleft = 0.0
      dright = 0.0
      ditemp = 0.0
      tstart = 0.0
      tstop = 0.0
      tdelta = 0.0
      pdelta = 0.0
      pleft = 0.0
      pright = 0.0
      remleft = 0.0
      remrig = 0.0
      itemp = 0
 
*
      tstart = Gtistart - Epoch
      tstop = Gtistop - Epoch
      pdelta = Period*(Pstop-Pstart)
      tdelta = tstop - tstart
 
      EXPCORR = 0
 
 
c There are two cases, the GTI interval is less than or
c equal to one period:
      IF ( tdelta.LE.Period ) THEN
c  left is the period number of the period just before the GTI
c  pleft is the time value for the start of the phase window
c  pright is the time value of the end of the phase window
         dleft = DINT(tstart/Period)
         IF ( dleft.GE.0.D0 ) THEN
            pleft = Period*(dleft+Pstart)
            pright = Period*(dleft+Pstop)
         ELSE
            pleft = Period*(dleft+Pstart-1)
            pright = Period*(dleft+Pstop-1)
         ENDIF
c Now just a bunch of if tests to get the corrected value.  Remember that
c the GTI may sit in two phase windows (but no more)
         DO 50 itemp = 1 , 2
            IF ( pleft.LE.tstart ) THEN
               IF ( pright.LE.tstart ) THEN
               ELSEIF ( pright.LE.tstop ) THEN
                  EXPCORR = EXPCORR + pright - tstart
               ELSE
                  EXPCORR = EXPCORR + tstop - tstart
               ENDIF
            ELSEIF ( pleft.LT.tstop ) THEN
               IF ( pright.LE.tstop ) THEN
                  EXPCORR = EXPCORR + pright - pleft
               ELSE
                  EXPCORR = EXPCORR + tstop - pleft
               ENDIF
            ENDIF
            pleft = pleft + Period
            pright = pright + Period
 50      CONTINUE
c To do the second case, we find the first and last phase intervals
c in the GTI, then calculate how much of each is included, and how many periods the GTI contains between these two.
      ELSE
         ditemp = DINT(tstart/Period)
 
         IF ( (Period*(ditemp+Pstop)).GT.tstart ) THEN
            dleft = ditemp
         ELSE
            dleft = ditemp + 1D0
         ENDIF
 
         ditemp = DINT(tstop/Period)
         IF ( (Period*(ditemp+Pstart)).LT.tstop ) THEN
            dright = ditemp
         ELSE
            dright = ditemp - 1D0
         ENDIF
 
         IF ( Period*(dleft+Pstart).GE.tstart ) THEN
            remleft = pdelta
         ELSE
            remleft = Period*(dleft+Pstop) - tstart
         ENDIF
 
         IF ( Period*(dright+Pstop).LE.tstop ) THEN
            remrig = pdelta
         ELSE
            remrig = tstop - Period*(dright+Pstart)
         ENDIF
 
         EXPCORR = remrig + remleft + pdelta*(dright-(dleft+1D0))
      ENDIF
 
      RETURN
      END

c *****************************************************************

      FUNCTION SUBSET(str1,str2,val)

      IMPLICIT NONE

      LOGICAL SUBSET
      CHARACTER*(*) str1, str2
      INTEGER val

c Returns true if str1 is contained in str2. Returned val is the
c integer interpretation of the rest of str2.

      INTEGER len1, len2, i, d

      INTEGER lenact
      EXTERNAL lenact

      SUBSET=.false.
      val = 0

      len1 = LENACT(str1)
      len2 = LENACT(str2)
      
      IF ( len1 .GT. len2 ) RETURN
       
      IF ( str1(1:len1) .NE. str2(1:len1) ) RETURN
        
      SUBSET = .true.
      val = 0
      d = 1
      DO i = len2, len1+1, -1
         val = val + (ICHAR(str2(i:i))-48)*d
         d = d*10
      ENDDO

      RETURN
      END

c **********************************************************************     
        
        SUBROUTINE SPLITGTI(Gti,Maxgti,Imaxgti,Gtis,Maxgtis,Imaxgtis,
     &                      Ccd,Maxccd,Imaxccd,Status)
        INTEGER Maxgti,Imaxgti,Maxccd,Imaxccd
        INTEGER Maxgtis, Imaxgtis(Maxccd),Status
        DOUBLE PRECISION Gti(3,Maxgti),Gtis(3,Maxgtis,Maxccd)

        INTEGER i,j
        LOGICAL Found

        DOUBLE PRECISION ccd(Maxccd)

        IF(Status.NE.0) RETURN


        Found =.false.

        ccd(1)=Gti(3,1)
        Imaxccd=1
        DO i = 2, Imaxgti
           DO j=1, Imaxccd
             IF(abs(Gti(3,i)-ccd(j)).lt.0.05) THEN
                Found = Found.OR..true.
             ELSE
                Found = Found.OR..false.
             ENDIF
           ENDDO
           IF(Found) THEN
              Found=.false.
           ELSE
              Imaxccd=Imaxccd+1
              ccd(Imaxccd)=Gti(3,i)
           ENDIF
        ENDDO
        DO i =1, Imaxccd
           Imaxgtis(i)=0
        ENDDO

        DO 10 i =1, Imaxgti
           DO j=1,Imaxccd
             IF(abs(Gti(3,i)-ccd(j)).lt.0.05) THEN
                Imaxgtis(j)=Imaxgtis(j)+1
                Gtis(1,Imaxgtis(j),j)=Gti(1,i)
                Gtis(2,Imaxgtis(j),j)=Gti(2,i)
                Gtis(3,Imaxgtis(j),j)=Gti(3,i)
                GOTO 10
              ENDIF
           ENDDO
10     CONTINUE

        RETURN

        END

