
      SUBROUTINE extsev(Status)

      IMPLICIT NONE

      INTEGER Status

c Subroutine to read the event files and set the global variables.
c Also checks that event files are compatible.

c Arguments
c       status         i          r: 0==OK

      INCLUDE 'expar.inc'
      INCLUDE 'extractor.inc'


      DOUBLE PRECISION tdelta(2), location(21), mjdrff1
      DOUBLE PRECISION mintime, maxtime, tmptime(2)

      REAL deadc1, equin

      INTEGER bounds(12), mjdrfi1, obsinf(2)
      INTEGER npoint, ifile

      character(255) infile1, contxt, wrtstr
      character(20) cproj(9), tsys(3), dmkey(8)
      character(40) instru, telesc, filt, datamd, detnam, target
      character(68) ctime(4)

      LOGICAL goodfwcs, setfwcs, qmjdrf1

      INTEGER lenact, exnevf
      character(255) exevfl
      EXTERNAL lenact, exevfl, exnevf

c Get the name of the first event file

      infile1 = exevfl(1, .FALSE., status)
      contxt = 'Failed to get name of first event file to process'
      IF ( status .NE. 0 ) GOTO 999

c Read its header information

      CALL extevh(infile1, tdelta, location, mjdrfi1, mjdrff1, 
     &            qmjdrf1, deadc1, equin, bounds, cproj, ctime, dmkey,
     &            instru, telesc, filt, datamd, detnam, target, tsys, 
     &            goodfwcs, obsinf, Status)
      contxt = 'Failure in EXTEVH'
      IF ( Status .NE. 0 ) GOTO 999

c Set the observation start and stop times

      CALL EXTCONVDT(ctime(1),ctime(2),mintime)
      CALL EXTCONVDT(ctime(3),ctime(4),maxtime)

      csdate = ctime(1)
      cstime = ctime(2)
      cedate = ctime(3)
      cetime = ctime(4)

c Load the global variables (stored in extractor.inc)

      timedel = tdelta(1)
      timepixr = tdelta(2)

      fcrpix(1)  = location(1)
      fcrpix(2)  = location(2)
      fcrval(1)  = location(3)
      fcrval(2)  = location(4)
      fcrdelt(1) = location(5)
      fcrdelt(2) = location(6)
      foptic(1)  = location(7)
      foptic(2)  = location(8)
      fcrota     = location(18)
      skypix     = location(19)

      fctype(1)  = cproj(1)
      fctype(2)  = cproj(2)
      fcname(1)  = cproj(3)
      fcname(2)  = cproj(4)

      fmtype(1) = dmkey(1)
      fmtype(2) = dmkey(2)
      fmform(1) = dmkey(3)
      fmform(2) = dmkey(4)

      hcrdelt(1) = location(9)
      hcrdelt(2) = location(10)
      hcrpix(1)  = location(11)
      hcrpix(2)  = location(12)
      hcrval(1)  = location(20)
      hcrval(2)  = location(21)
      hoptic(1)  = location(13)
      hoptic(2)  = location(14)

      hctype(1)  = cproj(5)
      hctype(2)  = cproj(6)
      hcname(1)  = cproj(7)
      hcname(2)  = cproj(8)

      hmtype(1) = dmkey(5)
      hmtype(2) = dmkey(6)
      hmform(1) = dmkey(7)
      hmform(2) = dmkey(8)

      nompnt(1)   = location(15)
      nompnt(2)   = location(16)
      nompnt(3)   = location(17)
      npoint = 1

      emjdrfi = mjdrfi1
      emjdrff = mjdrff1
      qmjdref = qmjdrf1

      timeref = tsys(1)
      timesys = tsys(2)
      timeunit = tsys(3)

      deadc   = deadc1
      equinox = equin
      radecsys = cproj(9)

      ebound(1) = bounds(1)
      ebound(2) = bounds(2)
      fbound(1) = bounds(3)
      fbound(2) = bounds(5)
      fbound(3) = bounds(4)
      fbound(4) = bounds(6)
      hbound(1) = bounds(7)
      hbound(2) = bounds(9)
      hbound(3) = bounds(8)
      hbound(4) = bounds(10)
      gbound(1) = bounds(11)
      gbound(2) = bounds(12)

      instrument = instru
      telescope  = telesc
      filter     = filt
      datamode   = datamd
      detector   = detnam
      object     = target

      setfwcs = goodfwcs

      scseqend = obsinf(1)
      numobi = obsinf(2)

c Now loop round the rest of the input event files updating time variables
c and checking that the rest are consistent

      DO ifile = 2, exnevf()

         infile1 = exevfl(ifile, .FALSE., status)
         WRITE(contxt, '(a,i4,a)') 'Failed to get name of ', ifile, 
     &                             'th event file'
         IF ( Status .NE. 0 ) GOTO 999

         CALL extevh(infile1, tdelta, location, mjdrfi1, mjdrff1, 
     &               qmjdrf1, deadc1, equin, bounds, cproj, ctime,
     &               dmkey, instru, telesc, filt, datamd, detnam, 
     &               target, tsys, goodfwcs, obsinf, Status)
         contxt = 'Failure in EXTEVH'
         IF ( Status .NE. 0 ) GOTO 999

c update the observation start and stop time

         CALL EXTCONVDT(ctime(1),ctime(2),tmptime(1))
         CALL EXTCONVDT(ctime(3),ctime(4),tmptime(2))

         IF ( tmptime(1) .LT. mintime ) THEN
            mintime = tmptime(1)
            csdate = ctime(1)
            cstime = ctime(2)
         ENDIF
         IF ( tmptime(2) .GT. maxtime ) THEN
            maxtime = tmptime(2)
            cedate = ctime(3)
            cetime = ctime(4)
         ENDIF         

c update the time global variables

         IF ( tdelta(1) .GT. 0 ) timedel = MAX(timedel, tdelta(1))
         IF ( timepixr .NE. tdelta(2) )
     &        CALL fcecho(
     &        'Input event files have inconsistent TIMEPIXR')
            
c check the valid minima and maxima in the relevant columns

         IF ( buildspec ) THEN
            IF ( ebound(1) .NE. bounds(1) .OR. 
     &           ebound(2) .NE. bounds(2) ) 
     &         CALL fcecho(
     &           'Input event files have inconsistent ECOL bounds')
         ENDIF

         IF ( fbound(1) .NE. bounds(3) .OR.
     &        fbound(3) .NE. bounds(4) )
     &      CALL fcecho(
     &           'Input event files have inconsistent XCOLF bounds')

         IF ( fbound(2) .NE. bounds(5) .OR.
     &        fbound(4) .NE. bounds(6) )
     &      CALL fcecho(
     &           'Input event files have inconsistent YCOLF bounds')

         IF ( buildwmap ) THEN

            IF ( hbound(1) .NE. bounds(7) .OR.
     &           hbound(3) .NE. bounds(8) ) THEN
               CALL fcecho(
     &              'Input event files have inconsistent XCOLH bounds')
               WRITE(wrtstr, '(4(a,i6),a)') '(', hbound(1), ',', 
     &              hbound(3), ')   (', bounds(7), ',', bounds(8), ')'
               CALL fcecho(wrtstr)
            ENDIF

            IF ( hbound(2) .NE. bounds(9) .OR.
     &           hbound(4) .NE. bounds(10) ) THEN
               CALL fcecho(
     &              'Input event files have inconsistent YCOLH bounds')
               WRITE(wrtstr, '(4(a,i6),a)') '(', hbound(2), ',', 
     &          hbound(4), ')   (', bounds(7), ',', bounds(8), ')'
               CALL fcecho(wrtstr)
            ENDIF

         ENDIF

         IF ( needgrade ) THEN
            IF ( gbound(1) .NE. bounds(11) .OR. 
     &           gbound(2) .NE. bounds(12) ) 
     &         CALL fcecho(
     &           'Input event files have inconsistent GCOL bounds')
         ENDIF

c Check instrument stuff

         IF ( instru .NE. instrument ) 
     &     CALL fcecho('Input event files have inconsistent INSTRUMENT')
         IF ( telesc .NE. telescope )
     &     CALL fcecho('Input event files have inconsistent TELESCOPE')
         IF ( filt .NE. filter )
     &     CALL fcecho('Input event files have inconsistent FILTER')
         IF ( datamd .NE. datamode )
     &     CALL fcecho('Input event files have inconsistent DATAMODE')
         IF ( detnam .NE. detector )
     &     CALL fcecho('Input event files have inconsistent DETNAM')
         IF ( target .NE. object )
     &     CALL fcecho('Input event files have inconsistent OBJECT')

c Check the MJD ref, deadtime and equinox

         IF ( mjdrfi1 .NE. emjdrfi .OR. mjdrff1 .NE. emjdrff ) THEN
            CALL fcecho(
     &        'Input event files have inconsistent MJD references')
         ENDIF
         IF ( deadc1 .NE. deadc ) 
     &     CALL fcecho('Input event files have inconsistent DEADC')
         IF ( equin .NE. 0 .AND. equin .NE. equinox )
     &     CALL fcecho('Input event files have inconsistent EQUINOX')
         IF ( cproj(9) .NE. ' ' .AND. 
     & cproj(9)(:lenact(cproj(9))) .NE. radecsys(:lenact(radecsys)) )
     &     CALL fcecho('Input event files have inconsistent RADECSYS')

c Check the time system stuff

         IF ( timeref .NE. tsys(1) )
     &     CALL fcecho('Input event files have inconsistent TIMEREF')
         IF ( timesys .NE. tsys(2) )
     &     CALL fcecho('Input event files have inconsistent TIMESYS')
         IF ( timeunit .NE. tsys(3) )
     &     CALL fcecho('Input event files have inconsistent TIMEUNIT')

c If they are in use set SCSEQEND to the higher value and sum the NUMOBI

         scseqend = MAX(scseqend, obsinf(1))
         IF ( obsinf(2) .GT. 0 ) numobi = numobi + obsinf(2)

c Now check the WCS stuff. If we haven't found good WCS previously then
c set from this file otherwise compare.

         IF ( .NOT. setfwcs .AND. goodfwcs ) THEN

            fcrpix(1)  = location(1)
            fcrpix(2)  = location(2)
            fcrval(1)  = location(3)
            fcrval(2)  = location(4)
            fcrdelt(1) = location(5)
            fcrdelt(2) = location(6)
            foptic(1)  = location(7)
            foptic(2)  = location(8)
            fcrota     = location(18)
            skypix     = location(19)

            fctype(1)  = cproj(1)
            fctype(2)  = cproj(2)
            fcname(1)  = cproj(3)
            fcname(2)  = cproj(4)

            fmtype(1)  = dmkey(1)
            fmtype(2)  = dmkey(2)
            fmform(1)  = dmkey(3)
            fmform(2)  = dmkey(4)

            hcrdelt(1) = location(9)
            hcrdelt(2) = location(10)
            hcrpix(1)  = location(11)
            hcrpix(2)  = location(12)
            hcrval(1)  = location(20)
            hcrval(2)  = location(21)
            hoptic(1)  = location(13)
            hoptic(2)  = location(14)

            hctype(1)  = cproj(5)
            hctype(2)  = cproj(6)
            hcname(1)  = cproj(7)
            hcname(2)  = cproj(8)

            hmtype(1)  = dmkey(5)
            hmtype(2)  = dmkey(6)
            hmform(1)  = dmkey(7)
            hmform(2)  = dmkey(8)

            nompnt(1)   = location(15)
            nompnt(2)   = location(16)
            nompnt(3)   = location(17)
            npoint = 1

            setfwcs = .TRUE.

         ELSEIF ( setfwcs .AND. goodfwcs ) THEN

            IF ( fcrpix(1) .NE. location(1) .OR.
     &           fcrpix(2) .NE. location(2) ) THEN
               CALL fcecho(
     &               'Input event files have inconsistent image TCRPX')
               WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') location(1),
     &          fcrpix(1), '  difference = ', location(1)-fcrpix(1)
               CALL fcecho(wrtstr)
               WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') location(2),
     &          fcrpix(2), '  difference = ', location(2)-fcrpix(2)
               CALL fcecho(wrtstr)
            ENDIF
            IF ( fcrval(1) .NE. location(3) .OR.
     &           fcrval(2) .NE. location(4) ) THEN
               CALL fcecho(
     &               'Input event files have inconsistent image TCRVL')
               WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') location(3),
     &          fcrval(1), '  difference = ', location(3)-fcrval(1)
               CALL fcecho(wrtstr)
               WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') location(4),
     &          fcrval(2), '  difference = ', location(4)-fcrval(2)
               CALL fcecho(wrtstr)
            ENDIF
            IF ( fcrdelt(1) .NE. location(5) .OR.
     &           fcrdelt(2) .NE. location(6) ) THEN
               CALL fcecho(
     &               'Input event files have inconsistent image TCRDLT')
               WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') location(5),
     &          fcrdelt(1), '  difference = ', location(5)-fcrdelt(1)
               CALL fcecho(wrtstr)
               WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') location(6),
     &          fcrdelt(2), '  difference = ', location(6)-fcrdelt(2)
               CALL fcecho(wrtstr)
            ENDIF
            IF ( fcrota .NE. location(18) ) THEN
               CALL fcecho(
     &               'Input event files have inconsistent image TCROT')
               WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') location(18),
     &          fcrota, '  difference = ', location(18)-fcrota
               CALL fcecho(wrtstr)
            ENDIF
            IF ( skypix .NE. location(19) ) THEN
               CALL fcecho(
     &           'Input event files have inconsistent sky pixel size')
               WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') location(19),
     &          skypix, '  difference = ', location(19)-skypix
               CALL fcecho(wrtstr)
            ENDIF
            IF ( foptic(1) .NE. location(7) .OR.
     &           foptic(2) .NE. location(8) ) THEN
               CALL fcecho(
     &               'Input event files have inconsistent image OPTIC')
               WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') location(7),
     &          foptic(1), '  difference = ', location(7)-foptic(1)
               CALL fcecho(wrtstr)
               WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') location(8),
     &          foptic(2), '  difference = ', location(8)-foptic(2)
               CALL fcecho(wrtstr)
            ENDIF
            IF ( fctype(1) .NE. cproj(1) .OR.
     &           fctype(2) .NE. cproj(2) ) 
     &         CALL fcecho(
     &               'Input event files have inconsistent image CTYPE')
            IF ( fcname(1) .NE. cproj(3) .OR.
     &           fcname(2) .NE. cproj(4) ) 
     &         CALL fcecho(
     &               'Input event files have inconsistent image CNAME')

            IF ( buildwmap ) THEN

               IF ( hcrdelt(1) .NE. location(9) .OR.
     &              hcrdelt(2) .NE. location(10) ) THEN
                  CALL fcecho(
     &               'Input event files have inconsistent wmap TCRDLT')
                  WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') 
     &             location(9), hcrdelt(1), '  difference = ', 
     &             location(9)-hcrdelt(1)
                  CALL fcecho(wrtstr)
                  WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') 
     &             location(10), hcrdelt(2), '  difference = ', 
     &             location(10)-hcrdelt(2)
                  CALL fcecho(wrtstr)
               ENDIF
               IF ( hcrpix(1) .NE. location(11) .OR.
     &              hcrpix(2) .NE. location(12) ) THEN
                  CALL fcecho(
     &              'Input event files have inconsistent wmap TCRPX')
                  WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') 
     &             location(11), hcrpix(1), '  difference = ', 
     &             location(11)-hcrpix(1)
                  CALL fcecho(wrtstr)
                  WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') 
     &             location(12), hcrpix(2), '  difference = ', 
     &             location(12)-hcrpix(2)
                  CALL fcecho(wrtstr)
               ENDIF
               IF ( hcrval(1) .NE. location(20) .OR.
     &              hcrval(2) .NE. location(21) ) THEN
                  CALL fcecho(
     &              'Input event files have inconsistent wmap TCRVL')
                  WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') 
     &             location(20), hcrval(1), '  difference = ', 
     &             location(20)-hcrval(1)
                  CALL fcecho(wrtstr)
                  WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') 
     &             location(21), hcrval(2), '  difference = ', 
     &             location(21)-hcrval(2)
                  CALL fcecho(wrtstr)
               ENDIF
               IF ( hoptic(1) .NE. location(13) .OR.
     &              hoptic(2) .NE. location(14) ) THEN
                 CALL fcecho(
     &             'Input event files have inconsistent wmap OPTIC')
                  WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') 
     &             location(13), hoptic(1), '  difference = ', 
     &             location(13)-hoptic(1)
                  CALL fcecho(wrtstr)
                  WRITE(wrtstr,'(2(1x,1pg15.8),a,1pg15.8)') 
     &             location(14), hoptic(2), '  difference = ', 
     &             location(14)-hoptic(2)
                  CALL fcecho(wrtstr)
               ENDIF
               IF ( hctype(1) .NE. cproj(5) .OR.
     &              hctype(2) .NE. cproj(6) ) 
     &            CALL fcecho(
     &               'Input event files have inconsistent WMAP CTYPE')
               IF ( hcname(1) .NE. cproj(7) .OR.
     &              hcname(2) .NE. cproj(8) ) 
     &            CALL fcecho(
     &               'Input event files have inconsistent WMAP CNAME')
            ENDIF

            nompnt(1) = nompnt(1) + location(15)
            nompnt(2) = nompnt(2) + location(16)
            nompnt(3) = nompnt(3) + location(17)
            npoint = npoint + 1

         ENDIF

      ENDDO

c Set the RA_PNT, DEC_PNT, PA_PNT keywords. These are an average pointing
c direction. With multiple files we somewhat arbitrarily take the mean of
c the values of the files.

      IF ( npoint .GT. 1 ) THEN
         nompnt(1) = nompnt(1)/npoint
         nompnt(2) = nompnt(2)/npoint
         nompnt(3) = nompnt(3)/npoint
      ENDIF

      IF ( setfwcs ) CALL fcecho(' Getting FITS WCS Keywords')
 

 999  CONTINUE
      IF ( Status .NE. 0 ) THEN
         CALL fcerr(contxt)
         CALL fcerrm(Status)
      ENDIF

      RETURN
      END


