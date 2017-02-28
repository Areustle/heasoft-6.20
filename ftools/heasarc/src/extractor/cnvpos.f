
      SUBROUTINE cnvpos(cpos, type, points, regeqi, ierr)

      DOUBLE PRECISION points(2)
      REAL regeqi
      CHARACTER*(*) cpos(2)
      INTEGER type, ierr

c Converts a position into pixels
c Arguments :
c      cpos          c         i: String array containing position
c      type          i         i: Type of position 1=HMS, 2=Degrees, 
c                                 3=Image Pixels, 4=Physical Pixels
c                                 NB case 3 is now obsolete
c      points        d         r: Position in physical coordinates
c      regeqi        r         i: Equinox used for region if type 1 or 2.
c      ierr          i         r: Status  0==OK

      INCLUDE 'extractor.inc'

      DOUBLE PRECISION degpos(2), dval

      INTEGER j, ipt, ilen, isign

      CHARACTER(4) fcoord

      INTEGER lenact
      EXTERNAL lenact

      ierr = 0

c Obsolete case

      IF ( type .EQ. 3 ) THEN
         points(1) = -1
         points(2) = -1
      ENDIF

c If the position is HMS then convert to degrees otherwise just read as
c degrees

      IF ( type .EQ. 1 ) THEN

         DO j = 1, 2

            IF ( index(cpos(j),'-') .EQ. 0 ) THEN
               isign = 1
            ELSE
               isign = -1
            ENDIF

            ilen = lenact(cpos(j))
            ipt = ilen
            DO WHILE ( cpos(j)(ipt:ipt) .NE. ':' .AND. ipt .GT. 1 )
               ipt = ipt - 1
            ENDDO
            READ(cpos(j)(ipt+1:ilen),*,iostat=ierr) dval
            IF ( ierr .NE. 0 ) RETURN
            degpos(j) = dval/3600.d0

            ipt = ipt - 1
            DO WHILE ( cpos(j)(ipt:ipt) .NE. ':' .AND. ipt .GT. 1 )
               ipt = ipt - 1
            ENDDO
            READ(cpos(j)(ipt+1:ipt+2),*,iostat=ierr) dval
            IF ( ierr .NE. 0 ) RETURN
            degpos(j) = degpos(j) + dval/60.d0

            READ(cpos(j)(1:ipt-1),*,iostat=ierr) dval
            IF ( ierr .NE. 0 ) RETURN
            degpos(j) = degpos(j) + ABS(dval)
            degpos(j) = isign * degpos(j)

         ENDDO

         degpos(1) = degpos(1) * 15.d0

      ELSEIF ( type .EQ. 2 ) THEN

         READ (cpos(1),*,IOSTAT=ierr) degpos(1)
         READ (cpos(2),*,IOSTAT=ierr) degpos(2)
         IF ( ierr .NE. 0 ) RETURN

      ENDIF

c If the equinox of the region is different from the event file then precess.
c We ignore the differences between the FK4 and FK5 systems and just transform
c on epoch.

      IF ( type .EQ. 1 .OR. type .EQ. 2 ) THEN

         IF ( regeqi .NE. equinox ) THEN
            IF ( equinox .EQ. 1950. ) THEN
               CALL prec(degpos(1), degpos(2), NINT(regeqi), 1)
            ELSE
               CALL prec(degpos(1), degpos(2), NINT(regeqi), 1)
               CALL prec(degpos(1), degpos(2), NINT(equinox), 2)
            ENDIF
         ENDIF

c Now we do the WCS conversion into pixels... use the fitsio routine
c FTXYPX. All the relevant keywords were read in from the event file
c and placed in the extractor common area. The parsing of fctype is
c to split off the last 4 characters (-SIN, -TAN, etc.) which is what 
c FTXYPX requires.

         fcoord = fctype(1)(lenact(fctype)-3:lenact(fctype))

         CALL ftxypx(degpos(1), degpos(2), fcrval(1), fcrval(2), 
     &               fcrpix(1), fcrpix(2), fcrdelt(1), fcrdelt(2),
     &               fcrota, fcoord, points(1), points(2), ierr)
         IF ( ierr .NE. 0 ) THEN
            CALL fcerr('Failure in FTXYPX')
            CALL fcerrm(1)
         ENDIF

      ENDIF

c Now handle the case when coordinates are physical

      IF ( type .EQ. 4 ) THEN
         READ (cpos(1),*,IOSTAT=ierr) points(1) 
         READ (cpos(2),*,IOSTAT=ierr) points(2) 
      ENDIF

      RETURN
      END








