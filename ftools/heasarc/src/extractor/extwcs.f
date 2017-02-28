
      SUBROUTINE extwcs(unit, xcol, ycol, quiet, descr, crpix, crval, 
     &                  crdelt, crota, ctype, cname, mtype, mform, 
     &                  goodfwcs, status)

      DOUBLE PRECISION crpix(2), crval(2), crdelt(2), crota

      INTEGER unit, xcol, ycol, status

      CHARACTER*(*) ctype(2), cname(2), descr, mtype(2), mform(2)

      LOGICAL goodfwcs, quiet

C Gets the WCS keywords from an event file. Assumes that the event file
C is opened at the correct HDU.
C Arguments :
C        unit      I         i: I/O unit for event file
C        xcol      I         i: X-axis column
C        ycol      I         i: Y-axis column
C        quiet     L         i: If true then no output to terminal
C        descr     C         i: descriptor used in diagnostic messages
C        crpix     D         r: CRPIX keywords
C        crval     D         r: CRVAL keywords
C        crdelt    D         r: CRDELT keyword
C        crota     D         r: CROTA keyword
C        ctype     C         r: CTYPE keywords
c        cname     C         r: CNAME keywords
c        mtype     C         r: MTYPE keywords (up to 2)
c        mform     C         r: MFORM keywords (up to 2)
C        goodfwcs  L         r: true if able to read good WCS info
C        status    I         r: 0==OK

      INCLUDE 'expar.inc'

      DOUBLE PRECISION dinput(MAXCOLS)
      character(10) cinput(MAXCOLS)

      INTEGER nfound, i, j, k, ldescr, ic

      CHARACTER(72) comment
      CHARACTER(20) ccheck(3)

      INTEGER lenact
      EXTERNAL lenact

      goodfwcs = .TRUE.

      ldescr = LENACT(descr)

C Do the CRPIX - first the X-axis

      DO i = 1 , MAXCOLS
         dinput(i) = -999.D0
      ENDDO
      CALL FTGKND(unit, 'tcrpx', 1, MAXCOLS, dinput, nfound, Status)
      IF ( Status.NE.0 ) THEN
         comment = ' tcrpx did not work for '//descr(:ldescr)
         IF ( .NOT.quiet ) CALL fcecho(comment)
         Status = 0
         Goodfwcs = .FALSE.
      ENDIF

      IF ( dinput(xcol).NE.-999.D0 ) THEN
         crpix(1) = dinput(xcol)
      ELSE
         Goodfwcs = .FALSE.
         CALL FTGKYD(unit, 'crpix1', crpix(1), comment, Status)
         IF ( Status.NE.0 ) THEN
            Status = 0
            CALL FTGKYD(unit, 'tcrpx1', crpix(1), comment, Status)
            IF ( Status.NE.0 ) THEN
               comment = ' No '//descr(:ldescr)//
     &                   ' X-axis TCRPX, set to 0'
               IF ( .NOT.quiet ) CALL fcecho(comment)
               crpix(1) = 0
               Status = 0
            ENDIF
         ENDIF
      ENDIF

C then the Y-axis
 
      IF ( dinput(ycol).NE.-999.D0 ) THEN
         crpix(2) = dinput(ycol)
      ELSE
         Goodfwcs = .FALSE.
         CALL FTGKYD(unit, 'crpix2', crpix(2), comment, Status)
         IF ( Status.NE.0 ) THEN
            Status = 0
            CALL FTGKYD(unit, 'tcrpx2', crpix(2), comment, Status)
            IF ( Status.NE.0 ) THEN
               comment = ' No '//descr(:ldescr)//
     &                   ' Y-axis TCRPX, set to 0'
               IF ( .NOT.quiet ) CALL fcecho(comment)
               crpix(2) = 0
               Status = 0
            ENDIF
         ENDIF
      ENDIF

c Now get the CRVAL - first the X-axis
 
      DO i = 1 , MAXCOLS
         dinput(i) = -999.D0
      ENDDO
      CALL FTGKND(unit, 'tcrvl', 1, MAXCOLS, dinput, nfound, Status)
      IF ( Status.NE.0 ) THEN
         comment = descr(:ldescr)//' tcrvl did not work'
         IF ( .NOT.quiet ) CALL fcecho(comment)
         Status = 0
         Goodfwcs = .FALSE.
      ENDIF

      IF ( dinput(xcol).NE.-999.D0 ) THEN
         crval(1) = dinput(xcol)
      ELSE
         Goodfwcs = .FALSE.
         CALL FTGKYD(unit, 'crval1', crval(1), comment, Status)
         IF ( Status.NE.0 ) THEN
            Status = 0
            CALL FTGKYD(unit, 'tcrvl1', crval(1), comment, Status)
            IF ( Status.NE.0 ) THEN
               comment = ' No '//descr(:ldescr)//
     &                   ' X-axis TCRVL, set to 0'
               IF ( .NOT.quiet ) CALL fcecho(comment)
               crval(1) = 0
               Status = 0
            ENDIF
         ENDIF
      ENDIF

C then the Y-axis
 
      IF ( dinput(ycol).NE.-999.D0 ) THEN
         crval(2) = dinput(ycol)
      ELSE
         Goodfwcs = .FALSE.
         CALL FTGKYD(unit, 'crval2', crval(2), comment, Status)
         IF ( Status.NE.0 ) THEN
            Status = 0
            CALL FTGKYD(unit, 'tcrvl2', crval(2), comment, Status)
            IF ( Status.NE.0 ) THEN
               comment = ' No '//descr(:ldescr)//
     &                   ' Y-axis TCRVL, set to 0'
               IF ( .NOT.quiet ) CALL fcecho(comment)
               crval(2) = 0
               Status = 0
            ENDIF
         ENDIF
      ENDIF

C Get the CRDELT - first the X-axis

      DO i = 1 , MAXCOLS
         dinput(i) = -999.D0
      ENDDO
      CALL FTGKND(unit, 'tcdlt', 1, MAXCOLS, dinput, nfound, Status)
      IF ( Status.NE.0 ) THEN
         comment = descr(:ldescr)//' tcdlt did not work'
         IF ( .NOT.quiet ) CALL fcecho(comment)
         Status = 0
         Goodfwcs = .FALSE.
      ENDIF
 
      IF ( dinput(xcol).NE.-999.D0 ) THEN
         crdelt(1) = dinput(xcol)
      ELSE
         Goodfwcs = .FALSE.
         CALL FTGKYD(unit, 'crdelt1', crdelt(1), comment, Status)
         IF ( Status.NE.0 ) THEN
            Status = 0
            CALL FTGKYD(unit, 'tcrdlt1', crdelt(1), comment, Status)
            IF ( Status.NE.0 ) THEN
               comment = ' No '//descr(:ldescr)//
     &                   ' X-axis TCDLT, set to 1'
               IF ( .NOT.quiet ) CALL fcecho(comment)
               crdelt(1) = 1.0
               Status = 0
            ENDIF
         ENDIF
      ENDIF

C then the Y-axis
 
      IF ( dinput(ycol).NE.-999.D0 ) THEN
         crdelt(2) = dinput(ycol)
      ELSE
         Goodfwcs = .FALSE.
         CALL FTGKYD(unit, 'crdelt2', crdelt(2), comment, Status)
         IF ( Status.NE.0 ) THEN
            Status = 0
            CALL FTGKYD(unit, 'tcrdlt2', crdelt(2), comment, Status)
            IF ( Status.NE.0 ) THEN
               comment = ' No '//descr(:ldescr)//
     &                   ' Y-axis TCDLT, set to 1'
               IF ( .NOT.quiet ) CALL fcecho(comment)
               crdelt(2) = 1.0
               Status = 0
            ENDIF
         ENDIF
      ENDIF

C Now the CROTA

      DO i = 1 , MAXCOLS
         dinput(i) = -999.D0
      ENDDO
      CALL FTGKND(unit, 'tcrot', 1, MAXCOLS, dinput, nfound, Status)
      IF ( Status.NE.0 ) THEN
         comment = descr(:ldescr)//' tcrot did not work'
         IF ( .NOT.quiet ) CALL fcecho(comment)
         Status = 0
         Goodfwcs = .FALSE.
      ENDIF
 
      crota = -999.D0
      IF ( dinput(xcol).NE.-999.D0 ) THEN
         crota = dinput(xcol)
      ENDIF
 
      IF ( dinput(ycol).NE.-999.D0 ) THEN
         crota = dinput(ycol)
      ENDIF

      IF ( crota .EQ. -999.D0 ) THEN
         crota = 0.D0
      ENDIF

C Get the CTYPE

      DO i = 1 , MAXCOLS
         cinput(i) = ' '
      ENDDO
      CALL FTGKNS(unit, 'tctyp', 1, MAXCOLS, cinput, nfound, Status)

      IF ( Status.NE.0 .OR. nfound.EQ.0 ) THEN
         Status = 0
         CALL FTGKNS(unit, 'ctype', 1, MAXCOLS, cinput, nfound, Status)
      ENDIF
 
      Status = 0

      ctype(1) = cinput(xcol)
      ctype(2) = cinput(ycol)

C Get the CNAME

      DO i = 1 , MAXCOLS
         cinput(i) = ' '
      ENDDO
      CALL FTGKNS(unit, 'tcnam', 1, MAXCOLS, cinput, nfound, Status)

      IF ( Status.NE.0 .OR. nfound.EQ.0 ) THEN
         Status = 0
         CALL FTGKNS(unit, 'cname', 1, MAXCOLS, cinput, nfound, Status)
      ENDIF
 
      Status = 0

      cname(1) = cinput(xcol)
      cname(2) = cinput(ycol)

c Set any appropriate MFORM and their matching MTYPE. First need to get
c the TTYPE for the X column.

      CALL FTGKNS(unit, 'ttype', xcol, 1, ccheck(1), nfound, Status)

      DO i = 1 , MAXCOLS
         cinput(i) = ' '
      ENDDO
      CALL FTGKNS(unit, 'mform', 1, MAXCOLS, cinput, nfound, Status)
      mform(1) = ' '
      mform(2) = ' '
      mtype(1) = ' '
      mtype(2) = ' '

c Now look for a match between the contents of MFORM and any of TTYPE,
c CTYPE, or CNAME. If the keywords end in -* remove that when doing the
c check.

      ccheck(2) = ctype(1)
      ccheck(3) = cname(1)
      DO i = 1, 3
         ic = index(ccheck(i),'-')
         IF ( ic .NE. 0 ) ccheck(i) = ccheck(i)(1:ic-1)
      ENDDO

      j = 1
      DO i = 1, nfound
         ic = index(cinput(i),',') - 1
         IF ( ( cinput(i)(1:ic).EQ.ccheck(1)(:lenact(ccheck(1))) .OR.
     &          cinput(i)(1:ic).EQ.ccheck(2)(:lenact(ccheck(2))) .OR.
     &          cinput(i)(1:ic).EQ.ccheck(3)(:lenact(ccheck(3))) )
     &        .AND. j .LE. 2 ) THEN
            mform(j) = cinput(i)
            CALL FTGKNS(unit, 'mtype', i, 1, mtype(j), k, Status)
            j = j + 1
         ENDIF
      ENDDO

      RETURN
      END
