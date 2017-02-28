
      SUBROUTINE xprnge(string, nrange_in, rngnam, descr, rnglw,
     &                  rnghi, idefrg, qcor, qsing, qextrm, qintgr, 
     &                  iflag)

      INTEGER   nrange_in, iflag
      REAL      rnglw(nrange_in), rnghi(nrange_in)
      INTEGER   idefrg(nrange_in)
      CHARACTER string*(*), rngnam(nrange_in)*(*), descr*(*)
      LOGICAL   qintgr(2,nrange_in), qextrm(2,nrange_in)
      LOGICAL   qcor, qsing

C---
C Subroutine to parse an argument as several ranges
C delimited by commas. Returns whether each range was
C specified as an integer or as a real
C---

C Arguments :
C string  i    string to be parsed for a general range.
C              N.B., this string must already by extracted
C              from the parse string.  No corrections to
C              the input are made to this string.
C nrange_in i  number of ranges
C rngnam  i    id of ranges
C descr   i    description of all ranges
C rnglw  i/r  returned low end of range (not modified
C              unless xprrng is .TRUE. on return).
C rnghi  i/r  returned hi end of range (see RNGLW).
C idefrg  i    the default ranges if fewer than nrange
C              ranges are input (low values indicate higher
C              priority).  if idefrg(1) is < 0, then the
C              significance of the
C              subranges are handled in strict order of
C              left to right.  If = 0, then the order is
C              from right to left.
C qcor    i    If true, then corrections for individual
C              ranges may be input (but the entire argument
C              is never replaced).
C qsing   i    If true, then only single entrys for each
C              sub-range are allowed (e.g. "1:4:3:2").  If
C              false, then range values are allowed ("1-4").
C              If true only RNGLW is modified.
c qintgr  r    If true then this part of the range was specified as 
c              an integer
c qextrm  r    If true then this part of the range was set to its extremum
C iflag   r     0 - string correctly parsed as a set of ranges.
C              -1 - Eof during the parsing of a correction
C               2 - String was not a range.
C---

      INCLUDE 'xparinc.inc'

      character(1) colon,star
      character(2) DOublestar
      PARAMETER (colon=':',star='*',DOublestar='**')
      INTEGER   maxrng
      PARAMETER(maxrng=10)

      DOUBLE PRECISION dvalue

      INTEGER ivalue, jflag, nrange, icol, i, lnam, lact
      INTEGER irange, jrange, irb, ire, lrng, lc, idash
      INTEGER iparse, lmsg, lmsgp, nret, ifirst2

      CHARACTER substr*30, rngstr*30, type*15, cdummy*80, wrtstr*255

      LOGICAL qdash, qdone, qparin, qint, qnumb

      INTEGER lenact
      LOGICAL xisnum
      EXTERNAL lenact, xisnum

      lact = lenact(string)

c handle the case of the user wanting some information

      iflag=2
      IF ( string .EQ. inquiry ) THEN
         IF ( qcor ) THEN
            xprmsw=' Range of '//descr
            CALL xwrtpr(xprmsw)
            icol=2
            xprmsw(2:)='('
            DO i = 1, nrange_in
               lnam=lenact(rngnam(i))
               xprmsw(icol+1:icol+lnam)=rngnam(i)(:lnam)
               icol=icol+lnam+1
               xprmsw(icol:icol)=colon
               IF ( (icol.GT.60) .AND. (i.LT.nrange_in) ) THEN
                  CALL xwrtpr(xprmsw)
                  icol=1
               ENDIF
            ENDDO
            IF ( icol .EQ. 1 ) THEN
               icol=2
            ENDIF
            xprmsw(icol:icol)=')'
            CALL xwrtpr(xprmsw)
         ENDIF
         RETURN
      ENDIF

C count the number of ranges in the parse string using the number 
C of ':' separators

      irange=0
      DO icol = 1, lact
         IF (string(icol:icol) .EQ. colon) irange = irange + 1
      ENDDO
      irange = irange + 1

      nrange = nrange_in
      IF ( MAX(nrange,irange) .GT. maxrng ) THEN
         WRITE(wrtstr,'(a,i4,a,i4,a,a)') 
     &         ' XPRNGE : Too many sub ranges', MAX(irange,nrange),
     &         ' > ', maxrng, ' while parseing :',
     &         string (:MIN(len(wrtstr)-53,lenact(string)))
         CALL xwrite(wrtstr, 10)
         nrange = MIN(maxrng,nrange)
         irange = MIN(maxrng,irange)
      ENDIF

c loop round the ranges

      icol = 0
      DO jrange = 1, nrange

         qextrm(1,jrange) = .FALSE.
         qextrm(2,jrange) = .FALSE.

c check whether this range is to be processed - if not jump to the
c next range

         IF ( (idefrg(1).GE.0) .AND.
     &        ((idefrg(1).NE.0).OR.(jrange.LT.nrange-irange+1)).AND.
     &        (idefrg(jrange).GT.irange) ) GOTO 400

c find the string for this range

         irb = icol + 1
         icol = INDEX(string(irb:lact),colon) + icol
         IF ( icol .LT. irb ) THEN
            ire = lact
         ELSE
            ire = icol - 1
         ENDIF
         ire = MIN(ire, len(rngstr)+irb-1)
         rngstr = string(irb:ire)
         lrng = ire - irb + 1

c if there is an empty range then take it as equivalent to '*' and
c go onto the next range

         IF ( lrng .LE. 0 ) GOTO 400

c process the current range string

         qdone = .FALSE.
         DO WHILE ( .NOT.qdone )
            qdone = .TRUE.

c if the range is a query then try to get a replacement

            IF ( rngstr .EQ. inquiry ) THEN
               xprmsg = rngnam(jrange)(:lenact(rngnam(jrange)))//':'
               lc = len(rngstr)
               CALL xinfix(xprmsg(:lenact(xprmsg)), rngstr, 
     &                     lc, jflag)
               IF ( jflag .NE. 0 ) THEN
                  iflag = -1
                  RETURN
               ENDIF
               lrng = lenact(rngstr)
            ENDIF

c check whether this range string contains a '-' or a '..' to
c indicate multiple values in the range

            idash = INDEX(rngstr(2:lrng),'-')
            ifirst2 = idash+2
            IF ( idash .EQ. 0 ) THEN
               idash = INDEX(rngstr(2:lrng),'..')
               ifirst2 = idash+3
            ENDIF
            qdash = ( idash .NE. 0 )

c loop over the lower and upper end definitions of the range

            DO i = 1, 2

c set the substring for this part of the range

               IF ( .NOT.qdash ) THEN

                  substr = rngstr
                  type = 'single val for '

               ELSEIF ( qdash .AND. i .EQ. 1 ) THEN

                  substr = rngstr(1:idash)
                  type = 'lower limit of '

               ELSEIF ( qdash .AND. i .EQ. 2 ) THEN

                  substr = rngstr(ifirst2:)
                  type = 'upper limit of '

               ENDIF

c parse the substring for this part of the range

               iparse = 0
               qparin = .FALSE.
               DO WHILE ( .NOT.qparin )
                  qparin = .TRUE.

c if '**' was input then just set to the extremum

                  IF ( substr .EQ. DOublestar ) THEN

                    qextrm(i, jrange) = .TRUE.
                    IF ( i .EQ. 2 ) qintgr(2,jrange) = qintgr(1,jrange)

c if the user is inquiring then prompt to try to get an answer

                  ELSEIF ( substr .EQ. inquiry ) THEN

                     qparin = .FALSE.
                     iparse = len(substr)
                     xprmsg = type//rngnam(jrange)(:
     &                        lenact(rngnam(jrange)))//' ('
                     lmsg = lenact(xprmsg)
                     IF ( i .EQ. 1 ) THEN
                        WRITE(xprmsg(lmsg+1:),'(i12,a)')
     &                       (rnglw(jrange)), ')'
                     ELSE
                        WRITE(xprmsg(lmsg+1:),'(i12,a)')
     &                       (rnghi(jrange)), ')'
                     ENDIF
                     CALL xsquez(xprmsg(lmsg+1:),' ',0,lmsgp)
                     lmsg = lmsg + lmsgp
                     CALL xinfix(xprmsg(:lmsg),substr,iparse,jflag)
                     IF ( jflag .LT. 0 ) THEN
                        iflag = -1
                        RETURN
                     ELSEIF ( jflag .GT. 0 ) THEN
                        qdone = .FALSE.
                        rngstr = inquiry
                        GOTO 300
                     ENDIF

c if this is a real string to interpret

                  ELSEIF ( substr .NE. star ) THEN

c first check if it is a number

                     qnumb = xisnum(substr(:lenact(substr)), dvalue,
     &                              qint, ivalue)

c if corrections are not allowed and it is not a number then give up

                     IF ( .NOT.qcor .AND. .NOT.qnumb ) THEN
                        iflag=2
                        RETURN
                     ENDIF

c try to get a number from the string

                     cdummy = type//rngnam(jrange)(:
     &                        lenact(rngnam(jrange)))//' range'
                     IF ( i .EQ. 1 ) THEN
                        CALL xgtnum(substr, iparse, 1, cdummy, 1, 0.,
     &                              0., 0, 4, rnglw(jrange), nret, 
     &                              jflag, -1)
                     ELSE
                        CALL xgtnum(substr, iparse, 1, cdummy, 1, 0.,
     &                              0., 0, 4, rnghi(jrange), nret, 
     &                              jflag, -1)
                     ENDIF

c if fell off the end of the string then give up

                     IF ( jflag .LT. -1 ) THEN

                        iflag = -1
                        RETURN

c if there is no value input then try again

                     ELSEIF ( jflag .LT. 0 ) THEN

                        qdone = .FALSE.
                        rngstr = inquiry
                        GOTO 300

                     ENDIF

c successfully parsed a number so set logical to indicate whether
c it was an integer or a real

                     qintgr(i, jrange) = .FALSE.
                     IF ( qint ) qintgr(i, jrange) = .TRUE.

                  ENDIF

c end loop on attempts to parse the substring

               ENDDO

c end loop on parts of the range

            ENDDO

c come from for break out of the parsing of the range when an EOF on 
c the input is reached

 300        CONTINUE

c end of loop on attempts to parse the range

         ENDDO

 400     CONTINUE

c end of loop on ranges

      ENDDO

c Ranges successfully processed

      iflag = 0

      RETURN
      END

