      SUBROUTINE xprrng(string, nrange_in, rngnam, descr, irnglw,
     &                  irnghi, irngmn, irngmx, idefrg, qcor, qsing, 
     &                  iflag)
      INTEGER   nrange_in, iflag
      CHARACTER string*(*), rngnam(nrange_in)*(*), descr*(*)
      INTEGER   irnglw(nrange_in), irnghi(nrange_in)
      INTEGER   irngmn(nrange_in), irngmx(nrange_in)
      INTEGER   idefrg(nrange_in)
C---
C Subroutine to parse an arguement as several ranges
C delimited by commas
C---
C XPRRNG    r  If true, then a range was sucessfully parsed.
C              If false, then no such parsing was possible.
C string  i    string to be parsed for a general range.
C              N.B., this string must already by extracted
C              from the parse string.  No corrections to
C              the input are made to this string.
C nrange_in i  number of ranges
C rngnam  i    id of ranges
C descr   i    description of all ranges
C irnglw  i/r  returned low end of range (not modified
C              unless xprrng is .TRUE. on return).
C irnghi  i/r  returned hi end of range (see IRNGLW).
C irngmn  i    allowed miminum
C irngmx  i    allowed maximum.  If irngmx(i)<irngmn(i),
C              then no min/max checking is done for that range.
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
C              If true only IRNGLW is modified.
C iflag     r  0 - string correctly parsed as a set of ranges.
C              -1 - Eof during the parsing of a correction
C               2 - String was not a range.
C---
C 1984-Feb-08 - rashafer
C   Version 1.0 only one range can be input, of the form 'first-last'
C      errors in parse conditions are met by prompting for
C      new input.
C   2.0: Modified to XPARSE format, to use the XPARSE routines for
C      I/O.
C      Compound ranges are separted by ':'s, the min and max
C      range are separated by a '-' (after the first CHARACTER)
C      a single * indicates that the previous value for min,max
C      (or both) are to be maintained.  A ** indicates that the
C      min, max (or both) are to take the extrema allowed values.
C---
      INCLUDE 'xparinc.inc'
      character(1) colon,star
      character(2) DOublestar
      PARAMETER (colon=':',star='*',DOublestar='**')
      INTEGER*4 maxrng
      PARAMETER(maxrng=10)

      INTEGER*4 ivalue, jflag
      INTEGER*4 extrem, nrange
      INTEGER*4 tmprng(2,maxrng)
      INTEGER*4 icol, i, lnam, lenact, lact
      INTEGER*4 irange, jrange, irb, ire, lrng, lc, idash
      INTEGER*4 iparse, lmsg, lmsgp, nret

      CHARACTER*(30)substr,rngstr
      character(15) type
      character(80) cdummy
      LOGICAL*4 qcor,qsing
      LOGICAL*4 qdash,qrange,qparin
      LOGICAL*4 xisint
C---
      iflag=2

C If the string has ASCII character > 58 then it can't be a range so
C give up immediately.

      DO icol = 1, lenact(string)
         IF ( ichar(string(icol:icol)) .GT. 58 ) RETURN
      ENDDO

C Test whether the user asked for help

      IF(string.EQ.inquiry) THEN
         IF(qcor) THEN
            xprmsw=' Range of '//descr
            CALL xwrtpr(xprmsw)
            icol=2
            xprmsw(2:)='('
            DO i=1,nrange_in
               lnam=lenact(rngnam(i))
               xprmsw(icol+1:icol+lnam)=rngnam(i)(:lnam)
               icol=icol+lnam+1
               xprmsw(icol:icol)=colon
               IF((icol.GT.60).and.(i.LT.nrange_in)) THEN
                  CALL xwrtpr(xprmsw)
                  icol=1
               ENDIF
            ENDDO
            IF(icol.EQ.1) THEN
               icol=2
            ENDIF
            xprmsw(icol:icol)=')'
            CALL xwrtpr(xprmsw)
         ENDIF
         RETURN
      ENDIF

C** count the number of ranges in the parse string,
C** using the number of ':' separators

      irange=0
      DO icol = 1, lenact(string)
         IF (string(icol:icol) .EQ. colon) irange = irange + 1
      ENDDO
      irange = irange + 1

      nrange=nrange_in
      IF(max(nrange,irange).GT.maxrng) THEN
          write(*,*) 'ERROR*:XPARSE:XPRRNG: Too many sub ranges'
     &         ,max(irange,nrange),' > ',maxrng,' while parseing :',
     &      string
          nrange=min(maxrng,nrange)
          irange=min(maxrng,irange)
      ENDIF
      icol=0
      lact = lenact(string)
      DO jrange=1,nrange
         tmprng(1,jrange)=irnglw(jrange)
         tmprng(2,jrange)=irnghi(jrange)
C**      **check if the range is in the priority range
         IF((idefrg(1).LT.0).or.
     &       ((idefrg(1).EQ.0).and.(jrange.GE.nrange-irange+1)).OR.
     &       (idefrg(jrange).LE.irange)) THEN
            irb=icol+1
            icol=index(string(irb:lact),colon)+icol
            IF(icol.lt.irb) THEN
               ire=lact
            ELSE
               ire=icol-1
            ENDIF
            rngstr=string(irb:ire)
            lrng=ire-irb+1

C **          ** check if there is a real range (an empty range
C **          ** equivalent to *, i.e. nothing between the colons as
C **          ** indicated by lrng = 0). If there is an empty range
C **          ** then return with flag indicating invalid range.

            qrange=(lrng.le.0)
            DO WHILE(.NOT.qrange)
               IF(rngstr.EQ.inquiry) THEN
                  xprmsg=rngnam(jrange)(:lenact(rngnam(jrange)))//':'
                  lc=len(rngstr)
                  CALL xinfix(xprmsg(:lenact(xprmsg)),rngstr,lc,jflag)
                  IF(jflag.ne.0) THEN
                     iflag = -1
                     RETURN
                  ENDIF
                  lrng=lenact(rngstr)
               ENDIF
               idash=index(rngstr(2:min(lrng,len(rngstr))),'-')
               IF(idash.EQ.0) 
     &            idash=index(rngstr(2:min(lrng,len(rngstr))),'..')
               qdash=idash.ne.0
               IF(qdash)idash=idash+1
               qrange=.true.
               DO i=1,2
                  IF(i.EQ.1) THEN
                     IF(qdash) THEN
                        substr=rngstr(1:idash-1)
                        type='lower limit of '
                     ELSE
                        substr=rngstr
                        type='single val for '
                     ENDIF
                     extrem=irngmn(jrange)
                  ELSE
                     type='upper limit of '
                     IF(qdash) THEN
                        IF(rngstr(idash:idash).EQ.'-') THEN
                           substr=rngstr(idash+1:)
                        ELSE
                           substr=rngstr(idash+2:)
                        ENDIF
                     ENDIF
                     extrem=irngmx(jrange)
                  ENDIF
                  iparse=0
                  qparin=.false.
                  DO WHILE(.NOT.qparin)
                     qparin=.true.
                     IF(substr.EQ.DOublestar) THEN
                       tmprng(i,jrange)=extrem

                     ELSEIF(substr.EQ.inquiry) THEN
                        qparin=.false.
                        iparse=len(substr)
                        xprmsg=type//rngnam(jrange)(:
     &                   lenact(rngnam(jrange)))//' ('
                        lmsg=lenact(xprmsg)
                        WRITE(xprmsg(lmsg+1:),'(i12,a)')(tmprng(
     &                   i,jrange)),')'
                        CALL xsquez(xprmsg(lmsg+1:),' ',0,lmsgp)
                        lmsg=lmsg+lmsgp
                        CALL xinfix(xprmsg(:lmsg),substr,iparse,
     &                   jflag)
                        IF(jflag.LT.0) THEN
                           iflag = -1
                           RETURN
                        ELSEIF(jflag.GT.0) THEN
                           qrange=.false.
                           rngstr=inquiry
                           goto 300
                        ENDIF

                     ELSEIF(substr.NE.star) THEN

C **                ** if corrections not allowed, first
C **                ** check if it actually is a number

                        IF(.NOT.qcor) THEN
                           IF(.NOT.xisint(substr(:lenact(substr)),
     &                      ivalue)) THEN
                              iflag=2
                              RETURN
                           ENDIF
                        ENDIF
c                        xprmsg=type//rngnam(jrange)(:
c     &                   lenact(rngnam(jrange)))//' range'
c                        call xgtlin(substr,iparse,1,xprmsg,
c
c fix put in by nick as per Allyns instructions to fix crash
c if non vaild chars entered
c
                        cdummy=type//rngnam(jrange)(:
     &                   lenact(rngnam(jrange)))//' range'
                        call xgtlin(substr,iparse,1,CDUMMY,
     &                   1,irngmn(jrange),irngmx(jrange),1,
     &                   tmprng(i,jrange),nret,jflag,-1)
                        IF(jflag.LT.-1) THEN
                           iflag = -1
                           RETURN
                        ELSEIF(jflag.LT.0) THEN
C **               ** As there is no value input, allow
C **               ** the particular range to be
C **               ** reinput
                           qrange=.false.
                           rngstr=inquiry
                           goto 300
                        ENDIF
                     ENDIF
                  ENDDO
               ENDDO
C **         ** come from for break out of the parsing of the
C **         ** range when an EOF on the input is reached
  300          continue
            ENDDO
         ENDIF
      ENDDO
      iflag=0
      DO i=1,nrange
          irnglw(i)=tmprng(1,i)
          irnghi(i)=tmprng(2,i)
      ENDDO
      RETURN
      END

