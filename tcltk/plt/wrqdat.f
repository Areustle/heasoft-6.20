      SUBROUTINE WRQDAT(Lun, Ndig, Y, Iery, Mxrow, Nvec)
      INTEGER   Lun, Ndig, Mxrow, Nvec
      INTEGER   Iery(*)
      REAL      Y(*)
C Entry wrqsl/wrqql
      INTEGER   Mxlen1
C---
C Write a single row of data to a QDP file.
C---
C Lun    I    Open LUN
C Ndig   I    Number of digits to write out
C Y      I    Data array
C Iery   I    Error array
C Mxrow  I    First index of Y
C Nvec   I    Number of vectors
C---
C 1996-Jan-24 - [AFT]
C---
      CHARACTER cbuf*1024
      CHARACTER ctok*80
      REAL      rtmp, tmper
      INTEGER   idig, igroup, ind, itmp, lbuf, ltok
      INTEGER   mxlen
      SAVE      mxlen
      DATA      mxlen/256/
C---
 11   FORMAT (A)
C---
      cbuf = ' '
      lbuf = 0
      ind = 1
      DO igroup = 1,Nvec
         rtmp = Y(ind)
         ind = ind + Mxrow
         ctok = ' '
         ltok = 0
         IF(iery(igroup).GT.0) THEN
            tmper = Y(ind)
            IF(Ndig.LT.0) THEN
               idig = 500
               IF(rtmp.NE.0) idig = INT(LOG10(ABS(rtmp))+500.)
               itmp = 500
               IF (tmper.NE.0) itmp = INT(LOG10(ABS(tmper))+500.)
               idig = Ndig-idig+itmp
               CALL CRAMFF(rtmp,6,idig,ctok,ltok)
               ltok = ltok+1
               CALL CRAMFF(tmper,6,Ndig,ctok,ltok)
               IF (iery(igroup).GT.1) THEN
                  ltok = ltok+1
                  tmper = Y(ind+Mxrow)
                  CALL CRAMFF(tmper,6,Ndig,ctok,ltok)
               END IF
            ELSE IF (Ndig.EQ.0) THEN
               CALL CRAMF(rtmp,ctok,ltok)
               ltok = ltok+1
               CALL CRAMF(tmper,ctok,ltok)
               IF(iery(igroup).GT.1) THEN
                  ltok = ltok+1
                  tmper = Y(ind+Mxrow)
                  CALL CRAMF(tmper,ctok,ltok)
               END IF
            ELSE
               CALL CRAMFF(rtmp,0,-Ndig,ctok,ltok)
               ltok = ltok+1
               CALL CRAMFF(tmper,0,-Ndig,ctok,ltok)
               IF(iery(igroup).GT.1) THEN
                  ltok = ltok+1
                  tmper = Y(ind+Mxrow)
                  CALL CRAMFF(tmper,0,-Ndig,ctok,ltok)
               END IF
            END IF
            ind = ind+iery(igroup)*Mxrow
         ELSE IF (Ndig.EQ.0) THEN
            CALL CRAMF(rtmp,ctok,ltok)
         ELSE
            CALL CRAMFF(rtmp,0,-ABS(Ndig),ctok,ltok)
         END IF
         IF (lbuf+ltok.GT.mxlen) THEN
C Write at most mxlen columns on a line, then continue
            WRITE(Lun,11) cbuf(:lbuf-1)//'-'
            cbuf = '  '//ctok
            lbuf = ltok + 3
         ELSE
            cbuf(lbuf+1:lbuf+ltok) = ctok(:ltok)
            lbuf = lbuf + ltok + 1
         END IF
      END DO
C
      WRITE(Lun,11) cbuf(:lbuf-1)
      RETURN
C---
      ENTRY wrqdl()
C Display (SHow) current value.
      WRITE(*,311) mxlen
  311 FORMAT('LEngth ',I5,' ! Maximum length of lines written with',
     &      ' the WData command.')
      RETURN
C---
      ENTRY wrqsl(Mxlen1)
      IF ( Mxlen1.LE.10 ) THEN
         WRITE(*,*) 'LEngth must be larger than 10.  Ignored.'
      ELSE IF ( LEN(cbuf).LT.Mxlen1 ) THEN
         WRITE(*,*) 'LEngth set to maximum of',LEN(cbuf)
         mxlen=LEN(cbuf)
      ELSE
         mxlen=Mxlen1
      END IF
      RETURN
C---
      ENTRY wrqql(Mxlen1)
      mxlen1=mxlen
      RETURN
      END
