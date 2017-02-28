      SUBROUTINE SMATCH(Test, String, Qpart, Qcase, Qmatch)
      CHARACTER   Test*(*), String*(*)
      LOGICAL     Qpart, Qcase, Qmatch
C---
C Search for single string matches in XHELP files.
C---
C TEST    C*    I: test string
C STRING  C*    I: base string
C QPART   L4    I: if true, partial matches (where test is shorter than
C                  string) is allowed, if false, EXACT matches are needed
C QCASE   L4    I: if true, then upper and lower case are NOT signific.
C QMATCH  L4    R: if true, a match was found, if false, then not
C---
C 30-May-1985 - rashafer
C---
      INTEGER   LENACT
      INTEGER*4 length, ilen, ictest, icstr
C---
      qmatch = .FALSE.
      length = lenact(test)
      IF (qpart) THEN
         IF (length.GT.lenact(string)) RETURN
      ELSE
         IF (length.NE.lenact(string)) RETURN
      ENDIF
      IF (qcase) THEN
         DO 190 ilen = 1, length
            ictest = ichar(test(ilen:ilen))
            IF (ictest.GE.ICHAR('A') .AND. ictest.LE.ICHAR('Z'))
     &         ictest = ictest + (ICHAR('a')-ICHAR('A'))
            icstr = ichar(string(ilen:ilen))
            IF (icstr.GE.ICHAR('A') .AND. icstr.LE.ICHAR('Z'))
     &         icstr = icstr + (ICHAR('a')-ICHAR('A'))
            IF (icstr.NE.ictest) RETURN
 190     CONTINUE
      ELSE
         IF (string(:length).NE.test(:length)) RETURN
      ENDIF
      qmatch = .TRUE.
      RETURN
      END
