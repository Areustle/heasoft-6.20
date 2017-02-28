c	qmatch	rashafer 30 May 1985
c		subroutine to look for single string matches

      LOGICAL*4 FUNCTION qmatch(test,string,qpart,qcase)

c	test	c*	i: test string
c	string	c*	i: base string
c	qpart	L4	i: if true, partial matches (where test is shorter than
c			string) is allowed, if false, EXACT matches are needed
c	qcase	L4	i: if true, then upper and lower case are NOT signific.
c	qmatch	L4	r: if true, a match was found, if false, then not

      CHARACTER*(*) test,string
      LOGICAL*4 qpart,qcase
      INTEGER*4 ica, icz, idel
      INTEGER*4 ilen, length, lenact, ictest, icstr

      ica=ICHAR('A')
      icz=ICHAR('Z')
      idel=ICHAR('a')-ICHAR('A')

      qmatch=.false.
      length=lenact(test)
      IF (qpart) THEN
         IF (length .GT. lenact(string)) RETURN
      ELSE
         IF (length .NE. lenact(string)) RETURN
      ENDIF

      IF (qcase) THEN
         DO ilen=1,length
            ictest=ICHAR(test(ilen:ilen))
            IF ((ictest .GE. ica) .AND. (ictest .LE. icz)) 
     &             ictest=ictest+idel
            icstr=ICHAR(string(ilen:ilen))
            IF ((icstr .GE.ica) .AND. (icstr .LE. icz)) icstr=icstr+idel
            IF (icstr .NE. ictest) RETURN
         ENDDO
      ELSE
         IF (string(:length) .NE. test(:length) ) RETURN
      ENDIF
      qmatch=.true.

      RETURN
      END
