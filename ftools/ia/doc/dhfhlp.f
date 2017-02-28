      PROGRAM DHFHLP
C---
C DHF utility program to convert a direct access help format
C file to the sequential form used for editing
C---
C 29-Jun-1985 - rashafer
C---
      INTEGER MXCHR, MXTOP, MBLSIZ, MXPAGE
      PARAMETER (MXCHR=20000)
      PARAMETER (MXTOP=1000)
C** The block size (size of a single page)
      PARAMETER (MBLSIZ=512)
C** The number of pages to keep in the internal buffer.
      PARAMETER (MXPAGE=5)
      INTEGER   I4CONV
C
      INTEGER ITAB(6+2*MXPAGE)
      INTEGER ICHSTR(MXTOP), ICHSTP(MXTOP), IPRTOP(MXTOP), INXTOP(MXTOP)
     &        , ITXSIZ(MXTOP), ISLSIZ(MXTOP)
      INTEGER IER, LIN, LOUT, IOS, LENACT, ITOP, IDEPTH, ihlen
      INTEGER IERR, NCHAR, JPRTOP, IADD, LENSTR, JTOP, LENN, NTOP
      INTEGER LENTOP
      CHARACTER CTOPIC*(MXCHR)
      CHARACTER CBUF*(MXPAGE*MBLSIZ)
      CHARACTER CLINE*256
      CHARACTER CIN*132, COUT*132
      CHARACTER CTMP*1
      CHARACTER C4TMP
      INTEGER I4TMP
      EQUIVALENCE (C4TMP, I4TMP)
      LOGICAL   QEQUIV(MXTOP)
C** ichstr      first char of topic name
C** ichstp      pointer to last char of topic name
C               indicating that the topic has been equivalenced to
C               a previously defined topic, it is the topic number
C               of that topic.
C** iprtop      topic number of the topic that this topic belongs to.
C               The root topic is topic number 1, which has by
C               definition iprtop = 0.
C** inxtop      topic number of the next topic in the same level.  If
C               zero, then there is currently no next topic at this
C               level.
C** itxsiz      address of text portion of topic, used to identify
C               if two topics are equivalenced
C** islsiz      address in the selection page of the sub-topic of this
C               topic currently being processed.
C
C---
   11 FORMAT (A)
   21 FORMAT (I1, 1X, A)
C---
c** First get the name of the input file
  100 CALL GTBUF('Input .DHF file, [output .HLP file]?', IER)
      IF (IER.LT.0) THEN
         GOTO 8000
      ENDIF
      CALL GTCHAR(CIN, LIN)
      CALL GTCHAR(COUT, LOUT)
      IF (LOUT.LE.0) COUT = CIN
      CALL XTEND(CIN, 'DHF')
      CALL OPNCHE(1, CIN, ITAB, MXPAGE*MBLSIZ, .TRUE., MBLSIZ, IOS)
      IF (IOS.NE.0) THEN
         WRITE (*, *) 'Unable to open DHF file ', CIN(:LENACT(CIN))
         GOTO 100
      ENDIF
C** create output file name from optional second argument or from
C** first argument
      CALL XTEND(COUT, '.HLP')
      CALL OPENWR(2, COUT, 'NEW', ' ', 'L', 0, 0, IOS)
      IF (IOS.NE.0) THEN
         WRITE (*, *) ' Unable to open sequential output file ',
     &                COUT(:LENACT(COUT))
         GOTO 100
      ENDIF
      ITOP = 0
      IDEPTH = 0
      NTOP = 1
      CALL RDCHE(ITAB, CBUF, CTMP, 0, 1, IERR)
      NCHAR = ICHAR(CTMP)
      IF ( NCHAR.EQ.0 ) THEN
         ihlen = 1
         CALL RDCHE(ITAB, CBUF, CTMP, ihlen, 1, IERR)
         NCHAR = ICHAR(CTMP)
      END IF
      CALL RDCHE(ITAB, CBUF, CLINE, 1+ihlen, NCHAR, IERR)
      CTOPIC(1:NCHAR) = CLINE(1:NCHAR)
      ICHSTR(1) = 1
      ICHSTP(1) = NCHAR
      QEQUIV(1) = .FALSE.
      IPRTOP(1) = 0
      INXTOP(1) = 0
      WRITE (2, 21) 1, 'HELP'
      IADD = NCHAR + 1 + ihlen
      JPRTOP = 1
C** jprtop is the topic whose sub-topics are currently being processed
C** loop through for each topic
  200 CONTINUE
C** skip the default top topic
      IADD = IADD + 4
      CALL RDCHE(ITAB, CBUF, C4TMP, IADD, 4, IERR)
      ISLSIZ(NTOP) = I4CONV(I4TMP) + ihlen
      IADD = IADD + 4
C** come from for loop for topic text
  210 CONTINUE
      CALL RDCHE(ITAB, CBUF, CTMP, IADD, 1, IERR)
      LENSTR = ICHAR(CTMP)
      IF (LENSTR.EQ.0) THEN
         GOTO 220
      ENDIF
      IADD = IADD + 1
      CALL RDCHE(ITAB, CBUF, CLINE, IADD, LENSTR, IERR)
      WRITE (2, 11) CLINE(:LENSTR)
      IADD = IADD + LENSTR
      GOTO 210
C** come from when all the topic text has been copied
  220 CONTINUE
C** check that there are any subtopics
      IF (ISLSIZ(NTOP).NE.ihlen) THEN
         IDEPTH = IDEPTH + 1
         JPRTOP = NTOP
      ENDIF
C** come from when popping up a level
  240 CONTINUE
C** islsiz contains the address of the current position in the
C** select page
      IADD = ISLSIZ(JPRTOP)
      CALL RDCHE(ITAB, CBUF, CTMP, IADD, 1, IERR)
      IF (ICHAR(CTMP).EQ.0) THEN
C** the end of the subtopic list
         IDEPTH = IDEPTH - 1
         JPRTOP = IPRTOP(JPRTOP)
C** if jprtop is zero, then we have gone through all the sub-topics
C** of the top level topic, ergo we are finished
         IF (JPRTOP.EQ.0) THEN
            GOTO 400
         ENDIF
         GOTO 240
      ENDIF
C** a new topic
      NTOP = NTOP + 1
      LENSTR = ICHAR(CTMP)
      IADD = IADD + 1
      CALL RDCHE(ITAB, CBUF, CTOPIC(NCHAR+1:), IADD, LENSTR, IERR)
      ICHSTR(NTOP) = NCHAR + 1
      NCHAR = NCHAR + LENSTR
      ICHSTP(NTOP) = NCHAR
      IADD = IADD + LENSTR
      CALL RDCHE(ITAB, CBUF, C4TMP, IADD, 4, IERR)
      ITXSIZ(NTOP) = I4CONV(I4TMP) + ihlen
      IPRTOP(NTOP) = JPRTOP
C** update islsiz to point to the next sub-topic in the selection page
      ISLSIZ(JPRTOP) = IADD + 4
      IADD = ITXSIZ(NTOP)
      IF (IADD.LE.ISLSIZ(JPRTOP)) THEN
C** as the text page address is < the current address the topic is
C** equivalenced, so find which one
         JTOP = 0
         ITOP = 1
  250    IF ((JTOP.EQ.0) .AND. (ITOP.LT.NTOP)) THEN
            IF (ITXSIZ(ITOP).EQ.IADD) JTOP = ITOP
            ITOP = ITOP + 1
            GOTO 250
         ENDIF
         IF (JTOP.EQ.0) THEN
            WRITE (*, *) ' Unmatched equivalenced topic'
         ENDIF
C** Now create the string of names used to identify the equivalenced text
         CLINE = ' '
         LENN = LEN(CLINE)
  260    IF (JTOP.GT.1) THEN
            LENTOP = ICHSTP(JTOP) - ICHSTR(JTOP)
            IF (LENN.LE.LENTOP) THEN
               WRITE (*, *) 'TOO long equivalence string'
               WRITE (*, *) 'ABORT!!!'
               GOTO 8000
            ENDIF
            CLINE(LENN-LENTOP:LENN) = CTOPIC(ICHSTR(JTOP):ICHSTP(JTOP))
            LENN = LENN - LENTOP - 2
            JTOP = IPRTOP(JTOP)
            GOTO 260
         ENDIF
         CLINE(LENN:LENN) = '='
         LENN = LENN - 1
         NCHAR = ICHSTP(NTOP) - ICHSTR(NTOP) + 1
         CLINE(LENN-NCHAR:LENN-1) = CTOPIC(ICHSTR(NTOP):ICHSTP(NTOP))
         WRITE (2, 21) IDEPTH, CLINE(LENN-NCHAR:)
C** an equivalenced sub-topic has no text or sub-topics of its own,
C** so goto the next sub-topic at this level
         GOTO 240
      ELSE
C** just write out the topic header and then go on
         WRITE (2, 21) IDEPTH, CTOPIC(ICHSTR(NTOP):ICHSTP(NTOP))
         GOTO 200
      ENDIF
C** come from end of loop, all the topics have been traced out
  400 CONTINUE
      WRITE (*, *) ' Ntop=', NTOP
      CLOSE (2)
 8000 CONTINUE
      CALL EDICOM('OFF',3)
      END
