      SUBROUTINE tyxhlp(IUNIT, CFILE, CTOPIC)
      INTEGER IUNIT
      CHARACTER CFILE*(*), CTOPIC*(*)
C---
C Main subroutine to manipulate a DHF format help file interactively.
C---
C IUNIT     I    Unit for reading the help file
C CFILE     I    Name of help file
C CTOPIC    I/O  Initial command string on input, on output
C		the remainder of the prompt string when a @ special
C		character is read.
C---
C 11-mar-1989 fwjhaberl
C		adapted from GTXHLP to do the same job as GTXHLP but to
C               exit at the topic level at which the subroutine is entered.
C---
C** The block size (size of a single page)
      INTEGER MBLSIZ
      PARAMETER (MBLSIZ=512)
C** The number of pages to keep in the internal buffer.
      INTEGER MXPAGE, ICLSIZ, ILNSIZ
      PARAMETER (MXPAGE=5)
      PARAMETER (ICLSIZ=13, ILNSIZ=78)
C** MXTOP = maximum depth of topics
      INTEGER MXTOP
      PARAMETER (MXTOP=32)
      INTEGER LENACT
C
      CHARACTER CBUF*(MXPAGE*MBLSIZ)
      CHARACTER CCOM*256, CHEAD*256, COUT*256
      CHARACTER CTOK*64, ctok1*64
      CHARACTER COMCH*1
      CHARACTER CTMP*1
      INTEGER ITAB(6+2*MXPAGE)
      INTEGER ICADD, ICMODE, IER, IEND, LEN, IMATCH
      INTEGER IPAGE, ISEL, ISTA, ITMP
      INTEGER JCADD, JCHAR, JCSLNM, JMATCH, JPAGE, KP
      INTEGER LCOM, LENSTR, LENTOP, LENTOT, LTOK
      INTEGER MODE, NCHAR, NTOP
      LOGICAL QCOM, QMATCH, QNDONE
      LOGICAL QLOW

      LOGICAl flag
C
      CHARACTER C4TMP*4
      INTEGER I4TMP
      EQUIVALENCE (C4TMP, I4TMP)
C
      INTEGER INTPAD(MXTOP), INSLAD(MXTOP), ICHSTR(MXTOP), ICHSTP(MXTOP)
     &        , ICNXSL(MXTOP), ICSLNM(MXTOP)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INTEGER*4 start_level
      LOGICAL*4 qbegin
 
      qbegin = .TRUE.
      flag = .false.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C**
C** INTPAD	initial topic address
C** INSLAD	initial selection page address
C** ICHSTR	first char of topic name
C** ICHSTP	last car of topic name
C** ICNXSL	next selection address for this topic
C** ICSLNM	current selection no. for this topic, i.e.,
C**		icslnm(i) is the sub-topic number current for topic i.
C---
C- Set GTBUF mode to 1 (ignore @, $ and ! characters)
      MODE = 1
      CALL GTMODE(MODE)
C
      IER = 0
      CALL XSQUEZ(CFILE, ' ', 0, LEN)
      CCOM = CFILE
      CALL XTEND(CCOM, 'DHF')
      CALL OPNCHE(IUNIT, CCOM, ITAB, MXPAGE*MBLSIZ, .TRUE., MBLSIZ, IER)
      IF (IER.NE.0) THEN
         GOTO 900
      ENDIF
      NTOP = 1
      CCOM = CTOPIC
      CALL RDCHE(ITAB, CBUF, CTMP, 0, 1, IER)
      LENSTR = ICHAR(CTMP)
      CALL RDCHE(ITAB, CBUF, CHEAD, 1, LENSTR, IER)
      CALL DIRPOS(CFILE, ISTA, IEND)
      CHEAD = CFILE(IEND+1:)
      ITMP = INDEX(CHEAD, '.')
      IF (ITMP.GT.0) CHEAD(ITMP:) = ' '
      ICHSTR(1) = 1
      ICHSTP(1) = LENACT(CHEAD)
      ICSLNM(1) = 0
C---
C** ICADD is the current address
C** ICMODE is the current mode
C** 0 - at the start of the text page
C** 1 - in the midst of the text page
C** 2 - beginning of the selection page
C** 3 - beginning of selection page (continuing from text)
C** 4 - continuing the selection page
C** 5 - at the end of the current topic (= EoText if no sel.)
C** 6 - move to the beginning of the 'next' topic.
      ICMODE = 0
      ICADD = LENSTR + 1
      ICADD = ICADD + 4
      CALL RDCHE(ITAB, CBUF, C4TMP, ICADD, 4, IER)
      INSLAD(1) = I4TMP
      ICADD = ICADD + 4
      INTPAD(1) = ICADD
C** Come from for a new command line to process
 100  CONTINUE
      LCOM = LENACT(CCOM)
      IF (LCOM.NE.0) THEN
C** Some command line
         QCOM = .TRUE.
         KP = 0
 130     IF ((QCOM) .AND. (KP.LT.LCOM)) THEN
            KP = KP + 1
            COMCH = CCOM(KP:KP)
            IF (COMCH.EQ.'?') THEN
C** Check if the next char also ?, indicating that XHELP
C** documentation is needed
               IF (CCOM(KP+1:KP+1).EQ.'?') THEN
                  KP = KP + 1
                  CALL SHFDOC
               ELSE
C** Goto the selection page of current topic otherwise, popup
C** to prev. topic sel page
                  ICADD = INSLAD(NTOP)
                  IF (ICADD.EQ.0) THEN
                     WRITE (*, *) 'No selections for ',
     &                            CHEAD(ICHSTR(NTOP):ICHSTP(NTOP))
                     NTOP = NTOP - 1
                     ICADD = INSLAD(NTOP)
                  ENDIF
                  ICMODE = 2
               ENDIF
            ELSEIF (COMCH.EQ.'^') THEN
               IF (NTOP.GT.1) THEN
                  NTOP = NTOP - 1
                  ICMODE = 2
                  ICADD = INSLAD(NTOP)
               ELSE
                  GOTO 900
               ENDIF
            ELSEIF (COMCH.EQ.'<') THEN
               IF (NTOP.GT.1) THEN
C** Move to the previous topic if possible
                  JCSLNM = MAX(ICSLNM(NTOP-1)-1, 1)
                  ICSLNM(NTOP-1) = JCSLNM
                  ICADD = INSLAD(NTOP-1)
C** Skip over the intervening ones
                  DO ISEL = 1, JCSLNM - 1
                     CALL RDCHE(ITAB, CBUF, CTMP, ICADD, 1, IER)
                     LENTOP = ICHAR(CTMP)
                     ICADD = ICADD + 1
                     CALL RDCHE(ITAB, CBUF, COUT, ICADD, LENTOP, IER)
                     ICADD = ICADD + 4 + LENTOP
 160              ENDDO
C** Now install the new topic name
                  CALL RDCHE(ITAB, CBUF, CTMP, ICADD, 1, IER)
                  LENTOP = ICHAR(CTMP)
                  ICADD = ICADD + 1
                  CALL RDCHE(ITAB, CBUF, CHEAD(ICHSTR(NTOP):), ICADD,
     &                       LENTOP, IER)
                  ICHSTP(NTOP) = ICHSTR(NTOP) + LENTOP - 1
                  ICADD = ICADD + LENTOP
                  CALL RDCHE(ITAB, CBUF, C4TMP, ICADD, 4, IER)
                  JCADD = I4TMP
                  ICNXSL(NTOP-1) = ICADD + 4
                  ICADD = JCADD + 4
                  CALL RDCHE(ITAB, CBUF, C4TMP, ICADD, 4, IER)
                  INSLAD(NTOP) = I4TMP
                  ICADD = ICADD + 4
                  INTPAD(NTOP) = ICADD
               ELSE
                  ICADD = INTPAD(NTOP)
               ENDIF
               ICMODE = 0
            ELSEIF (COMCH.EQ.'>') THEN
               IF (NTOP.GT.1) THEN
                  ICADD = ICNXSL(NTOP-1)
                  CALL RDCHE(ITAB, CBUF, CTMP, ICADD, 1, IER)
                  LENTOP = ICHAR(CTMP)
                  IF (LENTOP.GT.0) THEN
                     ICSLNM(NTOP-1) = ICSLNM(NTOP-1) + 1
                     ICADD = ICADD + 1
                     CALL RDCHE(ITAB, CBUF, CHEAD(ICHSTR(NTOP):), ICADD,
     &                          LENTOP, IER)
                     ICHSTP(NTOP) = ICHSTR(NTOP) + LENTOP - 1
                     ICADD = ICADD + LENTOP
                     CALL RDCHE(ITAB, CBUF, C4TMP, ICADD, 4, IER)
                     JCADD = I4TMP
                     ICNXSL(NTOP-1) = ICADD + 4
                     ICADD = JCADD + 4
                     CALL RDCHE(ITAB, CBUF, C4TMP, ICADD, 4, IER)
                     INSLAD(NTOP) = I4TMP
                     ICADD = ICADD + 4
                     INTPAD(NTOP) = ICADD
                  ELSE
                     ICADD = INTPAD(NTOP)
                  ENDIF
               ELSE
                  ICADD = INTPAD(NTOP)
               ENDIF
               ICMODE = 0
            ELSEIF (COMCH.EQ.'/'.and. LCOM.eq.1 ) THEN
C** Skip to the next topic in the heirarchy
               IF (INSLAD(NTOP).NE.0) THEN
                  ICADD = INSLAD(NTOP)
                  CALL RDCHE(ITAB, CBUF, CTMP, ICADD, 1, IER)
                  LENTOP = ICHAR(CTMP)
               ELSE
                  LENTOP = 0
 220              IF (LENTOP.EQ.0) THEN
                     NTOP = NTOP - 1
                     IF (NTOP.LE.0) THEN
                        LENTOP = -1
                     ELSE
                        ICADD = ICNXSL(NTOP)
                        CALL RDCHE(ITAB, CBUF, CTMP, ICADD, 1, IER)
                        LENTOP = ICHAR(CTMP)
                     ENDIF
                     GOTO 220
                  ENDIF
               ENDIF
C---
               IF (NTOP.LE.0) THEN
C** Have moved all the way to the end
                  NTOP = 1
                  ICADD = INTPAD(1)
               ELSE
                  NTOP = NTOP + 1
                  ICADD = ICADD + 1
                  ICHSTR(NTOP) = ICHSTP(NTOP-1) + 1
                  CALL RDCHE(ITAB, CBUF, CHEAD(ICHSTR(NTOP):), ICADD,
     &                       LENTOP, IER)
                  ICADD = ICADD + LENTOP
                  ICHSTP(NTOP) = ICHSTP(NTOP-1) + LENTOP
                  CALL RDCHE(ITAB, CBUF, C4TMP, ICADD, 4, IER)
                  JCADD = I4TMP
                  ICNXSL(NTOP-1) = ICADD + 4
                  ICADD = JCADD + 4
                  CALL RDCHE(ITAB, CBUF, C4TMP, ICADD, 4, IER)
                  INSLAD(NTOP) = I4TMP
                  ICADD = ICADD + 4
                  INTPAD(NTOP) = ICADD
               ENDIF
               ICMODE = 0
            ELSEIF (COMCH.EQ.'@') THEN
C** Return, with the CTOPIC reset
               CTOPIC = CCOM(KP+1:)
               GOTO 990
            ELSE
               QCOM = .FALSE.
               KP = KP - 1
            ENDIF
            GOTO 130
         ENDIF
C---
 250     IF (KP.LT.LCOM) THEN
C** There is still stuff on the command string, which you treat as
C** sub-topic name
            IF (INSLAD(NTOP).EQ.0) THEN
               KP = LCOM + 1
               WRITE (*, *) 'No selections for ',
     &                      CHEAD(ICHSTR(NTOP):ICHSTP(NTOP))
            ELSE
               CALL ALF(CCOM, LCOM, KP, CTOK, LTOK)
               CTOK(LTOK+1:) = ' '
               IMATCH = 0
               JCADD = INSLAD(NTOP)
               JCSLNM = 0
C** Loop for finding matches with topic names
 120           CONTINUE
               CALL RDCHE(ITAB, CBUF, CTMP, JCADD, 1, IER)
               LENTOP = ICHAR(CTMP)
               IF (LENTOP.EQ.0) THEN
                  GOTO 140
               ENDIF
               JCSLNM = JCSLNM + 1
               JCADD = JCADD + 1
               CALL RDCHE(ITAB, CBUF, COUT, JCADD, LENTOP, IER)
               JCADD = JCADD + LENTOP
               CALL SMATCH(CTOK, COUT(:LENTOP), .TRUE., .TRUE., QMATCH)
               IF (QMATCH) THEN
                  CALL RDCHE(ITAB, CBUF, C4TMP, JCADD, 4, IER)
                  JMATCH = I4TMP
                  IF ((LENTOP.NE.LTOK) .AND. IMATCH.NE.0) THEN
C** There is a cross match
                     IF (IMATCH.GT.0) THEN
                        WRITE (*, *) ' Multiple choices:'
                        WRITE (*, *) CHEAD(ICHSTR(NTOP):ICHSTP(NTOP))
C** Cancel the selector indicator
                        NTOP = NTOP - 1
                        IMATCH = -1
                     ENDIF
                     WRITE (*, *) COUT(:LENTOP)
                  ELSEIF (LENTOP.EQ.LTOK) THEN
C** An exact match
                     IF (IMATCH.GT.0) THEN
                        NTOP = NTOP - 1
                     ELSEIF (IMATCH.LT.0) THEN
                        WRITE (*, *) '...oops, exact match'
                     ENDIF
                     IMATCH = JMATCH
                  ELSE
                     IMATCH = JMATCH
                  ENDIF
C---
                  IF (IMATCH.GT.0) THEN
C** Install the name
                     NCHAR = ICHSTP(NTOP)
                     ICSLNM(NTOP) = JCSLNM
                     ICNXSL(NTOP) = JCADD + 4
                     NTOP = NTOP + 1
                     ICHSTR(NTOP) = NCHAR + 1
                     ICHSTP(NTOP) = NCHAR + LENTOP
                     CHEAD(ICHSTR(NTOP):ICHSTP(NTOP)) = COUT(:LENTOP)
                     IF (LENTOP.EQ.LTOK) THEN
                        GOTO 140
                     ENDIF
                  ENDIF
               ENDIF
               JCADD = JCADD + 4
               GOTO 120
C** Come from at the end of the loop
 140           CONTINUE
C** Check if a match is found, otherwise insert the correction
C** and continue
               IF (IMATCH.EQ.0) THEN
                  WRITE (*, *) 'Sorry, there is no sub-topic ',
     &                         CTOK(:LTOK)
                  CCOM = '?'
                  GOTO 100
               ELSEIF (IMATCH.LT.0) THEN
                  CTOK = 'Please re-enter:'
                  GOTO 280
               ELSE
                  ICADD = IMATCH
                  ICMODE = 0
                  ICADD = ICADD + 4
                  CALL RDCHE(ITAB, CBUF, C4TMP, ICADD, 4, IER)
                  INSLAD(NTOP) = I4TMP
                  ICADD = ICADD + 4
                  INTPAD(NTOP) = ICADD
               ENDIF
            ENDIF
            GOTO 250
         ENDIF
      ENDIF
C** Now having processed the command line, output some help begin with
C** the header
 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF (qbegin) THEN
         qbegin = .FALSE.
         start_level = ntop
      ENDIF
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      IF (ICMODE.LE.1) THEN
         IF (ICMODE.EQ.0) THEN
            CALL WRTTOP(NTOP, CHEAD, ICHSTR, ICHSTP, COUT)
            ctok1 = cout
         ENDIF
         IPAGE = 0
         QNDONE = .TRUE.
 320     IF (QNDONE) THEN
            CALL RDCHE(ITAB, CBUF, CTMP, ICADD, 1, IER)
            LENSTR = ICHAR(CTMP)
            IF (LENSTR.GT.0) THEN
               IPAGE = IPAGE + 1
               ICADD = ICADD + 1
               CALL RDCHE(ITAB, CBUF, COUT, ICADD, LENSTR, IER)
               WRITE (*, *) COUT(:LENSTR)
               QNDONE = IPAGE.LT.18
               ICADD = ICADD + LENSTR
            ELSE
               QNDONE = .FALSE.
            ENDIF
            GOTO 320
         ENDIF
         IF (LENSTR.EQ.0) THEN
C** The end of the topic text was reached
            IF (INSLAD(NTOP).GT.0) THEN
C** There are sub-tropic selections, so see if there is room for
C** sub-topic names
               JCADD = INSLAD(NTOP)
               ICADD = JCADD
               JCHAR = 0
               QNDONE = .TRUE.
               JPAGE = 0
 350           IF (QNDONE) THEN
                  CALL RDCHE(ITAB, CBUF, CTMP, JCADD, 1, IER)
                  LENSTR = ICHAR(CTMP)
                  IF (LENSTR.GT.0) THEN
                     LENTOT = (((LENSTR/ICLSIZ)+1)*ICLSIZ)
                     IF (LENTOT+JCHAR.GT.ILNSIZ) THEN
                        JPAGE = JPAGE + 1
                        QNDONE = JPAGE.LT.18
                        JCHAR = 0
                     ENDIF
                     IF (QNDONE) THEN
                        JCADD = JCADD + LENSTR + 5
                        JCHAR = JCHAR + LENTOT
                     ENDIF
                  ELSE
                     QNDONE = .FALSE.
                  ENDIF
                  GOTO 350
               ENDIF
C---
               WRITE (*, *)
               IF (ntop.GT.1) WRITE (*, *)
     &                             '  Additional information available:'
               WRITE (*, *)
               IF ((IPAGE+JPAGE+1.LT.18) .OR. (JPAGE.GE.18)) THEN
                  ICMODE = 2
                  flag =.true.
                  GOTO 400
               ELSE
                  ICMODE = 3
               ENDIF
            ELSE
C** There were no-sub topics, so set the pop up get
               ICMODE = 5
            ENDIF
         ELSE
C** Not the end
            ICMODE = 1
         ENDIF
      ENDIF
C---
c Ziqin Pan, June 23, 2005
c Move '400 CONTINUE' out of the if-then-endif block.
c
 400        CONTINUE
      IF ((ICMODE.EQ.2) .OR. (ICMODE.EQ.3) .OR. (ICMODE.EQ.4)) THEN
         if(.not.flag) then
         IPAGE = 0
         endif
         IF (ICMODE.EQ.2) THEN
            if(.not.flag) then
            CALL WRTTOP(NTOP, CHEAD, ICHSTR, ICHSTP, COUT)
            ctok1 = cout
            endif
C** Come from when the selection page is to be written directly
C** after the sub topic text.
c 400        CONTINUE
            ICADD = INSLAD(NTOP)
         ENDIF
         QNDONE = .TRUE.
         JCHAR = 0
         COUT = ' '
 420     IF (QNDONE) THEN
            CALL RDCHE(ITAB, CBUF, CTMP, ICADD, 1, IER)
            LENSTR = ICHAR(CTMP)
            IF (LENSTR.GT.0) THEN
               LENTOT = (((LENSTR/ICLSIZ)+1)*ICLSIZ)
               IF (LENTOT+JCHAR.GT.ILNSIZ) THEN
C** Write out the line
                  WRITE (*, *) COUT(:JCHAR)
                  IPAGE = IPAGE + 1
                  QNDONE = IPAGE.LT.18
                  JCHAR = 0
                  COUT = ' '
               ENDIF
               IF (QNDONE) THEN
                  ICADD = ICADD + 1
                  CALL RDCHE(ITAB, CBUF, COUT(JCHAR+1:), ICADD, LENSTR,
     &                       IER)
                  JCHAR = JCHAR + LENTOT
                  ICADD = ICADD + LENSTR + 4
               ENDIF
            ELSE
               QNDONE = .FALSE.
            ENDIF
            GOTO 420
         ENDIF
         IF (LENSTR.GT.0) THEN
C** There are more sub-topic selections
            ICMODE = 4
         ELSE
            ICMODE = 5
         ENDIF
         IF (JCHAR.GT.0) THEN
C** Write out the last line
C		    if(icmode.EQ.5.AND.ntop.gt.1) then
C			WRITE(*,*) '  ',COUT(:JCHAR)
C		    else
            WRITE (*, *) COUT(:JCHAR)
C		    endif
         ENDIF
      ENDIF
C---
      IF (ICMODE.EQ.1 .OR. ICMODE.EQ.4) THEN
         CTOK = 'Press RETURN to continue ... '
      ELSEIF (ICMODE.EQ.5) THEN
         WRITE (*, *)
         len = lenact(ctok1)
         IF (inslad(ntop).EQ.0) THEN
C ** reached lowest sub-topic level
            len = len - (ichstp(ntop)-ichstr(ntop)+2)
            ntop = ntop - 1
            qlow = .TRUE.
         ELSE
            qlow = .FALSE.
         ENDIF
         IF (ntop.EQ.1) THEN
            ctok = ctok1(1:len)//' topic? '
         ELSE
            ctok = ctok1(1:len)//' sub-topic? '
         ENDIF
      ELSE
         CTOK = 'XHELP>'
      ENDIF
C---
 280  CALL GTBUF(CTOK, IER)
      IF (IER.LT.0) THEN
         GOTO 900
      ENDIF
      CALL GTREST(CCOM, LCOM)
      CCOM(LCOM+1:) = ' '
      IF (ICMODE.EQ.5) THEN
         IF (LCOM.LE.0) THEN
            CCOM(1:1) = '^'
            IF (qlow) ntop = ntop + 1
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
            IF (ntop.LE.start_level) RETURN
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
         ELSEIF (ccom.EQ.'/' .OR. ccom.EQ.'>' .OR. ccom.EQ.'<' .OR.
     &           ccom.EQ.'??') THEN
            IF (qlow) ntop = ntop + 1
         ENDIF
      ENDIF
      IF (ICMODE.EQ.1 .AND. CCOM.EQ.'/') CCOM = ' '
      GOTO 100
C** Come from for the end of the line
 900  CONTINUE
      CTOPIC = ' '
 990  CONTINUE
      CALL GTMODE(MODE)
      RETURN
      END