      SUBROUTINE GTXHLP(Iunit,Cfile,Ctopic)
      INTEGER   Iunit
      CHARACTER Cfile*(*) , Ctopic*(*)
C---
C Main subroutine to manipulate a DHF format help file interactively.
C---
C IUNIT   I    Unit for reading the help file
C CFILE   I    Name of help file
C CTOPIC  I/O  Initial command string on input, on output the
C              -remainder of the prompt string when a @ special
C              -character is read.
C---
C  6-Jul-1985 rashafer
C version 1.1: modified to allow return with update information in
C      the CTOPIC field when a @ special character is read.
C 29-Jul-1988 - Use GTBUF and some reformatting of output [AFT]
C 11-jan-1989 fwjhaberl
C      Some cosmetic changes to make it more similar to VMS HELP.
C      At the lowest (sub-)topic level the prompt for the
C      next (sub-)topic asks you for (sub-)topics at this
C      lowest level still.
C 02-Oct-1992 BEO/Heasrc
C      Replaced all the WRITE statements with calls to XHELP.
C 09-Nov-1993 BEO/Heasrc
C      Replaced all xhelp calls to xhelpsub calls
C---
C** The block size (size of a single page)
      INTEGER MBLSIZ
      PARAMETER (MBLSIZ=512)
C** The number of pages to keep in the internal buffer.
      INTEGER MXPAGE , ICLSIZ , ILNSIZ
      PARAMETER (MXPAGE=5)
      PARAMETER (ICLSIZ=13,ILNSIZ=78)
C** MXTOP = maximum depth of topics
      INTEGER MXTOP
      PARAMETER (MXTOP=32)
      INTEGER   I4CONV, LENACT
C
      CHARACTER cbuf*(MXPAGE*MBLSIZ)
      CHARACTER ccom*256 , chead*256 , cout*256
      CHARACTER ctok*64 , ctok1*64
      CHARACTER comch*1
      CHARACTER ctmp*1
      INTEGER   itab(6+2*MXPAGE)
      INTEGER   icadd , icmode , ier , iend , ilen , imatch
      INTEGER   ipage , ihlen, isel , ista , iswap, itmp
      INTEGER   jcadd , jchar , jcslnm , jmatch , jpage , kp
      INTEGER   lcom , lenstr , lentop , lentot , ltok
      INTEGER   mode , nchar , ntop
      LOGICAL   qcom , qmatch , qndone
      LOGICAL   qlow
cziqin pan
      LOGICAL   flag
C
      CHARACTER c4tmp*4
      INTEGER i4tmp
      EQUIVALENCE (c4tmp,i4tmp)
C
      INTEGER intpad(MXTOP) , inslad(MXTOP) , ichstr(MXTOP) ,
     &        ichstp(MXTOP) , icnxsl(MXTOP) , icslnm(MXTOP)
C**
C** INTPAD   initial topic address
C** INSLAD   initial selection page address
C** ICHSTR   first char of topic name
C** ICHSTP   last car of topic name
C** ICNXSL   next selection address for this topic
C** ICSLNM   current selection no. for this topic, i.e.,
C**          -icslnm(i) is the sub-topic number current for topic i.
C---
C- Set GTBUF mode to 1 (ignore @, $ and ! characters)
      flag =.false.
      mode = 1
      CALL GTMODE(mode)
C
      ier = 0
      CALL XSQUEZ(Cfile,' ',0,ilen)
      ccom = Cfile
      CALL XTEND(ccom,'dhf')
      CALL OPNCHE(Iunit,ccom,itab,MXPAGE*MBLSIZ,.TRUE.,MBLSIZ,ier)
      IF ( ier.NE.0 ) THEN
C** Come from for the end of the line
         Ctopic = ' '
         GOTO 500
      ENDIF
      ntop = 1
      ccom = Ctopic
      CALL RDCHE(itab,cbuf,ctmp,0,1,ier)
      lenstr = ICHAR(ctmp)
      IF ( lenstr.EQ.0 ) THEN
C New format, iswap>0 indicates bytes should be swapped, ihlen is the
C length, in bytes, of the additional header info. required by new format.
         iswap = 1
         ihlen = 1
         CALL RDCHE(itab,cbuf,ctmp,ihlen,1,ier)
         lenstr = ICHAR(ctmp)
      ELSE
C Old format
         iswap = 0
         ihlen = 0
      END IF
      CALL RDCHE(itab,cbuf,chead,ihlen+1,lenstr,ier)
      CALL DIRPOS(Cfile,ista,iend)
      chead = Cfile(iend+1:)
      itmp = INDEX(chead,'.')
      IF ( itmp.GT.0 ) chead(itmp:) = ' '
      ichstr(1) = 1
      ichstp(1) = LENACT(chead)
      icslnm(1) = 0
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
      icmode = 0
      icadd = lenstr + 1 + ihlen
      icadd = icadd + 4
      CALL RDCHE(itab,cbuf,c4tmp,icadd,4,ier)
      IF ( iswap.NE.0 ) THEN
         inslad(1) = I4CONV(i4tmp)
      ELSE
         inslad(1) = i4tmp
      END IF
      icadd = icadd + 4
      intpad(1) = icadd
 100  CONTINUE
C** Come from for a new command line to process
         lcom = LENACT(ccom)
         IF ( lcom.NE.0 ) THEN
C** Some command line
            qcom = .TRUE.
            kp = 0
            DO WHILE ( (qcom) .AND. (kp.LT.lcom) )
               kp = kp + 1
               comch = ccom(kp:kp)
               IF ( comch.EQ.'?' ) THEN
C** Check if the next char also ?, indicating that XHELP
C** documentation is needed
                  IF ( ccom(kp+1:kp+1).EQ.'?' ) THEN
                     kp = kp + 1
                     CALL SHFDOC
                  ELSE
C** Goto the selection page of current topic otherwise, popup
C** to prev. topic sel page
                     icadd = inslad(ntop) + ihlen
                     IF ( icadd.EQ.ihlen ) THEN
                        CALL XHELPSUB('No selections for '//
     &                             chead(ichstr(ntop):ichstp(ntop)))
                        ntop = ntop - 1
                        icadd = inslad(ntop) + ihlen
                     ENDIF
                     icmode = 2
                  ENDIF
               ELSEIF ( comch.EQ.'^' ) THEN
                  IF ( ntop.GT.1 ) THEN
                     ntop = ntop - 1
                     icmode = 2
                     icadd = inslad(ntop) + ihlen
                  ELSE
                     Ctopic = ' '
                     GOTO 500
                  ENDIF
               ELSEIF ( comch.EQ.'<' ) THEN
                  IF ( ntop.GT.1 ) THEN
C** Move to the previous topic if possible
                     jcslnm = MAX(icslnm(ntop-1)-1,1)
                     icslnm(ntop-1) = jcslnm
                     icadd = inslad(ntop-1) + ihlen
C** Skip over the intervening ones
                     DO 102 isel = 1 , jcslnm - 1
                        CALL RDCHE(itab,cbuf,ctmp,icadd,1,ier)
                        lentop = ICHAR(ctmp)
                        icadd = icadd + 1
                        CALL RDCHE(itab,cbuf,cout,icadd,lentop,ier)
                        icadd = icadd + 4 + lentop
 102                 CONTINUE
C** Now install the new topic name
                     CALL RDCHE(itab,cbuf,ctmp,icadd,1,ier)
                     lentop = ICHAR(ctmp)
                     icadd = icadd + 1
                     CALL RDCHE(itab,cbuf,chead(ichstr(ntop):),icadd,
     &                          lentop,ier)
                     ichstp(ntop) = ichstr(ntop) + lentop - 1
                     icadd = icadd + lentop
                     CALL RDCHE(itab,cbuf,c4tmp,icadd,4,ier)
                     IF ( i4tmp.NE.0 ) THEN
                        jcadd = I4CONV(i4tmp)
                     ELSE
                        jcadd = i4tmp
                     END IF
                     icnxsl(ntop-1) = icadd + 4
                     icadd = jcadd + 4
                     CALL RDCHE(itab,cbuf,c4tmp,icadd,4,ier)
                     IF ( iswap.NE.0 ) THEN
                        inslad(ntop) = I4CONV(i4tmp)
                     ELSE
                        inslad(ntop) = i4tmp
                     END IF
                     icadd = icadd + 4
                     intpad(ntop) = icadd
                  ELSE
                     icadd = intpad(ntop)
                  ENDIF
                  icmode = 0
               ELSEIF ( comch.EQ.'>' ) THEN
                  IF ( ntop.GT.1 ) THEN
                     icadd = icnxsl(ntop-1)
                     CALL RDCHE(itab,cbuf,ctmp,icadd,1,ier)
                     lentop = ICHAR(ctmp)
                     IF ( lentop.GT.0 ) THEN
                        icslnm(ntop-1) = icslnm(ntop-1) + 1
                        icadd = icadd + 1
                        CALL RDCHE(itab,cbuf,chead(ichstr(ntop):),icadd,
     &                             lentop,ier)
                        ichstp(ntop) = ichstr(ntop) + lentop - 1
                        icadd = icadd + lentop
                        CALL RDCHE(itab,cbuf,c4tmp,icadd,4,ier)
                        IF ( iswap.NE.0 ) THEN
                           jcadd = I4CONV(i4tmp) + ihlen
                        ELSE
                           jcadd = i4tmp + ihlen
                        END IF
                        icnxsl(ntop-1) = icadd + 4
                        icadd = jcadd + 4
                        CALL RDCHE(itab,cbuf,c4tmp,icadd,4,ier)
                        IF ( iswap.NE.0 ) THEN
                           inslad(ntop) = I4CONV(i4tmp)
                        ELSE
                           inslad(ntop) = i4tmp
                        END IF
                        icadd = icadd + 4
                        intpad(ntop) = icadd
                     ELSE
                        icadd = intpad(ntop)
                     ENDIF
                  ELSE
                     icadd = intpad(ntop)
                  ENDIF
                  icmode = 0
               ELSEIF ( comch.EQ.'/' .AND. lcom.EQ.1 ) THEN
C** Skip to the next topic in the heirarchy
                  IF ( inslad(ntop).NE.0 ) THEN
                     icadd = inslad(ntop) + ihlen
                     CALL RDCHE(itab,cbuf,ctmp,icadd,1,ier)
                     lentop = ICHAR(ctmp)
                  ELSE
                     lentop = 0
                     DO WHILE ( lentop.EQ.0 )
                        ntop = ntop - 1
                        IF ( ntop.LE.0 ) THEN
                           lentop = -1
                        ELSE
                           icadd = icnxsl(ntop)
                           CALL RDCHE(itab,cbuf,ctmp,icadd,1,ier)
                           lentop = ICHAR(ctmp)
                        ENDIF
                     ENDDO
                  ENDIF
C---
                  IF ( ntop.LE.0 ) THEN
C** Have moved all the way to the end
                     ntop = 1
                     icadd = intpad(1)
                  ELSE
                     ntop = ntop + 1
                     icadd = icadd + 1
                     ichstr(ntop) = ichstp(ntop-1) + 1
                     CALL RDCHE(itab,cbuf,chead(ichstr(ntop):),icadd,
     &                          lentop,ier)
                     icadd = icadd + lentop
                     ichstp(ntop) = ichstp(ntop-1) + lentop
                     CALL RDCHE(itab,cbuf,c4tmp,icadd,4,ier)
                     IF ( iswap.NE.0 ) THEN
                        jcadd = I4CONV(i4tmp) + ihlen
                     ELSE
                        jcadd = i4tmp + ihlen
                     END IF
                     icnxsl(ntop-1) = icadd + 4
                     icadd = jcadd + 4
                     CALL RDCHE(itab,cbuf,c4tmp,icadd,4,ier)
                     IF ( iswap.NE.0 ) THEN
                        inslad(ntop) = I4CONV(i4tmp)
                     ELSE
                        inslad(ntop) = i4tmp
                     END IF
                     icadd = icadd + 4
                     intpad(ntop) = icadd
                  ENDIF
                  icmode = 0
               ELSEIF ( comch.EQ.'@' ) THEN
C** Return, with the CTOPIC reset
                  Ctopic = ccom(kp+1:)
                  GOTO 500
               ELSE
                  qcom = .FALSE.
                  kp = kp - 1
               ENDIF
            ENDDO
C---
            DO WHILE ( kp.LT.lcom )
C** There is still stuff on the command string, which you treat as
C** sub-topic name
               IF ( inslad(ntop).EQ.0 ) THEN
                  kp = lcom + 1
                  CALL XHELPSUB('No selections for '//
     &                       chead(ichstr(ntop):ichstp(ntop)))
               ELSE
                  CALL ALF(ccom,lcom,kp,ctok,ltok)
                  ctok(ltok+1:) = ' '
                  imatch = 0
                  jcadd = inslad(ntop) + ihlen
                  jcslnm = 0
                  DO WHILE ( .TRUE. )
C** Loop for finding matches with topic names
                     CALL RDCHE(itab,cbuf,ctmp,jcadd,1,ier)
                     lentop = ICHAR(ctmp)
                     IF ( lentop.EQ.0 ) GOTO 105
                     jcslnm = jcslnm + 1
                     jcadd = jcadd + 1
                     CALL RDCHE(itab,cbuf,cout,jcadd,lentop,ier)
                     jcadd = jcadd + lentop
                     CALL SMATCH(ctok,cout(:lentop),.TRUE.,.TRUE.,
     &                           qmatch)
                     IF ( qmatch ) THEN
                        CALL RDCHE(itab,cbuf,c4tmp,jcadd,4,ier)
                        IF ( iswap.NE.0 ) THEN
                           jmatch = I4CONV(i4tmp)
                        ELSE
                           jmatch = i4tmp
                        END IF
                        IF ( (lentop.NE.ltok) .AND. imatch.NE.0 ) THEN
C** There is a cross match
                           IF ( imatch.GT.0 ) THEN
                              CALL XHELPSUB(' Multiple choices:')
                              CALL XHELPSUB
     &                           (chead(ichstr(ntop):ichstp(ntop)))
C** Cancel the selector indicator
                              ntop = ntop - 1
                              imatch = -1
                           ENDIF
                           CALL XHELPSUB(cout(:lentop))
                        ELSEIF ( lentop.EQ.ltok ) THEN
C** An exact match
                           IF ( imatch.GT.0 ) THEN
                              ntop = ntop - 1
                           ELSEIF ( imatch.LT.0 ) THEN
                              CALL XHELPSUB('...oops, exact match')
                           ENDIF
                           imatch = jmatch
                        ELSE
                           imatch = jmatch
                        ENDIF
C---
                        IF ( imatch.GT.0 ) THEN
C** Install the name
                           nchar = ichstp(ntop)
                           icslnm(ntop) = jcslnm
                           icnxsl(ntop) = jcadd + 4
                           ntop = ntop + 1
                           ichstr(ntop) = nchar + 1
                           ichstp(ntop) = nchar + lentop
                           chead(ichstr(ntop):ichstp(ntop))
     &                        = cout(:lentop)
                           IF ( lentop.EQ.ltok ) GOTO 105
                        ENDIF
                     ENDIF
                     jcadd = jcadd + 4
                  ENDDO
C** Come from at the end of the loop
C** Check if a match is found, otherwise insert the correction
C** and continue
 105              IF ( imatch.EQ.0 ) THEN
                     CALL XHELPSUB('Sorry, there is no sub-topic '//
     &                          ctok(:ltok))
                     ccom = '?'
                     GOTO 100
                  ELSEIF ( imatch.LT.0 ) THEN
                     ctok = 'Please re-enter:'
                     GOTO 400
                  ELSE
                     icadd = imatch + ihlen
                     icmode = 0
                     icadd = icadd + 4
                     CALL RDCHE(itab,cbuf,c4tmp,icadd,4,ier)
                     IF ( iswap.NE.0 ) THEN
                        inslad(ntop) = I4CONV(i4tmp)
                     ELSE
                        inslad(ntop) = i4tmp
                     END IF
                     icadd = icadd + 4
                     intpad(ntop) = icadd
                  ENDIF
               ENDIF
            ENDDO
         ENDIF
C** Now having processed the command line, output some help begin with
C** the header
         IF ( icmode.LE.1 ) THEN
            IF ( icmode.EQ.0 ) THEN
               CALL WRTTOP(ntop,chead,ichstr,ichstp,cout)
               ctok1 = cout
            ENDIF
            ipage = 0
            qndone = .TRUE.
            DO 120 WHILE ( .TRUE. )
               IF ( qndone ) THEN
                  CALL RDCHE(itab,cbuf,ctmp,icadd,1,ier)
                  lenstr = ICHAR(ctmp)
                  IF ( lenstr.GT.0 ) THEN
                     ipage = ipage + 1
                     icadd = icadd + 1
                     CALL RDCHE(itab,cbuf,cout,icadd,lenstr,ier)
                     CALL XHELPSUB(cout(:lenstr))
                     qndone = ipage.LT.18
                     icadd = icadd + lenstr
                  ELSE
                     qndone = .FALSE.
                  ENDIF
                  GOTO 120
               ENDIF
               IF ( lenstr.NE.0 ) THEN
C** Not the end
                  icmode = 1
C** The end of the topic text was reached
               ELSEIF ( inslad(ntop).GT.0 ) THEN
C** There are sub-tropic selections, so see if there is room for
C** sub-topic names
                  jcadd = inslad(ntop) + ihlen
                  icadd = jcadd
                  jchar = 0
                  qndone = .TRUE.
                  jpage = 0
                  DO 110 WHILE ( .TRUE. )
                     IF ( qndone ) THEN
                        CALL RDCHE(itab,cbuf,ctmp,jcadd,1,ier)
                        lenstr = ICHAR(ctmp)
                        IF ( lenstr.GT.0 ) THEN
                           lentot = (((lenstr/ICLSIZ)+1)*ICLSIZ)
                           IF ( lentot+jchar.GT.ILNSIZ ) THEN
                              jpage = jpage + 1
                              qndone = jpage.LT.18
                              jchar = 0
                           ENDIF
                           IF ( qndone ) THEN
                              jcadd = jcadd + lenstr + 5
                              jchar = jchar + lentot
                           ENDIF
                        ELSE
                           qndone = .FALSE.
                        ENDIF
                        GOTO 110
                     ENDIF
C---
                     CALL XHELPSUB(' ')
                     IF ( ntop.GT.1 ) CALL XHELPSUB(
     &                    '  Additional information available:')
                     CALL XHELPSUB(' ')
                     IF ( (ipage+jpage+1.LT.18) .OR. (jpage.GE.18) )
     &                    THEN
                        icmode = 2
                        flag =.true.
                        GOTO 160
                     ELSE
                        icmode = 3
                     ENDIF
                     GOTO 150
 110              CONTINUE
               ELSE
C** There were no-sub topics, so set the pop up get
                  icmode = 5
               ENDIF
               GOTO 150
 120        CONTINUE
         ENDIF
C---
c 
c Ziqin Pan
c Move label 160 out of IF-THEN-ENDIF block.
c
 160     CONTINUE
 150     IF ( (icmode.EQ.2) .OR. (icmode.EQ.3) .OR. (icmode.EQ.4) ) THEN
            if (.not.flag) then
            ipage = 0
            endif
            IF ( icmode.EQ.2 ) THEN
               if (.not.flag) then
               CALL WRTTOP(ntop,chead,ichstr,ichstp,cout)
               ctok1 = cout
               endif
C** Come from when the selection page is to be written directly
C** after the sub topic text.
c 160           icadd = inslad(ntop) + ihlen
               icadd = inslad(ntop) + ihlen
            ENDIF
            jchar = 0
            cout = ' '
            qndone = .TRUE.
            DO 180 WHILE ( .TRUE. )
               IF ( qndone ) THEN
                  CALL RDCHE(itab,cbuf,ctmp,icadd,1,ier)
                  lenstr = ICHAR(ctmp)
                  IF ( lenstr.GT.0 ) THEN
                     lentot = (((lenstr/ICLSIZ)+1)*ICLSIZ)
                     IF ( lentot+jchar.GT.ILNSIZ ) THEN
C** Write out the line
C                  WRITE (*, '(1x,a)') COUT(:JCHAR)
                        CALL XHELPSUB(cout(:jchar))
                        ipage = ipage + 1
                        qndone = ipage.LT.18
                        jchar = 0
                        cout = ' '
                     ENDIF
                     IF ( qndone ) THEN
                        icadd = icadd + 1
                        CALL RDCHE(itab,cbuf,cout(jchar+1:),icadd,
     &                             lenstr,ier)
                        jchar = jchar + lentot
                        icadd = icadd + lenstr + 4
                     ENDIF
                  ELSE
                     qndone = .FALSE.
                  ENDIF
                  GOTO 180
               ENDIF
               IF ( lenstr.GT.0 ) THEN
C** There are more sub-topic selections
                  icmode = 4
               ELSE
                  icmode = 5
               ENDIF
               IF ( jchar.GT.0 ) THEN
C** Write out the last line
                  CALL XHELPSUB(cout(:jchar))
               ENDIF
               GOTO 200
 180        CONTINUE
         ENDIF
C---
 200     IF ( icmode.EQ.1 .OR. icmode.EQ.4 ) THEN
            ctok = 'Press RETURN to continue ... '
         ELSEIF ( icmode.EQ.5 ) THEN
            CALL XHELPSUB(' ')
            ilen = LENACT(ctok1)
            IF ( inslad(ntop).EQ.0 ) THEN
C ** reached lowest sub-topic level
               ilen = ilen - (ichstp(ntop)-ichstr(ntop)+2)
               ntop = ntop - 1
               qlow = .TRUE.
            ELSE
               qlow = .FALSE.
            ENDIF
            IF ( ntop.EQ.1 ) THEN
               ctok = ctok1(1:ilen) // ' topic? '
            ELSE
               ctok = ctok1(1:ilen) // ' sub-topic? '
            ENDIF
         ELSE
            ctok = 'XHELP>'
         ENDIF
C---
 400     CALL GTBUF(ctok,ier)
         IF ( ier.LT.0 ) THEN
            Ctopic = ' '
            GOTO 500
         ENDIF
         CALL GTREST(ccom,lcom)
         ccom(lcom+1:) = ' '
         IF ( icmode.EQ.5 ) THEN
            IF ( lcom.LE.0 ) THEN
               ccom(1:1) = '^'
               IF ( qlow ) ntop = ntop + 1
            ELSEIF ( ccom.EQ.'/' .OR. ccom.EQ.'>' .OR. ccom.EQ.'<' .OR.
     &            ccom.EQ.'??' ) THEN
               IF ( qlow ) ntop = ntop + 1
            ENDIF
         ENDIF
         IF ( icmode.EQ.1 .AND. ccom.EQ.'/' ) ccom = ' '
      GOTO 100
 500  CALL GTMODE(mode)
      RETURN
      END
