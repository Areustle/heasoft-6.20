      PROGRAM hlpdhf
C---
C XHELP utility program to convert a sequential help format file
C to the direct access form used by the actual help routines.
C The program accomplishes this through two passes through the
C file, the first to generate the sizes and addresses of the various
C topics, the second to actually write out the DHF file.
C    This version can read the same files that can be installed into
C a VMS help library.  All rules for creating a VMS .HLP file should
C be followed, with the following restriction:
C The first line of the .HLP file MUST contain the "1 HELP" topic,
C followed by the top level message.  This is because the current
C version of GTXHLP assumes that the first topic in the .DHF file
C contains the top-level message.
C---
C  3-Aug-1988 - Modified to read .HLP files [AFT]
C 30-May-1985 - rashafer
C---
      INTEGER MXCHR, MXTOP, MXBUF
      PARAMETER (MXCHR=20000)
      PARAMETER (MXTOP=2000)
      PARAMETER (MXBUF=512)
      INTEGER   I4CONV, LENACT, NEWTOP
C
      CHARACTER ctops*(MXCHR)
      CHARACTER cline*256
      CHARACTER cin*132, cout*132
      CHARACTER CTOK*64, CSCR*12
      CHARACTER CBUF*(MXBUF), CTMP*1
      CHARACTER c4tmp*4
      INTEGER   i4tmp
      EQUIVALENCE (c4tmp, i4tmp)
      INTEGER   ichstr(MXTOP), ichstp(MXTOP), inisub(MXTOP),
     &   iprtop(MXTOP), inxtop(MXTOP), itxsiz(MXTOP), islsiz(MXTOP)
      INTEGER   i, icssiz, icsize, ictsiz, idepth, ieqtop, ier, ios
      INTEGER   ipage, IPTOP, IREC, IRHELP, ISUBT, ITLEN, itop
      INTEGER   JDEPTH, JTOP, KP, lbuf, lin, lout
      INTEGER   LLINE, LSCR, LTOK, LTOPS, NTOP
      LOGICAL   qequiv(MXTOP)
C---
C** ichstr   first char of topic name
C** ichstp   pointer to last char of topic name
C** inisub   topic number of first sub-topic or if qequiv is true,
C      indicating that the topic has been equivalenced to
C      a previously defined topic, it is the topic number
C      of that topic.
C** iprtop   topic number of the topic that this topic belongs to.
C      The root topic is topic number 1, which has by
C      definition iprtop = 0.
C** inxtop   topic number of the next topic in the same level.  If
C      zero, then there is currently no next topic at this
C      level.
C** itxsiz   total no. of bytes in the topic text (including the
C      byte for the string lengths)
C** islsiz   total no. of bytes in the selection page, including
C      the byte for the string lengths and addresses.
C---
   11 FORMAT (A)
C---
C** First get the name of the input file
 100  CALL GTBUF('Input .HLP file, [output .DHF file]:', ier)
      IF (ier.LT.0) THEN
         GOTO 900
      END IF
      CALL gtchar(cin, lin)
      CALL gtchar(cout, lout)
      IF (lout.LE.0) cout = cin
C
      CALL xtend(cin, 'hlp')
      CALL openwr(1,cin,'OLD',' ',' ', 0, 1, ios)
      IF (ios.NE.0) THEN
         WRITE (*, *) 'Unable to open sequential file ',
     &                cin(:LENACT(cin))
         GOTO 100
      END IF
C
C** Create output file name from optional second argument or from
C** first argument
C
      CALL xtend(cout, 'dhf')
      CALL OPENWR(2, cout, 'NEW', 'D', ' ', MXBUF, 0, ios)
      IF (ios.NE.0) THEN
         WRITE (*, *) 'Unable to open direct access output file ',
     &                cout(:LENACT(cout))
         GOTO 100
      END IF
      IRHELP=0
C---
C- First scan the file, making the index
      itop = 0
      idepth = 0
      NTOP = 0
      LTOPS = 0
      itxsiz(1) = 0
      IREC = 0
 120  CONTINUE
      READ (1, 11, IOSTAT=ios) cline
      IF (ios.NE.0) THEN
         GOTO 200
      END IF
      IREC = IREC + 1
      IF (NEWTOP(cline).EQ.0) THEN
C- Repeat until next topic is found (counting bytes)
         IF (NTOP.EQ.0) THEN
            WRITE (*, *) 'Error--File must begin with a topic.'
            STOP
         END IF
         CALL notag(cline, lline)
         itxsiz(NTOP) = itxsiz(NTOP) + MAX(1, lline) + 1
         GOTO 120
      END IF
C** The end of a block's text
      JTOP = NTOP + 1
      IF (JTOP.GT.MXTOP) THEN
         WRITE (*, *) '*ERROR*: exceeded max no. of topics'
         GOTO 900
      END IF
      itxsiz(JTOP) = 0
C** Find out the depth of the block.  For level 1, the topic 'HELP'
C** generates the XHELP level 0 text.
      JDEPTH = ICHAR(cline(1:1)) - 48
      LSCR = LENACT(cline)
      IF (JDEPTH.EQ.1 .AND. LSCR.LT.12) THEN
         CSCR = cline
         CALL UPC(CSCR)
         IF (CSCR(LSCR-4:LSCR).EQ.' HELP') THEN
            IF(IREC.EQ.1) THEN
               IRHELP=1
               JDEPTH = JDEPTH - 1
            ELSE
               IF(IRHELP.EQ.0) THEN
                  WRITE (*, *) '"1 HELP" must be first line in file.'
                  STOP
               END IF
            END IF
         END IF
      END IF
      IF (NTOP.LE.0) THEN
C- First time, set initial depth.
         idepth = JDEPTH
      ELSE
         IF (JDEPTH.GT.idepth) THEN
            idepth = idepth + 1
            IF (JDEPTH.GT.idepth) THEN
               WRITE (*, *) 'Topic ', JTOP, ' has incorrect nesting'
            END IF
C** The depth has increased by one, so check if the last topic was
C** not an equivalenced one
            IF (qequiv(NTOP)) THEN
               WRITE (*, *) 'An equivalenced topic can not have a',
     &                      ' sub-topic'
               GOTO 900
            END IF
            inisub(NTOP) = JTOP
            iprtop(JTOP) = NTOP
            inxtop(JTOP) = 0
         ELSEIF (JDEPTH.EQ.idepth) THEN
C** The same depth as the last topic
            inxtop(NTOP) = JTOP
            iprtop(JTOP) = iprtop(NTOP)
            inxtop(JTOP) = 0
         ELSE
C** The topic depth must be reduced
            DO 160 i = idepth - 1, JDEPTH, -1
               NTOP = iprtop(NTOP)
 160        CONTINUE
            idepth = JDEPTH
            inxtop(NTOP) = JTOP
            iprtop(JTOP) = iprtop(NTOP)
            inxtop(JTOP) = 0
         END IF
      END IF
C** JTOP is currently the most recent topic, while NTOP is the topic
C** that it is subsidiary to
      inxtop(JTOP) = 0
      inisub(JTOP) = 0
      LLINE = LENACT(cline)
      KP = 1
      CALL ALF(cline, LLINE, KP, CTOK, LTOK)
C** Check for an equivalenced topic
      qequiv(JTOP) = .FALSE.
      IF (LTOK.GE.2) THEN
         IF (CTOK(LTOK-1:LTOK).EQ.'==') THEN
            CALL FNDTOP(CTOK, LTOK-2, NTOP, ieqtop, ctops, ichstr,
     &                  ichstp)
            IF (ieqtop.NE.0) THEN
               qequiv(JTOP) = .TRUE.
               inisub(JTOP) = ieqtop
               LTOK = LTOK - 2
            ELSE
               WRITE (*, *) 'Unable to make an equivalence match'
            END IF
         END IF
      END IF
C---
      ichstr(JTOP) = LTOPS + 1
      LTOPS = LTOPS + LTOK
      ctops(ichstr(JTOP):LTOPS) = CTOK(:LTOK)
      ichstp(JTOP) = LTOPS
      IPTOP = iprtop(JTOP)
      IF (IPTOP.GT.0) islsiz(IPTOP) = islsiz(IPTOP) + LTOK + 5
      islsiz(JTOP) = 0
      NTOP = JTOP
      GOTO 120
C---
 200  CONTINUE
C---
C- A few debug statements (to print the index)
C      WRITE(*,*) '   I,               TOPIC,STAR,STOP,ISUB,PREV,'//
C     :      'NEXT,TXSZ,SLSIZ'
C      DO 220 I=1,NTOP
C         WRITE(*,211) I,ctops(ichstr(I) :ichstp(I)), ichstr(I),
C     :      ichstp(I), inisub(I), iprtop(I), inxtop(I),
C     :       itxsiz(I), islsiz(I)
C211      FORMAT(I5,1X,A20,7I5,L2)
C220   CONTINUE
C---
C** Having accumulated the information itxsiz will become the block
C** address of the topic, and islsiz will be the address of the
C** selection information.
      icsize = ichstp(1) + 1
      DO 270 itop = 1, NTOP
         ictsiz = itxsiz(itop) + 9
C** The term 9 is added to include 4 bytes for the default higher
C** level block add., the selection page add. (4 bytes), and a
C** single byte for the terminating text string (indicated by a
C** single length byte= 0)
         IF ((.NOT.qequiv(itop)) .AND. (inisub(itop).NE.0)) THEN
            icssiz = islsiz(itop) + 1
         ELSE
            icssiz = 0
         END IF
         itxsiz(itop) = icsize
         icsize = icsize + ictsiz
         IF (icssiz.NE.0) THEN
            islsiz(itop) = icsize
            icsize = icsize + icssiz
         ELSE
            islsiz(itop) = 0
         END IF
 270  CONTINUE
C---
C- Rewind the input file, go through again copying the text to output.
      REWIND (1)
      lbuf = 0
      ipage = 0
C Write a null character to the file to flag that this is a new type
C (portable) DHF file.
      CTMP = CHAR(0)
      CALL CHTOBL(CTMP, 1, CBUF, lbuf, ipage, MXBUF, 2)
      CTMP = CHAR(ichstp(1))
      CALL CHTOBL(CTMP, 1, CBUF, lbuf, ipage, MXBUF, 2)
C
      CALL CHTOBL(ctops(ichstr(1):), ichstp(1), CBUF, lbuf, ipage,
     &            MXBUF, 2)
      itop = 0
      ios = 0
 290  IF (ios.EQ.0) THEN
         READ (1, 11, IOSTAT=ios) cline
         IF (itop.EQ.0 .OR. NEWTOP(cline).EQ.1 .OR. ios.NE.0) THEN
C** A new topic has begun
            IF (itop.GT.0) THEN
C** Close out the previous topic's text page(s)
               CTMP = CHAR(0)
               CALL CHTOBL(CTMP, 1, CBUF, lbuf, ipage, MXBUF, 2)
               IF (islsiz(itop).GT.0) THEN
C** Write out previous topic's selection page
                  ISUBT = inisub(itop)
 310              IF (ISUBT.GT.0) THEN
                     ITLEN = ichstp(ISUBT) - ichstr(ISUBT) + 1
                     CTMP = CHAR(ITLEN)
                     CALL CHTOBL(CTMP, 1, CBUF, lbuf, ipage, MXBUF, 2)
                     CALL CHTOBL(ctops(ichstr(ISUBT):), ITLEN, CBUF,
     &                           lbuf, ipage, MXBUF, 2)
                     IF (qequiv(ISUBT)) THEN
                        i4tmp = I4CONV(itxsiz(inisub(ISUBT)))
                        CALL CHTOBL(c4tmp, 4, CBUF, lbuf, ipage, MXBUF,
     &                              2)
                     ELSE
                        i4tmp = I4CONV(itxsiz(ISUBT))
                        CALL CHTOBL(c4tmp, 4, CBUF, lbuf, ipage, MXBUF,
     &                              2)
                     END IF
                     ISUBT = inxtop(ISUBT)
                     GOTO 310
                  END IF
                  CTMP = CHAR(0)
                  CALL CHTOBL(CTMP, 1, CBUF, lbuf, ipage, MXBUF, 2)
               END IF
            END IF
C
            IF (ios.EQ.0) THEN
C** The old topic was terminated by reading in a new topic card
               itop = itop + 1
C** The beginning of a topic has the default previous topic and the
C** select page's address
               IF (iprtop(itop).EQ.0) THEN
                  i4tmp = 0
                  CALL CHTOBL(c4tmp, 4, CBUF, lbuf, ipage, MXBUF, 2)
               ELSE
                  i4tmp = I4CONV(itxsiz(iprtop(itop)))
                  CALL CHTOBL(c4tmp, 4, CBUF, lbuf, ipage, MXBUF, 2)
               END IF
               i4tmp = I4CONV(islsiz(itop))
               CALL CHTOBL(c4tmp, 4, CBUF, lbuf, ipage, MXBUF, 2)
            END IF
         ELSE
C** An ordinary addition to the current topic text page.
            CALL notag(cline, lline)
            ITLEN = MAX(lline, 1)
            CTMP = CHAR(ITLEN)
            CALL CHTOBL(CTMP, 1, CBUF, lbuf, ipage, MXBUF, 2)
            CALL CHTOBL(cline, ITLEN, CBUF, lbuf, ipage, MXBUF, 2)
         END IF
         GOTO 290
      END IF
C---
C Flush the buffer.
      CALL CHTOBL(cline, -1, CBUF, lbuf, ipage, MXBUF, 2)
 900  CONTINUE
      CALL EDICOM('OFF',3)
      END
C*********
      INTEGER FUNCTION NEWTOP(Cline)
      CHARACTER Cline*(*)
C---
C Searches the the start of a new topic.  For VMS style, the topic
C lines start with a single digit followed by a space.  In the future,
C the topic level may require two digits, in which case this code
C will to be enhanced.
C---
C 29-Jul-1988 - [AFT]
C---
      INTEGER IX
C---
      NEWTOP = 0
      IF (Cline(2:2).NE.' ') RETURN
      IX = ICHAR(Cline(1:1))
      IF (49.LE.IX .AND. IX.LE.57) NEWTOP = 1
      RETURN
      END
C*********
      SUBROUTINE notag(cline, lline)
      CHARACTER cline*(*)
      INTEGER   lline
C---
C Return cline with standard HTML tags (currently <BR>, <PRE> and </PRE>)
C removed.
C---
      CHARACTER ctmp*10
      INTEGER   LENACT
      INTEGER   i, itag, ival
C---
      itag = 0
      ival = 0
      DO i=1,LENACT(cline)
         IF( itag.EQ.0 ) THEN
            IF( cline(i:i).EQ.'<' )THEN
               ctmp = cline(i:)
               CALL UPC(ctmp)
               IF ( ctmp(1:4).EQ.'<BR>' .OR.
     &   ctmp(1:5).EQ.'<PRE>' .OR. ctmp(1:6).EQ.'</PRE>' ) THEN
                  itag=1
               ELSE
                  ival = ival+1
                  cline(ival:ival) = cline(i:i)
               END IF
            ELSE
               ival = ival+1
               cline(ival:ival) = cline(i:i)
            END IF
         ELSE
            IF( cline(i:i).EQ.'>' )THEN
               itag = 0
            END IF
         END IF
      END DO
      lline = ival
      cline(ival+1:) = ' '
      RETURN
      END
