      PROGRAM mkhelp
C---
C XHELP utility program to convert a sequential help format file
C to the direct access form used by the actual help routines.
C The program accomplishes this through two passes through the
C file, the first to generate the sizes and addresses of the various
C topics, the second to actually write out the DHF file.
C---
C 30-May-1985 - rashafer
C---
      INTEGER MXCHR, MXTOP, MXBUF
      PARAMETER (MXCHR=20000)
      PARAMETER (MXTOP=2000)
      PARAMETER (MXBUF=512)
      INTEGER   I4CONV, LENACT
C
      CHARACTER ctops*(MXCHR)
      character(1) bopics(MXCHR)
      EQUIVALENCE (ctops,bopics)
      CHARACTER cline*256
      character(1) tbyte, bstring(256)
      EQUIVALENCE (cline,bstring)
      CHARACTER cin*132, cout*132
      LOGICAL*1 qbuf(MXBUF)
      INTEGER   i4tmp
      INTEGER   ichstr(MXTOP), ichstp(MXTOP), inisub(MXTOP),
     &   iprtop(MXTOP), inxtop(MXTOP), itxsiz(MXTOP), islsiz(MXTOP)
      INTEGER   i, icssiz, icsize, ictsiz, idepth, ieqtop, ier
      INTEGER   ios, ipage, irec, lenb, lene, idelim, ichar
      INTEGER   iptop, isubt, itlen, itop, lbuf
      INTEGER   jdepth, jtop, jtxsiz, lin, lout, ntop, nchar
      INTEGER   lenn, iflag
      INTEGER   lu_in, lu_out
      LOGICAL   qskip, qeof
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
 100  CALL GTBUF('Input .SHF file, [output .DHF file]:', ier)
      IF (ier.LT.0) THEN
         GOTO 900
      END IF
      CALL gtchar(cin, lin)
      CALL gtchar(cout, lout)
      IF (lout.LE.0) cout = cin
C
      CALL xtend(cin,'shf')
      CALL getlun(lu_in)
      CALL openwr(lu_in,cin,'OLD',' ',' ',0,1,ios)
      IF ( ios.NE.0 ) THEN
         WRITE (*,*) 'Unable to open sequential file ',
     &                cin(:LENACT(cin))
         GOTO 100
      END IF
C
C** Create output file name from optional second argument or from
C** first argument
C
      CALL xtend(cout,'.dhf')
      CALL getlun(lu_out)
C        lu_out=2
      CALL openwr(lu_out,cout,'NEW','D',' ',MXBUF,0,ios)

      IF ( ios.NE.0 ) THEN
         WRITE (*,*) ' Unable to open direct access output file ',
     &              cout(:LENACT(cout))
         GOTO 100
      END IF
      itop = 0
      idepth = 0
      ntop = 1
      READ (lu_in,11) cline
      nchar = LENACT(cline)
      ctops(1:nchar) = cline(1:nchar)
      ichstr(1) = 1
      ichstp(1) = nchar
      qequiv(1) = .FALSE.
      iprtop(1) = 0
      inxtop(1) = 0
      jtxsiz = 0
      irec = 1
      READ (lu_in,11,IOSTAT=ios) cline
      qeof = (ios.NE.0)
      DO WHILE (.NOT.qeof)
         irec = irec + 1
         DO WHILE (cline(1:1).NE.'*')
            jtxsiz = jtxsiz + max(1,LENACT(cline)) + 1
            READ (lu_in,11,IOSTAT=ios) cline
            IF ( ios.NE.0 ) THEN
               cline(1:1) = '*'
               qeof = .TRUE.
            ELSE
               irec = irec + 1
            END IF
         END DO
C** The end of a block's text
         itxsiz(ntop) = jtxsiz
         jtxsiz = 0
         IF ( .NOT.qeof ) THEN
            jtop = ntop + 1
            IF ( jtop.GT.MXTOP ) THEN
               WRITE (*,*) '*ERROR*: exceeded max no. of topics'
               GO TO 900
            END IF
C** Find out the depth of the block
            jdepth = 2
            DO WHILE (cline(jdepth:jdepth).EQ.'*')
               jdepth = jdepth + 1
            END DO
            jdepth = jdepth - 1
C** JDEPTH corresponds to the last asterixed char
            IF ( jdepth.GT.idepth ) THEN
               idepth = idepth + 1
               IF ( jdepth.GT.idepth ) WRITE (*,*) ' Topic ', jtop,
     &           ' has incorrect nesting'
C** The depth has increased by one, so check if the
C** last topic was not an equivalenced one
               IF ( qequiv(ntop) ) THEN
                  WRITE (*,*) ' An equivalenced topic ',
     &                    ctops(ichstr(ntop):ichstp(ntop)),
     &                    ' can not have a sub-topic:'
                  WRITE (*,*) cline(:LENACT(cline))
                  GOTO 900
               END IF
               inisub(ntop) = jtop
               iprtop(jtop) = ntop
               inxtop(jtop) = 0
            ELSE IF ( jdepth.EQ.idepth ) THEN
C** The same depth as the last topic
               inxtop(ntop) = jtop
               iprtop(jtop) = iprtop(ntop)
               inxtop(jtop) = 0
            ELSE
C** The topic depth must be reduced
               DO i = idepth - 1, jdepth, -1
                  ntop = iprtop(ntop)
               END DO
               idepth = jdepth
               inxtop(ntop) = jtop
               iprtop(jtop) = iprtop(ntop)
               inxtop(jtop) = 0
            END IF
C** JTOP is currently the most recent topic, while NTOP is the topic
C** that it is subsidiary to
            inxtop(jtop) = 0
            inisub(jtop) = 0
            lenn = jdepth
            CALL xgtarg(cline,lenn,lenb,lene,qskip,iflag,idelim)
            ichar = nchar + 1
            nchar = nchar + lene - lenb + 1
            ctops(ichar:nchar) = cline(lenb:lene)
            ichstr(jtop) = ichar
            ichstp(jtop) = nchar
            iptop = iprtop(jtop)
            islsiz(iptop) = islsiz(iptop) + lene - lenb + 6
            islsiz(jtop) = 0
            ntop = jtop
C** Check for an equivalenced topic
            CALL xgtarg(cline,lenn,lenb,lene,qskip,iflag,idelim)
            IF ( cline(lenb:lene).EQ.'=' ) THEN
               CALL gettop(cline,lenn,ieqtop,ctops,ichstr,ichstp,
     &            inisub,inxtop)
               IF ( ieqtop.EQ.0 ) THEN
                  WRITE (*,*) ' No match after topic ',
     &               ctops(ichar:nchar),', equivalence abandoned'
                  qequiv(jtop) = .FALSE.
               ELSE
                  qequiv(jtop) = .TRUE.
                  inisub(jtop) = ieqtop
               END IF
            ELSE
               WRITE (*,*) ' Expected an ''='' in topic header: ',
     &                  cline(:LENACT(cline))
C** Come from for running off the end of the line
               qequiv(jtop) = .FALSE.
            END IF
            READ (lu_in,11,IOSTAT=ios) cline
            IF ( ios.NE.0 ) THEN
               qeof = .TRUE.
            ELSE
               irec = irec + 1
            END IF
         END IF
      END DO
C
      WRITE(*,*) '   I,               TOPIC,STAR,STOP,ISUB,PREV,'//
     :      'NEXT,TXSZ,SLSIZ'
      DO i = 1, ntop
        WRITE (*,'(i5,1x,a20,7i5,l2)') i, ctops(ichstr(i):ichstp(i)),
     &                                 ichstr(i), ichstp(i), inisub(i),
     &                                 iprtop(i), inxtop(i), itxsiz(i),
     &                                 islsiz(i)
      END DO
C
C** Having accumulated the information itxsiz will become the block
C** address of the topic, and islsiz will be the address of the
C** selection information.
      icsize = ichstp(1) + 1
      DO itop = 1, ntop
         ictsiz = itxsiz(itop) + 9
C** The factor of 9 added includes 4 bytes for the
C** default higher level block add., the selection page add.
C** (4 bytes), and a single byte for the terminating text
C** string (indicated by a single length byte = 0)
         IF ( (.NOT.qequiv(itop)) .AND. (inisub(itop).NE.0) ) THEN
            icssiz = islsiz(itop) + 1
         ELSE
            icssiz = 0
         END IF
         itxsiz(itop) = icsize
         icsize = icsize + ictsiz
         IF ( icssiz.NE.0 ) THEN
            islsiz(itop) = icsize
            icsize = icsize + icssiz
         ELSE
            islsiz(itop) = 0
         END IF
      END DO
      lbuf = 0
      ipage = 0
C Write a null character to the file to flag that this is a new type
C (portable) DHF file.
      tbyte = char(0)
      CALL bytobl(tbyte,1,qbuf,lbuf,ipage,MXBUF,lu_out)
      tbyte = char(ichstp(1))
      CALL bytobl(tbyte,1,qbuf,lbuf,ipage,MXBUF,lu_out)
C
      CALL bytobl(bopics(ichstr(1)),ichstp(1),qbuf,lbuf,ipage,MXBUF,
     &            lu_out)
      CLOSE (lu_in)
      CALL openwr(lu_in,cin,'OLD',' ',' ',0,1,ios)
      itop = 0
      ios = 0
      DO WHILE (ios.EQ.0)
         READ (lu_in,11,IOSTAT=ios) cline
         IF ( (itop.EQ.0) .OR. (cline(1:1).EQ.'*') .OR. (ios.NE.0) )
     &        THEN
C** A new topic has begun
            IF ( itop.GT.0 ) THEN
C** Close out the previous topic's text page(s)
               CALL bytobl(0,1,qbuf,lbuf,ipage,MXBUF,lu_out)
               IF ( islsiz(itop).GT.0 ) THEN
C** Write out previous topic's selection page
                  isubt = inisub(itop)
                  DO WHILE (isubt.GT.0)
                     itlen = ichstp(isubt) - ichstr(isubt) + 1
                     tbyte = char(itlen)
                     CALL bytobl(tbyte,1,qbuf,lbuf,ipage,MXBUF,lu_out)
                     CALL bytobl(bopics(ichstr(isubt)),itlen,qbuf,lbuf,
     &                  ipage,MXBUF,lu_out)
                     IF ( qequiv(isubt) ) THEN
                        i4tmp = I4CONV(itxsiz(inisub(isubt)))
                        CALL bytobl(i4tmp,4,qbuf,lbuf,
     &                     ipage,MXBUF,lu_out)
                     ELSE
                        i4tmp = I4CONV(itxsiz(isubt))
                        CALL bytobl(i4tmp,4,qbuf,lbuf,ipage,MXBUF,
     &                        lu_out)
                     END IF
                     isubt = inxtop(isubt)
                  END DO
               CALL bytobl(0,1,qbuf,lbuf,ipage,MXBUF,lu_out)
               END IF
            END IF
            IF ( ios.EQ.0 ) THEN
C** The old topic was terminated by reading in a new topic
C** card
               itop = itop + 1
C** The beginning of a topic has the default previous topic
C** and the select page's address
               IF ( iprtop(itop).EQ.0 ) THEN
                  i4tmp = 0
                  CALL bytobl(0,4,qbuf,lbuf,ipage,MXBUF,lu_out)
               ELSE
                  i4tmp = I4CONV(itxsiz(iprtop(itop)))
                  CALL bytobl(i4tmp,4,qbuf,lbuf,ipage,MXBUF,
     &                    lu_out)
               END IF
               i4tmp = I4CONV(islsiz(itop))
               CALL bytobl(i4tmp,4,qbuf,lbuf,ipage,MXBUF,lu_out)
            END IF
         ELSE
C** An ordinary addition to the current topic text page
            itlen = max(LENACT(cline),1)
            tbyte = char(itlen)
            CALL bytobl(tbyte,1,qbuf,lbuf,ipage,MXBUF,lu_out)
            CALL bytobl(bstring,itlen,qbuf,lbuf,ipage,MXBUF,lu_out)
         END IF
      END DO
      cline = ' '
      IF ( lbuf.GT.0 ) CALL bytobl(bstring,MXBUF-lbuf,qbuf,lbuf,ipage,
     &                             MXBUF,lu_out)
  900 CONTINUE
      CLOSE (lu_in)
      CLOSE (lu_out)
      CALL frelun(lu_in)
      CALL EDICOM('OFF',3)
      END
