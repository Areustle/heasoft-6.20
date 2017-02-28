      PROGRAM HLPTOHTML
C Read .HLP file, convert to .HTML hierarchy
C A.Daviel, TRIUMF,  16-FEB-1994 <advax@triumf.ca>
C
C Modifications:
C   1) Allyn Tennant <allyn.tennant@msfc.nasa.gov> some Fortran cleanup,
C      bug fixes, and should now generate HTML that htmlcheck says
C      is clean 1995-Oct-11.
C---
C Logical Unit assignments:
C  1                 Input .hlp file
C  2                 Output .href file
C 10 to 10+MAXLEVEL  Assigned to output .html files, one per level.
C---
C <html>
C <a href="ftp://sundae.triumf.ca/pub2/hlptohtml/hlptohtml.html">HLPTOHTML</a>
C </html>

      IMPLICIT NONE
      INTEGER ITAB
      PARAMETER (ITAB=9)
      INTEGER   LENACT

      character(66) descrip(21)
      DATA descrip/
     &   'HLPTOHTML: This program converts Digital''s DCL HELP files ',
     &   '(.HLP suffix) to HTIML, the Hypertext Markup Language used',
     &   'on the Web. Separate files are produced for different levels',
     &   'in the HELP hierarchy, preserving the hierarchical nature',
     &   'of the Help information, and allowing use of the browser''s',
     &   'BACK command. The Hypertext ouput may be generated with the',
     &   'pre-formatted flag set (<pre>); the user may remove this',
     &   'and refine the presentation after conversion. Alternatively,',
     &   'pairs of <pre> and </pre> directives may be pre-inserted',
     &   'in the HELP file to protect tables, etc., and formatted',
     &   'output produced (the default).',
     &   ' ',
     &'Usage : the program expects a Help file prefix, eg. "FOOBAR".',
     &   'It will then read FOOBAR.HLP, creating the Hypertext files',
     &'FOOBAR.HTML,FOOBAR_1.HTML,FOOBAR_2.HTML, etc. plus FOOBAR.HREF,',
     &   'a list of the references created which may be used to add',
     &   'further cross-references manually.',
     &   'Depending on the format of the original HLP file, the',
     &   'hypertext output may start either in FOOBAR.HTML or in',
     &   'FOOBAR_1.HTML.', ' '/
      INTEGER MAXLEVEL
      PARAMETER (MAXLEVEL=9)    ! max. no. of levels in HLP file
C
      character(132) line, line2
      character(132) title
      character(80) filenm
      character(40) cpath
      INTEGER lpath
      character(30) prefix
      character(80) word, words(MAXLEVEL)
      character(34) part, parts(MAXLEVEL)
      character(7) ref
      character(1) pre_flag
      INTEGER wns(MAXLEVEL)
      INTEGER ios, level, prev_level, k, j
      INTEGER wn, fln, pxn, tn
      INTEGER partn, nl, refn
      INTEGER nch, nquotes
C The qpre flag is .TRUE. if a "<pre>" (preformat) command has been
C written to the output file and is active.  Several HTML commands are
C not legal inside a <pre>...</pre> block, so we must track this state.
      LOGICAL qpre
C The </dir> command can only be issued if <dir> was issued somewhere
C in the current file.  Hence its status must be tracked on a per level
C basis.
      LOGICAL qdir(0:MAXLEVEL)
      DATA qdir/.FALSE.,MAXLEVEL*.FALSE./
C---
 100  PRINT *, 'Enter Help file prefix (blank for help):'
      READ 101, prefix
 101  FORMAT (a)
      IF ( prefix.EQ.' ' ) THEN
         WRITE (*,111) descrip
 111     FORMAT (1X,A)
         GOTO 100
      END IF
      pxn = LENACT(prefix)
      OPEN (UNIT=1,FILE=prefix(:pxn)//'.hlp',STATUS='old',
     &   IOSTAT=IOS)
      IF ( IOS.NE.0 ) THEN
         WRITE(*,*) 'Unable to open file: ',prefix(:pxn)//'.hlp'
         GOTO 100
      END IF
      OPEN (UNIT=2,FILE=prefix(:pxn)//'.href',STATUS='new')
C     &      CARRIAGECONTROL='list')
C
      PRINT *, 'Default to pre-formatted HTML (y/n) ?'
      READ 121, pre_flag
 121  FORMAT (a)
      IF ( pre_flag.EQ.'y' ) pre_flag = 'Y'
C
      PRINT *, 'Enter optional href filename path (default blank):'
      READ 101, cpath
      lpath = LENACT(cpath)
      IF ( lpath.EQ.0 ) THEN
C Avoid trouble with cpath(1:0); squash will eat extra space
         lpath = 1
         cpath = ' '
      END IF
C
      prev_level = 0
      level = 0
      partn = 0
      refn = 0
      nquotes = 0
      WRITE (2,*) 'List of references for ', prefix

C Create base file
      filenm = cpath(1:lpath) // prefix // '.html'
      CALL SQUASH(filenm,fln)
      CALL OPHTML(10+level, filenm, fln, prefix, pxn)
      qpre = .FALSE.

 200  READ (1,101,END=900) line
      nch = LENACT(line)
      nl = nl + 1
      IF ( nch.EQ.0 .OR. line.EQ.' ' ) THEN
C Paragraph
         IF ( level.GT.0 ) THEN
            IF ( qpre ) THEN
               WRITE (10+level,121)
            ELSE
               WRITE (10+level,121) '<p>'
            END IF
         END IF
         GOTO 200
      END IF
C
      IF ( (line(1:1).LT.'1' .OR.  line(1:1).GT.'9') .OR.
     &     (line(2:2).NE.' ' .AND. line(2:2).NE.CHAR(ITAB)) ) GOTO 300
      READ (line,201,ERR=300) level
 201  FORMAT (bn,i1)
C
      word = line(2:nch)
      wn = nch - 1
      CALL SQUASH(word, wn)
      wns(level) = wn
      words(level) = word
C      WRITE(*,*) 'Level=',level,', Key="',word(1:wn),'"'
      IF ( level.LT.prev_level ) THEN
         IF ( qpre ) THEN
            WRITE (10+prev_level,121) '</pre>'
            qpre = .FALSE.
         END IF
         DO 220 k = prev_level, level + 1, -1
            IF ( qdir(k) ) THEN
               WRITE (10+k,121) '</dir>'
               qdir(k) = .FALSE.
            END IF
            WRITE (10+k,121) '</body>'
C            WRITE (10+k,121) '<!-- Converted from .HLP to .HTML by -->'
C            WRITE (10+k,121) '<!-- <a href='//
C     & '"ftp://sundae.triumf.ca/pub2/hlptohtml/hlptohtml.html">'//
C     & 'hlptohtml</a>. -->'
            WRITE (10+k,121) '</html>'
            CLOSE (UNIT=10+k)
            PRINT *, 'Closing unit ', 10 + k
 220     CONTINUE
         part = parts(level)    ! restore filename to old level
         filenm = cpath(1:lpath) // part // '.html'
         CALL SQUASH(filenm,fln)
         WRITE (10+level,121) '</dir>'
         qdir(level) = .FALSE.
      END IF
      IF ( level.GT.prev_level+1 ) PRINT *,
     &     'ERROR - skipped level in HELP file',level,prev_level
      IF ( level.GT.prev_level ) THEN
         partn = partn + 1
         WRITE (part,231) prefix, partn
 231     FORMAT (a,'_',i3)
         parts(level) = part
         filenm = cpath(1:lpath) // part // '.html'
         CALL SQUASH(filenm,fln)
C Write header for new file
         title = words(1)(1:wns(1))
         tn = wns(1)
         DO 240 k = 2, level - 1
            title = title(1:tn) // ':' // words(k)(1:wns(k))
            CALL SQUASH(title,tn)
 240     CONTINUE
         CALL OPHTML(10+level, filenm, fln, title, tn)

         IF ( qpre ) THEN
            WRITE (10+prev_level,121) '</pre>'
            qpre = .FALSE.
         END IF
         IF ( level.GT.1 ) THEN
            WRITE(10+prev_level,121) '<p>Additional Information on:<br>'
         ELSE
            WRITE (10+prev_level,121) '<p>Information available on:<br>'
         END IF
         WRITE (10+prev_level,121) '<dir>'
         qdir(prev_level) = .TRUE.
      END IF  ! (level.gt.prev_level)
C if (level.le.prev_level) then
      refn = refn + 1
      WRITE (ref,241) refn
 241  FORMAT ('Ref',i4.4)
C
      IF ( qpre ) THEN
         WRITE (10+level,121) '</pre>'
         qpre = .FALSE.
      END IF
C Write keyword in new file as heading and name
      WRITE (10+level,121) '<h2><a name="' // ref // '">' // word(1:wn)
     &                   // '</a></h2>'
C Write keyword in parent file as href and list item
      WRITE (10+level-1,121) '<li><a href="' // filenm(1:fln) // '#' //
     &                    ref // '">' // word(1:wn) // '</a>'
C Write in list of references
      WRITE (2,121) '<a href="' // filenm(1:fln) // '#' //
     &                       ref // '">' // word(1:wn) // '</a>'
      prev_level = level
      GOTO 200

 300  IF ( level.LT.0 ) GOTO 200
C Support for pre-conditioning of HLP file formatting
      IF ( (INDEX(line,'<pre>').GT.0 .OR. INDEX(line,'<PRE>').GT.0) .OR.
     &     (INDEX(line,'</pre>').GT.0 .OR. INDEX(line,'</PRE>').GT.0)
     &     .OR. (INDEX(line,'&').EQ.0 .AND. INDEX(line,'>').EQ.0 .AND.
     &     INDEX(line,'<').EQ.0) ) THEN
         IF ( .NOT.qpre .AND. pre_flag.EQ.'Y' ) THEN
            WRITE (10+level,121) '<pre>'
            qpre = .TRUE.
         END IF
         WRITE (10+level,121) line(1:nch)
         GOTO 200
      ELSE
C Quote HTML reserved characters
         j = 0
         DO 320 k = 1, nch
            IF ( line(k:k).EQ.'&' ) THEN
               line2(j+1:j+5) = '&amp;'
               j = j + 5
            ELSE IF ( line(k:k).EQ.'<' ) THEN
               line2(j+1:j+5) = '&lt;'
               j = j + 4
            ELSE IF ( line(k:k).EQ.'>' ) THEN
               line2(j+1:j+5) = '&gt;'
               j = j + 4
            ELSE
               j = j + 1
               line2(j:j) = line(k:k)
            END IF
 320     CONTINUE
         IF ( .NOT.qpre .AND. pre_flag.EQ.'Y' ) THEN
            WRITE (10+level,121) '<pre>'
            qpre = .TRUE.
         END IF
         WRITE (10+level,121) line2(1:j)
C         WRITE(*,*) 'Reserved characters quoted, line',nl
         nquotes = nquotes + 1
         GOTO 200
      END IF

 900  CONTINUE
      IF ( qpre ) THEN
         WRITE (10+level,121) '</pre>'
         qpre = .FALSE.
      END IF
      DO 920 k = level, 0, -1
         IF ( qdir(k) ) THEN
            WRITE (10+k,121) '</dir>'
            qdir(k) = .FALSE.
         END IF
         WRITE (10+k,121) '</body>'
C         WRITE (10+k,121) '<!-- Converted from .HLP to .HTML by -->'
C         WRITE (10+k,121) '<!-- <a href='//
C     & '"ftp://sundae.triumf.ca/pub2/hlptohtml/hlptohtml.html">'//
C     & 'hlptohtml</a>. -->'
         WRITE (10+k,121) '</html>'
C Close all intermediate level files
         CLOSE (UNIT=10+k)
         PRINT *, 'Closing unit ', 10 + k
 920  CONTINUE
      PRINT *, 'HTML reserved characters were quoted on ', nquotes,
     &      ' lines'
      CLOSE (2)
      CLOSE (1)
      END

      SUBROUTINE SQUASH(Word,Nc)
C Remove spaces from char. string
      CHARACTER*(*) Word
      INTEGER Nc

      INTEGER ITAB
      PARAMETER (ITAB=9)

      character(132) cooked
      INTEGER nr, k, j

      j = 0
      nr = LEN(Word)
      DO 110 k = 1, nr
         IF ( Word(k:k).NE.' ' .AND. Word(k:k).NE.CHAR(ITAB) ) THEN
            j = j + 1
            cooked(j:j) = Word(k:k)
         END IF
 110  CONTINUE
      Nc = j
      IF ( Nc.GT.0 ) Word = cooked(1:Nc)
      RETURN
      END

      SUBROUTINE OPHTML(Lun, Cfile, Lfile, Ctitle, Ltitle)
      INTEGER   Lun, Lfile, Ltitle
      CHARACTER Cfile*(*), Ctitle*(*)
C---
C Open HTML output file and write standard header.
C---
   11 FORMAT(A)
C
      WRITE(*,*) 'Creating file ', Cfile(1:Lfile)
      OPEN (UNIT=lun,FILE=Cfile(:Lfile),STATUS='new')
C     &   CARRIAGECONTROL='list')
C Write header for new file
      WRITE (lun,11) '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">'
      WRITE (lun,11) '<html>'
      WRITE (lun,11) '<head>'
C Put in a fake LINK to make htmlcheck happy (and alert user that this
C statement is strongly suggested).
      WRITE (lun,11) '<LINK REV="made" HREF="mailto:root">'
      WRITE (lun,101) '<title>',Ctitle(1:Ltitle),'</title>'
  101 FORMAT(A,A,A)
      WRITE (lun,11) '</head>'
      WRITE (lun,11) '<body>'
      WRITE (lun,101) '<H1>',Ctitle(1:Ltitle),'</H1>'

      RETURN
      END
