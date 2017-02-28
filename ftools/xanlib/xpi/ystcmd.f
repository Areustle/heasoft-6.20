**==ystcmd.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* $Id: ystcmd.f,v 3.13 2015/06/11 21:00:26 kaa Exp $
* $Log: ystcmd.f,v $
* Revision 3.13  2015/06/11 21:00:26  kaa
* Fixed possible string truncation.
*
* Revision 3.12  2015/06/11 20:02:35  kaa
* Added the history command to the set of standard gtcom2 commands. This toggles
* writing the history file on and off. The immediate use is in xselect to avoid
* problems when running multiple instances of the program.
*
* Note that adding a new command required putting it in lists in both ldcmds1.f
* and xpitbldcm.f - one of these ought to be redundant.
*
* Tidied up a bunch of compiler warnings.
*
* Revision 3.11  2013/05/21 19:08:47  irby
* Change character*n to character(n) to silence warnings: "Obsolescent
* feature: Old-style character length".
*
* Revision 3.10  2004/08/02 19:45:02  irby
* Fix spelling error in output text.
*
* Revision 3.9  2004/08/02 19:41:49  irby
* Fix spelling error in output text.
*
* Revision 3.8  1999/02/02 17:38:41  toliver
* revised internal write statements to eliminate possibility of overflow
*
c Revision 3.7  1998/10/29  16:13:50  peachey
c Changing back to original version using xerror until after Ftools release
c
c Revision 3.5.1.1  1996/04/16  01:39:43  dunfee
c Start of pristine ftools CVS...
c
c Revision 1.3  1995/12/06  16:31:07  oneel
c changed a write to xwrite
c
c Revision 1.2  1995/04/17  17:25:14  oneel
c Changed calls from GMATCH to GMATCHXPI.  GMATCHXPI is case
c insensitive.
c
*
* This is a "too big" routine.  It will access a load o' common blocks in an
* effort to decide if the command just entered is valid.  If it is valid it
* will return true, else it will do it's best to diagnose the problem,
* type a message, and return false.

      FUNCTION YSTCMD()

      IMPLICIT NONE
      LOGICAL YSTCMD

      character(1000) str1 , str2 , str3 , str4 , str5
      character(20) cluster(10)
      INTEGER icluster
      LOGICAL YSTMAT
      LOGICAL goodcmd

      INTEGER i
      INTEGER LENACT
      INTEGER j
      INTEGER npos
      INTEGER k
      INTEGER ipos
      LOGICAL found
      INTEGER TBLFPR
      INTEGER minlen
      INTEGER idx
      character(1000) tmpstr
      LOGICAL fndspc
      INTEGER ii
      INTEGER ind
      INTEGER status

      INCLUDE 'yaccfor.inc'
      INCLUDE 'tbl.inc'

      YSTCMD = .FALSE.


      IF ( TBLstandalone ) THEN
*
* we have a standalone command
*
*
* Are there any blank parameter names?
*
         found = .FALSE.
         DO 50 i = 1 , NPArs
            found = (SVAl(i).NE.' ') .OR. found
 50      CONTINUE
         IF ( found ) THEN
*
* Yes, so clean them up (parameter values look like a=b)
*
            j = 1
            DO 60 i = 1 , NPArs
               idx = INDEX(SVAl(i),'=')
               IF ( idx.NE.0 ) THEN
                  IF ( LEN(SVAl(i)).GT.idx+1 ) THEN
                     IF ( SVAl(i)(idx+1:idx+1).EQ.'=' ) idx = 0
                  ENDIF
               ENDIF
               IF ( idx.NE.0 .AND. SPArs(i).EQ.' ' ) THEN
                  tmpstr = SVAl(i)
                  fndspc = .FALSE.
                  DO 55 ii = 1 , idx
                     IF ( tmpstr(ii:ii).EQ.' ' ) fndspc = .TRUE.
 55               CONTINUE
                  IF ( .NOT.fndspc ) THEN
                     SPArs(i) = tmpstr(1:idx-1)
                     SVAl(i) = tmpstr(idx+1:)
                  ENDIF
               ENDIF
*
* WHile we're at it, fill in the quereied parameter names which are still
* blank
*
               IF ( SPArs(i).EQ.' ' .AND. SVAl(i).NE.' ' ) THEN
                  IF ( INDEX(TBLpupd(j),'h').EQ.0 ) THEN
                     SPArs(i) = TBLpname(j)
                     j = j + 1
                  ELSE
                     DO WHILE ( j.LE.TBLpcnt .AND. INDEX(TBLpupd(j),'h')
     &                          .NE.0 )
                        IF ( INDEX(TBLpupd(j),'h').NE.0 ) j = j + 1
                     ENDDO
                     IF ( j.LE.TBLpcnt ) THEN
                        SPArs(i) = TBLpname(j)
                        j = j + 1
                     ENDIF
                  ENDIF
               ENDIF
 60         CONTINUE
         ENDIF

* Do we still have blank ones?

         found = .FALSE.
         DO 100 i = 1 , NPArs
            IF ( SVAl(i).NE.' ' .AND. SPArs(i).EQ.' ' ) THEN
               found = .TRUE.
               CALL XERROR('Value not assigned to a parameter name: '//
     &                     SVAl(i),5)
            ENDIF
 100     CONTINUE
         IF ( found ) THEN
            YSTCMD = .FALSE.
            RETURN

c$$$* Yes, try the hidden ones
c$$$
c$$$             j = 1
c$$$            DO i = 1 , NPArs
c$$$               IF ( SPArs(i).EQ.' ' .AND. SVAl(i).NE.' ' ) THEN
c$$$                  if (index(tblpupd(j),'h') .ne. 0) then
c$$$                     SPArs(i) = TBLpname(j)
c$$$                     j = j + 1
c$$$                  end if
c$$$               ENDIF
c$$$            end do
         ENDIF


         DO 150 i = 1 , NPArs


* go find the parameter name
            str4 = SPArs(i)
            CALL UPC(str4)
            j = LENACT(str4)
            IF ( str4(j:j).EQ.'+') then
               str4(j:j) = ' '
               sval(i) = 'yes'
            end if
            if (str4(j:j).EQ.'-' ) then
               str4(j:j) = ' '
               sval(i) = 'no'
            end if
            j = 1
            found = .FALSE.
            IF ( str4.EQ.' ' ) found = .TRUE.

* Do the name expansion
            CALL LOCASE(str4)
            CALL GMATCHXPI(TBLpname,TBLpcnt,str4,ind,status)
            CALL UPC(str4)
            IF ( ind.NE.0 ) THEN
               SPArs(i) = TBLpname(ind)
               found = .TRUE.
               CALL YPRINT ('YSTCMD 1')
            ENDIF
c$$$            DO WHILE ( j.LE.TBLpcnt .AND. .NOT.found )
c$$$               str2 = TBLpname(j)
c$$$               CALL UPC(str2)
c$$$               str3 = TBLpname(j)
c$$$               CALL UPC(str3)
c$$$               IF ( str3.EQ.str4 ) THEN
c$$$                  CALL YPRINT ('YSTCMD 2')
c$$$                  found = .TRUE.
c$$$               ENDIF
c$$$               j = j + 1
c$$$            ENDDO

* did we find it?

            IF ( .NOT.found ) THEN
               YSTCMD = .FALSE.
               CALL XWRITE(' Parameter name not found: '//SPArs(i),5)
               CALL YPRINT ('YSTCMD 3')
               RETURN

            ENDIF
 150     CONTINUE

*     Make sure that the user didn't enter two copyies of the same parameter
*
         DO 200 i = 1 , NPArs
            DO 160 j = 1 , NPArs
               IF ( i.NE.j ) THEN
                  str1 = SPArs(i)
                  str2 = SPArs(j)
                  CALL UPC(str1)
                  CALL UPC(str2)
                  IF ( str1.EQ.str2 .AND. str1.NE.' ' ) THEN
                     YSTCMD = .FALSE.
                     CALL XWRITE('Parameter duplicated, '//
     &                           'possibly ambiguous: '//SPArs(i),5)
                     CALL YPRINT ('YSTCMD 4')
                     RETURN
                  ENDIF
               ENDIF
 160        CONTINUE
 200     CONTINUE

         YSTCMD = .TRUE.
         CALL YPRINT ('YSTCMD 4')
         RETURN
      ENDIF

      DO 300 i = 1 , 10
         cluster(i) = ' '
 300  CONTINUE
      icluster = 0
* Get the command
      str1 = SCOm

* Now step through all the valid commands and see if we have a valid command
* this really needs to be expanded to deal with aliases, command classes
* and alias expansion.
*
      CALL UPC(str1)
      goodcmd = .FALSE.
      i = 1

* go find the command

      goodcmd = str1.EQ.'?' .OR. str1.EQ.'??'

      DO WHILE ( i.LE.TBLccnt .AND. .NOT.goodcmd )
         str2 = TBLcname(i)
         CALL UPC(str2)
         goodcmd = YSTMAT(str1,str2)
         IF ( goodcmd ) SCOm = str2
         i = i + 1
      ENDDO

* did we find the command?

      IF ( .NOT.goodcmd ) THEN
         i = 1
         DO WHILE ( i.LE.TBLwcnt .AND. .NOT.goodcmd )
            str2 = TBLwname(i)
            CALL UPC(str2)
            goodcmd = YSTMAT(str1,str2)
            IF ( goodcmd ) SCOm = str2
            i = i + 1
         ENDDO
         IF ( .NOT.goodcmd ) THEN
C            CALL XWRITE(' YY-E Unknown Command',5)
            YSTCMD = .FALSE.
            CALL YPRINT ('YSTCMD 5')
            RETURN
         ENDIF
      ENDIF

* clean up the number of parameters

      i = 1
      DO WHILE ( i.LE.NPArs )
         IF ( SPArs(i).EQ.' ' .AND. SVAl(i).EQ.' ' ) THEN
            DO 320 j = i , NPArs - 1
               SPArs(j) = SPArs(j+1)
               SVAl(j) = SVAl(j+1)
 320        CONTINUE
            NPArs = NPArs - 1
         ENDIF
         i = i + 1
      ENDDO
*


* Now do parameters
*
*
* Convert the positional parameters to named parameters
*
      npos = 1
      i = 1
      DO WHILE ( i.LE.NPArs )

         IF ( SPArs(i).EQ.' ' ) THEN
*
* We have a positional parameter
*
            j = 1
            found = .FALSE.

* go find the parameter name

            DO WHILE ( j.LE.TBLkcnt .AND. .NOT.found )
               ipos = TBLkposo(j)
               str2 = TBLkname(j)
               CALL UPC(str2)
               str1 = SCOm
               CALL UPC(str1)
               IF ( npos.EQ.ipos .AND. str1.EQ.str2 ) THEN
                  SPArs(i) = TBLkparm(j)
                  found = .TRUE.
                  CALL YPRINT ('YSTCMD 6')
               ENDIF
               j = j + 1
            ENDDO

* did we find it?

            IF ( .NOT.found ) THEN
               YSTCMD = .FALSE.
               str5 = ' Positional parameter for ' //
     &                SVAl(i)(1:LENACT(SVAl(i))) // ' not found '
               CALL XWRITE(str5,5)
               CALL YPRINT ('YSTCMD 7')
               RETURN
            ENDIF
            npos = npos + 1
         ENDIF


* first find the parameter value
         j = TBLFPR(SPArs(i))
         IF ( j.NE.0 ) THEN
            IF ( TBLptype(j).EQ.'g' ) CALL YCMPRG(i)
         ENDIF

         i = i + 1

      ENDDO


* now, check the validity of the parameter names


      DO 400 i = 1 , NPArs


* go find the parameter name
         str1 = SCOm
         CALL UPC(str1)

         str4 = SPArs(i)
         CALL UPC(str4)
         j = LENACT(str4)
         IF ( str4(j:j).EQ.'+' .OR. str4(j:j).EQ.'-' ) str4(j:j) = ' '

         j = 1
         found = .FALSE.

         DO WHILE ( j.LE.TBLkcnt .AND. .NOT.found )
            str2 = TBLkname(j)
            CALL UPC(str2)
            str3 = TBLkparm(j)
            CALL UPC(str3)
            minlen = MIN(LENACT(str3),LENACT(str4))
            IF ( str3(1:minlen).EQ.str4(1:minlen) .AND. str1.EQ.str2 )
     &           THEN
               IF ( SPArs(i).NE.' ' .AND. LENACT(SPArs(i))
     &              .LT.LENACT(TBLkparm(j)) ) THEN
                  IF ( found ) THEN
                     CALL XWRITE(' Parameter ambiguous: '//SPArs(i),5)
                     YSTCMD = .FALSE.
                  ENDIF
                  SPArs(i) = TBLkparm(j)
               ENDIF
               CALL YPRINT ('YSTCMD 8')
               found = .TRUE.
            ENDIF
            j = j + 1
         ENDDO

* did we find it?

         IF ( .NOT.found ) THEN
            YSTCMD = .FALSE.
            CALL XWRITE(' Parameter name not found: '//SPArs(i),5)
            CALL YPRINT ('YSTCMD 8')
            RETURN

         ENDIF
 400  CONTINUE

      CALL YPRINT ('YSTCMD 9')

* Check to see if all the parameters are in the same cluster


*     Make sure that the user didn't enter two copyies of the same parameter
*
      DO 500 i = 1 , NPArs
         DO 450 j = 1 , NPArs
            IF ( i.NE.j ) THEN
               str1 = SPArs(i)
               str2 = SPArs(j)
               CALL UPC(str1)
               CALL UPC(str2)
               IF ( str1.EQ.str2 ) THEN
                  YSTCMD = .FALSE.
                  CALL XWRITE('Parameter duplicated, '//
     &                        'possibly ambiguous: '//SPArs(i),5)
                  CALL YPRINT ('YSTCMD 10')
                  RETURN
               ENDIF
            ENDIF
 450     CONTINUE
 500  CONTINUE

* Get a list of all the clusters possible
*
      DO 600 i = 1 , NPArs
         str1 = SPArs(i)
         j = LENACT(str1)
         IF ( str1(j:j).EQ.'+' .OR. str1(j:j).EQ.'-' ) str1(j:j) = ' '
         str2 = SCOm
         CALL UPC(str1)
         CALL UPC(str2)
         DO 550 j = 1 , TBLkcnt
            str3 = TBLkparm(j)
            str4 = TBLkname(j)
            CALL UPC(str3)
            CALL UPC(str4)
            IF ( str1.EQ.str3 .AND. str2.EQ.str4 ) THEN
               found = .FALSE.
               DO 510 k = 1 , icluster
                  IF ( cluster(k).EQ.TBLkclus(j) ) found = .TRUE.
 510           CONTINUE
               IF ( .NOT.found ) THEN
                  icluster = icluster + 1
                  cluster(icluster) = TBLkclus(j)
               ENDIF
            ENDIF
 550     CONTINUE
 600  CONTINUE

      DO 700 i = 1 , icluster
*         call xwrite(XWRIT cluster ' , i , cluster(i)
 700  CONTINUE

      YSTCMD = .TRUE.

      CALL YPRINT ('YSTCMD 11')
      RETURN
      END
**==xpirange.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994


      SUBROUTINE XPIRANGE(Entered,Low,High,Parameter,Status)

      CHARACTER*(*) Entered , Low , High , Parameter
      INTEGER Status , LENACT , ierr

      character(80) dsnafu
      REAL FPNUM , rentered , rlow , rhigh

      Status = 0

      IF ( Entered.EQ.' ' .OR. Low.EQ.' ' .OR. High.EQ.' ' ) RETURN

      Status = 1
      dsnafu = Entered
      rentered = FPNUM(dsnafu,LEN(dsnafu),ierr)
      IF ( ierr.NE.0 ) THEN
         dsnafu = 'Parameter '//Parameter(1:LENACT(Parameter))
     &            //' has an invalid default value of '//
     &            Entered(1:LENACT(Parameter))
         CALL XERROR(dsnafu,5)
         RETURN
      ENDIF
      dsnafu = Low
      rlow = FPNUM(dsnafu,LEN(dsnafu),ierr)
      IF ( ierr.NE.0 ) THEN
         dsnafu = 'Parameter '//Parameter(1:LENACT(Parameter))
     &            //' has an invalid low bounds value of '//
     &            Low(1:LENACT(Low))
         CALL XERROR(dsnafu,5)
         RETURN
      ENDIF
      dsnafu = High
      rhigh = FPNUM(dsnafu,LEN(dsnafu),ierr)
      IF ( ierr.NE.0 ) THEN
         dsnafu = 'Parameter '//Parameter(1:LENACT(Parameter))
     &            //' has an invalid high bounds value of '//
     &            High(1:LENACT(High))
         CALL XERROR(dsnafu,5)
         RETURN
      ENDIF

      IF ( rlow.LE.rentered .AND. rhigh.GE.rentered ) THEN
         Status = 0
         RETURN
      ENDIF

      dsnafu = 'Parameter '//Parameter(1:LENACT(Parameter))
     &         //' has a value of '//Entered(1:LENACT(Entered))
     &         //' which is out of the bounds of '//Low(1:LENACT(Low))
     &         //' '//High(1:LENACT(High))
      CALL XERROR(dsnafu,5)
      RETURN

      END


**==GMATCH.spg  processed by SPAG 3.09I  at 09:43 on 20 Aug 1992
*- get_match - verify answers
      SUBROUTINE GMATCHXPI(List,N,Value,Index,Status)
* changed to be case insensitive
* Description :
*  checks if a string appears in a list
* History :
*  1 July 1988 : original
*  8 November 1988 : put index=0 for blank input value
*  17 November 1988 : remove lower case conversion
*  2 February 1989 : check for exact and abbreviated match
*  2 March 1989 : status=error__ changed to warning__ for ambiguity
*  25 June 1991 : renamed to gmatch
*  8  July 1991 : set index = 0 if no match found
* Author :
*  Andy Pollock (EXOSAT::ANDY)

      INCLUDE 'status.codes'
* Import :
      CHARACTER*(*) List(*)
      INTEGER N
      CHARACTER*(*) Value
* Export :
      INTEGER Index
* Status :
      INTEGER Status
* Local variable :
      character(80) x , y , NOSTAR
      INTEGER lx
      INTEGER nin
      INTEGER j
* External reference :
      INTEGER LENACT
*-
      Status = 0
      x = Value
      call locase(x)
* find the non-blank length of the string
      lx = LENACT(x)
      IF ( lx.EQ.0 ) THEN
         Index = 0
      ELSE
         nin = 0
* check first for a exact match or exact match up to '*'
         DO 50 j = 1 , N
            y = NOSTAR(List(j))
            call locase(y)
            IF ( (y.EQ.x) .OR.
     &           ((List(j)(:lx).EQ.x(:lx)) .AND. List(j)(lx+1:lx+1)
     &           .EQ.'*') ) THEN
               nin = nin + 1
               x = y
               Index = j
            ENDIF
 50      CONTINUE
* if no precise match found check for abbreviated match
         IF ( nin.EQ.0 ) THEN
            DO 60 j = 1 , N
               y = NOSTAR(List(j))
               IF ( y(:lx).EQ.x(:lx) ) THEN
                  nin = nin + 1
                  x = y
                  Index = j
               ENDIF
 60         CONTINUE
         ENDIF
         IF ( (Status.EQ.OK__) .AND. (nin.EQ.0) ) THEN
            Status = ERROR__
            Index = 0
         ELSEIF ( (Status.EQ.OK__) .AND. (nin.EQ.1) ) THEN
            Value = x
         ELSEIF ( (Status.EQ.OK__) .AND. (nin.GT.1) ) THEN
            x = Value(1:lx) // ' is ambiguous'
            lx = LENACT(x)
            call xWRITE (' '// x(1:lx),5)
            Status = WARNING__
         ENDIF
      ENDIF

      RETURN

      END
