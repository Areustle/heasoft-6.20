C---
C This program reads of list of PLT *.mod file names and produces
C a .qdp file that plots parameter values vs iteration number.
C After Ndigits user can enter N, S, or NS.  N for no errors and S to
C skip frozen parameters.
C---
C 1997-02-13 - Read a series of *.MOD files [AFT]
C 1996-08-29 - Based on xlgqdp [AFT]
C---
      REAL      NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   MXPAR
      PARAMETER (MXPAR=256)
C
      REAL FPNUM
      INTEGER LENACT
C
      CHARACTER cbuf*132
      CHARACTER cnam*64, cout*64, ctok*64, ctmp*64
      REAL      pval(2,MXPAR)
      REAL      xerr, xval
      INTEGER   iery(100)
      INTEGER   i, idoer, ier, ifirst, ios
      INTEGER   ip, iparm, irun, iskip, istate, ixflag
      INTEGER   k, kp, lbuf, lnam, ltmp, ltok, ndig, nparm
C---
 11   FORMAT (A)
C---
      ndig = -2
      idoer = 1
      iskip = 0
      CALL GTBUF('Output file name, [Ndigits(-2)]:',ier)
      IF ( ier.NE.0 ) GOTO 900
      CALL GTCHAR(cnam,lnam)
      CALL GTINT(ndig, ier)
      CALL GTCHAR(ctmp,ltmp)
      CALL UPC(ctmp)
      IF ( ltmp.GT.0 .AND. INDEX(ctmp,'N').GT.0 ) idoer=0
      IF ( ltmp.GT.0 .AND. INDEX(ctmp,'S').GT.0 ) iskip=1
C
      CALL XTEND(cnam,'qdp')
      CALL OPENWR(2,cnam,'NEW',' ','L',0,0,ier)
      cout = cnam
      ifirst = 1
      irun = 0
C
 120  CONTINUE
      CALL GTBUF('Model file:',ier)
      CALL GTCHAR(cnam,lnam)
      IF ( lnam.LE.0 ) GOTO 900
      CALL XTEND(cnam,'mod')
      CALL OPENWR(1,cnam,'OLD',' ',' ',0,1,ier)
      IF ( ier.NE.0 ) THEN
         WRITE(*,*) 'Unable to open ',cnam(:LENACT(cnam))
         GOTO 120
      END IF
      ixflag = 0
      CALL GTPEEK(ctmp, ltmp)
      IF ( ltmp.GT.0 ) THEN
         ixflag = 1
         CALL GTREAL(xval, ier)
         xerr = .1
         CALL GTREAL(xerr, ier)
      END IF
C---
      istate = 0
  150 CONTINUE
      READ(1,11,IOSTAT=ios) cbuf
      IF ( ios.NE.0 ) THEN
         CLOSE (UNIT=1)
         IF ( ifirst.NE.0 ) THEN
            ifirst = 0
            nparm = iparm
            DO i=1,nparm-1
               iery(i) = 1
            END DO
            iery(nparm) = 0
            IF ( idoer.NE.0 ) THEN
               WRITE(2,161) (k,k=1,nparm-1)
 161           FORMAT('READ SERR ',20I3)
               CALL XTEND(cout,'.pco')
               WRITE(2,11) '@'//cout(:LENACT(cout))
               WRITE(2,11) '!'
            END IF
         END IF
         IF ( idoer.NE.0 ) THEN
            WRITE(2,11) '! '//cnam(:lnam)
            CALL WRQDAT(2, ndig, pval, iery, 1, nparm)
         ELSE
            WRITE(2,171) (pval(1,k),CHAR(9),k=1,nparm)
  171       FORMAT(1X,100(1pG12.5,A1))
         END IF
         GOTO 120
      END IF
      lbuf = LENACT(cbuf)
C
  190 CONTINUE
      IF ( istate.EQ.0 ) THEN
C Search for FILE= line
         ip = INDEX(cbuf,'FILE=')
         IF ( ip.GT.0 ) THEN
            istate = 1
            cnam = cbuf(ip:)
            lnam = LENACT(cnam)
         ELSE
            istate = 2
         END IF
C First parameter value is either the number we read on the command line
C or the run number.
         irun = irun + 1
         iparm = 1
         IF ( ixflag.EQ.0 ) THEN
            pval(1,iparm) = irun
            pval(2,iparm) = 0.5
         ELSE
            pval(1,iparm) = xval
            pval(2,iparm) = xerr
         END IF
      ELSE IF ( istate.EQ.1 ) THEN
C Skip model definition line
         istate = 2
      ELSE IF ( istate.EQ.2 ) THEN
C Read parameter values
         ip = INDEX(cbuf,'WVAR=')
         IF ( ip.GT.0 ) THEN
            istate = 3
            IF ( iparm.LT.MXPAR ) THEN
               iparm = iparm + 1
               kp = ip + 4
               CALL ALFSKS(cbuf,lbuf,kp)
               CALL ALF(cbuf,lbuf,kp,ctok,ltok)
               pval(1,iparm) = FPNUM(ctok,ltok,ier)
            END IF
         ELSE
C Leave room for chi^2
            IF ( iparm.LT.MXPAR-1 ) iparm = iparm + 1
            kp = 0
            CALL ALFSKS(cbuf,lbuf,kp)
            CALL ALF(cbuf,lbuf,kp,ctok,ltok)
            pval(1,iparm) = FPNUM(ctok,ltok,ier)
            CALL ALF(cbuf,lbuf,kp,ctok,ltok)
            pval(2,iparm) = FPNUM(ctok,ltok,ier)
            IF ( pval(2,iparm).LT.0.0 .AND. iskip.NE.0 ) THEN
C If iskip.NE.0 then ignore frozen parameter.
               iparm = iparm - 1
            END IF
         END IF
      ELSE IF ( istate.EQ.3 ) THEN
C Continue until either EOF or next FILE= statement
         ip = INDEX(cbuf,'FILE=')
         IF ( ip.GT.0 ) THEN
            istate = 0
            GOTO 190
         END IF
      END IF
      GOTO 150
C---
 900  CONTINUE
      CALL EDICOM('OFF',3)
      CLOSE (UNIT=2)
      END
