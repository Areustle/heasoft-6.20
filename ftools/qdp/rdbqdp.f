C RDBQDP read an RDB file and send to PLT
C---
C 1997-01-07 - [AFT]
C---
      INTEGER   MXCMD, MXCOL, MXPTS
      PARAMETER (MXCMD=67, MXCOL=64, MXPTS=131072)
      REAL      FPNUM
      INTEGER   LENACT
C
      CHARACTER cmd(MXCMD)*80
      CHARACTER cbuf*512, clast*512
      CHARACTER cnam*256, ctmp*64
      REAL      aray(MXPTS,MXCOL)
      INTEGER   iery(MXCOL)
      INTEGER   i, icol, ier, ilab, ios, istate, kp
      INTEGER   lbuf, lcmd, lnam, ltmp, ncmd, npts, nvec
C---
   11 FORMAT(A)
C---
  100 CONTINUE
      CALL GTBUF('Input file name:', ier)
      IF ( ier.NE.0 ) GOTO 900
      CALL GTCHAR(cnam, lnam)
      CALL OPENWR(1,cnam,'OLD',' ',' ',0,1,ier)
      IF ( ier.NE.0 ) GOTO 100
C
      cmd(1) = 'LAB F  '//cnam
      npts = 0
      istate = 0
      ilab = 0
  200 CONTINUE
      READ(1,11,IOSTAT=ios) cbuf
      IF ( ios.EQ.0 ) THEN
         lbuf = LENACT(cbuf)
         IF ( cbuf(1:1).EQ.'#' ) THEN
            clast = cbuf(2:)
C Comment line.  MeasureType should contain TRWID
            IF ( INDEX(cbuf, 'measureType').GT.0 .AND.
     &            NCMD.LT.MXCMD ) THEN
               ncmd = ncmd + 1
               kp = INDEX(cbuf,':')
               cmd(ncmd) = 'LAB T   '//cbuf(kp+1:)
            END IF
         ELSE
C not a comment.  Blank lines are skipped and start data.
            IF ( lbuf.LE.0 ) istate = 1
            IF ( istate.EQ.0 ) THEN
C first non-comment line contains keywords
               istate = 1
               CALL MKLABS(cbuf, lbuf, mxcmd, ncmd, cmd)
               ilab = 1
            ELSE IF ( istate.EQ.1 ) THEN
C second non-comment line is skipped
               istate = 2
            ELSE
C read data from rest of file
               IF ( npts.LT.MXPTS) THEN
                  npts = npts + 1
               ELSE
                  WRITE(*,*) 'Too many points.  Increase MXPTS!'
               END IF
               icol = 0
               kp = 0
  280          CONTINUE
               CALL ALF(cbuf, lbuf, kp, ctmp, ltmp)
               IF ( ltmp.NE.0 ) THEN
                  IF ( icol.LT.MXCOL ) THEN
                     icol = icol + 1
                     aray(npts, icol) = FPNUM(ctmp, ltmp, ier)
                     GOTO 280
                  ELSE
                     WRITE(*,*) 'Too many columns.  Increase MXCOL.'
                     STOP
                  END IF
               END IF
            END IF
         END IF
         GOTO 200
      END IF
C
      IF ( ilab.LE.0 ) THEN
         lbuf = LENACT(clast)
         CALL MKLABS(clast, lbuf, mxcmd, ncmd, cmd)
      END IF
      nvec = icol
      write(*,*) 'nvec=',nvec
      write(*,*) 'npts=',npts
      IF ( ncmd+3.LE.MXCMD ) THEN
         cmd(ncmd+1) = 'XAX lin 1 1'
         cmd(ncmd+2) = 'PLOT V'
         cmd(ncmd+3) = 'LAB X Line'
         ncmd = ncmd + 3
      END IF
      DO i=1,nvec
         iery(i) = 0
      END DO
      CALL PLT(aray,iery,MXPTS,npts,nvec,cmd,ncmd,ier)
      IF(IER.LT.0) GOTO 100
C---
  900 CONTINUE
      CTMP = 'OFF'
      CALL EDICOM(CTMP,3)
      END
C*********
      SUBROUTINE MKLABS(cbuf, lbuf, mxcmd, ncmd, cmd)
      INTEGER   lbuf, mxcmd, ncmd
      CHARACTER cbuf*(*), cmd(MXCMD)*(*)
C---
      CHARACTER ctmp*64
      INTEGER   kp, lcmd, ltmp
C---
      kp = 0
  240 CONTINUE
      CALL ALF(cbuf, lbuf, kp, ctmp, ltmp)
      IF ( ltmp.NE.0 ) THEN
         IF ( ncmd.LT.MXCMD ) THEN
            ncmd = ncmd + 1
            cmd(ncmd) = 'LAB G'
            lcmd = 5
            CALL CRAMI(ncmd, cmd(ncmd), lcmd)
            cmd(ncmd)(lcmd+2:) = ctmp(:ltmp)
         END IF
         GOTO 240
      END IF
C---
      RETURN
      END
