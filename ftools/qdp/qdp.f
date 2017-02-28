C Program QDP, the Quick and Dandy Plotter.
C Reads and plots a QDP file.
C---
C [AFT]
C---
      INTEGER   MXPTS, MXVEC, MXCMD
      PARAMETER (MXPTS=2097152)
      PARAMETER (MXVEC=1024)
      PARAMETER (MXCMD=1000)
C
      CHARACTER cmd(MXCMD)*100
      CHARACTER cnam*256
      REAL      ydat(MXPTS)
      INTEGER   iery(MXVEC)
      INTEGER   ichat, ier, lun, ncmd, npts, nrow, nvec
C---
  100 CONTINUE
      cnam=' '
      ichat=0
      lun=0
      CALL RDQDP(ichat, lun, cnam, ydat, MXPTS, iery, MXVEC,
     :   nrow, npts, nvec, cmd, MXCMD, ncmd, ier)
      IF ( ier.NE.0 ) GOTO 900
      IF ( npts.LE.0 ) THEN
         WRITE(*,*) 'No data found.'
         GOTO 900
      END IF
      CALL PLT(ydat,iery,nrow,npts,nvec,cmd,ncmd,ier)
      IF ( ier.LT.0 ) GOTO 100
C---
  900 CONTINUE
      CALL EDICOM('off',3)
      END
