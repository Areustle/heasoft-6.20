C Program QDPFI
C Reads QDP file and outputs a FITS image.
C Assumes that that data makes up a 2D image only.
C---
C [AFT]
C---
      INTEGER   MXPTS, MXVEC, MXCMD
      PARAMETER (MXPTS=524288)
      PARAMETER (MXVEC=512)
      PARAMETER (MXCMD=200)
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
      CALL WRMAP(4, ydat, nrow, npts, nvec, ' ', 0, ier)
C---
  900 CONTINUE
      CALL EDICOM('off',3)
      END
