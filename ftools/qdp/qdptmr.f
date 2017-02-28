C--- Program QDPTMR
C--- Converts a QDP file to TMR
C---
C- [AFT]
C---
      REAL       NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   MXCOL, MXVEC, MXPTS, MXCMD
      PARAMETER (MXCOL=10, MXVEC=10)
      PARAMETER (MXPTS=524288)
      PARAMETER (MXCMD=16)
      INTEGER   IOFSET
C-
      CHARACTER cnam*256
      CHARACTER cprom*72
      CHARACTER cmd(MXCMD)*80
      CHARACTER chis(1)*80
      CHARACTER cunit(MXCOL)*16, cname(MXCOL)*16
      DOUBLE PRECISION days, dtb, dts
      REAL      bscal(MXCOL)
      REAL      xc(2)
      REAL      ydat(MXPTS), yt(MXPTS,2)
      REAL      renorm, rtim, tbin, time, tmp
      INTEGER   iery(MXVEC)
      INTEGER   i, ibin, ichat, ier, ioff, itmp, ivec, ixoff0, iyoff
      INTEGER   lery, lun, luntmr
      INTEGER   ncmd, ndim, nfield, npts, nrow, ntot, nvec
C-
      DATA iery/MXVEC*0/,ivec/1/
      DATA dts,dtb/0.,0./
      DATA bscal/MXCOL*1.0/
C---
      DO i=1,MXPTS
         yt(i,1)=NO
      END DO
      ntot=0
C---
  100 lun=0
      cnam=' '
      ichat=0
      CALL RDQDP(ichat, lun, cnam, ydat, MXPTS, iery, MXVEC, nrow,
     :    npts, nvec, cmd, MXCMD, ncmd, ier)
      IF(ier.NE.0) GOTO 800
  120 WRITE(cprom,121) ivec
  121 FORMAT('ivec(',I4,')?')
      CALL GTBUF(cprom,ier)
      IF(ier.LT.0) GOTO 100
      CALL GTINT(ivec,ier)
      IF(ivec.LE.0 .OR. ivec.GT.nvec) THEN
         WRITE(*,*) 'Must be in range 1 to ',nvec
         GOTO 120
      END IF
      renorm=1.0
  150 WRITE(cprom,151) renorm
  151 FORMAT('Multiply rate by (',F9.3,')?')
      CALL GTBUF(cprom,ier)
      IF(ier.LT.0) GOTO 120
      CALL GTREAL(renorm,ier)
C---
      lery=iery(ivec)
      CALL INITX(cmd,ncmd,nrow,ixoff0)
      IF(dtb.LE.0.) THEN
C- Find dtb by looking at first 10 data points for the minimum
C- separation.  This allows a point to be missing.
         tbin=1.E32
C         dts=FNX(ydat,1,ixoff0)
         CALL PLTXCC(ydat, 1, 1, xc, ndim, iyoff)
         dts = xc(1)
         DO i=1,10
C            rtim=FNX(ydat,i,ixoff0)
            CALL PLTXCC(ydat, i, 1, xc, ndim, iyoff)
            rtim = xc(1)
C            tmp=FNX(ydat,i+1,ixoff0)-rtim
            CALL PLTXCC(ydat, i+1, 1, xc, ndim, iyoff)
            tmp = xc(1) - rtim
            tbin=MIN(tbin,tmp)
         END DO
C- tmp is the approximate number of bins beteen data points 1 and 10,
C- itmp is the actual number, and dtb is the average bin size.
C         rtim=FNX(ydat,10,ixoff0)-dts
         CALL PLTXCC(ydat, 10, 1, xc, ndim, iyoff)
         rtim = xc(1) - dts
         tmp=rtim/tbin
         itmp=NINT(tmp)
         dtb=rtim/itmp
         WRITE(*,*) 'dts, dtb=',dts,dtb
      END IF
C---
      ioff=IOFSET(ivec,iery,nvec,nrow)
      DO i=1,npts
C         time=FNX(ydat,i,ixoff0)
         CALL PLTXCC(ydat, i, 1, xc, ndim, iyoff)
         time = xc(1)
         ibin=NINT((time-dts)/dtb+1.)
         IF(ibin.GT.MXPTS) THEN
            WRITE(*,171)
  171       FORMAT(' Array overflow.')
            GOTO 100
         END IF
         IF(ydat(ioff+i).NE.NO) THEN
            yt(ibin,1)=renorm*ydat(ioff+i)
            ntot=MAX(ntot,ibin)
            IF(lery.GT.0) yt(ibin,2)=renorm*ydat(ioff+nrow+i)
         END IF
      END DO
      GOTO 100
C---
C- Write out TMR file.
  800 IF(ntot.LE.0) GOTO 900
      nvec=1
C- Note, in QDP files dts is center of bin whereas in TMR
C- files dts is at beginning of bin.
      dts=dts-dtb/2.
C
C Write optional TMR file.
      CALL GETLUN(luntmr)
      CALL TMRNEW(luntmr, chis, 0, ier)
      IF ( ier.EQ.0 ) THEN
         nfield = 1
         cname(1) = 'SRATE'
         cunit(1) = 'cts/sec'
         days = 0.0d0
         CALL TMRWR(luntmr, 1, yt, MXPTS, ntot, nfield,
     &       cname, cunit, bscal, days, dts, dtb, ier)
      END IF
      CALL FRELUN(luntmr)
C---
  900 CONTINUE
      CALL EDICOM('OFF',3)
      END
