CH    SUBROUTINE XDSC5.FTN         FROM PDP 11/70 RD1:[300,147]XDSC5.FTN;1
CH                                 TO MICROVAX 3/26/88
CCCC  XDSC5.FTN
CCCC  ALTERED TO CORRECT ATTITUDE TIMES 9/13/79
      SUBROUTINE DSC5(lcunit, NT,HDET,HD,BKGD,NB,BER,DB,CHI,
     + IDAY , IMSEC,HFLAG2, ANGLIM, QOK, QANG,
     + SPRA , SPDEC , YRA , YDEC , HDSC, SRA,SDEC,row,
     + status)
CCCCC THIS VERSION FOR TESTING IF POINT DATA HAS ANY NEARBY SOURCE
C     THIS IS SUBROUTINE FOR XRATE, USES 5.12 S DISC SCALARS
CH    11 Aug 1997  Altered to give ASCII output to unit 3 (Jesse Allen)
C                   and to give time at start of record, not middle
C     11 Nov 1997  FITS light curve replaces ASCII output
C     23 Jan 1998  Implicit typing removed
C     26 Jan 1998  Handles collimator correction coefficients instead of 
C                  effective cross sectional areas
C     19 Oct 1998  Handle just 1 lightcurve instead of up to 4 (L.Breedon)
C     26 Oct 1998  Removed XRREC common block and associated unused variables
C     26 Oct 1998  Removed ERROR common block and associated unused variables..
C     26 Oct 1998  Parse ANGLIM parameter (instead of via common)
C     26 Oct 1998  Removed FLAG common block and associated unused variables..

      implicit none
 

      integer*2 hflag2(4), hdsc(5,8,8,6)
      integer iday, imsec
      real spra(32), spdec(32), yra(32), ydec(32)
      real ber, db, anglim(2)



      logical qok(4), qang
      character(40) taskname
      common /TASK/ taskname

C Local variables

      logical qang5

      
      integer*2 hdet, hd(4), hdst, hhh, hsign

      integer nt, lcunit, row, status, nb
      integer idayd, imsecd, mft, isday
      integer ic2, j, k, kk, js,jfd
c      integer jd
c      real dtr
      real eff, avgeff
      real sra, sdec, yrai, ydeci, zrai, zdeci
      real bkgd,  chi, src, err, dev, sig2

      character(80) message

c      DATA ISDAY/86400000/,MFT/320/,DTR/.0174532925D0/
      DATA ISDAY/86400000/,MFT/320/

c  Initialize to avoid warning
      QANG5 = .FALSE.
      AVGEFF = 0.
c  --

      IDAYD=IDAY
      IMSECD=IMSEC
      IC2=1
      DO 30 J=1,8
         IMSECD=IMSECD+16*MFT
         IF(IMSECD.LT.ISDAY) GO TO 50
         IMSECD=IMSECD-ISDAY
         IDAYD=IDAYD+1
 50      CONTINUE
         DO 31 K=1,4
            IF(K.GT.1)GO TO 52
            DO 51 KK=1,1
               QANG5=.TRUE.
 51         continue
 52         CONTINUE
            IF(IC2.GT.1) GO TO 41
            YRAI=1.25*YRA(IC2)-.25*YRA(IC2+1)
            YDECI=1.25*YDEC(IC2)-.25*YDEC(IC2+1)
            ZRAI=1.25*SPRA(IC2)-.25*SPRA(IC2+1)
            ZDECI=1.25*SPDEC(IC2)-.25*SPDEC(IC2+1)
            GO TO 42
 41         YRAI=.75*YRA(IC2)+.25*YRA(IC2-1)
            YDECI=.75*YDEC(IC2)+.25*YDEC(IC2-1)
            ZRAI=.75*SPRA(IC2)+.25*SPRA(IC2-1)
            ZDECI=.75*SPDEC(IC2)+.25*SPDEC(IC2-1)
 42         CONTINUE
            CALL EFFIC(YRAI,YDECI,ZRAI,ZDECI,SRA,SDEC,HDET,HD,JFD,
     &                ANGLIM,QOK,QANG,eff)
            IC2=IC2+1
            DO 32 KK=1,1
               IF(.NOT.QANG) QANG5=.FALSE.
               IF (K.EQ.1) avgeff= 0.0
               avgeff = avgeff + eff
               IF (K.EQ.4) eff = avgeff / 4.0
 32         CONTINUE
 31      CONTINUE
 
         IF(NT.EQ.2) GO TO 500

CCC   THIS SECTION COMPUTES BACKGROUND FOR EACH RATE AND DET

c         DO 100 JD=1,4
            KK = HDET
            IF (KK.EQ.0) GO TO 100
            IF (.NOT.QOK(KK)) GO TO 100
            IF (.NOT.QANG5) GO TO 100
            if (eff .gt. 0.0) go to 100
            HDST=0
            DO 101 JS=1,4
               HHH=ABS(HD(JS))
               IF(HHH.EQ.0) GO TO 101
               IF(HDSC(5,J,HHH,HDET).LT.0)GO TO 100
               HSIGN=1
               IF(HHH.NE.HD(JS)) HSIGN=-1
               HDST=HDST+HSIGN*HDSC(5,J,HHH,HDET)
 101        CONTINUE
            NB=NB+1
            BKGD=(BKGD*(NB-1)+HDST)/NB
 100     CONTINUE

         IF(NT.EQ.1) GO TO 600

CCC   THIS IS THE SECOND TIME THRU, COMPUTES SOURCE
CC    RATES, AND CHISQUARED FOR BKGD

c 500     DO 200 JD=1,4
  500       KK = HDET
            IF (KK.LE.0) GO TO 200
            IF (.NOT.QOK(KK)) GO TO 200
            IF (.NOT.QANG5)GO TO 200
            HDST=0
            DO 201 JS=1,4
               HHH=ABS(HD(JS))
               IF(HHH.EQ.0) GO TO 201
               IF(HDSC(5,J,HHH,HDET).LT.0)GO TO 200
               HSIGN=1
               IF(HHH.NE.HD(JS)) HSIGN=-1
               HDST=HDST+HSIGN*HDSC(5,J,HHH,HDET)
 201        CONTINUE

C            IF (JBK.EQ.0) THEN 
            IF (eff .lt. 0.15) GO TO 203
            src = (hdst - bkgd) / (eff * 5.12)
            err = sqrt(hdst + (ber * ber)) / (eff * 5.12)
C            ELSE
C               SRC=HDST/5.12
C               ERR=SQRT(1.*HDST)/5.12
C            ENDIF

c            NENT=NENT+1
            row = row + 1
            status = 0
            call writefitslc(lcunit, row, IDAYD, IMSECD, src, 
     +           err, eff, status)
            if (status .ne. 0) then
               write(message, '('' Trouble writing FITS light curve '', 
     +           i1)')
c               write(message, '('' Trouble writing FITS light curve '', 
c     +           i1)') jd
               call xaerror(message,1)
               call ftgmsg(message)
               call xaerror(message,1)
               write(message, '('' FITSIO status = '', I3)') status
               call xaerror(message,1)
               go to 999
            endif

            GO TO 200
 203        if (eff .gt. 0.0000025) go to 200
            IF(IAND(INT(HFLAG2(3)),1).EQ.1)GO TO 200
            NB=NB+1
            DEV=-BKGD+HDST
            DB=(DB*(NB-1)+DEV)/NB
            SIG2=BKGD
            IF(SIG2.EQ.0.)GO TO 200
            CHI=CHI+DEV*DEV/SIG2
 200     CONTINUE
 600     CONTINUE
 30   CONTINUE

 999  RETURN

      END
