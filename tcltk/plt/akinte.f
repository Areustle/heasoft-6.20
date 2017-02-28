      SUBROUTINE AKINTE(Xval, Y, Igroup, Iy0, Npts, iper, Init, Yval)
      REAL      Xval, Y(*), Yval
      INTEGER   igroup, Iy0, Npts, iper, Init
C---
C Interpolate a value using an Akima spline.  This is an PLT internal
C routine.
C---
C Xval    I    The X position to interpolate
C Y       I    The data array
C Igroup  I    Used by pltxc to calculate the current X value.
C Iy0     I    Y(Iy0+I) is the y-value of the Ith data point.
C Npts    I    The last data point in the fit range.
C iper    I    =0 not periodic, <>0 force periodic boundary condition.
C Init    I/O  <>0 force spline coef. to be recalcuated.  Set to zero on exit.
C Yval      O  The interpolated Y position
C---
C 1991-Nov-20 - AFT
C---
      REAL      NO
      PARAMETER (NO=-1.2E-34)
      INTEGER   MXDIM
      PARAMETER (MXDIM=2)
C
      REAL      slope(-2:2), xt(MXDIM), x1(MXDIM)
      REAL      rc1(0:1), rc2, rc3
      SAVE      rc1, rc2, rc3
      REAL      dx, dd, rdif1, rdif2, rim2, rim1, rip1, sdel, xdel
      INTEGER   ip(-2:3)
      SAVE      ip
      INTEGER   i, itry, j, ndim, iyoff, iyof1
      INTEGER   inew, ie, is, iyof0
      SAVE      inew, ie, is, iyof0
C---
      IF ( Init.NE.0 ) THEN
         ip(0)=0
         Init=0
         DO i=1,Npts
            CALL PLTXCC(Y, i, Igroup, xt, ndim, iyoff)
            IF ( Y(Iy0+iyoff).NE.NO .AND. xt(1).NE.NO ) THEN
               IS=I
               GOTO 110
            END IF
         END DO
  110    CONTINUE
         DO i=Npts,1,-1
            CALL PLTXCC(Y, i, Igroup, xt, ndim, iyoff)
            IF ( Y(Iy0+iyoff).NE.NO .AND. xt(1).NE.NO ) THEN
               ie=i
               GOTO 120
            END IF
         END DO
  120    CONTINUE
         inew=is
      END IF
C
C Find the proper interval
      CALL PLTXCC(Y, inew, igroup, xt, ndim, iyoff)
      dx = XVAL-xt(1)
C      IF ( dx ) 130,200,150
      IF ( dx.GT.0.0 ) GOTO 150
      IF ( dx.EQ.0.0 ) GOTO 200
C---
C Search backwards until Y value<>NO and D becomes positive
  130 CONTINUE
      IF ( inew.EQ.is ) GOTO 200
         inew = inew-1
         CALL PLTXCC(Y, inew, igroup, xt, ndim, iyoff)
         IF ( Y(Iy0+iyoff).EQ.NO .OR. xt(1).EQ.NO ) GOTO 130
         dx = XVAL-xt(1)
         IF ( dx.GE.0.0 ) GOTO 200
      GOTO 130
C---
C Search forwards until Y value<>NO and next D is negative
  150 itry=inew
  160 IF ( itry+1.GE.ie ) GOTO 200
         itry=itry+1
         CALL PLTXCC(Y, itry, igroup, xt, ndim, iyoff)
         IF ( Y(Iy0+iyoff).EQ.NO .OR. xt(1).EQ.NO ) GOTO 160
         dd = XVAL-xt(1)
         IF ( dd.LT.0.0 ) GOTO 200
         dx = dd
         inew=itry
      GOTO 160
C---
  200 CONTINUE
      IF ( inew.NE.ip(0) ) THEN
C---
C Set up pointer array for periodic boundary condition.  Add Npts before
C taking MOD to ensure positive result.
         ip(-2)=MOD(Npts+inew-3,Npts)+1
         ip(-1)=MOD(Npts+inew-2,Npts)+1
         ip( 0)=inew
         ip( 1)=MOD(     inew  ,Npts)+1
         ip( 2)=MOD(     inew+1,Npts)+1
         ip( 3)=MOD(     inew+2,Npts)+1
C---
C Remove any pointers to NO data values
         DO i=1,3
  240       CONTINUE
            CALL PLTXCC(Y, ip(i), igroup, xt, ndim, iyoff)
            IF ( Y(Iy0+iyoff).EQ.NO .OR. xt(1).EQ.NO ) THEN
               DO J=I,3
                  ip(J)=MOD(ip(J),Npts)+1
               END DO
               IF(ip(3).LT.IE) GOTO 240
            END IF
         END DO
         DO i=-1,-2,-1
  260       CONTINUE
            CALL PLTXCC(Y, ip(i), igroup, xt, ndim, iyoff)
            IF ( Y(Iy0+iyoff).EQ.NO .OR. xt(1).EQ.NO ) THEN
               DO J=I,-2,-1
                  ip(J)=MOD(Npts+ip(J)-2,Npts)+1
               END DO
               IF(ip(-2).GT.IS) GOTO 260
            END IF
         END DO
C
C Calculate slopes assuming periodic boundary condition.
         DO i=-2,2
            CALL PLTXCC(Y, ip(i), igroup, x1, ndim, iyoff)
            CALL PLTXCC(Y, ip(i+1), igroup, xt, ndim, iyof1)
            xdel=xt(1) - x1(1)
            slope(I)=(Y(Iy0+iyof1)-Y(Iy0+iyoff))/xdel
         END DO
C---
C If not periodic, then fix up ends if needed.
         IF ( iper.EQ.0 ) THEN
            IF ( ip(0).EQ.IS ) THEN
               sdel=slope(0)-slope(1)
               slope(-2)=slope(0)+2.*sdel
               slope(-1)=slope(0)+   sdel
            ELSE IF ( ip(0).EQ.IS+1 ) THEN
               sdel=slope(-1)-slope(0)
               slope(-2)=slope(-1)+  sdel
            ELSE IF ( ip(0).EQ.IE-2 ) THEN
               slope(2)=2*slope(1)-slope(0)
            ELSE IF ( ip(0).EQ.IE-1 ) THEN
               slope(1)=2*slope(0)-slope(-1)
               slope(2)=slope(1)+slope(0)-slope(-1)
            END IF
         END IF
C---
C Calcuate parameters for Akima Spline.
         DO i=0,1
            rim2=slope(I-2)
            rim1=slope(I-1)
            rip1=slope(I+1)
            rdif1=ABS(rip1-slope(I))
            rdif2=ABS(rim1-rim2)
            IF ( rdif1+rdif2.GT.0. ) THEN
               rc1(i)=(rdif1*rim1+rdif2*slope(i))/(rdif1+rdif2)
            ELSE
               rc1(i)=(rip1+slope(I))/2.
            END IF
         END DO
C
         CALL PLTXCC(Y, ip(0), Igroup, x1, ndim, iyof0)
         CALL PLTXCC(Y, ip(1), Igroup, xt, ndim, iyoff)
         xdel = xt(1) - x1(1)
         rc2=(3.*slope(0)-2.*rc1(0)-rc1(1))/xdel
         rc3=(rc1(0)+rc1(1)-2.*slope(0))/(xdel*xdel)
      END IF
C---
C Calculate function value.
      Yval=((rc3*dx+rc2)*dx+rc1(0))*dx+Y(Iy0+iyof0)
      RETURN
      END
