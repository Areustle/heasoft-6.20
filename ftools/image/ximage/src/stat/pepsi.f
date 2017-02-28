      SUBROUTINE PEPSI(R,MAXPSF,FRAC)
      IMPLICIT NONE
c
c  calculates expected cma psf
c
c  I  r      (r)  Offaxis in original pixels
c  I  maxpsf (i)  Maximum index for frac
c  O  frac   (r)  PSF output
c
      integer maxpsf
      real*4 r, frac(0:maxpsf)

c     INCLUDE 'ximpsf.inc'
      REAL*4 psf1 , psf2 , psf3 , psf5 , psf10 , psf12 , psf16
      REAL*4 psf20 , psf30 , psf27 , psf35 , ag , bg , cg , ai
      INTEGER*4 i
      REAL*4 xin(4) , yin(4) , FERMI , PAR
c
c Old
c     REAL*4 X , Y
c     r = SQRT(X*X+Y*Y)

      IF ( r.LT.180. ) THEN
c box=0.5
         psf1 = .0954
c box=2.5
         psf2 = .4781
c box=4.5
         psf3 = .7333
c box=8.5
         psf5 = .8906
      ELSE
c box=0.5
         psf1 = FERMI(r,.0028,.114,216.,58.9)
c box=2.5
         psf2 = FERMI(r,.0184,.544,246.,66.7)
c box=4.5
         psf3 = FERMI(r,.0408,.776,289.,72.8)
c box=8.5
         psf5 = FERMI(r,0.108,.812,381.,69.9)
      ENDIF
c box=18.5
      psf10 = FERMI(r,.258,.707,527.,64.7)
c box=22.5
      psf12 = FERMI(r,.337,.634,562.,58.4)
c box=30.5
      psf16 = FERMI(r,.500,.483,615.,53.8)
c box=38.5
      psf20 = FERMI(r,.641,.352,662.,60.3)
c box=48.5
      psf30 = FERMI(r,.699,.302,764.,87.8)
c box=56.5
      psf27 = FERMI(r,.701,.302,943.,128.)
      psf35 = 1.
c
      yin(2) = psf1
      yin(3) = psf2
      yin(4) = psf3
      xin(2) = .5
      xin(3) = 2.5
      xin(4) = 4.5
c
      CALL PARIN(xin,yin,-1,ag,bg,cg)
      DO 100 i = 0 , 2
         ai = i
         FRAc(i) = MAX(PAR(ai,ag,bg,cg),1.E-3)
 100  CONTINUE
      CALL MOVXY(xin,yin)
      yin(4) = psf5
      xin(4) = 8.5
      CALL PARIN(xin,yin,0,ag,bg,cg)
      FRAc(3) = PAR(3.,ag,bg,cg)
      FRAc(4) = PAR(4.,ag,bg,cg)
      CALL MOVXY(xin,yin)
      yin(4) = psf10
      xin(4) = 18.5
      CALL PARIN(xin,yin,0,ag,bg,cg)
      DO 200 i = 5 , 9
         ai = i
         FRAc(i) = PAR(ai,ag,bg,cg)
 200  CONTINUE
      CALL MOVXY(xin,yin)
      yin(4) = psf12
      xin(4) = 22.5
      CALL PARIN(xin,yin,0,ag,bg,cg)
      DO 300 i = 10 , 18
         ai = i
         FRAc(i) = PAR(ai,ag,bg,cg)
 300  CONTINUE
      CALL MOVXY(xin,yin)
      yin(4) = psf16
      xin(4) = 30.5
      CALL PARIN(xin,yin,0,ag,bg,cg)
      DO 400 i = 19 , 22
         ai = i
         FRAc(i) = MIN(PAR(ai,ag,bg,cg),1.)
 400  CONTINUE
      CALL MOVXY(xin,yin)
      yin(4) = psf20
      xin(4) = 38.5
      CALL PARIN(xin,yin,0,ag,bg,cg)
      DO 500 i = 23 , 30
         ai = i
         FRAc(i) = MIN(PAR(ai,ag,bg,cg),1.)
 500  CONTINUE
      CALL MOVXY(xin,yin)
      yin(4) = psf30
      xin(4) = 48.5
      CALL PARIN(xin,yin,0,ag,bg,cg)
      DO 600 i = 31 , 38
         ai = i
         FRAc(i) = MIN(PAR(ai,ag,bg,cg),1.)
 600  CONTINUE
      CALL MOVXY(xin,yin)
      yin(4) = psf27
      xin(4) = 56.5
      CALL PARIN(xin,yin,0,ag,bg,cg)
      DO 700 i = 39 , 48
         ai = i
         FRAc(i) = MIN(PAR(ai,ag,bg,cg),1.)
 700  CONTINUE
      CALL MOVXY(xin,yin)
      yin(4) = psf35
      xin(4) = 75.0
      CALL PARIN(xin,yin,0,ag,bg,cg)
      DO 800 i = 49 , 56
         ai = i
         FRAc(i) = MIN(PAR(ai,ag,bg,cg),1.)
 800  CONTINUE
      CALL MOVXY(xin,yin)
      CALL PARIN(xin,yin,1,ag,bg,cg)
      DO 900 i = 57 , 70
         ai = i
         FRAc(i) = MIN(PAR(ai,ag,bg,cg),1.)
 900  CONTINUE
      DO 1000 i = 71 , MAXPSF
         FRAc(i) = 1.
 1000 CONTINUE
      RETURN
      END
