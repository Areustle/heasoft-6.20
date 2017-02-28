      SUBROUTINE LOCAL_BG(Xpos,Ypos,Ibgx,Ibgy,Zmx,Baccnts,Bacpix,Status)
      IMPLICIT NONE
c
c  Calculate local background
c
c  I  x/ypos   (r)  Location of detection in image coordinates
c  I  ibgx/y   (i)  Location of detection in BGMAP
c  I  zmx      (r)  Image zoom in x
c  O  baccnts  (r)  Counts in local background box
c  O  bacpix   (r)  Pixels in local background box
c
      real*4 Xpos, Ypos, Baccnts, Bacpix
      integer*4 Ibgx, Ibgy, Status
      real*8 Zmx

      include 'backgd.inc'
      include 'detect.inc'
      include '../include/io.inc'
c
c  Local variables
c
      REAL*4  bai , bmezzi
      REAL*4 ajx , ajy , asm , wc
      INTEGER*4 ilb , ikk
      INTEGER*4 ijy , ijx , isb
      integer*4 order(MAXbox)
      real*4 rdist(MAXbox)

c  Initialize to avoid warning
      ijy = 0
c  --
      asm = 10./Zmx
      bai = 0.
      Baccnts = 0.
      bmezzi = IBBac/2.
      DO 100 ilb = 1 , NBOxes
         order(ilb) = ilb
         IF ( NBY.GT.0 ) THEN
            ijy = (ilb-1)/NBY
         ELSE
            call XWRITE (' nby is zero in local_bg', 10)
         ENDIF
         ijx = ilb - ijy*NBX
         ajx = IBBac*(ijx-1) + bmezzi
         ajy = IBBac*ijy + bmezzi
         rdist(ilb) = SQRT((Xpos-ajx)**2.+(Ypos-ajy)**2.)
 100  CONTINUE
      CALL QSORT_DETECT(rdist, order, MAXbox, NBOxes, Status)

      isb = 1
      Bacpix = 0
      write(ZWRite,*) NUMdet, ' - Nearest good bg box ', order(1)
      call XWRITE(ZWRite, 25)

      DO WHILE ( .TRUE. )
         IF ( isb.GE.NBOxes ) THEN
            CALL XWRITE(' Local background not well estimated',10)
            Bacpix = 0
            GOTO 200
         ENDIF
         isb = isb + 1
C  isb starts from 2 to avoid box containing source
         ikk = order(isb)
         IF ( BB(ikk).NE.-1 ) THEN
            Bacpix = Bacpix + 1
            IF ( WW(ikk).GT.0 ) THEN
               wc = BGMap(Ibgx,Ibgy)/WW(ikk)
            ELSE
               call XWRITE(' ww(ikk)=0 in local_bg', 10)
               wc = 1.
            ENDIF

            Baccnts = Baccnts + BB(ikk)*wc

            IF ( Bacpix.GE.asm ) GOTO 200
         ENDIF
      ENDDO
 200  continue
      RETURN
      END
