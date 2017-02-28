      SUBROUTINE SET_THRESHOLD(Bnew,Boxsiz,Szx,Szy,Tr)
      IMPLICIT NONE
c
c  I  bnew    (r)  Background in cts per image pixel
c  I  boxsiz  (i)  Source box size
c  I  szx/y   (i)  Size of image
c  O  tr      (r)  Threshold value
c
      REAL*4 Bnew
      INTEGER*4 Boxsiz, Szx, Szy, Tr

      include '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 ntrial
      REAL*4 proli, bkperbox, zz, q, ppp, XPOLOG

      ntrial = float(Szx*Szy)/float(Boxsiz*Boxsiz)
      proli = .1/FLOAT(ntrial)
      bkperbox = Bnew*float(Boxsiz*Boxsiz)

      write (ZWRite,*) ' Background (cts in source box) : ', bkperbox
      call XWRITE(ZWRite, 15)
      if ( bkperbox.lt.300. ) then
         call XWRITE(' < 300, using Poissionian', 20)
      else
         call XWRITE(' >= 300, using Poissionian', 20)
      endif
      write (ZWRite,*) ' Value of proli : ', proli
      call XWRITE(ZWRite, 20)

      Tr = bkperbox
      DO WHILE ( .TRUE. )
         Tr = Tr + 1
         IF ( bkperbox.LT.300. ) THEN
            ppp = XPOLOG(Tr,bkperbox,1)
         ELSE
            zz = (Tr-bkperbox)/SQRT(bkperbox)
            CALL CUMGA(zz,q)
            ppp = q/2.
         ENDIF
         write (ZWRite,*) ' Tr ', Tr, ' ppp ', ppp
         call XWRITE(ZWRite, 25)
         IF ( ppp.LT.proli ) GOTO 100
      ENDDO
 100  CONTINUE
      RETURN
      END
