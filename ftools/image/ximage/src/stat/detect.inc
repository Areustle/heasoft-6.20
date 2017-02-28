c
c  Detections
c
c  maxdet = maximum number of detections
c  numdet = final number of detections
c  dtsox/y = location of detected box (detector coords)
c  imsox/y = location of detected box (image coords)
c     (uses convention where center of first pixel is 1)
c  psfco = psf correction
c  vco = vignetting correction
c  baso = background local to source
c  sint = signal
c  err = error
c  prob = probability
c  bxha = final halfsize of source box in original pixels
c  hot = 
c  goodflag = 
c
      integer*4 maxdet, numdet
      parameter (maxdet=50000) 
      real*4 dtsox(maxdet), dtsoy(maxdet)
      real*4 imsox(maxdet), imsoy(maxdet)
      real*4 psfco(maxdet), vco(maxdet), baso(maxdet)
      real*4 sint(maxdet), err(maxdet), prob(maxdet), bxha(maxdet)
      integer*4 hot(maxdet), goodflag(maxdet)
c
      common /detcom/ numdet, goodflag, dtsox, dtsoy, imsox, imsoy,
     &                psfco, vco, baso, sint, err, prob, bxha, hot
