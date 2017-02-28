c
c  Map calculation is valid for
c
      character(10) statid
c
c  Background map
c
      integer bgsz
      parameter (bgsz = 128)
      real*4 bgmap(bgsz,bgsz)
c
c  Background boxes
c
c  nboxes     number of boxes
c  nbx        number of boxes in x direction
c  nby        number of boxes in y direction
c  ibbac      box size in image pixels
c  bb         box values
c  bb_sig     box error
c  ww         box weights
c  bb_flags   0 = good
c             1 = barycenter too far from center
c             2 = non-poissonian statistics
c             3 = back value too far from average
c  bb_npix    number of pixels in box
c
      integer maxbox, nboxes, nbx, nby, ibbac
      parameter (maxbox = 4096)
      real*4 bb(maxbox), bb_sig(maxbox), ww(maxbox)
      integer*4 bb_flags(maxbox), bb_npix(maxbox)
c
c  background output
c
c  back = image background in cnts/image pixel (first pass)
c  bnew = image background in cnts/image pixel (final)
c
      real*4 back, bnew
c
c  threshold
c
c  sigmult = multiple of sigma to use when determining 
c            good background boxes
c
      real*4 sigmult, barylim, bxhprob
      real*4 defsigm, defbarl, defhprob
c     parameter (defsigm = 3.0, defbarl = 0.35)
      parameter (defsigm = 3.0, defbarl = 0.35, defhprob = 0.)
c
c  Flags
c
c  poisson    whether to use poissonian stats to calculate background
c  draw_boxes whether to draw rejected boxes
c  izero      1 = all background boxes rejected (DETECT_BGR->INTENSITY)
c
      logical poisson, draw_boxes
      integer izero

      common /backgd/ bgmap, nboxes, nbx, nby, ibbac, bb, bb_sig, ww,
     &                bb_flags, bb_npix, back, bnew, sigmult, barylim,
     &                bxhprob, poisson, draw_boxes, izero, statid
