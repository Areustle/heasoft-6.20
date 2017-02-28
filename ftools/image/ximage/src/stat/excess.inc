c
c  Common for excesses
c
c  boxsiz = source box size
c
      integer*4 boxsiz
c
c  Excesses
c
c  maxsou = maximum number of excesses
c  numsou = number of excesses found
c  nbros = order array
c  rbox = value of excess boxes
c  sourx/y = location of excess boxes
c    (uses convention where leftmost edge of first pixel is 1)
c
c  numexs = number of non-contiguous excesses found
c  asoux/y = location of non-contiguous excess
c    (uses convention where leftmost edge of first pixel is 1)
c  bxn = size of non-contiguous excess box
c  intm = value of non-contiguous excess box
c
      integer*4 maxsou, numsou, numexs
      parameter (maxsou = 50000)
      integer*4 nbros(maxsou)
      real*4 rbox(maxsou), sourx(maxsou), soury(maxsou)
      real*4 asoux(maxsou), asouy(maxsou), bxn(maxsou)
      integer*4 intm(maxsou)

      common /exscom/ boxsiz, numsou, numexs, nbros, rbox, sourx, soury,
     &                asoux, asouy, bxn, intm
