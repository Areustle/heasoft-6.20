c  
c  Note: Analogous values are also defined in maxvals.h for C 
c        When changing a value here, take care to update maxvals.h,
c        if necessary.  The only difference between analogous values 
c        should be that string lengths take one more character in C.
c

c
c General limits
c
      integer MAX_FILELEN, MAX_INT
      parameter (MAX_FILELEN = 1024)
      parameter (MAX_INT = 2147483647)
c
c Command limits
c
      integer MAX_CMDNUM, MAX_CMDLEN
      parameter (MAX_CMDNUM = 250, MAX_CMDLEN = 1000)
c
c Image Limits
c
      integer MAX_SLOTS, MAX_MAPS, MAX_TMPMAPS, MAX_ISIZE
      parameter (MAX_SLOTS = 9)
      parameter (MAX_TMPMAPS = 5)
      parameter (MAX_MAPS = MAX_SLOTS*2 + MAX_TMPMAPS + 1)
      parameter (MAX_ISIZE = 2048)
c
c Header limits
c
      integer MAX_IDSTR
      parameter (MAX_IDSTR = 10)
c
c Event File Limits
c
      integer MAX_TBLCOLS, MAX_ROWBUF, MAX_GTIS
      parameter (MAX_TBLCOLS = 30)
      parameter (MAX_ROWBUF = 500)
      parameter (MAX_GTIS =  10000)
c
c Level limits
c
      integer MAX_NUMLEVS, DEF_NUMLEVS
      parameter (MAX_NUMLEVS = 239, DEF_NUMLEVS = 16)
c     parameter (MAX_NUMLEVS = 83, DEF_NUMLEVS = 16)
c
c Text file column limit
c
      integer MXQCOL
      parameter (MXQCOL = 20)
