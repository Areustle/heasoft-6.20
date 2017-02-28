c
c  State variables for ximage program
c 

c  XIMAGE startup

c  prname = program name
c  version = program version
c  status = status flag

      character(6) prname
      character(12) version

c  imgcnt = Image counter

      integer*4 imgcnt

c  no_of_levels = Number of color levels
c  device = PGPLOT device
c  filename = Location of loaded file
c  equinox = XIMAGE equinox

      integer*4 no_of_levels
      character*(MAX_FILELEN) device, filename
      integer*4 equinox
c
c  Display
c
c    Viewport
c
      real*4 vpset(4)
      character(255) vpfile
      integer vpnum
      integer maxvps, vpconnum
      parameter (maxvps=1000)
      real*4 vpconfig(maxvps,4)
      logical vpframe
c
c    Level loading
c
      integer*4 numload
      real*4 ldlevs(256)
c
c    Titling
c
      character(80) uptitle, lotitle

      common /ximenv/ imgcnt, no_of_levels, equinox
      common /ximvpc/ vpset, vpnum, vpconnum, vpconfig, vpframe
      common /ximlvl/ numload, ldlevs
      common /ximstr/ prname, version, device, filename, vpfile,
     &                uptitle, lotitle
