c
c  Startup info
c    itel <-> telescop/instrume/detnam mapping
c
c zexpmax= max number of allow experiments
c zexpnum= number of loaded experiments
c ztelescop= telescope
c zinstrume= instrument 
c zdetnam  = detector name 
c
      INTEGER*4 zexpmax, zexpnum
      PARAMETER (zexpmax=50)
      character(30) ztelescop(zexpmax)
      character(30) zinstrume(zexpmax)
      character(30) zdetnam(zexpmax)
c
      COMMON /startup/ ztelescop, zinstrume, zdetnam
c
      COMMON /start1/  zexpnum
c
