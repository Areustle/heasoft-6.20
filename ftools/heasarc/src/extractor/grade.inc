c Include file for grade stuff

c The grade array
c      garray             i           grade array (max size is GMAXSIZE)
c      gmin               i           lowest value of grade
c      gmax               i           highest value of grade

      INTEGER GMAXSIZE
      PARAMETER (GMAXSIZE=256)

      INTEGER gmin, gmax
      LOGICAL garray(0:GMAXSIZE-1)

      COMMON / ext_grade / garray, gmin, gmax






