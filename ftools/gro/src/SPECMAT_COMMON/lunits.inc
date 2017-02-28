* Logical unit numbers used for files opened by SPECMAT

      integer ludft,lusar,luedp,luexh,lumat,lusel,lupsd,luscale
      integer lutml
      parameter (ludft=11)     ! cones.default
      parameter (lusar=17)     ! sensitive area calibration file
      parameter (luedp=18)     ! energy dispersion calibration file
      parameter (lupsd=19)     ! point spread function calib. file
      parameter (luexh=32)     ! exposure history file
      parameter (lumat=7)      ! response matrix output file
      parameter (lusel=12)     ! input spectrum data file
      parameter (luscale=71)   ! instrument scale factor file
      parameter (lutml=31)     ! timeline file
