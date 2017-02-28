* Include file for windows.  Holds the common blocks and so on


c  definitions for windows in xronos
      integer*4 nwi(20),nwito
      real*4 pwia(10),pwio(10),fwia(9,10),fwio(9,10),ewia(9),ewio(9)
      real*8 twia(100),twio(100),pwi(2)
	character(100) winfilename
	save winfilename, twia, twio, pwi,
     *		pwia, pwio,
     *		fwia, fwio, ewia, ewio,
     *		nwi,
     *		nwito

