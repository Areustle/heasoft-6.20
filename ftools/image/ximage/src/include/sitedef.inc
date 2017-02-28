c	SITEDEF.INCLUDE
c		XIMAGE include file with site specific definitions and
c
c LHEA_HELP variable
	character lheahelp*255

c The directory with the XIMAGE commands :
	character cman*255

c  The directory with the parameter files:
         character cpar*255

c  The directory with the calibration files:
         character ccal*255
c  The disk
         character cxan*255
c
        data lheahelp/'$LHEA_HELP'/
        data cxan/'$XANADU'/
        data cman/'image/ximage/manager'/
        data cpar/'image/ximage/files'/   
        data ccal/'image/ximage/cal'/ 
