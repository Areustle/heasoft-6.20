c=================================================================
c	This include file containes common block with
c	channel boundaries definitions
c	Is filled in fill_en subroutine
c
c	ch2e[1-2](channel,detector)
c	where  channel: 0-1023
c              detector: 0-3
c
c	1. ich-th channel of idet-th detector
c	   corresponds to ch2e1(ich,idet) - ch2e2(ich,idet) [keV]
c       2. Get the boundaries from the RMF files. 
c
c	Created: Wed May 18 15:11:04 EDT 1994
c       Modified: Wed Sep 22 16:32:48 EDT 1999 (Ilana Harrus)
c=================================================================

	integer znchmax,zndet
 	parameter(znchmax=1023,zndet=3)
	real ch2e1(0:znchmax,0:zndet)
	real ch2e2(0:znchmax,0:zndet)	

	common/chann_com/ch2e1,ch2e2
