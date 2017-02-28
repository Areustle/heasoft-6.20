*+GRPRMF
	subroutine grprmf(chatter, maxchan, ichan, maxen, ienerg, 
     &		matrix, thresh, maxgrp, 
     &		zerochan, ngrp, F_chan, N_chan, ierr)

        IMPLICIT NONE
	integer chatter, ierr, zerochan
	integer ichan, ienerg, maxgrp, maxchan,maxen
	real thresh
	real matrix(maxchan, *)
	integer ngrp(*), F_chan(maxen,*)
	integer N_chan(maxen,*)
c
c Description
c  Program to find and return the channel subsets from a full matrix
c  ie groups of adjacent channels with values above THRESH for each 
c  energy.
c
c Passed parameters
c  CHATTER             i  : Chatter flag (5 low, 10 normal,>20 debugging)
c  MAXCHAN             i  : Maximum Channel index array dimension
c  ICHAN               i  : Number of channels in full matrix
c  MAXEN               i  : Maximum energy index array dimension
c  IENERG              i  : Number of energy bins in full matrix
c  MATRIX              i  : FULL matrix
c  THRESH              i  : Threshold at or below which elements are ignored
c  MAXGRP              i  : Max no of chan subsets allowed per energy bin
c  ZEROCHAN	       i  : The number of the first detector channel (0 or 1)
c  NGRP                 o : Number of chan subsets per energy bin
c  F_CHAN               o : First chan is each chan subset
c  N_CHAN               o : Number chans is each chan subset
c  IERR                 o : Error Flag (0=OK)
c
c Called routines
c  subroutine WTINFO	: (CALLIB) Standard informational o/p to STDOUT
c
c Origin
c  Original
c
c Authors/Modification History:
c  grp_rmf history:
c  	Ian M George    (1992 Nov 11), original
c  	Ian M George    (1993 Feb 18), major overhaul
c  	Rehana Yusaf    (1.1.1: 1993 July 29), MAXCHAN & MAXEN args added for 
c                                  array dimensions
c  	Ian M George    (1.1.2: 1993 Jul 30), cosmetics
c  grprmf history:
c  Ian M George    (1.0.0: 1996 Nov 20) renamed from grp_rmf, with zerochan
c					as additional parameter so as to 
c					cope with the bloody 0vs1 channel
c					numbering schemes
c  Peter D Wilson  (1.0.1: 1998 Feb 23) Initialize arrays in case some rows
c                                       lack data exceeding threshold
c
        character(7) version
        parameter (version = '1.0.1')
*-
c Internals 
        character(6) subname
        parameter (subname = 'grprmf')
	integer ie, ic, ig, istop
        character(80) message

c Initialize
	ierr = 0

c Dump rubbish if daft
        message = ' using '//subname//' '//version
        call wtinfo(chatter,20,1,message)

c Main Sorting - basically using the condition that a new channel subset 
c   needs to be started if the threshold is exceeded, yet the current channel 
c   isnt one higher than the last chan (istop) for which the threshold was 
c   exceeded
c   The zerochan nightmare is handled crudely, but I think it's now right
	do ie = 1, ienerg
  	        ngrp(ie) = 0
	        do 12 ig=1,maxgrp
	            F_chan(ie,ig) = zerochan
	            N_chan(ie,ig) = 0
 12	        continue
		ig = 0
		istop = -99
	  	do ic = 1, ichan
		    if(matrix(ic,ie).GT.thresh) then
			if(ic-1.NE.istop) then
			   ig = ig + 1
			   ngrp(ie) = ig 
			   F_chan(ie,ig) = ic + zerochan - 1
			   istop = ic
			   if(ig.EQ.maxgrp) then
			     n_chan(ie,ig) = ichan - ic  + 1
		             goto 123
			   else
			     N_chan(ie,ig) = 1
			   endif
		        else
			   istop = ic
			   N_chan(ie,ig) = istop 
     &				- F_chan(ie,ig) + zerochan 
			endif
		    endif
		enddo
123	enddo



	return
	end

