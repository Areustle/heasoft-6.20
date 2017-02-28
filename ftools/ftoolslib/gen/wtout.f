*+WTOUT
        subroutine wtout(level, string)

	IMPLICIT NONE
	integer level
        character*(*) string
c 
c Description:
c  Writes callib/roslib standard message string(s) to STDOUT, with the 
c format controled by the level parameter:
c 	level = 0 	high level message 
c       level = 1       moderate level message
c	level = 2 	low level message
c
c Passed parameters
c  LEVEL         i   : (int) importance level (see above)
c  STRING	 i   : (char) Context string to be appended to standard msg
c
c Called Routines:
c  function CLENACT      : (CALLIB) Returns actual length of string
c  subroutine CRMVLBK    : (CALLIB) Removes leading blanks from a string
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c  Ian M George     (1.1.0: 1996 Feb 06) lobbed in the islop stuff
c  Ian M George     (1.2.0: 1996 Sep 17) removed calls to crmvlbk to prevent
c                                        problems under Solaris 2.2
c	character(7) version
c	parameter (version = '1.2.0')
*- 
c Internals 
	integer istart, istop, strlen
	integer clenact, ifront, i, islop
	character(11) front, blank
	character(80) outstr

c Initialize
	blank = '           '
	strlen = 0	
	istart = 1
	istop = 0
	islop = 10

c Remove all leading blanks from i/p string & check-out size
c	call crmvlbk(string)
	strlen = clenact(string)

c Sort out the level
	if(level.eq.0) then
		front = ' '
		ifront = 1
	elseif(level.eq.1) then
		front = ' ... '
		ifront = 5
	elseif(level.eq.2) then
		front = ' ...... '
		ifront = 8
	else 
		front = ' ......... '
		ifront = 11
	endif

c Dump the first line
	istop = MIN(strlen,80 - ifront)
	if((istop.LT.strlen).and.
     &	   (string(istop+1:istop+1).NE.' ')) then
	   do i = istop,istop-islop,-1
	     if(string(i:i).EQ.' ') then
		istop = i
		goto 122
	     endif	
	   enddo
	endif
122	outstr = front(:ifront)//string(1:istop)
	call fcecho(outstr)

c Return if we've finished
123	if(istop.GE.strlen) then
		return
	else
125		istart = MAX(1,istop+1)
	        if((string(istart:istart).EQ.' ').and.
     &		  (istart+1.LT.strlen)) then
		  istop = istop + 1
		  goto 125
	        endif
		istop = MIN(strlen,istop + 80 - 9)
		if((istop.LT.strlen).and.
     &	   	  (string(istop+1:istop+1).NE.' ')) then
	   	  do i = istop,istop-islop,-1
	     	     if(string(i:i).EQ.' ') then
			istop = i
			goto 124
	     	     endif	
	   	  enddo
	        endif
124		outstr = blank(:ifront)//string(istart:istop)
		call fcecho(outstr)
		go to 123
	endif

	end
c -------------------------------------------------------------------
