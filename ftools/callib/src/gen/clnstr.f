*+CLNSTR
	subroutine clnstr(chatter, instr, maxlen, outstr, 
     &		outlen, nbad, ibad, errstat)

	IMPLICIT NONE
	integer chatter, maxlen, outlen, nbad, errstat
	integer ibad(maxlen)
	character*(*) instr, outstr
c 
c Description:
c  Replaces "illegal" ASCII characters within a string with OGIP-approved 
c  characters.
c  NOTE: current version simply replaces any character with an ASCII code
c        less than 32 with a space (char code 32), except for tabs
c
c Passed parameters
c  CHATTER       i   : chattiness flag for o/p (<20 quite,>40 for debugging)
c  INSTR         i   : Input character string to be sanitised
c  MAXLEN        i   : Max length of requested output string
c        	       (must be >= length of bit of i/p string 2 be searchd)
c  OUTSTR	   o : Returned sanitised (& maybe expanded) string
c  OUTLEN          o : length of output string
c  NBAD            o : Number of "illegal" characters found
c  IBAD            o : Array of integer ASCII codes of illegal characters found
c  ERRSTAT         o : Error Flag (0 if everything OK)
c
c Called Routines:
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p device
c
c Origin:
c  Noddy original
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1993 Jul 16), original 
	character(7) version
	parameter (version = '1.0.0')
*- 
c Internals 
	integer ichar, len, i
	integer ilen, iascii, ixtra, iout
	character(30) wrnstr
	character(80) message
	logical qbad
c Initialization
	qbad = .false.	
	errstat = 0
	ixtra = 0
	nbad = 0
	do i = 1, maxlen
		ibad(i) = 32
	enddo
	wrnstr = '** CLNSTR WARNING:'

c START MAIN
	ilen = LEN(instr)
	do i = 1, ilen
	   iascii = ichar(instr(i:i))
	   iout = i + ixtra
	   if(iascii.GE.32) then
		goto 156
	   else
	        qbad = .true.
		nbad = nbad + 1 
		ibad(nbad) = iascii
		if(iascii.EQ.0) then
	   		outstr(iout:iout) = ' '
	   	elseif(iascii.EQ.9) then       
	   		outstr(iout:iout+5) = '     '
			ixtra = 5
	   	elseif(iascii.LT.32) then
	   		outstr(iout:iout) = ' '
		endif
		goto 157
	   endif
156	   outstr(i:i) = instr(i:i)
157	enddo

	outlen = LEN(outstr)
	if(CHATTER.GT.40) then
	  if(ilen.LE.40) then
	     message = ' ... using CLNSTR '// version //
     &		' for string ' // instr(:40)
	  else
	     message = ' ... using CLNSTR '// version //
     &		' for string '// instr(:30) // ' (..etc)'
	  endif
	  call fcecho(message)
	endif

	if(outlen.GT.maxlen .AND. CHATTER.GT.20) then
	   message = wrnstr // ' Expanded string exceeds max'
	   call fcecho(message)
	   message = ' ... (will be truncated)'
	   call fcecho(message)
	endif

	return
	end
