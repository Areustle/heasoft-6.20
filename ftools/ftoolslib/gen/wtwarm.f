*+WTWARM
        subroutine wtwarm(subrout, version, chatter, wtchatter, string)

	IMPLICIT NONE
	integer chatter, wtchatter
        character*(*) subrout, version, string
c 
c Description:
c  Writes callib/roslib standard warning message string(s) to STDOUT.
c  Current format is approximately:
c  	' WARNING - '//subrout//version//string
c  but with a few extra bits of punctuation, plus some attempt to 
c  handle strings greater than 80 characters.
c
c Passed parameters
c  SUBROUT       i   : (char) Name of the subroutine from which wterr called
c  VERSION       i   : (char) Version of SUBROUT
c  CHATTER       i   : (int) chatter flag (nowt written if chatter = 0)
c  WTCHATTER     i   : (int) chatter flag at or above which string written
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
c  Original, based on wterrm (1.0.0)
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c  Ian M George     (1.1.0: 1996 Sep 17) removed calls to crmvlbk to prevent 
c					 problems under Solaris 2.2
c	character(7) version
c	parameter (version = '1.1.0')
*- 
c Internals 
	integer sublen, verlen, strlen
	integer istart, istop, str1len
	integer clenact
	character(80) outstr

c Initialize
	sublen = 0
	verlen = 0
	strlen = 0	
	istart = 1
	istop = 0
	str1len = 0

c Return if chatter flag is zero
	if(chatter.LT.wtchatter) return

c Remove all leading blanks from i/p strings
c	call crmvlbk(subrout)
c	call crmvlbk(version)
c	call crmvlbk(string)

c Check out the size of each character string
	sublen = clenact(subrout)
	verlen = clenact(version)
	strlen = clenact(string)

c Work out the 1st bit of the first line (up to where string will begin)	
	outstr = ' WARNING - '//subrout(:sublen)//
     &		' '//version(:verlen)//': '
	str1len = clenact(outstr) + 1

c Dump the first line
	istop = MIN(strlen,80 - str1len)
	outstr = outstr(:str1len)//string(1:istop)
	call fcecho(outstr)

c Return if we've finished
123	if(istop.GE.strlen) then
		return
	else
		istart = MAX(1,istop+1)
		istop = MIN(strlen,istop + 80 - 11)
		outstr = '           '//string(istart:istop)
		call fcecho(outstr)
		go to 123
	endif

	end
c -------------------------------------------------------------------
