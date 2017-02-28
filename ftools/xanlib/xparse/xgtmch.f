      subroutine xgtmch(string, iparse, matchs, nmatch, type, imatch,
     &                  iflag, idelim, *, *)
c		rashafer 86 mar 5
c	XPARSE subroutine to match the next argument on the parse string
c	with a list of allowed matches.  If the argument does not match
c	the argument is discarded, and the user is prompted for a
c	replacement.
c	string	c*	i/r: parse string
c	iparse	i4	i/r: parse position
c	matchs	c*(nmatch)	I: array of allowed matches
c	nmatch	i4	i: no. of allowed matches
c	type	c*	i: description of the entries in the arrray
c	imatch	i4	i/r: the index in the array of the next argument
c			if the argument is skipped over, then imatch is
c			unchanged.
c			(If <= 0 no successful match, see iflag)
c	iflag	i4	r: unusual circumstance flag
c			1 - fell off the end of the argument string
c			-1 - eof while prompting for replacement string
c	idelim	i4	r: the delimeter flag for what follows the argument
c	alternate returns:
c		*1 - fell off the string
c		*2 - eof while prompting for replacement string
	character*(*) string,matchs(*),type
        character(256) ctmp
	integer*4 iparse,nmatch,imatch,iflag,idelim
c
	integer*4 lenact
	integer*4 ibeg,iend,iflag2,ierr, jmatch
	logical*4 qskip
c **	** come from for a successful insertion after a bad match
100	continue
	iflag=0
	call xgtarg(string,iparse,ibeg,iend,qskip,iflag2,idelim)
	if((qskip).and.(imatch.gt.0))then
	    return
	    end if
	if(iflag2.ne.0)then
	    iflag=1
	    return 1
	    end if
	jmatch=imatch
	if(.not.qskip)call xmatch(string(ibeg:iend),matchs,nmatch,jmatch)
	if(jmatch.le.0)then
	    call xunids(string(ibeg:iend),matchs,nmatch,jmatch,type)
	    if(jmatch.lt.0) imatch = min(nmatch,abs(jmatch))
	    if(imatch.gt.0) then
                ctmp = 'Insert selection: ('//matchs(imatch)
     &                 (:lenact(matchs(imatch)))//') '
		call xinfix(ctmp,string,iparse,ierr)
	    else
		call xinfix('Insert selection (no default):',string,
     &		 iparse,ierr)
		end if
	    if(ierr.lt.0)then
		imatch=jmatch
		iflag=-1
		return 2
		end if
c **	    ** now go back and try again
	    goto 100
	    end if
	imatch=jmatch
	return
	end
