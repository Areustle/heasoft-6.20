*+CBDCS
	subroutine cbdcs(str,value,result,status)

	implicit none
	character*(*) str
	character*(*) value
	logical result
	integer status

C-----------------------------------------------------------------------
C Description: Compares a string value with the value descriptors in a
C              CBD string.  A result of true is returned if a match is
C              made.  An asterisk prepended or appended to the string 
C              will match zero or more characters at that position.  
C              (e.g. *bc* will match 'abcd' and 'bcde') Otherwise a 
C              result of .false. is reutrned.  If the CBD string is 
C              unable to be parsed, then a non-zero status is returned.
C
C Arguments:   str     (i): The CBD string
C              value   (i): The value tobe compared to the value 
C                           descriptors in the CBD string
C              result  (r): Flag describing if the value is contained in
C                           the CBD string
C              status  (r): Success flag for this routine
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar Jan 31, 1994 -- Original Version
C              Ron Zellar Nov 17, 1994 -- Added wild card searches
C-----------------------------------------------------------------------
*- Version 1.0

	integer n,type,errstat,fcstln,lpos,rpos,lft,rit,idxflg,astflg

C	initialize the result, pointer, and status variables
	result = .false.
	n = 0
	status = 0
	lft = 1
	rit = fcstln(value)
	idxflg = 0
	astflg = 0

C	Determine which type of search we need to do
	if ((value(1:1) .eq. '*').and.(value(rit:rit) .eq. '*')) then
	     astflg = 1
	     lft = 2
	     rit = rit - 1
	else if ((value(1:1).ne.'*').and.(value(rit:rit).eq.'*')) then
	     astflg = 2
	     rit = rit - 1
	else if ((value(1:1).eq.'*').and.(value(rit:rit).ne.'*')) then
	     astflg = 3
	     lft = 2
	else
	     astflg = 4
	endif

C       Top of the comparison loop
100     continue

C       Move the pointer to the next value descriptor
        call cbdmp(str,n,errstat)
        if ( (errstat .ne. 0) .and. (errstat .ne. 2) ) then
                status = 1
                return
        endif

C       if errstat = 2 then finished examining the string
        if (errstat .eq. 2) return

C       Determine the type of value descriptor
        call cbdtyp(str,n,type,errstat)
        if (errstat .ne. 0) then
                status = 2
                return
        endif

C	if value descriptor is a range or a real value, then ignore
C	if it's a string then compare
	if (type .eq. 1) then
	     call cbdgs(str,n,lpos,rpos,errstat)

	     if (errstat .ne. 0) then
	          status = 3
	          return
	     endif

	  if ((rpos - lpos) .ge. (rit - lft)) then
	     idxflg = index(str(lpos:rpos),value(lft:rit))
	     if ((astflg .eq. 1) .and. (idxflg .ne. 0)) then
	          result = .true.
	          return
	     else if ((astflg .eq. 2) .and. (idxflg .eq. 1)) then
	          result = .true.
	          return
	     else if ((astflg .eq. 3) .and.
     &	              (idxflg .eq. (rpos-lpos-rit+lft+1))) then
	          result = .true.
	          return
	     else if ((astflg .eq. 4) .and. 
     &	              (value(:fcstln(value)) .eq. str(lpos:rpos))) then
	          result = .true.
	          return
	     endif
	  endif
	endif

C	Go back to top of search loop
	goto 100

	end
