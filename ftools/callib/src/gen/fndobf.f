*+FNDOBF
     	subroutine fndobf(chatter, iunit, nsearch, 
     &		obfversn, hduclas, hduvers, 
     &   	ierr)

      IMPLICIT NONE
      integer iunit, nsearch, ierr, chatter
      character*(*) hduclas(9), hduvers(9)
      character*(*) obfversn

c Description 
c  Routine attempts to find an OBSFACTOR dataset within file already 
c opened using IUNIT by searching for appropriate HDUCLAS* values.
c The error flag will be tripped if more than one extension 
c appearing to be an OBSFACTOR dataset is found.
c
c Passed Parameters
c  CHATTER	    i  : Chattiness flag 
c  IUNIT            i  : FORTRAN i/o unit number of opened file
c  NSEARCH          i  : Max number of extensions in file to search
c  OBFVERSN	      o: Value of OBFVERSN keyword for extn found 
c  HDUCLAS 	      o: Array of HDUCLAS* values for extn found
c  HDUVERS	      o: Array of HDUVERS* values for extn found
c  IERR               o: Error Flag (zero = OK, -ve if no extn found)
c
c Origin
c  Original
c
c Called Routines
c   {incomplete}
c
c Author/Modification History
c  Ian M George (1.0.0:95 Jun 30) Original
c  Ian M George (1.0.1:96 Feb 04) added wtinfo and friends
      character(5) version
      parameter (version = '1.0.1')
*-
c Internals
	character(6) subname
	parameter (subname = 'fndobf')
	integer status, i, maxsearch, isearch
	integer ninstr, nfound, ngot, htype
	parameter (maxsearch=99)
	integer next(maxsearch), extnum
	character(1) dummy2(9,maxsearch), dummy1(maxsearch)
	character(20) instr(9), comm
	character(80) message, string

c Initialize	
	status = 0
      	ierr = 0
	obfversn = ' '
	do i = 1, 9
	  hduclas(i) = 'Undefined'
	  hduvers(i) = ' '
	enddo

c Give the nutters some info
        message = 'using '//subname//' '//version
        call wtinfo(chatter,15,1,message)

c ........ Dont yet know the ext# we're after - search using HDUCLAS* keys
	   message = ' Searching for OBSFACTOR dataset via'//
     &		' putative HDUCLASn keywords'
	   call wtinfo(chatter, 12,1, message)
c ........ Check for sillies
	   if(maxsearch.LT.nsearch) then
	      call wtwarm(subname, version, chatter, 1, 
     &			'NSEARCH too large')
	      isearch = maxsearch
	      write(message,'(a,i12)') 
     &		' Max no. ext search will be ', isearch
	      call wtinfo(chatter, 12, 2, message)
	   else
	      isearch = nsearch
	   endif


c ........ Here we go 
	   ninstr = 2
	   instr(1) = 'RESPONSE'
	   instr(2) = 'OBSFACTOR'
	   call fndhdu(chatter, iunit, ninstr, instr, isearch, nfound,
     &		next, dummy2, dummy2, dummy1, status)
	   if(status.NE.0) then
		ierr = 1
		goto 998
	   endif
	   if(nfound.EQ.0) then
		call wtwarm(subname, version, chatter, 12,
     &			'No datasets found')
		ierr = -1
		goto 998
	   elseif(nfound.GT.1) then
		write(string,'(i12,a)')
     &		   nfound, ' Datasets located'
		call rmvexsp(string,message)
		call wtwarm(subname,version,chatter,1,message)
		call wtinfo(chatter,1,2,
     &			'Using the first one found')
		extnum = next(1)
	   else
		call wtinfo(chatter,12, 2,
     &			'Located acceptable Extension')
		extnum = next(1)
	   endif

c Move to the required extension
	call ftmrhd(iunit, extnum, htype, status)
	if(status.ne.0) then
	   call wtferr(subname, version, status,
     &		' Moving to required extension')
	   ierr = 1
	   goto 998
	endif

c Get a few keywords
	status = 0
	call ftgkns(iunit,'HDUCLAS',1,9,hduclas,ngot,status)
	status = 0
	call ftgkns(iunit,'HDUVERS',1,9,hduvers,ngot,status)
	if(hduvers(2).EQ.' ') then
		status = 0
		call ftgkys(iunit,'OBFVERSN', obfversn, comm, status)
		if(status.NE.0) then
	   	   call wtferr(subname, version, status,
     &		  	' Getting OBFVERSN keyword')
		     status = 0
		endif
	else
		obfversn = hduvers(2)
	endif	


c Final Error check
998	if(ierr.GT.0) then
	   call wterrm(subname, version, 'Incomplete Execution')
	endif

	return
      	end
