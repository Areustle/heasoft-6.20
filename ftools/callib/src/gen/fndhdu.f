c ------------------------------------------------------------------
*+ FNDHDU
	subroutine fndhdu(chatter, iunit, ninstr, instr,
     &		nsearch, nfound, next, outhdu, outver, extnam, ierr)

	IMPLICIT NONE
	integer iunit, chatter, ninstr, ierr, nsearch, nfound
	integer next(*)
	character*(*) instr(*)
	character*(*) outhdu(9,*), extnam(*), outver(9,*)

c Description
c  Searches through the current, plus a specified number of additional 
c  extensions, of a FITS file looking for extensions which contain HDUCLASn 
c  values which match passed i/p strings. Returned are the number of extensions 
c  safifying the criteria, the extension number (relative to the current 
c  extension on input), the full HDUCLASn hierarchy, the EXTNAME, and the full 
c  HDUVERSn hierarchy for each of those extensions.
c  The FITS file is assumed to be open, and is rewound back to the 
c  Header unit (extension) it was at on entry.
c  !!! NOTE !!! Backwards searches are not yet supported.
c	        The i/p strings must be in ordered is the same way as the 
c		  expected HDUCLASn hierarchy, with instr(1) corresponding 
c		  to the highest level (HDUCLAS1)
c               The special wild-card character "*" is allowed in any/all 
c 		  of the i/p strings to indicate that all HDUCLASn values 
c		  corresponding to this string to deemed acceptable.
c
c Passed Parameters
c  CHATTER		i   : The chattiness flag (>9 increasing verbose)
c  IUNIT 		i   : The FORTRAN unit of the open FITS file
c  NINSTR		i   : The number of strings to be searched for
c  INSTR		i   : Array of strings to be searched for
c  NSEARCH		i   : The number extensions to be searched 
c				 (in addition to current extension)
c  NFOUND		  o : The number of extensions found with HDUCLASn
c				 matching INSTR(n)
c  NEXT			  o : Array of the numbers of each extension found 
c				(relative to the extension on input)
c  OUTHDU		  o : 2-d array of HDUCLASn values for each xtens found
c  OUTVER		  o : 2-d array of HDUVERSn values for each xtens found
c  EXTNAM		  o : Array of EXTNAME value for each xtens found
c  IERR			  o : Error flag (zero = OK)
c 
c Origin
c   Original
c
c Called Routines
c  {incomplete}
c
c Author/Modification History
c  Ian M George    (1.0.0: 93 Nov 17) Original
c  Ian M George    (1.1.0: 94 Jun 27) make passed arrays (*)
	character(7) version
	parameter (version='1.1.0')
*-
c Internals 
	integer status, CLENACT
	integer htype, ngot
	integer i,jj, imove, iextno
	logical qyep
	character(20) comm
	character(20) hduclas(9)
	character(40) extname
        character(80) message
        character(40)  errstr, wrnstr
c Initialize
	nfound = 0
	ierr = 0
	do i = 1, nsearch
	  next(i) = 0
	enddo
        errstr = '** FNDHDU '//version//' ERROR: '
        wrnstr = '** FNDHDU '//version//' WARNING: '
	status = 0

c User info, if requested
        if(chatter.GE.20) then
             message = ' ... using FNDHDU '// version
             call fcecho(message)
        endif
 
c Check for sillies
	if(ninstr.LE.0) then
           message = errstr // ' Illegal no. i/p strings requested'
           call fcecho(message)
	   ierr = 1
	   goto 482
	endif

c Prepare the input strings, leading removing blanks etc
	do jj = 1, ninstr
	   call crmvlbk(instr(jj))
	enddo

c Loop through the extensions, noting those which we want
      	do iextno = 0, nsearch
	    if(chatter.GT.25) then
	       write(message,'(a,i12)') '... searching extension: ',
     &                                  iextno
	       call fcecho(message)
	    endif 
c	  ... Read the EXTNAME (usually present)
          status = 0
          call ftgkys(iunit,'EXTNAME',extname,comm,status)
	  IF(status.NE.0) then
	    extname = 'Undefined'
	    status = 0
	  ENDIF
	    if(chatter.GT.25) then
	       message = '...... EXTNAME = '// extname
	       call fcecho(message)
	    endif 
c	  ... Reset the hduclas array, then attempt to fill from CHU
	  do jj = 1, 9
	    hduclas(jj) = 'Undefined'
	  enddo
          status = 0
   	  call ftgkns(iunit,'HDUCLAS',1,9,hduclas,ngot,status)
c	  ... Go on to next extension if too few values recovered 
	  if(ngot.LT.ninstr) goto 321
	  do jj = 1, ngot
	   call crmvlbk(hduclas(jj))
	  enddo
c	  ... Now compare with (i/p) requested strings
	  qyep = .true.
	  do jj = 1, ninstr
	     if(instr(jj).NE.hduclas(jj)(:clenact(instr(jj))) .AND.
     &		instr(jj)(:1).NE.'*')
     &		qyep = .false.
	  enddo
c	  ... Fill in the necessary if we've found a match
	  IF(qyep) then
	        if(chatter.GT.25) then
	          message = '... Located acceptable extension'
	          call fcecho(message)
	        endif 
      		nfound = nfound + 1
		next(nfound) = iextno
		extnam(nfound) = extname
		do jj = 1, 9
		  outhdu(jj,nfound) = hduclas(jj)
	          if(chatter.GT.30) then
	            write(message,'(a,i1,a,a)') '...... HDUCLAS',jj,
     &				' = ', hduclas(jj)
		    call fcecho(message)
		  endif
		enddo
	  ENDIF
c	  ... Move on the the next extension
321       status = 0
	  call ftmrhd(iunit,1,htype,status)
c	  ... Stop if hit the end of the file
          IF ((status.EQ.107)) THEN
	    if(chatter.GT.25) then
               message = '... end-of-file found'
               call fcecho(message)
	    endif 
	    goto 123
	  endif
	enddo

c Move the pointer back to the original place in i/p file
123	if(iextno.GT.0) then
	   imove = -1*iextno
	   status = 0
           call ftmrhd(iunit,imove,htype,status)
	   if(status.NE.0) then
             message = wrnstr // ' Problem moving back to orig xtens'
             call wt_ferrmsg(status, message)
	     ierr = -1
	   endif
	endif

482     if(ierr.GT.0) then
           message = errstr // ' Incomplete Execution'
           call fcecho(message)
        endif
 


	return
	end
c ------------------------------------------------------------------

