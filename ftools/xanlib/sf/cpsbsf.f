	subroutine cpsbsf (inunit, outunit, nsub, buffer, buflen,
     &	 qterm, ierrsf)
c		rashafer  12 Jan 1986
c	SF subroutine to copy one or more subsidiary records
c
	integer*4 inunit	
c  i: unit opened to the source SF file
	integer*4 outunit	
c  i: unit opened to the output SF File, the
				
c 	header must already be written
	integer*4 nsub		
c  i: no. of subsidary records to be written.
				
c 	If < 0 then all the records written
				
c 	until the end of the package.
	character(1) buffer(*)		
c  w: workspace for transfer
	integer*4 buflen	
c  i: The maximum size of the buffer. N.B.
				
c 	The number used is NOT returned
	logical*4 qterm		
c  i:  If true, then a terminal record is
				
c 	automatically written after this set
				
c 	(see TERMSF and the NSUB argument above)
	integer*4 ierrsf	
c  i/r: Error flag (see RSUBSF for error codes).
				
c 	22 - premature end of package/eof
c
	integer*4 lentmp, isub
	logical*4 qdone
	qdone = .false.
	isub = 0
	do while (.not. qdone)
	    lentmp = buflen
	    call rsubsf (inunit, buffer, lentmp, ierrsf)
	    isub = isub + 1
	    qdone = isub.eq.nsub   
c N.B. This only true for NSUB > 1 cases
	    if((ierrsf.eq.0).and.(lentmp.gt.0))then
		call wsubsf (outunit, buffer, lentmp, ierrsf)
	    else
		qdone=.true.
		if((nsub.gt.0).and.((ierrsf.eq.7) .or. (lentmp.le.0))) then
		    if(ierrsf.eq.0)then
			write(*,*)' *ERROR*:CPSBSF: Premature end of package:'
			end if
		    ierrsf = 22
		    end if
		end if
	    end do
	if(qterm)call termsf(outunit, ierrsf)
	return
	end
