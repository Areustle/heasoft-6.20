*+ RMAP1D
	subroutine rmap1d(chatter, nin, xin_lo, xin_hi, y_in,
     &		nout, xout_lo, xout_hi, y_out, mode, acc, ierr)

	IMPLICIT NONE
	integer nin, nout, chatter, ierr
	integer mode
	real acc
	real xin_lo(*), xin_hi(*), y_in(*)
	real xout_lo(*), xout_hi(*), y_out(*)
c
c Description
c  Remaps (interpolates/extrapolates) user-supplied 1-d array (y_in) binned 
c on a grid specified by the lower & upper bin boundaries (xin_lo & xin_hi) 
c onto a new user-specified grid with bin boundaries xout_lo & xout_hi, 
c with the output written to y_out.
c
c Passed Parameters
c  CHATTER        i  : Chattiness flag (>20 increasing verbose)
c  NIN            i  : No. bins in i/p grid
c  XIN_LO         i  : Array of Lower boundary of bins in i/p grid
c  XIN_HI         i  : Array of Upper boundary of bins in i/p grid
c  Y_IN		  i  : Array to be remapped
c  NOUT           i  : No. bins in o/p grid
c  XOUT_LO        i  : Array of Lower boundary of bins in o/p grid
c  XOUT_HI        i  : Array of Upper boundary of bins in o/p grid
c  Y_OUT	    o: Remapped array 
c  MODE           i  : Mode in which calc to be performed
c			MODE = 1 standard mode
c			MODE = 2 remap in /delta-x space
c			MODE = 3 remap in *delta-x space
c  ACC            i  : Fractional accuracy in region of overlap between the 
c			grid, which if not obtained warning messages will 
c			be written (and IERR set to bad)
c  IERR             o: Error flag on return (zero = OK)
c
c Author/Modification History
c  Ian M George  (1.0.0:94 Oct 24) copied & updated from REMAP.F
c  Ian M George  (1.0.1:95 Jul 10) passed arrays made (*)
c  M Tripicco    (1.0.2:97 Jan 03) added istart initialization
c  M Tripicco    (1.0.3:03 Aug 14) fixed istart initialization!
c 
	character(7) version
	parameter (version = '1.0.3')
*- 

c Internals 
	integer i, j
	integer istart,istop
	real sum_in, sum_out, xslo, xshi
	real din,dout, chk
	character(80) message
	character(30) errstr, wrnstr
c Initialize
	sum_in = 0
	sum_out = 0
	din = 0
	dout = 0
	xslo = 0
	xshi = 0
	ierr = 0
	wrnstr = '*** RMAP1D '//version//' WARNING:'
	errstr = '*** RMAP1D '//version//' ERROR:'

c Inform debugging users
	if(chatter.GT.30) then
	  message = ' ... using RMAP1D '//version
	  call fcecho(message)
	endif
c Check for sillies
	if(nin.LE.0) then
	   message = errstr // ' Incompatible no. elements'
	   call fcecho(message)
	   write(message,'(a,i12)')
     &		' ...... No. bins in i/p grid = ', nin
	   call fcecho(message)
	   ierr = 1
	   goto 995
	endif
	if(nout.LE.0) then
	   message = errstr // ' Incompatible no. elements'
	   call fcecho(message)
	   write(message,'(a,i12)')
     &		' ...... No. bins in o/p grid = ', nout
	   call fcecho(message)
	   ierr = 1
	   goto 995
	endif

c Find the start and stops of the i/p array
	istart = nin
	do j = 1, nin
	   if(xin_lo(j).GT.xout_lo(1)) then
	      if(j.gt.1) then
		istart = j - 1
	      else
		istart = 1
	      endif
	      goto 654	
	   endif
	enddo
654	dout = xin_hi(istart) - xout_lo(1)
	din = xin_hi(istart) - xin_lo(istart)

	if(din.NE.0.0) then
	  xslo = y_in(istart)*(1-dout/din) 
	else 
	  xslo = 0
	endif
	if(chatter.GT.35) then
	 write(message,'(a,i12)') ' ...... i/p grid start bin:', istart
	 call fcecho(message)
	 write(message,'(a,f10.5)') ' ...... start bin lo excess: ',xslo
	 call fcecho(message)
	endif


	istop = nin
	do j = 1, nin
	   if(xin_lo(j).GE.xout_hi(nout)) then
	      if(j.gt.1) then
		istop = j - 1
	      else
	        istop = 1
	      endif
	      goto 987
	   endif
	enddo
987 	dout = xin_hi(istop) - xout_hi(nout)
	din = xin_hi(istop) - xin_lo(istop)

	if(din.NE.0.0) then
	  xshi = y_in(istop)*(dout/din) 
	else 
	  xshi = 0
	endif
	if(chatter.GT.35) then
	  write(message,'(a,i12)') ' ...... i/p grid stop bin:', istop
	  call fcecho(message)
	  write(message,'(a,f10.5)') ' ...... stop bin hi excess: ', xshi
	  call fcecho(message)
	endif

c ... check it's all sensible
	if(istop.LT.istart) then
	  message = errstr // ' No overlap/error in remapping'
	  call fcecho(message)
	  ierr = 1
	  goto 995
	endif

c ... Fix up the i/p array if necessary prior to remapping
	if(mode.EQ.1) then
	   goto 623
	elseif(mode.EQ.2) then
	  do i = 1, nin
	    din = xin_hi(i) - xin_lo(i)
	    y_in(i) = y_in(i)/din
	  enddo
	  xslo = xslo/(xin_hi(istart)-xin_lo(istart))
	  xshi = xshi/(xin_hi(istop)-xin_lo(istop))
	elseif(mode.EQ.3) then
	  do i = 1, nin
	    din = xin_hi(i) - xin_lo(i)
	    y_in(i) = y_in(i)*din
	  enddo
	  xslo = xslo*(xin_hi(istart)-xin_lo(istart))
	  xshi = xshi*(xin_hi(istop)-xin_lo(istop))
	else
	   message = errstr// ' Unrecognized MODE'
	   call fcecho(message)
	   ierr = 99
	   goto 995
	endif
623	continue

c ... Perform the remapping
	do i = 1, nout
	   y_out(i) = 0.
	   do j = istart,istop
		if(xin_lo(j).GT.xout_hi(i)) goto 123
		if(xin_hi(j).LT.xout_lo(i)) goto 456
		if(xin_hi(j).EQ.xin_lo(j)) goto 456
		if(y_in(j).EQ.0.0) goto 456
c ... i/p fully enclosed within o/p
		if(xin_lo(j).GE.xout_lo(i) .AND.
     &			xin_hi(j).LE.xout_hi(i)) then
		  y_out(i) = y_out(i) + y_in(j)
c ... o/p fully enclosed within i/p
		elseif(xin_lo(j).LE.xout_lo(i) .AND.
     &			xin_hi(j).GE.xout_hi(i)) then
	          dout = xout_hi(i) - xout_lo(i)
		  din = xin_hi(j) - xin_lo(j)
		  if(din.EQ.0.0) goto 456
		  y_out(i) = y_out(i) + y_in(j)*(dout/din)
c ... i/p overlaps lower bound of o/p
		elseif(xin_lo(j).LE.xout_lo(i)) then
	   	  dout = xin_hi(j) - xout_lo(i)
		  din = xin_hi(j) - xin_lo(j)
		  if(din.EQ.0.0) goto 456
		  y_out(i) = y_out(i) + y_in(j)*(dout/din)
c ... i/p overlaps upper  bound ff o/p
		elseif(xin_lo(j).LE.xout_hi(i)) then
	   	  dout = xout_hi(i) - xin_lo(j)
		  din = xin_hi(j) - xin_lo(j)
		  if(din.EQ.0.0) goto 456
		  y_out(i) = y_out(i) + y_in(j)*(dout/din)
		else
		   message = errstr // ' Problem rebinning '
		   call fcecho(message)
		   ierr = 1
		   goto 995
		endif
456	   enddo
123	enddo

	do j = istart,istop
	   if(xin_hi(j).NE.xin_lo(j)) then
     	   		sum_in = sum_in + y_in(j)
	   endif
	enddo
	sum_in = sum_in - xslo - xshi

	do i = 1, nout
	   sum_out = sum_out + y_out(i)
	enddo


c conservation check
	if((sum_in+sum_out).NE.0.0) then
	  chk = ABS((sum_in - sum_out)/(sum_in+sum_out)) 
	else
	  chk = ABS(sum_in - sum_out)
	endif

	if( chk .GT. acc) then
	  write(message,'(a,a,f10.5)') wrnstr, 
     &		' Rebinning accuracy less than', acc
	  call fcecho(message)
	  ierr = -1

	  write(message,'(a,f10.5)') 
     &		' ... sum of i/p array in overlap region ',sum_in 
	  call fcecho(message)
	  write(message,'(a,f10.5)') 
     &		' ... sum of o/p array                   ',sum_out
	  call fcecho(message)
	  if(ABS(sum_out).GT.1E-30) then
	     write(message,'(a,f10.5)') 
     &		' ... ratio sum_in/sum_out', sum_in/sum_out
   	  else
	    message = ' ... ratio sum_in/sum_out = Infinity'
	  endif
	  call fcecho(message)
	endif


995	continue

c ... Reset the i/p array if necessary
	if(mode.EQ.2) then
	  do i = 1, nin
	    din = xin_hi(i) - xin_lo(i)
	    y_in(i) = y_in(i)*din
	  enddo
	  do i = 1, nout
	    dout = xout_hi(i) - xout_lo(i)
	    y_out(i) = y_out(i)*dout
	  enddo
	elseif(mode.EQ.3) then
	  do i = 1, nin
	    din = xin_hi(i) - xin_lo(i)
	    y_in(i) = y_in(i)/din
	  enddo
	  do i = 1, nout
	    dout = xout_hi(i) - xout_lo(i)
	    y_out(i) = y_out(i)/dout
	  enddo
	endif


	return
	end
