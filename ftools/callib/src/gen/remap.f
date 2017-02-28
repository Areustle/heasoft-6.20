*+ REMAP
	subroutine remap(chatter, nin, xin_lo, xin_hi, y_in,
     &		nout, xout_lo, xout_hi, y_out, ierr)

	IMPLICIT NONE
	integer nin, nout, chatter, ierr
	real acc
	real xin_lo(nin), xin_hi(nin), y_in(nin)
	real xout_lo(nout), xout_hi(nout), y_out(nout)
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
c  IERR             o: Error flag on return (zero = OK)
c
c Author/Modification History
c  Ian M George  (1.0.0:93 Nov 17)
c  Ian M George  (2.0.0:93 Dec 01) fixed istop bug & added more checks
c  Keith Arnaud  (3.0.0:97 Jul 25) fixed so correctly interpolates
	character(7) version
	parameter (version = '3.0.0')
*- 
c Internals 
	integer i, j
	integer istart,istop
	real din, dout
	character(80) message
	character(30) errstr, wrnstr
c Initialize
	ierr = 0
	wrnstr = '*** REMAP '//version//' WARNING:'
	errstr = '*** REMAP '//version//' ERROR:'


c Inform debugging users
	if(chatter.GT.30) then
	  message = ' ... using REMAP version'//version
	  call fcecho(message)
	endif

c Find the start and stops of the i/p array
	do j = 1, nin
	   if(xin_lo(j).GT.xout_lo(1)) then
		istart = j - 1
		goto 654	
	   endif
	enddo
654	continue

	if(chatter.GT.35) then
	  write(message,'(a,i12)') ' ...... i/p grid start bin:', istart
	  call fcecho(message)
	endif


	istop = nin
	do j = 1, nin
	   if(xin_lo(j).GE.xout_hi(nout)) then
		istop = j - 1
		goto 987
	   endif
	enddo
987 	continue

	if(chatter.GT.35) then
	  write(message,'(a,i12)') ' ...... i/p grid stop bin:', istop
	  call fcecho(message)
	endif

c ... check it's all sensible
	if(istop.LT.istart) then
	  message = errstr // ' No overlap/error in remapping'
	  call fcecho(message)
	  ierr = 1
	  return
	endif

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
		  din = xin_hi(j) - xin_lo(j)
		  y_out(i) = y_out(i) + y_in(j)*din
c ... o/p fully enclosed within i/p
		elseif(xin_lo(j).LE.xout_lo(i) .AND.
     &			xin_hi(j).GE.xout_hi(i)) then
	   	  din = xout_hi(i) - xout_lo(i)
		  y_out(i) = y_out(i) + y_in(j)*din
c ... i/p overlaps lower bound of o/p
		elseif(xin_lo(j).LE.xout_lo(i)) then
	   	  din = xin_hi(j) - xout_lo(i)
		  y_out(i) = y_out(i) + y_in(j)*din
c ... i/p overlaps upper  bound ff o/p
		elseif(xin_lo(j).LE.xout_hi(i)) then
	   	  din = xout_hi(i) - xin_lo(j)
		  y_out(i) = y_out(i) + y_in(j)*din
		else
		   message = errstr // ' Problem rebinning '
		   call fcecho(message)
		   ierr = 1
		   return
		endif
456	   continue
           enddo

123	   continue
           dout = xout_hi(i) - xout_lo(i)
           IF ( dout .NE. 0. ) y_out(i) = y_out(i) / dout

        enddo

	return
	end


