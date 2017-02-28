*+BILINT
        subroutine bilint(chatter, x_in, y_in,
     &                  max_ix,max_iy,
     &                  nix, niy, X, Y, image,
     &                  qextrap, value, ierr)

	IMPLICIT NONE
	integer chatter, ierr
	integer nix, niy, max_ix, max_iy
	real x(*), y(*), image(max_ix,max_iy)
	real x_in, y_in, value
	logical qextrap

c Description:
c  This routine performs a bilinear interpolation on the values stored 
c in the passed array "image" (which has grid points stored in the "X" & "Y"
c arrays) returning the result ("value") at the passed position "x_in, y_in".
c  The passed boolean, "qextrap", controls whether extrapolations are performed
c when the position "x_in, y_in" is outside the "X,Y" grid.
c
c Passed parameters
c  CHATTER       i   : chattiness flag for o/p in case of error
c			(5 quite,9 normal,>20 silly)
c  X_IN          i   : X-coord of requested point
c  Y_IN          i   : Y-coord of requested point
c  MAX_IX        i   : Size of 1st dimension of IMAGE array
c  MAX_IY        i   : Size of 2nd dimension of IMAGE array
c  NIX    	 i   : Number of elements in X array
c  NIY    	 i   : Number of elements in Y array
c  X	     	 i   : Array of "x-axis" grid points for IMAGE array
c  Y	     	 i   : Array of "y-axis" grid points for IMAGE array
c  IMAGE	 i   : 2D array within which interpolation is to be performed
c  QEXTRAP       i   : Logical flag has to whether extrapolations are performed
c  IERR            o : Error flag (0 = OK)
c
c Called Routines:
c
c Compilation & Linking
c  link with CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0; 1994 Aug 09), 
	character(7) version
	parameter (version = '1.0.0')
*- 
c Internals
	integer i, j
	integer ilo, ihi, jlo, jhi
	real frac_x, frac_y
	real temp1, temp2
	character(80) message
        character(25)  errstr, wrnstr
	logical qx_out, qy_out
c Initialize
	ierr = 0
	ilo = 0
	ihi = 0
	jlo = 0
	jhi = 0
	qx_out = .false.
	qy_out = .false.
        errstr = '** BILINT '//version//' ERROR: '
        wrnstr = '** BILINT '//version//' WARNING: '
	
c Check whether outside range in oreder to return quickly

	if((x_in.LT.X(1)).OR.(x_in.gt.X(nix)))then
		qx_out = .true.
		if(.NOT.qextrap) then
		  if(chatter.gt.20) then
			message = errstr //
     &		' axis-1 coord outside range: extrap not allowed'
		        call fcecho(message)
		  endif
		  ierr = -1
		  return
		endif
	endif
	if((y_in.LT.Y(1)).OR.(y_in.gt.Y(niy)))then
		qy_out = .true.
		if(.NOT.qextrap) then
		  if(chatter.gt.20) then
			message = errstr //
     &		' axis-2 coord outside range: extrap not allowed'
		        call fcecho(message)
		  endif
		  ierr = -1
		  return
		endif
	endif
	
c INTERPOLATION CASE
	if((.NOT.qx_out).AND.(.NOT.qy_out)) then
	   do i = 2, nix
	      if((x_in.GE.X(i-1)).AND.(x_in.LE.X(i))) then
		ilo = i - 1
		ihi = i
		goto 123
	      endif
	   enddo
	   message = errstr //
     &          ' Unable to locate 1st coord grid point'
           call fcecho(message)
           ierr = 2
           return
123	   do j = 2, niy
	      if((y_in.GE.Y(j-1)).AND.(y_in.LE.Y(j))) then
		jlo = j - 1
		jhi = j
		goto 666
	      endif
	   enddo
	   message = errstr //
     &          ' Unable to locate 2nd coord grid point'
           call fcecho(message)
           ierr = 2
           return
	endif

c EXTRAPOLATION CASE
	if(x_in.LT.X(1))then
	        ilo = 1
		ihi = 2
	elseif(x_in.gt.X(nix))then
	        ilo = nix-1
		ihi = nix
	endif
	if(y_in.LT.Y(1))then
	        jlo = 1
		jhi = 2
	elseif(y_in.gt.Y(niy))then
	        jlo = niy-1
		jhi = niy
	endif

c DO THE BUSINESS
666	   frac_x = (x_in - X(ilo))/(X(ihi) - X(ilo))
	   frac_y = (y_in - Y(jlo))/(Y(jhi) - Y(jlo))
	   temp1 = image(ilo,jlo) + 
     &		frac_x*(image(ihi,jlo) - image(ilo,jlo))
	   temp2 = image(ilo,jhi) + 
     &		frac_x*(image(ihi,jhi) - image(ilo,jhi))
	   value = temp1 + 
     &		frac_y*(temp2 - temp1)


	return
	end

