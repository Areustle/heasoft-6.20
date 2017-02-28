	subroutine xasthn (astarg)
c		rashafer 9 feb 1987
c		This subroutine is used in the ATTN catcher routines, mainly
c		to get around a stupidity of Fortran.  It just calls another
c		entry point of XSTATN, where the action all is.
	character(1) astarg(*)
	logical*4 xastft
	logical*4 dummy
	dummy = xastft(astarg)
	return
	end
