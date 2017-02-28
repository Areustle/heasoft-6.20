      subroutine matchfunc (Value, Funcs, Nfuncs, Ipos, Status)
      implicit none
c
c  Matches functions of form: FUNC(args) 
c  If function in list, index of function returned as Ipos
c   also function syntax will be stripped from Value leaving only
c   the arguments to the function
c  If function is not listed or Value is not in that form,
c   a negative status is returned and the Value is left untouched.
c
c I/O value  (c)  Entered value
c  I  funcs  (c)  Array of possible values
c  I  nfuncs (i)  Number of possible values
c  O  ipos   (i)  Position of matching function
c I/O status (i)  Status flag ( 0 = OK )
c
      character*(*) Value
      integer Nfuncs, Ipos, Status
      character*(*) Funcs(Nfuncs)
c
c  Local variables
c
      integer ipar, len, LENACT
      character(1) ch

      ipos = 0
      len = LENACT(Value)
      call findchr(Value, 1, len, '(', ipar, ch, Status)
      if ( Status.eq.0 .and. Value(len:len) .ne. ')' ) Status = -2
      if ( Status.ne.0 ) return

      call matchkey (Value(1:ipar-1), Funcs, Nfuncs, Ipos, Status)
      if ( Status.ne.0 ) return

      Value = Value(ipar+1:len-1)

      return
      end
