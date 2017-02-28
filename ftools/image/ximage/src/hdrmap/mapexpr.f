      subroutine mapexpr(expr, arg1, arg2, mode, status)
      implicit none
c
c  Evaluate simple expression for map operation
c
c I/O expr   (s)  Map arithmetic expression
c  O  arg1   (s)  First argument
c  O  arg2   (s)  Second argument
c  O  mode   (i)  1=+ 2=- 3=* 4=/
c  0  status (i)  Error flag (0=OK)
c
c  VALID SYNTAX:
c  
c  MAP op MAP
c  MAP op CONST
c  CONST op MAP
c  opfunction(MAP)
c
c  RULES:
c
c  1. Spaces between +-/*= and words are ignored
c
c  2. Valid ops are +,-,*,/
c
c  3. Valid opfunctions are int, float
c
c
      character*(*) expr, arg1, arg2
      integer mode, status
c
c  Local variables
c
      integer i, ibeg, iend, slen, LENACT
      character(100) ds
      character(10) chrlist
      character(1) ch

      chrlist = '+-*/()'
      status = 0
c
c  Remove spaces from a copy
c
      call rmvblk(expr)
      slen = LENACT(expr)
c
c  Find first argument
c
      ibeg = 1
      iend = slen

      call findchr(expr, ibeg, iend, chrlist, i, ch, status)
      if ( status.ne.0 ) then
         call xwrite(' Expression contains no operations', 10)
         status = -1
         return
      endif
c
c  Check for exponential number case
c
      if ( ch.eq.'+' .or. ch.eq.'-' ) then
         if ( expr(i-1:i-1).eq.'e' .or. expr(i-1:i-1).eq.'E' ) then
            ibeg = i + 1
            call findchr(expr, ibeg, iend, chrlist, i, ch, status)
         endif
      endif
      if ( status.ne.0 ) then
         call xwrite(' Expression contains no operations', 10)
         status = -1
         return
      endif
      arg1 = expr(1:i-1)
c
c  Determine operation
c
      if ( ch.eq.'+' ) then
         mode = 1
      elseif ( ch.eq.'-' ) then
         mode = 2
      elseif ( ch.eq.'*' ) then
         mode = 3
      elseif ( ch.eq.'/' ) then
         mode = 4
      elseif ( ch.eq.'(' ) then
         ds = arg1
         call upc(ds)
         if ( ds.eq.'INT' ) then
            mode = 5
         elseif ( ds.eq.'FLOAT' ) then
            mode = 6
         else
            call xwrite(' Invalid map function: use int() or float()', 
     &                  10)
            status = -1
            return
         endif
      else
         call xwrite(' Invalid operation: use +,-,*,/,int(),float()',
     &                10)
         status = -1
         return
      endif
c
c  Use rest as second argument
c
      if ( mode.eq.5. .or. mode.eq.6 ) then
         if ( expr(slen:slen).ne.')' ) then
            call xwrite(' Expression missing ")" ', 10)
            status = -1
            return
         endif
         arg2 = expr(i+1:slen-1)
      else
         arg2 = expr(i+1:slen)
      endif
         
      return
      end
