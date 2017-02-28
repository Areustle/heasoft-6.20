      subroutine streval(instr, varnames, varvals, numvar, outstr,
     &                   status)
      implicit none
c
c  Evaluate string, expanding variables denoted %variable% using
c  list of varnames
c  
c  I  instr    (s)  String to evaluate
c  I  varnames (s)  List of variables names
c  I  varvals  (s)  List of variables values
c  I  numvar   (s)  Number of variables
c  O  outstr   (s)  Evaluated string
c  O  status   (i)  Error flag (0=OK)
c
      character*(*) instr, outstr
      character*(*) varnames(*), varvals(*)
      integer*4 numvar, status
c
c  Local variables
c
      integer*4 ibeg, obeg, i, j
      integer*4 inlen, outlen, vallen, LENACT
      character(1) ch, dc
      character(10) var

      status = 0

      ch = '%'
      outstr = ' '
      
      ibeg = 1
      obeg = 1
      inlen = LENACT(instr)
      outlen = LEN(outstr)
      do while ( ibeg.le.inlen )
         call findchr(instr, ibeg, inlen, ch, j, dc, status)
         if ( status.ne.0 ) j = inlen + 1
         if ( j-ibeg+obeg-1.gt.outlen ) then
            status = -1
            call XWRITE(' streval: string overflow', 5)
            return
         endif
c
c  Copy string up to variable
c
         outstr(obeg:) = instr(ibeg:j-1)
         obeg = obeg + j - ibeg
         ibeg = j + 1
         if ( status.eq.0 ) then
            call findchr(instr, ibeg, inlen, ch, j, dc, status)
            if ( status.ne.0 ) then
               call XWRITE(' streval: variable parse error', 5)
               return
            endif
            var = instr(ibeg:j-1)
            ibeg = j + 1
c
c  Two variable delimeters together evaluates to one
c
            if ( var.eq.' ' ) then
               outstr(obeg:obeg) = ch
               obeg = obeg + 1
            else
               call matchkey(var, varnames, numvar, i, status)
               if ( status.ne.0 ) then
                  call XWRITE(' streval: variable undefined', 5)
                  return
               endif
               vallen = LENACT(varvals(i))
               if ( obeg+vallen-1.gt.outlen ) then
                  status = -1
                  call XWRITE(' streval: string overflow', 5)
                  return
               endif
c
c  Insert variable value
c
               outstr(obeg:) = varvals(i)(:vallen)
               obeg = obeg + vallen
            endif
         endif
         status = 0
  200 enddo
          
      return
      end
