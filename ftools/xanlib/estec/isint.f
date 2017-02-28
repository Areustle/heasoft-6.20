*- isint - identify decimal integer string
* 11 November 1992 : AMTP
* 16 November 1992 : don't forget possible leading '+' or '-'
      logical*4 function isint(s)
* Import :
      character*(*) s
* Local variables :
      integer*4 js
      integer*4 ls
* External references :
      logical*4 isdigit
      integer*4 lenact
*-
      ls=lenact(s)
      if(ls.eq.0)then
         isint=.false.
      else
         isint=isdigit(s(1:1)).or.(s(1:1).eq.'+').or.(s(1:1).eq.'-')
         js=1
         do while((js.lt.ls).and.isint)
            js=js+1
            isint=isdigit(s(js:js))
         end do
      endif
      return
      end
