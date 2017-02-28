*- isfloat.for - identify decimal floating point string
* 3 May 1993 : AMTP
      logical*4 function isfloat(string)
* Import :
      character*(*) string
* Local variables :
      character(30) s
      integer*4 jp
      integer*4 je
      integer*4 ls
* External references :
      logical*4 isint
      integer*4 lenact
*-
      s=string
      ls=lenact(s)
      if(ls.eq.0)then
         isfloat=.false.
      else
         call upc(s)
         if((s(1:1).eq.'+').or.(s(1:1).eq.'-'))s=s(2:)
         jp=index(s,'.')
         je=index(s,'E')
         if((jp.gt.0).and.(jp+1.eq.je))then
            s(jp:)=s(je:)
            jp=0
         endif
         if(je.eq.0)then
            if(jp.eq.0)then
               isfloat=.false.
            else if(jp.eq.1)then
               isfloat=isint(s(2:))
            else if(isint(s(:jp-1)))then
               isfloat=isint(s(jp+1:)).or.(s(jp+1:).eq.' ')
            else
               isfloat=.false.
            endif
         else if(.not.isint(s(je+1:)).or.(je.le.jp+1))then
            isfloat=.false.
         else
            if(jp.eq.0)then
               isfloat=isint(s(:je-1))
            else if(jp.eq.1)then
               isfloat=isint(s(2:je-1))
            else
               isfloat=isint(s(:jp-1)).and.isint(s(jp+1:je-1))
            endif
         endif
      endif
      return
      end
