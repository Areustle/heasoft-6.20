*- isdouble.for - identify decimal double precision floating point string
* 3 May 1993 : AMTP
      logical*4 function isdouble(string)
* Import :
      character*(*) string
* Local variables :
      character(30) s
      integer*4 jp
      integer*4 jd
      integer*4 ls
* External references :
      logical*4 isint
      integer*4 lenact
*-
      s=string
      ls=lenact(s)
      if(ls.eq.0)then
         isdouble=.false.
      else
         call upc(s)
         if((s(1:1).eq.'+').or.(s(1:1).eq.'-'))s=s(2:)
         jp=index(s,'.')
         jd=index(s,'D')
         if((jp.gt.0).and.(jp+1.eq.jd))then
            s(jp:)=s(jd:)
            jp=0
         endif
         if(jd.eq.0)then
            if(jp.eq.0)then
               isdouble=.false.
            else if(jp.eq.1)then
               isdouble=isint(s(2:))
            else if(isint(s(:jp-1)))then
               isdouble=isint(s(jp+1:)).or.(s(jp+1:).eq.' ')
            else
               isdouble=.false.
            endif
         else if(.not.isint(s(jd+1:)).or.(jd.le.jp+1))then
            isdouble=.false.
         else
            if(jp.eq.0)then
               isdouble=isint(s(:jd-1))
            else if(jp.eq.1)then
               isdouble=isint(s(2:jd-1))
            else
               isdouble=isint(s(:jp-1)).and.isint(s(jp+1:jd-1))
            endif
         endif
      endif
      return
      end
