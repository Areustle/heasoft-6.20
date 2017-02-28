      subroutine xgtstr (string, iparse, nreq, desc, ndesc, retstr, 
     &                   nret, iflag, idelim, * , *, *)
c		rashafer 9 March 1986
c	XPARSE subroutine to peel off a requested number of arguements as
c	strings
c	string	c*	i/r: parse string
c	iparse	c*	i/r: parse position
c	nreq	i4	i: no. of strings requested
c	desc	c*(ndesc)	i: description of the string requested
c	ndesc	i4	i: no. of descriptions passed (if = 0 then '?' will
c			not trigger a description)
c	retstr	c*(nreq)	r: the strings actually picked up (no change
c				on skips)
c	nret	i4	r: no. of strings actually processed (where skips
c				and infinite skips are included as processed)
c				if nret not = NREQ then there was a fall off
c				the end of the string, or a special delimeter
c				met (see input value of IDELIM)
c	iflag	i4	r: value of condition flag at last call to XGTARG
c	idelim	i4	i/r: if <= -1  there is no checking for special
c			delimeters, else on return IDELIM contains the value
c			of the delimeter that triggered the return (if 0
c			then a comma, if 1 then special del 1, etc.)
c	Alternate returns:
c 		*1	same as first alternate return of XGTARG (fell off
c				line)
c		*2	same as second alternate return of XGTARG (infinite
c				skip)
c		*3	special delimeter (see IDELIM)
      character*(*) string,desc(*),retstr(*)
      integer iparse,nreq,ndesc,nret,iflag,idelim
c
      include 'xparinc.inc'
c
      integer lenact
      integer jdelim,ibeg,iend,destmp,ierr
      logical qskip

      nret=0
      do while(nret.lt.nreq)
         call xgtarg(string,iparse,ibeg,iend,qskip,iflag,jdelim,*910,
     &               *920,*900)
900      continue
         nret=nret+1
         if(.not.qskip)then
            if((ndesc.gt.0).and.(string(ibeg:iend).eq.inquiry))then
               destmp=min(ndesc,nret)
               xprmsg=desc(destmp)(:lenact(desc(destmp)))//' : ('//
     &                retstr(nret)(:max(1,lenact(retstr(nret))))//')'
               call xinfix(xprmsg(:lenact(xprmsg)+1),string,iparse,ierr)
               nret=nret-1
               if(ierr.eq.1)then
                  iflag=-1
                  return 1
               end if
            else
               retstr(nret)=string(ibeg:iend)
               if((idelim.ge.0).and.(jdelim.ne.0))then
                  idelim=jdelim
                  return 3
               end if
            end if
         end if
      end do
      if((idelim.ge.0))idelim=jdelim
      return
c **  ** alternate returns from xgtarg
c **  ** fell off the end
910   continue
      return 1
c **  ** infinite skip condition
920   continue
      nret = nreq
      return 2
      end
