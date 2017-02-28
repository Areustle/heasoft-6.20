      subroutine xgtnum (string, iparse, nreq, desc, ndesc,
     &                 	 valmin, valmax, nrange, numtype, retval, 
     &                   nret, iflag,idelim, * , *, *)
c		rashafer 9 March 1986
c		modified from xgtstr 14 april 1986
c	XPARSE subroutine to peel off a requested number of arguements as
c	general numbers.
c	string	c*	i/r: parse string
c	iparse	c*	i/r: parse position
c	nreq	i4	i: no. of strings requested
c	desc	c*(ndesc)	i: description of the string requested
c	ndesc	i4	i: no. of descriptions passed
c	valmin	***	i: the minimum allowed value for the ith string
c	valmax	***	i: the maximum allowed value for the ith string
c	nrange	i4	i: the number of min-max ranges (if <=0 then no
c			range checking is performed)
c	numtype	i4	i: index indicating the type of value requested:
c			= 3 for integer*4.
c			= 4 for real*4
c	retval	***	r: the values actually picked up (no change
c				on skips)
c	nret	i4	r: no. of strings actually processed (where skips
c				and infinite skips are included as processed)
c				if nret not = NREQ then there was a fall off
c				the end of the string, or a special delimeter
c				met (see input value of IDELIM)
c	iflag	i4	r: value of condition flag at last call to XGTARG (or
c				to XINFIX for EOFs). A -2 indicates an EOF
c				after an XINFIX.
c	idelim	i4	i/r: if <= -1  there is no checking for special
c			delimeters, else on return IDELIM contains the value
c			of the delimeter that triggered the return (if 0
c			then a comma, if 1 then special del 1, etc.)
c	Alternate returns:
c 		*1	same as first alternate return of XGTARG (fell off
c				line) OR EOF
c		*2	same as second alternate return of XGTARG (infinite
c				skip)
c		*3	special delimeter (see IDELIM)
c
c **	** N.B. values in the above list indicated as type *** are actually
c **	** arrays of the kind indicated by NUMTYPE, although in this routine
c **	** they are declared as byte arrays.
      character*(*) string,desc(*)
      integer*4 iparse,nreq,ndesc,nret,iflag,idelim,nrange,numtype
      integer*4 valmin(0:*),valmax(0:*),retval(0:*)
c
      include 'xparinc.inc'
c
      integer*4 lenact, jdelim, ibeg, iend, ierr
      integer*4 offset,curdesc,range,rangeoff
      logical*4 qskip,isnum,inrange
      integer*4 ntype
      parameter(ntype=4)
      integer*4 multi(ntype)
      logical*4 xtsti4,xpari4,xtstr4,xparr4
      data multi/-1,-1,1,1/

      if((numtype.le.0).or.(numtype.gt.ntype).or.
     &    (multi(ntype).le.0))then
         write(*,*)
     &	  '*ERROR*:XPARSE:XGTNUM: unsupported numerical type',numtype
         return
      end if
      nret=0
      do while(nret.lt.nreq)
         call xgtarg(string,iparse,ibeg,iend,qskip,iflag,jdelim,*910,
     &               *920)
         offset=multi(numtype)*nret
         nret=nret+1
         curdesc = min(ndesc, nret)
         if(.not.qskip)then
            goto (100,100,130,140) numtype
100         continue
            return
c **        ** integer*4
130         continue
            isnum = xpari4(string(ibeg:iend), curdesc,
     &              desc(curdesc), retval(offset))
            goto 200
c **        ** real*4
140         continue
            isnum = xparr4(string(ibeg:iend), curdesc,
     &              desc(curdesc), retval(offset))
            goto 200
c **        ** end of the case (numtype)
c200         continue
         else
            isnum=.true.
         end if
c
c Ziqin Pan, June 20, 2005
c Move '200 continue' out of if-then-endif block.
c
200         continue
         if(isnum.and.(nrange.gt.0))then
            range = min(nret,nrange)
            rangeoff = (range-1)*multi(numtype)
            goto (200,200,230,240),numtype
c **        ** integer*4
230         inrange = xtsti4(retval(offset),valmin(rangeoff),
     &                valmax(rangeoff),curdesc, desc(curdesc))
            isnum = inrange
            goto 300
c **        ** real*4
240         continue
            inrange = xtstr4(retval(offset),valmin(rangeoff),
     &                valmax(rangeoff), curdesc, desc(curdesc))
            isnum = inrange
            goto 300
c **        ** end of case (numtype)
C300         continue
         else
            inrange = .true.
         end if
C Ziqin Pan, June 20, 2005
C Move '300 continue' out of if-then-endif block.
c
300         continue
         if(.not.isnum)then
            call xinfix(xprmsg(:lenact(xprmsg)+1),string,iparse,ierr)
            nret=nret-1
            if(ierr.ne.0)then
               if(.not.inrange) then
                  goto (300,300,330,340),numtype
c **              ** i4
330               continue
                  call xseti4(valmin(rangeoff),retval(offset))
                  goto 400
c **              ** r4
340               continue
                  call xsetr4(valmin(rangeoff),retval(offset))
                  goto 400
c **              ** end case numtype
400               continue
               end if
               iflag=-2
               return 1
            end if
         elseif((idelim.ge.0).and.(jdelim.gt.0))then
            idelim=jdelim
            return 3
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
