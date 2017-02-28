      subroutine xprrrg(string, nrange_in, rngnam, descr,
     &     rnglw, rnghi, rngmn, rngmx, idefrg, qcor, qsing, iflag)
      integer   nrange_in, iflag
      character string*(*), rngnam(nrange_in)*(*), descr*(*)
      real      rnglw(nrange_in), rnghi(nrange_in)
      real      rngmn(nrange_in), rngmx(nrange_in)
      integer   idefrg(nrange_in)
      logical   qcor, qsing
C---
C Subroutine to parse an argument as several real ranges.
C Compound ranges are separted by ':'s, the min and max range are
C separated by a '-' (after the first character) a single * indicates
C that the previous value for min,max (or both) are to be maintained. 
C A ** indicates that the min, max (or both) are to take the extrema
C allowed values.
C---
C XPRRRG    O  If true, then a range was sucessfully parsed.
C              If false, then no such parsing was possible.
C string  I    string to be parsed for a general range.
C              N.B., this string must already by extracted
C              from the parse string.  No corrections to
C              the input are made to this string.
C nrange_in I  number of ranges
C rngnam  I    id of ranges
C descr   I    description of all ranges
C rnglw   I/O  returned low end of range (not modified
C              unless xprrng is .TRUE. on return).
C rnghi   I/O  returned hi end of range (see rnGLW).
C rngmn   I    allowed miminum
C rngmx   I    allowed maximum.  If rngmx(i)<rngmn(i),
C              then no min/max checking is done for that range.
C idefrg  I    the default ranges if fewer than nrange
C              ranges are input (low values indicate higher
C              priority).  if idefrg(1) is < 0, then the
C              significance of the
C              subranges are handled in strict order of
C              left to right.  If = 0, then the order is
C              from right to left.
C qcor    I    If true, then corrections for individual
C              ranges may be input (but the entire argument
C              is never replaced).
C qsing   I    If true, then only single entrys for each
C              sub-range are allowed (e.g. "1:4:3:2").  If
C              false, then range values are allowed ("1-4").
C              If true only rnGLW is modified.
C iflag     O  0 - string correctly parsed as a set of
C              ranges.
C              -1 - Eof during the parsing of a correction
C               2 - String was not a range.
C---
C 1989-Jun-09 - Modified from xprrng [kaa]
C---
      include 'xparinc.inc'
      character(1) colon,star
      character(2) doublestar
      parameter(colon=':',star='*',doublestar='**')
      integer*4 maxrng
      parameter (maxrng=10)
C
      real*4 extrem, rvalue
      character(30) substr, rngstr
      character(15) type
      logical*4 qdash,qrange,qparin,qint
      logical*4 xisnum
      integer*4 ivalue, jflag
      integer*4 nrange
      real*4 tmprng(2,maxrng)
      integer*4 icol, i, lnam, lenact, lact
      integer*4 irange, jrange, irb, ire, lrng, lc, idash
      integer*4 iparse, lmsg, lmsgp, nret
C---
      iflag=2
      if(string.eq.inquiry) then
         if(qcor) then
            xprmsw=' Range of '//descr
            call xwrtpr(xprmsw)
            icol=2
            xprmsw(2:)='('
            do i=1,nrange_in
               lnam=lenact(rngnam(i))
               xprmsw(icol+1:icol+lnam)=rngnam(i)(:lnam)
               icol=icol+lnam+1
               xprmsw(icol:icol)=colon
               if((icol.gt.60).and.(i.lt.nrange_in)) then
                  call xwrtpr(xprmsw)
                  icol=1
               end if
            end do
            if(icol.eq.1) then
               icol=2
            end if
            xprmsw(icol:icol)=')'
            call xwrtpr(xprmsw)
         end if
         return
      end if
      lact=lenact(string)
      icol=1
      irange=0
C** Count the number of ranges in the parse string,
C** using the number of ':' separators
      do while((icol.gt.0).and.(icol.le.lact))
         irange=irange+1
         icol=index(string(icol:lact),colon)
         if(icol.ne.0)icol=icol+1
      end do
      nrange=nrange_in
      if(max(nrange,irange).gt.maxrng) then
         write(*,*) 'ERROR*:XPARSE:XPRRRG: Too many sub ranges'
     &         ,max(irange,nrange),' > ',maxrng,' while parseing :',
     &      string
         nrange=min(maxrng,nrange)
         irange=min(maxrng,irange)
      end if
      icol=0
      do jrange=1,nrange
         tmprng(1,jrange)=rnglw(jrange)
         tmprng(2,jrange)=rnghi(jrange)
C**      **check if the range is in the priority range
         if((idefrg(1).lt.0).or.
     &    ((idefrg(1).eq.0).and.(jrange.ge.nrange-irange+1)).or.
     &    (idefrg(jrange).le.irange)) then
            irb=icol+1
            icol=index(string(irb:lact),colon)+icol
            if(icol.lt.irb) then
               ire=lact
            else
               ire=icol-1
            end if
            rngstr=string(irb:ire)
            lrng=ire-irb+1
C **          ** check if there is a real range (an empty range
C **          ** equivalent to *, i.e. nothing between the colons as
C **          ** indicated by lrng = 0)
            qrange=(lrng.le.0)
            do while(.not.qrange)
               If(rngstr.eq.inquiry) then
                  xprmsg=rngnam(jrange)(:lenact(rngnam(jrange)))//':'
                  lc=len(rngstr)
                  call xinfix(xprmsg(:lenact(xprmsg)),rngstr,lc,jflag)
                  if(jflag.ne.0) then
                     iflag = -1
                     return
                  end if
                  lrng=lenact(rngstr)
               end if
               idash=index(rngstr(2:lrng),'-')
               if(idash.eq.0) idash=index(rngstr(2:lrng),'..')
               qdash=idash.ne.0
               if(qdash) idash=idash+1
               qrange=.true.
               do i=1,2
                  if(i.eq.1) then
                     if(qdash) then
                        substr=rngstr(1:idash-1)
                        type='lower limit of '
                     else
                        substr=rngstr
                        type='single val for '
                     end if
                     extrem=rngmn(jrange)
                  else
                     type='upper limit of '
                     if(qdash) then
                        if(rngstr(idash:idash).eq.'-') then
                           substr=rngstr(idash+1:)
                        else
                           substr=rngstr(idash+2:)
                        end if
                     end if
                     extrem=rngmx(jrange)
                  end if
                  iparse=0
                  qparin=.false.
                  do while(.not.qparin)
                     qparin=.true.
                     if(substr.eq.doublestar) then
                        tmprng(i,jrange)=extrem
                     elseif(substr.eq.inquiry) then
                        qparin=.false.
                        iparse=len(substr)
                        xprmsg=type//rngnam(jrange)(:
     &                   lenact(rngnam(jrange)))//' ('
                        lmsg=lenact(xprmsg)
                        write(xprmsg(lmsg+1:),'(i12,a)')(tmprng(
     &                   i,jrange)),')'
                        call xsquez(xprmsg(lmsg+1:),' ',0,lmsgp)
                        lmsg=lmsg+lmsgp
                        call xinfix(xprmsg(:lmsg),substr,iparse,
     &                   jflag)
                        if(jflag.ne.0) then
                           qrange=.false.
                           rngstr=inquiry
                           goto 300
                        end if
                     elseif(substr.ne.star) then
C **                ** if corrections not allowed, first
C **                ** check if it actually is a number
                        if(.not.qcor) then
                           if(.not.xisnum(substr(:lenact(substr)),
     &                      rvalue,qint,ivalue)) then
                              iflag=2
                              return
                           end if
                        end if
                        xprmsg=type//rngnam(jrange)(:
     &                   lenact(rngnam(jrange)))//' range'
                        call xgtlr4(substr,iparse,1,xprmsg,
     &                   1,rngmn(jrange),rngmx(jrange),1,
     &                   tmprng(i,jrange),nret,jflag,-1)
                        if(jflag.lt.0) then
C** As there is no value input, allow the particular range to be
C** reinput
                           qrange=.false.
                           rngstr=inquiry
                           goto 300
                        end if
                     end if
                  end do
               end do
C **         ** come from for break out of the parsing of the
C **         ** range when an EOF on the input is reached
  300          continue
            end do
         end if
      end do
      iflag=0
      do i=1,nrange
         rnglw(i)=tmprng(1,i)
         rnghi(i)=tmprng(2,i)
      end do
      return
      end
