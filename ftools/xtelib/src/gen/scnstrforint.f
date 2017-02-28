
        subroutine scnstrforint(cstring,cmatch,cdelimitstart,
     &     cdelimitend,cval,ival,lmatch)
        implicit none
        character*(*) cstring,cval,cmatch
        character(1) cdelimitstart,cdelimitend
        integer ival,i,istart,iend
        integer ilen,imatchlen,fcstln
        logical lmatch

        cval=' '
        ival=0
        lmatch=.FALSE.
        ilen=0
        imatchlen=0
        istart=0
        iend=0

c     Get the length of the string that we will be searching.
        ilen=fcstln(cstring)

c      Lets strip off any trailing spaces at the end of the string
c      that we will be searching.
        do 10 i=1,ilen
          if(cstring(i:i).ne.' ')iend=i
10      continue
        ilen=iend

c      Get the length of the string we will be searching for.
        imatchlen=fcstln(cmatch)

c      Let's strip off any trailing spaces at the end of the string
c      that we will be searching for. 
        do 20 i=1,imatchlen
          if(cmatch(i:i).ne.' ')iend=i
20      continue
        imatchlen=iend

c        print*,'ilen and imatchlen is',ilen,imatchlen
        
        do 100 i=1,ilen
          if(cstring(i:i-1+imatchlen).eq.cmatch(:iend))lmatch=.TRUE.
          if(lmatch.and.(cstring(i:i).eq.cdelimitstart))then
            istart=i+imatchlen
          endif
          if(cstring(i:i).eq.cdelimitend.and.
     &       istart.ne.0.and.lmatch)then
            cval=cstring(istart:i-1)
            ival=(i-1)-istart
            istart=0
            goto 200
          endif
100     continue
200     continue
          
        return
        end
