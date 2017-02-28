
c**********************************************************************
c      This subroutine takes a string of time intervals and returns
c      two arrays containing those times as a series of real numbers.
c      
c     04Jan01 (MJT) modified format to accomodate 16 sig figs
c
      subroutine str2real(timeint,ntimes,numranges,
     &     timerange1,timerange2)
c      implicit none
      integer nb

c      Define all of the common parameters used in the arrays.
      parameter (nb = 15)
      
      character*(*) timeint
      character(80) timelist(nb)
      character(80) context
      integer fcstln,timelen,ntimes,i,j,tlist_index,
     &     rangelen,range_index,numranges
      double precision timerange1(nb),timerange2(nb)
      logical range, lbailout

      common/bail/lbailout
      
      range=.FALSE.
      range_index=0

C  get the number and list of time ranges
      timelen = fcstln(timeint)
c      print*,'timelen is',timelen
      tlist_index = 1
      i = 1
      j = 1
10    if (i .eq. timelen) goto 12
      if (timeint(i:i) .eq. ',') goto 11
      i = i + 1
      goto 10
11    timelist(j) = timeint(tlist_index:i-1)
      i = i + 1
      tlist_index = i
      j = j + 1
      goto 10
12    timelist(j) = timeint(tlist_index:i)
      numranges = j

c      print*,'timelist is ',(timelist(j),j=1,numranges)
c      print*,'number of ranges is ',numranges
c      print*,'ranges are ',(timelist(i),i=1,numranges)

C      get the time range limits
      do 20 i = 1, numranges
C      search for - range separator
         rangelen = fcstln(timelist(i))
         range = .false.
         do 30 j = 1, rangelen
            if (timelist(i)(j:j) .eq. '-') then
               range_index = j
               range = .true.
            endif
30       continue
         if (range) then
            if (range_index .eq. 1) then
               timerange1(i) = 1
               read(timelist(i)(2:rangelen),1000,err=999) 
     &              timerange2(i)
            else if (range_index .eq. rangelen) then
               read(timelist(i)(1:rangelen-1),1000,err=999) 
     &              timerange1(i)
               timerange2(i) = ntimes
            else
               read(timelist(i)(1:range_index-1),1000,err=999) 
     &              timerange1(i)
               read(timelist(i)(range_index+1:rangelen),1000,
     &              err=999) timerange2(i)
            endif
         else
            read(timelist(i),1000,err=999) timerange1(i)
            timerange2(i) = timerange1(i)
         endif
20    continue
      return

999   context = 'Error in parsing ranges ' // timelist(i)

      call fcerr (context)
      timerange1(1) = 0.0d0
      timerange2(1) = 0.0d0
      numranges = 1
      
      if(lbailout)then
        call fcecho(' ')
        call fcecho('You set bailout=TRUE')
        call fcecho('Bailing out. Fix the problem in your input!')
        call fcecho('Or turn off bailout.')
        stop
      endif
      
c 1000  format(BN,F19.9)        
1000  format(BN,F21.16)        
      return
      end
