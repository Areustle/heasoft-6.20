c**********************************************************************
c This routine takes in a CPIX keyword (in subset1) value and parses it
c and returns the number of ranges, as well as the start and stop values.
c It has also been adapted for usage with some of the input strings for
c channel boundaries that the user will input. This was done due to serious
c deficiencies in the colparse and subsequently the xtecolparse routines.
c**********************************************************************
      
      subroutine parsebd(subset1,ranges,ndims,
     &   istartval,istopval,ldryrun,status)
      implicit none
      character*(*) subset1
      character(1) separator
      character(20) cval
      character(80) cinfo
      integer ranges,istartval(*),istopval(*),status,outlen1,
     &   fcstln,inc1,inc2,i,j,ival,ifirst,ndims,icomma,
     &   itilde, icolon, isemicolon,iend,
     $   ival1,ival2,ival3
      logical ldryrun
      
      separator=','
      cinfo=' '
      inc1=0
      inc2=0
      icomma=0
      icolon=0
      isemicolon=0
      itilde=0
      outlen1=fcstln(subset1)

      ifirst=1
      iend=0
      inc1=0
      inc2=0
      ival1=0
      ival2=0
      ival3=0

      if(subset1.eq.' ')then

        do i=1,ndims
          istartval(i)=i
          istopval(i)=i
        enddo
        
        ranges=ndims
        
      else

        do i=1,outlen1
          
          if(subset1(i:i).eq.',')icomma=i
          if(i.eq.outlen1)icomma=i+1

          if(subset1(i:i).eq.'(')ifirst=i+1
          if(subset1(i:i).eq.'~')itilde=i
c         To allow for people unfamiliar with the ~ switch we have to
c allow for people inputting a - as well and have it behave in a similar
c manner.
          if(subset1(i:i).eq.'-')itilde=i
          
          if(subset1(i:i).eq.':')icolon=i
          if(subset1(i:i).eq.';')isemicolon=i
          if(subset1(i:i).eq.')')iend=i-1

c          print*,'ifirst,itilde,icolon,isemicolon,iend,icomma,i'
c          print*,ifirst,itilde,icolon,isemicolon,iend,icomma,i
c          print*,'subset1(i:i) is ',subset1(i:i)

          if(icomma.ne.0)then

            if(iend.eq.0)iend=icomma-1
            
            inc1=inc1+1
            
            if(itilde.ne.0.and.icolon.eq.0)then
              cval=subset1(ifirst:itilde-1)
              call ftc2i(cval,ival1,status)
              istartval(inc1)=ival1+1
c              print*,'cval and ival1 1 ',cval,ival1,istartval(inc1)

              if(isemicolon.eq.0)then
                cval=subset1(itilde+1:iend)
                call ftc2i(cval,ival2,status)
                istopval(inc1)=ival2+1
                
              elseif(isemicolon.ne.0)then
                cval=subset1(itilde+1:isemicolon-1)
                call ftc2i(cval,ival2,status)
                cval=subset1(isemicolon+1:iend)
                call ftc2i(cval,ival3,status)
                
                do 200 j=ival1,ival2,ival3
                  istartval(inc1)=j+1
                  istopval(inc1)=j+ival3
                  inc1=inc1+1
200             continue
                
                inc1=inc1-1
              endif

            elseif(itilde.eq.0.and.icolon.ne.0)then
              cval=subset1(ifirst:icolon-1)
              call ftc2i(cval,ival1,status)
              istartval(inc1)=ival1+1

              if(isemicolon.eq.0)then
                cval=subset1(icolon+1:iend)
                call ftc2i(cval,ival2,status)
                ival3=1
                
                do 300 j=ival1,ival2,ival3
                  istartval(inc1)=j+1
                  istopval(inc1)=j+1
                  inc1=inc1+1
300             continue

                inc1=inc1-1
                
              elseif(isemicolon.ne.0)then
                cval=subset1(icolon+1:isemicolon-1)
                call ftc2i(cval,ival2,status)
                cval=subset1(isemicolon+1:iend)
                call ftc2i(cval,ival3,status)

                do 400 j=ival1,ival2,ival3
                  istartval(inc1)=j+1
                  istopval(inc1)=j+1
                  inc1=inc1+1
                  
400             continue
                inc1=inc1-1
              endif
              
            elseif(itilde.eq.0.and.icolon.eq.0)then
              cval=subset1(ifirst:iend)
              call ftc2i(cval,ival,status)
              istartval(inc1)=ival+1
              istopval(inc1)=ival+1
            endif
            
            ifirst=icomma+1
            icomma=0
            itilde=0
            icolon=0
            iend=0
            isemicolon=0

          endif

          
        enddo
        
      endif
        
      ranges=inc1

      cval=' '

c      print*,'ILBD and IUBD are:'
      do i=1,inc1
        istartval(i)=istartval(i)-1
        istopval(i)=istopval(i)-1
c        print*,i,istartval(i),istopval(i)
      enddo
      
      if(ldryrun)then
        call fcecho(' ')
        call fti2c(inc1,cval,status)
        call fcecho('The number of channel bins found were:')
        call fcecho(cval)
        call fcecho(' ')
        
        call fcecho('BIN   CHANNEL START        CHANNEL STOP')

        do i=1,inc1
          call fti2c(i,cval,status)
          cinfo(1:5)=cval(16:20)
          istartval(i)=istartval(i)
          call fti2c(istartval(i),cval,status)
          cinfo(6:26)=cval
          istopval(i)=istopval(i)
          call fti2c(istopval(i),cval,status)
          cinfo(28:48)=cval
          call fcecho(cinfo)
        enddo
        
      endif
      
      return
      end
