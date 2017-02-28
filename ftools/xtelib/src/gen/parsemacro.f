c**********************************************************************
c
c This subroutine is used for determining is macro substitution is
c occurring. If it is, then the keyword is searched for and that
c valuse is substituted into the subset string.
c**********************************************************************
      
      subroutine parsemacro(iunit,subset)
      implicit none
      character*(*) subset
      character(8) keyword
      character(80) comn
      character(800) subset1
      integer iunit
      integer outlen,fcstln,i,istart,istop,status

      outlen=fcstln(subset)

      istart=0
      istop=0
      status=0
      subset1=' '

      do 100 i=1,outlen
        if(subset(i:i+1).eq.'"!')istart=i+2
        if(subset(i:i).eq.'"'.and.istart.ne.0)istop=i-1
100   continue

      if(istart.ne.0.and.istop.ne.0)then

        keyword=subset(istart:istop)
        call ftgkys(iunit,keyword,subset1,comn,status)
        
        if(status.ne.0)then
          call fcecho('ERROR - could not find KEYWORD for')
          call fcecho('MACRO substitution. Original keyword')
          call fcecho('value is being left unchanged')
        else
          subset=' '
          subset=subset1

        endif
      endif

      return
      end
