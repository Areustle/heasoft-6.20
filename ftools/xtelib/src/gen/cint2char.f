c**********************************************************************
c
c This subroutine takes as input:
c inval - integer input
c     
c and returns the LEFT justified character string:
c substr - character
c strlen - length of string returned
c status - did everything succeed???
c
c**********************************************************************
c
      subroutine cint2char(inval,substr,strlen,abort)

      integer inval,i,strlen,status
      character*(*) substr
      character(20) cval
      logical abort

      status=0
      cval=' '
      call fti2c(inval,cval,status)
      if(status.ne.0)then
        call fcecho(' ')
        call fcecho('Error converting integer to string.')
        call fcecho('Error in CINT2CHAR subroutine.')
        abort=.TRUE.
        goto 999
      endif
      
      do 10 i=1,20
        if(cval(i:i).ne.' ')goto 20
10    continue
      
20    continue

      substr(1:(20-i+1))=cval(i:20)
      strlen = (20-i+1)

999   continue
      
      return
      end
