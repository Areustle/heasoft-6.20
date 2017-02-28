
      subroutine filen_find(iunit,keywrd,ival,outkey)
      integer iunit,ival
      character*(*) keywrd,outkey(100)
      character(80) strval,comm
      character(8) keynam
      integer i,status
      status=0
      ival=0
      strval=' '
      comm=' '
      
      do 100 i=1,999
        call ftkeyn(keywrd,i,keynam,status)
        if(status.ne.0)then
          call fcecho(' ')
          call fcecho('Error generating keynam using ftkeyn')
          call fcecho('Returning to calling routine.')
          return
        endif
        
        call ftgkys(iunit,keynam,strval,comm,status)
        if(status.eq.0)then
          ival=ival+1
          outkey(ival)=strval
        else
          return
        endif

100   continue

      return
      end
