      subroutine setrca(zstring,ierr)
c
c set the flag so the recall buffer is enabled 
c  or listed                                       22.6.91 Nick
c 
      character*(*) zstring
      integer*4 ierr, ln
c
c remove "buffer " from zstring
c
      ln=index(zstring,' ')
      zstring=zstring(ln+1:)
      call upc(zstring)
      if(zstring.eq.'ON')then
       zstring='%ed% on'
      elseif(zstring.eq.'OFF')then
       zstring='%ed% off'
      elseif(zstring.eq.'LIST')then
       zstring = '%recall%all'
      else
       call xwrite(' Type "buf on", "buf off", or "buf list"',5)
      endif
c do it
      call ldbuf(zstring,1,ierr)            
      return
      end
