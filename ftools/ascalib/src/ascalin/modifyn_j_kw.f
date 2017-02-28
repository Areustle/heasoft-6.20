
      subroutine modifyn_j_kw(unit, root, n, value, comment, status)
      
      integer unit, n, status, value
      character*(*) root, comment
      
      character(80) keyword
      
      call catnum(keyword, root, n)
      call ftmkyj(unit, keyword, value, comment, status)
      if (status .eq. 202) then
         status = 0
         call ftpkyj(unit, keyword, value, comment, status)
      end if
      
      end
