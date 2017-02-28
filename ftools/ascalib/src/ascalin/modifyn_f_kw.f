
      subroutine modifyn_f_kw(unit, root, n, value, decimals,
     &     comment, status)
      
      integer unit, n, decimals, status
      real value
      character*(*) root, comment
      
      character(80) keyword
      
      call catnum(keyword, root, n)
      call ftmkyf(unit, keyword, value, decimals, comment, status)
      if (status .eq. 202) then
         status = 0
         call ftpkyf(unit, keyword, value, decimals, comment, status)
      end if
      
      end
