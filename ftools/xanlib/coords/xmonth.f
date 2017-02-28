*- xmonth - month name from number
      character*(*) function xmonth(n)
* Author :
*  Andy Pollock
* History :
*  3 October 1991 : original
* Import :
      integer*4 n
* Local data :
      character(10) month(12)
      data month /'January',
     &            'February',
     &            'March',
     &            'April',
     &            'May',
     &            'June',
     &            'July',
     &            'August',
     &            'September',
     &            'October',
     &            'November',
     &            'December'/
     
*-
      if((n.lt.1).or.(n.gt.12))then
         xmonth=' '
      else
         xmonth=month(n)
      endif

      return

      end
