*- isctime.sun - recognise SUN format character time definition
      logical*4 function isctime(s)
* Author :
*  Andy Pollock
* History :
*  25 May 1992 : original
* Import :
      character*(*) s
* External references :
      logical*4 isday
      logical*4 isdom
      logical*4 ismonth
      logical*4 isyear
      logical*4 isclock
*-
      isctime=(isday(s(1:3)).and.
     &         (s(4:4).eq.' ').and.
     &         ismonth(s(5:7)).and.
     &         (s(8:8).eq.' ').and.
     &         isdom(s(9:10)).and.
     &         (s(11:11).eq.' ').and.
     &         isclock(s(12:19)).and.
     &         (s(20:20).eq.' ').and.
     &         isyear(s(21:24)))

      return

      end
