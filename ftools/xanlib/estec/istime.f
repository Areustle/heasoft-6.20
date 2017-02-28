*- istime.for - recognise string time specifications
* Modules :
*  logical*4 function issec - seconds
*  logical*4 function ismin - minutes
*  logical*4 function ishour - hours
*  logical*4 function isclock - hh:mm:ss[.ss]
*  logical*4 function isdom - day-of-month
*  logical*4 function isdoy - day-of-year
*  logical*4 function isday - day name
*  logical*4 function ismonth - month name
*  logical*4 function isyear - year
*  logical*4 function isydoy - year/day-of-year
*  logical*4 function isxdat - XANADU format date "dd-mmm-yyyy"
*  logical*4 function isxdattim - XANADU format "dd-mmm-yyyy hh:mm:ss"
*  logical*4 function iswhen - assorted date format
* History :
*  5 October 1992 : original
* Author :
*  Andy Pollock (EXOSAT::ANDY)

***************************************************************************
*** issec - recognise seconds *********************************************
***************************************************************************
      logical*4 function issec(string)
* Import :
      character*(*) string
* Local variables :
      character(10) s
      integer*4 ls
      integer*4 i
* External references :
      logical*4 isdigit
      integer*4 lenact
*-
      s=string

      issec=((s(1:1).ge.'0').and.(s(1:1).le.'6').and.isdigit(s(2:2)))
      if(s(3:).ne.' ')then
         if(s(3:3).eq.'.')then
            ls=lenact(s)
            i=4
            do while((i.lt.ls).and.issec)
               issec=issec.and.isdigit(s(i:i))
               i=i+1
            end do
         endif
      endif

      return

      end

***************************************************************************
*** ismin - recognise minutes *********************************************
***************************************************************************
      logical*4 function ismin(string)
* Import :
      character*(*) string
* Local variable :
      character(2) s
* External reference :
      logical*4 isdigit
*-
      s=string

      ismin=((s(1:1).ge.'0').and.(s(1:1).le.'6').and.isdigit(s(2:2)))

      return

      end

***************************************************************************
*** ishour - recognise hours **********************************************
***************************************************************************
      logical*4 function ishour(string)
* Import :
      character*(*) string
* Local variable :
      character(2) s
* External references :
      logical*4 isdigit
      integer*4 lenact
*-
      s=string

      ishour=((s(1:1).ge.'0').and.(s(1:1).le.'2').and.isdigit(s(2:2)))
      ishour=ishour.and.(lenact(string).eq.2)

      return

      end

***************************************************************************
*** isclock - recognise 24-hour digital clock *****************************
***************************************************************************
      logical*4 function isclock(string)
* Import :
      character*(*) string
* Local variable :
      character(30) s
* External references :
      logical*4 ishour
      logical*4 ismin
      logical*4 issec
      logical*4 isalnum
*-
      s=string

      isclock=(ishour(s(1:2)).and.
     &         .not.isalnum(s(3:3)).and.
     &         ismin(s(4:5)).and.
     &         .not.isalnum(s(6:6)).and.
     &         ((s(7:).eq.' ').or.issec(s(7:))))

      return

      end

***************************************************************************
*** isdom - recognise day-of-month ****************************************
***************************************************************************
      logical*4 function isdom(string)
* Import :
      character*(*) string
* Local variable :
      character(2) s
* External references :
      logical*4 isdigit
      integer*4 lenact
*-
      s=string

      isdom=(((s(1:1).ge.'0').and.(s(1:1).le.'2')
     &                       .and.isdigit(s(2:2))).or.
     &       ((s(1:1).eq.'3').and.((s(2:2).eq.'0').or.
     &                             (s(2:2).eq.'1'))))
      isdom=isdom.and.(lenact(string).eq.2)

      return

      end

***************************************************************************
*** isdoy - recognise day-of-year *****************************************
***************************************************************************
      logical*4 function isdoy(string)
* Import :
      character*(*) string
* Local variables :
      character(3) s
      integer*4 ls
* External references :
      logical*4 isdigit
      integer*4 lenact
*-
      s=string

      ls=lenact(string)
      if(ls.eq.3)then
         isdoy=((s(1:1).ge.'0').and.
     &          (s(1:1).le.'3').and.
     &          isdigit(s(2:2)).and.
     &          isdigit(s(3:3)).and.
     &          (s.ne.'000'))
      else if(ls.eq.2)then
         isdoy=(isdigit(s(1:1)).and.
     &          isdigit(s(2:2)).and.
     &          (s.ne.'00'))
      else if(ls.eq.1)then
         isdoy=(isdigit(s(1:1)).and.
     &          (s.ne.'0'))
      else
         isdoy=.false.
      endif

      return

      end

***************************************************************************
*** isyear - recognise year ***********************************************
***************************************************************************
      logical*4 function isyear(string)
* Import :
      character*(*) string
* Local variables :
      character(4) s
      integer*4 ls
* External references :
      logical*4 isdigit
      integer*4 lenact
*-
      s=string

      ls=lenact(string)
      if(ls.eq.4)then
         isyear=((s(1:1).ge.'1').and.
     &           (s(1:1).le.'2').and.
     &           isdigit(s(2:2)).and.
     &           isdigit(s(3:3)).and.
     &           isdigit(s(4:4)))
      else if(ls.eq.2)then
         isyear=isdigit(s(1:1)).and.isdigit(s(2:2))
      else
         isyear=.false.
      endif

      return

      end

***************************************************************************
*** isday - recognise day name ********************************************
***************************************************************************
      logical*4 function isday(string)
* Import :
      character*(*) string
* Local variable :
      character(3) s
*-
      s=string
      call upc(s)

      isday=((s.eq.'SUN').or.
     &       (s.eq.'MON').or.
     &       (s.eq.'TUE').or.
     &       (s.eq.'WED').or.
     &       (s.eq.'THU').or.
     &       (s.eq.'FRI').or.
     &       (s.eq.'SAT'))

      return

      end

***************************************************************************
*** ismonth - recognise month name ****************************************
***************************************************************************
      logical*4 function ismonth(string)
* Import :
      character*(*) string
* Local variable :
      character(3) s
*-
      s=string
      call upc(s)

      ismonth=((s.eq.'JAN').or.
     &         (s.eq.'FEB').or.
     &         (s.eq.'MAR').or.
     &         (s.eq.'APR').or.
     &         (s.eq.'MAY').or.
     &         (s.eq.'JUN').or.
     &         (s.eq.'JUL').or.
     &         (s.eq.'AUG').or.
     &         (s.eq.'SEP').or.
     &         (s.eq.'OCT').or.
     &         (s.eq.'NOV').or.
     &         (s.eq.'DEC'))

      return

      end

***************************************************************************
*** isydoy - recognise year/day-of-year combination ***********************
***************************************************************************
      logical*4 function isydoy(string)
* Import :
      character*(*) string
* Local variable :
      character(80) s
* External references :
      logical*4 isyear
      logical*4 isalnum
      logical*4 isdoy
*-
      s=string
      if(s.eq.' ')then
         isydoy=.false.
      else
      isydoy=(isyear(s(1:4)).and.
     &        .not.isalnum(s(5:5)).and.
     &        isdoy(s(6:8)))
      endif

      return

      end

***************************************************************************
*** isxdat - recognise XANADU format date  ********************************
***************************************************************************
      logical*4 function isxdat(string)
* History :
*  5 October 1992 : original
* Author :
*  Andy Pollock (EXOSAT::ANDY)

* Import :
      character*(*) string
* Local variable :
      character(11) s
* External references :
      logical*4 isdom
      logical*4 ismonth
      logical*4 isalnum
      logical*4 isyear
      integer*4 lenact
*-
      s=string

      isxdat=((lenact(string).eq.11).and.
     &        isdom(s(1:2)).and.
     &        .not.isalnum(s(3:3)).and.
     &        ismonth(s(4:6)).and.
     &        .not.isalnum(s(7:7)).and.
     &        isyear(s(8:11)))

      return

      end

***************************************************************************
*** isxdattim - recognise XANADU format date and time *********************
***************************************************************************
      logical*4 function isxdattim(string)
* History :
*  5 October 1992 : original
* Author :
*  Andy Pollock (EXOSAT::ANDY)

* Import :
      character*(*) string
* Local variables :
      character(80) s

* External references :
      logical*4 isxdat
      logical*4 isalnum
      logical*4 isclock
      integer*4 lenact
*-
      s=string

      isxdattim=((lenact(string).eq.20).and.
     &           isxdat(s(1:11)).and.
     &           .not.isalnum(s(12:12)).and.
     &           isclock(s(13:20)))

      return

      end

***************************************************************************
*** iswhen - recognise assorted date formats ******************************
***************************************************************************
      logical*4 function iswhen(string)
* History :
*  5 October 1992 : original
* Author :
*  Andy Pollock (EXOSAT::ANDY)

* Import :
*  string - possible time representation
      character*(*) string

* Local variable :
      character(80) s
      integer*4 ls

* External references :
      logical*4 isdigit
      logical*4 isctime
      logical*4 isydoy
      logical*4 isxdattim
      integer*4 lenact
*-
      s=string
      call upc(s)
      ls=lenact(s)
      if((s(1:1).eq.'"').and.(s(ls:ls).eq.'"').and.(ls.gt.2))then
         s=s(2:ls-1)
      endif

      iswhen=((s.eq.'NOW').or.
     &        (s.eq.'TODAY').or.
     &        (s.eq.'YESTERDAY').or.
     &        (s.eq.'LAST_WEEK').or.
     &        (s.eq.'LAST_MONTH').or.
     &        (s.eq.'LAST_YEAR').or.
     &        ((index(s,'_DAYS_AGO').ne.0).and.isdigit(s(1:1))).or.
     &        ((index(s,'_WEEKS_AGO').ne.0).and.isdigit(s(1:1))).or.
     &        ((index(s,'_MONTHS_AGO').ne.0).and.isdigit(s(1:1))).or.
     &        ((index(s,'_YEARS_AGO').ne.0).and.isdigit(s(1:1))).or.
     &        isydoy(s).or.
     &        isxdattim(s).or.
     &        isctime(s))

      return

      end
