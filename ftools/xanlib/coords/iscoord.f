*- iscoord.for - recognise character coordinates
* Modules - they have these unwealdy names to distinguish them from other
*           XANLIB routines
*  logical*4 function isalphaco - hh mm ss
*  logical*4 function isdeltaco - <+|->dd mm ss[.ss]
* Author :
*  Andy Pollock
* History :
*  6 October 1992
*-

***************************************************************************
*** isalphaco - recognise character alpha coordinate **********************
***************************************************************************
      logical*4 function isalphaco(string)
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
      integer*4 lenact
*-
      s=string
      call upc(s)
      ls=lenact(s)
      if((s(1:1).eq.'"').and.(s(ls:ls).eq.'"').and.(ls.gt.2))then
         s=s(2:ls-1)
      endif

      do while(s(1:1).eq.' ')
         s=s(2:)
      end do

      if(.not.isdigit(s(1:1)))then
         isalphaco=.false.
      else if(s(3:3).eq.' ')then
         isalphaco=((s(1:1).ge.'0').and.
     &             (s(1:1).le.'2').and.
     &             isdigit(s(2:2)).and.
     &             (s(4:4).ge.'0').and.
     &             (s(4:4).le.'5').and.
     &             isdigit(s(5:5)))
      else if(s(2:2).eq.' ')then
         isalphaco=((s(3:3).ge.'0').and.
     &              (s(3:3).le.'5').and.
     &              isdigit(s(4:4)))
      else if(s(3:3).eq.'H')then
         isalphaco=((s(1:1).ge.'0').and.
     &              (s(1:1).le.'2').and.
     &              isdigit(s(2:2)).and.
     &              ((s(4:).eq.' ').or.
     &               ((s(4:4).ge.'0').and.
     &                (s(4:4).le.'5').and.
     &                isdigit(s(5:5)))))
      else if(s(2:2).eq.'H')then
         isalphaco=((s(3:).eq.' ').or.
     &              ((s(3:3).ge.'0').and.
     &               (s(3:3).le.'5').and.
     &               isdigit(s(4:4))))
      endif

      return

      end

***************************************************************************
*** isdeltaco - recognise character delta *********************************
***************************************************************************
      logical*4 function isdeltaco(string)
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
      integer*4 lenact
*-
      s=string
      call upc(s)
      ls=lenact(s)
      if((s(1:1).eq.'"').and.(s(ls:ls).eq.'"').and.(ls.gt.2))then
         s=s(2:ls-1)
      endif

      do while(s(1:1).eq.' ')
         s=s(2:)
      end do

      isdeltaco=((s(1:1).eq.'+').or.(s(1:1).eq.'-')).and.
     &           isdigit(s(2:2))

      return

      end
