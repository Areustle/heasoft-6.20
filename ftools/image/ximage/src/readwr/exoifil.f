      subroutine exoifil(Filter, Ifil, Mode)
      implicit none
c
c  Translate between EXOSAT filter name and index
c
c  I/O  filter   (s)  Filter name
c  I/O  ifil     (i)  Filter index
c   I   mode     (i)  (1 = set filter from ifil)
c                     (2 = set ifil from filter) 
c
      character*(*) Filter
      integer*4 Ifil, Mode
c
c  Local variables
c
      integer*4 i, numfilts
      parameter (numfilts = 8) 
      character(6) filters(numfilts)
      character(6) infilt, tmpfilt
      logical found
      
      DATA filters/'CLOSED' , 'PPL   ' , '4Lx  ' , '      ' , 'Fe Cal' , 
     &     'Al/P  ' , '3Lx   ' , 'Bor   '/

      if ( Mode.eq.1 ) then
         if ( Ifil.ge.1 .and. Ifil.le.numfilts ) then
            Filter = filters(Ifil)
         else
            Filter = ' '
         endif
      elseif ( Mode.eq.2 ) then
         infilt = Filter(1:6)
         call UPC(infilt)
         i = 1
         found = .false.
         do while ( i.le.numfilts .and. .not.found ) 
            tmpfilt = filters(i)
            call UPC(tmpfilt)
            if ( infilt.eq.tmpfilt ) then
               Ifil = i
               found = .TRUE.
            endif
            i = i + 1
         enddo
         if ( .not.found ) Ifil = -2
      else
         call XWRITE(' Error: Invalid mode in exoifil', 10)
      endif

      return
      end
