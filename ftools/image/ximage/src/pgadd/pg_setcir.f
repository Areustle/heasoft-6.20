      subroutine pg_setcir (No_of_levels)

      implicit none
c
c  Sets color index range for use by image plotting routine
c  PGIMAG (also the modified version XPGIMAG)
c
      integer No_of_levels

      include '../include/io.inc'
c
c  Local variables
c
      integer minci, maxci, maxcols, devcols
      character(10) oldstr, newstr
      integer olen, nlen
       
      maxcols = No_of_levels + 1

      call PGQCOL(minci, maxci)
      devcols = maxci - minci + 1

      if ( devcols.ge.maxcols+16 ) then
         minci = 16
         devcols = maxci - minci + 1
         call PGSCIR(minci, minci+maxcols-1)
      else if ( maxcols.lt.3 .or. devcols.lt.3 ) then
         call XWRITE (' Low-color device: Plotting with PGGRAY', 10)
         No_of_levels = 1
      else
         call xistr(No_of_levels, oldstr, olen)
         No_of_levels = MAX(1, devcols - 17)
         call xistr(No_of_levels, newstr, nlen)
         write(ZWRite,'(5a)') ' Device incapable of displaying ',
     &                        oldstr(1:olen), ' levels. Truncating to ',
     &                        newstr(1:nlen), ' levels...'
         call xwrite(ZWRite, 10)
         call PGSCIR(16, No_of_levels+16)
         call PGQINF('TYPE',newstr,nlen)
         if ( newstr.eq.'XWINDOW' .or. newstr.eq.'XSERVER' ) then
            call xwrite(
     &        ' Edit your .Xdefaults to contain the following line', 10)
            call xwrite(' pgxwin.Win.maxColors: 255', 10)
            call xwrite(' Close pgxwin_server and try again', 10)
         elseif ( newstr.eq.'XTK' ) then
            call xwrite(
     &        ' Use command: set xtkcols 255', 10)
            call xwrite(' Close_pg_window and try again', 10)
         endif
      endif

      return
      end
