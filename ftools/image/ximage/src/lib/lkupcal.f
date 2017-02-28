      subroutine lkupcal(Itel, Pattern, Filename, Status)
      implicit none
c
c Find calibration file. First try filename based on
c  detnam, then instrume
c
c  I  itel     (i) Telescope index
c  I  pattern  (s) File pattern, replace * with INSTRUME or DETNAME
c                                if no *, use as is
c  O  filename (s) Location of file
c  O  status   (i) Error flag (0=OK)
c
      integer Itel, Status
      character*(*) Pattern, Filename

      include '../include/io.inc'
      include '../include/startup.inc'
      include '../include/sitedef.inc'
c
c  Local variables
c
      integer istar, LENACT
      logical there

      character(80) teltmp, insttmp, dettmp

      status = 0
c
      if ( Itel.le.0 ) then
         teltmp = 'unknown'
         insttmp = 'unknown'
         dettmp = ' '
      else
         teltmp = ZTElescop(Itel)
         insttmp = ZINstrume(Itel)
         dettmp = ZDEtnam(Itel)
         call locase(teltmp)
         call locase(insttmp)
         call locase(dettmp)
      endif

      istar = index(Pattern, '*')
      if ( istar.le.0 ) then
         filename = teltmp(:LENACT(teltmp))//'/'//
     &              insttmp(:LENACT(insttmp))//'/'//
     &              Pattern(1:LENACT(Pattern))
         call PTEND(Cxan,Ccal,filename)
      elseif ( dettmp.ne.' ' ) then
         filename = teltmp(:LENACT(teltmp))//'/'//
     &              insttmp(:LENACT(insttmp))//'/'//
     &              Pattern(1:istar-1)//dettmp(:LENACT(dettmp))//
     &              Pattern(istar+1:LENACT(Pattern))
         call PTEND(Cxan,Ccal,filename)
      else
         filename = ' '
         there = .FALSE.
      endif

      if ( filename.ne.' ' ) then
         call xwrite(' Look for calibration file:', 25)
         call xwrite(filename, 25)
         inquire (FILE=filename, EXIST=there)
      endif

      if ( there ) then
         if ( istar.gt.0 ) then
            call xwrite(' Found DETNAM calibration file:', 20)
            call xwrite(filename, 20)
         else
            call xwrite(' Found specified calibration file:', 20)
            call xwrite(filename, 20)
         endif
      else
         if ( istar.gt.0 ) then
            filename = teltmp(:LENACT(teltmp))//'/'//
     &                 insttmp(:LENACT(insttmp))//'/'//
     &                 Pattern(1:istar-1)//insttmp(:LENACT(insttmp))//
     &                 Pattern(istar+1:LENACT(Pattern))
            call PTEND(Cxan,Ccal,filename)
         else
            filename = ' '
            there = .FALSE.
         endif

         if ( filename.ne.' ' ) then
            call xwrite(' Look for calibration file:', 25)
            call xwrite(filename, 25)
            inquire (FILE=filename, EXIST=there)
         endif

         if ( there ) then
            call xwrite(' Found INSTRUME calibration file:', 20)
            call xwrite(filename, 20)
         else
            write(ZWRite,*) ' No calibration file for current mission: '
     &                      , Pattern
            call xwrite(ZWRite, 20)
            filename = ' '
            status = -1
         endif
      endif

      return
      end
