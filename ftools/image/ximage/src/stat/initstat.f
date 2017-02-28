      subroutine initstat()
c
c  Initialize include files for statistics commands
c  so it will be known whether various things, such as
c  the background, excesses, etc. have been
c  calculated for this image.
c
      include 'backgd.inc'
      include 'excess.inc'
      include 'detect.inc'

      integer status
c
c  Initialize the map calculation is valid for
c
      STAtid = ' '
      status = 0
      call tclrun('catch {unset ::background}', status)
c
c  Background map
c
      NBOxes = 0
c
c  Excess
c
      NUMexs = 0
c
c  (Detect) Search
c
      NUMdet = 0

      return
      end
