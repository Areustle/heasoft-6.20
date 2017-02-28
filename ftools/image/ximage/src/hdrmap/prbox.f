      SUBROUTINE PRBOX (Map, Szx, Szy, Mapid, Maptype, Boxsize,
     &                  Xpix, Ypix, Status)
      IMPLICIT NONE
c
c  I  Map     (r)  Image map
c  I  Szx/y   (i)  Size of map
c  I  Mapid   (s)  Map id
c  I  Maptype (s)  Map type (I=integer, R=real)
c  I  Boxsize (i)  Size of box to print (n=n pixels around selection)
c  I  X/Ypix  (r)  Selected pixel
c  O  Status  (i)  Error flag (0=OK)
c
c   Prints a box of values to command screen for selected 
c   pixel
c
      CHARACTER*(*) Mapid, Maptype
      INTEGER Szx, Szy, Boxsize, Status
      REAL*4 Map(Szx,Szy)
      REAL    Xpix, Ypix

      include '../include/io.inc'
c
c  Local variables
c
      INTEGER  LENACT
      REAL*4   xpos, ypos
      real*8 dd
      integer*4 i, j, imin, imax, width, slen, sigdig, dslen, spc
      integer*4 bufsiz, buffil, ix, iy, lennul
      character(30) fmt, fmti, str, dstr
      character(4) strnul
      logical isrnull, fndnul
      real*4 rnull, rmin, rmax

      strnul = 'NULL'
      lennul = LENACT(strnul)

      Status = 0

      if ( Boxsize.lt.0 ) return

      call XWRITE(' ',10)

C Get MAP value for selection
      call imgpix(Mapid,Xpix,Ypix,xpos,ypos,2,status)
      ix = NINT(xpos)
      iy = NINT(ypos)

      if ( Maptype.eq.'I' ) then
c
c  Make sure width can fit largest and smallest number, plus
c  'NULL' string if a null exists
c
         rmin = rnull()
         rmax = rnull()
         fndnul = .FALSE.
         do j = iy-Boxsize, iy+Boxsize
            if ( j.ge.1 .and. j.le.Szy ) then
               do i = ix-Boxsize, ix+Boxsize
                  if ( i.ge.1 .and. i.le.Szx ) then
                     if ( isrnull(Map(i,j)) ) then
                        fndnul = .TRUE.
                     else
                        if ( isrnull(rmin) .or. Map(i,j).lt.rmin ) 
     &                     rmin = Map(i,j)
                        if ( isrnull(rmax) .or. Map(i,j).gt.rmax )
     &                     rmax = Map(i,j)
                     endif
                  endif
               enddo
            endif
         enddo
         if ( isrnull(rmin) .and. isrnull(rmax) ) then
            rmin = 0
            rmax = 0
         endif
         imin = INT(rmin)
         call xistr(imin, str, slen)
         width = slen
         imax = INT(rmax)
         call xistr(imax, str, slen)
         width = MAX(width, slen)
         if ( fndnul .and. width.lt.lennul ) width = lennul
         call xistr(width, str, slen)
         fmti = '(a,1x,i'//str(1:slen)//')'
      else
c
c  No special code to check for null string length as there
c  should always be enough room
c
         sigdig = 8
         width = sigdig+5
      endif

      bufsiz = LEN(ZWRite)
      ZWRite = ' '
      do j = iy+Boxsize, iy-Boxsize, -1
         buffil = 0
         if ( j.ge.1 .and. j.le.Szy ) then
            do i = ix-Boxsize, ix+Boxsize
               if ( i.ge.1 .and. i.le.Szx ) then
                  buffil = buffil + width + 1
                  if ( buffil.gt.bufsiz ) then
                     call XWRITE(' Buffer overflow, try smaller box',
     &                           10)
                     status = -1
                     return
                  endif
                  if ( isrnull(Map(i,j)) ) then
                     spc = width - lennul + 1
                     call xistr(spc, str, slen)
                     fmt = '(a,'//str(1:slen)//'x,a)'
                     write(ZWRite,fmt) ZWRite(:LENACT(ZWRite)),
     &                                 strnul
                  elseif ( Maptype.eq.'I' ) then
                     write(ZWRite,fmti) ZWRite(:LENACT(ZWRite)),
     &                                  INT(Map(i,j))
                  else
                     dd = Map(i,j)
                     call xdstr(dd, sigdig, dstr, dslen)
                     spc = width - dslen + 1
                     call xistr(spc, str, slen)
                     fmt = '(a,'//str(1:slen)//'x,a)'
                     write(ZWRite,fmt) ZWRite(:LENACT(ZWRite)),
     &                                 dstr(:dslen)
                  endif
               endif
            enddo
            call XWRITE(ZWRite,10)
            ZWRite = ' '
         endif
      enddo

      RETURN
      END
