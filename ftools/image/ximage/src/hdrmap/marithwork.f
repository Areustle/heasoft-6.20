      subroutine marithwork(Amap, Aszx, Aszy, Bmap, Bszx, Bszy, 
     &                      Cmap, Cszx, Cszy, Mode, Mapmin,
     &                      Mapmax, Status)
      implicit none
c
c  Perform arithmetic on maps
c  Amap [+-*/] Bmap = Cmap
c  Note: It is possible that Cmap is the same array as Amap or Bmap
c
c  Due to speed concerns all tests occur outside loops
c
c  I  Amap   (r)  Map A
c  I  Aszx/y (i)  Size of map A
c  I  Bmap   (r)  Map B
c  I  Bszx/y (i)  Size of map B
c  O  Cmap   (r)  Map C
c  I  Cszx/y (i)  Size of map C
c  I  Mode   (i)  1=add 2=sub 3=mult 4=div 5=int 6=float
c  O  Mapmin (r)  Minimum map value
c  O  Mapmax (r)  Maximum map value
c
      integer Aszx, Aszy, Bszx, Bszy, Cszx, Cszy, Mode, Status
      real*4 Amap(Aszx,Aszy), Bmap(Bszx,Bszy), Cmap(Cszx,Cszy)
      real*4 Mapmin, Mapmax

      include '../include/io.inc'
c
c  Local variables
c
      integer i, j
      logical isrnull
      real*4 rnull

      Mapmin = rnull()
      Mapmax = rnull()

      if ( Aszx.eq.1 .and. Aszy.eq.1 ) then
c
c  Constant + Bmap
c
         if ( Bszx.ne.Cszx .or. Bszy.ne.Cszy ) then
            call xwrite(' marithwork: Bmap != Cmap', 5)
            Status = -1
            return
         endif

         if ( mode.eq.1 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Bmap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Amap(1,1)+Bmap(i,j)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         elseif ( mode.eq.2 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Bmap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Amap(1,1)-Bmap(i,j)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         elseif ( mode.eq.3 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Bmap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Amap(1,1)*Bmap(i,j)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         elseif ( mode.eq.4 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Bmap(i,j)) .or. Bmap(i,j).eq.0. ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Amap(1,1)/Bmap(i,j)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         elseif ( mode.eq.5 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Bmap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = INT(Bmap(i,j))
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         elseif ( mode.eq.6 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Bmap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Bmap(i,j)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         else
            write(ZWRite,'(a,i4)') ' marithwork: invalid mode = ', mode
            call xwrite(ZWRite, 10)
            Status = -1
         endif

      elseif ( Bszx.eq.1 .and. Bszy.eq.1 ) then
c
c  Amap + Constant 
c
         if ( Aszx.ne.Cszx .or. Aszy.ne.Cszy ) then
            call xwrite(' marithwork: Bmap != Cmap', 5)
            Status = -1
            return
         endif

         if ( mode.eq.1 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Amap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Amap(i,j)+Bmap(1,1)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         elseif ( mode.eq.2 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Amap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Amap(i,j)-Bmap(1,1)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         elseif ( mode.eq.3 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Amap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Amap(i,j)*Bmap(1,1)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         elseif ( mode.eq.4 ) then
            if ( Bmap(1,1).eq.0 ) then
               call xwrite(' Constant is zero, cannot divide', 10)
               Status = -1
               return
            endif
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Amap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Amap(i,j)/Bmap(1,1)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         else
            write(ZWRite,'(a,i4)') ' marithwork: invalid mode = ', mode
            call xwrite(ZWRite, 10)
            Status = -1
         endif

      elseif ( Aszx.gt.1 .and. Aszy.gt.1 .and.
     &         Bszx.gt.1 .and. Bszy.gt.1 ) then
c
c  Amap + Bmap 
c
c  Use of NULL in operation alway results in NULL, unless
c  summing two maps.  Then, non-null value is carried over to new map
c
         if ( Aszx.ne.Cszx .or. Aszy.ne.Cszy .or.
     &        Bszx.ne.Cszx .or. Bszy.ne.Cszy ) then
            call xwrite(' marithwork: Amap|Bmap != Cmap', 5)
            Status = -1
            return
         endif

         if ( mode.eq.1 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Amap(i,j)).and.isrnull(Bmap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     if ( isrnull(Amap(i,j)) ) then
                        Cmap(i,j) = Bmap(i,j)
                     elseif ( isrnull(Bmap(i,j)) ) then
                        Cmap(i,j) = Amap(i,j)
                     else
                        Cmap(i,j) = Amap(i,j)+Bmap(i,j)
                     endif
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         elseif ( mode.eq.2 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Amap(i,j)) .or. isrnull(Bmap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Amap(i,j)-Bmap(i,j)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         elseif ( mode.eq.3 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Amap(i,j)) .or. isrnull(Bmap(i,j)) ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Amap(i,j)*Bmap(i,j)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         elseif ( mode.eq.4 ) then
            do i = 1, Cszx
               do j = 1, Cszy
                  if ( isrnull(Amap(i,j)) .or. isrnull(Bmap(i,j)) .or.
     &                 Bmap(i,j).eq.0. ) then
                     Cmap(i,j) = rnull()
                  else
                     Cmap(i,j) = Amap(i,j)/Bmap(i,j)
                     if ( isrnull(Mapmin) .or. Cmap(i,j).lt.Mapmin )
     &                  Mapmin = Cmap(i,j)
                     if ( isrnull(Mapmax) .or. Cmap(i,j).gt.Mapmax )
     &                  Mapmax = Cmap(i,j)
                  endif
               enddo
            enddo
         else
            write(ZWRite,'(a,i4)') ' marithwork: invalid mode = ', mode
            call xwrite(ZWRite, 10)
            Status = -1
         endif

      else

         call xwrite(' Invalid arguments to marithwork', 10)
         Status = -1

      endif
c
c  Set min/max to zero for all-null image
c
      if ( isrnull(Mapmin) .and. isrnull(Mapmax) ) then
         Mapmin = 0
         Mapmax = 0
      endif

      return
      end
