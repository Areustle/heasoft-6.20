      subroutine cprrmap (Rmap1, Xdim1, Ydim1, Xmin1, Xmax1, Ymin1, 
     &                    Ymax1, Rmap2, Xdim2, Ydim2, Xmin2, Xmax2, 
     &                    Ymin2, Ymax2, Status)
      implicit none
c
c  Copy real map into another real map
c   Any out-of-bounds copies result in NULL
c   Note: Destination map is not initialized by this routine.
c         If only portion is copied onto, initialize with NULL first
c
c  I  Rmap1     (r)  Source real map
c  I  X/Ydim1   (i)  Dimensions of source map
c  I  Xmin/max1 (i)  Range in X to copy
c  I  Ymin/max1 (i)  Range in Y to copy
c  O  Rmap2     (r)  Destination map
c  I  X/Ydim2   (i)  Dimensions of destination map
c  I  Xmin/max2 (i)  Range in X to copy
c  I  Ymin/max2 (i)  Range in Y to copy
c  O  Status    (i)  Error flag
c
      integer*4 Xdim1, Ydim1, Xmin1, Xmax1, Ymin1, Ymax1
      real*4 Rmap1(Xdim1,Ydim1)
      integer*4 Xdim2, Ydim2, Xmin2, Xmax2, Ymin2, Ymax2
      real*4 Rmap2(Xdim2,Ydim2)
      integer*4 Status
c
c  Local variables
c
      integer*4 i, j, i1, j1, i2, j2
      integer*4 cpszx, cpszy, dsszx, dsszy
      real*4 RNULL

      cpszx = Xmax1 - Xmin1 + 1
      cpszy = Ymax1 - Ymin1 + 1
      dsszx = Xmax2 - Xmin2 + 1
      dsszy = Ymax2 - Ymin2 + 1

      if ( cpszx.ne.dsszx .or. cpszy.ne.dsszy ) then
         call XWRITE (' cprrmap: Copy/Destination size mismatch', 5)
         Status = -1
         return
      endif

      Status = 0

      do i = 0, cpszx-1
         do j = 0, cpszy-1
            i1 = Xmin1+i
            j1 = Ymin1+j
            i2 = Xmin2+i
            j2 = Ymin2+j
            if ( i2.ge.1 .and. i2.le.Xdim2 .and. 
     &           j2.ge.1 .and. j2.le.Ydim2 ) then
               if ( i1.ge.1 .and. i1.le.Xdim1 .and.
     &              j1.ge.1 .and. j1.le.Ydim1 ) then
                  Rmap2(i2,j2) = Rmap1(i1,j1)
               else
                  Rmap2(i2,j2) = RNULL()
               endif
            endif
         enddo
      enddo

      return
      end
