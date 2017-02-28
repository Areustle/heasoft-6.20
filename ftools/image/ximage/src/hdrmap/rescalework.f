      SUBROUTINE RESCALEWORK(Map,Szx,Szy,Mapid,Vector,Vecsz,Factor,Mult,
     &                       Filevec,Column,X_dir,Y_dir,
     &                       Min_frac,Status)
      IMPLICIT NONE
c
c  Perform rescaling
c
c  I  map         (r)  Image map
c  I  szx/y       (i)  Size of map
c  I  mapid       (s)  Map id string
c  I  vector      (r)  Vector buffer
c  I  vecsz       (i)  Size of buffer
c  I  factor      (r)  Scalar factor to mult or add
c  I  mult        (l)  Mult if true, add/div if false
c  I  filevec     (s)  File containing vector data
c  I  x/y_dir     (l)  Direction for vector operation
c  I  min_frac    (r)  Min exposure fraction to rescale image 
c                      (used when dividing an exposure fraction vector)
c  O  status      (i)  Error flag (0=OK)
c
      integer*4 Szx, Szy, Vecsz, Column, Status
      real*4 Map(Szx,Szy)
      real*4 Vector(Vecsz), Factor, Min_frac
      logical Mult, X_dir, Y_dir
      character*(*) Mapid, Filevec

      include '../include/maxvals.inc'
c
c  Local variables
c
      INTEGER i, j, vecnum
      real*4 rmin, rmax, rnull
      LOGICAL divide, isrnull
      REAL*8 dd

      Status = 0
c
c done to stop multiply twice the array 
c
      IF ( Filevec.ne.' ' ) then
         if ( .NOT. Mult ) then
            CALL XWRITE (' division  by a vector ',10)
            divide = .TRUE.
         else
            divide = .FALSE.
         endif
c
c reading file including vector data
c       
         call txrdcol(Filevec,Column,Vecsz,Vector,vecnum,status)
         if ( status.ne.0 ) return

         IF ( x_dir .AND. vecnum.LT.Szx ) THEN
           CALL XWRITE
     &     (' Warning: vector dimension less than image size ',5)
           CALL XWRITE
     &    ('          image will be rescaled to vector dimension only! '
     &                ,5)
         ENDIF
         IF ( y_dir .AND. vecnum.LT.Szy ) THEN
           CALL XWRITE
     &     (' Warning: vector dimension less than image size ',5)
           CALL XWRITE
     &    ('          image will be rescaled to vector dimension only! '
     &                ,5)
         ENDIF
         DO j = vecnum+1,Szx
            Vector(j)=1.0
         ENDDO
         IF ( divide ) THEN
            DO j = 1, Szx
               if ( Vector(j).eq.0. ) then
                  call xwrite (' zero vector ', 15)
                  Vector(j) = 0.
               else
                  Vector(j)=1./Vector(j)
                  if (Vector(j).lt.Min_frac) then
                     call xwrite (' vector less than min_frac ', 15)
                     Vector(j)=0.
                  endif
               endif
            ENDDO
         ENDIF
c
c  vector multiplication on map
c
         IF ( x_dir ) then
            DO i = 1 , Szx
               DO j = 1, Szy
                  if ( .not.isrnull(Map(i,j)) ) then
                     Map(i,j) = factor*Vector(i)*Map(i,j)
                  endif
               ENDDO
            ENDDO
         ELSE IF ( y_dir ) then
            DO j = 1, Szy
               DO i = 1 , Szx
                  if ( .not.isrnull(Map(i,j)) ) then
                     Map(i,j) = factor*Vector(j)*Map(i,j)
                  endif
               ENDDO
            ENDDO
         ENDIF
       ELSE

c
c  performs a scalar sum or multiplication on map
c
         IF ( Mult ) THEN
            DO i = 1 , Szx
               DO j = 1 , Szy
                  if ( .not.isrnull(Map(i,j)) ) then
                     Map(i,j) = Factor*Map(i,j)
                  endif
               ENDDO
            ENDDO
         ELSE
            DO i = 1 , Szx
               DO j = 1 , Szy
                  if ( .not.isrnull(Map(i,j)) ) then
                     Map(i,j) = Factor+Map(i,j)
                  endif
               ENDDO
            ENDDO
         ENDIF
      ENDIF
c
c  Find min/max
c
      rmin = rnull()
      rmax = rnull()

      do i = 1, Szx
         do j = 1, Szy
            if ( .not.isrnull(Map(i,j)) ) then
               if ( isrnull(rmin) .or. Map(i,j).lt.rmin )
     &            rmin = Map(i,j)
               if ( isrnull(rmax) .or. Map(i,j).gt.rmax )
     &            rmax = Map(i,j)
            endif
         enddo
      enddo
c
c   Set min/max to zero for all-null image
c
      if ( isrnull(rmin) .and. isrnull(rmax) ) then
         rmin = 0
         rmax = 0
      endif
c
c update header
c
      dd = rmin
      call gheadd(mapid, 'DATAMIN', dd, 1, status)
      dd = rmax
      call gheadd(mapid, 'DATAMAX', dd, 1, status)

      RETURN
      END
