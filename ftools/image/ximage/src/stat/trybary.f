      SUBROUTINE TRYBARY(Map,Szx,Szy,Xpos,Ypos,Optbox,Status)
      IMPLICIT NONE
c
c  Calculates barycenter by local search method
c
c  Uses barycenter results from first guess a nearby point to get 
c  new guess, and repeats.  If guess doesn't change (much) shrink box.
c  This continues until box is <0.6*Optbox or maxloop is reached.
c
c  I  map    (r)  Image map
c  I  szx/y  (i)  Size of image map
c I/O x/ypos (r)  Box center
c  I  optbox (i)  Optimal box for highest S/N (image coordinates)
c I/O status (i)  Error flag (0=OK)
c
      integer*4 Szx, Szy, Optbox, Status
      real*4 Map(Szx,Szy)
      real*4 Xpos, Ypos

      include '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 iloop , maxloop, tmpbox
      REAL*4 oxpos, oypos, nxpos, nypos, xlast, ylast
      REAL*4 xjump , yjump , scc, eps
      REAL*4 xpos1, ypos1, xpos2, ypos2
      REAL*4 xdif1, ydif1, xdif2, ydif2
      LOGICAL converge
      
c  Initialize to avoid warning
      xlast = 0.
      ylast = 0.
c  --
      oxpos = Xpos
      oypos = Ypos
      iloop = 0
      maxloop = 10
      tmpbox = Optbox

      eps = 1e-5
      xjump = 1.
      yjump = 1.
      converge = .FALSE.

      DO WHILE ( .TRUE. )
c
c  Calculate barycenter distance from starting box center (guess)
c
         xpos1 = oxpos
         ypos1 = oypos
         call offbary(Map,Szx,Szy,xpos1,ypos1,tmpbox,xdif1,ydif1,
     &                Status)
         if ( Status.ne.0 ) return
c
c  Jump in direction from starting box center to calculated barycenter
c  and calculate barycenter from that point
c
         if ( abs(xdif1).le.xjump .and. abs(ydif1).le.yjump ) then
            if ( xdif1.gt.0. ) then
               xpos2 = xpos1 + xjump
            elseif ( xdif1.lt.0 ) then
               xpos2 = xpos1 - xjump
            else
               xpos2 = xpos1
            endif
            if ( ydif1.gt.0. ) then
               ypos2 = ypos1 + yjump
            elseif ( ydif1.lt.0 ) then
               ypos2 = ypos1 - yjump
            else
               ypos2 = ypos1
            endif
c
            call offbary(Map,Szx,Szy,xpos2,ypos2,tmpbox,xdif2,ydif2,
     &                   Status)
c
c  Based on distance between the box centers and their calculated
c  barycenters, weigh the two results to come up with approximation
c  for next guess. Then, shrink the jump distance to the minimum 
c  distance between the new guess and the two barycenter results
c
            if ( xdif1*xdif2.ge.0 ) then
               if ( abs(xdif1) .lt. abs(xdif2) ) then
                  nxpos = xpos1
               else
                  nxpos = xpos2
               endif
               xjump = 0.
            else
               nxpos = xpos1 + xjump*xdif1/(abs(xdif1)+abs(xdif2))
               xjump = min(abs(nxpos-(xpos1+xdif1)),
     &                     abs(nxpos-(xpos2+xdif2)))
            endif
            if ( ydif1*ydif2.ge.0 ) then
               if ( abs(ydif1) .lt. abs(ydif2) ) then
                  nypos = ypos1
               else
                  nypos = ypos2
               endif
               yjump = 0.
            else
               nypos = ypos1 + yjump*ydif1/(abs(ydif1)+abs(ydif2))
               yjump = min(abs(nypos-(ypos1+ydif1)),
     &                     abs(nypos-(ypos2+ydif2)))
            endif
         else
            nxpos = xpos1+xdif1
            nypos = ypos1+ydif1
         endif

         IF ( iloop.LE.maxloop .AND. Status.eq.0 ) THEN
            iloop = iloop + 1
c
c  If the guess hasn't moved (<eps) since the last iteration,
c  shrink the box
c
            IF ( abs(oxpos-nxpos).lt.eps .AND. 
     &           abs(oypos-nypos).lt.eps ) THEN
               converge = .TRUE.
               xlast = nxpos
               ylast = nypos
               scc = FLOAT(tmpbox)*.8
               IF ( scc.LT.0.6*FLOAT(Optbox) ) GOTO 200
               tmpbox = NINT(scc)
               xjump = 1.
               yjump = 1.
            ENDIF
            oxpos = nxpos
            oypos = nypos
            GOTO 100
         ENDIF
         GOTO 200
 100  ENDDO
 200  CONTINUE
c
c  Use the tightest value calculated for last box. Don't want maxloop
c  aborting in middle to skew result
c
      if ( converge ) then
         Xpos = xlast
         Ypos = ylast
      else
         Xpos = nxpos
         Ypos = nypos
      endif

      RETURN
      END
