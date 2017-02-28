      SUBROUTINE BARYCENTER(Map,Szx,Szy,Xpos,Ypos,Optbox,Good)
      IMPLICIT NONE
c
c  Calculates barycenter
c
c  Loops through, recalculating from new center or shrinking box
c  if center has remained constant, until box is <0.6*Optbox
c  or maxloop is reached.
c
c  Note on NULLs: As long as special NULL value is such that
c                 NINT(rnull()) = 0 no change is needed
c
c  I  map    (r)  Image map
c  I  szx/y  (i)  Size of image map
c I/O x/ypos (r)  Box center
c  I  optbox (i)  Optimal box for highest S/N (image coordinates)
c I/O good   (l)  Whether box is good or not
c
      integer*4 Szx, Szy, Optbox
      real*4 Map(Szx,Szy)
      real*4 Xpos, Ypos
      logical Good

      include '../include/io.inc'
c
c  Local variables
c
      INTEGER*4 iloop , maxloop , kx , ky , ii , jj , jjx, iix
      INTEGER*4 tmpbox
      REAL*4 oxpos, oypos, tmpbxh
      REAL*4 xparz , yparz , total , aj , valx , valy , scc
      
      oxpos = Xpos
      oypos = Ypos
      iloop = 0
      maxloop = 10
      tmpbox = Optbox
      DO WHILE ( .TRUE. )
         write(ZWRite,*) ' Bary box size, box center: ', tmpbox, oxpos,
     &                                                           oypos
         call xwrite(ZWRite, 25)
         tmpbxh = float(tmpbox)/2.
         kx = NINT(oxpos - tmpbxh)
         ky = NINT(oypos - tmpbxh)
         xparz = 0.
         yparz = 0.
         total = 0.
         write(ZWRite,*) ' Bary box xmin/max, ymin/max ', kx, kx+tmpbox,
     &                                                    ky, ky+tmpbox
         call xwrite(ZWRite, 25)
c
c  This algorithm considers every pixel in the box, Map(ii,jj)
c
c   -> aj is the distance from the center of the considered pixel,
c        (jj+0.5) to the box edge (ky) in the y direction
c
c   -> Map(jjx,iix) is the pixel directly opposite the considered
c        pixel across a diagonal drawn through the box from 
c        minimum x & y to maximum x & y. aj also corresponds to the
c        distance from the center of this opposite pixel to
c        to box edge (kx) in the x direction.
c
c      ---------
c      | | | |/|  * = considered pixel    Note: Pixels on the diagonal are
c      ---------  o = its opposite              their own opposite
c      |*| |/| |  / = the diagonal    
c   jj>---------
c      | |/| | |  aj = 2.5 in this illustration
c      --------- 
c      |/| |o| |  
c   ky>---------<iix
c      ^   ^      
c      kx  jjx     
c      ii          
c
c   Only the considered pixel is added to total as they need only 
c    be summed once.  Every pixel takes the role of considered and 
c    opposite once during the process. 
c
c   The sum of weighted distance is made by adding the value of the
c    considered pixel, Map(ii,jj), multiplied by aj to the 
c    to the variable for the y direction, yparz.  Similarly,
c    Map(jjx,iix)*aj is added to the x direction, xparz.
c
c   At the end, the sum of weighted distances is divided by the
c    sum of the Map values for all the pixels in the box, 
c    xparz/total and yparz/total.  Adding these to kx and ky,
c    respectively yields the location of the barycenter in 
c    image coordinates.
c
c        DO 50 ii = kx , kx + tmpbox - 1
         DO 50 ii = kx , kx + tmpbox
            IF ( ii.LE.0 .OR. ii.GT.Szx ) THEN
c              Source near image edge
               Good = .FALSE.
               write(ZWRite,*) ' barycenter: box outside image (x)',
     &                         ii,jj
                  call xwrite(ZWRite, 25)
               GOTO 50
            ENDIF
c           DO 20 jj = ky , ky + tmpbox - 1
            DO 20 jj = ky , ky + tmpbox
               IF ( jj.LE.0 .OR. jj.GT.Szy ) THEN
c                 Source near image edge
                  Good = .FALSE.
                  write(ZWRite,*) ' barycenter: box outside image (y)',
     &                            ii,jj
                  call xwrite(ZWRite, 25)
                  GOTO 20
               ENDIF
               aj = jj + 0.5 - ky
               jjx = jj - ky + kx
               iix = ii - kx + ky
               IF ( jjx.GT.0 .AND. jjx.LE.Szx ) THEN
                  IF ( iix.GT.0 .AND. iix.LE.Szy ) THEN
                     valx = nint(Map(jjx,iix))
                     valy = nint(Map(ii,jj))
                     xparz = xparz + aj*valx
                     yparz = yparz + aj*valy
                     total = total + valy
                  ENDIF
               ENDIF
 20         CONTINUE
 50      CONTINUE
         IF ( total.LE.0.0 ) THEN
            Good = .FALSE.
            call xwrite(' barycenter: total <=0', 15)
         ELSE
            Xpos = float(kx) + xparz/total
            Ypos = float(ky) + yparz/total
         ENDIF
         IF ( iloop.LE.maxloop .AND. Good ) THEN
            iloop = iloop + 1
            IF ( oxpos.EQ.Xpos .AND. oypos.EQ.Ypos ) THEN
               scc = FLOAT(tmpbox)*.8
               IF ( scc.LT.0.6*FLOAT(Optbox) ) GOTO 200
               tmpbox = NINT(scc)
            ENDIF
            oxpos = Xpos
            oypos = Ypos
            GOTO 100
         ENDIF
         GOTO 200
 100  ENDDO
 200  CONTINUE
      RETURN
      END
