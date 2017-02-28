      SUBROUTINE binchan(startchan, endchan, nbinup, npha, 
     &                   eng, pha, nbin)
      IMPLICIT NONE
c
c  Perform binning on energy channels
c
c  I  startchan   (i)  Start channel
c  I  endchan     (i)  End channel
c  I  nbinup      (i)  Number of channels in bin
c  I  npha        (i)  Number of values in pha array
c I/O eng         (r)  Energies
c I/O pha         (r)  Distribution
c  O  nbin        (i)  Final number of bins
c
      INTEGER*4 startchan,endchan,nbinup,npha,nbin
      REAL*4 pha(*), eng(*)
c
c  Local variables
c
      INTEGER*4 i,j,kk,ij,ii
      REAL*4 phasum, esum, psum

      i = startchan
      j = endchan
      IF ( j-i.GT.nbinup ) THEN
         kk = 1
         ii = i
         DO WHILE ( ii.LE.j )
            phasum = 0
            esum = 0
            ij = 0
            i = ii
            DO WHILE ( ij.LT.nbinup .AND. i.LE.npha )
               IF ( i.LE.npha ) THEN
                  phasum = pha(i) + phasum
                  esum = eng(i) + esum
                  ij = ij + 1
                  i = ii + ij
               ENDIF
            ENDDO
            if(ij.GT.0) THEN
               eng(kk) = esum/ij
               pha(kk) = phasum
            else
               eng(kk) = 0
               pha(kk) = 0
               kk = kk - 1
            endif
            kk = kk + 1
            ii = ii + nbinup
         ENDDO
         kk = kk - 1
c
c smooting histo
c
         psum = 0.0
         DO 3 ii = 1 , kk
            IF( ii.EQ.1 )THEN
               pha(ii) = (pha(ii)+pha(ii+1))/2.
            ELSE IF(ii.EQ.kk) THEN
               pha(ii) = (pha(ii)+pha(ii-1))/2.
            ELSE
               pha(ii) = (pha(ii-1)+pha(ii)+pha(ii+1))/3.
            ENDIF
            psum = psum+pha(ii)
 3       CONTINUE
         DO 4 ii = 1, kk
            IF ( psum.GT.0.0 ) pha(ii) = pha(ii)/psum
c            write(*,*) ii, eng(ii),pha(ii),psum
 4       CONTINUE
      ELSE
         kk = 1
         if(eng(j).LE.0) eng(j)=1
         if(eng(i).LE.0) eng(i)=1
         eng(kk) = 10**((ALOG10(eng(j))+ALOG10(eng(i)))/2.0)
         pha(kk) = 1.0
      ENDIF
c     
      nbin = kk
      RETURN
      END
