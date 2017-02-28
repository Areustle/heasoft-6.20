      subroutine xrforcesim(nbint,iflags,yi,syi,expi)
      implicit none
C     Description: forces newbins to be simultaneous by zeroing
C     those which aren't, affects light curve arrays yi,syi, and expi
      integer iflags(*),nbint
      real yi(nbint,*),expi(nbint,*),
     $     syi(nbint,*)
C     LOCAL
      integer iv,m,k,mm
      character(80) context
      
      iv = 0
      DO m = 1, iflags(10)
         DO k = 1, nbint
            IF (yi(k,m).LE.-1.1E34 .AND. yi(k,m).GT.-1.285E34) THEN
               DO mm = 1, iflags(10)
                  IF (yi(k,mm).GT.-1.1E34) THEN
                     iv = iv + 1
                     yi(k, mm) = -1.29E34
                     syi(k, mm) = -1.29E34
                     expi(k, mm) = -1.29E34
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ENDDO
      call xwrite(' ',5)
      WRITE (context, 1102) iv
 1102 FORMAT (1X, 9X, '   Warning: ', I7, ' Newbs. excluded to ',
     &     'force series simultaneousness !')
      call xwrite(context,5)
      return
      end
         
