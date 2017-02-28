      subroutine xrexpprof(iflags,nbint,yi,syi)
      implicit none
      integer iflags(*),nbint
      real yi(nbint,*),syi(nbint,*)
C     Description
C        Fills light curve array yi with 1 for exposed bins and 
C        0 for non-exposed.  Puts dummy error in syi.  
C
C     LOCAL
      integer k,m
      character(80) context


      DO m = 1, iflags(10)
         DO k = 1, nbint
            IF (yi(k,m).GT.-1.1E34) THEN
               yi(k, m) = 1.
            ELSE
               yi(k, m) = 0.
            ENDIF
c     dummy error
            syi(k, m) = 1.E-3
         ENDDO
         WRITE (context, 1303) m
         call xwrite(context,5)
 1303    FORMAT (1X, '    Ser.', I1,
     &        '   Warning: only exposure profile',
     &        ' as requested !')
      enddo
      return
      end
