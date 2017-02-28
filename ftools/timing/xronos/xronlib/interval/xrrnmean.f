      subroutine xrrnmean(iflags,nbint,yi,syi)
      implicit none
      integer iflags(*),nbint
      real yi(nbint,*),syi(nbint,*)
C     LOCAL
      real rv,xrrunmean
      integer k,iv,m
      external xrrunmean
      character(80) context


      DO m = 1, iflags(10)
         iv = 0
c     loop for running mean routine
         DO k = 1, nbint
            rv = xrrunmean(yi(1,m), syi(1,m), k, 1, nbint, iflags(17)
     &           , 1)
c     if corresponding point is a gap replace with running mean
            IF (yi(k,m).LT.-1.1E34) THEN
               yi(k, m) = rv
            ENDIF
         ENDDO
c     loop for replacing errors with 0. in filled gaps
         DO k = 1, nbint
            IF (syi(k,m).LT.-1.1E34 .AND. yi(k,m).GT.-1.1E34) THEN
               syi(k, m) = 0.
               iv = iv + 1
            ENDIF
         ENDDO
         WRITE (context, 1304) m, iv
         call xwrite(context,5)
 1304    FORMAT (1X, '    Ser.', I1, '   Warning:', I7,
     &        ' empty newbins replaced with running mean values ')
      enddo
      return
      end





