c
      SUBROUTINE xrtyana(nanal, rebin, iflags)
c
c ls  21/9/88 to type no. of analysis points and set iflags(13)
c
c     I   nanal = no. of analysis points before rebinning
c     I   rebin = rebinning constant (>1 linear, <-1 geom. series)
c     I/O iflags = output file flags (to set iflags(13)= no.of point after
c                                     rebinning)
c
c
      implicit none
      INTEGER*4 nanal, k, iflags(*)
      REAL*4 rebin, rbe
      character(80) context
c
      iflags(13) = 0
      IF (abs(rebin).GT.1.) THEN
         IF (rebin.GT.1.) THEN
c linear rebinning
            iflags(13) = (nanal-1)/int(rebin) + 1
c geom.  rebinning
         ELSEIF (rebin.GT.-999. .AND. rebin.LT.-1.) THEN
            rbe = 1.
            k = 0
 1          CONTINUE
            rbe = rbe*abs(rebin)
            k = k + int(rbe)
            iflags(13) = iflags(13) + 1
            IF (k.LT.nanal) THEN
               GOTO 1
            ENDIF
         ENDIF
         WRITE (context, 1000) nanal, iflags(13)
         call xwrite(' ',5)
         call xwrite(context,5)
         call xwrite(' ',5)
 1000    FORMAT (' ', I7, ' analysis results per intv.',
     &          ' will be rebinned to ', I7)
      ELSE
         iflags(13) = nanal
         WRITE (context, 1001) nanal
         call xwrite(' ',5)
         call xwrite(context,5)
         call xwrite(' ',5)
 1001    FORMAT (' ', I7, ' analysis results per interval')
      ENDIF
c
      RETURN
      END
c
c
