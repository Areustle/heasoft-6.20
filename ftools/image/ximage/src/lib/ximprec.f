      subroutine ximprec (ra, dec, equ1, equ2)
      implicit none
c
c  Wrapper to simplify calls to precess sky coordinates
c
c I/O ra,dec (d)  Sky coordinates
c  I  equ1   (i)  Input equinox
c  I  equ2   (i)  Output equinox
c
      real*8 ra, dec
      integer equ1, equ2
      
      if ( equ1.eq.equ2 ) return

      IF ( equ2.EQ.1950 ) THEN
         CALL PREC(ra,dec,equ1,1)
      ELSE
         CALL PREC(ra,dec,equ1,1)
         CALL PREC(ra,dec,equ2,2)
      ENDIF
      
      return
      end
