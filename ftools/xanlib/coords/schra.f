      SUBROUTINE SCHRA(Ra,string)
C
C CHANGE ra in degs to a string containing hr, min and sec 
C 
c
      character*(*) string
      character(80) xalpha
      REAL*8 Ra 
      INTEGER*4 status
c
         string = xalpha (Ra,status)
         if(status.ne.0)then
          string='** ** ******'
         endif
      RETURN
      END
