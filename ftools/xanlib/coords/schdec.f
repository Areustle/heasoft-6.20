      SUBROUTINE SCHDEC(Dec,String)
c
c change decimal degrees to a string containing deg mn sec 
c 
c
      character*(*) String
      character(80) xdelta
      integer*4 status
      REAL*8 Dec 
      string = xdelta(Dec,String)
      status = 0
      if(status.ne.0)then
       string='*** ** *****'
      endif
c
      RETURN
      END
