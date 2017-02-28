*- isliteral.for - identify string constant
* 20 October 1993 : AMTP
      logical*4 function isliteral(string)
* Import :
      character*(*) string
* Local constant :
      character(1) quote
      parameter (quote='''')
* Local variable :
      integer*4 ls
* External reference :
      integer*4 lenact
*-
      ls=lenact(string)
      isliteral=((string( 1: 1).eq.quote).and.
     &           (string(ls:ls).eq.quote))
      return
      end
