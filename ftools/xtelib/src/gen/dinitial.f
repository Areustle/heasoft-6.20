
      subroutine dinitial(inumber,value)
      integer inumber,i
      double precision value(*)

      do 10 i=1,inumber
        value(i)=0.0d0
10    continue

      return
      end
