c**********************************************************************
c  This function is a way of converting a double precision number
c into a truncated value. It takes as input a double precision and
c returns a truncated double precision value by using DMOD and DNINT
c functions to perform this.
c**********************************************************************
      
      double precision function dtruc(dval)
      
      double precision dval, dmodval, done, dhold
      done=1.0d0

      dmodval = dmod((dval+1.0d-10),done)
      dhold = (dval - dmodval)+1.0d-10
      dtruc = dnint(dhold)

      return
      end
      
      
