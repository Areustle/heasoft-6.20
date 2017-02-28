
c**********************************************************************
c     This routine will either combine an integer and fractional
c part of a value into a single double precision value - if
c LSEPARATE eq FALSE. If LSEPARATE is true then it will take
c a double precision number and return and integer and a fractional
c part.
c
c dvalue      - d (input or output)
c ivalue      - i (output or input)
c fvalue      - d (output or input)
c lseparate   - l (True   or False)
c**********************************************************************

      subroutine d2if(dvalue,ivalue,fvalue,lseparate)

      double precision dvalue, fvalue
      integer ivalue
      logical lseparate

      if(.not.lseparate)then
        dvalue=dfloat(ivalue)+fvalue
        return
      endif

      if(lseparate)then
        ivalue=dint(dvalue)
        fvalue=dvalue-(dfloat(ivalue))
        return
      endif

      return
      end
