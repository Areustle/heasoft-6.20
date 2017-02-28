
        double precision function dremain (dividend,divisor)
        double precision dividend, divisor, temp
        integer truncval
        truncval=ifix(sngl(dividend/divisor))
        temp = (dividend/divisor)-dfloat(truncval)
        dremain = temp*divisor
        return
        end
