
        double precision function dot_prod(a, b, n)

        integer k, n 
        double precision a(n), b(n), sum

        sum = 0.0D0
        do 10 k=1, n
           sum = sum + a(k) * b(k)
10	continue
        
        dot_prod = sum

        end


