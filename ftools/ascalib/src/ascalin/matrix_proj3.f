
        subroutine matrix_proj3(a, v, y)

        integer i, k
        double precision a(3,3), v(3), y(3), sum

        do 20 i=1, 3

           sum = 0.0D0
           do 10 k=1, 3
              sum = sum + a(i,k) * v(k)
10	   continue
           y(i) = sum

20	continue
        
        end
