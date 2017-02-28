
C******************************************************************************

        subroutine matrix_mult3(a, b, y)

        integer i, j, k
        double precision a(3,3), b(3,3), y(3,3), sum

        do 30 i=1, 3
           do 20 j=1, 3

              sum = 0.0D0
              do 10 k=1, 3
                 
                 sum = sum + a(i,k) * b(k,j)
                 
10	      continue
              y(i,j) = sum
           
20	   continue
30      continue

        end
