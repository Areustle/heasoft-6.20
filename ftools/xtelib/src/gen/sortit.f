 
        subroutine sortit(chararay,intaray,no)
        implicit none

        integer nb

c      Define all of the common parameters used in the arrays.
        parameter (nb = 512)
        
        character(80) chararay(*)
        character(40) ctemp(nb)
        integer intaray(*),no,ibig,i,j,maxit,itemp(nb),k
           
        ibig=1000
        k=0
        
        do 20 i=1,no
          maxit=ibig

          do 30 j=1,no

            if(maxit.ge.intaray(j))then
              maxit=intaray(j)
              k=j
            endif

30        continue
          
c      Store things into temporary arrays such that they are now
c      in the proper time order.

          ctemp(i)=chararay(k)
          itemp(i)=intaray(k)
          intaray(k)=ibig
              
20      continue

c      Now move all of the files back into the original arrays.
        do 40 i=1,no
          chararay(i)=ctemp(i)
          intaray(i)=itemp(i)
40      continue
        
        return
        end
