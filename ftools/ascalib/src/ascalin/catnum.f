        subroutine catnum(catname,name,n)
        character*(*) catname, name
        integer n
        
        integer i, j, k, p
        character(80) temp

        i = len(name)
        k = 1

        do while (i.gt. 0 .and. name(i:i) .eq. ' ') 
           i = i - 1
        end do
        j = i

        call int2str(n, k, temp)
        
        p = min(len(temp), k)
        do i=1, j
           catname(i:i) = name(i:i)
        end do
        do i=1, p
           catname(j+i:j+i) = temp(i:i)
        end do
        do i=j+p+1,len(catname)
           catname(i:i) = ' '
        end do
        
        end
