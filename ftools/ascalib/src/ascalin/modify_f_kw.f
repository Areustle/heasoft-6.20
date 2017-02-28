                
        subroutine modify_f_kw(unit, keyword, value, decimals,
     &       comment, status)
        
        integer unit, decimals, status
        real value
        character*(*) keyword, comment
        
        call ftmkyf(unit, keyword, value, decimals, comment, status)
        if (status .eq. 202) then
           status = 0
           call ftpkyf(unit, keyword, value, decimals, comment, status)
        end if
        
        end
