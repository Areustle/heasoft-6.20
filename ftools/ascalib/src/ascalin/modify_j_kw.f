
        subroutine modify_j_kw(unit, keyword, value, comment, status)
        
        integer unit, status, value
        character*(*) keyword, comment
        
        call ftmkyj(unit, keyword, value, comment, status)
        if (status .eq. 202) then
           status = 0
           call ftpkyj(unit, keyword, value, comment, status)
        end if
        
        end
