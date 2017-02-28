C******************************************************************************
C SUBROUTINE:
C
C       update_coordpro
C
C DESCRIPTION:
C
C      Update COORDPRO keyword.
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,	  MARCH 25 1994
C       ASTRO-D GOF, GSFC
C
C******************************************************************************

        subroutine update_coordpro(unit, coordpro, status)
        integer unit, coordpro, status

        integer stat, hdtype
        character(40) taskname, text, contxt

        common /task/ taskname

        stat = 0

        call ftmahd (unit, 1, hdtype, stat)
        contxt = 'Error moving to primary header (3)'
        
        if (stat .eq. 0) then

           if (coordpro .lt. 0 .and. status .ne. 0) then

c     text = 'UNCALIBRATED'
              call ftgkys(unit, 'COORDPRO', text, '&', stat) 
              if (stat .eq. 202) stat = 0
              contxt = 'Error updating COORDPRO keyword '

           else

              if (status .eq. 0) then
                 
                 if (coordpro .gt. 0) then
                    
                    write(text, '(a19)') taskname
                    
                 else if (coordpro .eq. 0) then
                    
                    text = 'SUSPECT1'
                    
                 else if (coordpro .gt. 0) then                 
                    
                    text = 'SUSPECT2'
                    
                 end if
                 
              else 
                 
                 if (coordpro .eq. 0) then
                    
                    text = 'CAL_ERROR'
                    
                 else if (coordpro .gt. 0) then
                    
                    text = 'FILE_ERROR'
                    
                 end if
                 
              end if
              
              call ftmkys(unit, 'COORDPRO', text, '&', stat) 
              if (stat .eq. 202) stat = 0
              contxt = 'Error updating COORDPRO keyword '
                           
           end if
           
        end if

        if (stat .ne. 0) then
           status = stat
           call fcerr(contxt)
        endif

c        write(contxt,'(a16,a20,i3)') '    COORDPRO = ',text,coordpro
c        call fcecho(contxt)

        end



