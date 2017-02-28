c
c          set_pgdev -- get PGPLOT device name
c

	subroutine set_pgdev(indevice)

        character*(*) indevice

        integer*4 lenstr
        integer lenact

        character cret*64
        integer     lret

        lenstr = lenact(indevice)

        if (indevice(1:1) .eq. '?') then
           call pgdev('?')
    
ccc           call trlog ('PGPLOT_DEVICE',13,cret,lret)
           call trlog ('PGPLOT_TYPE',11,cret,lret)


           indevice(:lret) = cret(:lret)
           lenstr = lret
           if (indevice(1:1) .eq. ' ') then
              indevice(1:1) = '?'
           end if
         end if
                 
        call setenv(indevice)


        return
	end
