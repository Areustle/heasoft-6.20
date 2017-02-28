c
c          pgdev -- set output device
c
	subroutine pgdev(file)
        implicit none

        character*(*) file
     
        integer       LR,LDEV,LF
        integer       ier

        character*256 req,device

        integer lenact


        call pgqinf ('DEVICE',device,ldev)
        if ( device(1:1).eq.'?' ) then
           device = ' '
           ldev = 0
        endif
        if (file(1:1) .eq. '?') then
            if (ldev .le. 0) then
              call pgldev
 10           call xcread('Graphics device/type (? to see list): ', req,
     &                    ier)
              lr = LENACT(req)
              if (ier .ne. 0)
     &            call xwrite(' error reading device specification',5)
              if (lr .lt. 1) goto 10
              if (req(1:1) .eq. '?') then
                call pgldev
                goto 10
              end if

              call setenv (req)

             end if
            else if (file(1:1) .eq. '!') then
             call pgldev
            else
             lf=index(file,' ')-1
             if (lf .eq. -1) lf=lenact(file)

             call setenv(file)

           end if

  
        return
	end
