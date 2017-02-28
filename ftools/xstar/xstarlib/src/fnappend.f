      subroutine fnappend(knam,nint)
      character*16 knam
      if (nint.gt.9) then
          write (knam(3:4),'(i2)')nint
        else
          write (knam(3:3),'(a1)')'0'
          write (knam(4:4),'(i1)')nint
        endif
      return
      end
