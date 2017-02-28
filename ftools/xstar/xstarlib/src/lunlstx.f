C------------------------------------------------------------------------------
        subroutine lunlstx(iounit)

C       generic routine to manage logical unit numbers in the range 10-49
C       
C       I/O  (i) iounit - The logical unit number to be allocated/freed
C       
C       James Peachey, HEASARC/GSFC/NASA  Hughes STX, November, 1996
C       Copied with minor changes from the FITSIO routine ftxiou.

        integer iounit,i
        integer array(40)
        save array
        data array/40*0/

c        write (6,*)'entering lunlstx',iounit
c        
        if (iounit .eq. 0)then
C           get an unused logical unit number
            do 10 i=40,1,-1

C        The following would be a more robust way of testing for
C        an available unit number, however, this cannot work
C        when building XANLIB using the IRAF/SPP version, because
C        IRAF does not use Fortran I/O.
C
C                inquire(unit=iounit, exist=exists, opened=open)
C                if(exists .and. .not. open)then
C                    array(iounit-9)=1
C                    return
C                end if

c               write (6,*)'i,array(i)',i,array(i)
               if (array(i) .eq. 0)then
                     array(i)=1
                     iounit=i+9
c                     write (6,*)'allocating:',iounit,i
                     return
                 end if
10          continue
C           error: all units are allocated
            iounit=-1
            call xaerror(
     &           'GETLUNx has no more available unit numbers.', 1)

        else if (iounit .eq. -1)then
C           deallocate all the unit numbers
            do 20 i=1,40
                 array(i)=0
20          continue

        else
C            deallocat a specific unit number
             if (iounit .ge. 10 .and. iounit .le. 49)then
c                write (6,*)'deallocating:',iounit,iounit-9
                array(iounit-9)=0
             end if
        endif
        end
