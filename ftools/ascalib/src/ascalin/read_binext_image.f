        
        subroutine read_binext_image (iunit, iext, isize, idim, 
     &       jdim, matrix,status)
        
C     subroutine arguments:
        
        implicit none
        
        integer maxdims
        parameter(maxdims=4)
        integer iunit, iext, isize, idim, jdim, status
        real matrix(isize, *)
        
C     local variables: 
C     FITS declarations
        
        real nullval
        integer hdutyp, bitpix,naxis,naxes(maxdims),pcount,gcount
        integer group
        logical simple,extend,anyf
        
C     INTIALIZE VARIABLES:
        
C     START:
        
C     MOVE TO EXTENTION AND READ FITS IMAGE:
        
        call ftmahd(iunit,iext+1,hdutyp,status)
        
        call ftghpr(iunit,4,simple,bitpix,naxis,naxes,pcount,gcount,
     &       extend,status)
        
C     READ AN ADDITIONAL INTEGER KEYWORD AND A COMMENT AND HISTORY KEYWORD:
        
        if (naxis .eq. 2) then
           idim = naxes(1)
           jdim = naxes(2)
           if (idim .le. isize .and. jdim .le. isize) then
              
C     READ THE PRIMARY ARRAY OF DATA:
              
           group = 1
           nullval = 0.0 
              call ftg2de(iunit,group,nullval,isize,idim,jdim,
     &             matrix,anyf,status)
           else
              call 
     &        fcerr('Error reading in FITS image - NAXIES too large')
              status = 10
           end if
        else
           call fcerr(
     &          'Error reading in FITS image - NAXIS not equal to 2')
           status = 10
        end if
        
        return
        end
