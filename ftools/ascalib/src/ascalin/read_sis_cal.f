        
C******************************************************************************
C SUBROUTINE:
C
C     read_sis_cal
C
C DESCRIPTION:
C
C      opens and gets information from the calibration file
C
C AUTHOR/DATE:
C
C       Eric Gotthelf,    Jan 1993
C       ASTRO-D GOF, GSFC
C
C MODIFICATION HISTORY:
C       
C NOTES:
C
C USAGE:
C
C     call read_cal(filename, status)
C
C ARGUMENTS:
C
C     filename  - input calibration FITS file and extension number
C     status    - FITSIO status
C
C PRIMARY LOCAL VARIABLES:
C
C     iunit       - FORTRAN I/O unit for calibration file
C     matrix_size - calibration map size
C     cal         - common block containing calibration data
C
C     fitsio variables - nullval, iunit, status, bitpix, naxis, naxes(99)
C                        pcount, group, rwmode, blocksize, hdutyp, gcount
C                        simple, extend, anyf
C
C CALLED ROUTINES:
C
C     subroutine parse_sis_cal_hdr - parse header information
C     subroutine read_binext_image - read in a FITS image from a bin xtention
C     subroutine fcecho            - echo message to terminal
C     function fcstln              - returns index of last non-blank character
C     subroutine ftxxxx            - FITSIO calls
C
C******************************************************************************

        subroutine read_sis_cal(filename, iunit, cal_time, status)
      
        implicit none

        include 'asca_defs.inc'
        
C     SUBROUTINE ARGUMENTS:
        
        integer status
        character*(*) filename, cal_time
        
C     LOCAL VARIABLES:
        
        integer iunit, idim, jdim, stat, bitpix, naxis, naxes(99)
        integer rwmode, blocksize, gcount, pcount, cal_inst
        logical simple, extend

        character(80) contxt, cal_det

        include 'asca_common.inc'

C     START:
        
C     INTIALIZE VARIABLES:
        
        stat = 0
        status = 0
        rwmode = 0
        blocksize = 0
        
C     START:
        
C     OPEN THE FITS FILE:
        
        call ftopen(iunit, filename, rwmode, blocksize, status)
        
        if (status .eq. 0) then
           
C     READ PRIMARY HEADER INFO:
           
           call ftghpr(iunit, 99, simple, bitpix, naxis, naxes,
     &          pcount, gcount, extend, status)
           
           if (status .eq. 0) then
              
C     PARSE HEADER DATA:
              
              call ftgkys(iunit, 'INSTRUME', cal_det, contxt, status)

              if (status .eq. 0) then
                 
                 
                 cal_inst = -1
                 if (index(cal_det, 'SIS0') .ne. 0) cal_inst = SIS0
                 if (index(cal_det, 'SIS1') .ne. 0) cal_inst = SIS1
                 if (index(cal_det, 'SIS2') .ne. 0) cal_inst = SIS2
                 
                 if (cal_inst .eq. -1) then
                    write(contxt,'(a46, 1x, a33)')
     &                   'ERROR: Dont recognize instrument :', cal_det
                    status = 1
                 end if
                 if (cal_inst .ne. detector) then
                    write(contxt,'(a46, 1x, a33)')
     &                   'Cal and data file INSTRUME differ :', cal_det
                    status = 1
                 end if
                 
                 if (status .eq. 0) then
                    
                    call parse_sis_cal_hdr(iunit, cal_time, status)
                    
                    if (status .eq. 0) then
                       
                       call ftclos(iunit, status)
                       
                       if (status .eq. 0) then
                          
                          return
                          
                       else
                          contxt = 'Error closing SIS CAL file '
                       end if
                       
                    else
                       contxt = 'Error parsing SIS CAL file header '
                    end if

                 end if
                 
              else
                 contxt = 'Unable to get INSTRUME keyword'
              end if
              
           else
              contxt = 'Unable read SIS CAL file primary header'
           end if 
           
           call ftclos(iunit, stat)
           
        else
           contxt = 'Unable to open SIS CAL file: '//filename
        end if 
        
        call fcerr(contxt)
        
        return
        
        end
