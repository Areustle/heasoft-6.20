
C******************************************************************************
C     SUBROUTINE:
C     
C     read_cal
C     
C     DESCRIPTION:
C     
C     opens and gets information from the calibration file
C     
C     AUTHOR/DATE:
C     
C     Eric Gotthelf,      Jan 1993
C     ASTRO-D GOF, GSFC
C     
C     MODIFICATION HISTORY:
C     
C     NOTES:
C     
C     USAGE:
C     
C     call read_cal(filename, status)
C     
C     ARGUMENTS:
C     
C     filename  - input calibration FITS file and extension number
C     status    - FITSIO status
C     
C     PRIMARY LOCAL VARIABLES:
C     
C     iunit       - FORTRAN I/O unit for calibration file
C     matrix_size - calibration map size
C     cal         - common block containing calibration data
C     
C     fitsio variables - nullval, iunit, status, bitpix, naxis, naxes(99)
C     pcount, group, rwmode, blocksize, hdutyp, gcount
C     simple, extend, anyf
C     
C     CALLED ROUTINES:
C     
C     subroutine parse_CAL_header  - parse header information
C     subroutine read_binext_image - read in a FITS image from a bin xtention
C     subroutine fcecho            - echo message to terminal
C     function fcstln              - returns index of last non-blank character
C     subroutine ftxxxx            - FITSIO calls
C     
C******************************************************************************

        subroutine read_gis_cal(filename, iunit, cal_time,
     &                calxo, calyo, status)
        
        implicit none
        include 'asca_defs.inc'
        
        integer maxdims
        parameter (maxdims = 99)
        
C     SUBROUTINE ARGUMENTS:
        
        integer iunit, calxo, calyo, status
        character*(*) filename, cal_time
        
C     LOCAL VARIABLES:
        
        integer matrix_size, idim, jdim, cal_inst, cal_meth, ip, jp
        real x, y, dx, dy, tx, ty, sx, sy, det_x_offset, det_y_offset
        character(80) contxt, cal_det, cal_pos

C     FITS DECLARATIONS
        
        integer stat
        integer bitpix, naxis, naxes(maxdims), pcount
        integer rwmode, blocksize, gcount, ghversion
        logical simple, extend
        
        include 'asca_common.inc'
        
C     START:
        
C     INTIALIZE VARIABLES:
        
        stat = 0
        rwmode = 0
        cal_meth = -1
        cal_inst = -1
        matrix_size = gis_size

C     START:
        
C     OPEN THE FITS FILE:
        
        call ftopen(iunit, filename, rwmode, blocksize, status)
        
        if (status .eq. 0) then
           
C     READ PRIMARY HEADER INFO:
           
           call ftghpr(iunit, maxdims, simple, bitpix, naxis, naxes,
     &          pcount, gcount, extend, status)
           
           if (status .eq. 0) then
              
C     PARSE HEADER DATA:
           
              call ftgkys(iunit, 'INSTRUME', cal_det, contxt, status)
              call ftgkys(iunit, 'POS_DET', cal_pos, contxt, status)

              if (status .eq. 0) then
                 
                 if (index(cal_pos, 'FLF' ) .ne. 0) cal_meth = FLF
                 if (index(cal_pos, 'POW2') .ne. 0) cal_meth = POW2
                 if (index(cal_det, 'GIS2') .ne. 0) cal_inst = GIS2
                 if (index(cal_det, 'GIS3') .ne. 0) cal_inst = GIS3
                 
                 if (cal_meth .ne. pos_meth) then 
                    write(contxt,'(a45, 1x, a32)') 
     &                'Error: Cal and data file POS_DET differ; CAL:',
     &                   cal_pos
                    call fcerr (contxt)
                    status = 1
                 end if
                 
                 if (cal_inst .ne. detector) then 
                    write(contxt,'(a46, 1x, a31)') 
     &                'Error: Cal and data file INSTRUME differ; CAL:',
     &                   cal_det
                    call fcerr (contxt)
                    status = 1
                 end if
                 
              else

                 call fcerr('Unable to get POS_DET or INSTRUME keyword')
                 
              end if              

              if (status .eq. 0) call parse_giscal_header(
     &             iunit, cal_time, calxo, calyo, status)

              if (status .eq. 0) then

C     READ IN THE GAIN MAP:
                 
                 call read_binext_image(iunit, 2, matrix_size, 
     &                idim, jdim, gis_gain, status)
                 
                 if (status .eq. 0) then
                    
C     READ IN THE X ERROR MAP:
                    
                    call read_binext_image(iunit, 4, matrix_size, 
     &                   idim, jdim, deltax, status)
                    
                    if (status .eq. 0) then
                       
C     READ IN THE Y ERROR MAP:

                       call read_binext_image(iunit, 6, matrix_size,
     &                      idim, jdim, deltay, status)

                       if (status .eq. 0) then

C     READ IN THE RT MAP:

                          call read_binext_image(iunit, 7, idim, 
     &                         jdim, matrix_size, rt_map, status)
                          
                          if (status .eq. 0) then
                             
C     CLOSE THE FITS FILE:
                             
                             call ftclos(iunit, status)
                             
                             if (status .eq. 0) then
                                
C     ALL WENT WELL, RETURN: 
                                return
                                
                             else
                                contxt = 'Error closing GIS CAL file '
                             end if
                             
                          else
                             contxt = 
     &                            'Error reading GIS CAL file RT map'
                          end if 
                          
                       else
                          contxt = 
     &                         'Error reading GIS CAL file Y error map'
                       end if 
                       
                    else
                       contxt = 
     &                      'Error reading GIS CAL file X error map'
                    end if 
                    
                 else
                    contxt = 'Error reading GIS CAL file gain map '
                 end if 
                 
              else
                 contxt = 'Error parsing GIS CAL file header '
              end if 
              
           else
              contxt = 'Unable read GIS CAL file primary header'
           end if 

           call ftclos(iunit, stat)
           
        else
           contxt = 'Unable to open GIS CAL file: '//filename
        end if 

        call fcerr(contxt)
   
        return
        
        end
