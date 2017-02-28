
C******************************************************************************
C SUBROUTINE:
C      putimage
C
C DESCRIPTION:
C      Put the image into the extension with correct datatype.
C
C AUTHOR:
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C             call putimage(ounit,dtype,pcount,gcount,bscale,
C     &                     bzero,naxis1,naxis2,image,status)
C
C ARGUMENTS:
C    ounit         Output unit number
C    dtype         Image data type (8->B,16->I,32->J,-32->E,-64->D)
C    pcount        Value of PCOUNT keyword
C    gcount        Value of GCOUNT keyword
C    bscale        Image scale factor
C    bzero         Image offset factor
C    naxis1        Number of pixels along X-axis
C    naxis2        Number of pixels along Y-axis
C    image         Smoothed image to be output
C    status        FITSIO error status flag
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C    ft____        FITSIO routines
C
C******************************************************************************
      subroutine putimage(ounit,dtype,pcount,gcount,bscale,
     &                    bzero,naxis1,naxis2,image,status)
      integer ounit,dtype,pcount,gcount,naxis1,naxis2,status
      double precision bscale,bzero,image(naxis1,naxis2)
      integer group,naxis,naxes(2)

      group = 1
      naxis = 2
      naxes(1) = naxis1
      naxes(2) = naxis2
      call ftpdef(ounit,dtype,naxis,naxes,pcount,gcount,status)
      call ftpscl(ounit,bscale,bzero,status)
      call ftp2dd(ounit,group,naxis1,naxis1,naxis2,image,status)

      return
      end
