
C******************************************************************************
C SUBROUTINE:
C      convolve
C
C DESCRIPTION:
C      Convolve an arbitrary kernel with an image(array).
C
C AUTHOR:
C      Dr. James Kent Blackburn / March '94
C
C MODIFICATION HISTORY:
C      Treatment of upper boundary handling in 'wrap' and 'reflect' modes
C      corrected by Mohan Rajagopal, Stanford, May 16, 1997.
C NOTES:
C
C USAGE:
C       call convolve(inimag,nxi,nyi,kernel,patch,nxk,nyk,
C      &              boundary,constant,outimag)
C
C ARGUMENTS:
C    inimag        Input Image
C    nxi,nyi       Image dimensions
C    kernel        Elliptical gaussian kernel array
C    patch         Patch of image that is size of kernel array
C    nx,ny         kernel dimensions
C    boundary      type of boundary condition
C    constant      value of constant used with CONSTANT B.C.
C    outimag       Output Image
C
C PRIMARY LOCAL VARIABLES:
C    i,j,ii,jj     Dummy loop counters
C    npx,npy       Location in image space + extended boundary
C    imgx,imgy     Location in image space
C    nxpat,nypat   Location in the patch
C    constflg      Flag set to true when the pixel value = constant
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine convolve(inimag,nxi,nyi,kernel,patch,nxk,nyk,
     &                    boundary,constant,outimag)
      integer  nxi,nyi,nxk,nyk
      double precision  inimag(nxi,nyi),outimag(nxi,nyi)
      double precision  kernel(nxk,nyk),patch(nxk,nyk),constant
      character*(*)  boundary
      integer i,j,ii,jj,npx,npy,nxpat,nypat,imgx,imgy
      logical constflg

      imgx = 0
      imgy = 0
      constflg = .false.
      do 40 j = 1, nyi
      do 30 i = 1, nxi
        nypat = 1
        do 25 npy = j - nyk/2, j + nyk/2
          if ((npy .ge. 1) .and. (npy .le. nyi)) then
            imgy = npy
          else if (npy .lt. 1) then
            if ( boundary .eq. 'CONSTANT' ) then
              constflg = .true.
            else if ( boundary .eq. 'NEAREST' ) then
              imgy = 1
            else if ( boundary .eq. 'REFLECT' ) then
              imgy = 1 - npy
            else if ( boundary .eq. 'WRAP' ) then
              imgy = nyi + npy
            end if
          else if (npy .gt. nyi) then
            if ( boundary .eq. 'CONSTANT' ) then
              constflg = .true.
            else if ( boundary .eq. 'NEAREST' ) then
              imgy = nyi
            else if ( boundary .eq. 'REFLECT' ) then
              imgy = nyi - (npy - nyi - 1)
            else if ( boundary .eq. 'WRAP' ) then
              imgy = (npy - nyi)
            end if
          endif
        nxpat = 1
        do 15 npx = i - nxk/2, i + nxk/2
          if ((npx .ge. 1) .and. (npx .le. nxi)) then
            imgx = npx
          else if (npx .lt. 1) then
            if ( boundary .eq. 'CONSTANT' ) then
              constflg = .true.
            else if ( boundary .eq. 'NEAREST' ) then
              imgx = 1
            else if ( boundary .eq. 'REFLECT' ) then
              imgx = 1 - npx
            else if ( boundary .eq. 'WRAP' ) then
              imgx = nxi + npx
            end if
          else if (npx .gt. nxi) then
            if ( boundary .eq. 'CONSTANT' ) then
              constflg = .true.
            else if ( boundary .eq. 'NEAREST' ) then
              imgx = nxi
            else if ( boundary .eq. 'REFLECT' ) then
              imgx = nxi - (npx - nxi -1)
            else if ( boundary .eq. 'WRAP' ) then
              imgx = (npx - nxi)
            end if
          endif
          if ( constflg ) then
            patch(nxpat,nypat) = constant
            constflg = .false.
          else
            patch(nxpat,nypat) = inimag(imgx,imgy)
          endif
          nxpat = nxpat + 1
  15    continue
        nypat = nypat + 1
  25    continue

        outimag(i,j) = 0.0d0
        do 20 jj = 1, nyk
        do 10 ii = 1, nxk
          outimag(i,j) = outimag(i,j) + patch(ii,jj)*kernel(ii,jj)
  10    continue
  20    continue
  30  continue
  40  continue

      return
      end
