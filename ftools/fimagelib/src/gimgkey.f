C******************************************************************************
C SUBROUTINE:
C      gimgkey
C
C DESCRIPTION:
C       gets the coordinate transformation keywords from the mask/image file
C       or from the par file's hidden parameters.
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       March, 1993
C
C MODIFICATION HISTORY:
C
C $Log: gimgkey.f,v $
C Revision 1.2  2013/05/21 19:08:17  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 1.1  1998/06/02 16:48:19  peachey
C Routine specific to fimage package. This source code was moved here from
C the libftools.a (library/) source tree.
C
C
C NOTES:
C
C USAGE:
C       call gimgkey (imgfile, xcolname, ycolname, status)
C
C ARGUMENTS:
C       xcolname    - name of the X column
C       ycolname    - name of the Y column
C       status      - status of the operation
C
C PRIMARY LOCAL VARIABLES:
C      context   - error message
C
C CALLED ROUTINES:
C
C******************************************************************************
      subroutine gimgkey (imgfile, xcolname, ycolname, status)

      character*(*) imgfile, xcolname, ycolname
      integer status

      character(80) context, sval, keyword
      character(160) filename
      double precision dval
      integer ival, htype, block, extnumb, iunit

      double precision crval(2), cdelt(2)
      integer crpix(2)
      common / cvalues / crval, cdelt, crpix

C initialize variables
      iunit = 15

C first check the image file for the values we need
C if no image file is specified, jump down to the hidden parameter part
      if ((imgfile .eq. ' ') .or. (imgfile .eq. '-')) goto 999

C get the input fits filename and extension
      call fcpars (imgfile, filename, extnumb, status)

C EAG 8/25/93 default to primary extension
      if (extnumb .eq. -99) extnumb = 0

C open the image file
      call ftopen (iunit, filename, 0, block, status)
      if (status .ne. 0) then
         context = ' Cannot open image file - using hidden parameters'
         call fcecho (context)
         status = 0
         goto 999
      endif

C move to the extension
      call ftmahd (iunit, extnumb+1, htype, status)
      if (status .ne. 0) then
         context = ' Error moving to requested extension'
         call fcerr (context)
         goto 998
      endif

C check for reasonable extension type
      if (htype .ne. 0) then
         context = ' Requested extension is not an image'
         call fcerr (context)
         goto 998
      endif

C read the keywords we need
      keyword = 'XCOLNAME'
      call ftgkys (iunit, keyword, sval, context, status)
      if (status .eq. 0) xcolname = sval
      status = 0
      keyword = 'YCOLNAME'
      call ftgkys (iunit, keyword, sval, context, status)
      if (status .eq. 0) ycolname = sval
      status = 0

      keyword = 'CRVAL1'
      call ftgkyd (iunit, keyword, dval, context, status)
      if (status .eq. 0) crval(1) = dval
      status = 0
      keyword = 'CRVAL2'
      call ftgkyd (iunit, keyword, dval, context, status)
      if (status .eq. 0) crval(2) = dval
      status = 0

      keyword = 'CDELT1'
      call ftgkyd (iunit, keyword, dval, context, status)
      if (status .eq. 0) cdelt(1) = dval
      status = 0
      keyword = 'CDELT2'
      call ftgkyd (iunit, keyword, dval, context, status)
      if (status .eq. 0) cdelt(2) = dval
      status = 0

      keyword = 'CRPIX1'
      call ftgkyj (iunit, keyword, ival, context, status)
      if (status .eq. 0) crpix(1) = ival
      status = 0
      keyword = 'CRPIX2'
      call ftgkyj (iunit, keyword, ival, context, status)
      if (status .eq. 0) crpix(2) = ival
      status = 0

 998  call ftclos (iunit, status)

 999  continue
      return
      end
