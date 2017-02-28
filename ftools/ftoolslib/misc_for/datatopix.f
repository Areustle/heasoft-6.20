
C******************************************************************************
C FUNCTION:
C      datatopix
C
C DESCRIPTION:
C       converts a value from data to pixel coordinates using
C       CRPIXn, CRVALn and CDELTn keywords
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       April, 1993
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C       converted_value = datatopix(value, axis, offset)
C
C ARGUMENTS:
C       value   - value to be converted
C       axis    - 1 for X and 2 for Y
C       offset  - whether to apply the offset or not
C
C PRIMARY LOCAL VARIABLES:
C       crval1  - Coordinate value of X reference pixel
C       crval2  - Coordinate value of Y reference pixel
C       crpix1  - X axis reference pixel number
C       crpix2  - Y axis reference pixel number
C       cdelt1  - X axis coordinate increment
C       cdelt2  - Y axis coordinate increment
C      context   - error message
C
C CALLED ROUTINES:
C
C****************************************************************************
      double precision function datatopix (value, axis, offset)

      double precision value
      integer axis
      logical offset

      double precision crval(2), cdelt(2)
      integer crpix(2)
      common / cvalues / crval, cdelt, crpix

      if (offset) then
         datatopix = (value - crval(axis)) / cdelt(axis) +
     &        dble(crpix(axis))
      else
         datatopix = value / cdelt(axis)
      endif

      return
      end
