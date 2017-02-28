

C*****************************************************************************
C      dataty
C
C DESCRIPTION:
C      Determines the bitpix for an image based on a datatype parameter value
C
C AUTHOR:
C      Emily A. Greene
C       Hughes STX
C       7/14/93
C
C MODIFICATION HISTORY:
C
C Notes:
C      bitpix should be initialized to a default value before calling dataty
C
C
C Calling sequence:
C
C       call dataty (datatype, bitpix, status)
C
C ARGUMENTS:
C
C       datatype - input datatype string
C       bitpix    - output value based on datatype
C       status   - status of operation
C
C PRIMARY LOCAL VARIABLES:
C
C*****************************************************************************

      subroutine dataty (datatype, bitpix, status)

      character*(*) datatype

      integer bitpix, status
      character(80) context

C determine the correct bitpix based on datatype:
C bit/byte
      if ((datatype .eq. ' ') .or. (datatype .eq. '-')) then
         continue
      else if ((datatype .eq. '8') .or. (datatype .eq. 'B')) then
         bitpix = 8
      else if ((datatype .eq. '16') .or. (datatype .eq. 'I') .or.
     &        (datatype .eq. 'SHORT')) then
         bitpix = 16
      else if ((datatype .eq. '32') .or. (datatype(1:3) .eq. 'INT')
     &        .or. (datatype .eq. 'J') .or. (datatype .eq. 'LONG')) then
         bitpix = 32
      else if ((datatype .eq. '-32') .or. (datatype .eq. 'R') .or.
     &        (datatype .eq. 'E') .or. (datatype .eq. 'F') .or.
     &        (datatype .eq. 'REAL') .or. (datatype .eq. 'FLOAT')) then
         bitpix = -32
      else if ((datatype .eq. '-64') .or. (datatype .eq. 'D') .or.
     &        (datatype .eq. 'DOUBLE')) then
         bitpix = -64
      else
         context = ' Unknown datatype: ' // datatype
         call fcerr (context)
         status = 1
      endif

      return
      end
