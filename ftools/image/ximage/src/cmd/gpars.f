c
c  FORTRAN routine to retrieve string parameter values
c
c  Must feed string size to C to avoid string overflows
c
      subroutine gpars(cmdid, parname, parval, status)
      implicit none
c
c  I  cmdid   (i)  Command id
c  I  parname (s)  Parameter name
c  O  parval  (s)  Parameter value
c  O  status  (i)  Error flag (0=OK)
c
      integer cmdid, status
      character*(*) parname, parval
c
c  Local variables
c
      integer maxparlen

      maxparlen = LEN(parval) + 1
      call gnpars(cmdid, parname, parval, maxparlen, status)

      return
      end
