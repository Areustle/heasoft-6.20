
C******************************************************************************
C FUNCTION:
C      fcafml
C
C DESCRIPTION:
C      Find length of ASCII column's data field
C
C AUTHOR/DATE:
C      Janice Tarrant  12/12/91
C
C MODIFICATION HISTORY:
C       EAG  1/22/93 - Added BN to format statement for VAX
C       EAG 9/22/93 - Allow for characters before E (fortran format style)
C
C NOTES:
C
C USAGE:
C      x = fcafml(form)
C
C ARGUMENTS:
C      form - column format string
C
C PRIMARY LOCAL VARIABLES:
C      lform - length of column format string
C
C CALLED ROUTINES:
C      function   fcstln - returns length of character string (integer)
C
C******************************************************************************
      integer function fcafml(form)

      character*(*) form
      integer       i,lform,fcstln, ipos, apos, fpos, epos, dpos, pos

      fcafml = 0
      ipos = index(form, 'I')
      apos = index(form, 'A')
      fpos = index(form, 'F')
      epos = index(form, 'E')
      dpos = index(form, 'D')

      pos = max(ipos, apos)
      if (pos .gt. 0) then
         lform = fcstln(form)
         read(form(pos+1:lform),1000) i
         fcafml = i
      endif

      pos = max(fpos, epos)
      pos = max(pos, dpos)
      if (pos .gt. 0) then
         lform = index(form,'.') - 1
         read(form(pos+1:lform),1000) i
         fcafml = i
      endif

 1000 format( BN,I3 )
      return
      end
