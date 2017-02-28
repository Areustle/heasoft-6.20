


C******************************************************************************
C FUNCTION:
C      fcbfml
C
C DESCRIPTION:
C      Find length of binary column's data field
C
C AUTHOR/DATE:
C      Janice Tarrant  12/12/91
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      x = fcbfml(dtype,repeat,width)
C
C ARGUMENTS:
C      dtype  - data type of keyword
C      repeat - length of element vector
C      width  - width of character string field
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C******************************************************************************
      integer function fcbfml(dtype,repeat,width)

      integer dtype,repeat,width,temp

      fcbfml = 0
      temp = repeat
      repeat = 1
C  This is a bit format
      if ( dtype .eq. 1 ) then
         fcbfml = repeat * 6
C  This is a byte format
      else if ( dtype .eq. 11 ) then
         fcbfml = repeat * 6
C This is a logical
      else if ( dtype .eq. 14 ) then
         fcbfml = repeat
C This is a ASCII character
      else if ( dtype .eq. 16 ) then
         fcbfml = repeat * width
C This is a short integer (*2)
      else if ( dtype .eq. 21 ) then
         fcbfml = repeat * 6
C This is an integer (*4)
      else if ( dtype .eq. 41 ) then
         fcbfml = repeat * 11
C This is a real (*4)
      else if ( dtype .eq. 42 ) then
         fcbfml = repeat * 15
C This is a long long integer (*8)
      else if ( dtype .eq. 81 ) then
         fcbfml = repeat * 21
C This is a double (*8)
      else if ( dtype .eq. 82 ) then
         fcbfml = repeat * 23
C This is a complex (*8)
      else if ( dtype .eq. 83 ) then
         fcbfml = repeat * 33
C This is a double complex (*16)
      else if ( dtype .eq. 163 ) then
         fcbfml = repeat * 49
      endif
      repeat = temp

      return
      end
