


C******************************************************************************
C SUBROUTINE:
C      fcb2s
C
C DESCRIPTION:
C      Gets value from bin table and converts it to a string
C
C AUTHOR/DATE:
C      Janice Tarrant  12/12/91
C
C MODIFICATION HISTORY:
C      W Pence  7/22/92  rewrote fcbtos subroutine to handle vectors
C      EAG     12/22/92  allow for handling of variable length vectors
C       EAG     2/12/93  support TDISPn formatting keywords
C       EAG     11/24/93 performance improvement attempt (CEAG)
C       EAG/BEO 3/24/94  fixed concatenation problems with tmpstr variable
C                        added support for hex and octal output
C       EAG     4/26/94  real is not precise enough to read in long int
C       Ning Gan 8/3/98  Add support for the unsigned long and unsign
C                        int.
C
C NOTES:
C
C USAGE:
C      call fcb2s(iunit,col,row,elem,form,disp,string)
C
C ARGUMENTS:
C      iunit  - unit number associated with input FITS file
C      col    - column number
C      row    - beginning row number
C      elem   - element number within the column vector
C      form   - format of column data
C       disp  - the display format (blank for default)
C      string - converted bin table value
C
C PRIMARY LOCAL VARIABLES:
C      dtype   - data type of keyword
C      repeat  - length of element vector
C      width   - width of character string field
C      status  - fitsio returned error status code
C      context - error message string
C      errstr  - data type error message string
C      anyf    - undefined data value flag
C      lvalue  - logical data values
C      svalue  - string data values
C      evalue  - real data values
C      cvalue  - complex data values
C      dvalue  - double precision data values
C      mvalue  - double precision complex data values
C
C CALLED ROUTINES:
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C      function   fcstln - determine last non-blank character
C      subroutine ftgcl  - get logical data values
C      subroutine ftbnfm - get binary table data format
C      subroutine ftgcvc - get complex data values
C      subroutine ftgcvd - get double precision data values
C      subroutine ftgcve - get real data values
C      subroutine ftgcvi - get short integer data values
C      subroutine ftgcvj - get long integer data values
C      subroutine ftgcvm - get double precision complex data values
C      subroutine ftgcvs - get string data values
C
C******************************************************************************
      subroutine fcb2s(iunit,col,row,elem,form,disp,string)

      integer     iunit, col, row, elem
      character*(*) form, string, disp
      integer     status
      logical     lvalue,anyf,nflags
      character(80)  context,errstr
      character(83)  tmpstr
      character(3)   version
      real        evalue,cvalue(2)
      double precision   dvalue,mvalue(2)
      integer*8 kvalue
      integer ios

C  initialize variables
      string = ' '
      status=0
      version = '2.10'
      errstr = 'FCB2S '//version//' error for data type: '
      nflags = .false.
      ios=0

C  get the binary table data format
C       if this is an ASCII field, calculate the string repeat count

C       This is an ASCII character
      if (index(form, 'A') .ne. 0) then
         call ftgcvs(iunit,col,row,elem,1,'INDEF',string,anyf,status)
         if (status .gt. 0)string=' '

C       This is an integer (*4) or unsigned long (*4).
      else if (index(form, 'J') .ne. 0) then
         call ftgcfd(iunit,col,row,elem,1,dvalue,nflags,anyf,status)
         if (status .gt. 0)then
            string=' '
         else
            if ( nflags ) then
               string = 'INDEF'
            else if (disp .ne. ' ') then
               if ((index(disp, 'I') .ne. 0) .or.
     &              (index(disp, 'Z') .ne. 0) .or.
     &              (index(disp, 'O') .ne. 0)) then
                  tmpstr = '('//disp//')'
                  write(string,tmpstr,iostat=ios,err=21) nint(dvalue)
               else
                  tmpstr = '('//disp//')'
                  write(string,tmpstr,iostat=ios,err=21) dvalue
               endif
            else
C take care the unsigned long.
                if( dvalue .lt. 2147483648.0) then
                    write(string,1041) nint(dvalue)
                else
                    write(string,1032)dvalue
                endif
            endif
         end if

C       This is an integer (*8) or long long integer:
      else if (index(form, 'K') .ne. 0) then
         call ftgcfk(iunit,col,row,elem,1,kvalue,nflags,anyf,status)
         if (status .gt. 0)then
            string=' '
         else
            if ( nflags ) then
               string = 'INDEF'
            else
               write(string,1043) kvalue
            endif
         end if

C       This is a real (*4)
      else if (index(form, 'E') .ne. 0) then
         call ftgcfe(iunit,col,row,elem,1,evalue,nflags,anyf,status)
         if (status .gt. 0)then
            string=' '
         else
            if (nflags) then
               string = 'INDEF'
            else if (disp .ne. ' ') then
               tmpstr = '('//disp//')'
               write(string,tmpstr,iostat=ios,err=42) evalue
            else
c Ziqin Pan , June 20, 2005
c move the following code to the end.
            write(string,1042) evalue
            endif
         endif

C       This is a double (*8)
      else if (index(form, 'D') .ne. 0) then
CEAG        else if ( dtype .eq. 82 ) then
         call ftgcfd(iunit,col,row,elem,1,dvalue,nflags,anyf,status)
         if (status .gt. 0)then
            string=' '
         else
            if (nflags ) then
               string = 'INDEF'
CEAG              else if (fcstln(disp) .gt. 0) then
            else if (disp .ne. ' ') then
               tmpstr = '('//disp//')'
               write(string,tmpstr,iostat=ios,err=82) dvalue
            else
c Ziqin Pan , June 20, 2005
c move the following code to the end.
            write(string,1082)dvalue
            endif
         endif

C       This is for bit and byte format and a short integer (*2)
      else if ((index(form, 'X') .ne. 0) .or.
     &        (index(form, 'B') .ne. 0) .or.
     &        (index(form, 'I') .ne. 0)) then
CEAG        else if ((dtype .eq. 1) .or. (dtype .eq. 11) .or.
CEAG     &           (dtype .eq. 21)) then
         call ftgcfe(iunit,col,row,elem,1,evalue,nflags,anyf,status)
         if (status .gt. 0)then
            string=' '
         else
            if (nflags) then
               string = 'INDEF'
CEAG              else if (fcstln(disp) .gt. 0) then
            else if (disp .ne. ' ') then
               if ((index(disp,'I') .ne. 0) .or.
     &              (index(disp, 'Z') .ne. 0) .or.
     &              (index(disp, 'O') .ne. 0)) then
                  tmpstr = '('//disp//')'
                  write(string,tmpstr,iostat=ios,err=21) nint(evalue)
               else
                  tmpstr = '('//disp//')'
                  write(string,tmpstr,iostat=ios,err=21) evalue
               endif
            else
c Ziqin Pan , June 20, 2005
c move the following code to the end.
            write(string,1021) nint(evalue)
            endif
         end if

C       This is a logical
      else if (index(form, 'L') .ne. 0) then
CEAG        if ( dtype .eq. 14 ) then
         call ftgcfl(iunit,col,row,elem,1,lvalue,nflags,anyf,status)
         if (status .gt. 0)then
            string=' '
         else
            if (nflags)then
               string='U'
            else if (lvalue)then
               string='T'
            else
               string='F'
            end if
         end if

C       This is a complex (*8)
      else if (index(form, 'C') .ne. 0) then
CEAG        else if ( dtype .eq. 83 ) then
         call ftgcfc(iunit,col,row,elem,1,cvalue,nflags,anyf,status)
         if (status .gt. 0)then
            string=' '
         else
            if (nflags) then
               string = 'INDEF'
            else
               write(string,1083)cvalue(1),cvalue(2)
            endif
         endif

C       This is a double complex (*16)
      else if (index(form, 'M') .ne. 0) then
CEAG        else if ( dtype .eq. 163 ) then
         call ftgcfm(iunit,col,row,elem,1,mvalue,nflags,anyf,status)
         if (status .gt. 0)then
            string=' '
         else
            if (nflags) then
               string = 'INDEF'
            else
               write(string,1163)mvalue(1),mvalue(2)
            endif
         endif

      endif

cZiqin Pan
 21         continue
            if (ios.ne.0) then
                   ios=0
                   write(string,1021) nint(evalue)
            endif
            goto 999
 42         continue
            if (ios.ne.0) then
                   ios=0
                   write(string,1042) evalue
            endif
            goto 999
 82         continue
            if (ios.ne.0) then
                   ios=0
                   write(string,1082)dvalue
            endif

 999  continue

C       suppress 'illegal starting element' error message
      if (status .eq. 308)status=0
C       suppress '0 length variable length array' error message
      if (status .eq. 318) status = 0
C       suppress 'end of variable length array' error message
      if (status .eq. 319) status = 0

      if ( status .ne. 0 ) then
CEAG          write(context,2000) errstr, dtype
         write(context,2000) errstr, form
         call fcerr(context)
         call fcerrm(status)
         status = 0
      endif

 1021 format(I6)
 1032 format(F11.0)
 1041 format(I11)
 1042 format(1PE15.7)
 1043 format(I20)
 1082 format(1PE23.15)
 1083 format('(',1PE15.7,',',1PE15.7,')')
 1163 format('(',1PE23.15,',',1PE23.15,')')
CEAG2000   format(A26,I4)
 2000 format(A33,' ',A8)

      return
      end
