
C ***************************************************************************
C FUNCTION:
C      comnum
C
C DESCRIPTION:      
C      converts a real*8 value to a string, and concatenates it to an input string
C      
C AUTHOR:
C      James Lochner  4/29/94
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USEAGE:
C	s = comnun comnun(instr,inval)
C      
C ARGUMENTS:
C      instr - fixed input string
C      inval - input value to be conversted to string
C
C PRIMARY LOCAL VARIABLES:
C      inlen  - length of input string
C      cval   - character string version of input value
c      vlen   - length of the character string version of input value
C      ctemp2 - copy of cval, with leading blanks removed
C      ctemp  - left justified character string for input value
C      
C CALLED ROUTINES:
C   subroutine ftd2e - convert at real*8 value to a E format character string
C ***************************************************************************

      function comnum(instr,inval)

      character*(*) instr
      character(80) comnum
      double precision inval

      integer ftstatus
      character(35) cval, ctemp, ctemp2
      integer i, j, jlen, vlen, inlen

      ftstatus=0
      inlen = len(instr)
      call ftd2e(inval,5,cval,vlen,ftstatus)
      j = 0
      do i = 20,1,-1
         if (cval(i:i) .ne. ' ') then
            j = j + 1
            ctemp2(j:j) = cval(i:i)
         endif
      end do
      jlen = j
c      
c      cval (and ctemp2) is right justified, so turn it around
c      
      i = 1
      do j = jlen,1,-1
         ctemp(i:i) = ctemp2(j:j)
         i = i + 1
      end do
      comnum = instr//ctemp
      do i = inlen+jlen+1,80
         comnum(i:i) = ' '
      end do

      return
      end
