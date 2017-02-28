c**********************************************************************
c  This routine takes a character string and scans that string for
c a particular character. If that character is found LMATCH is returned
c as TRUE and IPOS contains the position of the matching character.
c
c  cval - character*(ival), input character string.
c  ival - integer, (length of input character string) - checked by fcstln.
c  cmatch - character(1), input character that the string is searched for.
c  lmatch - logical, output logical telling if a match was found.
c  ipos - integer, position the character was found at.  
c
c**********************************************************************
        subroutine scnstrforchar(cval,ival,cmatch,lmatch,ipos)
        implicit none
        character*(*) cval
        character(1) cmatch 
        integer ival,ipos,i,fcstln,ichk
        logical lmatch

        ichk=fcstln(cval)
c        if(ichk.ne.ival)then
c          call fcecho(' ')
c          call fcecho('In SCNSTRFORCHAR')
c          call fcecho('Ival is incorrect. Using ichk')
c        endif

        lmatch=.FALSE.
        do 10 i=1,ival
          if((cval(i:i).eq.cmatch).and.(.not.lmatch))then
            lmatch=.TRUE.
            ipos=i
          endif
10      continue
        return
        end
          
          
