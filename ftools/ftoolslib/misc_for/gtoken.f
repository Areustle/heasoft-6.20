
C******************************************************************************
C SUBROUTINE:
C      gtoken
C
C DESCRIPTION:
C	find the starting position and width of the NFIELD tokens in the
C	input character string
C
C AUTHOR:
C      William Pence   6/2/92
C
C MODIFICATION HISTORY:
C      Peter D Wilson  7/14/98: Add TAB as valid whitespace character
C
C NOTES:
C
C USAGE:
C	call gtoken(nfield,line,fields,begcol,twidth)
C
C ARGUMENTS:
C   Input
C	nfield - number of tokens to be found in the line
C	line   - input character string to be parsed
C   Output
C       fields - number of fields actually found
C	begcol - (array) starting position of each column
C	twidth - (array) width, in characters, of each column
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C
C*****************************************************************************
      subroutine gtoken(nfield,line,fields,begcol,twidth)

      integer nfield,fields,begcol(*),twidth(*)
      character*(*) line
      integer maxlen,i,itoken

      fields=0
      maxlen=len(line)
      itoken=1
      i=0
C       find a non-blank character indicating the start of the next token
 10   i=i+1
      if (i .gt. maxlen)then
         return
      end if

C PDW 7/14/98:  Add TAB character (ctrl-I=9) for skipped whitespace
      if ( line(i:i).ne.' ' .and. line(i:i).ne.char(9) ) then
C		found the beginning of the next token
         begcol(itoken)=i
         go to 20
      end if
      go to 10

 20   continue
C       now find the end of the token
      if (line(i:i) .eq. '''')then
C		this token is enclosed in quotes; find the closing quote
         i=i+1
         begcol(itoken)=i
 30      i=i+1
         if (line(i:i+1) .eq. '''''')then
C			skip 2 single quotes in a row
CEAG remove one of the two single quotes
            line(i:) = line(i+1:)
CEAG			line(maxlen:) = ' '
CEAG			i=i+1
CEAG			maxlen = maxlen - 1
            go to 30
         else if (i .gt. maxlen .or. line(i:i) .eq. '''')then
C			found the closing quote or end of string
            twidth(itoken)=i-begcol(itoken)
         else
            go to 30
         end if
      else
 40      i=i+1
C PDW 7/14/98:  Add TAB character (ctrl-I=9) for ending whitespace
         if (i .gt. maxlen .or. line(i:i) .eq. ' '
     &                     .or. line(i:i) .eq. char(9) ) then
C			found the end of the token
            twidth(itoken)=i-begcol(itoken)
         else
            go to 40
         end if
      end if
      fields=itoken
      itoken=itoken+1
      if (itoken .gt. nfield)return
      go to 10
      end
