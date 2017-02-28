	subroutine xparse(delstr)
c		XPARSE	rashafer	2 Mar 86
c	XPARSE initialization subroutine - used to modify the delimeter values
c	delstr	c*	i: the string that modifies the delimeters used
c			by the various XPARSE subroutines, a non-blank char
c			indicates that that particular char should be
c			modified
* 21 October 1991 : modify delimiters with delstr as follows (Andy Pollock)
*   'BLANK OFF' -  to turn off the BLANK delimiter
*   'BLANK ON'  -  to turn on the BLANK delimiter
*   'BLANK'  -  to toggle the BLANK delimiter on and off
*       ' ' may be used instead of 'BLANK' except for toggling
*   'TAB OFF' -  to turn off the TAB delimiter
*   'TAB ON'  -  to turn on the TAB delimiter
*   'TAB'  -  to toggle the TAB delimiter on and off
*       CHAR(9) may be used instead of 'TAB'
*   'COMMA OFF' -  to turn off the COMMA delimiter
*   'COMMA ON'  -  to turn on the COMMA delimiter
*   'COMMA'  -  to toggle the COMMA delimiter on and off
*       ',' may be used instead of 'COMMA'
*
* PARENTHESES may be treated as special characters within which
* delimiters are ignored.
*   'PARENTHESES OFF' -  do not treat parentheses as special characters
*   'PARENTHESES ON' -  treat parentheses as special characters
*   'PARENTHESES' -  to toggle checking delimiters within parentheses
*       '()' may be used instead of 'PARENTHESES'

        include 'xparinc.inc'

* Import :
	character*(*) delstr
* Local parameter :
        character(1) ILLEGAL
C        parameter (ILLEGAL=char(-1))
* Local variables :
        character(30) s
        character(15) delimiter
        character(10) switch
        integer*4 i,j
* External reference :
	integer*4 lenact

C---
C Define initial values (old XPRSBD block data):
C---
        QXCASE = .TRUE.
        QXPART = .TRUE.
        QXPFIR = .TRUE.
        XPREOF = '/*  '
        LXPEOF = 2
        BLANK = ' '
        OPNSTR = '"'
        CLSSTR = '"'
        COMMA = ','
        COMMNT = '!'
        SKPALL = '/'
        SPCBR1 = '|'
        SPCBR2 = '|'
        INDRCT = '@'
        OPNPR = '('
        CLSPR = ')'
        TESTPR = .FALSE.
        CONTIN = '-'
        TAB = '	'
        COMMAND = '#'
        OPSYS = '$'
        INQUIRY = '?'
        REQUEST_INQUIRY = '??'
        REQUIRE_INQUIRY = .FALSE.
        RETURN_INQUIRY = .FALSE.
        LGUNIT = 0

        ILLEGAL=char(255)
        if(delstr.eq.' ')return

        s=delstr
        call upc(s)
        i=1
        do while(s(1:1).eq.' ')
           s=s(2:)
        end do

        if(s.eq.'ON')then
           BLANK=' '
        else if(s.eq.'OFF')then
           BLANK=ILLEGAL
        else
           j=i+1
           do while(s(j:j).ne.' ')
              j=j+1
           end do
           delimiter=s(i:j-1)
           if(s(j:).ne.' ')then
              do while(s(j:j).eq.' ')
                 j=j+1
              end do
              switch=s(j:)
           else
              switch=' '
           endif
           if((delimiter.eq.',').or.(delimiter.eq.'COMMA'))then
              if(switch.eq.'ON')then
                 COMMA=','
              else if(switch.eq.'OFF')then
                 COMMA=ILLEGAL
              else if(switch.eq.' ')then
                 if(COMMA.eq.ILLEGAL)then
                    COMMA=','
                 else if(COMMA.eq.',')then
                    COMMA=ILLEGAL
                 endif
              endif
           else if((delimiter.eq.' ').or.(delimiter.eq.'BLANK'))then
              if(switch.eq.'ON')then
                 BLANK=' '
              else if(switch.eq.'OFF')then
                 BLANK=ILLEGAL
              else if(switch.eq.' ')then
                 if(BLANK.eq.ILLEGAL)then
                    BLANK=' '
                 else if(BLANK.eq.' ')then
                    BLANK=ILLEGAL
                 endif
              endif
           else if(delimiter.eq.'TAB')then
              if(switch.eq.'ON')then
                 TAB=char(9)
              else if(switch.eq.'OFF')then
                 TAB=ILLEGAL
              else if(switch.eq.' ')then
                 if(TAB.eq.ILLEGAL)then
                    TAB=char(9)
                 else if(TAB.eq.char(9))then
                    TAB=ILLEGAL
                 endif
              endif
           else if((delimiter.eq.'()').or.
     &             (delimiter.eq.'PARENTHESES'))then
              if(switch.eq.'ON')then
                 TESTPR=.TRUE.
              else if(switch.eq.'OFF')then
                 TESTPR=.FALSE.
              else if(switch.eq.' ')then
                 TESTPR=(.NOT.TESTPR)
              endif
           endif
        endif

	return

	end
