*+RCNFGL
	subroutine rcnfgl(unit,chatter,linenum,misval,insval,cifdev,
     &	cifdir,cif,insdev,insdir,status)

	implicit none
	integer unit, status,linenum, chatter
	character*(*) misval,insval,cifdev,cifdir,cif,insdev,insdir

C-----------------------------------------------------------------------
C Description: Reads the next record from the caldbconfig file and 
C              parses the tokens found on each line into the arguments
C              misval, insval, cifdev, cifdir, cif, insdev, and insdir.
C              The file should have been opened already with a logical
C              unit number given by the unit argument.  
C
C Arguments:   unit   (i): the logical unit number with which the 
C                          caldbconfig file was opened.
C              chatter(i): if true no messages will be output
C              misval (r): the value of the first token found on the
C                          next line of the caldbconfig file.
C              insval (r): the value of the second token.
C              cifdev (r): the value of the third token.
C              cifdir (r): the value of the fourth token.
C              cif    (r): the value of the fifth token.
C              insdev (r): the value of the sixth token.
C              insdir (r): the value of the seventh token.
C              status (r): the success status of this routine.  Non-zero
C                          values means this routine failed.
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C  Ron Zellar    (1.0.0: Sep 13 1994) -- Original Version
c  Ian M George  (2.0.0: 95 Dec 28) quiet->chatter as passed param
        character(7) version
        parameter (version = '2.0.0')
C-----------------------------------------------------------------------
*-
c Internals
        character(6) subname
        parameter (subname = 'rcnfgl')
	integer linelen
	parameter (linelen = 160)
	character(160) line

	character(4) cval
	character(80) contxt
	integer fcstln,errstat

C Initialize the internal and external error flags
	errstat = 0
	status = 0

c Give user info if requested
         contxt = ' using '//subname//' '//version
         call wtinfo(chatter,26,1,contxt)

100	continue

	linenum = linenum + 1

	read(unit,'(A)',iostat=errstat,end=1000)line
	if (errstat .ne. 0) then
           call wterrm(subname, version,
     &          'Error reading record of CALDBCONFIG file')
	   status = 30
	   goto 999 
	endif
	     
C Skip blank lines or lines beginning with '#' characters
	if ((line(1:1).eq.'#').or.(fcstln(line).eq.0)) goto 100

C Parse the line and assign values to variables
	call pcnfgl(line,misval,insval,cifdev,cifdir,cif,
     &	insdev,insdir,errstat)
	if(errstat.ne.0) then
           call wterrm(subname, version,
     &          'Error parsing record of CALDBCONFIG file')
	   write(cval,'(I4)')errstat
	   contxt='Offending token number is '//cval
	   call wtinfo(chatter,5,3,contxt)
	   status = 35
	   goto 999 
	endif
	         
C Convert misval and insval to uppercase
	call ftupch(misval)
	call ftupch(insval)
	goto 999	
	
     

1000	continue
	status = -1

999     if(status.gt.0) then
           call wterrm(subname, version,
     &          'Unable to continue')
        endif

	return
	end
C-----------------------------------------------------------------------

*+PCNFGL
	subroutine pcnfgl(line,misval,insval,cifdev,cifdir,cif,
     &	insdev,insdir,status)

	implicit none
	character*(*)line,misval,insval,cifdev,cifdir,cif,
     &	insdev,insdir
	integer status

C-----------------------------------------------------------------------
C Description: Parses the seven tokens contained in the argument line
C              and separated by spaces or tabs, then assigns the tokens
C              to the arguments misval, insval, cifdev, cifdir, cif,
C              insdev, and insdir.
C
C Arguments:   line     (i): the variable containing the tokens to be
C                            parsed
C              misval   (r): assigned the first token in line
C              insval   (r): assigned the second token in line
C              cifdev   (r): assigned the third token in line
C              cifdir   (r): assigned the fourth token in line
C              cif      (r): assigned the fifth token in line
C              insdev   (r): assigned the sixth token in line
C              insdir   (r): assigned the seventh token in line
C              status   (r): if non-zero, then returned value is the
C                            token number which could not be parsed.
C                            Otherwise, 0 = OK
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C              Ron Zellar, Aug 24, 1994 -- Original Version
C-----------------------------------------------------------------------
*- Version 1.0

	integer curpos,wrdend,linelen,len

C	initialize the status flag, and the current position of the 
C       line pointer
	status = 0
	curpos = 1

C 	Get the length of the line
	linelen = len(line)

C	Move to the next word in line, get the pointer for the location
C       of the end of the word, and assign the word to the misval arg
	call nxtwrd(line,curpos)
	call endwrd(line,curpos,wrdend)
	if ((curpos.eq.linelen).or.(wrdend.eq.0)) then
	     status = 1
	     return
	endif
	misval = line(curpos:wrdend)

C	Increment the current position by one so that I don't end up 
C       in a loop
	curpos = wrdend + 1

C	Assign the next token to the insval argument
	call nxtwrd(line,curpos)
	call endwrd(line,curpos,wrdend)
	if ((curpos.eq.linelen).or.(wrdend.eq.0)) then
	     status = 2
	     return
	endif
	insval = line(curpos:wrdend)

C	Assign the next token to the cifdev argument
	curpos = wrdend + 1
	call nxtwrd(line,curpos)
	call endwrd(line,curpos,wrdend)
	if ((curpos.eq.linelen).or.(wrdend.eq.0)) then
	     status = 3
	     return
	endif
	cifdev = line(curpos:wrdend)

C	Assign the next token to the cifdir argument
	curpos = wrdend + 1
	call nxtwrd(line,curpos)
	call endwrd(line,curpos,wrdend)
	if ((curpos.eq.linelen).or.(wrdend.eq.0)) then
	     status = 4
	     return
	endif
	cifdir = line(curpos:wrdend)

C	Assign the next token to the cif argument
	curpos = wrdend + 1
	call nxtwrd(line,curpos)
	call endwrd(line,curpos,wrdend)
	if ((curpos.eq.linelen).or.(wrdend.eq.0)) then
	     status = 5
	     return
	endif
	cif = line(curpos:wrdend)

C	Assign the next argument to the insdev argument
	curpos = wrdend + 1
	call nxtwrd(line,curpos)
	call endwrd(line,curpos,wrdend)
	if ((curpos.eq.linelen).or.(wrdend.eq.0)) then
	     status = 6
	     return
	endif
	insdev = line(curpos:wrdend)

C	Assign the next token to the insdir argument
	curpos = wrdend + 1
	call nxtwrd(line,curpos)
	call endwrd(line,curpos,wrdend)
	if ((curpos.eq.linelen).or.(wrdend.eq.0)) then
	     status = 7
	     return
	endif
	insdir = line(curpos:wrdend)

	return
	end


C-----------------------------------------------------------------------
*+NXTWRD
	subroutine nxtwrd(line,curpos)

	implicit none
	character*(*) line
	integer curpos
C-----------------------------------------------------------------------
C Description: moves the pointer curpos to the next word in line.
C
C Arguments:   line   (i)  : the line containing word tokens
C              curpos (i/r): the current position of the pointer
C                            gets incremented until it points at a non-
C                            whitespace character
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History
C Ron Zellar Aug 25, 1994 -- Original version
C-----------------------------------------------------------------------
*-version 1.0

	integer linelen, len, i

	linelen = len(line)

	Do 100 i=curpos,linelen
             if((line(i:i).ne.' ').and.(line(i:i).ne.'	')) then
	          curpos = i
	          goto 101
	     endif
100	continue
101	continue
	return
	end

*+ENDWRD
	subroutine endwrd(line,curpos,wrdend)

	implicit none
	character*(*) line
	integer curpos,wrdend
C-----------------------------------------------------------------------
C Description: Returns the location in line of the end of the word
C              pointed at by the curpos pointer
C
C Arguments:   line   (i)  : the line containing word tokens
C              curpos (i)  : the current position of the pointer
C              wrdend (r)  : the location of the end of the word
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History
C Ron Zellar Aug 25, 1994 -- Original version
C-----------------------------------------------------------------------
*-version 1.0

	integer sp,tab,min

	sp = index(line(curpos:),' ')
	tab = index(line(curpos:),'	')

	if ((sp .eq. 0).and.(tab.eq.0)) then
	     wrdend = 0
	     return
	endif

	if (sp .eq. 0) then
	     wrdend = tab + curpos - 2
	     return
	else if (tab .eq. 0) then
	     wrdend = sp + curpos - 2
	     return
	endif

	wrdend = min(sp,tab) + curpos - 2

	return
	end
