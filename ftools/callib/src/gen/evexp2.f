*+EVEXP2
	subroutine evexp2(chatter, unit,row,cbdcol,expr,result,status)
	
	implicit none
	integer unit, row, cbdcol, status, chatter
	character*(*) expr
	logical result
	
C
C Description: 
C  Evaluates the boolean expression contained in EXPR by using the values 
C found in the CAL_CBD column for the row specified by the row argument.  
C If the expression evaluates as true then result is returned as true, 
C otherwise it is returned as false.
C
C Arguments:   
C chatter	(i): chatter flag
C unit   	(i): the logical unit number of the CIF
C row    	(i): the row within the file used for evaluation
C cbdcol 	(i): the column number of the CAL_CBD column
C expr   	(i): the boolean expression
C                          NOTE: This version only evaluates
C                                expressions of the form
C                                <boundary>.eq.<value>.and. ...
C result 	(i): whether or not the boolean expr is true
C status 	(i): the success status for this subroutine
C
C Origin:      Written for the Calibration Database
C
C Authors/Modification History:
C  Ron Zellar (1.0.0:94 Jun 27) Original Version
C  Ron Zellar (1.1.0:94 Jul 21) Changed name to evexp
c  Ian George (2.0.0:96 Feb 01) Added CHATTER & removed QUIET as passed params
C                               plus cosmetics
c  Ian George (2.1.0:96 Oct 9) initialised variable (LINUX problem)
c
c  Lorraine Breedon (2.2.0:97 Dec 8) initialised logical variable last
c
c  Mike Corcoran (2.2.1 Apr 19 2006) changed call to cbdcma to use 
c               new cbdcma version
c
c  Mike Corcoran (2.2.2 Sep 12 2007) convert all cbdvals to uppercase
c
        character(7) version
        parameter (version = '2.2.2')

C---------------------------------------------------------------------
*- 
c Internals
        character(5) subname
        parameter (subname = 'evexp2')
	integer errstat,landpt,randpt,eqpt,fcstln,bndlen,vallen
	character(40) bound,value
	character(80) cbdval(9),incbd
	logical anyf,last
	character(4) cval1,cval2
	character(160) contxt
        integer pmatch(1), iq
        logical valtest(1), unitstest(1)

c Initialize	
	landpt = 0
	result = .false.
        last = .false. 
	errstat = 0
	pmatch(1) = 0
	valtest(1) = .false.
	unitstest(1) = .false.

	call ftgcvs(unit,cbdcol,row,1,9,' ',cbdval,anyf,errstat)
	if (errstat .ne. 0) then
                call wtferr(subname, version, errstat,
     &                  'Error reading CIF (ftgcvs)')
                write(cval1,'(I4)')row
                write(cval2,'(I4)')cbdcol
                contxt='problematic row = '//cval1//', col = '//cval2
                call wtinfo(chatter,1,2,contxt)
                status = 2
                goto 999
	endif
200	continue

c       convert cbdval to uppercase
c	
c	write(*,*) 'evexp2 version 2.2.2'
c	write(*,*) 'CBDVAL is  ',cbdval	
	do 33 iq=1,9 
	   call ftupch(cbdval(iq))
c	   write(*,*) 'CBDVAL now is  ',cbdval	(iq)
 33     continue

C	Find the next '.AND.'
	randpt=index(expr(landpt+1:),'.AND.') + landpt
	if (randpt.eq.landpt) then
	     randpt=fcstln(expr)+1
	     last = .true.
	endif

	eqpt=index(expr(landpt+1:),'.EQ.')+landpt

	bound = expr(landpt+1:eqpt-1)
	bndlen= eqpt-landpt-1
	value = expr(eqpt+4:randpt-1)
	vallen= randpt-eqpt-4

	incbd = bound(:bndlen)//'('//value(:vallen)//')'

	call cbdcma(incbd,1,cbdval,9,chatter,result,pmatch,valtest,
     &   unitstest,errstat)
	if (errstat .ne. 0) then
                call wtferr(subname, version, errstat,
     &                  'Error reading CIF (cbdcma)')
                write(cval1,'(I4)')row
                write(cval2,'(I4)')cbdcol
                contxt='problematic row = '//cval1//', col = '//cval2
                call wtinfo(chatter,1,2,contxt)
                status = 2
                goto 999
	endif


	if (.not.result) return

	if (.not.last) then
	     landpt = randpt + 4
	     goto 200
	endif

c Final error checking
999     if (errstat.ne.0) then
          call wterrm(subname, version, ' Unable to continue')
        endif

	return
	end
