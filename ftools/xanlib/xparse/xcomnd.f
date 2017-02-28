	subroutine xcomnd(string,iparse,ierr)
c		rashafer 30 April 1987
c		subroutine to look for special XPARSE commands
c
	character*(*) string	
c  i/r: parse string
	integer*4 iparse	
c  i/r: parse position
	integer*4 ierr		
c  r: condition flag 0 = success
				
c 	-1 = EOF during process
				
c 	Positive values:  Error during command
c
	include 'xparinc.inc'
	integer*4 ncom
	parameter (ncom=12)
	character(10) xcom(ncom)
	integer*4 icom,idelim,jerr
	integer*4 lenact
	logical*4 xgtonf
	data xcom/'chatter','debug',
     &	 'exit','fix*','help*','inquire','log','pause','quit',
     &	 'set*','suggest*','xparsehelp'/
	data icom/0/

C needed to ensure XPRSBD common block is initialized under gfortran

        CALL XPARSE(' ')

	call xgtmch(string,iparse,xcom,ncom,
     &	 'XPARSE supported commands (# commands)',icom,ierr,idelim)
	if(ierr.lt.0)return
	ierr=0
	if(icom.le.0) return
c **	** DO CASE for command index
	goto (650,500,200,100,100,300,600,400,200,100,100,100),icom
100	continue
        call xwrite(' XPARSE Command "'//xcom(icom)(:lenact(xcom(icom)))
     &	            //'" is not currently implemented.',1)
	goto 9000
c **	** EXIT or QUIT handler
200	continue
	call xwrite(' XPARSE generated exit',11)
C	CHANGED EXIT TO STOP FOR HPUX
C	call exit(0)
	stop
c **	** INQUIRE handler
300	continue
	require_inquiry=xgtonf(string,iparse,'Set inquiry mode (??)',
     &	     require_inquiry,jerr)
	goto 9000
c **	** PAUSE handler
400	continue
	call xwrite(' Program PAUSED, use CONTINUE to resume',-1)
	pause 'Program PAUSED, use CONTINUE to resume'
	goto 9000
c **	** DEBUG handler
500	continue
	call xdebug
	goto 9000
c **	** LOG handler
600	continue
	call xlogit(string,iparse,ierr)
	goto 9000
c **	** CHATTER handler
650	continue
	call xchatr(string,iparse,ierr)
	goto 9000
c **	** End CASE for command index
9000	continue
	return
	end
