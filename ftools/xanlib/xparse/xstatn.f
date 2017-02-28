	logical*4 function xstatn (trap_mult_in, use_sub_in, 
     &	 sub, lenbuf_in, buffer,
     &	 unwind_in, iflag)
c	     rashafer 9 Feb 1986
c	Subroutine to set up an Attention handler --- e.g. trap for Ctrl-Cs.
c		THIS IS HIGHLY VMS Specific
c **	** XSTATN - returned value: false only if iflag .ne. 0 on return
	logical*4 trap_mult_in	
c  I:  If true, then allow for multiple traps
				
c 	e.g. re-que the trap to catch the next
				
c 	one.
	logical*4 use_sub_in	
c  I:  If true, then the Attn will generate
c 	a call to a user defined subroutine.
c 	NOT CURRENTLY IMPLEMENTED, MUST BE
c 	.FALSE.
c!!	external sub
	logical sub
c  I:  The subroutine to be called.
	integer*4 lenbuf_in	
c  I:  The buffer size for the subroutine.
c$$$	Structure/Attnbuf/
c$$$	    Integer*4	Nattn
c$$$	    Integer*4   Curlen
c$$$	    Byte        Buf(1)
c$$$	End Structure
c$$$	Record/Attnbuf/Buffer
				
c  SPECIAL:  This  array acts as a storage area
c 	for a result returned by SUB.  Must
c 	NOT be used for any other purpose.  The
c 	first four bytes will contain the
c 	current value of the interrupt counter
c 	while the next four bytes contain
c 	the current no. of bytes returned
c 	by SUB (if not zero).  Buffer should
c 	therefore always have at least 8 bytes
c 	+LENBUF size.  It also should be in the
c 	root segment if this is overlaid
c 	(remember overlays?).
	integer*4 unwind_in	
c  I:  Unwind flag.  CURRENTLY NOT IMPLEMENTED.
c  	SHOULD ALWAYS BE ZERO.
	integer*4 iflag		
c  R:  Success flag.  If 0, then all OK
c
c$$$	structure /iostat_block/
c$$$	  integer*2 iostat
c$$$	  byte transmit, receive, crfill, lffill, parity, zero
c$$$	end structure
c$$$	external xasthn
c$$$	integer*4 nattn
c$$$	data nattn/0/
c
c!!	include '($IODEF)'  
c  I/O op symbols
c
c	XASTFT declarations   
c   Called only by XSATHN
	logical*4 xastft
c$$$	record/attnbuf/ astarg_in 
c  i/r:  just letting people know its been set.
c
c	XQATTN declarations
	logical*4 xqattn	
c  FUNCTION - TRUE there has been an ATTN
c 	False, no new ATTN since last call.
	integer*4 nattn_xq	
c  r:  The number of ATTNs since the
c 	last call to XSTATN or XQATTN
	integer*4 lenbuf_xq	
c  i:  The allowed size of the buffer
	character(1) buf_xq(*)		
c  r:  The returned contents of the buffer from
c 	the last ATTN call to SUB
	integer*4 nret		
c  r:  The total size returned.

c!!	iflag = 0
c!!	nattn = 0
c!!	lenbuf = lenbuf_in
c!!	trap_mult = trap_mult_in
c!!	use_sub = use_sub_in
c!!	unwind = unwind_in
c **	** Assign channels
c!!	status = sys$assign ('SYS$INPUT', input_chan,,)
c!!	if (.not. status) call lib$signal (%val(status))
c!!	code = io$_setmode .or. IO$M_CTRLCAST 
		
c  Set the code for catching ctrl-C
c	Que the AST
c!!	status = sys$qiow(, %val(input_chan), %val(code), iosb,,,
c!!     &	  xasthn, buffer,,,,)
c!!	if(.not. status) call lib$signal (%val(status))
c!!	if (.not. iosb.iostat) call lib$signal(%val(iosb.iostat))
	return
c
	entry xastft  (astarg_in)
c!!	nattn = nattn+1
c!!	call getlun(terminal)
c!!	if(jflag.eq.0)then
c!!	    call openwr(terminal,'SYS$ERROR','UNKNOWN',' ',' ',0,0,ierr)
c!!	else
c!!	    terminal=0
c!!	end if
c!!	if(terminal.ne.0)then
c!!	    write(terminal,*)' OUCCHHH
c !! (Ctrl-C Intercepted)'
c!!	    close(terminal)
c!!	    call frelun(terminal)
c!!	end if
c!!	if(trap_mult)then
c **	    ** reset the AST
c!!	    status = sys$qiow(, %val(input_chan), %val(code), iosb,,,
c!!     &	  xasthn, astarg_in,,,,)
c!!	    if(.not. status) call lib$signal(%val(status))
c!!	    if(.not. iosb.iostat) call lib$signal(%val(iosb.iostat))
c!!	end if
	return
c
	entry xqattn( nattn_xq, lenbuf_xq, buf_xq, nret)
c!!	xqattn = nattn.ne.0
c!!	nattn_xq=nattn
c!!	nattn=0
	return
	end

