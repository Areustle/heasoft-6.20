	subroutine xchatr(string,lenn,iflag)
c	setcht		rashafer	16 March 1985
c		XSPEC subroutine to set the chattyness level
c
c	Version 2.0 (XCHATR)  XPARSE version
	character*(*)string 
c 	i/r: parse string
	integer*4 lenn		
c  i/r: Parse position
	integer*4 iflag 
c  r: The status condition (-1 is an EOF)
c
	character(14) descr(2)
	integer*4 chatvl(2)
	integer*4 nret
	data descr/'chattyness','log file chat.'/
	call xgtcht(chatvl(1),chatvl(2))
	call xgtint(string,lenn,2,descr,2,chatvl,nret,iflag,-1)
	call xchaty(chatvl(1),chatvl(2))
	return
	end
