	subroutine setcht(string,lenn)
c	setcht		rashafer	16 March 1985
c		XSPEC subroutine to set the chattyness level
c
c	string	c*	i/r: parse string
c	lenn		i/r: parse position
	character*(*)string
	integer*4 lenn
c
	character(14) descr(2)
	integer*4 chatvl(2)
	integer*4 nret,iflag
	data descr/'chattyness','log file chat.'/
	call xgtcht(chatvl(1),chatvl(2))
	call xgtint(string,lenn,2,descr,2,chatvl,nret,iflag,-1)
	call xchaty(chatvl(1),chatvl(2))
	return
	end
