	subroutine xgtcht (inter, log)
c		rashafer 22 march 1986
c	XPARSE subroutine to return the current interactive and log file
c	chattyness levels
c	inter	i4	r: the current interactive chattyness
c	log	i4	r: the current log file chattyness
	integer*4 inter,log
	include 'xparinc.inc'
	inter=trmcht
	log=logcht
	return
	end
