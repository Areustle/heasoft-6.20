	subroutine xcmsav(string,iparse,savstrg,maxcom,nccom)
c	fwj haberl   3-FEB-1989 21:16:00 
c	xparse routine to save away command strings for recalling
c	arguments
c	string	    c*		i:   command string to save
c	iparse	    i4		i:   current parse position
c	savstrg	    c*(maxcom)	i/r: character array for command storage
c	maxcom 	    i4		i:   maximum no of saved commands
c	nccom	    i4		i/r: current no of saved commands

	integer*4 iparse,nccom,maxcom
	character*(*) string,savstrg(maxcom)
	integer*4 icom

c	if character array to save commands is full, overwrite 1st command
	if(nccom.eq.maxcom)then
	    nccom=maxcom-1
	    do icom=1,nccom
		savstrg(icom)=savstrg(icom+1)
	    enddo
	endif
	nccom=nccom+1
	savstrg(nccom)=string
	return
	end
