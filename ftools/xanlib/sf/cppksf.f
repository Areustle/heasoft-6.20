       Subroutine  cppksf(iunit,pkgtyp,index,outunit,newtype,
     &            newindex,buffer,lenbuf,ierrsf)
c
c Subroutine to copy sf packages
c lo 15/9/86
c	rashafer 21 October 1986
c

c	Arguments:
	integer*4	iunit	
c  I: Input unit number
	character*(*)	pkgtyp	
c  I: Input pkgtyp (if blank next package is
				
c 	copied)
	integer*4	index	
c  I: Input index (if 0 any index is used)
	integer*4	outunit	
c  I: Output unit number
	character*(*)	newtype	
c  I: Packagetype written (if ' ' same as the
				
c 	input package type).
				
c  R: The actual package type copied.
	integer*4	newindex
c  I: Index of written package (if -1 then same
				
c 	as the input package index).
				
c  R: The actual index used.
	character(1)	buffer(*)	
c  W: workspace
	integer*4	lenbuf	
c  I: available workspace (note that the
				
c 	space used is NOT returned).
	integer*4	ierrsf	
c  I/R: SF error flag (determined by NXPKSF,
				
c 	WPKHSF, RSUBSF, and WSUBSF)
       integer*4 nsubs,
     &          infoar(4),length,oldindex
       character(12) oldtype
	oldtype=pkgtyp
	oldindex=index
	length=lenbuf
       call nxpksf(iunit,pkgtyp,index,nsubs,infoar,
     &            buffer,length,.true.,ierrsf)
	if(ierrsf.ne.0)return
	if(newtype.eq.' ')then
	    newtype=oldtype
	    end if
	if(newindex.lt.0)then
	    newindex=oldindex
	    end if
       call wpkhsf(outunit,pkgtyp,index,nsubs,infoar,
     &           buffer,length,ierrsf)
	if(ierrsf.ne.0)return
	if(nsubs.ne.0)then
	    call cpsbsf(iunit,outunit,nsubs,buffer,lenbuf, 
     &	     nsubs.le.0, ierrsf)
	    end if
       return
       end
