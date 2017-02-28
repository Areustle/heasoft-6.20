	SUBROUTINE OPNWSF(FILENM, IUNIT, TYPE, HEADER, TMPLTE, NHIST,
     &	  IERRSF, SPARE, OLDFIL, OLDID, PGNAME)
	CHARACTER*(*) FILENM, TYPE, HEADER, TMPLTE
	CHARACTER*(*) SPARE, OLDFIL, OLDID, PGNAME
	INTEGER    IUNIT, NHIST, IERRSF
C---
C Subroutine to open for writing an SF format file, creating
C a new file and updating the information on the ID record.
C An optional additional history record will be created, if
C the information of a previous version of the file is passed.
C On return, the file is positioned after the
C optional history record, or after any header of a history
C package or template package.
C---
C FILENM    I    File name to be opened
C IUNIT     I    Fortran unit to use
C TYPE      I    The type of the file
C HEADER    I    An optional string to append to the ID record
C TMPLTE    I    The template file, if non blank, will produce
C 		a template package.
C NHIST     I    No. of history records in ADDITION to any records
C 		created by OPNWSF.  IF < 0, then the history package
C 		is marked as an indeterminate no. of subsequent record
C 		package.  It is up to the user to cleanly terminate
C 		the package with a zero length record, to avoid
C 		backspaces.  If zero, a history package is still
C 		created if OPNWSF is to create a history record.
C IERRSF    I/O  SF error flag
C SPARE     I    Spare slot for expansion
C OLDFIL    I    The name of a previous version of the file.  If
C 		non-blank then a new history record is generated after
C 		the ID record.
C OLDID     I    The ID record of the previous version.
C PGNAME    I    The name of the program calling OPNWSF
C---
C 1985-Mar-08 - rashafer
C 1998-Jul-17 - James Peachey, HEASARC/GSFC/NASA, Raytheon STX
C               Y2K compliance changes: sdate was changed from
C               character(9) to character(11). It is possible that
C               the "header" may lose two characters as a result.
C---
	character(78) idrec
	character(72) tmptmp
	character(11) sdate
	character(8)  stime
	character(1) tmplbf(72)
	equivalence (tmptmp,tmplbf)
	integer*4 infoar(4)
	integer*4 lenact, ierrin, ios
	integer*4 ltp, lof, jhist, loid
	logical*4 qnewhs,qnewtp,qwerr
	data infoar/4*0/
C---
	qwerr=ierrsf.eq.0
	ierrin=ierrsf
	call openwr(iunit,filenm,'new','u',' ',0,0,ios)
	if(ios.ne.0) then
		ierrsf=13
		if(qwerr) then
		    write(*,*)'OPNWSF: Unable to open file:',filenm(:lenact(
     &		     filenm))
		    write(*,*)'   IOSTAT error = ',ios
		end if
		return
	end if
	idrec='SF01'
	idrec(5:16)=type
	idrec(17:24)=spare
	ltp=lenact(tmplte)
	qnewtp=ltp.gt.0
	if(qnewtp) then
		idrec(25:25)='*'
	end if
	lof=lenact(oldfil)
	qnewhs=lof.gt.0
	if(qnewhs.and.(nhist.ge.0)) then
		jhist=nhist+1
	else
		jhist=nhist
	end if
	if(jhist.ne.0) then
		idrec(26:26)=':'
	end if
	call getdat(sdate)
	call gettim(stime)
	idrec(27:)=' by '//pgname(:lenact(pgname))//' at '//
     :    stime//' '//sdate//' '//header
	write(iunit,iostat=ios)idrec
	if(ios.ne.0) then
		ierrsf=14
		if(qwerr)write(*,*)'OPNWSF:I/O error durring write of ID rec'
		goto 900
	end if
	ierrsf=0
	if(qnewtp) then
		ltp=min(ltp,72)
		tmptmp=tmplte
		ierrsf=ierrin
		call wpkhsf(iunit,'SF template',0,0,infoar,tmplbf,ltp,ierrsf)
		if(ierrsf.ne.0) then
			goto900
		end if
	end if
	if(jhist.ne.0) then
		ierrsf=ierrin
		call wpkhsf(iunit,'SF history',0,jhist,infoar,tmplbf,0,ierrsf)
		if(ierrsf.ne.0) then
			goto 900
		end if
		if(qnewhs) then
			idrec='From '//oldfil(:max(1,lof))
			loid=lenact(oldid)
			if(loid.ge.27) then
				idrec(lenact(idrec)+2:)=oldid(27:loid)
			end if
			write(iunit,iostat=ios)lenact(idrec),idrec
			if(ios.ne.0) then
				ierrsf=14
				if(qwerr)write(*,*)'OPNWSF:Write error',ios
				goto 900
			end if
		end if
	end if
900	continue
	if(ierrsf.ne.0) then
		close(iunit)
	end if
	return
	end
