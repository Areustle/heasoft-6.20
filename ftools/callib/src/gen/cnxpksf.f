*+ CNXPKSF
      subroutine CNXPKSF(iunit,pkgtyp,index,nsubs,infoar,buffer
     &   ,lenbuf,qskip,ierrsf)

	IMPLICIT NONE 
      	integer   iunit, index, nsubs, lenbuf, ierrsf
      	integer   infoar(4)
      	character(1) buffer(*)
      	logical qskip
      	character*(*) pkgtyp
c Description 
c  SF subroutine to go to a given package, and decode the package header.
c  The bulk of this code is a direct copy of the XANADU routine NXPKSF, with
c minor modifications required for FTOOLS/CALTOOLS purposes
c
c Passed Parameters
c  IUNIT   I    Io unit
c  PKGTYP  I/O  Pkg type to be searched for.  If blank, then
c              -the routine will return at the next package header,
c              -with pkgtyp = to the its type
c  INDEX   I/O  Pkg index searched for.   This is ignored if
c              -pkgtyp is blank.  If it is zero on input, then any
c              -package is allowed, and its value on return is the
c              -value particular to the given package.
c  NSUBS     O  No. of subsequent records
c  INFOAR    O  An array of information taken from the package
c              -ID. (some of these values may not be modified)
c  BUFFER    O  Work array to hold the header buffer.
c  LEN     I/O  On input the maximum size of the buffer, on
c              -output the actual size returned in the buffer.
c  QSKIP   I    If true, then entire packages are skipped over until
c              -one with the given properties is found, or an eof
c              -occurs.
c  IERRSF  I/O  SF error flag (see COPNRSF)
c   6 - read error
c   7 - EOF before next package (n.b. this is a 'silent'
c         error... it will create no message no matter what the
c         initial value of ierrsf
c   8 - wrong type package (qskip = false)
c   9 - wrong index package (qskip = false)
c  10 - wrong type package, backspace error
c  11 - wrong index package, backspace error
c
c  N.B. Errors 8 and 9 will generate an automatic backspace in
c   the file (if possible) so that the next read will re-read the
c   package header that caused the question.  It must be explicitly
c   skipped over if not desired to read it again in the next call
c   to CNXPKSF.
c
c Author & Modification History
c  rashafer (85-Mar-08), original XANLIB version
c  Ian M George (1.0.0: 1993 Apr 22), made compatible with FTOOLS/CALTOOLS
c  Ian M George (1.0.1: 1993 Jul 19), better error handling
c
	character(7) version
	parameter (version = '1.0.1')
*-
c In the following the integer function LENACT has been 
c replaced by the fitsio equivalent
      integer fcstln
c
      	character(12) intype
      	logical qwerr
      	integer pkghed(7)
      	character(1) pkgbuf(28)
      	equivalence(pkghed(1),pkgbuf(1))
      	integer lenin
      	integer ios, ilen, i, j, lent
	character(80) message
C---
      qwerr=ierrsf.eq.0
      lenin=lenbuf
  100 continue
      ierrsf=0
      read(iunit,iostat=ios) ilen,(pkgbuf(i),i=1,min(28,-ilen)),
     :   (buffer(j),j=1,min(lenin,-ilen-28))
      intype(1:1)=pkgbuf(1)
      if(ios.ne.0) then
         if(ios.gt.0) then
            if(qwerr) then 
		write(message,'(3a,i12)')  'CNXPKSF ',version, 
     &			' ERROR: Read i/o status = ', ios
		call fcecho(message)
	    endif
            ierrsf=6
         else
            ierrsf=7
         end if
         return
      end if
      if(ilen.ge.0) then
C** a subsidiary record was processed
         goto 100
      end if
      nsubs=pkghed(5)
      lent=-ilen-28
      if(lent.gt.lenin) then
         ierrsf=12
      end if
      infoar(1)=pkghed(6)
      infoar(2)=pkghed(7)
      if(pkgtyp.eq.' ') then
         pkgtyp=intype
         index=pkghed(4)
      elseif(pkgtyp.ne.intype) then
         if(qskip) goto 100
         ierrsf=8
         if(qwerr) then 
	    message = 'CNXPKSF '// version // 
     &			' ERROR: Wrong type package'
	    call fcecho(message)
	    message = ' ... expected ' // pkgtyp(:fcstln(pkgtyp))
	    call fcecho(message)
	    message = ' ... but read ' // intype(:fcstln(intype))
	    call fcecho(message)
	 endif
         backspace(iunit,iostat=ios)
         if(ios.ne.0) then
            ierrsf=10
	    if(qwerr) then
		write(message,'(3a,i12)')  'CNXPKSF ',version, 
     &			' ERROR: Backspace i/o status = ', ios
	      call fcecho(message)
	    endif
         end if
         return
      elseif((index.ne.0).and.(pkghed(4).ne.index)) then
         if(qskip) goto 100
         ierrsf=9
         if(qwerr) then 
	    message = 'CNXPKSF '// version // 
     &			' ERROR: Wrong package Index:'
	    call fcecho(message)
	    write(message,'(a,i12)')  '... expected ', index
	    call fcecho(message)
	    write(message,'(a,i12)')  '... expected ', pkghed(4)
	    call fcecho(message)
	 endif
         index=pkghed(4)
         backspace(iunit,iostat=ios)
         if(ios.ne.0) then
            ierrsf=11
	    if(qwerr) then
		write(message,'(3a,i12)')  'CNXPKSF ',version, 
     &			' ERROR: Backspace i/o status = ', ios
	      call fcecho(message)
	    endif
         end if
         return
      end if
      if(ierrsf.eq.12.and.qwerr) then
	    message = 'CNXPKSF '// version // 
     &		' ERROR: Package ' // pkgtyp(:fcstln(pkgtyp))
	    call fcecho(message)
	    write(message,'(a,i12,a,i12)') 
     &	    ' ... buffer length ', lenin,
     &        ' too small, actual length:', lent
	    call fcecho(message)
            lenbuf=lent
      elseif(lent.lt.lenbuf) then
         lenbuf=lent
      end if
      return
      end
