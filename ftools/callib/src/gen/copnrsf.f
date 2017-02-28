*+ COPNRSF
      subroutine COPNRSF(filenm,iunit,type,header,tmplte,nhist,ierrsf)

	IMPLICIT NONE
       	character*(*) filenm, type, header, tmplte
      	integer   iunit, nhist, ierrsf
c
c Description 
c  Subroutine to open for reading an SF format file, stripping
c  out information from the ID record.
c  On return, the file is positioned at the first history record
c  or at the first package (if there is no history package).
c
c Passed Parameters
c  FILENM  I    File name to be opened
c  IUNIT   I    Fortran unit to use
c  TYPE    I/O  If non-blank, it is checked against the current file
C               -type.  If blank, it is set to the current file type.
c  HEADER    O  The actual contents of the ID record.
c  TMPLTE    O  Template file (Could be empty).
c  NHIST     O  No. of history records in initial history package.
c               -If zero, there is no history package (or records).
c  IERRSF    O  sf error (zero = OK)
c
c Author & Modifications
c  rashafer (unknown date), XANADU original
c  Ian M George (1.0.1:1993 Apr 22), made FTOOLS/CALTOOLS compatible
c  Ian M George (1.0.2:1993 Jul 19), improved error handling
c  Ian M George (1.0.3:1993 Aug 09), removed "readonly" from OPEN 
c  Ian M George (1.0.4:1993 Oct 12), rationalize the error handling
	character(7) version
	parameter (version = '1.0.4')
c
*-
c Internals & Initialization
	character(40) errstr
      	integer fcstln
      	character(78) idrec
	character(80) message
     	character(12) pkgtyp
      	character(1) tmplbf(72)
      	integer infoar(4)
      	integer  ios, ios2, index
      	integer length, nsubs
	errstr = '** COPNRSF '//version//' ERROR:'
C---
      ierrsf = 0
      close(unit=iunit)
      OPEN(unit=iunit,file=filenm,status='old',
     &	form='UNFORMATTED', iostat=ios)
      if(ios.ne.0) then
	   call fcecho(errstr)
	   write(message,'(3a,i5)',iostat=ios2)
     &      '... Unable to open file ''',
     &      filenm(:MIN(fcstln(filenm),len(message)-55)),
     &      ''' for input, system flag:',ios2
	   call fcecho(message)
         ierrsf=1
         return
      end if
      read(iunit,iostat=ios) idrec
      if(ios.ne.0) then
         close(iunit)
	   call fcecho(errstr)
	   write(message,'(a,i12)') 
     &		'... Unable to read ID record:',ios
	   call fcecho(message)
         ierrsf=2
         return
      end if
      if(idrec(1:4).ne.'SF01') then
         ierrsf=3
	   call fcecho(errstr)
	   write(message,'(2a)') 
     &		' ... Not an SF File; type = ',idrec(1:4)
	   call fcecho(message)
	   if(idrec(1:4).eq.'SIMP') then
	    message = ' ... Sure this is not a FITS file ?!'
	    call fcecho(message)
	   endif
         close(iunit)
         return
      end if
      if(type.eq.' ') then
         type=idrec(5:16)
      elseif(type.ne.idrec(5:16)) then
	   message = errstr(:fcstln(errstr))// ' file type mismatch:'
	   call fcecho(message)
	   write(message, '(4a)') ' expected ',
     &          type(:MIN(fcstln(type),len(message)-34)),
     &		' and opened ', idrec(5:16)
	   call fcecho(message)
         ierrsf=4
         close(iunit)
         return
      end if
      header=idrec
      if(idrec(25:25).ne.' ') then
         pkgtyp='SF template'
         index=0
         length=72
         call CNXPKSF(iunit,pkgtyp,index,nsubs,infoar,tmplbf,length,
     &        .false., ierrsf)
         if(ierrsf.eq.0) then
            call CBFTOCH(tmplbf,72,tmplte)
         else
		 call fcecho(errstr)
		 message = 
     &           '... Unable to process expected template package'
	         call fcecho(message)
            tmplte=' '
         end if
      else
         tmplte=' '
      end if
      if(idrec(26:26).ne.' ') then
         pkgtyp='SF history'
         index=0
         length=0
         ierrsf=0
         call CNXPKSF(iunit,pkgtyp,index,nsubs,infoar,tmplbf,length,
     &        .false., ierrsf)
         if(ierrsf.eq.0) then
            nhist=nsubs
         else
		 call fcecho(errstr)
		 message = 
     &           '... Unable to process expected history package'
	         call fcecho(message)
            nhist=0
         end if
      else
         nhist=0
      end if

c Final check on error
	if(ierrsf.ne.0) then
	  message = errstr // 'Unable to access SF file'
	  call fcecho(message)
	endif
      return
      end
