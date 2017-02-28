      subroutine OPNRSF(filenm,iunit,type,header,tmplte,nhist,ierrsf)
      character*(*) filenm, type, header, tmplte
      integer   iunit, nhist, ierrsf
C---
C Subroutine to open for reading an SF format file, stripping
C out information from the ID record.
C On return, the file is positioned at the first history record
C or at the first package (if there is no history package).
C---
C filenm  I    File name to be opened
C iunit   I    Fortran unit to use
C type    I/O  If non-blank, it is checked against the current file
C              -type.  If blank, it is set to the current file type.
C header    O  The actual contents of the ID record.
C tmplte    O  Template file (Could be empty).
C nhist     O  No. of history records in initial history package.
C              -If zero, there is no history package (or records).
C ierrsf  I/O  sf error (if zero on input, then messages will
C              -be written to the * unit)
C     1 - io error in open
C     2 - io error/premature eof in read.
C     3 - File is not an SF file (possible io error in
C            reading the first record or bad match to first
C            four characters.
C     4 - File does not match the input type.
C---
C rashafer
C---
      integer   LENACT
C
      character(78) idrec
      character(12) pkgtyp
      character(1) tmplbf(72)
      integer*4 infoar(4)
      logical*4 qioerr
      integer*4 initer, ios, ios2, index
      integer*4 length, nsubs
C---
      qioerr=(ierrsf.eq.0)
      initer=ierrsf
      close(unit=iunit)
      call OPENWR(iunit,filenm,'old','u',' ',0,1,ios)
      if(ios.ne.0) then
c        if(qioerr) write(*,'(3a,i5)',iostat=ios2)
c     &      ' OPNRSF: Unable to open file ''',filenm(:LENACT(filenm)),
c     &      ''' for input, system flag:',ios
         ierrsf=1
         return
      end if
      read(iunit,iostat=ios) idrec
      if(ios.ne.0) then
         close(iunit)
         if(qioerr) write(*,*)' OPNRSF: Unable to read ID record:',ios
         ierrsf=2
         return
      end if
      if(idrec(1:4).ne.'SF01') then
         ierrsf=3
         if(qioerr) write(*,*)' OPNRSF: Not an SF File:',idrec(1:4)
         close(iunit)
         return
      end if
      if(type.eq.' ') then
         type=idrec(5:16)
      elseif(type.ne.idrec(5:16)) then
         if(qioerr) write(*,*)' OPNRSF: file type mismatch: expected '''
     &        ,type(:LENACT(type)),''' and opened ''',
     &        idrec(5:16),''''
         ierrsf=4
         close(iunit)
         return
      end if
      header=idrec
      if(idrec(25:25).ne.' ') then
         pkgtyp='SF template'
         index=0
         length=72
         ierrsf=initer
         call NXPKSF(iunit,pkgtyp,index,nsubs,infoar,tmplbf,length,
     &        .false., ierrsf)
         if(ierrsf.eq.0) then
            call BFTOCH(tmplbf,72,tmplte)
         else
            if(qioerr) write(*,*)
     &           'OPNRSF: Unable to process expected template package'
            tmplte=' '
         end if
      else
         tmplte=' '
      end if
      if(idrec(26:26).ne.' ') then
         pkgtyp='SF history'
         index=0
         length=0
         ierrsf=initer
         call NXPKSF(iunit,pkgtyp,index,nsubs,infoar,tmplbf,length,
     &        .false., ierrsf)
         if(ierrsf.eq.0) then
            nhist=nsubs
         else
            if(qioerr) write(*,*)
     &           'OPNRSF: Unable to process expected history package'
            nhist=0
         end if
      else
         nhist=0
      end if
      return
      end
