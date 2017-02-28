      subroutine NXPKSF(iunit,pkgtyp,index,nsubs,infoar,buffer
     &   ,lenbuf,qskip,ierrsf)
      integer   iunit, index, nsubs, lenbuf, ierrsf
      integer   infoar(4)
      character(1) buffer(*)
      logical*4 qskip
      character*(*) pkgtyp
C---
C SF subroutine to go to a given package, and decode the package header.
C---
C iunit   I    Io unit
C pkgtyp  I/O  Pkg type to be searched for.  If blank, then
C              -the routine will return at the next package header,
C              -with pkgtyp = to the its type
C index   I/O  Pkg index searched for.   This is ignored if
C              -pkgtyp is blank.  If it is zero on input, then any
C              -package is allowed, and its value on return is the
C              -value particular to the given package.
C nsubs     O  No. of subsequent records
C infoar    O  An array of information taken from the package
C              -ID. (some of these values may not be modified)
C buffer    O  Work array to hold the header buffer.
C len     I/O  On input the maximum size of the buffer, on
C              -output the actual size returned in the buffer.
C qskip   I    If true, then entire packages are skipped over until
C              -one with the given properties is found, or an eof
C              -occurs.
C ierrsf  I/O  SF error flag (see OPNRSF)
C   6 - read error
C   7 - EOF before next package (n.b. this is a 'silent'
C         error... it will create no message no matter what the
C         initial value of ierrsf
C   8 - wrong type package (qskip = false)
C   9 - wrong index package (qskip = false)
C  10 - wrong type package, backspace error
C  11 - wrong index package, backspace error
C
C  N.B. Errors 8 and 9 will generate an automatic backspace in
C   the file (if possible) so that the next read will re-read the
C   package header that caused the question.  It must be explicitly
C   skipped over if not desired to read it again in the next call
C   to NXPKSF.
C---
C 85-Mar-08 - rashafer
C---
      integer   LENACT
C
      character(12) intype
      logical*4 qwerr
      integer*4 pkghed(7)
      character(1) pkgbuf(28)
      equivalence(pkghed(1),pkgbuf(1))
      integer*4 lenin
      integer*4 ios, ilen, i, j, lent
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
            if(qwerr) write(*,*)'NXPKSF: Read i/o error:',ios
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
         if(qwerr) write(*,*)'NXPKSF: Wrong type package:',
     &        ' expected ''',pkgtyp(:LENACT(pkgtyp)),''' and read ''',
     &        intype(:LENACT(intype)),''''
         backspace(iunit,iostat=ios)
         if(ios.ne.0) then
            ierrsf=10
            if(qwerr) write(*,*)'NXPKSF: Backspace error:',ios
         end if
         return
      elseif((index.ne.0).and.(pkghed(4).ne.index)) then
         if(qskip) goto 100
         ierrsf=9
         if(qwerr) write(*,*)'NXPKSF: Wrong package index: Expected ',
     &        index,' and read ',pkghed(4)
         index=pkghed(4)
         backspace(iunit,iostat=ios)
         if(ios.ne.0) then
            ierrsf=11
            if(qwerr) write(*,*) 'NXPKSF: Backspace error:',ios
         end if
         return
      end if
      if(ierrsf.eq.12.and.qwerr) then
         write(*,'(3a,i5,/,a,i5)') ' NXPKSF: Package ''',
     &        pkgtyp(:LENACT(pkgtyp)),
     &        ''', buffer length ',lenin,
     &        ' too small, actual length:',lent
         lenbuf=lent
      elseif(lent.lt.lenbuf) then
         lenbuf=lent
      end if
      return
      end
