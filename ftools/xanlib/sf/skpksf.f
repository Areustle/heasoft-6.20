
      subroutine skpksf(iunit,pkgtyp,index,nsubs,infoar,buffer,
     &                  len,ierrsf)

c      skpksf            rashafer 8 March 85
c            SF subroutine to go to a given package, and decode the
c            package header.  Intermediate packages are skiped over, with
c            a message printed.   As the NXPKSF subroutine is used,
c            the errors are generally passed from that routine.
c      iunit      i4      i: io unit
c      pkgtyp      c*      i/r: pkg type to be searched for.  If blank, then
c                  the routine will return at the next package header,
c                  with pkgtyp = to the its type
c      index      i4      i/r: pkg index searched for.   This is ignored if
c                  pkgtyp is blank.  If it is zero on input, then any
c                  package is allowed, and its value on return is the
c                  value particular to the given package.
c      nsubs      i4      r: no. of subsequent records
c      infoar      i4(4)      r: an array of information taken from the package
c                  ID. (some of these values may not be modified)
c      buffer      b*      r: work array to hold the header buffer.
c      len      i4      i/r: on input the maximum size of the buffer, on
c                  output the actual size returned in the buffer.
c      ierrsf      i4      i/r: SF error flag (see OPNRSF)

      character*(*) pkgtyp
      character(12) pkgtmp
      integer*4 infoar(4)
      character(1) buffer(*)
      integer*4 iunit, index, nsubs, len, ierrsf, indtmp, lentmp
100   continue
      pkgtmp=' '
      indtmp=0
      lentmp=len
      call nxpksf(iunit,pkgtmp,indtmp,nsubs,infoar,buffer,lentmp,
     &            .false.,ierrsf)
      if(ierrsf.ne.0)return
      if(((pkgtyp.eq.' ').or.(pkgtyp.eq.pkgtmp)).and.((index.eq.0).or.
     &            (index.eq.indtmp)))then
         pkgtyp=pkgtmp
         index=indtmp
         len=lentmp
         return
      else
         write(*,*)' SKPKSF: Skipping over SF package ',pkgtmp,indtmp
         goto 100
      end if

      end
