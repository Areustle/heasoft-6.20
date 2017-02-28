      subroutine exshuffle(nfiles,tshuf,cfile,tlist)

C This routine will sort the array tsort according to the shuffling
c instructions in tshuf
C
C I   nfile    number of files
C I   tshuf    instructions of what has to be to done to the list
c                to be ordered
C I/O cfile    event file names
C I/O tlist    observation start and stop times
C
      implicit none

      character(160) cfile(256),tcfile(256)
      INTEGER*4 nfiles,tlist(256,2),i,ttlist(256,2),ord
      REAL tshuf(256)

      do i=1,nfiles
         tcfile(i)=cfile(i)
         ttlist(i,1)=tlist(i,1)
         ttlist(i,2)=tlist(i,2)
      enddo
      do i=1,nfiles
         ord=tshuf(i)
         cfile(i)=tcfile(ord)
         tlist(i,1)=ttlist(ord,1)
         tlist(i,2)=ttlist(ord,2)
      enddo
      end
