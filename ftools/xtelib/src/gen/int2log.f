c     This subroutine will accept an array of I*4 integers and will
c write out an array of logicals with a TRUE for each array element with
c a 1 in it, and a FALSE for each array element with a 1 in it.

      subroutine int2log(iunit,colnum,frow,nelem,valarray,status)
      integer iunit,colnum,frow,nelem,valarray(*),status
      integer i,j
      logical laray(1024)

c     Initialize the logical array to be false

      do 10 i=1,1024
        laray(i)=.FALSE.
10    continue

c     Since FITSIO writes LARAY out as an array with LARAY(1) being the
c the MOST significant bit, which is the exact opposite of how one might
c expect. So at this point we reverse it so that we increment down from
c nelem to 1.
      
      j=1
      do 20 i=nelem,1,-1
        if(valarray(i).eq.1)laray(j)=.TRUE.
        j=j+1
20    continue

c     Actually write the bit-information to the file.
      
      call ftpclx(iunit,colnum,frow,1,nelem,laray,status)

      return
      end
