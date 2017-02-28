
*+
      subroutine xfvv(iunit,col,row,nelem,tdim,ndims,vector,
     $ status)
C     eXtract Fastest Varying Vector
      implicit none
      integer col,row,status
      integer iunit,ndims,nelem(ndims),tdim(ndims)
      double precision vector(*)
C     Description
C     extract a tdim(1) length vector along the fastest varying index
C     (i.e. the first) in an n-dimensional array, reading from an array
C     element in a FITS binary table. If nelem(1)!=1 then the output vector
C     will start at the nelem(1)th position and be tdim(1)-nelem(1)+1
C     elements long
C Example
C     Suppose you have a 3-d array (729x20x5) in the fifth column of the
C     first row of a BINTABLE extension in a fits file. You want a vector
C     corresponding to the elements (1-729,3,7) in that array.
C     Make sure the file is positioned for the correct extension.
C
C     call xfvv(iunit,5,1,(1,3,7),(729,20,5),3,outvector,status)
C
C Passed Parameters
C  IUNIT            i   : The Unit number of the input file
C  COL              i   : The column number where the array is
C  ROW              i   : The row number where the array is
C  NELEM            i   : Array of starting indices for desired output vector
C  TDIM             i   : Array of dimensions of the array
C  NDIMS            i   : Number of dimensions of the array
C  VECTOR             o : The output vector
C  STATUS             o : Error flag
C
C Author/Modification History
C  Lawrence E Brown (1.0.0:94 Aug) Original

      character(7) version
      parameter (version='1.0.0')
*-

C

C     LOCAL variables
      integer i,felem
      double precision nullval
      character(80) context
      logical anyf
      if(status.ne.0) return

      anyf=.false.
      nullval=0
      felem=nelem(1)
      do i=2,ndims
         felem=felem+tdim(i-1)*(nelem(i)-1)
      enddo
      call ftgcvd(iunit,col,row,felem,tdim(1)-nelem(1)+1,nullval,
     $ vector,anyf,status)
      if(status.ne.0)then
         context='** XFVV ERROR'//version//
     $        'Unable to get vector from BINTABLE element array'
         call fcerr(context)
         call fcerrm(status)
      endif
      return
      end
