
C******************************************************************************
C SUBROUTINE:
C      fimcol
C
C DESCRIPTION:
C      moves values from each column a column at a time with no NULL checking
C
C AUTHOR/DATE:
C      James Kent Blackburn 11/05/91
C
C MODIFICATION HISTORY:
C      Tarrant  4/17/92  added vector elements
C       Greene  2/11/93  fixed ASCII column bug, add variable columns
C       Greene  4/29/93  deal with > maxsize vectors and variables
C       Greene  6/14/93  speed up, and allow for input start row
C       Greene 11/19/93  fixed problem with very large vectors
C
C NOTES:
C
C
C USAGE:
C       call fimcol (iunit, ounit, irow, orow, ncols, nrows,
C                    tform, incol, hdutype, status)
C
C ARGUMENTS:
C      iunit - input file unit number
C      ounit - output file unit number
C       irow - input row number
C       orow - output row number
C      ncols - number of columns
C      nrows - number of rows
C      tform - data format
C      incol - column mapping between in and out FITS files
C      hdutype - header unit type
C      status - error number
C
C PRIMARY LOCAL VARIABLES:
C      lvalues - array of logical values
C      svalues - array of string values
C      ivalues - array of integer*2 values
C      jvalues - array of integer values
C      evalues - array of real values
C      dvalues - array of double values
C      cvalues - array of complex values
C      mvalues - array of double complex values
C
C CALLED ROUTINES:
C      subroutine fcasfm - get ascii format
C      subroutine ftbnfm - get binary format
C      subroutine ftgcvx - get x type column values
C      subroutine ftpclx - put x type column values
C      subroutine ftpclu - put undefined column values
C
C******************************************************************************
      subroutine fimcol (iunit, ounit, irow, orow, ncols, nrows,
     &     tform, incol, hdutype, status)

      integer      maxcl, maxsize
      parameter  ( maxcl = 999 )
      parameter (maxsize = 1024)

      integer     iunit,ounit,ncols,nrows,incol(maxcl),status
      character*(*)  tform(maxcl)
      integer     i,dattyp(maxcl), repeat(maxcl), width, remain
      integer     nelem, felem, irow, orow, hdutype, ifrow, ofrow
      logical     lvalues(maxsize), anyf
      character(1024)  svalues(maxsize)
      integer*2     ivalues(maxsize)
      integer     jvalues(maxsize), offset, maxrows
      real        evalues(maxsize),cvalues(2,maxsize)
      double precision  dvalues(maxsize),mvalues(2,maxsize)

      logical first
      data first /.true./

      maxrows = 0

C if this is the first call, find the repeat and datatype values
      if (first) then
         do 5 i = 1, ncols
            if ( hdutype .eq. 1 ) then
               call fcasfm(tform(i),dattyp(i),status)
               width = 1
               repeat(i) = 1
            else if ( hdutype .eq. 2 ) then
               call ftbnfm(tform(i),dattyp(i),repeat(i),width,status)
            endif
            if (abs(dattyp(i)) .eq. 16) repeat(i) = repeat(i)/width
 5       continue
         first = .false.
      endif


C --- Loop through each column of new table ---

      do 30 i = 1, ncols

C skip this column if no input
         if (incol(i) .le. 0) goto 30

         felem = 1
         ifrow = irow
         ofrow = orow
         remain = nrows * repeat(i)

C variable length array
C if dattyp < 0, then we have a variable length array and must go one
C row at a file.  Find the nelem for this row.
         if (dattyp(i) .lt. 0) then
            call ftgdes (iunit, incol(i), ifrow, remain,
     &           offset, status)
            repeat(i) = remain
            maxrows = irow + nrows
         endif

 10      if ( remain .gt. 0 ) then

            if ( remain .ge. maxsize ) then
               nelem = maxsize
            else
               nelem = remain
            end if

C --- Must use different subroutine for each data type ---

C ---       Data type is bits ---
            if ( abs(dattyp(i)) .eq. 1 ) then
               call ftgcvi(iunit,incol(i),ifrow,felem,nelem,0,ivalues,
     &              anyf,status)
               call ftpcli(ounit,i,ofrow,felem,nelem,ivalues,status)

C ---       Data type is byte ---
            else if ( abs(dattyp(i)) .eq. 11 ) then
               call ftgcvi(iunit,incol(i),ifrow,felem,nelem,0,ivalues,
     &              anyf,status)
               call ftpcli(ounit,i,ofrow,felem,nelem,ivalues,status)

C ---       Data type is logical ---
            else if ( abs(dattyp(i)) .eq. 14 ) then
               call ftgcl(iunit,incol(i),ifrow,felem,nelem,lvalues,
     &              status)
               call ftpcll(ounit,i,ofrow,felem,nelem,lvalues,status)

C ---       Data type is ASCII characters ---
            else if ( abs(dattyp(i)) .eq. 16 ) then
               call ftgcvs(iunit,incol(i),ifrow,felem,nelem,' ',svalues,
     &              anyf,status)
               call ftpcls(ounit,i,ofrow,felem,nelem,svalues,status)

C ---       Data type is short integer ---
            else if ( abs(dattyp(i)) .eq. 21 ) then
               call ftgcvi(iunit,incol(i),ifrow,felem,nelem,0,ivalues,
     &              anyf,status)
               call ftpcli(ounit,i,ofrow,felem,nelem,ivalues,status)

C ---       Data type is integer ---
            else if ( abs(dattyp(i)) .eq. 41 ) then
               call ftgcvj(iunit,incol(i),ifrow,felem,nelem,0,jvalues,
     &              anyf,status)
               call ftpclj(ounit,i,ofrow,felem,nelem,jvalues,status)

C ---       Data type is real ---
            else if ( abs(dattyp(i)) .eq. 42 ) then
               call ftgcve(iunit,incol(i),ifrow,felem,nelem,0.,evalues,
     &              anyf,status)
               call ftpcle(ounit,i,ofrow,felem,nelem,evalues,status)

C ---       Data type is double precision ---
            else if ( abs(dattyp(i)) .eq. 82 ) then
               call ftgcvd(iunit,incol(i),ifrow,felem,nelem,0.D0,
     &              dvalues,anyf,status)
               call ftpcld(ounit,i,ofrow,felem,nelem,dvalues,status)

C ---       Data type is complex ---
            else if ( abs(dattyp(i)) .eq. 83 ) then
               call ftgcvc(iunit,incol(i),ifrow,felem,nelem,0.,cvalues,
     &              anyf,status)
               call ftpclc(ounit,i,ofrow,felem,nelem,cvalues,status)

C ---       Data type is double complex ---
            else if ( abs(dattyp(i)) .eq. 163 ) then
               call ftgcvm(iunit,incol(i),ifrow,felem,nelem,0.D0,
     &              mvalues,anyf,status)
               call ftpclm(ounit,i,ofrow,felem,nelem,mvalues,status)
            endif

         end if

C --- Update the pointers in the column ---

         if (dattyp(i) .gt. 0) then
            remain = remain - nelem
            felem = felem + nelem
            if (felem .gt. repeat(i)) then
               ifrow = ifrow + (felem-1)/repeat(i)
               ofrow = ofrow + (felem-1)/repeat(i)
               felem = felem - ((felem-1)/repeat(i))*repeat(i)
            endif
         else
            remain = remain - nelem
            felem = felem + nelem
            if (felem .gt. repeat(i)) then
               felem = 1
               ifrow = ifrow + 1
               ofrow = ofrow + 1
               if (ifrow .gt. maxrows) goto 200
               call ftgdes (iunit, incol(i), ifrow, remain,
     &              offset, status)
               repeat(i) = remain
            endif
         endif

         if ( remain .gt. 0 ) goto 10

 200     if (status .ne. 0 ) then
            call fcerrm(status)
            status = 0
         endif

 30   continue

      irow = irow + nrows
      orow = orow + nrows

      return
      end
