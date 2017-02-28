

C******************************************************************************
C SUBROUTINE:
C      fnmcol
C
C DESCRIPTION:
C      moves values from each column a column at a time, checking for NULLS
C
C AUTHOR/DATE:
C      James Kent Blackburn 11/05/91
C
C MODIFICATION HISTORY:
C      Tarrant  4/17/92  added vector elements
C       Greene  2/11/93  fixed ASCII column bug, add variable columns
C       Greene  4/29/93  deal with > maxsize vectors and variables
C       Greene 11/19/93  fixed problem with very long vectors
C       Pence   9/19/95  fixed problem with nulls in vectors and Bit datatype
C
C NOTES:
C
C
C USAGE:
C      call fnmcol(iunit,ounit,irow,orow,ncols,nrows,tform,incol,hdutype,status)
C
C ARGUMENTS:
C      iunit - input file unit number
C      ounit - output file unit number
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
C      subroutine ftasfm - get ascii format
C      subroutine ftbnfm - get binary format
C      subroutine ftgcfx - get x type column values
C      subroutine ftpclx - put x type column values
C      subroutine ftpclu - put undefined column values
C
C******************************************************************************
      subroutine fnmcol (iunit, ounit, irow, orow, ncols, nrows,
     &     tform, incol, hdutype, status)

      integer      maxcl, maxsize
      parameter  ( maxcl = 999 )
      parameter (maxsize = 200)

      integer     iunit,ounit,ncols,nrows,incol(*),status
      integer     dummy1,dummy2,nulrow,nulelem
      character*(*)  tform(*)
      integer     i,j,dattyp(maxcl),repeat(maxcl),width,remain,nelem,
     &     felem, irow, orow,hdutype
      logical     lvalues(maxsize),flagvals(maxsize),anyf,first
      character(1024)  svalues(maxsize)
      character(1) bvalues(maxsize)
      character(8)  keyword
      character(80) snull
      integer*2     ivalues(maxsize)
      integer     jvalues(maxsize), offset, ifrow, ofrow, maxrows
      real        evalues(maxsize),cvalues(2,maxsize)
      double precision  dvalues(maxsize),mvalues(2,maxsize)

      equivalence (mvalues,dvalues)
      equivalence (cvalues,evalues)
      equivalence (jvalues,ivalues)

      common /snldef/ snull
      data first /.true./

      maxrows = 0

      if (first) then
         do 5 i = 1, ncols
            if ( hdutype .eq. 1 ) then
                  call ftasfm(tform(i),dattyp(i),dummy1,dummy2,status)
                  width = 1
                  repeat(i) = 1
            else if ( hdutype .eq. 2 ) then
                  call ftbnfm(tform(i),dattyp(i),repeat(i),width,status)
                  if (abs(dattyp(i)) .eq. 16) then
                     repeat(i) = repeat(i)/width
                  else if (abs(dattyp(i)) .eq. 1)then
C                    treat BIT type as a BYTE
                     dattyp(i)=11*dattyp(i)
                     repeat(i)=(repeat(i)+7)/8
                  end if
            endif
 5       continue
         if (snull .eq. ' ') snull = 'NaN'
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
            anyf = .false.

C --- Must use different subroutine for each data type ---

C ---       Data type is byte or bit ---
            if ( abs(dattyp(i)) .eq. 11 ) then
               call ftgcfb(iunit,incol(i),ifrow,felem,nelem,bvalues,
     &              flagvals,anyf,status)
               call ftpclb(ounit,i,ofrow,felem,nelem,bvalues,status)

C ---       Data type is logical ---
            else if ( abs(dattyp(i)) .eq. 14 ) then
               call ftgcfl(iunit,incol(i),ifrow,felem,nelem,lvalues,
     &              flagvals,anyf,status)
               call ftpcll(ounit,i,ofrow,felem,nelem,lvalues,status)

C ---       Data type is ASCII characters ---
            else if ( abs(dattyp(i)) .eq. 16 ) then
               call ftgcfs(iunit,incol(i),ifrow,felem,nelem,svalues,
     &              flagvals,anyf,status)
               call ftpcls(ounit,i,ofrow,felem,nelem,svalues,status)

C ---       Data type is short integer ---
            else if ( abs(dattyp(i)) .eq. 21 ) then
               call ftgcfi(iunit,incol(i),ifrow,felem,nelem,ivalues,
     &              flagvals,anyf,status)
               call ftpcli(ounit,i,ofrow,felem,nelem,ivalues,status)

C ---       Data type is integer ---
            else if ( abs(dattyp(i)) .eq. 41 ) then
               call ftgcfj(iunit,incol(i),ifrow,felem,nelem,jvalues,
     &              flagvals,anyf,status)
               call ftpclj(ounit,i,ofrow,felem,nelem,jvalues,status)

C ---       Data type is real ---
            else if ( abs(dattyp(i)) .eq. 42 ) then
               call ftgcfe(iunit,incol(i),ifrow,felem,nelem,evalues,
     &              flagvals,anyf,status)
               call ftpcle(ounit,i,ofrow,felem,nelem,evalues,status)

C ---       Data type is double precision ---
            else if ( abs(dattyp(i)) .eq. 82 ) then
               call ftgcfd(iunit,incol(i),ifrow,felem,nelem,dvalues,
     &              flagvals,anyf,status)
               call ftpcld(ounit,i,ofrow,felem,nelem,dvalues,status)

C ---       Data type is complex ---
            else if ( abs(dattyp(i)) .eq. 83 ) then
               call ftgcfc(iunit,incol(i),ifrow,felem,nelem,cvalues,
     &              flagvals,anyf,status)
               call ftpclc(ounit,i,ofrow,felem,nelem,cvalues,status)

C ---       Data type is double complex ---
            else if ( abs(dattyp(i)) .eq. 163 ) then
               call ftgcfm(iunit,incol(i),ifrow,felem,nelem,mvalues,
     &              flagvals,anyf,status)
               call ftpclm(ounit,i,ofrow,felem,nelem,mvalues,status)
            end if

C ---       If any undefine values present then set them ---
 19         if ( anyf ) then
               do 20 j = 1, nelem
                  if ( flagvals(j) ) then
                     nulrow=ofrow + (j-1)/repeat(i)
                     nulelem=j-(j-1)/repeat(i)*repeat(i)
                     call ftpclu(ounit,i,nulrow,nulelem,1,status)
                  end if
 20            continue
            end if
C if the Null was undefined, define to be snull
            if (status .eq. 314) then
               status = 0
               call ftkeyn ('TNULL', i, keyword, status)
               call ftpkys (ounit, keyword, snull, ' ', status)
               call ftsnul (ounit, i, snull, status)
               goto 19
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
