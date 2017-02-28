      subroutine xrstrext(in_fil,fname,iext,ierr)
c STRip the EXTension number from the filename
c Two cases are searched: filename[ext#] and filename+ext#.
c The filename can contain a system-dependent path.
c I/O in_fil  (c)  input filename, with or without an extension substring,
c  O  fname   (c)  input filename, returned without an extension substring.
c  O  iext    (i)  extension number stripped off
c I/O ierr   (i)  error status
c
c Authors:  la, el  Goddard Space Flight Center   June, 1994

      CHARACTER*(*) in_fil,fname
      character(16) cbuf
      INTEGER*4 ierr,in1,in2,last,iext,lenact
      integer*4 idigits, left, right ,i 

      if(ierr.gt.0) return

c  Initialize to avoid warning
      right = 0
c  --
c
c Default extension number.

      iext = -99
c
c Parse the filename string.
c
      CALL rmvblk(in_fil)
      CALL dirpos(in_fil,in1,in2)
c
c maybe this create the bus error if the dirpos is not specified 
c than in2 should be increase by 1, this should be done any how
      in2=in2+1      
      last=lenact(in_fil)
c
c save last and search for ] or +
      left=last
      IF(in_fil(last:last).eq.']'. or. in_fil(last:last).eq.'+')
     &            left=last-1

c
c Look for a '+' and '[' in the filename. Start backward 
c to account the filename that included more than one +
c  
      DO i=last-1,in2,-1  
         right=i
         if(in_fil(i:i).eq. '+') GOTO 900
         if(in_fil(i:i).eq. '[') GOTO 900
      ENDDO
c
c Put in cbuf the string containing the digits for the extension
c put in idigits the number of digits

900   CONTINUE
      cbuf = in_fil(right+1:left)
      idigits=left-right    
         if ( idigits .eq. 4 ) then
            read(cbuf,1004,err=999) iext
         else if ( idigits .eq. 3 ) then
            read(cbuf,1003,err=999) iext
         else if ( idigits .eq. 2 ) then
            read(cbuf,1002,err=999) iext
         else if ( idigits .eq. 1 ) then
            read(cbuf,1001,err=999) iext
         end if
         fname = in_fil(1:right - 1)
c
c go back to the in_fil if neither the [#] or +mode are found            
      IF (iext.eq.-99) fname = in_fil

      RETURN
c
 1001 format( BN,i1 )
 1002 format( BN,i2 )
 1003 format( BN,i3 )
 1004 format( BN,i4 )

999   CONTINUE
c
c if an internal writing error is found come back with the original filename
c and extension reset to -99. This is beacuse the interal write error 
c produces a iext=0  
      fname = in_fil 
      iext=-99
c
      RETURN
      END
