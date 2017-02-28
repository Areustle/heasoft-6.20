C******************************************************************************
C FTOOLS TASK:
C      fkeypar
C
C FILE:
C      fkeypar.f 
C
C DESCRIPTION: 
C      Copy a FITS header keyword value to an IRAF parameter   
C
C AUTHOR:  
C      William Pence  6/30/92
C
C MODIFICATION HISTORY:
C	10/9/92 (EAG) - added exist keyword - whether the keyword was found
C	12/7/93 (EAG) - keywords longer than 8 characters disallowed
C       8/23/94 (EAG) 3.0a - clear FITSIO error message stack
C       5/21/98 NG 3.0b- Replace ftmrhd(...,extnum,...) with ftmahd(..,extnum+1,..) 
C      02/01/1999 toliver 3.1 - keyword max length increased to 70 characters
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C      uclgst - parameter interface subroutine to read a text parameter
C      uclpst - parameter interface subroutine to write a text parameter
C      fitsio library - calls to subroutines beginning with 'ft....'
C
C******************************************************************************
      subroutine fkeypr

      integer funit, fcstln
      parameter (funit=37)
      integer status,blksiz,extnum,hdutyp,ii,jj
      character(160) fitfil,infil
      character(80) coment,context
      character(800) value,longval
      character(1) dtype
      character keywrd*70
      logical exist
      character(40) taskname
      common /task/ taskname

      taskname = 'fkeypar3.1'
      status=0
      call ftcmsg

C       get the name of the input FITS file
      call uclgst('fitsfile',fitfil,status)
      if (status .ne. 0) then
         context = 'could not get fitsfile parameter'
         call fcerr(context)
         goto 999
      endif

C       get the name of the keyword
      call uclgst('keyword',keywrd,status)
      if (status .ne. 0) then
         context = 'could not get keyword parameter'
         call fcerr(context)
         goto 999
      endif

C keyword can only have 70 characters
      if (fcstln(keywrd) .gt. 70) then
         context = ' Keywords longer than 70 characters is not allowed'
         call fcerr (context)
         status = 10
         goto 999
      endif

C       parse the output string to get the file name and extension number
      call fcpars(fitfil,infil,extnum,status)

C EAG 8/25/93 default to 1st extension
      if (extnum .eq. -99) extnum = 1

      if (status .gt. 0 .or. extnum .lt. 0)then
         context='unable to parse the filename[extension]'
         call fcerr(context)
         go to 999
      end if

C       open the new FITS file with read access
      call ftopen(funit,infil,0,blksiz,status)
      if (status .gt. 0)then
         context='unable to open the FITS file'
         call fcerr(context)
         go to 998
      end if

C       move to the correct extension
      call ftmahd(funit,extnum+1,hdutyp,status)
      if (status .gt. 0)then
         context='unable to move to specified extension'
         call fcerr(context)
         go to 998
      end if

C       read the keyword and comment strings
      call ftgkey(funit,keywrd,value,coment,status)

C     ignore 204 error: keyword has no value string
      if (status .eq. 204) status = 0
      
      if (status .gt. 0)then
         status = 0
         exist = .false.
         call ftcmsg
      else
         exist = .true.
C        if a string keyword, reread it to support possible long strings
         call ftdtyp(value, dtype, status)
	 
C     ignore 204 error: keyword has no value string
      if (status .eq. 204) status = 0
      
         if (dtype .eq. 'C')then
C            move back to top of header
             call ftgrec(funit, 0, value, status)
             call ftgkys(funit,keywrd,longval,coment,status)
C            add quotes around the value
             value = ''''//longval
             jj = 3
             do ii = 799, 2, -1
                if (value(ii:ii) .ne. ' ') then
                   jj = ii + 1
                   go to 100
                end if
             end do
100          continue
             value(jj:jj) = ''''
         end if         
      endif

      call uclpsb ('exist', exist, status)
      if (status .ne. 0) then
         context='unable to write exist parameter'
         call fcerr(context)
         go to 998
      end if

C       write the value string to the .par file
      call uclpst('value',value,status)
      if (status .gt. 0)then
         context='unable to write keyword value'
         call fcerr(context)
         go to 999
      end if

C       write the comment string to the .par file
      call uclpst('comm',coment,status)
      if (status .gt. 0)then
         context='unable to write comment string'
         call fcerr(context)
         go to 999
      end if
      go to 999

 998  continue
C       print out the FITSIO error number and text string
      call fcerrm(status)

 999  continue
C       close the fits file 
      call ftclos(funit,status)
      end
