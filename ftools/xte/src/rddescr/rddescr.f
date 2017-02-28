C ***************************************************************************
C SELECTOR TASK
C      rddescr
C
C FILE:
C      rddescr.f
C
C DESCRIPTION:      
C     Read an XTE PCA or HEXTE  .pha file for its channel descriptor
C      and gain/offset values (PCA only).
C      Output info in ASCII format for RBNRMF and (PCA) GCORRMF.
C      
C AUTHOR:
C      James Lochner  5/95
C     with Tod Strohmeyer (PCA) and Ian George (HEASARC)
C      
C MODIFICATION HISTORY:
C      1996 Aug 31 - modified GTDESCR to work on HEXTE data
C                    P. Blanco (HEXTE/UCSD).
C      
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      phafil    - name of input pha file
C      chanfil   - name for output channel file (ascii)
C      chatter   - how much to tell user
C
C CALLED ROUTINES:
C     subroutine gpardescr  - gets parameters from environment
c     subroutine doxcpaarf - constructs and outputs the .arf file
C ************************************************************************** 

      Subroutine RDDESR

c start with initial declarations
      character(160) phafil, chanfil
      integer chatter
      
      logical abort
        
      character(40) taskname
      common /task/ taskname

      taskname = 'rddescr1.11'
      abort = .false.
        
c get the parameters from the par file
      call gpardescr(phafil, chanfil, chatter)

c Perform the Algorithm:   
      call gtdescr(phafil, chanfil, chatter)

c  Exit subroutine

	return
	end
C*****************************************************************
C SUBROUTINE:
C      gpardescr
C
C DESCRIPTION:      
C      Gets parameters from parameter file
C      
C AUTHOR:
C      James Lochner  5/95
C
C MODIFICATION HISTORY:
C      
C NOTES:
C      gpardescr uses F77/VOS like calls to read parameters from .par file
C
C USEAGE:      
C      call gpardescr(phafil, chanfil, chatter)
C      
C ARGUMENTS:
C     phafil	 - name of input PCA pha file
C     chanfil   - name of output ASCII file with channel binning
C     chatter   - parameter for how much to tell user
C     
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      subroutine uclgst - get string parameter
C      
C *******************************************************************************

      SUBROUTINE gpardescr(phafil,chanfil, chatter)


c start with the declarations
      character*(*) phafil, chanfil
      integer chatter
      character(80) context
      integer  status
      
      status = 0

      
c get the name of the input pha file
	call uclgst('phafil',phafil,status)
	if (status .ne. 0) then
	    context = 'could not get PHAFIL parameter'
	    call fcerr(context)
	    go to 999
	endif

c get the name of the output channel binning file
	call uclgst('chanfil',chanfil,status)
	if (status .ne. 0) then
	    context = 'could not get CHANFIL parameter'
	    call fcerr(context)
	    go to 999
         endif
         
c get the chatter parameter
	call uclgsi('chatter',chatter,status)
	if (status .ne. 0) then
	    context = 'could not get CHATTER parameter'
	    call fcerr(context)
            status = 0
            context = 'setting CHATTER = 10'
            call fcerr(context)
            chatter = 10
	endif
        

c Exit subroutine
999	continue 
	if (status .ne. 0) call fcerrm(status)

	return
	end
C*****************************************************************
C SUBROUTINE:
C      gtdescr
C
C DESCRIPTION:      
C     Gets portions of XFF data descriptor from XTE PHA file.
C     Interprets the channel boundaries into GRPPHA syntax
C      
C AUTHOR:
C      James Lochner  8/95
C
C MODIFICATION HISTORY:
C     Sept. 8, 1995 - look for either CPIX1 or CPIX2 for channel descriptor
C     Aug 31, 1996 - added skip over GAINAPP code for non-PCA data (eg. HEXTE)
C     Apr 3, 1997  - look in CPIXn keyword for channel descriptor, using
C                     whatever is in the largest n
C	7/6/98 - by ZG to change the string length of obs_date and obs_time
C		in compliance with y2k new format date string.
C NOTES:
C
C USEAGE:      
C      call gtdescr(phafil, chanfil, chat)
C      
C ARGUMENTS:
C     phafil  - name of input PCA pha file
C     chanfil - name of output ASCII file with channel binning
C     chat    - amount to tell user
C     
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      subroutine uclgst - get string parameter
C      
C *******************************************************************************

      SUBROUTINE gtdescr(phafil,chanfil, chat)


c start with the declarations
      character*(*) phafil, chanfil

      character(800) cpix(10)
      character(8) pha_telescop,pha_instrume,pha_detnam,pha_filter
C      character(16) obs_date, obs_time
	character(68) obs_date, obs_time
      character(80) context, comment, kwnam
      
      integer ftstatus, ierr
      integer chat, iunit, ireadwrite
      logical gainapp

c Initialize all error variables to avoid problems with various compilers.
      ftstatus=0
      ierr=0
      
C Open the XTE PHA file
      ireadwrite = 0
      call ck_xtepha(chat, phafil, iunit, ireadwrite,
     &   pha_telescop, pha_instrume,
     &   pha_detnam, pha_filter, obs_date, obs_time, ierr)
      
C Read the CPIXn keyword to get the channel binning
C  (nfound is the largest n found)
      call ftgkns(iunit,'CPIX',1,10,cpix,nfound,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to obtain CPIX keyword value'
         call fcerr(context)
         go to 999
      endif

c 16Jun98: ffgkns (the CFITSIO routine for which ftgkns is now
c just a wrapper) doesn't support long strings. There is going
c to be a wrapper to ffgkls, ie ftgkls, which can be used but
c since we need to patch the v4.1 release version we'll just use
c ftgkys (which DOES support long strings in its wrapper)
c
c here come de kludge...
      call ftkeyn('CPIX',nfound,kwnam,ftstatus)
      call ftgkys(iunit,kwnam,cpix(nfound),comment,ftstatus)
      if (ftstatus .ne. 0) then
         context = 'unable to obtain CPIX keyword value'
         call fcerr(context)
         go to 999
      endif

C Parse the channel descriptor into GRPPHA style
      call parschan(cpix(nfound),chanfil)

C Execute the rest of this section only for the PCA, otherwise skip
      IF (INDEX(pha_instrume, 'PCA') .EQ. 0) GO TO 999
      
C Look for GAINAPP keyword. If GAINAPP = 'T', then decipher and output
c     gain and offset info.  If keyword not found, assume F
      call ftgkyl(iunit,'GAINAPP',gainapp,comment,ftstatus)
      if (ftstatus .eq. 202) ftstatus = 0
      if (ftstatus .ne. 0) then
         context = 'unable to obtain GAINAPP keyword value'
         call fcerr(context)
         go to 999
      endif
      if (gainapp) then
         call shftchan(iunit, ierr)
         if (ierr .eq. 0) then
            context = 'EDS gain & offset applied to data'
            call fcecho(context)
            context = '-> Run GCORRMF on .rmf files'
            call fcecho(context)
         else
            context = 'Unable to write files for gain shifting'
         endif
      endif
      

C - end of PCA-specific section      
          
c Exit subroutine
999   continue
      call ftclos(iunit,ftstatus)
      call ftfiou(iunit,ftstatus)     

      if (ierr .ne. 0) call fcecho(context)
      if (ftstatus .ne. 0) call fcerrm(ftstatus)

      return
      end



C*****************************************************************
C SUBROUTINE:
C      parschan
C
C DESCRIPTION:      
C     Interprets the channel boundaries into GRPPHA syntax
C      
C AUTHOR:
C      James Lochner  8/95
C
C MODIFICATION HISTORY:
C      
C NOTES:
C
C USEAGE:      
C      call parschan(chanstr)
C      
C ARGUMENTS:
C     chanstr   - channel binning from data descriptor
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcecho  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      
C **************************************************************************

      SUBROUTINE parschan(subset1, chanfil)


c start with the declarations

      character*(*) subset1,chanfil
      character(1) separator
      character(20) cval
      character(80) context
      integer status,outlen1,
     &   fcstln, i, ival, ifirst, icomma,
     &   itilde, icolon, isemicolon, iend,
     &   ival1, ival2, ival3, izero, ounit, ione,
     &   icount, iholds(256), iholde(256), iholdi(256),
     &   inegative, i255
      status=0
      
C Initialize temporary string
      
C Open the output file
      call ftgiou(ounit,status)
      call faopen(ounit, chanfil, 2, 133, status)
      if (status .ne. 0) then
         context = 'could not open output channel file:'//
     $        ' file already exists ?'
         call fcerr(context)
         go to 999
      endif
      
c**********************************************************************

      icount=0
      separator=','
      icomma=0
      icolon=0
      isemicolon=0
      itilde=0
      outlen1=fcstln(subset1)

      ifirst=1
      iend=0
      ival1=0
      ival2=0
      ival3=0
      izero=0
      ione=1
      i255 = 255
      inegative = -1

1000  format(1x,I4,2x,I4,2x,I4)
      
      do 10 i=1,outlen1
          
        if(subset1(i:i).eq.',')icomma=i
        if(i.eq.outlen1)icomma=i+1

        if(subset1(i:i).eq.'(')ifirst=i+1
        if(subset1(i:i).eq.'~')itilde=i
        if(subset1(i:i).eq.':')icolon=i
        if(subset1(i:i).eq.';')isemicolon=i
        if(subset1(i:i).eq.')')iend=i-1

        if(icomma.ne.0)then

          if(iend.eq.0)iend=icomma-1
            
          if(itilde.ne.0.and.icolon.eq.0)then
            cval=subset1(ifirst:itilde-1)
            call ftc2i(cval,ival1,status)

            if(isemicolon.eq.0)then
              cval=subset1(itilde+1:iend)
              call ftc2i(cval,ival2,status)
              ival3=(ival2-ival1)+1
              icount=icount+1
              iholds(icount)=ival1
              iholde(icount)=ival2
              iholdi(icount)=ival3
c              write(ounit,1000)ival1,ival2,ival3
            elseif(isemicolon.ne.0)then
              cval=subset1(itilde+1:isemicolon-1)
              call ftc2i(cval,ival2,status)
              cval=subset1(isemicolon+1:iend)
              call ftc2i(cval,ival3,status)
              icount=icount+1
              iholds(icount)=ival1
              iholde(icount)=ival2
              iholdi(icount)=ival3
c              write(ounit,1000)ival1,ival2,ival3
            endif

          elseif(itilde.eq.0.and.icolon.ne.0)then
            cval=subset1(ifirst:icolon-1)
            call ftc2i(cval,ival1,status)
            
            if(isemicolon.eq.0)then
              cval=subset1(icolon+1:iend)
              call ftc2i(cval,ival2,status)
              ival3=1
              icount=icount+1
              iholds(icount)=ival1
              iholde(icount)=ival2
              iholdi(icount)=ival3
c              write(ounit,1000)ival1,ival2,ival3
            elseif(isemicolon.ne.0)then
              cval=subset1(icolon+1:isemicolon-1)
              call ftc2i(cval,ival2,status)
              cval=subset1(isemicolon+1:iend)
              call ftc2i(cval,ival3,status)
              icount=icount+1
              iholds(icount)=ival1
              iholde(icount)=ival2
              iholdi(icount)=ival3
c              write(ounit,1000)ival1,ival2,ival3
            endif
              
          elseif(itilde.eq.0.and.icolon.eq.0)then
            cval=subset1(ifirst:iend)
            call ftc2i(cval,ival,status)
            icount=icount+1
            iholds(icount)=ival
            iholde(icount)=ival
            iholdi(icount)=ione
c            write(ounit,1000)ival,ival,ione

          endif
          
          ifirst=icomma+1
          icomma=0
          itilde=0
          icolon=0
          iend=0
          isemicolon=0
          
        endif
          
10    continue

c     Okay, now that we have the channel boundaries stored in
c an array we can do some sanity checks as well as output a file
c that is ready to be input into rbnrmf.

      do 20 i=1,icount

c  Let's check to make sure that all of the channels are valid!
        if(iholds(i).lt.0.or.iholde(i).lt.0.or.iholds(i).gt.255.or.
     &     iholde(i).gt.255)then
          call fcecho(' ')
          call fcecho('ERROR!!!! INVALID channel value found!')
          call fcecho('Check CPIXn keyword!')
          call fcecho('Cannot continue... ABORTING...')
          goto 999
          
        endif
        
        if(i.eq.1)then

c   The first channel should always be ZERO! So let's deal with
c the case where it doesn't equal zero. 
          if(iholds(i).ne.0)then
            write(ounit,1000)izero, iholds(i)-1, inegative
            write(ounit,1000)iholds(i),iholde(i),iholdi(i)            
          else
            write(ounit,1000)iholds(i),iholde(i),iholdi(i)
          endif

c  Let's deal with all of the in between cases cases. 
        elseif((i.gt.1).and.(i.lt.icount))then
          if(iholde(i).ne.iholds(i+1)-1)then
            write(ounit,1000)iholds(i),iholde(i),iholdi(i)
            write(ounit,1000)iholde(i)+1,iholds(i+1)-1,inegative
          else
            write(ounit,1000)iholds(i),iholde(i),iholdi(i)
          endif

c   The last channel should always be 255! So let's deal with
c the case where it doesn't equal 255. 

        elseif(i.eq.icount)then
          if(iholde(i).ne.255)then
            write(ounit,1000)iholds(i),iholde(i),iholdi(i)            
            write(ounit,1000)iholde(i)+1, i255, inegative
          else
            write(ounit,1000)iholds(i),iholde(i),iholdi(i)
          endif

        endif

20    continue
          
c Exit subroutine
999   continue
      
C     Close the output file
      CLOSE(UNIT=ounit)
      call ftfiou(ounit,status)

      if (status .ne. 0) call fcerrm(status)

      return
      end

C*****************************************************************
C SUBROUTINE:
C      shftchan
C
C DESCRIPTION:      
C     Create channel shifting files for each detector, using info
c     in the PCUnGAIN and PCUnOFST keywords
C      
C AUTHOR:
C      James Lochner  8/95
C
C MODIFICATION HISTORY:
C      
C NOTES:
C
C USEAGE:      
C      call shftchan(iunit, ierr)
C      
C ARGUMENTS:
C     iunit     - unit number for input pha file
C
C PRIMARY LOCAL VARIABLES:
C      context  - error message
C      status   - error number
C
C CALLED ROUTINES:
C      subroutine fcerr  - echo message to terminal
C      subroutine fcerrm - echo fitsio error message to terminal 
C      
C *******************************************************************************

      SUBROUTINE shftchan(iunit, ierr)

c start with the declarations
      integer iunit
      
      integer n, jshft_lo, jshft_hi
      integer ounit, ftstatus, ierr
      integer gain, offset
      character(80) context,comment,shftfil
      character(20) n20
      character(8) gainkey, offkey
      character(1) nchar
      real r
      
      ierr = 0
      ftstatus = 0
      
C     For Each detector,
      do n = 0,4
         
C     convert the integer into a character
         call fti2c(n,n20,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'could not convert integer to character'
            call fcerr(context)
            go to 999
         endif
         nchar = n20(20:20)
         
C     read the PCUnGAIN and PCUnOFST values
         gainkey = 'PCU'//nchar//'GAIN'
         offkey = 'PCU'//nchar//'OFST'
         call ftgkyj(iunit,gainkey,gain,comment,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'could not read '//gainkey//'keyword'
            call fcecho(context)
            ierr=1
            ftstatus=0
            go to 999
         endif
         call ftgkyj(iunit,offkey,offset,comment,ftstatus)
         if (ftstatus .ne. 0) then
            context = 'could not read '//offkey//'keyword'
            call fcecho(context)
            ierr=1
            ftstatus=0
            go to 999
         endif
         
C      Open the output file
         shftfil = 'pcu'//nchar//'_shft.txt' 
         call ftgiou(ounit,ftstatus)
         call faopen(ounit, shftfil, 2, 133, ftstatus)         
         if (ftstatus .ne. 0) then
            context = 'could not open output shift file: '//
     $           'file already exists ?'
            call fcerr(context)
            go to 999
         endif
         
C     compute the channel shifting
         if (gain .gt. 0) then
            jshft_lo = 0
            jshft_hi = 0
            r = offset + 0.5
            do while (jshft_hi .lt. 255)
               jshft_hi = 256 * (r - offset) / gain
               if (jshft_hi - 256*(r-offset)/gain .eq. 0)
     $              jshft_hi = jshft_hi - 1
               if (jshft_hi .gt. 0) then
                  write(ounit,1000) jshft_lo, jshft_hi, int(r-0.5)
1000              format(' ',i3,2x,i3,2x,i3)
                  jshft_lo = jshft_hi + 1
               endif
               
               r = r + 1.0
            end do

         else if (gain .lt. 0) then
            jshft_lo = 0
            jshft_hi = 0
            r = offset - 0.5
            do while (jshft_hi .lt. 255 .and. r .gt. -10.0)
               jshft_hi =  256 * (r - offset) / gain
               if (jshft_hi + 256*(r-offset)/gain .eq. 0)
     $              jshft_hi = jshft_hi - 1
               if (jshft_hi .gt. 0) then
                  write(ounit,1000) jshft_lo, jshft_hi, int(r+0.5)
                  jshft_lo = jshft_hi + 1
               endif
               
               r = r - 1.0
            end do
            
         else
            jshft_lo = 0
            jshft_hi = 255
            write(ounit,1000) jshft_lo,jshft_hi,offset 
         endif
         
C     Close the output file
         CLOSE(UNIT=ounit)
         call ftfiou(ounit,ftstatus)
      end do
      
c Exit subroutine
999   continue
      
      if (ftstatus .ne. 0) then
         call fcerrm(ftstatus)
         ierr = 1
      endif
      
      return
      end
