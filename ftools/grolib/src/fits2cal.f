c 	Module Name:    FITS2CAL -- Generic version	
c	Module Author:	Don Jennings	- 	CSC		
c	Project:	Compton Observatory Science Support Center
c       Date:           7/1/92
c       Version:        1.1
c                He who shall, so shall he who		
c	***********************************************************	
c	
c	Module Description:					
c								
c     This program converts EGRET FITS calibration files back into
c     thier original EGRET CAL format. It first looks for the TELESCOP,
c     INSTRUME and PRIMTYPE primary header keywords; if these are not
c     present, or not set to the correct values, the program terminates.
c
c     The user is prompted for the input FITS file name and the output
c     file name. If no output file name is specified, then the original
c     CAL filename (retrieved from the FITS header) is used instead.
c
c     One must set the ARCH variable to the proper computer architecture.
c     The choices are "SUN" for SUN microsystem computers, "VAX" for 
c     Digital VAX systems, and "DEC" for Digital DECstations.
c
c     All FITS read and write routines are based upon the FITSIO library
c     writen by Bill Pence of the HEASARC. This program must link to
c     these routines in order to compile.
c
c     This routine is based upon an older version of FITS2CAL. Therefore,
c     it does not strictly conform to COSSC FITS converter style.
c
c	***********************************************************
c
c	Revision History:					
c								
c	7/1/92 D. Jennings, program updated from original FITS2CAL
c       routine to make version 1.1
c		
c	***********************************************************
c
c	Outline:
c	
c       initialize the variables
c       set the read in record length, according to the archecture type
c       get command line arguments and open files
c       check TELESCOP, INSTRUME and PRIMTYPE keywords for correct values
c       If PRIMTYPE equals EGRET_SARCAL
c       THEN call fits2sar()
c       ELSE
c         If PRIMTYPE equals EGRET_PSDCAL or EGRET_EDPCAL
c         THEN call fits2oth()
c         END IF
c       END IF
c       END
c
c	************************************************************
c
c	Variable Descriptions:					
c								
c	Inputs			Description			
c	--------------		------------------------------	
c       fname                   name of input FITS file
c       oname                   name of output file
c									
c	Outputs 		Description			
c	--------------		------------------------------	
c       none
c
c	Locals			Description			 
c	--------------		------------------------------	 
c       reclen                  read in record length
c       status                  return code for FITSIO routines
c       blklsize                FITS file record length
c       num                     general counting var
c       comment                 FITS header comment
c       keyval                  FITS keyword value
c       arch                    archecture type: DEC SUN or VAX
c       verbose                 report on program status?
c								 
c	Files			Description
c	--------------		---------------------------------
c	none
c
c	***************************************************************
c
c
c      program fits2cal
	subroutine fits2cal(fileid, fname, oname)

C  Common blocks used:
c      INCLUDE '../../COMMON/jnfrep.copy'
c
      save
c
c ---   DYNAMIC MEMORY ALLOCATION ---
c  the following MEM common block definition is in the system iraf77.inc
c  file
        LOGICAL          MEMB(100)
        INTEGER*2        MEMS(100)
        INTEGER*4        MEMI(100)
        INTEGER*4        MEML(100)
        REAL             MEMR(100)
        DOUBLE PRECISION MEMD(100)
        COMPLEX          MEMX(100)
        EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
        COMMON /MEM/ MEMD
c ----------------------------------------------------------------------+

      integer        reclen,status,blksize,fid
      integer        numrec,p_of,p_good
      character(40)   comment
      character(70)   fname,keyval
      character(80)   oname
      character(3)    arch
      character*(*)  fileid
      logical        verbose

c     if VERBOSE = TRUE, the program informs the user of its status
      verbose = .true.

      status = 0
c
c     Set the computer archetecture to VAX, DEC, or SUN
c
      arch = 'SUN'
c               if on a VAX, reclen is 128 REAL*4 elements
      if(arch .eq. 'VAX') then
         reclen = 128
      else
         if (arch .eq. 'DEC')then
c               DECstation record sizes are arbitary, but they access
c               their bytes 4 at a time
            reclen = 360
         else
c               SUNs access files one byte at a time. Thus, a SUN record
c               length of 1440 bytes equals a DEC record of 360 real*4
c               elements
            reclen = 1440
         endif
      endif


c
c               open the files
c
c      num = iargc()
c      if(num .ne. 1 .and. num .ne. 2)then
c         write(6,*)'USAGE: fits2cal FITS_file [output_file]'
c         write(6,*)' '
c         stop
c      end if
c      call getarg(1,fname)
c      oname = ' '
c      if(num .eq. 2) call getarg(2,oname)


      call ftopen(2,fname,0,blksize,status)


      if(status .ne. 0) stop 'ERROR: FITS file not found'

      if(oname .ne. ' ')then
         open(1,file=oname,status='new',access='direct',form=
     *        'unformatted',recl=reclen)
      else
         call ftgkys(2,'FILENAME',oname,comment,status)
         if(status .ne. 0) stop 'ERROR: no FILENAME keyword found
     * in primary header'
         open(1,file=oname,status='new',access='direct',form=
     *        'unformatted',recl=reclen)
      endif
c
c               check the TELESCOP, INSTRUME and PRIMTYPE keyword
c               values to make sure this is a valid FITS EGRET
c               calibration file
c
      call ftgkys(2,'telescop',keyval,comment,status)
      if(status .ne. 0) stop 'ERROR: TELESCOP keyword not found'
      if(keyval .ne. 'GRO') stop 'ERROR: FITS file not a GRO file'
      call ftgkys(2,'INSTRUME',keyval,comment,status)
      if(status .ne. 0) stop 'ERROR: INSTRUME keyword not found'
      if(keyval .ne. 'EGRET') stop 'ERROR: FITS file not a EGRET file'
      call ftgkys(2,'PRIMTYPE',keyval,comment,status)
      if(status .ne. 0) stop 'ERROR: PRIMTYPE keyword not found'


c  sb:  12/06/01
c  calculate numele outside of fits2sar and fits2oth to save the duplicate
c  effort and also to allow dynamic memory allocation
c  numele is CAL file dependant 

      read(fileid,*) fid
      if (fid .gt. 10) then
         numrec = 20*9*16*75
      else
	 numrec = 20*9*3*74
      endif

      if(keyval .eq. 'EGRET_SARCAL')then 
         call  fits2sar(reclen,arch,verbose,numrec)
      else

c  sb:  12/06/01
c  dynamically allocate numrec bytes of memory to p_of and p_good
c  6 indicates the type REAL
         p_of = 0 
         p_good = 0
	 call udmget(numrec, 6, p_of, status)
	 call udmget(numrec, 6, p_good, status)

c  sb:  12/06/01
c  pass pointers to allocated memory to fits2oth
         if(keyval .eq. 'EGRET_EDPCAL')then
            call  fits2oth(reclen,arch,verbose,numrec,MEMR(p_of),
     *			   MEMR(p_good))
         else
            if(keyval .eq. 'EGRET_PSDCAL')then
               call  fits2oth(reclen,arch,verbose,numrec,MEMR(p_of),
     *			      MEMR(p_good))
            else
               stop 'ERROR: FITS file not a calibration file'
            endif
         endif

	 call udmfre(p_of, 6, status)
	 call udmfre(p_good, 6, status)
      end if
C
      call ftclos(2,status)
c      if(verbose)print *,'ftclos status ',status
      close(1)
      return
      end
C-----------------------------------------------------------------------
      subroutine fits2sar(reclen,arch,verbose,numele)

c     This subroutine converts SARCAL FITS files into their original
c     SARFIL format.
c
      real*4       buff1(360),buff2(128)
      integer      reclen,excess,numele,numrec,i,status,fpixel
      character(3)  arch
      logical      fany,verbose

c               numele is CAL file dependant
      status = 0

c               calculate the number of records to be read in
      if(arch .eq. 'SUN') reclen = reclen/4 
      numrec = numele/reclen
      excess = numele - numrec*reclen
      fpixel = 1
c               loop until all full records have been read in
      do i = 1,numrec
         if(arch .eq. 'VAX')then
            call ftgpve(2,0,fpixel,reclen,0,buff2,fany,status)
            write(1,rec=i)buff2
         else
            call ftgpve(2,0,fpixel,reclen,0,buff1,fany,status)
            write(1,rec=i)buff1
         endif
         if(verbose .and. status .ne. 0)print *,'ftgpve status',status
         fpixel = fpixel + reclen
      enddo
c               read in any partically full records
      if(excess .ne. 0) then
         if(arch .eq. 'VAX')then
            call ftgpve(2,0,fpixel,reclen,0,buff2,fany,status)
            write(1,rec=i)buff2
         else
            call ftgpve(2,0,fpixel,reclen,0,buff1,fany,status)
            write(1,rec=i)buff1
         endif
         if(verbose .and. status .ne. 0)print *,'ftgpve status',status
      endif
c
      return
      end
C---------------------------------------------------------------------
      subroutine fits2oth(reclen,arch,verbose,numrec,of,good)

c     This subroutine converts FITS EDPCAL and PSDCAL files into their
c     original EDPFIL and PSDFIL formats


      real*4       buff0(128),buff1(360),buff2(100),of(*)
      real*4       good(*)
      integer      reclen,i,j,status,fpixel
      integer      pgood,pof,hdutype,k,m,n,ct,diff 
      integer      numrec
      character(3)  arch
      logical      fany,verbose


      status = 0
C     move to the binary table HDU and get the values there
      call ftmahd(2,2,hdutype,status)
      if(hdutype .ne. 2)then
         write(6,*)'ERROR: binary table not found'
         return
      endif


      call ftgcvj(2,1,1,1,numrec,0,good,anyf,status)
      if (status .ne. 0) then
         write(6,*) 'ERROR: Error while reading column 1: status: ',
     *			status
         return
      endif

      call ftgcve(2,2,1,1,numrec,0,of,anyf,status)
      if(status .ne. 0)then
         write(6,*) 'ERROR: Error while reading column 2: status: ',
     *			status
         return
      endif

C               move back to the primary array
      call ftmahd(2,1,hdutype,status)
      if(hdutype .ne. 0)then
         write(6,*)'ERROR: cannot re-access the primary array: ',
     *			'status: ', status
         return
      endif

C     read the primary array values, re-combine with the binary table
C     data, and write to the output file.

c               calculate the number of records to be read in
      if(arch .eq. 'SUN') reclen = reclen/4 
      fpixel = 1
      ct     = 0
      pgood  = 0
      pof    = 0
      n      = 1
c               loop until all full records have been read in
      do i = 1,numrec
         call ftgpve(2,0,fpixel,100,0,buff2,fany,status)
c         if(verbose)print *,'ftgpve status ',status
c               the following code recombines the goodness flags and
c               overflow channells with the primary array data
         fpixel = fpixel + 100
         diff = reclen - ct
         ct       = ct + 1
         pgood    = pgood + 1
         if(arch .eq. 'VAX')then
            buff0(ct) = good(pgood)
         else
            buff1(ct) = good(pgood)
         endif
         if(100 .lt. diff-1)then
            m = 100
         else 
            m = diff-1
         endif
         do j = 1,m
            ct = ct + 1
            if(arch .eq. 'VAX')then
               buff0(ct) = buff2(j)
            else
               buff1(ct) = buff2(j)
            endif
         enddo
c               if buffer is full, flush it
         if(ct .eq. reclen)then
            if(arch .eq. 'VAX')then
               write(1,rec=n)buff0
            else
               write(1,rec=n)buff1
            endif
            n = n + 1
            ct = 0
         endif
         do k = j,100
            ct = ct + 1
            if(arch .eq. 'VAX')then
               buff0(ct) = buff2(k)
            else
               buff1(ct) = buff2(k)
            endif
         enddo
c               if buffer is full, flush it
         if(ct .eq. reclen)then
            if(arch .eq. 'VAX')then
               write(1,rec=n)buff0
            else
               write(1,rec=n)buff1
            endif
            n = n + 1
            ct = 0
         endif
         pof = pof + 1
         ct  = ct + 1
         if(arch .eq. 'VAX')then
            buff0(ct) = of(pof)
         else
            buff1(ct) = of(pof)
         endif
c               if buffer is full, flush it
         if(ct .eq. reclen) then
            if(arch .eq. 'VAX')then
               write(1,rec=n)buff0
            else
               write(1,rec=n)buff1
            endif
            n = n + 1
            ct = 0
         endif
      enddo

C     we might have a partically full buffer. In that case, flush it
C     to the output file
      if(ct .ne. 0) then
         if(arch .eq. 'VAX')then
            write(1,rec=n)buff0
         else
            write(1,rec=n)buff1
         endif
      endif
      if(pgood .ne. numrec) write(6,*)'ERROR: CAL file corrupted'
      if(pof .ne. numrec) write(6,*)'ERROR: CAL file corrupted'
C
      return
      end
C------------------thats all folks--------------------------------------
