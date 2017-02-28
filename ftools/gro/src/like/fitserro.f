c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE FITSERROR(STOP,STATUS,LOC)
C
C
C  $Id: fitserro.f,v 1.3 2013/05/21 19:08:25 irby Exp $
C=======================================================================
c	Effect:
C++    ERROR = proc(STOP,SIGNAL,SIGMSG,LOC)
C++          effect: Prints SIGNAL, SIGMSG, and LOCATION.
C++                  If stop=1 then PAUSE
c
c--------------------------------------------------------------------------
c     Subroutine Arguments Definitions
c     integer      stop      stop=1 => halt execution
c     integer      status    FITSIO status
c     character(8)  loc	     location of error, or subroutine indicator
c
c--------------------------------------------------------------------------
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:     by  JRM
c
c 11-30-95 JAE 	Updated to check various levels of file existance
c		for status=stop=-1 (use this instead of INQUIRE).
c		if the file or directory does not exist and status=-1
c		then status is returned as 103, if existance is true
c		but read privilage (or cd to directory privilage) is
c		restricted then status is returned as 104.
c
c		For status = -1 and stop = -1 existence and no restriction
c		returns status = 0
c
c
C=======================================================================
C  $Log: fitserro.f,v $
C  Revision 1.3  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.2  2006/04/17 20:54:28  irby
C  Updates for compliance with g95 and gfortran:
C
C  - Replace call to lnblnk with equivalent call to len_trim.
C    lnblnk (a routine which used to live in libg2c) is not currently
C    available with g95 or gfortran.
C
C  - Change calls to "perror" (also libg2c) to fcerr or c_fcerr.
C
C  - Change calls to IDATE (libg2c) to new libgro routine GIDATE.
C
C  - Fix non-integer loop variables.
C
C  Revision 1.1  2002/04/16 20:27:30  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.2  1996/03/06  21:19:34  jae
c Updated code: FITS_DIR read with GETENV only
c once at program start in like.f
c
c Revision 5.1  1996/02/29  20:47:52  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:59  jae
c Subroutine Module for like V5.00
c
C
c
c--------------------------------------------------------------------------

      SUBROUTINE FITSERROR(STOP,STATUS,LOC)

C     Common blocks used:
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE '../COMMON/cnfrep.copy'

      save

      character(80) id, fits_dir
      common /id/id
      INTEGER STATUS, access
      INTEGER STOP,ierr0,ierr1


      id = '$Id: fitserro.f,v 1.3 2013/05/21 19:08:25 irby Exp $'

      nj=nj+1
      if(jae_fitserror)write(*,'("In routine FITSERROR. Call # ",
     &     i4,"  STATUS = ",i4)')  nj,status
      ierr0=0
      ierr1=0
      fits_dir = data_dir

c     if(status.eq.-1)stop=-1
      SIGNAL=' '
      IF(STATUS.ne.0) then
c1
	 if(STATUS.eq.104)then
c2
            ierr0=access(mapfile,' ')
            ierr1=access(mapfile,'r')
            if(jae_fitserror)then
               write(*,*)' '
               write(*,'("Status:",i4,"  ierr0:",i4,
     &              "  ierr1:",i4)') status,ierr0,ierr1
               write(*,*)' '
            endif
            if(ierr0.ne.0)then	
c3
               write(SIGMSG,'(" The file does not exist")')
               SIGNAL='E'
               write(*,*)' '
               write(*,*)' '
               goto 104
            elseif(ierr1.ne.0)then
               write(*,*)' '
               write(SIGMSG,'(" Data access restricted")')
               SIGNAL='E'
               write(*,*)' '
               goto 104
            else
               write(*,*)' '
               write(SIGMSG,'(" Data file may be corrupted")')
               SIGNAL='E'
               write(*,*)' '
               goto 104
            endif
	 endif
c1
	 if(STATUS.eq.103.or.status.eq.-1)then
c2
            n = len_trim(FITS_DIR)
            if(FITS_DIR(n:n).eq.'/')then
               FITS_DIR(n:n)=' '
               n=n-1
               if(n.lt.1)n=1
            endif
            if(FITS_DIR(1:n).eq.MAPFILE(1:n).and.n.gt.0)then
c3
               ierr0=access(FITS_DIR(1:n),' ')
               ierr1=access(FITS_DIR(1:n),'x')
               if(jae_fitserror)then
c4
		  write(*,*)' '
		  write(*,'("Checking FITS_DIR:",a)') FITS_DIR(1:n)
		  write(*,'("Status:",i4,"  ierr0:",i4,
     &                 "  ierr1:",i4)') status,ierr0,ierr1
		  write(*,*)' '
               endif
c3
               if(ierr0.ne.0)then	
c4
		  write(SIGMSG,'(" FITS_DIR Path does not exist")')
		  SIGNAL='E'
		  if(status.eq.-1)then
c5
                     status=103
                     goto 104
		  endif
c4
		  write(*,*)' '
		  goto 104
               elseif(ierr1.ne.0)then
		  write(SIGMSG,'(" Directory/Data access restricted")')
		  SIGNAL='E'
		  if(status.eq.-1)then
c5
                     status=104
                     goto 104
		  endif
c4
		  write(*,*)' '
		  goto 104
               endif
c3
               ierr0=access(mapfile,' ')
               ierr1=access(mapfile,'r')
               if(jae_fitserror)then
c4
		  write(*,*)' '
		  n = index(MAPFILE, ' ') - 1
		  if(n.eq.0)n=1
		  write(*,'("Checking MAPFILE:",a)') MAPFILE(1:n)
		  write(*,'("Status:",i4,"  ierr0:",i4,
     &                 "  ierr1:",i4)') status,ierr0,ierr1
		  write(*,*)' '
               endif
c3
               if(ierr0.ne.0)then	
c4
		  write(SIGMSG,'(" The file does not exist")')
		  SIGNAL='E'
		  if(status.eq.-1)then
c5
                     status=-103
                     goto 104
		  endif
c4
		  write(*,*)' '
		  goto 104
               elseif(ierr1.ne.0)then
		  write(SIGMSG,'(" Data access restricted")')
		  SIGNAL='E'
		  if(status.eq.-1)then
c5
                     status=104
                     goto 104
		  endif
c4
		  write(*,*)' '
		  goto 104
               else
		  if(status.eq.-1)then
c5
                     status=0
                     goto 104
		  endif
c4
		  write(SIGMSG,'(" Data file may be corrupted")')
		  SIGNAL='E'
		  write(*,*)' '
		  goto 104
               endif
c3
            else
 40            ierr0=access(mapfile,' ')
               ierr1=access(mapfile,'r')
               if(jae_fitserror)then
c4
		  write(*,*)' '
		  n = index(MAPFILE, ' ') - 1
		  if(n.eq.0)n=1
		  write(*,'("Checking MAPFILE:",a)') MAPFILE(1:n)
		  write(*,'("Status:",i4,"  ierr0:",i4,
     &                 "  ierr1:",i4)') status,ierr0,ierr1
		  write(*,*)' '
               endif
c3
               if(ierr0.ne.0)then	
c4
		  write(SIGMSG,'(" The file does not exist")')
		  SIGNAL='E'
		  if(status.eq.-1)then
c5
                     status=-103
                     goto 104
		  endif
c4
		  write(*,*)' '
		  goto 104
               elseif(ierr1.ne.0)then
		  write(SIGMSG,'(" Data access restricted")')
		  SIGNAL='E'
		  if(status.eq.-1)then
c5
                     status=104
                     goto 104
		  endif
c4
		  write(*,*)' '
		  goto 104
               else
		  if(status.eq.-1)then
c5
                     status=0
                     goto 104
		  endif
c4
		  write(SIGMSG,'(" Data file may be corrupted")')
		  SIGNAL='E'
		  write(*,*)' '
		  goto 104
               endif
c3
            endif
c2
	 endif
c1
C
 104     if(jae_fitserror)write(*,'("EXIT: ierr0:",i4," ierr1:",i4)') 
     &        ierr0,ierr1

         if(status.le.0)return
	 IF(STOP.ge.0)WRITE(LU(1),103) STATUS,LOC,SIGMSG
         IF (STOP.gt.0) THEN
c2
            WRITE(LU(1),*)' Execution paused.'
            PAUSE
            return
         ELSEIF(STOP.eq.0)then
            WRITE(LU(1),*)' Execution continues.'
            RETURN
	 ELSE
            RETURN
         ENDIF
c1
      ENDIF
c0
 103  FORMAT(' Status code (',I4,') found in ',A,
     &     '; additional info.:',/,A,//)
      END
c
c 102  too many FITS files open at once; all internal buffers full
c 103  unable to find requested file to open; does it exist?
c 104  error opening existing file
c 105  error creating new FITS file; (does a file with this name already exist?)
c 106  error writing record to FITS file
