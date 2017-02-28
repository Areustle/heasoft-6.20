C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE ERROR(STOP,LOC)
C
C
C  $Id: error.f,v 1.4 2013/05/21 19:08:25 irby Exp $
C=======================================================================
c	Effect: check for error condition(signal flag in CNFREP common
c 	block .ne. ''). On error, print the location(parameter LOC) from
c 	which ERROR has been called, and print the error message (SIGNAL
c 	in CNFREP). Take action according to STOP parameter:
c		STOP = -1  => Require cr from user before continuing
c		STOP =  0  => Print message and continue
c		STOP = +1  => Print message and stop
c
c-----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c	integer     	stop	determines stop or continue condition
c	character(8)	loc	location: indicates subroutine where ERROR 
c				is called
c
C----------------------------------------------------------------------
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C             UPDATED:     by  JRM
C----------------------------------------------------------------------
C  $Log: error.f,v $
C  Revision 1.4  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2006/04/17 20:54:28  irby
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
C  Revision 1.2  2002/12/26 18:45:05  irby
C  Use alternate method for file I/O keyword access='append' which is not
C  recognized by f90.
C
C  Revision 1.1  2002/04/16 20:27:30  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.8  1997/10/31  22:04:26  jae
c Replaced all stop, input and pause statements
c with error remark output to console.
c
c Revision 5.7  1996/06/20  19:59:26  jae
c Added close(81) line and commented out
c line to 'CALL INTENT'
c
c Revision 5.6  1996/06/20  18:47:15  jae
c Added lines to date output
c
c Revision 5.5  1996/06/19  16:10:56  jae
c Added error type -2 which is the same as
c type 0 but does not reset the SIGNAL to blank.
c Improved format to file jerror.txt.
c
c Revision 5.4  1996/06/03  17:13:13  jae
c fixed syntax error in last set of edits.
c Made append into 'append'
c
c Revision 5.3  1996/06/03  17:02:19  jae
c added output logging to file jerror.txt if
c flag jae_error is TRUE
c
c Revision 5.2  1996/03/29  21:53:44  jae
c added lines to stop program on error if
c jae_error flag is set true:: debug mode
c
c Revision 5.1  1996/02/29  20:47:28  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:49  jae
c Subroutine Module for like V5.00
c
C
c
c----------------------------------------------------------------------

      SUBROUTINE ERROR(STOP,LOC2)

C     Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'

      save

      character(80) id
      common /id/id
      INTEGER STOP
      character input*50, LOC2*20
      logical eof


      id = '$Id: error.f,v 1.4 2013/05/21 19:08:25 irby Exp $'
      LOC='ERROR'	

      M = len_trim(LOC2)

      if (jae_error.and.signal.ne.' ') then
	 jerror=jerror+1
	 if (jerror.eq.1) then
            call system('rm -f date.tmp; date > date.tmp')
            open(unit=82,file='date.tmp')
            read(82,'(a)') input
            numcar = len_trim(input)
            close(unit=82)
            call system('rm -f date.tmp')
	 endif

C access='append' not recognized by f90:
C	 open(unit=81,file='jerror.txt',access='append')
 	 open(unit=81,file='jerror.txt')
C move to end of file
         eof=.false.
         do while (.not. eof)
	     read(81,'( )',end=100)
         end do
 100     continue

	 if (jerror.eq.1)write(81,'(A)')input(1:numcar)
	 write(81,'(i5," ERROR:",A," LOC:",A," MESSAGE:",A)')
     *        jerror,signal,loc2(1:M),SIGMSG
	 close(unit=81)
      endif

      IF (SIGNAL.NE.' ') THEN
         WRITE(LU(1),103) SIGNAL,LOC2(1:M),SIGMSG
         IF (STOP.EQ.-1) THEN
            if (jae_error)write(*,'("LOC:",a)') LOC(1:5)
            WRITE(LU(1),106)
c     READ(LU(12),'(A)')input
            SIGNAL=' '
            RETURN
         ELSEIF (STOP.EQ.0.or.STOP.eq.-2) THEN
            if (jae_error)write(*,'("LOC:",a)') LOC(1:5)
            WRITE(LU(1),105)
            IF (STOP.eq.0)SIGNAL=' '
c     if (jae_error)CALL INTENT(determined)
            RETURN
         ELSE
            if (jae_error)write(*,'("LOC:",a)') LOC(1:5)
            WRITE(LU(1),104)
c     PAUSE
         END IF
      ENDIF

 103  FORMAT(' Error code (',A,') found in ',A,
     &     '; message is:',/,A)
 104  FORMAT(' Execution Continues: Severe ERROR occurred.')
 105  FORMAT(' Execution continues: Minor ERROR occurred.')
 106  FORMAT(
     &     ' Execution Continues: A questionable occurance! ')

      END
c
