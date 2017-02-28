C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       subroutine PIXEL_SELECT(flag,text)
C
C
C  $Id: pixel_se.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++   Effect: Selects position for likelihood analysis.
C++   Position srcL,srcB corresponds to
C++   indeces IsrcL,IsrcB in likrep.copy and
C++   inner-pixel shift LSHIFT,BSHIFT in psmrep.copy.
C++   The PSF array is then built with appropriate
C++   LSHIFT,BSHIFT, and CLAT.
C++   If flag is true, consule is queried  (using text string)
C++   for coordinates which
C++   are put into commons.
C++   If flag is false, srcL,srcB coordinates are assumed
C++   and other parameters & PSF array changed without a report.
c
c----------------------------------------------------------------------
c     Subroutine Argument Desriptions
c     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c     flag 	logical   true => query console
c     character text describing the use of the coordinates being requested
c
c
C=======================================================================
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
c
C=======================================================================
C 
C    Changes:                                                           
c     jrm 11/7/93 Move L_check call to after end of consule flag if block
c
C  $Log: pixel_se.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:41  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:20  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/14  22:45:32  jae
c Subroutine Module for like V5.00
c
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      subroutine PIXEL_SELECT(flag,text)

C  Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/likrep.copy'

      save
c
c
c--------------------------------------------------------------------------
      character(80) id
      common /id/id
      character input*50,text*20
      logical flag

      id = '$Id: pixel_se.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='pixslct'


 10   if (flag) then             ! console is queried
         continue
         theLong=srcL

         if (theLong.lt.0.) theLong=theLong+360.

         if (coord_sys.eq.'G') then
            write(lu(1),20)text,theLong,srcB

	 elseif (coord_sys.eq.'C') then
            write(lu(1),30)text,theLong,srcB

	 else
            write(lu(1),40)text,theLong,srcB
         endif

         READ(LU(12),'(a)') input
         numcar = index(input, ' ') - 1
	 
         if (input.eq.'a'.or.input.eq.'A')  then
c     abort operation
            SIGNAL='A'
            SIGMSG='Position input aborted.'
            return 
         endif

         if (numcar.gt.0) read(input,*,end=10)srcL,srcB

         write(lu(1),'("Input Source Name (cr for ",A18,"):",$)')srcN
         READ(LU(12),'(a)') input
         numcar = index(input, ' ') - 1

         if (numcar.ne.0) srcN=input

 20      format('Input ',A20,' GLON and GLAT'
     &        ,/,' (A to abort, cr for ', 2f8.3,'):',$)
 30      format('Input ',A20,' RA (J2000) and DEC'
     &        ,/,' (A to abort, cr for ', 2f8.3,'):',$)
 40      format('Input ',A20,' Map longitude and latitude'
     &        ,/,' (A to abort, cr for ', 2f8.3,'):',$)

      else
c     use srcL,srcB from the likrep common
      endif                     !end consule flag block

      call L_CHECK(srcL)
      if (signal.ne.' ') then
c     CALL ERROR(0,LOC)
c     SIGNAL='A'
c     SIGMSG='Position input aborted.'
         return 
      endif

C     CONVERT TO PIXEL INDEX:
      CALL MAPIXL(srcL,srcB,IsrcL,IsrcB)
      if (signal.ne.' ') then
         if (flag) then 
            CALL ERROR(0,LOC)
            goto 10
         else
            return
         endif
      endif

C     Find pixel center:
      CALL MAPCOR (IsrcL,IsrcB,srcLpxcnt,srcBpxcnt)
      CALL ERROR(1,LOC)
      LSHIFT=srcL-srcLpxcnt
      BSHIFT=srcB-srcBpxcnt
      CLAT=srcBpxcnt            ! latitude of PSM matrix center
      call psmmat(LSHIFT,BSHIFT)
      CALL ERROR(1,LOC)
      LikTotaled=.false.

      return
      END
c
