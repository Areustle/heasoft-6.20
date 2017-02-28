**==tbldstand.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* Loads the parameter table for a standalone program
*
* $Id: tbldstand.f,v 3.8 2013/05/21 19:08:47 irby Exp $
* $Log: tbldstand.f,v $
* Revision 3.8  2013/05/21 19:08:47  irby
* Change character*n to character(n) to silence warnings: "Obsolescent
* feature: Old-style character length".
*
* Revision 3.7  1998/03/25 20:48:26  peachey
* Added a '1' to xinird.f and xnxtrd.f to distiguish these routines from
* routines of the same name in xparse
*
* Revision 3.6  1996/07/26 15:02:06  miket
* MJT 26July96 calling YINIT (for xselect -- doesn't use opdefpf)
*
c Revision 3.5.1.1  1996/04/16  01:39:21  dunfee
c Start of pristine ftools CVS...
c
c Revision 1.6  1995/06/15  20:02:51  oneel
c Opps, got tblextrapar backwards
c
c Revision 1.5  1995/06/15  20:01:24  oneel
c tblextrapar needs to be set to true, not to false
c
c Revision 1.4  1995/06/15  12:52:24  oneel
c added tblextrapar for extra parameters in tbldpr
c
c Revision 1.3  1995/06/14  18:03:27  oneel
c added log and ID
c
*
* program_name is the name of the program to use for the paramter file
* sysdisk is the disk, and sysdir is the directory for the system
* parameter files.  AN example would be (for either VMS or unix)
* 'xanadu'  'par/xpi'
* ierr is 0 if everything is ok
*
      SUBROUTINE TBLDSTAND(Program_name,Sysdisk,Sysdir,Ierr)
 
*
      IMPLICIT NONE
 
      CHARACTER*(*) Program_name , Sysdisk , Sysdir
      INTEGER*4 Ierr
 
      INTEGER*4 LENACT
 
      character(100) filename
 
      character(1000) str , str1
      INTEGER*4 lstr
      LOGICAL*4 YSTCMD
 
      INCLUDE 'tbl.inc'
 
      call yinit
 
      CALL XPIOPENLOG
 
 
* find the .par file name
 
      filename = ' '
*      filename = Program_name(1:LENACT(Program_name)) // '.par'
 
      CALL TBFDPR(Program_name(:LENACT(Program_name)),Sysdisk,Sysdir,
     &            filename,Ierr)
      IF ( Ierr.NE.0 ) RETURN
 
      tblextrapar = .false.

      CALL TBLDPR(filename,Ierr)
 
      TBLstandalone = .TRUE.
      TBLstandalonecmd = Program_name
C      TBLpcnt = TBLpcnt - TBLPEXTRA
C      TBLpcntorig = TBLpcntorig - TBLPEXTRA
      CALL SETSTANDALONE
 
 
      lstr = LEN(str)
C      CALL RDFORN(str,lstr)
      CALL GTBUF(' ',Ierr)
      CALL GTREST(str,lstr)
 
 
      TBLstandalonecmd = TBLstandalonecmd(1:LENACT(TBLstandalonecmd))
     &                   //' '//str
 
      IF ( str.NE.' ' ) THEN
 
         str1 = 'You should not see this!'
 
         CALL XINIRD1(str1,str,Ierr)
      ENDIF
      IF ( .NOT.YSTCMD() ) STOP

      call apply_mode
 
      RETURN
      END
