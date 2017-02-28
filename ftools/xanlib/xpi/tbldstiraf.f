**==tbldstandiraf.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* Loads the parameter table for a standalone program
*
* $Id: tbldstiraf.f,v 3.9 2013/05/21 19:08:47 irby Exp $
* $Log: tbldstiraf.f,v $
* Revision 3.9  2013/05/21 19:08:47  irby
* Change character*n to character(n) to silence warnings: "Obsolescent
* feature: Old-style character length".
*
* Revision 3.8  2011/07/13 20:07:15  irby
* Check return status of TBLDPR and return if non-zero.
*
* Revision 3.7  2006/02/13 21:34:07  irby
* Increase length of par file name to accomodate long directory paths.
*
* Revision 3.6  1998/03/25 20:48:28  peachey
* Added a '1' to xinird.f and xnxtrd.f to distiguish these routines from
* routines of the same name in xparse
*
* Revision 3.5.1.1  1996/04/16 01:39:22  dunfee
* Start of pristine ftools CVS...
*
c Revision 1.8  1995/06/16  15:38:01  oneel
c Fix irafpargetnpars to not subtract 13, this is fixed correctly these
c days
c
c Revision 1.7  1995/06/15  20:02:26  oneel
c fix tblextrapar
c
c Revision 1.6  1995/06/15  20:02:14  oneel
c tblextrapar needs to be set false, not true
c
c Revision 1.5  1995/06/15  12:54:12  oneel
c added tblextrapar
c
c Revision 1.4  1995/06/14  18:07:04  oneel
c made sure tblstandalone was set true before par file loaded
c
c Revision 1.3  1995/06/02  14:08:20  oneel
c more multiple par file changse
c
*
* parfile_name is the name of the program to use for the paramter file
*
      SUBROUTINE TBLDSTANDIRAF(Parfile_name)
 
*
      IMPLICIT NONE
 
      CHARACTER*(*) Parfile_name
      INTEGER*4 ierr
 
      INTEGER*4 LENACT
 
      character(1000) filename
 
      character(1000) str , str1
      INTEGER*4 lstr
      LOGICAL*4 YSTCMD
 
      INCLUDE 'tbl.inc'
 
      CALL XCHATY(10,10)
 
      CALL XPIOPENLOG
 
*	call ydebug (.true.)
 
* find the .par file name
 
      filename = ' '
*      filename = parfile_name(1:LENACT(parfile_name)) // '.par'
*	print *,parfile_name
 
      CALL TBFDPRIRAF(Parfile_name(:LENACT(Parfile_name)),filename,ierr)
      IF ( ierr.NE.0 ) RETURN
 
 
      tblextrapar = .false.
     
      CALL TBLDPR(filename,ierr)
      IF ( ierr.NE.0 ) RETURN
 
      tblstandalone = .true.
      TBLstandalonecmd = Parfile_name
C      TBLpcnt = TBLpcnt - TBLPEXTRA
C      TBLpcntorig = TBLpcntorig - TBLPEXTRA
      CALL SETSTANDALONE
      CALL GTBUFSTAND
      call gtbufnoexpandat
 
      lstr = LEN(str)
C      CALL RDFORN(str,lstr)
      CALL GTBUF(' ',ierr)
      CALL GTREST(str,lstr)
 
 
      TBLstandalonecmd = TBLstandalonecmd(1:LENACT(TBLstandalonecmd))
     &                   //' '//str
 
      IF ( str.NE.' ' ) THEN
 
         str1 = 'You should not see this!'
 
         CALL XINIRD1(str1,str,ierr)
      ENDIF

      call gtbufnoexpandat

      IF ( .NOT.YSTCMD() ) STOP

      call apply_mode

      RETURN
      END
**==irafpargetpfname.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
 
 
 
* Support iraf routines
 
* Returns the parameter file name
      SUBROUTINE IRAFPARGETPFNAME(Pfname)
 
      CHARACTER*(*) Pfname
      INCLUDE 'tbl.inc'
 
      Pfname = TBLpfname(tblpcpf)
      RETURN
      END
**==irafpargetnpars.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
 
* Returns the number of parameters
      SUBROUTINE IRAFPARGETNPARS(I)
 
      INTEGER I
      INCLUDE 'tbl.inc'
 
C      I = TBLpcntorig - 13
      I = TBLpcnt
      RETURN
      END
**==irafpargetline.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
 
* Returns the string values for parameter number i
 
      SUBROUTINE IRAFPARGETLINE(I,Name,Desc,Type,Minp,Maxp,Defl,Mode)
 
      INCLUDE 'tbl.inc'
 
      INTEGER I
 
      CHARACTER*(*) Name , Desc , Type , Minp , Maxp , Defl , Mode
 
      IF ( I.GT.TBLpcnt ) RETURN
 
      Name = TBLpname(I)
      Desc = TBLpdesc(I)
      Type = TBLptype(I)
      Minp = TBLpminp(I)
      Maxp = TBLpmaxp(I)
      Defl = TBLpdefl(I)
      Mode = TBLpupd(I)
      RETURN
      END
**==irafparputline.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
 
 
* Put the default value for parameter number i
 
      SUBROUTINE IRAFPARPUTLINE(I,Defl)
 
      INCLUDE 'tbl.inc'
 
      INTEGER I
 
      CHARACTER*(*) Defl
 
      IF ( I.GT.TBLpcnt ) RETURN
 
      TBLpdefl(I) = Defl
      RETURN
      END
 
