**==tbsvpr.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* $Id: tbsvpr.f,v 3.10 2013/05/21 19:08:47 irby Exp $
* $Log: tbsvpr.f,v $
* Revision 3.10  2013/05/21 19:08:47  irby
* Change character*n to character(n) to silence warnings: "Obsolescent
* feature: Old-style character length".
*
* Revision 3.9  2011/07/14 21:14:22  irby
* Relocate label "100".
*
* Revision 3.8  2011/07/13 20:25:37  irby
* Supersede XPI's file output function with that of APE: file output
* is now handled by ape_trad_save.
*
* Revision 3.7  1999/02/02 17:35:31  toliver
* revised internal write statements to eliminate possibility of overflow
*
c Revision 3.6  1997/03/26  13:26:16  oneel
c updated so that default values can't have " in them
c
* Revision 3.5.1.1  1996/04/16 01:39:22  dunfee
* Start of pristine ftools CVS...
*
c Revision 1.6  1995/12/06  16:22:51  oneel
c changed some print * to xwrite
c
c Revision 1.5  1995/06/14  18:03:52  oneel
c Removed references to TBLPEXTRA
c
c Revision 1.4  1995/06/13  13:56:54  oneel
c Saved only the first character of tblptype, so pset pars weren't saved
c correctly.
c
c Revision 1.3  1995/06/02  19:53:45  oneel
c added some debugging
c
c Revision 1.2  1995/06/02  14:09:42  oneel
c Multiple parameter file changes
c
*
* Saves a parameter file
*
*
      SUBROUTINE TBSVPR(Filename,parnum,Ierr)
 
* filename is the file to save it in
* ierr is 0 if everything is ok
*
      IMPLICIT NONE
 
      INTEGER i
      INTEGER parnum
      INTEGER APE_TRAD_SET_STRING
      INTEGER APE_TRAD_SAVE
      INTEGER Ierr
      INTEGER LENACT
      CHARACTER*(*) Filename
 
      character(1000) outline
 
      INCLUDE 'tbl.inc'
      INCLUDE 'yaccfor.inc'
 
      IF ( DEBug ) THEN
         outline = 'tbsvpr called with ' // Filename(1:LENACT(Filename))
         CALL XWRITE(outline,5)
      ENDIF
 
      Ierr = 0
 
      IF ( TBLpcnt.NE.TBLpcntorig ) THEN
         call xwrite(' This is odd....',5)
         write (outline,*)' Origional count: ' , TBLpcntorig
         call xwrite(outline,5)
         write (outline,*)' Current count: ' , TBLpcnt
         call xwrite(outline,5)
         IF ( DEBug ) THEN
            WRITE (outline,*,IOSTAT=Ierr) 'tblpcnt not tblpcntorig' , 
     &             TBLpcnt , TBLpcntorig
            CALL XWRITE(outline,5)
         ENDIF
      ENDIF
 
      IF ( Ierr.EQ.0 ) THEN
 
         Ierr = APE_TRAD_SAVE()
         IF ( Ierr.NE.0 ) GOTO 100

         IF ( DEBug ) THEN
            WRITE (outline,*,IOSTAT=Ierr)
     $           'tbsvpr exited successfull'
            CALL XWRITE(outline,5)
         ENDIF
 
         RETURN
      ENDIF
    
 100  CALL XERROR(' Problem saving parameter file',5)
      IF ( DEBug ) THEN
         WRITE (outline,*,IOSTAT=Ierr) 'tbsvpr exited successfull'
         CALL XWRITE(outline,5)
      ENDIF
      STOP
      END



      SUBROUTINE YSTCLAQ(Instr)
 
* remove the " from a string
 
      CHARACTER*(*) Instr
      INTEGER i
 
      DO 100 i = 1 , LEN(Instr)
         IF ( Instr(i:i).EQ. '"' ) Instr(i:i) = ' '
 100  CONTINUE
 
      RETURN
      END



