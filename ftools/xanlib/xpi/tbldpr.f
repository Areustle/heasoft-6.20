**==tbldpr.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* $Id: tbldpr.f,v 3.8 2013/05/21 19:08:47 irby Exp $
* $Log: tbldpr.f,v $
* Revision 3.8  2013/05/21 19:08:47  irby
* Change character*n to character(n) to silence warnings: "Obsolescent
* feature: Old-style character length".
*
* Revision 3.7  2011/07/13 20:23:47  irby
* Supersede XPI's file input function with that of APE: file open is
* now handled by a call to ape_trad_init from OpenDefaultPF, and
* parsing each parameter into the tblp* arrays is handled by the new
* routine apeparsepar (replaces xpiparsepar).
*
* Revision 3.6  1997/05/20 17:35:21  oneel
* Fixed a bug where the default value stored in the parameter file was
* way big (> 255 chars) and I'd put the length in a character(silly)
*
* Added path searching to tbldpriraf which seems to work like the SAO
* Host interface
*
* Revision 3.5.1.1  1996/04/16 01:39:21  dunfee
* Start of pristine ftools CVS...
*
c Revision 1.21  1995/06/30  12:39:46  oneel
c Removed a debuging print statement which was added to fix a problem
c with gtcom2 and tbldpr
c
c Revision 1.20  1995/06/21  13:51:06  oneel
c Forgot to include yaccfor.inc
c
c Revision 1.19  1995/06/20  14:26:53  oneel
c Removing a extra print statement
c
c Revision 1.18  1995/06/20  14:26:28  oneel
c Still more work on loading multiple par files
c
c Revision 1.17  1995/06/19  18:28:55  oneel
c still playing with pset par files
c
c Revision 1.16  1995/06/19  15:55:20  oneel
c Again, changing the start of the pset loop
c
c Revision 1.15  1995/06/19  13:26:46  oneel
c Fix the start of the loop to do pset variables
c
c Revision 1.14  1995/06/19  13:12:38  oneel
c Fix a looping problem
c
c Revision 1.13  1995/06/16  18:14:21  oneel
c Forgot to loop multiple times for tbldpr to catch multiple pset
c parameters.
c
c Revision 1.12  1995/06/15  20:04:10  oneel
c tblextrapar had the wrong sense, now set it to true if you want the
c gtcom parameters loaded
c
c Revision 1.11  1995/06/15  12:54:50  oneel
c added tblextrapar
c
c Revision 1.10  1995/06/14  18:09:27  oneel
c Don't set tblstandalone here, set it before you call tbldpr
c
c Revision 1.9  1995/06/14  18:07:36  oneel
c Made sure that tblstandalone was checked before adding gtcom2
c parameters
c
c Revision 1.8  1995/06/13  21:03:34  oneel
c Forgot .par in tbldpr when it finds a pset parameter
c
c Revision 1.7  1995/06/08  14:11:09  oneel
c Load multiple parfiles which have a type of pset
c
c Revision 1.6  1995/06/06  14:39:09  oneel
c Added tbldpr_reset to reset tbldpr
c Renamed old tbldpr to tbldpr_doit to allow tbldpr to step through pset
c pars
c
c Revision 1.5  1995/06/05  11:25:25  oneel
c Make sure you add the parameter file number to the parameter table
c
c Revision 1.4  1995/06/02  19:51:07  oneel
c The builtin parameters now have a par file number of 0
c
c Revision 1.3  1995/06/02  14:05:48  oneel
c Add changes for loading multiple parameter files
c
c Revision 1.2  1995/05/30  19:57:11  oneel
c Ron Zellar's ihf changes
c
* Loads the parameter table for a command driven program.
      SUBROUTINE TBLDPR(File_parameter,Ierr)
 
*
      CHARACTER*(*) File_parameter
      INTEGER*4 Ierr
      character(255) new_pfbase, new_pfname
      integer lenact, i
      logical found
      integer numtodo, start
      INCLUDE 'tbl.inc'
      include 'yaccfor.inc'

      call tbldpr_doit(file_parameter,ierr)
*
* Now do pset variables
*
      start = 1
 10   continue
      found = .false.
      numtodo = tblpcnt
      do i=start,numtodo
         if (tblptype(i) .eq. 'pset') then
            found = .true.
            new_pfbase = tblpname(i)(1:lenact(tblpname(i)))
     $           //'.par'
            call tbfdpriraf(new_pfbase,new_pfname,ierr)
            if (debug) then
               call xwrite (' Loading '//
     $              new_pfname(1:lenact(new_pfname)),5)
            end if
            call tbldpr_doit(new_pfname,ierr)
         end if
      end do
      start = numtodo+1
      if (found) goto 10
      return
      end

      SUBROUTINE TBLDPR_doit(File_parameter,Ierr)
 
*
      CHARACTER*(*) File_parameter
      INTEGER*4 Ierr
 
 
      INTEGER*4 lun
      INTEGER*4 i
      INTEGER*4 j
      character(1000) inline
      character(50) str1 
      character(1) tchar
      logical first
      INCLUDE 'tbl.inc'
      save first
      data first /.true./


      if (first) then
         tblpcpf = 1
         TBLpcnt = 0
         first = .false.
      else
         tblpcpf = tblpcpf + 1
      end if
 
      CALL TRLOG('XPIDEBUG',8,str1,i)
      IF ( i.NE.0 ) THEN
         IF ( str1(1:1).EQ.'0' ) THEN
            CALL YDEBUG(.FALSE.)
         ELSE
            CALL YDEBUG(.TRUE.)
         ENDIF
      ENDIF
 
      TBLstandalone = .FALSE.
      TBLstandalonecmd = ' '
 
      CALL GETLUN(lun)
 
      TBLpfname(tblpcpf)  = File_parameter
 
      Ierr = 0
 
         DO WHILE ( Ierr.EQ.0 )
               TBLpcnt = TBLpcnt + 1
               IF ( TBLpcnt.GT.TBLPMAX ) THEN
                  CALL XWRITE(' parameter table too small',5)
                  TBLpcnt = TBLPMAX
                  RETURN
               ENDIF
 
               i = 1
               j = 1
               TBLpname(TBLpcnt) = ' '
               TBLpdesc(TBLpcnt) = ' '
               TBLptype(TBLpcnt) = ' '
               TBLpminp(TBLpcnt) = ' '
               TBLpmaxp(TBLpcnt) = ' '
               TBLpdefl(TBLpcnt) = ' '
               TBLpupd(TBLpcnt) = ' '
               TBLpupda(TBLpcnt) = ' '

               TBLpname(TBLpcnt)(1:1) = char(len(tblpname(tblpcnt)))
               TBLpdesc(TBLpcnt)(1:1) = char(len(tblpdesc(tblpcnt)))
               TBLptype(TBLpcnt)(1:1) = char(len(tblptype(tblpcnt)))
               TBLpminp(TBLpcnt)(1:1) = char(len(tblpminp(tblpcnt)))
               TBLpmaxp(TBLpcnt)(1:1) = char(len(tblpmaxp(tblpcnt)))
               TBLpdefl(TBLpcnt)(1:1) =
     $              char(mod(len(tblpdefl(tblpcnt)),256))
               TBLpdefl(TBLpcnt)(2:2) =
     $              char(len(tblpdefl(tblpcnt))/256)
               TBLpupda(TBLpcnt)(1:1) = char(len(tblpupda(tblpcnt)))
               
C Call APE routine to get next parameter and parse it into the tblp* arrays:

               call apeparsepar (tblpname(tblpcnt),
     $              tblptype(tblpcnt),
     $              tblpupda(tblpcnt),
     $              tblpdefl(tblpcnt),
     $              tblpminp(tblpcnt),
     $              tblpmaxp(tblpcnt),
     $              tblpdesc(tblpcnt), ierr)
               if (ierr .eq. 0) then
                  CALL YSTCLS1(TBLpname(TBLpcnt))
                  CALL YSTCLS1(TBLpdesc(TBLpcnt))
                  CALL YSTCLS1(TBLptype(TBLpcnt))
                  CALL YSTCLS1(TBLpminp(TBLpcnt))
                  CALL YSTCLS1(TBLpmaxp(TBLpcnt))
                  CALL YSTCLS1(TBLpdefl(TBLpcnt))
                  CALL YSTCLS1(TBLpupda(TBLpcnt))
                  CALL YSTCLQ1(TBLpname(TBLpcnt))
                  CALL YSTCLQ1(TBLpdesc(TBLpcnt))
                  CALL YSTCLQ1(TBLptype(TBLpcnt))
                  CALL YSTCLQ1(TBLpminp(TBLpcnt))
                  CALL YSTCLQ1(TBLpmaxp(TBLpcnt))
                  CALL YSTCLQ1(TBLpdefl(TBLpcnt))
                  CALL YSTCLQ1(TBLpupda(TBLpcnt))
                  tblpupd(tblpcnt) = tblpupda(tblpcnt)
                  tblppfnum(tblpcnt) = tblpcpf
               endif
 
         ENDDO
         Ierr = 0
C        Reduce TBLpcnt since loop was called one extra time before
C        apeparsepar returned a non-zero status:
         if (TBLpcnt .gt. 0) then
            TBLpcnt = TBLpcnt - 1
         endif
 
 
      CLOSE (lun)
      CALL FRELUN(lun)
 
* Now, see if we have a mode parameter
 
 
      IF ( TBLpcnt+TBLPEXTRA.GT.TBLPMAX ) THEN
         CALL XWRITE(' parameter table too small',5)
         TBLpcnt = TBLPMAX
         RETURN
      ENDIF
 
      if (tblextrapar) then


         TBLpname(TBLpcnt+1) = 'FULL'
         TBLpdesc(TBLpcnt+1) = 'Full? '
         TBLptype(TBLpcnt+1) = 'b'
         TBLpminp(TBLpcnt+1) = ' '
         TBLpmaxp(TBLpcnt+1) = ' '
         TBLpdefl(TBLpcnt+1) = 'no'
         TBLpupd(TBLpcnt+1) = ' '
         TBLpupda(TBLpcnt+1) = 'h'
         tblppfnum(tblpcnt+1) = 0
         TBLpname(TBLpcnt+2) = 'HIDDEN'
         TBLpdesc(TBLpcnt+2) = 'Hidden? '
         TBLptype(TBLpcnt+2) = 'b'
         TBLpminp(TBLpcnt+2) = ' '
         TBLpmaxp(TBLpcnt+2) = ' '
         TBLpdefl(TBLpcnt+2) = 'no'
         TBLpupd(TBLpcnt+2) = ' '
         TBLpupda(TBLpcnt+2) = 'h'
         tblppfnum(tblpcnt+2) = 0
         TBLpname(TBLpcnt+3) = 'name'
         TBLpdesc(TBLpcnt+3) = 'Name for help? '
         TBLptype(TBLpcnt+3) = 's'
         TBLpminp(TBLpcnt+3) = ' '
         TBLpmaxp(TBLpcnt+3) = ' '
         TBLpdefl(TBLpcnt+3) = ' '
         TBLpupd(TBLpcnt+3) = ' '
         TBLpupda(TBLpcnt+3) = 'h'
         tblppfnum(tblpcnt+3) = 0
         TBLpname(TBLpcnt+4) = 'aname'
         TBLpdesc(TBLpcnt+4) = 'Name: '
         TBLptype(TBLpcnt+4) = 's'
         TBLpminp(TBLpcnt+4) = ' '
         TBLpmaxp(TBLpcnt+4) = ' '
         TBLpdefl(TBLpcnt+4) = ' '
         TBLpupd(TBLpcnt+4) = ' '
         TBLpupda(TBLpcnt+4) = 'q'
         tblppfnum(tblpcnt+4) = 0
         TBLpname(TBLpcnt+5) = 'acmd'
         TBLpdesc(TBLpcnt+5) = 'Command to alias: '
         TBLptype(TBLpcnt+5) = 's'
         TBLpminp(TBLpcnt+5) = ' '
         TBLpmaxp(TBLpcnt+5) = ' '
         TBLpdefl(TBLpcnt+5) = ' '
         TBLpupd(TBLpcnt+5) = ' '
         TBLpupda(TBLpcnt+5) = 'q'
         tblppfnum(tblpcnt+5) = 0
         TBLpname(TBLpcnt+6) = 'recallwhat'
         TBLpdesc(TBLpcnt+6) = 'Command number to recall: '
         TBLptype(TBLpcnt+6) = 'i'
         TBLpminp(TBLpcnt+6) = ' '
         TBLpmaxp(TBLpcnt+6) = ' '
         TBLpdefl(TBLpcnt+6) = '-99'
         TBLpupd(TBLpcnt+6) = ' '
         TBLpupda(TBLpcnt+6) = 'h'
         tblppfnum(tblpcnt+6) = 0
         TBLpname(TBLpcnt+7) = 'xpifname'
         TBLpdesc(TBLpcnt+7) = 'File name: '
         TBLptype(TBLpcnt+7) = 's'
         TBLpminp(TBLpcnt+7) = ' '
         TBLpmaxp(TBLpcnt+7) = ' '
         TBLpdefl(TBLpcnt+7) = ' '
         TBLpupd(TBLpcnt+7) = ' '
         TBLpupda(TBLpcnt+7) = 'q'
         tblppfnum(tblpcnt+7) = 0
         TBLpname(TBLpcnt+8) = 'show'
         TBLpdesc(TBLpcnt+8) = 'show boolean '
         TBLptype(TBLpcnt+8) = 'b'
         TBLpminp(TBLpcnt+8) = ' '
         TBLpmaxp(TBLpcnt+8) = ' '
         TBLpdefl(TBLpcnt+8) = ' '
         TBLpupd(TBLpcnt+8) = ' '
         TBLpupda(TBLpcnt+8) = 'h'
         tblppfnum(tblpcnt+8) = 0
         TBLpname(TBLpcnt+9) = 'all'
         TBLpdesc(TBLpcnt+9) = 'All boolean: '
         TBLptype(TBLpcnt+9) = 'b'
         TBLpminp(TBLpcnt+9) = ' '
         TBLpmaxp(TBLpcnt+9) = ' '
         TBLpdefl(TBLpcnt+9) = ' '
         TBLpupd(TBLpcnt+9) = ' '
         TBLpupda(TBLpcnt+9) = 'h'
         tblppfnum(tblpcnt+9) = 0
         TBLpname(TBLpcnt+10) = 'delete'
         TBLpdesc(TBLpcnt+10) = 'Delete boolean '
         TBLptype(TBLpcnt+10) = 'b'
         TBLpminp(TBLpcnt+10) = ' '
         TBLpmaxp(TBLpcnt+10) = ' '
         TBLpdefl(TBLpcnt+10) = ' '
         TBLpupd(TBLpcnt+10) = ' '
         TBLpupda(TBLpcnt+10) = 'h'
         tblppfnum(tblpcnt+10) = 0
         TBLpname(TBLpcnt+11) = 'system'
         TBLpdesc(TBLpcnt+11) = 'System boolean '
         TBLptype(TBLpcnt+11) = 'b'
         TBLpminp(TBLpcnt+11) = ' '
         TBLpmaxp(TBLpcnt+11) = ' '
         TBLpdefl(TBLpcnt+11) = ' '
         TBLpupd(TBLpcnt+11) = ' '
         TBLpupda(TBLpcnt+11) = 'h'
         tblppfnum(tblpcnt+11) = 0
         TBLpname(TBLpcnt+12) = 'previous'
         TBLpdesc(TBLpcnt+12) = 'previous boolean '
         TBLptype(TBLpcnt+12) = 'b'
         TBLpminp(TBLpcnt+12) = ' '
         TBLpmaxp(TBLpcnt+12) = ' '
         TBLpdefl(TBLpcnt+12) = ' '
         TBLpupd(TBLpcnt+12) = ' '
         TBLpupda(TBLpcnt+12) = 'h'
         tblppfnum(tblpcnt+12) = 0
         TBLpname(TBLpcnt+13) = 'new'
         TBLpdesc(TBLpcnt+13) = 'New boolean '
         TBLptype(TBLpcnt+13) = 'b'
         TBLpminp(TBLpcnt+13) = ' '
         TBLpmaxp(TBLpcnt+13) = ' '
         TBLpdefl(TBLpcnt+13) = ' '
         TBLpupd(TBLpcnt+13) = ' '
         TBLpupda(TBLpcnt+13) = 'h'
         tblppfnum(tblpcnt+13) = 0
         TBLpname(TBLpcnt+14) = 'helptopic'
         TBLpdesc(TBLpcnt+14) = 'help topic '
         TBLptype(TBLpcnt+14) = 's'
         TBLpminp(TBLpcnt+14) = ' '
         TBLpmaxp(TBLpcnt+14) = ' '
         TBLpdefl(TBLpcnt+14) = ' '
         TBLpupd(TBLpcnt+14) = ' '
         TBLpupda(TBLpcnt+14) = 'h'

         TBLpcnt = TBLPEXTRA + TBLpcnt

      end if
 
      TBLpcntorig = TBLpcnt
 
 
      RETURN
99001 FORMAT (' Error opening file ',a,' no ',i4)


      entry tbldpr_reset
*
* Resets tbldpr
*

      tblpcpf = 1
      TBLpcnt = 0
      first = .false.
      return

      end
      


      subroutine apply_mode

      integer i
      character(100) str2
      INCLUDE 'tbl.inc'
      include 'yaccfor.inc'

      i = 0
      call uclgst ('mode',str2,i)
      if (i .ne. 0) return

      DO 200 i = 1 , TBLpcnt
         IF ( INDEX(TBLpupda(i),'a').NE.0 ) TBLpupd(i) = str2
         IF ( TBLpupd(i).EQ.' ' ) TBLpupd(i) = TBLpupda(i)
 200  CONTINUE
      if (debug) then
         call dumppar
      end if
      return

      end
