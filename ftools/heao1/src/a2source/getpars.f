C----------------------------------------------------------------------------
C Calls parameters for a2source 
C
C Author: Jesse S. Allen (Raytheon STX; HEASARC/GSFC/NASA)
C History:
C  Version 0.0  20 Mar 1997  First draft version
C          0.9  29 Jan 1998  Includes log and terminal chatter levels
C          1.0  28 Oct 1998  tidied up (L. Breedon)

      subroutine getpars(rastr, decstr, equinox, srcenam, dayfile,
     &     lchat, tchat, clobber, irafsts)

      implicit none

      integer irafsts, tchat, lchat
      logical clobber

      double precision equinox

      character*(*) rastr, decstr,srcenam
      character*(*) dayfile
      character(80) message


      irafsts = 0

      call uclgsd('equinox', equinox, irafsts)
      if (irafsts .ne. 0) then
         message = 'Error reading EQUINOX'
         call xaerror(message,1)
         return
      endif 

      call uclgst('ra', rastr, irafsts)
      if (irafsts .ne. 0) then
         message = 'Error reading RA'
         call xaerror(message,1)
         return
      endif 

      call uclgst('dec', decstr, irafsts)
      if (irafsts .ne. 0) then
         message = 'Error reading DEC' 
         call xaerror(message,1)
         return
      endif 

      call uclgst('srcname', srcenam, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get SRCNAME parameter'
         call xaerror(message,1)
         return
      endif

      call uclgst('dayfile', dayfile, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get DAYFILE parameter'
         call xaerror(message,1)
         return
      endif

      call uclgsi('lchat', lchat, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get LCHAT parameter'
         call xaerror(message,1)
         return
      endif

      call uclgsi('tchat', tchat, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get TCHAT parameter'
         call xaerror(message,1)
         return
      endif

      call uclgsb('clobber', clobber, irafsts)
      if (irafsts .ne. 0) then
         message = 'Unable to get CLOBBER parameter'
         call xaerror(message,1)
         return
      endif




      return

      end

