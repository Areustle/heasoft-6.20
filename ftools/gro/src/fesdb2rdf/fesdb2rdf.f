C**********************************************************************
C FTOOLS TASK:
C      fesdb2rdf
C
C FILE:
C      fesdb2rdf.f
C
C DESCRIPTION:
C      This routine is to evolve into an Ftool for converting EGRET
C      QVP and Exposure files into the current standard FITS format.
C  
C      The Task requires a few minor changes to some of the keywords,
C      Combination of columns into a single column (TIME) and the 
C      addition of some keywords that are not present in the original.
C
C AUTHOR:
C      Mark Cresitello-Dittmar       24-March 1994
C
C MODIFICATION HISTORY:
C      01-Aug-94   Changed value of EXTNAME from EVENT to EVENTS 
C                  to conform to an OGIP requirement.
C      01-Jul-98   Deleted the two digit year interpretation.   
C                  (Taken care by the the fidate).
C                  updated for the new format of date keyword.
C      23-Jul-98   Added check for the old date format before 
C                  write out the datexxx keywords.  Ning Gan
C                         
C                  
C
C NOTES:
C
C ARGUMENTS:
C      none
C
C PRIMARY LOCAL VARIABLES:
C      qvpfile     Input QVP FITS file
C      expfile     Input EXP FITS file (optional)
C      rdffile     Output RDF FITS file
C      exp         Flag indicating if exposure file is provided
C      status      Status flag
C
C CALLED ROUTINES:
C      subroutine gsdb2rdf  - gets parameters from par file
C      subroutine fcerr     - write error message 
C      subroutine docnvt    - convert file(s) to RDF format
C
C**********************************************************************

       subroutine fesdbf

       implicit none
C
       character*(180) qvpfile, expfile, rdffile
       character*(80) errtxt
       logical exp
       integer status
C
       character*(40) taskname
       common /task/ taskname
C
       external fcerr
C
C
       taskname = 'fesdb2rdf v1.0'
       status = 0
C
C  Get run parameters. All calls to Parameter files are to be made
C  from this routine.

       call gsdb2rdf(qvpfile, expfile, rdffile, exp, status)
       if (status .ne. 0) then
          write(errtxt,'(''Error from gsdb2rdf'')')
          call fcerr(errtxt)
       endif
C
C  Pass Parameters to meat routine.  All FITSIO and Data manipulation
C  is done through this routine.
C
       call docnvt(qvpfile, expfile, rdffile, exp, status)
       if (status .ne. 0) then
         write(errtxt,'('' Error from docnvt'')')
         call fcerr(errtxt)
       endif
C
C  Exit  routine with a smile on your face... well done
C
       end

C**********************************************************************
C SUBROUTINE:
C      gsdb2rdf
C
C DESCRIPTION:
C      Get run parameters from parameter file
C
C AUTHOR:
C      Mark Cresitello-Dittmar       24-March 1994
C
C MODIFICATION HISTORY:
C      
C NOTES:
C      uses F77/VOS like calls to read parameters from .par file
C
C USAGE:
C      call gsdb2rdf(qvpfile,expfile,rdffile, exp, status)
C
C ARGUMENTS:
C     qvpfile      Input QVP FITS file.
C     expfile      Input exposure history (EXP) Fits file (optional)
C     rdffile      Output RDF FITS file 
C     exp          Flag indicating exposure history provided
C     status       Status flag
C     
C PRIMARY LOCAL VARIABLES:
C     errtxt       error message
C
C CALLED ROUTINES:
C     subroutine fcerr     - write error message
C     subroutine uclgs_    - gets parameters from par file
C
C**********************************************************************
       subroutine gsdb2rdf(qvpfile,expfile,rdffile, exp, status)
C
       implicit none
C
       character*(*) qvpfile, expfile, rdffile
       logical exp
       integer  status
C
       character*(80) errtxt
C
C
C  get the name of the input QVP file.
       call uclgst('qvpfile',qvpfile,status)
       if (status .ne. 0) then
          errtxt = 'could not get input QVP file parameter'
          call fcerr(errtxt)
          goto 999
       endif
C
C  get the name of the input EXP file.
       call uclgst('expfile',expfile,status)
       if (status .ne. 0) then
          errtxt = 'could not get input EXP file parameter'
          call fcerr(errtxt)
          goto 999
       endif
C
C  get the name of the output RDF file.
       call uclgst('rdffile',rdffile,status)
       if (status .ne. 0) then
          errtxt = 'could not get input RDF file parameter'
          call fcerr(errtxt)
          goto 999
       endif
C
C  get flag indicating that an exposure file is provided.
       call uclgsb('exp', exp, status)
       if (status .ne. 0) then
          errtxt = 'could not get exposure file flag parameter'
          call fcerr(errtxt)
          goto 999
       else
          exp = .true.
          if ((expfile .eq. ' ').or.(expfile .eq. '-')) exp = .false.
       endif


 999   continue
       return
       end


C**********************************************************************
C SUBROUTINE:
C      gobkwd
C
C DESCRIPTION:
C      This subroutine writes the observation description keywords to
C      the header of the file pointed to by luout.  The values are 
C      obtained through the primary header of the QVP file.  Keyword 
C      and comment changes are made to many of the items during the 
C      transfer.
C  
C AUTHOR:
C      Mark Cresitello-Dittmar              March 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C
C ARGUMENTS:
C      luqvp        Unit number of source  QVP file
C      luout        Unit number of target file
C      status       status flag
C
C PRIMARY LOCAL VARIABLES;
C
C      oldhdu       current hdu number
C      hdutype      header type
C      intval       variable to hold integer value fields
C      fvalue       variable to hold real value fields
C      svalue       variable to hold string value fields
C      card         holds a single reader card record
C      comnt        holds header comment field (columns 34-80)
C      errtxt       string for error output
C      
C CALLED ROUTINES:
C      subroutine fcerr     - write error message
C      subroutine ft____    - FITSIO routines
C      subroutine gobkwd    - defines observation keywords
C      subroutine trnscol   - transfer data column
C
C**********************************************************************
       subroutine gobkwd(luqvp,luout,status)
C
       implicit none
C
       integer luqvp, luout
       integer status

       character*(20) svalue
       character*(60) comnt
       character(68)   dt_str
       character*(50) blnk50
       character*(80) blnk80
       character*(80) card
       character*(80) errtxt
       integer i
       integer oldhdu, hdutype
       integer stday,stmon,styear
       integer enday,enmon,enyear
       integer sthour,stmin
       integer enhour,enmin
       integer intval
       real stsec,sttjd
       real ensec,entjd
       real    fvalue
       double precision dpsec,dptjd
C
       external ftghdn,
     +          ftmahd,
     +          fcerr,
     +          ftprec,
     +          ftgcrd,
     +          ftmcom,
     +          ftpkys,
     +          ftgkys,
     +          fidate,
     +          fiptim,
     +          ftgkye,
     +          ftpkyf,
     +          int2mjd,
     +          ftpkye,
     +          ftpkyl,
     +          ftpcom,
     +          ftgkyj,
     +          ftpkyj
C
C----------------------------------------------------------------------
C  initialize blank character strings
C
C  The fits standard is for the keyword to have 8 characters
C with no embedded blanks.  An entire line of blanks violates this
C but having some spacing does make the file more readable, so I 
C have changed the blanks to the word COMMENT.
C The variable blnk50 is used with a proper keyword call so it 
C will still be filled with spaces. 
C
C  Jeff Silvis RSTX 
C  Jan 1998
C
       do 20 i = 1,80
         if (i .le. 50) blnk50(i:i) = ' '
C         blnk80(i:i) = ' '
 20    continue
        blnk80 = 'COMMENT' 

C
C  get current hdu number before moving to primary.
       call ftghdn(luqvp, oldhdu)
C
C  move to primary hdu to transfer observation keywords if necessary
       i = 1
       call ftmahd(luqvp, i, hdutype, status)
       if (status .ne. 0 ) then
         write(errtxt,'('' Error moving to primary header'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  Comment text describing the file
       card = 
     +  'COMMENT *****************************************************'
        call ftprec(luout,card,status)
       card = 
     +  'This FITS file contains an EGRET photon list and       '
        call ftpcom(luout,card,status)
       card = 
     +  'optionally and extension containing information about  '
        call ftpcom(luout,card,status)
       card = 
     +  'temporal changes in satellite status. The photon data  '
        call ftpcom(luout,card,status)
       card = 
     +  'are essentially as described in Mattox, et al., The    '
        call ftpcom(luout,card,status)
       card = 
     +  'EGRET data products, in The Compton observatory Science'
        call ftpcom(luout,card,status)
       card = 
     +  'Workshop, NASA Conference publication 3137, 1992, p126.'
        call ftpcom(luout,card,status)
       card = 
     +  'Time columns have been combined to a single            '
        call ftpcom(luout,card,status)
       card = 
     +  'column giving the time in seconds since MJD 40000.     '
        call ftpcom(luout,card,status)
       card = 
     +  'COMMENT *****************************************************'
        call ftprec(luout,card,status)

       call ftprec(luout,blnk80,status)
C
C  Comments and keywords for instrument configuration
       card = 'COMMENT  *** Observation description keywords ***    '
        call ftprec(luout,card,status)
C
        call ftprec(luout,blnk80,status)
C
C  check error status
       if (status .ne.0) then
         write(errtxt,'('' Error '',i4,'' writing comment text'')') 
     +      status
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** Instrument configuration
C
       card = 'COMMENT  *** Instrument configuration keywords       '
       call ftprec(luout,card,status)
C
       call ftprec(luout,blnk80,status)
C
       call ftgcrd(luqvp,'TELESCOP',card,status)
       call ftprec(luout,card,status)
       comnt = 'Observatory/satellite used                     '
       call ftmcom(luout,'TELESCOP',comnt,status)
C
       call ftgcrd(luqvp,'INSTRUME',card,status)
       call ftprec(luout,card,status)
       comnt = 'Instrument                                     '
       call ftmcom(luout,'INSTRUME',comnt,status)
C
       call ftgcrd(luqvp,'OBSERVER',card,status)
       call ftprec(luout,card,status)
       comnt = 'PI associated with observation                 '
       call ftmcom(luout,'OBSERVER',comnt,status)
C
       comnt = 'Standard observation mode                      '
       call ftpkys(luout,'OBS_MODE','POINTING',comnt,status)
C
       call ftprec(luout,blnk80,status)
C
C  check status
       if (status .ne. 0) then 
         write(errtxt,'(''Error writing Instrument configurations'')') 
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** Target/observation identification
C
       card = 'COMMENT  *** Target/observation identification keywords'
       call ftprec(luout,card,status)
C
       call ftprec(luout,blnk80,status)
C
       call ftgcrd(luqvp,'GROVPN  ', card, status)
       call ftprec(luout,card,status)
       comnt = 'GRO Viewing period number                      '
       call ftmcom(luout,'GROVPN',comnt,status)
C
       call ftgkys(luqvp,'EGTVPN  ', svalue, comnt, status)
       comnt = 'EGRET Viewing period number                    '
       call ftpkys(luout,'EGRETVPN',svalue,comnt,status)
C
       call ftprec(luout, blnk80, status)
C
C  check status
       if (status .ne. 0) then 
         write(errtxt,'(''Error writing Target/obs identifications'')') 
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** Timing data 
C
       card = 'COMMENT  *** Timing keywords                           '
       call ftprec(luout,card,status)
C
       call ftprec(luout,blnk80,status)

       call ftgkys(luqvp,'DATE-OBS', dt_str, comnt, status)
c       comnt = 'Date of observation start: dd/mm/yy            '
C
C   parse date string into components for conversion to TJD
         call fidate(dt_str,stday,stmon,styear,status)
C
C   check that year has the appropriate 4 digits
C         if (styear .lt. 50) then
C           styear = styear + 2000
C         else if (styear .lt. 100) then
C           styear = styear + 1900
C         endif
C
C   convert the old format to new format.
C       if(index(dt_str,'/').ne.0) then 
C	   status = 0
	   call ftdt2s(styear,stmon,stday,dt_str,status)
C	   status = 0
C       endif
       comnt = 'Date of observation end: yyyy-mm-dd   ' 
       call ftpkys(luout,'DATE-OBS', dt_str, comnt, status)

       call ftgkys(luqvp,'TIME-OBS', dt_str, comnt, status)
       comnt = 'Time of observation start: hh:mm:ss.fff        '
       call ftpkys(luout,'TIME-OBS', dt_str, comnt, status)
C
C   parse time string into components for conversion to TJD
         call fiptim(dt_str,sthour,stmin,stsec,status)
C

       call ftgkys(luqvp,'DATE-END', dt_str, comnt, status)
C
C   parse date string into components for conversion to TJD
         call fidate(dt_str,enday,enmon,enyear,status)
C   convert the old format to new format.
       if(index(dt_str,'/').ne.0) then 
	   call ftdt2s(enyear,enmon,enday,dt_str,status)
       endif
       comnt = 'Date of observation end: yyyy-mm-dd            '
       call ftpkys(luout,'DATE-END', dt_str, comnt, status)
C
C   check that year has the appropriate 4 digits
C         if (enyear .lt. 50) then
C           enyear = enyear + 2000
C         else if (enyear .lt. 100) then
C           enyear = enyear + 1900
C         endif
C
       call ftgkys(luqvp,'TIME-END', dt_str, comnt, status)
       comnt = 'Time of observation end: hh:mm:ss.fff          '
       call ftpkys(luout,'TIME-END', dt_str, comnt, status)
C
C   parse time string into components for conversion to TJD
         call fiptim(dt_str,enhour,enmin,ensec,status)
C
       call ftprec(luout, blnk80, status)
C
       call ftgkye(luqvp,'STRT-DAY', fvalue, comnt, status)
       comnt = 'Year.ddd of start of obs                      '
       call ftpkyf(luout,'STRT_DAY', fvalue, 3, comnt, status)

       call ftgkye(luqvp,'STRT-TIM', fvalue, comnt, status)
       comnt = 'Seconds of day of start of obs                '
       call ftpkyf(luout,'STRT_TIM', fvalue, 3, comnt, status)

       call ftgkye(luqvp,'END-DAY ', fvalue, comnt, status)
       comnt = 'Year.ddd of end of obs                        '
       call ftpkyf(luout,'END_DAY ', fvalue, 3, comnt, status)

       call ftgkye(luqvp,'END-TIM ', fvalue, comnt, status)
       comnt = 'Seconds of day of end of obs                  '
       call ftpkyf(luout,'END_TIM ', fvalue, 3, comnt, status)

       call ftgkye(luqvp,'OBS-TIME', fvalue, comnt, status)
       comnt = 'Total of time in all modes (s)                '
       call ftpkyf(luout,'ONTIME  ', fvalue, 3, comnt, status)

       call ftprec(luout, blnk80, status)
C
C   add Timesystem keyword
       comnt = 'Time system for following keywords             '
       call ftpkys(luout,'TIMESYS ','TJD     ', comnt, status)
C
C   add Timeunit keyword
       comnt = 'Units of following keywords                    '
       call ftpkys(luout,'TIMEUNIT','d       ', comnt, status)
C
C   add Timezero keyword
       comnt = 'Offset of times from MJD 0 (days)              '
       fvalue = 0.0
       call ftpkyf(luout,'TIMEZERO', fvalue, 4, comnt, status)
C
       dpsec = dble(stsec)
       call int2mjd(styear,stmon,stday,sthour,stmin,dpsec,dptjd,status)
       sttjd = sngl(dptjd)
       sttjd = sttjd - 40000.0
       comnt = 'Truncated JD of start of obs                   '
       call ftpkyf(luout,'TSTART  ', sttjd, 4, comnt, status)
C
C   calculate end time as TJD date
       dpsec = dble(ensec)
       call int2mjd(enyear,enmon,enday,enhour,enmin,dpsec,dptjd,status)
       entjd = sngl(dptjd)
       entjd = entjd - 40000.0
       comnt = 'Truncated JD of end of obs                     '
       call ftpkyf(luout,'TEND    ', entjd, 4, comnt, status)
C
C   add smallest time interval
       fvalue = 1.1574074E-11
       comnt = 'Smallest time increment possible (days)        '
       call ftpkye(luout,'TIMEDEL ', fvalue, 7, comnt, status)
C
C   add Tassign keyword
       comnt = 'Times use satellite clock                      '
       call ftpkys(luout,'TASSIGN ','SATELLITE', comnt, status)
C
C   add Clockapp comment and keyword
       comnt = 'No correction to spacecraft time               '
       call ftpkyl(luout,'CLOCKAPP', 'F', comnt, status)
       call ftmcom(luout,'CLOCKAPP', comnt, status)
       call ftprec(luout, blnk80, status)
C
       card = 
     +  'NOTE that the spacecraft clock is periodically adjusted from'
       call ftpcom(luout, card, status)
C
       card = 
     +  'the ground so that no clock correction should be necessary'
       call ftpcom(luout, card, status)
C
       call ftprec(luout, blnk80, status)
       call ftprec(luout, blnk80, status)
C
C  check status
       if (status .ne. 0) then 
         write(errtxt,'(''Error writing Timing keywords'')') 
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** Positional keywords
C
       card = '*** Positional keywords'
       call ftpcom(luout, card, status)
C
       call ftprec(luout, blnk80, status)
C
       call ftgkyj(luqvp,'EQUINOX ', intval, comnt, status)
       comnt = 'Equinox of coordinate system                   '
       call ftpkyj(luout,'EQUINOX ', intval, comnt, status)
C
       call ftprec(luout, blnk80, status)
C
       call ftgkye(luqvp,'SC-Z-RA ', fvalue, comnt, status)
       comnt = 'Spacecraft Z axis                              '
       call ftpkyf(luout,'RA_SCZ  ', fvalue, 3, comnt, status)
C
       call ftgkye(luqvp,'SC-Z-DEC', fvalue, comnt, status)
       call ftpkyf(luout,'DEC_SCZ ', fvalue, 3, blnk50, status)
C
       call ftgkye(luqvp,'SC-X-RA ', fvalue, comnt, status)
       comnt = 'Spacecraft X axis                              '
       call ftpkyf(luout,'RA_SCX  ', fvalue, 3, comnt, status)
C
       call ftgkye(luqvp,'SC-X-DEC', fvalue, comnt, status)
       call ftpkyf(luout,'DEC_SCX ', fvalue, 3, blnk50, status)
C
       call ftprec(luout, blnk80, status)
C
       call ftgkye(luqvp,'SC-Z-LII', fvalue, comnt, status)
       comnt = 'Spacecraft Z axis                              '
       call ftpkyf(luout,'GLON_SCZ', fvalue, 3, comnt,status)
C
       call ftgkye(luqvp,'SC-Z-BII', fvalue, comnt, status)
       call ftpkyf(luout,'GLAT_SCZ', fvalue, 3, blnk50, status)
C
       call ftgkye(luqvp,'SC-X-LII', fvalue, comnt, status)
       comnt = 'Spacecraft X axis                              '
       call ftpkyf(luout,'GLON_SCX  ',fvalue, 3, comnt, status)
C
       call ftgkye(luqvp,'SC-X-BII', fvalue, comnt, status)
       call ftpkyf(luout,'GLAT_SCX',fvalue, 3, blnk50, status)
C
       call ftprec(luout, blnk80, status)
C
       call ftgkye(luqvp,'TARG-RA ', fvalue, comnt, status)
       comnt = 'RA of center of FOV                            '
       call ftpkyf(luout,'RA_PNT  ',fvalue, 3, comnt, status)
C
       call ftgkye(luqvp,'TARG-DEC', fvalue, comnt, status)
       comnt = 'Dec of canter of FOV                           '
       call ftpkyf(luout,'DEC_PNT ',fvalue, 3, comnt, status)
C
       call ftprec(luout, blnk80, status)
C
       call ftgkye(luqvp,'SC-Z-LII', fvalue, comnt, status)
       comnt = 'L of center of FOV                             '
       call ftpkyf(luout,'GLON_PNT', fvalue, 3, comnt,status)
C
       call ftgkye(luqvp,'SC-Z-BII', fvalue, comnt, status)
       comnt = 'B of center of FOV                             '
       call ftpkyf(luout,'GLAT_PNT', fvalue, 3, comnt, status)
C
       call ftprec(luout, blnk80, status)
C
       call ftgkye(luqvp,'RADIUS  ', fvalue, comnt, status)
       if (status .eq. 202) then
         status = 0
         call ftgkye(luqvp,'MAXANG  ', fvalue, comnt, status)
       endif
       comnt = 'Nominal radius of FOV in degrees               '
       call ftpkyf(luout,'RADIUS  ', fvalue, 3, comnt, status)
C
       call ftprec(luout, blnk80, status)
C
C  check status
       if (status .ne. 0) then 
         write(errtxt,'(''Error writing Posititional keywords'')') 
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** Processing keywords 

       card = '*** Processing keywords'
C
       call ftprec(luout, blnk80, status)
C
       comnt = 'Type of file                                   '
       call ftpkys(luout,'FILETYPE','EGRET_RDF', comnt, status)
C
       comnt = 'Institution defining standard                  '
       call ftpkys(luout,'HDUCLAS','OGIP    ', comnt, status)
C
       call ftprec(luout, blnk80, status)
C
       card =  'Following parameters set during generation of EGRET SDB'
       call ftpcom(luout, card, status)
C
       call ftgkye(luqvp,'MINENG  ', fvalue, comnt, status)
       comnt = 'Minimum energy of photons included             '
       call ftpkyf(luout,'E_MIN   ', fvalue, 3,comnt, status)
C
       call ftgkye(luqvp,'MAXENG  ', fvalue, comnt, status)
       comnt = 'Maximum energy of photons included             '
       call ftpkyf(luout,'E_MAX   ', fvalue, 3, comnt, status)
C
       comnt = 'Units of energy keywords                       '
       call ftpkys(luout,'EUNIT   ','MeV     ', comnt, status)
C
       call ftgkye(luqvp,'MINENG  ', fvalue, comnt, status)
       comnt = 'Repeat E_MIN for EGRET software                '
       call ftpkyf(luout,'MINENG  ', fvalue, 3, comnt, status)
C
       call ftgkye(luqvp,'MAXENG  ', fvalue, comnt, status)
       comnt = 'Repeat E_MAX for EGRET software                '
       call ftpkyf(luout,'MAXENG  ', fvalue, 3, comnt, status)
C
C  transfer VARZEN keyword if present
       call ftgcrd(luqvp,'VARZEN  ', card, status)
       if (status .eq. 202) then
          status = 0
       else
         call ftprec(luout, card, status)
       endif
C
C  if ZENMAX keyword is not found, set to default setting
       call ftgkye(luqvp,'ZENMAX  ', fvalue, comnt, status)
       comnt = 'Maximum zenith angle in degrees                '
       if (status .eq. 202) then
         fvalue = 105.000
         status = 0
         comnt = 'DEFAULT Maximum zenith angle in degrees      '
       endif
       call ftpkyf(luout,'ZENMAX', fvalue, 3, comnt, status)
C
C  the following keywords will be included only if they exist in SDB file
C   VARANG,ACCANG, MINCON, MINANG, MAXANG 
C  transfer VARZEN keyword if present
       call ftgcrd(luqvp,'VARANG  ', card, status)
       if (status .eq. 202) then
          status = 0
       else
         call ftprec(luout, card, status)
       endif
C
       call ftgkye(luqvp,'ACCANG  ', fvalue, comnt, status)
       if (status .eq. 202) then
          status = 0
       else
         call ftpkyf(luout,'ACCANG  ', fvalue, 3, comnt, status)
       endif
C
       call ftgkye(luqvp,'MINCON  ', fvalue, comnt, status)
       if (status .eq. 202) then
          status = 0
       else
         call ftpkyf(luout,'MINCON  ', fvalue, 3, comnt, status)
       endif
C
       call ftgkye(luqvp,'MINANG  ', fvalue, comnt, status)
       if (status .eq. 202) then
          status = 0
       else
         call ftpkyf(luout,'MINANG  ', fvalue, 3, comnt, status)
       endif
C
       call ftgkye(luqvp,'MAXANG  ', fvalue, comnt, status)
       if (status .eq. 202) then
          status = 0
       else
         call ftpkyf(luout,'MAXANG  ', fvalue, 3, comnt, status)
       endif
C
       comnt = 'Creator of this file                           '
       call ftpkys(luout,'ORIGIN  ','GSFC/COSSC', comnt, status)
C
       call ftprec(luout, blnk80, status)
C
       comnt = 'FTOOL                                          '
       call ftpkys(luout,'CREATOR ','FESDB2RDF v1.0', comnt, status)
C
       call ftprec(luout, blnk80, status)
C
C  check status
       if (status .ne. 0) then 
         write(errtxt,'(''Error writing Processing keywords'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  return to original hdu of QVP file
       call ftmahd(luqvp, oldhdu, hdutype, status)
       if (status .ne. 0 ) then
         write(errtxt,'('' Error from ftmahd'')')
         call fcerr(errtxt)
         call fcerrm(status)
       endif
C
 999   continue
       return
       end

C**********************************************************************
C SUBROUTINE:
C      trnscol.f
C
C DESCRIPTION:
C      This subroutine is a variation on the FIMCOL ftool written
C      by James Kent Blackburn.  It performs the same basic
C      task.. transfers data from one file to another one column
C      at a time.  The differences are:
C        - will do tform - dattype conversion each time called
C        - brought dattype setting into second loop allows dattyp and
C          repeat to become single values instead of arrays
C        - added format type 'Z' for ascii table input which
C          transfers a hexinteger character to a straight integer output.
C        - added format type 'L' for ascii table input which
C          transfers A1 input to binary logical output
C        - added format type 'B' for ascii table input, which
C          transfers A1 input to binary byte output
C        - added format type 'R'  which transfers real (E) input 
C          column AND applies conversion from RADIANS to DEGREES
C
C AUTHOR:
C      Mark Cresitello-Dittmar              March 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C USAGE:
C      call trnscol(luin, luout, irow, orow, ncols, drows,
C     +             tform, colin, hdutype, status)
C
C ARGUMENTS:
C      luin         Unit number of source file
C      luout        Unit number of target file
C      irow         row of input file to begin reading from
C      orow         row of output file to begin writing at
C      ncols        number of columns
C      drows        number of rows in table
C      tform        formats of columns
C      colin        Map of columns from output to input files
C                   (index  = output column number)
C                   (values = input column number)
C      hdutype      header type 
C      status       status flag
C
C PRIMARY LOCAL VARIABLES;
C      lvalues      array of logical values
C      bvalues      array of byte string values
C      svalues      array of string values
C      ivalues      array of integer*2 values
C      jvalues      array of integer values
C      evalues      array of real values
C      dvalues      array of double values
C      cvalues      array of complex values
C      mvalues      array of double complex values
C      errtxt       error message
C      anyf         flag for undefined data value
C      
C CALLED ROUTINES:
C      subroutine fcerr     - write error message
C      subroutine fcasfm    - get ascii format
C      subroutine ftbnfm    - get binary format
C      subroutine ftgcvx    - get x type column values 
C      subroutine ftpclx    - put x type column values 
C      subroutine ftpclu    - put undefined column values 
C      subroutine ft____    - FITSIO routines
C
C**********************************************************************
       subroutine trnscol(luin, luout, irow, orow, ncols, drows,
     +                   tform, colin, hdutype, status)
C
       implicit none
C
       integer maxcl, maxsize
       parameter (maxcl = 999)
       parameter (maxsize = 1024)

       character*(8) tform(maxcl)
       integer luin, luout
       integer irow, orow, ifrow, ofrow
       integer ncols,drows 
       integer colin(maxcl)
       integer hdutype
       integer status


       integer i,j, felem, nelem
       integer dattyp, repeat, width, remain
       logical anyf
       real radeg
       parameter (radeg = 57.2957795)

       logical lvalues(maxsize)
       character*(1)    bvalues(maxsize)
       character*(1024) svalues(maxsize)
       integer*2 ivalues(maxsize)
       integer*2 nullval_space, nullval_zero
       integer   jvalues(maxsize), offset, maxrows
       real      evalues(maxsize),cvalues(2,maxsize)
       double precision  dvalues(maxsize),mvalues(2,maxsize)
C
C
       external fimcol,
     +          ftgcvs,
     +          ftpcli,
     +          fcerrm

       nullval_space = ICHAR (' ')
       nullval_zero = 0
       maxrows = 0

C
C----------------------------------------------------------------------
C  set dattype arrays from tform (adding new format)
       do 100 i = 1,ncols
C
         if ( hdutype .eq. 1 ) then
           call fcasfm(tform(i),dattyp,status)
           if (tform(i) .eq. 'B       ') then
             dattyp = 12
             status = 0
           elseif (tform(i) .eq. 'L       ') then
             dattyp = 13
             status = 0
           elseif (tform(i) .eq. 'Z       ') then
             dattyp = 29
             status = 0
           endif
           width = 1
           repeat = 1
         else if ( hdutype .eq. 2 ) then
           call ftbnfm(tform(i),dattyp,repeat,width,status)
         endif
         if (tform(i).eq. 'R       ') then
           dattyp = 43
           status = 0
         endif
         if (abs(dattyp) .eq. 16) repeat = repeat/width
C
         if (status .ne. 0) then
           call fcerrm(status)
         endif
C
C  skip column if no input
          if (colin(i) .le. 0) goto 100
         
          felem = 1
          ifrow = irow
          ofrow = orow
	  remain = drows * repeat

C variable length array
C if dattyp < 0, then we have a variable length array and must go one
C row at a time.  Find the nelem for this row.
          if (dattyp .lt. 0) then
            call ftgdes (luin, colin(i), ifrow, remain, offset, status)
            repeat = remain
            maxrows = irow + drows
          endif

 150      if ( remain .gt. 0 ) then

	    if ( remain .ge. maxsize ) then
	      nelem = maxsize
	    else
	      nelem = remain
	    end if

C --- Must use different subroutine for each data type ---

C ---       Data type is bits ---
	    if ( abs(dattyp) .eq. 1 ) then
	      call ftgcvi(luin,colin(i),ifrow,felem,nelem,nullval_space,
     &                    ivalues,anyf,status)
	      call ftpcli(luout,i,ofrow,felem,nelem,ivalues,status)

C ---       Data type is byte ---
	    else if ( abs(dattyp) .eq. 11 ) then
	      call ftgcvi(luin,colin(i),ifrow,felem,nelem,nullval_space,
     &                    ivalues,anyf,status)
	      call ftpcli(luout,i,ofrow,felem,nelem,ivalues,status)

C *** NEW   Data type is ASCII byte ---
	    else if ( abs(dattyp) .eq. 12 ) then
	      call ftgcvs(luin,colin(i),ifrow,felem,nelem,' ',bvalues,
     &	                  anyf,status)
              do 305 j = 1,nelem
                read(bvalues(j),'(z1)') jvalues(j)
                ivalues(j) = jvalues(j)
 305            continue
	      call ftpcli(luout,i,ofrow,felem,nelem,ivalues,status)

C *** NEW   Data type is ASCII logical ---
	    else if ( abs(dattyp) .eq. 13 ) then
              call ftgcvs(luin,colin(i),ifrow,felem,nelem,' ',svalues,
     +                    anyf,status)
                do 300 j = 1,nelem
                   read(svalues(j),'(l1)') lvalues(j)
 300            continue
             call ftpcll(luout,i,ofrow,felem,nelem,lvalues,status)

C ---       Data type is logical ---
	    else if ( abs(dattyp) .eq. 14 ) then
	      call ftgcl(luin,colin(i),ifrow,felem,nelem,lvalues,
     &	                  status)
	      call ftpcll(luout,i,ofrow,felem,nelem,lvalues,status)

C ---       Data type is ASCII characters ---
	    else if ( abs(dattyp) .eq. 16 ) then
	      call ftgcvs(luin,colin(i),ifrow,felem,nelem,' ',svalues,
     &	                  anyf,status)
	      call ftpcls(luout,i,ofrow,felem,nelem,svalues,status)

C ---       Data type is short integer ---
	    else if ( abs(dattyp) .eq. 21 ) then
	      call ftgcvi(luin,colin(i),ifrow,felem,nelem,nullval_zero,
     &                    ivalues, anyf,status)
	      call ftpcli(luout,i,ofrow,felem,nelem,ivalues,status)

C *** NEW   Data type is hex integer ---
            else if ( abs(dattyp) .eq. 29 ) then
              call ftgcvs(luin,colin(i),ifrow,felem,nelem,' ',svalues,
     +                    anyf,status)
              do 310 j = 1,nelem
                read(svalues(j),'(z4)') jvalues(j)
                ivalues(j) = jvalues(j)
 310          continue
              call ftpcli(luout,i,ofrow,felem,nelem,ivalues,status)
C
C ---       Data type is integer ---
	    else if ( abs(dattyp) .eq. 41 ) then
	      call ftgcvj(luin,colin(i),ifrow,felem,nelem,0,jvalues,
     &	                  anyf,status)
	      call ftpclj(luout,i,ofrow,felem,nelem,jvalues,status)

C ---       Data type is real ---
	    else if ( abs(dattyp) .eq. 42 ) then
	      call ftgcve(luin,colin(i),ifrow,felem,nelem,0.,evalues,
     &	                  anyf,status)
	      call ftpcle(luout,i,ofrow,felem,nelem,evalues,status)

C *** NEW   Data type is real w/ Radian to Degree conversion---
	    else if ( abs(dattyp) .eq. 43 ) then
	      call ftgcve(luin,colin(i),ifrow,felem,nelem,0.,evalues,
     &	                  anyf,status)
              do 320 j = 1,nelem
                evalues(j) = evalues(j) * radeg
 320          continue
	      call ftpcle(luout,i,ofrow,felem,nelem,evalues,status)

C ---       Data type is double precision ---
	    else if ( abs(dattyp) .eq. 82 ) then
	      call ftgcvd(luin,colin(i),ifrow,felem,nelem,0.D0,dvalues,
     &	                  anyf,status)
	      call ftpcld(luout,i,ofrow,felem,nelem,dvalues,status)

C ---       Data type is complex ---
	    else if ( abs(dattyp) .eq. 83 ) then
	      call ftgcvc(luin,colin(i),ifrow,felem,nelem,0.,cvalues,
     &	                  anyf,status)
	      call ftpclc(luout,i,ofrow,felem,nelem,cvalues,status)

C ---       Data type is double complex ---
	    else if ( abs(dattyp) .eq.163 ) then
	      call ftgcvm(luin,colin(i),ifrow,felem,nelem,0.D0,mvalues,
     &	                  anyf,status)
	      call ftpclm(luout,i,ofrow,felem,nelem,mvalues,status)
	    endif
	  end if

C --- Update the pointers in the column ---

          if (dattyp .gt. 0) then
            remain = remain - nelem
            felem = felem + nelem
            if (felem .gt. repeat) then
              if (repeat .ne. 0) then 
                 ifrow = ifrow + (felem-1)/repeat
                 ofrow = ofrow + (felem-1)/repeat
                 felem = felem - ((felem-1)/repeat)*repeat
              endif
             endif
          else
            remain = remain - nelem
            felem = felem + nelem
            if (felem .gt. repeat) then
               felem = 1
               ifrow = ifrow + 1
               ofrow = ofrow + 1
               if (ifrow .gt. maxrows) goto 200
               call ftgdes(luin, colin(i), ifrow, remain,offset, status)
               repeat = remain
            endif
          endif

          if ( remain .gt. 0 ) goto 150

 200      if (status .ne. 0 ) then
            call fcerrm(status)
            status = 0
          endif
C	
 100   continue

       irow = irow + drows
       orow = orow + drows
C
C
 999   continue
       return
       end

**********************************************************************
C SUBROUTINE:
C      extpri
C
C DESCRIPTION:
C      This subroutine writes the primary header to the output         
C      fits file and then defines the primary extension data buffer,      
C      which is null.                                                     
C
C AUTHOR:
C      Mark Cresitello-Dittmar              March 1994
C
C MODIFICATION HISTORY:
C
C  Replaced the blank inserted in the header of
C  the rdf file with the word "COMMENT".
C
C                Jeff Silvis RSTX 
C                             Jan 1998
C
C
C NOTES:
C
C USAGE:
C      call extpri(luqvp,luout,status)
C
C ARGUMENTS:
C      luqvp        Unit number of source  QVP file
C      luout        Unit number of target file
C      status       status flag
C
C PRIMARY LOCAL VARIABLES;
C      comnt        comment field text
C      errtxt       error message
C      hdutype      header type 
C      simple       Flag indicating FITS file conforms to FITS standards
C      extend       Flag for possible extensions following primary data 
C      bitpix       Bits per Pixel: 8, 16, 32, -32, -64
C      naxis        Number of dimensions in the FITS array
C      naxes        Array for sizes of FITS array dimensions 
C      pcount       Value of PCOUNT keyword (usually = 0)
C      gcount       Value of GCOUNT keyword (usually = 1)
C      tfields      Number of fields (columns) in the table 
C      nrows        Number of rows in table extension
C      
C CALLED ROUTINES:
C      subroutine fcecho    - echo message to terminal
C      subroutine fcerr     - write error message
C      subroutine fcerrm    - write status definition
C      subroutine ft____    - FITSIO routines
C      subroutine gobkwd    - defines observation keywords
C
C**********************************************************************
       subroutine extpri(luqvp,luout,status)
C
       implicit none

       integer luqvp,luout
       integer status


       integer i
       integer hdutype
       integer bitpix
       integer naxis
       integer naxes(10)
       integer pcount
       integer gcount
       integer nrows
       logical simple
       logical extend
       character*(60) comnt
       character*(80) blnk80
       character*(50) blnk50
       character*(80) errtxt

C

       external ftghpr,
     +          fcerr,
     +          fcerrm,
     +          ftphpr,
     +          ftprec,
     +          ftpcom,
     +          ftmahd,
     +          ftgkyj,
     +          ftpkyj,
     +          fcecho,
     +          ftpdef
C
C----------------------------------------------------------------------
C
C
C  The fits standard is for the keyword to have 8 characters
C with no embedded blanks.  An entire line of blanks violates this
C but having some spacing does make the file more readable, so I 
C have changed the blanks to the word COMMENT.
C The variable blnk50 is used with a proper keyword call so it 
C will still be filled with spaces. 
C
C  Jeff Silvis RSTX 
C  Jan 1998
C
       do 50 i = 1,80
         if (i .le. 50) blnk50(i:i) = ' '
C         blnk80(i:i) = ' '
 50    continue
        blnk80 = 'COMMENT' 

C
C  Get primary keyword values from input file
       call ftghpr(luqvp,10,simple,bitpix,naxis,naxes,pcount,
     +                      gcount,extend,status)
       if (status .ne. 0) then
         write(errtxt,'('' Error returned from ftghpr'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  Transfer these values to new output file
       call ftphpr(luout,simple,bitpix,naxis,naxes,pcount,
     +                   gcount,extend,status)
       if (status .ne. 0) then
         write(errtxt,'('' Error returned from ftphpr'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C Write end of required keyword separator
       call ftprec(luout,blnk80, status)
       call ftpcom(luout,'End of required structural keywords.',status)
       call ftprec(luout,blnk80, status)
C
C  Write observation keywords to output file
       call gobkwd(luqvp,luout,status)
       if (status .ne. 0) then
         write(errtxt,'('' Error returned from gobkwd'')')
         call fcerr(errtxt)
         goto 999
       endif
C
C  Get the Number of rows in the events list from the binary table
C  extension and write it into the primary header.
C
C    MOVE to extension header
       call ftmahd(luqvp, 2,hdutype,status)
C
C    Get the NAXIS2 keyword value from the extension
       call ftgkyj(luqvp,'NAXIS2  ',nrows,comnt,status)
C
C    Write numer of rows to primary header
       comnt = 'Number of events in the event list'
       call ftpkyj(luout,'NEVENT  ',nrows,comnt,status)
C
C    Return to primary header.
       call ftmahd(luqvp, 1,hdutype,status)
C
C    Check status
       if(nrows .eq. 0) then
         write(errtxt,'('' Error: Header returned invalid file size'')')
         call fcerr(errtxt)
         write(errtxt,'(''        NAXIS2 value = 0'')')
         call fcerr(errtxt)
         status = -1
         goto 999
       endif
       if(status .ne. 0) then
         write(errtxt,'('' Error transferring NEVENT value '')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  write status prompt to terminal so user has something to read
       call fcecho(' Primary Header defined')
C
C
C ----  Define the structure of the PRIMARY DATA ARRAY:  "NULL" ----
C
       call ftpdef(luout,bitpix,naxis,naxes,pcount,gcount,status)
       if (status .ne.0) then
         write(errtxt,'('' Error returned from ftpdef '')')
         call fcerr(errtxt)
         call fcerrm(status)
       endif
C
C
 999   return
       end


C**********************************************************************
C SUBROUTINE:
C      extevt
C
C DESCRIPTION:
C      This routine defines/writes the EVENTS binary table header
C      and data table to the file pointed to by luout. 
C
C AUTHOR:
C      Mark Cresitello-Dittmar              March 1994
C
C MODIFICATION HISTORY:
C
C  Replaced the blank inserted in the header of
C  the rdf file with the word "COMMENT".
C
C                Jeff Silvis RSTX 
C                             Jan 1998
C
C
C NOTES:
C
C USAGE:
C       call extevt(luqvp,luout,filnam,status)
C
C ARGUMENTS:
C      luqvp        Unit number of source  QVP file
C      luout        Unit number of target file
C      filnam       input QVP file name
C      status       status flag
C
C PRIMARY LOCAL VARIABLES;
C      hdutype      header type 
C      nrows        number of rows in data table
C      tfields      number of fields (columns) in the table extension
C      bitpix       bits per pixel
C      pcount       No group parameters (required keyword)
C      gcount       One data group  (required keyword)
C      sidx         pointer for start index of file name within file path
C      eidx         pointer for end index of file name within file path
C      frow         row at which to begin reading
C      felem        first pixel of the element vector
C      colnum       column map from input to output files
C      scalval      scale value for TSCALnn keyword
C      colform      formats of the output Binary Table columns
C      keywrd       Keyword variable
C      comnt        comment field text
C      anyf         flag for undefined elements
C      card         full header card
C      errtxt       error message
C      
C CALLED ROUTINES:
C      subroutine fcecho    - echo message to terminal
C      subroutine fcerr     - write error message
C      subroutine fcerrm    - write status definition
C      subroutine ft____    - FITSIO routines
C      subroutine gobkwd    - defines observation keywords
C      subroutine trnscol    - transfer data column
C
C**********************************************************************
       subroutine extevt(luqvp,luout,filnam,status)
C
       implicit none
C
       integer luqvp, luout
       integer status
       character*(180) filnam


       integer i
       integer hdutype
       integer nrows,tfields,varidat
       integer bitpix,pcount,gcount
       integer sidx,eidx
       integer fcol,frow,felem
       integer drows,remain,nelem
       integer colnum(37)
       integer*2 nullval_zero
       parameter (nullval_zero = 0)
       logical anyf
       character*(8) colform(37)
       character*(8) trnsfrm(37)
       character*(8) keywrd
       character*(60) comnt
       character*(80) errtxt
       character*(80) card
       character*(50) blnk50
       character*(80) blnk80
       double precision scalval
C
       integer*2 tjday(1024), fmlsec(1024)
       integer   milsec(1024)
       double precision time(1024)
C
       data trnsfrm/'D       ',2*'R       ',2*'E       ',8*'R       ',
     +            3*'E       ',2*'B       ',  'J       ',  'I       ',
     +              '2B      ',  'D       ',  'E       ',2*'R       ',
     +              'E       ',3*'J       ',8*'B       '/

       data colform/'D       ',15*'E       ',2*'B       ',
     +              'J       ',   'I       ',  '2B      ',
     +              'D       ', 4*'E       ',3*'J       ',
     +            8*'B       '/
C
       data colnum/ 0,17,18,21,22,17,18,20,19,13,14,16,15,04,
     +             05,06,10,11,07,08,09,25,26,28,29,27,30,31,
     +             32,33,34,35,36,37,38,39,40/
C
C
       intrinsic index,dble
       external ftmahd,
     +          ftcrhd,
     +          fcecho,
     +          ftgkyj,
     +          ftpkys,
     +          ftpkyj,
     +          ftpcom,
     +          ftprec,
     +          ftpkyf,
     +          ftgcrd,
     +          ftikys,
     +          ftbdef,
     +          ftgcvj,
     +          ftgcvi,
     +          ftpcld
C
C----------------------------------------------------------------------
C
C
C  The fits standard is for the keyword to have 8 characters
C with no embedded blanks.  An entire line of blanks violates this
C but having some spacing does make the file more readable, so I 
C have changed the blanks to the word COMMENT.
C The variable blnk50 is used with a proper keyword call so it 
C will still be filled with spaces. 
C
C  Jeff Silvis RSTX 
C  Jan 1998
C
       do 50 i = 1,80
         if (i .le. 50) blnk50(i:i) = ' '
C         blnk80(i:i) = ' '
 50    continue
        blnk80 = 'COMMENT' 

C
C  Move to extension hdu of QVP file
       call ftmahd(luqvp, 2, hdutype, status)
       if (status .ne.0) then
         write(errtxt,'('' Error returned from ftmahd '')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  Create a new header in the output file.
       call ftcrhd(luout, status)
       if (status .ne.0) then
         write(errtxt,'('' Error returned from ftcrhd'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** Required Keywords
C
C  Write the required keywords to the output file
C    get values for required keywords
       call ftgkyj(luqvp,'BITPIX  ',bitpix,comnt,status)
       call ftgkyj(luqvp,'NAXIS2  ',nrows ,comnt,status)
       call ftgkyj(luqvp,'PCOUNT  ',pcount,comnt,status)
       call ftgkyj(luqvp,'GCOUNT  ',gcount,comnt,status)
       tfields = 37
C
       call ftpkys(luout,'XTENSION','BINTABLE',
     +  'Standard binary table', status)
       call ftpkyj(luout,'BITPIX  ',bitpix,
     +  'Required value', status)
       call ftpkyj(luout,'NAXIS   ',2,
     +  'Required value', status)
       call ftpkyj(luout,'NAXIS1  ',122,
     +  'Number of bytes in row', status)
       call ftpkyj(luout,'NAXIS2  ',nrows,
     +  'Number of rows in table', status)
       call ftpkyj(luout,'PCOUNT  ',pcount,
     +  'No group parameters -- Required value', status)
       call ftpkyj(luout,'GCOUNT  ',gcount,
     +  'One group of data -- Required value', status)
       call ftpkyj(luout,'TFIELDS ',tfields,
     +  'Number of fields (columns) in table', status)
C
C  Write end of required keyword separator
       call ftpcom(luout,'End of required structural keywords.',status)
       call ftprec(luout,blnk80,status)
C
C **** Extension name
C
C  Extension name keyword
       call ftpkys(luout,'EXTNAME ','EVENTS  ',
     +  'EGRET events list in RDF style format', status)
       call ftprec(luout,blnk80,status)
C
C  check error status
       if (status .ne.0) then
         write(errtxt,'('' Error writing required keywords'')') 
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** Observation keywords
C
C  Add observation keywords
       call gobkwd(luqvp,luout,status)
       if (status .ne. 0) then
         write(errtxt,'('' Error returned from gobkwd'')') 
         call fcerr(errtxt)
         goto 999
       endif
C
C **** Number of Events in list
C
C  Write the number of events/rows in list.
       comnt = 'Number of events in the event list'
       call ftpkyj(luout,'NEVENT  ',nrows,comnt,status)
C
C  check status
       if (status .ne.0) then
         write(errtxt,'('' Error writing NEVENT value'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** COLUMN NAMES
C
       call ftprec(luout,blnk80,status)
       call ftprec(luout,blnk80,status)
       call ftpcom(luout,'*** Column names           ',status)
       call ftprec(luout,blnk80,status)
       call ftpkys(luout,'TTYPE1  ','TIME    ',
     +  'Time in seconds ', status)
       call ftpkys(luout,'TTYPE2  ','X       ',
     +  'X and Y are duplicates of RA and DEC',status)
       call ftpkys(luout,'TTYPE3  ','Y       ',
     +  'All angles are in degrees           ',status)
       call ftpkys(luout,'TTYPE4  ','ENERGY  ',blnk50,status)
       call ftpkys(luout,'TTYPE5  ','ENERGY_ERROR',
     +  'One sigma uncertainty in error      ',status)
       call ftpkys(luout,'TTYPE6  ','RA      ',blnk50,status)
       call ftpkys(luout,'TTYPE7  ','DEC     ',blnk50,status)
       call ftpkys(luout,'TTYPE8  ','L       ',blnk50,status)
       call ftpkys(luout,'TTYPE9  ','B       ',blnk50,status)
       call ftpkys(luout,'TTYPE10 ','XZ_ANGLE',blnk50,status)
       call ftpkys(luout,'TTYPE11 ','YZ_ANGLE',blnk50,status)
       call ftpkys(luout,'TTYPE12 ','AZIMUTH ',blnk50,status)
       call ftpkys(luout,'TTYPE13 ','ZEN_ANGLE',
     +  'Angle from Earth zenith             ',status)
       call ftpkys(luout,'TTYPE14 ','X_INERTIAL',
     +  'X,Y,Z relative to Earth (km)        ',status)
       call ftpkys(luout,'TTYPE15 ','Y_INERTIAL',blnk50,status)
       call ftpkys(luout,'TTYPE16 ','Z_INERTIAL',blnk50,status)
       call ftpkys(luout,'TTYPE17 ','TASC_FLAG_1',
     +  'Instrument configuration codes      ',status)
       call ftpkys(luout,'TTYPE18 ','TASC_FLAG_2',blnk50,status)
       call ftpkys(luout,'TTYPE19 ','B_C_TAGS',blnk50,status)
       call ftpkys(luout,'TTYPE20 ','COINCIDENCE_MOD',blnk50,status)
       call ftpkys(luout,'TTYPE21 ','PACK_ERROR_FLAG',blnk50,status)
       call ftpkys(luout,'TTYPE22 ','BARYTIME_CORRECTION',
     +  'Barycenter time correction for      ',status)
       call ftpkys(luout,'TTYPE23 ','BARY_PHASE',
     +  ' pulsar at assumed location         ',status)
       call ftpkys(luout,'TTYPE24 ','RA_PULSAR',
     +  'RA,DEC assumed for pulsar           ',status)
       call ftpkys(luout,'TTYPE25 ','DEC_PULSAR',
     +  'Columns 22-26 may not be filled     ',status)
       call ftpkys(luout,'TTYPE26 ','PHASE_PULSAR',blnk50,status)
       call ftpkys(luout,'TTYPE27 ','XBARYCEN',
     +  'X,Y,Z in lightseconds in solar barycenter',status)
       call ftpkys(luout,'TTYPE28 ','YBARYCEN',blnk50,status)
       call ftpkys(luout,'TTYPE29 ','ZBARYCEN',blnk50,status)
       call ftpkys(luout,'TTYPE30 ','STR_ANAL_WORD',blnk50,status)
       call ftpkys(luout,'TTYPE31 ','RC_SINGLE',
     +  'Return codes from EGRET team software',status)
       call ftpkys(luout,'TTYPE32 ','RC_SAGE ',blnk50,status)
       call ftpkys(luout,'TTYPE33 ','RC_SFLAGS_1',blnk50,status)
       call ftpkys(luout,'TTYPE34 ','RC_SFLAGS_2',blnk50,status)
       call ftpkys(luout,'TTYPE35 ','RC_SCATR',blnk50,status)
       call ftpkys(luout,'TTYPE36 ','RC_ENERGY',blnk50,status)
       call ftpkys(luout,'TTYPE37 ','RC_DIRCTN',blnk50,status)
C
C  check status
       if (status .ne.0) then
         write(errtxt,'('' Error writing column names'')') 
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** COLUMN FORMATS
C
       call ftprec(luout,blnk80,status)
       call ftpcom(luout,'*** Column formats         ',status)
       call ftprec(luout,blnk80,status)
C
C   write the set of formats to the header file
       keywrd = 'TFORM  '
       do 120, i = 1, tfields
         if (i .lt. 10) then
            write(keywrd(6:6),'(i1)') i
         else
            write(keywrd(6:7),'(i2)') i
         endif
         call ftpkys(luout,keywrd,colform(i),blnk50,status)

 120   continue
C
C  check status
       if (status .ne.0) then
         write(errtxt,'('' Error writing column formats'')') 
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** COLUMN UNITS
C
       call ftprec(luout,blnk80,status)
       call ftpcom(luout,'*** Column Units           ',status)
       call ftprec(luout,blnk80,status)

       call ftpkys(luout,'TUNIT1  ','s       ',
     +  'Seconds                  ',status)
       call ftpkys(luout,'TUNIT2  ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT3  ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT4  ','MeV     ',blnk50,status)
       call ftpkys(luout,'TUNIT5  ','MeV     ',blnk50,status)
       call ftpkys(luout,'TUNIT6  ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT7  ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT8  ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT9  ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT10 ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT11 ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT12 ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT13 ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT14 ','km      ',
     +  'Kilometers from earth center',status)
       call ftpkys(luout,'TUNIT15 ','km      ',
     +  'Kilometers from earth center',status)
       call ftpkys(luout,'TUNIT16 ','km      ',
     +  'Kilometers from earth center',status)
       call ftpkys(luout,'TUNIT22 ','s       ',
     +  'Seconds                  ',status)
       call ftpkys(luout,'TUNIT24 ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT25 ','deg     ',
     +  'Degrees                  ',status)
       call ftpkys(luout,'TUNIT27 ','km      ',
     +  'Distance from Solar System barycenter ',status)
       call ftpkys(luout,'TUNIT28 ','km      ',
     +  'Unscaled values are in lightmicroseconds ',status)
       call ftpkys(luout,'TUNIT29 ','km      ',
     +  '  (TSCALnn = speed of light)          ',status)
C
       scalval = 0.299792500
       call ftprec(luout,blnk80,status)
       comnt = 'Speed of light in km/microsecond      '
       call ftpkyg(luout,'TSCAL27 ', scalval, 10, comnt, status)
       call ftpkyg(luout,'TSCAL28 ', scalval, 10, comnt, status)
       call ftpkyg(luout,'TSCAL29 ', scalval, 10, comnt, status)
C
C  check status
       if (status .ne.0) then
         write(errtxt,'('' Error writing column units '')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** COORDINATE/PROJECTION INFORMATION  KEYWORDS
C
       call ftprec(luout,blnk80,status)
       card = 
     +  '*** Following keywords handle coord/projection information. '
       call ftpcom(luout,comnt,status)
       call ftprec(luout,blnk80,status)
       card =
     + 'The following pairs of columns form natural coordinate pairs:'
       call ftpcom(luout,comnt,status)
       comnt = ' X,Y  RA,DEC  GLON,GLAT '
       call ftpcom(luout,comnt,status)
       call ftprec(luout,blnk80,status)
       comnt = '*** Coordinate/projection type information for columns'
       call ftpcom(luout,comnt,status)
       call ftprec(luout,blnk80,status)
       call ftpkys(luout,'TCTYP2  ','RA---CAR',
     +  'Cartesian (Rectangular) projection ', status)
       call ftpkys(luout,'TCTYP3  ','DEC--CAR',
     +  '(X and Y are copies of RA and DEC)', status)
       call ftpkys(luout,'TCTYP6  ','RA---CAR',
     +  'Cartesian (Rectangular) projection ', status)
       call ftpkys(luout,'TCTYP7  ','DEC--CAR',blnk50, status)
       call ftpkys(luout,'TCTYP8  ','GLON-CAR',
     +  'Galactic coordinates can also be   ', status)
       call ftpkys(luout,'TCTYP9  ','GLAT-CAR',
     +  'viewed as a Cartesian projection   ', status)
C
C  check status
       if (status .ne. 0) then
        write(errtxt,'('' Error writing TCTYPx keywords'')')
        call fcerr(errtxt)
        call fcerrm(status)
        goto 999
       endif
C
C **** COORDINATE/PROJECTION SCALING INFORMATION
C
       call ftprec(luout,blnk80,status)
       comnt = '*** Coordinate/projection scaling information.'
       call ftpcom(luout,comnt,status)
       call ftprec(luout,blnk80,status)
C
       call ftpkyf(luout,'TCDLT2  ', 1.0, 5, blnk50, status)
       call ftpkyf(luout,'TCDLT3  ', 1.0, 5, blnk50, status)
       call ftpkyf(luout,'TCDLT6  ', 1.0, 5, blnk50, status)
       call ftpkyf(luout,'TCDLT7  ', 1.0, 5, blnk50, status)
       call ftpkyf(luout,'TCDLT8  ', 1.0, 5, blnk50, status)
       call ftpkyf(luout,'TCDLT9  ', 1.0, 5, blnk50, status)
C
C  check status
       if (status .ne. 0) then
        write(errtxt,'('' Error writing TCDLTx keywords'')')
        call fcerr(errtxt)
        call fcerrm(status)
        goto 999
       endif
C
C **** COORDINATE/PROJECTION OFFSETS
C
       call ftprec(luout,blnk80,status)
       comnt = '*** Coordinate/projection offsets'
       call ftpcom(luout,comnt,status)
       call ftprec(luout,blnk80,status)
C
       call ftpkyf(luout,'TCRPX2  ', 0.0, 5, blnk50, status)
       call ftpkyf(luout,'TCRPX3  ', 0.0, 5, blnk50, status)
       call ftpkyf(luout,'TCRPX6  ', 0.0, 5, blnk50, status)
       call ftpkyf(luout,'TCRPX7  ', 0.0, 5, blnk50, status)
       call ftpkyf(luout,'TCRPX8  ', 0.0, 5, blnk50, status)
       call ftpkyf(luout,'TCRPX9  ', 0.0, 5, blnk50, status)
C
C  check status
       if (status .ne. 0) then
        write(errtxt,'('' Error writing TCRPXx keywords'')')
        call fcerr(errtxt)
        call fcerrm(status)
        goto 999
       endif
C
C
C **** COORDINATE/PROJECTION REFERENCE POINT VALUES
C
       call ftprec(luout,blnk80,status)
       comnt = '*** Coordinate/projection reference point values'
       call ftpcom(luout,comnt,status)
       call ftprec(luout,blnk80,status)
C
       call ftpkyf(luout,'TCRVL2  ', 0.0, 5, blnk50, status)
       call ftpkyf(luout,'TCRVL3  ', 0.0, 5, blnk50, status)
       call ftpkyf(luout,'TCRVL6  ', 0.0, 5, blnk50, status)
       call ftpkyf(luout,'TCRVL7  ', 0.0, 5, blnk50, status)
       call ftpkyf(luout,'TCRVL8  ', 0.0, 5, blnk50, status)
       call ftpkyf(luout,'TCRVL9  ', 0.0, 5, blnk50, status)
C
C  check status
       if (status .ne. 0) then
        write(errtxt,'('' Error writing TCRVLx keywords '')') 
        call fcerr(errtxt)
        call fcerrm(status)
        goto 999
       endif
C
C **** Keyword insertions
C
C  insert keyword FILENAME after FILETYPE
C    determine filename size and position from full input filename
       sidx   = index(filnam,'QVP')
       eidx   = index(filnam,' ')
       eidx   = eidx - 1
C
       call ftgcrd(luout,'FILETYPE',card,status)
       comnt = 'Input file data derived from'
       if ((eidx - sidx) .lt. 18) then       
         call ftikys(luout,'FILENAME',filnam(sidx:eidx),comnt,status)
       else
         call ftikys(luout,'FILENAME',filnam(sidx:sidx+18),comnt,status)
       endif
C
C  insert keyword HDUCLAS1 after HDUCLAS
       call ftgcrd(luout,'HDUCLAS ',card,status)
       comnt = 'Basis class of data '
       call ftikys(luout,'HDUCLAS1','EVENTS  ',comnt,status)
C
C  check status
       if (status .ne. 0) then
        write(errtxt,'('' Error inserting keywords'')')
        call fcerr(errtxt)
        call fcerrm(status)
        goto 999
       endif
C
C  write status flag so user has something to read
       call fcecho(' EVENT Binary extension header defined')
C
C----------------------------------------------------------------------
C  assign some values
       felem = 1
       frow  = 1
       varidat = 0
       remain = nrows
C
C  Define the structure of the output BINARY TABLE EXTENSION
       call ftbdef(luout,tfields,colform,varidat,nrows,status)
       if (status .ne. 0) then
         write(errtxt,'('' Error returned from ftbdef'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  Convert time fields from input QVP file into output format
 150   continue
       if (remain .ge. 1024) then
          nelem = 1024
       else
          nelem = remain
       endif
C
C    retrieve time field columns
       fcol = 1
       call ftgcvj(luqvp,fcol,frow,felem,nelem,0,milsec,anyf,status)
       fcol = 2
       call ftgcvi(luqvp,fcol,frow,felem,nelem,nullval_zero,fmlsec,
     &             anyf,status)
       fcol = 3
       call ftgcvi(luqvp,fcol,frow,felem,nelem,nullval_zero,tjday,
     &             anyf,status)
C
       if (status .ne. 0) then
         if (status .eq. 107) then
            write(errtxt,'(''ERROR: Premature End-of-File found'')')
         else
            write(errtxt,'('' Error retrieving time field.'')')
         endif
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C    Combine time fields into output format
       do 140 i = 1,nelem
         time(i) = dble(tjday(i))*86400.0
         time(i) = time(i)  + dble(milsec(i))*(1.00D-03)
         time(i) = time(i)  + dble(fmlsec(i))*(1.00D-06)
C
 140   continue
C
C    Write TIME array into output file
       fcol = 1
       call ftpcld(luout,fcol,frow,felem,nelem,time,status)
C
C    check status
       if (status .ne. 0) then
         write(errtxt,'('' Error writing time field.'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C    loop back till all rows have been read
       remain = remain - nelem
       if (remain .gt. 0) then
         frow = frow + nelem
         goto 150
       endif
C
C  Transfer other columns according to formats specified in colnum array
       drows = nrows
       frow = 1
       call trnscol(luqvp,luout,frow,frow,tfields,drows,
     +              trnsfrm,colnum,hdutype,status)
       if (status .ne. 0) then
         write(errtxt,'('' Error returned from trnscol'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
      call fcecho(' EVENT data transferred')
C
 999  continue
      return
      end

C**********************************************************************
C SUBROUTINE:
C      exttsi
C
C DESCRIPTION:
C      This subroutine writes the TSI (exposure) extension header, and 
C      table to the file specified as luout.
C
C AUTHOR:
C      Mark Cresitello-Dittmar              March 1994
C
C MODIFICATION HISTORY:
C
C NOTES:
C
C  Replaced the blank inserted in the header of
C  the rdf file with the word "COMMENT".
C
C                Jeff Silvis RSTX 
C                             Jan 1998
C
C USAGE:
C
C ARGUMENTS:
C      luqvp        Unit number of source  QVP file
C      luexp        Unit number of source  EXP file
C      luout        Unit number of target file
C      filnam       name of file
C      status       status flag
C
C PRIMARY LOCAL VARIABLES;
C      hdutype      header type 
C      tfields      number of fields (columns) in the table extension
C      pcount       no group parameters  (required keyword)
C      gcount       data groups (required keyword)
C      sidx         pointer for start index of file name within file path
C      eidx         pointer for end index of file name within file path
C      frow         row at which to begin reading
C      felem        first pixel of the element vector
C      colnum       column map from input to output files
C      scalval      scale value for TSCALnn keyword
C      outform      formats of the output Binary Table columns
C      keywrd       Keyword variable
C      comnt        comment field text
C      anyf         flag for undefined elements
C      card         full header card
C      naxis1       Number of bytes in a row 
C      telmod       direction modes
C      typs         mask result for mode determination
C      errtxt       error message
C      
C CALLED ROUTINES:
C      subroutine fcecho    - echo message to terminal
C      subroutine fcerr     - write error message
C      subroutine fcerrm    - write status definition
C      subroutine ft____    - FITSIO routines
C      subroutine gobkwd    - defines observation keywords
C      subroutine trnscol    - transfer data column
C
C**********************************************************************
       subroutine exttsi(luqvp,luexp,luout,filnam,status)
C
       implicit none
C
       integer luqvp,luexp,luout
       integer status
       character*(180) filnam

       integer*2 typs
       integer*2 mode
       integer i,j
       integer hdutype,tfields,varidat
       integer bitpix,pcount,gcount
       integer frow,orow,felem,nrows,nelem,remain
       integer sidx,eidx
       integer telmod(74)
       integer naxis1
       integer colnum(19)
       integer*2 nullval_zero
       parameter (nullval_zero = 0)
       logical anyf
       character*(8) keywrd
       character*(60) comnt
       character*(50) blnk50
       character*(80) blnk80
       character*(80) card
       character*(80) errtxt

       integer*2        ivalues(1024)
       integer          jvalues(1024)
       double precision dvalues(1024)
C
C  Arrays of column formats for the TSI Table in order of output
C   trnform = formats input to trnscol to properly transfer columns
C   outform = formats for output file..
       character*(8) trnform(19)
       character*(8) outform(19)
C
       data colnum/0,20,21,9,5,6,7,9,11,13,15,16,17,18,19,22,23,24,25/
C
       data trnform/'D       ',2*'E        ',  'Z       ',
     +            3*'L       ',  'Z        ',3*'B      ',
     +            8*'R       '/
C
       data outform/'D       ',2*'E        ',  'I       ',
     +            3*'L       ',  'I        ',3*'B       ',
     +            8*'E       '/
C
       data telmod /
     +         0 ,   4096 ,  16384 ,    256 ,   1024 ,   5120 ,  20480 ,
     +     16640 ,   1280 ,   7168 ,   6144 ,  12288 ,  28672 ,  24576 ,
     +     16512 ,  16768 ,    384 ,    768 ,   1792 ,   1536 ,   3072 ,
     +     14336 ,  24704 ,    896 ,   3584 ,  15360 ,  30720 ,  28800 ,
     +     24960 ,  17280 ,   1920 ,   3840 ,   7680 ,  31744 ,  29056 ,
     +     18304 ,   7936 ,  15872 ,  30848 ,  25472 ,   3968 ,  15875 ,
     +     15363 ,  31747 ,  30723 ,  30851 ,  28803 ,  29059 ,  24963 ,
     +     25475 ,  17283 ,  18307 ,   1923 ,   3971 ,   3843 ,   7939 ,
     +      7683 ,  32259 ,  31875 ,  31107 ,  29571 ,  26499 ,  20355 ,
     +      8067 ,  16131 ,  32387 ,  31619 ,  28547 ,  16259 ,  32515 ,
     +     32131 ,  30595 ,  24451 ,  32643 /
C
C Original hex representations
C     +   x'0000', x'1000', x'4000', x'0100', x'0400', x'1400', x'5000',
C     +   x'4100', x'0500', x'1C00', x'1800', x'3000', x'7000', x'6000',
C     +   x'4080', x'4180', x'0180', x'0300', x'0700', x'0600', x'0C00',
C     +   x'3800', x'6080', x'0380', x'0E00', x'3C00', x'7800', x'7080',
C     +   x'6180', x'4380', x'0780', x'0F00', x'1E00', x'7C00', x'7180',
C     +   x'4780', x'1F00', x'3E00', x'7880', x'6380', x'0F80', x'3E03',
C     +   x'3C03', x'7C03', x'7803', x'7883', x'7083', x'7183', x'6183',
C     +   x'6383', x'4383', x'4783', x'0783', x'0F83', x'0F03', x'1F03',
C     +   x'1E03', x'7E03', x'7C83', x'7983', x'7383', x'6783', x'4F83',
C     +   x'1F83', x'3F03', x'7E83', x'7B83', x'6F83', x'3F83', x'7F03',
C     +   x'7D83', x'7783', x'5F83', x'7F83'/
C
       intrinsic index,dble,iand
C
       external ftcrhd,
     +          fcerr,
     +          fcerrm,
     +          ftmrhd,
     +          ftgkyj,
     +          ftpkys,
     +          ftpkyj,
     +          ftpcom,
     +          ftprec,
     +          ftgcrd,
     +          ftikys,
     +          ftmkye,
     +          fcecho,
     +          ftbdef,
     +          ftgcvi,
     +          ftgcvj,
     +          ftpcld,
     +          ftpcli

C----------------------------------------------------------------------
C
       frow  = 1
       felem = 1
C
C  initialize blank character strings
C
C  The fits standard is for the keyword to have 8 characters
C with no embedded blanks.  An entire line of blanks violates this
C but having some spacing does make the file more readable, so I 
C have changed the blanks to the word COMMENT.
C The variable blnk50 is used with a proper keyword call so it 
C will still be filled with spaces. 
C
C  Jeff Silvis RSTX 
C  Jan 1998
C
       do 50 i = 1,80
         if (i .le. 50) blnk50(i:i) = ' '
C        blnk80(i:i) = ' '
 50    continue
        blnk80 = 'COMMENT' 

C
C  Create a new header for the TSI extension
       call ftcrhd(luout, status)
       if (status .ne.0) then
         write(errtxt,'('' Error returned from ftcrhd'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  Move to exposure file (TSI) extension header
       i = 1
       call ftmrhd(luexp, i,hdutype,status)
       if(status .ne. 0) then
         write(errtxt,'('' Error returned from ftmrhd'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  Set required keyword values
       call ftgkyj(luexp,'NAXIS2  ',nrows,comnt,status)
       bitpix = 8
       naxis1 = 58
       pcount = 0
       gcount = 1
       tfields = 19
C
C  Write required keywords
       call ftpkys(luout,'XTENSION','BINTABLE',
     +    'binary table extension ', status)
       call ftpkyj(luout,'BITPIX  ',bitpix,'Required value', status)
       call ftpkyj(luout,'NAXIS   ',     2,'Required value', status)
       call ftpkyj(luout,'NAXIS1  ',naxis1,
     +    'Number of bytes in row', status)
       call ftpkyj(luout,'NAXIS2  ',nrows,
     +    'Number of rows in table', status)
       call ftpkyj(luout,'PCOUNT  ',pcount,
     +    'No group parameters -- Required value', status)
       call ftpkyj(luout,'GCOUNT  ',gcount,
     +    'One group of data -- Required value', status)
       call ftpkyj(luout,'TFIELDS ',tfields,
     +    'Number of fields (columns) in table', status)
C
C    Write end of required keyword separator
       call ftpcom(luout,'End of required structural keywords',status)
       call ftprec(luout,blnk80,status)
C
C   check status
       if (status .ne.0) then
        write(errtxt,'('' Error writing required keywords'')')
        call fcerr(errtxt)
        call fcerrm(status)
        goto 999
       endif
C
C **** Extension name
C
C   Extension name keyword
       call ftpkys(luout,'EXTNAME ','TSI     ',
     +    'Telemetry scalar file (< exposure hist.)', status)
       call ftprec(luout,blnk80,status)
C
C **** Observation keywords
C
C   Add the observation keywords
       call gobkwd(luqvp,luout,status)
       if (status .ne. 0) then
         write(errtxt,'('' Error returned from gobkwd'')')
         call fcerr(errtxt)
         goto 999
       endif
C
C **** COLUMN NAMES
       call ftprec(luout,blnk80,status)
       call ftprec(luout,blnk80,status)
       call ftpcom(luout,'*** Column names           ',status)
       call ftprec(luout,blnk80,status)

       call ftpkys(luout,'TTYPE1  ','TIME    ', 
     +    'Time of beginning of interval      ', status)
       call ftpkys(luout,'TTYPE2  ','LIVETIME',
     +    'Livetime during interval            ',status)
       call ftpkys(luout,'TTYPE3  ','T_ELAPSE',
     +    'Elapsed time during interval        ',status)
       call ftpkys(luout,'TTYPE4  ','MODE    ',
     +    'EGRET exposure mode index           ',status)
       call ftpkys(luout,'TTYPE5  ','SAA     ',
     +    'SAA passage interval?               ',status)
       call ftpkys(luout,'TTYPE6  ','POINT_DEV',
     +     'Deviation from pointing > .5 deg?  ',status)
       call ftpkys(luout,'TTYPE7  ','EXCL_FLAG',
     +     'Interval excluded in EGRET timeline?',status)
       call ftpkys(luout,'TTYPE8  ','HODOSCOP',
     +     'EGRET HODOSCOPE code               ',status)
       call ftpkys(luout,'TTYPE9  ','COINCIDENCE',
     +     'Coincidence mode of EGRET          ',status)
       call ftpkys(luout,'TTYPE10 ','TASC1_DEPOSIT',
     +     'TASC1 deposition requirements      ',status)
       call ftpkys(luout,'TTYPE11 ','TASC2_DEPOSIT',
     +     'TASC2 deposition requirements      ',status)
       call ftpkys(luout,'TTYPE12 ','RA_SCZ  ',
     +     'RA,DEC of Z (pointing) axis        ',status)
       call ftpkys(luout,'TTYPE13 ','DEC_SCZ ',blnk50,status)
       call ftpkys(luout,'TTYPE14 ','RA_SCX  ',
     +     'RA,DEC of X axis                   ',status)
       call ftpkys(luout,'TTYPE15 ','DEC_SCX ',blnk50,status)
       call ftpkys(luout,'TTYPE16 ','RA_EARTH_START',
     +     'RA,DEC of Earth at start of interval',status)
       call ftpkys(luout,'TTYPE17 ','DEC_EARTH_START',blnk50,status)
       call ftpkys(luout,'TTYPE18 ','RA_EARTH_END',
     +     'RA,DEC of Earth at end of interval ',status)
       call ftpkys(luout,'TTYPE19 ','DEC_EARTH_END',blnk50,status)
C
C  Check status
       if (status .ne.0) then
        write(errtxt,'('' Error writing column names'')')
        call fcerr(errtxt)
        call fcerrm(status)
        goto 999
       endif
C
C **** COLUMN FORMATS
C
       call ftprec(luout,blnk80,status)
       call ftprec(luout,blnk80,status)
       call ftpcom(luout,'*** Column formats         ',status)
       call ftprec(luout,blnk80,status)
C
C   write the set of formats to the header file
       keywrd = 'TFORM  '
       do 120 i = 1, tfields
         if (i .lt. 10) then
            write(keywrd(6:6),'(i1)') i
         else
            write(keywrd(6:7),'(i2)') i
         endif
         call ftpkys(luout,keywrd,outform(i),blnk50,status)

 120   continue
C
C  Check status
       if (status .ne.0) then
        write(errtxt,'('' Error writing column formats'')')
        call fcerr(errtxt)
        call fcerrm(status)
        goto 999
       endif
C
C **** COLUMN UNITS
C
       call ftprec(luout,blnk80,status)
       call ftprec(luout,blnk80,status)
       call ftpcom(luout,'*** Column Units           ',status)
       call ftprec(luout,blnk80,status)

       call ftpkys(luout,'TUNIT1  ','s       ',
     +    'Seconds                  ',status)
       call ftpkys(luout,'TUNIT2  ','s       ',
     +    'Seconds                  ',status)
       call ftpkys(luout,'TUNIT3  ','s       ',
     +    'Seconds                  ',status)
       call ftpkys(luout,'TUNIT12 ','deg     ',
     +    'Degrees                  ',status)
       call ftpkys(luout,'TUNIT13 ','deg     ',
     +    'Degrees                  ',status)
       call ftpkys(luout,'TUNIT14 ','deg     ',
     +    'Degrees                  ',status)
       call ftpkys(luout,'TUNIT15 ','deg     ',
     +    'Degrees                  ',status)
       call ftpkys(luout,'TUNIT16 ','deg     ',
     +    'Degrees                  ',status)
       call ftpkys(luout,'TUNIT17 ','deg     ',
     +    'Degrees                  ',status)
       call ftpkys(luout,'TUNIT18 ','deg     ',
     +    'Degrees                  ',status)
       call ftpkys(luout,'TUNIT19 ','deg     ',
     +    'Degrees                  ',status)
C
C  Check status
       if (status .ne.0) then
         write(errtxt,'('' Error writing column units'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** Insert Keywords
C
C  Insert keyword FILENAME after FILETYPE
C    determine filename size and position from full input filename
       sidx   = index(filnam,'exphst')
       eidx   = index(filnam,' ')
       eidx   = eidx - 1
C
       call ftgcrd(luout,'FILETYPE',card,status)
       comnt = 'Input file data derived from'
       if ((eidx - sidx) .lt. 18) then       
         call ftikys(luout,'FILENAME',filnam(sidx:eidx),comnt,status)
       else
       call ftikys(luout,'FILENAME',filnam(sidx:sidx+18),comnt,status)
       endif
C
C  Insert keyword HDUCLAS1 after HDUCLAS
       call ftgcrd(luout,'HDUCLAS ',card,status)
       comnt = 'Telemetry scalar Information                   '
       call ftikys(luout,'HDUCLAS1','TSI     ',comnt,status)
C
C  Check status
       if (status .ne. 0) then
         write(errtxt,'('' Error inserting keywords'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C **** Keyword changes
C
C  Change value of TIMEDEL keyword
        comnt = 'Time resolution                                '
        call ftmkye(luout,'TIMEDEL ',1.1574074E-08, 7, comnt, status)
C
       if (status .ne. 0) then
         write(errtxt,'('' Error making keyword changes: '')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  Write status to terminal so the user has something to read
       call fcecho(' TSI extension header defined')
C
C----------------------------------------------------------------------
C  Set some variables
       frow = 1
       orow = 1
       varidat = 0
C
C  Define the structure of the output BINARY TABLE EXTENSION
       call ftbdef(luout,tfields,outform,varidat,nrows,status)
       if (status .ne. 0) then
         write(errtxt,'('' Error returned from ftbdef'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  Transfer data items for output columns 2-19
       call trnscol(luexp, luout, frow, orow, tfields, nrows,
     +              trnform, colnum, hdutype, status)
       if (status .ne. 0 ) then
         write(errtxt,'('' Error returned from trnscol'')')
         call fcerr(errtxt)
         goto 999
       endif
C
C  Process the time fields and calculate/replace mode
       frow = 1
       remain = nrows

 150   continue
       if (remain .ge. 1024) then
          nelem = 1024
       else
          nelem = remain 
       endif
C
C   Get time data from input file and convert to seconds
       call ftgcvi(luexp,2,frow,felem,nelem,nullval_zero,
     &             ivalues,anyf,status)
       call ftgcvj(luexp,3,frow,felem,nelem,0,jvalues,anyf,status)
C
       do 140 i = 1,nelem
         dvalues(i) = dble(ivalues(i))*86400.0 
         dvalues(i) = dvalues(i) + dble(jvalues(i))*0.001
 140   continue
C
C    Write TIME array into output file
       i = 1
       call ftpcld(luout,i,frow,felem,nelem,dvalues,status)
C
C    Check status
       if (status .ne. 0 ) then
         write(errtxt,'('' Error processing time fields'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C   Get values from column 4 and convert them to mode values
       call ftgcvi(luout,4,frow,felem,nelem,nullval_zero,
     &             ivalues,anyf,status)

       do 160 i = 1,nelem
C    Determine mode
         typs = IAND(INT(ivalues(i)),124)
C
C     Find the type and direction mode index
         mode = -1
         do j=1,74
           if (ivalues(i) .eq. telmod(j)+typs) mode = j
         end do

         ivalues(i) = mode

 160   continue
       call ftpcli(luout,4,frow,felem,nelem,ivalues,status)
C
C    Check status
       if (status .ne. 0 ) then
         write(errtxt,'('' Error processing mode column'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif 
C
C  update pointer and get rest of column
       remain = remain - nelem
       if (remain .gt. 0) then
         frow = frow + nelem
         goto 150
       endif
C
       call fcecho(' TSI data transferred')
C
 999   continue
       return
       end


C**********************************************************************
C SUBROUTINE:
C      docnvt
C
C DESCRIPTION:
C      This routine is the meat of the task.  It will make all the 
C      FITSIO calls and do the data manipulation.  
C
C AUTHOR:
C      Mark Cresitello-Dittmar       24-March 1994
C 
C MODIFICATION HISTORY:
C
C NOTES:

C USAGE:
C      call docnvt(qvpfile, expfile, rdffile, exp, status)
C
C ARGUMENTS:
C     qvpfile      Input QVP FITS file.
C     expfile      Input exposure history (EXP) Fits file (optional)
C     rdffile      Output RDF FITS file 
C     exp          Flag indicating exposure history provided
C     status       Status flag
C     
C
C PRIMARY LOCAL VARIABLES:
C     errtxt       Error message
C     luqvp     \
C     luexp      > Unit numbers for files
C     lurdf     /
C     blksiz       Fits file blocking factor
C
C CALLED ROUTINES:
C     subroutine fcerr  - write error message 
C     subroutine extpri - creates primary extension
C     subroutine extevt - creates events binary table extension
C     subroutine exttsi - creates telemetry xxx binary table extension 
C                         from exposure history file if available.
C     subroutine ft____ - FITSIO routines
C
C**********************************************************************
       subroutine docnvt(qvpfile, expfile, rdffile, exp, status)
C
       implicit none 
C
       character*(*) qvpfile,expfile,rdffile
       logical exp
       integer status
C
       character*(80) errtxt
       integer luqvp, luexp, luout
       integer blksiz
C
C
       EXTERNAL ftopen,
     +          ftinit,
     +          fcerr,
     +          fcerrm,
     +          ftclos
C
C======================================================================
C***** Open Files *****
C  Do not continue if input status is not zero
       if (status .ne. 0) goto 999
C
       luqvp = 11
       luexp = 12
       luout = 13
C
C  Open QVP fits file
       call ftopen(luqvp, qvpfile, 0, blksiz, status)
       if (status .ne. 0) then
         write(errtxt,'('' Error opening QVP file'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C  Open exposure file if requested
       if (exp) then
         call ftopen(luexp, expfile, 0, blksiz, status)
         if (status .ne. 0 ) then
           write(errtxt,'('' Error opening EXP file'')')
           call fcerr(errtxt)
           call fcerrm(status)
           goto 999
         endif
       endif
C
C  Open output file
       call ftinit(luout, rdffile, blksiz, status)
       if (status .ne. 0) then
         write(errtxt,'('' Error opening output file'')')
         call fcerr(errtxt)
         call fcerrm(status)
         goto 999
       endif
C
C----------------------------------------------------------------------
C***** Primary Extension *****
C
       call extpri(luqvp,luout,status)
       if (status .ne. 0) then
         write(errtxt,'('' Error returned from extpri'')')
         call fcerr(errtxt)
         goto 900
       endif
C      
C======================================================================
C*** EVENTS EXTENSION ***
C
       call extevt(luqvp,luout,qvpfile,status)
       if (status .ne. 0) then
         write(errtxt,'('' Error returned from extevt'')')
         call fcerr(errtxt)
         goto 900
       endif
C
C======================================================================
C*** TSI EXTENSION ***
C  When an exposure file is specified, add second extension 
C  table to end of RDF file providing this information.
       if (exp) then 
         call exttsi(luqvp,luexp,luout,expfile,status)
         if (status .ne. 0) then
           write(errtxt,'('' Error returned from exttsi'')')
           call fcerr(errtxt)
           goto 900
         endif
       endif
C
C======================================================================
C*** END PROGRAM ***
C
 900   continue
       call ftclos(luqvp, status)
       if (exp) call ftclos(luexp, status)
       call ftclos(luout, status)

 999   continue
       end
