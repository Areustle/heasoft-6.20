c
c $Id: xsel_read.f,v 3.37 2016/02/22 21:19:09 kaa Exp $
c
C $Log: xsel_read.f,v $
C Revision 3.37  2016/02/22 21:19:09  kaa
C Improved diagnostic output and changed to handle tabs in xselect.mdb.
C
C Revision 3.36  2015/10/27 19:30:43  kaa
C Changed CHARACTER declaration to remove those in obsolete format.
C
C Revision 3.35  2013/05/21 19:08:48  irby
C Change character*n to character(n) to silence warnings: "Obsolescent
C feature: Old-style character length".
C
C Revision 3.34  2012/03/23 19:20:15  kaa
C Fixes for the problem that xselect has a tendency to go into an infinite loop
C if a command is given the wrong argument. Now it nicely passes the user back
C to the main prompt so they can try again.
C
C Revision 3.33  2011/08/18 20:01:09  kaa
C Lots of tidying up, mainly to remove compiler warnings.
C
C Revision 3.32  2009/04/02 20:12:35  kaa
C Fixed typo in warning message.
C
C Revision 3.31  2009/03/03 15:47:33  kaa
C Fixed bug in handling submissions - I think this only matters for Chandra
C grating data.
C
C Revision 3.30  2007/04/27 03:23:49  kaa
C Change requested by Suzaku XIS team to allow multiple extensions in HK files.
C The read hk command now allows an extension to be given eg filename+3. This
C entire name plus extension is passed to maketime when doing a select hk.
C
C Revision 3.29  2006/09/11 15:37:44  kaa
C Increased flexibility when reading event files - now both read and choose will
C work correctly for gti extensions called either GTI or STDGTI regardless of the
C setting of the gti setting in xselect.mdb.
C
C Revision 3.28  2005/09/14 19:00:37  kaa
C Fixed error that datamode keyword was not allowed to be different for
C different instruments.
C
C Revision 3.27  2005/05/20 22:50:16  kaa
C Fixed so that case-insensitivity to name of mission, instrument, etc.
C
C Revision 3.26  2004/11/02 15:57:04  kaa
C Replaced getting MJDREF from the MDB by reading MJDREF[F/I] and TIMESYS from
C the event file. These are used when time filtering based on UTC or MJD ranges.
C Now includes conversion from UTC to TT if TIMESYS='TT'.
C
C Revision 3.25  2003/08/08 02:21:52  kaa
C Fixed bug in the xsl_getkw* routines. The option to search all extensions after that
C specified when reading a keyword did not work. Also ensured that the value of timedel
C read from an event file was not reset by the routines that set up the mission/instrument/
C datamode if this is the first event file.
C
C Revision 3.24  2003/08/07 17:06:46  kaa
C Rationalized the reading of the MDB standard parameters when setting mission,
C instrument, or datamode. This ensures that any standard parameter can be changed
C at any level. This does require that all standard parameters are defined at the
C mission level in the MDB file.
C
C Revision 3.23  2002/03/27 20:04:26  kaa
C Added support for the extractor ccol parameter. Also removed error if the GTI cannot
C be found because if data subspace keywords are in use we can find GTIs that way.
C Probably need to improve this. Also still need to rework the select chip command.
C
C Revision 3.22  1999/12/05 04:19:15  kaa
C Made the program a little more forgiving of attempts to mix possibly
C incompatible instruments. Now only writes a strongly worded warning
C instead of removing files.
C
C Revision 3.21  1999/07/17 22:20:49  kaa
C Added some warning comments about resetting the submission
C
C Revision 3.20  1999/05/02 00:16:36  ngan
C New xselect(put a & at the end of plotting commands).
C
c Revision 3.18  1998/08/27  19:09:28  kaa
c Fixed unwanted case sensitivity
c
C Revision 3.17  1998/08/25 21:26:43  kaa
C Cosmetic changes
C
C Revision 3.16  1998/06/16 15:49:14  kaa
C Fixed bug that led to case-dependency in comparing current datamode and that
C in a new file.
C
C Revision 3.15  1998/05/11 22:08:53  kaa
C No longer require an @ in front of the filename of a list of files for the
C read command. Also tidied up command matching so it works through a single
C subroutine.
C
C Revision 3.14  1998/05/08 16:34:35  kaa
C Initial changes for XTE support
C
C Revision 3.13  1997/11/24 23:42:32  kaa
C Major overhaul to create v1.4. Most of the mission-dependence is switched out
C into the ascii file xselect.mdb. All other mission-dependent code is in the
C file xsel_mission.f.
C
C Revision 3.12  1997/06/30 19:29:56  kaa
C Do not reset gtinam if the GTI is called STDGTI rather than that set as the mission keyword, this is now handled by extractor
C
C Revision 3.11  1997/05/16 19:35:24  kaa
C Fixed format problem with negative values of dec
C
C Revision 3.10  1997/04/17 23:00:40  kaa
C Cosmetic changes to eliminate g77 compilation warnings
C
c Revision 3.9  1997/02/18  22:21:58  kaa
c Fix to allow files with different ROSAT RDF versions to be used together
c
c Revision 3.8  1997/02/12  19:57:29  kaa
c Removed checking of ASCA processing version since it is no longer necessary but messes up for rev1+
c
c Revision 3.7  1996/12/23  18:11:50  kaa
c Added improved error checking and fixed erroneous running of fixrev0pha for ASCA MPC mode
c
c Revision 3.6  1996/11/26  22:15:30  kaa
c Added helpful message if read events does not find any files
c
c Revision 3.5.1.1  1996/04/16  01:54:35  dunfee
c Start of pristine ftools CVS...
c
c Revision 1.5  1995/10/20  18:04:31  oneel
c Added a test for ROSAT around the xsl_findext call
c
c Revision 1.4  1995/08/22  17:59:45  oneel
c Forgot to declare and set rwmode in xsl_findext.
c
c Revision 1.3  1995/08/18  17:31:20  oneel
c Opps, xsel_findext was returning an extension number that was one
c greater than it should
c
c Revision 1.2  1995/08/18  15:36:57  oneel
c Added xsl_findext to find the events extension and xsl_ystcls to
c remove leading spaces.  This fixes the case where you use an old style
c rosat file, do something like a bin events or a bct and then try to
c work with it.  Xselect thinks that the events extension is the third,
c when it might now be the second extension etc...
c

c
c ---------------------------------------------
      subroutine XSL_READ()
c ---------------------------------------------
c 
c Called by XSELECT main
c
c     Alan Smale 1992 Nov
c     Jim Ingham 1993 Jan ->

      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(512) str1
c File I/O unit numbers
      
c General reusable integers for lengths of strings 
      integer len1, len2
c ---------------------------------------------
      integer i,diflen,LENACT,nadded,nold,myindex(MAXFIL),statv(4),j
      character(80) hkext,rdmode,kwdval(4),newval(4)
      character(72) kwdnam(4)
      character(512) contxt
      character(255) temp,vtemp(MAXFIL)

c ----------------------------------------
c newval(1) = the new mission
c newval(2) = the new instrument
c newval(3) = the new datamode 
c newval(4) = the new submission
c ----------------------------------------

      integer MXCMDS
      parameter (MXCMDS = 20)
      double precision tmpdel
      character(64) commad(MXCMDS),comdes(MXCMDS)
      integer nwhats,indx,extno,tmpstat
      integer rbnvec(MAXFIL)
      logical QUIET,MAKEOBS
      logical RESET_MISS, RESET_SUBM, RESET_INST, RESET_MODE

      data commad /'DEFAULT',
     &     'HK',
     &     'EVENTS',
     &     17*'NA'/
      data comdes /'default linking of event and hk names',
     &     'enter only HK files',
     &     'enter only event files',
     &     17*'NA'/

      data nwhats / 3 /

      CALL xsl_match_cmd('read_what', commad, comdes, nwhats, MXCMDS,
     &                   comno, rdmode, indx, status)

      if(status.ne.0) then
         status = 0
         return
      endif

      call xsl_uclgsb('quiet',QUIET,status)
      IF ( status .NE. 0 ) RETURN
      
c ----------------------------------------------------
c     rdmode: h = hk readin, no linking with SF
c
      IF(indx.eq.2) then
         if(.not. HKREAD) then
C            call xsl_uclgst('data_dir',str1,status)
 26         call xsl_uclgst('vishk_dir',hkdir,status)
            IF ( status .NE. 0 ) RETURN
C            call XSL_RELDIR(datdir,str1,hkdir,status)
            call XSL_CHKDIR(hkdir,status)
            if(status.eq.-10) then
               status = 0
               goto 26
            else if ( status .ne. 0) then
               len1 = LENACT(hkdir)
               str1 = 'Directory '//hkdir(1:len1)//' doesn''t exist'
               call XWRITE(str1,5)
               return
            endif
         endif
         call xsl_uclgst('hkfiles',temp,status)
         if(status.ne.0) then
            call XWRITE('Error getting hkfiles parameter',5)
            status = 0
            return
         endif
         status = 0
         call xsl_uclgsb('quiet',QUIET,status)
         IF ( status .NE. 0 ) RETURN

         call XSL_RDSTRING(temp,nhkfil,hkflnm,nadded,
     &        hkdir,0,QUIET,.FALSE.)

         IF(nadded.gt.0) then
c Now set the read logicals
            if(nhkfil.ge.1) HKREAD = .TRUE.
            if(nhkfil.gt.1) MANYHK = .TRUE.
            
c If we are adding files, we still need to reset some logicals
            FILTER = .FALSE.
            MERGHK = .FALSE.
            WORKHK = .FALSE.
c Now find out whether the files are expanded or not

            call xsl_uclgsb('expand',EXPAND,status)
            IF ( status .NE. 0 ) RETURN
         ENDIF
c ---------------------------------------------------------------
c  indx = 3: readin just the SF and GTI files
c This is where the mission and instrument ID is done...
         
      ELSE IF(indx.eq.3.or.indx.eq.1) THEN

         if(.not.READ) then
c This resets the datadir if a directory was entered on the command line.
            call XSL_SET_DIR('data_dir',datdir,status)
            if (status.ne.0) then
               return
            endif
         endif
         call xsl_uclgst('infiles',temp,status)
         if(status.ne.0) then
            call XWRITE('Error getting infiles parameter',5)
            status = 0
            return
         endif
         nold = nfiles
         nfiles = 0

c READ the files into a temp space:

         call XSL_RDSTRING(temp, nfiles, vtemp, nadded, datdir, 0, 
     &                     .FALSE.,.TRUE.)

         IF ( nadded .EQ. 0 ) THEN
            CALL XWRITE('No files read', 5)
            RETURN
         ENDIF

         timedel = -1.d0

c Now we will type the files, and use the keywords to get the
c instrument and mission info

         do i = 1, nfiles

c First check for MISSION and TELESCOP keywords

            kwdnam(1) = 'MISSION'
            kwdnam(2) = 'TELESCOP'
            call XSL_GETKWST(vtemp(i),'0',datdir,kwdnam,kwdval,
     &           statv,2,2,.TRUE.,status)
            contxt = 'Could not get MISSION or TELESCOP '//
     &               'from file: '//vtemp(i)
            IF ( status .NE. 0 ) GOTO 100

c If MISSION was read then use this to set the mission in use, otherwise
c use TELESCOP

            IF ( statv(1) .NE. 0 ) THEN
               kwdnam(1) = kwdnam(2)
               kwdval(1) = kwdval(2)
               statv(1) = 0
            ENDIF

c Now get the other keywords to be used. Note that we do these in descending
c order in case eg the datamode keyword depends on the instrument in use.
c Ordering in the kwdnam array is a bit strange for historical reasons.

            CALL XSL_MDBS(kwdval(1), ' ', ' ', ' ', 'submkey', 
     &                    kwdnam(4), status)
            contxt = 'Could not find MDB entry for submkey for '//
     &               kwdval(1)(:lenact(kwdval(1)))
            IF ( status .NE. 0 ) GOTO 100

            IF ( kwdnam(4) .EQ. 'NONE' ) THEN
               kwdval(4) = 'NONE'
            ELSE
               call XSL_GETKWST(vtemp(i),'0',datdir,kwdnam(4),kwdval(4),
     &              statv(4),1,1,.TRUE.,status)
               contxt = 'Could not get '//kwdnam(4)(:lenact(kwdnam(4)))
     &                  //' from file: '//vtemp(i)(:lenact(vtemp(i)))
               IF(status.ne.0) GOTO 100
               IF ( statv(4)  .NE. 0 ) kwdval(4) = 'NONE'
            ENDIF

            CALL XSL_MDBS(kwdval(1), kwdval(4), ' ', ' ', 'instkey', 
     &                    kwdnam(2), status)
            contxt = 'Could not find MDB entry for instkey for '//
     &               kwdval(1)(:lenact(kwdval(1)))//' '//
     &               kwdval(4)(:lenact(kwdval(4)))
            IF ( status .NE. 0 ) GOTO 100

            call XSL_GETKWST(vtemp(i),'0',datdir,kwdnam(2),kwdval(2),
     &           statv(2),1,1,.TRUE.,status)
            contxt = 'Could not get '//kwdnam(2)(:lenact(kwdnam(2)))//
     &               ' from file: '//vtemp(i)(:lenact(vtemp(i)))
            IF(status.ne.0) GOTO 100
            IF ( statv(2)  .NE. 0 ) kwdval(2) = 'NONE'

            CALL XSL_MDBS(kwdval(1), kwdval(4), kwdval(2), ' ', 
     &                    'dmodekey', kwdnam(3), status)
            contxt = 'Could not find MDB entry for dmodekey for '//
     &               kwdval(1)(:lenact(kwdval(1)))//' '//
     &               kwdval(4)(:lenact(kwdval(4)))//' '//
     &               kwdval(2)(:lenact(kwdval(2)))
            IF ( status .NE. 0 ) GOTO 100

            call XSL_GETKWST(vtemp(i),'0',datdir,kwdnam(3),kwdval(3),
     &           statv(3),1,1,.TRUE.,status)
            contxt = 'Could not get '//kwdnam(3)(:lenact(kwdnam(3)))//
     &               ' from file: '//vtemp(i)(:lenact(vtemp(i)))
            IF(status.ne.0) GOTO 100
            IF ( statv(3)  .NE. 0 ) kwdval(3) = 'NONE'
                  
c If we don't find a value, set it to 'NONE' and continue 
c We will allow 'NONE' to pass most of the tests, since some
c missions don't have all the keywords.

            do j=1,4
               CALL upc(kwdval(j))
               if(statv(j).ne.0) THEN
                  kwdval(j) = 'NONE'
               endif
               if(i.eq.1) then
                  newval(j) = kwdval(j)
                  CALL upc(newval(j))
               else if(kwdval(j).ne.newval(j)
     &                 .and.kwdval(j).ne.'NONE'
     &                 .and.newval(j).ne.'NONE') THEN
                  str1 = '*************************************'//
     &                   '*************************************'
                  call XWRITE(str1,5)
                  str1 = 'Warning ! The files you are entering '//
     &                   'have diverse values for '//kwdnam(j)
                  call XWRITE(str1,5)
                  str1 = 'One file has '//
     &                   kwdval(j)(:lenact(kwdval(j)))//
     &                   ' and another '//newval(j)
                  call XWRITE(str1,5)
                  call XWRITE(
     &              'I hope you know what you are doing.',5)
                  str1 = '*************************************'//
     &                   '*************************************'
                  call XWRITE(str1,5)
               endif
            enddo

C Now check the TIMEDEL Keyword, keep the max.

            call XSL_GETKWD(vtemp(i),'0',datdir,'TIMEDEL',
     &                      tmpdel,tmpstat,1,1,.TRUE.,status)

            if(tmpstat.eq.0.and.status.eq.0) then
               timedel = max(tmpdel,timedel)
            else
               status = 0
               tmpstat = 0
            endif

C Read the TIMESYS and MJDREF(F/I) keywords

            CALL XSL_GETKWST(vtemp(i),'0',datdir,'TIMESYS',timesys,
     &                       tmpstat,1,1,.TRUE.,status)
            tmpstat = 0
            status = 0

            mjdrefi = 0
            mjdreff = 0.0d0
            call XSL_GETKWD(vtemp(i),'0',datdir,'MJDREF',
     &                      mjdreff,tmpstat,1,1,.TRUE.,status)
            IF ( tmpstat .EQ. 0 .AND. status .EQ. 0 ) THEN
               mjdrefi = INT(mjdreff)
               mjdreff = mjdreff - mjdrefi
            ELSE
               tmpstat = 0
               status = 0
               call XSL_GETKWD(vtemp(i),'0',datdir,'MJDREFF',
     &                         mjdreff,tmpstat,1,1,.TRUE.,status)
               call XSL_GETKWI(vtemp(i),'0',datdir,'MJDREFI',
     &                      mjdrefi,tmpstat,1,1,.TRUE.,status)
               IF ( tmpstat .NE. 0 .OR. status .NE. 0 ) THEN
                  CALL xwrite('No MJDREF(F/I) keyword(s) found',5)
                  tmpstat = 0
                  status = 0
               ENDIF
            ENDIF

C Now check for RAWXBINS (used for ASCA GIS)

            call XSL_GETKWI(vtemp(i),'0',datdir,'RAWXBINS',
     &                         rbnvec(i),tmpstat,1,1,.TRUE.,status)
            tmpstat = 0
            status = 0

c Error from trying to get an MDB keyword

 100        CONTINUE
            IF ( status .NE. 0 ) THEN
               call XWRITE(contxt,5)
               status = 0
            ENDIF

         enddo

c Now analyse the kwd values that were found:
            
c First, check the RAWXBINS( since we will toss it out
c if these are different):

         rxbval = rbnvec(1)
         do i=2,nadded
            if ( rxbval .ne. rbnvec(i) ) rxbval = -20
         enddo
         IF( rxbval .eq. -20 ) THEN
            call XWRITE('The files you have entered have'//
     &                  ' diverse values of RAWXBINS:',5)
            rxbval = 0
            do i=1,nadded
               rxbval = max(LENACT(vtemp(i)),rxbval)
            enddo
            do i=1,nadded
               write(str1,599) vtemp(i)(:rxbval), rbnvec(i) 
 599           format(a,':  ',i5)
               call XWRITE(str1,5)
            enddo
            call XWRITE('These files cannot be analysed'//
     &           ' together.',5)
            call XWRITE('No files added',5)
            return
         ENDIF
         IF ( rxbval .LT. 0 ) rxbval = 0

c FIRST do the case where the  kwd values were not found
c Substitute the set values, if there are any, otherwise bail out.

         IF(newval(1).eq.'NONE'.and.keymis.eq.'NONE') THEN
            call XWRITE(' ',5)
            call XWRITE('*****************************',5)
            call XWRITE('No mission was found in the files,'
     &           ,5)
            call XWRITE('and none was set before the read.',
     &           5)
            call XWRITE('Use SET MISSION to set the mission',
     &           5)
            call XWRITE('and then do the read again',5)
            call XWRITE('*****************************',5)
            call XWRITE(' ',5)
            call XWRITE('No files added',5)
            nfiles = nfiles - nadded
            return
         ELSE IF (newval(1).eq.'NONE') THEN
            newval(1) = keymis
         ENDIF
         IF(newval(2).eq.'NONE') THEN
            IF(instru.eq.'NONE') THEN
               call XWRITE(' ',5)
               call XWRITE('*****************************',5)
               call XWRITE('No instrument was found '//
     &              'in the files,',5)
               call XWRITE('and none was set before '//
     &              'the read.',5)
               call XWRITE('Use SET INSTRUMENT to '//
     &              'set the instrument',5)
               call XWRITE('and then do the read again',5)
               call XWRITE('*****************************',5)
               call XWRITE(' ',5)
               call XWRITE('No files added',5)
               nfiles = nfiles - nadded
               return
            ELSE
               newval(2) = instru(:min(lenact(instru),len(newval(2))))
            ENDIF
         ELSE IF (LENACT(newval(2)).eq.0) THEN
            call XWRITE('Your data files have empty INSTRUME'//
     &           ' keywords in them.',5)
            call XWRITE('Use FPARKEY to set the value.',5)
            IF(instru.eq.'NONE') THEN
               call XWRITE('Use SET INSTRUMENT to set the'//
     &              ' instrument, and then try the read again.',5)
               call XWRITE(' ',5)
               call XWRITE('No files added',5)
               nfiles = nfiles - nadded
               return                  
            ENDIF
         ENDIF
         IF(newval(3).eq.'NONE') THEN
            CALL XSL_MDBS(newval(1), newval(4), newval(2), ' ', 
     &                    'modes', str1, status)
            IF ( status .NE. 0 ) str1 = 'NONE'
            status = 0
            IF( datamode.eq.'NONE' .and. str1.ne.'NONE' ) THEN
               call XWRITE(' ',5)
               call XWRITE('*****************************',5)
               call XWRITE('No datamode was found '//
     &              'in the files,',5)
               call XWRITE('and none was set before '//
     &              'the read.',5)
               call XWRITE('Use SET DATAMODE to '//
     &              'set the datamode',5)
               call XWRITE('*****************************',5)
               call XWRITE(' ',5)
               call XWRITE('No files added',5)
               return
            ELSE
               newval(3) = datamode
               call XSL_SET(6)
            ENDIF
         ELSE IF ( LENACT(newval(3)) .eq. 0 ) THEN
            datamode = 'NONE'
            newval(3) = 'NONE'
         ENDIF

c NEXT do the case where the value has been found:
c Compare it with the set value, and use the found one instead.
c First check which keywords have changed and check with the
c user that they really want to do this.

         RESET_MISS = .FALSE.
         RESET_SUBM = .FALSE.
         RESET_INST = .FALSE.
         RESET_MODE = .FALSE.

         IF ( newval(1) .NE. keymis ) THEN
            IF ( keymis .NE. 'NONE' ) THEN
               temp = 'Got new mission: '//
     &                newval(1)(:LENACT(newval(1)))
               call XWRITE(temp,5)
               call xsl_uclgsb('reset_miss',RESET_MISS,status)
               IF ( status .NE. 0 ) RETURN
               IF ( .NOT.RESET_MISS ) THEN
                  call XWRITE('No files added',5)
                  nfiles = nfiles - nadded
                  GOTO 200
               ENDIF
            ENDIF
            RESET_SUBM = .TRUE.
            RESET_INST = .TRUE.
            RESET_MODE = .TRUE.
         ENDIF

         IF ( .NOT.RESET_SUBM .AND. newval(4) .NE. submis ) THEN
            IF ( submis .NE. 'NONE' ) THEN
               temp = 'Got new submission: '//
     &                newval(4)(:LENACT(newval(4)))
               call XWRITE(temp,5)
               call xsl_uclgsb('reset_subm',RESET_SUBM,status)
               IF ( status .NE. 0 ) RETURN
               IF ( .NOT.RESET_SUBM ) THEN
                  call XWRITE('No files added',5)
                  nfiles = nfiles - nadded
                  GOTO 200
               ENDIF
            ELSE
               RESET_SUBM = .TRUE.
            ENDIF
            RESET_INST = .TRUE.
            RESET_MODE = .TRUE.
         ENDIF

         IF ( .NOT.RESET_INST .AND. newval(2) .NE. instru ) THEN
            IF ( instru .NE. 'NONE' ) THEN
               temp = 'Got new instrument: '//
     &                newval(2)(:LENACT(newval(2)))
               call XWRITE(temp,5)
               call xsl_uclgsb('reset_inst',RESET_INST,status)
               IF ( status .NE. 0 ) RETURN
               IF ( .NOT.RESET_INST ) THEN
                  call XWRITE('No files added',5)
                  nfiles = nfiles - nadded
                  GOTO 200
               ENDIF
            ELSE
               RESET_INST = .TRUE.
            ENDIF
            RESET_MODE = .TRUE.
         ENDIF

         IF ( .NOT.RESET_MODE .AND. newval(3) .NE. datamode ) THEN
            IF ( datamode .NE. 'NONE' ) THEN
               temp = 'Got new datamode: '//
     &                newval(3)(:LENACT(newval(3)))
               call XWRITE(temp,5)
               call xsl_uclgsb('reset_datamode',RESET_MODE,status)
               IF ( status .NE. 0 ) RETURN
               IF ( .NOT.RESET_MODE ) THEN
                  call XWRITE('No files added',5)
                  nfiles = nfiles - nadded
                  GOTO 200
               ENDIF
            ELSE
               RESET_MODE = .TRUE.
            ENDIF
         ENDIF

c Save the current timedel since it gets reset when resetting the 
c mission/instrument/datamode

         tmpdel = timedel

c Now make the changes. The shuffling around is because XSL_SET resets
c the datdir and number of files. 

         IF ( RESET_MISS ) THEN

            keymis = newval(1)
            submis = newval(4)
            nold = nfiles
            str1 = datdir                     
            call XSL_SET(4)
            nfiles = nold
            datdir = str1(:min(lenact(str1),len(datdir)))

         ENDIF

c Note that resetting the submission doesn't do anything at the moment.
c This needs improving and the whole concept of the submission thought 
c through.

         IF ( RESET_SUBM ) THEN

            submis = newval(4)

         ENDIF

         IF ( RESET_INST ) THEN

            instru = newval(2)
            nold = nfiles
            str1 = datdir                     
            call XSL_SET(5)
            nfiles = nold
            datdir = str1(:min(lenact(str1),len(datdir)))

         ENDIF

         IF ( RESET_MODE ) THEN

            datamode = newval(3)(:min(lenact(newval(3)),len(datamode)))
            nold = nfiles
            str1 = datdir                     
            call XSL_SET(6)
            nfiles = nold
            datdir = str1(:min(lenact(str1),len(datdir)))

         ENDIF

c Set timedel again

         timedel = tmpdel

c Tell the user what we have just done

         call XWRITE(' ',5)
         write(str1,'(1x,a,a20)') 'Notes: XSELECT set up for      ', 
     &                            keymis(1:20)
         call XWRITE(str1,5)

         WRITE(str1,'(a,a10,a,a10)') ' Time keyword is ', 
     &      keytim(1:10), ' in units of ', keyuni(1:10)
         call XWRITE(str1,5)
         write(str1,'(1x,a,g12.5)') 'Default timing binsize = ', binsiz
         call XWRITE(str1,5)

         call XWRITE(' ',5)
         write(str1,'(a)') 'Setting...'
         call XWRITE(str1,5)
         IF ( xcolf .NE. 'NONE' .AND. ycolf .NE. 'NONE' ) THEN
            write(str1,'(a,a10,1x,a10,1x,a,i4)') 
     &        ' Image  keywords   = ', xcolf(1:10), ycolf(1:10), 
     &        ' with binning = ', xbinf
            call XWRITE(str1,5)
         ENDIF
         IF ( xcolh .NE. 'NONE' .AND. ycolh .NE. 'NONE' ) THEN
            write(str1,'(a,a10,1x,a10,1x,a,i4)') 
     &        ' WMAP   keywords   = ', xcolh(1:10), ycolh(1:10), 
     &        ' with binning = ', extrbinh
            call XWRITE(str1,5)
         ENDIF
         IF ( keypha .NE. 'NONE' ) THEN
            write(str1,'(a,a21,1x,a,i4)') 
     &        ' Energy keyword   = ', keypha(1:21),
     &        ' with binning = ', phabin
            call XWRITE(str1,5)
         ENDIF

c set the number of files

         nadded = nfiles
         IF ( RESET_MISS .OR. RESET_SUBM .OR. RESET_INST .OR. 
     &        RESET_MODE ) THEN
            nold = 0
            do i=1,nfiles
               filenm(i) = vtemp(i)
            enddo
         ELSE
            nfiles = nold + nadded
            do i= 1,nadded
               filenm(i+nold) = vtemp(i)
            enddo
         ENDIF
 200     CONTINUE

         if(nfiles.gt.0) THEN
            READ = .TRUE.
         endif

c For display reasons, set the catidx to integer sequence:
         do i=nfiles-nadded+1,nfiles
            catidx(i) = i
         enddo

c Next get the extension number.
         call XSL_GET_EXTNO(filenm(1), datdir, evtnam,
     &                      extno, evtnum, status)
         if ( status .ne. 0 ) then
c Okay, this may be extractor data, so try EVENTS:
            i = 0
            call XSL_GET_EXTNO(filenm(1),datdir,'EVENTS',
     &                         extno,evtnum,i)
            if(i.eq.0) THEN
               status = 0
               evtnam = 'EVENTS'
            ELSE
c Otherwise bag it and look in the primary header for keywords.
c Boy, talk about punting...
               status = 0
               evtnum = '1'
            ENDIF
         endif
            
c Now look in the files to get the TLMIN and TLMAX for the chosen
c energy column, and set the phalcut and phahcut to this:

         call XSL_CHKPHASIZE(0)

c Remember that this CHKPHASIZE can remove files, so we may need to
c reset variables:
         if(nfiles.eq.0) then
            READ = .FALSE.
            call XWRITE('No files added',5)
            return
         else if(nfiles.gt.1) then
            MANY = .TRUE.
         endif
c  If we are adding files, we still have to reset some logicals

         WRKGTI = .FALSE.
         SPEC   = .FALSE.
         CURV   = .FALSE.
         IMAGE  = .FALSE.
         SELCT  = .FALSE.
         MERGED = .FALSE.
         WORK   = .FALSE.
            
         status = 0

c Now check TIMEDEL against the binsiz and warn if greater:
            
         if(timedel.gt.0) then
            write(str1,551)timedel
 551        format('Got the minimum time resolution of the',
     &           ' read data: ',g12.5)
            call XWRITE(str1,5)
            if(timedel.gt.binsiz) then
               write(str1,552) binsiz
 552           format('This is greater than the chosen binsize: ',
     &              e12.5)
               call XWRITE(str1,5)
               write(str1,553) timedel
 553           format('Setting binsize =  ',
     &              e12.5) 
               call XWRITE(str1,5)
               binsiz = SNGL(timedel)
            endif
         else
            call XWRITE(
     & 'could not get minimum time resolution of the data read',5)
         endif

         write(str1,'(a,1pe20.13,a,a)') 'MJDREF = ', (mjdrefi+mjdreff),
     &    ' with TIMESYS = ', timesys(:lenact(timesys))
         CALL xwrite(str1, 5)

         if(rxbval.lt.256.and.rxbval.gt.0) then
            write(str1,555) rxbval
 555        format('Got RAWXBINS = ',i4,
     &           ', setting WMAP binning to 1')
            call XWRITE(str1,5)
            extrbinh = 1
         endif

c  If the GTI extension name is NONE then no GTIs are in use so set
c  the flag appropriately

         USEGTI = .TRUE.
         IF ( gtinam .EQ. 'NONE' ) USEGTI = .FALSE.

c  Now construct the filenames.  It is assumed that the files have common
c  stems, and that only the endings vary.  So for two extensions in the 
c  same file, the endings are [1] and [2], but they could be different
c  extensions.

         IF(USEGTI) THEN

            call XSL_GET_EXTNO(filenm(1),datdir,gtinam,
     &           extno,gtinum,status)

c If we can't find gtiname try STDGTI and GTI as a possible name for the 
c gti extension

            if(status.ne.0) then
               status = 0
               call XSL_GET_EXTNO(filenm(1),datdir,'STDGTI',
     &              extno,gtinum,status)
            endif
            if(status.ne.0) then
               status = 0
               call XSL_GET_EXTNO(filenm(1),datdir,'GTI',
     &              extno,gtinum,status)
            endif

c none of the options worked so give up

            if ( status .ne. 0 ) then
               str1 = 'Could not find the GTI extension: '//
     &                 gtinam(:LENACT(gtinam))//
     &                 ' in the input files'
               call XWRITE(str1,15)
                USEGTI = .FALSE.
               status = 0
            endif                     
         ENDIF
               
         IF(USEGTI) THEN
            do i=nold+1,nfiles
               gtifnm(i) = filenm(i)(1:LENACT(filenm(i)))
     &              //'+'//gtinum
            enddo
         ENDIF

         IF(indx.eq.1) THEN
            if(.not. HKREAD) then
 29            call xsl_uclgst('hk_dir',str1,status)
               IF ( status .NE. 0 ) RETURN
               call XSL_RELDIR(datdir,str1,hkdir,status)
               if(status .eq. -10 ) then
                  call XWRITE('Remember to enter '//
     &                 'relative path',5)
                  status = 0
                  goto 29
               else if ( status .ne. 0) then 
                  len1 = LENACT(hkdir)
                  str1 = 'HK Directory '//hkdir(1:len1)//
     &                 ' doesn''t exist'
                  call XWRITE(str1,5)
                  return
               endif
            endif
            call xsl_uclgst('hkext',hkext,status)
            IF ( status .NE. 0 ) RETURN
            call xsl_uclgsi('hkdifflen',diflen,status)
            IF ( status .NE. 0 ) RETURN
            do i=nhkfil+1,nfiles
               str1 = filenm(i)
               len1 = LENACT(str1)
               vtemp(i-nhkfil) = str1(1:len1-diflen)//hkext
            enddo
            len2 = LENACT(datdir)
            call XSL_RDVECT(vtemp,nfiles-nhkfil,nhkfil,
     &           hkflnm,myindex, len1,hkdir,len2,QUIET)

C   Clear the HKWRK1 array
               
            If(nhkfil.ne.nfiles) then
               call XWRITE('Fewer HK than event files',5)
               call XWRITE('To use HK files, clear and '//
     &              'start over',5)
               HKREAD = .FALSE.
               MANYHK = .FALSE.
            else          
               HKREAD = .TRUE.
               IF(nhkfil.gt.1) MANYHK = .TRUE.
               MERGHK = .FALSE.
               WORKHK = .FALSE.
               FILTER = .FALSE.
               call xsl_uclgsb('expand',EXPAND,status)
               IF ( status .NE. 0 ) RETURN
            endif
         ENDIF
         
         write(str1,*)'Number of files read in: ',nfiles
         call XWRITE(str1,5)

c Now make the temporary catalogue:
         call xsl_uclgsb('make_obscat',MAKEOBS,status)
         IF ( status .NE. 0 ) RETURN

         if(nfiles.gt.0 .and. MAKEOBS) then
            call XSL_MAKEO(1)
c This doesn't have the name of a MADE catalogue, so set LOADED.
            LOADED = .TRUE.
c Do any additional mission-specific set-up required.
            CALL XSL_MISS_SETUP()
         endif
      ELSE
         call XWRITE('This error should not occur', 5)
      ENDIF

      return
      end

c ---------------------------------------------
      subroutine XSL_LISTSTUFF()
c ---------------------------------------------
c This just lists the obscats without loading one.

      include 'xsel.inc'
      include 'xselvar.inc'

      character(255) str1
      integer MXCMDS
      parameter (MXCMDS = 20 )
      character(64) commad(MXCMDS),comdes(MXCMDS)
      character(64) chosen(1,4)
      integer nwhats,myindex,nchose
      logical BRIEF,ALL,ONLYDD
      
      data commad /'DATADIRECTORY',
     &            'OBSCATS',
     &            'WORKDIRECTORY',
     &            17*'NA'/
      
      data comdes /'lists the files in the data directory',
     &            'list the obscats in the work and data directories',
     &            'lists the files in the work directory',
     &            17*'NA'/
      data nwhats / 3 /

      CALL xsl_match_cmd('list_what', commad, comdes, nwhats, MXCMDS,
     &                   comno, str1, myindex, status)

      IF ( status .NE. 0 ) THEN
         status = 0
         return
      ENDIF

C ---------------------------------------------
C LIST DATADIRECTORY      
      IF(str1.eq.'DATADIRECTORY') THEN
c Get the data directory if not already set
         call XSL_SET_DIR('data_dir',datdir,status)
         if (status.ne.0) then
            return
         endif
         call XSL_LIST('NONE','*',datdir,wrkdir,status)
C ---------------------------------------------
C LIST WORKDIRECTORY      
      ELSE IF(str1.eq.'WORKDIRECTORY') THEN
         call XSL_LIST('NONE','*',wrkdir,wrkdir,status)
C ---------------------------------------------
C LIST OBSCATS      
      ELSE IF(str1.eq.'OBSCATS') THEN
         call xsl_uclgsb('brief',BRIEF,status)
         IF ( status .NE. 0 ) RETURN
         call xsl_uclgsb('all',ALL,status)
         IF ( status .NE. 0 ) RETURN
         call xsl_uclgsb('only_datadir',ONLYDD,status)
         IF ( status .NE. 0 ) RETURN
         call XSL_SHOWCATS(0,str1,chosen,nchose,1,ONLYDD,BRIEF,ALL)
      ENDIF

      return
      end

c
c
c ---------------------------------------------
      subroutine XSL_LOAD()
c ---------------------------------------------
c This loads obscats at present.  
c J. Ingham 1/94
      include 'xsel.inc'
      include 'xselvar.inc'
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(255) str1
c File I/O unit numbers
      
c General reusable integers for lengths of strings 
c      integer len1, len2
c ---------------------------------------------      
      integer MXCMDS
      parameter (MXCMDS = 20 )
      character(64) commad(MXCMDS),comdes(MXCMDS)
      integer nwhats,myindex
c      integer LENACT

      
      data commad /'OBSCAT',
     &     'SESSION',
     &     18*'NA'/
      data comdes /'loads an OBSCAT for data selection',
     &     'loads a saved session',
     &     18*'NA'/
      
      data nwhats / 2 /

      CALL xsl_match_cmd('load_what', commad, comdes, nwhats, MXCMDS,
     &                   comno, str1, myindex, status)

      IF ( status .NE. 0 ) THEN
         status = 0
         return
      ENDIF

c ------------------------------------------------------------
c LOAD OBSCAT
      IF(str1.eq.'OBSCAT') THEN
         call XSL_LOADCAT()
      ELSE IF(str1.eq.'SESSION') THEN
         call XWRITE('LOAD SESSION not supported yet.',5)
      ELSE
         call XWRITE('LOAD option not recognized',5)
      ENDIF

      return
      end
c
c

*********************
* xsl_findext finds an extension given it's name
*
      subroutine xsl_findext (extension, filename, number)

      character*(*) extension, filename, number

      character(20) extname, testextname
      character(200) comment
      integer lun
      integer inum
      integer rwmode
      integer block
      integer status
      integer hdutype

      testextname = extension
      call upc(testextname)
      call getlun(lun)
      status = 0
      rwmode = 0
      call ftopen(lun,filename,rwmode,block,status)
      if (status .ne. 0) goto 999

      inum = 0
      extname = ' '
      do while (status.eq.0 .and. extname.ne.testextname)
         inum = inum + 1
         call ftmahd(lun,inum,hdutype,status)
         if (status .eq. 0) then
            call ftgkys(lun,'EXTNAME',extname,comment,status)
            call upc(extname)
            status = 0
         end if
      end do
      if (status .ne. 0) goto 999
*
* ftools has the primary array at offset 0, fitsio uses 1 for the primary array
*
      inum = inum - 1
      write (number,'(i5)')inum
      call xsl_ystcls(number)
      call ftclos(lun,status)
      call frelun(lun)
      return
* error exit
 999  continue
      status = 0
      call ftclos(lun,status)
      call frelun(lun)
      return
      end

**==xsl_ystcls.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* Removes leading spaces in a string
 
      SUBROUTINE XSL_YSTCLS(Instr)
 
      IMPLICIT NONE
 
      CHARACTER*(*) Instr
 
      INTEGER i
 
      i = 1
 
      IF ( LEN(Instr).EQ.1 ) RETURN
      IF ( Instr.EQ.' ' ) RETURN
 
      DO WHILE ( Instr(i:i).EQ.' ' .AND. i.LT.LEN(Instr) )
         i = i + 1
      ENDDO
 
      Instr = Instr(i:LEN(Instr))
 
      RETURN
      END 

