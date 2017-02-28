*+CALDB_INFO
        subroutine caldb_info(chatter, mode,missn, inst, ierr)

        IMPLICIT NONE
        character*(*) missn,inst
        character*(*) mode
        integer chatter, ierr
c 
c Description:
c  The subroutine checks that the CALDB is accessible to the user. If 
c NOT, then an error message is written to STDERR (irrespective of chatter)
c and IERR is returned as non-zero. If a CALDB is accessible, then depending 
c upon the chatter & mode flags, various levels of information are written 
c to STDOUT (nothing if chatter=0.0)
c 
c Passed parameters
c  CHATTER       i   : (int) Chattiness flag (0 for silent running)
c  MODE          i   : (int) Mode flag (for future use)
c  MISSN         i   : (str) Mission string
c  INSTR         i   : (str) Instrument name
c  IERR            r : (int) Error flag (zero, if CALDB appears set-up)
c
c Called Routines:
c  subroutine CTRLOG     : (CALLIB) Translates system logical/envar
c  subroutine WTINFO     : (CALLIB) Writes callib/roslib info to STDOUT
c  subroutine WTERRM     : (CALLIB) Writes callib/roslib error message
c
c Compilation & Linking
c  link with FITSIO & CALLIB & FTOOLS
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c  Ian M George     (1.1.0: 1996 Feb 06) also checks for CALDBALIAS
c  Lorraine Breedon (1.2.0: 1997 Aug 15) gets rid of instrument 'alias' problem
C                                        when calling rdcnfg routine
c  Peter D Wilson   (1.2.1: 1998 Jul 20) restore ascaarf mods (from Ian/1996),
c                                        hiding messages if caldb not setup
c                                        and chatter<0
        character(7) version
        parameter (version = '1.2.1')
*- 
c Internals 
        character(10) subname
        parameter (subname = 'caldb_info')
        integer caldblen, cnfglen, aliaslen, fcstln
        character(20)  cnfgvar, caldbvar, aliasvar
        character(160) message
        character(120) cnfgpth,caldbpth
        character(120) aliaspth
        character(120) ciffil, datadir

c Initialize
        caldbvar = 'CALDB'
        cnfgvar = 'CALDBCONFIG'
        aliasvar = 'CALDBALIAS'
        ierr = 0


c Give user info if requested
         message = ' using '//subname//' '//version
         call wtinfo(chatter,20,1,message)

c See if the user has set the environment variable specified in the
c CALDBVAR argument
        call ctrlog(caldbvar,fcstln(caldbvar),caldbpth,caldblen)
        if (caldblen .eq. 0) then
           if (chatter.ge.0) call wterrm(subname, version,
     &          'CALDB environment variable not set')
           message = 'The environ-var/logical CALDB must'//
     &          ' be set to point to the top of the Caldb'//
     &          ' directory structure'
           call wtinfo(chatter,5,1, message)
           message = 'See the Caldb Users Guide (CAL/GEN/94-002)'//
     &          ' for details'
           call wtinfo(chatter,5,2,message)
           ierr = 2
           goto 999
        else
           call wtinfo(chatter,10,2,
     &          'environ-var/logical CALDB defined')
           message = '  CALDB path = '//caldbpth(:caldblen)
           call wtinfo(chatter,15,3,message)
        endif
c See if the user has set the environment variable specified in the
c CNFGVAR argument
        call ctrlog(cnfgvar,fcstln(cnfgvar),cnfgpth,cnfglen)
        if (cnfglen .eq. 0) then
           call wterrm(subname, version,
     &          'CALDBCONFIG environ-var/logical not set')
           message = 'The environment variable/logical CALDBCONFIG '//
     &          ' must be set to point your local Caldb configuration'//
     &          ' file'
           call wtinfo(chatter,5,1, message)
           message = 'See the Caldb Users Guide (CAL/GEN/94-002)'//
     &          ' for details'
           call wtinfo(chatter,5,2,message)
           ierr = 2
           goto 999
        else
           call wtinfo(chatter,10,2,
     &          'environ-var/logical CALDBCONFIG defined')
           message = '  CALDBCONFIG file = '//cnfgpth(:cnfglen)
           call wtinfo(chatter,15,3,message)
        endif

c See if the user has set the environment variable specified in the
c ALIASVAR argument
        call ctrlog(aliasvar,fcstln(aliasvar),aliaspth,aliaslen)
        if (aliaslen .eq. 0) then
           call wterrm(subname, version,
     &          'CALDBALIAS environ-var/logical not set')
           message = 'The environment variable/logical CALDBALIAS '//
     &          ' must be set to point your local Caldb alias'//
     &          ' file'
           call wtinfo(chatter,5,1, message)
           message = 'See the Caldb Users Guide (CAL/GEN/94-002)'//
     &          ' for details'
           call wtinfo(chatter,5,2,message)
           ierr = 2
           goto 999
        else
           call wtinfo(chatter,10,2,
     &          'environ-var/logical CALDBALIAS defined')
           message = '  CALDBALIAS file = '//aliaspth(:aliaslen)
           call wtinfo(chatter,15,3,message)
        endif

c Now perform mode-specific checks....
        if(mode.eq.'INST') then
          if (inst(:fcstln(inst)).eq.'xrt'.and.missn.eq.'ASCA') then
              inst='xrt1'
          elseif (inst(:fcstln(inst)).eq.'sis'.and.missn.eq.'ASCA') then
               inst='sis0'
          elseif (inst(:fcstln(inst)).eq.'gis'.and.missn.eq.'ASCA') then
             inst='gis2'
          elseif (inst(:fcstln(inst)).eq.'pspc'.and.missn.eq.'ROSAT')
     &       then
             inst='pspcb'
          endif
   
           call rdcnfg(missn,inst,.false.,ciffil,datadir,ierr)
           if(ierr.ne.0) then
              message = 'CALDB NOT correctly configured for the '//
     &             inst(:fcstln(inst))//' instrument onboard '//
     &             missn(:fcstln(missn))
              call wterrm(subname, version,message)
              goto 999
           else
              message = 'CALDB is configured for the '//
     &             inst(:fcstln(inst))//' instrument onboard '//
     &             missn(:fcstln(missn))
              call wtinfo(chatter,10,2,message)
              message = 'Cal Index File: '//ciffil(:fcstln(ciffil))
              call wtinfo(chatter,15,3,message)
               message = 'Data directory: '//datadir(:fcstln(datadir))
              call wtinfo(chatter,15,3,message)
           endif


        endif


999     if(ierr.ne.0) then
           if (chatter.ge.0) call wterrm(subname, version, 
     &          'Serious problem with CALDB set-up')
        endif

        return
        end
