**==xpivers.spg  processed by SPAG 4.50F  at 15:18 on 26 Aug 1994
* XPI version number
 
*
* 2.0  -- Version number to start, why start with 1.0?  Version 1.0 always
*         has loads of bugs :-)
* 2.0a -- Added logical/environment variable for debugging
* 2.0b -- Fixed bug with leading spaces in command
* 2.0c -- Built in commands now work!
* 2.0d -- Fix bug in tbldpr.  I wasn't setting the update field correctly
*         for built in commands
* 2.0e -- Fix for bug in gtcom2.  Wasn't setting zparse correctly
* 2.0f -- Fix for bug in gtcom2.  Recall and pipes were not reparsed
* 2.0g -- Fix alias command
* 2.0h -- Add parameter name checking
* 2.0i -- Add parameter matching to parameter names so you only have
*         to type the closest match.
* 2.0j -- Add lparm command to list parameters
* 2.0k -- alias command upper cases everything
*         doubles now recognized by lparm
*         / can now be part of a parameter
* 2.0l -- Default log file opened with gtcom2
* 2.0m -- Alias command wasn't getting all of it's parameters
* 2.0n -- Bug fix to tbsvpr
* 2.0o -- Consistancy checks to tbsvpr
* 2.0p -- Changed uclgsg so that if the user enters a range like:
*         20,30,50 it will return 20-20, 30-30, and 50-50 which
*         is more sensible than 20-rmax, 30-rmax, and 50-rmax as
*         it did before
* 2.0q -- Delete log file on startup
* 2.0r -- Fix bug where command is ambigious and last command is repeated
*	  Add xpivers1 to return version string
*         Add uclgno to return the number of parameters on the command line
*         Add uclgs[*]d routines to get default value
*         Fix formatting bug in lparm
* 2.1a -- Remove CFORTRAN for unix boxes.
* 2.1b -- Change the behavior of gtcom2 where it appends the version number
*         to the program name to open all the files except for the .udc files
*         Add gtbuf here so that I can control how long a string it uses.
*         Fix a bug in yaccfor.inc where some strings were too short.
* 2.1c -- Increase the number of parameters allowed in yaccfor.inc
* 2.1d -- Bug fix: I was calling ydebug in ystcmd wrong.  Fixed.
*         Bug fix: Ambigious parameters were causing problems if two
*                  parameter names had one as a subset of the other.
*         Bug Fix: Extra spaces don't come out as often at random times.
* 2.1e -- Big change!! tbsvpr (save a parameter file) is not called
*                  automatically anymore, you MUST call xpisavepar.for
*                  on you own!
*         Bug Fix: File names are no longer lower cased
* 2.1f -- Bug Fix: Now takes 100 parameters, up from 30
* 2.1g -- Bug Fix: Fix crash in stackcmd on sun
* 2.1h -- Bug Fix: Tokens can begin with a . now
*         Bug Fix: Recall of partial commands works
*         Change the common blocks around to make the Alpha happy
* 2.1i -- Bug Fix: You can now specify a script file
*         Bug Fix: Fix a problem reading initial command line with
*                  parse errors all over the place.
* 2.1j -- VMS port
* 2.1k -- Fixed buffer and recall commands
* 2.1l -- Cfortran is back!
* 2.1m -- Call in gtbuf to put gtbuf in standalone mode.  When you do this
*         ! is no-longer a comment character, and @filename expands to the
*         contents of that file on the command line.
*         CloseDefaultPF fixed so that it now saves the .par file (duh!)
*         Some calls changed from open to openwr to make it work on vms
* 2.1n -- A few vms file protection fixes
* 2.1o -- inline -> inline1 in tbfdpr.c for gcc
*         Better error checking for uclgs* for numbers
*         Added a call to gtbuf to stop scripts
* 2.1p -- Bug fix, dec fortran doesn't like strings when you convert them to
*         numbers when they don't have a trailing space and you use * as a
*         format.
* 2.1q -- Split range parser out to uclgsgparse
*         EOF works now
*         Expanded parameter table size
*         Fixed the error checking when reading paramater tables &  overflowing
*         Added xpiparmode to get and set parameter mode
*         Added uclgot to get what was entered on the command line
*         Fixed a lenght problem in uclgsg
*         Fixed a length problem in gtbuf with cnam
* 2.1r -- Prepare for logging
*         ADD READLINE!!
*         Clean up error reporting for copying par files.
* 2.1s -- Replace host interface, add pset, pget, punlearn, plist, pquery.
*         opndefpf.c was changed a bit to get parameters correctly
*         ystcmd was changed to work with unix command parameters better
*         Added c interface routines to uclgs* called cuclgs*
*         Added ',' as a special character in OpenDefaultPF
* 2.1t -- A number of changes to make XPI as close as possible to the SAO
*          host interface.
*          @  Only queryed pars are asssigned off the command line unless
*             they have par=val
*          @  you should not see this is gone
*          @  gtcom2 pars are not loaded unless needed
*          @  PFILES and PFCLOBBER now work like the SAO Host
*          @  parameters are once again expanded
*          @  range checks are applied to numbers
*          @  if the parameter is given on the command line and it's standalone
*             then never prompt.
*          @  one has to work harder with plist et al to get something other
*             then the correct .par file out.
* 2.1u -- uclgsg and uclgsgd didn't have consistant parameter lists,
*             and uclgsgd wouldn't have worked.
*         % is now allowed in xantok
*         gtcom2 now has gtcom2_nolog and gtcom2_dolog to turn logging off/on
*         now have apply_mode to let the user enter mode=xx from the cmd line
*         fixed punlearn if they don't have PFCLOBBER set
*         changed apply_mode so that prompting works if the mode parameter is
*            mode q.
*         added a new par file parser.
*         pset now uses freadline, the gnu readline interface for unix.
*         pquery now calls applymode so it properly saves automatic parameters
*         changed yprint to give a location
*         changed ystcmd to not loose + and - on the end of a parameter
* --------------------------------------------
* put under RCS
*   - Case problems with parameter names added when I added gmatch removed
*   - pquery fixed again

 
 
 
      SUBROUTINE XPIVERS
 
      CHARACTER*(*) Version
 
      character(9) vers
      DATA vers/' XPI 2.1u'/
 
      CALL XWRITE(vers,5)
 
 
      RETURN
 
      ENTRY XPIVERS1(Version)
 
      Version = vers
      RETURN
 
      END
 
