*+WTBEGM
        subroutine wtbegm(taskname, version, chatter)

	IMPLICIT NONE
        character*(*) taskname, version
        integer chatter
c 
c Description:
c  Noddy routine which writes the standard introductory message for 
c ROSAT/CALTOOLS FTOOLS tasks
c
c Passed parameters
c  TASKNAME 	 i   : (char) name of the task (lower case !)
c  VERSION       i   : (char) version string of the task
c			      should be in the format 1.2.3
c  CHATTER       i   : (int)  chatter flag - this message only
c                             displayed at chatter > 1
c
c Called Routines:
c  function CLENACT       : (CALLIB) Returns useful length of string
c  subroutine CRMVLBK	  : (CALLIB) Strips leading blanks from string
c  subroutine WTINFO      : (CALLIB) Standard roslib/callib string writer
c
c Compilation & Linking
c  link with CALLIB 
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0: 1995 Nov 29) original
c  Rehana Yusaf     (1.0.1: 1995 Dec 13) add chatter parameter
c  Ian M George     (1.1.0: 1996 Sep 17) removed calls to crmvlbk to prevent
c                                        problems under Solaris 2.2
c	character(7) version
c	parameter (version = '1.1.0')
*- 
c Internals
	integer tasklen, verslen, tsklen, clenact
	character(80) outstr
	character(40) tskstr
c Initialize
	COMMON/task/tskstr

c Remove all leading blanks from i/p strings 
	call crmvlbk(taskname)
	call crmvlbk(version)

c Check-out useful string lengths
	tasklen = clenact(taskname)
	verslen = clenact(version)

c Construct the output string
	tskstr = taskname(:tasklen)//' '//version(:verslen)
	tsklen = clenact(tskstr)
	outstr = '** '//tskstr(:tsklen)

c Write the bugger
        call wtinfo(chatter,1,0,outstr)

	return
	end
c -------------------------------------------------------------------
