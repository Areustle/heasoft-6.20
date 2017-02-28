*+WTENDM
        subroutine wtendm(taskname, version, ierr, chatter)

	IMPLICIT NONE
	integer ierr,chatter
        character*(*) taskname, version
c 
c Description:
c  Noddy routine which writes the standard sign-off message for 
c ROSAT/CALTOOLS FTOOLS tasks
c
c Passed parameters
c  TASKNAME 	 i   : (char) name of the task (lower case !)
c  VERSION       i   : (char) version string of the task
c			      should be in the format 1.2.3
c  IERR          i   : (int) error flag (zero=OK)
c  CHATTER       i   : (int) chatter flag, sign off message only
c                      written if chatter > 1
c
c Called Routines:
c  function CLENACT       : (CALLIB) Returns useful length of string
c  subroutine CRMVLBK	  : (CALLIB) Strips leading blanks from string
c  subroutine FCERR       : (FTOOLS) Writes message to STDERR
c  subroutine WTERRM            :(CALLIB) writes error message to STDOUT
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
c	parameter (version = '1.1.0.')
*- 
c Internals
	integer tasklen, verslen, clenact
	character(40) taskstr
	character(80) outstr
c Commons
        COMMON/task/taskstr

c Remove all leading blanks from i/p strings 
	call crmvlbk(taskname)
	call crmvlbk(version)

c Check-out useful string lengths
	tasklen = clenact(taskname)
	verslen = clenact(version)

c Write out the appropriate messages
        if(ierr.NE.0) then
          call wterrm(taskname, version,'INCOMPLETE EXECUTION')
	  outstr = '** '//taskname(:tasklen)//' '//version(:verslen)//
     &          ' stop'
          call wtinfo(0,0,0,outstr)	  

c	  taskstr = ' ERROR - '//taskname(:tasklen)//' '//
c     &		version(:verslen)
	  taskstr = ' '//taskname(:tasklen)//' '//
     &		version(:verslen)
           call fcerr('FAILED')
        else
	  outstr = '** '//taskname(:tasklen)//' '//version(:verslen)//
     &          ' completed successfully'
          call wtinfo(chatter,1,0,outstr)
        endif
 
	return
	end
c -------------------------------------------------------------------
