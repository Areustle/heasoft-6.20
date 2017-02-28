*+CALCLAS
	subroutine calclas(class,chatter,ierr)

	implicit none
        character*(*) class
	integer ierr,chatter


C Description:  checks that entry for CAL_CLAS column in CIF is OGIP-
C                approved
C
C              
C passed parameters:
C  CLASS      :    user-defined entry for CAL_CLAS column in CIF
C  CHATTER    :    chattiness flag (0 for silent running)
C  IERR       :    error flg (0=OK)
C
C 
C Called routines : 
C  subroutine WTINFO  : (CALLIB) writes callib info to standard o/p
C       
C compilation & linking :
C  link with CALLIB
C
C Origin: Written for the Calibration Database.
C
C Authors/Modification History:
C  Lorraine Breedon (1.0.0:96 Aug 5) original version
        character(7) version
        parameter (version='1.0.0')
*-
C Internals

        character(30) errstr
        character(160) message
        
C initialise
C Set ierr flg to 'no problem!'
        ierr = 0
	errstr = '** CALCLAS '//version//' ERROR: '


C do the stuff
	if ((class(1:3) .ne. 'CPF') .and. (class(1:3) .ne. 'BCF')) then
      message = errstr// 'entry for the CAL_CLAS column in the CIF'//
     &     ' is NOT OGIP-approved ...see memo CAL/GEN/92-011'
	   call wtinfo(chatter,5,1,message)
	   ierr=1
        endif

	return
	end

C--------------------End of CALCLAS subroutine--------------------------
