*+ CGETLUN  (UNX version)
      	subroutine CGETLUN(LUN)
	IMPLICIT NONE
      	INTEGER   LUN
c
c Description 
C  SYSTEM DEPENDENT routine to return a free Logical Unit Number.  
c Note, units 1-9 should be avoided, since many programs use these units 
c without warning.
c
c Passed Parameters
c  LUN        o  : An unopened logical unit number.
c
c Called Routines
c  subroutine FCERR      : (FTOOLS) Writes to standard Error
c
c Origin
c  Swiped from XANLIB for inclusion in CALLIB
c
C Authors/Modification History:
c anon (unknown), Original XANADU version
c Ron Zellar (1993, Feb 3) Modified for Calibration Library
c Ian M George (1993 Jul 19), added version number & error checks
c
	character(9) version 
	parameter(version = '1.0.2.UNX')
*-
c Internals/Initialization
      INTEGER   I
      character(80) CONTXT

c Main
      lun = 0
      i = 0
      call ftgiou(lun,i)
      if (i .ne. 0) then
         LUN = -99
         CONTXT = ' ** CGTLUN ' // version // 
     &        ' ERROR :Out of free units'
         CALL FCERR(CONTXT)
      end if
      call ftfiou(lun,i)

      RETURN
      END
