*+CAL_CNAM
	subroutine calcnam(cnam,chatter,bcf,cpf,ierr)

	implicit none
        character*(*) cnam
	integer ierr,chatter
        logical cpf, bcf


C Description:  checks that entry for CAL_CNAM column in CIF is OGIP-
C                approved
C
C              
C passed parameters:
C  TYPE       :    user-defined entry for CAL_CNAM column in CIF
C  CHATTER    :    chattiness flag (0 for silent running)
C  IERR       :    error flg (0=OK)
C  BCF,CPF    :    output=.T. or .F.; if CAL_CNAM entry corresponds to  
C                                     a BCF or CPF

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

        character(30) wrnstr
        character(160) message
	
        
C Set ierr flg to 'no problem!'
        bcf=.false.
        cpf=.false.
        ierr = 0
	wrnstr = '** CALCNAM '//version//' WARNING :'

C do the stuff
	if ((cnam(1:7) .eq. 'RAW2PHY') .or. 
     &           (cnam(1:7) .eq. 'RAW2LIN') .or.
     &           (cnam(1:7) .eq. 'LIN2EQU') .or.
     &           (cnam(1:7) .eq. 'PHY2ECL') .or.
     &           (cnam(1:7) .eq. 'LIN2XMA') .or. 
     &           (cnam(1:7) .eq. 'EQU2PHY')) then

	   bcf=.true.
           cpf=.false.
	endif 

	if ((cnam(1:6) .eq. 'BADPIX') .or.
     &           (cnam(1:11) .eq. 'BKGRND_EVTS') .or.   
     &           (cnam(1:8) .eq. 'COLLRESP') .or. 
     &           (cnam(1:6) .eq. 'DETEFF') .or.
     &           (cnam(1:6) .eq. 'DETMSK') .or.
     &           (cnam(1:7) .eq. 'DET_EFF') .or.
     &           (cnam(1:9) .eq. 'DET_ENRES') .or. 
     &           (cnam(1:8) .eq. 'DET_GAIN') .or.
     &           (cnam(1:11) .eq. 'DET_POSCORR') .or.
     &           (cnam(1:10) .eq. 'DET_POSRES') .or. 
     &           (cnam(1:7) .eq. 'EFFAREA') .or.
     &           (cnam(1:5) .eq. 'FATOM')) then

	   bcf=.true.
           cpf=.false.
	endif

        if ((cnam(1:6) .eq. 'FTRANS') .or.
     &           (cnam(1:6) .eq. 'HKCONV') .or. 
     &           (cnam(1:8) .eq. 'OBSCFACT') .or.
     &           (cnam(1:4) .eq. 'TEMP') .or. 
     &           (cnam(1:7) .eq. 'TVIGNET') .or.
     &           (cnam(1:6) .eq. 'VIGNET') .or. 
     &           (cnam(1:5) .eq. 'WATOM') .or.
     &           (cnam(1:6) .eq. 'WTRANS') .or. 
     &           (cnam(1:5) .eq. 'XSECT')) then

	   bcf=.true.
           cpf=.false.
	endif

	if ((cnam(1:7) .eq. 'ASCALIN') .or. 
     &           (cnam(1:11) .eq. 'ASCALIN_FLF') .or.
     &           (cnam(1:12) .eq. 'ASCALIN_POW2') .or.   
     &           (cnam(1:7) .eq. 'EDS_COR') .or. 
     &           (cnam(1:8) .eq. 'GRIDTRNS') .or.
     &	         (cnam(1:16) .eq. 'PART_BKGD_MAP_AP') .or.
     &           (cnam(1:17) .eq. 'PART_BKGD_MAP_EXT') .or.
     &           (cnam(1:17) .eq. 'PART_BKGD_MAP_INT') .or.
     &           (cnam(1:9) .eq. 'RTIBOUNDS')) then

           bcf=.true. 
           cpf=.false.

        endif

        if ((cnam(1:5) .eq. 'SGC_E') .or.    
     &           (cnam(1:7) .eq. 'SGC_POS') .or. 
     &           (cnam(1:4) .eq. 'WC_E') .or.
     &           (cnam(1:8) .eq. 'WC_POS_X') .or. 
     &           (cnam(1:8) .eq. 'WC_POS_Y') .or.
     &           (cnam(1:8) .eq. 'WINTHICK')) then

	   bcf=.true.
           cpf=.false.
	endif

	if ((cnam(1:6) .eq. '2D_PSF') .or. 
     &           (cnam(1:3) .eq. 'EEF') .or. 
     &           (cnam(1:11) .eq. 'ENERGY_GRID') .or.
     &           (cnam(1:4) .eq. 'RPSF')) then

	   bcf=.true.
           cpf=.true.
	endif 

	if ((cnam(1:6) .eq. 'DETMAP') .or. 
     &           (cnam(1:7) .eq. 'EBOUNDS') .or.
     &           (cnam(1:6) .eq. 'MATRIX') .or.
     &           (cnam(1:8) .eq. 'SPECRESP') .or.
     &           (cnam(1:15) .eq. 'SPECRESP_MATRIX')) then

	   bcf=.false.
           cpf=.true.
	endif 

	if ((bcf .eqv. .false.) .and. (cpf .eqv. .false.)) then 
      message = wrnstr// 'entry for the CAL_CNAM column in the CIF'//
     &     ' is NOT OGIP-approved ...see memo CAL/GEN/92-011'
 	   call wtinfo(chatter,5,1,message)
	   
        endif

	return
	end

C--------------------End of CALCNAM subroutine--------------------------

C---------------------------------------------------------------------
  
