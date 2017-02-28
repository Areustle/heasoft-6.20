C ***********************************************************
C SUBROUTINE:
C	compkey
C
C DESCRIPTION:
C	compare the values for the telescop, instume and detector
C         keywords extracted from two FITS files
C
C AUTHOR:
C	James Lochner  5/95
C
C MODIFICATION HISTORY:
C     Aug 17, 1995 - allow b_detnam to have value ALL which matches
C                    any a_detnam value
C      
C NOTES:
C 
C USEAGE:
C 	CALL compkey (chatter, infil, a_telescop, a_instrume, a_detnam,
C             a_filter, b_name,b_telescop, b_instrume, b_detnam, b_filter,
C             ierr)
C
C ARGUMENTS:
C       chatter         - amount of chatter
C	infile		- name of file from which a_values extracted
C 	b_name		- type of file from which b_values extracted
C	a_,b_telescop	- value of TELESCOP keyword
C	a_,b_instrume	- value of INSTRUME keyword
C	a_,b_detnam	- value of DETNAM keyword
C	a_,b_filter	- value of FITLER keyword
C	ierr		- error status
C
C PRIMARY LOCAL VARIABLES:
C
C CALLED ROUTINES:
C	subroutine grab_name
C
C **************************************************************

      subroutine compkey(chatter, infil, a_telescop, a_instrume,
     &     a_detnam,a_filter,b_name,b_telescop,b_instrume,b_detnam,
     &     b_filter, ierr)

      IMPLICIT NONE
      CHARACTER*(*) infil, b_name
      CHARACTER*(*) a_telescop, a_instrume, a_detnam, a_filter
      CHARACTER*(*) b_telescop, b_instrume, b_detnam, b_filter
      INTEGER chatter, ierr
      INTEGER ifs, ife
      character(160) message
      character(12) errstr, wrnstr
      
      errstr = '** ERROR: '
      wrnstr = '** WARNING: '

      CALL grab_name(infil,ifs,ife)

      if(a_telescop .NE. b_telescop)THEN
         ierr = 1
         message=errstr // infil(ifs:ife) // ' telescope: '
         CALL fcecho(message)
c     CALL fcerr(message)
         message= a_telescop // 
     &        'doesn''t match' // b_name // 'telescope: ' // b_telescop
         CALL fcecho(message)
c     CALL fcerr(message)
      ENDIF

      if(a_instrume .NE. b_instrume)THEN
         ierr = 1
         message=errstr // infil(ifs:ife) // ' instrument ('
     &         // a_instrume // ') doesn''t match ' // b_name
     &         // ' instrument (' // b_instrume // ')'
c         CALL fcerr(message)
         CALL fcecho(message)
      ENDIF
      
      if(a_detnam .NE. b_detnam .AND. b_detnam .NE. 'ALL')THEN
c         ierr = 1
         message=wrnstr//infil(ifs:ife)//' detector ('//a_detnam//
     &        ') doesn''t match '//b_name//' detector ('
     &        //b_detnam//')'
c         CALL fcerr(message)
         CALL fcecho(message)
      ENDIF

998   IF (ierr .NE. 0) THEN
         message = errstr // 'Fatal'
         CALL fcecho(message)
      ENDIF

      RETURN
      END
