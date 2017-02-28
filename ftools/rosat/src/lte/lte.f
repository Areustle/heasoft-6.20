
C*********************************************************************
C SELECTOR TASK:
C      			LTE	
C
C FILE:
C      lte.f
C
C DESCRIPTION:
C      
C
C AUTHOR:
C	Steve Snowden
C       FTOOLS development by Srilal Weera  
C
C MODIFICATION HISTORY:
C       2000/01/05 Ning Gan. Changed the filenames from lowercase to
C       uppercase     
C
C NOTES:
C   
C
C USAGE:
C      HOST: call lte
C      IRAF: task lte 
C
C ARGUMENTS:
C      none
C
C
C CALLED ROUTINES:
C      subroutine glte - gets parameters from parameter file
C
C******************************************************************************




			 subroutine lte

c       PROGRAM LTE 
C
C  other.f evaluates the fit results of rate_fit.f to determine the number
C  of LTE contamination counts.  It requires the values T0, I0, I1, I2, I3
C  and I4, the accepted time file valid_times.dat, and two of the observation
C  data set files.  other.f will work on the REV0 data sets of both German 
C  (files *ORBIT.MT and *EVENTRATES.MT) and US (files *.so and *.evr) data.  
C  The files must be renamed to xxx_ORBIT.FITS and xxx_eventrates.fits where 
C  xxx is any string up to eight characters long; the string is queried for 
C  by the program.  The accepted time file should be the same as that used
C  by cast_data.f, cast_exp.f, and tilt.f
C
C  other.f integrates the model LTE light curve (the up to fourth-order 
C  polynomial fitted by rate_fit.f) to determine the total number of counts
C  under the curve.  It also calculates the number of counts from the time-
C  variable level of the light curve.  In other words, it finds the minimum
C  level of the fitted polynomial and integrates the excess above that level.
C
        IMPLICIT NONE
C
        INTEGER*4 BLOCKE, HDUTYPEE, I, IA1LL, IACTBE(1000), 
     +      IACTEN(1000), IAEXE, IAXE, IE, IERR, ILIVEN, IOS, IS, 
     +      ISTOT, ISTYLE, ITEMP, LUNE, NN, NROWSE, ROWLENE, STATUS, 
     +      T0, TBCOLE(23), TFIELDSE, VARIDATE
C
	integer ivalid, chatter
C
        REAL*4 I0, I1, I2, I3, I4
C     +      , IP0, IP1, ITH
        REAL*4 A1LL, AEXE, AXE, COUNTS, DEADTP, FLIVE1, FLIVE2, FLIVE, 
     +      RATE, RMIN, TCOUNTS, THETA, TIME  
C
        REAL*8 DELT, DRATE, EXP, SCSO
C
        character(1) OBS(8), OBS1
        character(2) OBS2
        character(3) OBS3
        character(4) OBS4
        character(5) OBS5
        character(6) OBS6
        character(7) OBS7
        character(8) OBS8
        character(8) TTYPEE(35), TFORME(35), TUNITE(35)
        character(20) EXTNAMEE
        character(80) INEVE
	character(80) valfile, context
        LOGICAL ANYFE, BADSTA, FLAGVALE, LCHECK
C
        EQUIVALENCE (OBS(1),OBS1,OBS2,OBS3,OBS4,OBS5,OBS6,OBS7,OBS8)
C
        DATA DEADTP /234./
        DATA LCHECK /.TRUE./
        DATA RMIN /100./
        DATA SCSO /0.D0/
C
      character(40) taskname
      common /task/ taskname
        taskname = 'lte v1.0'
	call ftcmsg

C Initialize:
	ISTYLE = 0
	STATUS = 0
	chatter = 9
	nn =0
        badsta =.false.
C
C  get parameters from parameter file
C
      call glte(OBS, ISTYLE, T0, I0, I1, I2, I3, I4, STATUS)
         if (status .ne. 0) then
	 context = 'lte.f: Error in obtaining parameters'
         call fcerr(context)
         goto 999
	 endif

C
C  Create the file names
C
        IF(OBS8 .NE. '        ') THEN
            DO I=2,8
                IF((OBS(I) .EQ. ' ') .AND. (OBS(I-1) .NE. ' '))
     +                  NN = I - 1
            ENDDO
            IF((OBS8 .NE. '        ') .AND. (NN .EQ. 0)) NN = 8
            IF(NN .EQ. 1) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INEVE,3010) OBS1
 3010               FORMAT(A1,'_anc.fits')
                ELSE
                    WRITE(INEVE,3015) OBS1
 3015               FORMAT(A1,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 2) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INEVE,3020) OBS2
 3020               FORMAT(A2,'_anc.fits')
                ELSE
                    WRITE(INEVE,3025) OBS2
 3025               FORMAT(A2,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 3) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INEVE,3030) OBS3
 3030               FORMAT(A3,'_anc.fits')
                ELSE
                    WRITE(INEVE,3035) OBS3
 3035               FORMAT(A3,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 4) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INEVE,3040) OBS4
 3040               FORMAT(A4,'_anc.fits')
                ELSE
                    WRITE(INEVE,3045) OBS4
 3045               FORMAT(A4,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 5) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INEVE,3050) OBS5
 3050               FORMAT(A5,'_anc.fits')
                ELSE
                    WRITE(INEVE,3055) OBS5
 3055               FORMAT(A5,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 6) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INEVE,3060) OBS6
 3060               FORMAT(A6,'_anc.fits')
                ELSE
                    WRITE(INEVE,3065) OBS6
 3065               FORMAT(A6,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 7) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INEVE,3070) OBS7
 3070               FORMAT(A7,'_anc.fits')
                ELSE
                    WRITE(INEVE,3075) OBS7
 3075               FORMAT(A7,'_eventrates.fits')
                ENDIF
            ELSEIF(NN .EQ. 8) THEN
                IF(ISTYLE .EQ. 3) THEN
                    WRITE(INEVE,3080) OBS8
 3080               FORMAT(A8,'_anc.fits')
                ELSE
                    WRITE(INEVE,3085) OBS8
 3085               FORMAT(A8,'_eventrates.fits')
                ENDIF
            ENDIF
        ENDIF
C
c     The values T0, I0, I1, I2, I3, I4 are read in glte routine. / Srilal
C
C        READ *, T0, I0, I1, I2, I3, I4, IP0, IP1, ITH
C
C  Open the event rate file.  The data are used to calculate the
C  livetime fraction
C
        IF(OBS8 .EQ. '        ') THEN
            call fcecho ( 'Enter EVENTRATE table name' )
            READ 1000, INEVE
 1000       FORMAT(A80)
        ENDIF
C
	context = INEVE
	call fcecho(context)
C
	call cgetlun(LUNE)
        CALL FTOPEN(LUNE,INEVE,0,BLOCKE,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
	    context = 'Unable to open infile: '//INEVE
	    call fcerr(context)
	    go to 999
        ENDIF
C
        IF(ISTYLE .EQ. 3) THEN
C
C  Skip to the proper extension for RDF data
C
            CALL FTMAHD (LUNE,6,HDUTYPEE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
	    context = 'Unable to move to the FITS extension'//INEVE
	    call fcerr(context)
	    go to 999
            ENDIF
            CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
     +                  TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
		context = 'Unable to read the FITS header '//INEVE
	    call fcerr(context)
	    go to 999
            ENDIF
        ELSE
C
C  Skip to the proper extension for German or US data
C
            CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
            IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                BADSTA = .TRUE.
		context='Unable to move to the FITS extension:'//INEVE
	    call fcerr(context)
	    go to 999
            ENDIF
C
C  Read the FITS file header information
C
            IF(ISTYLE .EQ. 1) THEN
                CALL FTGHTB (LUNE,20,ROWLENE,NROWSE,TFIELDSE,
     +                  TTYPEE,TBCOLE,TFORME,TUNITE,EXTNAMEE,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                    BADSTA = .TRUE.
		    context = 'Unable to read the FITS header:'//INEVE
	    call fcerr(context)
	    go to 999
                ENDIF
            ELSE
                CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
     +                  TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
                IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
                    BADSTA = .TRUE.
		    context = 'Unable to read the FITS header:'//INEVE
	    call fcerr(context)
	    go to 999
                ENDIF
            ENDIF
        ENDIF
C
C  Open accepted time input file
C
        valfile = 'valid_times.dat'
	call cgetlun(IVALID)
        OPEN(UNIT=ivalid,STATUS='OLD',FORM='FORMATTED',
     +          FILE=valfile)
C
C  Read in the accepted time intervals
C
        IS = 0
        IOS = 0
        DO WHILE (IOS .EQ. 0)
            IS = IS + 1
            READ(ivalid,*,IOSTAT=IOS) ITEMP, IACTBE(IS), IACTEN(IS)
            IF(IOS .EQ. 0) then
	    write(context,'(''BEGIN = '',i10,'' END = '',i10)')
     &      IACTBE(IS), IACTEN(IS)
	    call fcecho(context)
	    endif
        ENDDO
        ISTOT = IS - 1
	call fcecho(' ')
	write(context,'(''TOTAL NUMBER OF INTERVALS IS:'',i4)') ISTOT
	call fcecho(context)
	call fcecho(' ')
        CLOSE(ivalid)
C
C  Loop over the accepted time intervals
C
        DO IS=1,ISTOT
C
C  Check for an accepted time
C
            DO I=IACTBE(IS),IACTEN(IS)
C
C  Accepted time, now find the live time
C
C  ILIVEN = time in  S/C seconds
C  IAEXE = AEXE count rate
C  IAXE = AXE count rate
C  IA1LL = A1LL count rate
C
   30           CONTINUE
                DO WHILE ((I .GT. ILIVEN) .AND. (IE .LT. NROWSE))
                    IE = IE + 1
                    CALL FTGCFJ(LUNE,1,IE,1,1,ILIVEN,
     +                          FLAGVALE,ANYFE,STATUS)
                ENDDO
C
C  Check for another extension using US data
C
                IF((ISTYLE .EQ. 2) .AND. (IE .EQ. NROWSE) .AND.
     +                      (I .GT. ILIVEN)) THEN
                    STATUS = 0
C
C  Skip the first HDU
C
                    CALL FTMRHD (LUNE,1,HDUTYPEE,STATUS)
                    IF (STATUS .EQ. 0) THEN
                        IE = 0
C
C  Read the FITS file header information
C
                        CALL FTGHBN (LUNE,35,NROWSE,TFIELDSE,TTYPEE,
     +                          TFORME,TUNITE,EXTNAMEE,VARIDATE,STATUS)
                        GO TO 30
                    ELSE
                        GO TO 40
                    ENDIF
                ENDIF
C
                CALL FTGCFJ(LUNE,3,IE,1,1,IAEXE,FLAGVALE,ANYFE,STATUS)
                CALL FTGCFJ(LUNE,4,IE,1,1,IA1LL,FLAGVALE,ANYFE,STATUS)
                CALL FTGCFJ(LUNE,7,IE,1,1,IAXE,FLAGVALE,ANYFE,STATUS)
C
                A1LL = IA1LL
                AEXE = IAEXE
                AXE = IAXE
                CALL LIVTIM(A1LL,DEADTP,AXE,AEXE,FLIVE1,
     +                          FLIVE2,FLIVE,IERR)
                FLIVE1 = 1. - 0.0001*AEXE
                EXP = FLIVE*FLIVE1
                TIME = TIME + EXP
C
C  Use the live time SCS for the scale factor
C
                DELT = DBLE(I - T0)
C corrected the above expression since T0,I are integers./ Srilal
C
                THETA = AMOD(SNGL(DELT),5760.)
                THETA = THETA*6.283185/5760.
                DRATE = I0 + I1*1.D-5*DELT + 
     +                  I2*1.D-10*DELT*DELT +
     +                  I3*1.D-15*DELT*DELT*DELT +
     +                  I4*1.D-20*DELT*DELT*DELT*DELT
C     +                  + (IP0 + IP1*1.D-5*DELT)*SIN(THETA+ITH)
                RATE = DRATE
                IF(RATE .LT. RMIN) RMIN = RATE
                COUNTS = RATE*EXP
                TCOUNTS = TCOUNTS + COUNTS
            ENDDO
        ENDDO
C
        CALL FTCLOS(LUNE,STATUS)
        IF((STATUS .NE. 0) .AND. (.NOT. BADSTA)) THEN
            BADSTA = .TRUE.
	    context='Unable to close  FITS file: '//INEVE
	    call fcerr(context)
	    go to 999
        ENDIF
C
   40   CONTINUE
C
	 call fcecho(' ')
	 write(context,'(''TOTAL TIME='',f12.5)') TIME
	 call fcecho(context)
	 call fcecho(' ')
C
	 call fcecho(' ')
	 write(context,'(''TOTAL COUNTS='',f12.5)') TCOUNTS
	 call fcecho(context)
	 call fcecho(' ')
C
        IF((I1 .EQ. 0.) .AND. (I2 .EQ. 0.) .AND. (I3 .EQ. 0.) 
     +              .AND. (I4 .EQ. 0.)) THEN
	call fcecho(' ')
        call fcecho ('NET COUNTS =          0.0')
        ELSE
	 call fcecho(' ')
	 write(context,'(''NET COUNTS='',f12.5)') TCOUNTS - RMIN*TIME
	 call fcecho(context)
	 call fcecho(' ')
        ENDIF

999     continue
	if (status .ne. 0) then
	call fcerrm(status)
     	stop
        endif
C
        STOP
        END

C**********************************************************************
C SUBROUTINE:
C      glte
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C
C NOTES:
C       glte uses F77/VOS like calls to read parameter from .par file
C
C USAGE:
C     call glte(OBS, ISTYLE, T0, I0, I1, I2, I3, I4, STATUS)
C
C ARGUMENTS:
C      OBS     - Observation control (prefix of the data files)
C      ISTYLE  - 1 for German data, 2 for US data and 3 for RDF data
C      T0,I0.. - Time offset and fitted values (output of RATEFIT routine)
C
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst(i,r) - get parameter value
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C*****************************************************************************

      subroutine glte(OBS, ISTYLE, T0, I0, I1, I2, I3, I4, STATUS)
C
      character(8) OBSSTR
      character(1) OBS(8)
      character(80) context
      integer T0, istyle, status
      real*4  I0, I1, I2, I3, I4

C  initialize variables
	T0 = 0
	I0 = 0.0
	I1 = 0.0
	I2 = 0.0
	I3 = 0.0
	I4 = 0.0
      status = 0

C  get the contents of the OBS string
      call uclgst('obs',obsstr,status)
	    if (status .ne. 0) then
      context = 'could not get OBS parameter'
      call fcerr(context)
       goto 999
      endif
      DO i=1,8
      obs(i)= obsstr(i:i)
      enddo

C  get the value of the ISTYLE parameter
      call uclgsi('istyle',istyle,status)
	    if (status .ne. 0) then
      context = 'could not get ISTYLE parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the T0 parameter
      call uclgsi('t0',t0,status)
	    if (status .ne. 0) then
      context = 'could not get T0 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the I0 parameter
      call uclgsr('I0',I0,status)
	    if (status .ne. 0) then
      context = 'could not get I0 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the I1 parameter
      call uclgsr('I1',I1,status)
	    if (status .ne. 0) then
      context = 'could not get I1 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the I2 parameter
      call uclgsr('I2',I2,status)
	    if (status .ne. 0) then
      context = 'could not get I2 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the I3 parameter
      call uclgsr('I3',I3,status)
	    if (status .ne. 0) then
      context = 'could not get I3 parameter'
      call fcerr(context)
       goto 999
      endif

C  get the value of the I4 parameter
      call uclgsr('I4',I4,status)
	    if (status .ne. 0) then
      context = 'could not get I4 parameter'
      call fcerr(context)
       goto 999
      endif

 999  continue
       if (status .ne. 0) then
	call fcerrm(status)
 	 stop
       endif

      return
      end
