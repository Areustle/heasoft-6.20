
C*********************************************************************
C SELECTOR TASK:
C      			ratefit
C
C FILE:
C      ratefit.f
C
C DESCRIPTION:
C      
C
C AUTHOR:
C	Steve Snowden
C       FTOOLS development by Srilal Weera  
C
C MODIFICATION HISTORY:
C     
C
C NOTES:
C   
C
C USAGE:
C      HOST: call ratefit
C      IRAF: task ratefit
C
C ARGUMENTS:
C      none
C
C
C CALLED ROUTINES:
C
C      subroutine gratefit - gets parameters from parameter file
C
C******************************************************************************


			 subroutine rateft

C       PROGRAM RATE_FIT
C
C  Author: Steve Snowden
C  Date:   
C  Update: 1 October 1993
C
C  Program rate_fit.f uses the output of programs ao.f (AO_SSX_SC.OUT) 
C  and rate.f (RATE1.DAT and RATE2.DAT), and the accepted time file 
C  valid_times_all.dat, to fit the observation light curve with the 
C  particle background, scattered solar X-ray background, long-term 
C  enhancement background, and the possibility of a periodic background 
C  (with the orbit).  The periodic background part is presently turned 
C  off since it has never been useful.
C
C  rate_fit.f produces an output file PLOT.QDP which, surprise, surprise
C  can be plotted with QDP, or can be easily modified to work with your
C  favorite plotting package.  The output should be examined for how well
C  the model curve fits the data.  Short-term enhancements can play
C  havoc with the fits and if they contribute a significant number of
C  counts, the time intervals should be excluded from the 
C  valid_times_all.dat file.  rate_fit.f should be used interatively in
C  order to get a reasonable fit.
C
C  rate_fit.f requires the subroutines func_rate.for, rep_rate.for, 
C  mqm_rate.for, mrq_rate.for, dcvsrt.for and dgausj.for.
C
        IMPLICIT NONE
C
        SAVE
C
        EXTERNAL FUNC
C
        character(80) INFILE, PNAME(10)
        character(1) FIX(10)
        character(15) PSHORT(10)
	character(80) context
C
        INTEGER*4 I, IACTBE(100), IACTEN(100), IC, IERR, IFLAG, 
     +      II, IOS, IS, ISTOT, ITEMP, IUP, IUPLIM, LISTA(10), 
     +      MAXITR, N, NP, SC(5000), SSXTIM, SSXTO, T0, TI, TO
C
        REAL*4 A(10), ALAMDA, ALMLIM, BAND(7), CHISQ, 
     +      DELTCH, DUM(5000), I0, I1, I2, I3, I4, 
     +      IP0, IP1, IS0, IS1, RATE, RATO, OCHISQ, OLAMDA, PHI, 
     +      P(5000), PT(7), R(5000), RT(7), S(5000), 
     +      SSX(5000), ST(7), 
     +      THETA, TIME(5000), 
     +      X(5000), XMIN, XMAX, Y(5000), YMIN, YMAX
C
        REAL*8 ALPHA(10,10), COVAR(10,10)
C
        character(72) LINES(7)
C
C The following are read from gratefit routine
      integer bndcntrl, i0flag, i1flag, i2flag, i3flag, i4flag 
      integer is0flag, is1flag, status
C
C  Background line control common block
C
        COMMON /MODPAR/ I0, I1, I2, I3, I4, IS0, IS1, IP0, IP1, PHI
C
        EQUIVALENCE (A(1),I0)
C
        DATA FIX/10*' '/
        DATA PSHORT/'I0', 'I1', 'I2', 'I3', 'I4', 'IS0', 'IS1', 
     +      'IP0', 'IP1', 'PHI'/
        DATA DUM /5000*0./
C
C------------------------------------------------------
C
        character(40) taskname
	common /task/ taskname

	taskname = 'ratefit v1.0'
	call ftcmsg

C  get parameters from the parameter file
       call gratefit(IC, A(1), i0flag, A(2), i1flag,
     &      A(3), i2flag, A(4), i3flag, A(5), i4flag,
     &      A(6), is0flag, A(7), is1flag, status)
C
      if (status .ne. 0) then
       context = ' Error in obtaining data from parameter file: '
       call fcerr(context)
       goto 999
      endif


C
C  Open the accepted time file
C
        INFILE = 'valid_times_all.dat'
        OPEN(UNIT=93,STATUS='OLD',FORM='FORMATTED',
     +          FILE=INFILE)
        IOS = 0
        IS = 0
        DO WHILE (IOS .EQ. 0)
            IS = IS + 1
            READ(93,*,IOSTAT=IOS) ITEMP, IACTBE(IS), IACTEN(IS)
            IF(IOS .EQ. 0) then
        	write(context,'(''BEGIN = '',i10,'' END = '',i10)')
     &          IACTBE(IS), IACTEN(IS)
	        call fcecho(context)
            endif
        ENDDO
        CLOSE(93)
        IACTBE(IS) = 149999999
        IACTEN(IS) = 150000000
        ISTOT = IS - 1
	call fcecho(' ')
	write(context,'(''TOTAL NUMBER OF INTERVALS IS:'',i4)') ISTOT
	call fcecho(context)
	call fcecho(' ')
        IS = 1
C
        OPEN(UNIT=90,STATUS='OLD',FILE='RATE1.DAT')
        OPEN(UNIT=91,STATUS='OLD',FILE='RATE2.DAT')
        OPEN(UNIT=92,STATUS='OLD',FILE='AO_SSX_SC.OUT')
        READ(92,*,IOSTAT=IOS) SSXTIM, BAND
        RATE = BAND(IC)
C
        IOS = 0
        YMIN = 200.
        DO WHILE (IOS .EQ. 0)
            N = N + 1
            READ(90,*,IOSTAT=IOS) TI, RT(1), ST(1), RT(2), ST(2), 
     +                  RT(3), ST(3), RT(4), ST(4), RT(5), ST(5), 
     +                  RT(6), ST(6), RT(7), ST(7)
            READ(91,*,IOSTAT=IOS) TI, PT(1), PT(2), PT(3), PT(4), 
     +                  PT(5), PT(6), PT(7)
C
C  Process the attitude step, first check the accepted time file
C  to see if the attitude step is in an accepted time period
C
            DO WHILE ((TI .GT. IACTEN(IS)) .AND. (IS .LE. ISTOT))
                IS = IS + 1
            ENDDO
C
            IF((TI .GE. IACTBE(IS)) .AND. (IS .LE. ISTOT)) THEN
                SC(N) = TI
                R(N) = RT(IC) - PT(IC)
                S(N) = ST(IC)
                P(N) = PT(IC)
                IF(N .EQ. 1) THEN
                    T0 = TI - 100
                ENDIF
                TIME(N) = TI - T0
                IF((IOS .EQ. 0) .AND. (R(N) .GT. 0.)) THEN
                    IF(YMAX .LT. R(N)) YMAX = R(N)
                    IF(YMIN .GT. R(N)) YMIN = R(N)
                    DO WHILE ((SSXTIM .LT. TI) .AND. (IOS .EQ. 0))
                        SSXTO = SSXTIM
                        RATO = RATE
                        READ(92,*,IOSTAT=IOS) SSXTIM, BAND
                        RATE = BAND(IC)
                    ENDDO
C
                    IF((SSXTIM-SSXTO .LE. 90.) .AND. (IOS .EQ. 0)) THEN
                        SSX(N) = RATO + (TI-SSXTO)*(RATE-RATO)/
     +                      (SSXTIM-SSXTO)
                    ELSE
                        N = N - 1
                    ENDIF
                ELSE
                    N = N - 1
                ENDIF
            ELSE
                N = N - 1
            ENDIF
        ENDDO
C
         write(context,'('' YMIN = '',f10.5,''    YMAX = '',f10.5)')
     &   YMIN, YMAX
	 call fcecho(context)
         write(context,'(''  '',I5)')N
	 call fcecho(context)
	 call fcecho(' ')

C
C  *********************************************************************
C  *                                                                   *
C  *               Raw Data Entered, Now Get Parameters                *
C  *                                                                   *
C  *********************************************************************
C
   10   CONTINUE
        NP = 0

        IF(I0FLAG .EQ. 0) THEN
            NP = NP + 1
            LISTA(NP) = 1
            FIX(1) = '*'
            PNAME(NP) = 'I0'
        ENDIF
C
        IF(I1FLAG .EQ. 0) THEN
            NP = NP + 1
            LISTA(NP) = 2
            FIX(2) = '*'
            PNAME(NP) = 'I1'
        ENDIF
C
        IF(I2FLAG .EQ. 0) THEN
            NP = NP + 1
            LISTA(NP) = 3
            FIX(3) = '*'
            PNAME(NP) = 'I2'
        ENDIF
C
        IF(I3FLAG .EQ. 0) THEN
            NP = NP + 1
            LISTA(NP) = 4
            FIX(4) = '*'
            PNAME(NP) = 'I3'
        ENDIF
C
        IF(I4FLAG .EQ. 0) THEN
            NP = NP + 1
            LISTA(NP) = 5
            FIX(5) = '*'
            PNAME(NP) = 'I4'
        ENDIF
C
        IF(IS0FLAG .EQ. 0) THEN
            NP = NP + 1
            LISTA(NP) = 6
            FIX(6) = '*'
            PNAME(NP) = 'IS0'
        ENDIF
C
        IF(IS1FLAG .EQ. 0) THEN
            NP = NP + 1
            LISTA(NP) = 7
            FIX(7) = '*'
            PNAME(NP) = 'IS1'
        ENDIF
C
C        PRINT *, 'Enter the value for periodic scale IP0'
C        READ *, A(8)
        A(8) = 0.
C        PRINT *, 'Enter 1 to fix the value'
C        READ *, IFLAG
        IFLAG = 1
        IF(IFLAG .EQ. 0) THEN
            NP = NP + 1
            LISTA(NP) = 8
            FIX(8) = '*'
            PNAME(NP) = 'IP0'
        ENDIF
C
C        PRINT *, 'Enter the value for periodic scale IP1'
C        READ *, A(9)
        A(9) = 0.
C        PRINT *, 'Enter 1 to fix the value'
C        READ *, IFLAG
        IFLAG = 1
        IF(IFLAG .EQ. 0) THEN
            NP = NP + 1
            LISTA(NP) = 9
            FIX(9) = '*'
            PNAME(NP) = 'IP1'
        ENDIF
C
C        PRINT *, 'Enter the value for periodic scale PHI'
C        READ *, A(10)
        A(10) = 0.
C        PRINT *, 'Enter 1 to fix the value'
C        READ *, IFLAG
        IFLAG = 1
        IF(IFLAG .EQ. 0) THEN
            NP = NP + 1
            LISTA(NP) = 10
            FIX(10) = '*'
            PNAME(NP) = 'PHI'
        ENDIF
C
C  *********************************************************************
C  *                                                                   *
C  *                       Main Fit Begins Here                        *
C  *                                                                   *
C  *********************************************************************
C
        MAXITR = 100
        ALMLIM = 1.E3
        IUPLIM = 10
C
        CALL REPORT(-1,' Initial Model Value Settings:',A,FIX)
C
        ALAMDA = -1.0
        OCHISQ = 0.0
        IUP = 0
        I = 1
        IFLAG = 0
        DO WHILE ((IFLAG .EQ. 0) .AND. (I .LT. MAXITR))
            I = I + 1
            OLAMDA = ALAMDA
            CALL MQM_RATE (TIME,R,S,SSX,N,A,10,LISTA,NP,
     +          COVAR,ALPHA,10,CHISQ,FUNC,ALAMDA,IERR)
            DELTCH = ABS(OCHISQ - CHISQ)
C
C  Check for various termination conditions
C
            IF((ALAMDA .LT. OLAMDA) .AND. (DELTCH .LT. 0.1))
     +          IFLAG = 1
            IF(IFLAG .EQ. 0) THEN
                IF(ALAMDA .GT. OLAMDA) IUP = IUP + 1
                IF(ALAMDA .LT. OLAMDA) IUP = 0
                IF(IUP .GT. IUPLIM) IFLAG = 1
                IF(ALAMDA .GT. ALMLIM) IFLAG = 1
C
                WRITE(*,*)
                WRITE(*,*) 'ITER =',I,'  ALAMDA =',ALAMDA,
     +                  '  M.L.E. =',CHISQ
                CALL REPORT(-1,'Intermediate step..',A,FIX)
            ENDIF
        ENDDO
C
        WRITE(*,*)
        CALL REPORT(-1,' Final Model Values: (*=Floating)',A,FIX)
        WRITE(*,1010) ' Np =',NP,' N =',N,' M.L.E. =',CHISQ
C
 1010   FORMAT(/,1X,A,I2,A,I4,A,G12.4,A,G14.7)
C
C  Finish off the fit to get the covarience matrix
C
        ALAMDA = 0.
        CALL MQM_RATE (TIME,R,S,SSX,N,A,10,LISTA,NP,
     +      COVAR,ALPHA,10,CHISQ,FUNC,ALAMDA,IERR)
C
C  Write out final values and one sigma uncertainties
C
       call fcecho(' ')
       context=' Final parameter values and the formal standard errors:'
       call fcecho(context)
       call fcecho(' ')
C
        DO 1100 I=1,NP
            WRITE(6,2100) PSHORT(LISTA(I)),A(LISTA(I)),
     +              SNGL(DSQRT(DABS(COVAR(LISTA(I),LISTA(I)))))
 2100       FORMAT(1H ,A15,F13.5,' +/-',F11.5)
 1100   ENDDO
C
        II = 10
        DO I=1,N
            TI = TIME(I)
            THETA = AMOD(TIME(I),5760.)
            THETA = THETA*6.283185/5760.
            Y(I) = I0 + I1*1.E-5*TI + I2*1.E-10*TI*TI +
     +              I3*1.E-15*TI*TI*TI + I4*1.E-20*TI*TI*TI*TI +
     +              SSX(I)*(IS0 + IS1*1.E-5*TI) +
     +              (IP0 + IP1*1.E-5*TI)*SIN(THETA+PHI) + P(I)
            R(I) = R(I) + P(I)
            IF(TI-TO .GT. 45) II = II + 3
            II = II + 1
            X(I) = II
            TO = TI
        ENDDO
C
        XMIN = 0.
        XMAX = X(N) + 10.

c       WRITE(LINES(4),2990) T0
c2990   FORMAT('TIME OFFSET: T0 =',I12)
c       WRITE(LINES(5),3000) I0, I1, I2, I3, I4
c3000   FORMAT('LONG TERM: I0,I1,I2,I3,I4=',5F9.3)
C
c       WRITE(LINES(6),3010) IS0, IS1
c3010   FORMAT('SSX: IS0,IS1 =',2F9.4)
c       WRITE(LINES(7),3020) N, NP, CHISQ, CHISQ/(N-NP)
c3020   FORMAT('NO =',I5,'   NP =',I2,'   CHISQ =',F6.1,
c    +      '   REDCHISQ =',F10.4)
c       PRINT *, LINES(4)
c       PRINT *, LINES(5)
c       PRINT *, LINES(6)
c       PRINT *, LINES(7)
C
        write(context,'(I12,F12.8,4F14.8)')T0, I0, I1, I2, I3, I4
	call fcecho(context)
 	write(context,'('' TIME OFFSET: T0 ='',I12)')T0
        call fcecho(context)
 	write(context,'('' LONG TERM: I0,I1,I2,I3,I4='',5F9.3)')
     &	I0, I1, I2, I3, I4
        call fcecho(context)
 	write(context,'('' SSX: IS0,IS1 ='', 2F9.4)')IS0, IS1
        call fcecho(context)
 	write(context,'('' NO = '',I5,'' NP = '',I2)')N,NP
        call fcecho(context)
        write(context,'('' CHISQ = '',F9.1, '' REDCHISQ = '',F10.4)')
     &  CHISQ, CHISQ/(N-NP)
        call fcecho(context)
C
        WRITE(INFILE,3030) IC
 3030   FORMAT('plot_',I1,'.qdp')
        OPEN(UNIT=66,STATUS='UNKNOWN',FILE=INFILE)
        WRITE(66,*) 'READ SERR 2'
        WRITE(66,*) 'CSIZE 1.5'
        WRITE(66,*) 'FONT ROMAN'
        WRITE(66,*) 'LA X STEP'
        WRITE(66,*) 'LA Y COUNT RATE (COUNTS/S)'
        WRITE(INFILE, 3040) IC
 3040   FORMAT('LA T BAND R',I1,' COUNT RATE PROFILE')
        WRITE(66,*) INFILE
        WRITE(66,*) 'LA T COUNT RATE PROFILE'
        YMAX = 0.
        DO I=1,N
            IF(R(I)+S(I) .GT. YMAX) YMAX = R(I)+S(I)
        ENDDO
        YMAX = 1.1*YMAX
        YMAX = INT(YMAX) + 1.
        XMAX = 10.*(INT(X(N)/10.)) + 10.
        WRITE(66,*) 'R X 10', XMAX
        WRITE(66,*) 'R Y 0', YMAX
        WRITE(66,*) 'LINE STEPPED 3'
        DO I=1,N
            WRITE(66,*) X(I), R(I), S(I), Y(I), SC(I)
        ENDDO

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
C      gratefit
C
C DESCRIPTION: 
C      Get parameters from parameter file
C
C
C NOTES:
C      
C
C USAGE:
C      call gratefit(bndcntrl, i0val, i0flag, i1val, i1flag,
C    & i2val, i2flag, i3val, i3flag, i4val, i4flag,
C    & is0val,is0flag, is1val, is1flag, status)
C
C ARGUMENTS:
C
C PRIMARY LOCAL VARIABLES:
C      context - error message 
C      status  - error number
C
C CALLED ROUTINES:
C      subroutine uclgst - get string parameter
C      subroutine fcecho - echo message to terminal
C      subroutine fcerrm - echo error message to terminal
C
C*****************************************************************************

       subroutine gratefit(bndcntrl, i0val, i0flag, i1val, i1flag,
     & i2val, i2flag, i3val, i3flag, i4val, i4flag,
     & is0val,is0flag, is1val, is1flag, status)

      integer bndcntrl, i0flag, i1flag, i2flag, i3flag, i4flag 
      integer is0flag, is1flag, status
      real i0val, i1val, i2val, i3val, i4val, is0val, is1val
      character(80) context

C  initialize variables
      status = 0
      bndcntrl = 0
      i0val = 0.0
      i1val = 0.0
      i2val = 0.0
      i3val = 0.0
      i4val = 0.0
      is0val = 0.0
      is1val = 0.0
      i0flag = 0
      i1flag = 0
      i2flag = 0
      i3flag = 0
      i4flag = 0
      is0flag = 0
      is1flag = 0


C  get the value of the BNDCNTRL parameter
      call uclgsi('bndcntrl',bndcntrl,status)
	    if (status .ne. 0) then
      context = 'could not get BNDCNTRL parameter'
      call fcerr(context)
       goto 999
      endif

C  get the I0VAL parameter
      call uclgsr('i0val',i0val,status)
      if (status .ne. 0) then
       context = 'could not get I0VAL parameter'
       call fcerr(context)
       goto 999
      endif
C
      if (i0val .eq. 0.0) then
      	 i0flag = 1
      else
C  get the I0FLAG parameter
       call uclgsi('i0flag',i0flag,status)
       if (status .ne. 0) then
       context = 'could not get I0FLAG parameter'
       call fcerr(context)
       goto 999
       endif
      endif



C  get the I1VAL parameter
      call uclgsr('i1val',i1val,status)
      if (status .ne. 0) then
       context = 'could not get I1VAL parameter'
       call fcerr(context)
       goto 999
      endif
C
      if (i1val .eq. 0.0) then
      	 i1flag = 1
      else
C  get the I1FLAG parameter
       call uclgsi('i1flag',i1flag,status)
       if (status .ne. 0) then
       context = 'could not get I1FLAG parameter'
       call fcerr(context)
       goto 999
       endif
      endif
 
C  get the I2VAL parameter
      call uclgsr('i2val',i2val,status)
      if (status .ne. 0) then
       context = 'could not get I2VAL parameter'
       call fcerr(context)
       goto 999
      endif    
C
      if (i2val .eq. 0.0) then
      	 i2flag = 1
      else
C  get the I2FLAG parameter
       call uclgsi('i2flag',i2flag,status)
       if (status .ne. 0) then
       context = 'could not get I2FLAG parameter'
       call fcerr(context)
       goto 999
       endif
      endif

C  get the I3VAL parameter
      call uclgsr('i3val',i3val,status)
      if (status .ne. 0) then
       context = 'could not get I3VAL parameter'
       call fcerr(context)
       goto 999
      endif
C
      if (i3val .eq. 0.0) then
      	 i3flag = 1
      else
C  get the I3FLAG parameter
       call uclgsi('i3flag',i3flag,status)
       if (status .ne. 0) then
       context = 'could not get I3FLAG parameter'
       call fcerr(context)
       goto 999
       endif
      endif

C  get the I4VAL parameter
      call uclgsr('i4val',i4val,status)
      if (status .ne. 0) then
       context = 'could not get I4VAL parameter'
       call fcerr(context)
       goto 999
      endif
C
      if (i4val .eq. 0.0) then
      	 i4flag = 1
      else
C  get the I4FLAG parameter
       call uclgsi('i4flag',i4flag,status)
       if (status .ne. 0) then
       context = 'could not get I4FLAG parameter'
       call fcerr(context)
       goto 999
       endif
      endif


C  get the IS0VAL parameter
      call uclgsr('is0val',is0val,status)
      if (status .ne. 0) then
       context = 'could not get IS0VAL parameter'
       call fcerr(context)
       goto 999
      endif
C
      if (is0val .eq. 0.0) then
      	 is0flag = 1
      else
C  get the IS0FLAG parameter
       call uclgsi('is0flag',is0flag,status)
       if (status .ne. 0) then
       context = 'could not get IS0FLAG parameter'
       call fcerr(context)
       goto 999
       endif
      endif

C  get the IS1VAL parameter
      call uclgsr('is1val',is1val,status)
      if (status .ne. 0) then
       context = 'could not get IS1VAL parameter'
       call fcerr(context)
       goto 999
      endif
C
      if (is1val .eq. 0.0) then
      	 is1flag = 1
      else
C  get the IS1FLAG parameter
       call uclgsi('is1flag',is1flag,status)
       if (status .ne. 0) then
       context = 'could not get IS1FLAG parameter'
       call fcerr(context)
       goto 999
       endif
      endif

       
 999  continue
       if (status .ne. 0) then
	call fcerrm(status)
 	 stop
       endif

      return
      end

C---------------- end of gratefit routine -------------------

