      SUBROUTINE READ_EBOUNDS
C
C---------------------------------------------------------------------
C Author: Brendan Perry
C   date: 10/15/93
C
C This program reads an EBOUNDS calibration file.
C---------------------------------------------------------------------
C
      INCLUDE 'COMMON.INC'
      INTEGER i , frows , status, fcstln
      LOGICAL anyf
 
      Iunit = 20
      IF ( telescop.EQ.'COS-B' ) THEN
         Ebounds_file = calpath(1:fcstln(calpath)) // 'ebounds_cosb.rmf'
      ELSE
         Ebounds_file = calpath(1:fcstln(calpath)) // 'ebounds_sas2.rmf'
      ENDIF
C
C OPEN THE FITS FILE.
C
      CALL FTOPEN(Iunit,Ebounds_file,0,Block,status)
      IF ( status.NE.0 ) THEN
         WRITE (context,99001)
     &        '*** ERROR OPENING EBOUNDS FILE , status = ', status
         call fcerr (context)
         GOTO 200
      ENDIF
C
C PPPPP
C
C  PRIMARY ARRAY VALUES
C
C PPPPP
C
      CALL FTGHPR(Iunit,99,Simple,Bitpix,Naxis,Naxes,Pcount,Gcount,
     &            Extend,status)
C
C11111
C
C EXTENSION 1 GETS READ HERE
C
C11111
C
      CALL FTMRHD(Iunit,1,Htype,status)
      CALL FTGHBN(Iunit,99,N_chan,Tfield,Ttype,Tform,Tunit,Extnam,
     &            Pcount,status)
C
C READ THE ACTUAL DATA HERE
C
C N_CHAN is the actual number of channels in the fits file. It should
C always be the number of rows in the ebounds file, no matter which
C instrument (cos-b or sas-2) is used.
C
      DO 100 i = 1 , N_chan
         frows = i
         CALL FTGCVE(Iunit,2,frows,1,1,0.0,Ebounds_elo(i),anyf,status)
         CALL FTGCVE(Iunit,3,frows,1,1,0.0,Ebounds_ehi(i),anyf,status)
 100  CONTINUE
 
      IF ( status.NE.0 ) THEN
         WRITE (context,99001)
     &        '*** ERROR READING EBOUNDS FILE , status = ', status
         call fcerr (context)
         GOTO 200
      ENDIF
C
C CLOSE THE FITS FILE.
C
      CALL FTCLOS(Iunit,status)
      IF ( status.EQ.0 ) THEN
         RETURN
      ENDIF
 200  call fcerr ('*** FADMAP EXITED IN SUBROUTINE READ_EBOUNDS ***')
      status = 0
      CALL FTCLOS(Iunit,status)
      STOP
 
99001 FORMAT (A42,I4)
      END
C
C
C
C
      SUBROUTINE READ_EFFAREA (effarea_array)
C
C---------------------------------------------------------------------
C
C Author: Brendan Perry
C   date: 10/1/93
C
C This program reads a effective area (old SAREA) calibration file.
C---------------------------------------------------------------------
C
      INCLUDE 'COMMON.INC'
      character(2) filnum
      INTEGER n_array , status , i , j , k , fcstln
      REAL array(10000), effarea_array (n_chan, n_theta)
      LOGICAL anyf
 
      Iunit = 35
      filnum = '90'
 
      IF ( telescop.EQ.'COS-B' ) THEN
         IF ( Paira.LE.20.1 ) filnum = '20'
         IF ( Paira.LE.12.1 ) filnum = '12'
         IF ( Paira.LE.10.1 ) filnum = '10'
         IF ( Paira.LE.8.1 ) filnum = '08'
         IF ( Paira.LE.6.1 ) filnum = '06'
         IF ( Paira.LE.4.1 ) filnum = '04'
         Effarea_file = calpath(1:fcstln(calpath)) // 'effarea' //
     &        filnum // '.rmf'
      ELSE
         Effarea_file = calpath(1:fcstln(calpath)) // 'effarea_sas2.rmf'
      ENDIF
C
C OPEN THE FITS FILE.
C
      CALL FTOPEN(Iunit,Effarea_file,0,Block,status)
      IF ( status.NE.0 ) THEN
         WRITE (context,99001)
     &        '*** ERROR OPENING EFFAREA FILE , status = ', status
         call fcerr (context)
         GOTO 100
      ENDIF
C
C PPPPP
C
C  PRIMARY ARRAY VALUES
C
C PPPPP
C
      CALL FTGHPR(Iunit,99,Simple,Bitpix,Naxis,Naxes,Pcount,Gcount,
     &            Extend,status)
C
C11111
C
C EXTENSION 1 GETS READ HERE
C
C11111
C
      CALL FTMRHD(Iunit,1,Htype,status)
      CALL FTGHBN(Iunit,99,Nrows,Tfield,Ttype,Tform,Tunit,Extnam,Pcount,
     &            status)
C
C n_theta is the number of angle intervals in the EFFAREA file. It
C is parsed out of the tform3 value in the file, which, for example,
C is '61E' for cos-b.
C
c      READ (Tform(3)(:INDEX(Tform(3),'E')-1),FMT=*) N_theta
C
C n_chan * n_theta = size of effective area array
C 115 * 61 = 7015	for cos-b
C n_chan comes from the ebounds file, which actually contains
C redundant energ_lo and energ_hi arrays of energies.
C
      n_array = N_chan*N_theta

      CALL FTGCVE(Iunit,3,1,1,N_theta,0.0,Theta_array,anyf,status)
C
C READ THE ACTUAL EFFECTIVE AREA MATRIX HERE
C
      CALL FTGCVE(Iunit,4,1,1,n_array,0.0,array,anyf,status)
      k = 0
      do 60 j = 1, n_theta
       do 50 i = 1, n_chan
	  k = k + 1
          effarea_array(i,j) = array(k)
50     continue       
60    continue       

      IF ( status.NE.0 ) THEN
         WRITE (context,99001)
     &        '*** ERROR READING EFFAREA FILE , status = ', status
         call fcerr (context)
         GOTO 100
      ENDIF
C
C CLOSE THE FITS FILE.
C
      CALL FTCLOS(Iunit,status)
      RETURN
 100  call fcerr ('*** FADMAP EXITED IN SUBROUTINE READ_EFFAREA ***')
      status = 0
      CALL FTCLOS(Iunit,status)
      STOP
 
99001 FORMAT (A42,I4)
99002 FORMAT (I2.2)
      END
C
C
C
C
      SUBROUTINE READ_EVENTS
C
C---------------------------------------------------------------------
C
C Author: Brendan Perry
C   date: 6/28/93
C
C This subroutine reads the cos-b data file and returns to
C the main program event information, FADMAP to continue
C map processing.
C---------------------------------------------------------------------
C
      INCLUDE 'COMMON.INC'
      INTEGER i , frows , status
      LOGICAL anyf
      Iunit = 30
C
C OPEN FITS FILE HERE
C
      CALL FTOPEN(Iunit,Data_file,0,Block,status)
      IF ( status.NE.0 ) THEN
         WRITE (context,99001) 'unable to open infile, status = ',
     &        status
         call fcerr (context)
         GOTO 200
      ENDIF
C
C READ THE PHOTON EXTENSION HERE
C
      CALL FTMRHD(Iunit,2,Htype,status)
      CALL FTGHBN(Iunit,99,Nrowsph,Tfield,Ttype,Tform,Tunit,Extnam,
     &            Pcount,status)
 
      DO 100 i = 1 , Nrowsph
         frows = i
         CALL FTGCVD(Iunit,4,frows,1,1,0.0,Tstartph(frows),anyf,status)
         CALL FTGCVE(Iunit,3,frows,1,1,0.0,Engyph(frows),anyf,status)
         CALL FTGCVE(Iunit,7,frows,1,1,0.0,Raph(frows),anyf,status)
         CALL FTGCVE(Iunit,8,frows,1,1,0.0,Decph(frows),anyf,status)
         CALL FTGCVE(Iunit,9,frows,1,1,0.0,Lph(frows),anyf,status)
         CALL FTGCVE(Iunit,10,frows,1,1,0.0,Bph(frows),anyf,status)
         CALL FTGCVI(Iunit,21,frows,1,1,0.0,Fovph(frows),anyf,status)
         IF ( telescop.EQ.'COS-B') THEN
            CALL FTGCVE(Iunit,11,frows,1,1,0.0,Angleph(frows),anyf,
     &                  status)
            CALL FTGCVI(Iunit,12,frows,1,1,0.0,Viewph(frows),anyf,
     &                  status)
            CALL FTGCVI(Iunit,13,frows,1,1,0.0,Eclassph(frows),anyf,
     &                  status)
         ENDIF
 100  CONTINUE
 
      CALL FTCLOS(Iunit,status)
      IF ( status.EQ.0 ) THEN
         RETURN
      ENDIF
 200  call fcerr
     &     ('*** ERROR: FADMAP EXITED IN SUBROUTINE READ_PHOTON ***')
      status = 0
      CALL FTCLOS(Iunit,status)
      STOP
99001 FORMAT (A32,I4)
99002 FORMAT (1x,f12.5,4(i10,3x))
99003 FORMAT (1x,f12.5,2x,f12.5,3x,4(i10,3x))
      END
C
C
C
C
      SUBROUTINE READ_EXPOSURE
C
C---------------------------------------------------------------------
C
C Author: Brendan Perry
C   date: 6/28/93
C
C This subroutine reads a cos-b or sas-2 data file and returns to
C the main program exposure information, FADMAP to continue map
C processing.
C
C---------------------------------------------------------------------
C
      INCLUDE 'COMMON.INC'
      INTEGER frows , i , status
      LOGICAL anyf 
      character(20) comment
 
      Iunit = 25
C
C OPEN FITS FILE HERE
C
      CALL FTOPEN(Iunit,Data_file,0,Block,status)
      IF ( status.NE.0 ) THEN
         WRITE (context,99001) 'unable to open infile, status = ',
     &        status
         call fcerr (context)
         GOTO 100
      ENDIF
      CALL FTGKYS(Iunit,'DATE-OBS',Date_obs,comment,status)
      CALL FTGKYS(Iunit,'TIME-OBS',Time_obs,comment,status)
      CALL FTGKYS(Iunit,'DATE-END',Date_end,comment,status)
      CALL FTGKYS(Iunit,'TIME-END',Time_end,comment,status)
 
      CALL FTGKYS(Iunit,'TELESCOP',telescop,comment,status)
      CALL FTGKYS(Iunit,'OBJECT',Object,comment,status)
      CALL FTGKYS(Iunit,'OBS_ID',Obsid,comment,status)
      CALL FTGKYE(Iunit,'RA_OBJ',Raob,comment,status)
      CALL FTGKYE(Iunit,'DEC_OBJ',Decob,comment,status)
      CALL FTGKYE(Iunit,'L_OBJ',Lob,comment,status)
      CALL FTGKYE(Iunit,'B_OBJ',Bob,comment,status)
C
C	 READ THE EXPOSURE EXTENSION HERE
C
      CALL FTMRHD(Iunit,3,Htype,status)
 
      CALL FTGHBN(Iunit,99,Nrowsxp,Tfield,Ttype,Tform,Tunit,Extnam,
     &            Pcount,status)
 
      CALL FTGKYD(Iunit,'TSTART',Tstart,comment,status)
      CALL FTGKYD(Iunit,'TSTOP',Tstop,comment,status)
 
      IF ( status.EQ.0 ) THEN
         DO 50 i = 1 , Nrowsxp
            frows = i
            CALL FTGCVD(Iunit,1,frows,1,1,0.0,Tstartxp(frows),anyf,
     &                  status)
            CALL FTGCVD(Iunit,2,frows,1,1,0.0,Tstopxp(frows),anyf,
     &                  status)
            CALL FTGCVE(Iunit,3,frows,1,1,0.0,Ra_exp(frows),anyf,status)
            CALL FTGCVE(Iunit,4,frows,1,1,0.0,Dec_exp(frows),anyf,
     &                  status)
            CALL FTGCVE(Iunit,5,frows,1,1,0.0,L_exp(frows),anyf,status)
            CALL FTGCVE(Iunit,6,frows,1,1,0.0,B_exp(frows),anyf,status)
            CALL FTGCVE(Iunit,9,frows,1,1,0.0,Te0(frows),anyf,status)
            CALL FTGCVE(Iunit,10,frows,1,1,0.0,Te15(frows),anyf,status)
            CALL FTGCVE(Iunit,11,frows,1,1,0.0,Te20(frows),anyf,status)
            CALL FTGCVE(Iunit,12,frows,1,1,0.0,Te25(frows),anyf,status)
            CALL FTGCVE(Iunit,13,frows,1,1,0.0,Te30(frows),anyf,status)
            CALL FTGCVE(Iunit,14,frows,1,1,0.0,Te35(frows),anyf,status)
            CALL FTGCVE(Iunit,15,frows,1,1,0.0,Sensitiv(frows),anyf,
     &                  status)
            CALL FTGCVE(Iunit,17,frows,1,1,0.0,Cntrate3(frows),anyf,
     &                  status)
 50      CONTINUE
      ELSE
         GOTO 100
      ENDIF
C
C CLOSE THE FITS FILE, RETURN TO MAIN PROGRAM
C
      CALL FTCLOS(Iunit,status)
      IF ( status.EQ.0 ) THEN
         RETURN
      ENDIF
 100  call fcerr('*** FADMAP EXITED IN SUBROUTINE READ_EXPOSURE ***')
      status = 0
      CALL FTCLOS(Iunit,status)
      STOP
99001 FORMAT (A32,I4)
99002 FORMAT (1x,f12.5,4(i10,3x))
99003 FORMAT (1x,f12.5,2x,f12.5,3x,4(i10,3x))
      END
C
C
C
C
      SUBROUTINE READ_REDIST(Incline,Row)
C
C---------------------------------------------------------------------
C
C Author: Brendan Perry
C   date: 10/1/93
C
C This program reads a redistribution matrix calibration file.
C---------------------------------------------------------------------
C
      character(2) filnum
      INTEGER Row , Incline , status, fcstln
      LOGICAL anyf
 
      INCLUDE 'COMMON.INC'
      Iunit = 45
 
      IF ( telescop.EQ.'COS-B') THEN
         IF ( Incline.EQ.1 ) filnum = '00'
         IF ( Incline.EQ.2 ) filnum = '05'
         IF ( Incline.EQ.3 ) filnum = '10'
         IF ( Incline.EQ.4 ) filnum = '15'
         IF ( Incline.EQ.5 ) filnum = '20'
         IF ( Incline.EQ.6 ) filnum = '25'
         IF ( Incline.EQ.7 ) filnum = '30'
         Redist_file = calpath(1:fcstln(calpath)) // 'redist' //
     &        filnum // '.rmf'
      ELSE
         Redist_file = calpath(1:fcstln(calpath)) // 'redist_sas2.rmf'
      ENDIF
C
C OPEN THE FITS FILE.
C
      CALL FTOPEN(Iunit,Redist_file,0,Block,status)
      IF ( status.NE.0 ) THEN
         WRITE (context,99001)
     &        '*** ERROR OPENING REDIST FILE , status = ', status
         call fcerr (context)
         GOTO 100
      ENDIF
C
C PPPPP
C
C  PRIMARY ARRAY VALUES
C
C PPPPP
C
      CALL FTGHPR(Iunit,99,Simple,Bitpix,Naxis,Naxes,Pcount,Gcount,
     &            Extend,status)
C
C11111
C
C EXTENSION 1 GETS READ HERE
C
C11111
C
      CALL FTMRHD(Iunit,1,Htype,status)
      CALL FTGHBN(Iunit,99,Nrows,Tfield,Ttype,Tform,Tunit,Extnam,Pcount,
     &            status)
C
C READ THE ACTUAL DATA HERE
C
      CALL FTGCVE(Iunit,6,Row,1,N_chan,0.0,Redist_array,anyf,status)
 
      IF ( status.NE.0 ) THEN
         WRITE (context,99001)
     &        '*** ERROR READING REDIST FILE , status = ', status
         call fcerr (context)
         GOTO 100
      ENDIF
 
c         PRINT * , '*** Redistribution calibration data entered into
c     &FADMAP ***'
C
C CLOSE THE FITS FILE.
C
      CALL FTCLOS(Iunit,status)
      RETURN
 100  call fcerr ('*** FADMAP EXITED IN SUBROUTINE READ_REDIST ***')
      status = 0
      CALL FTCLOS(Iunit,status)
      STOP
 
99001 FORMAT (A45,I4)
99002 FORMAT (I2.2)
      END
