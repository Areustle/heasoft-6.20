C=======================================================================
      SUBROUTINE MAP ( THETA1,PHI1,IMOD,THETA2,PHI2,IMODNW)
C=======================================================================
C* EGRET calibration analysis. This routine maps the different sectors
C* (see EGRET/MPE/CVM/89/JUN/23) onto the calibration sector. It also
C* maps the direction modes.
C*
C* THETA1   -  inclination angle of infalling event (real,input)
C* PHI1     -  azimuth angle of infalling event (real,input)
C* IMOD     -  current viewing mode  (integer,input)
C* THETA2   -  corresponding calibration angle (real,output)
C* PHI2     -       "          "          "    (real,output)
C* IMODNW   -  viewing mode corresponding to calibration angles (integer
C*                                                               output)
C=======================================================================
C+ ISSUE: 2   STARTED: 26 JUN 1989    PROGRAMMER: C. VON MONTIGNY
C+            UPDATED: 16 Oct 1990    BY  CVM
C+ $Id: map.f,v 3.2 2013/05/21 19:08:27 irby Exp $
C=======================================================================
C%  CHANGES:
C%  26 JUN 1989 BY CVM:
C%  12 JUL 1990 BY CVM: CONTROL OUTPUT ACTIVATED
C%  16 OCT 1990 BY CVM: description of variables in header corrected
C+ 2.0	E.S.Panduranga	06/21/91	Moved source from IBM to SUN.
C+					Stripped off trailing blanks.
C+					Declare undeclared variables.
C+					Z constants changed to x constants
C+ $Log: map.f,v $
C+ Revision 3.2  2013/05/21 19:08:27  irby
C+ Change character*n to character(n) to silence warnings: "Obsolescent
C+ feature: Old-style character length".
C+
C+ Revision 3.1  2002/04/16 20:32:12  irby
C+ Additions to libgro - previously these codes existed in the following
C+ libraries:
C+
C+   libsenstv
C+   libsysutil
C+   libutil
C+   libftio
C+
c Revision 1.1  1997/01/23  17:02:00  programs
c Initial revision
c
c Revision 1.2  1996/08/15  17:58:44  programs
c implemented FAN-modes
c
c Revision 2.1  1991/09/09  17:42:41  nancy
c First controlled version on the Sun.
c
C%  04 OCT 1995 BY CVM: FAN-MODES (imod=76-87) included in array MODE
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      integer nmod
      parameter(nmod=87)
Cesp  ! declaring variables undeclared on ibm !
      real	theta1, phi1, theta2, phi2, rad, beta, alpha
      integer	imod, imodnw,  ki, k, i, j, kk, ialpha
      INTEGER D(2,9), DIRMOD, DIRNEW, A(2,2,7), DPRIME(2)
      INTEGER*4 MODE(nmod)

      save

C                                              INITIALISATION
Cesp  ! Z constants (IBM) changed to x constants (f77) !
Cesp  DATA MODE/Z1FF,Z1DF,Z17F,Z1FD,Z1F7,Z1D7,Z15F,Z17D,Z1F5,Z1C7,Z1CF,
Cesp & Z19F,Z11F,Z13F,Z17E,Z17C,Z1FC,Z1F9,Z1F1,Z1F3,Z1E7,Z18F,Z13E,Z1F8,
Cesp & Z1E3,Z187,Z10F,Z11E,Z13C,Z178,Z1F0,Z1E1,Z1C3,Z107,Z11C,Z170,
Cesp & Z1C1,Z183,Z10E,Z138,Z1E0,Z83,Z87,Z7,ZF,ZE,Z1E,Z1C,Z3C,Z38,Z78,
Cesp & Z70,ZF0,ZE0,ZE1,ZC1,ZC3,Z3,Z6,ZC,Z18,Z30,Z60,ZC0,Z81,Z2,Z8,Z20,
Cesp & Z80,Z1,Z4,Z10,Z40,Z0/
      DATA MODE/x'1FF',x'1DF',x'17F',x'1FD',x'1F7',x'1D7',x'15F',
     &	x'17D',x'1F5',x'1C7',x'1CF',
     & 	x'19F',x'11F',x'13F',x'17E',x'17C',x'1FC',x'1F9',x'1F1',
     &	x'1F3',x'1E7',x'18F',x'13E',x'1F8',
     &	x'1E3',x'187',x'10F',x'11E',x'13C',x'178',x'1F0',x'1E1',
     &	x'1C3',x'107',x'11C',x'170',
     &	x'1C1',x'183',x'10E',x'138',x'1E0',x'83',x'87',x'7',x'F',
     &	x'E',x'1E',x'1C',x'3C',x'38',x'78',
     &	x'70',x'F0',x'E0',x'E1',x'C1',x'C3',x'3',x'6',x'C',x'18',
     &	x'30',x'60',x'C0',x'81',x'2',x'8',x'20',
     &	x'80',x'1',x'4',x'10',x'40',x'0',x'0FF',x'1FE',x'0FE',x'1EF',
     &  x'0EF',x'1EE',x'0EE',x'1FB',x'0FB',x'1BF',x'0BF',x'1BB',
     &  x'0BB'/

      DATA A/ 0, 1, 1, 0,
     &        0,-1, 1, 0,
     &       -1, 0, 0, 1,
     &       -1, 0, 0,-1,
     &        0,-1,-1, 0,
     &        0, 1,-1, 0,
     &        1, 0, 0,-1/
      DATA D/1, 0,
     &       1, 1,
     &       0, 1,
     &      -1, 1,
     &      -1, 0,
     &      -1,-1,
     &       0,-1,
     &       1,-1,
     &       0, 0/

      character(80)	id
      common	/id/	id
      id = '$Id: map.f,v 3.2 2013/05/21 19:08:27 irby Exp $'

      DIRNEW= 511         ! = 1FF
      DIRMOD=MODE(IMOD)
C     WRITE(6,'(''  DIRMOD='',Z3)') DIRMOD
      RAD=3.141592654/180.
C                                  DETERMINATION OF THE SECTOR K
      K = PHI1/45. + 1
C     WRITE(6,'(/'' MAP: SECTOR = '',I3)') K
      KI=K-1
C     WRITE(6,'('' MAP: A     =('',I2,'','',I2,'')'')')A(1,1,KI)
C    & ,A(1,2,KI)
C     WRITE(6,'('' MAP:        ('',I2,'','',I2,'')'')')A(2,1,KI)
C    & ,A(2,2,KI)
      IF (K.GT.1) THEN
C                                  CALCULATION OF THE NEW VIEWING MODE
          DO 20 I=1,9
            IF(.NOT. BTEST(DIRMOD,I-1)) THEN
               DPRIME(1) = A(1,1,KI) * D(1,I) + A(1,2,KI) * D(2,I)
               DPRIME(2) = A(2,1,KI) * D(1,I) + A(2,2,KI) * D(2,I)
C     WRITE(6,'('' MAP: D     =('',I2,'','',I2,'')'')') D(1,I),D(2,I)
C     WRITE(6,'('' MAP: DPRIME=('',I2,'','',I2,'')'')') DPRIME
               DO 10 J=1,9
               IF(DPRIME(1).EQ. D(1,J) .AND. DPRIME(2).EQ.D(2,J)) THEN
                  DIRNEW = IBCLR(DIRNEW,J-1)
               END IF
  10           CONTINUE
            END IF
  20      CONTINUE
      ELSE
C                                      IDENTITY
          DIRNEW = DIRMOD
      END IF
C                                  CALCULATION OF THE CALIBRATION ANGLES
      THETA2 = THETA1
      KK= MOD(K,2)
      IF (KK.EQ.0) THEN
         IALPHA= (0.5 * K - 1)
         ALPHA = IALPHA * 90.
         BETA = SIN( (PHI1-ALPHA) * RAD)
         PHI2  =ACOS(BETA) / RAD
      ELSE
         IALPHA= 0.5 * K
         ALPHA = IALPHA * 90.
         PHI2 = PHI1 -ALPHA
      END IF
      DO 30 I=1,NMOD
        IF(DIRNEW.EQ.MODE(I)) THEN
          IMODNW=I
C         WRITE(6,'(''  DIRNEW='',Z3,'' IMODNW='',I3)') DIRNEW,IMODNW
          GOTO 9999
        END IF
  30  CONTINUE
 9999 RETURN
      END
