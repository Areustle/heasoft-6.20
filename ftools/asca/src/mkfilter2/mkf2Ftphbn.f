C
        subroutine mkf2Ftphbn( funit, iret )
C
        IMPLICIT NONE

        integer tfields, tdisps
        parameter( tfields = 133 )
        parameter( tdisps = 84 )

        character(3) tform( tfields )
        character(8) tunit( tfields )
        character(8) ttypk( tfields ), ttypv( tfields )
        character(8) tdspk( tdisps ), tdspv( tdisps )
        character(8) extname
        character(60) comment( tfields )

        integer iret, funit, i
        integer nrows, varidat

*       Note: For version 2.04, column order has been reorganized
*       Old (2.03) 1-73 are still 1-73; 74-103 became 82-111, inserting
*       8 new Sn_DFEm columns in between.
*       Also, GIS HV columns are added (now 112-117), Gn_DEADT
*       columns are now 118-119 (from 110-111), and old 104-109
*       are now 120-125.
*
*       New columns Sn_SATFm added as 74-81, old 74-125 pushed
*       by 8 --- 1995 May
C
C       Changed col 2 (START) to A23.-- Jeff Guerber, July 1998
C
C       Fixed misspellings in header-comment strings -- Jeff Guerber, Sept 1998
C

        data nrows / 21600 /
*                    Default nrows: 21600 is 1 day divided by 4 sec
*                    ...a good initial guess of the size of mkf file

        data varidat / 0 /

        data ( tform( i ), i = 1, 90 ) /
     +                           '1D', '23A',  '1D',  '1B',  '1B',
     +                           '1E',  '1E',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1E',  '1B',
     +                           '1E',  '1B',  '1E',  '1B',  '1E',
     +                           '1B',  '1B',  '1I',  '1I',  '1B',
     +                           '1B',  '1B',  '1B',  '1B',  '1I',
     +                           '1I',  '1I',  '1I',  '1I',  '1I',
     +                           '1I',  '1I',  '1I',  '1I',  '1I',
     +                           '1I',  '1I',  '1I',  '1I',  '1I',
     +                           '1B',  '1B',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1B',  '1B',
     +                           '1B',  '1B',  '1B',  '1B',  '1B',
     +                           '1B',  '1E',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1E',  '1B' /
        data ( tform( i ), i = 91, tfields ) /
     +                           '1B',  '1B',  '1B',  '1B',  '1E',
     +                           '1E',  '1E',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1E',  '1B',
     +                           '1B',  '1B',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1E',  '1E',
     +                           '1E',  '1E',  '1E',  '1B',  '1B',
     +                           '1E',  '1I',  '1E' /

        data ( tunit( i ), i = 1, 90 ) /
     +  's       ', '        ', 's       ', '        ', '        ',
     +  'deg     ', 'deg     ', 'deg     ', 'deg     ', 'km      ',
     +  'deg     ', 'deg     ', 'deg     ', 'GeV     ', '        ',
     +  'deg     ', '        ', 's       ', '        ', 's       ',
     +  '        ', '        ', '        ', '        ', '        ',
     +  '        ', '        ', '        ', '        ', '        ',
     +  '        ', '        ', '        ', '        ', '        ',
     +  '        ', '        ', '        ', '        ', '        ',
     +  '        ', '        ', '        ', '        ', '        ',
     +  '        ', '        ', 'degC    ', 'degC    ', 'count /s',
     +  'count /s', 'count /s', 'count /s', 'count /s', 'count /s',
     +  'count /s', 'count /s', 'count /s', 'count /s', 'count /s',
     +  'count /s', 'count /s', 'count /s', 'count /s', 'count /s',
     +  'count /s', 'count /s', 'count /s', 'count /s', 'count /s',
     +  'count /s', 'count /s', 'count /s', '        ', '        ',
     +  '        ', '        ', '        ', '        ', '        ',
     +  '        ', 'ADU     ', 'ADU     ', 'ADU     ', 'ADU     ',
     +  'ADU     ', 'ADU     ', 'ADU     ', 'ADU     ', '        ' /
        data ( tunit( i ), i = 91, tfields ) /
     +  '        ', '        ', '        ', '        ', 'count /s',
     +  'count /s', 'count /s', 'count /s', 'count /s', 'count /s',
     +  'count /s', 'count /s', 'count /s', 'count /s', 'count /s',
     +  'count /s', 'count /s', 'count /s', 'count /s', 'count /s',
     +  'count /s', 'count /s', 'count /s', 'count /s', '        ',
     +  '        ', '        ', 'degC    ', 'degC    ', 'V       ',
     +  'V       ', 'uA      ', 'V       ', 'V       ', 'uA      ',
     +  '        ', '        ', 'degC    ', '        ', '        ',
     +  'count /s', '        ', 'deg     ' /

        data ( ttypk( i ), i = 1, 90 ) /
     +  'TTYPE1  ', 'TTYPE2  ', 'TTYPE3  ', 'TTYPE4  ', 'TTYPE5  ',
     +  'TTYPE6  ', 'TTYPE7  ', 'TTYPE8  ', 'TTYPE9  ', 'TTYPE10 ',
     +  'TTYPE11 ', 'TTYPE12 ', 'TTYPE13 ', 'TTYPE14 ', 'TTYPE15 ',
     +  'TTYPE16 ', 'TTYPE17 ', 'TTYPE18 ', 'TTYPE19 ', 'TTYPE20 ',
     +  'TTYPE21 ', 'TTYPE22 ', 'TTYPE23 ', 'TTYPE24 ', 'TTYPE25 ',
     +  'TTYPE26 ', 'TTYPE27 ', 'TTYPE28 ', 'TTYPE29 ', 'TTYPE30 ',
     +  'TTYPE31 ', 'TTYPE32 ', 'TTYPE33 ', 'TTYPE34 ', 'TTYPE35 ',
     +  'TTYPE36 ', 'TTYPE37 ', 'TTYPE38 ', 'TTYPE39 ', 'TTYPE40 ',
     +  'TTYPE41 ', 'TTYPE42 ', 'TTYPE43 ', 'TTYPE44 ', 'TTYPE45 ',
     +  'TTYPE46 ', 'TTYPE47 ', 'TTYPE48 ', 'TTYPE49 ', 'TTYPE50 ',
     +  'TTYPE51 ', 'TTYPE52 ', 'TTYPE53 ', 'TTYPE54 ', 'TTYPE55 ',
     +  'TTYPE56 ', 'TTYPE57 ', 'TTYPE58 ', 'TTYPE59 ', 'TTYPE60 ',
     +  'TTYPE61 ', 'TTYPE62 ', 'TTYPE63 ', 'TTYPE64 ', 'TTYPE65 ',
     +  'TTYPE66 ', 'TTYPE67 ', 'TTYPE68 ', 'TTYPE69 ', 'TTYPE70 ',
     +  'TTYPE71 ', 'TTYPE72 ', 'TTYPE73 ', 'TTYPE74 ', 'TTYPE75 ',
     +  'TTYPE76 ', 'TTYPE77 ', 'TTYPE78 ', 'TTYPE79 ', 'TTYPE80 ',
     +  'TTYPE81 ', 'TTYPE82 ', 'TTYPE83 ', 'TTYPE84 ', 'TTYPE85 ',
     +  'TTYPE86 ', 'TTYPE87 ', 'TTYPE88 ', 'TTYPE89 ', 'TTYPE90 ' /
        data ( ttypk( i ), i = 91, tfields ) /
     +  'TTYPE91 ', 'TTYPE92 ', 'TTYPE93 ', 'TTYPE94 ', 'TTYPE95 ',
     +  'TTYPE96 ', 'TTYPE97 ', 'TTYPE98 ', 'TTYPE99 ', 'TTYPE100',
     +  'TTYPE101', 'TTYPE102', 'TTYPE103', 'TTYPE104', 'TTYPE105',
     +  'TTYPE106', 'TTYPE107', 'TTYPE108', 'TTYPE109', 'TTYPE110',
     +  'TTYPE111', 'TTYPE112', 'TTYPE113', 'TTYPE114', 'TTYPE115',
     +  'TTYPE116', 'TTYPE117', 'TTYPE118', 'TTYPE119', 'TTYPE120',
     +  'TTYPE121', 'TTYPE122', 'TTYPE123', 'TTYPE124', 'TTYPE125',
     +  'TTYPE126', 'TTYPE127', 'TTYPE128', 'TTYPE129', 'TTYPE130',
     +  'TTYPE131', 'TTYPE132', 'TTYPE133' /

        data ( ttypv( i ), i = 1, 90 ) /
     +  'TIME    ', 'START   ', 'BN_WIDTH', 'BIT_RATE', 'ACS     ',
     +  'NSAS    ', 'Z_ALPHA ', 'Z_DELTA ', 'EULER_3 ', 'SAT_ALT ',
     +  'SAT_LON ', 'SAT_LAT ', 'ELV     ', 'COR     ', 'FOV     ',
     +  'BR_EARTH', 'SAA     ', 'T_SAA   ', 'SUNSHINE', 'T_DY_NT ',
     +  'S0_MODE ', 'S1_MODE ', 'S0_ID   ', 'S1_ID   ', 'S0_DSCR ',
     +  'S1_DSCR ', 'SIS_ADRS', 'S0_GRADE', 'S1_GRADE', 'S0_EVTR0',
     +  'S0_EVTR1', 'S0_EVTR2', 'S0_EVTR3', 'S1_EVTR0', 'S1_EVTR1',
     +  'S1_EVTR2', 'S1_EVTR3', 'S0_SPTR0', 'S0_SPTR1', 'S0_SPTR2',
     +  'S0_SPTR3', 'S1_SPTR0', 'S1_SPTR1', 'S1_SPTR2', 'S1_SPTR3',
     +  'S0_AE   ', 'S1_AE   ', 'S0_TEMP ', 'S1_TEMP ', 'S0_EVNT0',
     +  'S0_EVNT1', 'S0_EVNT2', 'S0_EVNT3', 'S1_EVNT0', 'S1_EVNT1',
     +  'S1_EVNT2', 'S1_EVNT3', 'S0_PIXL0', 'S0_PIXL1', 'S0_PIXL2',
     +  'S0_PIXL3', 'S1_PIXL0', 'S1_PIXL1', 'S1_PIXL2', 'S1_PIXL3',
     +  'S0_TELM0', 'S0_TELM1', 'S0_TELM2', 'S0_TELM3', 'S1_TELM0',
     +  'S1_TELM1', 'S1_TELM2', 'S1_TELM3', 'S0_SATF0', 'S0_SATF1',
     +  'S0_SATF2', 'S0_SATF3', 'S1_SATF0', 'S1_SATF1', 'S1_SATF2',
     +  'S1_SATF3', 'S0_DFE0 ', 'S0_DFE1 ', 'S0_DFE2 ', 'S0_DFE3 ',
     +  'S1_DFE0 ', 'S1_DFE1 ', 'S1_DFE2 ', 'S1_DFE3 ', 'GIS_MODE' /
        data ( ttypv( i ), i = 91, tfields ) /
     +  'GHV2_L  ', 'GHV3_L  ', 'GHV2_H  ', 'GHV3_H  ', 'G2_LDHIT',
     +  'G3_LDHIT', 'G2_H0   ', 'G3_H0   ', 'G2_H1   ', 'G3_H1   ',
     +  'G2_H2   ', 'G3_H2   ', 'G2_L0   ', 'G3_L0   ', 'G2_L1   ',
     +  'G3_L1   ', 'G2_L2   ', 'G3_L2   ', 'G2_CPU_I', 'G3_CPU_I',
     +  'G2_CPU_O', 'G3_CPU_O', 'G2_TELM ', 'G3_TELM ', 'G2_CPU_S',
     +  'G3_CPU_S', 'GIS_HAMM', 'G2_TEMP ', 'G3_TEMP ', 'G2_HVHMN',
     +  'G2_HVLMN', 'G2_HVHCM', 'G3_HVHMN', 'G3_HVLMN', 'G3_HVHCM',
     +  'G2_DEADT', 'G3_DEADT', 'RBM_TEMP', 'GIS_RBMF', 'SIS_RBMF',
     +  'RBM_CONT', 'ETI     ', 'ANG_DIST' /

        data ( tdspk( i ), i = 1, tdisps ) /
     +  'TDISP1  ', 'TDISP2  ', 'TDISP3  ', 'TDISP6  ', 'TDISP7  ',
     +  'TDISP8  ', 'TDISP9  ', 'TDISP10 ', 'TDISP11 ', 'TDISP12 ',
     +  'TDISP13 ', 'TDISP14 ', 'TDISP16 ', 'TDISP18 ', 'TDISP20 ',
     +  'TDISP23 ', 'TDISP24 ', 'TDISP48 ', 'TDISP49 ', 'TDISP50 ',
     +  'TDISP51 ', 'TDISP52 ', 'TDISP53 ', 'TDISP54 ', 'TDISP55 ',
     +  'TDISP56 ', 'TDISP57 ', 'TDISP58 ', 'TDISP59 ', 'TDISP60 ',
     +  'TDISP61 ', 'TDISP62 ', 'TDISP63 ', 'TDISP64 ', 'TDISP65 ',
     +  'TDISP66 ', 'TDISP67 ', 'TDISP68 ', 'TDISP69 ', 'TDISP70 ',
     +  'TDISP71 ', 'TDISP72 ', 'TDISP73 ', 'TDISP82 ', 'TDISP83 ',
     +  'TDISP84 ', 'TDISP85 ', 'TDISP86 ', 'TDISP87 ', 'TDISP88 ',
     +  'TDISP89 ', 'TDISP95 ', 'TDISP96 ', 'TDISP97 ', 'TDISP98 ',
     +  'TDISP99 ', 'TDISP100', 'TDISP101', 'TDISP102', 'TDISP103',
     +  'TDISP104', 'TDISP105', 'TDISP106', 'TDISP107', 'TDISP108',
     +  'TDISP109', 'TDISP110', 'TDISP111', 'TDISP112', 'TDISP113',
     +  'TDISP114', 'TDISP118', 'TDISP119', 'TDISP120', 'TDISP121',
     +  'TDISP122', 'TDISP123', 'TDISP124', 'TDISP125', 'TDISP126',
     +  'TDISP127', 'TDISP128', 'TDISP131', 'TDISP133' /

Cebi  TDISP6-TDISP9 are set to f8.3, instead of f6.1 (K.E. 94/09/20)
        data ( tdspv( i ), i = 1, tdisps ) /
     +  'F13.3   ', 'A23     ', 'F5.1    ', 'F8.3    ', 'F8.3    ',
     +  'F8.3    ', 'F8.3    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'I4.4    ', 'I4.4    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.2    ', 'F6.2    ',
     +  'F6.2    ', 'F6.2    ', 'F6.2    ', 'F6.2    ', 'F6.2    ',
     +  'F6.2    ', 'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ',
     +  'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.1    ', 'F6.3    ',
     +  'F6.3    ', 'F6.1    ', 'F9.1    ', 'F6.3    ' /

        data extname / '        ' /

        data ( comment( i ), i = 1, 18 ) /
     +         'Seconds from Jan. 1, 1993, of bin center',
     +         'Start time of record',
     +         'Data Bin Width',
     +         'DP Bit Rate 0:H, 1:M, 2:L',
     +         'ACS Status 0:fine mode, 1:coarse mode',
     +         'NSAS cone angle (sun angle)',
     +         'R.A.(2000) of Z-Axis',
     +         'Dec.(2000) of Z-Axis',
     +         '3rd Euler Angle (Z-Y-Z)',
     +         'Satellite altitude',
     +         'Satellite longitude',
     +         'Satellite latitude',
     +         'Target elevation above the Earth rim',
     +         'Cut Off Rigidity',
     +         'Field of View  0:Sky/1:Night Earth/2:Day Earth',
     +         'Angular distance from Bright Earth',
     +         'Passage of South Atlantic Anomaly  1:yes/0:no',
     +         'Time after SAA passage (sec)' /
        data ( comment( i ), i = 19, 36 ) /
     +         '1:satellite is in daytime/ 0:night',
     +         'Time after day/night transition (sec)',
     +         'SIS_0 observation mode 0:Fnt/1:Brt/2:Fst/3:Frm',
     +         'SIS_1 observation mode 4:DkFrm/5:Hst/6:Intg',
     +         'SIS_0 CCD ID LIST',
     +         'SIS_1 CCD ID LIST',
     +         'SIS_0 Discriminator status',
     +         'SIS_1 Discriminator status',
     +         'SIS Address Dscri 0:in 1:out for each chip',
     +         'SIS_0 grade discriminate value',
     +         'SIS_1 grade discriminate value',
     +         'SIS_0 Chip_0 Event Threshold',
     +         'SIS_0 Chip_1 Event Threshold',
     +         'SIS_0 Chip_2 Event Threshold',
     +         'SIS_0 Chip_3 Event Threshold',
     +         'SIS_1 Chip_0 Event Threshold',
     +         'SIS_1 Chip_1 Event Threshold',
     +         'SIS_1 Chip_2 Event Threshold' /
        data ( comment( i ), i = 37, 54 ) /
     +         'SIS_1 Chip_3 Event Threshold',
     +         'SIS_0 Chip_0 Split Threshold',
     +         'SIS_0 Chip_1 Split Threshold',
     +         'SIS_0 Chip_2 Split Threshold',
     +         'SIS_0 Chip_3 Split Threshold',
     +         'SIS_1 Chip_0 Split Threshold',
     +         'SIS_1 Chip_1 Split Threshold',
     +         'SIS_1 Chip_2 Split Threshold',
     +         'SIS_1 Chip_3 Split Threshold',
     +         'SIS_0 AE Status 0:normal/1:A-off/2:Power off',
     +         'SIS_1 AE Status 0:normal/1:A-off/2:Power off',
     +         'SIS_0 CCD temperature',
     +         'SIS_1 CCD temperature',
     +         'SIS_0 chip_0 HK Event number',
     +         'SIS_0 chip_1 HK Event number',
     +         'SIS_0 chip_2 HK Event number',
     +         'SIS_0 chip_3 HK Event number',
     +         'SIS_1 chip_0 HK Event number' /
        data ( comment( i ), i = 55, 72 ) /
     +         'SIS_1 chip_1 HK Event number',
     +         'SIS_1 chip_2 HK Event number',
     +         'SIS_1 chip_3 HK Event number',
     +         'SIS_0 chip_0 HK Pixel number over the threshold',
     +         'SIS_0 chip_1 HK Pixel number over the threshold',
     +         'SIS_0 chip_2 HK Pixel number over the threshold',
     +         'SIS_0 chip_3 HK Pixel number over the threshold',
     +         'SIS_1 chip_0 HK Pixel number over the threshold',
     +         'SIS_1 chip_1 HK Pixel number over the threshold',
     +         'SIS_1 chip_2 HK Pixel number over the threshold',
     +         'SIS_1 chip_3 HK Pixel number over the threshold',
     +         'SIS_0 chip_0 telemetry event number',
     +         'SIS_0 chip_1 telemetry event number',
     +         'SIS_0 chip_2 telemetry event number',
     +         'SIS_0 chip_3 telemetry event number',
     +         'SIS_1 chip_0 telemetry event number',
     +         'SIS_1 chip_1 telemetry event number',
     +         'SIS_1 chip_2 telemetry event number' /
        data ( comment( i ), i = 73, 90 ) /
     +         'SIS_1 chip_3 telemetry event number',
     +         'SIS_0 chip_0 saturation flag 0:no/ 1:yes',
     +         'SIS_0 chip_1 saturation flag 0:no/ 1:yes',
     +         'SIS_0 chip_2 saturation flag 0:no/ 1:yes',
     +         'SIS_0 chip_3 saturation flag 0:no/ 1:yes',
     +         'SIS_1 chip_0 saturation flag 0:no/ 1:yes',
     +         'SIS_1 chip_1 saturation flag 0:no/ 1:yes',
     +         'SIS_1 chip_2 saturation flag 0:no/ 1:yes',
     +         'SIS_1 chip_3 saturation flag 0:no/ 1:yes',
     +         'SIS_0 chip_0 dark frame error',
     +         'SIS_0 chip_1 dark frame error',
     +         'SIS_0 chip_2 dark frame error',
     +         'SIS_0 chip_3 dark frame error',
     +         'SIS_1 chip_0 dark frame error',
     +         'SIS_1 chip_1 dark frame error',
     +         'SIS_1 chip_2 dark frame error',
     +         'SIS_1 chip_3 dark frame error',
     +         'GIS observation mode, 0:PH/ 1:MPC/ 2:PCAL' /
        data ( comment( i ), i = 91, 108 ) /
     +         'GIS HVL2 status 16:off/ 8:reduction/ 0-7:level',
     +         'GIS HVL3 status 16:off/ 8:reduction/ 0-7:level',
     +         'GIS HVH2 status 16:off/ 8:reduction/ 0-7:level',
     +         'GIS HVH3 status 16:off/ 8:reduction/ 0-7:level',
     +         'GIS_2 Lower Discri Hit counting rate',
     +         'GIS_3 Lower Discri Hit counting rate',
     +         'GIS_2 HK monitor H0',
     +         'GIS_3 HK monitor H0',
     +         'GIS_2 HK monitor H1',
     +         'GIS_3 HK monitor H1',
     +         'GIS_2 HK monitor H2',
     +         'GIS_3 HK monitor H2',
     +         'GIS_2 HK monitor L0',
     +         'GIS_3 HK monitor L0',
     +         'GIS_2 HK monitor L1',
     +         'GIS_3 HK monitor L1',
     +         'GIS_2 HK monitor L2',
     +         'GIS_3 HK monitor L2' /
        data ( comment( i ), i = 109, 126 ) /
     +         'GIS_2 HK monitor CPU_IN',
     +         'GIS_3 HK monitor CPU_IN',
     +         'GIS_2 HK monitor CPU_OUT',
     +         'GIS_3 HK monitor CPU_OUT',
     +         'GIS_0 telemetry event number',
     +         'GIS_1 telemetry event number',
     +         'GIS_2 CPU status, run +4, stop +2, error +1',
     +         'GIS_3 CPU status, run +4, stop +2, error +1',
     +         'GIS Hamming Error 0:off/1:on',
     +         'GIS_2 temperature',
     +         'GIS_3 temperature',
     +         'GIS2 HV-H monitor',
     +         'GIS2 HV-L monitor',
     +         'GIS2 HV-H current monitor',
     +         'GIS3 HV-H monitor',
     +         'GIS3 HV-L monitor',
     +         'GIS3 HV-H current monitor',
     +         'GIS_2 Dead time correction factor (0-1)' /
        data ( comment( i ), i = 127, tfields ) /
     +         'GIS_3 Dead time correction factor (0-1)',
     +         'RBM temperature',
     +         'RBM flag status of GIS 0:off/1:on',
     +         'RBM flag status of SIS 0:off/1:on',
     +         'RBM counting rate',
     +         'Extended TI counter',
     +         'Angular Distance of FOV from specified direction' /

      call ftphbn( funit, nrows, tfields, ttypk, tform, tunit,
     &             extname, varidat, iret )

      call ftbdef( funit, tfields, tform, varidat, nrows, iret )

      do 10  i = 1, tfields
          call ftmkys( funit, ttypk(i),
     &                 ttypv(i), comment(i), iret )
 10   continue
      do 20  i = 1, tdisps
          call ftpkys( funit, tdspk(i),
     &                 tdspv(i), 'Format for display', iret )
 20   continue

      return
      end
