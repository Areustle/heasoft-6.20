C  $Header: /headas/headas/ftools/asca/src/timeconv/timedefs.inc,v 3.6 1996/04/16 23:44:43 dunfee Exp $
C          

C       FITS COLUMN VARIABLES 

c	Integer time_col

C       GENERAL VARIABLES

c        integer nhdr

c        double precision tstart, tstop ,mjdref

c        integer detector, bit_rate, datamode, n_events

C       INSTRUMENT INDEX

        integer GIS, SIS, SIS0, SIS1, SIS2, GIS2, GIS3

        parameter (GIS = 1)     
        parameter (SIS = 2)     

        parameter (SIS0 = 1)    
        parameter (SIS1 = 2)    
        parameter (SIS2 = 3)    
        parameter (GIS2 = 4)    
        parameter (GIS3 = 5)

C       POS_DET INDEX

        integer FLF, POW2

        parameter(FLF  = 1)
        parameter(POW2 = 2)


C       EVENT RECORD INDEX

c	Integer time

C       GIS DATA MODE INDEX

        integer PH_mode, PH2_mode, MPC_mode, MEMORY_mode, PCAL_mode

        parameter (PH_mode = 1) 
        parameter (PH2_mode = 2)
        parameter (MPC_mode = 3)
        parameter (MEMORY_mode = 4)
        parameter (PCAL_mode = 5)

C       SIS MODE INDEX

        integer BRIGHT_mode, BRIGHT2_mode, FAINT_mode
        integer FAST_mode, FAST2_mode, FRAME_mode
        integer HISTOGRAM_mode, DARK_IMAGE_mode, INTEGRATION_mode
	integer NONOBS_mode

        parameter (FAINT_mode = 1)
        parameter (BRIGHT_mode = 2)
        parameter (BRIGHT2_mode = 3)
        parameter (FAST_mode = 4)
        parameter (FAST2_mode = 5)
        parameter (FRAME_mode = 6)
        parameter (HISTOGRAM_mode = 7)  
        parameter (DARK_IMAGE_mode = 8) 
        parameter (INTEGRATION_mode = 9)
        parameter (NONOBS_mode = 10)

        integer NA_mode
        parameter (NA_mode = 100)

C       BIT RATE INDEX

        integer HIGH_bit_rate, MEDIUM_bit_rate, LOW_bit_rate

        parameter (LOW_bit_rate = 1)
        parameter (MEDIUM_bit_rate = 16)
        parameter (HIGH_bit_rate = 32)

        integer NA_bit_rate
        parameter (NA_bit_rate = 1024)  


cc	common /operation_mod/ detector, bit_rate, datamode, n_events,
cc     &	time_col

cc	common /operation_mod/ detector, bit_rate, datamode, n_events

cc	common /start_stop/ tstart, tstop, mjdref

