c  Structure for rate buffers. Original due to la (Sept. 85)
c        
c  31/5/88 updated by ls
c  19/7/88 ls rev.1 see words 73-76 of rec.1
c  20/7/89 ls rev.2 SSS layout for rec 2 added (see also "expt" rec.1)
c  23/8/89 ls rev.3 MPC layout for rec 2 added (see also "expt" rec.1)
c
c  30/1/90 LE Z file structure added by JO/LA
c  11/9/91 HEAO1 A-1 handler added by BP/LA
c
c    LE Z file header structure
c 
	structure/rnewbuf_z/
c                               ='PAT ' for valid Z file
		character(4) code	
c                              number of subfiles present
		integer*2 n	
c                              total number of records in file
		integer*2 r		
		structure/rec_index/ subfile(31)
c                              rec no. of 1st rec of the  
c			       nth subfile
			integer*2 start_rec	
c                              name of associated normal 
c			       rates file
			character(6) file	
		end structure
	endstructure
c
c
	structure/rnewbuf_rec_1/
		character(16) source_name
c                              s/c pointing scaled by 10**9=360deg
		integer*4 ira, idec    
c                              in units of 0.1 deg
		integer*2 roll         
c                              =1 if slew (0 otherwise)
                integer*2 slew_flag    
c                              word 15 spare
		byte spare1(2)         
c                              type of rates file
c                              =0 for old low-res rates file
c                              =1 for old msec rates file
c                              =2 for new low-res rates file		
c                              =3 for new msec rates file
c                              =4 for old low-res from arriv. time
		integer*2 type     
c                              bin integration time/bin. it can be
c                              specified in the following units depending
c                              on the value of word 29 (flagunit):
c                              a) 2**-14 sec word 29 =0 
c                              b) microsec   word 29 =1 
c		               c) sec        word 29 =2     
		integer*4 inttime  
c                              SHF key for start of rates file
		integer*4 startshf     
c                              SHF key for end of rates file 
     		integer*4 endshf       
c                              experiment no.
c                              ( 1, 2, 3, 4, 5,  6,  10=
c                               L1,L2,ME,GS,SSS,MPC, A-1) 
c                              this determines layout of rec 2
 		integer*2 expt       
c                              0=none;1=barycentric (type0);2=header
c                              (i.e. results stored in recs 3 and 4)  
		integer*2 correct  
c                              no.of good data points
		integer*4 npt	       
c                              SHF key of file creation time
		integer*4 shfcreate    
c                              Flag to control the units of the
c                              bin integration time (see word 
c                              17-18 inttime) and 1st bin shift 
c                              (see timebin). The allowed value are:
c                              =0 for 2**-14 sec
c                              =1 for microsec
c                              =2 for sec
                integer*2 flagunit 
c                              words 30-31 spare
		byte spare2(4)         
c                              power of 2 -multiply cnts/sec (type2)
		integer*2 power1       
c                                   "         "     err/sec     "
		integer*2 power2       
c                                   "         "     deadtimes(type?) 
		integer*2 power3       
c                              avg.count rate (units of 0.001 ct/s, 
c                              or 1.e-6 for LE1 , LE2)
		integer*4 average      
c                              standard dev. (units of 0.001 ct/s, 
c                              or 1.e-6 for LE1 , LE2)  
 		integer*4 sdev          
c                              no.data gaps in msec.file 
		integer*2 ngap         
c                              word 40 spare
c		byte spare3(2)         
c                              flag =1 if real*4 are converted to
c                              the IEEE hp9000 standard
                integer*2 r4hp9000     
c                              time of center of 1st bin in file
c 		               relative to d.ref.pt.(s/c clock units)
                integer*4 timebin      
c                              s/c clock of datation reference point
c			       i.e.the ref.time of 1st HK record,
c                              specified in the following units depending
c                              on the value of word 29 (flagunit):
c                              a) 2**-14 sec word 29 =0 
c                              b) microsec   word 29 =1 
c		               c) sec        word 29 =2   
c                              centre of first bin = reftime1+timebin
		integer*4 reftime1     
c                              the utc of the datation ref.point
c			       i.e. grd station time of 1st HK rec
c			       contains the whole no.of seconds
 		integer*4 utctime1     
c                              the remaining microseconds to complete
c			       the UTC of the datation reference
		integer*4 micro1       
c                              as above but for eof (last HK record)
 		integer*4 reftime2     
c                              "       "        "
                integer*4 utctime2     
c                              "       "        "
    		integer*4 micro2       
c                              =1 for new;=0 for old format
c			       defines format for rec 1-4
c                              xronos supports only new format
		integer*2 form	       
c                              word 56 spare 
		byte spare4(2)         
c                              scaled sych that 360 deg=1.E9
		integer*4 source_ra    
c                              scaled such that  90 deg=1.E9	
                integer*4 source_dec   
c                              no. of 128 word records in file
                integer*4 nrec	       
c                              avg barycentric corr., units of 0.1 s
                integer*2 avg_bary     
c                              s/c clock correction (correct=1)
c                              (no. of pulses)/(expected pulses) =
c                              = (0.999999+s_c_corr*1.d-10)
                integer*2 s_c_corr      
c                              =1 if file concatenated with conlc
                integer*2 flag_con     
c                              word 66 spare
                byte spare5(2)         
c                              msec to be added to reftime1 to
c                              correct arrival times to the baryc.
c                              (correct=2)
                integer*4 ms_rftm1     
c                              msec as above but for reftime2
                integer*4 ms_rftm2     
c                              microsec to be added as ms_rftm1
                integer*2 mms_rftm1    
c                              microsec to be added as ms_rftm2
                integer*2 mms_rftm2    
c rev.1 start
c                              shf key of obervation start
                integer*4 startshfobs  
c                              shf key of obervation stop
                integer*4 endshfobs    
c                              words 77-128 spare                 
                byte spare6(104)       
c                              words 73-128 spare
c                byte spare6(112)       
c rev.1 stop
		end structure
c
c	ME buffer(record 2)
c	
c
	structure/rnewbuf_me/
c                              AR=1, XE=2, both=3
 		integer*2 instrument   
c                              record type e.g.'E3'		
     		character(2) rec_type   
c                              start detector no. selected
		integer*2 startdet     
c                              stop     "         "
  		integer*2 stopdet      
c                              start BG det.no.(1-8)0 if no bg sub.
 		integer*2 startbg      
c                              stop     "             "	
		integer*2 stopbg       
c                              start bin no. (1-128)
                integer*2 startbin     
c                              stop    "
                integer*2 stopbin      
c                              words 9-10 spare
                byte spare7(4)         
c                              collimator efficiency(10**4=1.0)
		integer*2 coll_eff     
c                              word 12 spare
                byte spare8(2)         
c                              BG correction method: 0=none  		
c         		       1=auto; 2=by file
                integer*2 bgflag       
c                              BG correction file name (if bgflag=2)
		character(6) bgfile     
c                              minimum wait time(HTR3 only) 
                integer*2 minwait      
c                              avg deadtime correction factor
c 			       only if DT:CA (scaled st.10**3=1.0)
      		integer*2 dtfact       
c                              HK cts/sec for all dets(if DT:CA)
		integer*4 hkcount      
c                              words21-22 spare
		byte spare9(4)         
c                              LC correction factor (10**3=1.0)
		integer*4 lcfact       
c                              =1 if cts are dt corrected
		integer*2 dtflag       
c                              word 26 spare
		byte spare10(2)        
c                              start channel no.(as opp.to bin no)
		integer*2 startchan    
c                              end channel no.    "             
		integer*2 endchan      
c                              words 29-33 spare
		byte spare11(10)       
c                              no. of on-source detectors
                integer*2 ndet	       
c                              no. of BG detectors used
                integer*2 nbgdet       
c                              words 36-128 spare
		byte spare12(186)      
	end structure
c
c            GS handler
c
	structure/rnewbuf_gs/
c                              =4 ,meaning GS
                integer*2 instrument    
c                              app.program no.
 		integer*2 nprog         
c                              words 3-4 spare
 		byte spare13(4)         
c                              BL lower value
   		integer*2 blmin         
c                              BL upper value
 		integer*2 blmax	        
c                              start channel
		integer*2 startchan     
c                              stop channel
 		integer*2 stopchan      
c                              words 9-10 spare
		byte spare14(4)         
c                              collimator eff. scaled s.t. 1=1e4
		integer*2 coll_eff      
c                              words 12-128 spare
                byte spare15(234)       
c					  
	end structure
c
c            LE handler
c
	structure/rnewbuf_le/
c                              =5 for both L1,L2
        	integer*2 instrument    
c                              OBC AP no.
		integer*2 nprog  	
c                              filter wheel position no.
		integer*2 nfw		
c                              minimum wait time(2**-5 secs)
		integer*2 minwait	
c                              word 5-10 spare
		byte spare16(12)	
c                              exponent of normalis. applied to data
		integer*2 norm1		
c                              mantissa of normalis. applied to data
c                              normal.=norm2/3000.*10.**norm1
		integer*2 norm2		
c                              min. fract. expos. to accept a bin
c                              1=1e4
                integer*2 min_expo       
c                              version no. of prog. lcurv
                integer*2 lcurver       
c                              avg dead time correction (1=1e3)
                integer*2 dtfact        
c
c                              word 16-29 spare
		byte spare17(28)	
c                              =1 if dt corrected for sample rate
		integer*2 dtflag        
c                              =1 if corrected for telemetry avail.
		integer*2 telflag       
c                              =1 if BG subtracted
		integer*2 bgflag        
c                              =1 if source only, =0 if BG only
		integer*2 sbflag        
c                              =0 generally (normal cts in file)
c                              =1 for "existence profile"(bins=0,1)
c                              =2 for exposure time profile
c                              i.e. (bins=between 0.0 & 1.0)
		integer*2 proflag       
c                              no. of source boxes (up to 3)
		integer*2 nbox		
c                              no. of background boxes (up to 3)
c		               (only 2 allowed for in old format)
		integer*2 bgbox		
c                              low x coord source box 1
		integer*2 x1low
c                              low y coord source box 1
 	
    		integer*2 y1low     
c                              high x coord source box 1
 		integer*2 x1high
c                              high y coord source box 1
        
		integer*2 y1high        
c                              low x coord BG box 1
		integer*2 x1blow        
c                              low y coord BG box 1
		integer*2 y1blow        
c                              high x coord BG box 1
		integer*2 x1bhigh       
c                              high y coord BG box 1
		integer*2 y1bhigh       
c                              low x coord source box 2
		integer*2 x2low	        
c                              low y coord source box 2
    		integer*2 y2low         
c                              high x coord source box 2
 		integer*2 x2high        
c                              high y coord source box 2
		integer*2 y2high        
c                              low x coord BG box 2 
		integer*2 x2blow        
c                              low y coord BG box 2 
		integer*2 y2blow        
c                              high x coord BG box 2 
		integer*2 x2bhigh       
c                              high y coord BG box 2 
		integer*2 y2bhigh       
c                              low x coord source box 3
		integer*2 x3low	        
c                              low y coord source box 3
    		integer*2 y3low         
c                              high x coord source box 3
 		integer*2 x3high        
c                              high y coord source box 3
		integer*2 y3high        
c                              low x coord BG box 3 
		integer*2 x3blow        
c                              low y coord BG box 3 
		integer*2 y3blow        
c                              high x coord BG box 3 
		integer*2 x3bhigh       
c                              high y coord BG box 3 
		integer*2 y3bhigh       
c
c                              source x coordinate
                integer*2 xsource       
c                              source y coordinate
                integer*2 ysource       
c                              radius of source box
                integer*2 rsource       
c                              inner radius of bkgd box
                integer*2 ribkgd        
c                              outer radius of bkgd box
                integer*2 robkgd        
c                              (source area)/(bkgd area) 1=1e4
                integer*2 sb_rat        
c                              non-uniform bkgd corr. applied
c                              (1=1e4)
                integer*2 nu_bkgd       
c                              word 68-128 spare
		byte spare18(122)	
	end structure
c
c           SSS handler     (rev.2 start)
c
	structure/rnewbuf_sss/
c                              instrument mode 
                integer*2 instr_mode    
c                              word 2 spare
 		byte spare19(2)         
c                              shf key of last defrost 
                integer*4 shfdefrost    
c                              start channel
		integer*2 startchan     
c                              stop channel
 		integer*2 stopchan      
c                              opto rate scaled 10000 = 1.0 
                integer*4 optorate      
c                              what rate is this ? 
		integer*2 whatrate      
c                              words 10-128 spare
                byte spare20(238)       
c					  
	end structure
c                            (rev.2 stop)
c
c
c           MPC handler     (rev.3 start)
c
	structure/rnewbuf_mpc/
c                              start channel
		integer*2 startchan     
c                              stop channel
 		integer*2 stopchan      
c                              L cut-off (scaled 10000=1)
 		integer*2 lcutoff       
c                              B cut-off (scaled 10000=1)
 		integer*2 bcutoff       
c                              anticoinc. cut-off (scaled 100=1) 
 		integer*2 anticutoff    
c                              sequence no. 
                integer*2 seqno         
c                              words 7-128 spare
                byte spare21(244)       
c					  
	end structure
c                            (rev.3 stop)
c
c
c       HEAO1 A1 handler
c
	structure/rnewbuf_a1/

c                              10= HEAO1 A1
		integer*2 instrument 	
c                              spare
		integer*2 spare1a       
c                              catalog name ie :3u1656+28
	        character(16) name2      
c                              Galactic longitude (10**9 units)
        	integer*4 liii          
c                              Galactic latitude (10**9 units)
	        integer*4 biii          
c                              First major frame in the file
		integer*4 startmjf      
c                              Last major frame in file
		integer*4 stopmjf       
c                              First instrumnet mode
        	character(8) mode1       
c                              Second instrument mode
	        character(8) mode2       
c                              Third instrument mode
        	character(8) mode3       
c                              Total number of mjf's 
	        integer*2 mjftot        
c                              Lower energy range (times 100)
		integer*2 e1		
c                              Upper  energy range (times 100)
		integer*2 e2	
	end structure
c
c
	structure/rnewbuf_rec_2/
	    union
		map
c                              Makes sure the structure is 256 bytes
		   byte spare(256)
                end map
c
		map
		   record/rnewbuf_me/newme
		endmap
c
		map
		   record/rnewbuf_gs/newgs
                end map
c
		map
		   record/rnewbuf_le/newle
		endmap
c  Rev.2
		map
		   record/rnewbuf_sss/newsss
                end map
c  Rev.2
c  Rev.3
		map
		   record/rnewbuf_mpc/newmpc
                end map
c  Rev.3
                map
                   record/rnewbuf_a1/newa1
                end map
	end union
        end structure
c
c
c	record 3-4 contain 256 baryc. corrections if correct=2
c
	structure/rnewbuf_rec_3_4/
		integer*2 val(256)  
	end structure

c  	records 5-N
c
	structure/rdata_0/
c
c	     file type=0 ie. normal file (to be phased out)		
c	     each record contains 16 frames of data.
c	     contents of each frame given below
c
c                              word N; day no.
         	integer*2 nday		
c                              word N+1,N+2; time of day (msec)
		integer*4 time		
c                              word N+3; scaled deadtime (0-10000)
		integer*2 dt		
c                              word N+4,N+5; counts/sec
		real*4  count		
c                              word N+6,N+7; error (cts/sec)
		real*4  err		
		end structure
c
	integer*4 num_rdata_0
	parameter (num_rdata_0=16)
c
	integer*4 num_rdata_1
	parameter (num_rdata_1=256)
c
	structure/rdata_2/
c
c	     file type=2 ie. compressed low resolution file
c	     64 points per record
c
c                              word N ; cts/sec as semi-log
            	integer*2 count		
c                              word N+1; err/sec   "	
		integer*2 err		
	        end structure
	integer*4 num_rdata_2
	parameter (num_rdata_2=64)
c
	structure/rdata_Z/
c
c	     file type=4 ie. LE Z file
c	     64 times per record
c                              word N ; 2**-14 sec since requested
c			       start timec
            	integer*4 time		
	        end structure
	integer*4 num_rdata_Z
	parameter (num_rdata_Z=64)
	structure/rnewbuf_data/
	   union
	     map
c                              Makes sure the structure is 256 bytes
	        byte spare(256)
             end map
c the structure for type 0 file is not mapped with the others 
c because the alignment problem on the SUN version
	     map
		record/rdata_0/type_0(num_rdata_0)
	     endmap
c					
c	     file type=1 ie. msec file (to be phased out)
c
	     map
		byte type_1(num_rdata_1)
	     endmap
	     map
		record/rdata_2/type_2(num_rdata_2)
	     end map
	     map
		record/rdata_Z/type_Z(num_rdata_Z)
	     end map
	     end union
	end structure
c

