      SUBROUTINE SENSTV1(DATA_DIR,SELECT,SLCTLV,ITYPS,WALLDI,ACS,
     *		         TASCCO,FILEID,FNAME,IRET,caltbl)   
C=======================================================================
C* Version: 2.3                        Date: 21 Nov 1994 
C*
C* Variable 'caltbl' included in order to select between 'old Calibration
C* files' (caltbl=0), 'Vertical only' .and. 'PMT C43 off' (caltbl=5)
C* as well as 'ALL modes enabled' .and. 'PMT C43 off' (caltbl=10).
C* 
C* This program returns the fileids FILEID(2) and the names FNAME(2) of 
C* the calibration file corresponding to the selection parameters.      
C*                                                                      
C*    INPUT PARAMETERS:                                                 
C*    SELECT    I*4     Selection parameter (e.g. 7= Event Classes      
C*    SLCTLV    I*4     Selection level (e.g. Event Classes:            
C*                                        1= ALL EVENTS                 
C*                                        2= CLASS A,                   
C*                                        3= CLASS B,                   
C*                                        4= CLASS C)                   
C*                                        5= CLASS A+C)                 
C*                                                                      
C*    ITYPS     I*2     Combination of type modes enabled               
C*                                                                      
C*    WALLDI    I*4     Wall Distance  (in mm)                          
C*    ACS       I*4     ACS  Sensitivity (0 OR 1) (not in use)          
C*    TASCCO    I*4     TASC in Coincidence flag: 0 = not in coincidence
C*                                                1 = in coincidence    
C*                                                    (only two lowest  
C*                                                    threshold allowed!
C*                                                                      
C*    Return parameters:                                                
C*    FILEID    C*2  dim(2)      CALIBRATION FILE NUMBERS               
C*                                1) sensitive area                     
C*                                2) energy dispersion                  
C*    FNAME     C*8  dim(2)      CALIBRATION FILE NAMES                 
C*                                1) sensitive area                     
C*                                2) energy dispersion                  
C*    IRET      I*4              Signal:  0 = sucessful completion      
C*                                        1 = one or both cal. files    
C*                                            don't exist               
C*                              Message: reason for failure             
C*                                                                      
C*    caltbl    I*4     indicator which kind of calibration files is
C*                      used (3 or 16 azimuths, resp)
C*			 0 = old calibration files (azimuth=3)
C*               	 5 = Vertical only, C43 off (azimuth=16)
C*      		10 = C43 off (azimuth=16)
C*                                                                         
C=======================================================================
C+ ISSUE:     STARTED: 24 NOV 1988    PROGRAMMER: C. VON MONTIGNY       
C+            UPDATED: 09 Jan 1995    BY  CVM                           
C=======================================================================
C%  CHANGES:                                                            
C%  12 JUL 1990 BY CVM: argument list changed according to document     
C%  12 JUL 1990 BY CVM:    issue 1, date: 11 Jul 90                     
C%  20 JUL 1990 BY CVM: changed according to new calfil files           
C%  13 AUG 1990 BY CVM: subroutine CTALOG + CMPARE attached             
C%  10 OCT 1990 BY CVM: select also for t-modes (ityps), error handling 
C%  11 OCT 1990 BY CVM:     and messages revised                        
C%  21 MAR 1991 BY CVM: error handling corrected                        
C%  03 JUN 1991 BY CVM: TASCCO put to param(10)                         
C%  21 Nov 1994 BY CVM: variable CALTBL included                         
C%  09 Jan 1995 BY CVM: cosmetics
c   08/09/01   Linux version  Only change is to initilize iret1 and iret2.
c   D.L.Bertsch
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      INTEGER*2 PARAM(30),ITYPS,TYPS(6),IANTI                           
      INTEGER*4 SELECT,SLCTLV,WALLDI,ACS,TASCCO,caltbl,j,iret,icode
      integer iret1,iret2           
      CHARACTER*(*) FNAME(2), DATA_DIR
      character(2)  FILEID(2)
      DATA PARAM/1,3*0,3*1,23*0/                                        
      DATA TYPS/x'007C',x'003C',x'001C',x'000C',x'0004',x'0000'/      
      character(80)	id
      common	/id/	id
      id = '$Id: senstv1.f,v 3.2 2013/05/21 19:08:27 irby Exp $'

      IRET=99
      iret1 = 0
      iret2 = 0
                                                           
      DO 10 j=1,6                                                       
         if(ITYPS.eq.TYPS(j)) IANTI= J-1                                
   10 continue                                                          
      PARAM(5)=SLCTLV                                                   
      PARAM(6)=SELECT                                                   
      PARAM(7)=74                                                       
      PARAM(8)=WALLDI                                                   
      PARAM(9)=120                                                      
      PARAM(10)=TASCCO                                                  
c     PARAM(28)=IANTI                     
      PARAM(28)=caltbl
c      write(*,*),'PARAM5,6,7: ', PARAM(5), ' ', PARAM(6), ' ', PARAM(7)
c      write(*,*),'PARAM8,9,10,28: ', PARAM(8), ' ', PARAM(9), ' ',
c     *     PARAM(10), ' ', PARAM(28)
C-----------------------------------------------------------------------
C                                     look for sensitive area file      
c      write(6,'('' 1 param=''/ 2(15i3/))') param                          
      CALL CTALOG(data_dir,2,PARAM,30,FILEID(1),ICODE)  
c      write(6,*) 'SENSTV1: ICODE=',icode                                
      IF (ICODE.EQ.2) THEN                                              
         WRITE(6,'('' SENSITIVE AREA CALIBRATION FILE DOES NOT EXIST FOR
     & EVENT SELECTION:'')')                                            
         write(6,100)select,slctlv,ityps, walldi,param(9),tascco,caltbl        
         IRET1=1                                                        
      ELSE IF (ICODE.EQ.0) THEN                                         
         IRET1=0                                                        
      END IF                                                            
C                                                                       
C      IF (IRET1.EQ.0) FNAME(1)='SARFIL'//FILEID(1)                      

      if (iret1 .eq. 0) then
         WRITE(6,*)'Using SARFILE with suffix:',fileid(1)
         fname(1)='sarfil'//fileid(1)
         i = index(fname(1),' ')-1
         fname(1)=fname(1)(1:i) // '.fits'
      endif

C-----------------------------------------------------------------------
C                                     look for energy dispersion file   
c     write(6,'('' 2 param=''/ 2(15i3/))') param                          
      CALL CTALOG(data_dir,0,PARAM,30,FILEID(2),ICODE)  
c     write(6,*)  'icode = ',icode,'   FILEID = ',fileid                 
C                                                                       
      IF (ICODE.EQ.2) THEN                                              
         WRITE(6,'('' ENERGY DISPERSION CALIBRATION FILE DOES NOT EXIST 
     &FOR EVENT SELECTION:'')')                                         
         write(6,100)select,slctlv,ityps, walldi,param(9),tascco,caltbl        
         IRET2=1                                                        
      ELSE IF (ICODE.EQ.0) THEN                                         
         IRET2=0                                                        
      END IF                                                            
C                                                                       
c      IF (IRET2.eq.0) FNAME(2)='EDPFIL'//FILEID(2)                      
      if (iret2 .eq. 0) then
         WRITE(6,*)'Using EDPFILE with suffix:',fileid(2)
         fname(2)='edpfil'//fileid(2)
         i = index(fname(2),' ')-1
         fname(2)=fname(2)(1:i) // '.fits'
      endif
C-----------------------------------------------------------------------
C                                       ERROR RETURN CODE               
      IF (IRET1.eq.0 .and. IRET2.eq.0) then                             
          IRET=0                                                        
      ELSE IF (IRET1.ne.0 .or. IRET2.ne.0) then                         
          IRET=1                                                        
      END IF 
                      
C-----------------------------------------------------------------------
      RETURN                                                            
C-----------------------------------------------------------------------
 100  FORMAT(' selected  parameter  :',i6                               
     &      /' selection   level    :',i6                               
     &      /' type mode combination: Z',z4                             
     &      /' wall   distance      :',i6                               
     &      /' vertex-beam  distance:',i6                               
     &      /' TASC in coincidence  :',i6                              
     &      /' CALTBL               :',i6)                              
C-----------------------------------------------------------------------
      END                                                               
