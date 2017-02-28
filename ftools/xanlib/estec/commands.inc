c
c command common
c
c no read in, no to be listed
      INTEGER*4 zcom_no, zlist_no     
c max com, no not to be listed
      INTEGER*4 zncom, zlist_nolist   
c max no of commands
      PARAMETER (zncom=250)                  
c commands to be listed
cccc      character(10) zcom_list(zncom)           
      character(16) zcom_list(zncom)           
c description of them
      character(30) zcom_descrip_list(zncom)   
c all valid commands
cccc      character(10) zcom_name(zncom)          
      character(16) zcom_name(zncom)          
c description
      character(30) zcom_descrip(zncom)        
c hidden command list
cccc      character(10) zcom_nolist(zncom)        
      character(10) zcom_nolist(zncom)        
c description
      character(30) zcom_descrip_nolist(zncom)    
c current command name
      character(16) zcommand
      COMMON /command/ zcom_list, zcom_descrip_list, zcom_name, 
     &                 zcom_descrip, zcom_descrip_nolist, zcom_nolist, 
     &                 zcom_no, zlist_no, zlist_nolist, zcommand
