         Subroutine xsel_init(prefix_str,prefixlen)
c --------------------------------------------------------------------
c
c This is the starting-up routine of the xselect. It will be called by the 
C Tcl-main program. 
C
C The codes are borrowed from the xsel.f. For details and Modification 
C history, please read the  original code.   
C 
c  Authors:    Ning Gan, 1999 January V2.0
c              kaa   5/8/2013  modified to call common initialization routine
c
c -------------------------------------------------------------------

      implicit none
      INCLUDE 'xsel.inc'
      INCLUDE 'xselvar.inc'

c ---------------------------------------------
c DECLARATIONS
c ---------------------------------------------
c Scratch variables
c Strings used, reused, reused again as temporary space
      character(255) prefix_str
      integer prefixlen
      character(255) str1
      character(255) tmpf
c General reusable integers for lengths of strings 
      integer len1, len2, len3,len4
c ---------------------------------------------
c LENACT
      integer LENACT

C xslname for the tcl version
      character(255) txslnam

C Standard initialization common across all versions of xselect

      CALL xsel_common_init(prefix_str, prefixlen)

C Then load the .cmd1, .kw, .key , and .udc1 files. 

      txslnam = "t"//xslnam(:len(xslnam)-1)
      len1 = LENACT(txslnam)
      str1 = txslnam(:len1)

      len2 = LENACT(xsldsk)
      len3 = LENACT(xsldir)
      call ptend(xsldsk(:len2),xsldir(:len3),str1) 
      len4 = lenact(str1)
      tmpf = str1(:len4)//'.cmd1'
      CALL XPITBLDCM(tmpf,ierr)  
      if(ierr.ne.0) return 
     
      tmpf = str1(:len4)//'.kw'
      CALL TBLDKW1(tmpf,ierr)  
      if(ierr.ne.0) return 

      tmpf = str1(:len4)//'.key'
      CALL TBLDKY(tmpf,ierr)  
      if(ierr.ne.0) return 

      len1 = LENACT(xslnam)
      str1=xslnam(:len1)
      call ptend(xsldsk(:len2),xsldir(:len3),str1) 
      len4 = lenact(str1)
      tmpf = str1(:len4)//'.udc1'
      CALL TBLDAL(tmpf,ierr)  
      if(ierr.ne.0) return 

C     initialize the yaccfor common blocks
      call ldpospar("XSELECT_INIT")

C     define the commands
      call xsel_set_control

      Return 
      End 


