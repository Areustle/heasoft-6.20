* Last processed by NICE on 23-SEP-1991 16:43:08
* Customized for :  VAX         VMS          
* PI is defined with more digits than necessary to avoid losing
* the last few bits in the decimal to binary conversion
      REAL*8 PI
      PARAMETER (PI=3.14159265358979323846D0)
      REAL*4 PIS
      PARAMETER (PIS=3.141592653)
* Relative precision of REAL*4
      REAL*4 EPSR4
      PARAMETER (EPSR4=1E-7)
      REAL*4 MAXR4
      PARAMETER (MAXR4=1E38)
* Maximum acceptable integer
      INTEGER MAX_INTEG
      PARAMETER (MAX_INTEG=2147483647)
