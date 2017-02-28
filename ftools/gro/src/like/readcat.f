C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C      subroutine readcat(ift,indx,itasc,imode,ival,fileid)
C
C
C  $Id: readcat.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C LIKE Version: 4.8 DELIVERED: November 8th 1993, Programmer J.R. MATTOX
C=======================================================================
C  Reads the MPE catalog of calibration files and returns the proper
C  two-digit suffix for the desired file.
C  Input arguments are:
C   ift:  File type
C     0 = EDP
C     1 = PSD
C     2 = SAR
C   indx: Event class (curve index)
C     1 = all
C     2 = A
C     3 = B
C     4 = C
C     5 = A+C
C   itasc: TASC coincidence mode
C     0 = TASC out
C     1 = TASC in
C
C  P. L. Nolan, Stanford University
C  26 Feb 1992
C=======================================================================
C LIKE Version: 4.1  DELIVERED: April 15 1993      JMATTOX
C+             UPDATED:    by  JRM
C=======================================================================
C%   Changes:                                                           
C%  6 Mar  1992 by JRM: port to gsfc, make into a subroutine
c
c   1995/22/01 by JAE: included mode and ival to input parameter list
c			imode = data mode
c			ival = 0 data before vp4030;  3 azimuth cal files
c			ival = 5 vertical only mode; 16 azimuth cal files
c			ival =10 all modes up to 74; 16 azimuth cal files
c	local variable  parama(29): holds initial values
c
c	changed ctalog call variable #3 from 10 to 29
c
C  $Log: readcat.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:42  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:53:06  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:58  jae
c Subroutine Module for like V5.00
c
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine readcat(ift,indx,itasc,imode,ival,fileid)
c
c Common blocks included
c
      INCLUDE '../COMMON/cnfrep.copy'
      INCLUDE '../COMMON/errrep.copy'

      save

      INTEGER*4      IFT,INDX,ITASC,imode,ival
      integer*2      parama(29),param(29)
      character(80)   id
      character(2)    fileid

      common /id/id
      data parama/1,0,0,0,2,7,74,6,120,1,19*0/

      id = '$Id: readcat.f,v 1.2 2013/05/21 19:08:26 irby Exp $'


      do j=1,29
         param(j)=parama(j)
      enddo

      param(5) = indx
      param(7) = imode
      param(10) = itasc
      param(28) = ival

      call ctalog(data_dir, ift,param,29,fileid,iret)

      if (iret.eq.0) then
      else
         fileid='  '
         signal='C'
         sigmsg='READCAT:Requested calfile not in catlog'
      endif

      RETURN
      end
