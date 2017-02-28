C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       subroutine DOCADD(nchar,MAPDOC,MAPTYP,string)
C
C
C  $Id: docadd.f,v 1.2 2013/05/21 19:08:25 irby Exp $
C=======================================================================
C++             effect: add documentation to map headers
C=======================================================================
C LIKE Version: 4
C
C
C  $Log: docadd.f,v $
C  Revision 1.2  2013/05/21 19:08:25  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:29  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:47:24  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:36:43  jae
c Subroutine Module for like V5.00
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      subroutine DOCADD(nchar,MAPDOC,MAPTYP,string)

C     Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/errrep.copy'

      save

      character(80) id
      common /id/id
      CHARACTER  MAPTYP*4, MAPDOC*700,string


      id = '$Id: docadd.f,v 1.2 2013/05/21 19:08:25 irby Exp $'
      LOC='DOCADD '

      i=700
      do while (mapdoc(i:i).eq.' ')
         i=i-1
         if (i.lt.1) then
	    i=1
	    goto 100
         endif
      enddo
	
 100  if (700-i-1.lt.nchar) then
         WRITE(lu(1),*)"DOCADD: Requested to add,"
         WRITE(lu(1),*)string(1:nchar)
         WRITE(lu(1),*)
     &        "to ",MAPTYP," MAP documentation header,",
     &        " but didn't because the header is full."
         return
      endif

      mapdoc=mapdoc(1:i)//' '//string(1:nchar)
	
      return
      end
c
