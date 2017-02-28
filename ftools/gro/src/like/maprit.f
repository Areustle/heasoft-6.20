C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C       SUBROUTINE MAPRIT(MAP,MAPTYP,MAPDOC,IMAPWIDTH,IMAPHEIGHT)
C
C
C  $Id: maprit.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C*     Effect: The MAP map is written to the disk file
C*             appropriate to the energy range.
c
c----------------------------------------------------------------------
c     	Subroutine Argument Desriptions
c   	^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c       REAL   map data matrix to write
c       INTEGER   IMAPWIDTH,IMAPHEIGHT working map size
c       CHARACTER 	MAPTYP*4		MAPTYP
c 	character(70) 	MAPDOC(10)		MAPDOC array
c
c-----------------------------------------------------------------------
C LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+             UPDATED:    by  JRM
c
C=======================================================================
C  $Log: maprit.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:39  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:52:00  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:55:16  jae
c Subroutine Module for like V5.00
c
C
c 	11/10/92, JRM: Add MAPTYPE_new
c 	11/12/92, JRM: Write pixel centered CRVALs
c
c
c-------------------------------------------------------------------------
      SUBROUTINE MAPRIT(MAP,MAPTYP,MAPDOC,IMAPWIDTH,IMAPHEIGHT)
C  Common blocks used:
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/likrep.copy'

      save
c
c-------------------------------------------------------------------------
      character(80) id
      common /id/id
      CHARACTER MAPTYP*4,MAPTYPE_new*20,BUNIT*20
      character(70) MAPDOC(10)
      REAL MAP(IMAPWIDTH,IMAPHEIGHT),VENG(10,2)
C
      id = '$Id: maprit.f,v 1.2 2013/05/21 19:08:26 irby Exp $'


      if (MAPFILE.eq.' '.or.MAPFILE.eq.'null') return

      do i=1,10
	 if(MAPDOC(i).eq.' ') goto 10
         ndoc=i
      enddo
 10   continue

c     if(ndoc.lt.10)then
c     ndoc=ndoc+1
c     MAPDOC(ndoc)=CTLDOC(2)
c     endif

      VENG(1,1)=CTLEMN
      VENG(1,2)=CTLEMX
      if (MAPTYP.eq.'CMAP') then
         BUNIT='Gamma-ray counts'
         MAPTYPE_new=BUNIT
      elseif (MAPTYP.eq.'CNTS') then
         BUNIT='Estimate of counts'
         MAPTYPE_new=BUNIT
      elseif (MAPTYP.eq.'GMAP') then
         BUNIT='cm^-2s^-1sr^-1'
         MAPTYPE_new='Model flux'
      elseif (MAPTYP.eq.'GBIA') then
         BUNIT='10^-5 cm^-2 s^-1 sr^-1'
         MAPTYPE_new='Isotropic flux'
      elseif (MAPTYP.eq.'GMUL') then
         BUNIT='dimensionless'
         MAPTYPE_new='Diffuse model factor'
      elseif (MAPTYP.eq.'EMAP') then
         BUNIT='cm^2 s sr'
         MAPTYPE_new='Exposure'
      elseif (MAPTYP.eq.'EDAY') then
         BUNIT='on-axis days'
         MAPTYPE_new='Exposure'
      elseif (MAPTYP.eq.'LSTA') then
         BUNIT='Likelihood ratio TS'
         MAPTYPE_new=BUNIT
      elseif (MAPTYP.eq.'RMAP') then
         BUNIT='Normlzd resid cnts'
         MAPTYPE_new=BUNIT
      elseif (MAPTYP.eq.'RFMP') then
         BUNIT='cm^-2 s^-1 sr^-1'
         MAPTYPE_new='Residual flux'
      elseif (MAPTYP.eq.'RCMP') then
         BUNIT='Residual counts'
         MAPTYPE_new=BUNIT
      elseif (MAPTYP.eq.'GCMP') then
         BUNIT='Model counts'
         MAPTYPE_new=BUNIT
      else
         BUNIT='Unknown'
         MAPTYPE_new='Unknown'
      endif

c     pixel centered RVAL
      CALL FITRIT(MAP,CTLMSZ1,CTLMSZ2,CTLSCL,CTLORG(1),
     &     CTLORG(2),coord_sys,1,VENG,BUNIT,LU(4),MAPFILE
     &     ,mapdoc,ndoc,MAPTYPE_new)
c     if (signal.ne.'') CALL ERROR(0,LOC)

	
      RETURN
      END
c
