*+ WT_REGDESC
	subroutine wt_regdesc(chatter, ounit, 
     &	pixsize,
     &  nsreg, sdesc,sfil,sname,sframe,
     &	sxname, sxunit, syname, syunit,
     &	sxblk,syblk,sxmin,sxmax,symin,symax,
     &  sxref, syref, npixsou, sxpsc, sypsc,
     &  nbreg, bdesc,bfil,bname,bframe,
     &	bxname, bxunit, byname, byunit,
     &	bxblk,byblk,bxmin,bxmax,bymin,bymax,
     &  bxref, byref, npixbkg, bxpsc, bypsc,
     &	ierr)

	IMPLICIT NONE
	integer chatter, ounit, ierr
	integer sxblk,syblk,bxblk,byblk
	integer nsreg, nbreg
	real npixsou, npixbkg, pixsize
	real sxmin, sxmax, symin, symax
	real sxref, syref
	real bxmin, bxmax, bymin, bymax
	real bxref, byref
	real sxpsc, sypsc, bxpsc, bypsc
	character(70) sdesc(*),bdesc(*), sfil, bfil
	character(70) sname, sframe, bname, bframe
	character(70) sxname, syname, bxname, byname
	character(70) sxunit, syunit, bxunit, byunit
c
c Description
c   Writes the region descriptors to the current FITS header unit
c The following keywords may be written (assuming valid values)
c  PIXSIZE  - (real) 'Pixel size (degrees)'
c  SR_NDSC  - (int)  'No. of SR_DSC strings/keywords'
c  SR_DSCnn - (char) 'source region descriptor'
c  SR_FILE  - (char) 'filename from which SR_DSCnn defined'
c  SR_IMAG  - (char) 'name of Source image'
c  SR_FRAM  - (char) 'coordinate frame of SR_IMAG'
c  SR_XNAM  - (char) 'name of X-axis of SR_IMAG'
c  SR_XUNT  - (char) 'physical units of SR_XNAM'
c  SR_YNAM  - (char) 'name of Y-axis of SR_IMAG'
c  SR_YUNT  - (char) 'physical units of SR_YNAM'
c  SR_XBLK  - (int)  'X-coord blocking factor for SR_DSC values'
c  SR_XMIN  - (real) 'Min X-coordinate in image SR_IMAG'
c  SR_XMAX  - (real) 'Max X-coordinate in image SR_IMAG'
c  SR_YBLK  - (int)  'Y-coord blocking factor for SR_DSC values'
c  SR_YMIN  - (real) 'Min Y-coordinate in image SR_IMAG'
c  SR_YMAX  - (real) 'Max Y-coordinate in image SR_IMAG'
c  NPIXSOU  - (real) 'No. pixels in source region'
c  SR_XREF  - (real) 'Reference point in X-coord of SR_DSCnn'
c  SR_YREF  - (real) 'Reference point in Y-coord of SR_DSCnn'
c  SR_XPSC  - (real) 'Pixel size (in degrees) of X-axis of SR_IMAG'
c  SR_YPSC  - (real) 'Pixel size (in degrees) of Y-axis of SR_IMAG'
c  BR_NDSC  - (int)  'No. of BR_DSC strings/keywords'
c  BR_DSCnn - (char) 'background region descriptor'
c  BR_FILE  - (char) 'filename from which BR_DSCnn defined'
c  BR_IMAG  - (char) 'name of Background image'
c  BR_FRAM  - (char) 'coordinate frame of BR_IMAG'
c  BR_XNAM  - (char) 'name of X-axis of BR_IMAG'
c  BR_XUNT  - (char) 'physical units of BR_XNAM'
c  BR_YNAM  - (char) 'name of Y-axis of BR_IMAG'
c  BR_YUNT  - (char) 'physical units of BR_YNAM'
c  BR_XBLK  - (int)  'X-coord blocking factor for BR_DSC values'
c  BR_XMIN  - (real) 'Min X-coordinate in image BR_IMAG'
c  BR_XMAX  - (real) 'Max X-coordinate in image BR_IMAG'
c  BR_YBLK  - (int)  'X-coord blocking factor for BR_DSC values'
c  BR_YMIN  - (real) 'Min Y-coordinate in image BR_IMAG'
c  BR_YMAX  - (real) 'Max Y-coordinate in image BR_IMAG'
c  NPIXBACK - (real) 'No. pixels in Background region'
c  BR_XREF  - (real) 'Reference point in X-coord of BR_DSCnn'
c  BR_YREF  - (real) 'Reference point in Y-coord of BR_DSCnn'
c  BR_XPSC  - (real) 'Pixel size (in degrees) of X-axis of BR_IMAG'
c  BR_YPSC  - (real) 'Pixel size (in degrees) of Y-axis of BR_IMAG'
c
c Passed Parameters
c  CHATTER	i   : Chattiness flag
c  OUNIT        i   : FORTRAN logical unit of o/p file
c  PIXSIZE      i   : Pixel size in degrees
c  NSREG        i   : Number of source region keywords to be written
c  SDESC        i   : Array of source region descriptor strings
c  SFIL         i   : filename from which source region defined
c  SNAME        i   : name of Source image
c  SFRAME       i   : coordinate frame of source image
c  SXBLK        i   : X-coordinate blocking factor for source image
c  SYBLK        i   : Y-coordinate blocking factor for source image
c  SXMIN        i   : Min X-coordinate in source image
c  SXMAX        i   : Max X-coordinate in source image
c  SYMIN        i   : Min Y-coordinate in source image
c  SYMAX        i   : Max Y-coordinate in source image
c  NBREG        i   : No. of background region descriptors to be written
c  BDESC        i   : array of background region descriptor strings 
c  BFIL         i   : filename from which BRd region defined
c  BNAME        i   : name of Background image
c  BFRAME       i   : coordinate frame of BRd image
c  BXBLK        i   : X-coordinate blocking factor for BRd image
c  BYBLK        i   : Y-coordinate blocking factor for BRd image
c  BXMIN        i   : Min X-coordinate in BRd image
c  BXMAX        i   : Max X-coordinate in BRd image
c  BYMIN        i   : Min Y-coordinate in BRd image
c  BYMAX        i   : Max Y-coordinate in BRd image
c  NPIXSOU      i   : No pixels in Source region
c  NPIXBKG      i   : No pixels in Background region
c  SXREF        i   : Reference point in X-coord of source descriptor
c  SYREF        i   : Reference point in Y-coord of source descriptor
c  BXREF        i   : Reference point in X-coord of BRd descriptor
c  BYREF        i   : Reference point in Y-coord of BRd descriptor
c  IERR           o : Return error flag (0 = OK)
c
c Called Routines
c  subroutine FCECHO            : (FTOOLS) writes to standard o/p
c  subroutine FTPHIS            : (FITSIO) writes a history keyword
c  subroutine FTPKYn            : (FITSIO) writes a keyword of type n
c  subroutine WT_FERRMSG        : (CALLIB) writes FITSIO error message etc
c
c Origin
c   An IMG original (and cant you just tell)
c
c Authors/Modification History
c  Ian M George    (1.1.0:1993 Jun 13), first 'proper' version
c  Ian M George    (1.1.1:1993 Jun 20), added npixsou, npixbkg, qBR
c  Ian M George    (1.1.2:1993 Jul 25), added sxref,syref,bxref,byref
c  Ian M George    (1.2.0:1993 Aug 22), added sr_dsc & bk_dsc keys
c  Ian M George    (1.3.0:1994 Mar 17), npixsou/npixbkgd reals + pixsize
c
	character(7) version
	parameter (version = '1.3.0')
*-

c Internals
	integer status, decimals
	integer ir
	parameter (decimals = 6)
	character(8) keywrd
	character(30) errstr, wrnstr
	character(80) message

c Initialize	
	errstr = '** WT_REGDESC ERROR: '
	wrnstr = '** WT_REGDESC WARNING: '
	ierr = 0
	status = 0

c Give users info if really wanted
	if(chatter.GE.20) then
		message = ' ... using WT_REGDESC '// version
		call fcecho(message)
	endif

c Add a history record to this effect
	if(nsreg.GT.0 .OR. nbreg.GT.0) then
	message = ' Region Descriptors written by WT_REGDESC '// version
	call FTPHIS(ounit, message, status)
	message = wrnstr // ' Problem writing History record'
	call wt_ferrmsg(status,message)
	status = 0
	endif


c Pixel size
	if(pixsize.GT.0.0) then
	   call FTPKYE(ounit, 'PIXSIZE', 
     &		pixsize, decimals,
     &		'Pixel size (degrees)',
     &		status)
	   message = wrnstr // ' Putting PIXSIZE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

c Start adding the SOURCE region descriptors

	if(nsreg.GT.0) then
	   call FTPKYJ(ounit, 'SR_NDSC', 
     &		nsreg, 
     &		'Number of source region descriptors',
     &		status)
	   message = wrnstr // ' Putting SR_NDSC keyword'
	   call wt_ferrmsg(status,message)
	   status = 0


	do ir = 1, nsreg
	   write(keywrd,'(a6,i2)') 'SR_DSC',ir
	   call crmvblk(keywrd)
	   call FTPKYS(ounit, keywrd, 
     &		sdesc(ir), 
     &		'source region descriptor',
     &		status)
	   message = wrnstr // ' Putting '//keywrd//' keyword'
	   call wt_ferrmsg(status,message)
	   if(status.NE.0) then
	      message = ' - (missing record) fitsio illegal character ?'
	      status = 0
	      call FTPKYS(ounit, keywrd, 
     &		message, 
     &		'source region descriptor',
     &		status)
	   endif
	   status = 0
	enddo

	if(sfil.EQ.' ') sfil = 'UNKNOWN' 
	   call FTPKYS(ounit, 'SR_FILE', 
     &		sfil, 
     &		'filename from which SR_DSC defined',
     &		status)
	   message = wrnstr // ' Putting SR_FILE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(sname.EQ.' ') sname = 'NONE/UNKNOWN' 
	   call FTPKYS(ounit, 'SR_IMAG', 
     &		sname, 
     &		'name of Source image',
     &		status)
	   message = wrnstr // ' Putting SR_IMAG keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(sframe.EQ.' ') sframe = 'UNKNOWN'
	   call FTPKYS(ounit, 'SR_FRAM', 
     &		sframe, 
     &		'coordinate frame of SR_IMAG',
     &		status)
	   message = wrnstr // ' Putting SR_FRAM keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(sxname.EQ.' ') sxname = 'UNKNOWN'
	   call FTPKYS(ounit, 'SR_XNAM', 
     &		sxname, 
     &		'name of X-axis of SR_IMAG',
     &		status)
	   message = wrnstr // ' Putting SR_XNAM keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(sxunit.EQ.' ') sxunit = 'UNKNOWN'
	   call FTPKYS(ounit, 'SR_XUNT', 
     &		sxunit, 
     &		'physical units of SR_XNAM',
     &		status)
	   message = wrnstr // ' Putting SR_XUNT keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(syname.EQ.' ') syname = 'UNKNOWN'
	   call FTPKYS(ounit, 'SR_YNAM', 
     &		syname, 
     &		'name of Y-axis of SR_IMAG',
     &		status)
	   message = wrnstr // ' Putting SR_YNAM keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(syunit.EQ.' ') syunit = 'UNKNOWN'
	   call FTPKYS(ounit, 'SR_YUNT', 
     &		syunit, 
     &		'physical units of SR_YNAM',
     &		status)
	   message = wrnstr // ' Putting SR_YUNT keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   if(sxblk.GT.0.0) then
	   call FTPKYJ(ounit, 'SR_XBLK', 
     &		sxblk, 
     &		'X-coordinate blocking factor for SR_IMAG',
     &		status)
	   message = wrnstr // ' Putting SR_XBLK keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   call FTPKYF(ounit, 'SR_XMIN', 
     &		sxmin, decimals,
     &		'Min X-coordinate in image SR_IMAG',
     &		status)
	   message = wrnstr // ' Putting SR_XMIN keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYF(ounit, 'SR_XMAX', 
     &		sxmax, decimals,
     &		'Max X-coordinate in image SR_IMAG',
     &		status)
	   message = wrnstr // ' Putting SR_XMAX keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   if(syblk.GT.0.0) then
	   call FTPKYJ(ounit, 'SR_YBLK', 
     &		syblk, 
     &		'Y-coordinate blocking factor for SR_IMAG',
     &		status)
	   message = wrnstr // ' Putting SR_YBLK keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   call FTPKYF(ounit, 'SR_YMIN', 
     &		symin, decimals,
     &		'Min Y-coordinate in image SR_IMAG',
     &		status)
	   message = wrnstr // ' Putting SR_YMIN keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYF(ounit, 'SR_YMAX', 
     &		symax, decimals,
     &		'Max Y-coordinate in image SR_IMAG',
     &		status)
	   message = wrnstr // ' Putting SR_YMAX keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(npixsou.GE.0) then
	   call FTPKYE(ounit, 'NPIXSOU', 
     &		npixsou,decimals, 
     &		'No. pixels in source region',
     &		status)
	   message = wrnstr // ' Putting NPIXSOU keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(sxref.GT.-99999) then
	   call FTPKYE(ounit, 'SR_XREF', 
     &		sxref, decimals,
     &		'Reference point in X-coord of SR_DSC',
     &		status)
	   message = wrnstr // ' Putting SR_XREF keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(syref.GT.-99999) then
	   call FTPKYE(ounit, 'SR_YREF', 
     &		syref, decimals,
     &		'Reference point in Y-coord of SR_DSC',
     &		status)
	   message = wrnstr // ' Putting SR_YREF keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif


	if(sxpsc.GT.0.0) then
	   call FTPKYE(ounit, 'SR_XPSC', 
     &		sxpsc, decimals,
     &		'Pixel size (in degrees) of X-axis of SR_IMAG',
     &		status)
	   message = wrnstr // ' Putting SR_XPSC keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(sypsc.GT.0.0) then
	   call FTPKYE(ounit, 'SR_YPSC', 
     &		sypsc, decimals,
     &		'Pixel size (in degrees) of Y-axis of SR_IMAG',
     &		status)
	   message = wrnstr // ' Putting SR_YPSC keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif
	endif




c Add the BACKGROUND region descriptors, if defined

	if(nbreg.LE.0) then
		if(chatter.GE.20) then
		 message = ' ... No Background region specified'
		 call fcecho(message)
		endif 
		goto 946
	else
	   call FTPKYJ(ounit, 'BR_NDSC', 
     &		nbreg, 
     &		'Number of bkgd region descriptors',
     &		status)
	   message = wrnstr // ' Putting BR_NDSC keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   do ir = 1, nbreg
	   write(keywrd,'(a6,i2)') 'BR_DSC',ir
	   call crmvblk(keywrd)
	   call FTPKYS(ounit, keywrd, 
     &		bdesc(ir), 
     &		'background region descriptor',
     &		status)
	   message = wrnstr // 'Putting '// keywrd //' keyword'
	   call wt_ferrmsg(status,message)
	   if(status.NE.0) then
	      message = ' - (missing record) fitsio illegal character ?'
	      status = 0
	      call FTPKYS(ounit, keywrd, 
     &		message, 
     &		'background region descriptor',
     &		status)
	      status = 0
	   endif
	   enddo

	if(bfil.EQ.' ') bfil = 'UNKNOWN' 
	   call FTPKYS(ounit, 'BR_FILE', 
     &		bfil, 
     &		'filename from which BR_DSC defined',
     &		status)
	   message = wrnstr // ' Putting BR_FILE keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(bname.EQ.' ') bname = 'NONE/UNKNOWN' 
	   call FTPKYS(ounit, 'BR_IMAG', 
     &		bname, 
     &		'name of Background image',
     &		status)
	   message = wrnstr // ' Putting BR_IMAG keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(bframe.EQ.' ') bframe = 'UNKNOWN'
	   call FTPKYS(ounit, 'BR_FRAM', 
     &		bframe, 
     &		'coordinate frame of BR_IMAG',
     &		status)
	   message = wrnstr // ' Putting BR_FRAM keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(bxname.EQ.' ') bxname = 'UNKNOWN'
	   call FTPKYS(ounit, 'BR_XNAM', 
     &		bxname, 
     &		'name of X-axis of BR_IMAG',
     &		status)
	   message = wrnstr // ' Putting BR_XNAM keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(bxunit.EQ.' ') bxunit = 'UNKNOWN'
	   call FTPKYS(ounit, 'BR_XUNT', 
     &		bxunit, 
     &		'physical units of BR_XNAM',
     &		status)
	   message = wrnstr // ' Putting BR_XUNT keyword'
	   call wt_ferrmsg(status,message)
	   status = 0


	if(byname.EQ.' ') byname = 'UNKNOWN'
	   call FTPKYS(ounit, 'BR_YNAM', 
     &		byname, 
     &		'name of Y-axis of BR_IMAG',
     &		status)
	   message = wrnstr // ' Putting BR_YNAM keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(byunit.EQ.' ') byunit = 'UNKNOWN'
	   call FTPKYS(ounit, 'BR_YUNT', 
     &		byunit, 
     &		'physical units of BR_YNAM',
     &		status)
	   message = wrnstr // ' Putting BR_YUNT keyword'
	   call wt_ferrmsg(status,message)
	   status = 0


	   if(bxblk.GT.0.0) then
	   call FTPKYJ(ounit, 'BR_XBLK', 
     &		bxblk, 
     &		'X-coordinate blocking factor for BR_IMAG',
     &		status)
	   message = wrnstr // ' Putting BR_XBLK keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   call FTPKYF(ounit, 'BR_XMIN', 
     &		bxmin, decimals,
     &		'Min X-coordinate in image BR_IMAG',
     &		status)
	   message = wrnstr // ' Putting BR_XMIN keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYF(ounit, 'BR_XMAX', 
     &		bxmax, decimals,
     &		'Max X-coordinate in image BR_IMAG',
     &		status)
	   message = wrnstr // ' Putting BR_XMAX keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   if(byblk.GT.0.0) then
	   call FTPKYJ(ounit, 'BR_YBLK', 
     &		byblk, 
     &		'Y-coordinate blocking factor for BR_IMAG',
     &		status)
	   message = wrnstr // ' Putting BR_YBLK keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	   endif

	   call FTPKYF(ounit, 'BR_YMIN', 
     &		bymin, decimals,
     &		'Min Y-coordinate in image BR_IMAG',
     &		status)
	   message = wrnstr // ' Putting BR_YMIN keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	   call FTPKYF(ounit, 'BR_YMAX', 
     &		bymax, decimals,
     &		'Max Y-coordinate in image BR_IMAG',
     &		status)
	   message = wrnstr // ' Putting BR_YMAX keyword'
	   call wt_ferrmsg(status,message)
	   status = 0

	if(npixbkg.GE.0) then
	   call FTPKYE(ounit, 'NPIXBACK', 
     &		npixbkg, decimals, 
     &		'No. pixels in background region',
     &		status)
	   message = wrnstr // ' Putting NPIXBACK keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(bxref.GT.-99999) then
	   call FTPKYE(ounit, 'BR_XREF', 
     &		bxref, decimals,
     &		'Reference point in X-coord of BR_DSC',
     &		status)
	   message = wrnstr // ' Putting BR_XREF keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(byref.GT.-99999) then
	   call FTPKYE(ounit, 'BR_YREF', 
     &		byref, decimals,
     &		'Reference point in Y-coord of BR_DSC',
     &		status)
	   message = wrnstr // ' Putting BR_YREF keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif


	if(bxpsc.GT.0.0) then
	   call FTPKYE(ounit, 'BR_XPSC', 
     &		bxpsc, decimals,
     &		'Pixel size (in degrees) of X-axis of BR_IMAG',
     &		status)
	   message = wrnstr // ' Putting BR_XPSC keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	if(bypsc.GT.0.0) then
	   call FTPKYE(ounit, 'BR_YPSC', 
     &		bypsc, decimals,
     &		'Pixel size (in degrees) of Y-axis of BR_IMAG',
     &		status)
	   message = wrnstr // ' Putting BR_YPSC keyword'
	   call wt_ferrmsg(status,message)
	   status = 0
	endif

	endif





946	Return
	End
