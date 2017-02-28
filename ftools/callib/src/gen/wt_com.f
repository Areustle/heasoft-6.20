* +WT_COM
c     ------------------------------------------------------------
      subroutine wt_com(infile,outfile,hduclas2,phaversn,fchan,extnum,
     &                  channel,counts,rcts,dtype,qerror,serr,
     &                  qsys,syserr,qqual,qualty,qgroup,grping,
     &                  qascale,ascale,qbscale,bscale,
     &                  nchan,detchans,conv,phsize,mnver,task,n_comm,
     &                  comms,ckeys,cform,ckrec,cdesc,
     &                  nckeys,killit,cmpmode,ctype,errflg,chatter)
c     ------------------------------------------------------------
c --- DESCRIPTION ----------------------------------------------------
c
c This subroutine is implemented if the user chooses to write to a 
c file, that is the commands WRITE and EXIT (in grppha)
c implement this routine. 
c It makes a copy of the input file, apart from the SPECTRUM extension, 
c this extension is written with the current changes in standard OGIP
c format.
c
c --- VARIABLES ------------------------------------------------------
c
      IMPLICIT NONE
      character*(*) infile,outfile,ckeys(*),ckrec(*),task, comms(*)  
      character*(*) hduclas2,phaversn,cform(*),cdesc(*),cmpmode
      integer phsize,chatter,nchan,nckeys,n_comm,extnum
      integer qualty(*),grping(*),channel(*)
      integer counts(*),dtype, errflg,detchans,fchan
      integer*2 conv(*)
      real syserr(*),serr(*),rcts(*),ascale(*),bscale(*)
      logical qqual,qgroup,qsys,qerror,killit
      logical qascale,qbscale          
      character*(*) mnver,ctype
     
c       
c
c --- VARIABLE DIRECTORY ---------------------------------------------
c
c Arguments ...
c
c
c infile      char  : PHA filename, user defined
c outfile     char  : Output filename, user defined
c chatter     int   : Chattines flag
c phsize      int   : Array dimensions
c channel     int   : Array of detector channels
c dtype       int   : Datatype, 1 if counts, 2 is rcts
c counts      int   : Array of observed counts
c rcts        real  : Array of count rate
c subinfo     char  : Subroutine info for user
c qualty      int   : Array of qualty flag
c grping      int   : Array of grouping flag
c serr        real  : Array of Observed statistical errors
c syserr      real  : Array of Fractional systematic error
c ascale      real  : Array of areascal values
c bscale      real  : Array of backscal values
c qgroup      logical: True if data is grouped
c qqual       logical: True if data has qualty flags
c qerror      logical: True if statistical errors included
c qsys        logical: True if systematic errors included
c qascale     logical: True if vector areascal
c qbscale     logical: True if vector backscal
c conv        int*2  : Conversion array, within the program
c                      some of the data arrays, such as
c                      qualty are stored as int*4, OGIP standard
c                      requires that they are wriiten in int*2,
c                      this array is used for such conversions
c ckeys       char   : Array containing keywords that can have
c                      their values changed (CHKEY command)
c cform       char   : Array containing CHKEY keywords and their
c                      format, "S" or "E"
c ckrec       char   : Array containing ckeys keyword records
c nckeys      int    : counter for the above
c                      
c --- CALLED ROUTINES ------------------------------------------------
c
c
c WT_COPYPHD    : This subroutine copys the primary header of infile
c WT_SPEC       : This routine writes the SPECTRUM extension with the
c                 modifications. The extension is written after the
c                 primary header.
c WT_COPY       : This copys the remaining extensions from the input
c                 file
c
c --- AUTHORS/MODIFICATION HISTORY -----------------------------------
c
c Rehana Yusaf (1993 March 31)
c Rehana Yusaf (1993 July 6)      : Pass ckeys,ckrec, and nckeys for
c                                   CHKEY command
c Rehana Yusaf (1993 Oct 21) 1.0.2; Pass detchans to wt_spec as this
c 				    is not always the same as nchan
c Rehana Yusaf (1993 Nov 18) 1.0.3; Pass hduclas2,phaversn and fchan
c			            as arguments to this and to wt_spec
c Rehana Yusaf (1994 July 7) 1.0.4; Additional argument passed to this
c                                   routine, cform,cdesc,this is later passed
c                                   to wt_spec
c                                   Replace cgetlun with ftgiou and ftfiou
c Rehana Yusaf (1994 Sept 13) 1.0.5; additional argument, killit
c Rehana Yusaf (1995 Dec 14) 1.0.6; add wtinfo and friends
c Rehana Yusaf (1996 Feb 22) 1.0.7; added cmpmode
c 
c Banashree Mitra Seifert (1996 March) 1.1.0:
c        . variable array size to carry over from calling routine 
c          instead of defining as phsize
c
c Banashree Mitra Seifert (1996 July) 1.2.0:
c        . ctype=CHANTYPE included to write into output file
c        . also FCPARS is introduced so that it separates infile and 
c          extension no. 
c Peter D Wilson (2000 Apr 13) 1.2.1:
c        . Open input file with READONLY priviledge and add status check
c kaa (2001 June 6) 1.3.0:
c        . Added support for vector AREASCAL and BACKSCAL
c --------------------------------------------------------------------

      character(5) version 
      parameter (version = '1.3.0')
*-
c --------------------------------------------------------------------
c
c
c --- INTERNALS ------------------------------------------------------
c 
      character(7) subname
      parameter (subname='wt_com')
      character(70) subinfo
      integer iunit,status
      integer nhdu,htype,block,specext,tothd
      logical endfile
      character(180) filename
      integer extn
c
c --- USER INFO ---
c
      subinfo = ' using '//subname//' '//version
      call wtinfo(chatter,10,1,subinfo)
      errflg = 0
c ----------------------------------------------------------------
c These next 3lines are introduced so that if user provides filename with
c extension, then it should separate out the extension and filename.
c Otherwise it was giving problem as it assumes filename as "filename+extension"
c version 1.2.0

      status=0
      call fcpars(infile,filename,extn,status)
      infile=filename
c ----------------------------------------------------------------
c
c --- FIND SPECTRUM EXTENSION NUMBER ---
c
      status = 0
      call ftgiou(iunit,status)
      IF (status.NE.0) THEN
         subinfo = ' finding free unit number'
         call wterrm(subname,version,subinfo)
         errflg = status
         return
      ENDIF 

      call ftopen(iunit,infile,0,block,status)
      IF (status.NE.0) THEN
         subinfo = ' opening input file'
         call wterrm(subname,version,subinfo)
         errflg = status
         return
      ENDIF 

      endfile =.false.
      nhdu = 1
      do WHILE(.NOT.endfile)
        status = 0
        call ftmahd(iunit,nhdu,htype,status)
        IF ((status.EQ.107).OR.(status.EQ.207)) THEN
           endfile = .true.
           tothd = nhdu - 2
        ENDIF
        nhdu = nhdu + 1
      enddo
      call ftclos(iunit,status)
      call ftfiou(iunit,status)
      if (extn .le. 0) then
          specext = extnum
      else
          specext = extn
      endif
c
c --- COPY PRIMARY HEADER AND PRIMARY ARRAY ---
c
      call copyphd(infile,outfile,killit,errflg,chatter)
      IF (errflg.NE.0) THEN
        subinfo = ' writing primary header'
        call wterrm(subname,version,subinfo)
        return
      ENDIF
c
c --- APPEND SPECTRUM EXTENSION WITH MODIFICATIONS ---
c

      call wt_spec(infile,outfile,hduclas2,phaversn,fchan,
     &             channel,counts,rcts,dtype,qerror,serr,
     &             qsys,syserr,qqual,qualty,qgroup,grping,
     &             qascale,ascale,qbscale,bscale,
     &             nchan,detchans,conv,phsize,specext,mnver,task,
     &             n_comm,comms,ckeys,cform,ckrec,cdesc,nckeys,cmpmode,
     &             ctype,errflg,chatter)
      IF (errflg.NE.0) THEN
        subinfo = ' writing PHA extension'
        call wterrm(subname,version,subinfo)
        return
      ENDIF        
c
c --- COPY EVERYTHING EXCEPT SPECTRUM EXTENSION ---
c
      call wt_copy(infile,outfile,specext,tothd,chatter)
      return
      end
c -------------------------------------------------------------------
c     END OF WT_COM
c -------------------------------------------------------------------

