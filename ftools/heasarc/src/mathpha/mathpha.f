*       +MATHPHA
      SUBROUTINE MATHPA

      IMPLICIT NONE
c 
c Description:
c  Program to perform mathematical operations on PHA files
c  NOTE - The par file is assumed to have been opened. 
c
c Passed Parameters
c  None
c
c User i/ps required (prompted for):
c  None here, isolated in GP_MATHPHA (see below)
c
c Include/Common blocks files
c  common TASK                   : (FTOOLS) standard fatal error message thingy
c 
c Called routines
c  subroutine GP_MATHPHA        : (below) Gets parameters from XPI par file
c  subroutine DO_MATHPHA        : (below) Performs the operations
c  subroutine FCECHO              : (ftools) Writes to standard o/p
c  subroutine FCERR            : (ftools) Writes standard error message
c  
c Compilation:
c  subroutines require CALLIB, FTOOLS, FITSIO
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1993 Nov 22), quick & a little dirty 
c  Ian M George     (2.0.0:1994 Mar 09), added Dynamic Memory Allocation
c  Ian M George     (2.1.0:1994 Mar 30), added Pois error recipe
c  Ian M George     (3.0.0:1994 Apr 21), added units parameter
c  Ian M George     (3.0.2:1994 Apr 22), cleaned up DMA
c  Rehana Yusaf     (3.0.3:1994 Sept 12), added clobber parameter
c                                         updated DO_MATHPHA and GP_MATHPHA
c  Ian M George     (3.0.4:1994 Oct 22), bug fix in GT_PHA
c  Ian M George     (3.1.0:1994 Dec 07), bug fix in cases of "missing" chans
c  Ian M George     (3.1.1:1994 Dec 12), cleaned-up after above
c  Ian M George     (3.2.0:1994 Dec 13), added areascal warnings throughout
c  Ian M George     (3.3.0:1995 Mar 13), fchan bug fix in GT_PHA
c  Ian M George     (3.4.0:1995 Apr 10), new checking/testing in populate
c  Ian M George     (3.5.0:1995 Apr 12), added parameters for 'aux files'
c  Ian M George     (3.5.1:1995 Apr 17), added areascal parameter
c  Ian M George     (3.5.2:1995 Apr 20), bug fix for areascal = 'NULL'
c  Ian M George     (3.5.3:1995 Jun 05), bug fix in POPULATE
c  Ian M George     (4.0.0:1995 Aug 01), added qproperr & errmeth params
c  Ian M George     (4.0.1:1995 Dec 02) fixed bug for errmeth=G in GT_PHA
c
c Banashree Mitra Seifert (4.1.0:1996 MAr 20) 
c       . Introduced screen display routines
c       . taskname replaced to 8 characters instead of 40 characters
c  Ian M George     (4.1.1:1995 Sep 25) fixed bug for messages>60 chars
c  Ian M George     (4.1.2:1995 Sep 25) maxstack passed to fndpm w/ checks added
c  Banashree Mitra Seifert (4.2.0:1997 Mar 27)
c       . backscal, corscal, areascal and texpos if given on commandline
c         then it updates the output file keywords with these values
c         (Ken Wanted this way)
c  Banashree Mitra Seifert (4.3.0:1997 June 9)
c       . bug that it didnot do backscal, corrscal factors right in
c         subroutine populate line # 3496,3592
c  Banashree Mitra Seifert (4.4.0:1997 Aug 8)
c       . calculation of exposure factor was misplaced. 
c         was not working if exposure was given as filename.
c         line#1016 needed to go below if statement 
c  Banashree Mitra Seifert (4.5.0:1997 Aug 13)
c       . when backscal & corrscal=<value> is given on the commandline, 
c         it didnot update to the value. This has been corrected now
c         in the routine POPULATE
c         NOTE: <value> must not contain any letter, e.g 1.0e-1
c               in that case value=0.1
c  Peter D Wilson (4.5.1:1998 Apr 3)
c       . In do_mathpha, initialize filename strings to ' ', without
c         which the strings contained all zeros which is not considered
c         a blank string in str.EQ.' ' comparisons.
c       . In mainloop, initialize qdiv array to .false.
c Peter D Wilson (June 29, 1998) 4.5.2:
c       . Added max_xflt parameter to rdpha1 function call
c  toliver (4.5.3:1998 Aug 06)
c       . variable initializations in do_mathpha and gt_pha
c Peter D Wilson (Sept 04, 1998) 4.5.4:
c       . more status=0 initializations added!!!
c James Peachey (4.5.5:1999 Mar 5)
c       . maxcomm increased to 256 to avoid overwriting other
c         variables with runaway comments. Really, bounds checking
c         should be added someday. By the way, note that maxcomm
c         denotes the maximum number of *comments*, as distinct
c         from maxcom, (with one m), which is presumably the
c         maximum number of *commands*. Hard to believe, huh?
c
c AM (04.28.99) 4.5.6:
c        . added a new option for the errmeth parameter,'POISS-0'.
c          For this option disabled writing the STAT_ERR column and 
c          changed the POISSERR keyword to 'T'.
c Peter D Wilson (Sept 03, 199) 4.5.7:
c        . Check for single-quote character within expression.  Accept
c          all characters within them as part of a file name.  This
c          allows unix paths to be used without being interpretted as
c          a division operator.
c
c ---------------------------------------------------------------------
      character(8) taskname
      character(7) version
      parameter (version = '4.5.7')
*- 
c Internals 
      integer maxucom
      parameter (maxucom=20)
      integer chatter, ierr, nk_ucom
      real divzero
      character(1) alg_units
      character(5) phaversn
      character(10) errmeth
      character(70) ucomm(maxucom)
      character(80) outfil, message
      character(80) exposexp
      character(80) backfexp, backsexp, corfexp, corsexp
      character(80) rmfexp, arfexp, areascalexp
      character(16384) expr
      logical killit, qproperr

c Initialize
      taskname ='mathpha'
      ierr = 0

      call wtbegm(taskname,version,chatter)

c Get Parameters from the par file
      call gp_mathpha(expr, alg_units, 
     &                outfil, chatter, phaversn, divzero, 
     &                exposexp, areascalexp, nk_ucom, ucomm, killit, 
     &                backfexp, backsexp, corfexp, corsexp,
     &                rmfexp, arfexp, qproperr, errmeth, ierr)
      if(ierr.NE.0) goto 926

c Do the nasty deed      
      call do_mathpha(expr, alg_units,
     &                outfil, chatter, phaversn, divzero,
     &                exposexp, areascalexp, nk_ucom, ucomm, killit, 
     &                backfexp, backsexp, corfexp, corsexp,
     &                rmfexp, arfexp, qproperr, errmeth, ierr)

c
 926  if(ierr.NE.0) then
         message = 'returning from do_mathpha'
         call wterrm(taskname,version,message)
      endif
        
      call wtendm(taskname,version,ierr,chatter)


      return
      end

c -------------------------------------------------------------------------
*+GP_MATHPHA
      subroutine gp_mathpha(expr, alg_units,
     &                      outfil, chatter, phaversn,divzero,
     &                      exposexp, areascalexp, nk_ucom, ucomm, 
     &                      killit, backfexp, backsexp, corfexp,
     &                      corsexp, rmfexp, arfexp, qproperr, errmeth,
     &                      ierr)

      IMPLICIT NONE
      integer chatter, nk_ucom, ierr
      real divzero
      character*(*) phaversn, errmeth
      character*(*) ucomm(*)
      character*(*) exposexp, areascalexp
      character*(*) backfexp, backsexp
      character*(*) corfexp, corsexp
      character*(*) rmfexp, arfexp
      character*(*) outfil
      character*(*) expr
      character*(*) alg_units
      logical killit, qproperr
c 
c Description:
c  Gets the parameters required by MATHPHA from the parameter file
c  NOTE - The par file is assumed to have been opened.
c
c User i/ps required (prompted for):
c  EXPR        - expression to be evaluated
c  OUTFIL      - name of o/p PHA file to be created
c  CHATTER     - chattiness flag for o/p (5 quite,10 normal,15 high,>20 silly)
c  PHAVERSN    - OGIP version of PHA file required
c  DIVZERO     - value to be inserted in cases where there is a divide by zero
c  EXPOSEXP    - the exposure flag/value
c  BACKFEXP    - the BACKFILE flag/filename
c    {incomplete}
c  NK_UCOM     - the number of user comments to be entered (up to 4)
c  UCOMM       - Array of string-defined comment strings
c
c Origin:
c  Original
c
c Called Routines
c  subroutine CRMVBLK           : (CALLIB) Removes blanks from string
c  subroutine FCECHO            : (FTOOLS) writes to standard o/p device
c  subroutine UCLGSn            : (FTOOLS/XPI) gets parameter from par file
c  subroutine WT_FERRMSG        : (CALLIB) Writes standard FITSIO message etc
c
c Compilation:
c  requires XPI/Host interface etc and CALLIB
c
c Authors/Modification History:
c  Ian M George     (1.0.1: 1992 Dec 23), Original
c  Ian M George     (1.0.1: 1993 Dec 07), added divzero
c  Ian M George     (2.0.0: 1994 Apr 21), added units parameter
c  Rehana Yusaf     (2.0.1: 1994 Sept 12), read in clobber from parfile
c  Ian M George     (3.0.0: 1995 Apr 10), added parameters for 'aux files'
c  Ian M George     (4.0.0: 1995 Aug 01), added qproperr & errmeth params
c                                changed '?' to 'H'/'h' for help
c                                auxfiles dont override file-parms
c Banashree Mitra Seifert (4.1.0: 1996 Mar20)
c          . Introduced screen display routines
c ----------------------------------------------------------------
      character(11) subname
      parameter (subname='gp_mathpha')
      character(7) version
      parameter (version = '4.1.0')
*- 
c Internals
      integer i
      character(5) properr
      character(20)  param
      character(80)  auxfiles
      character(80)  message
      logical qauxfiles, qdone

c Initialize
      qdone = .false.
      ierr = 0
      qauxfiles = .true.

c Get the expression
      call uclgst('expr',expr, ierr)
      if(ierr.NE.0) then
         message = 'Getting EXPR parameter'
         call wterrm(subname,version,message)
         return
      elseif(expr.EQ.' ')then
         ierr = 1
         return
      endif

c Get the units of the algebraic expression
 583  call uclgst('units',param, ierr)
      if(ierr.NE.0) then
         message = 'Getting UNITS parameter'
         call wterrm(subname,version,message)
         message = ' Setting UNITS to C (COUNTS)'
         call wtinfo(chatter,0,1,message)
         ierr = 0
         param = 'C'
      elseif(param.EQ.' ')then
         message = 'Setting UNITS to C (COUNTS)'
         call wtinfo(chatter,0,1,message)
         param = 'C'
      endif
      
      call crmvblk(param)
      call ftupch(param)
      alg_units = param(1:1)


      if((alg_units(1:1).EQ.'H').OR.
     &     (alg_units(1:1).EQ.'h')) then
         message = ' ... This parameter indicates the physical '//
     &        'units in which the algebraic'
         call wtinfo(chatter,0,1,message)
         message = '     expression is to be evaluated, and the '//
     &        'units in which the output file will'
         call wtinfo(chatter,0,1,message)
         message = '     be written. The allowed values are C '//
     &        '(ie COUNTS), or R (ie RATE), implying'
         call wtinfo(chatter,0,1,message)
         message = '     that the algebra will be performed '//
     &        'in COUNTS or COUNTS PER SECOND space'
         call wtinfo(chatter,0,1,message)
         message = '     respectively. The algebra will be in '//
     &        'this space irrespective of whether'
         call wtinfo(chatter,0,1,message)
         message = '     the input files contain data stored ' //
     &        'in counts or in counts per second'
         call wtinfo(chatter,0,1,message)
         message = '     (ie, if units=C, input PHA histograms '//
     &        'stored in counts per sec will be'
         call wtinfo(chatter,0,1,message)
         message = '     converted to counts prior to any '//
     &        'mathematical operations being performed).'
         call wtinfo(chatter,0,1,message)
         message = '     Similarly, this flag gives the implied '//
     &        'units of any numerical constants'
         call wtinfo(chatter,0,1,message)
         message = '     within the input expression. A third '//
     &        'option value is also allowed, units=F'
         call wtinfo(chatter,0,1,message)
         message = '     (ie FILE) whereby the algebra is '//
     &        'performed in which ever units most of the'
         call wtinfo(chatter,0,1,message)
         message = '     files are stored in (COUNTS in the '//
     &        'event of a tie).'
         call wtinfo(chatter,0,1,message)
         goto 583
      elseif((alg_units(1:1).NE.'F').AND.(alg_units(1:1).NE.'C').AND.
     &        (alg_units(1:1).NE.'R')) then
         message = 'Unrecognized UNITS parameter'
         call wterrm(subname,version,message)
         message = 'Setting UNITS to COUNT'
         call wtinfo(chatter,0,1,message)
         alg_units = 'C'
      endif

      param = ' '

c Get the name of the o/p file
      call uclgst('outfil',outfil, ierr)
      if(ierr.NE.0) then
         message = 'Getting OUTFIL parameter'
         call wterrm(subname,version,message)
         return
      endif

c Get the exposure FLAG
 584  call uclgst('exposure',exposexp, ierr)
      if((ierr.NE.0).OR.(exposexp.EQ.' ')) then
         message = 'Getting EXPOSURE parameter'
         call wterrm(subname,version,message)
         message = 'setting exposure flag to NULL'
         call wtinfo(chatter,0,1,message)
         exposexp = 'NULL'
         ierr = 0
      endif

      if((exposexp(1:1).EQ.'H').OR.
     &     (exposexp(1:1).EQ.'h')) then
         message = ' ... This parameter controls the value '//
     &        'written as the exposure time in the'
         call wtinfo(chatter,0,1,message)
         message = '     output file. The allowed values are:'
         call wtinfo(chatter,0,1,message)
         message = '    {a numerical value} '
         call wtinfo(chatter,0,1,message)
         message = '        - where the entered value '//
     &        '(assuming it can be parsed as a real), is'
         call wtinfo(chatter,0,1,message)
         message = '        written to the output dataset. '//
     &        'The value will be assumed to be in '
         call wtinfo(chatter,0,1,message)
         message = '        units of seconds. At the current '//
     &        'time, numerals of the form  1E04' 
         call wtinfo(chatter,0,1,message)
         message = '        or 1E-04 are NOT supported.'
         call wtinfo(chatter,0,1,message)
         message = '    {an input filename} '
         call wtinfo(chatter,0,1,message)
         message = '        - whereby the exposure time read '//
     &        'from the specified input file is '
         call wtinfo(chatter,0,1,message)
         message = '        written to the output dataset '
         call wtinfo(chatter,0,1,message)
         message = '    CALC'
         call wtinfo(chatter,0,1,message)
         message = '        - where the exposure time is '//
     &        'calculated from the input expression'
         call wtinfo(chatter,0,1,message)
         message = '        by substituting each filename '//
     &        'with its exposure time, and performing '
         call wtinfo(chatter,0,1,message)
         message = '        the specified calculation.'
         call wtinfo(chatter,0,1,message)
         message = '    NULL '
         call wtinfo(chatter,0,1,message)
         message = '        - where an exposure of 1 second '//
     &        'is written to the output dataset'
         call wtinfo(chatter,0,1,message)
         goto 584
      endif

c Get the areascal FLAG
      call uclgst('areascal',areascalexp, ierr)
      if((ierr.NE.0).OR.(areascalexp.EQ.' ')) then
         message = 'Getting AREASCAL parameter'
         call wterrm(subname,version,message)
         message = 'setting areascal flag to NULL'
         call wtinfo(chatter,0,1,message)
         areascalexp = 'NULL'
         ierr = 0
      elseif((areascalexp.EQ.'NONE').OR.(areascalexp.EQ.'none')
     &   .OR.(areascalexp.EQ.'NULL').OR.(areascalexp.EQ.'null')) then
         areascalexp = 'NULL'
      endif

c Get the auxfiles FLAG
 864  call uclgst('auxfiles',auxfiles, ierr)
      if((ierr.NE.0)) then
         message =  'Getting AUXFILES parameter'
         call wterrm(subname,version,message)
         message = ' any value of AUXFILES will be ignored'
         call wtinfo(chatter,0,1,message)
         backfexp = 'NONE'
         ierr = 0
         qauxfiles = .false.
      elseif((auxfiles.EQ.'NONE').OR.(auxfiles.EQ.'none').OR.
     &        (auxfiles.EQ.'NULL').OR.(backfexp.EQ.'null').OR.
     &        (auxfiles.EQ.' ')) then
         qauxfiles = .false.
      elseif((auxfiles(1:1).EQ.'H')
     &        .OR.(auxfiles(1:1).EQ.'h')) then
         message = ' ... This parameter allows one of the i/p'//
     &        ' files to be specified such that the'
         call wtinfo(chatter,0,1,message)
         message = '     values of the auxiliary file'//
     &        ' information in the o/p file (ie the values'
         call wtinfo(chatter,0,1,message)
         message = '     of the BACKFILE, BACKSCAL, CORRFILE,'//
     &        ' CORRSCAL, ANCRFILE & ANCRFILE'
         call wtinfo(chatter,0,1,message)
         message = '     keywords) are copied from the'//
     &        ' specified i/p file. The default value,'
         call wtinfo(chatter,0,1,message)
         message = '     NONE, will give rise to the above'//
     &        ' parameters having their default/null'
         call wtinfo(chatter,0,1,message)
         message = '     values in the o/p, UNLESS they are '//
     &        ' individually specified by the' 
         call wtinfo(chatter,0,1,message)
         message = '     corresponding hidden parameters'//
     &        ' (backfile, backscal, corrfile, corrscal,'
         call wtinfo(chatter,0,1,message)
         message = '     arfile & rmfile respectively). It should '//
     &        ' be noted that if the auxfiles'
         call wtinfo(chatter,0,1,message)
         message = '     parameter is set, then it will'//
     &        ' override any settings of the above six'
         call wtinfo(chatter,0,1,message)
         message = '     parameters. See memo'//
     &        ' OGIP/95-008 for further details.'
         call wtinfo(chatter,0,1,message)
         goto 864
      else
         qauxfiles = .true.
         backfexp = auxfiles
         backsexp = auxfiles
         corfexp  = auxfiles
         corsexp  = auxfiles
         arfexp   = auxfiles
         rmfexp   = auxfiles
      endif
      
c      if(.NOT.qauxfiles) then
c Get the backfile FLAG
      call uclgst('backfile',backfexp, ierr)
      if((ierr.NE.0).OR.(backfexp.EQ.' ')) then
         message = 'Getting BACKFILE parameter'
         call wterrm(subname,version,message)
         message = 'setting backfile flag to NONE'
         call wtinfo(chatter,0,1,message)
         backfexp = 'NONE'
         ierr = 0
      elseif((backfexp.EQ.'none').OR.(backfexp.EQ.'NULL').OR.
     &        (backfexp.EQ.'null')) then
         backfexp = 'NONE'
      endif
c      endif

c      if(.NOT.qauxfiles) then
c Get the backscal FLAG
      call uclgst('backscal',backsexp, ierr)
      if((ierr.NE.0).OR.(backsexp.EQ.' ')) then
         message = 'Getting BACKSCAL parameter'
         call wterrm(subname,version,message)
         message = 'setting backscal flag to NONE'
         call wtinfo(chatter,0,1,message)
         backsexp = 'NONE'
         ierr = 0
      elseif((backsexp.EQ.'none').OR.(backsexp.EQ.'NULL').OR.
     &        (backsexp.EQ.'null')) then
         backsexp = 'NONE'
      endif
c      endif

c      if(.NOT.qauxfiles) then
c Get the corfile FLAG
      call uclgst('corrfile',corfexp, ierr)
      if((ierr.NE.0).OR.(corfexp.EQ.' ')) then
         message =  'Getting CORRFILE parameter'
         call wterrm(subname,version,message)
         message = 'setting corrfile flag to NONE'
         call wtinfo(chatter,0,1,message)
         corfexp = 'NONE'
         ierr = 0
      elseif((corfexp.EQ.'none').OR.(corfexp.EQ.'NULL').OR.
     &        (corfexp.EQ.'null')) then
         corfexp = 'NONE'
      endif
c     endif

c      if(.NOT.qauxfiles) then
c Get the corscal FLAG
      call uclgst('corrscal',corsexp, ierr)
      if((ierr.NE.0).OR.(corsexp.EQ.' ')) then
         message = 'Getting CORRSCAL parameter'
         call wterrm(subname,version,message)
         message = ' setting corrscal flag to NONE'
         call wtinfo(chatter,0,1,message)
         corsexp = 'NONE'
         ierr = 0
      elseif((corsexp.EQ.'none').OR.(corsexp.EQ.'NULL').OR.
     &        (corsexp.EQ.'null')) then
         corsexp = 'NONE'
      endif
c      endif


c      if(.NOT.qauxfiles) then
c Get the arf file FLAG
      call uclgst('arfile',arfexp, ierr)
      if((ierr.NE.0).OR.(arfexp.EQ.' ')) then
         message = 'Getting ARFILE parameter'
         call wterrm(subname,version,message)
         message = 'setting ARF file flag to NONE'
         call wtinfo(chatter,0,1,message)
         arfexp = 'NONE'
         ierr = 0
      elseif((arfexp.EQ.'none').OR.(arfexp.EQ.'NULL').OR.
     &        (arfexp.EQ.'null')) then
         arfexp = 'NONE'
      endif
c      endif

c      if(.NOT.qauxfiles) then
c Get the rmf file FLAG
      call uclgst('rmfile',rmfexp, ierr)
      if((ierr.NE.0).OR.(rmfexp.EQ.' ')) then
         message = 'Getting RMFILE parameter'
         call wterrm(subname,version,message)
         message = 'setting RMF file flag to NONE'
         call wtinfo(chatter,0,1,message)
         rmfexp = 'NONE'
         ierr = 0
      elseif((rmfexp.EQ.'none').OR.(rmfexp.EQ.'NULL').OR.
     &        (rmfexp.EQ.'null')) then
         rmfexp = 'NONE'
      endif
c      endif

c Get the number of user-defined comments
      call uclgsi('ncomments',nk_ucom, ierr)
      if(ierr.NE.0) then
         message = 'Getting NCOMMENTS parameter'
         call wterrm(subname,version,message)
         ierr = 0
         nk_ucom = -99
      endif
      
      if(nk_ucom.GT.4) then
         message = 'Illegal number of comments requested'
         call wterrm(subname,version,message)
         message = 'Maximum of 4 allowed'
         call wtinfo(chatter,0,1,message) 
         nk_ucom = 4
      elseif(nk_ucom.LE.0) then
         nk_ucom = 1
         ucomm(1) = '         {none}  '
         goto 123
      endif

c Get the user-defined comment string
      do i = 1, nk_ucom
         write(param,'(a7,i12)') 'comment',i
         call crmvblk(param)      
         call uclgst(param,ucomm(i), ierr)
         if(ierr.NE.0) then
            message = 'Getting '//param(:8)//' parameter'
            call wterrm(subname,version,message)
            message = 'continuing regardless'
            call wtinfo(chatter,0,1,message)
            nk_ucom = i
            ucomm(nk_ucom) = ' {unable to be read}'            
            ierr = 0
            goto 123
         endif
      enddo

 123  continue
c Get the chattiness flag
      call uclgsi('chatter',chatter, ierr)
      if(ierr.NE.0) then
         ierr = 0 
         message = 'Getting CHATTER parameter'
         call wterrm(subname,version,message)
         message = 'Setting CHATTER = 10'
         call wterrm(subname,version,message)
         chatter = 10
         ierr = 0
      endif      

c Get the OGIP version number of the PHA file format to be created
      call uclgst('phaversn',phaversn, ierr)
      if(ierr.NE.0) then
         message = 'Getting PHAVERSN parameter'
         call wterrm(subname,version,message)
         message = 'setting PHAVERSN = 1.2.0'
         call wtinfo(chatter,0,1,message)
         phaversn='1.2.0'
         ierr = 0
      endif
      if(phaversn.EQ.'1992a') then
         phaversn='1.2.0'
      endif

c Get the value to be inserted in the o/p whether there is a divide by 
c  zero in the maths expression.
      call uclgsr('divzero',divzero, ierr)
      if(ierr.NE.0) then
         message = 'Getting DIVZERO parameter'
         call wterrm(subname,version,message)
         message = 'setting value to -99'
         call wtinfo(chatter,0,1,message)
         divzero = -99
         ierr = 0
      endif      

c Read in clobber

      call uclgsb('clobber',killit,ierr)
      if (ierr.NE.0) then
         killit = .false.
         ierr = 0
         message = 'reading CLOBBER'
         call wterrm(subname,version,message)
         message = ' default is false'
         call wtinfo(chatter,0,1,message)
      endif 

c Get the error-propagation flag
 684  call uclgst('properr',properr, ierr)
      if((ierr.NE.0)) then
         message = 'Getting PROPERR parameter'
         call wterrm(subname,version,message)
         message = 'setting PROPERR to TRUE'
         call wtinfo(chatter,0,1,message)
         qproperr = .true.
         ierr = 0
      elseif((properr(1:1).EQ.'N').OR.
     &        (properr(1:1).EQ.'n').OR.
     &        (properr(1:1).EQ.'F').OR.
     &        (properr(1:1).EQ.'f')) then
         qproperr = .false.
      elseif((properr(1:1).EQ.'Y').OR.
     &        (properr(1:1).EQ.'y').OR.
     &        (properr(1:1).EQ.'T').OR.
     &        (properr(1:1).EQ.'t')) then
         qproperr = .true.
      elseif((properr(1:1).EQ.'H').OR.
     &        (properr(1:1).EQ.'h')) then
         message = ' ... This parameter controls whether the'//
     &        ' errors on each i/p PHA dataset are'
         call wtinfo(chatter,0,1,message)
         message = '     propagated during the calculation, or'//
     &        ' simply calculated from the values in'
         call wtinfo(chatter,0,1,message)
         message = '     the resultant PHA dataset. The latter'//
     &        ' is probably ONLY valid for the'
         call wtinfo(chatter,0,1,message)
         message = '     addition of non-background-subtracted'//
     &        ' datasets when no rescaling is'
         call wtinfo(chatter,0,1,message)
         message = '     required. See memo OGIP/95-008 for'//
     &        ' further details. If in doubt'
         call wtinfo(chatter,0,1,message)
         message = '     leave this parameter set to TRUE.'
         call wtinfo(chatter,0,1,message)
         goto 684
      endif


c Get the method whereby errors are calculated
      qdone = .false.
 683  call uclgst('errmeth',errmeth, ierr)
      if((ierr.NE.0)) then
         message = 'Getting ERRMETH parameter'
         call wterrm(subname,version,message)
         message = 'setting ERRMETH to POISS-1'
         call wtinfo(chatter,0,1,message)
         errmeth = 'POISS-1'
         ierr = 0
      endif
      call crmvblk(errmeth)
      call ftupch(errmeth)
      
      if((errmeth(1:1).EQ.'H').AND.(.NOT.qdone)) then
         message = 'This parameter the prescription used to'//
     &        ' calculate Poissionian errors'
         call wtinfo(chatter,0,1,message)
         message = '     (should this be necessary). The following'//
     &        'values are currently allowed:'
         call wtinfo(chatter,0,1,message)
         message = '       ERRMETH = Gauss   whereby the errors'//
     &        ' are calculated using SQRT(N).'
         call wtinfo(chatter,0,1,message)
         message = '       ERRMETH = POISS-1 whereby the'//
     &        ' algorithm of Gehrel (1986 ApJ, 303, 336)'
         call wtinfo(chatter,0,1,message)
         message = '                         eqn 7 (+ve error)'//
     &        ' is used.'
         call wtinfo(chatter,0,1,message)
         message = '       ERRMETH = POISS-2 whereby the'//
     &        ' algorithm of Gehrel (1986 ApJ, 303, 336)'
         call wtinfo(chatter,0,1,message)
         message = '                         eqn 11 (-ve error)'//
     &        ' is used.'
         call wtinfo(chatter,0,1,message)
         message = '       ERRMETH = POISS-3 whereby 0.5 '//
     &        ' * (POISS-1 + POISS-2) is used'
         call wtinfo(chatter,0,1,message)
         message = '     Caution is urged, particularly when using'//
     &        ' ERRMETH = Gauss, as unexpected'
         call wtinfo(chatter,0,1,message)
         message = '     and/or misleading results can be produced.'
     &        //' See OGIP/95-008 for details.'
         call wtinfo(chatter,0,1,message)
         qdone = .true.
         goto 683
      elseif(errmeth(1:1).EQ.'G') then
         errmeth = 'Gauss'
      elseif((errmeth(1:7).EQ.'POISS-0').OR.
     &        (errmeth(1:2).EQ.'P0')) then
         errmeth = 'POISS-0'
      elseif((errmeth(1:7).EQ.'POISS-1').OR.
     &        (errmeth(1:2).EQ.'P1')) then
         errmeth = 'POISS-1'
      elseif((errmeth(1:7).EQ.'POISS-2').OR.
     &        (errmeth(1:2).EQ.'P2')) then
         errmeth = 'POISS-2'
      elseif((errmeth(1:7).EQ.'POISS-3').OR.
     &        (errmeth(1:2).EQ.'P3'))then
         errmeth = 'POISS-3'
      else
         if(qdone) then
            message = 'setting ERRMETH to POISS-1'
            call wtinfo(chatter,0,1,message)
            errmeth = 'POISS-1'
            goto 876
         endif
         ierr = 1 
         message =  'Unsupported Error prescription'
         call wterrm(subname,version,message)
         return
      endif

 876  qdone = .false.
c Give user info if requested
      message = 'using GP_MATHPHA ' // version
      call wtinfo(chatter,20,2,message)



      return
      end
c ------------------------------------------------------------------------
c ------------------------------------------------------------------------
*+DO_MATHPHA
      subroutine do_mathpha(expr, alg_units_in,
     &                      outfil, chatter, phaversn, 
     &                      divzero, exposexp, areascalexp, nk_ucom, 
     &                      ucomm, killit, backfexp, backsexp, corfexp,
     &                      corsexp, rmfexp, arfexp, qproperr, errmeth,
     &                      ierr)

      IMPLICIT NONE
      integer chatter, ierr, nk_ucom
      real divzero
      character*(*) phaversn
      character*(*) ucomm(nk_ucom)
      character*(*) exposexp, areascalexp
      character*(*) backfexp, backsexp
      character*(*) corfexp, corsexp
      character*(*) rmfexp, arfexp
      character*(*) outfil
      character*(*) expr
      character*(*) alg_units_in
      character*(*) errmeth
      logical killit, qproperr
c 
c Description:
c  Program to perfom mathematical operations on PHA datasets 
c
c User i/ps required (prompted for):
c  None
c
c Passed parameters
c  EXPR          i   : expression to be evaluated
c  ALG_UNITS_IN  i   : Units in which algebra to be performed
c  OUTFIL        i   : name of o/p PHA file to be created
c  CHATTER       i   : chattiness flag for o/p (<9 quite,>9 inc silly)
c  PHAVERSN      i   : OGIP version of PHA file required
c  DIVZERO       i   : value inserted in o/p whenever divide by zero
c  EXPOSEXP       i   : String controling handling of exposure time
c  AREASCALEXP   i   : String controling handling of areascaling factor
c  NK_UCOM       i   : No. of user-defined comments to be added
c  UCOMM         i   : Arrary of user-defined comments
c  KILLIT        i   : Flag indicating whether automatic over-write allowed
c  BACKFEXP      i   : String controling handling of background file
c  BACKSEXP      i   : String controling handling of background scaling 
c  CORFEXP       i   : String controling handling of correction file
c  CORSEXP       i   : String controling handling of correction scaling 
c  RMFEXP        i   : String controling handling of RMF
c  ARFEXP        i   : String controling handling of ARF
c  QPROPERR      i   : Flag as to whether errors are to be propagated
c  ERRMETH       i   : (Poissionian) Error method to be used
c  IERR            o : Error Flag (zero if all OK)
c   
c Called Routines:
c  subroutine CHK_CONT       : (below) Checks string could be a control string
c  subroutine CHK_OPER       : (below) Checks string could be maths operator
c  subroutine CHK_REAL       : (below) Checks string could be a real
c  subroutine CK_FILE    : (CALLIB) Checks if file exists, deletes if reqd
c  subroutine CGETLUN    : (CALLIB) Gets a free logical unit
c  function CLENACT      : (CALLIB) Returns length of string
c  subroutine CRMVBLK       : (CALLIB) Removes blanks from string
c  subroutine FCECHO     : (FTOOLS) Writes to standard o/p
c  subroutine FTCLOS       : (FITSIO) Closes a FITS file
c  subroutine FTPKYx     : (FITSIO) writes a keyword of type "x"
c  subroutine FTVERS     : (FITSIO) Returns version of FITSIO
c  subroutine OP_NPA     : (CALLIB) Opens & writes a null P.Header 
c  subroutine PARSEXPR   : (below) Parses string to RP
c  subroutine POPULATE        : (below) Opens, reads, preprocesses PHA file
c  subroutine WTPHA2     : (CALLIB) writes a PHA dataset extension
c
c Origin:
c  Original
c
c Authors/Modification History:
c  Ian M George     (1.0.0:1993 Nov 22), original
c  Ian M George     (1.0.1:1993 Dec 08), major rewrite
c  Ian M George     (2.0.0:1994 Mar 09), added Dynamic Memory Allocation
c  Ian M George     (3.0.0:1994 Apr 21), added units parameter
c  Rehana Yusaf     (3.0.1:1994 Sep 12), added killit as an argument,
c                                          for ck_file
c  Ian M George     (4.0.0:1995 Apr 10), passed parameters for 'aux files'
c  Ian M George     (4.0.1:1995 Apr 18), passed areascalexp parameter
c  Ian M George     (4.1.0:1995 Aug 01), added properr & errmeth params
c
c Banashree Mitra Seifert (4.2.0: 1996 Mar 20)
c             . Introduced DMA to qpos,qdiv,qprob
c             . Introduced screen display
c             . removed common block for taskname
c Banashree Mitra Seifert (5.0.0: 1997 Mar 27)
c             . backscal, texpos, areascal, and corscal is given on command line
c               then it writes the output file keywords updated with these 
c               values.
c               (Ken Ebisawa wanted this way)
c Banashree Mitra Seifert (5.1.0: 1997 Aug 8)
c             . calculation for exposure was misplaced. line#1016 needs to be
c               inside if loop
c  Peter D Wilson (5.1.1:1998 Apr 3)
c             . Initialize filename strings to ' ', without which
c               the strings contained all zeros which is not considered
c               a blank string in str.EQ.' ' comparisons.
c toliver (5.1.2:1998 Aug 6)
c             . Initialize nk_comm to 0
c Peter D Wilson (5.1.3:1998 Sep 4)
c             . Initialize status to 0
c -----------------------------------------------------------------
      character(11) subname
      parameter (subname='do_mathpha')
      character(7) version
      parameter (version = '5.1.3')
*- 
c Commons
      character(8) taskname
      parameter(taskname='mathpha')

C **** DYNAMIC MEMORY ALLOCATION ****
C  the following MEM common block definition is in the system iraf77.inc file
      LOGICAL          MEMB(100)
      INTEGER*2        MEMS(100)
      INTEGER*4        MEMI(100)
      INTEGER*4        MEML(100)
      REAL             MEMR(100)
      DOUBLE PRECISION MEMD(100)
      COMPLEX          MEMX(100)
      EQUIVALENCE (MEMB, MEMS, MEMI, MEML, MEMR, MEMD, MEMX)
      COMMON /MEM/ MEMD
c ************************************
c Max Array sizes
      integer maxcom, maxfil, maxstack
      integer maxhist, maxcomm
      parameter (maxcom = 16384, maxfil=1000)
ccc   parameter (maxchan = 4096, maxstack = 10)
      parameter (maxstack = 500)
      parameter (maxhist = 500, maxcomm = 256)
c Internals
c ... parameters & static arrays
      integer iunit, i, j1, j2, istart, ncom, ounit
      integer clenact, nfil, fcstln
      integer nk_hist, nk_comm
      real ftsver
      real buf_exp(2), save_exp, expos(maxfil)
      character(1) ctype(maxcom), alg_units
      character(80) message
      character(16384) long_message
      character(80) asciifil
      character(80) file(maxfil), dmyfile(maxfil)      
      character(16384) dmyexpr
      character(32768) expr_in, expr_out
      character(80) comm(maxcom)
      character(80) repeatmsg
      character(70) hist(maxhist), comment(maxcomm)
ccc      logical qans, qdiv(maxchan), qpos(maxchan)
      logical qans
      integer p_qdiv, p_qpos
      logical qok,qcalexp, qfexp, qokexp
c ...... PHA stuff
      integer detchan, fchan
      integer filtyp(maxfil)
      character(20) tlscop, instrum, filter, detnam
ccc      logical qprob(maxchan)
      integer p_qprob
c ...... O/p stuff
      integer status
      integer dtype
      real texpos, areascal,corscal, backscal
      character(16) chantyp
      character(20) hduclas2
      character(70) backfil,corfil,rmffil, arffil
      logical qerror, qosys,qqual,qgroup,poiss_err

c ... pointers to "arrays" to be dynamically allocated
      integer p_chan, p_ipha, p_group, p_qual
      integer p_qual_in, p_grping, p_channel, p_buf_dat
      integer p_buf_err, p_save_dat, p_save_err, p_stack_dat
      integer p_stack_err, p_stack_exp, p_data, p_err
      integer p_pha, p_error, p_sysfrc, p_syserr
      integer p_work, p_iwork, p_iwork2
      integer imaxchan,col_num
c ... "arrays" to be dynamically allocated
c      integer chan(maxchan)              integer ipha(maxchan)
c      integer group(maxchan)              integer qual(maxchan)
c      integer qual_in(maxchan)        integer grping(maxchan)
c      integer channel(maxchan)
c      real buf_dat(maxchan,2)              real buf_err(maxchan,2)
c      real save_dat(maxchan)              real save_err(maxchan)
c      real stack_dat(maxchan,maxstack)  real stack_err(maxchan,maxstack)
c      real stack_exp(maxstack)        
c      real data(maxchan,maxfil)        real err(maxchan,maxfil)
c      real pha(maxchan)              real error(maxchan)
c      real sysfrc(maxchan)                 real syserr(maxchan)
c
c dummy variable
      real rdummy

c Initialize
      nk_comm = 0
      ierr = 0
      nfil=0
      status = 0
      qans = .false.
      qfexp = .false.
      qcalexp = .false.
      qokexp = .true.
      asciifil = ' '
      backfil  = ' '
      corfil   = ' '
      arffil   = ' '
      rmffil   = ' '
      repeatmsg = ' ... Try repeating the calculation in stages'
      col_num = 3
      poiss_err = .false.

c ** START MAIN **
c Give user info if requested
      message = 'using DO_MATHPHA '// version
      call wtinfo(chatter,20,1,message)
      call ftvers(ftsver)
      write(message,'(a,f6.3)') 
     &     'using FITSIO Version ', ftsver
      call wtinfo(chatter,20,1,message)
      message = 'running in full diagnostic mode'
      call wtinfo(chatter,30,1,message)

c ... Remove spaces from the expression
      call crmvblk(expr)

c ... Trap out the expression-from-a-file case
      if(expr(1:1).EQ.'@') then
         asciifil = expr(2:clenact(expr))
         message = 'reading expression from file:'//
     &        asciifil(:MIN(clenact(asciifil),40))
         call wtinfo(chatter,20,1,message)
         call cgetlun(iunit)
         open(iunit,file=asciifil,status='old')
         expr = ' '
         do i = 1, 99999
            read(iunit,'(a)',end=123) dmyexpr
            expr = expr(:clenact(expr))//dmyexpr(:clenact(dmyexpr))
         enddo         
 123     call crmvblk(expr)
         close(iunit)
      endif

c ... Store the compressed/read-in original expression
      expr_in = expr

c Fill in a few history & comment records
      hist(1) = ' Dataset had mathematical operations performed by'
     &     // subname //version
      nk_hist = 1
      comment(1) = subname //version//'Summary:'
      comment(2) = ' ... I/p parameters'
      if(asciifil.EQ.'      ') then      
         comment(3) = '  Expression (read from terminal):'
      else
         comment(3) = '  Expression (read from file '//
     &        asciifil(:MIN(50,clenact(asciifil)))//'):'
      endif
      nk_comm = nk_comm + 3
      do i = 1, ABS(clenact(expr_in)/60) + 1
         nk_comm = nk_comm + 1
         comment(nk_comm) = '   '// expr_in((i-1)*60+1:i*60)
      enddo
      nk_comm = nk_comm + 1
      comment(nk_comm) = '  Units   : '//alg_units_in
      nk_comm = nk_comm + 1
      if(alg_units_in.EQ.'F') then
         comment(nk_comm) = '    (algebra to be performed in '//
     &        'the units read from files)'
      elseif(alg_units_in.EQ.'C') then
         comment(nk_comm) = '    (algebra to be performed in COUNTS) '
      elseif(alg_units_in.EQ.'R') then
         comment(nk_comm) = '    (algebra to be performed in '//
     &        'COUNTS PER SECOND)' 
      endif
      nk_comm = nk_comm + 1
      if(qproperr) then
         comment(nk_comm) = '  Errors to be calculated & propagated'
      else
         comment(nk_comm) = '  Errors NOT propagated, only'//
     &        ' calcd at end'
      endif      
      nk_comm = nk_comm + 1
      comment(nk_comm) = '  Error Prescription: '//errmeth
      nk_comm = nk_comm + 1
      comment(nk_comm) = '  Exposure: '//exposexp(:60)
      nk_comm = nk_comm + 1
      comment(nk_comm) = '  AreaScal: '//areascalexp(:60)
      nk_comm = nk_comm + 1
      write(comment(nk_comm),'(a,f12.6)') '  Divzero : ', divzero
      nk_comm = nk_comm + 1
      comment(nk_comm) = '  Source of Aux Info keyword values:   '
      comment(nk_comm+1) = '  BACKFILE: '//backfexp
      comment(nk_comm+2) = '  BACKSCAL: '//backsexp
      comment(nk_comm+3) = '  CORRFILE: '//corfexp
      comment(nk_comm+4) = '  CORRSCAL: '//corsexp
      comment(nk_comm+5) = '  ANCRFILE: '//arfexp
      comment(nk_comm+6) = '  RESPFILE: '//rmfexp
      nk_comm = nk_comm + 7
      comment(nk_comm) = '  User-defined comments:'
      do i = 1, nk_ucom
         nk_comm = nk_comm + 1
         comment(nk_comm) = '   '// ucomm(i)
      enddo

c Sort out the exposure handling flag
      if((exposexp.EQ.'NULL').OR.(exposexp.EQ.'null')) then
         texpos = 1
      elseif((exposexp.EQ.'CALC').OR.(exposexp.EQ.'calc')) then
         qcalexp = .true.
      else
c           try to get the real value, reverse sense of success/failure
c           to determine how to handle value later on
         call get_real(chatter, exposexp, texpos, qans)
         qfexp = .not. qans
      endif

c *** PARSE the expression to construct pseudo-RP o/p expression ***
      call parsexpr(chatter,expr,expr_out,ierr)
      if(ierr.NE.0) then
         message = ' Unable to parse expression:'
         call wterrm(subname,version,message)
         long_message=expr(:clenact(expr))
         call wtinfo(chatter,0,1,long_message) 
         goto 482
      endif

c *** PARSE the o/p expression to construct the command array
      istart = 1
      ncom = 0
      do i = 1, clenact(expr_out)-4
         if(expr_out(i:i).EQ.',') then
            ncom = ncom + 1
            if(ncom.GT.maxcom) then
               message = ' No. commands exceeds MAXCOM'
               call wterrm(subname,version,message)
               call wtinfo(chatter,0,1,repeatmsg)
               ierr = 1
               goto 482
            endif
            if((i-istart).GT.80) then
               message = ' Command exceeds buffer'
               call wterrm(subname,version,message)
               message = 'Offending command:'//
     &              expr_out(istart:istart+40)//' .. etc ..'
               call wtinfo(chatter,0,1,repeatmsg)
               ierr = 1
               goto 482
            endif
            comm(ncom) = expr_out(istart:i-1)
            istart = i+1
         endif
      enddo


c Run through the command array, assigning command types (ctype)
c ... Run through the command array assigning command "types"
      do i = 1, ncom
         qans = .false.
c     ... check if its an operator
         call chk_oper(chatter,comm(i),qans,ierr)
         if(qans) then
            if(i.EQ.1) then
               message = 'Problems performing calculation'
               call wterrm(subname,version,message)
               ierr = 1
               goto 482
            else
               ctype(i) = 'O'
            endif
         else
c     ... check if its a control-string
            call chk_cont(chatter,comm(i),qans,ierr)
            if(qans) then
               ctype(i) = 'C'
            else
c     ... check if its an integer/real
               call get_real(chatter, comm(i), rdummy, qans)
               if(qans) then
                  ctype(i) = 'R'
               else
c     ... assume it's a filename/extension
                  ctype(i) = 'F'
                  nfil = nfil + 1
                  if(nfil.GT.maxfil) then
                     message = ' No. files exceeds MAXFIL'
                     call wterrm(subname,version,message)
                     call wtinfo(chatter,0,1,repeatmsg)
                     ierr = 1
                     goto 482
                  endif
C     Copy file over, but strip off quote characters
                  file(nfil) = ' '
                  j2 = 1
                  do j1=1,fcstln(comm(i))
                     if( comm(i)(j1:j1).ne.'''' ) then
                        file(nfil)(j2:j2) = comm(i)(j1:j1)
                        j2 = j2 + 1
                     endif
                  enddo
                  dmyfile(nfil) = file(nfil)
               endif
            endif
         endif
      enddo
c ... Check that we have some things we think might be PHA files
      if(nfil.LE.0) then
         message = ' No filenames found in expression'
         call wterrm(subname,version,message)
         ierr = 1
         goto 482
      endif

c ... Dump the filenames, if requested
      write(message,'(a,i12)') 
     &     ' No. files parsed from expression: ', nfil
      call wtinfo(chatter,20,1,message)  
      do i = 1, nfil
         message =  file(i)(:MIN(70,clenact(file(i))))
         call wtinfo(chatter,20,1,message)
      enddo
      
c *** Check for an illegal o/p filename
      call ck_file(outfil,dmyfile, nfil, qok, killit, chatter)
      if(.NOT.qok) then
c                message = '... OUTFIL has same name as an i/p file'
c     &                  //' (which cannot be overwritten)'
c                call fcecho(message)
         ierr = -1
         goto 482
      endif

c *****
c Go and finger the first pha file to suss out the size of the matrix
      message = ' Allocating Dynamic Memory using file:'
      call wtinfo(chatter,15,1,message)
      message = file(1)
      call wtinfo(chatter,15,1,message)
      
      call chkpha_math(chatter,file(1),imaxchan,ierr)
      if(ierr.NE.0) goto 482
ccc      if(imaxchan.GT.maxchan) then
ccc        message = ' Max number channels exceeded'
ccc        call wterrm(subname,version,message)
ccc        ierr = 1
ccc        goto 482
ccc      elseif(imaxchan.LE.0) then
ccc        message = ' Zero or -ve number channels'
ccc        call wterrm(subname,version,message)
ccc        ierr = 1
ccc        goto 482
ccc      endif

c Allocate dynamic memory

      p_chan = 0
      p_ipha = 0
      p_group = 0
      p_qual = 0
      p_qual_in = 0
      p_grping = 0
      p_channel = 0
      p_buf_dat = 0
      p_buf_err = 0
      p_save_dat = 0
      p_save_err = 0
      p_stack_dat = 0
      p_stack_err = 0
      p_stack_exp = 0
      p_data = 0
      p_pha = 0 
      p_error = 0
      p_sysfrc = 0
      p_syserr = 0
      p_work = 0
      p_iwork = 0
      p_iwork2 = 0
      p_qprob = 0
      p_qdiv = 0
      p_qpos = 0

      call udmget(imaxchan, 4, p_chan, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 4, p_ipha, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 4, p_group, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 4, p_qual, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 4, p_qual_in, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 4, p_grping, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 4, p_channel, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan*2, 6, p_buf_dat, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan*2, 6, p_buf_err, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 6, p_save_dat, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 6, p_save_err, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan*maxstack, 6, p_stack_dat, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan*maxstack, 6, p_stack_err, status)
      if(status.NE.0) goto 765
      call udmget(maxstack, 6, p_stack_exp, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan*maxfil, 6, p_data, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan*maxfil, 6, p_err, status)      
      if(status.NE.0) goto 765
      call udmget(imaxchan, 6, p_pha, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 6, p_error, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 6, p_sysfrc, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 6, p_syserr, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 6, p_work, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 4, p_iwork, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan, 4, p_iwork2, status)
      if(status.NE.0) goto 765
      call udmget(imaxchan,1,p_qprob,status)
      if(status.NE.0) goto 765
      call udmget(imaxchan,1,p_qdiv,status)
      if(status.NE.0) goto 765
      call udmget(imaxchan,1,p_qpos,status)
      if(status.NE.0) goto 765

 765  if(status.NE.0) then
         message = ' Failed to allocate Dynamic Memory'
         call wterrm(subname,version,message)
         ierr = -1
         goto 482
      endif
c *****

      alg_units = alg_units_in

c *** DATA file i/p
      call populate(chatter,nfil,file,
     &              filtyp,alg_units,
     &              comment, nk_comm, imaxchan, 
     &              detchan, fchan, tlscop, instrum, filter,
     &              detnam, chantyp,texpos, expos,
     &              MEMR(p_data), MEMR(p_err),MEMI(p_qual),
     &              MEMI(p_chan),MEMI(p_ipha), MEMR(p_pha),
     &              MEMR(p_error), MEMR(p_sysfrc), MEMI(p_group), 
     &              MEMI(p_qual_in), MEMB(p_qprob), MEMB(p_qpos), qfexp, 
     &              exposexp, areascalexp, areascal, 
     &              backfexp, backfil, backsexp, backscal, 
     &              corfexp, corfil, corsexp, corscal,
     &              rmfexp, rmffil, arfexp, arffil,
     &              MEMR(p_work), MEMI(p_iwork), MEMI(p_iwork2), 
     &              errmeth, ierr)
      if(ierr.NE.0) then
         goto 482
      endif

      if(alg_units_in.EQ.'F') then
         nk_comm = nk_comm + 1
         if(alg_units.EQ.'C') then
            comment(nk_comm) = ' ... performing algebra in units of '//
     &           'COUNTS '
         elseif(alg_units.EQ.'R') then
            comment(nk_comm) = ' ... performing algebra in units of '//
     &           'COUNTS PER SECOND'
         endif
      endif

      if(alg_units.EQ.'C') then
         message = ' ... performing algebra in units of '//
     &        'COUNTS '
      elseif(alg_units.EQ.'R') then
         message = ' ... performing algebra in units of '//
     &        'COUNTS PER SECOND'
      endif
      call wtinfo(chatter,5,1,message)


c The MAIN COMMAND do loop 
      call mainloop(chatter, ncom, comm, ctype,
     &              imaxchan, detchan, 
     &              MEMR(p_buf_dat), MEMR(p_buf_err), buf_exp,
     &              MEMR(p_data), MEMR(p_err), expos,
     &              MEMR(p_stack_dat), MEMR(p_stack_err),
     &              MEMR(p_stack_exp), MEMR(p_save_dat), 
     &              MEMR(p_save_err), save_exp,
     &              MEMB(p_qdiv), qokexp, ierr)
      if(ierr.NE.0) then
         goto 482
      endif

c Fill the o/p arrays 
      call filop(chatter, imaxchan, detchan, alg_units, 
     &           MEMB(p_qdiv), divzero,
     &           MEMR(p_pha), MEMR(p_error), MEMI(p_qual), 
     &           MEMR(p_buf_dat), MEMR(p_buf_err), buf_exp,
     &           MEMI(p_grping), MEMR(p_syserr), MEMI(p_channel), 
     &           fchan, qcalexp, qokexp, texpos, qok, MEMB(p_qpos),
     &           errmeth,ierr)

      if(ierr.NE.0) then
         goto 482
      endif

c ... Last minute warnings
      if(texpos.LE.0) then
         message =  ' Exposure time <= 0 seconds'
         call wtwarm(subname,version,chatter,0,message)
      endif      

   
      ierr = 0
c Open & Write the FITS file
c ------------------------ PRIMARY HEADER ---------------------------
      if(phaversn(1:1).EQ.'1')then
c             ... Open the FITS file and write a null primary header
         call opnpa(outfil, chatter, ounit, killit,ierr)
         if(ierr.ne.0) goto 482

c             ... Add additional keywords to Primary Header
         call FTPKYS(ounit,'CREATOR', taskname,
     &               's/w task which wrote this dataset', status)

         call FTPKYS(ounit,'CONTENT', 'PHA SPECTRUM',
     &             'SPECTRUM xtens (at least) present', status)
      else
         message =  'Unknown format: '// phaversn
         call wterrm(subname,version,message)
         ierr = 1
         goto 482
      endif
c ------------------------ finished PRIMARY ---------------------------

c Set defaults
      hduclas2 = 'DERIVED'      
      qosys = .false.
      qqual = .true.
      qgroup = .false.

c Prep PHA odds and sods
      call prep_pha(chatter, alg_units, qproperr, errmeth,
     &              detchan, texpos, MEMR(p_pha), MEMR(p_error),
     &              MEMI(p_qual), dtype, qerror, ierr)
      if(ierr.NE.0) goto 482

c ------------------------ SPECTRUM EXTENSION ----------------------------

      if(phaversn(1:1).EQ.'1') then
         call wtpha2(ounit,chatter,nk_hist,hist,nk_comm,
     &               comment,tlscop,instrum,detnam,filter,
     &               phaversn,hduclas2,fchan,texpos,.FALSE.,
     &               areascal,backfil,.FALSE.,backscal,corfil,
     &               corscal,rmffil,arffil,detchan,chantyp,
     &               MEMI(p_channel),MEMR(p_pha),dtype,
     &               qerror,MEMR(p_error),qosys,
     &               MEMR(p_syserr),qqual,MEMI(p_qual),qgroup,
     &               MEMI(p_grping),detchan,ierr)

         if(ierr.NE.0) goto 482
c             ... Add additional keywords to Primary Header
         call FTPKYS(ounit,'CREATOR', taskname,
     &             's/w task which wrote this dataset', status)
      else
         message =  'Unknown format: '// phaversn
         call wterrm(subname,version,message)
         ierr = 1
         goto 482
      endif

 
      if((errmeth(1:7).EQ.'POISS-0').OR.
     &     (errmeth(1:2).EQ.'P0')) then
         call FTGCNO(ounit,0,'STAT_ERR',col_num,status)
         call FTDCOL(ounit,col_num,status)
         if(status.ne.0) then
            message =  'Problem with deleting STAT_ERR column'
            call wterrm(subname,version,message)
            goto 482
         endif
         poiss_err = .true.
         message = 'Poissonian errors are applicable'
         call FTUKYL(ounit,'POISSERR',poiss_err,message,status)
         if(status.ne.0) then
            message =  'Problem with writing POISSERR keyword'
            call wterrm(subname,version,message)
            goto 482
         endif
      endif

c --------------------- finished SPECTRUM EXTENSION -----------------------


c Close the FITS file
      call ftclos(ounit, status) 
      if(status.ne.0) then
         message =  'Closing o/p file'
         call wterrm(subname,version,message)
         ierr = 6
         goto 482
      endif

c Final check for errors/messaage etc
 482  if(ierr.ne.0) then
         message = ' Incomplete Execution'
         call wterrm(subname,version,message)
      endif

c *****
c Free the dynamic Memory
      call udmfre(p_chan, 4, status)
      if(status.NE.0) goto 766
      call udmfre(p_ipha, 4, status)
      if(status.NE.0) goto 766
      call udmfre(p_group, 4, status)
      if(status.NE.0) goto 766
      call udmfre(p_qual, 4, status)
      if(status.NE.0) goto 766
      call udmfre(p_qual_in, 4, status)
      if(status.NE.0) goto 766
      call udmfre(p_grping, 4, status)
      if(status.NE.0) goto 766
      call udmfre(p_channel, 4, status)
      if(status.NE.0) goto 766
      call udmfre(p_buf_dat, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_buf_err, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_save_dat, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_save_err, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_stack_dat, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_stack_err, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_stack_exp, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_data, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_err, 6, status)      
      if(status.NE.0) goto 766
      call udmfre(p_pha, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_error, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_sysfrc, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_syserr, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_work, 6, status)
      if(status.NE.0) goto 766
      call udmfre(p_iwork, 4, status)
      if(status.NE.0) goto 766
      call udmfre(p_iwork2, 4, status)
 766  if(status.NE.0) then
         message = ' Failed to deallocate Dynamic Memory'
         call wterrm(subname,version,message)
         ierr = 99
      endif

c *****

      return
      end

c -------------------------------------------------------------------------
c -------------------------------------------------------------------------
*+CHK_OPER
      subroutine chk_oper(chatter,string,qans,ierr)

      IMPLICIT NONE
      integer ierr, chatter
      character*(*) string
      logical qans

c Description
c  Routine checks the i/p string and returns true/false answer as to 
c whether the string *could* be a maths operation.
c !!! NOTE !!! Currently the subroutine is rather ridiculous, given only 
c              the most basic maths operations are allowed, however it 
c              is intended that future enhancements will allow 
c              operations such as SQRT, COS, SIN etc
c
c Passed Parameters
c  CHATTER          i   : Chattiness flag - o/p only if >40
c  STRING           i   : i/p character string to be searched
c  QANS               o : O/p flag (.true. = yes, could be an operator)
c  IERR               o : Error flag (zero if everything OK)
c 
c Called Routines
c  Function CLENACT     : (CALLIB) Finds length of a string
c  subroutine FCECHO    : (FTOOLS) writes to standard o/p
c
c Author/Modification History
c  Ian M George (1.0.0:93 Dec 07) Original
c
c Banashree Mitra Seifert(1.1.0: Mar20 1996)
c       . Introduced screen display
c 
c -----------------------------------------------------------------------

      character(9) subname
      parameter (subname='chk_oper')
      character(7) version
      parameter (version='1.1.0')
*-
c Internals 
      integer iend, clenact
      character(80) message

c Initialize
      qans = .false.
      ierr = 0

c ... give user info if requested
      message = 'using '//subname//'Ver '// version
      call wtinfo(chatter,10,2,message)

c ... find length of string & set defaults
      iend = clenact(string)

c ... do the checks
      if(iend.NE.1) then
         qans = .false.
         return
      endif

      if(string(1:iend) .EQ. '*' .OR.
     &     string(1:iend) .EQ. '/' .OR.
     &     string(1:iend) .EQ. '+' .OR.
     &     string(1:iend) .EQ. '-') then
         qans = .true.
         return      
      endif

      return
      end
c -------------------------------------------------------------------------
*+CHK_CONT
      subroutine chk_cont(chatter,string,qans,ierr)

      IMPLICIT NONE
      integer ierr, chatter
      character*(*) string
      logical qans

c Description
c  Routine checks the i/p string and returns true/false answer as to 
c whether the string is a "control" string in the pseudo-RP notation 
c used by MATHPHA.
c
c Passed Parameters
c  CHATTER          i   : Chattiness flag - o/p only if >40
c  STRING           i   : i/p character string to be searched
c  QANS               o : O/p flag (.true. = yes, could be an operator)
c  IERR               o : Error flag (zero if everything OK)
c 
c Called Routines
c  Function CLENACT     : (CALLIB) Finds length of a string
c  subroutine FCECHO    : (FTOOLS) writes to standard o/p
c
c Author/Modification History
c  Ian M George (1.0.0:93 Dec 07) Original
c
c Banashree Mitra Seifert (1.1.0:1996 Mar20)
c            . Introduced screen display routines
c --------------------------------------------------------------------
      character(9) subname
      parameter (subname='chk_cont')
      character(7) version
      parameter (version='1.1.0')
*-
c Internals 
      integer iend, clenact
      character(80) message

c Initialize
      qans = .false.
      ierr = 0

c ... give user info if requested
      message = 'using'//subname//'Ver '// version
      call wtinfo(chatter,10,2,message)

c ... find length of string & set defaults
      iend = clenact(string)

c ... do the checks
      if(string(1:1).EQ.'<' .AND. string(iend:iend).EQ.'>') then
         qans = .true.
         return
      elseif(string(1:iend) .EQ. 'TEMP' .OR.
     &        string(1:iend) .EQ. 'PUSH' .OR.
     &        string(1:iend) .EQ. 'RECALL' .OR.
     &        string(1:iend) .EQ. 'SAVE') then
         qans = .true.
         return      
      endif

      return
      end
c -------------------------------------------------------------------------
*+ PARSEXPR
      subroutine parsexpr(chatter,expr,expr_out, ierr)

      IMPLICIT NONE
      integer chatter, ierr
ccc      character(1024) expr, expr_out
      character*(*) expr, expr_out

c Description
c  Takes the input string (assumed to contain a maths expression to later 
c be evaluated) and converts it to an o/p string in pseudo Reverse Polish 
c notation, consisting of the series of maths commands (in the 'correct' 
c order to be later executed.
c  This is achived in several steps:
c First, the i/p string containing the maths expression is divided 
c     into "stacks" based upon any pairs of () brackets present. The purpose
c     being to cope with expressions which contain embedded brackets 
c     (and embedded embedded brackets) indicating chunks of maths which must 
c     be evaluated first. The object is thus to find such bracket pairs, and 
c     work out from the 'inside out' to get the order in which such 
c     embedded maths must be evaluated. This is achieved by making multiple 
c     passes through the 'running' expression, looking for bracket pairs, 
c     writing any 'self-contained' expressions (ie without further brackets 
c     inside them) to the list of known stacks, and substituting the stack 
c     number (surrounded by <>) for any such found expressions.
c THEN each of the SAFE() stacks is stepped through, and "sub-stacks" looked 
c     for which are separated by "+" and "-" operations. The contents of these 
c     sub-stacks thus contain any combination of filenames, numerals and "*" 
c     and/or "/" operations.
c     Thus the sub-stacks can be relatively easily parsed to pseudo-Reverse 
c     Polish notation (simply by working from left to right), the 
c     relationship between the sub-stacks can also easily be parsed to
c     RP (again by working from left to right), and thus the pseudo-RP expressn
c     for the whole stack be constructed. Since the Bracket Parser (above) 
c     delivers the stacks in the 'correct' order, the full psuedo-RP translation
c     for the i/p expression can thus be constructed.
c
c Passed Parameters
c  CHATTER            i   : Chattiness flag
c  EXPR               i   : I/p expression to be parsed
c  EXPR_OUT             o : O/p, pseudo-RP expression
c  IERR                 o : Error flag (zero = OK)
c 
c Called Routines
c  subroutine CRMVBLK              : (CALLIB) Removes blanks from string
c  subroutine FCECHO            : (FTOOLS) write to standard o/p
c  subroutine FNDPM            : (below) Finds location of all "=" & "-"
c  subroutine FNDSTACK             : (below) Finds start/stops of stacks
c  subroutine PARSTCK          : (below) Parses stack
c  subroutine SUBSTACK        : (below) Finds start/stops of substacks
c  
c Author/Modification History
c  Ian M George (1.0.0: 93 Dec 02) original
c  Ian M George (1.1.0: 93 Dec 08) fixed single bracket case
c
c Banashree Mitra Seifert (1.2.0:1996 March 20)
c           . Introduced screen display routines
c -------------------------------------------------------------------
      character(9) subname
      parameter (subname='parsexpr')
      character(7) version
      parameter (version='1.2.0')
*- 
c Internals
      integer iend, j, len
      integer maxstacks
      parameter (maxstacks = 1692)
      integer istack, clenact, i
      integer istart(maxstacks), istop(maxstacks)
      integer lstack(maxstacks), index(maxstacks)
      integer isub, nfound, MIN
      character(1) oper(maxstacks-1)
      character(16384) stack(maxstacks), expr_in
      character(16384) subexpr(maxstacks), safe(maxstacks)
      character(32768) tmpexpr, dmyexpr
      character(80) message
      character(32768) long_message

      message='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,message)
c Initialize
      ierr = 0
      nfound=0
      
c ... Remove spaces from the expression
      call crmvblk(expr)

c ... Store original expresssion
      expr_in = expr

c ------------------------- START BRACKET PARSER ----------------------
c ... This takes the i/p string containing the maths expression & divides it
c     into "stacks" based upon any pairs of () brackets present. The purpose
c     being to cope with expressions which contain embedded brackets 
c     (and embedded embedded brackets) indicating chunks of maths which must 
c     be evaluated first. The object is thus to find such bracket pairs, and 
c     work out from the 'inside out' to get the order in which such 
c     embedded maths must be evaluated. 
c ... This is achieved by making multiple passes through the 'running' 
c     expression, looking for bracket pairs (in fndstack), writing any 
c     'self-contained' expressions (ie without further brackets inside them) 
c     to the list of known stacks, and substituting the stack number (surrounded
c      by <>) for any such found expressions (in substack).

c ... (Re)Initialize all the arrays
 953  do i = 1, maxstacks
         istart(i) = 0
         istop(i) = 0
      enddo
      istack = 0
      iend = clenact(expr)

c ... Find out the number & location of all the stacks 
      call fndstack(chatter,expr,istack,istart,istop,ierr)
      if(ierr.NE.0) goto 999      
      message = 'current expression:'// 
     &     expr(:MIN(clenact(expr),60))
      call wtinfo(chatter,30,1,message)
      write(message,'(a,i12)') '# stacks found: ', istack
      call wtinfo(chatter,30,1,message)
c ..... fix no-brackets case
      if(istack.EQ.0) then
         istack = 1
         istart(1) = 1
         istop(1)  = iend
      endif
c ... perform initial population of the stacks 
      do i = 1, istack
         stack(i) = expr(istart(i)+1:istop(i)-1)
         lstack(i) = clenact(stack(i))
         index(i) = nfound + istack - (i-1)
      enddo
 
c ... Now, search for stacks within stacks, and substitute any found with 
c     the corresponding stack number surrounded by <>
c      (yukky idea, but I cant think of anything better)
c      ... forget it in the no-brackets and/or single stack case
      if(istack.EQ.1 .AND. istart(nfound+1).EQ.1 
     &     .AND. istop(nfound+1).EQ.iend) then
         nfound = nfound + 1
         if(expr(1:1).EQ.'(') then
            safe(nfound) = expr(2:iend-1)
         else
            safe(nfound) = expr(:iend)
         endif
         goto 876
      else
c       ... lets go hunting
         len = clenact(expr)
         call substack(chatter,expr(:len),istack,stack,
     &        index,isub,subexpr,dmyexpr, ierr)
         expr = dmyexpr
         if(isub.GE.1) then
c     ... found new stack(s), which we save, incrementing the 
c     stack pointer
            do i = 1, isub
               nfound = nfound + 1
               safe(nfound) = subexpr(i)
            enddo
            goto 953
         else
c              ... assume we must have run out of stacks to find
c                  increment stack pointer & save this result
            nfound = nfound + 1
            safe(nfound) = expr(:len)
            goto 876
         endif
      endif 

c ... Dump info to the user, if required
 876  continue
      long_message = 'Input expression:'//expr_in
      call wtinfo(chatter,25,1,long_message)
      write(message,'(a,i12)') ' of stacks:', nfound
      call wtinfo(chatter,25,1,message)
      do i = 1, nfound
         write(long_message,'(a,i12,a,a)') 'stack ', i, 
     &        ' ', safe(i)(1:clenact(safe(i)))
         call wtinfo(chatter,25,1,long_message)
      enddo
c ------------------------- END BRACKET PARSER ----------------------

c ...Now, hopefully what we have is an array of NFOUND character 
c    expressions SAFE(), each of which contains a set of maths operation which 
c    must be evaluated in the order 1 -> NFOUND (since latter stacks can 
c    depend upon the results of earlier stacks).


c ------------------------- START STACK PARSER ----------------------
c ... Now, we step through each of the SAFE() stacks founds above 
c     First we look for "sub-stacks" which are separated by "+" and "-" 
c     operations. The contents of these sub-stacks thus contain any 
c     combination of filenames, numerals and "*" and/or "/" operations.
c     Thus the sub-stacks can be relatively easily parsed to pseudo-Reverse 
c     Polish notation (simply by working from left to right), the 
c     relationship between the sub-stacks can also easily be parsed to
c     RP (again by working from left to right), and thus the pseudo-RP expressn
c     for the whole stack be constructed. Since the Bracket Parser (above) 
c     delivers the stacks in the 'correct' order, the full psuedo-RP translation
c     for the i/p expression can thus be constructed.
c     NOTE - some h.duty recycling of arrays is used here

      expr_out = ' '
c ... START main (stack) DO LOOP
      do j = 1, nfound

c       ... (Re)Initialize all the arrays
         do i = 1, maxstacks
            istart(i) = 0
            istop(i) = 0
         enddo
         istack = 0
         iend = clenact(safe(j))

c         ... Find out the number & location of all the sub-stacks 
c            (dividing on the basis of '+' & '-' operations)
         call fndpm(chatter,safe(j),maxstacks,istack,istart,istop,ierr)
         if(ierr.ne.0) goto 999
c     .... Fill the operations array
         do i = 1, istack-1
            oper(i) = safe(j)(istop(i)+1:istop(i)+1)
         enddo

c         ... perform population of the sub-stacks 
         do i = 1, istack
            stack(i) = safe(j)(istart(i):istop(i))
c           .... check for illegal character (at start)
            if(i.EQ.1 .AND. istart(1).NE.1) then
               ierr = 1
               message =  'Illegal expression'
               call wterrm(subname,version,message)
               message = 'offending characters is first '//
     &              'occurance of: "'// safe(j)(1:1)//'"'
               call wtinfo(chatter,0,1,message)
               message = 'in (sub) expression: "'//
     &              safe(j)(1:MIN(50,iend))//'"'
               call wtinfo(chatter,0,1,message)
               goto 999
c     .... check for illegal character (at end)
            elseif(i.EQ.istack .AND. istop(i).NE.iend) then
               ierr = 1
               message =  'Illegal expression'
               call wterrm(subname,version,message)
               message = 'offending characters is last '//
     &              'occurance of: "'// safe(j)(iend:iend)//'"'
               call wtinfo(chatter,0,1,message)
               message = 'in (sub) expression: "'//
     &              safe(j)(MAX(1,iend-50):iend)//'"'
               call wtinfo(chatter,0,1,message)
               goto 999
            endif


c            ... Parse the sub-stack expression
            call parstck(chatter,stack(i),tmpexpr, ierr)
            if(ierr.NE.0) then
               ierr = 1
               message =  ' Unable to parse expression'
               call wterrm(subname,version,message)
               goto 999
            endif

           
c             ... Add this to the current expression
c           ..... Remember, here:
c            ....... istack is the number of sub-stacks in this stack
c           ....... i is the counter of the sub-stacks (from 1->istack)
            if(istack.EQ.1) then
               dmyexpr = expr_out(:clenact(expr_out)) // 
     &              tmpexpr(:clenact(tmpexpr)) //','
            elseif(i.EQ.1) then
               dmyexpr = expr_out(:clenact(expr_out)) // 
     &              tmpexpr(:clenact(tmpexpr)) //',SAVE,'
            elseif(i.NE.istack) then
               dmyexpr = expr_out(:clenact(expr_out)) // 
     &              tmpexpr(:clenact(tmpexpr)) //
     &              ',RECALL,'//oper(i-1)//',SAVE,'
            else
               dmyexpr = expr_out(:clenact(expr_out)) // 
     &              tmpexpr(:clenact(tmpexpr)) //
     &              ',RECALL,'//oper(i-1)//','
            endif

            expr_out = dmyexpr(:clenact(dmyexpr))

         enddo
c        ... add a 'PUSH' after every set of substacks except last
         if(j.NE.nfound) then
            dmyexpr = expr_out(:clenact(expr_out)) // 'PUSH,'
         else
            dmyexpr = expr_out(:clenact(expr_out)) // 'STOP'
         endif
         expr_out = dmyexpr

c ... END main (stack) DO LOOP
      enddo


c ------------------------- END STACK PARSER ----------------------

c So now we have a character string EXPR_OUT containg the pseudo-RP 
c commands, each of which is separated by commas. This now has to be 
c parsed (location filenames, stacks, mathematical operations and reals) 
c and executed.


c Final error check
 999  if(ierr.NE.0) then
         message =  'Incomplete Execution'      
         call wterrm(subname,version,message)
      endif

      return
      end
c -------------------------------------------------------------------------
*+FNDSTACK
      subroutine fndstack(chatter,string,nfnd,istart,istop,ierr)

      IMPLICIT NONE
      integer nfnd, ierr, chatter
      integer istart(*), istop(*)
      character*(*) string
c Description
c  Routine to search a string for pairs of matching (), and return 
c the number of such pairs found, their locations, or an error flag
c
c Passed Parameters
c  CHATTER          i   : Chattiness flag - o/p only if >20
c  STRING           i   : i/p character string to be searched
c  NFND               o : No. pairs of () found
c  ISTART             o : Array of locations of each ( found
c  ISTOP              o : Array of locations of each ) found
c  IERR               o : Error flag (zero if everything OK)
c 
c Called Routines
c  Function CLENACT     : (CALLIB) Finds length of a string
c  subroutine FCECHO    : (FTOOLS) writes to standard o/p
c
c Author/Modification History
c  Ian M George (1.0.0:93 Nov 30) Original
c 
c Banashree Mitra Seifert (1.1.0: 1996Mar 20)
c            . Introduced screen display routines
c -----------------------------------------------------------------
      character(9) subname
      parameter(subname='fndstack')
      character(7) version
      parameter (version='1.1.0')
*-
c Internals 
      integer iend, icheck, i, clenact, j
      character(80) message
      logical qok(16384)

c ... give user info if requested
      message = 'using '//subname//'Ver '// version
      call wtinfo(chatter,10,2,message)

c Initialize
      nfnd = 0
      ierr = 0
      icheck = 0

c ... find length of string & populate OK array
      iend = clenact(string)
      if(iend.LE.0) then
         message =  'String of zero or -ve length'
         call wterrm(subname,version,message)
         ierr = 1
         return
      endif
      do i = 1, iend
        qok(i) = .true.
      enddo

c ... do the searching 
c      ... find the starts 
      do i = 1,iend
         if(string(i:i) .EQ. '(') then
            nfnd = nfnd + 1
            istart(nfnd) = i
         endif
      enddo
c       ... find the appropriate stops, working our way out from inside
      do j = nfnd,1,-1
         do i = istart(j)+1, iend
            if(qok(i)) then            
               if(string(i:i) .EQ. ')') then
                  istop(j) = i
                  qok(i) = .false.
                  icheck = icheck + 1
                  goto 753
               endif
            endif
         enddo
 753  enddo
      
c ... check for matching pairs of brackets
      if(nfnd .NE. icheck) then
         ierr = 1
         message =  'Unclosed brackets within expresssion'
         call wterrm(subname,version,message)
      endif

      return
      end
c -------------------------------------------------------------------------
*+FNDPM
      subroutine fndpm(chatter,string,maxstack,nfnd,istart,istop,ierr)

      IMPLICIT NONE
      integer nfnd, ierr, chatter, maxstack
      integer istart(*), istop(*)
      character*(*) string
c Description
c  Routine to search a string for "+" or "-", and return the number of 
c substrings bounded by such characters, and the start & stop locations 
c of those substrings within the string.
c
c Passed Parameters
c  CHATTER          i   : Chattiness flag - o/p only if >40
c  STRING           i   : i/p character string to be searched
c  MAXSTACK         i   : Max numbers of stacks allowed
c  NFND               o : No. pairs of "+" or "-" characters  found
c  ISTART             o : Array of locations of start of each substring found
c  ISTOP              o : Array of locations of stop of each substring found
c  IERR               o : Error flag (zero if everything OK)
c 
c Called Routines
c  Function CLENACT     : (CALLIB) Finds length of a string
c  subroutine FCECHO    : (FTOOLS) writes to standard o/p
c
c Author/Modification History
c  Ian M George (1.0.0:93 Dec 02) Original
c
c Banashree Mitra Seifert (1.1.0:1996 Mar 20)
c          . Introduced screen display routines
c  Ian M George (2.0.0:96 Sep 25) maxstack passed & nfnd.le.maxstack check added
c  Peter D Wilson (2.0.1:99 Sep 03) Don't parse quoted strings in expression
c ------------------------------------------------------------------
      character(6) subname
      parameter (subname='fndpm')
      character(7) version
      parameter (version='2.0.1')
*-
c Internals 
      integer iend, i, clenact
      character(80) message
      logical inQuotes

c ... give user info if requested
      message = 'using '//subname//' Ver '// version
      call wtinfo(chatter,40,2,message)

c Initialize
      nfnd = 0
      ierr = 0

c ... find length of string & set defaults
      iend = clenact(string)
      istart(1) = 1
      istop(1) = iend
      nfnd = 1

c ... do the searching 
      inQuotes = .false.
      do i = 1,iend
         if( string(i:i).eq.'''' ) then
            inQuotes = .not.inQuotes
         elseif( .not.inQuotes ) then
            if(string(i:i).EQ.'+' .OR. string(i:i).EQ.'-') then
               istop(nfnd) = i-1
               nfnd = nfnd + 1
               if(nfnd.GT.maxstack) then
                  ierr = 1
                  message = 'No. files appears to exceed MAXSTACKS'
                  call wterrm(subname,version,message)
                  message = 'Try repeating the calculation in stages'
                  call wtinfo(chatter,1,2,message)
                  goto 567
               endif
               istart(nfnd) = i+1
               istop(nfnd) = iend      
            endif
         endif
      enddo
      
      if( inQuotes ) then
         ierr = 1
         message = 'unbalanced quotes in expression'
         call wterrm(subname,version,message)
      endif
      
c     ... check
 567  if(ierr.ne.0) then
         message = 'unable to continue'
         call wterrm(subname,version,message)
      endif
      return
      end
c -------------------------------------------------------------------------
*+PARSTCK
      subroutine parstck(chatter,instr,outstr, ierr)

      IMPLICIT NONE
      integer ierr, chatter
      character*(*) instr,outstr
c Description
c  Routine to search i/p string and recast it in reverse polish.
c  !!! NOTE !!! This routine can only cope with the "*" & "/" maths 
c               operation, an error is returned of the i/p strings 
c               contains a "+" or "-" operators, or "()", all other parts of 
c               the i/p string are assumed to be either reals/integers or 
c               strings representing variables.
c  !!! NOTE !!! The recasting to RP is performed from left to right 
c
c Passed Parameters
c  CHATTER          i   : Chattiness flag - o/p only if >40
c  INSTR            i   : i/p character string to be converted 
c  OUTSTR           i   : o/p character string in RP notation
c  IERR               o : Error flag (zero if everything OK)
c 
c Called Routines
c  Function CLENACT     : (CALLIB) Finds length of a string
c  subroutine FCECHO    : (FTOOLS) writes to standard o/p
c
c Author/Modification History
c  Ian M George (1.0.0:93 Dec 03) Original
c
c Banashree Mitra Seifert (1.1.0: 1996 Mar20)
c         . Introduced screen display routines
c  Peter D Wilson (1.1.1:99 Sep 03) Don't parse quoted strings in expression
c --------------------------------------------------------------
      character(8) subname
      parameter (subname='parstck')
      character(7) version
      parameter (version='1.1.1')
*-
c Internals 
      integer iend, i, clenact, lout
      integer j, imark(499), nfnd
      character(80) message
      logical inQuotes

c Initialize
      ierr = 0

c ... give user info if requested
      message = 'using '//subname//'Ver '// version
      call wtinfo(chatter,40,2,message)

c ... find length of string & set defaults
      iend = clenact(instr)
      nfnd = 0

      outstr = ' '

c ... find all the '*' & '/' operators
      inQuotes = .false.
      do i = 1, iend
         if( instr(i:i).eq.'''' ) then
            inQuotes = .not.inQuotes
         elseif( .not.inQuotes ) then
            if(instr(i:i).EQ.'*' .OR. instr(i:i).EQ.'/') then
               nfnd = nfnd + 1
               imark(nfnd) = i
            elseif(instr(i:i).EQ.'+' .OR. instr(i:i).EQ.'-') then
               message = 'Expression contains illegal char'
               call wterrm(subname,version,message)
               ierr = 1
               return
            endif
         endif
      enddo
      
      if( inQuotes ) then
         ierr = 1
         message = 'unbalanced quotes in expression'
         call wterrm(subname,version,message)
         return
      endif

c Construct the relevant RP strings
c     NONE found
      if(nfnd.LE.0) then
         outstr = instr(1:iend)
c ONE found
      elseif(nfnd.EQ.1) then
         outstr = instr(1:imark(1)-1)//','//
     &        instr(imark(1)+1:iend)//','//
     &        instr(imark(1):imark(1))
c MANY found
      else
         outstr = instr(1:imark(1)-1)//','//
     &        instr(imark(1)+1:imark(2)-1)//','//
     &        instr(imark(1):imark(1))//',TEMP,'
         do j = 2, nfnd-1
            lout = clenact(outstr)
            outstr = outstr(:lout)//instr(imark(j)+1:imark(j+1)-1)
     &           //','//instr(imark(j):imark(j))//',TEMP,'
         enddo
         lout = clenact(outstr)
         outstr = outstr(:lout)//instr(imark(nfnd)+1:iend)
     &        //','//instr(imark(nfnd):imark(nfnd))
      endif

      return
      end
c -------------------------------------------------------------------------
*SUBSTACK
      subroutine substack(chatter,instr,nsubs,subs,
     &     index, idone, substr, outstr, ierr)

      IMPLICIT NONE
      integer nsubs, ierr, chatter, idone
      integer index(nsubs)
      character*(*) subs(nsubs), substr(nsubs)
      character*(*) instr, outstr
c Description
c  Routine to search an i/p string for embedded substrings and
c return an o/p string with any such substrings found replaced
c by the value of the substring index surrounded by <>
c
c Passed Parameters
c  CHATTER          i    : Chattiness flag - o/p only if >20
c  INSTR            i     i/p character string to be searched
c  NSUBS            i     No. substrings passed down
c  SUBS             i    : Array of substrings to be searched for
c  IDONE              0   The number of substrings substituted
c  SUBSTR             0   Array of substituted substrings
c  OUTSTR             o   Resultant string including any substitutions
c  IERR               o   Error flag (zero if everything OK)
c
c Called Routines
c  Function CLENACT     : (CALLIB) Finds length of a string
c  subroutine CRMVBLK   : (CALLIB) Removes blanks from string
c  subroutine FCECHO    : (FTOOLS) writes to standard o/p
c  subroutine FNDSTACK  : (below) Finds the starts/stops of stacks
c
c Author/Modification History
c  Ian M George (1.0.0:93 Nov 30) Original
c  Ian M George (1.0.1:93 Dec 08) Scanned version of o/p dump
c
c Banashree Mitra Seifert (1.1.0: 1996 Mar20)
c        . Introduced screen display routines
c ------------------------------------------------------------------
      character(9) subname
      parameter (subname='substack')
      character(7) version
      parameter (version='1.0.1')

c Internals
      integer clenact, fcstln, i, iend
      integer status, lstck
      integer lsubs(16384), istck
      integer istart(16384), istop(16384)
      character(80) message
      character(16384) newstr, stck, dumstr         
      logical qsubs(16384)

c Initialize
      idone = 0
      ierr = 0
      status = 0

c     give user info if requested
      message ='using '//subname//'ver '//  version
      call wtinfo(chatter,40,2,message)

c       calc the lengths of the substrings & set "used flags" to false
      do i = 1, nsubs
         lsubs(i) = fcstln(subs(i))
         qsubs(i) = .false.
      enddo
c ... Initialize our running string
      newstr = instr

c ... find the locations of any pairs of brackets remaining in newstr
 123  call crmvblk(newstr)
      iend = clenact(newstr)
      call fndstack(chatter,newstr,istck,istart,istop,status)
      if(status.NE.0) then
         ierr = 1
         goto 345
      endif
      
c ... return if no brackets found
      if(istck.EQ.1 .AND. istart(1).EQ.1
     &     .AND. istop(1).EQ.iend) then
         outstr = newstr
         goto 345
      elseif(istck.EQ.0) then
         outstr = newstr
         goto 345
c ... check whether the last found pair of brackets is known
      else
c          ..... extract the string containing the last found “stack”
         stck  = newstr(istart(istck)+1:istop(istck)-1)
         lstck  = fcstln(stck)
c          ..... loop through the known strings looking for a match
         do i  = 1,nsubs
            if(.NOT.qsubs(i)) then
               if(lstck.EQ.lsubs(i)) then
                  if(stck(:lstck) .EQ. subs(i)(:lsubs(i))) then
c                       .... GOT A MATCH !
                     qsubs(i) = .true.   
                     idone = idone + 1
                     substr(idone) = stck(:lstck)
                     if(istart(istck).LE.1) then
                        write(dumstr,'(a,i12,2a)') '<',index(istck)
     &                       ,'>', newstr(istop(istck)+1:iend)
                     elseif(istop(istck).GE.iend) then
                        write(dumstr,'(2a,i12,a)')
     &                       newstr(:istart(istck)-1),
     &                       '<',index(istck), '>'
                     else
                        write(dumstr, '(2a,i12,2a)')
     &                       newstr(:istart(istck)-1),
     &                       '<',index(istck),'>',
     &                       newstr(istop(istck)+1:iend)
                     endif
                     newstr = dumstr
                     goto 123
                  endif
               endif
            endif
         enddo            
         outstr = newstr
         goto 345
      endif

 345  if(ierr.NE.0) then
         message =  'Incomplete Execution'
         call wterrm(subname,version,message)
      endif

      return
      end
c -------------------------------------------------------------------------
*+GT_PHA
      subroutine gt_pha(chatter,phaexp,maxchan, chantyp,
     &     tlscop, instrum, filter, detnam,texpos,
     &     detchan, fchan, chan,ipha, pha, error,
     &     dtype,sysfrc,group,qualty,qpos, areascal, 
     &     backscal, corscal,backfil,corfil,rmffil,arffil,
     &     work, iwork, iwork2, errmeth, ierr)

      IMPLICIT NONE
      integer ierr, chatter, maxchan
      integer detchan, fchan
      integer chan(*), ipha(*)
      integer group(*), qualty(*)
      integer iwork(*), iwork2(*)
      integer dtype
      real texpos, areascal, backscal, corscal
      real pha(*), error(*), sysfrc(*)
      real work(*)
      character*(*) errmeth
      character*(*) chantyp
      character*(*) phaexp
      character*(*) tlscop, instrum, filter, detnam
      character*(*) backfil, corfil, rmffil, arffil
      logical qpos(*)


c Description
c  Opens the PHA, finds the correct/specified extension, and reads the PHA 
c dataset. The relevant info/arrays are returned. In the case of the PHA 
c data itself, the values are returned as count/sec irrespective of how 
c they are stored in the i/p file. 
c  NOTE: Any necessary conversion back to counts must be performed back in 
c  the POPULATE subroutine so that knowledge of all the files & want the 
c  user wants is not required here.
c
c Passed Parameters
c  CHATTER          i   : Chattiness flag - o/p only if >40
c  incomplete
c  IERR               o : Error flag (zero if everything OK)
c 
c Called Routines
c  subroutine CGETLUN   : (CALLIB) Gets a free logical unit
c  subroutine FCECHO      : (FTOOLS) Writes to standard o/p
c  subroutine FCPARS    : (FTOOLS) Parses filename[extn#] string
c  subroutine FNDEXT      : (CALLIB) Finds xtens based on EXTNAME value
c  subroutine FNDHDU      : (CALLIB) Finds xtens based on HDUCLASn values
c  subroutine FTMAHD      : (FITSIO) Move to an absolute xtens no.
c  subroutine FTMRHD      : (FITSIO) Move a relative no. xtens rel to CDU
c  subroutine FTOPEN      : (FITSIO) Open a FITS file
c  subroutine RDPHA1    : (CALLIB) reads a PHA dataset
c  subroutine WT_FERRMSG: (CALLIB) Writes standard FITSIO error message
c
c Author/Modification History
c  Ian M George (1.0.0:93 Dec 07) Original
c  Ian M George (2.0.0:94 Apr 20) passed back dtype variable
c  Ian M George (2.1.0:94 Oct 21) nsearch = 999 taken out
c  Ian M George (3.0.0:94 Dec 07) bug fix for missing chans
c  Ian M George (4.0.0:94 Dec 13) areascal added as passed paramter
c  Ian M George (4.1.0:95 Mar 13) bug fix when fchan = 0
c  Ian M George (5.0.0:95 Apr 09) backscal, corscal, backfil, corfil, 
c                          rmffil, arffil added as passed paramters
c  Ian M George (5.0.1:95 Aug 01) added errmeth are passed parameter
c  Ian M George (5.1.0:95 Dec 02) fixed bug for errmeth=G
c
c Banashree Mitra Seifert (5.2.0:1996 Mar20)
c          . Introduced screen display routines
c Peter D Wilson (June 29, 1998) 5.2.1:
c        . Added max_xflt parameter to rdpha1 function call
c toliver (5.2.2:1998 Aug 06)
c          . initialized status to 0 before call to fcpars
c -------------------------------------------------------------------
      character(7) subname
      parameter (subname='gt_pha')        
      character(7) version
      parameter (version='5.2.1')
*-
c Max arrays
      integer maxextn
      parameter (maxextn=99)
c Internals 
      integer i, jj, kk, imove
      integer extn, iunit, htype, status, block
      integer ninstr, nsearch, nfound
      integer next(maxextn)
      integer nbum
      real temp
      character(20) outhdu(9,maxextn), outver(9,maxextn)
      character(20) extnam(maxextn)
      character(20) instr(9)
      character(128) message
      character(2048) long_message
      character(80) phafil
c ... PHA-related stuff
      integer nchan
      integer n_xflt,max_xflt
      logical qerror, qsys, qqual,qgroup, pois
      real calcpois, calcpois2, calcpois3
      character(20) dmode
      character(20) hduclas2
      character(7) phaversn
      character(70) xflt(1)

c ... give user info if requested
      message = 'using '//subname//'Ver '// version
      call wtinfo(chatter,10,2,message)
c Initialize
      ierr = 0
      extn = 0
      nbum = 0
      qsys = .false.
c ... flush all the passed arrays
      do i = 1, maxchan
         chan(i)       = 0
         ipha(i)      = 0
         group(i)       = 0
         qualty(i)      = 0
         pha(i)       = 0.0
         error(i)       = 0.0
         sysfrc(i)       = 0.0
         qpos(i)      = .false.
      enddo


c Parse the supplied filenames, stripping off incld extension numbers
      status = 0
      call fcpars(phaexp,phafil,extn,status)
      if(status.NE.0) then
         message = 'Problem parsing the expression:'
         call wtwarm(subname,version,chatter,0,message)
         long_message = phaexp
         call wtinfo(chatter,0,2,long_message)
         message = 'will search all extensions'
         call wtinfo(chatter,0,2,message)
         extn = -99
      endif

c Open i/p file
      status = 0
      call cgetlun(iunit)
      call ftopen(iunit,phafil,0,block,status)
      IF (status.NE.0) THEN
         message = ' opening file: '//phafil(:20)
         call wtferr(subname,version,status,message)
         ierr = 1
         return
      ENDIF
      
c Find the SPECTRUM extension in the PHA file 
c - Extension number NOT given as part of phaexp (search for HDUCLAS/EXTNAM)
      if(extn.LT.0) then
         extnam(1)='SPECTRUM'
         ninstr = 1
         instr(1) = 'SPECTRUM'
         nsearch = maxextn
         call fndhdu(chatter, iunit, ninstr, instr,
     &        nsearch, nfound, next, outhdu, outver, extnam, ierr)
c     ... check for old-style EXTNAME values if no OK HDUCLASn values found
         if(nfound.LE.0) then
            message ='Ext w/ allowed HDUCLASn keywrds not found'
            call wtwarm(subname,version,chatter,0,message)
            message = 'offending file: '//phafil
            call wtinfo(chatter,0,1,message)
            message =
     &           ' ... searching for extension with EXTNAME = SPECTRUM'
            call wtinfo(chatter,0,1,message)
            call fndext(chatter, iunit, 'SPECTRUM',
     &           nsearch, nfound, next, outhdu, outver, extnam, ierr)
            if(nfound.EQ.1) then
               message = ' located acceptable extension'
               call wtinfo(chatter,0,1,message)
            endif
         endif
c  - Extension number IS given as part of phafil 
      else
         call ftmahd(iunit,extn+1,htype,status)
         message = ' Problem moving to specified xtens'
         call wtfwrn(subname,version,chatter,0,status, message)
c     ... grab the HDUCLAS values for reference
         ninstr = 1
         instr(1) = '*'
         nsearch = 1
         call fndhdu(MIN(chatter,20), iunit, ninstr, instr,
     &        nsearch, nfound, next, outhdu, outver, extnam, ierr)
         nfound = 1
         next(1) = 0
      endif

c - sort out what we've got
 369  if(nfound.GT.1) then
         do i = 1, nfound
            if(outhdu(2,i) .EQ.'DETECTOR') then
               if(i.NE.nfound) then
                  do kk = i,nfound
                     do jj = 1, 3
                        outhdu(jj,kk) = outhdu(jj,kk+1)
                     enddo
                     next(kk) = next(kk+1)      
                     extnam(kk) = extnam(kk+1)
                  enddo              
               endif
               nfound = nfound - 1
               goto 369
            endif
         enddo
         message = ' PHA file contains >1 PHA dataset'
         call wterrm(subname,version,message)
         write(message,'(a,i12,a)') ' ... ',
     &        nfound,' extensions found:'
         call wtinfo(chatter,0,1,message)
         do i = 1, nfound
            write(message,'(a,i12,a)') 'Ext ',next(i),':'      
            call wtinfo(chatter,0,1,message)
            write(message,'(6X,a,a)') 'EXTNAME  = ', extnam(i)
            call wtinfo(chatter,0,1,message)
            do jj = 1, 3
               write(message,'(6X,a,i1,2a)') 
     &              'HDUCLAS',jj,' = ',outhdu(jj,i)
               call wtinfo(chatter,0,1,message)
            enddo
         enddo
         message = 
     &        ' ... Extension # must be specified '//
     &        ' via "file[ext#]" in i/p expression'
         call wtinfo(chatter,0,1,message)
         ierr = 2
         goto 482
      elseif(nfound.LE.0) then
         message =
     &        ' Unable to locate a SPECTRUM extension'
         call wtwarm(subname,version,chatter,0,message)
         ierr = 2
         goto 482
      endif
      
c     Move to the Extension if not already there
      if(next(1).GT.0) then
         imove = next(1)
         status = 0
         call ftmrhd(iunit,imove,htype,status)
         message = ' Problem moving to SPECTRUM xtens'
         call wtfwrn(subname,version,chatter,0,status, message)
      endif

      max_xflt=1
c Read in the PHA data
      call rdpha1(iunit,maxchan,nchan,tlscop,instrum,detnam,
     &            filter,detchan,texpos,areascal,backscal,
     &            corscal,backfil,corfil,rmffil,arffil,xflt,
     &            max_xflt,n_xflt,dmode,chantyp,phaversn,hduclas2,
     &            fchan,chan,dtype,
     &            ipha,pha,qerror,error,qsys,sysfrc,qqual,
     &            qualty,qgroup,group,pois,ierr,chatter)
      if(ierr.NE.0) then
         message = ' Problem reading SPECTRUM xtens'
         call wtwarm(subname,version,chatter,0,message)
         goto 482
      endif

c Work out the errors, and Convert to counts per second if data not already 
c  in this format 
c  NOTE: data & errors are passed back in counts/sec irrespective of 
c  whether the user wants the algebra to be performed in counts or 
c  counts/sec space. Any necessary conversion back is performed back in 
c  the POPULATE subroutine so that knowledge of all the files & want the 
c  user wants is not required here.
c  If texpos = 0.0, then donot calculate the errors and warn user that
c  error calculation could be erroneous

      if(texpos .eq. 0) then
         message =  'exposure is zero. So error calculation '
     >        //'is skipped.  May be erroneous! '
         call wtwarm(subname,version,chatter,0,message)
         message =  'Errors are forced to 0.0 in this case!'
         call wtwarm(subname,version,chatter,0,message)

         do i=1,nchan
            error(i) = 0.0
         enddo
      else

         do i = 1, nchan
            if(dtype.EQ.1) then
               pha(i) = float(ipha(i))/texpos
               if(qsys) then
                  temp = ipha(i)*sysfrc(i)
               else
                  temp = 0.0
               endif
               if(pois) then
                  if(ipha(i).LT.20) then
                     qpos(i) = .true.
                  endif
                  if(ipha(i).GE.0)then
                     if(errmeth(1:7).EQ.'POISS-0') then
                        error(i) = 0.
                     elseif(errmeth(1:7).EQ.'POISS-1') then
                        error(i) = SQRT(calcpois(ipha(i))**2.0
     &                       + temp**2.0)/texpos
                     elseif(errmeth(1:7).EQ.'POISS-2') then
                        error(i) = SQRT(calcpois2(ipha(i))**2.0
     &                       + temp**2.0)/texpos
                     elseif(errmeth(1:7).EQ.'POISS-3') then
                        error(i) = SQRT(calcpois3(ipha(i))**2.0
     &                       + temp**2.0)/texpos
                     elseif(errmeth(1:1).EQ.'G') then
                        error(i) = SQRT(ipha(i)
     &                       + temp**2.0)/texpos
                     else
                        message =  'Unsupported Error Method'
                        call wterrm(subname,version,message)
                        message = 'Illegal value: Errmeth='//errmeth
                        call wtinfo(chatter,0,2,message)
                        ierr = 99
                        goto 482
                     endif
                  else
                     error(i) = (temp**2.0)/texpos  
                     if(qualty(i).NE.0) then
                        qualty(i) = 5
                        qqual = .true.
                        nbum = nbum + 1
                     endif
                  endif
               else
                  error(i) = SQRT(error(i)**2.+temp**2.0)/texpos
               endif
            else
               if(qsys) then
                  temp = pha(i)*sysfrc(i)
               else
                  temp = 0.0
               endif
               error(i) = SQRT(error(i)**2.0 + temp**2.0)
            endif
         enddo
         
      endif

c Look out for funnies:
c ... -ve Poiss errors
      if(nbum.GT.0) then
         message = 'Errors requested for -ve counts'
         call wtwarm(subname,version,chatter,0,message)
         message = ' ... in file: '//phafil
         call wtinfo(chatter,0,1,message)
         write(message,'(2a,i12)') 'No. channels ',
     &        'affected (not prev marked as bad): ', nbum
         call wtinfo(chatter,0,1,message)
         message =
     &        ' These channels will be set to bad in '//
     &        'the o/p file'
         call wtinfo(chatter,0,1,message)
      endif
      
      
c Shift everything in arrays to account for missing channels
c ... (ie make index of pha, error, sysfrc, qualty, group 
c         datasets equivalent to CHANNEL number)
c         Temporary (Naff) fix

c ... First find all the missing channels 
      nbum = 0
      do i = 1, nchan
 985     if(chan(i).EQ.(i+nbum+fchan-1))then
            iwork2(i) = i+nbum
         else            
            nbum = nbum+1
            if((i+nbum+fchan-1).GT.maxchan) then
               ierr = -99
               message = 'Problem with missing channels'
               call wterrm(subname,version,message)
               goto 482
            else
               goto 985
            endif
         endif
      enddo
c     ... Warn the user
      if(nbum.GT.0) then
         message =  'Some channels not stored'
         call wtwarm(subname,version,chatter,10,message)
         write(message,'(2a,i12)') ' No. channels ',
     &        'affected (not prev marked as bad): ', nbum
         call wtinfo(chatter,10,1,message)
         message =
     &        ' These channels will be set to bad in '//
     &        'the o/p file'
         call wtinfo(chatter,10,1,message)
      endif


c ... PHA DATASET
      do i = 1, nchan
         work(i) = pha(i)
         pha(i) = 0.0
      enddo
      do i = 1, nchan
         pha(iwork2(i)) = work(i)
      enddo      
c ... PHA ERRORS
      do i = 1, nchan
         work(i) = error(i)
         error(i) = 0.0
      enddo
      do i = 1, nchan
         error(iwork2(i)) = work(i)
      enddo      
c ... SYSFRAC ERRORS
      do i = 1, nchan
         work(i) = sysfrc(i)
         sysfrc(i) = 0.0
      enddo
      do i = 1, nchan
         sysfrc(iwork2(i)) = work(i)
      enddo      
c ... QUALITY 
      do i = 1, nchan
         iwork(i) = qualty(i)
         qualty(i) = 5
      enddo
      do i = 1, nchan
         qualty(iwork2(i)) = iwork(i)
      enddo

c ... GROUP 
      do i = 1, nchan
         iwork(i) = group(i)
         group(i) = 0
      enddo
      do i = 1, nchan
         group(iwork2(i)) = iwork(i)
      enddo      


c Check for errors
 482  if(ierr.ne.0) then
         message = 'Unable to recover'
         call wterrm(subname,version,message)
      endif
c Close the FITS file
      call ftclos(iunit, status) 
      call ftfiou(iunit, status) 

      return
      end
c -------------------------------------------------------------------------
*+ POPULATE
        subroutine populate(chatter,nfil,file,filtyp,
     &     alg_units,
     &     comment, nk_comm, maxchan, 
     &     rdetchan, rfchan, rtlscop, rinstrum, rfilter,
     &     rdetnam, rchantyp,texpos, expos,
     &     data, err, qual, 
     &     chan,ipha, pha, error, sysfrc, 
     &     group, qual_in, qprob, qpos, qfexp, 
     &     exposexp, areascalexp, areascal, 
     &     backfexp, backfil, backsexp, backscal, 
     &     corfexp, corfil, corsexp, corscal,
     &     rmfexp, rmffil, arfexp, arffil,
     &     work, iwork, iwork2, errmeth, ierr)

      IMPLICIT NONE
      integer chatter, nfil, maxchan, nk_comm, ierr
      integer chan(*), ipha(*), group(*), qual_in(*), qual(*)
      integer filtyp(*)
      integer rdetchan, rfchan, iwork(*), iwork2(*)
      real pha(*), error(*), sysfrc(*), texpos, expos(*)
      real data(maxchan,*), err(maxchan,*), work(maxchan)
      real areascal, backscal, corscal
      character*(*) backfexp, backsexp, corfexp, corsexp
      character*(*) arfexp, rmfexp, areascalexp, errmeth
      character*(*) backfil, corfil, rmffil, arffil
      character*(*) file(*), exposexp, comment(*), alg_units
      character*(*) rtlscop, rinstrum, rfilter, rdetnam, rchantyp
      logical qprob(*), qpos(*), qfexp
      
c Description
c  This routine finds the correct extension for each PHA file, performs a 
c number of checks, and if all OK, populates the data arrays. It was added 
c (ie code taken out of main) in order to allow the introduction of Dynamic
c Memory Allocation.
c
c Author/Modification History
c  Ian M George     (1.0.0:1994 Mar 10), original
c  Ian M George     (2.0.0:1994 Apr 21), added units parameter
c  Ian M George     (3.0.0:94 Dec 07) added workspace arrays for GT_PHA
c  Ian M George     (3.1.0:94 Dec 13) added areascal as passed parameter
c                              plus a bunch of related checking
c  Ian M George     (4.0.0:94 Dec 13) added backfil,backscal,corfil,corscal,
c                              rmffil,arffil as passed parameters
c                              plus a bunch of related checking
c  Ian M George     (5.0.0:95 Apr 10), added parameters for 'aux files'
c                              plus loads of related checking
c  Ian M George     (5.0.1:95 Apr 18), added areascalexp parameter
c  Ian M George     (5.0.2:95 Apr 20), bug fix for areascalexp='NULL'
c  Ian M George     (5.0.3:95 Jun 05), bug fix for backsexp & corsexp = REAL #
c  Ian M George     (5.0.4:95 Aug 01), added errmeth as passed parameter
c
c Banashree Mitra Seifert (5.1.0:1996 Mar 20)
c            . Introduced screen display routines
c Banashree Mitra Seifert (5.2.0:1997 June 9)
c            . it was not checking the backscal value when qfbacks is false
c              so it is introduced 
c              if(qfbacks=false) backscal=in_backscal line#3496
c              Also while doing this I found that corrscal had also same
c              problem. So fixed that as:
c              if(qfcors=false) corrscal=in_corrscal line#3592  
c Banashree Mitra Seifert (5.3.0:1997 August 13)
c            . last fix was not ok. So it needed modification when
c              on the command line backscal keyword is set, (e.g.
c              mathpha backscal=1.0
c            . same correction done for corrscal   
c ---------------------------------------------------------------
      character(9) subname
      parameter (subname='populate')
      character(7) version
      parameter (version='5.3.0')
*-
c Internals 
      integer j, i, clenact, dtype
      integer detchan, fchan
      integer itype1, itype2
      real ontime, rareascal, rbackscal, rcorscal
      real in_backscal, in_corscal, in_areascal
      real areasum, areaerr
      real backsum, backerr
      real corsum, corerr
      character(80) rbackfil, rcorfil, rrmffil, rarffil
      character(80) in_backfil, in_corfil, in_arffil, in_rmffil
      character(20) chantyp
      character(20) tlscop, instrum, filter, detnam
      character(80) message
      logical qans
      logical qgrp, qokarea, qokbackf, qokbacks
      logical qfbackf, qfbacks, qfcorf, qfcors
      logical qokcorf, qokcors, qfarea
      logical qfarf, qfrmf, qokarf, qokrmf
      logical qokinst, qokdetnam, qokchantyp

      message='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,message)

c Initialize
      qgrp = .false.
      qokinst = .true.
      qokdetnam = .true.
      qokchantyp = .true.
      qfarea = .true.      
      qokarea = .true.
      areasum = 0.0
      qokbackf = .true.
      qokbacks = .true.
      backscal = -99.0
      backsum = 0.0
      qokcorf = .true.
      qokcors = .true.
      corscal = -99.0
      corsum = 0.0
      qokarf = .true.
      qokrmf = .true.
      ierr = 0
      itype1=0
      itype2=0


      do i = 1, maxchan
         qual(i) = 0
      enddo


c ... Areascal
      if(areascalexp.EQ.'%')then
         qfarea = .false.
      elseif(areascalexp.EQ.'NULL') then
         qfarea = .false.
         areascal = 1.0
      else
c        try to get the real value, reverse sense of success/failure
c        to determine how to handle value later on
         call get_real(chatter, areascalexp, areascal, qans)
         qfarea = .not. qans
      endif

c Sort out the ancillary keyword handling flags
c ... backfile 
      if(backfexp.EQ.'NONE') then
         backfil = 'NONE'
         qfbackf = .false.
      elseif(backfexp.EQ.'%') then
         qfbackf = .false.
      else
         qfbackf = .true.
      endif
c ... backscal
      if((backsexp.EQ.'%').OR.(backsexp.EQ.'NONE')) then
         qfbacks = .false.
      else
c        try to get the real value, reverse sense of success/failure
c        to determine how to handle value later on
         call get_real(chatter, backsexp, backscal, qans)
         qfbacks = .not. qans
      endif

c ... corrfile 
      if(corfexp.EQ.'NONE') then
         corfil = 'NONE'
         qfcorf = .false.
      elseif(corfexp.EQ.'%') then
         qfcorf = .false.
      else
         qfcorf = .true.
      endif
c ... corrscal
      if((corsexp.EQ.'%').OR.(corsexp.EQ.'NONE')) then
         qfcors = .false.
      else
c        try to get the real value, reverse sense of success/failure
c        to determine how to handle value later on
         call get_real(chatter, corsexp, corscal, qans)
         qfcors = .not. qans
      endif
c ... arf file 
      if(arfexp.EQ.'NONE') then
         arffil = 'NONE'
         qfarf = .false.
      elseif(arfexp.EQ.'%') then
         qfarf = .false.
      else
         qfarf = .true.
      endif
c ... rmf file 
      if(rmfexp.EQ.'NONE') then
         rmffil = 'NONE'
         qfrmf = .false.
      elseif(rmfexp.EQ.'%') then
         qfrmf = .false.
      else
         qfrmf = .true.
      endif

c *********************** Start of files do loop *************************
      do j = 1, nfil
         message = 'processing file: '//
     &        file(j)(:MIN(clenact(file(j)),40))
         call wtinfo(chatter,9,3,message)

c         *** Go find, open the file, locate the extens with the PHA 
c            dataset, read in the data & close the file
         call gt_pha(chatter,file(j),maxchan, chantyp,
     &        tlscop, instrum, filter, detnam,ontime,
     &        detchan, fchan, chan,ipha, pha, error,
     &        dtype,sysfrc,group,qual_in,qprob, in_areascal,
     &        in_backscal, in_corscal,in_backfil, in_corfil,
     &        in_rmffil,in_arffil,
     &        work, iwork, iwork2, errmeth, ierr)
         if(ierr.NE.0) then
            goto 482
         endif
         filtyp(j) = dtype      
         
c Check out if this is file the user wants various values copied from
c ... the exposure time 
         if(qfexp) then
            if(exposexp.EQ.file(j)) then
               texpos = ontime
            endif
         endif
c ... the areascal 
         if(qfarea) then
            if(areascalexp.EQ.file(j)) then
               areascal = in_areascal
            endif
         endif
c ... the backfil
         if(qfbackf) then
            if(backfexp.EQ.file(j)) then
               backfil = in_backfil
            endif
         endif
c ... the backscal
         if(qfbacks) then
            if(backsexp.EQ.file(j)) then
               backscal = in_backscal
            endif
         endif      

c ... the corrfil
         if(qfcorf) then
            if(corfexp.EQ.file(j)) then
               corfil = in_corfil
            endif
         endif
c ... the corscal
         if(qfcors) then
            if(corsexp.EQ.file(j)) then
               corscal = in_corscal
            endif
         endif
c ... the arf fil
         if(qfarf) then
            if(arfexp.EQ.file(j)) then
               arffil = in_arffil
            endif
         endif
c ... the rmf fil
         if(qfrmf) then
            if(rmfexp.EQ.file(j)) then
               rmffil = in_rmffil
            endif
         endif


c         *** Check for inconsistencies
c        ... The number of detector channels and/or first channel
         if(j.EQ.1) then
            rdetchan  = detchan
            rfchan    = fchan
         else
            if(detchan.NE. rdetchan .OR. 
     &           fchan.NE .rfchan) then
               message = ' Detector channel mismatch'
               call wterrm(subname,version,message)
               message = 'Previous files have had:'
               call wtinfo(chatter,0,1,message)
               write(message,'(a,i12)') 
     &              'No. Det channels (DETCHANS) = ',
     &              rdetchan
               call wtinfo(chatter,0,2,message)
               write(message,'(a,i12)') 
     &              '1st Det channel   (TLMIN1) = ',
     &              rfchan
               call wtinfo(chatter,0,2,message)
               message = 'Current file ('// 
     &              file(j)(:MIN(clenact(file(j)),30))// ') has:'
               call wtinfo(chatter,0,1,message)
               write(message,'(a,i12)') 
     &              'No. Det channels (DETCHANS) = ',
     &              detchan
               call wtinfo(chatter,0,2,message)
               write(message,'(a,i12)') 
     &              '1st Det channel   (TLMIN1) = ',fchan
               call wtinfo(chatter,0,2,message)
               message = '---Unable to resolve discrepancy---'
               call wtinfo(chatter,0,0,message)
               ierr = 1
               goto 482
            endif
         endif


c  ... The telescope & (probably important) friends
         if(j.EQ.1) then
c ... Fill in the reference values
            rtlscop  = tlscop
            rinstrum = instrum
            rfilter  = filter
            rdetnam  = detnam
            rchantyp = chantyp
            rareascal = in_areascal
            areasum = rareascal
            rbackfil = in_backfil
            rbackscal = in_backscal
            backsum = rbackscal
            rcorfil = in_corfil
            rcorscal = in_corscal
            corsum = rcorscal
            rrmffil = in_rmffil
            rarffil = in_arffil
         else
c .... Check the mission/instrument/filter
            if(tlscop.NE.rtlscop .OR.
     &           instrum.NE.rinstrum .OR.
     &           filter.NE.rfilter) then
               message = 'Instrument mismatch'
               call wtwarm(subname,version,chatter,5,message)
               nk_comm = nk_comm + 1
               comment(nk_comm) = message
               nk_comm = nk_comm + 1
               comment(nk_comm) = ' ... for file: '//
     &              file(j)(:MIN(clenact(file(j)),40))
               message = 'data from different'//
     &              ' mission/inst/filter than prev datasets'
               call wtinfo(chatter,5,2,message)
               nk_comm = nk_comm + 1
               comment(nk_comm) = message
               qokinst = .false.
            endif
            if(rchantyp.NE.chantyp) then
               message = ' Detector channels mismatch'
               call wtwarm(subname,version,chatter,5,message)
               message = 'channel type different'//
     &              ' to prev datasets'
               call wtinfo(chatter,5,2,message)
               qokchantyp = .false.
            endif
            if(rdetnam.NE.detnam ) then
               message = 'Sub-detector mismatch'
               call wtwarm(subname,version,chatter,5,message)
               message = 'data from different'//
     &              ' sub-detector to prev datasets'
               call wtinfo(chatter,5,2,message)
               qokdetnam = .false.
            endif
c .... Check the AREASCALing (if necessary)
            if(areascalexp.EQ.'%') then
               areasum = areasum + in_areascal
               areaerr = ABS(rareascal-in_areascal)/rareascal
               if(areaerr.GT.0.01) then
                  message = 'Difference area-scaling factors'
                  call wtwarm(subname,version,chatter,9,message)
                  write(message,'(a,g12.6)') 
     &                 '1st dataset reference value: ',rareascal
                  call wtinfo(chatter,9,2,message)
                  write(message,'(a,g12.6)') 
     &                 'this dataset (>1% different): ',in_areascal
                  call wtinfo(chatter,9,2,message)
                  qokarea = .false.
               endif
            endif
c .... Check the BACKFILE
            if((backfexp.EQ.'%').AND.
     &           (in_backfil.NE.rbackfil)) then
               message =  ' Background file mismatch'
               call wtinfo(chatter,15,2,message)
               nk_comm = nk_comm + 1
               comment(nk_comm) = message
               nk_comm = nk_comm + 1
               comment(nk_comm) = ' ... for file: '//
     &              file(j)(:MIN(clenact(file(j)),40))
               message = 'pointer to different'//
     &              ' file cf prev datasets'
               call wtinfo(chatter,15,2,message)
               nk_comm = nk_comm + 1
               comment(nk_comm) = message
               qokbackf = .false.
            endif
c .... Check the BACKSCALing
            if(backsexp.EQ.'%')then
               backsum = backsum + in_backscal
               backerr = ABS(rbackscal-in_backscal)/rbackscal
               if(backerr.GT.0.01) then
                  message = 'Difference bkgd-scaling factors'
                  call wtwarm(subname,version,chatter,9,message)
                  write(message,'(a,g12.6)') 
     &                 '1st dataset reference value: ',rbackscal
                  call wtinfo(chatter,9,2,message)
                  write(message,'(a,g12.6)') 
     &                 'this dataset (>1% different): ',in_backscal
                  call wtinfo(chatter,9,2,message)
                  qokbacks = .false.
               endif
            endif
c .... Check the CORRFILE
            if((corfexp.EQ.'%').AND.
     &           (in_corfil.NE.rcorfil)) then
               message = 'Correction file mismatch'
               call wtwarm(subname,version,chatter,15,message)
               nk_comm = nk_comm + 1
               comment(nk_comm) = message
               nk_comm = nk_comm + 1
               comment(nk_comm) = ' ... for file: '//
     &              file(j)(:MIN(clenact(file(j)),40))
               message = ' pointer to different'//
     &              ' file cf prev datasets'
               call wtinfo(chatter,15,2,message)      
               nk_comm = nk_comm + 1
               comment(nk_comm) = message
               qokcorf = .false.
            endif
c .... Check the CORRSCALing
            if(corsexp.EQ.'%')then
               corsum = corsum + in_corscal
               corerr = ABS(rcorscal-in_corscal)/rcorscal
               if(corerr.GT.0.01) then
                  message = 'Difference corrfile-scalings'
                  call wtwarm(subname,version,chatter,9,message)
                  write(message,'(a,g12.6)') 
     &                 '1st dataset reference value: ', rcorscal
                  call wtinfo(chatter,9,2,message)
                  write(message,'(a,g12.6)') 
     &                 'this dataset (>1% different): ',in_corscal
                  call wtinfo(chatter,9,2,message)
                  qokcors = .false.
               endif
            endif
c .... Check the ANCRFILE
            if((arfexp.EQ.'%').AND.
     &           (in_arffil.NE.rarffil)) then
               message =  ' ARF file mismatch'
               call wtwarm(subname,version,chatter,15,message)
               nk_comm = nk_comm + 1
               comment(nk_comm) = message
               nk_comm = nk_comm + 1
               comment(nk_comm) = ' ... for file: '//
     &              file(j)(:MIN(clenact(file(j)),40))
               message = 'pointer to different'//
     &              ' file cf prev datasets'
               call wtinfo(chatter,15,2,message)
               nk_comm = nk_comm + 1
               comment(nk_comm) = message
               qokarf = .false.
            endif
c     .... Check the RESPFILE
            if((rmfexp.EQ.'%').AND.
     &           (in_rmffil.NE.rrmffil)) then
               message =  ' RMF file mismatch'
               call wtwarm(subname,version,chatter,15,message)
               nk_comm = nk_comm + 1
               comment(nk_comm) = message
               nk_comm = nk_comm + 1
               comment(nk_comm) = ' ... for file: '//
     &              file(j)(:MIN(clenact(file(j)),40))
               message = 'pointer to different'//
     &              ' file cf prev datasets'
               call wtinfo(chatter,15,2,message)     
               nk_comm = nk_comm + 1
               comment(nk_comm) = message
               qokrmf = .false.
            endif

c         ... Warn for grouping, if requested
            if(chatter.GE.20) then
               qgrp = .false.
               do i = 1, detchan
                  if(group(i).NE.0) then
                     qgrp = .true.
                     message = ' Grouping info will be lost'
                     call wtwarm(subname,version,chatter,20,message)
                     goto 567
                  endif
               enddo
 567        endif
            
         endif

c      *** Finished with the warnings, get on with the array population
         do i = 1, detchan
            data(i,j) = pha(i)
            err(i,j) = error(i)
            if(qprob(i)) qpos(i) = .true.
            if(qual_in(i).NE.0) qual(i) = 5
         enddo
c     if(qcalexp) expos(j) = ontime
         expos(j) = ontime


c *********************** End of files do loop *************************
      enddo


c *** Sort out how algebra should be performed & fix up arrays
c            .... in counts or counts/s
c                 (default is counts/s)

      if(alg_units.EQ.'F') then
         do j = 1, nfil
            if(filtyp(j).EQ.1) then
               itype1 = itype1 + 1
            elseif(filtyp(j).EQ.2) then
               itype2 = itype2 + 1
            endif      
         enddo
         if(itype1.GE.itype2) then
            alg_units = 'C'
         else
            alg_units = 'R'
         endif
      endif

c ... Convert from count/s to counts
      if(alg_units.EQ.'C') then
         do j = 1, nfil
            do i = 1, detchan
               data(i,j) = data(i,j)*expos(j)
               err(i,j) = err(i,j)*expos(j)
            enddo
         enddo
      endif

c ................ Check out the MISSION/INSTRUMENT/FILTER values ........
      if((.NOT.qokinst).OR.(.NOT.qokdetnam).OR.
     &     (.NOT.qokchantyp)) then
         message = 'Instrumental discrepancies detected'
         call wtwarm(subname,version,chatter,0,message)
         nk_comm = nk_comm + 1
         comment(nk_comm) = message
         message = 'proceeding, but maybe you should '//
     &        'check your i/p files'
         call wtinfo(chatter,0,2,message)          
      endif
      
c     ................ Check out the MISSION/INSTRUMENT/FILTER values ........
      if(.NOT.qokinst) then
         message = 'Following keywrds set to '//
     &        'null values in o/p file:'
         call wtinfo(chatter,0,1,message)
         message = 'TELESCOP, INSTRUME, FILTER'
         call wtinfo (chatter,0,2,message)
         rtlscop = 'UNKNOWN'
         rinstrum= 'UNKNOWN'
         rfilter = 'UNKNOWN'
      endif
c     ................ Check out the DETNAM values ...........................
      if(.NOT.qokdetnam) then
         message = 'DETNAM keywrd set to '//
     &        'UNKNOWN in o/p file'
         call wtinfo(chatter,0,1,message)
         rdetnam = 'UNKNOWN'
      endif
c     ................ Check out the CHANTYPE values ..........................
      if(.NOT.qokchantyp) then
         message = 'CHANTYPE keywrd set to '//
     &        'UNKNOWN in o/p file'
         call wtinfo(chatter,0,1,message)
         rchantyp = 'UNKNOWN'
      endif

c ... Worry about AREASCAL 
c ... case when asked to try and work it out for ourselves
      if(areascalexp.EQ.'%') then
         if(qokarea) then
            areascal = areasum/float(nfil)
            if(chatter.ge.5) then
               if(nfil.GT.1) then
                  write(message,'(a,f10.3)')
     &                 'AREASCAL keyword in o/p file set to ', areascal
                  call wtinfo(chatter,0,1,message)
                  message = 
     &                 ' (mean of values from i/p files;'
     &                 // ' all of which agreed to <1%)'
                  call wtinfo(chatter,0,2,message)
               endif
            endif
         else
            areascal = 1.0
            message = 'Probable AREASCAL problem'
            call wtwarm(subname,version,chatter,0,message)
            nk_comm = nk_comm + 1
            comment(nk_comm) = message
            message =  'values from i/p files disagree '//
     &           'by >1% (see above)'
            call wtinfo(chatter,0,2,message)
            nk_comm = nk_comm + 1
            comment(nk_comm) = message
            message = 
     &           'MATHPHA unable to determine correct value '
            call wtinfo(chatter,0,2,message)
            nk_comm = nk_comm + 1
            comment(nk_comm) = message
            message = 
     &           'AREASCAL keyword in o/p file will be set to 1.0'
            call wtinfo(chatter,0,1,message)
            message = 
     &           '(you may wish to reset the AREASCAL keyword '//
     &           'value using {say} GRPPHA)'
            call wtinfo(chatter,0,1,message)
         endif
      else
c     ...... case when it should be taken from a file
         if(qfarea) then
            if(areascal.EQ.-99.0) then
               areascal = 1.0
               message = 'file for AREASCAL keywrd '//
     &              'not found'
               call wtwarm(subname,version,chatter,0,message)
               message = 
     &              'AREASCAL keywrd set to '//
     &              'null value (1.0) in o/p file'
               call wtinfo(chatter,0,1,message)
            endif
         endif
      endif

c ................ Check out the BACKGROUND file............................
c     ... case when asked to try and work it out for ourselves
      if(backfexp.EQ.'%')then
         if(qokbackf) then
            if(chatter.ge.9) then
               if((nfil.GT.1).AND.(in_backfil.NE.'none').AND.
     &              (in_backfil.NE.'none').AND.(in_backfil.NE.' ')) then
                  message = 
     &          'BACKFILE keyword in o/p file copied from i/p files '//
     &                 '(all agree)'
                  call wtinfo(chatter,9,1,message)
               endif
               backfil = in_backfil
            endif
         else
            backfil = 'NONE'
            message = 'BACKFILE keywrds from '//
     &           'i/p files disagree'
            call wtwarm(subname,version,chatter,0,message)
            message = 
     &           'BACKFILE keywrd set to '//
     &           'null/default value in o/p file'
            call wtinfo(chatter,0,1,message)
         endif
      else
c     ...... case when it should be taken from a file
         if(qfbackf) then
            if(backfil.EQ.' ') then
               backfil = 'NONE'
               message = 'file for BACKFILE keywrd '//
     &              'not found'
               call wtwarm(subname,version,chatter,0,message)
               message = 'BACKFILE keywrd set to '//
     &              'null/default value in o/p file'
               call wtinfo(chatter,0,1,message)
               qokbackf = .false.
            endif
c ...... case when it's been entered not necessary (performed in main)
         endif
      endif

c ................ Check out the BACKGROUND scaling .........................
c ... case when asked to try and work it out for ourselves
      if((qokbackf).AND.(backsexp.EQ.'%')) then
         if(qokbacks) then
            if(chatter.ge.9) then
               if((nfil.GT.1).AND.(in_backfil.NE.'none').AND.
     &              (in_backfil.NE.'none').AND.(in_backfil.NE.' ')) then
                  write(message,'(a,g12.6)')
     &                 'BACKSCAL keyword in o/p file set to ', backscal
                  call wtinfo(chatter,0,1,message)
                  message = '(mean of values from i/p files;'
     &                 // ' all of which agreed to <1%)'
                  call wtinfo(chatter,0,2,message)
               endif
               backscal = in_backscal
            endif
         else
            backscal = 1
            message = 'BACKSCAL keywrds from '//
     &           'i/p files disagree'
            call wtwarm(subname,version,chatter,0,message)
            message = 'BACKSCAL keywrd set to '//
     &           'null/default value in o/p file'
            call wtinfo(chatter,0,1,message)
         endif
      elseif((.NOT.qokbackf).AND.(backsexp.EQ.'%')) then
c     ...... case when calc'd OK, but BACKFILE is corrupt
         backscal = 1
         message = 'BACKSCAL keywrd set to '//
     &        'null/default value in o/p file'
         call wtinfo(chatter,0,1,message)
      else
c     ...... case when it should be taken from a file
         if(qfbacks) then
            if(backscal.EQ.-99.0) then
               backscal = 1
               message = 'file for BACKSCAL keywrd '//
     &              'not found'
               call wtwarm(subname,version,chatter,0,message)
               message = 
     &              'BACKSCAL keywrd set to '//
     &              'unity in o/p file'
               call wtinfo(chatter,0,1,message)
            endif
         else
            if(backscal.EQ.-99.0)  backscal = 1
ccc   backscal  = in_backscal
       
c ...... case when it's been entered not necessary (performed in main)
         endif
      endif

c ................ Check out the CORRECTION file............................
c ... case when asked to try and work it out for ourselves
      if(corfexp.EQ.'%')then
         if(qokcorf) then
            if(chatter.ge.9) then
               if((nfil.GT.1).AND.(in_corfil.NE.'none').AND.
     &              (in_corfil.NE.'none').AND.(in_corfil.NE.' ')) then
                  message = 'CORRFILE keyword in o/p file copied '//
     &                 'from i/p files (all agree)'
                  call wtinfo(chatter,0,1,message)
               endif
               corfil = in_corfil
            endif
         else
            corfil = 'NONE'
            message = 'CORRFILE keywrds from '//
     &           'i/p files disagree'
            call wtwarm(subname,version,chatter,0,message)
            message = 
     &           'CORRFILE keywrd set to '//
     &           'null/default value in o/p file'
            call wtinfo(chatter,0,1,message)
         endif
      else
c     ...... case when it should be taken from a file
         if(qfcorf) then
            if(corfil.EQ.' ') then
               corfil = 'NONE'
               message =  'file for CORRFILE keywrd '//
     &              'not found'
               call wtwarm(subname,version,chatter,0,message)
               message = 
     &              'CORRFILE keywrd set to '//
     &              'null/default value in o/p file'
               call wtinfo(chatter,0,1,message)
               qokcorf = .false.
            endif
c     ...... case when it's been entered not necessary (performed in main)
         endif
      endif

c ................ Check out the CORRFILE scaling .........................
c ... case when asked to try and work it out for ourselves
      if((qokcorf).AND.(corsexp.EQ.'%')) then
         if(qokcors) then
            if(chatter.ge.9) then
               if((nfil.GT.1).AND.(in_corfil.NE.'none').AND.
     &              (in_corfil.NE.'none').AND.(in_corfil.NE.' ')) then
                  write(message,'(a,g12.6)')
     &                 ' ... CORRSCAL keyword in o/p file set to ', 
     &                 corscal
                  call wtinfo(chatter,0,1,message)
                  message = 
     &                 '(mean of values from i/p files;'
     &                 // ' all of which agreed to <1%)'
                  call wtinfo(chatter,0,2,message)
               endif
               corscal = in_corscal
            endif
         else
            corscal = 1
            message = 'CORRSCAL keywrds from '//
     &           'i/p files disagree'
            call wtwarm(subname,version,chatter,0,message)
            message = 
     &           'CORRSCAL keywrd set to '//
     &           'null/default value in o/p file'
            call wtinfo(chatter,0,1,message)
         endif
      elseif((.NOT.qokcorf).AND.(corsexp.EQ.'%')) then
c     ...... case when calc'd OK, but CORRFILE is corrupt
         corscal = 1
         message = 
     &        'CORRSCAL keywrd set to '//
     &        'null/default value in o/p file'
         call wtinfo(chatter,0,1,message)
      else
c ...... case when it should be taken from a file
         if(qfcors) then
            if(corscal.EQ.-99.0) then
               corscal = 1
               message = 'file for CORRSCAL keywrd '//
     &              'not found'
               call wtwarm(subname,version,chatter,0,message)
               message = 
     &              'CORRSCAL keywrd set to '//
     &              'unity in o/p file'
               call wtinfo(chatter,0,1,message)
            endif
         else
            if(corscal.EQ.-99.0)  corscal = 1
ccc            corscal = in_corscal
     
c ...... case when it's been entered not necessary (performed in main)
         endif
      endif

c ................ Check out the ARF file............................
c ... case when asked to try and work it out for ourselves
      if(arfexp.EQ.'%')then
         if(qokarf) then
            if(chatter.ge.9) then
               if((nfil.GT.1).AND.(in_arffil.NE.'none').AND.
     &              (in_arffil.NE.'none').AND.(in_arffil.NE.' ')) then
                  message = 'ANCRFILE keyword in o/p file copied '//
     &                 'from i/p files (all agree)'
                  call wtinfo(chatter,0,1,message)
               endif
               arffil = in_arffil
            endif
         else
            arffil = 'NONE'
            message = 'ANCRFILE keywrds from '//
     &           'i/p files disagree'
            call wtwarm(subname,version,chatter,0,message)
            message = 
     &           'ANCRFILE keywrd set to '//
     &           'null/default value in o/p file'
            call wtinfo(chatter,0,1,message)
         endif
      else
c     ...... case when it should be taken from a file
         if(qfarf) then
            if(arffil.EQ.' ') then
               arffil = 'NONE'
               message = 'file for ANCRFILE keywrd '//
     &              'not found'
               call wtwarm(subname,version,chatter,0,message)
               message = 
     &              'ANCRFILE keywrd set to '//
     &              'null/default value in o/p file'
               call wtinfo(chatter,0,1,message)
               qokarf = .false.
            endif
c     ...... case when it's been entered not necessary (performed in main)
         endif
      endif

c ................ Check out the RMF file............................
c ... case when asked to try and work it out for ourselves
      if(rmfexp.EQ.'%')then
         if(qokrmf) then
            if(chatter.ge.9) then
               if((nfil.GT.1).AND.(in_rmffil.NE.'none').AND.
     &              (in_rmffil.NE.'none').AND.(in_rmffil.NE.' ')) then
                  message = 'RESPFILE keyword in o/p file copied '//
     &                 'from i/p files (all agree)'
                  call wtinfo(chatter,0,1,message)
               endif
               rmffil = in_rmffil
            endif
         else
            rmffil = 'NONE'
            message = 'RESPFILE keywrds from '//
     &           'i/p files disagree'
            call wtwarm(subname,version,chatter,0,message)
            message = 
     &           'RESPFILE keywrd set to '//
     &           'null/default value in o/p file'
            call wtinfo(chatter,0,1,message)
         endif
      else
c ...... case when it should be taken from a file
         if(qfrmf) then
            if(rmffil.EQ.' ') then
               rmffil = 'NONE'
               message = 'file for RESPFILE keywrd '//
     &              'not found'
               call wtwarm(subname,version,chatter,0,message)
               message = 
     &              'RESPFILE keywrd set to '//
     &              'null/default value in o/p file'
               call wtinfo(chatter,0,1,message)
               qokrmf = .false.
            endif
c ...... case when it's been entered not necessary (performed in main)
         endif
      endif

c For Single i/p PHA file, dump reasuring messages
      if(nfil.EQ.1) then
         message = 'Single file detected in i/p expression '
         call wtinfo(chatter,9,1,message)
         message = 'following keywords in o/p file have '//
     &        'values copied from i/p file:'
         call wtinfo(chatter,9,2,message)
         message = 'TELESCOP INSTRUME DETNAM FILTER '//
     &        'CHANTYPE AREASCAL'
         call wtinfo(chatter,9,2,message)
         message = 'BACKFILE BACKSCAL CORRFIL '//
     &        'CORRSCAL RESPFILE ANCRFILE '
         call wtinfo(chatter,9,2,message)
         tlscop  = rtlscop
         instrum = rinstrum
         filter  = rfilter
         detnam  = rdetnam
         chantyp = rchantyp
         areascal= rareascal
         backfil = rbackfil
         backscal= rbackscal
         corfil       = rcorfil
         corscal = rcorscal
         rmffil       = rrmffil
         arffil       = rarffil
      endif




c Error Trap
 482  if(ierr.ne.0) then
         message = 'Incomplete Execution'
         call wterrm(subname,version,message)
      endif

      return
      end
c -------------------------------------------------------------------------
*+ MAINLOOP
      subroutine mainloop(chatter, ncom, comm, ctype,
     &     maxchan, detchan, 
     &     buf_dat, buf_err, buf_exp,
     &     data, err, expos,
     &     stack_dat, stack_err, stack_exp,
     &     save_dat, save_err, save_exp,
     &     qdiv, qokexp, ierr)
      
      IMPLICIT NONE
      integer ncom, ierr, chatter,maxchan
      integer detchan
      real buf_dat(maxchan,*), buf_err(maxchan,*), buf_exp(*)
      real data(maxchan,*), err(maxchan,*), expos(*)
      real stack_dat(maxchan,*), stack_err(maxchan,*)
      real stack_exp(*), save_dat(*), save_err(*), save_exp
      character*(*) comm(*), ctype(*)
      logical qdiv(*), qokexp

c Description
c       This is the MAIN COMMAND do loop for mathpha. It was extracted from 
c the main in order to allow the introduction of Dynamic Memory Allocation.
c The basic set up here is that we will have 3 sets of arrays
c   There will be:
c       2 arrays to act as the operational buffers (eg BUF_DAT)
c            (o/p from operations always written to buffer 1)
c       1 array to act as the short-term storage buffer (eg SAVE_DAT)
c        n arrays to act as the long-term "stack" storage (eg STACK_DAT)
c   The following control-strings operate on the arrays as follows
c        PUSH - move current operational buffer 1 to stack storage
c       SAVE - move current operational buffer 1 to s-term storage
c       RECALL - move s-term storage to relevant operational buffer
c       TEMP - do nothing regarding storage
c Step through each command filling appropriate arrays etc. The only real 
c tricky bit is to keep the buffer counters updated appropriately.
c The important counters are:
c      IBUF - the next active operational buffer to be used
c      ISTACK - the next active long-term "stack" to be used
c
c Author/Modufication History
c  Ian M George (1.0.0:1994 Mar 10), original
c
c Banashree Mitra Seifert (1.1.0: 1996 March20)
c           . Introduced screen display routine
c Peter D Wilson (1.1.1: 1998 Apr 3)
c           . Initialize qdiv array to .false.
c ------------------------------------------------------------------
      character(8) subname
      parameter (subname='comloop')
      character(7) version
      parameter (version='1.1.1')
*-
c Internals
      integer j, i, clenact
      integer ibuf, istack, ifil, istck
      real value, temp
      character(80) message, reportmsg
      integer undef

      message='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,message)


c ... Initialization
      undef=-999999
      ifil = 0
      ibuf = 1
      istack = 1
      value = 0.0
      ierr = 0
      reportmsg = 'please report this to '//
     &     ' ftoolshelp@athena.gsfc.nasa.gov'
      do i = 1, maxchan
         qdiv(i) = .false.
      end do

c Dump info for diagnostics
      message = 'Stepping through command string:'
      call wtinfo(chatter,30,2,message)
      write(message,'(a,i12)') 'number of commands: ',
     &     ncom
      call wtinfo(chatter,30,2,message)

c *** START of MAIN COMMAND do loop *********************************
      do j = 1, ncom
         message = comm(j)
         call wtinfo(chatter,30,2,message)
c ... ctype = FILE
         if(ctype(j).EQ.'F') then
            ifil = ifil + 1
            do i = 1, detchan
               buf_dat(i,ibuf) = data(i,ifil)
               buf_err(i,ibuf) = err(i,ifil)
            enddo
            buf_exp(ibuf)      = expos(ifil)
            ibuf = ibuf + 1
            
c ... ctype = REAL
         elseif(ctype(j).EQ.'R') then
            read(comm(j),*) value
            do i = 1, detchan
               buf_dat(i,ibuf) = value
               buf_err(i,ibuf) = 0.0
            enddo
            buf_exp(ibuf)      = value
            ibuf = ibuf + 1

c ... ctype = COMMAND
         elseif(ctype(j).EQ.'C') then
            if(comm(j).EQ.'PUSH') then
               do i = 1, detchan
                  stack_dat(i,istack) = buf_dat(i,1) 
                  stack_err(i,istack) = buf_err(i,1) 
                  buf_dat(i,1) = 0.0
                  buf_err(i,1) = 0.0
                  buf_dat(i,2) = 0.0
                  buf_err(i,2) = 0.0
                  save_dat(i) = 0.0
                  save_err(i) = 0.0
               enddo
               stack_exp(istack)      = buf_exp(1)
               buf_exp(1) = 0.0
               buf_exp(2) = 0.0
               istack = istack + 1
               ibuf = 1
            elseif(comm(j).EQ.'TEMP') then
               ibuf = 2
            elseif(comm(j).EQ.'SAVE') then
               do i = 1, detchan
                  save_dat(i) = buf_dat(i,1) 
                  save_err(i) = buf_err(i,1) 
               enddo
               save_exp = buf_exp(1)
               ibuf = 1
            elseif(comm(j).EQ.'RECALL') then
               do i = 1, detchan
                  buf_dat(i,ibuf) = save_dat(i)
                  buf_err(i,ibuf) = save_err(i)
               enddo
               buf_exp(ibuf) = save_exp
               ibuf = ibuf + 1
            elseif(comm(j)(1:1).EQ.'<') then
               read(comm(j)(2:clenact(comm(j))-1),*)
     &              istck
               do i = 1, detchan
                  buf_dat(i, ibuf) = stack_dat(i,istck) 
                  buf_err(i, ibuf) = stack_err(i,istck) 
               enddo
               buf_exp(ibuf) = stack_exp(istck)
               ibuf = ibuf + 1
            else
               message = ' Bug associated with Control string'
               call wterrm(subname,version,message)
               call wtinfo(chatter,0,1,reportmsg)
               ierr = 1
               goto 482
            endif

c ... ctype = OPERATION
         elseif(ctype(j).EQ.'O') then
            if(ibuf.EQ.3) then
               if(comm(j).EQ.'+') then
                  do i = 1, detchan
                     if(buf_dat(i,1).ne.undef.and.
     &                    buf_dat(i,2).ne.undef) then      
                        buf_dat(i,1) = buf_dat(i,1) + buf_dat(i,2)
                        buf_err(i,1) = SQRT(buf_err(i,1)**2.0 
     &                       + buf_err(i,2)**2.0)
                     endif
                  enddo
                  buf_exp(1) = buf_exp(1) + buf_exp(2)
                  ibuf = 2
               elseif(comm(j).EQ.'-') then
c     ... NOTE, this is done back-to-front 
                  do i = 1, detchan
                     if(buf_dat(i,1).ne.undef.and.
     &                    buf_dat(i,2).ne.undef) then      
                        buf_dat(i,1) = buf_dat(i,2) - buf_dat(i,1)
                        buf_err(i,1) = SQRT(buf_err(i,1)**2.0 
     &                       + buf_err(i,2)**2.0)
                     endif
                  enddo
                  buf_exp(1) = buf_exp(2) - buf_exp(1)
                  ibuf = 2
               elseif(comm(j).EQ.'*') then
                  do i = 1, detchan
                     if(buf_dat(i,1).ne.undef.and.
     &                    buf_dat(i,2).ne.undef) then      
                        if(buf_dat(i,1).NE.0) then
                           temp = (buf_err(i,1)/buf_dat(i,1))**2.0
                        else 
                           temp = 0.0
                        endif
                        if (buf_dat(i,2).NE.0) then
                           buf_err(i,1) = SQRT(temp + 
     &                          (buf_err(i,2)/buf_dat(i,2))**2.0)
                        else 
                           buf_err(i,1) = SQRT(temp)
                        endif
                        buf_dat(i,1) = buf_dat(i,1) * buf_dat(i,2)
                        buf_err(i,1) = buf_err(i,1) * buf_dat(i,1)
                     endif
                  enddo
                  buf_exp(1) = buf_exp(1) * buf_exp(2)
                  ibuf = 2
               elseif(comm(j).EQ.'/') then
                  do i = 1, detchan
                     if(buf_dat(i,1).ne.undef.and.
     &                    buf_dat(i,2).ne.undef) then      
                        if (buf_dat(i,2).NE.0.0) then
                           if (buf_dat(i,1).NE.0) then
                              temp = (buf_err(i,1)/buf_dat(i,1))**2.0
                           else 
                              temp = 0.0
                           endif
                           buf_err(i,1) = SQRT(temp + 
     &                          (buf_err(i,2)/buf_dat(i,2))**2.0)
                           buf_dat(i,1) = buf_dat(i,1) / buf_dat(i,2)
                           buf_err(i,1) = buf_err(i,1) * buf_dat(i,1)
                        else
                           qdiv(i) = .true.
                           buf_dat(i,1) = 0.0
                           buf_err(i,1) = 0.0
                        endif
                     endif
                  enddo
                  if (buf_exp(2).NE.0) then
                     buf_exp(1) = buf_exp(1) /buf_exp(2)
                  else
                     qokexp = .false.
                     buf_exp(1) = 0.0
                  endif
                  ibuf = 2
               else
                  message = ' Unknown operator: '//
     &                 comm(j)(1:MIN(50,clenact(comm(j))))
                  call wterrm(subname,version,message)
                  ierr = 1
                  goto 482
               endif
            else
               message = ' Bug associated with ibuf'
               call wterrm(subname,version,message)
               call wtinfo(chatter,0,1,reportmsg)
               ierr = 1
               goto 482
            endif
            
c ... ctype = UNKNOWN
         else
            message = ' Unknown command-type'//ctype(j)
            call wterrm(subname,version,message)
            ierr = 1
            goto 482
         endif
      enddo
c *** END of MAIN COMMAND do loop
      message = 'end of command string'
      call wtinfo(chatter,30,2,message)


c Error trap
 482  if(ierr.ne.0) then
         message = 'Incomplete Execution'
         call wterrm(subname,version,message)
      endif

      return
      end
c -------------------------------------------------------------------------

*+ FILOP
      subroutine filop(chatter, maxchan, detchan, alg_units,
     &     qdiv, divzero,
     &     pha, error, qual, buf_dat, buf_err, buf_exp,
     &     grping, syserr, channel, fchan,
     &     qcalexp, qokexp, texpos, qok, qpos,errmeth,ierr)
      
      IMPLICIT NONE
      integer chatter, detchan, maxchan, fchan, ierr
      integer qual(*), grping(*), channel(*)
      character*(*) alg_units,errmeth
      real divzero, texpos
      real pha(*), error(*), syserr(*)
      real buf_dat(maxchan,*), buf_err(maxchan,*), buf_exp(*)
      logical qdiv(*), qcalexp, qokexp, qok, qpos(*)

c Description
c  The subroutine populates the o/p arrays, putting in the divide-by-zero 
c value into all array (channels) for which it is neccessary. 
c This hunk of code was removed from the main in order to allow the 
c addition of Dynamic Memory Allocation.
c
c Author/Modification History
c  Ian M George     (1.0.0:1994 Mar 10), original
c  Ian M George     (2.0.0:1994 Apr 21), added units parameter
c
c Banashree Mitra Seifert(2.1.0:1996 Mar20)
c       . Introduced screen display routine
c ---------------------------------------------------------------
      character(6) subname
      parameter (subname='filop')
      character(7) version
      parameter (version = '2.1.0')
*-
c Internals
      integer i, j
      character(80) message

      message='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,message)

c Initialize
      ierr = 0

      if(qcalexp) then
         if(.NOT.qokexp) then
            texpos = 1
            message = 'Problem calculating exposure'
            call wtwarm(subname,version,chatter,0,message)
            message = 'exposure set to 1 second'
            call wtinfo(chatter,0,1,message)
         else
            texpos = buf_exp(1)
         endif
      endif

      do i = 1, detchan

         if(qdiv(i)) then
            pha(i) = divzero
            error(i) = divzero
            qual(i) = 5
         else
c           if(alg_units.EQ.'C') then
c            pha(i) = buf_dat(i,1)/texpos
c            error(i) = buf_err(i,1)/texpos
c           else
            pha(i) = buf_dat(i,1)
            error(i) = buf_err(i,1)
c           endif
         endif
         grping(i) = 0
         syserr(i) = 0.0
         channel(i) = i - 1 + fchan
      enddo
      

c ... Write out info concerning channels which either suffered a 
c     divide-by-zero, or for which the errors are not handled correctly
c     Recycling arrays & variables here
c ... Poisson errors
      j = 0
      qok = .false.
      do i = 1, detchan
         if(.NOT.qok) then
            if(qpos(i)) then
               j = j + 1
               buf_dat(j,1) = i
               qok = .true.
            endif
         else
            if(.NOT.qpos(i)) then
               buf_dat(j,2) = i-1
               qok = .false.
            elseif(i.EQ.detchan)then
               buf_dat(j,2) = detchan
            endif
         endif      
      enddo
      if( (j.GT.0).AND.(errmeth(1:7).NE.'POISS-0') ) then 
         message=' '
         call wtwarm(subname,version,chatter,0,message)
         message = 'Poissonian errors with N<20 assumed '
     &        //' for: '
         call wtinfo(chatter,0,1,message)
         do i = 1, j
            write(message,'(a,f6.0,a,f6.0)') 
     &           'channels ', buf_dat(i,1), '-', buf_dat(i,2)
            call wtinfo(chatter,0,2,message)
         enddo
      endif

c Divide by zero
      j = 0
      qok = .false.
      do i = 1, detchan
         if(.NOT.qok) then
            if(qdiv(i)) then
               j = j + 1
               buf_dat(j,1) = i
               qok = .true.
            endif
         else
            if(.NOT.qdiv(i)) then
               buf_dat(j,2) = i-1
               qok = .false.
            elseif(i.EQ.detchan)then
               buf_dat(j,2) = detchan
            endif
         endif      
      enddo
      if(j.GT.0) then
         message =' '
         call wtwarm(subname,version,chatter,0,message)
         message = 'Divide-by-zero encountered for: '
         call wtinfo(chatter,0,1,message)
         do i = 1, j
            write(message,'(a,f6.0,a,f6.0)') 
     &           'channels ', buf_dat(i,1), '-', buf_dat(i,2)
            call wtinfo(chatter,0,2,message)
         enddo
         message = 'These channels have been set bad, and '
     &        //' value DIVZERO inserted'
         call wtinfo(chatter,0,1,message)
      endif
      
c Error trap
      if(ierr.ne.0) then
         message = 'Incomplete Execution'
         call wterrm(subname,version,message)
      endif
      
      return
      end
c -------------------------------------------------------------------------
*+CHKPHA_MATH
      subroutine chkpha_math(chatter,phaexp,detchans,ierr)

      IMPLICIT NONE
      integer ierr, chatter, detchans
      character*(*) phaexp

c Description
c  Opens a PHA file, finds the correct/specified extension, and reads the 
c max number of PHA channels for the instrument/detector (from the DETCHANS
c keyword.
c  Note the file is closed again on completion
c
c Passed Parameters
c  CHATTER          i   : Chattiness flag - o/p only if >40
c  incomplete
c  IERR               o : Error flag (zero if everything OK)
c 
c Called Routines
c  subroutine CGETLUN   : (CALLIB) Gets a free logical unit
c  subroutine FCECHO      : (FTOOLS) Writes to standard o/p
c  subroutine FCPARS    : (FTOOLS) Parses filename[extn#] string
c  subroutine FNDEXT      : (CALLIB) Finds xtens based on EXTNAME value
c  subroutine FNDHDU      : (CALLIB) Finds xtens based on HDUCLASn values
c  subroutine FTMAHD      : (FITSIO) Move to an absolute xtens no.
c  subroutine FTMRHD      : (FITSIO) Move a relative no. xtens rel to CDU
c  subroutine FTOPEN      : (FITSIO) Open a FITS file
c  subroutine RDPHA1    : (CALLIB) reads a PHA dataset
c  subroutine WT_FERRMSG: (CALLIB) Writes standard FITSIO error message
c
c Author/Modification History
c  Ian M George (1.0.0:94 Dec 12) Original
c
c Banashree Mitra Seifert (1.1.0:1996 March 20)
c         . Introduced screen display routines
c Peter D Wilson (1.1.1:1998 Sept 4)
c         . Initialize status to 0
c -----------------------------------------------------------------
      character(11) subname
      parameter (subname='chkpha_math')
      character(7) version
      parameter (version='1.1.1')
*-
c Max arrays
      integer maxextn
      parameter (maxextn=99)
c Internals 
      integer i, jj, kk, imove, clenact
      integer extn, iunit, htype, status, block
      integer ninstr, nsearch, nfound
      integer next(maxextn)
      character(20) outhdu(9,maxextn), outver(9,maxextn)
      character(20) extnam(maxextn)
      character(20) instr(9)
      character(30) comm
      character(128) message
      character(80) phafil

c Initialize
      ierr = 0
      extn = 0
      status = 0

c ... give user info if requested
      message = 'using '//subname//'Ver '// version
      call wtinfo(chatter,10,2,message)

c Parse the supplied filenames, stripping off incld extension numbers
      call fcpars(phaexp,phafil,extn,status)
      if(status.NE.0) then
         message = 'Problem parsing the expression:'
         call wtwarm(subname,version,chatter,0,message)
         message = phaexp(:MIN(50,clenact(phaexp)))
         call wtinfo(chatter,0,2,message)
         message = 'will search all extensions'
         call wtinfo(chatter,0,2,message)
         extn = -99
      endif

c Open i/p file
      status = 0
      call cgetlun(iunit)
      call ftopen(iunit,phafil,0,block,status)
      IF (status.NE.0) THEN
         message = ' opening file: '//phafil(:20)
         call wtferr(subname,version,status,message)
         ierr = 1
         return
      ENDIF

c Find the SPECTRUM extension in the PHA file 
c     - Extension number NOT given as part of phaexp (search for HDUCLAS/EXTNAM)
      if(extn.LT.0) then
         ninstr = 1
         instr(1) = 'SPECTRUM'
         nsearch = maxextn
         call fndhdu(chatter, iunit, ninstr, instr,
     &        nsearch, nfound, next, outhdu, outver, extnam, ierr)
c         ... check for old-style EXTNAME values if no OK HDUCLASn values found
         if(nfound.LE.0) then
            message = 
     &           ' Ext w/ allowed HDUCLASn keywrds not found'
            call wtwarm(subname,version,chatter,0,message)
            message = 'offending file: '//phafil
            call wtinfo(chatter,0,1,message)
            message =
     &           'searching for extension with EXTNAME = SPECTRUM'
            call wtinfo(chatter,0,1,message)
            call fndext(chatter, iunit, 'SPECTRUM',
     &           nsearch, nfound, next, outhdu, outver, extnam, ierr)
            if(nfound.EQ.1) then
               message = 'located acceptable extension'
               call wtinfo(chatter,0,1,message)
            endif
         endif
c     - Extension number IS given as part of phafil 
      else
         call ftmahd(iunit,extn+1,htype,status)
         message = 'Problem moving to specified xtens'
         call wtfwrn(subname,version,chatter,0,status, message)
c     ... grab the HDUCLAS values for reference
         ninstr = 1
         instr(1) = '*'
         nsearch = 1
         call fndhdu(MIN(chatter,20), iunit, ninstr, instr,
     &        nsearch, nfound, next, outhdu, outver, extnam, ierr)
         nfound = 1
         next(1) = 0
      endif

c - sort out what we've got
 369  if(nfound.GT.1) then
         do i = 1, nfound
            if(outhdu(2,i) .EQ.'DETECTOR') then
               if(i.NE.nfound) then
                  do kk = i,nfound
                     do jj = 1, 3
                        outhdu(jj,kk) = outhdu(jj,kk+1)
                     enddo
                     next(kk) = next(kk+1)      
                     extnam(kk) = extnam(kk+1)
                  enddo              
               endif
               nfound = nfound - 1
               goto 369
            endif
         enddo
         message = ' PHA file contains >1 PHA dataset'
         call wtinfo(chatter,0,2,message)
         write(message,'(a,i12,a)') nfound,' extensions found:'
         call wtinfo(chatter,0,1,message)
         do i = 1, nfound
            write(message,'(a,i12,a)') 'Ext ',next(i),':'      
            call wtinfo(chatter,0,2,message)
            write(message,'(6X,a,a)') 'EXTNAME  = ', extnam(i)
            call wtinfo(chatter,0,0,message)
            do jj = 1, 3
               write(message,'(6X,a,i1,2a)') 
     &              'HDUCLAS',jj,' = ',outhdu(jj,i)
               call wtinfo(chatter,0,0,message)
            enddo
         enddo
         message = 
     &        'Extension # must be specified '//
     &        ' via "file[ext#]" in i/p expression'
         call wtinfo(chatter,0,1,message)
         ierr = 2
         goto 482
      elseif(nfound.LE.0) then
         message ='Unable to locate a SPECTRUM extension'
         call wtwarm(subname,version,chatter,0,message)
         ierr = 2
         goto 482
      endif

c Move to the Extension if not already there
      if(next(1).GT.0) then
         imove = next(1)
         status = 0
         call ftmrhd(iunit,imove,htype,status)
         message = 'Problem moving to SPECTRUM xtens'
         call wtfwrn(subname,version,chatter,0,status, message)
      endif

c Read in the PHA data
      call ftgkyj(iunit,'DETCHANS',detchans,comm,status)
      if(status.NE.0) then
         message = ' reading DETCHANS keyword'
         call wtferr(subname,version,status,message)
         ierr = -1
         goto 482
      endif


c Check for errors
 482  if(ierr.ne.0) then
         message =  ' Unable to recover'
         call wterrm(subname,version,message)
      endif
c Close the FITS file
      call ftclos(iunit, status) 

      return
      end

c -------------------------------------------------------------------------
*+PREP_PHA
      subroutine prep_pha(chatter, alg_units, qproperr, errmeth,
     &     detchans, texpos, pha, error,
     &     quality, dtype, qerror, ierr)

      IMPLICIT NONE
      integer ierr, chatter, detchans, dtype
      integer quality(*)
      real pha(*), error(*), texpos
      character*(*) alg_units, errmeth
      logical qproperr, qerror

c Description
c  Sets a bunch of parameters in preparation for writing out the PHA dataset.
c
c Passed Parameters
c  CHATTER          i   : Chattiness flag - o/p only if >40
c  incomplete
c  IERR               o : Error flag (zero if everything OK)
c 
c Called Routines
c
c Author/Modification History
c  Ian M George (1.0.0:95 Aug 01) Original
c
c Banashree Mitra Seifert (1.1.0: 1996 March1996)
c          . Introduced screen display routine
c -------------------------------------------------------------------
      character(9) subname
      parameter (subname='prep_pha')
      character(7) version
      parameter (version='1.0.0')
*-
c Internals
      integer i, icounts
      real calcpois, calcpois2, calcpois3
      real counts
      character(80) message
      logical qoops


      message='using '//subname//' Ver '//version
      call wtinfo(chatter,10,2,message)


c Initialize
      qoops = .false.
      ierr = 0


c Sort out the data type (ie how algebra was performed, and hence the format
c  of the o/p dataset.
      if(alg_units.EQ.'C') then
         dtype = 1
      else
         dtype = 2
      endif

c Set the error flags, and if necessary work out the errors.
c  Errors only need to be calculated here if they have NOT been 
c  propagated, and the o/p is a RATE. In cases where the errors 
c  have been progated, then the errors have already been calculated.
c  In cases where the errors have not been propagated, but in which 
c  the o/p in in COUNTS, then the qerror = .false. is simily set leading 
c  to a POISSERR = .true. keyword in the o/p.
      if(qproperr) then
         qerror = .true.
      elseif((.NOT.qproperr).AND.(dtype.EQ.1)) then
         if(errmeth(1:1).EQ.'G') then
            qerror = .false.
         else
            qerror = .true.
            do i = 1, detchans
               if(errmeth(1:7).EQ.'POISS-1') then
                  Icounts = NINT(pha(i))
                  if(icounts.GE.0.0) then
                     error(i) = calcpois(icounts)
                  else
                     if(quality(i).EQ.0) qoops = .true.
                     error(i) = 0.0
                     quality(i) = 5
                  endif      
               elseif(errmeth(1:7).EQ.'POISS-2') then
                  Icounts = NINT(pha(i))
                  if(icounts.GE.0.0) then
                     error(i) = calcpois2(icounts)
                  else
                     if(quality(i).EQ.0) qoops = .true.
                     error(i) = 0.0
                     quality(i) = 5
                  endif
               elseif(errmeth(1:7).EQ.'POISS-0') then
                  Icounts = NINT(pha(i))
                  if(icounts.GE.0.0) then 
                     error(i) = 0.
                  else
                     if(quality(i).EQ.0) qoops = .true.
                     error(i) = 0. 
                     quality(i) = 5
                  endif      
               elseif(errmeth(1:7).EQ.'POISS-3') then
                  Icounts = NINT(pha(i))
                  if(icounts.GE.0.0) then
                     error(i) = calcpois3(icounts)
                  else
                     if(quality(i).EQ.0) qoops = .true.
                     error(i) = 0.0
                     quality(i) = 5
                  endif      
               else
                  message = 'Unsupported Error Method'
                  call wterrm(subname,version,message)
                  message = 'Illegal value: Errmeth='//errmeth
                  call wtinfo(chatter,0,2,message)
                  ierr = 99
                  goto 482
               endif
            enddo
         endif
      else
         qerror = .true.
         if(texpos.LE.0.0) then
            message =  'Problem calculating errors'
            call wtwarm(subname,version,chatter,0,message)
            message = 'Unable to calc errors due to zero'//
     &           ' or -ve exposure time'
            call wtinfo(chatter,0,1,message)
            message = 'Errors in all channels set to zero'
            call wtinfo(chatter,0,2,message)
            do i = 1, detchans
               error(i) = 0.0
            enddo
            goto 482
         endif
         do i = 1, detchans
            if(errmeth(1:7).EQ.'POISS-0') then
               Icounts = NINT(pha(i)*texpos)
               if(icounts.GE.0.0) then
                  error(i) = 0.
               else
                  if(quality(i).EQ.0) qoops = .true.
                  error(i) = 0.
                  quality(i) = 5
               endif
            elseif(errmeth(1:7).EQ.'POISS-1') then
               Icounts = NINT(pha(i)*texpos)
               if(icounts.GE.0.0) then
                  error(i) = calcpois(icounts)/texpos
               else
                  if(quality(i).EQ.0) qoops = .true.
                  error(i) = 0.0
                  quality(i) = 5
               endif
            elseif(errmeth(1:7).EQ.'POISS-2') then
               Icounts = NINT(pha(i)*texpos)
               if(icounts.GE.0.0) then
                  error(i) = calcpois2(icounts)/texpos
               else
                  if(quality(i).EQ.0) qoops = .true.
                  error(i) = 0.0
                  quality(i) = 5
               endif      
            elseif(errmeth(1:7).EQ.'POISS-3') then
               Icounts = NINT(pha(i)*texpos)
               if(icounts.GE.0.0) then
                  error(i) = calcpois3(icounts)/texpos
               else
                  if(quality(i).EQ.0) qoops = .true.
                  error(i) = 0.0
                  quality(i) = 5
               endif      
            elseif(errmeth(1:1).EQ.'G') then
               Counts = NINT(pha(i)*texpos)
               if(pha(i).GT.0.0) then
                  error(i) = SQRT(counts)/texpos
               elseif(pha(i).EQ.0.0) then
                  error(i) = 0.0
               else
                  if(quality(i).EQ.0) qoops = .true.
                  error(i) = 0.0
                  quality(i) = 5
               endif
            else
               message = 'Unsupported Error Method'
               call wterrm(subname,version,message)
               message = 'Illegal value: Errmeth='//errmeth
               call wtinfo(chatter,0,2,message)
               ierr = 99
               goto 482
            endif
         enddo
      endif

c Warn if problem with error calculation
      if(qoops) then
         message =  ' Problem calculating errors'
         call wtwarm(subname,version,chatter,0,message)
         message = 'One or more channels in o/p file has'//
     &        ' counts incompatible with errmeth'
         call wtinfo(chatter,0,1,message)
         message = '        ** (Suspect you should have'//
     &        ' propagated the errors using properr=yes) **'
         call wtinfo(chatter,0,1,message) 
         message = 'Error in these chans'//
     &        ' set to zero and they will be flagged bad '
         call wtinfo(chatter,0,2,message)
         message = 'These channels will have error'//
     &        ' set to zero, and flagged bad in o/p file'
         call wtinfo(chatter,0,2,message)
      endif


c Check for errors
 482  if(ierr.ne.0) then
         message = 'Unable to recover'
         call wterrm(subname,version,message)
      endif

      return
      end

c -------------------------------------------------------------------------
c                 end of prep_pha
c --------------------------------------------------------------





































