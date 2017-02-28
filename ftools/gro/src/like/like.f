C       PROGRAM LIKE
C
C
C  $Id: like.f,v 1.4 2013/05/21 19:08:26 irby Exp $
C=======================================================================
C++     effect: Set up for likelihood analysis.
C++             Provide menu driven options.
C=======================================================================
C  LIKE Version: 5.0 DELIVERED: October 1st 1994, Programmer J.R. MATTOX
C+            Updated: by JRM
C=======================================================================
C%   Changes:
c	jrm 1/6/94 Add script input
c
c	JAE 8/25/94 Changed nominal values of Gbias_min/max, Gmult_min/max
c		    Restrict_Gbias/Gmult set to true, Gbias/Gmult_nom set
c		    to 150 and 100 respectively for all energies.
C  $Log: like.f,v $
C  Revision 1.4  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.3  2007/01/31 21:04:11  irby
C  Replace use of FITSERROR routine (fitserro.f) with calls to fcerr/fcerrm.
C  The reliance of FITSERROR on the Fortran 'access' function prevents it
C  from compiling with gfortran (in which 'access' is not [yet?] implemented).
C  Also, fix some lines that were too long (>72 chars).
C
C  Revision 1.2  2002/12/26 17:33:17  irby
C  - Change negative exponents to e.g. -1*number instead of -number for f90
C    compatibility (and put in parens (-1) where necessary.
C
C  - Fix write statements (commas).
C
C  Revision 1.1  2002/04/16 20:27:36  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.24  1997/10/02  21:03:49  jae
c Unpdated like.f for WWW users guide.  The routine
c had a few more lines needing editing.
c
c Revision 5.23  1997/10/02  15:30:59  jae
c Changed HTML link to http://lheawww/gsfc.nasa.gov/~jae/like/
c from SUN directory file in EGRET_DOCS.  This makes the on-line
c documentation more accessable.
c
c Revision 5.22  1997/09/16  17:29:48  jae
c Test of makefile.
c Creation of directory structure
c
c Revision 5.21  1997/03/06  21:03:51  jae
c Added command 'LPC' as:
c  if (input(1:3).eq.'LPC')then
c 		calcdl catproca()
c 		goto 100
c  endif
c
c This by-passes call to jlocpos and outputs a catalog
c file and summary
c
c GMAP_DIR, EGRET_DIR, EGRET_PROGRAMS, CALIB_DIR, MISC_DIR and FITS_DIR are now 
c read from like.par.  Deleted call to getenv to get these variable - 
c Sandhia Bansal 12/01
c
c Revision 5.20  1996/11/05  18:52:40  jae
c BUG Repair: Variable PI was not set !
c It is now set to 4*atan(1.0)
c
c Revision 5.19  1996/07/31  18:48:56  jae
c added new routines to namelists
c
c Revision 5.18  1996/07/19  22:52:52  jae
c Repaired typos on lines 1104 and 1111
c
c Revision 5.17  1996/07/19  18:59:01  jae
c Revised code for handeling the PHASEx
c environment variables.
c
c Revision 5.16  1996/07/18  14:58:58  jae
c Added command AO[L] to toggle[display]
c the auto-output flag value
c
c Revision 5.15  1996/06/27  19:56:13  jae
c Added command R(x) to command list
c
c Revision 5.14  1996/06/06  18:29:53  jae
c Changed PSF reset question to occur if
c JTRUE == T else set PSF array to null
c
c Revision 5.13  1996/06/06  17:52:36  jae
c Made sure LPx variables are initialized
c Added user supplied if-endif on resetting
c PSF array to null
c
c Revision 5.12  1996/06/04  17:12:19  jae
c Added variable keepfitsdir (char*100) to
c save FITS_DIR value.
c
c Revision 5.11  1996/05/24  15:41:25  jae
c Added lines to output FITS_DIR to stdout if
c variable jae_like==T.  This is for debug
c purposes
c
c Revision 5.10  1996/04/24  18:02:06  jae
c Moved varable 'script_on' to locpos.copy
c as a common block variable
c
c Revision 5.9  1996/04/08  16:19:16  jae
c Added lines to fix MISC_DIR array to end
c with a slash
c
c Revision 5.8  1996/04/08  16:13:06  jae
c Changed INFO_CMD to use EGRET_DOC
c
c Revision 5.7  1996/03/22  22:28:54  jae
c repaired ctlorg and ctlend to reflect ctlscl/2
c offset for ceter bins.
c
c Revision 5.6  1996/03/06  22:21:20  jae
c syntax bugs fixed
c
c Revision 5.5  1996/03/06  21:27:44  jae
c Updated code: FITS_DIR read in with GETENV only
c once at program start in like.f
c
c Revision 5.4  1996/03/03  23:00:31  jae
c Changed input of version
c from cnfrep to cerrep t
c
c Revision 5.3  1996/02/29  21:08:03  jae
c no changes done
c
c Revision 5.2  1996/02/29  20:51:37  jae
c *** empty log message ***
c
c Revision 5.1  1996/02/27  23:09:33  jae
c Removed setting the VER variable from like.f to a
c COMMON block data statement (VER=5.00m) jae
c
c Revision 5.0  1996/02/14  22:45:20  jae
c Subroutine Module for like V5.00
c
c
c
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE LIKE
C     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/nmprep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/locpos.copy'
      INCLUDE  '../COMMON/verrep.copy'

      save

      namelist /USERIN/jae_debug,jae_allfit,jae_amoebaj,
     &     jae_auto_out,jae_basis_funcs,jae_biastest,jae_catproc,jae_cbgaus,
     &     jae_celgal,jae_celgald,jae_chood,jae_cnfcal,jae_cntsbias,
     &     jae_cnttest,jae_command,jae_cospsf,jae_ctlred,jae_datared,
     &     jae_dbrent,jae_derivative2,jae_docadd,jae_egrpsf,jae_errmap,
     &     jae_error,jae_exp_anal,jae_fiterr,jae_fitred,jae_fitrit,
     &     jae_fitserror,jae_fknif,jae_func_limit,jae_funkj,jae_gasbad,
     &     jae_gasbias,jae_gascnts,jae_gastest,jae_get_flux,jae_getsrcnm,
     &     jae_golden,jae_gtcirc,jae_hdrred,jae_hood,jae_iauname,jae_info,
     &     jae_input_id_pos,jae_intent,jae_invert,jae_jfind_iloc,
     &     jae_jfnderr1,jae_jfnderr2,jae_jfndpos,jae_jfndposa,
     &     jae_jgetsystem,jae_jladj,jae_jlistproc,jae_jloc_pos,
     &     jae_jmapfine,jae_jopt_pos,jae_jpsfinfo,jae_jreport,jae_jwrmap,
     &     jae_l_check,jae_lhood,jae_like,jae_liktot,jae_limit,
     &     jae_listproc,jae_lubksb
c
      namelist /USERIN2/jae_ludcmp,jae_mapadd,jae_mapcnv,
     &     jae_mapcor,jae_mapcpy,jae_mapcpyroi,jae_mapdiv,jae_mapdivroi,
     &     jae_maperr,jae_mapfine,jae_maphigh,jae_mapinc,jae_mapixl,
     &     jae_mapmax,jae_mapmlt,jae_mapmult,jae_mapred,jae_maprit,
     &     jae_mapritroi,jae_maprst,jae_mapsrc,jae_mapsrcj,jae_mapsub,
     &     jae_mapsum,jae_mapval,jae_multproc,jae_net_info,
     &     jae_nextsrc,jae_not_privilaged,jae_omap,jae_optimize,
     &     jae_optimum,jae_pixel_select,jae_profile,jae_psf,jae_psf_rep,
     &     jae_psf_scal,jae_psfadd,jae_psfadj,jae_psfbld,jae_psfremove,
     &     jae_psfreplace,jae_psfval,jae_psmcor,jae_psmget,jae_psmmat,
     &     jae_readcat,jae_realcomp,jae_residue,jae_residuej,jae_roiadj,
     &     jae_roiset,jae_rosatpsf,jae_saspsf,jae_set_amat,jae_set_debug,
     &     jae_setup,jae_srcounts,jae_srctest,jae_svbksb,jae_svdcmp,
     &     jae_svdfit,jae_swapchar,jae_swapreal,jae_totmap,jae_to_upper,
     &     jae_valmap,jae_jputenv_,jae_jsetenv_,tttflg

      character(80)  id
      common /id/id
      REAL          MAPVAL
      logical       pntrue
      integer       Cnts_ENERGY(2,20) !Upper and lower energy boundries
      character     last_fits_dir*150
      character     input*50,GMAPFILE_original*80,ex_dir*80,moreinput*80
      character     script_name*50
      character     NAMELST1*100
      character     fits_dir*80
      logical       nmlst,iired
      logical       longshift,determined,FEXIST
c     external myhandler
      integer       Elow_nom(18),Ehigh_nom(18),G_b_nom(18), 
     &     G_m_nom(18),R_nom(18)
c     integer       access
      integer       status
c     nominal analysis parameters
      data Elow_nom             !lower edge of energy range
     &     /30,50, 70,100,150,300, 500,1000,2000,10000, 30,100, 300,
     &     100,  300, 1000, 4000, 
     &     0/
      data Ehigh_nom            !high edge of energy range
     &     /50,70,100,150,300,500,1000,2000,4000,99999,100,300,1000, !exact upper E
     &     10000,10000,10000,10000, !these are minimum upper energy
     &     0/                   !value for energy ranges not in table
      data G_m_nom              !100*nominal value of Gmult
     &     /50,80, 80, 20, 60, 20,  20,  10,   5,    2,100, 60, 30,
     &     100,30,10,3,
     &     100/
      data G_b_nom              !100*nominal value of Gbias
     &     /49,41, 43, 33, 33, 13,  10,   5,   3,    1,180, 67, 23,
     &     100,33,10,3,
     &     100/
c     data G_nom !100*nominal value of Gmult & Gbias V4.9
c     &  /50,80, 80, 20, 60, 20,  20,  10,   5,    2,100, 60, 30,
c     &   100,30,10,3,
c     &   100/
      data R_nom                !nominal value of Ranal
     &     /30,28, 25, 21, 19, 17,  15,  15,  15,  15, 28, 20,  15,
     &     15,13,13,15,
     &     15/
      external jputenv

 1    id = '$Id: like.f,v 1.4 2013/05/21 19:08:26 irby Exp $'

      LOC='LIKE5   '
      PRG='LIKE5  '
      input='                    '
      egret_psf=.true.
      PI = 4.0*ATAN(1.0)
      top1=.false.
      pntrue=.false.
      nmlst=.false.
      catalog=.true.
      J=GETCWD(PWD1)
      numcar = index(PWD1, ' ')

      if (numcar.ge.2.and.PWD1(numcar-1:numcar-1).ne.'/')
     & 	   PWD1(numcar:numcar)='/'

      NAMELST1='jaenamelist.add'
      INQUIRE (FILE=NAMELST1,EXIST=FEXIST)
 2    if (FEXIST) then
         open(43,file=NAMELST1,err=999)
         read(43,USERIN,err=999)
         read(43,USERIN2,err=999)
         close(unit=43)
         nmlst=.true.
         call set_debug(nmlst)
      else
         call set_debug(nmlst)
      endif
c
c     read the input parameter file  sb - 12/01
c
      status = 0
      call read_input(input_ranal, clobber, status)
      if (status .ne. 0) return
c
c     remove nulls from strings read by the C function read_input
c
      call removeNull(cmapfile)
      call removeNull(emapfile)
      call removeNull(gmapfile)
      call removeNull(lmapfile)
      call removeNull(gbiasfile)
c
      n = index(MISC_DIR, ' ') - 1
      if (n.eq.0) n=1
      if (MISC_DIR(n:n).ne.'/') then
         n=n+1
         MISC_DIR(n:n)='/'
      endif
c
      n = index(EGRET_DOC, ' ') - 1
      if (EGRET_DOC(n:n).ne.'/') then
         n=n+1
         EGRET_DOC(n:n)='/'
      endif

      input='      '
c     
 201  numcar = index(PHASE(1), ' ') - 1
      if (numcar.le.0) then
         numcar=1
         PHASE(1)=PWD1
      elseif (numcar.ge.1.and.PHASE(1)(numcar:numcar).ne.'/') then
         PHASE(1)(numcar+1:numcar+1)='/'
      endif

      numcar = index(PHASE(2), ' ') - 1
      if (numcar.le.0) then
         numcar=1
         PHASE(2)='   '
      elseif (numcar.ge.1.and.PHASE(2)(numcar:numcar).ne.'/') then
         PHASE(2)(numcar+1:numcar+1)='/'
      endif

      numcar = index(PHASE(3), ' ') - 1
      if (numcar.le.0) then
         numcar=1
         PHASE(3)='   '
      elseif (numcar.ge.1.and.PHASE(3)(numcar:numcar).ne.'/') then
         PHASE(3)(numcar+1:numcar+1)='/'
      endif

      numcar = index(PHASE(4), ' ') - 1
      if (numcar.le.0) then
         numcar=1
         PHASE(4)='   '
      elseif (numcar.ge.1.and.PHASE(4)(numcar:numcar).ne.'/') then
         PHASE(4)(numcar+1:numcar+1)='/'
      endif

      numcar = index(PHASE(5), ' ') - 1
      if (numcar.le.0) then
         numcar=1
         PHASE(5)='   '
      elseif (numcar.ge.1.and.PHASE(5)(numcar:numcar).ne.'/') then
         PHASE(5)(numcar+1:numcar+1)='/'
      endif

      numcar = index(PHASE(6), ' ') - 1
      if (numcar.le.0) then
         numcar=1
         PHASE(6)='   '
      elseif (numcar.ge.1.and.PHASE(6)(numcar:numcar).ne.'/') then
         PHASE(6)(numcar+1:numcar+1)='/'
      endif
      
      numcar = index(PHASE(7), ' ') - 1
      if (numcar.le.0) then
         numcar=1
         PHASE(7)='   '
      elseif (numcar.ge.1.and.PHASE(7)(numcar:numcar).ne.'/') then
         PHASE(7)(numcar+1:numcar+1)='/'
      endif

      numcar = index(PHASE(8), ' ') - 1
      if (numcar.le.0) then
         numcar=1
         PHASE(8)='   '
      elseif (numcar.ge.1.and.PHASE(8)(numcar:numcar).ne.'/') then
         PHASE(8)(numcar+1:numcar+1)='/'
      endif

      numcar = index(PHASE(9), ' ') - 1
      if (numcar.le.0) then
         numcar=1
         PHASE(9)='   '
      elseif (numcar.ge.1.and.PHASE(9)(numcar:numcar).ne.'/') then
         PHASE(9)(numcar+1:numcar+1)='/'
      endif

      numcar = index(PHASE(10), ' ') - 1
      if (numcar.le.0) then
         numcar=1
         PHASE(10)='   '
      elseif (numcar.ge.1.and.PHASE(10)(numcar:numcar).ne.'/') then
         PHASE(10)(numcar+1:numcar+1)='/'
      endif

      numcar = index(PHASE(11), ' ') - 1
      if (numcar.le.0) then
         numcar=1
         PHASE(11)='   '
      elseif (numcar.ge.1.and.PHASE(11)(numcar:numcar).ne.'/') then
         PHASE(11)(numcar+1:numcar+1)='/'
      endif
c
c     output environment while in debug mode
c
      if (jae_like) then
         numcar = index(PWD1, ' ') - 1
         write(*,'("PWD1:",A)')PWD1(1:numcar)
         
         numcar = index(PHASE(1), ' ') - 1
         write(*,'("PHASE0:",A)')PHASE(1)(1:numcar)

         numcar = index(PHASE(2), ' ') - 1
         write(*,'("PHASE1:",A)')PHASE(2)(1:numcar)

         numcar = index(PHASE(3), ' ') - 1
         write(*,'("PHASE2:",A)')PHASE(3)(1:numcar)

         numcar = index(PHASE(4), ' ') - 1
         write(*,'("PHASE3:",A)')PHASE(4)(1:numcar)

         numcar = index(PHASE(5), ' ') - 1
         write(*,'("PHASE4:",A)')PHASE(5)(1:numcar)

         numcar = index(PHASE(6), ' ') - 1
         write(*,'("PHASE5:",A)')PHASE(6)(1:numcar)

         numcar = index(PHASE(7), ' ') - 1
         write(*,'("PHASE6:",A)')PHASE(7)(1:numcar)

         numcar = index(PHASE(8), ' ') - 1
         write(*,'("PHASE7:",A)')PHASE(8)(1:numcar)
         
         numcar = index(PHASE(9), ' ') - 1
         write(*,'("PHASE8:",A)')PHASE(9)(1:numcar)

         numcar = index(PHASE(10), ' ') - 1
         write(*,'("PHASE9:",A)')PHASE(10)(1:numcar)

         numcar = index(PHASE(11), ' ') - 1
         write(*,'("PHASE10:",A)')PHASE(11)(1:numcar)
         
         numcar = index(MISC_DIR, ' ') - 1
         write(*,'("MISC_DIR:",A)')MISC_DIR(1:numcar)

         numcar = index(EGRET_DOC, ' ') - 1
         write(*,'("EGRET_DOC:",A)')EGRET_DOC(1:numcar)
      endif

      if (input(1:2).eq.'AP') goto 100
c
c+++
c     Change: 8/26/94 JAE - Set restriction on Gmult and Gbias at program 
c     initialization only set Gmult(bias)_nomj values to 100 and 150 Resp.
c
 3    Gbias_normj=150./((100.**(-1.*0.1))-(99999.**(-1.*0.1)))
      Gmult_nomj=100.
      Gbias_nomj=150.
      Restrict_Gmult=.true.
      Restrict_Gbias=.true.
      Restrict_notice=.true.
c
      fits_dir = data_dir
      
      n = index(FITS_DIR, ' ') - 1
      if (FITS_DIR(n:n).eq.'/') then
         FITS_DIR(n:n)=' '
         n=n-1
      endif

      if (n.le.0) n=1

      keepfitsdir=FITS_DIR(1:n)//'   '
      if (jae_like) write(*,'("3 FITS_DIR->",a)') FITS_DIR(1:n)
      n=index(INFO_CMD,'/  ')-2
      if (n.le.0) n=1
      if (jae_like) write(*,'("INFO_CMD->",a)') INFO_CMD(1:n)
      
csb-02/26	CALL GETENV('NO_RES_G',input)

      input = 'TRUE'

      if (input(1:4).eq.'TRUE'.or.input(1:4).eq.'true') then
         Restrict_Gmult=.false.
         Restrict_Gbias=.false.
         Restrict_notice=.false.
      endif

30001 n = index(FITS_DIR, ' ') -1 
      if (n.lt.1) n=1
      
      if (FITS_DIR(n:n).eq.'/'.and.n.gt.1) then
         FITS_DIR(n:n)=' '
      endif

      if (jae_like) write(*,'("30001 FITS_DIR->",a)') FITS_DIR(1:n)
      if (pntrue.or.top1) goto 51
 5    if (.not.pntrue) write(lu(1),'(a,"- version ",a)') PRG,VER

      top1=.false.
      if (pntrue) then
         pntrue=.false.
         goto 100
      endif

      initial=.true.
      longshift=.false.
      lshift=0.
      bshift=0.
      iired=.false.
 51   if (jae_like) print *,LOC,' iired:',iired,'  pntrue:',pntrue

      CALL SETUP(iired)

      top1=.false.
      if (SIGNAL.eq.'P'.and.SIGMSG.eq.'FD') top1=.true.
      if (jae_like) then
         n = index(FITS_DIR, ' ') - 1
         if (n.le.0) n=1
         write(*,'("51-a FITS_DIR->",a)') FITS_DIR(1:n)
      endif

      if (SIGNAL.eq.'1') return
      if (SIGNAL.eq.'2') then
         iired=.true.
         goto 51
      endif

      if (top1) then
         input='FD'
         if (jae_like) then
            print *,' '
            print *,LOC
            print *,'SIGNAL:',SIGNAL,' MSG:',SIGMSG(1:2)
            print *,' '
         endif
         
         if (SIGNAL.eq.'P'.and.SIGMSG.eq.'FD') then
            pntrue=.false.
            SIGNAL=' '
            SIGMSG='      '
         endif

         if (pntrue) top1=.false.
         
         if (jae_like) then
            print *,' '
            print *,LOC
            print *,'iired:',iired,' pntrue:',pntrue
            print *,' '
         endif

         if (jae_like) print *,'input:',input(1:5)
         goto 104
      endif

      CALL ERROR(1,LOC)
      if (jae_like) write(*,'("51-b  FITS_DIR->",a)') FITS_DIR(1:n)
      iired=.false.

 10   continue
c
c     Read counts map
c
      MAPTYP='CMAP'

      MAPFILE=MAPFILE_C

      if (jae_like) then
         n = index(CMAPFILE, ' ')
         m = index(MAPFILE, ' ') 
         write(6,'("CMAPFILE:",a)') CMAPFILE(1:n)
         write(6,'("MAPFILE:",a)') MAPFILE(1:m)
      endif

      CALL MAPRED(MAP,MAPTYP,MAPDOC,.false.) ! false ->ctl parms come from map
      PSMSCL = CTLSCL
      PSFSCL = PSMSCL

      IF (SIGNAL.NE.' ') then
         if (.not.initial.and.signal.eq.'N') then !file doesn't exist
            CALL ERROR(0,LOC)
            cmapfile='ask'
            goto 10             ! fat fingers.
         else
            CALL ERROR(1,LOC)
            cmapfile='ask    '
            goto 10
         endif
      endif

      do n=1,Nfits_energy
         do m=1,2
            Cnts_ENERGY(m,n)=energy(m,n)
         enddo
      enddo

 15   continue 
c
c     set nominal analysis parameters
c
      do i=1,13
         if (Elow_nom(i).eq.CTLEMN.and.
     &        Ehigh_nom(i).eq.CTLEMX) then
            Gmult_nom=float(G_m_nom(i))
            Gbias_nom=float(G_b_nom(i))
            Ranal=float(R_nom(i))
            goto 16 
         endif
      enddo

      do i=14,17
         if (Elow_nom(i).eq.CTLEMN.and.
     &        CTLEMX.ge.Ehigh_nom(i)) then
            Gmult_nom=float(G_m_nom(i))
            Gbias_nom=float(G_b_nom(i))
            Ranal=float(R_nom(i))
            goto 16 
         endif
      enddo
c
c     not in table of energies - use last values
c
      Gmult_nom=float(G_m_nom(i))
      Gbias_nom=float(G_b_nom(i))
      Ranal=float(R_nom(i))
 16   continue
c
c     Read Emap
c
      Gmult_nom=100
      Gbias_nom=Gbias_normj*((CTLEMN**(-1.*0.1))-(CTLEMX**(-1.*0.1)))
      Gbias_nomj=Gbias_nom 
      Gmult_nom_sav=Gmult_nom
      Gbias_nom_sav=Gbias_nom
      gmap_E_dif=.false.

      if (EMAPFILE.eq.'unity') then
         write(6,*)'Setting all pixels in Emap to 1.'
         EMPTYP='EMAP'
         EMPDOC(1)='EMAP set to unity.'
         
         do i=2,10
            EMPDOC(i)=' '
         enddo

         call MAPRST(emap,CTLMSZ1,CTLMSZ2)
         call MAPADD(1.,emap,CTLMSZ1,CTLMSZ2)
         goto 30
      endif

      MAPFILE=MAPFILE_E
      EMPTYP='EMAP'
      
      CALL MAPRED(EMAP,EMPTYP,EMPDOC,.true.)
      CALL ERROR(1,LOC)

 30   continue
c
c     provide for combining maps - addmap usually used
c
      input=' '
      IF (input.ne.' ') then
c     read more data

         MAPFILE=input
         TMPTYP='CMAP'
         CALL MAPRED(tmpMAP,tmpTYP,tmpDOC,.true.)
         
         IF (SIGNAL.NE.' ') then
            if (signal.eq.'E') then !file doesn't exist
               CALL ERROR(0,LOC)
               goto 30          ! fat fingers.
            else
               CALL ERROR(1,LOC)
            endif
         endif

         call mapsum(map,tmpmap,CTLMSZ1,CTLMSZ2)
         
         do i=1,70
            if (MAPFILE(i:i+5).eq.'counts') goto 40
         enddo
         
         write(lu(1),'("Counts map file,",a," not in standard form.",
     &        "Dont know where to look for exposure.")') MAPFILE
         return

 40      MAPFILE=MAPFILE(1:i-1)//'exposr'//MAPFILE(i+6:80)
         TMPTYP='EMAP'
         CALL MAPRED(tmpMAP,tmpTYP,tmpDOC,.true.)
         CALL ERROR(1,LOC)
         call mapsum(Emap,tmpmap,CTLMSZ1,CTLMSZ2)
         write(input,'("Added image ",A11,",")')MAPFILE(8:18)
         nchar=24
         call DOCADD(nchar,MAPDOC,MAPTYP,input)
         call DOCADD(nchar,EMPDOC,EMPTYP,input)
         goto 30
      endif
C
C     Verify that all  pixels with counts are exposed:
C
      DO  I1 = 1,CTLMSZ1
         DO  I2 = 1,CTLMSZ2
            IF (MAPVAL(MAP,I1,I2,CTLMSZ1,CTLMSZ2).GT.1.E-3) THEN
               IF (MAPVAL(EMAP,I1,I2,CTLMSZ1,CTLMSZ2).LT.1.E-3) THEN
                  CALL MAPCOR(I1,I2,GL,GB)
                  write(6,'("EMAP pixel at",2f6.1,
     &                 " empty! Exposure map wrong. ",
     &                 "Counts set to zero.")')GL,GB
                  call valmap(0.,MAP,I1,I2,CTLMSZ1,CTLMSZ2)
               ENDIF
            ENDIF
         ENDDO
      ENDDO

      IF (CTLORG(1).LT.0.0.AND.CTLEND(1).LT.0.0) THEN
         write(lu(1),'("The longitudes of both edges of the ",
     &        "MAPGEN map are < 0.  Adding 360 degrees ",
     &        "to longitudes.")')
         oldCTLORG1=CTLORG(1)
         oldCTLEND1=CTLEND(1)
         CTLORG(1)=360.+CTLORG(1)
         CTLEND(1)=360.+CTLEND(1)
         longshift=.true.
      ENDIF
c
c     does longitude span 360 degrees?
c
      if (abs(FLOAT(CTLMSZ1)*CTLSCL-360.).le.1.e-4) then
         write(lu(1),*)'The map spans complete longitude.'
         fullmap=.true.
      else
         fullmap=.false.
      endif

      if (initial.or.CTLEMN.ne.last_EMN.or.CTLEMX.ne.last_EMX) then
c
c     build PSF
c
         gamma=2.
         CALL PSMGET
         CALL ERROR(1,LOC)
         last_EMN=CTLEMN
         last_EMX=CTLEMX
      endif
c
c     Get diffuse emission model
c
      if (initial) then
csb           if (GMAPFILE.eq.'ask_once') then
csb              WRITE(LU(1),'("Enter GMAPFILE name:",$)')
csb              READ(LU(12),'(A)')GMAPFILE_original
csb              write(*,*),'1 g_orig: ', gmapfile_original
csb           else
         GMAPFILE_original=GMAPFILE
c              write(*,*),'2 g_orig: ', gmapfile_original
csb           endif
      endif

c        write(*,*),'13'
csb        if (GMAPFILE_original.eq.'ask') then
csb           WRITE(LU(1),'("Enter GMAPFILE name:",$)')
csb           READ(LU(12),'(A)')GMAPFILE
csb           if (GMAPFILE.ne.'standard') goto 60
csb        elseif (GMAPFILE_original.ne.'standard') then
csb           GMAPFILE=GMAPFILE_original
csb           write(*,*),'******* Gmapfile: ', gmapfile
csb           write(*,*),'goto 60'
csb           goto 60
csb        endif

c        write(*,*),'14'
 50   continue
c
c     select a standard diffuse map
c
      ex_dir = data_dir         ! so that code does not have to be modified.
                                ! data_dir is now read from the input par - sb-12/01
      in_dex = index(ex_dir, ' ') - 2
      if (in_dex.lt.2) then
         Print *,'GMAP_DIR environmental variable must be set',
     &        ' to use standard maps.'
         goto 61
      else
         GMAPFILE=ex_dir(1:in_dex)//'cfgas.'
      endif

      next_dex = index(GMAPFILE, ' ') - 1

      if (coord_sys.eq.'C') then
         GMAPFILE=GMAPFILE(1:next_dex)//'cel.'
      elseif (coord_sys.eq.'G') then
         GMAPFILE=GMAPFILE(1:next_dex)//'gal.'
      else
         write(6,*)
     &        'Diffuse maps not available for this coordinate system.'
         WRITE(LU(1),'("Enter GMAPFILE name:",$)')
         READ(LU(12),'(A)')GMAPFILE
         goto 60
      endif

      next_dex = index(GMAPFILE, ' ') - 1

      if (ctlemn.eq.30.and.ctlemx.eq.100) then
         GMAPFILE=GMAPFILE(1:next_dex)//'g002a'
      elseif (ctlemn.eq.100.and.ctlemx.ge.10000) then
         GMAPFILE=GMAPFILE(1:next_dex)//'g002b'
      elseif (ctlemn.eq.30.and.ctlemx.eq.50)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g001a'
      elseif (ctlemn.eq.50.and.ctlemx.eq.70)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g001b'
      elseif (ctlemn.eq.70.and.ctlemx.eq.100)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g001c'
      elseif (ctlemn.eq.100.and.ctlemx.eq.150)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g001d'
      elseif (ctlemn.eq.150.and.ctlemx.eq.300)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g001e'
      elseif (ctlemn.eq.300.and.ctlemx.eq.500)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g001f'
      elseif (ctlemn.eq.500.and.ctlemx.eq.1000)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g001g'
      elseif (ctlemn.eq.1000.and.ctlemx.eq.2000)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g001h'
      elseif (ctlemn.eq.2000.and.ctlemx.eq.4000)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g001i'
      elseif (ctlemn.eq.4000.and.ctlemx.ge.10000)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g001j'
      elseif (ctlemn.eq.100.and.ctlemx.eq.300)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g003a'
      elseif (ctlemn.eq.300.and.ctlemx.ge.10000)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g003b'
      elseif (ctlemn.eq.300.and.ctlemx.eq.1000)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g004a'
      elseif (ctlemn.eq.1000.and.ctlemx.ge.10000)then
         GMAPFILE=GMAPFILE(1:next_dex)//'g004b'
      else
         write(6,*)
     &        'Not a standard energy range. Will read unconvolved map.'
         if (in_dex.lt.2) then
            Print *,'GMAP_DIR environmental variable must be set',
     &	 	 '  to use standard.'
            goto 61
         else
            GMAPFILE=ex_dir(1:in_dex)//'gas.'
         endif
         
         next_dex = index(GMAPFILE, ' ') - 1
         if (coord_sys.eq.'C') then
            GMAPFILE=GMAPFILE(1:next_dex)//'cel'
         elseif (coord_sys.eq.'G')then
            GMAPFILE=GMAPFILE(1:next_dex)//'gal'
         endif
      endif
c
c     test
c
      INQUIRE (FILE=GMAPFILE,EXIST=FEXIST)

      IF (.not.FEXIST) THEN
         write(6,*)GMAPFILE
         write(6,*)
     &        'is not installed. Will read unconvolved map.'

         if (in_dex.lt.2) then
            Print *,'GMAP_DIR environmental variable must be set',
     &           '  to use standard.'
            goto 61
         else
            GMAPFILE=ex_dir(1:in_dex)//'gas.'
         endif

         next_dex = index(GMAPFILE, ' ') - 1
         if (coord_sys.eq.'C') then
            GMAPFILE=GMAPFILE(1:next_dex)//'cel'
         elseif (coord_sys.eq.'G') then
            GMAPFILE=GMAPFILE(1:next_dex)//'gal'
         endif
      ENDIF

 60   continue

      if (GMAPFILE.eq.'null') then
         call MAPRST(GASMAP,CTLMSZ1,CTLMSZ2)
         write(lu(1),*)'Setting diffuse  model to null.'
         GASTYP='GCNT'
         GASDOC(1)='Diffuse  model set to null.'
         do i=2,10
            GASDOC(i)=' '
         enddo
         gmap_null=.true.
         goto 80
      else
         gmap_null=.false.
      endif
c
c     OK (finally) read the darn map
c
      mapfile=GMAPFILE
      GASTYP='GMAP'
      CALL MAPRED(GASMAP,GASTYP,GASDOC,.true.)
      if (signal.ne.' ') then
c
c     problem reading file
c
         CALL ERROR(0,LOC)
      else
         goto 62
      endif

 61   WRITE(LU(1),'("Enter GMAPFILE name:",$)')
      READ(LU(12),'(A)')GMAPFILE
      if (GMAPFILE.eq.'standard') goto 50
      goto 60

 62   if (PSFTYP.eq.'COSBVELA....'.and.BMAPFILE.ne.' ') then
c
c     use cosb background file
c
         MAPFILE=BMAPFILE
         BMPTYP='BMAP'
         CALL MAPRED(BMAP,BMPTYP,BMPDOC,.true.)
         IF (SIGNAL.NE.' ') CALL ERROR(1,LOC)
         CALL GASBAD            !ADD BACKGROUND TO GAS
         IF (SIGNAL.NE.' ') CALL ERROR(1,LOC)
      else
c
c     egret has no background - oder?
c
      endif

      write(lu(1),*)'Multiplying the gasmap by exposure map.'
      CALL MAPMLT(GASMAP,EMAP,CTLMSZ1,CTLMSZ2)
      CALL ERROR(1,LOC)
      GASTYP='GCNT'

      if (gmap_conv) then
         WRITE(LU(1),'("The model has already been convolved.")')
         
         if (gmap_E_dif) then
            write(lu(1),*)
     *           'EGRET PSF is energy dependent!'
            return
         endif

      else
         if (gmap_E_dif) then
            write(lu(1),*)
     &           'The diffuse prediction energy range differs from ',
     *           'the counts map !'
            call intent(determined)
            if (.not.determined) return
         endif

 70      WRITE(LU(1),'(
     &        "Wish to convolve the diffuse map? (Y or N)",$)')
         READ(LU(12),'(A)') input

         IF (input.eq.'Y'.or.input.eq.'y') then
            Ranal_old=Ranal
 74         write(lu(1),'(
     &           "Input desired spectral index for PSF (cr for ",
     &           f4.2,":",$)') gamma
            READ(LU(12),'(A)')input
            
            if (input.ne.' ') then
               read(input,*,end=74)gamma
               CALL PSMGET
               CALL ERROR(1,LOC)
            endif

 75         write(lu(1),'(
     &       "Input radius for convolution (cr for",f7.2,"):",$)')Ranal
            READ(LU(12),'(A)')input

            if (input.ne.' ') then
               read(input,*,end=75)Ranal
            endif

            CTLNBC=Ranal/CTLSCL

            if (CTLNBC*2+1.gt.PSFSZ) then
               write(lu(1),*)
     &              'Sorry, your value would exceed the PSF array size'
c
c     201 maximum hard coded in psfrep.copy
c
               CTLNBC=(PSFSZ-1)/2
            endif

            Ranal=float(CTLNBC)*CTLSCL
            write(lu(1),'("Convolving the diffuse  model with ",
     &           "the PSF over Rconvolve=",f5.1)') Ranal
            CALL MAPCNV(GASMAP,TMPMAP,CTLMSZ1,CTLMSZ2)
            CALL MAPCPY(GASMAP,TMPMAP,CTLMSZ1,CTLMSZ2)
            gmap_conv=.true.
            write(input,'("Convolve with R=",f5.1,"; ")')Ranal
            nchar=23
            call DOCADD(nchar,GASDOC,GASTYP,input)
            Ranal=Ranal_old

         elseif (input.eq.'N'.or.input.eq.'n') then
c
c     the choice is to not convolve
c
            if (gmap_E_dif) then
               write(lu(1),*)
     &              'The diffuse prediction energy range differs from ',
     *              'the counts map!'
               call intent(determined)
               
               if (.not.determined) return
            endif
            
            write(lu(1),*)
     &           'In principle, your diffuse model should be convolved!'
            call intent(determined)
            if (.not.determined) goto 70
            write(lu(1),*)'The diffuse  model has not been convolved.'
            CTLNBC=Ranal/CTLSCL
            Ranal=float(CTLNBC)*CTLSCL
            write(lu(1),'("Setting Ranal to ",f5.1)') Ranal
         else
c
c     no clear choice on convolution
c
            goto 70
         endif
      endif                     ! (gmap_conv) clause

 77   Ranal = input_ranal       ! sb: 12/01
                                ! set Ranal to the value read from like.par  
      if (Ranal.gt.180.) then
         write(lu(1),*)
     &        'Setting Ranal to 180 degrees (maximum meaningful value)'
         Ranal=180.
      endif

      CTLNBC=Ranal/CTLSCL
      Ranal=float(CTLNBC)*CTLSCL
C
C     Verify that all  pixels with counts have background:
C
      DO  I1 = 1,CTLMSZ1
         DO  I2 = 1,CTLMSZ2
            IF (MAPVAL(MAP,I1,I2,CTLMSZ1,CTLMSZ2).NE.0.0) THEN
               IF (MAPVAL(GASMAP,I1,I2,CTLMSZ1,CTLMSZ2).LE.0.0) THEN
                  CALL MAPCOR(I1,I2,GL,GB)
                  write(6,'("Warning:Empty diffuse  model pixel at",
     &                 2f6.1)')GL,GB
               ENDIF
            ENDIF
         ENDDO
      ENDDO

 80   continue

      CALL ROISET(CTLORG(1),CTLORG(2),CTLEND(1),CTLEND(2),
     &     ROIORG,ROIEND,ROIPRG,ROIPND)
      CALL ERROR(1,LOC)

      write(6,*)'Setting ROI to entire map:'
      if (coord_sys.eq.'C') then
         write(6,'("RA.",f8.3," to",f8.3,"; Dec.",f8.3," to",
     &        f8.3)') CTLEND(2),CTLORG(1),CTLEND(1),CTLORG(2)
      else
         write(6,'("Long.",f8.3," to",f8.3,"; Lat.",f8.3," to",
     &        f8.3)') CTLORG(1),CTLEND(1),CTLORG(2),CTLEND(2)
      endif

      if (initial) then
c
c     initialization to be done on program start up
c
         script_name='SCRIPT'
         script_on=.false.
         Write(lu(1),*)'Setting up logarithm table for likelihood.'
         DO ILOG=1,300000
            XLOG(ILOG)=ALOG(FLOAT(ILOG)/1.E4-5.E-5)
         ENDDO
c
c     initialize analysis location
c
         CALL MAPCOR (CTLMSZ1/2,CTLMSZ2/2,srcL,srcB)
         CALL ERROR(1,LOC)
         TS_min=1.
         TS_max=25.
         delta_lnL=3.9/2.
         conf_precent=95.
         g_device='/XWIN'       !to CTL file?
         publish=.false.
         verbose=.false.
         spectral=.false.
         report=.true.
         Counts_min=0.
         Counts_max=1.e9
C+++
c     Change 8/30/94 nominal values of (min/max/nom) for Gmult and Gbias 
c     if Restricted_G(mult/bias)
c     JAE
c 
         if (fullmap) then
            aspj=360.
         else
            aspj=40.
         endif

         Gbias_min=0.5*Gbias_nom/100.
         Gbias_max=2*Gbias_nom/100.
         Gmult_min=0.80
         Gmult_max=1.20

         if (Restrict_Gmult) then
            Gmult_nom=Gmult_nomj
         endif

         if (Restrict_Gmult) then
            Gbias_nom=Gbias_nomj
         endif

         srcN=' '
         initial=.false.
      endif
c
c     initialization to be done for each new counts map
c
      call MAPRST(BMAP,CTLMSZ1,CTLMSZ2)
      BMPTYP='OCNT'
      do i=1,10
         BMPDOC(i)=' '
      enddo

      if (JTRUE) then
         write(lu(1),'("Set OTHER PSF map to null ? (T/f):"$)')
         read(LU(12),'(a)') input
         numcar = index(input, ' ') - 1
         if (numcar.eq.0.or.
     &        ((input(1:1).eq.'T'.or.input(1:1).eq.'t').and.
     &        numcar.eq.1))NSOURCE=0
      else
         NSOURCE=0
      endif

      Counts=1.
      Gmult=Gmult_nom/100.
      Gbias=Gbias_nom/100.
c
c     initialize analysis location
c
      dt = CTLSCL/2.
      if (.not.(CTLORG(1)-dt.le.srcL.and.CTLEND(1)+dt.ge.srcL.and.
     &     CTLORG(2)-dt.le.srcB.and.CTLEND(2)+dt.ge.srcB)) then
         CALL MAPCOR (CTLMSZ1/2,CTLMSZ2/2,srcL,srcB)
         CALL ERROR(1,LOC)
         srcN=' '
      endif

      CALL pixel_select(.false.,' ')
      CALL ERROR(1,LOC)
c
c     trick LIKTOT into finding totals for new maps
c
      Ranal_old=Ranal
      Ranal=1.
      call LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
      Ranal=Ranal_old
      LikTotaled=.false.
      call LIKTOT(map,bmap,gasmap,emap,CTLMSZ1,CTLMSZ2)
c+++
c     Change 8/30/94 JAE section commented out and restrict flags set true
c     These lines have been removed and new output to screen
c     code has been added
c 
c
      write(LU(1),*)' '
      write(LU(1),'(" Restrict Gmult: ",l2,"  Gmult value: ",
     &     f5.1)') Restrict_Gmult,Gmult
      write(LU(1),'(" Restrict Gbias: ",l2,"  Gbias value: ",
     &     f5.1)') Restrict_Gbias,Gbias
      write(LU(1),'(" Gmult notification range: ",f5.2,
     &     " to ",f5.2)')Gmult_min,Gmult_max
      write(LU(1),'(" Gbias notification range: ",f5.2,
     &     " to ",f5.2)')Gbias_min,Gbias_max

      if (.not.Restrict_notice) then
         write(LU(1),'(" User is notified if range is violated")')
      else
         write(LU(1),'(" User is NOT notified if range is violated")')
      endif
      
      write(LU(1),*)' '
c
c     call input_id_pos(char *) with call to return.  This will initialize
c     LPx local and global parameters.
c
      JTRUE=.FALSE.
      input='LPID#Q'
      CALL input_id_pos(input)
      input='LPID#ZC'
      CALL input_id_pos(input)
      input='       '
c
c     End of change
c
 100  CONTINUE	

      if (.not.JTRUE) then
         input='LPID#Q'
         CALL input_id_pos(input)
         input='LPID#ZC'
         CALL input_id_pos(input)
         input='       '
      endif

      jmapflg=.false.
      pntrue=.false.
      J=GETCWD(PWD1)
      i = index(FITS_DIR, ' ') - 1
      if (i.eq.0) i=1

      if (FITS_DIR(i:i).eq.'/'.and.i.gt.1) then
         FITS_DIR(i:i)=' '
      endif

      if (jae_like) write(*,'("100 FITS_DIR->",a)') FITS_DIR(1:i)

      if (nmlst) then
         open(43,file=NAMELST1,err=101)
         read(43,USERIN,err=101)
         read(43,USERIN2,err=101)
         close(unit=43)
         call set_debug(nmlst)
      endif

 101  IF (script_on) then
         WRITE(LU(1),'("like from script>",$)')
         READ(13,'(A)',end=120) input
         WRITE(LU(1),'(A)')input
      else
	 do j=1,50
	    input(j:j)=' '
	 enddo
         
         WRITE(LU(1),*)
         WRITE(LU(1),'("like>",$)')
         READ(LU(12),'(a)') input
	 numcar = index(input, ' ') - 1
      endif

      do j=1,50
	 call to_upper(input(j:j))
      enddo
c
      if (input(1:2).eq.'AO') then
	 if (input(3:3).eq.'L') then
	    write(*,*)' '
	    write(*,'("The current auto-output value is:",l2)')
     &           autooutput
	    write(*,*)' '
	    goto 100
	 endif

	 autooutput=.not.autooutput
	 write(*,*)' '
	 write(*,'("The NEW auto-output value is:",l2)')
     &        autooutput
	 write(*,*)' '
	 goto 100
      endif

      if (input(1:2).eq.'AP') goto 201
c
      if (input(1:2).eq.'FD') pntrue=.true.

 102  continue

      do I=1,numcar
	 call TO_UPPER(input(I:I))
      enddo

      IF (input(1:3).eq.'LPC') then
	 call catproca()
	 goto 100
      endif

      IF (input.EQ.'Z') THEN
c     
c     read script for commands
c
         WRITE(LU(1),
     &        '("Enter script name (cr for ",A20,"):",$)')script_name
         READ(LU(12),'(A)') input
         IF (input.ne.' ') script_name=input
	 script_on=.true.
	 open(13,file=script_name,err=120)
	 LU(12)=13
         goto 100
      ENDIF

 104  if (input.eq.'FD') then
	 if (jae_like) print *,LOC,' input:',input(1:5)
	 print *,'Choosing a new cycle will require your'
	 print *,'also choosing a new counts FITS file'
	 print *,' '
 1042    n = index(FITS_DIR, ' ') - 1

	 if (n.gt.0) then
	    print *,'The present FITS_DIR is:',FITS_DIR(1:n)
	 else
	    n = index(keepfitsdir, ' ') - 1
	    if (keepfitsdir(n:n).eq.'/') n=n-1
	    FITS_DIR=keepfitsdir(1:n)//'   '
 	    goto 1042
	 endif

	 write(*,'(" Please choose a new EGRET cycle (>= 1);")')
	 write(*,'(" Enter a path to another FITS directory;")')
	 write(*,'(" or use 0 for a user defined FITS_DIR:"$)')
	 read(LU(12),'(a)') ex_dir
	 numcar = index(ex_dir, ' ') - 1
	 print *,' '

	 if (numcar.eq.0.and.pntrue.and.n.gt.0) goto 100

	 if (numcar.eq.0.and.pntrue) then
	    print *,' '
	    print *,'FITS_DIR is not defined !'
  	    print *,' '
	    goto 104
	 endif

	 if (numcar.eq.0.and.top1) then
	    input='E.         '
	    goto 104
	 endif
         
	 read(ex_dir,*,err=105)icycle
	 goto 106
 105     continue
	 n = index(ex_dir, ' ') - 1

	 if (n.gt.1) then
	    if (ex_dir(n:n).eq.'/') n=n-1
C	    ierr0=access(ex_dir(1:n),' ')
C	    ierr1=access(ex_dir(1:n),'x')
C	    if (ierr0.ne.0) then	
C	       write(*,'(" Path does not exist")')
C	       write(*,*)' '
C	       write(*,*)' '
C	       goto 104
C	    elseif (ierr1.ne.0) then
C	       write(*,*)' '
C	       write(*,'(" Directory/Data access restricted")')
C	       write(*,*)' '
C	       goto 104
C	    else
	       last_fits_dir=FITS_DIR
	       FITS_DIR=ex_dir(1:n)
	       keepfitsdir=FITS_DIR(1:n)//'   '
	       if (keepfitsdir(n:n).eq.'/') keepfitsdir(n:n)=' '
	       if (jae_like) write(*,'("105 FITS_DIR->",a)')
     &              FITS_DIR(1:n)
	       goto 100
C	    endif
	 endif
         
	 print *,'ERROR: input error: ',ex_dir(1:numcar)
	 goto 104

 106     if (icycle.lt.0.or.icycle.gt.10) then
	    print *,' '
	    print *,' Input ',ex_dir(1:numcar),
     &           ' is out of RANGE (0-10) !'
	    print *,' '
	    goto 104
	 endif

	 last_fits_dir=FITS_DIR
 1062    m = index(last_fits_dir, ' ') - 1
         
	 if (last_fits_dir(m:m).eq.'/'.and.m.gt.1) m=m-1
	 if (icycle.eq.0) FITS_DIR=PHASE(1)
	 if (icycle.eq.1) FITS_DIR=PHASE(2)
	 if (icycle.eq.2) FITS_DIR=PHASE(3)
	 if (icycle.eq.3) FITS_DIR=PHASE(4)
	 if (icycle.eq.4) FITS_DIR=PHASE(5)
	 if (icycle.eq.5) FITS_DIR=PHASE(6)
	 if (icycle.eq.6) FITS_DIR=PHASE(7)
	 if (icycle.eq.7) FITS_DIR=PHASE(8)
	 if (icycle.eq.8) FITS_DIR=PHASE(8)
	 if (icycle.eq.9) FITS_DIR=PHASE(10)
	 if (icycle.eq.10) FITS_DIR=PHASE(11)

	 n = index(FITS_DIR, ' ') - 1

	 if (n.le.0) then
	    print *,' '
	    print *,'The chosen cycle:',icycle,' has not'
	    print *,'been defined in your environment'
	    print *,' '
	    FITS_DIR=last_fits_dir
	    goto 104
	 endif
         
	 if (jae_like) then
	    print *,'FITS_DIR:',FITS_DIR(1:n)
	    print *,'top1:',top1,' pntrue:',pntrue
	    print *,'input:',input(1:5)
	    print *,'cycle:',icycle
	 endif

	 if (FITS_DIR(n:n).eq.'/'.and.n.gt.1) then
	    FITS_DIR(n:n)=' '
	    n=n-1
	 endif

	 keepfitsdir=FITS_DIR(1:n)//'      '
	 if (pntrue.and.FITS_DIR(1:n).eq.
     &        last_fits_dir(1:n).and.m.eq.n) goto 100
	 input='E.'  
	 write(moreinput,'("FITS_DIR=",A)')FITS_DIR(1:n)
         I=-1
	 in_dexm = index(moreinput, ' ')
         moreinput(in_dexm:in_dexm)=char(0)
         I=jputenv(moreinput(1:in_dexm))
         if (I.ne.0) then
	    WRITE(*,*)' '
            print *,' FAILED to change environment variable: ',
     &           moreinput(1:in_dexm-1)
	    WRITE(*,*)' '
	 else
	    write(*,'("FITS_DIR=",A)')FITS_DIR(1:n)
	    WRITE(*,*)' '
         endif
      endif

      if (jae_like) print *,LOC,' input:',input(1:5)

      IF (input(1:1).EQ.'E') THEN
 109     if (input(1:2).eq.'E.') then
	    input='.    '
	    goto 111
	 endif
c
c     Re-select FITS energy range
c     
         WRITE(LU(1),'("THERE ARE ",i3," IMAGES with energy ",
     &        "ranges:")') Nfits_energy
         do n=1,Nfits_energy
            WRITE(LU(1),'(i3,i6," MeV - ",i5," MeV")')N,
     &           Cnts_ENERGY(1,N),Cnts_ENERGY(2,N)
         enddo
         
 110     WRITE(LU(1),'("WHICH DO YOU WANT? ",
     *        "(or . for new CMAP/EMAP, or Q to abort):",$)')
         READ(LU(12),'(A)') input

         if (input.eq.'Q') GO TO 100
 111     if (input(1:1).eq.'.') then
	    CMAPFILE='ask'
	    EMAPFILE='as cnts'
	    iired=.true.
	    if (pntrue.or.top1) goto 30001
	    goto 51
         endif

         READ(input,*,end=110)IFILE

         IF (IFILE.GT.Nfits_energy.or.IFILE.lt.1) GOTO 110
         ctlemn=Cnts_ENERGY(1,IFILE)
         ctlemx=Cnts_ENERGY(2,IFILE)
         zenith_cut=zenith(IFILE)
         
         if (longshift) then
c     
c     use fits map values
c
            CTLORG(1)=oldCTLORG1
            CTLEND(1)=oldCTLEND1
         ENDIF

         MAPFILE=MAPFILE_C
         CALL MAPRED(MAP,MAPTYP,MAPDOC,.true.)
         
         if (SIGNAL.ne.' ') then
	    input(50:50)=SIGNAL
            CALL ERROR(1,LOC)
	    goto 109
	 endif

         goto 15
c
      ELSEIF (input.EQ.'Q') THEN
	 return
      ELSEIF (input(1:3).eq.'WWW') then
	 N = 1
         call net_info('-',N)
      ELSEIF (input(1:4).EQ.'HELP'.or.input(1:1).EQ.'?') THEN
c
c     Provide command help.
c     
	 call info(input)
         
	 goto 100
         
      ELSEIF (input.EQ.'S') THEN
c     
c     Specify values for ROI range
c
         call roiadj
      ELSEIF (input(1:1).eq.'J') then
	 numcar = index(input, ' ') - 1

	 if (numcar.gt.1) then
	    if (input.eq.'JPO') then
	       INQUIRE(FILE='psf.output',EXIST=FEXIST)
	       if (FEXIST) call system('rm psf.output')
	       open(43,file='psf.output')
	       x=0.
	       y=0.
	       z=0.
	       xff2=0.

	       do i=1,100
		  x= PSFINC*(I) - PSFINC/2.0
		  xff= 2.*PI*(1.-COS(float(I)*PSFINC*PI180))
		  dy = xff - xff2
		  xff2 = xff
		  y = PSFARR(I)*dy
		  z = PSFARR(I)*dy+z
		  write(43,'(f5.1,3(2x,E14.7))')x,PSFARR(I),y,z
	       enddo

	       close(unit=43)
            elseif (input(1:2).eq.'JE') then
	       egret_psf=.true.
	       GAMMA = 2.0
	       CALL PSMGET
	       goto 100
	    else
	       write(*,'("Command ",A," is not in use")')input(1:numcar)
	    endif

	 else
	    call MAPRST(xrrmap,CTLMSZ1,CTLMSZ2)
	    call MAPRST(emap,CTLMSZ1,CTLMSZ2)
	    call MAPRST(map,CTLMSZ1,CTLMSZ2)
	    call MAPRST(gasmap,CTLMSZ1,CTLMSZ2)
	    call MAPRST(tmpmap,CTLMSZ1,CTLMSZ2)
	    call MAPRST(nmap,CTLMSZ1,CTLMSZ2)
	    call MAPRST(bmap,CTLMSZ1,CTLMSZ2)
	    NSOURCE=0
	    goto 3
	 endif

      ELSEIF (input(1:1).eq.'R')then
	 call psf_rep(input(1:2))
	 goto 100
      ELSE
         call likecommand(input)
      ENDIF
      
      GO TO 100

 120  continue
c
c     end of script
c
      script_on=.false.
      close(13)
      LU(12)=5
      goto 100
 999  FEXIST=.false.
      goto 2
      END
