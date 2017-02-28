*-- Author :    Rene Brun         20/03/91
      SUBROUTINE HLIMAP(LIMIT,NAME)
*.==========>
*.            Initialization routine for HBOOK
*.
*.         The routine maps the file NAME to memory
*          using the routine HCREATEM.
*          The ZEBRA store is created between the address
*          at the start of /PAWC/ and the end of the mapped region.
*
*          One must be careful in using this routine (Unix only)
*          to make sure that no other ZEBRA store or ZEBRA link area
*          is loaded between /PAWC/ and the mapped memory.
*          Either make sure that /PAWC/ is loaded at the end of memory
*          or force the loading of all ZEBRA stores/link areas
*          before /PAWC/
*
*          In case ZEBRA is not initialized, a dummy primary store
*          is created (BIDON)
*
*.            IF(LIMIT>0) CALL MZEBRA and MZPAW
*.            IF(LIMIT=0) no calls to MZEBRA and MZPAW
*.            IF(LIMIT<0) CALL MZPAW
*..=========> ( R.Brun )
*-- Author :
      INTEGER     NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,        LMAIN
      REAL                                       FENC   ,      HCV
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,HCV(9989)
      INTEGER   IQ        ,LQ
      REAL            Q
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      INTEGER       HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     +LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     +LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LNAME,LCHAR,LINT,
     +LREAL,LBLOK,LLBLK,LBUFM,LBUF,LTMPM,LTMP,LTMP1,LHPLIP,LHDUM,
     +LHFIT,LFUNC,LHFCO,LHFNA,LCIDN
      COMMON/HCBOOK/HVERSN,IHWORK,LHBOOK,LHPLOT,LGTIT,LHWORK,
     +LCDIR,LSDIR,LIDS,LTAB,LCID,LCONT,LSCAT,LPROX,LPROY,LSLIX,
     +LSLIY,LBANX,LBANY,LPRX,LPRY,LFIX,LLID,LR1,LR2,LNAME,LCHAR,LINT,
     +LREAL,LBLOK,LLBLK,LBUFM,LBUF,LTMPM,LTMP,LTMP1,LHPLIP,LHDUM(9),
     +LHFIT,LFUNC,LHFCO,LHFNA,LCIDN
*-- Author :
      PARAMETER (NLPATM=100, MXFILES=50)
      COMMON /HCDIRN/NLCDIR,NLNDIR,NLPAT,ICDIR,NCHTOP,ICHTOP(MXFILES)
     +              ,ICHTYP(MXFILES),ICHLUN(MXFILES)
      CHARACTER*16   CHNDIR,    CHCDIR,    CHPAT    ,CHTOP
      COMMON /HCDIRC/CHCDIR(NLPATM),CHNDIR(NLPATM),CHPAT(NLPATM)
     +              ,CHTOP(NLPATM)
      CHARACTER*80 HFNAME
      COMMON /HCFILE/HFNAME(MXFILES)

      COMMON/BIDON/IBID,FENBID(5),LQBID(10000)
*
c+SEQ,HCBOOK,HCFORM,HCDIRE.
      CHARACTER*(*) NAME
      INTEGER     HCREATEM
ccc      CHARACTER*4 GNAME
c+SEQ,VIDQQ.
*.___________________________________________
*
      CALL HMACHI
*
      NHBOOK=IABS(LIMIT)
      IF(LIMIT.GE.0)THEN
         CALL MZEBRA(-3)
         CALL MZSTOR(IBID,'/BIDON/',' ',FENBID,LQBID,LQBID,LQBID,
     +     LQBID(2000),LQBID(10000))
*
      ENDIF
*
ccc      GNAME=NAME
ccc      IS = HCREATEM(GNAME, LQ, NHBOOK, IOFFST)
      IS = HCREATEM(NAME, LQ, NHBOOK, IOFFST)
      IF (IS .EQ. 0) THEN
         PRINT *, 'GLOBAL MEMORY CREATED, offset from LQ =', IOFFST
      ELSE
         PRINT *, 'GLOBAL MEMORY ERROR = ',IS
         RETURN
      ENDIF
*
*          Option ':' disables checking of overlapping stores
      CALL MZSTOR (IXPAWC,'/PAWC/',':',FENC,LQ(1),LQ(1),LQ(1),
     +            LQ(IOFFST+10),LQ(IOFFST+NHBOOK-10))
      NWPAW  = NHBOOK
      CALL MZWORK(IXPAWC,LQ(2),LQ(IOFFST),0)
*
      IHDIV  = 0
      IXHIGZ = 0
      IXKU   = 0
*
      CALL MZLINK(IXPAWC,'/HCBOOK/',LHBOOK,LCDIR,LCIDN)
      ILAST=IOFFST+NHBOOK
      CALL MZLINK(IXPAWC,'HCMAP',LQ(ILAST),LQ(ILAST),LQ(ILAST))
*
***************************************************************
*                                                             *
*--   Structural links in LHBOOK and LCDIR                    *
*                                                             *
*      lq(lcdir-1)= lsdir : pointer to subdirectory          *
*      lq(lcdir-2)= lids  : pointer to 1st ID in directory   *
*      lq(lcdir-3)= ltab  : pointer to list of ordered IDs   *
*      lq(lcdir-4)= lbuf  : pointer to ntuple buffers        *
*      lq(lcdir-5)= ltmp  : pointer to ntuple buffers        *
*      lq(lcdir-6)= lhquad: pointer to HQUAD buffers         *
*      lq(lcdir-7)=       : free                             *
*      lq(lcdir-8)= labl  : used by HPLOT routine HPLABL     *
* R    lq(lcdir-9)= llid  : pointer to last ID in directory  *
* R    lq(lcdir-10)=      : free                             *
***************************************************************
*
*
      IHWORK=IXPAWC+1
      IHDIV =IXPAWC+2
*
      CALL MZFORM('HDIR','4H -I',IODIR)
      CALL MZFORM('HID1','1B 2I 6F -H',IOH1)
      CALL MZFORM('HID2','1B 2I 3F 1I 4F -H',IOH2)
      CALL MZFORM('HIDN','11I -H',IOHN)
      CALL MZFORM('HIDT','13I -H',IONT)
      CALL MZFORM('HBLK','7I -H',IOBL)
      CALL MZFORM('HCF1','2I 2F 4D -F',IOCF1)
      CALL MZFORM('HCB1','2I 2F 4D -B',IOCB1)
      CALL MZFORM('HCF2','2I -F',IOCF2)
      CALL MZFORM('HCF4','4I -F',IOCF4)
      CALL MZFORM('HCB2','2I -B',IOCB2)
      CALL MZFORM('HFIT','10I -F',IOFIT)
      CALL MZBOOK(IHDIV,LCDIR,LHBOOK, 1,'HDIR',50,8,10,IODIR,0)
      CALL UCTOH('PAWC            ',IQ(LCDIR+1),4,16)
      CALL MZBOOK(IHDIV,LTAB ,LHBOOK,-3,'HTAB',500,0,500,2,0)
*
      LMAIN =LHBOOK
      LQ(ILAST)=LMAIN
      LQ(IOFFST+1)=NHBOOK
      LQ(IOFFST+2)=IOFFST
      NLCDIR=1
      NLPAT =1
      CHCDIR(1)='PAWC'
      NCHTOP=1
      CHTOP(1)='PAWC'
      HFNAME(1)='COMMON /PAWC/ in memory'
      ICHTOP(1)=0
      ICHLUN(1)=0
      ICDIR=1
*
  99  RETURN
      END

