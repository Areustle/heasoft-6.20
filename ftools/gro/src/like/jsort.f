
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c
C        SUBROUTINE JSORT(tinput)
C
C
C  $Id: jsort.f,v 1.2 2013/05/21 19:08:26 irby Exp $
C======================================================================
C*     Effect: Sorts the PSF array (SRC_PARMS) and associated variables
c*	       by either TS, srcL, srcN or flux.
c*
c*	command   sort parameter
c*	   S		srcN (default)
c*	   ST		TS
c*	   SG		srcL
c*	   SF		flux
c
c Note: appending an 'A' causes sorting of active (SRC_PARMS(8,*) > 0.05)
c* 	sources only.
c*
c*	Example: STA  (sort on TS for active source only)
c*	Example: SG   (sort all sources on galactic longitude)
c*	
C*             
c
c----------------------------------------------------------------------
c     	Subroutine Argument Desriptions
c   	^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
c 	character(10) 	TINPUT(1:10)    first ten characters of tinput
c					array from routine input_id_pos.
c					This routine is typically called
c					from routine input_id_pos.
c
C=======================================================================
C  $Log: jsort.f,v $
C  Revision 1.2  2013/05/21 19:08:26  irby
C  Change character*n to character(n) to silence warnings: "Obsolescent
C  feature: Old-style character length".
C
C  Revision 1.1  2002/04/16 20:27:35  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 1.3  1996/10/31  16:24:52  jae
c Added jsav_act to sorted arrays.
c
c Revision 1.2  1996/08/27  15:21:16  jae
c fixed typo on line 30 (missing 1st col 'C')
c
c Revision 1.1  1996/08/26  21:45:44  jae
c Initial revision
c


C-------------------------------------------------------------------------

      SUBROUTINE JSORT(tinput)

c     Common blocks used:
      INCLUDE  '../COMMON/ctlrep.copy'
      INCLUDE  '../COMMON/cnfrep.copy'
      INCLUDE  '../COMMON/roirep.copy'
      INCLUDE  '../COMMON/likrep.copy'
      INCLUDE  '../COMMON/errrep.copy'
      INCLUDE  '../COMMON/maprep.copy'
      INCLUDE  '../COMMON/gasrep.copy'
      INCLUDE  '../COMMON/emprep.copy'
      INCLUDE  '../COMMON/bmprep.copy'
      INCLUDE  '../COMMON/tmprep.copy'
      INCLUDE  '../COMMON/psfrep.copy'
      INCLUDE  '../COMMON/psmrep.copy'
      INCLUDE  '../COMMON/xrrrep.copy'
      INCLUDE  '../COMMON/fitrep.copy'
      INCLUDE  '../COMMON/locpos.copy'

      save
c
      character(80) id
      common /id/id
      character sinput,ainput*18,sinput2*2,tinput*10
      logical jroi,fflg1, fflg2
c
      id = '$Id: jsort.f,v 1.2 2013/05/21 19:08:26 irby Exp $'
      LOC='JSORT'

      sinput2=tinput(1:2)
      sinput=tinput(1:1)
c
      print *,' SORTING '
      print *,' '
      jroi=JTRUE
      if(sinput2(1:2).eq.'ST'.or.sinput2(1:2).eq.'SF')then
	 do jj=1,NSOURCE
            if(.not.sv_dstsv(1,jj))then
               jroi=JFALSE
               if(jae_input_id_pos)
     &              print *,'LPON has not been performed on PSF: ',jj
            endif
	 enddo
      endif
      if(jroi.eqv.JFALSE.and.jae_input_id_pos)then
         print *,' '
         print *,'All sources which have not been LPON'
         print *,'optimized will be randomly sorted at'
         print *,'the bottom of the PSF list'
      endif
 505  fflg1=.false.
      do jj=1,NSOURCE-1
         if(index(tinput,'T').eq.0.and.index(tinput,'F')
     &        .eq.0.and.index(tinput,'G').eq.0)then
            srcL=SRC_PARMS(jj,1)
            srcB=SRC_PARMS(jj,2)
            ainput='                  '
            srcN='                  '
            CALL GETSRCNM()
            ainput=srcN
            srcL=SRC_PARMS(jj+1,1)
            srcB=SRC_PARMS(jj+1,2)
            CALL GETSRCNM()
            read(ainput(6:9),*)iiLt1
            read(srcN(6:9),*)iiLt2
            read(ainput(10:12),*)xBt1
            read(srcN(10:12),*)xBt2
            idif=iiLt2-iiLt1
            xdif=idif
            if(idif.eq.0)xdif=xdif+xBt2-xBt1
         elseif(index(tinput,'T').ne.0)then
            xdif=svn_TS(jj)-svn_TS(jj+1)
         elseif(index(tinput,'F').ne.0)then
            xdif=sv_flx(1,jj)-sv_flx(1,jj+1)
         elseif(index(tinput,'G').ne.0)then
            if(coord_sys.eq.'G')then
               xdif=SRC_PARMS(jj+1,1)-SRC_PARMS(jj,1)
               if(xdif.eq.0)xdif=
     &              SRC_PARMS(jj+1,2)-SRC_PARMS(jj,2)
            else
               pljj=SRC_PARMS(jj,1)
               pbjj=SRC_PARMS(jj,2)
               CALL CELGALD('GC',pLjj,pBjj,tmpLjj,tmpBjj,iiret)
               pljj1=SRC_PARMS(jj+1,1)
               pbjj1=SRC_PARMS(jj+1,2)
               CALL CELGALD('GC',pLjj1,pBjj1,
     &              tmpLjj1,tmpBjj1,iiret)
               xdif=tmpLjj1-tmpLjj
               if(xdif.eq.0)xdif=tmpBjj1-tmpBjj
            endif
         else
            print *,'Command ',sinput2(1:2),' is not valid'
            print *,' '
            return
         endif
         if(xdif.lt.0.and.
     &        ((index(tinput,'A').ne.0.and.SRC_PARMS(jj,8).gt.0.7).or.
     &        index(tinput,'A').eq.0))then
            fflg1=.true.
c     
            do joj=1,10
               tmpx=SRC_PARMS(jj+1,joj)
               SRC_PARMS(jj+1,joj)=SRC_PARMS(jj,joj)
               SRC_PARMS(jj,joj)=tmpx
            enddo
c
            tmpx=jsav_act(jj+1)
            jsav_act(jj+1)=jsav_act(jj)
            jsav_act(jj)=tmpx
c
            srcN=SRC_NAMES(jj+1)
            SRC_NAMES(jj+1)=SRC_NAMES(jj)
            SRC_NAMES(jj)=srcN
c
            tmpx=svn_TS(jj+1)
            svn_TS(jj+1)=svn_TS(jj)
            svn_TS(jj)=tmpx
c
            tmpx=sv_true_x(jj+1)
            sv_true_x(jj+1)=sv_true_x(jj)
            sv_true_x(jj)=tmpx
c
            tmpx=sv_true_y(jj+1)
            sv_true_y(jj+1)=sv_true_y(jj)
            sv_true_y(jj)=tmpx
c
            tmpx =  sv_err68(jj+1)
            sv_err68(jj+1) =  sv_err68(jj)
            sv_err68(jj) =  tmpx
c
            tmpx =  sv_err95(jj+1)
            sv_err95(jj+1) =  sv_err95(jj)
            sv_err95(jj) =  tmpx
c
            sinput =  sv_sigsv(jj+1)
            sv_sigsv(jj+1) =  sv_sigsv(jj)
            sv_sigsv(jj) =  sinput
c
            tmpx =  best_x(jj+1)
            best_x(jj+1) =  best_x(jj)
            best_x(jj) =  tmpx
c
            tmpx =  best_y(jj+1)
            best_y(jj+1) =  best_y(jj)
            best_y(jj) =  tmpx
c
            tmpx = sv_flx(1,jj+1)
            sv_flx(1,jj+1) = sv_flx(1,jj)
            sv_flx(1,jj) = tmpx
c
            tmpx = sv_flx(2,jj+1)
            sv_flx(2,jj+1) = sv_flx(2,jj)
            sv_flx(2,jj) = tmpx
c
            tmpx = sv_cnts(1,jj+1)
            sv_cnts(1,jj+1) = sv_cnts(1,jj)
            sv_cnts(1,jj) = tmpx
c
            tmpx = sv_cnts(2,jj+1)
            sv_cnts(2,jj+1) = sv_cnts(2,jj)
            sv_cnts(2,jj) = tmpx
c
            tmpx = sv_upperlim(jj+1)
            sv_upperlim(jj+1) = sv_upperlim(jj)
            sv_upperlim(jj) = tmpx
c
            tmpx = sv_params(1,jj+1)
            sv_params(1,jj+1) = sv_params(1,jj)
            sv_params(1,jj) = tmpx
c
            tmpx = sv_params(2,jj+1)
            sv_params(2,jj+1) = sv_params(2,jj)
            sv_params(2,jj) = tmpx
c
            tmpx = sv_cel_long(jj+1)
            sv_cel_long(jj+1) = sv_cel_long(jj)
            sv_cel_long(jj) = tmpx
c
            tmpx = sv_cel_lat(jj+1)
            sv_cel_lat(jj+1) = sv_cel_lat(jj)
            sv_cel_lat(jj) = tmpx
c
            tmpx = sv_gal_long(jj+1)
            sv_gal_long(jj+1) = sv_gal_long(jj)
            sv_gal_long(jj) = tmpx
c
            tmpx = sv_gal_lat(jj+1)
            sv_gal_lat(jj+1) = sv_gal_lat(jj)
            sv_gal_lat(jj) = tmpx
c
            tmpx = sv_expose(jj+1)
            sv_expose(jj+1) = sv_expose(jj)
            sv_expose(jj) = tmpx
c
            tmpx = sv_tmp_68_x(jj+1)
            sv_tmp_68_x(jj+1) = sv_tmp_68_x(jj)
            sv_tmp_68_x(jj) = tmpx
c
            tmpx = sv_tmp_68_y(jj+1)
            sv_tmp_68_y(jj+1) = sv_tmp_68_y(jj)
            sv_tmp_68_y(jj) = tmpx
c
            tmpx = sv_tmp_95_x(jj+1)
            sv_tmp_95_x(jj+1) = sv_tmp_95_x(jj)
            sv_tmp_95_x(jj) = tmpx
c
            tmpx = sv_tmp_95_y(jj+1)
            sv_tmp_95_y(jj+1) = sv_tmp_95_y(jj)
            sv_tmp_95_y(jj) = tmpx
c
            do ll=1,10
               fflg2=sv_dstsv(ll,jj+1)
               sv_dstsv(ll,jj+1)=sv_dstsv(ll,jj)
               sv_dstsv(ll,jj)=fflg2
            enddo
         endif
      enddo
      if(fflg1.eqv..true.)goto 505
      return
      end
      
