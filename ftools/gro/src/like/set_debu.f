C       SUBROUTINE SET_DBUG(logical)
C
C
C  $Id: set_debu.f,v 1.1 2002/04/16 20:27:43 irby Exp $
C=======================================================================
C++     effect: Set up for likelihood debugging output
C++             
C=======================================================================
C  LIKE Version: 4.20 DELIVERED: September 9,1995, 
C  Programmer J. A. Esposito
C+            Updated: by JAE
C=======================================================================
C  $Log: set_debu.f,v $
C  Revision 1.1  2002/04/16 20:27:43  irby
C  New GRO tool 'like'.  Submitted by S.Bansal.
C
c Revision 5.1  1996/02/29  20:53:23  jae
c *** empty log message ***
c
c Revision 5.0  1996/02/13  21:56:22  jae
c Subroutine Module for like V5.00
c
c
c%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	SUBROUTINE set_debug(flag)
c
c Common blocks included
	INCLUDE  '../COMMON/cnfrep.copy'
        INCLUDE  '../COMMON/errrep.copy'
	INCLUDE  '../COMMON/locpos.copy'

        save

	logical flag, jae_jladj
	LOC='SET_DEBUG'
	if(.not.flag)then
       		jae_debug=.false.
       		jae_allfit=.false.
       		jae_amoebaj=.false.
       		jae_basis_funcs=.false.
       		jae_biastest=.false.
       		jae_catproc=.false.
       		jae_cbgaus=.false.
       		jae_celgal=.false.
       		jae_celgald=.false.
       		jae_chood=.false.
       		jae_cnfcal=.false.
       		jae_cntsbias=.false.
       		jae_cnttest=.false.
       		jae_command=.false.
       		jae_cospsf=.false.
       		jae_ctlred=.false.
       		jae_datared=.false.
       		jae_dbrent=.false.
       		jae_derivative2=.false.
       		jae_docadd=.false.
       		jae_egrpsf=.false.
       		jae_errmap=.false.
       		jae_error=.false.
       		jae_exp_anal=.false.
       		jae_fiterr=.false.
       		jae_fitred=.false.
       		jae_fitrit=.false.
       		jae_fitserror=.false.
       		jae_func_limit=.false.
       		jae_funkj=.false.
       		jae_gasbad=.false.
       		jae_gasbias=.false.
       		jae_gascnts=.false.
       		jae_gastest=.false.
       		jae_get_flux=.false.
       		jae_getsrcnm=.false.
       		jae_golden=.false.
       		jae_gtcirc=.false.
       		jae_hdrred=.false.
       		jae_hood=.false.
       		jae_iauname=.false.
       		jae_info=.false.
       		jae_input_id_pos=.false.
       		jae_intent=.false.
       		jae_invert=.false.
       		jae_jfind_iloc=.false.
       		jae_jfnderr1=.false.
       		jae_jfnderr2=.false.
       		jae_jfndpos=.false.
       		jae_jfndposa=.false.
       		jae_jgetsystem=.false.
       		jae_jladj=.false.
       		jae_jlistproc=.false.
       		jae_jloc_pos=.false.
       		jae_jmapfine=.false.
       		jae_jopt_pos=.false.
       		jae_jpsfinfo=.false.
       		jae_jreport=.false.
       		jae_jwrmap=.false.
       		jae_l_check=.false.
       		jae_lhood=.false.
       		jae_like=.false.
       		jae_liktot=.false.
       		jae_limit=.false.
       		jae_listproc=.false.
       		jae_lubksb=.false.
       		jae_ludcmp=.false.
       		jae_mapadd=.false.
       		jae_mapcnv=.false.
       		jae_mapcor=.false.
       		jae_mapcpy=.false.
       		jae_mapcpyroi=.false.
       		jae_mapdiv=.false.
       		jae_mapdivroi=.false.
       		jae_maperr=.false.
       		jae_mapfine=.false.
       		jae_maphigh=.false.
       		jae_mapinc=.false.
       		jae_mapixl=.false.
       		jae_mapmax=.false.
       		jae_mapmlt=.false.
       		jae_mapmult=.false.
       		jae_mapred=.false.
       		jae_maprit=.false.
       		jae_mapritroi=.false.
       		jae_maprst=.false.
       		jae_mapsrc=.false.
       		jae_mapsrcj=.false.
       		jae_mapsub=.false.
      		jae_mapsum=.false.
       		jae_mapval=.false.
       		jae_multproc=.false.
       		jae_net_info=.false.
       		jae_nextsrc=.false.
		jae_not_privilaged=.false.
       		jae_omap=.false.
       		jae_optimize=.false.
       		jae_optimum=.false.
       		jae_pixel_select=.false.
       		jae_profile=.false.
       		jae_psf=.false.
       		jae_psf_scal=.false.
       		jae_psfadd=.false.
       		jae_psfadj=.false.
       		jae_psfbld=.false.
       		jae_psfremove=.false.
       		jae_psfreplace=.false.
       		jae_psfval=.false.
       		jae_psmcor=.false.
       		jae_psmget=.false.
       		jae_psmmat=.false.
       		jae_readcat=.false.
       		jae_realcomp=.false.
       		jae_residue=.false.
       		jae_residuej=.false.
       		jae_roiadj=.false.
       		jae_roiset=.false.
       		jae_rosatpsf=.false.
       		jae_saspsf=.false.
       		jae_set_amat=.false.
       		jae_setup=.false.
       		jae_set_debug=.false.
       		jae_srcounts=.false.
       		jae_srctest=.false.
       		jae_svbksb=.false.
       		jae_svdcmp=.false.
      		jae_svdfit=.false.
       		jae_swapchar=.false.
       		jae_swapreal=.false.
       		jae_totmap=.false.
       		jae_to_upper=.false.
       		jae_valmap=.false.
       		jae_jputenv_=.false.
       		jae_jsetenv_=.false.
		return
	endif
       		jae_debug=jae_debug.or.tttflg.or.jae_allfit.or.
     & jae_basis_funcs.or.jae_biastest.or.jae_catproc.or.jae_cbgaus.or.
     & jae_celgal.or.jae_celgald.or.jae_chood.or.jae_cnfcal.or.
     & jae_cntsbias.or.jae_cnttest.or.jae_command.or.jae_cospsf.or.
     & jae_ctlred.or.jae_datared.or.jae_dbrent.or.jae_derivative2.or.
     & jae_docadd.or.jae_egrpsf.or.jae_errmap.or.jae_error.or.
     & jae_exp_anal.or.jae_fiterr.or.jae_fitred.or.jae_fitrit.or.
     & jae_fitserror.or.jae_func_limit.or.jae_funkj.or.jae_gasbad
		jae_debug=jae_debug.or.jae_amoebaj.or.
     & jae_gasbias.or.jae_gascnts.or.jae_gastest.or.jae_get_flux.or.
     & jae_getsrcnm.or.jae_golden.or.jae_gtcirc.or.jae_hdrred.or.
     & jae_hood.or.jae_iauname.or.jae_info.or.jae_input_id_pos.or.
     & jae_intent.or.jae_invert.or.jae_jfind_iloc.or.jae_jfnderr1.or.
     & jae_jfnderr2.or.jae_jfndpos.or.jae_jfndposa.or.jae_jgetsystem.or.
     & jae_jladj.or.jae_jlistproc.or.jae_jloc_pos.or.jae_jmapfine.or.
     & jae_jopt_pos.or.jae_jpsfinfo.or.jae_jreport.or.jae_jwrmap
		jae_debug=jae_debug.or.jae_l_check.or.jae_lhood.or.
     & jae_like.or.jae_liktot.or.jae_limit.or.jae_listproc.or.
     & jae_lubksb.or.jae_ludcmp.or.jae_mapadd.or.jae_mapcnv.or.
     & jae_mapcor.or.jae_mapcpy.or.jae_mapcpyroi.or.jae_mapdiv.or.
     & jae_mapdivroi.or.jae_maperr.or.jae_mapfine.or.jae_maphigh.or.
     & jae_mapinc.or.jae_mapixl.or.jae_mapmax.or.jae_mapmlt.or.
     & jae_mapmult.or.jae_mapred.or.jae_maprit.or.jae_mapritroi.or.
     & jae_maprst.or.jae_mapsrc.or.jae_mapsrcj.or.jae_mapsub.or.
     & jae_mapsum.or.jae_mapval.or.jae_multproc.or.jae_net_info.or.
     & jae_nextsrc.or.jae_omap.or.jae_optimize.or.jae_optimum
     		jae_debug=jae_debug.or.jae_pixel_select.or.
     & jae_profile.or.jae_psf.or.jae_psf_scal.or.jae_psfadd.or.
     & jae_psfadj.or.jae_psfbld.or.jae_psfremove.or.jae_psfreplace.or.
     & jae_psfval.or.jae_psmcor.or.jae_psmget.or.jae_psmmat.or.
     & jae_readcat.or.jae_realcomp.or.jae_residue.or.jae_residuej.or.
     & jae_roiadj.or.jae_roiset.or.jae_rosatpsf.or.jae_saspsf.or.
     & jae_set_amat.or.jae_setup.or.jae_srcounts.or.jae_srctest.or.
     & jae_svbksb.or.jae_svdcmp.or.jae_svdfit.or.jae_swapchar.or.
     & jae_swapreal.or.jae_totmap.or.jae_to_upper.or.jae_valmap.or.
     & jae_jputenv_.or.jae_jsetenv_.or.jae_not_privilaged.or.
     & jae_set_debug
c
	if(jae_debug)print *,'-------------------------------'
c
	if(jae_debug)print *,'DEBUG: jae_debug'
	if(jae_allfit)print *,'DEBUG: jae_allfit'
	if(jae_amoebaj)print *,'DEBUG: jae_amoebaj'
	if(jae_basis_funcs)print *,'DEBUG: jae_basis_funcs'
	if(jae_biastest)print *,'DEBUG: jae_biastest'
	if(jae_catproc)print *,'DEBUG: jae_catproc'
	if(jae_cbgaus)print *,'DEBUG: jae_cbgaus'
	if(jae_celgal)print *,'DEBUG: jae_celgal'
	if(jae_celgald)print *,'DEBUG: jae_celgald'
	if(jae_chood)print *,'DEBUG: jae_chood'
	if(jae_cnfcal)print *,'DEBUG: jae_cnfcal'
	if(jae_cntsbias)print *,'DEBUG: jae_cntsbias'
	if(jae_cnttest)print *,'DEBUG: jae_cnttest'
	if(jae_command)print *,'DEBUG: jae_command'
	if(jae_cospsf)print *,'DEBUG: jae_cospsf'
	if(jae_ctlred)print *,'DEBUG: jae_ctlred'
	if(jae_datared)print *,'DEBUG: jae_datared'
	if(jae_dbrent)print *,'DEBUG: jae_dbrent'
	if(jae_derivative2)print *,'DEBUG: jae_derivative2'
	if(jae_docadd)print *,'DEBUG: jae_docadd'
	if(jae_egrpsf)print *,'DEBUG: jae_egrpsf'
	if(jae_errmap)print *,'DEBUG: jae_errmap'
	if(jae_error)print *,'DEBUG: jae_error'
	if(jae_exp_anal)print *,'DEBUG: jae_exp_anal'
	if(jae_fiterr)print *,'DEBUG: jae_fiterr'
	if(jae_fitred)print *,'DEBUG: jae_fitred'
	if(jae_fitrit)print *,'DEBUG: jae_fitrit'
	if(jae_fitserror)print *,'DEBUG: jae_fitserror'
	if(jae_func_limit)print *,'DEBUG: jae_func_limit'
	if(jae_funkj)print *,'DEBUG: jae_funkj'
	if(jae_gasbad)print *,'DEBUG: jae_gasbad'
	if(jae_gasbias)print *,'DEBUG: jae_gasbias'
	if(jae_gascnts)print *,'DEBUG: jae_gascnts'
	if(jae_gastest)print *,'DEBUG: jae_gastest'
	if(jae_get_flux)print *,'DEBUG: jae_get_flux'
	if(jae_getsrcnm)print *,'DEBUG: jae_getsrcnm'
	if(jae_golden)print *,'DEBUG: jae_golden'
	if(jae_gtcirc)print *,'DEBUG: jae_gtcirc'
	if(jae_hdrred)print *,'DEBUG: jae_hdrred'
	if(jae_hood)print *,'DEBUG: jae_hood'
	if(jae_iauname)print *,'DEBUG: jae_iauname'
	if(jae_info)print *,'DEBUG: jae_info'
	if(jae_input_id_pos)print *,'DEBUG: jae_input_id_pos'
	if(jae_intent)print *,'DEBUG: jae_intent'
	if(jae_invert)print *,'DEBUG: jae_invert'
	if(jae_jfind_iloc)print *,'DEBUG: jae_jfind_iloc'
	if(jae_jfnderr1)print *,'DEBUG: jae_jfnderr1'
	if(jae_jfnderr2)print *,'DEBUG: jae_jfnderr2'
	if(jae_jfndpos)print *,'DEBUG: jae_jfndpos'
	if(jae_jfndposa)print *,'DEBUG: jae_jfndposa'
	if(jae_jgetsystem)print *,'DEBUG: jae_jgetsystem'
	if(jae_jlistproc)print *,'DEBUG: jae_jlistproc'
	if(jae_jloc_pos)print *,'DEBUG: jae_jloc_pos'
	if(jae_jmapfine)print *,'DEBUG: jae_jmapfine'
	if(jae_jopt_pos)print *,'DEBUG: jae_jopt_pos'
	if(jae_jpsfinfo)print *,'DEBUG: jae_jpsfinfo'
	if(jae_jreport)print *,'DEBUG: jae_jreport'
	if(jae_jwrmap)print *,'DEBUG: jae_jwrmap'
	if(jae_l_check)print *,'DEBUG: jae_l_check'
	if(jae_lhood)print *,'DEBUG: jae_lhood'
	if(jae_like)print *,'DEBUG: jae_like'
	if(jae_liktot)print *,'DEBUG: jae_liktot'
	if(jae_limit)print *,'DEBUG: jae_limit'
	if(jae_listproc)print *,'DEBUG: jae_listproc'
	if(jae_lubksb)print *,'DEBUG: jae_lubksb'
	if(jae_ludcmp)print *,'DEBUG: jae_ludcmp'
	if(jae_mapadd)print *,'DEBUG: jae_mapadd'
	if(jae_mapcnv)print *,'DEBUG: jae_mapcnv'
	if(jae_mapcor)print *,'DEBUG: jae_mapcor'
	if(jae_mapcpy)print *,'DEBUG: jae_mapcpy'
	if(jae_mapcpyroi)print *,'DEBUG: jae_mapcpyroi'
	if(jae_mapdiv)print *,'DEBUG: jae_mapdiv'
	if(jae_mapdivroi)print *,'DEBUG: jae_mapdivroi'
	if(jae_maperr)print *,'DEBUG: jae_maperr'
	if(jae_mapfine)print *,'DEBUG: jae_mapfine'
	if(jae_maphigh)print *,'DEBUG: jae_maphigh'
	if(jae_mapinc)print *,'DEBUG: jae_mapinc'
	if(jae_mapixl)print *,'DEBUG: jae_mapixl'
	if(jae_mapmax)print *,'DEBUG: jae_mapmax'
	if(jae_mapmlt)print *,'DEBUG: jae_mapmlt'
	if(jae_mapmult)print *,'DEBUG: jae_mapmult'
	if(jae_mapred)print *,'DEBUG: jae_mapred'
	if(jae_maprit)print *,'DEBUG: jae_maprit'
	if(jae_mapritroi)print *,'DEBUG: jae_mapritroi'
	if(jae_maprst)print *,'DEBUG: jae_maprst'
	if(jae_mapsrc)print *,'DEBUG: jae_mapsrc'
	if(jae_mapsrcj)print *,'DEBUG: jae_mapsrcj'
	if(jae_mapsub)print *,'DEBUG: jae_mapsub'
	if(jae_mapsum)print *,'DEBUG: jae_mapsum'
	if(jae_mapval)print *,'DEBUG: jae_mapval'
	if(jae_multproc)print *,'DEBUG: jae_multproc'
	if(jae_net_info)print *,'DEBUG: jae_net_info'
	if(jae_nextsrc)print *,'DEBUG: jae_nextsrc'
	if(jae_not_privilaged)print *,'DEBUG: jae_not_privilaged'
	if(jae_omap)print *,'DEBUG: jae_omap'
	if(jae_optimize)print *,'DEBUG: jae_optimize'
	if(jae_optimum)print *,'DEBUG: jae_optimum'
	if(jae_pixel_select)print *,'DEBUG: jae_pixel_select'
	if(jae_profile)print *,'DEBUG: jae_profile'
	if(jae_psf)print *,'DEBUG: jae_psf'
	if(jae_psf_scal)print *,'DEBUG: jae_psf_scal'
	if(jae_psfadd)print *,'DEBUG: jae_psfadd'
	if(jae_psfadj)print *,'DEBUG: jae_psfadj'
	if(jae_psfbld)print *,'DEBUG: jae_psfbld'
	if(jae_psfremove)print *,'DEBUG: jae_psfremove'
	if(jae_psfreplace)print *,'DEBUG: jae_psfreplace'
	if(jae_psfval)print *,'DEBUG: jae_psfval'
	if(jae_psmcor)print *,'DEBUG: jae_psmcor'
	if(jae_psmget)print *,'DEBUG: jae_psmget'
	if(jae_psmmat)print *,'DEBUG: jae_psmmat'
	if(jae_readcat)print *,'DEBUG: jae_readcat'
	if(jae_realcomp)print *,'DEBUG: jae_realcomp'
	if(jae_residue)print *,'DEBUG: jae_residue'
	if(jae_residuej)print *,'DEBUG: jae_residuej'
	if(jae_roiadj)print *,'DEBUG: jae_roiadj'
	if(jae_roiset)print *,'DEBUG: jae_roiset'
	if(jae_rosatpsf)print *,'DEBUG: jae_rosatpsf'
	if(jae_saspsf)print *,'DEBUG: jae_saspsf'
	if(jae_set_amat)print *,'DEBUG: jae_set_amat'
	if(jae_setup)print *,'DEBUG: jae_setup'
	if(jae_set_debug)print *,'DEBUG: jae_set_debug'
	if(jae_srcounts)print *,'DEBUG: jae_srcounts'
	if(jae_srctest)print *,'DEBUG: jae_srctest'
	if(jae_svbksb)print *,'DEBUG: jae_svbksb'
	if(jae_svdcmp)print *,'DEBUG: jae_svdcmp'
	if(jae_svdfit)print *,'DEBUG: jae_svdfit'
	if(jae_swapchar)print *,'DEBUG: jae_swapchar'
	if(jae_swapreal)print *,'DEBUG: jae_swapreal'
	if(jae_totmap)print *,'DEBUG: jae_totmap'
	if(jae_to_upper)print *,'DEBUG: jae_to_upper'
	if(jae_valmap)print *,'DEBUG: jae_valmap'
	if(jae_jputenv_)print *,'DEBUG: jae_jputenv_'
	if(jae_jsetenv_)print *,'DEBUG: jae_jsetenv_'
c
c
	if(jae_debug)print *,'-------------------------------'
	return
	end

