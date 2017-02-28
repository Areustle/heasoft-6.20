HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= ${CALLIB}

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_LIBRARY_SRC_c	= gen/callibf2c.c gen/checkfile.c gen/checkinfile.c \
			  gen/cpfile.c gen/createeboundsext1.c \
			  gen/createnewfits.c gen/dispmsg.c gen/printerror.c \
			  gen/printwarning.c gen/readtypeIIpha.c \
			  gen/readtypeIpha.c gen/statuscheck.c gen/stlog.c \
			  gen/writearf1.c gen/writermf1.c gen/writetypeIIpha.c \
			  gen/writetypeIpha.c

HD_LIBRARY_SRC_f	= gen/bilint.f gen/calclas.f gen/calcnam.f \
			  gen/calcpois.f gen/calcpois2.f gen/calcpois3.f \
			  gen/calcrt.f gen/caldb_info.f gen/caldev.f \
			  gen/caldtyp.f gen/callib_wrap.f gen/calqual.f \
			  gen/cbftoch.f gen/cdjcal.f gen/cfappend.f \
			  gen/cifsl2.f gen/ck_crf.f gen/ck_man.f gen/ck_rmf.f \
			  gen/ckcbd.f gen/clnstr.f gen/cmp_fact.f \
			  gen/cnxpksf.f gen/copnrsf.f gen/copyphd.f \
			  gen/crstrsf.f gen/crsubsf.f gen/crtcif.f \
			  gen/do_addcom.f gen/evexp2.f gen/fndcol.f \
			  gen/fndext.f gen/fndhdu.f gen/fndobf.f gen/gcrefs.f \
			  gen/gcsygd.f gen/gcsylt.f gen/gcsynm.f gen/getkeys.f \
			  gen/grprmf.f gen/gt_crf_grids.f gen/gt_csysnms.f \
			  gen/gt_fchan.f gen/gt_rmf_grids.f gen/gt_wmap.f \
			  gen/gtcalf.f gen/gtclas.f gen/mkcdir.f gen/mkcint.f \
			  gen/mver.f gen/mvext.f gen/op_npa.f gen/opfits.f \
			  gen/opnpa.f gen/par_dim.f gen/pg_fcecho.f \
			  gen/pha_rd.f gen/rd_cifkys.f gen/rd_obsinfo.f \
			  gen/rd_rsp.f \
			  gen/rdalkmap1.f gen/rdarf1.f gen/rdatrd.f \
			  gen/rdatu0.f gen/rdcol1.f gen/rdebd1.f gen/rdebd3.f \
			  gen/rdebd4.f gen/rdeef1.f gen/rdegd1.f gen/rdeprd.f \
			  gen/rdetm1.f gen/rdgti1.f gen/rdhkrd.f gen/rdmvrd.f \
			  gen/rdmvu0.f gen/rdobf1.f gen/rdoht1.f gen/rdpha1.f \
			  gen/rdpha2.f gen/rdreg1.f gen/rdrmf1.f gen/rdrmf2.f \
			  gen/rdrmf3.f gen/rdrmf4.f gen/rdrmf5.f gen/rdrpf1.f \
			  gen/rdsss1.f gen/rdsss_info.f gen/rdstrd.f \
			  gen/rdtobs.f gen/remap.f gen/rmap1d.f gen/rmfsz.f \
			  gen/rmvexsp.f gen/set_key.f gen/shellig.f \
			  gen/strcal.f gen/updpha.f gen/wmap2oah.f \
			  gen/wt_addcom.f gen/wt_cifkys.f gen/wt_com.f \
			  gen/wt_copy.f gen/wt_ear1992a.f gen/wt_eef1992a.f \
			  gen/wt_flt1992a.f gen/wt_objradec.f gen/wt_obsinfo.f \
			  gen/wt_pntradec.f gen/wt_regdesc.f gen/wt_scradec.f \
			  gen/wt_spec.f gen/wtarf1.f gen/wtcol1.f gen/wtear1.f \
			  gen/wtebd1.f gen/wtebd2.f gen/wtebd3.f gen/wtebd4.f \
			  gen/wteef1.f gen/wteff1.f gen/wtegd1.f gen/wtehr1.f \
			  gen/wtexm1.f gen/wtftim.f gen/wtipc1.f gen/wtobf1.f \
			  gen/wtpcmkf.f gen/wtpha1.f gen/wtpha2.f gen/wtpha3.f \
			  gen/wtrhr1.f gen/wtrmf1.f gen/wtrmf2.f gen/wtrmf3.f \
			  gen/wtrmf4.f gen/wtrmf5.f gen/wtrpf1.f gen/wtrps1.f \
			  gen/wtspr1.f gen/wttobs.f gen/wttrs1.f gen/wtvig1.f \
			  gen/xtcol1.f unx/apndfl.f unx/cgdr.f unx/ck_file.f \
			  unx/copy.f unx/cr_tmpfile.f unx/cspawn.f \
			  unx/delfil.f unx/direx.f unx/gtcdir.f unx/gtdir.f \
			  unx/makdir.f unx/mvfile.f unx/opasci.f unx/ppthnm.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o} ${HD_LIBRARY_SRC_f:.f=.o}

include ${HD_STD_MAKEFILE}
