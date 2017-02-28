HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= ${FTOOLSLIB}

HD_LIB_STYLE		= ${HD_LIB_STYLE_F77}

HD_LIBRARY_SRC_c	= gen/rg2rf.c gen/set_binspec.c unx/gttime.c

HD_LIBRARY_SRC_f	= gen/cbdcma.f gen/cbdcom.f gen/cbdcr.f gen/cbdcrg.f \
			  gen/cbdcs.f gen/cbdgr.f gen/cbdgrg.f gen/cbdgs.f \
			  gen/cbdgv.f gen/cbdmp.f gen/cbdnv.f gen/cbdtyp.f \
			  gen/ccaldj.f gen/ccldj.f gen/ccnfg.f gen/cfrelun.f \
			  gen/clenact.f gen/clocase.f gen/crmvblk.f \
			  gen/crmvlbk.f gen/dt2mjd.f gen/dt2sc.f gen/faopen.f \
			  gen/fcidxd.f gen/fcidxe.f gen/fcidxl.f gen/fcidxs.f \
			  gen/fcrhky.f gen/fcsrtd.f gen/fcsrte.f gen/fcsrtl.f \
			  gen/fcsrts.f gen/ffinit.f gen/fgfcal.f gen/fgrfdr.f \
			  gen/ft_gammln.f gen/ft_gammq.f \
			  gen/ft_gcf.f gen/ft_gser.f gen/ft_ran2.f gen/fzext.f \
			  gen/gcfdir.f gen/gtdats.f gen/gtilib.f gen/ocnfg.f \
			  gen/rcnfgl.f gen/rcnfig.f gen/rdcnfg.f \
			  gen/rdkeycole.f gen/rdkeycols.f gen/tim2df.f \
			  gen/timestamp.f gen/trinst.f gen/trtele.f \
			  gen/wt_ferrmsg.f gen/wtbegm.f gen/wtendm.f \
			  gen/wterrm.f gen/wtferr.f gen/wtfwrn.f gen/wtinfo.f \
			  gen/wtout.f gen/wtwarm.f \
			  misc_for/baddummy.f misc_for/clobber.f \
			  misc_for/colparse.f misc_for/colsubset.f \
			  misc_for/convolve.f misc_for/copyfirst.f \
			  misc_for/copyhead.f misc_for/copylast.f \
			  misc_for/d2if.f misc_for/datatopix.f \
			  misc_for/dataty.f misc_for/doloops.f \
			  misc_for/elemparse.f misc_for/fcafml.f \
			  misc_for/fcasfm.f misc_for/fcb2s.f misc_for/fcbfml.f \
			  misc_for/fcclwd.f misc_for/fccmpl.f \
			  misc_for/fccmpr.f misc_for/fccmtd.f \
			  misc_for/fcecho.f misc_for/fcecls.f misc_for/fcerr.f \
			  misc_for/fcerrm.f misc_for/fcgcls.f \
			  misc_for/fcgrgd.f misc_for/fcgrgf.f \
			  misc_for/fcgrgs.f misc_for/fcisjul.f \
			  misc_for/fcislpyr.f misc_for/fcopyr.f \
			  misc_for/fcstln.f misc_for/fctofs.f \
			  misc_for/fdmpsl.f misc_for/fgetdval.f \
			  misc_for/fidate.f misc_for/fimcol.f \
			  misc_for/fiptim.f misc_for/fitsdstk.f \
			  misc_for/fnmcol.f misc_for/fprint.f \
			  misc_for/frmblk.f misc_for/gooddummy.f \
			  misc_for/goodwcs.f misc_for/gtaskn.f \
			  misc_for/gtoken.f misc_for/idxfmt.f \
			  misc_for/initdoublearray.f misc_for/initintarray.f \
			  misc_for/initrealarray.f misc_for/int2mjd.f \
			  misc_for/int2sec.f misc_for/jnt2str.f \
			  misc_for/leapsec.f misc_for/pgfini.f \
			  misc_for/pgfout.f misc_for/pixtodata.f \
			  misc_for/ptaskn.f misc_for/putimage.f \
			  misc_for/rmsubset.f misc_for/sec2int.f \
			  misc_for/xcopynoscale.f misc_for/xcopyscale.f \
			  misc_for/xftgkyd.f misc_for/xftgkys.f \
			  misc_for/xfvv.f \
			  unx/cconc.f unx/cgetlun.f unx/cptend.f unx/cpthnm.f \
			  unx/ctrlog.f unx/faopnw.f unx/fmpfdr.f unx/gtdati.f

# Expand HD_STD_FFLAGS so we can use FOPT_SYS (lower optimization to prevent
# runtime errors):
HD_FFLAGS		= ${FOPT_SYS} ${FFLAGS} ${HD_IFLAGS}

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o} ${HD_LIBRARY_SRC_f:.f=.o}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

include ${HD_STD_MAKEFILE}
