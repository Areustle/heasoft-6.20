HD_COMPONENT_NAME	= ftools

HD_CTASK		= fv

FV_VERSION		= 5.4

PKG_ARCH		= `uname`

PKG_DIR			= fv${FV_VERSION}

PKG_FILE		= fv${FV_VERSION}_${PKG_ARCH}.tar

HD_CTASK_SRC_c		= fvexec.c Et_AppInit.c

HD_CFLAGS		= ${FV_CFLAGS} \
			  -I${HC_BLD_EXEC_PFX}/include \
			  -I${TCLTK_BLD_EXEC_PFX}/include \
			  -I${TCL_DIR} -I${TCL_DIR}/../generic \
			  -I${TIX_DIR}/generic \
			  -I${HD_BLD_INC} ${XINCLUDES} \
			  -DHAVE_UNISTD_H=1 -DSTDC_HEADERS=1 -DHAVE_SYS_STAT_H=1

HD_CLIBS		= ${HC_BLD_EXEC_PFX}/lib/lib${CFITSIO}.a \
			  ${TCLTK_BLD_EXEC_PFX}/lib/libpow.a \
			  ${TCLTK_BLD_EXEC_PFX}/lib/${ITCL}/lib${ITCL}.a \
			  ${TCLTK_BLD_EXEC_PFX}/lib/${ITK}/lib${ITK}.a \
			  ${TCLTK_BLD_EXEC_PFX}/lib/lib${TCL}.a \
			  ${TCLTK_BLD_EXEC_PFX}/lib/lib${TK}.a \
			  ${TCLTK_BLD_EXEC_PFX}/lib/lib${XPA}.a \
			  ${TCLTK_BLD_EXEC_PFX}/lib/${TIX}/lib${TIX}.a \
			  ${HD_BLD_LIB}/libfitstcl.a \
			  -lXft ${XLIBS} ${FV_LIBS}

LDFLAGS_C		= ${FV_CFLAGS} ${FV_LDFLAGS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_CLEAN		= ${PKG_DIR} ${PKG_FILE}.gz pow

HD_DISTCLEAN		= fvexec.c fvexec.h mktclapp tklib

include ${HD_STD_MAKEFILE}

fv-tklib:
	@${HD_INSTALL} ${TCLTK_BLD_EXEC_PFX}/lib/${TCL} tklib/${TCL} ${HD_CP_P}
	@${HD_INSTALL} ${TCLTK_BLD_EXEC_PFX}/lib/tcl8 tklib/tcl8 ${HD_CP_P}
	@${HD_INSTALL} ${TCLTK_BLD_EXEC_PFX}/lib/${TK} tklib/${TK} ${HD_CP_P}
	@${HD_INSTALL} ${TCLTK_BLD_EXEC_PFX}/lib/${ITCL} \
		tklib/${ITCL} ${HD_CP_P}
	@${HD_INSTALL} ${TCLTK_BLD_EXEC_PFX}/lib/${ITK} tklib/${ITK} ${HD_CP_P}
	@${HD_INSTALL} ${TCLTK_BLD_EXEC_PFX}/lib/${IWIDGETS} \
		tklib/${IWIDGETS} ${HD_CP_P}
	@${HD_INSTALL} ${TCLTK_BLD_EXEC_PFX}/lib/${TIX} tklib/${TIX} ${HD_CP_P}
	@-rm -f tklib/${TIX}/Init.tcl
	@${HD_INSTALL} ${TIX_DIR}/library/Init.tcl.fv \
		tklib/${TIX}/Init.tcl ${HD_CP}
	@${HD_INSTALL} ${XPA_DIR}/pkgIndex.tcl tklib/xpa/pkgIndex.tcl ${HD_CP_P}
	@${HD_INSTALL} ${XPA_DIR}/test.tcl tklib/xpa/test.tcl ${HD_CP_P}
	@cd ${HD_SRC}/../tcltk/pow; ../BUILD_DIR/hmake install-pow-extras \
		HD_LIB=${HD_SRC}/guis/fv/unix/tklib POW_EXTRAS="" POW_HTML=""

fvexec: fv-tklib
	@${HD_BUILD_DIR}/hmake mktclapp HD_CTASK=mktclapp \
		HD_CTASK_SRC_c=mktclapp.c HD_CFLAGS="" LDFLAGS_C="" HD_CLIBS=""
	@echo "Launching xmktclapp, please wait..."
	@echo "...and click on 'Build' in the window when it opens."
	@PATH=.:${PATH}; export PATH; \
	${TCLTK_BLD_EXEC_PFX}/bin/wish xmktclapp.tcl &

fvexec-remake:
	./mktclapp -f appinit.mta > fvexec.c

fvpkg:
	@${HD_INSTALL} ${HD_CTASK} ${PKG_DIR}/${HD_CTASK} ${HD_CP_P}
	@${HD_INSTALL} ${XPA_DIR}/xpaaccess ${PKG_DIR}/xpabin/xpaaccess ${HD_CP_P}
	@${HD_INSTALL} ${XPA_DIR}/xpaget ${PKG_DIR}/xpabin/xpaget ${HD_CP_P}
	@${HD_INSTALL} ${XPA_DIR}/xpainfo ${PKG_DIR}/xpabin/xpainfo ${HD_CP_P}
	@${HD_INSTALL} ${XPA_DIR}/xpamb ${PKG_DIR}/xpabin/xpamb ${HD_CP_P}
	@${HD_INSTALL} ${XPA_DIR}/xpans ${PKG_DIR}/xpabin/xpans ${HD_CP_P}
	@${HD_INSTALL} ${XPA_DIR}/xpaset ${PKG_DIR}/xpabin/xpaset ${HD_CP_P}
	@${HD_INSTALL} tklib/${TIX} ${PKG_DIR}/${TIX} ${HD_CP_P}
	@cd ${HD_SRC}/../tcltk/pow; ../BUILD_DIR/hmake install-pow-extras \
		HD_LIB=${HD_SRC}/guis/fv/unix POW_EXTRAS="" POW_TCL=""
	@${HD_INSTALL} pow ${PKG_DIR}/doc ${HD_CP_P}
	@${HD_INSTALL} ../doc ${PKG_DIR}/doc ${HD_CP_P}
	@${HD_INSTALL} ../Release_Notes ${PKG_DIR}/Release_Notes ${HD_CP_P}
	@${HD_INSTALL} ../sample_data ${PKG_DIR}/sample_data ${HD_CP_P}
	@${HD_INSTALL} ../scripts ${PKG_DIR}/sample_scripts ${HD_CP_P}
	@-rm -f ${PKG_DIR}/${TIX}/lib*
	@-rm -rf ${PKG_DIR}/doc/CVS
	@-rm -rf ${PKG_DIR}/sample_data/CVS
	@-rm -rf ${PKG_DIR}/sample_scripts/CVS
	@cd ${PKG_DIR}/doc; \
	sed -e "s:..\/..\/pow\/::" fv_scripting.html > fv_scripting.html-edit; \
	sed -e "s:..\/fv\/doc\/::" Scripting.html > Scripting.html-edit; \
	${HD_INSTALL} fv_scripting.html-edit fv_scripting.html ${HD_MV_F}; \
	${HD_INSTALL} Scripting.html-edit Scripting.html ${HD_MV_F}
	@-rm -f ${PKG_FILE} ${PKG_FILE}.gz
	@echo "tar cf ${PKG_FILE} ${PKG_DIR}"; tar cf ${PKG_FILE} ${PKG_DIR}
	@echo "gzip ${PKG_FILE}"; gzip ${PKG_FILE}
