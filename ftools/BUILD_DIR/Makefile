HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

FTOOLS_LIBRARIES	= xanlib ftoolslib cftoolslib/src callib ascalib \
			  fimagelib/src grolib roslib velalib/src xtelib

FTOOLS_PACKAGES		= asca caltools einstein exosat fimage futils gro \
			  heao1 oso8 ia/doc qdp rosat time timing/xronos \
			  heasarc vela5b xselect xstar xte image guis

HD_SUBDIRS		= include refdata bin.perl \
			  ${FTOOLS_LIBRARIES} ${FTOOLS_PACKAGES}

HD_TEST_SUBDIRS		= ${FTOOLS_LIBRARIES} ${FTOOLS_PACKAGES}

HD_DISTCLEAN		= config.cache config.log config.status \
			  configure.lineno cxxtest.out fortest.out \
			  hd_install headas-setup hmakerc

HD_CLEAN		= ${HD_BLD_EXEC_PFX} ${HD_BLD_PFX}

HD_CTASK		= hd_install

HD_CTASK_SRC_c		= hd_install.c

HD_CFLAGS               = ${CFLAGS}

HD_INSTALL_TASKS	= hd_install

HD_INSTALL_SHELL_SCRIPTS	= hd_scanenv hmake lhea-fixperl lhea-fixpfiles

HD_INSTALL_EXTRA	= install-BUILD_DIR fixpfiles install-symlinks

include ${HD_STD_MAKEFILE}

# Install BUILD_DIR; tap dance needed because the built tools live in BUILD_DIR
#-------------------------------------------------------------------------------
install-BUILD_DIR:
	@${HD_INSTALL} ${HD_BUILD_DIR} ${HD_EXEC_PFX}/BUILD_DIR ${HD_CP_P}

# Fix .parin files
#-------------------------------------------------------------------------------
fixpfiles:
	@LHEA_DATA=${HD_TOP_REFDATA}; LHEA_HELP=${HD_TOP_HELP}; \
	export LHEA_DATA; export LHEA_HELP; \
	if [ -d ${HD_PFILES} ]; then \
		cd ${HD_PFILES}; lhea-fixpfiles; \
	fi
