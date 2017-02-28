HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	=

HD_SUBDIRS		= lib tasks help

HD_TEST_SUBDIRS         = tasks

HD_DISTCLEAN		= config.cache config.log config.status \
			  configure.lineno cxxtest.out fortest.out \
			  hd_install headas-setup hmakerc

HD_CLEAN		= ${HD_BLD_EXEC_PFX} ${HD_BLD_PFX}

HD_CTASK		= hd_install

HD_CTASK_SRC_c		= hd_install.c

HD_CFLAGS               = ${CFLAGS}

HD_INSTALL_TASKS	= hd_install

HD_INSTALL_SHELL_SCRIPTS	= hd_scanenv hmake lhea-fixperl

HD_INSTALL_EXTRA	= install-BUILD_DIR install-symlinks

include ${HD_STD_MAKEFILE}

# Install BUILD_DIR; tap dance needed because the built tools live in BUILD_DIR
#-------------------------------------------------------------------------------
install-BUILD_DIR:
	@${HD_INSTALL} ${HD_BUILD_DIR} ${HD_EXEC_PFX}/BUILD_DIR ${HD_CP_P}
