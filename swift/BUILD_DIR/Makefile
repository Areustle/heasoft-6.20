HD_COMPONENT_NAME	= swift

HD_COMPONENT_VERS	=

HD_SUBDIRS		= gen/lib/perl gen/tasks uvot/lib uvot/tasks xrt \
			  bat/lib bat/tasks bat/refdata help

HD_TEST_SUBDIRS		= uvot/tasks bat/tasks xrt gen/tasks

HD_DISTCLEAN		= config.cache config.log config.status \
			  configure.lineno cxxtest.out fortest.out \
			  hd_install headas-setup hmakerc \
			  ${HD_SRC}/xrt/include

HD_CLEAN		= ${HD_BLD_EXEC_PFX} ${HD_BLD_PFX}

HD_CTASK		= hd_install

HD_CTASK_SRC_c		= hd_install.c

HD_CFLAGS               = ${CFLAGS}

HD_INSTALL_TASKS	= hd_install

HD_INSTALL_SHELL_SCRIPTS	= hd_scanenv hmake lhea-fixperl

HD_INSTALL_EXTRA	= install-BUILD_DIR install-symlinks

include ${HD_STD_MAKEFILE}

gcn:
	${HD_MAKE} hd-std-subdir HD_SUBDIR=uvot/tasks/uvotgcn
	${HD_MAKE} hd-std-subdir HD_SUBDIR=uvot/tasks/uvotstarid/htm
	${HD_MAKE} hd-std-subdir HD_SUBDIR=uvot/tasks/uvotstarid/listgsc

install-gcn:
	${HD_MAKE} hd-std-subdir HD_SUBDIR=uvot/tasks/uvotgcn HD_TARGET=install
	${HD_MAKE} hd-std-subdir HD_SUBDIR=uvot/tasks/uvotstarid/htm HD_TARGET=install
	${HD_MAKE} hd-std-subdir HD_SUBDIR=uvot/tasks/uvotstarid/listgsc HD_TARGET=install

test-gcn:
	${HD_MAKE} hd-std-subdir HD_SUBDIR=uvot/tasks/uvotgcn HD_TARGET=test
	${HD_MAKE} hd-std-subdir HD_SUBDIR=uvot/tasks/uvotgcn HD_TARGET=install-test

# Install BUILD_DIR; tap dance needed because the built tools live in BUILD_DIR
#-------------------------------------------------------------------------------
install-BUILD_DIR:
	@${HD_INSTALL} ${HD_BUILD_DIR} ${HD_EXEC_PFX}/BUILD_DIR ${HD_CP_P}
