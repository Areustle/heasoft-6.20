HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	=

HD_SUBDIRS              = src/xslib src/XSUtil src/XSModel src/XSFit \
			  src/XSFunctions src/XSPlot \
			  src/XSModel/Model/MixFunction \
			  src/XSFit/FitMethod/Minuit/minuit2/src \
			  src/XSFit/FitMethod/Minuit src/XSUser python\
			  Integral src/include src/tools src/main src/manager \
			  src/scripts src/help src/spectral/modelData

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

subdir-python:
	@if [ "x${PYTHON_LIB}" != x ]; then \
		if [ "x${PYTHON_INC}" != x -o "x${EXT}" = xdarwin ]; then \
		   ${HD_MAKE} hd-std-subdir HD_SUBDIR=src/XSUser/Python/xspec; \
		fi; \
	fi
