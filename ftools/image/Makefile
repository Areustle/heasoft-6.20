HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_SUBDIRS		= ximage/src/astadd ximage/src/pgadd ximage/src \
			  tools/mkcolor tools/mkhratio \
			  ximage/scripts ximage/doc

HD_INSTALL_PFILES	= ximage/manager/ximage.par

HD_INSTALL_ONLY_EXTRA	= install-image-dirs

include ${HD_STD_MAKEFILE}

IMAGE_DIRS		= ximage/cal ximage/files ximage/manager \
			  ximage/pfiles ximage/tcl ximage/xtra

install-image-dirs:
	@for imgdir in ${IMAGE_DIRS}; do \
		${HD_INSTALL} $$imgdir ${IMAGE}/$$imgdir ${HD_CP_P}; \
	done
