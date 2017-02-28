HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_SUBDIRS		= autocor crosscor earth2sun efold efsearch lcmath \
			  lcstats lcurve listdata powspec rbf2fits timeskew

include ${HD_STD_MAKEFILE}

subdir-rbf2fits:
	@if [ "x${EXT}" = xsol -a ${G77} != yes ]; then \
		${HD_MAKE} hd-std-subdir HD_SUBDIR=rbf2fits; \
	fi
