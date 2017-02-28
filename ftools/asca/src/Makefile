HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_SUBDIRS		= ascaarf ascaeffmap ascaexpo ascalin ascaray \
			  attitude cleansis corpileup correctrdd dfe2mkf faint \
			  faintdfe fasttime fastwmap fmosaic ghkcurve ghkdump \
			  gisrti gqaplot ldeadtime mkdtime mkfilter2 mkgisbgd \
			  ofaintdfe oldaspect sisclean sishist sispi sqaplot \
			  sisrmg temp2gain timeconv

include ${HD_STD_MAKEFILE}

MACOSX_64BIT		= ${shell echo ${HD_HOST} | grep -c x86_64-apple-darwin}

subdir-sisrmg:
	@if [ ${MACOSX_64BIT} = 0 ]; then \
		${HD_MAKE} hd-std-subdir HD_SUBDIR=sisrmg; \
	fi
