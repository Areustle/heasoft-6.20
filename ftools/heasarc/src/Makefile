HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	=

HD_SUBDIRS		= addspec arf2arf1 ascii2pha bincurve binspec chkarf \
			  chkpha chkrmf ecd2pha extractor extrpsf fadmap \
			  fixregion fovdsp getocctimes grppha grppha2 gtisum \
			  hkexpand hkscale hkunexpand mathpha nh \
			  rbnpha searchflickpix sf2pha sky2xy spibkg_init xy2sky

HD_TEST_SUBDIRS		= searchflickpix

include ${HD_STD_MAKEFILE}

FC_BASENAME_G95		= ${shell echo ${FC_BASENAME} | grep -c g95}

subdir-sf2pha:
	@if [ ${G77} != yes -a ${FC_BASENAME_G95} = 0 ]; then \
		${HD_MAKE} hd-std-subdir HD_SUBDIR=sf2pha; \
	fi
