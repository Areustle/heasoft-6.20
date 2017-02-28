HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_LIB_STYLE		= static

HD_LIBRARY_ROOT		= XS

HD_LIBRARY_SRC_c	= cio.c cstderrwrt.c freadline.c setenv.c sysc.c \
			  hea_status.c \
			  ihf/ihf_be_helpful.c ihf/ihf.c ihf/ihf_rd_help_file.c

HD_LIBRARY_SRC_f	= sys.f edicom.f futils.f fxwrite.f gtbuf.f logger.f \
			  lunlst.f ttwrt.f umsput.f xchaty.f xtend.f xwrcmd.f \
			  script.f ufnydemo.f

HD_CFLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSModel/Model \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil ${HD_STD_CFLAGS}

HD_FFLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil ${HD_STD_FFLAGS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o} ${HD_LIBRARY_SRC_f:.f=.o}

HD_DISTCLEAN		= sys.f cio.c sysc.c

include ${HD_STD_MAKEFILE}

FC_BASENAME_IFORT	= ${shell echo ${FC_BASENAME} | grep -c ifort}

# Use sys.lnx for all GNU or Intel Fortran (G77 may be 'no' for ifort),
# except on Cygwin:
sys.f:
	@-rm -f $@; \
	if [ "x${EXT}" = xwin ]; then \
		${HD_LN_S} sys.win $@; \
	elif [ ${G77} = yes -o ${FC_BASENAME_IFORT} != 0 ]; then \
		${HD_LN_S} sys.lnx $@; \
	elif [ -f sys.${EXT} ]; then \
		${HD_LN_S} sys.${EXT} $@; \
	fi

cio.c:
	@-rm -f $@; \
	if [ -f cio${EXT}.c ]; then \
		${HD_LN_S} cio${EXT}.c $@; \
	fi

# Use sysclnx.c whenever GCC='yes', except in Cygwin:
sysc.c:
	@-rm -f $@; \
	if [ "x${EXT}" = xwin ]; then \
		${HD_LN_S} syscwin.c $@; \
	elif [ ${GCC} = yes ]; then \
		${HD_LN_S} sysclnx.c $@; \
	elif [ -f sysc${EXT}.c ]; then \
		${HD_LN_S} sysc${EXT}.c $@; \
	else \
		${HD_LN_S} syscspud.c $@; \
	fi
