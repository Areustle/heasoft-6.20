HD_COMPONENT_NAME	= heatools

HD_COMPONENT_VERS	=

HD_CTASK		= ftverify

HD_CTASK_SRC_c		= ftverify.c fvrf_file.c fvrf_key.c fvrf_head.c fvrf_data.c fvrf_misc.c

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -l${WCSLIB} ${HD_STD_LIBS} ${SYSLIBS}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ftverify.par

HD_INSTALL_HELP		= ftverify.html

HD_CLEAN		= fitsverify

include ${HD_STD_MAKEFILE}

standalone:
	rm -f ftverify.o
	${HD_MAKE} HD_CTASK=fitsverify HD_CTASK_SRC_c="${HD_CTASK_SRC_c}" HD_CFLAGS="-DSTANDALONE ${HD_STD_CFLAGS}"

# Build on Linux against a static version of CFITSIO (libcfitsio.a):
webtool:
	rm -f *.o
	${HD_MAKE} HD_CTASK=fitsverify HD_CTASK_SRC_c="${HD_CTASK_SRC_c}" HD_CFLAGS="-DSTANDALONE -DERR2OUT -DWEBTOOL ${HD_STD_CFLAGS}" HD_CLIBS="${HD_LFLAGS} ${HEADAS}/lib/libcfitsio.a -lm"
