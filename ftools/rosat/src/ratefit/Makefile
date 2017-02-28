HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= ratefit

HD_CTASK_SRC_c		= hratefit.c

HD_CTASK_SRC_f		= dcvsrt.f dgausj.f func_rate.f mqm_rate.f \
			  mrq_rate.f ratefit.f rep_rate.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lros${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= ratefit.par

HD_INSTALL_HELP		= ratefit.hlp ratefit.txt

include ${HD_STD_MAKEFILE}
