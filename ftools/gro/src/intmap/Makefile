HD_COMPONENT_NAME	= ftools

HD_COMPONENT_VERS	= 

HD_CTASK		= intmap

HD_CTASK_SRC_c		= hintmap.c checkreal.c listlevel.c namelist.c \
			  readinput.c

HD_CTASK_SRC_f		= binang.f celgal.f chkfts.f earthp.f exposr.f \
			  fndfil.f getval.f intens.f intmap.f nxtmod.f \
			  readat.f readft.f reflect.f shadow.f upcase.f \
			  wrtfts.f wrtlin.f wtdfactr.f

HD_CFLAGS		= ${HD_STD_CFLAGS}

HD_FFLAGS		= ${HD_STD_FFLAGS}

HD_CLIBS		= ${HD_LFLAGS} -lgro${FT_VERS} \
			  ${HD_STD_LIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_TASKS	= ${HD_CTASK}

HD_INSTALL_PFILES	= intmap.par

HD_INSTALL_HELP		= intmap.hlp intmap.txt

include ${HD_STD_MAKEFILE}
