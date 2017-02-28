HD_COMPONENT_NAME	= Xspec

HD_LIBRARY_ROOT		= _pyXspec

HD_PYLIB		= yes

HD_CXXFLAGS		= ${HD_STD_CXXFLAGS} ${PYTHON_INC} \
			  -I${HD_SRC}/src -I${HD_SRC}/src/include

HD_LIBRARY_SRC_cxx	= PyFit.cxx PyModel.cxx PyParameter.cxx PyPlot.cxx \
			  PyPymod.cxx PyResponse.cxx PySpectrum.cxx \
			  PyxsIO.cxx PyXset.cxx PyXSutils.cxx pyXspecmodule.cxx

HD_ADD_SHLIB_LIBS	= yes

HD_SHLIB_LIBS		= ${HD_LFLAGS} -lXSUser -lXSPlot -lXSMinuit -lMinuit2 \
			  -lxsmix -lXSFunctions -lXSFit -lXSModel -lXSUtil \
			  -lplt -lXS -l${WCSLIB} -l${TCLREADLINE} -l${TCL} \
			  ${PYTHON_LIB} ${LINK_PGPLOT_TCL} \
			  ${HD_STD_LIBS} ${XLIBS} ${SYSLIBS} ${F77LIBS4C}

HD_INSTALL_PYTHON_LIBS	= lib${HD_LIBRARY_ROOT}${SHLIB_SUFFIX} \
			  chain.py data.py fit.py __init__.py model.py parameter.py \
			  plot.py response.py spectrum.py xset.py

HD_PYTHON_SUBDIR	= xspec

HD_CLEAN		= ${HD_BLD_PYTHON_LIB}/${HD_PYTHON_SUBDIR}/${HD_LIBRARY_ROOT}.so

HD_INSTALL_EXTRA	= install-extra-libname

include ${HD_STD_MAKEFILE}

install-extra-libname:
	@if [ -f ${HD_TOP_PYTHON_LIB}/${HD_PYTHON_SUBDIR}/lib${HD_LIBRARY_ROOT}${SHLIB_SUFFIX} ]; then \
	    ${HD_INSTALL} \
	       ${HD_TOP_PYTHON_LIB}/${HD_PYTHON_SUBDIR}/lib${HD_LIBRARY_ROOT}${SHLIB_SUFFIX} \
	       ${HD_TOP_PYTHON_LIB}/${HD_PYTHON_SUBDIR}/${HD_LIBRARY_ROOT}${PY_SHLIB_SUFFIX} ${HD_LN_S}; \
	fi
