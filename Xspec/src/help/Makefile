HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_INSTALL_HELP		= xspec.txt

HD_INSTALL_ONLY_EXTRA	= install-help-tarfile

include ${HD_STD_MAKEFILE}

install-help-tarfile:
	@if [ ! -d ${SPECTRAL} ]; then \
		mkdir ${SPECTRAL}; \
	fi
	@if [ ! -d ${SPECTRAL_HELP} ]; then \
		mkdir ${SPECTRAL_HELP}; \
	fi
	@cd ${SPECTRAL_HELP}; \
	echo "cd ${SPECTRAL_HELP}"; \
	${GUNZIP} -c ${HD_SRC}/src/help/XspecManual-12.9.1.tar.gz | ${TAR} xf -; \
	echo "${GUNZIP} -c ${HD_SRC}/src/help/XspecManual-12.9.1.tar.gz | ${TAR} xf -"; \
	${GUNZIP} -c ${HD_SRC}/src/help/XspecManual-12.9.1.html.tar.gz | ${TAR} xf -; \
	echo "${GUNZIP} -c ${HD_SRC}/src/help/XspecManual-12.9.1.html.tar.gz | ${TAR} xf -"; \
	${HD_INSTALL} XSerror.pdf XSuncertain.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSexit.pdf XSquit.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelApec.pdf XSmodelVapec.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelBapec.pdf XSmodelBvapec.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelBbody.pdf XSmodelZbbody.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelBremss.pdf XSmodelZbremss.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelC6mekl.pdf XSmodelC6pmekl.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelC6mekl.pdf XSmodelC6pvmekl.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelC6mekl.pdf XSmodelC6vmekl.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelEdge.pdf XSmodelZedge.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelEquil.pdf XSmodelVequil.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelGnei.pdf XSmodelVgnei.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelHighecut.pdf XSmodelZhighect.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelMkcflow.pdf XSmodelVmcflow.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelMeka.pdf XSmodelVmeka.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelMekal.pdf XSmodelVmekal.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelNei.pdf XSmodelVnei.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelNpshock.pdf XSmodelVpnshock.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelPcfabs.pdf XSmodelZpcfabs.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelPhabs.pdf XSmodelVphabs.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelPhabs.pdf XSmodelZphabs.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelPhabs.pdf XSmodelZvphabs.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelPowerlaw.pdf XSmodelZpowerlw.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelPshock.pdf XSmodelVpshock.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelRaymond.pdf XSmodelVraymond.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelSedov.pdf XSmodelVsedov.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelTbabs.pdf XSmodelTbgrain.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelTbabs.pdf XSmodelTbvarabs.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelTbabs.pdf XSmodelZtbabs.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelVarabs.pdf XSmodelZvarabs.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelWabs.pdf XSmodelZwabs.pdf ${HD_LN_S}; \
	${HD_INSTALL} XSmodelWndabs.pdf XSmodelZwndabs.pdf ${HD_LN_S}
	@if [ ! -d ${SPECTRAL_HELP}/python ]; then \
		mkdir ${SPECTRAL_HELP}/python; \
	fi
	@cd ${SPECTRAL_HELP}/python; \
	echo "cd ${SPECTRAL_HELP}/python"; \
	${HD_INSTALL} ${HD_SRC}/src/help/python/PyXspec.pdf PyXspec.pdf ${HD_CP_P}; \
	${GUNZIP} -c ${HD_SRC}/src/help/python/PyXspec.html.tar.gz | ${TAR} xf -; \
	echo "${GUNZIP} -c ${HD_SRC}/src/help/python/PyXspec.html.tar.gz | ${TAR} xf -"
