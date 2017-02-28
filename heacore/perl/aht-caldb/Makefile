HD_COMPONENT_NAME		= heacore

HD_INSTALL_EXTRA		= install-caldb

AHT_CALDB_FILE			= alias_config.fits caldb.config

AHT_CALDB_PATH			= perl/aht/aht-caldb

include ${HD_STD_MAKEFILE}

install-caldb:
	@for file in ${AHT_CALDB_FILE}; do \
	  ${HD_INSTALL} $$file ${HD_LIB}/${AHT_CALDB_PATH}/$$file ${HD_CP_P}; \
	  if [ "x${HD_LIB}" != "x${HD_TOP_LIB}" ]; then \
	    ${HD_INSTALL} ${HD_LIB}/${AHT_CALDB_PATH}/$$file ${HD_TOP_LIB}/${AHT_CALDB_PATH}/$$file ${HD_LN_S}; \
	  fi; \
	done
