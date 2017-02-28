HD_COMPONENT_NAME	= attitude

HD_COMPONENT_VERS	=

HD_LIBRARY_ROOT		= ${ATFUNCTIONS}

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_LIBRARY_SRC_c	= atAberration.c atAddVect.c atAngDistance.c \
			  atAtTimeDToAtTime.c atAtTimeToAtTimeD.c atAzEl.c \
			  atAzElSet.c atBrazil.c atConvPol.c atCopyVect.c \
			  atCrossPts.c atCTime.c atDecToRadian.c \
			  atDegToDec.c atDegToRA.c atDivVect.c atEarthElev.c \
			  atEarthOccult.c atEcliptic.c atEllipsoid.c \
			  atEulerToQuat.c atEulerToRM.c atGeodcr.c \
			  atGeodesic.c atGeodetic.c atGeodeticToGeographic.c \
			  atGeographic.c atGeographicToGeodetic.c atGeomag.c \
			  atGroundCoord.c atHXDBrazil.c atInterpolateEuler.c \
			  atInterpolateQuat.c atInvGeodetic.c \
			  atInvRotMat.c atInvVect.c atJ2000B1950G.c atKepler.c \
			  atMissionTime.c atMJDate.c atMJulian.c atMoon.c \
			  atMulAddVect.c atMulVect.c atNorm.c atNormVect.c \
			  atOrbPlane.c atParseDec.c atParseRA.c atPathNum.c \
			  atPathNumUSC.c atPlanet.c atPol60ToVect.c \
			  atPolDegToVect.c atPolToVect.c atPrecessEuler.c \
			  atPrecession.c atPrecessRM.c atQuatProd.c \
			  atQuatToEuler.c atQuatToRM.c atRAToRadian.c \
			  atReformatAtTime.c atRigidity.c atRigidity2.c \
			  atRMCheck.c atRMProd.c atRMToEuler.c atRMToQuat.c \
			  atRotEuler2.c atRotPVect.c atRotVect.c atSAA.c \
			  atSaisa.c atSatPos.c atScalProd.c atSetElement.c \
			  atSetElement2.c atSetEuler.c atSetGeoRM.c \
			  atSetRotMat.c atSetRotMatZX.c atSidereal.c \
			  atSISBrazil.c atSpline.c atSplineD.c atSTTBrazil.c \
			  atSubVect.c atSun.c atVectProd.c atVectToPol.c \
			  atVectToPol60.c atVectToPolDeg.c \
			  atElementTime3.c atPathNum3.c atPathNumUSC3.c \
			  atSatPos3.c atSetElement3.c

HD_CFLAGS		= -I../include/ ${HD_STD_CFLAGS} -DSLALIB

HD_SHLIB_LIBS		= ${HD_LFLAGS} -l${CFITSIO}

HD_INSTALL_EXTRA	= install-extra-libname

include ${HD_STD_MAKEFILE}

install-extra-libname:
	@if [ -f ${HD_LIB}/lib${HD_LIBRARY_ROOT}${SHLIB_SUFFIX} ]; then \
		${HD_INSTALL} ${HD_LIB}/lib${HD_LIBRARY_ROOT}${SHLIB_SUFFIX} \
			${HD_LIB}/libatFunctions${SHLIB_SUFFIX} ${HD_LN_S}; \
	fi
	@if [ -f ${HD_LIB}/lib${HD_LIBRARY_ROOT}.a ]; then \
		${HD_INSTALL} ${HD_LIB}/lib${HD_LIBRARY_ROOT}.a \
			${HD_LIB}/libatFunctions.a ${HD_LN_S}; \
	fi

atAberration.o: ../include/atFunctions.h
atAddVect.o: ../include/atError.h ../include/atFunctions.h
atAngDistance.o: ../include/atError.h ../include/atFunctions.h
atAtTimeDToAtTime.o: ../include/atFunctions.h
atAtTimeToAtTimeD.o: ../include/atFunctions.h
atAzEl.o: ../include/atFunctions.h
atAzElSet.o: ../include/atError.h ../include/atFunctions.h
atBrazil.o: ../include/atError.h ../include/atFunctions.h
atCTime.o: ../include/atError.h ../include/atFunctions.h
atConvPol.o: ../include/atError.h ../include/atFunctions.h
atCopyVect.o: ../include/atError.h ../include/atFunctions.h
atCrossPts.o: ../include/atError.h ../include/atFunctions.h
atDecToRadian.o: ../include/atFunctions.h
atDegToDec.o: ../include/atFunctions.h
atDegToRA.o: ../include/atFunctions.h
atDivVect.o: ../include/atError.h ../include/atFunctions.h
atEarthElev.o: ../include/atFunctions.h
atEarthOccult.o: ../include/atError.h ../include/atFunctions.h
atEllipsoid.o: ../include/atError.h ../include/atFunctions.h
atEulerToQuat.o: ../include/atError.h ../include/atFunctions.h
atEulerToRM.o: ../include/atError.h ../include/atFunctions.h
atGeodcr.o: ../include/atError.h ../include/atFunctions.h
atGeodesic.o: ../include/atError.h ../include/atFunctions.h
atGeodetic.o: ../include/atError.h ../include/atFunctions.h
atGeodeticToGeographic.o: ../include/atError.h ../include/atFunctions.h
atGeographic.o: ../include/atError.h ../include/atFunctions.h
atGeographicToGeodetic.o: ../include/atError.h ../include/atFunctions.h
atGeomag.o: ../include/atError.h ../include/atFunctions.h
atGroundCoord.o: ../include/atFunctions.h
atHXDBrazil.o: ../include/atError.h ../include/atFunctions.h
atInterpolateEuler.o: ../include/atError.h ../include/atFunctions.h
atInterpolateQuat.o: ../include/atError.h ../include/atFunctions.h
atInvRotMat.o: ../include/atError.h ../include/atFunctions.h
atInvVect.o: ../include/atError.h ../include/atFunctions.h
atJ2000B1950G.o: ../include/atFunctions.h d_mod.c d_sign.c f2c.h
atKepler.o: ../include/atError.h ../include/atFunctions.h
atMissionTime.o: ../include/atFunctions.h
atMJDate.o: ../include/atError.h ../include/atFunctions.h
atMJulian.o: ../include/atError.h ../include/atFunctions.h
atMoon.o: ../include/atError.h ../include/atFunctions.h
atMulAddVect.o: ../include/atError.h ../include/atFunctions.h
atMulVect.o: ../include/atError.h ../include/atFunctions.h
atNorm.o: ../include/atFunctions.h
atNormVect.o: ../include/atError.h ../include/atFunctions.h
atOrbPlane.o: ../include/atError.h ../include/atFunctions.h
atParseDec.o: ../include/atFunctions.h
atParseRA.o: ../include/atFunctions.h
atPathNum.o: ../include/atError.h ../include/atFunctions.h
atPathNumUSC.o: ../include/atError.h ../include/atFunctions.h
atPlanet.o: ../include/atError.h ../include/atFunctions.h
atPol60ToVect.o: ../include/atFunctions.h
atPolDegToVect.o: ../include/atFunctions.h
atPolToVect.o: ../include/atError.h ../include/atFunctions.h
atPrecessEuler.o: ../include/atError.h ../include/atFunctions.h
atPrecessRM.o: ../include/atError.h ../include/atFunctions.h
atPrecession.o: ../include/atError.h ../include/atFunctions.h
atQuatProd.o: ../include/atError.h ../include/atFunctions.h
atQuatToEuler.o: ../include/atError.h ../include/atFunctions.h
atQuatToRM.o: ../include/atError.h ../include/atFunctions.h
atRAToRadian.o: ../include/atFunctions.h
atReformatAtTime.o: ../include/atError.h ../include/atFunctions.h
atRMCheck.o: ../include/atError.h ../include/atFunctions.h
atRMProd.o: ../include/atError.h ../include/atFunctions.h
atRMToEuler.o: ../include/atError.h ../include/atFunctions.h
atRMToQuat.o: ../include/atError.h ../include/atFunctions.h
atRigidity.o: ../include/atError.h ../include/atFunctions.h \
	../include/atSpline.h
atRotEuler2.o: ../include/atError.h ../include/atFunctions.h
atRotPVect.o: ../include/atError.h ../include/atFunctions.h
atRotVect.o: ../include/atError.h ../include/atFunctions.h
atSISBrazil.o: ../include/atFunctions.h
atSTTBrazil.o: ../include/atFunctions.h
atSaisa.o: ../include/atError.h ../include/atFunctions.h
atSatPos.o: ../include/atError.h ../include/atFunctions.h
atScalProd.o: ../include/atFunctions.h
atSetElement.o: ../include/atError.h ../include/atFunctions.h
atSetElement2.o: ../include/atError.h ../include/atFunctions.h
atSetEuler.o: ../include/atError.h ../include/atFunctions.h
atSetGeoRM.o: ../include/atError.h ../include/atFunctions.h
atSetRotMat.o: ../include/atError.h ../include/atFunctions.h
atSetRotMatZX.o: ../include/atFunctions.h
atSidereal.o: ../include/atError.h ../include/atFunctions.h
atSplineD.o: ../include/atError.h ../include/atFunctions.h ../include/atSpline.h
atSubVect.o: ../include/atError.h ../include/atFunctions.h
atSun.o: ../include/atError.h ../include/atFunctions.h
atVectProd.o: ../include/atError.h ../include/atFunctions.h
atVectToPol.o: ../include/atError.h ../include/atFunctions.h
atVectToPol60.o: ../include/atFunctions.h
atVectToPolDeg.o: ../include/atFunctions.h
