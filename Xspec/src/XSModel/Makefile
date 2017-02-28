HD_COMPONENT_NAME	= Xspec

HD_COMPONENT_VERS	= 

HD_LIBRARY_ROOT		= XSModel

HD_CXXFLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit \
			  -I${HD_SRC}/src/XSFit/FitMethod \
			  -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSModel/Model \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil \
			  ${HD_STD_CXXFLAGS} \
			  -DINXSPEC

#			  -IData -IData/Backcorr -IData/Detector \
#			  -IDataFactory -IGlobalContainer -IModel \
#			  -IModel/Component -IModel/EmissionLines \
#			  -IModel/MixFunction -IParameter

HD_CFLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil \
			  ${HD_STD_CFLAGS}

HD_FFLAGS		= -I${HD_SRC}/src \
			  -I${HD_SRC}/src/main -I${HD_SRC}/src/include \
			  -I${HD_SRC}/src/XSFit -I${HD_SRC}/src/XSFunctions \
			  -I${HD_SRC}/src/xslib -I${HD_SRC}/src/XSModel \
			  -I${HD_SRC}/src/XSPlot -I${HD_SRC}/src/XSUser \
			  -I${HD_SRC}/src/XSUtil \
			  ${HD_STD_FFLAGS}

#HD_LIBRARY_SRC_c	= Model/LocalModelTemplate/photoion.c

HD_LIBRARY_SRC_cxx	= Data/DataInputRecord.cxx \
			  Data/DataSet.cxx \
			  Data/DataSetBase.cxx \
			  Data/DataUtility.cxx \
			  Data/EinsteinData.cxx \
			  Data/FakeDataInputRecord.cxx \
			  Data/OGIP-92aData.cxx \
			  Data/SfData.cxx \
			  Data/SpectralData.cxx \
			  Data/BackCorr/Background.cxx \
			  Data/BackCorr/EinsteinBackground.cxx \
			  Data/BackCorr/OGIP-92aBackground.cxx \
			  Data/BackCorr/SfBackground.cxx \
			  Data/Detector/DummyResponse.cxx \
			  Data/Detector/EinsteinResponse.cxx \
			  Data/Detector/MultiResponse.cxx \
			  Data/Detector/OGIP-92aResponse.cxx \
			  Data/Detector/ResponseMatrix.cxx \
			  Data/Detector/RealResponse.cxx \
			  Data/Detector/Response.cxx \
			  Data/Detector/SfResponse.cxx \
			  Data/Detector/UserDummyResponse.cxx \
			  DataFactory/DataFactory.cxx \
			  DataFactory/EinsteinIO.cxx \
			  DataFactory/OGIP-92aIO.cxx \
			  DataFactory/SfIO.cxx \
			  DataFactory/XspecDataIO.cxx \
			  DataFactory/XspecRegistry.cxx \
			  GlobalContainer/Cosmology.cxx \
			  GlobalContainer/DataContainer.cxx \
			  GlobalContainer/MdefContainer.cxx \
			  GlobalContainer/Memento.cxx \
			  GlobalContainer/ModelContainer.cxx \
			  GlobalContainer/ResponseContainer.cxx \
			  GlobalContainer/Weight.cxx \
                          Model/CompCombiner.cxx \
			  Model/Model.cxx \
			  Model/ModelBase.cxx \
			  Model/UniqueEnergy.cxx \
			  Model/UniqueEnergyManager.cxx \
			  Model/Component/ACNComponent.cxx \
			  Model/Component/AddComponent.cxx \
			  Model/Component/AddTableComponent.cxx \
			  Model/Component/AMXComponent.cxx \
			  Model/Component/Component.cxx \
			  Model/Component/ComponentCreator.cxx \
			  Model/Component/ComponentGroup.cxx \
			  Model/Component/ConComponent.cxx \
			  Model/Component/MixUtility.cxx \
			  Model/Component/MixComponent.cxx \
			  Model/Component/MulComponent.cxx \
			  Model/Component/MulTableComponent.cxx \
			  Model/Component/SumComponent.cxx \
			  Model/Component/TableComponent.cxx \
			  Model/Component/OGIPTable/OGIPTable.cxx \
			  Model/Component/OGIPTable/OGIPTableDeferRead.cxx \
			  Model/Component/OGIPTable/OGIPTableReadAll.cxx \
			  Model/Component/OGIPTable/TableAccess.cxx \
			  Model/Component/OGIPTable/TableModel.cxx \
			  Model/EmissionLines/Apec.cxx \
			  Model/EmissionLines/Bearden.cxx \
			  Model/EmissionLines/LineList.cxx \
			  Model/EmissionLines/Mekal.cxx \
			  Parameter/ResponseParam.cxx \
			  Parameter/ModParam.cxx \
			  Parameter/ParamCreator.cxx \
			  Parameter/ParamData.cxx \
			  Parameter/ParamLinkList.cxx \
			  Parameter/Parameter.cxx \
			  Parameter/ParameterLink.cxx \
			  Parameter/ScaleParam.cxx \
			  Parameter/SwitchParam.cxx \
			  Parameter/TableModParam.cxx

HD_INSTALL_LIBRARIES	= ${HD_LIBRARY_ROOT}

HD_INSTALL_HEADERS	= Data/DataInputRecord.h \
			  Data/DataSet.h \
			  Data/DataSetBase.h \
			  Data/DataUtility.h \
			  Data/EinsteinData.h \
			  Data/FakeDataInputRecord.h \
			  Data/OGIP-92aData.h \
			  Data/SfData.h \
			  Data/SpectralData.h \
			  Data/BackCorr/Background.h \
			  Data/BackCorr/EinsteinBackground.h \
			  Data/BackCorr/OGIP-92aBackground.h \
			  Data/BackCorr/SfBackground.h \
			  Data/Detector/DummyResponse.h \
			  Data/Detector/EinsteinResponse.h \
			  Data/Detector/MultiResponse.h \
			  Data/Detector/OGIP-92aResponse.h \
			  Data/Detector/ResponseMatrix.h \
			  Data/Detector/RealResponse.h \
			  Data/Detector/Response.h \
			  Data/Detector/SfResponse.h \
			  Data/Detector/UserDummyResponse.h \
			  DataFactory/DataFactory.h \
			  DataFactory/EinsteinIO.h \
			  DataFactory/OGIP-92aIO.h \
			  DataFactory/SfIO.h \
			  DataFactory/XspecDataIO.h \
			  DataFactory/XspecRegistry.h \
			  GlobalContainer/Cosmology.h \
			  GlobalContainer/DataContainer.h \
			  GlobalContainer/DataSetTypes.h \
			  GlobalContainer/MdefContainer.h \
			  GlobalContainer/Memento.h \
			  GlobalContainer/ModelContainer.h \
			  GlobalContainer/ModelTypes.h \
			  GlobalContainer/ResponseContainer.h \
			  GlobalContainer/ResponseTypes.h \
			  GlobalContainer/TrashAdapter.h \
			  GlobalContainer/TrashPtr.h \
			  GlobalContainer/Weight.h \
                          Model/CompCombiner.h \
			  Model/Model.h \
			  Model/ModelBase.h \
			  Model/ModelFunctions.h \
			  Model/UniqueEnergy.h \
			  Model/UniqueEnergyManager.h \
			  Model/Component/ACNComponent.h \
			  Model/Component/AddComponent.h \
			  Model/Component/AddTableComponent.h \
			  Model/Component/AMXComponent.h \
			  Model/Component/Component.h \
			  Model/Component/ComponentCreator.h \
			  Model/Component/ComponentGroup.h \
			  Model/Component/ComponentListTypes.h \
			  Model/Component/ConComponent.h \
			  Model/Component/MixUtility.h \
			  Model/Component/MixComponent.h \
			  Model/Component/MulComponent.h \
			  Model/Component/MulTableComponent.h \
			  Model/Component/SumComponent.h \
			  Model/Component/TableComponent.h \
			  Model/Component/OGIPTable/OGIPTable.h \
			  Model/Component/OGIPTable/OGIPTableDeferRead.h \
			  Model/Component/OGIPTable/OGIPTableReadAll.h \
			  Model/Component/OGIPTable/TableAccess.h \
			  Model/Component/OGIPTable/TableModel.h \
			  Model/EmissionLines/Apec.h \
			  Model/EmissionLines/Bearden.h \
			  Model/EmissionLines/LineList.h \
			  Model/EmissionLines/Mekal.h \
			  Parameter/ResponseParam.h \
			  Parameter/ModParam.h \
			  Parameter/ParamCreator.h \
			  Parameter/ParamData.h \
			  Parameter/ParamLinkList.h \
			  Parameter/Parameter.h \
			  Parameter/ParameterLink.h \
			  Parameter/ScaleParam.h \
			  Parameter/SwitchParam.h \
			  Parameter/TableModParam.h

# HD_INSTALL_REFDATA	= Model/LocalModelTemplate/localTest.dat

HD_INSTALL_HELP		= DataFactory/sf.doc

HD_CLEAN		= ${HD_LIBRARY_SRC_c:.c=.o} \
			  ${HD_LIBRARY_SRC_cxx:.cxx=.o} \
			  ${HD_LIBRARY_SRC_f:.f=.o} \
			  Data/SunWS_cache \
			  Data/BackCorr/SunWS_cache \
			  Data/Detector/SunWS_cache \
			  DataFactory/SunWS_cache \
			  GlobalContainer/SunWS_cache \
			  Model/SunWS_cache \
			  Model/Component/SunWS_cache \
			  Model/Component/OGIPTable/SunWS_cache \
			  Model/EmissionLines/SunWS_cache \
			  Parameter/SunWS_cache

HD_SHLIB_LIBS		= ${HD_LFLAGS} -lXSUtil -lXS \
			  -l${CCFITS} -l${CFITSIO} ${SYSLIBS} ${F77LIBS4C}

include ${HD_STD_MAKEFILE}
