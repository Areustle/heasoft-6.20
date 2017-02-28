
#include <xsTypes.h>
#include <XSUtil/FunctionUtils/funcType.h>

#include <PsfTraits.h>
#include <Psf.h>
#include <xmmc.h>


void Xmmc::modFunction(const EnergyPointer& energyArray,
                                  const std::vector<Real>& parameterValues,
                                  GroupFluxContainer& flux,
                                  GroupFluxContainer& fluxError,
                                  MixUtility* mixUtility,
                                  const std::string& modelName)
{
   mixUtility->perform(energyArray,parameterValues,flux,fluxError);
}                                  

MixUtility* Xmmc::createUtility()
{
   // ASSUME this can't throw.
   return new Psf<XMM>("Xmmc");
}


template <>
void XSCall<Xmmc>::operator() (const EnergyPointer& energyArray, 
                        const std::vector<Real>& parameterValues, GroupFluxContainer& flux,
                        GroupFluxContainer& fluxError, MixUtility* mixGenerator, 
                        const string& modelName) const
{
   Xmmc::modFunction(energyArray,parameterValues,flux,fluxError, mixGenerator,modelName);
}

template <>
MixUtility* XSCall<Xmmc>::getUtilityObject() const
{
   return Xmmc::createUtility();
}

