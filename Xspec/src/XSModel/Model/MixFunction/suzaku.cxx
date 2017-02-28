
#include <xsTypes.h>
#include <XSUtil/FunctionUtils/funcType.h>

#include <PsfTraits.h>
#include <Psf.h>
#include <suzaku.h>

void SuzakuMix::modFunction(const EnergyPointer& energyArray,
                                  const std::vector<Real>& parameterValues,
                                  GroupFluxContainer& flux,
                                  GroupFluxContainer& fluxError,
                                  MixUtility* mixUtility,
                                  const std::string& modelName)
{
   mixUtility->perform(energyArray,parameterValues,flux,fluxError);
}                                  

MixUtility* SuzakuMix::createUtility()
{
   // ASSUME this can't throw.
   return new Psf<Suzaku>("Suzaku");
}


template <>
void XSCall<SuzakuMix>::operator() (const EnergyPointer& energyArray, 
                        const std::vector<Real>& parameterValues, GroupFluxContainer& flux,
                        GroupFluxContainer& fluxError, MixUtility* mixGenerator, 
                        const string& modelName) const
{
   SuzakuMix::modFunction(energyArray,parameterValues,flux,fluxError, mixGenerator,modelName);
}

template <>
MixUtility* XSCall<SuzakuMix>::getUtilityObject() const
{
   return SuzakuMix::createUtility();
}

