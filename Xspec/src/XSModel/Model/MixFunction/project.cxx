
#include <xsTypes.h>
#include <XSUtil/FunctionUtils/funcType.h>

#include <Projection.h>
#include <project.h>


void Project::modFunction(const EnergyPointer& energyArray,
                                  const std::vector<Real>& parameterValues,
                                  GroupFluxContainer& flux,
                                  GroupFluxContainer& fluxError,
                                  MixUtility* mixUtility,
                                  const std::string& modelName)
{
   mixUtility->perform(energyArray,parameterValues,flux,fluxError);
}                                  

MixUtility* Project::createUtility()
{
   // ASSUME this can't throw.
   return new Projection("Projection");
}


template <>
void XSCall<Project>::operator() (const EnergyPointer& energyArray, 
                        const std::vector<Real>& parameterValues, GroupFluxContainer& flux,
                        GroupFluxContainer& fluxError, MixUtility* mixGenerator, 
                        const string& modelName) const
{
   Project::modFunction(energyArray,parameterValues,flux,fluxError, mixGenerator,modelName);
}

template <>
MixUtility* XSCall<Project>::getUtilityObject() const
{
   return Project::createUtility();
}

