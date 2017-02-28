
#include <xsTypes.h>
#include <XSUtil/FunctionUtils/funcType.h>

#include <Cluster.h>
#include <ascaCluster.h>


void AscaCluster::modFunction(const EnergyPointer& energyArray,
                                  const std::vector<Real>& parameterValues,
                                  GroupFluxContainer& flux,
                                  GroupFluxContainer& fluxError,
                                  MixUtility* mixUtility,
                                  const std::string& modelName)
{
   mixUtility->perform(energyArray,parameterValues,flux,fluxError);
}                                  

MixUtility* AscaCluster::createUtility()
{
   // ASSUME this can't throw.
   return new Cluster("Cluster");
}

template <>
void XSCall<AscaCluster>::operator() (const EnergyPointer& energyArray, 
                        const std::vector<Real>& parameterValues, GroupFluxContainer& flux,
                        GroupFluxContainer& fluxError, MixUtility* mixGenerator, 
                        const string& modelName) const
{
   AscaCluster::modFunction(energyArray,parameterValues,flux,fluxError, mixGenerator,modelName);
}

template <>
MixUtility* XSCall<AscaCluster>::getUtilityObject() const
{
   return AscaCluster::createUtility();
}

