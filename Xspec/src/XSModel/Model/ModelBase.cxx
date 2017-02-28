//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Component
#include <XSModel/Model/Component/Component.h>
// UniqueEnergy
#include <XSModel/Model/UniqueEnergy.h>
// Response
#include <XSModel/Data/Detector/Response.h>
// SumComponent
#include <XSModel/Model/Component/SumComponent.h>
// UniqueEnergyManager
#include <XSModel/Model/UniqueEnergyManager.h>
// ComponentGroup
#include <XSModel/Model/Component/ComponentGroup.h>
// ModelBase
#include <XSModel/Model/ModelBase.h>

#include <XSModel/GlobalContainer/DataContainer.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/GlobalContainer/ResponseContainer.h>
#include <XSModel/Data/Detector/DummyResponse.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSUtil/Numerics/LinearInterp.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSstreams.h>
#include <XSsymbol.h>
#include <XSContainer.h>
#include <algorithm>
#include <cmath>
#include <set>
#include <sstream>


// Class ModelBase::NotAnAddComponent 

ModelBase::NotAnAddComponent::NotAnAddComponent (const string& diag)
  : YellowAlert(diag)
{
  tcerr << "\nAn additive component is needed" << std::endl;
}


// Class ModelBase 

ModelBase::ModelBase(const ModelBase &right)
    : m_sourceNumber(right.m_sourceNumber),
      m_index(right.m_index),
      m_name(right.m_name),
      m_dataGroupNumber(right.m_dataGroupNumber),
      m_parameterIndexBase(right.m_parameterIndexBase),
      m_modelParameterCount(right.m_modelParameterCount),
      m_modelComponentCount(right.m_modelComponentCount),
      m_fullExpression(right.m_fullExpression),
      m_folded(right.m_folded),
      m_mixingLocs(right.m_mixingLocs),
      m_lastModelFluxCalc(right.m_lastModelFluxCalc),
      m_lastModelLuminCalc(right.m_lastModelLuminCalc),
      m_areCompsSpecDependent(right.m_areCompsSpecDependent),
      m_autonomousEnergies(), // needs deep copies
      m_expressionTree(right.m_expressionTree),
      m_foldedModel(right.m_foldedModel),
      m_modelFlux(right.m_modelFlux),          
      m_modelFluxError(right.m_modelFluxError),    
      // Don't copy right's response pointers.  The proper ones
      // will be added via ModelContainer update mechanism.
      m_detector(),          
      m_foldedModelError(right.m_foldedModelError),
      m_energy(right.m_energy),
      m_keVFlux(right.m_keVFlux),
      m_ergFlux(right.m_ergFlux),
      m_keVFluxRange(right.m_keVFluxRange),
      m_ergFluxRange(right.m_ergFluxRange),
      m_componentSource(),
      m_uniqueEnergyFromResp(0),
      m_compGroupTree()
{
  m_compGroupTree = right.m_compGroupTree;
  // Need DEEP copies of the ComponentGroup*'s.
  std::vector<ComponentGroup*>& cGroups = m_compGroupTree.values();
  cGroups.clear();
  ModelExprTree::iterator e = m_expressionTree.postBegin();
  XSModExpTree<ComponentGroup*>::const_iterator itC = 
                right.m_compGroupTree.postBegin();
  XSModExpTree<ComponentGroup*>::const_iterator itCEnd = 
                right.m_compGroupTree.end();
  while (itC != itCEnd)
  {
     cGroups.push_back((*itC)->clone());
     cGroups.back()->parent(this);
     cGroups.back()->setComponentInfo(&(*e));
     itC.postOrderNext();
     e.postOrderNext();
  }

  std::list<SumComponent*>::const_iterator sc = right.m_componentSource.begin();
  std::list<SumComponent*>::const_iterator scEnd = right.m_componentSource.end();


  while (sc != scEnd)
  {
        // the component source list never uses the parent pointer
        m_componentSource.push_back( (*sc)->clone(0));
        ++sc;       
  }

  std::set<UniqueEnergy*>::const_iterator itEng = right.m_autonomousEnergies.begin();
  std::set<UniqueEnergy*>::const_iterator itEngEnd = right.m_autonomousEnergies.end();
  while (itEng != itEngEnd)
  {
     UniqueEnergy* autonomousEnergy = new UniqueEnergy(**itEng);
     // Let's not worry about properly filling in autonomousEnergy's client 
     // spectra here.  This will be done during ModelContainer's 
     // attachResponses/reAttachResponses via setSpectraForAutonomousEngs 
     // function, once we know all the spectra associated with this model.

     // At least init with the dummy resp number though, since the 
     // attachResponses functions won't do anything for that case.
     autonomousEnergy->addClient(0);
     m_autonomousEnergies.insert(autonomousEnergy);
     ++itEng;
  }
  // Create a UniqueEnergyManager for this copy, but leave it
  // empty for now.  It will be filled once this object gets
  // its responses attached.
  m_uniqueEnergyFromResp = new UniqueEnergyManager();
}

ModelBase::ModelBase (const string& modelName, size_t source)
      : m_sourceNumber(source),
        m_index(0),
        m_name(modelName),
        m_dataGroupNumber(1),
        m_parameterIndexBase(0),
        m_modelParameterCount(0),
        m_modelComponentCount(0),        
        m_fullExpression(""),
        m_folded(false),
        m_mixingLocs(),
        m_lastModelFluxCalc(),
        m_lastModelLuminCalc(),
        m_areCompsSpecDependent(false),
        m_autonomousEnergies(),
        m_expressionTree(),
        m_foldedModel(),
        m_modelFlux(),          
        m_modelFluxError(),    
        m_detector(),
        m_foldedModelError(),
        m_energy(),
        m_keVFlux(),
        m_ergFlux(),
        m_keVFluxRange(),
        m_ergFluxRange(),
        m_componentSource(),
        m_uniqueEnergyFromResp(new UniqueEnergyManager()),
        m_compGroupTree()
{
}


ModelBase::~ModelBase()
{
  // Deleting in pre-order will ensure we don't call delete on a nested 
  // CGroup whose CGroup parent still has a SumComponent pointer 
  // pointing to it.
  XSModExpTree<ComponentGroup*>::iterator itCGroup = m_compGroupTree.begin();
  XSModExpTree<ComponentGroup*>::iterator itCGEnd = m_compGroupTree.end();
  while (itCGroup != itCGEnd)
  {
     delete *itCGroup;
     ++itCGroup;
  }

  std::set<UniqueEnergy*>::iterator itEng = m_autonomousEnergies.begin();
  std::set<UniqueEnergy*>::iterator itEngEnd = m_autonomousEnergies.end();
  while (itEng != itEngEnd)
  {
     delete *itEng;
     ++itEng;
  }
  delete m_uniqueEnergyFromResp;
  clearSources();
}


void ModelBase::createParts ()
{
    if (m_compGroupTree.size())
    {
       throw RedAlert("ComponentGroup tree has not been properly cleared.");
    }
    // Tree insertion needs to be done with post-order traversal.
    // Therefore can't step through using XSModExpTree<T>::iterator
    // with its increment operator, which is a pre-order traversal.
    ModelExprTree::iterator itExp = 
                m_expressionTree.postBegin();
    ModelExprTree::iterator itExpEnd = 
                m_expressionTree.end();    
    while ( itExp != itExpEnd)
    {
    // Construct a ComponentGroup instance for every Expression in the 
    // expression tree. The ComponentGroup constructor takes three arguments: 
    // a Group Expression, a const reference to its containing Model, and
    // a nested flag. If the expression's location indicator is not the 
    // same place as its group then it's a nested expression.
        bool nested = itExp->location() != itExp->group();
        ComponentGroup* newGroup = new ComponentGroup(*itExp, this, nested);
        IntegerArray subCompGroups;
        itExp.childNodes(subCompGroups);
        m_compGroupTree.insert(newGroup, subCompGroups);
        itExp.postOrderNext();
    }
    m_compGroupTree.insertRoot(m_expressionTree.getRoot());                                      

    // this has to be done in two passes because the parts member needs to be
    // complete. One could argue to split this function into two, but...
    itExp = m_expressionTree.begin();
    XSModExpTree<ComponentGroup*>::iterator i = m_compGroupTree.begin();
    while ( itExp != itExpEnd)
    {
            // parsetmp contains the list of
            // component names in the ComponentGroup                                      
//            StringList parsetmp = (*i)->parse();    
            // for each word in the StringList, consult
            // the model info sources (model.dat, table
            // models etc.) and construct the appropriate
            // component and its parameters.
            (*i)->setElement(m_compGroupTree); 
            ++itExp;
	    ++i;
    }
    m_fullExpression = refreshExpression(m_expressionTree);
    checkForSpecDependency();
}

ModelBase* ModelBase::clone () const
{
  ModelBase* cloned = new ModelBase(*this);
  // need to look for component groups that are nested and make appropriate
  // links to the group flux item.
  // See the code for ComponentGroup::setElement for more details.
  XSModExpTree<ComponentGroup*>::iterator itCG(cloned->m_compGroupTree.begin());
  XSModExpTree<ComponentGroup*>::iterator itCGEnd(cloned->m_compGroupTree.end());  
  while (itCG != itCGEnd)
  {
     IntegerArray subCompGroups;
     itCG.childNodes(subCompGroups);
     for (size_t i=0; i<subCompGroups.size(); ++i)
     {
        string groupName = ModelExpression<ModExprTreeMember>::makeGroupString((int)subCompGroups[i]);
        ComponentGroup* subGroup = cloned->m_compGroupTree.values()[subCompGroups[i]];
        SumComponent* sc = subGroup->groupFlux();
        (*itCG)->setNestedElement(groupName, sc);
     }
     ++itCG;
  }
  return cloned;
}

void ModelBase::registerParameters () const
{
  XSModExpTree<ComponentGroup*>::const_iterator rp = m_compGroupTree.begin();
  XSModExpTree<ComponentGroup*>::const_iterator rpEnd = m_compGroupTree.end();

  while (rp != rpEnd)
  {
        (*rp)->registerParameters();
        ++rp;       
  }
}

int ModelBase::incrementModelParameterCount ()
{
  ++m_modelParameterCount;
  return m_modelParameterCount;
}

int ModelBase::incrementModelComponentCount ()
{
  ++m_modelComponentCount;
  return m_modelComponentCount;
}

void ModelBase::setDataGroupIndexing (size_t group)
{
  m_dataGroupNumber = group;
  std::map<size_t,std::map<size_t,size_t> >::const_iterator itSource =
                XSContainer::datasets->sourceToDgs().find(m_sourceNumber);
  size_t groupPos = 1;
  if (itSource != XSContainer::datasets->sourceToDgs().end())
  {
     std::map<size_t,size_t>::const_iterator itGroup =
                itSource->second.find(group);
     if (itGroup != itSource->second.end())
        groupPos = itGroup->second;
  }
  m_parameterIndexBase = (groupPos - 1)*m_modelParameterCount;
}

Parameter* ModelBase::getLocalParameter (size_t i) const
{
  XSModExpTree<ComponentGroup*>::const_iterator cg    = m_compGroupTree.begin();     
  XSModExpTree<ComponentGroup*>::const_iterator cgEnd = m_compGroupTree.end(); 
  Parameter* result(0);
  while (cg != cgEnd )
  {
          // localParameter(i): if the component group contains parameter i
          // return it, or return null pointer.
          if ( ( result = (*cg)->localParameter(i) ) != 0) break;
          ++cg;     
  }    
  return result;    
}

bool ModelBase::responseIsDummy () const
{


        bool dummy (false);
        std::map<size_t,Response*>::const_iterator dr = m_detector.begin();
        std::map<size_t,Response*>::const_iterator drEnd = m_detector.end();

        do 
        {
                dummy = !(dr->second->active());
                ++dr;
        }  while (dr != drEnd && !dummy);


        return dummy;
}

void ModelBase::makeInactive (const bool attachDummy)
{

        // the trailing index number is not significant, but responseList(name,index)
        // doesn't have default arguments. There is only one dummy response in the
        // container, and even if there weren't the copies are all identical since
        // DummyResponse is a singleton.
        Response* dummy = XSContainer::responses->responseList(DUMMY_RSP,1);
        // delete convolved arrays. They will need to be recomputed later anyway.
        deleteConv();

        m_uniqueEnergyFromResp->clearAll();
	m_detector.clear();
        // In this context, clearArrays must not be called before 
        // m_detector array is cleared.
	clearArrays();
	m_energy.clear();
        if (attachDummy)
        {
           attachResponse(dummy);
           fillEnergyContainer();
        }
}

void ModelBase::makeActive ()
{
  using namespace XSContainer;
  // go through ResponseContainer and look for all Responses with the same
  // source number as the current model and replace the detector map with 
  // a map containing all of them. 
  ResponseMap& globalResponses = responses->responseList();
  ResponseMapConstIter gr    = globalResponses.begin();
  ResponseMapConstIter grEnd = globalResponses.end();

  // The assumption is that makeActive has been preceded by makeInactive and
  // so this gets rid of a dummy response pointer.
  removeResponse(0);

  // I don't believe that this will work right if some of the datasets
  // have attached dummy responses. But, that may just be a way of saying
  // that one shouldn't represent non-loaded responses with dummies.
  while (gr != grEnd)
  {
        Response* current = gr->second;
        if (current->sourceNumber() == m_sourceNumber 
                        && current->dataGroup() == m_dataGroupNumber)
        {
                // safety check. hard to see how a dummy response
                // (should be the only thing with index == 0)
                // can get here.
                size_t index = current->spectrumNumber();
                if (index) 
                {
                   attachResponse(current);
                   //energy(index,ccurrent->energies());                              
                }
        }
        ++gr;
  }
  // If somehow no responses were attached in the loop above, re-attach
  // a dummy response.
  if (m_detector.empty())
  {
     makeInactive(true);
  }
  fillEnergyContainer();        
}

string ModelBase::refreshExpression (const ModelExprTree& expressionTree)
{
  string newExpression;
  if ( expressionTree.size() == 1)
  {
     newExpression = (expressionTree.values()[0]).exprString();       
  }
  else
  {
     ModelExprTree::const_iterator itExp = expressionTree.begin();
     ModelExprTree::const_iterator itEnd = expressionTree.end();
     while (itExp != itEnd)
     {
        string groupExp(itExp->exprString());
        // Note: It's possible for groupExp to be empty.  This
        // will happen when we get in here after a delcomp and
        // the last Component of a CGroup has just been removed.
        string::size_type groupLoc = groupExp.find("$GROUP");
        while (groupLoc != string::npos)
        {
           ++itExp;
           string replacement(itExp->exprString());
           // ASSUMES group specifier contains 2 and only 2 digits.
           groupExp.replace(groupLoc, 8, replacement);
           groupLoc = groupExp.find("$GROUP");
        }
        if (groupExp.length())
        {
           if (newExpression.length())
              newExpression.append(string(" + "));
           newExpression.append(groupExp); 
        }       
        ++itExp;
     }
  }

  return newExpression;
}

std::list<ModParam*> ModelBase::normParams () const
{
  XSModExpTree<ComponentGroup*>::const_iterator sc = m_compGroupTree.begin();
  XSModExpTree<ComponentGroup*>::const_iterator scEnd = m_compGroupTree.end();
  std::list<ModParam*> norms;
  for ( ; sc != scEnd ; ++sc)
  {
       std::list<ModParam*> groupNorms = (*sc)->normParams();
       std::copy( groupNorms.begin(), groupNorms.end(), back_inserter(norms) );          
  }
  return norms;
}

void ModelBase::setComputeFlag (bool value)
{
  XSModExpTree<ComponentGroup*>::iterator sc = m_compGroupTree.begin();
  XSModExpTree<ComponentGroup*>::iterator scEnd = m_compGroupTree.end();

  while (sc != scEnd)
  {
        (*sc)->setComputeFlag(value);
        ++sc;       
  }
}

void ModelBase::clearArrays ()
{

  // ... in preparation for a fit. If there's a dummy model array,
  // erase it. It will be put back by the dummy command.
  // Model::prepareForFit doesn't operate on inactive models,
  // so models that need dummy flux arrays are left alone.
  // This is also reached as a result of ModelContainer::update
  // calling the makeInactive function.

  IntegerArray currentSpecNums;
  std::map<size_t,Response*>::const_iterator m(m_detector.begin());
  std::map<size_t,Response*>::const_iterator last(m_detector.end());

  while (m != last)
  {
     currentSpecNums.push_back(m->first);
     ++m;
  }

  ArrayContainer::iterator d (m_modelFlux.begin());
  ArrayContainer::iterator dEnd (m_modelFlux.end());

  while  (d != dEnd)
  {
          int doomed (d->first);
          if (!std::binary_search(currentSpecNums.begin(),currentSpecNums.end(),doomed))
          {
                m_modelFlux.erase(d++); 
                ArrayContainer::iterator itErr = m_modelFluxError.find(doomed);
                if (itErr != m_modelFluxError.end())
                   m_modelFluxError.erase(itErr);      
          }
          else ++d;
  }

  XSModExpTree<ComponentGroup*>::iterator sc = m_compGroupTree.begin();
  XSModExpTree<ComponentGroup*>::iterator scEnd = m_compGroupTree.end();

  while (sc != scEnd)
  {
        (*sc)->clearArrays(getUniqueEnergies());
        ++sc;       
  }
}

void ModelBase::deleteConv ()
{
        m_foldedModel.clear();
        m_foldedModelError.clear();
        m_folded = false;
}

bool ModelBase::removeResponse (size_t spectrumNumber)
{
  bool found = false;  
  std::map<size_t,Response*>::iterator r = m_detector.find(spectrumNumber);

  if (r != m_detector.end())
  {
     found = true;
     //m_energy.erase(r->first);
     m_uniqueEnergyFromResp->removeRespEnergy(r->second);
     m_detector.erase(r);
  }

  // Note that we don't redo the external energy container at this 
  // point.  It is far more efficient to wait till after all 
  // attachResponse calls are made, which is why we do it instead 
  // from ModelContainer.

  return found;
}

void ModelBase::allButNorms (IntegerArray& paramsToFreeze) const
{
  XSModExpTree<ComponentGroup*>::const_iterator cg = m_compGroupTree.begin();
  XSModExpTree<ComponentGroup*>::const_iterator cgEnd = m_compGroupTree.end();
  while ( cg != cgEnd)
  {
        (*cg)->allButNorms(paramsToFreeze);
        ++cg;       
  }
}

void ModelBase::resetComponentFlux () const
{
  XSModExpTree<ComponentGroup*>::const_iterator cg = m_compGroupTree.begin();
  XSModExpTree<ComponentGroup*>::const_iterator cgEnd = m_compGroupTree.end();
  while ( cg != cgEnd)
  {
        (*cg)->resetComponentFlux();
        ++cg;       
  }
}

void ModelBase::saveComponentFlux () const
{
  XSModExpTree<ComponentGroup*>::const_iterator cg = m_compGroupTree.begin();
  XSModExpTree<ComponentGroup*>::const_iterator cgEnd = m_compGroupTree.end();
  while ( cg != cgEnd)
  {
        (*cg)->saveComponentFlux();
        ++cg;       
  }
}

void ModelBase::clearSources () throw ()
{
  std::list<SumComponent*>::iterator doomed  = m_componentSource.begin();
  std::list<SumComponent*>::iterator end     = m_componentSource.end();

  for ( ;  doomed != end; ++doomed)  delete *doomed;
  m_componentSource.clear();  
}

void ModelBase::deleteComponent (int group, int component, int index)
{
  ComponentGroup* cGroup = m_compGroupTree.values()[group];
  cGroup->deleteComponent(component,index);
  --m_modelComponentCount;
  checkForSpecDependency();
}

void ModelBase::decrementParameterCount (int by)
{
  m_modelParameterCount -= by;
}

void ModelBase::recreateParts ()
{
    XSModExpTree<ComponentGroup*> newGroups;
    ModelExprTree::iterator itExp = m_expressionTree.postBegin();
    while ( itExp != m_expressionTree.end() )
    {
    // Construct a ComponentGroup instance for every Expression in the 
    // master list. The ComponentGroup constructor takes two arguments: 
    // a Group Expression, and a const reference to its containing Modml.
    // If the expression's location indicator is not  the same place as its group
    //  then it's a nested expression
            bool nested = itExp->location() != itExp->group();
            IntegerArray subGroups;
            itExp.childNodes(subGroups);
            newGroups.insert(new ComponentGroup(*itExp,this, nested), subGroups); 
            itExp.postOrderNext();
    }                                      
    newGroups.insertRoot(m_expressionTree.getRoot());

    // this has to be done in two passes because the CGroups member needs to be
    // complete. One could argue to split this function into two, but...

    // Remember, we already have all the components we need in m_compGroupTree,
    // but they might not be in the proper component group as a result 
    // of an addcomp or editmod.  That's what we'll fix in resetElement 
    // by matching the new expressions in newGroups' m_componentInfo members 
    // with the components in the old m_compGroupTree.  Also remember that
    // existingComponents haven't been properly reindexed yet, but this
    // doesn't make use of them anyway.  resetElement will give them
    // their correct indices.
    XSModExpTree<ComponentGroup*>::iterator itNewGroups = newGroups.begin();
    XSModExpTree<ComponentGroup*>::iterator itNewGroupsEnd = newGroups.end();
    std::vector<Component*> existingComponents;
    bundleComponents(existingComponents);
    while (itNewGroups != itNewGroupsEnd)
    {
       (*itNewGroups)->resetElement(newGroups, existingComponents);
       ++itNewGroups;
    }

    /* 
       Previously, ComponentGroup->m_elements() was not being deleted
       in it's destructor. This seems to be because ownership was 
       being transfered above to newGroups. Therefore, in order to 
       properly free the memory taken up by m_elements(), one should 
       destroy that list in ~ComponentGroup(). BUT, since we 
       transfered ownership as mentioned, we need to resize the list here, 
       to 0 since m_compGroupTree no longer owns the memory, it is owned 
       by newGroups;
    */
    XSModExpTree<ComponentGroup*>::iterator itCGroup = m_compGroupTree.begin();
    XSModExpTree<ComponentGroup*>::iterator itCGEnd = m_compGroupTree.end();
    while (itCGroup != itCGEnd)
    {
       (*itCGroup)->resizeElements(0);
       delete *itCGroup;
       ++itCGroup;
    }
    m_compGroupTree = newGroups;
    checkForSpecDependency();
}

int ModelBase::decrementModelComponentCount ()
{
  return --m_modelComponentCount;
}

bool ModelBase::nestedGroups () const
{
  XSModExpTree<ComponentGroup*>::const_iterator ng = m_compGroupTree.begin();
  XSModExpTree<ComponentGroup*>::const_iterator ngEnd = m_compGroupTree.end();
  bool areNests(false);
  while ( ng != ngEnd && ! areNests )
  {
          areNests = (*ng)->isNested();
          ++ng;       
  }
  return areNests;
}

void ModelBase::insertComponent (int group, int componentOffset, int index, const string& name, char op)
{
  ComponentGroup* cGroup = m_compGroupTree.values()[group];
  cGroup->insertComponent(componentOffset,index,name,op);
}

std::pair<size_t,size_t> ModelBase::getComponentPeak (size_t compNumber, Real& compTotal)
{
  // tcout <<"\nSum components from componentSource list:"<<std::endl;
  std::list<SumComponent*>::const_iterator itsc = m_componentSource.begin();
  while (itsc != m_componentSource.end())
  {
    // tcout << "   sum component name: "<<(*itsc)->name()<<
    //           "  index: "<<(*itsc)->index()<<std::endl;

    if ((*itsc)->index() == -1*static_cast<int>(compNumber))
    {
       break;
    }
     ++itsc;
  }
  if (itsc == m_componentSource.end())
  {
     std::ostringstream msg;
     msg << "Component " << compNumber << " is not an additive component.";
     throw NotAnAddComponent(msg.str());
  }
  const SumComponent* sc = *itsc;
  bool isPeakNeg = false;
  // Need to dig out and check the add component this is based
  // on to see if peak is positive or negative.  If it passed
  // the additive component test above, then we know there's
  // an addComp corresponding to compNumber.
  XSModExpTree<ComponentGroup*>::iterator itCGroup = m_compGroupTree.begin();
  XSModExpTree<ComponentGroup*>::iterator itCGroupEnd = m_compGroupTree.end();
  const Component* addComp = 0;
  while (itCGroup != itCGroupEnd && !addComp)
  {
    addComp = (*itCGroup)->componentByNumber(compNumber);
    ++itCGroup;
  }
  const Parameter* normPar = addComp->parameter("norm");
  if (normPar && normPar->value() < 0.0)
     isPeakNeg = true;

  Real sign = isPeakNeg ? -1.0 : 1.0;
  Real maxPerEng = -sign*1.0e30;
  size_t maxIndex = 0;
  size_t maxSpecNum = 0;
  // Find which unique energy array contains the bin which corresponds to
  // the peak.  The specNum for the photonArray can be taken from any of 
  // the client spectra which use this unique energy.
  std::set<UniqueEnergy*>::const_iterator itUniqueEng = getUniqueEnergies().begin();
  std::set<UniqueEnergy*>::const_iterator itUniqueEngEnd = getUniqueEnergies().end();
  while (itUniqueEng != itUniqueEngEnd)
  {
     const RealArray& energies = (*itUniqueEng)->energy();
     const size_t specNum = *((*itUniqueEng)->clientSpectra().begin());
     const RealArray& unfoldedFlux = sc->photonArray(specNum);
     const size_t nBin = unfoldedFlux.size();
     if (nBin != energies.size()-1)
     {
        string msg("Array size mismatch: energies and unfoldedFlux");
        msg += "\nin ModelBase::getComponentPeak";
        throw RedAlert(msg);
     }
     for (size_t i=0; i<nBin; ++i)
     {
        Real fluxPerEng = .0;
        if (energies[i+1] != energies[i])
        {
           fluxPerEng = unfoldedFlux[i]/(energies[i+1]-energies[i]);
        }
        if (isPeakNeg)
        {
           if (fluxPerEng < maxPerEng)
           {
              maxPerEng = fluxPerEng;
              maxIndex = i;
              maxSpecNum = specNum;
           }
        }
        else if (fluxPerEng > maxPerEng)
        {
           maxPerEng = fluxPerEng;
           maxIndex = i;
           maxSpecNum = specNum;
        }  
     }

     ++itUniqueEng;
  }

  // Only use the energy array which contains the peak for the rest of the
  // eqwidth calculation.  Issue warning if the peak does not fall to
  // near-zero at either edge of the array (implying that a portion of the
  // peak is off of the array boundary).   
  const RealArray& unfoldedFlux = sc->photonArray(maxSpecNum);     
  const RealArray& energies = m_energy[maxSpecNum];
  const size_t nBin = unfoldedFlux.size();
  if (energies[1] != energies[0])
  {
     Real lowEdge = fabs(unfoldedFlux[0]/(energies[1]-energies[0]));
     if (lowEdge > 1.0e-3*fabs(maxPerEng))
     {
        Real peakEng = (energies[maxIndex]+energies[maxIndex+1])/2.0;
        tcout << "***Warning: Component peak is at " << peakEng <<" keV, and does not fall\n"
              << "      to a negligable value at the low edge of the energy array: "
              << energies[0] <<"\n"
              << "      It is recommended that you redo eqwidth using an extended\n"
              << "      energy range (see the \"energies\" command).\n" << std::endl;
     }
  }
  if (energies[nBin] != energies[nBin-1])
  {
     Real highEdge = fabs(unfoldedFlux[nBin-1]/(energies[nBin]-energies[nBin-1]));
     if (highEdge > 1.0e-3*fabs(maxPerEng))
     {
        Real peakEng = (energies[maxIndex]+energies[maxIndex+1])/2.0;
        tcout << "***Warning: Component peak is at " << peakEng <<" keV, and does not fall\n"
              << "      to a negligable value at the high edge of the energy array: "
              << energies[nBin] <<"\n"
              << "      It is recommended that you redo eqwidth using an extended\n"
              << "      energy range (see the \"energies\" command).\n" << std::endl;
     }
  }
  compTotal = unfoldedFlux.sum();
  if (tpout.consoleChatterLevel() >= 15 || tpout.logChatterLevel() >= 15)
  {
     tcout << "  Selected model total flux:  " << compTotal <<
              " photons cm**-2 s**-1" <<std::endl;
     tcout << "  In energy range:  " << energies[0] << " to " <<
              energies[nBin] << " keV" << std::endl;
  }  
  return std::pair<size_t,size_t>(maxIndex,maxSpecNum);
}

Real ModelBase::calcContinuumFlux (size_t compNumber, size_t iMax, size_t specNum, Real fraction)
{
  std::list<SumComponent*>::const_iterator itsc = m_componentSource.begin();
  while (itsc != m_componentSource.end())
  {
    if ((*itsc)->index() == -1*static_cast<int>(compNumber))
    {
       break;
    }
     ++itsc;
  }
  if (itsc == m_componentSource.end())
  {
     std::ostringstream msg;
     msg << "Component " << compNumber << " is not an additive component.";
     throw NotAnAddComponent(msg.str());
  }
  const SumComponent* sc = *itsc;
  const RealArray& energies = m_energy[specNum];

  // determine the energy range on both sides of eMax
  Real eMax = .5*(energies[iMax] + energies[iMax+1]);
  Real deltaE = fraction*eMax;
  // Make this index an int for decrementing.
  int iLow = static_cast<int>(iMax);
  while (iLow >= 0 && energies[iLow] > eMax-deltaE)  --iLow;
  size_t iHigh = iMax+1;
  while (iHigh < energies.size() && energies[iHigh] < eMax+deltaE)  ++iHigh;
  const RealArray& unfoldedModelFlux = m_modelFlux[specNum];
  const RealArray& unfoldedComponentFlux = sc->photonArray(specNum);
  Real continuum = .0;
  for (size_t i=(size_t)iLow; i<iHigh; ++i)
  {
     continuum += unfoldedModelFlux[i]-unfoldedComponentFlux[i];
  }
  // Even if deltaE = 0, it doesn't seem possible that
  // energies[iLow] could = energies[iHigh]
  continuum /= energies[iHigh]-energies[iLow];

  if (tpout.consoleChatterLevel() >= 15 || tpout.logChatterLevel() >= 15)
  {
     tcout << "  Peak model flux at    " << eMax << " keV" << std::endl;
     tcout << "  Continuum evaluated from   " << energies[iLow] << " to "
           << energies[iHigh] << " keV" << std::endl;
     tcout << "  Continuum diff. flux:  " << continuum << 
              " phot. cm**-2 s**-1 keV**-1" << std::endl;
  } 

  return continuum;
}

void ModelBase::deregisterParameters ()
{
  XSModExpTree<ComponentGroup*>::iterator rp = m_compGroupTree.begin();
  XSModExpTree<ComponentGroup*>::iterator rpEnd = m_compGroupTree.end();

  while (rp != rpEnd)
  {
        (*rp)->deregisterParameters();
        ++rp;       
  }
}

Component* ModelBase::firstComponent ()
{
  return (*m_compGroupTree.begin())->firstComponent();
}

void ModelBase::attachResponse (Response* response)
{
  const Response* cresponse = const_cast<const Response*>(response);
  detector(response->spectrumNumber(),response);  
  m_uniqueEnergyFromResp->addRespEnergy(cresponse, m_areCompsSpecDependent);  
  //energy(response->spectrumNumber(),cresponse->energies());  

  // Note that we don't fill the external energy container at this 
  // point.  It is far more efficient to wait till after all 
  // attachResponse calls are made, which is why we do it instead 
  // from ModelContainer.
}

void ModelBase::fillEnergyContainer ()
{
  m_energy.clear();
  const std::set<UniqueEnergy*>* sourceOfEnergies = 
        m_autonomousEnergies.size() ? &m_autonomousEnergies : 
        &m_uniqueEnergyFromResp->uniqueEnergies();
  std::set<UniqueEnergy*>::const_iterator itUnique = 
        sourceOfEnergies->begin();
  std::set<UniqueEnergy*>::const_iterator itUniqueEnd = 
        sourceOfEnergies->end();
  while (itUnique != itUniqueEnd)
  {
     const RealArray& uniqueEnergy = (*itUnique)->energy();
     std::set<size_t>::const_iterator itSpecNum = 
                (*itUnique)->clientSpectra().begin();
     std::set<size_t>::const_iterator itSpecNumEnd = 
                (*itUnique)->clientSpectra().end();
     while (itSpecNum != itSpecNumEnd)
     {
        energy(*itSpecNum, uniqueEnergy);
        ++itSpecNum;
     }     
     ++itUnique;
  }
}

const std::set<UniqueEnergy*>& ModelBase::getUniqueEnergies () const
{
   if (m_autonomousEnergies.size())
      return m_autonomousEnergies;
   else
      return m_uniqueEnergyFromResp->uniqueEnergies();
}

void ModelBase::checkForSpecDependency ()
{
   m_areCompsSpecDependent = false;
   bool foundTrue = false;
   XSModExpTree<ComponentGroup*>::iterator itCG = m_compGroupTree.begin();
   XSModExpTree<ComponentGroup*>::iterator itCGEnd = m_compGroupTree.end();
   while (!foundTrue && itCG != itCGEnd)
   {
     // Nested groups represented by SumComponent pointers will
     // simply report false, which is fine.  We're only interested
     // in finding a single non-nested Component* that reports true,
     // in which case entire ModelBase will be flagged as true.
     const ComponentGroup* compGroup = *itCG;
     ComponentList::const_iterator itComp = compGroup->elements().begin();
     ComponentList::const_iterator itCompEnd = compGroup->elements().end();
     while (!foundTrue && itComp != itCompEnd)
     {
        const Component* comp = *itComp;
        if (comp->isSpectrumDependency())
        {
           m_areCompsSpecDependent = true;
           foundTrue = true;
        }
        ++itComp;
     } 
      ++itCG;
   }
}

void ModelBase::setAutonomousEnergy (const RealArray& energyArray)
{
   // This function will both set energies to a user specified energy
   // array, or will remove them if input array is size 0.
   std::set<UniqueEnergy*>::iterator itEng = m_autonomousEnergies.begin();
   std::set<UniqueEnergy*>::iterator itEngEnd = m_autonomousEnergies.end();
   while (itEng != itEngEnd)
   {
      delete *itEng;
      ++itEng;
   }
   m_autonomousEnergies.clear();

   if (energyArray.size())
   {
      UniqueEnergy* autonomousEnergy = new UniqueEnergy(energyArray);
      m_autonomousEnergies.insert(autonomousEnergy);
     // The client spectra for this autonomousEnergy will be set through 
     // attachResponses/reAttachResponses via setSpectraForAutonomousEngs 
     // function, once we know all the spectra associated with this model.
     // Initialize with dummy resp (spec=0) though since nothing is done
     // for that case in attachResponse functions.
      autonomousEnergy->addClient(0);
   }
}

void ModelBase::setAutonomousEnergy (const XSContainer::ExtendRange& extended)
{
   // Unlike the situation for the other setAutonomousEnergy function,
   // this one should only be called AFTER responses have been set for
   // models, since it must operate on the response energies.
   // It is NOT to be called from the Model constructor.
   using namespace XSContainer;

   std::set<UniqueEnergy*>::iterator itEng = m_autonomousEnergies.begin();
   std::set<UniqueEnergy*>::iterator itEngEnd = m_autonomousEnergies.end();
   while (itEng != itEngEnd)
   {
      delete *itEng;
      ++itEng;
   }
   m_autonomousEnergies.clear();

   std::set<UniqueEnergy*>::const_iterator itRespEng =
        m_uniqueEnergyFromResp->uniqueEnergies().begin();
   std::set<UniqueEnergy*>::const_iterator itRespEngEnd =
        m_uniqueEnergyFromResp->uniqueEnergies().end();
   while (itRespEng != itRespEngEnd)
   {
      RealArray respEnergy((*itRespEng)->energy());
      // This is a static function
      makeExtendArray(extended, respEnergy);
      UniqueEnergy* autonomousEnergy = new UniqueEnergy(respEnergy);
      m_autonomousEnergies.insert(autonomousEnergy);

      // Even though the energy array is modified, the clients are
      // still the same.
      std::set<size_t>::const_iterator itSp = (*itRespEng)->clientSpectra().begin();
      std::set<size_t>::const_iterator itSpEnd = (*itRespEng)->clientSpectra().end();
      while (itSp != itSpEnd)
      {
         autonomousEnergy->addClient(*itSp);
         ++itSp;
      }

      ++itRespEng;
   }
}

void ModelBase::setSpectraForAutonomousEngs ()
{
   // This assumes Responses are up to date for this model.
   // Do NOT call this when using extended energies.
   if (m_autonomousEnergies.begin() != m_autonomousEnergies.end())
   {
      // If we entered here, autonomous energies of the non-extended
      //  variety are in use.  There should be exactly 1 UniqueEnergy
      //  object in m_autonomousEnergies set, placed there by
      //  setAutonomousEnergies.
      
      // We must now assign ALL spectra using this Model object to this
      //   UniqueEnergy object, UNLESS we have the unusual case of a 
      //   spectrum-dependent model (eg. mix models). If that's the case
      //   we must now add a UniqueEnergy object for EACH spectrum.
      if (m_autonomousEnergies.size() != 1)
      {
         throw RedAlert("Autonomous energy set size error in setSpectraForAutonomousEngs.");
      }
      if (!m_uniqueEnergyFromResp->uniqueEnergies().size())
      {
         throw RedAlert("Response energy set size error in setSpectraForAutonomousEngs.");
      }
      UniqueEnergy* firstAutonomousEnergy = *m_autonomousEnergies.begin();
      firstAutonomousEnergy->removeAllClients();
      std::set<UniqueEnergy*>::const_iterator itEng = 
                m_uniqueEnergyFromResp->uniqueEnergies().begin();
      std::set<UniqueEnergy*>::const_iterator itEngEnd = 
                m_uniqueEnergyFromResp->uniqueEnergies().end();
      if (m_areCompsSpecDependent)
      {
         // If comps are spec dependent, we can assume each uniqueEnergyFromResponse
         //  has one and only one client spectrum.
         std::set<size_t>::const_iterator itSp = (*itEng)->clientSpectra().begin();
         firstAutonomousEnergy->addClient(*itSp);
         ++itEng;
         while (itEng != itEngEnd)
         {
            UniqueEnergy* autonomousEnergy = new UniqueEnergy(*firstAutonomousEnergy);
            autonomousEnergy->removeAllClients();
            itSp = (*itEng)->clientSpectra().begin();
            autonomousEnergy->addClient(*itSp);
            m_autonomousEnergies.insert(autonomousEnergy);
            ++itEng;
         }
      }
      else
      {
         while (itEng != itEngEnd)
         {
            const UniqueEnergy* respEnergy = *itEng;
            std::set<size_t>::const_iterator itSp = respEnergy->clientSpectra().begin();
            std::set<size_t>::const_iterator itSpEnd = respEnergy->clientSpectra().end();
            while (itSp != itSpEnd)
            {
               firstAutonomousEnergy->addClient(*itSp);
               ++itSp;
            }
            ++itEng;
         }
      }                
   }
}

void ModelBase::alignFluxForFold (ArrayContainer& saveFlux, ArrayContainer& saveFluxError, SumComponent* sourceComp)
{
   // If sourceComp != 0, algin SumComponent flux arrays, 
   //    else align entire modelFlux.
   // If Model is using energies from Responses, this function should do
   //    absolutely nothing.  
   // If Model is using autonomous energies:
   //    If saveFlux is empty, store the original fluxes in saveFlux
   //       and realign the fluxes to the Response energies.
   //    Else, restore the flux arrays with what is stored in saveFlux.
   using namespace XSContainer;
   if (m_autonomousEnergies.size())
   {
      bool isRestoring = static_cast<bool>(saveFlux.size());
      if (sourceComp)
      {
         std::swap(sourceComp->photonArray(),saveFlux);
         std::swap(sourceComp->photonErrArray(),saveFluxError);
      }
      else
      {
         std::swap(m_modelFlux, saveFlux);
         std::swap(m_modelFluxError, saveFluxError);
      }

      if (!isRestoring)
      {         
         // Assuming if not extended m_autonomousEnergies has only 1 array,
         //   or for case of model-spectrum dependency, multiple 
         //   m_autonomousEnergies arrays will all have the same values.
         const RealArray& autonEng = (*m_autonomousEnergies.begin())->energy();
         std::set<UniqueEnergy*>::const_iterator itUniqueEng =
                m_uniqueEnergyFromResp->uniqueEnergies().begin();
         std::set<UniqueEnergy*>::const_iterator itUniqueEngEnd =
                m_uniqueEnergyFromResp->uniqueEnergies().end();
         const Real FUZZY=1.0e-8;
         while (itUniqueEng != itUniqueEngEnd)
         {
            const RealArray& respEng = (*itUniqueEng)->energy();
            const std::set<size_t>& specNums = (*itUniqueEng)->clientSpectra(); 
            RealArray respFlux(.0, respEng.size()-1);
            RealArray respFluxError;
            const size_t nRespFlux = respFlux.size();
            bool hasError = false;
            // autonFlux can be grabbed from any of the client spectra
            // of this UniqueEnergy.  Let's just take the first.
            // (If it's a case of model-spectrum dependency, UniqueEnergies
            //  will have just one client anyway.)
            size_t firstSpec = *specNums.begin();
            const RealArray& autonFlux = saveFlux[firstSpec];

            if (models->isExtended())
            {
               const size_t nlBins = (respEng[0] > models->extendedEnergy().first.energy) ?
                  models->extendedEnergy().first.nBins : 0;
               const size_t nhBins = (respEng[nRespFlux] < models->extendedEnergy().second.energy) ?                  
                  models->extendedEnergy().second.nBins : 0;
               if (autonFlux.size() != nRespFlux + nlBins + nhBins)
               {
                  throw RedAlert("Extend mismatch in alignFluxForFold function.");
               }
               for (size_t i=0; i<nRespFlux; ++i)
                  respFlux[i] = autonFlux[i+nlBins];
               ArrayContainer::const_iterator itAutonErr =
                        saveFluxError.find(firstSpec);
               if (itAutonErr != saveFluxError.end())
               {
                  hasError = true;
                  const RealArray& autonError = itAutonErr->second;
                  if (autonError.size() == nRespFlux)
                  {
                     respFluxError.resize(nRespFlux);
                     for (size_t i=0; i<nRespFlux; ++i)
                        respFluxError[i] = autonError[i+nlBins];
                  }
               }
            } // end if using extended energies
            else
            {
               size_t inStart=0, outStart=0;
               if (Numerics::Rebin::findFirstBins(autonEng, respEng,
                           FUZZY, inStart, outStart))
               {
                  IntegerArray startBin, endBin;
                  RealArray startWeight, endWeight;  
                  Numerics::Rebin::initializeBins(autonEng, respEng, FUZZY, 
                       inStart, outStart, startBin, endBin, startWeight, endWeight);
                  Numerics::Rebin::rebin(autonFlux, startBin, endBin,
                       startWeight, endWeight, respFlux);
                  ArrayContainer::const_iterator itAutonErr = 
                           saveFluxError.find(firstSpec);
                  // m_modelFluxError should have a valarray for each specNum
                  // even if there are no model errors.  The valarray will
                  // be size 0 in this case.
                  if (itAutonErr != saveFluxError.end())
                  {
                     hasError = true;
                     const RealArray& autonError = itAutonErr->second;                  
                     if (autonError.size() == autonFlux.size())
                     {
                        respFluxError.resize(respFlux.size(),.0);
                        Numerics::Rebin::rebin(autonError, startBin, endBin,
                             startWeight, endWeight, respFluxError);
                     }
                  }
               }
               else
               {
                  // There's no overlap between autonomous and response energy,
                  // so we can't interpolate to perform fold.  We must restore
                  // the original flux arrays before throwing.
                  std::ostringstream oss;
                  oss << "Cannot convolve model with response.  There is no overlap"
                     <<"\n    between the energies command array and RMF energies for spectrum "
                     << firstSpec << "\n(Enter \"energies reset\" to restore original energies.)\n" <<std::endl;
                  if (sourceComp)
                  {
                     std::swap(sourceComp->photonArray(),saveFlux);
                     std::swap(sourceComp->photonErrArray(),saveFluxError);
                  }
                  else
                  {
                     std::swap(m_modelFlux, saveFlux);
                     std::swap(m_modelFluxError, saveFluxError);
                  }
                  throw YellowAlert(oss.str());                  
               }
            } // end if NOT using extended energies
            std::set<size_t>::const_iterator itSpecs = specNums.begin();
            std::set<size_t>::const_iterator itSpecsEnd = specNums.end();
            while (itSpecs != itSpecsEnd)
            {
               size_t specNum = *itSpecs;
               if (saveFlux.find(specNum) == saveFlux.end())
               {
                  throw RedAlert("Flux map mismatch in alignFluxForFold function.\n");
               }
               // For valarrays, first insert then resize and assign.
               RealArray& inserted = sourceComp ? 
                                sourceComp->photonArray()[specNum] : m_modelFlux[specNum];
               inserted.resize(respFlux.size());
               inserted = respFlux;
               if (hasError)
               {
                  RealArray& insertedError = sourceComp ? 
                        sourceComp->photonErrArray()[specNum] : m_modelFluxError[specNum];
                  insertedError.resize(respFluxError.size());
                  insertedError = respFluxError;
               }
               ++itSpecs;
            }
            ++itUniqueEng;
         }
      } // end if not restoring saved flux
   }
}

void ModelBase::updateNewGainFromFit (const Response* response)
{
   // Remember, Responses with gain fit parameters do not share their
   // UniqueEnergy objects with anyone.  If they do, the following call
   // will trigger a RedAlert.
   m_uniqueEnergyFromResp->updateEnergyFromResponse(response);
   
   // If extended energies are in use, must also make conforming update to Model's
   // m_autonomousEnergies.  Can't do this using the setAutonomousEnergy function
   // since that destroys and rebuilds the UniqueEnergy objects in
   // m_autonomousEnergies -- wreaking havoc on the Component uniquePhotonArray
   // maps which use the UniqueEnergy pointers as keys.  The setAutonomousEnergy
   // function is intended for use only in the Notify/Update sequence, whereas
   // this is called during fit iterations.
   
   if (XSContainer::models->isExtended())
   {
      // Need to find which UniqueEnergy in m_autonomousEnergies corresponds
      // to this response.  This is O(N).
      UniqueEnergy* uniqEng = 0;
      const size_t specNum = response->spectrumNumber();
      std::set<UniqueEnergy*>::iterator itUniq = m_autonomousEnergies.begin();
      std::set<UniqueEnergy*>::iterator itUniqEnd = m_autonomousEnergies.end();
      while (!uniqEng && itUniq != itUniqEnd)
      {
         const std::set<size_t>& clientSpecs = (*itUniq)->clientSpectra();
         if (clientSpecs.find(specNum) != clientSpecs.end())
            uniqEng = *itUniq;
         else
            ++itUniq;
      }
      if (!uniqEng)
         throw RedAlert("Unable to locate array in updateNewGainFromFit.");
      
      RealArray newEnergies(response->energies());
      makeExtendArray(XSContainer::models->extendedEnergy(),newEnergies);
      uniqEng->changeEnergy(newEnergies);
   }
}

void ModelBase::bundleParameters (std::vector<Parameter*>& parameters) const
{
   std::vector<Component*> components;
   bundleComponents(components);
   for (size_t i=0; i<components.size(); ++i)
   {
      const Component* cComp = components[i];
      const std::vector<Parameter*>& paramSet = cComp->parameterSet();
      parameters.insert(parameters.end(), paramSet.begin(), paramSet.end());      
   }
}

void ModelBase::bundleComponents (std::vector<Component*>& components) const
{
   XSModExpTree<ComponentGroup*>::const_iterator itCg = m_compGroupTree.begin();
   XSModExpTree<ComponentGroup*>::const_iterator itCgEnd = m_compGroupTree.end();
   while (itCg != itCgEnd)
   {
      bundleSubTreeComponents(components, itCg);
      ++itCg;
   }
}

void ModelBase::bundleSubTreeComponents (std::vector<Component*>& components, XSModExpTree<ComponentGroup*>::const_iterator& itCg) const
{
   // A recursive helper function needed for bundleComponents.
   XSModExpTree<ComponentGroup*>::const_iterator itCgEnd = m_compGroupTree.end();
   if (itCg != itCgEnd)
   {
      ComponentListConstIterator itComp = (*itCg)->elements().begin();
      ComponentListConstIterator itCompEnd = (*itCg)->elements().end();
      while (itComp != itCompEnd)
      {
         if ((*itComp)->index() > 0)
         {
            components.push_back(*itComp);
         }
         else
         {
            ++itCg;
            bundleSubTreeComponents(components, itCg);
         }         
         ++itComp;
      }
   }
}

void ModelBase::makeExtendArray (const XSContainer::ExtendRange& extRange, RealArray& extArray)
{
    using namespace XSContainer;

    const ExtendRecord& lowExt = extRange.first;
    const ExtendRecord& highExt = extRange.second;
    const size_t nREngs = extArray.size();
    // Can nREngs = 0 even exist by this point? Check anyway.
    if (!nREngs)
    {
       string msg("A response has been detected with 0 energy bins.  Cannot run extend command.\n");
       throw YellowAlert(msg);
    }
    if (extArray[0] > extArray[nREngs-1])
    {
       string msg("An extension cannot be applied to an energy array which is in descending order.\n");
       throw YellowAlert(msg);
    }


    bool doExtendLow = false, doExtendHigh = false;
    int hIdx = static_cast<int>(nREngs)-1;
    doExtendLow = lowExt.energy < extArray[0];
    doExtendHigh = highExt.energy > extArray[hIdx];
    RealArray lowEnergies;
    RealArray highEnergies;
    // This is an obsolete boolean since reverse is no longer allowed.
    // Need to change makeExtendArrayHelper's interface at some point.
    bool dummyIsReverse = false;
    if (doExtendLow)
    {
       makeExtendArrayHelper(lowExt, extArray[0], true, dummyIsReverse, lowEnergies);
    } 
    if (doExtendHigh)
    {
       makeExtendArrayHelper(highExt, extArray[hIdx], false, dummyIsReverse, highEnergies);
    }

    const size_t newSize = lowEnergies.size() + highEnergies.size()
                              + extArray.size();
    RealArray newlyExtended(.0, newSize);
    // Note that hIdx will now refer to 1 beyond the array end
    hIdx = static_cast<int>(lowEnergies.size());
    int curIdx = 0;
    for (int i=0; i != hIdx; ++i)
    {
       newlyExtended[curIdx] = lowEnergies[i];
       ++curIdx;
    }
    hIdx = static_cast<int>(extArray.size());
    for (int i=0; i != hIdx; ++i)
    {
       newlyExtended[curIdx] = extArray[i];
       ++curIdx;
    }
    hIdx = static_cast<int>(highEnergies.size());
    for (int i=0; i != hIdx; ++i)
    {
       newlyExtended[curIdx] = highEnergies[i];
       ++curIdx;
    }
    extArray.resize(newlyExtended.size());
    extArray = newlyExtended;
}

void ModelBase::makeExtendArrayHelper (const XSContainer::ExtendRecord& extendInfo, Real edge, bool isLow, bool isReverse, RealArray& extendedArray)
{
   // isReverse is no longer used, interface change required.
   // Descending order energies are no longer allowed.
   // ASSUMES edge relation to extendInfo.energy has already been
   // validated, and nBins is not 0.
   extendedArray.resize(extendInfo.nBins);
   const bool fillBackwards = !isLow;
   const int lIdx = fillBackwards ? static_cast<int>(extendInfo.nBins) - 1 : 0;
   const int hIdx = fillBackwards ? -1 : static_cast<int>(extendInfo.nBins);
   const int incr = fillBackwards ? -1 : 1;
   Real binEnergy = extendInfo.energy;
   if (extendInfo.isLog)
   {
      const Real binWidth = log(edge/extendInfo.energy)/extendInfo.nBins;
      for (int i=lIdx; i != hIdx; i += incr)
      {
         extendedArray[i] = binEnergy;
         // binWidth is neg for high extension.
         binEnergy *= exp(binWidth);
      }
   }
   else
   {
      const Real binWidth = (edge - extendInfo.energy)/extendInfo.nBins;
      for (int i=lIdx; i != hIdx; i += incr)
      {
         extendedArray[i] = binEnergy;
         // binWidth is neg for high extension.
         binEnergy += binWidth;
      }
   }
}

const RealArray& ModelBase::foldedModel (size_t spectrumNumber) const
{
  ArrayContainer::const_iterator f = m_foldedModel.find(spectrumNumber);
  if (f != m_foldedModel.end())
  {
        return f->second;
  }
  else
  {
     std::ostringstream oss;
     oss <<"Cannot find folded model spectrum " 
       << spectrumNumber<<".\n";   
     throw YellowAlert(oss.str());       
  }      
}

const RealArray& ModelBase::modelFlux (size_t spectrumNumber) const
{
  ArrayContainer::const_iterator f = m_modelFlux.find(spectrumNumber);
  if (f != m_modelFlux.end())
  {
        return f->second;
  }
  else
  {
     std::ostringstream oss;
     oss <<"Cannot find model flux spectrum " 
       << spectrumNumber<<".\n";   
     throw YellowAlert(oss.str());       
  }
}

const RealArray& ModelBase::modelFluxError (size_t spectrumNumber) const
{
  ArrayContainer::const_iterator f = m_modelFluxError.find(spectrumNumber);
  if (f != m_modelFluxError.end())
  {
        return f->second;
  }
  else
  {
     std::ostringstream oss;
     oss <<"Cannot find model flux error array for spectrum " 
       << spectrumNumber<< std::endl;   
     throw YellowAlert(oss.str());       
  }      
}

const Response* ModelBase::detector (size_t spectrumNumber) const
{
  std::map<size_t,Response*>::const_iterator m(m_detector.find(spectrumNumber));
  if (m == m_detector.end()) return 0;
  else return m->second;
}

void ModelBase::detector (size_t spectrumNumber, Response* value)
{
  std::map<size_t,Response*>::iterator f = m_detector.find(spectrumNumber);
  if ( f != m_detector.end())
  {
        m_detector.erase(f);
  }
  m_detector.insert(std::map<size_t,Response*>::value_type(spectrumNumber,value));       
}

const RealArray& ModelBase::foldedModelError (size_t spectrumNumber) const
{
  ArrayContainer::const_iterator f = m_foldedModelError.find(spectrumNumber);
  if (f != m_foldedModelError.end())
  {
        return f->second;
  }
  else
  {
     std::ostringstream oss;
     oss <<"Cannot find folded model error spectrum " 
       << spectrumNumber<<".\n";   
     throw YellowAlert(oss.str());       
  }    
}

const RealArray& ModelBase::energy (size_t spectrumNumber) const
{
  ArrayContainer::const_iterator f = m_energy.find(spectrumNumber);
  if (f != m_energy.end())
  {
        return f->second;
  }
  else
  {
     std::ostringstream oss;
     oss <<"Cannot find energy spectrum " 
       << spectrumNumber<<".\n";   
     throw YellowAlert(oss.str());       
  }    
}

void ModelBase::checkZeroNorms (std::set<int>& parsWithZeroNorms)
{
   XSModExpTree<ComponentGroup*>::iterator itCG=m_compGroupTree.postBegin();
   XSModExpTree<ComponentGroup*>::iterator itCGEnd=m_compGroupTree.end();
   while (itCG != itCGEnd)
   {
      (*itCG)->checkZeroNorms(parsWithZeroNorms);
      itCG.postOrderNext();
   }
}

bool ModelBase::checkParameterMagnitudes(Real threshold) const
{
   std::vector<Parameter*> parameters;
   bundleParameters(parameters);
   Real highVal=1.0e-99;
   Real lowVal=1.0e99;
   bool isExceeded = false;
   for (size_t iPar=0; iPar<parameters.size(); ++iPar)
   {
      const ModParam* modPar = dynamic_cast<ModParam*>(parameters[iPar]);
      if (modPar)
      {
         if (modPar->value() != 0.0)
         {
            Real magnitude = fabs(modPar->value());
            if (magnitude < lowVal)
               lowVal = magnitude;
            if (magnitude > highVal)
               highVal = magnitude;
         }
      }
   }
   // If no non-zero ModParams, highVal will be less than lowVal.
   if (highVal > lowVal)
   {
      isExceeded = ((highVal/lowVal) > threshold);
   }
   
   return isExceeded;
}

// Additional Declarations
