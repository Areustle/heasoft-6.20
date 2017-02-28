//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// Response
#include <XSModel/Data/Detector/Response.h>
// DataSet
#include <XSModel/Data/DataSet.h>
// Component
#include <XSModel/Model/Component/Component.h>
// MdefContainer
#include <XSModel/GlobalContainer/MdefContainer.h>
// Parameter
#include <XSModel/Parameter/Parameter.h>
// ResponseContainer
#include <XSModel/GlobalContainer/ResponseContainer.h>
// DataContainer
#include <XSModel/GlobalContainer/DataContainer.h>
// ModelContainer
#include <XSModel/GlobalContainer/ModelContainer.h>

#include <XSModel/Data/SpectralData.h>
#include <XSModel/Model/CompCombiner.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/MixComponent.h>
#include <XSModel/Model/Component/AMXComponent.h>
#include <XSModel/Model/Component/SumComponent.h>
#include <XSModel/Parameter/ParameterLink.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <algorithm>
#include <memory>
#include <sstream>
#include <cmath>

namespace XSContainer {
    ModelContainer* models = 0;

    // Class XSContainer::ModelContainer::NoSuchParameter 

    ModelContainer::NoSuchParameter::NoSuchParameter (int index, const string& modelName)
        : YellowAlert(" No Such Parameter: ")         
    {
  if (!modelName.empty()) tcerr << "\nModel: "<< modelName << ':';
  tcerr << index << '\n';
    }


    // Class XSContainer::ModelContainer 
    ModelContainer* ModelContainer::s_instance = 0;
    const size_t ModelContainer::s_SHIFT = 16;

    ModelContainer::ModelContainer()
      : m_modelSet(),  
        m_parameterLookupTable(), 
        m_uniqueNamedModel(0), 
        m_parameterReverseLookup(),
        m_modelSystematicError(0),
        m_autonomousEnergy(),
        m_modelForSource(),
        m_extendedEnergy(),
        m_proportionalDelta(-1.0),
        m_cosmo(),
        m_userMdefs(0),
        m_parameterList(),
        m_modelLookupTable(), 
        m_activeModelNames()
    {

       // Register the ModelContainer as an observer of the DataContainer.
       // This will create the DataContainer if it doesn't already exist.
       datasets = DataContainer::Instance();
       datasets->Attach(this);

       m_userMdefs = new MdefContainer();
    }


    ModelContainer::~ModelContainer()
    {

      // cleanup.

      ModelMapIter n (m_modelSet.begin());
      ModelMapIter nEnd (m_modelSet.end());

      while (n != nEnd )
      {
          delete n->second;
          ++n;       
      }

      delete m_userMdefs;
    }


    ModelContainer* ModelContainer::Instance ()
    {
      if (s_instance == 0) {s_instance = new ModelContainer;} return s_instance;






    }

    void ModelContainer::Update (Subject* data)
    {
      using namespace std;
      // At present, the assumption is that this code is only reached from an
      // external object (ie DataContainer) calling its Notify function, not
      // from one of ModelContainer's own functions. Other assumptions to note: 
      // ALL model objects with the same model name will have the same sourceNumber.  

      map<string,vector<Model*> > doomed;
      map<string,vector<Model*> > alive;
      map<string, vector<size_t> > needed;
      determineModelObjectStatus(doomed, alive, needed);
      map<string,bool>::const_iterator itActMod = m_activeModelNames.begin();
      map<string,bool>::const_iterator itActEnd = m_activeModelNames.end();
      vector<Model*> doomedModVecs;
      while (itActMod != itActEnd)
      {
         const string& modName = itActMod->first;
         if (itActMod->second)
         {
            doomedModVecs.insert(doomedModVecs.end(), doomed[modName].begin(),
                        doomed[modName].end());
         }
         ++itActMod;
      }
      rerouteBrokenParLinks(doomedModVecs);

      itActMod = m_activeModelNames.begin();
      while (itActMod != itActEnd)
      {
         const string& modName = itActMod->first;
         if (itActMod->second)
         {
            deregisterModelParameters(modName);
            vector<Model*>& doomedVec = doomed[modName];
            vector<Model*>& aliveVec = alive[modName];
            const vector<size_t>& neededVec = needed[modName];
            Model* lowestExistingMod = 0;
            size_t lowestExistingDg = string::npos;
            for (size_t i=0; i<aliveVec.size(); ++i)
            {
               if (aliveVec[i]->dataGroupNumber() < lowestExistingDg)
               {
                  lowestExistingMod = aliveVec[i];
                  lowestExistingDg = lowestExistingMod->dataGroupNumber();
               }
            }
            vector<Model*> dataGroupCopies;
            // Unlike doomed and alive vectors, neededVec is in
            // order sorted by data group numbers.
            if (!neededVec.empty())
            {
               vector<size_t>::const_iterator itNeeded = neededVec.begin();
               size_t lowestNewCopy = *itNeeded;
               Model* lowestNewMod = 0;
               if (lowestNewCopy < lowestExistingDg)
               {
                  if (lowestExistingMod)
                  {
                     lowestNewMod = lowestExistingMod->clone();
                     lowestNewMod->setDataGroupIndexing(lowestNewCopy);
                     // addToList will also register parameters in global
                     // lists.  But that's OK here since lowestNewMod won't
                     // need any parameter reindexing.
                     addToList(lowestNewMod);
                  }
                  else
                  {
                     // Need an existing model from somewhere. If nothing
                     // is alive, there must be least 1 in doomed.
                     if (doomedVec.empty())
                     {
                        throw RedAlert("No pre-existing model found in ModelContainer Update.");
                     }
                     lowestNewMod = doomedVec[0];
                     // Don't want this object to get destroyed later with 
                     // rest of doomed array now that we're re-using it.
                     doomedVec[0] = 0;
                     vector<Parameter*> bundledPars;
                     lowestNewMod->bundleParameters(bundledPars);
                     for (size_t i=0; i<bundledPars.size(); ++i)
                     {
                        bundledPars[i]->isModelDoomed(false);
                     }
                     lowestNewMod->setDataGroupIndexing(lowestNewCopy);
                     lowestNewMod->reindexParameters();
                     lowestNewMod->registerParameters();
                  }
                  ++itNeeded;
                  const vector<size_t> stillNeeded(itNeeded,neededVec.end());
                  dataGroupCopies =
                          lowestNewMod->makeDataGroupCopies(stillNeeded);
               }
               else
               {
                  // If in here, lowestExistingMod must point
                  // to a valid pre-existing model.
                  dataGroupCopies = 
                     lowestExistingMod->makeDataGroupCopies(neededVec); 
               }               
            } // end if !neededVec.empty()
            // Models contained in alive vector have already
            // had their group numbers updated (via 
            // determineModelObjectStatus), but may need reindexing
            // and link updating prior to re-registering.
            for (size_t i=0; i<aliveVec.size(); ++i)
            {
               Model* persistMod = aliveVec[i];
               persistMod->setDataGroupIndexing(persistMod->dataGroupNumber());
               persistMod->reindexParameters();
               vector<Parameter*> persistParams;
               persistMod->bundleParameters(persistParams);
               for (size_t j=0; j<persistParams.size(); ++j)
               {
                  if (persistParams[j]->isLinked())
                     persistParams[j]->thisLink()->regenerateExpression();
               }
               persistMod->registerParameters();
            }
            // Link all data group copies to lowest mod.  None of
            // these will need reindexing.
            for (size_t i=0; i<dataGroupCopies.size(); ++i)
            {
               dataGroupCopies[i]->linkDataGroupParams();
               addToList(dataGroupCopies[i]);
            }                               
            for (size_t i=0; i<doomedVec.size(); ++i)
            {
               if (doomedVec[i])
               {
                  remove(modName, doomedVec[i]->dataGroupNumber());
               }
            }

            // Clear out any old response pointers.
            ModelMapIter itMod = m_modelSet.lower_bound(modName);
            ModelMapIter itModEnd = m_modelSet.upper_bound(modName);
            while (itMod != itModEnd)
            {
               itMod->second->makeInactive(false);
               ++itMod;
            }
            // Attach these same models to their new response objects.
            reAttachResponses(modName);
            // Reinitialize mixing models.  Does nothing if there isn't
            // one present.
            initializeMixingTransformation(modName);
         } // end if model is marked active
         ++itActMod;
      }
      // This does nothing if energy extensions aren't set,
      applyExtendedEnergies();

      // Observers may be interested.    
      Notify();
    }

    void ModelContainer::addToList (Model* newModel)
    {
          // add a model to the model list and to the global parameter lists.
          // newModel must be an owning pointer.

          const string& inputName = newModel->name();

          m_modelSet.insert(ModelMap::value_type(inputName,newModel));

          registerModel(newModel);
    }

    void ModelContainer::addParameterToList (const string& key, Parameter* value)
    {
          // Called by ComponentBase::registerParameters
          // also the key value needs to be encoded above and passed to this function
          // since Parameter doesn't have all the information to construct the key.
          std::ostringstream keyStr;
          keyStr << key << '$' << value->dataGroup();
          parameterList(keyStr.str(),value);
          string modelName(key.substr(0,key.find_first_of(':')));
          size_t lookupIndex = (m_modelLookupTable[modelName] << s_SHIFT) + value->index();
          addParameterLookup(lookupIndex,keyStr.str());  
    }

    void ModelContainer::deregisterModelParameters (const string& keyString)
    {
          // erase all keys beginning with the keyString: they are found
          // using SubStringMatch which finds keys. 
          // it returns the number of map elements deleted.
          // note the type passed in the template parameter to Match1stKeyString!

          // m finds the highest index number of the parameters deleted,
          // to allow the reindex operation to modify all the values
          // larger than itself.
          XSutility::Match1stKeyString<std::pair<const string,Parameter*> > mapMatch;
          ParamMapIter i0 = m_parameterList.begin();
          while ((i0=find_if(i0,m_parameterList.end(),bind2nd(mapMatch,keyString)))
	  		!= m_parameterList.end())
          {
              m_parameterList.erase(i0++);
          }

          // this is: erase all keys starting with "keyString", which is a model
          // name.

          XSutility::MatchSubKey<std::pair<const string,int> > revMap;
          std::map<string,int>::iterator rl0    = m_parameterReverseLookup.begin();
          while ((rl0 = find_if(rl0,m_parameterReverseLookup.end(),bind2nd(revMap,keyString)))
	  		 != m_parameterReverseLookup.end())
          {
	        int val = rl0->second;
// debugging stmt
// string key = rl0->first;
// tcerr << "EraseModPar " << rl0->first << " " << rl0->second << '\n';
                m_parameterReverseLookup.erase(rl0++);
                m_parameterLookupTable.erase(m_parameterLookupTable.find(val));
          }    
    }

    void ModelContainer::reindexParameterList (int first, int reduceBy)
    {
          ParamMapIter i = m_parameterList.begin();
          ParamMapIter iEnd = m_parameterList.end();
          while (i != iEnd)
          {
                if (int j = static_cast<int>(i->second->index()) > first ) 
                {
                    Parameter* x = i->second; 
                    x->index(j-reduceBy);
                    addParameterLookup(j-reduceBy,i->first);
                } 
                ++i;  

          }
    }

    Parameter* ModelContainer::lookupParameter (int index, const string& model) const
    {
          string paramName(indexToName(index,model));

          if (!paramName.empty()) 
          {
                return lookupParameter(paramName);
          }
          else return 0;  
    }

    Parameter* ModelContainer::lookupParameter (const string& qualKey) const
    {
          // Entries in the lookup table are of the form 
          // parameter name =  <MODEL>:<COMPONENT[n]>:<PARAMETER>$<group#>
          // where n indicates multiplicity of a component within a model.
          //   argument is a 'qualified key'.
          ParamMapConstIter f = m_parameterList.find(qualKey);
          if (f != m_parameterList.end())
          {
                return f->second;  
          }
          else return 0;  
    }

    void ModelContainer::addParameterLookup (int index, const string& key)
    {
          m_parameterLookupTable[index] = key;
          m_parameterReverseLookup[key] = index;
    }

    void ModelContainer::remove (const string& name)
    {
          size_t n(m_modelSet.count(name));
          if ( n > 0 )
          {
                  deregisterModelParameters(name);           
                  ModelMapIter first(m_modelSet.lower_bound(name));
                  ModelMapIter last(m_modelSet.upper_bound(name));
                  size_t sourceNum = first->second->sourceNumber();
                  // This may not be the active mod for this source,
                  // need to check.
                  std::map<size_t,string>::iterator itSource = 
                        m_modelForSource.find(sourceNum);
                  if (itSource != m_modelForSource.end() &&
                                itSource->second == first->second->name())
                        m_modelForSource.erase(itSource);
                  ModelMapIter i = first;
                  while ( i != last ) 
                  {
                          delete i->second;
                          ++i;
                  }

                  m_modelSet.erase(first,last);
                  m_modelLookupTable.erase(name);

                  m_activeModelNames.erase(name);
          }
    }

    void ModelContainer::remove (const string& name, size_t group)
    {

      // to be used for removing a set of models associated with a data
      // group when that data group is removed.      
      size_t n(m_modelSet.count(name));
      if ( n > 0 )
      {
         std::pair<ModelMapIter,ModelMapIter> range = m_modelSet.equal_range(name);
         ModelMapIter i(range.first);
         while ( i != range.second) 
         {
            if (i->second->dataGroupNumber() == group) 
            {
               // grab pointer and memory ownership. The following
               // operations consists of list erasures of tokens and
               // will not throw. 
               delete i->second;
               deregisterModelParameters(name,group);
               m_modelSet.erase(i++);
               break;

            }
            else ++i;
         }
      }  
    }

    Model* ModelContainer::lookup (const string& name, size_t group)
    {
       Model* result(0);
       const size_t n(m_modelSet.count(name));
       if ( n > 0 )
       {
          std::pair<ModelMapIter,ModelMapIter> range = m_modelSet.equal_range(name);
          ModelMapIter i(range.first);
          const size_t sourceNum = i->second->sourceNumber();
          if (!group)
          {
             // Treat this as meaning: "get the lowest data group model obj,
             // whatever group it belongs to."
             if (n == 1)
                result = i->second;
             else
                group = XSContainer::datasets->getLowestGroupForSource(sourceNum);
          }
          if (!result)
          {
             while ( i != range.second) 
             {
                Model*& current(i->second);
                if (current->dataGroupNumber() == group) 
                {
                   result = current;   
                   break;
                }
                ++i;
             }
          }
       }
       return result;
    }

    size_t ModelContainer::incrementModelCounter ()
    {
      ++m_uniqueNamedModel;
      return m_uniqueNamedModel;
    }

    void ModelContainer::clearLists ()
    {
          if (!m_modelSet.empty())
          {
                ModelMapIter m (m_modelSet.begin());
                ModelMapIter mEnd (m_modelSet.end());
                while (m != mEnd) 
                {
                        delete m->second;
                        ++m;
                }   
                m_modelSet.clear();
                m_parameterList.clear();
                m_modelLookupTable.clear();
                m_parameterLookupTable.clear();
                m_parameterReverseLookup.clear();
                m_uniqueNamedModel = 0;
                m_modelForSource.clear();
		m_activeModelNames.clear();
          }
    }

    void ModelContainer::deregisterModelParameters (const string& keyString, size_t group)
    {
       // NOTE:  This function may be called after the deletion of the owning Model object
       // (an example being from xsModel's setFirstOfModelName call, wherein it may replace
       // a previous model and destroy its higher numbered data groups).  Therefore we 
       // MUST NOT ASSUME that it->second is still a valid Parameter pointer.
       XSutility::Match1stKeyString<std::pair<const string, Parameter*> > mapMatch;
       ParamMapIter it = find_if(m_parameterList.begin(),m_parameterList.end(),
                                     bind2nd(mapMatch,keyString));
       while (it != m_parameterList.end() )
       {
          const string::size_type dollarPos = it->first.rfind('$');
          const string groupStr = it->first.substr(dollarPos+1);
          std::istringstream iss(groupStr);
          size_t parGroup = 0;
          iss >> parGroup;
	  if (parGroup == group)
	  {
             std::map<string,int>::iterator rlf = m_parameterReverseLookup.find(it->first);
             m_parameterLookupTable.erase(rlf->second); // std::map<int,string>::erase(key&);
             m_parameterReverseLookup.erase(rlf); // std::map<string,int>::erase(iterator); 
             m_parameterList.erase(it++); // std::map<string,Parameter*>::erase(iterator);
	  }
          else
          {
             ++it;
          }
          it = find_if(it,m_parameterList.end(),bind2nd(mapMatch,keyString));
       }  
    }

    bool ModelContainer::isModel (const string& name)
    {
        return (m_modelLookupTable.find(name) != m_modelLookupTable.end());
    }

    void ModelContainer::attachResponses (const string& modelName) throw (YellowAlert&)
    {
       using XSContainer::responses;

       // Only do this if more than the Dummy response has been loaded.
       if (responses->numberOfResponses() > 1)
       {
          ModelMapIter m,last;
          // NOTE: this function is NOT reached from the Update, but
          // only from the Model command handler.  Thus, m and last
          // form the boundaries of newly entered models w/ modelName.

          m = m_modelSet.lower_bound(modelName);       

          // Because of the remove call above, the upper_bound
          // iterator must be assigned here to be safe.
          last = m_modelSet.upper_bound(modelName); 

          while (m != last)
          {
             ResponseMapConstIter r(responses->responseList().begin());
             ResponseMapConstIter rEnd(responses->responseList().end());
             Model*& currentModel = m->second;
	     currentModel->setComputeFlag(true);
             size_t group(currentModel->dataGroupNumber());
	     size_t sourceNum(currentModel->sourceNumber());
	     bool isClean = false;
             while (r != rEnd)
             {
                if (r->second->dataGroup() == group && 
		        r->second->sourceNumber() == sourceNum)
                {
		   // First remove its initial dummy
		   // response (spectrum = 0).  (This
		   //only needs to be called once per model)
		   if (!isClean)
		   {
		      currentModel->removeResponse(0);
		      isClean = true;
		   }
                   currentModel->attachResponse(r->second);
                }
                ++r;       
             }
             if (!isExtended())
                currentModel->setSpectraForAutonomousEngs();
             currentModel->fillEnergyContainer();
             ++m;
          }
       }    
    }

    void ModelContainer::calculate (const string& modelName, bool saveComponentFlux, bool onlyFrozenNorms) const
    {
       std::vector<Model*> modGroups = lookupModelGroup(modelName);
       for (size_t iMod=0; iMod<modGroups.size(); ++iMod)
       {
          modGroups[iMod]->calcComponents(saveComponentFlux, onlyFrozenNorms);
       }
       CompCombiner combiner(modGroups);
       CompCombiner::iterator itComb = combiner.begin();
       CompCombiner::iterator itCombEnd = combiner.end();
       while (itComb != itCombEnd)
       {
          ++itComb;
       } 
    }

    void ModelContainer::setParameterFromPrompt (int index, const string& modelName, const string& inputArg) const
    {
          if (!inputArg.empty())
          {
                // process the input string
                Parameter* toSet = lookupParameter(index,modelName);
                if (toSet)
                {
                        std::istringstream s (inputArg);
                        s >> *toSet; 
                }  
                else throw NoSuchParameter(index,modelName);    
          }
          else
          {
                string parN;
                string paramName(indexToName(index,modelName));
                Parameter* toSet = lookupParameter(index,modelName); 
                if (toSet)
                {
                        using std::ios_base;
                        string input("");
                       // tcout << "Current Values: " << *toSet << '\n';
                        tcout << "Current value, delta, min, bot, top, and max values\n"
                              << toSet->parameterSetting() << std::endl;
                        std::ostringstream promptString;
                        promptString << toSet->index() << ':';
                        if (paramName.find(Model::DEFAULT()) != string::npos) 
                        {
                           parN = paramName.substr(Model::DEFAULT().size() + 1);
                        }
                        else
                           parN = paramName;
                        string::size_type dollarPos = parN.rfind('$'); 
                        if (dollarPos != string::npos)
                        {
                           parN[dollarPos] = ':';
                        } 
                        promptString << parN << '>';
                        XSstream::setPrompter(tcin,promptString.str());
                        try
                        {
                                tcin.exceptions(ios_base::badbit | ios_base::failbit | ios_base::eofbit );
                                getline(tcin,input);
                                if (input.empty()) throw XSparse::SkipThis("");
                                XSparse::catchSkips(input); 
                                std::istringstream s(input);
                                s >> *toSet;
                                // changing a parameter value changes the fit, not
                                // the model container.
                        }
                        catch (ios_base::failure &)
                        {
                                tcin.clear(ios_base::goodbit);
                                throw XSparse::SkipThis("");
                        }                
                }             
                else throw NoSuchParameter(index,modelName);    

          }
    }

    string ModelContainer::indexToName (int index, const string& modelName) const
    {
          string parName("");
          static const int modelNameDetect = 1 << s_SHIFT;
          if (index < modelNameDetect)
          {      
                  if ( !(modelName.empty() || modelName == Model::DEFAULT()))
                  {
                     size_t modIndex = modelLookupTable(modelName);
                     if (!modIndex)
                     {
                        // modelName is not found
                        return parName;
                     }   
                     index += (modelLookupTable(modelName) << s_SHIFT);
                  } 
          }     

          std::map<int,string>::const_iterator f = m_parameterLookupTable.find(index);
          if (f != m_parameterLookupTable.end()) parName = f->second;
          return parName;
    }

    void ModelContainer::setCompute (const string& name, bool value)
    {
          ModelMapIter m(m_modelSet.lower_bound(name));       
          ModelMapIter last(m_modelSet.upper_bound(name));  
          while (m != last)
          {
                  m->second->setComputeFlag(value);
                  ++m;
          }
    }

    int ModelContainer::keyToIndex (const string& key) const
    {
      std::map<string,int>::const_iterator reverseLookup = m_parameterReverseLookup.find(key);
      if (reverseLookup != m_parameterReverseLookup.end())
      {
              return reverseLookup->second;
      }
      else return XSparse::NOTFOUND();
    }

    void ModelContainer::setFirstOfModelName (Model* newModel)
    {
          const string& inputName = newModel->name();
          size_t n(m_modelSet.count(inputName));

          //If previous copies of the same name exist, as a result of
	  //previous Model commands, clean out all instances of the old 
	  //model objects from the global lists. this is not strongly 
	  //exception safe but it relies on standard library container 
	  //guarantees to give some degree of exception safety: 
	  //erasures are guaranteed not to throw, for example. If no 
	  //previous copies, set its unique name index unless it's the 
	  //default model name. That MUST have index 0. Refer to 
	  //developer notes: $HEADAS/../Xspec/doc/notes/XSPEC12.doc.

 	  if (n > 0) 
          { 
             std::vector<Model*> doomedMods(lookupModelGroup(inputName));
             rerouteBrokenParLinks(doomedMods);
	     remove(inputName); 
          }   

	  if (inputName != Model::DEFAULT())
	      newModel->setIndex(incrementModelCounter());

	  // In this particular call to designateActive is the only place
          // where new model names are entered into the activeModelNames map.
          designateActive(newModel);
    }

    string ModelContainer::modelName (int index) const
    {
        ModelMapConstIter mm = m_modelSet.begin();     
        ModelMapConstIter mmEnd = m_modelSet.end(); 

        string mName("");
        if ( index != 0)
        {
                while (mm != mmEnd)
                {
                        if ( mm->second->index() == static_cast<size_t>(index) )
                        {
                                mName = mm->first; 
                                break;     
                        }
                        ++mm;       
                }
        }    
        return mName;    
    }

    void ModelContainer::reAttachResponses (const string& modelName)
    {
       using XSContainer::responses;
       ModelMapIter m = m_modelSet.lower_bound(modelName);
       ModelMapIter last = m_modelSet.upper_bound(modelName);
       while (m != last)
       {
          ResponseMapConstIter r(responses->responseList().begin());
          ResponseMapConstIter rEnd(responses->responseList().end());
          Model*& currentModel = m->second;
	  currentModel->setComputeFlag(true);
          size_t group(currentModel->dataGroupNumber());
	  size_t sourceNum(currentModel->sourceNumber());
          bool attachDummy = true;
          while (r != rEnd)
          {
             if (r->second->dataGroup() == group && r->second->sourceNumber() == sourceNum)
             {
                currentModel->attachResponse(r->second);
                attachDummy = false;
             }
             ++r;       
          }
          if (attachDummy)
          {
             currentModel->makeInactive(true);
          }
          if (!isExtended())
             currentModel->setSpectraForAutonomousEngs();
          currentModel->fillEnergyContainer();
          ++m;
       }
    }

    Real ModelContainer::countRate (size_t spectrumNumber) const
    {
      Real rate(0);

      ModelMapConstIter mm = m_modelSet.begin();
      ModelMapConstIter mEnd = m_modelSet.end();
      try
      {
                while ( mm != mEnd )
                {
                        if (mm->second->isActive()) rate += mm->second->countRate(spectrumNumber);
                ++mm;
                }
      }   
      catch (YellowAlert& )
      {
                // absorb and return 0;
                rate = 0;

      }  
      return rate;       
    }

    int ModelContainer::fullIndex (int index, const string& modelName) const
    {
       // Returns 0 if modelName not found.
       int fullIndex = 0;
       static const int modelNameDetect = 1 << s_SHIFT;
       if (index < modelNameDetect)
       {      
          if ( !(modelName.empty() || modelName == Model::DEFAULT()))
          {
             const size_t modIdx = modelLookupTable(modelName);
             if (modIdx)
                fullIndex = index + (modIdx << s_SHIFT);
          }
          else
             fullIndex = index; 
       }
       return fullIndex;
    }

    void ModelContainer::makeSourceComponents ()
    {
       std::vector<Model*> modGroups;
       // Collect groups of models by name.
       std::map<string,bool>::const_iterator itNames = m_activeModelNames.begin();
       std::map<string,bool>::const_iterator itNamesEnd = m_activeModelNames.end();
       while (itNames != itNamesEnd)
       {
          string modelName = itNames->first;
          modGroups = lookupModelGroup(modelName);
          makeSourceComponents(modGroups);
          ++itNames;
       }       
    }

    void ModelContainer::makeSourceComponents (const std::vector<Model*>& mods)
    {
          for (size_t iMod=0; iMod<mods.size(); ++iMod)
          {
             mods[iMod]->calcComponents(false);
          }
          CompCombiner combiner(mods, true);
          CompCombiner::iterator itComb = combiner.begin();
          CompCombiner::iterator itCombEnd = combiner.end();
          while (itComb != itCombEnd)
          {
             ++itComb;
          } 
    }
    
    void ModelContainer::foldSources ()
    {
        ModelMapIter m = m_modelSet.begin();       
        ModelMapIter mEnd = m_modelSet.end();
        while ( m != mEnd )
        {
                Model* mm = m->second;
                if (mm->isActive()) mm->foldSources();   
                ++m;
        }       
    }

    void ModelContainer::clearSources () throw ()
    {
        ModelMapIter m = m_modelSet.begin();       
        ModelMapIter mEnd = m_modelSet.end();
        while ( m != mEnd )
        {
                Model* mm = m->second;
                mm->clearSources();   
                ++m;
        }       
    }

    void ModelContainer::fold ()
    {
        ModelMapIter m = m_modelSet.begin();       
        ModelMapIter mEnd = m_modelSet.end();
        while ( m != mEnd )
        {
                Model* mm = m->second;
                if (mm->isActive()) mm->fold();   
                ++m;
        }       
    }

    std::vector<Model*> ModelContainer::lookupModelGroup (const string& modelName) const
    {
        string searchArg(modelName);

        if ( searchArg.empty() ) searchArg = Model::DEFAULT();
        size_t N (m_modelSet.count(searchArg));
        std::vector<Model*> lookup;
        if ( N == 0 )
        {
           return lookup;
        }
        else
        {
           std::pair<ModelMapConstIter,ModelMapConstIter> p(m_modelSet.equal_range(searchArg));
           ModelMapConstIter ip(p.first);
           lookup.resize(N,0);
           if (N > 1)
           {
              // First sort by data group number.
              std::map<size_t,Model*> tmpSort;
              while (ip != p.second)
              {
                 tmpSort[ip->second->dataGroupNumber()] = ip->second;
                 ++ip;
              }
              std::map<size_t,Model*>::const_iterator itSort = tmpSort.begin();
              std::map<size_t,Model*>::const_iterator itSortEnd = tmpSort.end();
              size_t i=0;
              while (itSort != itSortEnd)
              {
                 lookup[i] = itSort->second;
                 ++itSort, ++i;
              }              
           }
           else
           {
              lookup[0] = ip->second;
           }           
           return lookup;                
        }      
    }

    void ModelContainer::fillFakeData (DataSet* dSet) const
    {
       const size_t group = dSet->dataGroup();
       const size_t nSources = datasets->numSourcesForSpectra();
       std::vector<Model*> modelHooks(nSources);
       dSet->setModelNamesForFake(StringArray(nSources));
       for (size_t i=1; i<=nSources; ++i)
       {
          std::map<string,bool>::const_iterator itNames = m_activeModelNames.begin();
          std::map<string,bool>::const_iterator lastName = m_activeModelNames.end();
          while (itNames != lastName)
          {
             if (itNames->second)
             {
                if (m_modelSet.find(itNames->first)->second->sourceNumber() == i)
                {
                   // We have found the name of the active model which
                   // corresponds to the source number we are looking
                   // for.  Now we need the entire range of objects
                   // with this name, to match the one with the correct
                   // group number to this data set.
                   ModelMapConstIter itMod = m_modelSet.lower_bound(itNames->first);
                   ModelMapConstIter last = m_modelSet.upper_bound(itNames->first);
                   while (itMod != last)
                   {
                      if (itMod->second->dataGroupNumber() == group)
                      {
                         const Model* model = itMod->second;
                         dSet->setModelNamesForFake(model->fullExpression(), i-1);
                         size_t start = dSet->isMultiple() ? 1 : 0;
                         size_t stop = dSet->isMultiple() ? 
                                        dSet->multiSpectralData().size()+1 : 1;
                         for (size_t j=start; j<stop; ++j)
                         {
                           SpectralData *sd = dSet->sourceData(j);
                           size_t specNum = sd->spectrumNumber();
                           // Not every spectrum in the data set needs to have
                           // a response corresponding to this model.  Need to check.
                           if (sd->detector(i-1))
                           {                           
                             // Spectrum and response (and therefore folded
                             // model) must have the same number of channels.
                             sd->addToSpectrum(model->foldedModel(specNum));
                           }
                         }         
                      }
                      ++itMod;
                   }
                }
             }
             ++itNames;
          }
       }
    }

    void ModelContainer::explActiveSources (IntegerArray& sourceNums) const
    {
       std::map<size_t,string>::const_iterator itSource = m_modelForSource.begin();
       std::map<size_t,string>::const_iterator itSourceEnd = m_modelForSource.end();
       while (itSource != itSourceEnd)
       {
          sourceNums.push_back(static_cast<int>(itSource->first));
          ++itSource;
       }
    }

    void ModelContainer::deactivateOthers (size_t sourceNum)
    {
       std::map<string,bool>::iterator itModNames = m_activeModelNames.begin();
       std::map<string,bool>::iterator itEnd = m_activeModelNames.end();
       // This function will render ALL model names with sourceNum
       // EXPLICITLY inactive, ie. "inactive/off".
       while (itModNames != itEnd)
       {
          if (itModNames->second)
          {
	     ModelMapIter m = m_modelSet.lower_bound(itModNames->first);
             // If there are several model objects w/ same name, only
             // need to check 1 of them for the source number.
             if (m->second->sourceNumber() == sourceNum)
             {
                // Remove all copies but that belonging to 
                // lowest data group.
                deactivateModel(itModNames->first);
                itModNames->second = false;
                m_modelForSource.erase(sourceNum);
                tcout << "\nPrevious model";
                if ( itModNames->first != Model::DEFAULT()) tcout << ": " << itModNames->first ;
                tcout  << " for source #: " << sourceNum 
                        << " has been rendered inactive." << std::endl;                     
             }
          }
          ++itModNames;
       }
    }

    int ModelContainer::eraseComponentParameters (Component* doomed)
    {
        // erase parameters from global list and return number of parameters erased.
        int modelIndexOffset (doomed->modelIndex() << s_SHIFT);
        IntegerArray parameters( doomed->parameterIndices() );
        int N (parameters.size());
        for ( int j = 0; j < N; ++j)
        {
                string paramName (indexToName(parameters[j] + modelIndexOffset,""));
                if (tpout.logChatterLevel() >= 30)
                {
                        tcerr << " Erasing parameter number " << parameters[j] 
                                        << " internal name "  << paramName << '\n';  
                        std::map<string,int>:: iterator dr = m_parameterReverseLookup.find(paramName);
                        std::map<int,string>:: iterator df = m_parameterLookupTable.find(parameters[j]);
                        tcerr << " Erasing lookup entry "
                                        << df-> first << "  " << df->second << '\n';    
                        tcerr << " Erasing reverse lookup key "
                                        << dr-> first << "  " << dr->second << '\n';                          
                }
                m_parameterReverseLookup.erase(paramName);
                m_parameterLookupTable.erase(parameters[j] + modelIndexOffset);
                m_parameterList.erase(paramName);       
        }        
       return N;

    }

    void ModelContainer::registerModel (Model* model)
    {
          // add a model to the model list and to the global parameter lists.
          // model must be an owning pointer.

          // check the count and remove models, parameters from lists.
          const string& inputName = model->name();
          // provide a map entry to convert model names to model indices without
          // having to go through the multimap.
          modelLookupTable(inputName,model->index());
          // register parameters in their respective global lists.        
          model->registerParameters();
    }

    Real ModelContainer::calcEqWidths (const EqWidthRecord& currComp, Model* mod)
    {
       const Real FUZZ = 1.0e-35;
       Real compTotal = 0.0;
       tcout << "\nData group number: " << mod->dataGroupNumber() <<std::endl;
       std::pair<size_t,size_t> peakLoc = mod->getComponentPeak(currComp.compNumber, compTotal);
       size_t iMax = peakLoc.first;
       size_t specNum = peakLoc.second;
       SpectralData::FluxCalc eqWidth;
       if (std::fabs(compTotal) <= FUZZ)
          eqWidth.value = 0.0;
       else
       {   
          Real continuum = mod->calcContinuumFlux(currComp.compNumber, iMax, 
                                specNum, currComp.fraction);
          if (std::fabs(continuum) <= FUZZ)
          {
             string errMsg("Infinite equivalent width due to no continuum\n");
             throw YellowAlert(errMsg);
          }
          eqWidth.value = compTotal/continuum;
       }
       tcout << "Additive group equiv width for";
       if (currComp.modelName != Model::DEFAULT())
       {
          tcout << " Model: " << currComp.modelName;
       }
       tcout << " Component " << currComp.compNumber
          << ":  " << eqWidth.value << " keV" << std::endl;

       // Store this value in all spectra belonging to data group,
       // for possible later retrieval by "tclout eqwidth" command.
       DataArrayIt itDs = datasets->dataArray().begin();
       DataArrayIt itDsEnd = datasets->dataArray().end();
       while (itDs != itDsEnd)
       {
          DataSet* ds = itDs->second;
          if (ds->dataGroup() == mod->dataGroupNumber() && mod->isActive())
          {
             if (ds->isMultiple())
             {
                SpectralDataMapConstIt itSd = ds->multiSpectralData().begin();
                SpectralDataMapConstIt itSdEnd = ds->multiSpectralData().end();
                while (itSd != itSdEnd)
                {
                   itSd->second->lastEqWidthCalc(eqWidth);
                   ++itSd;
                }
             }
             else
             {
                SpectralData* sd = ds->spectralData();
                if (sd)
                {
                   sd->lastEqWidthCalc(eqWidth);
                }
             }
          }
          ++itDs;
       }
       tcout << std::flush;

       return eqWidth.value;
    }


    void ModelContainer::deregisterParameter (int oldIndex, int modelIndex)
    {
            // function to remove parameterlookups when components are reindexed
            // during a model editing operation.
            int modelOffset (modelIndex << s_SHIFT) ;
            int oldLookup (oldIndex + modelOffset);
            std::map<int,string>::iterator r = m_parameterLookupTable.find(oldLookup);
            if (r != m_parameterLookupTable.end() )
            {
                string name(r->second);
                m_parameterLookupTable.erase(r);
                m_parameterReverseLookup.erase(name);
                m_parameterList.erase(name);    
            }
    }

    void ModelContainer::registerModelParameters (const string& keyString)
    {
      std::vector<Model*>  reregistrants(lookupModelGroup(keyString));

      for (size_t i = 0; i < reregistrants.size(); ++i)
      {
                reregistrants[i]->registerParameters();       
      }
    }

    void ModelContainer::saveData (std::ostream& s)
    {
       using namespace std;

       ostringstream modelInfo;

       map<string,size_t>::const_iterator    ml (m_modelLookupTable.begin());
       map<string,size_t>::const_iterator    mlEnd  (m_modelLookupTable.end());

       // Print the inactive models first so they get overwritten.
       // The source number order doesn't particularly matter since
       // inactive models shouldn't have links.
       while(ml != mlEnd) 
       {
          string modelName (ml->first);

          Model* model = lookup(modelName);

          if ( !model->isActive() )
          {
	     size_t nParameterCount = model->numberOfParameters();
	     modelInfo << "model ";
	     //check for default here
	     if(model->name() != Model::DEFAULT())
             {
	         modelInfo << ' ' << model->sourceNumber() << ':' << model->name()  ;
             }
	     modelInfo << ' ' << model->fullExpression() << '\n';

	     for(size_t i = 1; i <= nParameterCount; ++i) 
             {
	         Parameter* p = lookupParameter(i, modelName);
                 if (m_proportionalDelta > 0.0 && !p->isLinked())
                 {
                    // ModParams will need to have their proportional
                    // delta parenthetical strings removed before
                    // writing to output.  The corresponding "xset delta"
                    // command will be written to the save file elsewhere.
                    string parInfo(p->parameterSetting());
                    string::size_type startLoc = parInfo.find('(');
                    if (startLoc != string::npos)
                    {
                       string::size_type endLoc = parInfo.find(')',startLoc);
                       if (endLoc == string::npos)
                       {
                          string errMsg("Unbalanced parentheses detected while saving string:\n    ");
                          errMsg += parInfo;
                          errMsg += "\n";
                          throw YellowAlert(errMsg);
                       }
                       parInfo.erase(startLoc, endLoc-startLoc+1);
                    }
                    modelInfo << parInfo << '\n';
                 }
                 else                 
	            modelInfo  << p->parameterSetting() << '\n';
             }
          }
          ++ml;
       }

       // Newpar commands will only be needed for linking to higher
       // numbered pars in same model (see below).
       StringArray newparCmds;

       // Now output the active models in order of source number.  
       // This leaves the burden on the user to never link from
       // lower sourceNum models to higher.
       const size_t numSources = XSContainer::datasets->numSourcesForSpectra();
       for (size_t iSource=1; iSource<=numSources; ++iSource)
       {
          const string modName(lookupModelForSource(iSource));
          if (modName.length())
          {
             // These will be sorted by dg number.
             std::vector<Model*> modsForSource =
                lookupModelGroup(modName);
             const size_t nMods = modsForSource.size();
             // If it made it here there should always be at least
             // 1 model, but check anyway.
             if (nMods)
             {
                const Model* firstMod = modsForSource[0];
                // Active/off models were already written out above.
                if (nMods > 1 || firstMod->isActive())
                {
                   const size_t nParameterCount = firstMod->numberOfParameters();
                   modelInfo << "model ";
	           if(firstMod->name() != Model::DEFAULT())
                   {
		      modelInfo << ' ' << firstMod->sourceNumber() << ':' 
                           << firstMod->name()  ;
                   }
	           modelInfo << ' ' << firstMod->fullExpression() << '\n'; 
                   const size_t totPars = nParameterCount*nMods;               
	           for(size_t i = 1; i <= totPars; ++i)
                   {
	              Parameter* p = lookupParameter(i, modName);
                      // Patch fix: If parameter is linked to any par with
                      // a higher index, the par could belong to a higher
                      // data gruop copy.  To avoid trouble upon reloading,
                      // we'll hold off on the link string here and instead
                      // write it with a newpar command at the end.
                      if (p->isLinked())
                      {
                         const std::vector<const Parameter*>& toPars =
                                p->thisLink()->members();
                         bool isLinkedToHigher = false;
                         std::vector<const Parameter*>::const_iterator itPar =
                                toPars.begin();
                         while (!isLinkedToHigher && itPar != toPars.end())
                         {
                            if ((*itPar)->modelName() == firstMod->name() &&
                             (*itPar)->index() > p->index())
                               isLinkedToHigher = true;
                            ++itPar;
                         }

                         if (isLinkedToHigher)
                         {
                            modelInfo << "/\n";
                            std::ostringstream cmd;
                            cmd << "newpar ";
                            if (firstMod->name() != Model::DEFAULT())
                            {
                               cmd << firstMod->name() << ":";
                            }
                            cmd << p->index() << " = "
                                << p->thisLink()->linkExpression(false);
                            newparCmds.push_back(cmd.str());
                         }
                         else
                         {
                            modelInfo  << p->parameterSetting() << '\n';
                         }
                      } // end if linked
                      else if (m_proportionalDelta > 0.0)
                      {
                         // ModParams will need to have their proportional
                         // delta parenthetical strings removed before
                         // writing to output.  The corresponding "xset delta"
                         // command will be written to the save file elsewhere.
                         string parInfo(p->parameterSetting());
                         string::size_type startLoc = parInfo.find('(');
                         if (startLoc != string::npos)
                         {
                            string::size_type endLoc = parInfo.find(')',startLoc);
                            if (endLoc == string::npos)
                            {
                               string errMsg("Unbalanced parentheses detected while saving string:\n    ");
                               errMsg += parInfo;
                               errMsg += "\n";
                               throw YellowAlert(errMsg);
                            }
                            parInfo.erase(startLoc, endLoc-startLoc+1);
                         }
                         modelInfo << parInfo << '\n';
                      }
                      else
	                 modelInfo  << p->parameterSetting() << '\n';
                   }
                }
             }
          }
       } // end sources loop

       for (size_t i=0; i<newparCmds.size(); ++i)
       {
          modelInfo << newparCmds[i] << "\n";
       }
       if(modelInfo.str().length() )  s << modelInfo.str() << flush;
    }

    void ModelContainer::restoreFluxes (const string& modelName, const GroupFluxContainer& savedFluxes) const
    {
       std::pair<ModelMapConstIter,ModelMapConstIter> range (m_modelSet.equal_range(modelName));
       ModelMapConstIter m (range.first);
       while ( m != range.second )
       {
               Model* model (m->second);
               std::map<size_t,ArrayContainer>::const_iterator 
                               f (savedFluxes.find(model->dataGroupNumber()));
               model->resetModelFlux(f->second);
               ++m;
       }       
    }

    void ModelContainer::storeFluxes (const string& modelName, GroupFluxContainer& fluxes) const
    {
        std::pair<ModelMapConstIter, ModelMapConstIter> range (m_modelSet.equal_range(modelName));
        ModelMapConstIter m (range.first);
        fluxes.clear();
        while ( m != range.second )
        {
                Model* model (m->second);
                fluxes[model->dataGroupNumber()] = model->modelFlux();   
                ++m;
        }       
    }

    void ModelContainer::fold (const string& modelName) const
    {
        std::pair<ModelMapConstIter, ModelMapConstIter> range (m_modelSet.equal_range(modelName));
        ModelMapConstIter r (range.first);
        // should only be called if models are active anyway, but since the program
        // might crash if this is violated, perform the check again at the cost of
        // a small number of instructions.
        while ( r != range.second )
        {
                Model* model = r->second;
                if (model->isActive()) model->fold();   
                ++r;
        }       
    }

    void ModelContainer::makeDerivatives (const string& modelName, GroupFluxContainer& difference, Real delta, ArrayContainer& foldedDerivative) const
    {
        const Real DENOM (2*delta);            
        std::pair<ModelMapConstIter,ModelMapConstIter> range (m_modelSet.equal_range(modelName));
        ModelMapConstIter r (range.first);

        while ( r != range.second)
        {
                Model* model(r->second);
                ArrayContainer& diff = difference[model->dataGroupNumber()];
                const ArrayContainer& mf = model->modelFlux();
                ArrayContainer::iterator a    (diff.begin());
                ArrayContainer::iterator aEnd (diff.end());
                while ( a != aEnd )
                {
                        size_t index (a->first);
                        a->second -= mf.find(index)->second;
                        a->second /= DENOM;
                        ++a;       
                }
                model->resetModelFlux(diff);
                model->fold();
                model->storeDerivative(foldedDerivative);
                ++r;       
        }
    }

    void ModelContainer::resetComponentFluxes (const string& modelName) const
    {
        std::pair<ModelMapConstIter,ModelMapConstIter> range (m_modelSet.equal_range(modelName));
        ModelMapConstIter m (range.first);
        while ( m != range.second )
        {
                Model* model (m->second);
                model->resetComponentFlux();
                ++m;
        }       
    }

    void ModelContainer::storeFluxErrors (const string& modelName, GroupFluxContainer& fluxErrors) const
    {
        fluxErrors.clear();
        std::pair<ModelMapConstIter, ModelMapConstIter> range (m_modelSet.equal_range(modelName));
        ModelMapConstIter m (range.first);
        while ( m != range.second )
        {
           Model* model (m->second);
           fluxErrors[model->dataGroupNumber()] = model->modelFluxError();   
           ++m;
        } 
    }

    void ModelContainer::restoreFluxErrors (const string& modelName, const GroupFluxContainer& savedFluxes) const
    {
        std::pair<ModelMapConstIter,ModelMapConstIter> range (m_modelSet.equal_range(modelName));
        ModelMapConstIter m (range.first);
        while ( m != range.second )
        {
             Model* model (m->second);
             std::map<size_t,ArrayContainer>::const_iterator 
                     f (savedFluxes.find(model->dataGroupNumber()));
             if (f != savedFluxes.end()) model->resetModelFluxError(f->second);
             ++m;
        }    
    }

    EnergyPointer ModelContainer::gatherGroupEnergy (const string& modelName) const
    {
      std::pair<ModelMapConstIter, ModelMapConstIter> range (m_modelSet.equal_range(modelName));
      ModelMapConstIter m (range.first);
      // energies are just input data and are not changed. Therefore, we avoid
      // copying the information, either.
      EnergyPointer ensemble;
      while ( m != range.second )
      {
              Model* model (m->second);
              ensemble[model->dataGroupNumber()] = &model->energy();
              ++m;
      }

      return ensemble;
    }

    void ModelContainer::initializeMixingTransformation (const string& modelName) const
    {
        std::vector<Model*> sortedModelsByDG(lookupModelGroup(modelName));
        if (!sortedModelsByDG.size())
           throw RedAlert(string("No models found for model name: ")+modelName);
        IntegerArray allSpectraForMix;
        for (size_t i=0; i<sortedModelsByDG.size(); ++i)
        {
           ArrayContainer::const_iterator itEnergyArraysForMod = sortedModelsByDG[i]->energy().begin();
           ArrayContainer::const_iterator itEnergyArraysForModEnd = sortedModelsByDG[i]->energy().end();
           while (itEnergyArraysForMod != itEnergyArraysForModEnd)
           {
              // Don't want to include inactive or active/off model (assigned to specNum 0).
              if (itEnergyArraysForMod->first != 0)
                 allSpectraForMix.push_back(static_cast<int>(itEnergyArraysForMod->first));
              ++itEnergyArraysForMod;
           }           
        }
         
        //'true' tells model to insert mixing utility objects (if relevant)
        //  into lowest DG mix model components. 
        sortedModelsByDG[0]->initializeMixingTransformation(true, allSpectraForMix);
        for (size_t i=1; i<sortedModelsByDG.size(); ++i)
           // 'false' tells model to delete any leftover mix utility objects
           //    from higher DG components.
           sortedModelsByDG[i]->initializeMixingTransformation(false); 
    
    }

    void ModelContainer::deactivateModel (const string& name)
    {
      // This function should be used whenever a model's status has
      // changed from "active/on|off" to "inactive/off".  It
      // will destroy all data group copies of the model except that
      // belonging to the lowest data group, and then call makeInactive on
      // the remaining copy.  
      using XSContainer::datasets;
      std::vector<Model*> doomedMods(lookupModelGroup(name));
      rerouteBrokenParLinks(doomedMods);

      size_t nCopies = m_modelSet.count(name);
      if (nCopies)
      {
         const size_t sourceNum = m_modelSet.lower_bound(name)->second->sourceNumber();
         if (nCopies > 1)
         {
            std::map<size_t,std::map<size_t,size_t> >::const_iterator itSource = 
                     datasets->sourceToDgs().find(sourceNum);            
            if (itSource != datasets->sourceToDgs().end())
            {
               std::map<size_t,size_t>::const_iterator itGroup =
                  itSource->second.begin();
               std::map<size_t,size_t>::const_iterator itGroupEnd =
                  itSource->second.end();
               // Avoid removing the first group.
               if (itGroup != itGroupEnd)
                  ++itGroup;
               while (itGroup != itGroupEnd)
               {
                  remove(name, itGroup->first);
                  ++itGroup;
               }
            }            
         }
         m_modelSet.lower_bound(name)->second->makeInactive();  
         m_activeModelNames[name] = false; 
         std::map<size_t,string>::iterator itName = m_modelForSource.find(sourceNum);
         if (itName != m_modelForSource.end())
         {
            if (itName->second == name)
            {
               m_modelForSource.erase(itName);
            }
         }      
      }
    }

    Model* ModelContainer::lookup (const Response* resp)
    {
      Model* found = 0;
      std::map<size_t,string>::const_iterator itSourceToName =
                m_modelForSource.find(resp->sourceNumber());
      if (itSourceToName != m_modelForSource.end())
      {
         // Only active models (on or off) will get in here.
         string modName(itSourceToName->second);
         found = lookup(modName, resp->dataGroup());
      }
      return found;
    }

    void ModelContainer::applyAutonomousEnergies (const RealArray& energyArray)
    {
       // This function works on the assumption that the models
       // are up to date with respect to their attached responses.
       m_autonomousEnergy.resize(energyArray.size());
       m_autonomousEnergy = energyArray;
       // Turn off any extensions.  They wouldn't be applied here anyway,
       // but user should have to explicitly turn high or low extension
       // back on again after calling this command.
       m_extendedEnergy.first.nBins = 0;
       m_extendedEnergy.second.nBins = 0;
       ModelMap::const_iterator itMods = m_modelSet.begin();
       ModelMap::const_iterator itModsEnd = m_modelSet.end();
       while (itMods != itModsEnd)
       {
          Model* mod = itMods->second;
          mod->setAutonomousEnergy(m_autonomousEnergy);
          mod->setSpectraForAutonomousEngs();
          mod->fillEnergyContainer();
          mod->setComputeFlag(true);
          ++itMods;
       }
    }

    void ModelContainer::modifyExtendedEnergies (const ExtendRecord& extend, bool isHigh)
    {
       // This function works on the assumption that the models
       // are up to date with respect to their attached responses.
       if (isHigh)
          m_extendedEnergy.second = extend;
       else
          m_extendedEnergy.first = extend;
       // Enforce the constraint that when extensions are turned on,
       // the global autonomousEnergy array is turned off.
       m_autonomousEnergy.resize(0);
       applyExtendedEnergies();
    }

    void ModelContainer::rerouteBrokenParLinks (std::vector<Model*>& doomedMods)
    {
       // ASSUMES parameter lookup tables are all in their pre-updated state.
       // This should be called early on when coming from ModelContainer Update process.
       // If a ParLink points to a parameter in a doomed model object, see if the
       // doomed par points to a persistent par.  If so, re-route original link 
       // to that.  If not, original link should be flagged for removal.  If just
       // one par in a link statement is unable to be re-routed, entire ParamLink
       // should be flagged for removal.
       flagDoomedPars(doomedMods);

       std::set<Parameter*> doomedPars;
       std::vector<Parameter*> persistentPars;
       std::map<Parameter*,Parameter*> processedDoomedPars;
       std::map<string,Parameter*>::iterator itPars = m_parameterList.begin();
       std::map<string,Parameter*>::iterator itParsEnd = m_parameterList.end();
       while (itPars != itParsEnd)
       {
          if (itPars->second->isModelDoomed())
             doomedPars.insert(itPars->second);
          else
             persistentPars.push_back(itPars->second);
          ++itPars;
       }

       while (!doomedPars.empty())
       {
          Parameter::findPersistentLink(*doomedPars.begin(), doomedPars,
                processedDoomedPars);
       }
       for (size_t i=0; i<persistentPars.size(); ++i)
       {
          Parameter* par = persistentPars[i];
          if (par->isLinked())
          {
             std::vector<Parameter*> correctedToPars;
             bool correctionMade = false;
             bool removeLink = false;
             const std::vector<const Parameter*>& toPars = par->thisLink()->members();
             for (size_t j=0; !removeLink && j<toPars.size(); ++j)
             {
                Parameter* toPar = const_cast<Parameter*>(toPars[j]);
                if (toPar->isModelDoomed())
                {
                   Parameter* reRouted = processedDoomedPars.find(toPar)->second;
                   if (!reRouted)
                   {
                      removeLink = true;
                      par->untie(true);
                   }
                   else
                   {
                      correctedToPars.push_back(reRouted);
                      correctionMade = true;
                   }
                }
                else
                {
                   correctedToPars.push_back(toPar);
                }
             }
             if (!removeLink && correctionMade)
                par->thisLink()->rerouteLink(correctedToPars);
          }
       } // end persistentPars loop

       // Not all model objects marked as doomed are necessarily about to be
       // destroyed.  The model may have just been marked inactive.  We don't
       // want any links coming from inactive parameters, as the code isn't
       // designed to keep them updated.  So ensure this doesn't happen 
       // simply by removing links from all doomed parameters.
       std::map<Parameter*,Parameter*>::iterator itDoomed = processedDoomedPars.begin();
       while (itDoomed != processedDoomedPars.end())
       {
          // If par is not linked, this does nothing.
          itDoomed->first->untie(true);
          ++itDoomed;
       }
    }

    void ModelContainer::determineModelObjectStatus (std::map<string,std::vector<Model*> >& doomed, std::map<string,std::vector<Model*> >& alive, std::map<string,std::vector<size_t> >& needed)
    {
       using namespace std;
       const map<size_t, set<size_t> >& sourcesForGroup =
                datasets->dgToSources();
       map<string,bool>::const_iterator itActMod = m_activeModelNames.begin();
       map<string,bool>::const_iterator itActEnd = m_activeModelNames.end();
       while (itActMod != itActEnd)
       {
          const string& modName = itActMod->first;
          if (itActMod->second)
          {
             std::vector<Model*>& aliveVec = alive[modName];
             std::vector<Model*>& doomedVec = doomed[modName];
             vector<Model*> modelObjs = lookupModelGroup(modName);
             const size_t sourceNum = modelObjs[0]->sourceNumber();
             // Deliberately making a copy of this map since we may be 
             // removing items from it.
             map<size_t,size_t> groupsNeeded;
             map<size_t, map<size_t,size_t> >::const_iterator itDgs =
                        datasets->sourceToDgs().find(sourceNum);
             if (itDgs != datasets->sourceToDgs().end())
             {
                groupsNeeded = itDgs->second;
             }
             for (size_t i=0; i<modelObjs.size(); ++i)
             {
                Model* mod = modelObjs[i];
                size_t dgNum = mod->dataGroupNumber();
                if (!datasets->dgHistory().isEmpty())
                {
                   // If we're coming from xsData handler, any model object 
                   // that exists at this point ought to have a data group 
                   // number that exists in DataGroupHistory's reassignment 
                   // vector.               
                   dgNum = datasets->dgHistory().getReassignment(dgNum);
                   mod->dataGroupNumber(dgNum);
                }
                if (!dgNum)
                   doomedVec.push_back(mod);
                else
                {
                   const set<size_t>& sourcesUsed = 
                          sourcesForGroup.find(dgNum)->second;
                   if (sourcesUsed.find(sourceNum) != sourcesUsed.end())
                   {
                      aliveVec.push_back(mod);
                      groupsNeeded.erase(dgNum);
                   }
                   else
                      doomedVec.push_back(mod);
                }
             }
             vector<size_t>& stillNeeded = needed[modName];
             map<size_t,size_t>::const_iterator itNeededGroup = groupsNeeded.begin();
             map<size_t,size_t>::const_iterator itNeededEnd = groupsNeeded.end();
             while (itNeededGroup != itNeededEnd)
             {
                stillNeeded.push_back(itNeededGroup->first);
                ++itNeededGroup;
             }

             if (aliveVec.empty() && stillNeeded.empty())
             {
                // No data groups need this model.  Rescue the lowest dg copy
                // from doomed, which will then be in the active/off state.
                // Finding what was the lowest dg is messy by this point, since
                // dgNums may have all been set to 0 above.  However, what we
                // specifically want is the Model object whose pars begin at 1,
                // and that info fortunately still exists.
                Model* keep = 0;
                std::vector<Model*>::iterator itDoomed = doomedVec.begin();
                std::vector<Model*>::iterator itDoomedEnd = doomedVec.end();
                while (itDoomed != itDoomedEnd)
                {
                   if ((*itDoomed)->parameterIndexBase() == 0)
                   {
                      keep = *itDoomed;
                      // Calling erase on a vector, ugh. Do NOT use
                      // this iterator again!
                      doomedVec.erase(itDoomed);
                      break;
                   }
                   ++itDoomed;
                }
                if (keep)
                {
                   keep->dataGroupNumber(1);
                   aliveVec.push_back(keep);
                }
             }
          }
          ++itActMod;
       }
       // Don't want to accidentally reuse data group history if Update should
       // come from some other place than xsData.
       datasets->dgHistory().erase();
    }

    string ModelContainer::lookupModelForSource (size_t sourceNum) const
    {
       string activeModName;
       std::map<size_t,string>::const_iterator itName = 
                m_modelForSource.find(sourceNum);
       if (itName != m_modelForSource.end())
          activeModName = itName->second;
       return activeModName;
    }

    std::vector<const Model*> ModelContainer::getModsForSpec (const SpectralData* spectrum)
    {
      const std::vector<Response*>& dets = spectrum->detector();
      const size_t nDets = dets.size();
      std::vector<const Model*> mods(nDets, 0);
      for (size_t i=0; i<nDets; ++i)
      {
         if (dets[i])
         {
            mods[i] = lookup(dets[i]);
         }
      }
      return mods;
    }

    void ModelContainer::applyExtendedEnergies ()
    {
       if (isExtended())
       {
          ModelMap::const_iterator itMods = m_modelSet.begin();
          ModelMap::const_iterator itModsEnd = m_modelSet.end();
          while (itMods != itModsEnd)
          {
             Model* mod = itMods->second;
             mod->setAutonomousEnergy(m_extendedEnergy);
             mod->fillEnergyContainer();
             mod->setComputeFlag(true);
             ++itMods;
          }
       }
    }

    void ModelContainer::flagDoomedPars (std::vector<Model*>& doomedMods)
    {
       std::vector<Model*>::iterator itDoomed = doomedMods.begin();
       while (itDoomed != doomedMods.end())
       {
          std::vector<Parameter*> doomedPars;
          (*itDoomed)->bundleParameters(doomedPars);
          std::vector<Parameter*>::iterator itPar = doomedPars.begin();
          while (itPar != doomedPars.end())
          {
             (*itPar)->isModelDoomed(true);
             ++itPar;
          }
          ++itDoomed;
       }
    }

    void ModelContainer::designateActive (const Model* mod)
    {
       // Assume model has already been checked to have a valid source number.
       deactivateOthers(mod->sourceNumber());
       m_activeModelNames[mod->name()] = true;
       m_modelForSource[mod->sourceNumber()] = mod->name();

       std::vector<Parameter*> modPars;
       mod->bundleParameters(modPars);
       std::vector<Parameter*>::iterator itPar = modPars.begin();
       while (itPar != modPars.end())
       {
          (*itPar)->isModelDoomed(false);
          ++itPar;
       }

    }

    void ModelContainer::showModel () const
    {
      using namespace std;
      tcout << "\nCurrent model list:";
      if (modelSet().empty())
      {
         tcout << " none " << endl;
      }
      else
      {
         tcout << endl;
         map<string,bool>::const_iterator itModNames = m_activeModelNames.begin();
         map<string,bool>::const_iterator itNamesEnd = m_activeModelNames.end();
         while (itModNames != itNamesEnd)
         {
            // mods size must always be >= 1 if name is in activeModelNames map.
            vector<Model*> mods = lookupModelGroup(itModNames->first);
            mods[0]->printHeading();
            for (size_t i=0; i<mods.size(); ++i)
            {
               tcout << *mods[i];
            }
            mods[0]->printMixComp();
            tcout << string(72,'_') << '\n' << endl;
            ++itModNames;
         }   
      }
      if (m_autonomousEnergy.size())
      {
         tcout << "   Using autonomous energy array." << endl;
      }
      else if (isExtended())
      {
         tcout << "   Using extended response energies." << endl;
      }
      else
      {
         tcout << "   Using energies from responses." << endl;
      }
    }

    void ModelContainer::reportModelRate (const SpectralData* sd)
    {
      using namespace XSContainer;
      using namespace std;

      const size_t specNum = sd->spectrumNumber();
      
      Real total = countRate(specNum);

      ios_base::fmtflags saveFmt(tcout.flags());
      streamsize savePrec(tcout.precision());
      Real absVal = fabs(total);
      if (total > 0)
      {
         int exp = static_cast<int>(floor(log10(absVal)));
         if (exp >= -1 && exp <= 3)
         {
            // Want 6 significant digits, prec gives number
            // of digits after decimal.
            int prec = 5 - exp;
            tcout << fixed << showpoint << setprecision(prec);
         }
         else
         {
            tcout << scientific << uppercase << setprecision(5);
         } 
      }
      else
      {
         // Just print 0.0
         tcout << fixed << showpoint << setprecision(1);
      }  

      tcout << " Model predicted rate: " << total << endl;
      tcout.precision(savePrec);
      tcout.flags(saveFmt);
    }

    void ModelContainer::parameterList (string key, Parameter* value)
    {
        m_parameterList.insert(ParamMap::value_type(key,value));
    }

    // Additional Declarations

} // namespace XSContainer
