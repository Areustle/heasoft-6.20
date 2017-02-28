//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%

// SumComponent
#include <XSModel/Model/Component/SumComponent.h>
// UniqueEnergy
#include <XSModel/Model/UniqueEnergy.h>
// sstream
#include <sstream>
// XSparse
#include <XSUtil/Parse/XSparse.h>
// XSstream
#include <XSUtil/Utils/XSstream.h>
// ModelContainer
#include <XSModel/GlobalContainer/ModelContainer.h>
// Component
#include <XSModel/Model/Component/Component.h>
// ParamCreator
#include <XSModel/Parameter/ParamCreator.h>

#include <XSModel/GlobalContainer/MdefContainer.h>
#include <XSModel/Model/ModelBase.h>
#include <XSModel/Model/Component/ComponentGroup.h>
#include <XSModel/Parameter/ModParam.h>
#include <XSUtil/FunctionUtils/funcType.h>
#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSUtil/Parse/MathExpression.h>
#include <XSUtil/Utils/XSutility.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <XSsymbol.h>
#include <iostream>
#include <fstream>
#include <string>

extern  ModelFunctionMap  XSFunctionMap;

Component::InvalidInput::InvalidInput (const string& errMessage)
  : YellowAlert()
{
  tcerr << errMessage << '\n';
}

Component::InvalidTableModel::InvalidTableModel()
{
}


// Class Component::BadTableFileName 

Component::BadTableFileName::BadTableFileName (const string& errName)
   : YellowAlert( " Table Model file not found or has errors: file ") 
{
       tcerr << errName << '\n';
}


// Class Component::SpectrumNotDefined 

Component::SpectrumNotDefined::SpectrumNotDefined (const string& diag)
  : YellowAlert(" Attempt to reference spectrum number not associated with this model ")
{
  tcerr << diag << '\n';
}


// Class Component::NoSuchComponent 

Component::NoSuchComponent::NoSuchComponent (const string& msg)
   : YellowAlert (" No model component named ")             
{
  tcerr << msg << '\n';
  XSModelFunction::printComponentList(tcerr,msg);
}


// Class Component 
bool Component::s_currentIsTable = false;
ComponentInfo Component::s_currentModelInfo;

Component::Component(const Component &right)
  : m_name(right.m_name),
    m_index(right.m_index),
    m_maxEnergy(right.m_maxEnergy),
    m_minEnergy(right.m_minEnergy),
    m_numParams(right.m_numParams),
    m_error(right.m_error),
    m_compute(right.m_compute),
    m_fluxSaved(right.m_fluxSaved),
    m_initString(right.m_initString),
    m_isSpectrumDependency(right.m_isSpectrumDependency),
    m_isZeroNorm(right.m_isZeroNorm),
    m_parameterSet(),
    m_parent(right.m_parent),  
    m_generator(right.m_generator), // Non-owning shallow copy
    m_uniquePhotonArray(right.m_uniquePhotonArray),
    m_uniquePhotonErrArray(right.m_uniquePhotonErrArray),
    m_savedUniquePhotonArray(right.m_savedUniquePhotonArray)
{
}

Component::Component (ComponentGroup* p)
   : m_name(""),
     m_index(0),
     m_maxEnergy(),
     m_minEnergy(),
     m_numParams(0),
     m_error(false),
     m_compute(true),
     m_fluxSaved(false),
     m_initString(""),
     m_isSpectrumDependency(false),
     m_isZeroNorm(false),
     m_parameterSet(),
     m_parent(p),
     m_generator(0),
     m_uniquePhotonArray(),
     m_uniquePhotonErrArray(),
     m_savedUniquePhotonArray()
{
}


Component::~Component()
{
}


Component & Component::operator=(const Component &right)
{
  if ( this != &right ) copy(right);
  return *this;
}


int Component::read ()
{
// Read the information from model.dat, mdef map, or table model file, 
// and fill in the Component template.
// Then call the parameter functions to instantiate the model parameters.
   using namespace std;

// First, determine whether the model information is to be taken from the
// model.data file. The sign of this is that currentLoc != npos.
   int status = XSPEC_OK;

   if (s_currentModelInfo.location != std::string::npos)
   {
      m_name = s_currentModelInfo.name;
   // If we got here ok, then this open request will succeed unless the model
   // description file got changed since the program started running.
      ifstream modfile;
      modfile.open(s_currentModelInfo.sourceFile.c_str());
      if (!modfile)
      {
         string errString = "Error opening model description file " +
                  s_currentModelInfo.sourceFile + "\n";
         throw RedAlert(errString);
      }
      modfile.seekg(s_currentModelInfo.location);
      string line;
      getline(modfile,line);

      istringstream inputLine(line);

      inputLine >> s_currentModelInfo.name;
   //
   // check that we did actually get the right place, abort otherwise. 
   //               
      if (s_currentModelInfo.name == name() )
      {
         string errMsg("Format error in first line of ");
         errMsg += name() + string(" entry in model description file ");
         errMsg += s_currentModelInfo.sourceFile + "\n";
         size_t nP(0);
         Real minE(0.);
         Real maxE(0.);
         inputLine >> nP >> minE >> maxE;
         if (!inputLine)
            throw YellowAlert(errMsg);
         m_numParams = nP;
         m_minEnergy = minE;
         m_maxEnergy = maxE;
         // get function pointer from function map.
         m_generator = XSFunctionMap[name()];
         string dummy;
         // skip over function name and component type
         inputLine >> dummy;
         inputLine >> dummy;
         if (!inputLine)
            throw YellowAlert(errMsg);
         bool errFlag(false);
         inputLine >> errFlag;
         if (!inputLine)
            throw YellowAlert(errMsg);
         m_error = errFlag;
         // Remaining parameters are optional.  They may be:
         // <dependency flag> followed by <info string>,
         // <dependency flag> OR <info string>, or nothing at all. 
         string testString;
         bool dependencyFlag = false;
         string infoString;
         inputLine >> testString;
         if (testString.length())
         {
            // At least 1 optional param.  Is it a bool?
            istringstream issTest(testString);
            bool testBool = false;
            if (!(issTest >> testBool) || !issTest.eof())
            {
               // Not a bool, assume info string.
               infoString = testString;
            }
            else
            {
               dependencyFlag = testBool;
               // Still need to test for info string.
               testString.erase();
               if (inputLine >> testString)
               {
                  infoString = testString;
               } 
            }
            testString.erase();
            if (inputLine >> testString)
            {
               // This should catch case of dependency flag 
               // following info string.
               tcout << "***Warning: In first line of " << name() << " entry of model description file,"
                  << "\n     the string \"" << testString << "\" following \"" << infoString
                  << "\" will be ignored." << std::endl;
            }
         }
         m_isSpectrumDependency = dependencyFlag;
         m_initString = infoString;

         // get parameter initialization from file.
         Parameter* p;
         for (size_t i = 0; i < nP ; i++)
         {
            string paramLine;
            getline(modfile,paramLine);
            // the third argument (table) default to false.
            // read must be overriden for table models.
            p = ParamCreator::MakeParameter(paramLine,this,
                            incrementModelParameterCount());
            m_parameterSet.push_back(p);
         } 
      }
      else
      {
         string errString = string("Error in  file seek location:  \n") +
                 + "Check for corruption of " + s_currentModelInfo.sourceFile;
         throw RedAlert(errString);
      }
      modfile.close();
   } // end if location != npos
   else if (s_currentModelInfo.isPythonModel)
   {
      m_name = s_currentModelInfo.name;
      ParInfoContainer::const_iterator itParInfo = 
                XSModelFunction::parInfoCache().find(m_name);
      if (itParInfo == XSModelFunction::parInfoCache().end())
      {
         string err("Cannot find parameter information for Python model ");
         err += m_name;
         throw RedAlert(err);
      }
      const std::vector<string>& parInfoStrings = itParInfo->second;
      m_numParams = parInfoStrings.size();
      for (size_t i=0; i<m_numParams; ++i)
      {
         // This is where bad parameter info from Python will be dealt with.
         //   The following call with throw.
         Parameter* par = ParamCreator::MakeParameter(parInfoStrings[i],
                                this, incrementModelParameterCount());
         m_parameterSet.push_back(par);
      }
      m_generator = XSFunctionMap[m_name];
      m_error = s_currentModelInfo.error;
      m_isSpectrumDependency = s_currentModelInfo.isSpecDependent;
      // The following members cannot be currently set from Python.
      //   Just hardcode defaults.
      m_initString = string("");
      m_minEnergy = 0.0;
      m_maxEnergy = 1.0e20;      
   }
   else if (!s_currentIsTable)
   {
      // Not built-in or local mod, not a table mod or python, assume it must be
      // in mdefine map.
      using namespace XSContainer;
      MdefContainer::CompPair mdefComp(models->userMdefs()->
                                getComponent(s_currentModelInfo.name));
      if (mdefComp.first == MdefContainer::NOT_FOUND())
      {
         string err("Component named ");
         err += s_currentModelInfo.name;
         err += " is missing during Component::read() function.";
         throw RedAlert(err);
      }
      const MathExpression* mathExpr = mdefComp.second->generator();
      m_name = s_currentModelInfo.name;
      m_generator = mdefComp.second;
      m_minEnergy = mathExpr->eLow();
      m_maxEnergy = mathExpr->eHigh();
      const vector<string>& parNames = mathExpr->distinctParNames();
      m_numParams = parNames.size();
      for (size_t i=0; i<parNames.size(); ++i)
      {
         Parameter* newPar = new ModParam(parNames[i],this,
                1.0, .1, 1e22, 1e-22, 1e22, 1e-22);
         newPar->index(incrementModelParameterCount());
         m_parameterSet.push_back(newPar);         
      }      
   }
   s_currentModelInfo.reset();
   return status;
}

void Component::getSetupData (const string& modelName)
{
    // This function sets the values of static attributes as follows:
    //  name              model.dat       table model     userdef model
    // currentType        model type      model type      model type
    // currentLoc         seek location     -1              ??
    //                    in model.dat
    // currentModelName   model name      model file      userdef model name
    // These are static, because when this is called we don't know what type
    // of component to make.

    // can't check a name shorter than two characters because there are parameter names
    // which could produce hits in the name resolver.

    using namespace std;

    s_currentIsTable = false;
    try
    {
        s_currentModelInfo = XSModelFunction::fullMatch(modelName);       
    }
    catch (XSModelFunction::NoSuchComponent&)
    {
       using namespace XSContainer;
       // First check if model is an mdefine entry. 
       const MdefContainer* mdefs = models->userMdefs();
       const MdefContainer::CompPair mdefComp = mdefs->getComponent(modelName);
       if (mdefComp.first != MdefContainer::NOT_FOUND())
       {
          s_currentModelInfo.name = modelName;
          s_currentModelInfo.type = mdefComp.first;
          s_currentModelInfo.location = string::npos;
          s_currentModelInfo.error = false;
          s_currentIsTable = false;
       }
       else
       { 

          // check for table models and stuff. If the model doesn't match a table
          // model, rethrow, only this time with a message.

          // Any table abbreviations (ie. atab) will be expanded to the full specifier
          // when setting s_currentModelInfo.name.  This is not really necessary when
          // creating and loading new table components, since 
          // ComponentGroup::checkCurrentTableModelExpr also performs this expansion, but
          // it IS necessary when editmod needs to compare strings for differences.
          // This prevents it from seeing atable{...} and ata{...} as 3 different
          // components.  Also, this is consistent with treatment if non-table models
          // in this function, since their expanded names get retrieved.
          const string ATABLE("atable");
          const string MTABLE("mtable");
          const string ETABLE("etable");
          string::size_type bracePos = modelName.find_first_of('{');
          string lcName(XSutility::lowerCase(modelName));
          if (bracePos != string::npos)
          {
             string preBrace = lcName.substr(0, bracePos);
             if (ATABLE.find(preBrace) == 0)
             {
                s_currentModelInfo.name = ATABLE + modelName.substr(bracePos);
                s_currentModelInfo.type = "add";
                s_currentIsTable = true;
             }
             else if (MTABLE.find(preBrace) == 0)
             {
                s_currentModelInfo.name = MTABLE + modelName.substr(bracePos);
                s_currentModelInfo.type = "mul";
                s_currentIsTable = true;
             }
             else if (ETABLE.find(preBrace) == 0)
             {
                s_currentModelInfo.name = ETABLE + modelName.substr(bracePos);
                s_currentModelInfo.type = "mul";
                s_currentIsTable = true;
             }
             else
             {
                throw NoSuchComponent(modelName);
             }
          }
          else
          {
             throw NoSuchComponent(modelName);
          }
       } // end if table comp
    } // end if not built-in model
}

void Component::addParam (const string& paramName, const string& unit, Real value, Real top, Real max, Real bot, Real min, Real delta)
{
  // construct a model parameter in the Parameter container, parameterSet. Note that the
  // other two data members (addc, mult) will get their default values from the ModParam
  // constructor.
  m_parameterSet.push_back(new ModParam(paramName,this,value,delta,max,min,top,bot,unit));
  m_parameterSet.back()->index(incrementModelParameterCount());
  m_numParams += 1;
}

void Component::setParamValues (const std::vector<string>& paramString, const IntegerArray& parameterNumbers)
{
  static const string skip = XSparse::SKIP();  
  if (parameterNumbers.empty())
  {
        // just do all of them.
        int n = paramString.size();
        for (int i = 0; i < n; i++)
        {
              int first = paramString[i].find_first_not_of(" \t");
              // first, catch the skip character sequence and issue a return
              // to the calling routine.
              if (first >= 0)
              {
                  if (paramString[i].substr(first).size() >= 2)
                  {
                        if (paramString[i].substr(first,2) == skip) break;  
                  }    
                  // go on to the next string if it's a skip or a null entry.
                  if (paramString[i].at(first) == '/' || paramString[i].at(first) == '\n') continue;
                  std::istringstream s(paramString[i]);
                  s >> *m_parameterSet[i];
              }
              // or, go on to the next one.
        }

  }
  else
  {          

        for  (size_t j = 0; j < parameterNumbers.size(); j++)  
        {
               Parameter* newValue(m_parameterSet[j]);
               if (!newValue) throw RedAlert(" Component::setParamValues ");
               std::istringstream s(paramString[j]);
               s >> *newValue;       
        }
  }
}

void Component::getParamValues (const string& modString, std::vector<string>& componentParams) const
{
  static const string& skipAll = XSparse::SKIP();
  std::vector<Parameter*>::const_iterator p(m_parameterSet.begin());
  for (size_t i = 0; i < m_numParams; i++)
  {
        tcout << (*p)->parameterSetting() << '\n';       
        std::ostringstream promptString;
        promptString << (*p)->index() << ":";
        if (modString.length() > 0) promptString << modString << ':';
        string rawfromUser("");
        promptString  << name() << ':' << m_parameterSet[i]->name() << '>';
        XSstream::setPrompter(tcin,promptString.str());

        getline(tcin,rawfromUser);
        int first = rawfromUser.find_first_not_of(" \t\r");
        // strip leading blanks and tabs from the input string.
        if (first >= 0)
        {            
                string fromUser = rawfromUser.substr(first);
                componentParams.push_back(fromUser);
                if (fromUser ==  skipAll ) break;                
        }
        else componentParams.push_back("/");
        ++p;
  }
  XSstream::setPrompter(tcin, string(""));
}

const Parameter* Component::parameterSet (size_t index) const
{

  return m_parameterSet[index];
}

bool Component::isNested () const
{

  return false;
}

void Component::swap (Component& right)
{
  std::swap(m_name,right.m_name);
  std::swap(m_index,right.m_index);
  std::swap(m_maxEnergy,right.m_maxEnergy);
  std::swap(m_minEnergy,right.m_minEnergy);
  std::swap(m_fluxSaved,right.m_fluxSaved);
  std::swap(m_numParams,right.m_numParams);
  std::swap(m_generator,right.m_generator);
  std::swap(m_initString,right.m_initString);
  std::swap(m_parameterSet,right.m_parameterSet);  
  std::swap(m_parent,right.m_parent);  
  std::swap(m_error,right.m_error);   
  std::swap(m_compute,right.m_compute); 
  std::swap(m_uniquePhotonArray,right.m_uniquePhotonArray);  
  std::swap(m_uniquePhotonErrArray,right.m_uniquePhotonErrArray);
  std::swap(m_savedUniquePhotonArray,right.m_savedUniquePhotonArray);
  std::swap(m_isSpectrumDependency,right.m_isSpectrumDependency);
  std::swap(m_isZeroNorm, right.m_isZeroNorm);
}

size_t Component::dataGroup () const
{

  return m_parent->dataGroup();
}

void Component::deleteParameters () throw ()
{
  // delete & dereference don't throw.  
  // since this version is called in the dtor removing parameters
  // from the global registry must be handled separately.
  for ( std::vector<Parameter*>::iterator c = m_parameterSet.begin(); 
            c != m_parameterSet.end(); ++c )
  {
          delete *c;
  }
}

void Component::registerParameters () const
{
  std::vector<Parameter*>::const_iterator r = m_parameterSet.begin();
  std::vector<Parameter*>::const_iterator rEnd = m_parameterSet.end();
  int Index = index();  
  const string& componentName = name();
  const string& modelName = parentName();
  while ( r != rEnd )
  {
        std::ostringstream key;
        // the Index is the component index in the current model, used to
        // ensure the parameter key is unique for a given model.
        key << modelName << ':' << componentName << '[' << Index << "]:" << (*r)->name(); 
        XSContainer::models->addParameterToList(key.str(),*r);  
        ++r;       
  }  
}

size_t Component::parameterIndexBase () const
{

  return m_parent->parameterIndexBase();
}

void Component::reindexParameters (int parameterOffset)
{
  std::vector<Parameter*>::iterator r = m_parameterSet.begin();
  std::vector<Parameter*>::iterator rEnd = m_parameterSet.end();
  while ( r != rEnd )
  {
          (*r)->reindex(parameterOffset);
          ++r;
  }  
}

size_t Component::incrementModelParameterCount ()
{

  return m_parent->incrementModelParameterCount();
}

size_t Component::incrementModelComponentCount ()
{

  return m_parent->incrementModelComponentCount();
}

size_t Component::modelIndex () const
{
  return m_parent->modelIndex();
}

Parameter* Component::getLocalParameter (size_t i) const
{

  return m_parent->getLocalParameter(i);
}

Parameter* Component::localParameter (size_t i) const
{
  std::vector<Parameter*>::const_iterator vp = m_parameterSet.begin();
  std::vector<Parameter*>::const_iterator vpEnd = m_parameterSet.end();
  Parameter* result(0);

  while ( vp != vpEnd )
  {
          if ((*vp)->index() == i) 
          {
                  result = *vp;
                  break;
          }
          ++vp;
  }

  return result;  
}

void Component::linkParametersToPrimary ()
{
  std::vector<Parameter*>::iterator i(m_parameterSet.begin());
  std::vector<Parameter*>::iterator iEnd(m_parameterSet.end());
  int base(parameterIndexBase());   
  while (i != iEnd )
  {
        std::ostringstream linkString;
	string modelName = m_parent->parent()->name();
        linkString << "=  ";
        int linkTo  = (*i)->index() - base;
        linkString << modelName << ":" << linkTo ;
        (*i)->modify(linkString.str());
        ++i;       
  }        
}

const ArrayContainer& Component::getEnergyArray () const
{

  return m_parent->energyArray();
}

SumComponent& Component::operator * (SumComponent& right) const
{
   return right;
}

const string& Component::currentType ()
{

  return s_currentModelInfo.type;
}

const string& Component::currentModelName ()
{

  return s_currentModelInfo.name;
}

void Component::currentType (const string& value)
{

  s_currentModelInfo.type = value;
}

void Component::currentModelName (const string& value)
{

  s_currentModelInfo.name  = value;
}

void Component::debugPrint (std::ostream& s, const string& descript) const
{
  if (static_cast<XSstream&>(s).maxChatter() < 40) return;
  using namespace std;

  UniquePhotonContainer::const_iterator c = uniquePhotonArray().begin();
  UniquePhotonContainer::const_iterator cEnd = uniquePhotonArray().end();

  while (c != cEnd)
  {
     const RealArray& fluxArray = c->second;
     if (fluxArray.size())
     {
        const RealArray& energy = c->first->energy();
        const RealArray& fluxErrArray = uniquePhotonErrArray(c->first);
        bool fluxError( isError());
        s << descript << " name: " << name() << "\n";
        s << setw(4) << " I " << setw(15) << " Energy (keV) " 
              << setw(15) << " Flux  ";
        if (fluxError) s << setw(15) << " Error ";                
        s << "\n";

        size_t nEar(energy.size() - 1);
        for (size_t k = 0; k < nEar; ++k)
        {
                s << setw(4) << k + 1 << setw(15) << energy[k + 1] 
                                << setw(15) << fluxArray[k] ;
                if (fluxError) s  << setw(15) << fluxErrArray[k];
                s << '\n';
        }
        s << flush;
     } 
     ++c; 
  }     
}

Parameter* Component::parameter (const string& paramName) const
{

  std::vector<Parameter*>::const_iterator r = m_parameterSet.begin();
  std::vector<Parameter*>::const_iterator rEnd = m_parameterSet.end();

  Parameter* found = 0;

  while ( !found && r != rEnd )
  {
          if ( (*r)->name() == paramName )
          {
                  found = *r;
          }
          ++r;       
  }
  return found;   
}

const RealArray& Component::energyArray (size_t spectrumNumber) const
{
  const ArrayContainer& energies = getEnergyArray();
  ArrayContainer::const_iterator f = energies.find(spectrumNumber);
  if ( f != energies.end()) 
  {
          return f->second;
  }
  else
  {
          throw RedAlert("Energy Array seek error: Component");       
  }
}

size_t Component::modfileLocation () const
{
  ComponentInfo n (XSModelFunction::fullMatch(m_name));
  return n.location;  
}

string Component::modfileSource () const
{
  ComponentInfo n (XSModelFunction::fullMatch(m_name));
  return n.sourceFile;     
}

void Component::clearArrays (const std::set<UniqueEnergy*>& currentUniqueEngs)
{
  UniquePhotonContainer::iterator d (m_uniquePhotonArray.begin());
  UniquePhotonContainer::iterator dEnd (m_uniquePhotonArray.end());

  while  (d != dEnd)
  {
     // I really don't like doing this const_cast, but
     // it's only to allow d->first to be used as a key.
     // It doesn't actually modify what doomed points to.
     UniqueEnergy* doomed = const_cast<UniqueEnergy*>(d->first);
     if (currentUniqueEngs.find(doomed) == currentUniqueEngs.end())
     {
        m_uniquePhotonArray.erase(d++);  
        UniquePhotonContainer::iterator itErr = 
                m_uniquePhotonErrArray.find(doomed);
        if (itErr != m_uniquePhotonErrArray.end())
           m_uniquePhotonErrArray.erase(itErr);     
     }
     else ++d;
  }
}

std::vector<ModParam*> Component::getVariableParameters () const
{
  std::vector<Parameter*>::const_iterator ps = m_parameterSet.begin();        
  std::vector<Parameter*>::const_iterator psEnd = m_parameterSet.end(); 

  std::vector<ModParam*> vp;
  vp.reserve(m_parameterSet.size());
  while (ps != psEnd)
  {
     ModParam* t = dynamic_cast<ModParam*>(*ps);
     if (t)
     {
        if ( !t->isFrozen() && !t->isLinked()) vp.push_back(t);       
     }
     ++ps;       
  }       
  return vp;
}

bool Component::isGroup () const
{
  return (m_name.compare(0,6,ComponentGroup::GROUPTEST()) == 0);
}

void Component::cloneParameters (Component& left, const Component& right)
{
  size_t N(right.m_parameterSet.size());
  std::vector<Parameter*> clonedParameterSet(N,0);
  for ( size_t j = 0; j <  N; ++j )
  {
        clonedParameterSet[j] = right.m_parameterSet[j]->clone(&left);
  }  
  
  left.setParameterSet(clonedParameterSet);
}

const NameCacheType& Component::nameCache ()
{
  return XSModelFunction::nameCache();
}

IntegerArray Component::parameterIndices () const
{
  size_t N(m_parameterSet.size());
  IntegerArray indices(N);
  for ( size_t j = 0; j < N; ++j) indices[j] = m_parameterSet[j]->index();
  return indices;
}

int Component::resequenceParameters (int startValue)
{
  // resequence parameters in this component and return the last 
  // index number allocated.      
  int N(m_parameterSet.size());
  int newIndex(startValue);
  for (int i = 0  ; i < N; ++i, ++newIndex) 
  {
     m_parameterSet[i]->index(newIndex);  
  }  
  return newIndex;
}

void Component::deregisterParameters () const
{
  std::vector<Parameter*>::const_iterator r = m_parameterSet.begin();
  std::vector<Parameter*>::const_iterator rEnd = m_parameterSet.end();
  int Index = modelIndex();  
  while ( r != rEnd )
  { 
     XSContainer::models->deregisterParameter((*r)->index(),Index);          
     ++r;       
  }   
}

const ModelBase* Component::root () const
{
  return m_parent->parent();
}

const std::set<UniqueEnergy*>& Component::getUniqueEnergies () const
{
   return m_parent->uniqueEnergyArray();
}

void Component::saveUniquePhotonArray (bool setSaveFlag)
{
  // idea is that saveUniquePhotonArray will be called with fluxSaved false
  // at the start of a fit to get the array sizes right. During the iteration 
  // the save operation will do a simple copying of elements not an allocation,
  // with fluxSaved true.

  if (setSaveFlag)
  {     
        UniquePhotonContainer::const_iterator i = 
                m_uniquePhotonArray.begin();
        UniquePhotonContainer::const_iterator iEnd = 
                m_uniquePhotonArray.end();
        while (i != iEnd)
        {
	        savedUniquePhotonArray(i->first,i->second);
	        ++i;
        }
        m_fluxSaved = true;
  }
  else
  {
        m_savedUniquePhotonArray = m_uniquePhotonArray;          
  }
}

void Component::restoreUniquePhotonArray ()
{
  if (m_fluxSaved) 
  {
          UniquePhotonContainer::const_iterator i = 
                        m_uniquePhotonArray.begin();
          UniquePhotonContainer::const_iterator iEnd = 
                        m_uniquePhotonArray.end();
          UniquePhotonContainer::const_iterator s = 
                        m_savedUniquePhotonArray.begin();
	  while (i != iEnd)
	  {
		uniquePhotonArray(i->first,s->second);
		++i, ++s;
	  }
          m_fluxSaved = false;
  }
}

void Component::initializeForFit ()
{
}

bool Component::usingMdef (const XSCall<MathExpression>* mdef) const
{
   // mdef can be implicitly converted to an XSModelFunction*.
   return m_generator == mdef;
}

// Additional Declarations
std::ostream & operator<<(std::ostream &stream,const Component &right)
{
   using namespace std;

   if (!right.parameterSet().empty() )
   {
      for (std::vector<Parameter*>::const_iterator is = right.parameterSet().begin();
              is != right.parameterSet().end(); ++is) 
      {       
         stream << **is;
      }               
   }
   return stream;        
}    

const string& Component::parentName () const
{
  return m_parent->parentName();
}

