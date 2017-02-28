//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
#include <typeinfo>

// ModParam
#include <XSModel/Parameter/ModParam.h>
// Response
#include <XSModel/Data/Detector/Response.h>
// ModelBase
#include <XSModel/Model/ModelBase.h>
// ComponentGroup
#include <XSModel/Model/Component/ComponentGroup.h>
// Component
#include <XSModel/Model/Component/Component.h>
#include <XSModel/Model/Component/MixComponent.h>
#include <XSModel/Model/Component/AMXComponent.h>
// SumComponent
#include <XSModel/Model/Component/SumComponent.h>
// iostream
#include <iostream>
// ModelContainer
#include <XSModel/GlobalContainer/ModelContainer.h>
// ResponseContainer
#include <XSModel/GlobalContainer/ResponseContainer.h>
// ComponentListTypes
#include <XSModel/Model/Component/ComponentListTypes.h>
// XSparse
#include <XSUtil/Parse/XSparse.h>
// DataContainer
#include <XSModel/GlobalContainer/DataContainer.h>
// Model
#include <XSModel/Model/Model.h>

#include <XSUtil/FunctionUtils/XSModelFunction.h>
#include <XSUtil/FunctionUtils/ComponentInfo.h>
#include <XSModel/Data/Detector/DummyResponse.h>
#include <XSModel/Data/BackCorr/Background.h>
#include <XSModel/Parameter/ParameterLink.h>
#include <XSUtil/Numerics/Integrate.h>
#include <XSUtil/Parse/MathExpression.h>
#include <XSUtil/Parse/XSModExpTree.h>
#include <XSUtil/Utils/XSstream.h>
#include <XSstreams.h>
#include <XSContainer.h>
#include <memory>
#include <sstream>
extern const std::string DUMMY_RSP;
using std::endl;


// Class Model::ContextError 

Model::ContextError::ContextError (const string& name, const string& yesno, const string& context)
{
  string errMessage = "Error: component " + name +  "  " + yesno + " be additive in context: " +
        context;
  tcerr << errMessage << endl; 
}

Model::ContextError::ContextError (const string& errMessage)
{
  tcerr << errMessage << endl;
}


// Class Model::NotEnoughParameterStrings 

Model::NotEnoughParameterStrings::NotEnoughParameterStrings (const string& component)
        :YellowAlert("Error: insufficient parameter values supplied")
{
    tcerr << ": parameters not set for component " << component << endl;
}


// Class Model::NoSourceForModel 

Model::NoSourceForModel::NoSourceForModel()
  : YellowAlert(" no corresponding source exists for model source number\n")
{
}


// Class Model::ComponentNotPresent 

Model::ComponentNotPresent::ComponentNotPresent (const string& message)
  : YellowAlert (" component not present: ")
{
  tcerr << message << '\n';
}


// Class Model::InvalidModelEdit 

Model::InvalidModelEdit::InvalidModelEdit (const string& message)
  : YellowAlert(" invalid edit command: ")              
{
  tcerr << message << '\n';
}


// Class Model::EmptyModelResults 

Model::EmptyModelResults::EmptyModelResults()
   : YellowAlert(" cannot delete the last component in the model ")             
{
}


// Class Model::WrongDataForMixing 

Model::WrongDataForMixing::WrongDataForMixing (const string& diag)
  : YellowAlert (" data loaded incompatible with mixing component ")                
{
  tcerr << diag << '\n';
}


// Class Model 
const string Model::s_DEFAULT = "_DEFAULT";

Model::Model(const Model &right)
      : m_modelBase(right.m_modelBase->clone())
{
}

Model::Model (const string& name, size_t source, const string& modelArgs)
{
  std::auto_ptr<ModelBase> apModBase(new ModelBase(name, source));
  m_modelBase = apModBase.get();
  Response* dummyResponse(XSContainer::responses->responseList().find(DUMMY_RSP)->second);
  attachResponse(dummyResponse);
  if (XSContainer::models->autonomousEnergy().size())
  {
     m_modelBase->setAutonomousEnergy(XSContainer::models->autonomousEnergy());
  }
  // This merely puts the singleton dummy energies for spectrum 0
  // (or autonomousEnergy if it exists)into Model's energy container.
  m_modelBase->fillEnergyContainer();
  // this can throw
  define(modelArgs);
  m_modelBase = apModBase.release();
  // finally, put parameters in order in which the user entered them.
}


Model::~Model()
{
  destroy();
}


std::deque<string> Model::getParamValuesFromUser ()
{
  std::deque<string> returnString;


   // tasks: a) get prompt string
   //        b) get current value string
   //        c) print current value string
   //        d) set prompt
   //        e) extract user input for each parameter.

   //safety...
   returnString.clear();
   const string& skipAll  = XSparse::SKIP();

   std::ostringstream parPromptStream("");
   // print out model name and datagroup number if not default
   // values.
   if ( m_modelBase->name() != Model::DEFAULT() ) parPromptStream << m_modelBase->name() << ':';      
   if ( XSContainer::datasets->numberOfGroups() > 1 ) 
   {
      parPromptStream << "data group " << dataGroupNumber() << ':' ;
   }                 
   tcout <<  "\nInput parameter value, delta, min, bot, top, and max values for ...\n";

   std::vector<Component*> orderedComps;
   bundleComponents(orderedComps);

   for (size_t i=0; i<orderedComps.size(); ++i)
   {
      Component* comp = orderedComps[i];
      std::vector<string> componentParams;
      comp->getParamValues(parPromptStream.str(),componentParams);
      if (componentParams.size())
      {
         std::copy(componentParams.begin(),componentParams.end(),
                     back_inserter(returnString));
         if (componentParams.back() == skipAll) break;
         if (returnString.back() == skipAll)  break;
      }
   } 

  return returnString;
}

void Model::setParamValues (std::deque<string>& paramStrings, bool& skipEntered)
{

  // component parameter setting can throw an exception.     

  // check for infinite skip character, and suppress exception for
  // incorrect number of strings input if this is set.

  size_t nSettings = paramStrings.size();

  // When coming from a batch file, paramStrings originally can contain
  // entries for ALL data group copies.  We only want to know if a skip
  // is entered for the 1 current model copy.  So... 
  size_t nToCheck = std::min(nSettings, numberOfParameters());

  size_t sk = 0;
  while ( sk < nToCheck && !skipEntered )
  {
     // test for blank or trivial strings again since the user
     // could have entered a link string with just an "=".
     int first = paramStrings[sk].find_first_not_of(" \t");
     if ( first >= 0 && paramStrings[sk].substr(first).length() >= 2)
     {
        skipEntered = (paramStrings[sk].substr(first,2) == XSparse::SKIP());
     }
     ++sk;
  }    

  //if ( !skipEntered && nSettings < numberOfParameters() ) throw NotEnoughParameterStrings(name());
  // try to be a little friendlier than throwing an exception.
  if ( !skipEntered && nSettings < numberOfParameters() ) 
  {
     paramStrings.push_back(XSparse::SKIP());
     skipEntered = true;       
  }

  // in the case where a skip has been entered during or after  setting
  // parameters in the definition of a model, AND there are > 1 data groups
  // involved, we want to do something quite different.
  std::vector<Component*> orderedComps;
  bundleComponents(orderedComps);
  const size_t nComps = orderedComps.size();

  bool reachedSkip(false);
  size_t paramCount(0);
  size_t NdGroup (dataGroupNumber());
  // If lowestGroup = 0, model will be active/off.
  size_t lowestGroup = XSContainer::datasets->getLowestGroupForSource(sourceNumber());

  for (size_t i=0; i<nComps; ++i)
  {
      Component* current = orderedComps[i];
      // parameters in the current component are popped off the input deque
      // and added to the setParameters call for the component.
      size_t currentComponentNumParams = current->numParams();
      std::vector<string> curParamSettings;
      // note that j counts the number of parameters in the current component
      // only, not the complete model.
      if ( !reachedSkip || (reachedSkip && lowestGroup && NdGroup > lowestGroup))
      {
         size_t  j(0);
         while  (j < currentComponentNumParams)
         {   
            bool firstSkip(false);
            if (!reachedSkip)
            {
               // was a 'skip this and the rest' string entered?
               const string thisSetting(paramStrings.front());
               size_t lenBack(std::min(XSparse::SKIP().length(),thisSetting.length()));      
               reachedSkip = thisSetting.substr(thisSetting.find_first_not_of(" \t"),lenBack) 
                                       == XSparse::SKIP();  
               curParamSettings.push_back(paramStrings.front());
               paramStrings.pop_front();
               if ( reachedSkip )
               {
                  // tell future settings to skip [use default values]. 
                  // All the strings
                  // from this point forth will be ignored.
                  firstSkip = true;
                  paramStrings.clear();
                  paramStrings.push_back(XSparse::SKIP());                                        
               }
            }
            if (lowestGroup && NdGroup > lowestGroup)
            {
               // automatically make a link.
               if (reachedSkip) 
               {
                  if (firstSkip) curParamSettings.pop_back();
                  std::ostringstream linkString;
                  linkString << "= " << name() << ":" << paramCount + j + 1;
                  curParamSettings.push_back(linkString.str());
               }      
               else
               {
                  // catch the 'set these parameters to default' condition
                  // for higher datagroups - need to make a link rather than
                  // use the default.
                  const string& thisSetting = curParamSettings.back();
                  int nulltest = thisSetting.find_first_not_of(" \t");
                  if ( thisSetting == "/"  || thisSetting.length() == 0 
                          || nulltest < 0)
                  {
                     std::ostringstream linkString;
                     linkString << "= " << name() << ":" << paramCount + j + 1;
                     curParamSettings.pop_back();
                     curParamSettings.push_back(linkString.str());
                  }
          }    
            }
            ++j;
         } 
         paramCount += j;
      }
      else
      {
         curParamSettings.push_back(XSparse::SKIP());
         paramCount +=  currentComponentNumParams;     
      }

      current->setParamValues(curParamSettings);
  }  
}

bool Model::operator < (const Model& right) const
{

  return m_modelBase->name() < right.m_modelBase->name();
}

bool Model::operator > (const Model& right) const
{

  return m_modelBase->name() > right.m_modelBase->name();
}

void Model::destroy () throw ()
{
  delete m_modelBase; 
  m_modelBase = 0;
}

void Model::checkContext (const ModelExprTree& expressionTree)
{
  static const string ADD("add");   
  static const string SUM("sum");   
  static const string CON("con");   
  static const string MIX("mix");   
  static const string MUL("mul"); 
  static const string ACN("acn");  
  static const string AMX("amx");   

  // This assumes that expression syntax has already been checked.  It
  // performs checking related to XSPEC model semantics.

  // IMPORTANT: This is also called when testing an addcomp or delcomp
  // operation.  For the insertion case, a new dangling component will
  // have been placed in a new sub-expression by the time it gets here.
  // However, a deletion check will arrive here BEFORE the full tree
  // is re-evaluated.  Therefore sub-expr a(b+c) can show up here as
  // b+c.  Also, the sub-expr might be empty.  

  // rule 1 : Mix or convolution must not be last, or followed by ')'.
  // rule 2:  1 and only 1 add comp must exist within each '+' delimited 
  //          segment inside parentheses which enclose 1 or more '+'.
  //          Depth level is irrelevant for this rule.
  // rule 3:  total number of add/group comps = number of '+' plus one.

  ModelExprTree::const_iterator itExpr = expressionTree.begin();
  ModelExprTree::const_iterator itExprEnd =  expressionTree.end();
  while (itExpr != itExprEnd)
  {
     const ModelExpression<ModExprTreeMember>& current = *itExpr;
     const std::vector<AbstractExpression::TokenType>& tokens = current.tokenList();
     const std::vector<ModelExpression<ModExprTreeMember>::ComponentSpec>& comps =
                current.compSpecs();
     std::vector<size_t> addLocs;

     // Loop through components checking rule 1, and storing location
     // of all add or group components.     
     for (size_t iComp=0; iComp<comps.size(); ++iComp)
     {
        const string& compName = comps[iComp].content;
        const bool isGroup = 
                (compName.find(ComponentGroup::GROUPTEST()) != string::npos);
        if (isGroup)
        {
           Component::currentType(SUM);
           Component::currentModelName(compName);
        }
        else
           Component::getSetupData(compName);        
        const bool isAdditive = (isGroup || Component::currentType() == ADD);
        const size_t compLoc = current.getComponentLocation(iComp);
        if (isAdditive)
           addLocs.push_back(compLoc);

        // Stand-alone comp must be additive.  This is a special case
        // of rule 3, but it's easy to check and get out of the way.
        if (comps.size() == 1)
        {
           if (!isAdditive)
              throw ContextError("Error: standalone model must be additive");
           else
           {
              break;  // We're done with this sub-expression.
           }
        }

        // rule 1 
        std::pair<size_t,size_t> preAndPost(current.findPreAndPostOp(compLoc));
        if (Component::currentType() == CON || Component::currentType() == ACN ||
              Component::currentType() == MIX || Component::currentType() == AMX)
        {
           if (preAndPost.second == string::npos ||
                tokens[preAndPost.second].type == AbstractExpression::Rbrace)
              throw ContextError("Error: convolution, pileup, or mix must be followed by another component.");
        }        
     } // end component loop

     // rules 2 and 3
     // First check rule 3 as it is the more general case.
     const std::vector<size_t>& pluses = current.plusLocs();
     const size_t nPluses = pluses.size();
     size_t curPlusIdx = 0;
     const size_t nAddComp = addLocs.size();
     if (nAddComp > pluses.size() + 1)
        throw ContextError("Error: An add component cannot multiply another add component.");
     else if (nAddComp < pluses.size() + 1)
        throw ContextError("Error: Each '+' delimited subexpression must contain an add component.");

     // These refer to the optional 1 set of parentheses that may enclose '+' signs.
     const std::pair<size_t,size_t> groupParens = current.parenLocs();
     size_t startSegment = (groupParens.first == string::npos) ? 0 :
                groupParens.first;
     size_t endSegment = nPluses ? pluses[0] : current.exprString().length();

     if (endSegment <= startSegment)
        throw RedAlert("Error in Model::checkContext while parsing group parentheses.");

     // Must have exactly 1 add/group comp for each segment.  From above,
     // we already know we have at least one add/group comp.
     size_t curAddIdx = 0;
     do
     {
        size_t addFound = 0;
        while (curAddIdx < nAddComp && addLocs[curAddIdx] < startSegment)
           ++curAddIdx;
        while (curAddIdx < nAddComp && addLocs[curAddIdx] < endSegment)
        {
           ++addFound;
           ++curAddIdx;
        }
        if (addFound != 1)
           throw ContextError("Error: each subexpression must contain 1 and only 1 additive component.");

        startSegment = endSegment;
        if (curPlusIdx+1 >= nPluses)
        {
           // No more '+' signs, get either closing ')' or end.
           if (groupParens.second == string::npos)
              endSegment = current.exprString().length();
           else
              endSegment = groupParens.second;
        }
        else
        {
           ++curPlusIdx;
           endSegment = pluses[curPlusIdx];
        }

     } while (startSegment != current.exprString().length() &&
                startSegment != groupParens.second);


     ++itExpr;
  }
}

void Model::createParts ()
{
  m_modelBase->createParts();
}

void Model::initializeMixingTransformation (bool isLowest, const IntegerArray allSpectraForMix) const
{
  // Call the 'initialize' function on each Mix component in Model. 
  //   But only do it if this is the lowest dg Model copy.
  const XSContainer::MixLocations& mixLocs = m_modelBase->mixingLocs();
  if (mixLocs.first.size())
  {
     std::vector<Component*> allComponents;
     m_modelBase->bundleComponents(allComponents);
     for (size_t i=0; i<mixLocs.first.size(); ++i)
     {
        MixComponent* mix = static_cast<MixComponent*>(allComponents[mixLocs.first[i]]);
        if (isLowest)
        {
           mix->addMixUtility();
           mix->initialize(allSpectraForMix);
        }
        else
        {
           mix->removeMixUtility();
        }
     }
  }
}

void Model::initializeAMXingTransformation (bool isLowest) const
{
  const XSContainer::MixLocations& mixLocs = m_modelBase->mixingLocs();
  if (mixLocs.second.size() && isLowest)
  {
     std::vector<Component*> allComponents;
     m_modelBase->bundleComponents(allComponents);
     for (size_t i=0; i<mixLocs.second.size(); ++i)
     {
        AMXComponent* amx = static_cast<AMXComponent*>(allComponents[mixLocs.second[i]]);
        amx->initialize();
     }
  }
}

void Model::parse (const string& modelString)
{
  ModelExpression<ModExprStandAlone> inputExpression;
  inputExpression.init(modelString.c_str(),false);
  m_modelBase->fullExpression(modelString);
  ModelExprTree expTree;
  inputExpression.createExpTree(expTree);
  m_modelBase->expressionTree(expTree);
}

void Model::define (const string& modelArgs)
{
  parse(modelArgs);
  // check that the model makes sense, i.e. every term appears in context (additive models
  // appear as singletons in groups composed of varying types: mixing  components first,
  // convolutions precede additives.
  checkContext(m_modelBase->expressionTree());   
  // create the components. 
  createParts();  
  // find whether there's a mixing model, which is applied globally.
  checkForMixing(); 
  // put the parameters in order they were entered.
  reindexParameters(); 
}

Model* Model::clone () const
{

  return new Model(*this);
}

const string& Model::name () const
{

  return m_modelBase->name();
}

size_t Model::sourceNumber () const
{

  return m_modelBase->sourceNumber();
}

size_t Model::dataGroupNumber () const
{

  return m_modelBase->dataGroupNumber();
}

void Model::dataGroupNumber (size_t group)
{
  m_modelBase->dataGroupNumber(group);
}

void Model::sourceNumber (size_t source)
{
  m_modelBase->sourceNumber(source);
}

bool Model::isActive () const
{
  return !m_modelBase->responseIsDummy();
}

void Model::registerParameters () const
{
  m_modelBase->registerParameters();
}

size_t Model::parameterIndexBase () const
{

  return m_modelBase->parameterIndexBase();
}

void Model::setIndex (size_t i)
{

  m_modelBase->index(i);
}

void Model::setDataGroupIndexing (size_t group)
{
  m_modelBase->setDataGroupIndexing(group);
}

void Model::attachResponse (Response* response)
{
  m_modelBase->attachResponse(response);
}

void Model::linkDataGroupParams ()
{

  // function to create parameter links between current model, which is a
  // data group model copy, and the parameters of the primary model.

  // safety measure to protect against self-referencing nonsense
  // which almost certainly won't be executed

  size_t lowestGroup = XSContainer::datasets->getLowestGroupForSource(sourceNumber());      
  if (!lowestGroup || dataGroupNumber() == lowestGroup) return; 

  XSModExpTree<ComponentGroup*>::const_iterator g = m_modelBase->compGroupTree().begin();
  XSModExpTree<ComponentGroup*>::const_iterator gEnd = m_modelBase->compGroupTree().end(); 

  while (g != gEnd)
  {
        ComponentListConstIterator lEnd((*g)->elements().end());
        ComponentListConstIterator l((*g)->elements().begin());
        while (l != lEnd)
        {
             // got to check whether a particular component is actually
             // a nested expression. If it is then do nothing here. 

             int gt = ((*l)->name()).find(ComponentGroup::GROUPTEST());     
             // if it's not, link each parameter to the one in the input (primary)
             // model.
             if (gt < 0) 
             {
                 (*l)->linkParametersToPrimary();
             }  
             ++l;        
        }  
        ++g;
  }    
}

void Model::makeActive ()
{
  if (isActive()) return;
  m_modelBase->makeActive();
}

void Model::makeInactive (const bool attachDummy)
{
  m_modelBase->makeInactive(attachDummy);
  if (attachDummy)
  {
     // Even though inactive models aren't going to be fit,
     // some (ie. table models) may still require some initialization
     // prior to calling calculate.
     std::vector<Component*> modelComps;
     m_modelBase->bundleComponents(modelComps);
     for (size_t i=0; i<modelComps.size(); ++i)
     {
        modelComps[i]->initializeForFit();
     }
     setComputeFlag(true);
     XSContainer::models->calculate(m_modelBase->name());
  }
}

void Model::calcComponents (bool saveComponentFlux, bool frozen)
{

  // the boolean frozen, which defaults to false, is set true for the purposes
  // of computing renormalization constants. If frozen is true then only the 
  // model components that have frozen or linked norms are added to the sum.      

  XSModExpTree<ComponentGroup*>::const_iterator itEnd=m_modelBase->compGroupTree().end();
  XSModExpTree<ComponentGroup*>::const_iterator it=m_modelBase->compGroupTree().begin();

  while (it != itEnd )
  {
        ComponentGroup* current = *it;
        // calculate all component groups. The nested groups will be
        // calculated recursively.
        // the CGroupBase::calculate method controls whether the component
        // calculation is up-to-date, so this loop may perform very few 
        // operations.
        if (!current->isNested()) 
           current->calculate(saveComponentFlux,frozen); 
        ++it;      
  }

  // set flag to stop unnecessary recomputation of components.
  // Note that this flag has no effect in mixing components since
  //   their calculation functions don't do anything.
  setComputeFlag(frozen);
  // We're computed, but not folded.
  m_modelBase->folded(false);
}

const RealArray& Model::modelFlux (size_t responseNumber) const
{
  return m_modelBase->modelFlux(responseNumber);
}

const RealArray& Model::foldedModel (size_t responseNumber) const
{
  return m_modelBase->foldedModel(responseNumber);
}

const RealArray& Model::modelFluxError (size_t responseNumber) const
{
  return m_modelBase->modelFluxError(responseNumber);
}

const RealArray& Model::foldedModelError (size_t responseNumber) const
{
  return m_modelBase->foldedModelError(responseNumber);
}

void Model::foldedModel (size_t responseNumber, const RealArray& value)
{
  m_modelBase->foldedModel(responseNumber, value);
}

void Model::foldedModelError (size_t responseNumber, const RealArray& value)
{
  m_modelBase->foldedModelError(responseNumber, value);
}

size_t Model::numberOfParameters () const
{
  return m_modelBase->modelParameterCount();
}

size_t Model::numberOfComponents () const
{
  return m_modelBase->modelComponentCount();
}

const string& Model::fullExpression () const
{

  return m_modelBase->fullExpression();
}

void Model::setComputeFlag (bool value)
{
  m_modelBase->setComputeFlag(value);
}

Model& Model::fold ()
{

  std::map<size_t,Response*>::const_iterator ri    = m_modelBase->detector().begin();
  std::map<size_t,Response*>::const_iterator riEnd = m_modelBase->detector().end();
  // folded() is set false by Model::evaluate.


  if ( !m_modelBase->folded() ) 
  {
     // The alignFluxForFold function is only for the case of
     // using autonomous energy arrays, when fluxers must be realigned
     // prior to doing a fold.  Their original state is stored in
     // the saveFlux maps.  If not using autonomous energies, the
     // function does nothing.
     ArrayContainer saveFlux;
     ArrayContainer saveFluxError;
     m_modelBase->alignFluxForFold(saveFlux, saveFluxError);

     // This copying of pointers from map to vector is performed solely for
     // the benefit of openMP, which prefers simple for loops with integer
     // indices rather than STL iterator loops (particularly prior to 
     // openMP 3.0).  If not using openMP, this is a negligable time hit.
     const int nResp = static_cast<int>(m_modelBase->detector().size());
     std::vector<Response*> respVec(nResp,static_cast<Response*>(0));
     for (int i=0; i<nResp; ++i, ++ri)
        respVec[i] = ri->second;

#pragma omp parallel for
     for(int i=0; i<nResp; ++i)
     {
        // convolve the response corresponding to each individual
        // spectrum with the model. Response::operator*(Model&)
        Response& R = *(respVec[i]);
        R*(*this);
     } 
     // Put the fluxes back to their originally aligned state. 
     m_modelBase->alignFluxForFold(saveFlux, saveFluxError);
     m_modelBase->folded(true);
  }

  return *this;
}

void Model::debugPrint (std::ostream& s) const
{
  // print out flux arrays after convolution.   
  using namespace std;
  try
  {
     XSstream& xscout = dynamic_cast<XSstream&>(s);  
     if (!m_modelBase->folded() || xscout.maxChatter() < 25) return;
     xscout << xsverbose(25);
     // write only to log file and then if logChatter is very large.
     ios_base::fmtflags save(s.flags());
     s << "Model ";
     using XSContainer::datasets;
     if (name() != Model::DEFAULT() ) s << name() << ':';
     s << fullExpression();  
     if (datasets->numberOfGroups() > 1 ) s << " Fit to Data Group: " << dataGroupNumber();
     if (sourceNumber() >= 1 ) s << " Source No.: " << sourceNumber();
     s << "\n folded flux arrays: ";

     std::map<size_t,Response*>::const_iterator ir  = m_modelBase->detector().begin();
     std::map<size_t,RealArray>::const_iterator fi  = m_modelBase->foldedModel().begin();
     std::map<size_t,Response*>::const_iterator irEnd  = m_modelBase->detector().end();
     const bool fluxError = (m_modelBase->foldedModelError().size() ==
                          m_modelBase->foldedModel().size());
     std::map<size_t,RealArray>::const_iterator fei = m_modelBase->foldedModelError().begin();

     s.precision(7);
     s.setf(ios_base::scientific);
     for ( ; ir != irEnd; ++ir,++fi)
     {
        if (!dynamic_cast<DummyResponse*>(ir->second))
        {
           s << "\n Convolved model for response"            
             << " associated with spectrum number: " << ir->second->spectrumNumber() 
             << "\n\n";
           s << setw(4) << " I " << setw(15) << " Channel " 
                   << setw(15) << " flux \n";
           if (fluxError) s << setw(15) << " error ";  
           const RealArray& fluxArray = fi->second;
           size_t N = ir->second->numChannels();
           const std::valarray<size_t>&  IN = ir->second->source()->indirectNotice();     
           size_t M = IN.size();
           if (!fluxError)
           {
              for (size_t k = 0; k < M; ++k)
              {

                 size_t kk = IN[k];
                 s << setw(4) << kk + 1 << setw(15) << fluxArray[kk]  << '\n';
              }                        
           }
           else
           {      
              const RealArray& fluxErrArray = fei->second;
              // If it exists, the error array should always
              // be the same size as the flux.  But just to
              // make sure...
              if (fluxErrArray.size() == fluxArray.size())
              { 
                 for (size_t k = 0; k < N; ++k)
                 {
                    size_t kk = IN[k];
                    s << setw(4) << kk + 1 << setw(15) << fluxArray[kk]
                      << setw(15) << fluxErrArray[kk]  << '\n';
                 }
              }
              ++fei;
           }
           s << '\n';                
        }
     }
     s << std::flush; 
     s.flags(save);      
     xscout << xsverbose();        
  }
  catch (bad_cast)
  {

  } 
}

bool Model::removeResponse (size_t spectrumNumber)
{
  return m_modelBase->removeResponse(spectrumNumber);
}

SpectralData* Model::spectrum (size_t index) const
{
  const Response* s (m_modelBase->detector(index));
  if (s)
  {
        return s->source();
  }
  else 
  {
        std::ostringstream  ss;
        ss << "retrieving spectrum through detector: " << index <<"\n";
        throw YellowAlert(ss.str());
  }
}

void Model::difference (ArrayContainer& df) const
{

  // Produce difference array between data D and folded model F,
  // one RealArray per spectrum. Arrays df are supplied by the StatMethod
  // and are presized to # noticed channels for each spectrum.

  // This will be called from each StatMethod if multiple StatMethods are
  // in use.  In that case the ArrayContainers df will only contain subsets
  // of the total number of loaded spectra.  So we can't assume there will
  // be a data array for every modelFlux array.  Nor can we assume there's
  // a modelFlux array for every data array.

  ArrayContainer::iterator itData = df.begin();
  ArrayContainer::iterator itDataEnd = df.end();
  ArrayContainer::const_iterator itFluxEnd = modelFlux().end();
  while (itData != itDataEnd)
  {
     const size_t specNum = itData->first;
     RealArray& diff = itData->second;
     ArrayContainer::const_iterator itFlux = modelFlux().find(specNum);
     if (itFlux != itFluxEnd)
     {
        const SpectralData* obs = spectrum(specNum);
        const std::valarray<size_t>& IN = obs->indirectNotice();
        if (diff.size() != IN.size())
        {
           std::ostringstream oss;
           oss <<"Array size error for spectrum "<<specNum
             << " in Model::difference.";
           throw RedAlert(oss.str());
        }
        diff -= foldedModel(specNum)[IN];
        if ( tpout.maxChatter() >= 33 )
        {
           RealArray spec (obs->spectrum()[IN]);
           tcout << " difference " << specNum << " source " << sourceNumber() << '\n';
           size_t N (diff.size());
           for (size_t j = 0; j < N; ++j)
           {
              tcout  << j  << " -= " << diff[j] << "  " 
                 << "  " << spec[j] << "  " <<  foldedModel(specNum)[IN[j]] << '\n';

           }
           tcout << " spcounts " << spec.sum() << " mcounts " << foldedModel(specNum)[IN].sum() << '\n';
           tcout << std::flush;
        }
     }
     ++itData;
  }
}

const std::map<size_t,Response*>& Model::detector () const
{

  return m_modelBase->detector();
}

const ArrayContainer& Model::foldedModel () const
{

  return m_modelBase->foldedModel();
}

const ArrayContainer& Model::foldedModelError () const
{

  return m_modelBase->foldedModelError();
}

const ArrayContainer& Model::modelFlux () const
{

  return m_modelBase->modelFlux();
}

std::list<ModParam*> Model::normParams () const
{

  return m_modelBase->normParams();
}

void Model::integrateFlux (size_t index, Real eMin, Real eMax, Real& flux, Real& eFlux) const
{

    const RealArray& energy = m_modelBase->energy(index);
    const RealArray& modelArray = modelFlux(index);

    std::pair<Real,Real> integral (Numerics::integrationKernel(energy,modelArray,eMin,eMax));

    flux = integral.first;
    eFlux = integral.second;
}

void Model::prepareForFit ()
{
   // safety check. This function should not be called for inactive models
   // since it destroys the dummy model calculation.
  if (!isActive()) return;     

  m_modelBase->clearArrays();

  saveComponentFlux();

  std::vector<Component*> modelComps;
  m_modelBase->bundleComponents(modelComps);
  for (size_t i=0; i<modelComps.size(); ++i)
  {
     modelComps[i]->initializeForFit();
  }
}

void Model::allButNorms (IntegerArray& paramsToFreeze) const
{

  m_modelBase->allButNorms(paramsToFreeze);
}

void Model::resetModelFlux (const ArrayContainer& saved)
{
  m_modelBase->setModelFlux(saved);
}

void Model::resetComponentFlux () const
{

  m_modelBase->resetComponentFlux();
}

void Model::saveComponentFlux () const
{

  m_modelBase->saveComponentFlux();
}

Real Model::countRate (size_t spectrumNumber)
{
  Real rate(0);
  // If spectrum isn't using this model, no reason to proceed.
  if (!spectrumNumber || 
        modelFlux().find(spectrumNumber) != modelFlux().end())
  {
     fold();
     if ( spectrumNumber != 0)
     {
        SpectralData* sp = spectrum(spectrumNumber);
        rate = (foldedModel(spectrumNumber)*sp->areaScale()).sum();                
     }
     else
     {
        ArrayContainer::const_iterator f = foldedModel().begin();
        ArrayContainer::const_iterator fEnd = foldedModel().end();
        while ( f != fEnd)
        {
           size_t index = f->first;
           SpectralData* sp = spectrum(index);
           rate += (foldedModel(index)*sp->areaScale()).sum();
           ++f;
        }       
     }
  }

  return rate;
}

void Model::storeDerivative (ArrayContainer& diff)
{


  std::map<size_t,Response*>::const_iterator  sp (m_modelBase->detector().begin());
  std::map<size_t,Response*>::const_iterator  spEnd (m_modelBase->detector().end());

  while ( sp != spEnd)
  {
        size_t index (sp->first);
        // If model obj for this data group is inactive, index will be 0.
        if (index)
        {
           const std::valarray<size_t>& IN = spectrum(index)->indirectNotice();
           diff[index] += m_modelBase->foldedModel(index)[IN];     
           if (tpout.logChatterLevel() >= 33 )
           {
                   tcout << " spectrum " << sp->first << " source " << sourceNumber() << '\n';
                   size_t N (diff[index].size());
                   for (size_t j = 0; j < N; ++j)
                   {
                           tcout << " J " << j  << " += " << diff[index][j] << "  " 
                                   << m_modelBase->foldedModel(index)[IN[j]] << '\n';

                   }
                   tcout << std::flush;
           }
        }
        ++sp;       
  }
}

Real Model::ergFlux (size_t index) const
{

  return m_modelBase->ergFlux(index);
}

Real Model::keVFlux (size_t index) const
{

  return m_modelBase->keVFlux(index);
}

void Model::ergFlux (size_t index, Real value) const
{

  m_modelBase->ergFlux(index,value);
}

void Model::keVFlux (size_t index, Real value) const
{

  m_modelBase->keVFlux(index,value);
}

const ArrayContainer& Model::energy () const
{

  return m_modelBase->energy();
}

std::pair<Real,Real> Model::keVFluxRange (size_t index) const
{

  return m_modelBase->keVFluxRange(index);
}

std::pair<Real,Real> Model::ergFluxRange (size_t index) const
{

  return m_modelBase->ergFluxRange(index);
}

void Model::keVFluxRange (size_t index, std::pair<Real,Real>& value)
{

  m_modelBase->keVFluxRange(index,value);
}

void Model::ergFluxRange (size_t index, std::pair<Real,Real>& value)
{

  m_modelBase->ergFluxRange(index,value);
}

void Model::reportFluxes (size_t spectrumNumber, Real redshiftFactor, bool lumin, Real eMin, Real eMax, Real confLevel) const
{
  using namespace std;       
  using XSContainer::datasets;
  const pair<Real,Real>& kevf = keVFluxRange(spectrumNumber);      
  const pair<Real,Real>& ergf = ergFluxRange(spectrumNumber);      
  bool errorReport ( kevf.first != 0 || kevf.second != 0);
  Real ergF = ergFlux(spectrumNumber);


  ios_base::fmtflags save (tcout.flags());


  if (datasets->numSourcesForSpectra() > 1) 
  {
          tcout << " Source " << sourceNumber() << std::endl;
  }

  if ( lumin )
  { 
      // energy flux  
          streamsize p(tcout.precision(5));
          tcout << "Model Luminosity " << setw(9) << showpoint 
                << ergF*1.0e44 << " ergs/s  (" << eMin*redshiftFactor << " - " 
                << eMax*redshiftFactor << " keV rest frame)" << std::endl;
          if ( errorReport )
          {
	        tcout.precision(4);
                tcout << "     Error range   "   << setw(9) << ergf.first*1.0e44
                      << " - "  << ergf.second*1.0e44  << "   (" << confLevel
                      << "% confidence)" << std::endl;    
          }
          tcout.precision(p);
  }
  else
  {
        // photon flux
          streamsize p(tcout.precision(5));
          tcout << " Model Flux " << setw(9) << keVFlux(spectrumNumber) << " photons ("
                << ergF << " ergs/cm^2/s) range (" 
                << showpoint << eMin << " - "   << eMax << " keV)" << std::endl;  
          if ( errorReport )
          {
                tcout.precision(4);
                tcout << "     Error range "  << setw(9) << kevf.first
                      << " - "  << kevf.second << "    (" << ergf.first 
                      << " - " << ergf.second << ")  (" << confLevel 
                      << "% confidence)" << std::endl;
          } 
          tcout.precision(p);              
  }
  tcout.flags(save);     
}

const std::list<SumComponent*>& Model::sources () const
{

  return m_modelBase->componentSource();
}

void Model::clearSources () throw ()
{
  m_modelBase->clearSources();
}

void Model::foldSources ()
{
  std::map<size_t,std::list<std::pair<RealArray,RealArray> > > sourceFluxes;
  std::map<size_t,Response*>::const_iterator ri    = m_modelBase->detector().begin();
  std::map<size_t,Response*>::const_iterator riEnd = m_modelBase->detector().end();
  const std::list<SumComponent*>& components = sources(); 

  ri = m_modelBase->detector().begin();
  for ( ; ri != riEnd; ++ri)
  {
     // for every Response that the model is attached to, convolve
     // all of the component sources [ Response::operator*(SumComponent&) const ]
     Response& R = *(ri->second);
     std::list<SumComponent*>::const_iterator ci = components.begin();
     std::list<SumComponent*>::const_iterator ciEnd = components.end();
     for (  ; ci != ciEnd ; ++ci )
     {
        // See comments in fold() as to why alignment is performed.
        ArrayContainer saveFlux;
        ArrayContainer saveErrFlux;
        m_modelBase->alignFluxForFold(saveFlux,saveErrFlux,*ci);
        SumComponent& source = (**ci);
        R*source;
        m_modelBase->alignFluxForFold(saveFlux,saveErrFlux,*ci);        
     }
  }  
}

std::list<std::pair<Real, Real> > Model::integrateSourceFlux (size_t index, Real eMin, Real eMax) const
{
    std::list<std::pair<Real, Real> > sourceFlux;   
    const RealArray& energy = m_modelBase->energy(index);
    std::list<SumComponent*>::const_iterator sl(m_modelBase->componentSource().begin());
    std::list<SumComponent*>::const_iterator slEnd(m_modelBase->componentSource().end());
    while ( sl != slEnd)
    {
        // cast the pointer *to* const to call the public version of 
        // Component::photonArray(size_t)
        const RealArray& sourceFluxArray = const_cast<const SumComponent*>(*sl)->photonArray(index);
        sourceFlux.push_back(Numerics::integrationKernel(energy,sourceFluxArray,eMin,eMax));      
        ++sl;  
    }
    return sourceFlux; 
}

const Response* Model::detector (size_t spectrumNumber) const
{

  return m_modelBase->detector(spectrumNumber);
}

size_t Model::index ()
{

  return m_modelBase->index();
}

void Model::deleteComponent (int componentIndex)
{
  int group(0);
  int componentOffsetInGroup(0);
  //  grab a token for the component to be deleted. If the erasure is successful
  //  in the ModelBase::deleteComponent call
  if ( numberOfComponents() == 1 ) 
  {
          throw InvalidModelEdit( "<empty>.\n*** Use model [modelName] none to remove models" );
  }

  Component* doomed (componentByNumber(componentIndex,group,componentOffsetInGroup));

  if ( doomed )
  {
          // gr is an iterator to the Expression string in the component group
          // container the component.
          // Now we can delete it using its component Index

          ModelExprTree contextTest (m_modelBase->expressionTree());
          ModelExpression<ModExprTreeMember>* gr = &(contextTest.values()[group]);
          try
          {
                const string MIX("mix");
                const string AMX("amx");
                gr->deleteWordBySequence(componentIndex);
                // Note that deleteWordbySequence does not create a new sub-expression, 
                // even though one might be necessary.  
                string fullExpString (ModelBase::refreshExpression(contextTest));
                ModelExpression<ModExprStandAlone> step1Expression;
                step1Expression.init(fullExpString,false);
                ModelExprTree step1Tree;
                step1Expression.createExpTree(step1Tree);
                checkContext(step1Tree);

		deleteComponentParamInfo(doomed);

          	int numberOfComponentsBeforeDelete ( numberOfComponents() );

		// remove component from the list of those in the relevant component
                // group. Exceptions thrown by this operation will be caught in the 
		// driver.
                m_modelBase->deleteComponent(group,componentOffsetInGroup,componentIndex);

                // fix the indexing on the components and parameters to reflect the
                // new model, if necessary - recall component indices are 1-based.
                if (componentIndex < numberOfComponentsBeforeDelete )
		    reindexComponents(componentIndex+1);

                // update the string characterizing the model. singleton component
                // groups may be deleted, so in general need to do this.
                string newFullExpression (ModelBase::refreshExpression(m_modelBase->expressionTree()));
                ModelExpression<ModExprStandAlone> newExpression;
                newExpression.init(newFullExpression,false);
		m_modelBase->fullExpression(newFullExpression);
                ModelExprTree newExpTree;
                newExpression.createExpTree(newExpTree);
                m_modelBase->expressionTree(newExpTree);
                m_modelBase->recreateParts();

                // change the index base for data groups now that number of parameters 
                // has been changed.
                m_modelBase->setDataGroupIndexing(dataGroupNumber());
                // reindex and reregister all parameters in the model.
                reindexParameters();

                checkForMixing();
                // This code block is an addcomp/delcomp/edit patch fix that  
                // really should be moved into a new ParamLinkList function. 
                // Also, perhaps it could instead be called from the handlers 
                // after ALL the model objects have been edited, to spare an
                // O(N^2) operation, but for now this seems safer.
                std::map<Parameter*,ParameterLink*>::const_iterator itLinks =
                         ParamLinkList::Instance()->linkList().begin();
                std::map<Parameter*,ParameterLink*>::const_iterator itLinksEnd =
                         ParamLinkList::Instance()->linkList().end();
                while (itLinks != itLinksEnd)
                {
                   itLinks->second->regenerateExpression();
                   ++itLinks;
                }         

                registerParameters();

          }
          catch ( YellowAlert& )
          {
                  std::ostringstream msg;
                  msg << "delcomp ";
                  if ( name() != s_DEFAULT ) msg << name() << ": "; 
                  string subex (gr->exprString().empty() ? string("<empty>"): gr->exprString());
                  msg << componentIndex << " (subexpression " << subex << ")";
                  throw InvalidModelEdit(msg.str());
                  // throw here and have not altered the model == strong exception safe.
          }


  }
  else
  {       
        std::ostringstream msg;
        string mname = (name() != DEFAULT() ? name() : string(""));
        if (!mname.empty()) mname += " "; 
        msg  << mname << "with index: " << componentIndex ; 
        throw ComponentNotPresent(msg.str());       
  }
}

void Model::insertComponent (int componentIndex, const string& componentName)
{
    static const string ADD("add");   
    int group(0);
    int componentOffsetInGroup(0);
    //  find the location of the insertion point in the set of component groups.
    //  (we don't  need the return value from componentByNumber, except to know
    //  that it is non-null).
    //  note that for insertion at the start  any  recognizable component 
    //  is valid input.

    Component* insertionPoint = componentByNumber(componentIndex,group,componentOffsetInGroup);

    if (!insertionPoint)
    {
        // entered componentNumber is too high, use the next one.
        componentIndex = numberOfComponents() + 1;       
    }

    // gr is a pointer to the Expression string in the component group
    // containing the component.
    // Now we can edit it using its component Index

    // contextTest is a copy of Model's expression string tree used for
    // testing whether the model resulting after edit is syntactically valid.

    ModelExprTree contextTest (m_modelBase->expressionTree());
    ModelExpression<ModExprTreeMember>* gr = &(contextTest.values()[group]);
    try
    {
        Component::getSetupData(componentName);
        char op('*');
        if ( Component::currentType() == ADD ) op = '+';

        gr->insertWordBySequence(componentIndex,Component::currentModelName(),op);
        // Note that insertWordbySequence does not create a new sub-expression, 
        // even though one might be necessary.  Nor does insertComponent create
        // a new ComponentGroup.  That's why we go through the rigamarole
        // below with creating new trees and calling recreateParts.
        string fullExpString (ModelBase::refreshExpression(contextTest));
        ModelExpression<ModExprStandAlone> step1Expression;
        step1Expression.init(fullExpString,false);
        ModelExprTree step1Tree;
        step1Expression.createExpTree(step1Tree);
        checkContext(step1Tree);

        // If we've reached here, it's OK to call things which will modify 
        // the model.  Nothing after insertComponent ought to throw.

        // insert component in the list of the relevant component group. This will
        // redo the 'insertWordBySequence' only this time it will change Model's expression.
        // exceptions thrown by this operation will be caught in the driver.

        m_modelBase->insertComponent(group,componentOffsetInGroup,componentIndex,componentName, op);
        // The new component may have changed its expression during creation if it was a 
        // table model and required re-prompting for file, which is why we need to create 
        // yet another tree.

        fullExpString = ModelBase::refreshExpression(m_modelBase->expressionTree());
        ModelExpression<ModExprStandAlone> newExpression;
        newExpression.init(fullExpString,false);
        ModelExprTree newTree;
        newExpression.createExpTree(newTree);

        // change the index base for data groups now that number of parameters
        // has been changed.
        m_modelBase->setDataGroupIndexing(dataGroupNumber());
        // update the string characterizing the model, and rewrite the component group list
        // as necessary (required because of the possibility of the nesting situation changing
        // as a result of the deletion.

        // update the model expression, and rearrange the components under component groups 
        // as necessary (simple edits like A*B -> A*B + C create a new component group,
        // so expect to need to do this in the general case.
        m_modelBase->fullExpression(fullExpString);
        m_modelBase->expressionTree(newTree);
        m_modelBase->recreateParts();
        // fix the indexing on the  parameters to reflect the
        // new model and reregister parameters.
        reindexParameters();

        // This code block is an addcomp/delcomp/edit patch fix that  
        // really should be moved into a new ParamLinkList function. 
        // Also, perhaps it could instead be called from the handlers 
        // after ALL the model objects have been edited, to spare an
        // O(N^2) operation, but for now this seems safer.
        std::map<Parameter*,ParameterLink*>::const_iterator itLinks =
                 ParamLinkList::Instance()->linkList().begin();
        std::map<Parameter*,ParameterLink*>::const_iterator itLinksEnd =
                 ParamLinkList::Instance()->linkList().end();
        while (itLinks != itLinksEnd)
        {
           itLinks->second->regenerateExpression();
           ++itLinks;
        }         

        registerParameters();
    }
    catch ( Component::NoSuchComponent )
    {
	std::ostringstream msg;
	msg << "addcomp ";
	if ( name() != s_DEFAULT ) msg << name() << ": "; 
	msg << componentIndex <<  " " << componentName ;
	throw InvalidModelEdit(msg.str());
    }
    catch ( YellowAlert& )
    {
	std::ostringstream msg;
	msg << "addcomp ";
	if ( name() != s_DEFAULT ) msg << name() << ": "; 
	string subex (gr->exprString().empty() ? string("<empty>"): gr->exprString());
	msg << componentIndex << " (subexpression " << subex << ")";
	throw InvalidModelEdit(msg.str());
	// throw here and have not altered the model == strong exception safe.
    }
}

Component* Model::componentByNumber (int seekIndex, int& groupNumber, int& componentOffset) const
{
  Component* target(0);
  // return a component with a given index from a model. As a side effect, return
  // the group offset (the iterator difference between the start and the group 
  // containing the target) and the offset (the iterator difference between the
  // target and the beginning of the group's component list).
  XSModExpTree<ComponentGroup*>::const_iterator p (m_modelBase->compGroupTree().begin());
  XSModExpTree<ComponentGroup*>::const_iterator pHigh (m_modelBase->compGroupTree().begin());
  XSModExpTree<ComponentGroup*>::const_iterator pEnd (m_modelBase->compGroupTree().end());
  groupNumber = 0;
  int highestIndex (0);  
  while ( p != pEnd)
  {
     ComponentListConstIterator c( (*p)->elements().begin());
     ComponentListConstIterator cEnd( (*p)->elements().end());
     componentOffset = 0;
     while (c != cEnd  && !((*c)->index() == seekIndex))
     {
        if ( (*c)->index() > highestIndex )
        {
           highestIndex = (*c)->index();
           pHigh = p;       
        }
        ++componentOffset;
        ++c;
     }
     if (  c != cEnd  )
     {
        target =  *c;
        groupNumber = m_modelBase->compGroupTree().position(p);
        break;
     }
     else
     {
        ++p;
     }
  }
  //what does this acomplish that wasn't acomplished in the above loop?
  if ( target == 0 )
  {
        // need to return the group that contains the highest index number.
        // pHigh will always be set since the index is always > 0;
        groupNumber = m_modelBase->compGroupTree().position(pHigh);
        componentOffset = (*pHigh)->elements().size();

  }
  return target;
}

void Model::decrementParameterCount (int by)
{
  m_modelBase->decrementParameterCount(by);
}

void Model::reindexComponents (int start, int by)
{
  // 'by' represents the number of components that have just been removed.

  // it is NEGATIVE for deletions. 'start' is the first index affected.
  int N (by < 0 ? numberOfComponents() - by : numberOfComponents());
  int group(0);
  int componentInGroup(0);
  for (int j = start; j <= N; ++j)
  {
        Component* current(componentByNumber(j,group,componentInGroup));
        current->index(j + by);
  }
}

void Model::reindexParameters ()
{
  reindexComponents(1,0);
  std::vector<Component*> orderedComps;
  bundleComponents(orderedComps);
  size_t N = orderedComps.size();
  int start(1 + parameterIndexBase());
  for (size_t j = 0; j < N; ++j)
  {
        start = orderedComps[j]->resequenceParameters(start);
  }
}

std::pair<size_t,size_t> Model::getComponentPeak (size_t compNumber, Real& compTotal)
{
  return m_modelBase->getComponentPeak(compNumber, compTotal);  
}

Real Model::calcContinuumFlux (size_t compNumber, size_t iMax, size_t specNum, Real fraction)
{
  return m_modelBase->calcContinuumFlux(compNumber, iMax, specNum, fraction);
}

void Model::deregisterParameters ()
{
  m_modelBase->deregisterParameters();
}

void Model::setParamValues (int componentIndex, std::deque<string>& paramStrings, bool& skipEntered)
{


  // component parameter setting can throw an exception.     

  // check for infinite skip character, and suppress exception for
  // incorrect number of strings input if this is set.

  size_t nSettings(paramStrings.size());

  int dummyGroup(0);
  int dummyOffset(0);

  Component* newComponent = componentByNumber(componentIndex,dummyGroup,dummyOffset);
  size_t newComponentNumParams (newComponent->numParams());
  IntegerArray parameterIndex (newComponent->parameterIndices());

  // When coming from a batch file, paramStrings originally can contain
  // entries for ALL data group copies.  We only want to know if a skip
  // is entered for the 1 current model copy.  So... 
  size_t nToCheck = std::min(nSettings, newComponentNumParams);

  size_t sk (0);
  while ( sk < nToCheck && !skipEntered )
  {
     // test for blank or trivial strings again since the user
     // could have entered a link string with just an "=".
     int first = paramStrings[sk].find_first_not_of(" \t");
     if ( first >= 0 && paramStrings[sk].substr(first).length() >= 2)
     {
        skipEntered = (paramStrings[sk].substr(first,2) == XSparse::SKIP());
     }
     ++sk;
  }    

  //if ( !skipEntered && nSettings < newComponentNumParams ) throw NotEnoughParameterStrings(name());
  if ( !skipEntered && nSettings < newComponentNumParams ) 
  {
     paramStrings.push_back(XSparse::SKIP());
     skipEntered = true;       
  }

  // in the case where a skip has been entered during or after  setting
  // parameters in the definition of a model, AND there are > 1 data groups
  // involved, we want to do something quite different. 

  bool reachedSkip(false);
  size_t base(parameterIndexBase());
  size_t NdGroup (dataGroupNumber());
  // If lowestGroup = 0, model will be active/off.
  size_t lowestGroup = XSContainer::datasets->getLowestGroupForSource(sourceNumber());

  std::vector<string> curParamSettings;
  // note that j counts the number of parameters in the current component
  // only, not the complete model.
  if ( !reachedSkip || (reachedSkip && lowestGroup && NdGroup > lowestGroup))
  {
     size_t  j(0);
     while  (j < newComponentNumParams)
     {   
        bool firstSkip(false);
        if (!reachedSkip)
        {
           // was a 'skip this and the rest' string entered?
           const string thisSetting(paramStrings.front());
           size_t lenBack(XSparse::SKIP().length());      
           reachedSkip = thisSetting.substr(thisSetting.find_first_not_of(" \t"),lenBack) 
                                   == XSparse::SKIP();  
           curParamSettings.push_back(paramStrings.front());
           paramStrings.pop_front();
           if ( reachedSkip )
           {
              // tell future settings to skip [use default values]. 
              // All the strings
              // from this point forth will be ignored.
              firstSkip = true;
              paramStrings.clear();
              paramStrings.push_back(XSparse::SKIP());                                        
           }
        }


        if ( lowestGroup && NdGroup > lowestGroup )
        {
           // automatically make a link.
           if (reachedSkip) 
           {
              if (firstSkip) curParamSettings.pop_back();
              std::ostringstream linkString;
              linkString << "= " << name() << ":" << parameterIndex[j] - base;
              curParamSettings.push_back(linkString.str());
              // tcerr << "link: param " << parameterIndex[j] << " " 
                             // << curParamSettings.back() << std::endl;
           }      
           else
           {
              // catch the 'set these parameters to default' condition
              // for higher datagroups - need to make a link rather than
              // use the default.
              const string& thisSetting = curParamSettings.back();
              int nulltest = thisSetting.find_first_not_of(" \t");
              if ( thisSetting == "/"  || thisSetting.length() == 0 
                      || nulltest < 0)
              {
                 std::ostringstream linkString;
                 linkString << "= " << name() 
                                    << ":" << parameterIndex[j] - base; 
                 curParamSettings.pop_back(); 
                 curParamSettings.push_back(linkString.str());
                 // tcerr << "link: param " << parameterIndex[j] << " " 
                          // << curParamSettings.back() << std::endl;
              }
           }    
        }
        ++j;
     } 
  }
  else
  {
     curParamSettings.push_back(XSparse::SKIP());
  }

  newComponent->setParamValues(curParamSettings);

}

void Model::linkDataGroupParams (int componentIndex)
{

  // function to create parameter links between individual component, which is in a
  // data group model copy, and the parameters of the primary model.

  // safety measure to protect against self-referencing nonsense.
  size_t lowestGroup = XSContainer::datasets->getLowestGroupForSource(sourceNumber());
  if (!lowestGroup || dataGroupNumber() == lowestGroup) return;
  int dummyGroup(0), dummyOffset(0);
  Component* target = componentByNumber(componentIndex,dummyGroup,dummyOffset);
  target->linkParametersToPrimary();
}

std::deque<string> Model::getParamValuesFromUser (int componentIndex)
{
  std::deque<string> returnString;


        // tasks: a) get prompt string
        //        b) get current value string
        //        c) print current value string
        //        d) set prompt
        //        e) extract user input for each parameter.

        //safety...
        returnString.clear();        
        std::ostringstream parPromptStream("");
        // print out model name and datagroup number if not default
        // values.
        if ( m_modelBase->name() != Model::DEFAULT() ) parPromptStream << m_modelBase->name() << ':';      
        if ( XSContainer::datasets->numberOfGroups() > 1 ) 
        {
                parPromptStream << "data group " << dataGroupNumber() << ':' ;
        }                 
        tcout <<  "\nInput parameter value, delta, min, bot, top, and max values for ...\n";

        int dummyGroup(0), dummyOffset(0);
        Component* component (componentByNumber(componentIndex,dummyGroup,dummyOffset));
        std::vector<string> componentParams;
        component->getParamValues(parPromptStream.str(),componentParams);
        std::copy(componentParams.begin(),componentParams.end(), back_inserter(returnString));

  return returnString;
}

void Model::resolveComponentNames (ModelExprTree& expressionTree)
{
  ModelExprTree::iterator l (expressionTree.begin());
  ModelExprTree::iterator lEnd (expressionTree.end());

  while (l != lEnd )
  {
     int N(l->compSpecs().size()); 
     for (int j = 0; j < N; ++j)
     {
        const string& componentName = l->getComponentString(j);
        int gt (componentName.find(ComponentGroup::GROUPTEST()));
        // if this is not a symbol placeholder for a nested group...
        //  replace it with the full name of the component
        if ( gt < 0  )
        {
           Component::getSetupData(componentName);
           l->setComponentString(Component::currentModelName(),j);
        }
     }  
     ++l;  
  }
}

void Model::edit (const string& modelDefinition, int& componentToSet)
{
    static const int INSERT = 1, DELETE = -1;

    ModelExpression<ModExprStandAlone> fullDef(getExpressionFromString(modelDefinition));
    ModelExprTree newFullExpTree;
    fullDef.createExpTree(newFullExpTree);

    ModelExpression<ModExprStandAlone> curFullDef;
    curFullDef.init(fullExpression(),false);
    int toDo = static_cast<int>(fullDef.compSpecs().size()) - 
                static_cast<int>(curFullDef.compSpecs().size());

    int componentIndex=0;
    string componentName;

    //this function checks to ensure that one and only one component has
    //been modified (e.g. added, deleted, or exchanged) in the new model 
    //string. 
    checkNewModelString(fullDef, componentIndex, componentName);

    try
    {
       checkContext(newFullExpTree);
       int groupNum = 0;
       int groupOffset = 0;
       //Some re-used machinery from insertComponent/deleteComponent. At a closer
       //look, insertComponent and deleteComponent alone could not be used to 
       //implement this function. More about this in the notes.
       Component* toDelete = 0;
       toDelete = componentByNumber(componentIndex, groupNum, groupOffset);
       if (toDo == DELETE)
       {
	  if(toDelete)
          {
	      deleteComponentParamInfo(toDelete);
              // This will alter m_modelBase->expressionTree(), but 
              // that's OK since we're soon going to replace it 
              // altogether with newFullExpTree.
              m_modelBase->deleteComponent(groupNum, groupOffset, componentIndex);
          }         
       }
       else
       {
          // Need to create and insert the new component prior to calling
          // recreateParts.  Note: we don't use m_modelBase->insertComponent 
          // since that does additional things such as insert an operator.
          // Unlike addcomp, editmod already has its operator. 
          componentToSet = componentIndex;
          ComponentGroup* compGroup = m_modelBase->compGroupTree().values()[groupNum];
          string saveName(componentName);
          Component* newComp = compGroup->createComponent(componentName);
          // If it made it to here, assume it can't throw anymore.
          if (toDo == INSERT)
          {
             newComp->index(componentIndex);
             compGroup->addToElements(groupOffset, newComp);
             m_modelBase->incrementModelComponentCount();
          }
          else
          {
             // This is subtle, but don't want to do any calls
             // to parameter deregistering (which deleteComponentParamInfo
             // does), since in an exchange this could accidentally 
             // pull out new pars belonging to a newly edited lower dg copy.
             // So, just do some of deleteComponentParamInfo here.  This
             // could certainly be better organized at some point.
             int N=toDelete->parameterIndices().size();
             decrementParameterCount(N);
             const std::vector<Parameter*>& 
	         doomedParameters = const_cast<const Component*>(toDelete)->parameterSet();

             ParamLinkList* links = ParamLinkList::Instance();
             for (int j = 0; j < N; ++j)
	         links->removeDependentParameterLinks(doomedParameters[j]);          

             compGroup->exchangeComponent(groupOffset, newComp);
          }
          // Above calls may change expression due to table model re-prompt, so...
          if (componentName != saveName)
          {
             fullDef.replaceWordBySequence(componentIndex, componentName);
             newFullExpTree.clear();
             fullDef.createExpTree(newFullExpTree);
          }
       }
       m_modelBase->fullExpression(fullDef.exprString());       
       m_modelBase->expressionTree(newFullExpTree);
       m_modelBase->recreateParts();
       m_modelBase->setDataGroupIndexing(dataGroupNumber());
       reindexParameters();

       // This code block is an addcomp/delcomp/edit patch fix that  
       // really should be moved into a new ParamLinkList function. 
       // Also, perhaps it could instead be called from the handlers 
       // after ALL the model objects have been edited, to spare an
       // O(N^2) operation, but for now this seems safer.
       std::map<Parameter*,ParameterLink*>::const_iterator itLinks =
                ParamLinkList::Instance()->linkList().begin();
       std::map<Parameter*,ParameterLink*>::const_iterator itLinksEnd =
                ParamLinkList::Instance()->linkList().end();
       while (itLinks != itLinksEnd)
       {
          itLinks->second->regenerateExpression();
          ++itLinks;
       }         

       registerParameters();
    }
    catch ( Component::NoSuchComponent )
    {
       std::ostringstream msg;
       msg << "editmod ";
       if ( name() != s_DEFAULT ) msg << name() << ": "; 
       msg << componentIndex <<  " " << componentName ;
       throw InvalidModelEdit(msg.str());
    }
    catch ( YellowAlert& )
    {
       std::ostringstream msg;
       msg << "editmod ";
       if ( name() != s_DEFAULT ) msg << name() << ": "; 
       //          string subex (gr->exprString().empty() ? string("<empty>"): gr->exprString());
       //          msg << componentIndex << " (subexpression " << subex << ")";
       throw InvalidModelEdit(msg.str());
       // throw here and have not altered the model == strong exception safe.
    }
}

const XSContainer::MixLocations& Model::mixingLocations () const
{
  return m_modelBase->mixingLocs();
}

void Model::checkForMixing ()
{
  static const string MIX("mix");
  static const string AMX("amx");
  XSContainer::MixLocations mixLocs;
  if (m_modelBase->compGroupTree().size())
  {
     std::vector<Component*> allComponents;
     m_modelBase->bundleComponents(allComponents);
     for (size_t iComp=0; iComp<allComponents.size(); ++iComp)
     {
        Component* testComponent = allComponents[iComp];
        try
        {
           ComponentInfo compInfo = XSModelFunction::fullMatch(testComponent->name());
           if (compInfo.type == MIX)
              mixLocs.first.push_back(iComp);
           else if (compInfo.type == AMX)
              mixLocs.second.push_back(iComp);
        }
        catch (XSModelFunction::NoSuchComponent)
        {
           // By this point, assume component has already been
           // validated.  The only way it can get in here then
           // is if it is a table model (the table model indicator
           // (ie atable) has been stripped off, and thus it will
           // fail the Component::getSetupData test).
        }
     }  
     m_modelBase->mixingLocs(mixLocs);
  }
  else
  {
        throw RedAlert(" checkForMixing called before Model initialization ");       
  }
}

const ArrayContainer& Model::modelFluxError () const
{

  return m_modelBase->modelFluxError();
}

void Model::resetModelFluxError (const ArrayContainer& saved)
{
  m_modelBase->setModelFluxError(saved);
}

void Model::initializeFromData ()
{
}

void Model::checkNewModelString (ModelExpression<ModExprStandAlone> fullDef, int& componentIndex, string& componentName)
{
    ModelExpression<ModExprStandAlone> current;
    current.init(m_modelBase->fullExpression(),false);
    int numCurWords (current.compSpecs().size());
    int numNewWords (fullDef.compSpecs().size());

    std::ostringstream msg;
    msg << "Only one component may be changed:\n"
	<< "\t(old expression): " << fullExpression() << '\n'
	<< "\t(new expression): " << fullDef.exprString() << '\n';

    static const int 
	INSERT = 1, DELETE = -1, EXCHANGE = 0;

    int toDo = numNewWords - numCurWords;
    if (toDo < DELETE || toDo > INSERT)
       throw InvalidModelEdit(msg.str());

    // findWordDifference is only concerned with differences in words.
    // It doesn't check arithmetic operators or parentheses.  For cases
    // of consecutively repeating components such as:
    // wa(ga+po)+po -> wa(ga)+po
    // wa*ga+po -> wa(ga+po)+po
    // wa*ga+po -> wa(wa*ga+po)
    // it is unable to figure out which of the repeating components is the
    // one to mark for deletion/insertion.  In this case the return pair
    // specifies an inclusive range of repeating comps, and leaves it up
    // to this function to figure out the rest.  

    // If no word differences the return value is (0,0). Since case of 
    // new mod == old mod should have been caught before this point,
    // this can only happen due to changes in arithmetic operators or
    // parentheses, which we're not going to allow.

    std::pair<int,int> wordDiffPos(ModelExpression<ModExprStandAlone>::findWordDifference(current, fullDef));
    componentIndex = 0;
    if (wordDiffPos.first != 0 && wordDiffPos.second != 0)
    {
       if (wordDiffPos.first == wordDiffPos.second)
       {
          // The simple unambiguous case. 
          componentIndex = wordDiffPos.first; 
          if (!ModelExpression<ModExprStandAlone>::testEditOperation(current, fullDef, componentIndex))
          {
	     throw InvalidModelEdit(msg.str());
          }
       }
       else
       {
          // The not so simple ambiguous case.  Keep calling
          // testEditOperation for each possible component from left 
          // to right until the first is found whose deletion from the
          // larger expression exactly reproduces the smaller.
          // For example, if
          // A(B+C+C)+C+C -> A(B+C)+C+C,
          // it would determine that the first of the 4 C's should be removed.
          // If A(B+C+C)+C+C -> A(B+C+C)+C,
          // the third C would be flagged for removal.
          // If none of the components passes this test, then this is not
          // a valid editmod operation.
          for (int testIdx=wordDiffPos.first; testIdx <=wordDiffPos.second; ++testIdx)
          {
             if (ModelExpression<ModExprStandAlone>::testEditOperation(current, fullDef, testIdx))
             {
                componentIndex = testIdx;
                break;
             }
          }
          if (!componentIndex)
          {
	     throw InvalidModelEdit(msg.str());
          }
       }
       componentName = (toDo >= EXCHANGE ? fullDef.getComponentString(componentIndex-1) 
                        : current.getComponentString(componentIndex-1));
    }
    else
    {
       std::ostringstream oss;
       oss << "Only components may be changed with editmod:\n" 
	<< "\t(old expression): " << fullExpression() << '\n'
	<< "\t(new expression): " << fullDef.exprString() << '\n';
       throw InvalidModelEdit(oss.str());
    }
}

void Model::deleteComponentParamInfo (Component* toDelete)
{
    deregisterParameters();

    // reduce the number of parameters in the model
    int N (XSContainer::models->eraseComponentParameters(toDelete));

    decrementParameterCount(N);
    // de-link any parameters that are dependent on parameters of the component.
    // note that removeDependentParameterLinks doesn't delete any parameters,
    // but operates only on the addresses of the input parameters.
    // doomed must be cast to const to access the const version of
    // Component::parameterSet() since there is a non-const protected version.
    const std::vector<Parameter*>& 
	doomedParameters = const_cast<const Component*>(toDelete)->parameterSet();

    ParamLinkList* links = ParamLinkList::Instance();
    for (int j = 0; j < N; ++j)
	links->removeDependentParameterLinks(doomedParameters[j]);          
}

void Model::mixingLocations (const XSContainer::MixLocations& value)
{
  m_modelBase->mixingLocs(value);
}

ModelExpression<ModExprStandAlone> Model::getExpressionFromString (const string& modelDef)
{
    ModelExpression<ModExprStandAlone> def;
    def.init(modelDef,false);

    ModelExprTree newExpTree;
    def.createExpTree(newExpTree);

    checkContext(newExpTree);            
    resolveComponentNames(newExpTree);

    ModelExpression<ModExprStandAlone> fullDef;
    fullDef.init(ModelBase::refreshExpression(newExpTree),false);

    return fullDef;
}

const SpectralData::FluxCalc& Model::lastModelFluxCalc () const
{
    return m_modelBase->lastModelFluxCalc();
}

void Model::lastModelFluxCalc (const SpectralData::FluxCalc& modelFluxCalc)
{
    m_modelBase->lastModelFluxCalc(modelFluxCalc);
}

const SpectralData::FluxCalc& Model::lastModelLuminCalc () const
{
    return m_modelBase->lastModelLuminCalc();
}

void Model::lastModelLuminCalc (const SpectralData::FluxCalc& value)
{
    m_modelBase->lastModelLuminCalc(value);
}

void Model::setAutonomousEnergy (const RealArray& energy)
{
   m_modelBase->setAutonomousEnergy(energy);
}

void Model::setAutonomousEnergy (const XSContainer::ExtendRange& extended)
{
   m_modelBase->setAutonomousEnergy(extended);
}

void Model::fillEnergyContainer ()
{
    m_modelBase->fillEnergyContainer();
}

void Model::setSpectraForAutonomousEngs ()
{
   m_modelBase->setSpectraForAutonomousEngs();
}

void Model::updateNewGainFromFit (const Response* response)
{
   m_modelBase->updateNewGainFromFit(response);
}

void Model::bundleParameters (std::vector<Parameter*>& parameters) const
{
   m_modelBase->bundleParameters(parameters);
}

void Model::bundleComponents (std::vector<Component*>& components) const
{
   m_modelBase->bundleComponents(components);
}

std::vector<Model*> Model::makeDataGroupCopies (const std::vector<size_t>& groups)
{
  size_t nNewCopies = groups.size();
  std::vector<Model*> dataGroupCopy (nNewCopies,static_cast<Model*>(0));
  for (size_t i=0; i<nNewCopies; ++i)
  {
     dataGroupCopy[i] = clone();
     Model* newMod = dataGroupCopy[i];
     newMod->setDataGroupIndexing(groups[i]);
     std::vector<Component*> orderedComps;
     newMod->bundleComponents(orderedComps);
     for (size_t j=0; j<orderedComps.size(); ++j)
     {
           Component* modelComponent = orderedComps[j];
           modelComponent->reindexParameters(newMod->parameterIndexBase());   
     } 
  }
  return dataGroupCopy;
}

void Model::makeExtendArray (const XSContainer::ExtendRange& extRange, RealArray& extArray)
{
   ModelBase::makeExtendArray(extRange, extArray);
}

bool Model::usingMdef (const XSCall<MathExpression>* mdef) const
{
   bool foundMdef = false;
   XSModExpTree<ComponentGroup*>::const_iterator itCG = 
                m_modelBase->compGroupTree().begin();
   XSModExpTree<ComponentGroup*>::const_iterator itCGEnd = 
                m_modelBase->compGroupTree().end(); 
   while (!foundMdef && itCG != itCGEnd)
   {
         ComponentListConstIterator itComp = (*itCG)->elements().begin();
         ComponentListConstIterator itCompEnd = (*itCG)->elements().end();
         while (!foundMdef && itComp != itCompEnd)
         {
            if ((*itComp)->index() > 0)
               foundMdef = (*itComp)->usingMdef(mdef);
            ++itComp;
         }
      ++itCG;      
   }      
   return foundMdef;
}

void Model::printHeading () const
{
   using namespace XSContainer;
   const size_t linelen(72);
   const string top(linelen,'=');
   const string delim("()*+ \t");
   const string& storedFullExpr = fullExpression();
   string reportFullExpr;
   // Add bracketed component nums to full expression.
   int compIdx = 0;
   string::size_type startPos = 0;
   do 
   {
      string::size_type wordPos = storedFullExpr.find_first_not_of(delim, startPos);
      string::size_type endPos = string::npos;
      if (wordPos != string::npos)
      {
         endPos = storedFullExpr.find_first_of(delim, wordPos);
         string::size_type len = (endPos == string::npos) ?
                   string::npos : endPos - startPos;
         reportFullExpr += storedFullExpr.substr(startPos, len);
         ++compIdx;
         std::ostringstream oss;
         oss << '<' << compIdx << '>';
         reportFullExpr += oss.str(); 
      }
      else if (startPos != string::npos)
      {
         // We've reached the last component.  Pick up any
         // trailing ')'s.
         reportFullExpr += storedFullExpr.substr(startPos);
      }
      startPos = endPos;
   }  while (startPos != string::npos); 

   tcout << '\n' << top << '\n'; 
   tcout << "Model ";
   if (name() != Model::DEFAULT() ) tcout << name() << ':';
   tcout << reportFullExpr;
   if (datasets->numSourcesForSpectra() >= 1 ) tcout << " Source No.: " << sourceNumber();
   bool isAct = models->activeModelNames(name());
   string status = isAct ? string("   Active") : string("   Inactive");
   tcout << status << "/";
   status = isActive() ? string("On") : string("Off");
   tcout << status;
   tcout << std::endl; 
   tcout << "Model Model Component  Parameter  Unit     Value"
    <<"\n par  comp"<<std::endl;
}

void Model::printMixComp () const
{
   const std::vector<size_t>& mixModLocs = m_modelBase->mixingLocs().first;
   if (mixModLocs.size() && (!isActive() || 
          m_modelBase->dataGroupNumber() == 
          XSContainer::datasets->getLowestGroupForSource(m_modelBase->sourceNumber())))
   {
      const size_t linelen = 72;
      const string mixSeparator(linelen,'-');     
      std::vector<Component*> orderedComps;
      bundleComponents(orderedComps);
      for (size_t i=0; i<mixModLocs.size(); ++i)
      {
         tcout << mixSeparator << std::endl;
         tcout << *(orderedComps[mixModLocs[i]]);
      }  
   }  
}

bool Model::areCompsSpecDependent () const
{
   return m_modelBase->areCompsSpecDependent();
}

void Model::alignFluxForFold (ArrayContainer& saveFlux, ArrayContainer& saveFluxError, SumComponent* sourceComp)
{
   m_modelBase->alignFluxForFold(saveFlux, saveFluxError, sourceComp);
}

bool Model::isFolded () const
{
   return m_modelBase->folded();
}

void Model::isFolded (bool value)
{
   m_modelBase->folded(value);
}

void Model::checkZeroNorms (std::set<int>& parsWithZeroNorms)
{
   m_modelBase->checkZeroNorms(parsWithZeroNorms);
}

bool Model::checkParameterMagnitudes(Real threshold) const
{
   return m_modelBase->checkParameterMagnitudes(threshold);
}



// Additional Declarations
std::ostream& operator<<(std::ostream& s, const Model& right)
{

  using namespace XSContainer;
  ModelBase* input(right.m_modelBase);

  if (datasets->numberOfGroups() > 1 )
  {
     s << "                           Data group: " 
       << input->dataGroupNumber()
       << std::endl;
  }  

  std::vector<Component*> orderedComps;
  input->bundleComponents(orderedComps);
  const size_t nComps = orderedComps.size();
  
  // Mix/amx components will not be printed here.  This output
  // is printed for each data group copy whereas those components
  // should only be printed once (see printMixComp).  
  const XSContainer::MixLocations& mixComps = right.mixingLocations();
  std::set<size_t> compsToAvoid;
  for (size_t i=0; i<mixComps.first.size(); ++i)
  {
     compsToAvoid.insert(mixComps.first[i]);
  }
  for (size_t i=0; i<mixComps.second.size(); ++i)
  {
     compsToAvoid.insert(mixComps.second[i]);
  }
  std::set<size_t>::const_iterator itAvoid = compsToAvoid.begin();
  std::set<size_t>::const_iterator itAvoidEnd = compsToAvoid.end();
  for (size_t k = 0; k < nComps; ++k )
  {
     if (itAvoid != itAvoidEnd && k == *itAvoid)
     {
        ++itAvoid;
     }
     else
     {
        Component* comp = orderedComps[k];
        s << *comp;
     }
  }

  right.debugPrint(s);

  return s; 

}
