//
//  XSPEC12  November 2003
//
//

#include <XSUser/Global/Global.h>
#include <XSUser/Global/XSGlobal.h>
#include <XSUser/Handler/HandlerUtils.h>
#include <XSUser/Handler/XSinterface.h>
#include <XSModel/GlobalContainer/ModelContainer.h>
#include <XSModel/Model/Model.h>
#include <XSModel/Model/Component/Component.h>
#include <XSUtil/Parse/XSparse.h>
#include <XSContainer.h>
#include <XSstreams.h>
#include <xsTypes.h>


int
XSGlobal::xsEditmod(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doEditmod(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int XSGlobal::doEditmod(const StringArray& rawArgs)
{
   using XSContainer::models;
   const char* cmd = "editmod";
   const size_t nArgs = rawArgs.size();
   // editmod needs one argument  [model specifier:]model string
   if ( nArgs < 2 )
   {
      XSGlobal::printDocs(cmd,"?");      
      return 0;
   }
   else 
   {
      const string firstArg(rawArgs[1]);
      if ( firstArg[0] == '?')
      {
	 XSGlobal::printDocs(cmd,"?");      
	 return 0;      
      }    
      else
      {
	 // the first argument can either be
	 // a) a regular part of a model expression
	 // b) modelName:
	 // c) modelName:<first part of model expression>

	 // look for a colon.
	 string cmdStr;


	 // okay, we actually are creating rather than deleting 
	 // models. parse into command and "batch" information,
	 // supplied to parameter setting functions.
	 size_t colon = firstArg.find_first_of(':');
	 string modelName;
	 if ( colon == XSparse::NOTFOUND() )
	 {
	    cmdStr = firstArg;
	 }
	 else
	 {
	    modelName = firstArg.substr(0,colon);
	    cmdStr = firstArg.substr(colon+1);
	 }

	 for (size_t j = 2; j < nArgs; ++j)
	 {
            cmdStr += " ";
	    cmdStr += rawArgs[j];       
	 } 

	 string modelString;
	 std::deque<string> parameterStrings;
	 XSparse::findBatchString(cmdStr,modelString,parameterStrings);

	 // need ModelContainer method that returns array of model pointers
	 // of size number of data groups.
	 std::vector<Model*> modelsToBeEdited (models->lookupModelGroup(modelName));
	 // then need Model component deletion.  
	 size_t N = modelsToBeEdited.size();

	 // componentToSet will be set to a non-zero value if there 
	 // is a component with parameters to be prompted for.
	 int componentToSet(0);
         bool isDeregistered = false;
	 try 
	 {
	    if ( N == 0 ) 
	    {
	       string msg(" Model not defined ");
	       if ( modelName.length())
	       {
	          msg += string(": ");
	          msg += modelName;           
	       }
	       throw YellowAlert (msg);
	    }
            if (!modelName.length())
            {
               modelName = Model::DEFAULT();
            }
	    bool promptUser(parameterStrings.empty());
	    bool skipEntered(false);

            // These may throw.
	    ModelExpression<ModExprStandAlone> 
		newModel = Model::getExpressionFromString(modelString),
		curModel = Model::getExpressionFromString(modelsToBeEdited[0]->fullExpression());

	    if(!(newModel == curModel))
	    {
	       // deregister all parameters from the lookup table
	       // this makes the command destructive, but no real option
	       models->deregisterModelParameters(modelName);
               isDeregistered = true;

               // If this operation changes the model's spectrum dependency
               // state, it will require an additional layer of updating
               // to redo the model's unique energy container.
               const bool origSpecDep = modelsToBeEdited[0]->areCompsSpecDependent();

	       modelsToBeEdited[0]->edit(modelString,componentToSet); 
               bool specDep = modelsToBeEdited[0]->areCompsSpecDependent();
               modelsToBeEdited[0]->checkForMixing();   
	       if (componentToSet)
	       {
		  if (promptUser) 
		  {
		     // getParamValuesFromUser prompts only for the strings
		     // related to the current model, not for the higher data groups.
		     parameterStrings =
                        modelsToBeEdited[0]->getParamValuesFromUser(componentToSet);     
		  }
		  modelsToBeEdited[0]->setParamValues(componentToSet,parameterStrings, 
						       skipEntered);
	       }      


	       for (size_t j = 1; j < N ; ++j)
	       {
	          Model* dataGroupCopy (modelsToBeEdited[j]);
	          dataGroupCopy->edit(modelString,componentToSet);  
	          if ( componentToSet )
	          {  
		     if ( skipEntered )
		     {
		        dataGroupCopy->linkDataGroupParams(componentToSet);
		     }
		     else
		     {
		        if (promptUser)
		        {
			   parameterStrings  
			       = dataGroupCopy->getParamValuesFromUser
			       (componentToSet);  
		        }   
		        dataGroupCopy->setParamValues
			    (componentToSet,parameterStrings,skipEntered);     
		     }
	          }
                  dataGroupCopy->mixingLocations(modelsToBeEdited[0]->mixingLocations());
	       } 

               if (specDep != origSpecDep)
               {
                  // Can no longer trust the UniqueEnergy setup.  Redo by
                  // reattaching all responses.  This is actually overkill since
                  // it operates on all loaded models, but as yet there is no
                  // intermediate updating function.
                  XSContainer::models->Update();               
               }
               else
               {
                  // In case a mixing comp has been added...
                  XSContainer::models->initializeMixingTransformation(modelName);
	          // force computation of the models just edited.
	          XSContainer::models->setCompute(modelName);
                  if (!modelsToBeEdited[0]->isActive())
                  {
                     // For inactive models, need to initialize calculation
                     // here since it won't be done through Fit::Update.
                     std::vector<Component*> newComps;
                     modelsToBeEdited[0]->bundleComponents(newComps);
                     for (size_t i=0; i<newComps.size(); ++i)
                     {
                        newComps[i]->initializeForFit();
                     }
                     XSContainer::models->calculate(modelName);
                  }
	          XSContainer::models->Notify();
               }
               modelsToBeEdited[0]->printHeading();
	       for (size_t j = 0; j < N ; ++j)
	       {
		  tcout << *modelsToBeEdited[j] << std::endl;    
	       } 
               modelsToBeEdited[0]->printMixComp();
               tcout << string(72,'_') << std::endl; 

	       return 0;
	    }
	    else
	       throw YellowAlert("Models are identical");
	 }   
	 catch (YellowAlert&)
	 {
	    // restore registry of models that still exist...
            if (isDeregistered)
	       models->registerModelParameters(modelName);
	    // insert Component will throw if the seek on componentNumber
	    // fails, in which case the model will be unchanged.

	    // will throw InvalidModelEdit if the model 
	    // if the component fails the context checks.
	    return -1;                               
	 }  
      }   
   }
}
