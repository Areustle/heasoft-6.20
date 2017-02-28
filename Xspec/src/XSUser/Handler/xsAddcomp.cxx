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
#include <XSstreams.h>
#include <XSContainer.h>
#include <xsTypes.h>
#include <deque>


int
XSGlobal::xsAddcomp(ClientData cdata,Tcl_Interp* tclInterp,int objc, Tcl_Obj* CONST  objv[])
{
   StringArray rawArgs;
   HandlerUtils::tclArgsToCpp(objc, objv, rawArgs);
   int status = doAddcomp(rawArgs);
   if (!status)
      return globalData->autoSave(TCL_OK);
   else
      return globalData->autoSave(TCL_ERROR);
}

int
XSGlobal::doAddcomp(const StringArray& rawArgs)
{
   using XSContainer::models;
   const char* cmd = "addcomp";
   const size_t nArgs = rawArgs.size();
   // addcomp needs two arguments, a model location specifier and a component name.
   if ( nArgs < 3 )
   {
      XSGlobal::printDocs(cmd,"?");      
      return 0;
   }
   else 
   {
      const string& modelSpecifier = rawArgs[1];
      if ( modelSpecifier[0] == '?')
      {
         XSGlobal::printDocs(cmd,"?");      
         return 0;      
      }    
      else
      {
         const string& componentName = rawArgs[2];
         size_t componentNumber(0);
         string modelName("");
         // componentNumber represents the index number of the new component in the 
         // model.
         XSparse::stringIntPair(modelSpecifier,modelName,componentNumber);
         if (!componentNumber)
         {
            tcout << "***Error: Cannot add component number 0" << std::endl;
            return -1;
         }

         // okay, we actually are creating rather than deleting 
         // models. parse into command and "batch" information,
         // supplied to parameter setting functions.
         std::deque<string> parameterStrings;
         if (nArgs > 3 )
         {
            string dummy;
            string paramSettings = rawArgs[3];

            for (size_t j= 4; j < nArgs; ++j) 
            {
               paramSettings += " ";
               paramSettings += rawArgs[j];
            }

            XSparse::findBatchString(paramSettings,dummy,parameterStrings);
         }


         // need ModelContainer method that returns array of model pointers
         // of size number of data groups.
         std::vector<Model*> modelsToBeEdited (models->lookupModelGroup(modelName));
         // then need Model component deletion.  
         size_t N = modelsToBeEdited.size();

         if ( N == 0 ) 
         {
            string msg(" Model not defined ");
            if ( modelName.length())
            {
               msg += string(": ");
               msg += modelName;           
            }
            tcout << msg << std::endl;
            return -1;      
         }
         else
         {
            size_t nComps = modelsToBeEdited[0]->numberOfComponents();
            if (componentNumber > nComps+1)
            {
               tcout << "***Error: Cannot addcomp " << componentNumber 
                 <<", only " << nComps << " exist in model." << std::endl;
               return -1;
            }
         }

         try 
         {
            // deregister all parameters from the lookup table
            // this makes the command destructive, but no real option
            //
            if (!modelName.length())
            {
               modelName = Model::DEFAULT();
            }

            // If this operation changes the model's spectrum dependency
            // state, it will require an additional layer of updating
            // to redo the model's unique energy container.
            const bool origSpecDep = modelsToBeEdited[0]->areCompsSpecDependent();

            models->deregisterModelParameters(modelName);
            bool promptUser(parameterStrings.empty());
            bool skipEntered(false);

            modelsToBeEdited[0]->insertComponent(componentNumber,componentName);
            bool specDep = modelsToBeEdited[0]->areCompsSpecDependent();

            if (promptUser) 
            {
               // getParamValuesFromUser prompts only for the strings
               // related to the current model, not for the higher data groups.
               parameterStrings = modelsToBeEdited[0]->getParamValuesFromUser(componentNumber);     

            }
            modelsToBeEdited[0]->setParamValues(componentNumber, parameterStrings, skipEntered);      
            modelsToBeEdited[0]->checkForMixing();
            for (size_t j = 1; j < N ; ++j)
            {
               Model* dataGroupCopy (modelsToBeEdited[j]);
               dataGroupCopy->insertComponent(componentNumber,componentName);    
               if ( skipEntered )
               {
                  dataGroupCopy->linkDataGroupParams(componentNumber);
               }
               else
               {
                  if (promptUser)
                  {
                     parameterStrings  = dataGroupCopy->getParamValuesFromUser(componentNumber);  
                  }   
                  dataGroupCopy->setParamValues(componentNumber,parameterStrings,skipEntered);     
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
               // In case new component is a mixing component...
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
         catch (YellowAlert&)
         {
            // restore registry of models that still exist...
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
