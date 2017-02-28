//   Read the documentation to learn more about C++ code generator
//   versioning.
//	  %X% %Q% %Z% %W%
// componentCreator

// Component
#include <XSModel/Model/Component/Component.h>
// AddTableComponent
#include <XSModel/Model/Component/AddTableComponent.h>
// MulTableComponent
#include <XSModel/Model/Component/MulTableComponent.h>
// ConComponent
#include <XSModel/Model/Component/ConComponent.h>
// MixComponent
#include <XSModel/Model/Component/MixComponent.h>
// SumComponent
#include <XSModel/Model/Component/SumComponent.h>
// ComponentCreator
#include <XSModel/Model/Component/ComponentCreator.h>
#include <XSModel/Model/Component/ComponentGroup.h>

#include <XSModel/Model/Component/ACNComponent.h>
#include <XSModel/Model/Component/AMXComponent.h>
#include <memory>


// Class ComponentCreator 

ComponentCreator::ComponentCreator()
  : m_component(0)
{
}


ComponentCreator::~ComponentCreator()
{
}


Component* ComponentCreator::GetComponent (const string& name, ComponentGroup* group)
{
        if (m_component == 0)  {m_component =  Make(name, group);} return m_component;
}

Component* ComponentCreator::GetComponent (const string& id, ComponentGroup* group, const string& type, int table)
{
        if (m_component == 0)  {m_component =  Make(id, group, type,  table);} return m_component;
}

Component* ComponentCreator::Make (const string& name, ComponentGroup* group)
{
    // virtual constructor method for components. The getSetupData
  // function reads the type from the model.dat or table model file
  // (or any other component construction methods that might be added).
  // After the component is constructed there is enough information to read the
  // component information and the details of its parameters.



  Component::getSetupData(name);
  std::auto_ptr<Component> cp;
  const string type = Component::currentType();
  const bool table  = Component::currentIsTable();
  if (type == "add") 
  {
          if (table) cp.reset(new AddTableComponent(group));
          else cp.reset(new AddComponent(group));
  }
  if (type == "mul")  
  {
          if (table) cp.reset(new MulTableComponent(group));
          else cp.reset(new MulComponent(group));
  }
  if (type == "con") cp.reset(new ConComponent(group));
  if (type == "acn") cp.reset(new ACNComponent(group));
  if (type == "mix") cp.reset(new MixComponent(group));
  if (type == "amx") cp.reset(new AMXComponent(group));

  // the ctor for the components should throw a YellowAlert exception
  // if anything went wrong before this. So should the Component::read
  // function.

  cp->read();

  return cp.release();
}

Component* ComponentCreator::Make (const string& idstring, ComponentGroup* group, const string& type, int table)
{
  // This alternate form of the Make function allows the creation of empty components
// of specified class. It is used for creating a "container" for a computed component
// group. The formal "name" argument is called idstring here to stress the fact that
// this function is not used for creating a proper component, but an internal temporary
// labelled by an identifier.

// This function is most likely to be called for an additive (non-table) Component
// but has been written to be capable of returning a pointer to any type of component.
// Note also that this also creates an empty component unlike the two argument version that
// also reads the model.

        Component* cp;
        if (type == "add") 
        {
                if (table) cp =  new AddTableComponent(group);
                else cp = new AddComponent(group);
        }
        if (type == "mul")  
        {
                if (table) cp =  new MulTableComponent(group);
                else cp = new MulComponent(group);
        }        
        if (type == "con") cp =  new ConComponent(group);
        if (type == "mix") cp =  new MixComponent(group);
        if (type == "acn") cp =  new ACNComponent(group);
        if (type == "amx") cp =  new AMXComponent(group);
        if (type == "sum") cp =  new SumComponent(group);
        cp->name(idstring);
        return cp;
}

// Additional Declarations
