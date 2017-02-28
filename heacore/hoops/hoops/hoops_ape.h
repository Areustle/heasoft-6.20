/******************************************************************************
 *   File name: hoops_ape.h                                                   *
 *                                                                            *
 * Description: Definition of Ape-based interface.                            *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOPS_APE_H
#define HOOPS_APE_H
////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include "hoops/hoops.h"
#include "hoops/hoops_exception.h"
#include <string>
#include <vector>
////////////////////////////////////////////////////////////////////////////////

#ifndef EXPSYM
#ifdef WIN32
#define EXPSYM __declspec(dllexport)
#else
#define EXPSYM
#endif
#endif

namespace hoops {

  //////////////////////////////////////////////////////////////////////////////
  // Constants.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Type declarations/definitions.
  //////////////////////////////////////////////////////////////////////////////
  class ApeException : public Hexception {
    public:
      ApeException(const int & code, const std::string & msg,
                   const std::string & filename = std::string(), int line = 0);
      int ApeCode() const;

    protected:
      static int ape2Hoops(int status);
      int mApeCode;
  };

  class EXPSYM HoopsApeFile : public IParFile {
    public:
      HoopsApeFile(const HoopsApeFile & pf);
      HoopsApeFile(const IParFile & pf);
      HoopsApeFile(const std::string & comp, int argc = 0, char ** argv = 0);

      virtual ~HoopsApeFile();

      virtual HoopsApeFile & operator =(const HoopsApeFile & pf);
      virtual HoopsApeFile & operator =(const IParFile & pf);

      // Synchronize memory image with parameter file and vice versa.
      virtual void Load();
      virtual void Save() const;

      // Read member access.
      virtual const std::string & Component() const { return mComponent; }
      virtual IParGroup & Group();
      virtual const IParGroup & Group() const;

      // Write member access.
      virtual HoopsApeFile & SetComponent(const std::string & comp);
      virtual IParGroup * SetGroup(IParGroup * group = 0);

      virtual GenParItor begin();
      virtual ConstGenParItor begin() const;
      virtual GenParItor end();
      virtual ConstGenParItor end() const;

      virtual IParFile * Clone() const;

      void OpenParFile() const;
      void CloseParFile(int status = 0) const;
      void SetArgs(int argc, char ** argv);
      int Argc() const { return mArgc; }
      char ** const Argv() const { return mArgv; }

    protected:
      std::string mComponent;
      mutable IParGroup * mGroup;
      int mArgc;
      char ** mArgv;
      void CleanComponent(const std::string & comp, std::string & clean) const;
  };

  class EXPSYM HoopsApePrompt : public IParPrompt {
    public:
      HoopsApePrompt(const HoopsApePrompt & prompt);
      HoopsApePrompt(const IParPrompt & prompt);
      HoopsApePrompt(int argc, char ** argv, const std::string & comp_name = std::string());

      virtual ~HoopsApePrompt();

      virtual HoopsApePrompt & operator =(const HoopsApePrompt & p);
      virtual HoopsApePrompt & operator =(const IParPrompt & p);

      virtual HoopsApePrompt & Prompt();
      virtual HoopsApePrompt & Prompt(const std::string & pname);
      virtual HoopsApePrompt & Prompt(const std::vector<std::string> & pnames);

      virtual int Argc() const { return mFile->Argc(); }
      virtual char ** const Argv() const { return mFile->Argv(); }
      virtual IParGroup & Group();
      virtual const IParGroup & Group() const;

      virtual IParGroup * SetGroup(IParGroup * group = 0);

      virtual IParPrompt * Clone() const;

    protected:
      void Init(int argc, char ** argv, const std::string & comp_name = std::string());
      mutable HoopsApeFile * mFile;
  };
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Global variable forward declarations.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //////////////////////////////////////////////////////////////////////////////
  // Function declarations.
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

}
#endif

/******************************************************************************
 * $Log: hoops_ape.h,v $
 * Revision 1.4  2010/01/07 19:21:10  peachey
 * Add ApeCode() method and mApeCode member for getting Ape's status.
 *
 * Revision 1.3  2010/01/07 17:56:50  peachey
 * Improve and make universal the conversion of ape error codes to hoops codes.
 *
 * Revision 1.2  2009/12/23 20:27:19  peachey
 * Add some needed missing header files.
 *
 * Revision 1.1  2007/02/15 21:33:58  peachey
 * Initial version of Ape-based implementation.
 *
 ******************************************************************************/
