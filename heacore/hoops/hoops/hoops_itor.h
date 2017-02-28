/******************************************************************************
 *   File name: hoops_itor.h                                                  *
 *                                                                            *
 * Description: Declaration of container-independent iterator interface.      *
 *                                                                            *
 *    Language: C++                                                           *
 *                                                                            *
 *      Author: James Peachey, for HEASARC/GSFC/NASA                          *
 *                                                                            *
 *  Change log: see CVS Change log at the end of the file.                    *
 ******************************************************************************/
#ifndef HOOPS_ITOR_H
#define HOOPS_ITOR_H

////////////////////////////////////////////////////////////////////////////////
// C++ header files.
////////////////////////////////////////////////////////////////////////////////
#include <cassert>
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
  template <typename T>
  class EXPSYM IBiDirItor {
    public:
      typedef T Value_t;
      virtual ~IBiDirItor() {}
      virtual IBiDirItor * Clone() const = 0;
      virtual T & operator *() = 0;
      virtual T * operator ->() = 0;
      virtual IBiDirItor & operator ++() = 0;
      virtual IBiDirItor & operator --() = 0;
      virtual bool operator ==(const IBiDirItor & other) const = 0;
      virtual bool operator !=(const IBiDirItor & other) const = 0;
      virtual const T & Ref() const = 0;
  };

  template <typename T, typename Iterator_t>
  class EXPSYM BiDirItor : public IBiDirItor<T> {
    public:
      BiDirItor(): IBiDirItor<T>(), mItor() {}
      BiDirItor(const BiDirItor & other): IBiDirItor<T>(), mItor(other.mItor) {}
      BiDirItor(const Iterator_t & other): IBiDirItor<T>(), mItor(other) {}

      virtual ~BiDirItor() {}

      virtual BiDirItor * Clone() const
        { return new BiDirItor(*this); }
      virtual T & operator *()
        { return *mItor; }
      virtual T * operator ->()
        { return &(*mItor); } // works for non-class iterators too
      virtual BiDirItor & operator ++()
        { ++mItor; return *this; }
      virtual BiDirItor & operator --() 
        { --mItor; return *this; }
      virtual bool operator ==(const IBiDirItor<T> & other) const
        { return &(*mItor) == &(other.Ref()); }
      virtual bool operator !=(const IBiDirItor<T> & other) const
        { return &(*mItor) != &(other.Ref()); }
      virtual const T & Ref() const { return *mItor; }

    private:
      Iterator_t mItor;
  };

  template <typename T>
  class EXPSYM GenBiDirItor : public IBiDirItor<T> {
    public:
      GenBiDirItor(): IBiDirItor<T>(), mItor(0) {}
      GenBiDirItor(const GenBiDirItor & other): IBiDirItor<T>(), mItor(0)
        { if (other.mItor) mItor = other.mItor->Clone(); }
      GenBiDirItor(const IBiDirItor<T> & other): IBiDirItor<T>(), mItor(0)
        { mItor = other.Clone(); }

      virtual ~GenBiDirItor() { delete mItor; }

      virtual GenBiDirItor & operator =(const GenBiDirItor & other) {
        delete mItor;
        if (other.mItor) mItor = other.mItor->Clone();
        else mItor = 0;
        return *this;
      }

      virtual GenBiDirItor & operator =(const IBiDirItor<T> & other) {
        delete mItor;
        mItor = other.Clone();
        return *this;
      }

      virtual GenBiDirItor * Clone() const
        { return new GenBiDirItor(*this); }
      virtual T & operator *()
        { return *GetItor(); }
      virtual T * operator ->()
        { return &(*GetItor()); }
      virtual GenBiDirItor & operator ++()
        { ++GetItor(); return *this; }
      virtual GenBiDirItor & operator --()
        { --GetItor(); return *this; }
      virtual bool operator ==(const IBiDirItor<T> & other) const
        { return &(Ref()) == &(other.Ref()); }
      virtual bool operator !=(const IBiDirItor<T> & other) const
        { return &(Ref()) != &(other.Ref()); }
      virtual const T & Ref() const { assert(mItor); return **mItor; }

    protected:
      virtual IBiDirItor<T> & GetItor() {
        assert(mItor);
        return *mItor;
      }

    private:
      IBiDirItor<T> * mItor;
  };

  template <typename T>
  class EXPSYM IConstBiDirItor {
    public:
      typedef T Value_t;
      virtual ~IConstBiDirItor() {}
      virtual IConstBiDirItor * Clone() const = 0;
      virtual const T & operator *() = 0;
      virtual const T * operator ->() = 0;
      virtual IConstBiDirItor & operator ++() = 0;
      virtual IConstBiDirItor & operator --() = 0;
      virtual bool operator ==(const IConstBiDirItor & other) const = 0;
      virtual bool operator !=(const IConstBiDirItor & other) const = 0;
      virtual const T & Ref() const = 0;
  };

  template <typename T, typename Iterator_t>
  class EXPSYM ConstBiDirItor : public IConstBiDirItor<T> {
    public:
      ConstBiDirItor(): IConstBiDirItor<T>(), mConstItor() {}
      ConstBiDirItor(const ConstBiDirItor & other):
        IConstBiDirItor<T>(), mConstItor(other.mConstItor) {}
      ConstBiDirItor(const Iterator_t & other): IConstBiDirItor<T>(), mConstItor(other) {}

      virtual ~ConstBiDirItor() {}

      virtual ConstBiDirItor * Clone() const
        { return new ConstBiDirItor(*this); }
      virtual const T & operator *()
        { return *mConstItor; }
      virtual const T * operator ->()
        { return &(*mConstItor); } // works for non-class iterators too
      virtual ConstBiDirItor & operator ++()
        { ++mConstItor; return *this; }
      virtual ConstBiDirItor & operator --() 
        { --mConstItor; return *this; }
      virtual bool operator ==(const IConstBiDirItor<T> & other) const
        { return &(*mConstItor) == &(other.Ref()); }
      virtual bool operator !=(const IConstBiDirItor<T> & other) const
        { return &(*mConstItor) != &(other.Ref()); }
      virtual const T & Ref() const { return *mConstItor; }

    private:
      Iterator_t mConstItor;
  };

  template <typename T>
  class EXPSYM ConstGenBiDirItor : public IConstBiDirItor<T> {
    public:
      ConstGenBiDirItor(): IConstBiDirItor<T>(), mItor(0) {}
      ConstGenBiDirItor(const ConstGenBiDirItor & other): IConstBiDirItor<T>(), mItor(0)
        { if (other.mItor) mItor = other.mItor->Clone(); }
      ConstGenBiDirItor(const IConstBiDirItor<T> & other): IConstBiDirItor<T>(), mItor(0)
        { mItor = other.Clone(); }

      virtual ~ConstGenBiDirItor() { delete mItor; }

      virtual ConstGenBiDirItor & operator =(const ConstGenBiDirItor & other) {
        delete mItor;
        if (other.mItor) mItor = other.mItor->Clone();
        else mItor = 0;
        return *this;
      }

      virtual ConstGenBiDirItor & operator =(const IConstBiDirItor<T> & other) {
        delete mItor;
        mItor = other.Clone();
        return *this;
      }

      virtual ConstGenBiDirItor * Clone() const
        { return new ConstGenBiDirItor(*this); }
      virtual const T & operator *()
        { return *GetItor(); }
      virtual const T * operator ->()
        { return &(*GetItor()); }
      virtual ConstGenBiDirItor & operator ++()
        { ++GetItor(); return *this; }
      virtual ConstGenBiDirItor & operator --()
        { --GetItor(); return *this; }
      virtual bool operator ==(const IConstBiDirItor<T> & other) const
        { return &(Ref()) == &(other.Ref()); }
      virtual bool operator !=(const IConstBiDirItor<T> & other) const
        { return &(Ref()) != &(other.Ref()); }
      virtual const T & Ref() const { assert(mItor); return **mItor; }

    protected:
      virtual IConstBiDirItor<T> & GetItor() {
        assert(mItor);
        return *mItor;
      }

    private:
      IConstBiDirItor<T> * mItor;
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
 * $Log: hoops_itor.h,v $
 * Revision 1.5  2004/03/16 20:50:48  peachey
 * Explicitly invoke constructors for base classes to shut up compiler
 * warnings in the SLAC build.
 *
 * Revision 1.4  2003/11/26 18:50:03  peachey
 * Merging changes made to SLAC repository into Goddard repository.
 *
 * Revision 1.3  2003/11/26 17:54:09  peachey
 * Explicitly zero-initialize all pointers, as VS does not adhere to
 * the ISO standard for default-initialized pointers.
 *
 * Revision 1.2  2003/11/13 19:29:29  peachey
 * Add preprocessor macro needed on Windows to export symbols.
 *
 * Revision 1.1.1.1  2003/11/04 01:48:30  jchiang
 * First import
 *
 * Revision 1.1  2003/04/23 17:40:36  peachey
 * Inline definitions of iterator classes to be used by ParGroup family.
 *
 ******************************************************************************/
