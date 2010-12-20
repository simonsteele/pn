/**
 * @file allocator.h
 * @brief Allocator for simple memory blocks
 * @author Simon Steele
 * @note Copyright (c) 2007 Simon Steele - http://untidy.net/
 *
 * Programmer's Notepad 2 : The license file (license.[txt|html]) describes 
 * the conditions under which this source may be modified / distributed.
 */

#ifndef allocator_h__included
#define allocator_h__included

namespace PN {

class DefaultTraits
{
public:
	enum {
		RequiresConstruction = true
	};
};

template <typename T>
class Traits : DefaultTraits {};

// Specialise to allow allocators for these types:
template<> class Traits<char> { public: enum { RequiresConstruction = false }; };
template<> class Traits<wchar_t> { public: enum { RequiresConstruction = false }; };
template<> class Traits<unsigned short> { public: enum { RequiresConstruction = false }; };

template <typename T>
class Allocator
{
public:
	virtual T* Alloc(size_t noofTs) = 0;
	virtual void Free(T* mem) = 0;
	virtual T* ReAlloc(T* mem, size_t oldNoofTs, size_t noofTs) = 0;
};

/**
 * Implements a ReAlloc method that relies on allocating the memory
 * for the new storage first, and then copying the old into the new.
 * 
 * Used as a base class for other allocators so they don't need to 
 * re-implement.
 */
template <typename T>
class WastefulReallocAllocator : public Allocator<T>
{
public:
	WastefulReallocAllocator()
	{
		PNASSERT(Traits<T>::RequiresConstruction == false);
	}

	virtual T* ReAlloc(T* mem, size_t oldNoofTs, size_t noofTs)
	{
		PNASSERT(oldNoofTs < noofTs);

		T* newMem = Alloc(noofTs);
		if(newMem == NULL)
		{
			return NULL;
		}

		memcpy(newMem, mem, oldNoofTs * sizeof(T));

		Free(mem);

		return newMem;
	}
};

template <typename T>
class BasicAllocator : public WastefulReallocAllocator<T>
{
public:
	BasicAllocator()
	{
		PNASSERT(Traits<T>::RequiresConstruction == false);
	}

	virtual T* Alloc(size_t noofTs)
	{
		return new T[noofTs];
	}

	virtual void Free(T* mem)
	{
		delete [] mem;
	}
};

template <typename T>
class LocalAllocAllocator : public WastefulReallocAllocator<T>
{
public:
	LocalAllocAllocator()
	{
		PNASSERT(Traits<T>::RequiresConstruction == false);
	}

	virtual T* Alloc(size_t noofTs)
	{
		T* ptrs = static_cast<T*>( ::LocalAlloc(LMEM_FIXED, noofTs * sizeof(T)) );
		// if we want to support constructors we must do that here...
		return ptrs;
	}

	virtual void Free(T* mem)
	{
		::LocalFree(mem);
	}
};

} // namespace PN

#endif